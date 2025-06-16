import sys
import json
from datetime import datetime, date

import graph2
import barchart


def dump_json(data, fn):
    with open(fn, "w", encoding="utf-8") as file:
        json.dump(data, file, default=datetime_serializer)
    print(f"Successfully wrote to file: {fn}")


def select_null(item):
    return ""


def select_target(item):
    return item["target"]


def select_ncpus(item):
    return int(item["DOCKER_NCPUS"])


def select_tags(item):
    return item["TAG"]


def select_ratetime(item):
    return int(item["RATETIMELIMIT"])


def select_multi_rate(item):
    return int(item["multi_rate"])


def select_mean(item):
    return float(item["mean"])


def select_sender_count(item):
    return int(item["sender_count"])


def int_item_selector(name):
    def my_selector(item):
        if name in item:
            return int(item[name])
        else:
            return None

    return my_selector


def select_packed(item):
    if "NOPACK" in item and item["NOPACK"] != 0:
        return "UNPACKED"
    else:
        return "PACKED"


tail = lambda ax: ax[-1]
average = lambda ax: sum(ax) / len(ax)
average_int = lambda ax: round(average(ax))
std_dev_sample = lambda ax: (sum([(x - (sum(ax) / len(ax))) ** 2 for x in ax]) / (len(ax) - 1)) ** 0.5 if len(ax) > 1 else 0
average_with_sd = lambda ax: [average(ax), round(std_dev_sample(ax) / average(ax) * 100, 1)]
average_with_sd_str = lambda ax: f"{average(ax)}, {round(std_dev_sample(ax)/average(ax)*100,1)}%"
average_int_with_sd_str = lambda ax: f"{average_int(ax)}, {round(std_dev_sample(ax)/average(ax)*100,1)}%"
full_data_analysis = lambda ax: f"{average_int_with_sd_str(ax)},{len(ax)}"


def datetime_serializer(obj):
    if isinstance(obj, datetime) or isinstance(obj, date):
        return obj.isoformat()
    raise TypeError(f"Object of type {obj.__class__.__name__} is not JSON serializable")


# 'y' value projectors
select_conditioning_duration = lambda item: item["conditioning_duration"] / item["sender_count"]


# group selectors
select_window = lambda item: item["RATEWINDOW"]


def bar_chart_prep(gxx):
    # Assumes that the kernel (x,y) types are both arrays length=1
    # Will maybe fail when values other than float are present, e.g. error bars, SD etc. etc.
    return {group_key: {subgroup_key: y[0] for subgroup_key, (_, y) in subgroup.items()} for group_key, subgroup in gxx.items()}


class View:

    def __init__(self, opt):

        # defaults
        self.plot_text = {"title": "continuous rate test", "x_axis": "number of BGP peers", "y_axis": "update messages / second", "group_title": "cycle duration", "subgroup_title": "target"}
        self.select_x = select_sender_count
        self.select_subgroup = select_target
        self.select_group = select_null
        self.y_selector = select_multi_rate
        self.plan = average
        self.filepath = "tmp.json"
        self.no_graphic = False
        self.plot_style = "groups"

        # overrides
        match opt:
            case "default":
                pass
            case "bar":
                self.plot_style = "bar"
                self.plot_text["y_axis"] = "y axis label missing"
                self.y_selector = select_mean
                self.select_x = lambda _: 0
                self.select_subgroup = int_item_selector("PREFIXCOUNT")
                self.select_group = select_target
                self.plan = average

            case "cd" | "conditioning_duration":
                self.y_selector = select_conditioning_duration
                self.plot_text["y_axis"] = "mean conditioning duration (secs.)"
            case "cpu" | "ncpus":
                self.select_group = select_ncpus
                self.plot_text["group_title"] = "# cpus"
                self.plan = average
            case "rtl":
                self.select_group = select_ratetime
                self.plot_text["yfloat"] = True
            case "w" | "window":
                self.select_x = select_window
                self.plot_text["x_axis"] = "rate window size"
                # self.plot_text["logscalex"] = True
                self.plan = max
                filters += [lambda item: item["RATEWINDOW"] < 11]
            case "m" | "max":
                self.plan = max
            case "min":
                self.plan = min
            case "p" | "power":
                self.select_group = select_tags
                self.plot_text["group_title"] = ""

            case "" | "tag" | "tags":
                self.select_group = select_null

            case "ng":
                self.select_group = select_null
                self.select_x = select_target
                self.no_graphic = True

            case "t" | "table":
                self.select_x = select_target
                self.select_subgroup = select_packed
                self.no_graphic = True
                # plan=average_int_with_sd_str
                # plan=average_int
                self.plan = full_data_analysis
            case _:
                print(f"*** UNKNOWN option'{opt}'")

    def group_select(self, items):
        base = {}
        group_set = set()
        subgroup_set = set()
        x_set = set()
        for p in items:
            group = self.select_group(p)
            subgroup = self.select_subgroup(p)
            if subgroup is None:
                continue
            x = self.select_x(p)
            # TODO raise an exception log when x is None...
            if x is None:
                continue
            x_set.add(x)
            subgroup_set.add(subgroup)
            group_set.add(group)

            if group not in base:
                base[group] = {}

            if subgroup not in base[group]:
                base[group][subgroup] = {}

            if x not in base[group][subgroup]:
                base[group][subgroup][x] = [p]
            else:
                base[group][subgroup][x].append(p)

        missing_cells = set()
        duplicate_cells = set()

        # NB - the following procedure guarantees that the ordering of subgroups is constant over all groups
        # This guarantee is required to ensure that when plotting groups that the subgroup identities are maintained over the entire plot.
        # The essential property is that iterating over subgroup_set is consistent between groups.
        # Where a subgroup is not present in a specific group then an empty subgroup is inserted.
        # In future, where the subgroup or group is of known type then another consistent ordering, could be implemented.
        ordered_base = {}
        group_list = sorted(group_set, reverse=True)
        subgroup_list = sorted(subgroup_set)
        x_list = sorted(x_set)
        for group in group_list:
            ordered_base[group] = {}
            for subgroup in subgroup_list:
                ordered_base[group][subgroup] = {}
                if subgroup not in base[group]:
                    missing_cells.add(f"missing subgroup:{subgroup} in group:{group}")
                else:
                    for x in x_list:
                        if x not in base[group][subgroup]:
                            missing_cells.add(f"missing x in {group}:{subgroup}")
                            # print(f"missing x:{x} in {group}:{subgroup}")
                        else:
                            if len(base[group][subgroup][x]) > 1:
                                duplicate_cells.add(f"{len(base[group][subgroup][x])} repeated x in {group}:{subgroup}")
                            ordered_base[group][subgroup][x] = base[group][subgroup][x]

                            # print(f"duplicate x:{x} in {group}:{subgroup}")

        if len(missing_cells) == 0:
            print("***no missing cells!!!")
        else:
            print(f"*** missing cells from {missing_cells} !!!")

        if len(duplicate_cells) == 0:
            print("***no duplicate cells")
        else:
            print(f"*** duplicate cells in {duplicate_cells}")

        return ordered_base

    def project_y(self, base):
        # input - two-level grouped collection with underlying sorted (x,item) structure
        # output - same shaped two-level grouped collection with underlying sorted ([x],[y]) structure
        item_count = 0
        raw_item_count = 0

        newbase = {}
        for group, subgroups in base.items():
            newbase[group] = {}
            for subgroup, subgroup_vec in subgroups.items():
                vec_x = []
                vec_y = []
                for x, px in subgroup_vec.items():
                    item_count += 1
                    raw_item_count += len(px)
                    yx = list(map(self.y_selector, px))
                    # print(f"<<<{group}:{subgroup}:{x}:{yx}>>>")
                    vec_x.append(x)
                    vec_y.append(self.plan(yx))
                newbase[group][subgroup] = (vec_x, vec_y)
        print(f"*** {item_count} elements in plot (raw={raw_item_count}) (@project_y)")
        if raw_item_count < 2:
            print("can't plot less than two items")
            sys.exit(1)
        return newbase

    def do_it(self, summaries):

        group_data = self.group_select(summaries)
        projected_data = self.project_y(group_data)
        if self.no_graphic:
            dump_json(projected_data, self.filepath)
        else:
            match self.plot_style:
                case "bar":
                    path = barchart.plot(bar_chart_prep(projected_data), self.plot_text["title"], self.plot_text["y_axis"])
                case "groups":
                    path = graph2.plot_groups(projected_data, self.plot_text)
            return path
