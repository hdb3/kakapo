import json
import os
from sys import argv
from datetime import datetime
from dt import string_to_datetime
from logtext import parse_logtext
import matplotlib.pyplot as plt
import matplotlib as mpl
import matplotlib.ticker as ticker
from linestyle import linestyle_tuple
import matplotlib.colors as mcolors


def process_summary(item):

    logtext = parse_logtext("", item["UUID"], item["LOGTEXT"])
    item |= logtext
    time = string_to_datetime(item["time"])
    item["time"] = time
    del item["LOGTEXT"]
    return item


def process_json_list(jdata):
    rval = []
    ignore_count = 0
    count = 0
    for item in jdata:
        if not isinstance(item, dict):
            print(f"in got improper item, is not dict (object)")
        elif not "type" in item:
            print(f"in got improper item, has no type")
        elif item["type"] == "summary":
            rval.append(process_summary(item))
        else:
            ignore_count += 1
        count += 1
    return rval


common_keys = ["type", "file_name", "LOGTEXT"]


def report_summaries(sx):
    keys = {}
    print(f"got {len(sx)} items")
    for s in sx:
        uuid = s["UUID"]
        for k, v in s.items():
            if not k in common_keys:
                if not k in keys:
                    keys[k] = {}
                if not v in keys[k]:
                    keys[k][v] = 0
                else:
                    keys[k][v] += 1

    for k, vx in keys.items():
        print(f"key: {k} len(vx): {len(vx)}", end="")

        if len(vx) < 10:
            print(" [", end="")
            for v in vx:
                print(f'"{v}",', end="")
            print(" ]")
        else:
            print()


def select_target(item):
    return item["target"]


def select_ncpus(item):
    return int(item["DOCKER_NCPUS"])


def select_multi_rate(item):
    return int(item["multi_rate"])


def select_sender_count(item):
    return int(item["sender_count"])


# filters are true to allow / accept


def prefilter(item):
    return item["type"] == "summary" and "DOCKER_NCPUS" in item


def group_select(px, filters=[], select_x=select_sender_count, select_subgroup=select_target, select_group=select_ncpus):

    base = {}
    group_set = set()
    subgroup_set = set()
    x_set = set()
    for p in px:
        cond = True
        for fp in filters:
            try:
                cond = cond and fp(p)
            except KeyError:
                print(f"KeyError in {p}")
        if cond:
            for fp in filters:
                if fp(p):
                    continue
            group = select_group(p)
            subgroup = select_subgroup(p)
            x = select_x(p)
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
                # base[group][subgroup] = {}
            else:
                for x in x_list:
                    if x not in base[group][subgroup]:
                        missing_cells.add(f"missing x in {group}:{subgroup}")
                        # print(f"missing x:{x} in {group}:{subgroup}")
                    else:
                        if len(base[group][subgroup][x]) > 1:
                            duplicate_cells.add(f"duplicate x in {group}:{subgroup}")
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


def sort_on_x(base):
    # input - two-level grouped collection with underlying xs[x]=item structure
    # output - same high level shaped collection with underlying sorted (x,item) structure

    tuple_sort = lambda xs: sorted((xs.items()), key=lambda x: x[0])

    newbase = {}
    for group, subgroups in base.items():
        newbase[group] = {}
        for subgroup, xs in subgroups.items():
            newbase[group][subgroup] = tuple_sort(xs)
    return newbase


tail = lambda ax: ax[-1]
average = lambda ax: sum(ax) / len(ax)


def project_y(base, select_y=select_multi_rate, plan=tail):
    # input - two-level grouped collection with underlying sorted (x,item) structure
    # output - same shaped two-level grouped collection with underlying sorted ([x],[y]) structure

    newbase = {}
    for group, subgroups in base.items():
        newbase[group] = {}
        for subgroup, subgroup_vec in subgroups.items():
            vec_x = []
            vec_y = []
            for x, px in subgroup_vec.items():
                yx = list(map(select_y, px))
                # print(f"<<<{group}:{subgroup}:{x}:{yx}>>>")
                vec_x.append(x)
                vec_y.append(plan(yx))
            newbase[group][subgroup] = (vec_x, vec_y)
    return newbase


def plot_groups(gxx, plot_text):
    styles = iter(linestyle_tuple)
    next_style = lambda: (next(styles))[1]
    plt.rcParams.update({"font.size": 22})
    plt.rcParams["savefig.directory"] = os.path.dirname(__file__)
    # new_colours = lambda : iter(mpl.color_sequences["tab10"])
    new_colours = lambda: iter(
        mcolors.TABLEAU_COLORS,
    )

    _, ax = plt.subplots(figsize=(12, 8), layout="constrained")
    first_pass = True
    for group, subgroups in gxx.items():
        linestyle = next_style()
        colours = new_colours()
        for subgroup, (xs, ys) in subgroups.items():
            color = next(colours)
            print(f"<<<{group}:{subgroup}:{color}>>>")
            ax.plot(xs, ys, label=subgroup, linestyle=linestyle, color=color)

        if first_pass:
            first_pass = False
            ax.legend()
            ax.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
            ax.set_title(plot_text["title"])
            ax.set_ylabel(plot_text["y_axis"])
            ax.set_xlabel(plot_text["x_axis"])

    plt.show()


def main():

    fn = argv[1]
    opt = ""
    if len(argv) > 2:
        opt = argv[2]

    try:
        with open(fn, "r") as f:
            jdata = json.load(f)
            if isinstance(jdata, list):
                summaries = process_json_list(jdata)
                print(f"got {len(summaries)} summaries")
            else:
                print(f"Error JSON was not list in file {fn}")
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Error loading JSON data from {fn}: {e}")

    report_summaries(summaries)

    select_conditioning_duration = lambda item: item["conditioning_duration"] / item["sender_count"]

    recent = lambda item: item["time"] > datetime.fromisoformat("2025-03-11")
    bad_targets = lambda item: item["target"] not in ["gobgpV2"]
    has_ncpus = lambda item: "DOCKER_NCPUS" in item

    filters = [recent, bad_targets, has_ncpus]

    plot_text = {"title": "continuous rate test", "x_axis": "number of BGP peers", "y_axis": "update messages / second"}

    match opt:
        case "cd" | "conditioning_duration":
            y_selector = select_conditioning_duration
            plot_text["y_axis"] = "mean conditioning duration (secs.)"
        case _:
            y_selector = select_multi_rate
    group_data = group_select(summaries, filters=filters)
    # group_data = make_plot(summaries, filters=filters + [n_cpus_filter(16)], select_y=y_selector)
    # group_data = make_plot(summaries, filters=filters.append(n_cpus_filter(2)), select_y=y_selector)

    plot_groups(project_y(group_data, plan=max), plot_text)


if __name__ == "__main__":
    main()
