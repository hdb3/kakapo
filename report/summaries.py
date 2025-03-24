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
from matplotlib.lines import Line2D
from matplotlib.legend import Legend


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


def select_ratetime(item):
    return int(item["RATETIMELIMIT"])


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
    plt.rcParams.update({"font.size": 18})
    plt.rcParams["savefig.directory"] = os.path.dirname(__file__)
    fig, ax = plt.subplots(figsize=(12, 8), layout="constrained")

    colours = iter(mcolors.TABLEAU_COLORS)
    styles = iter(linestyle_tuple)

    group_linestyle = {}
    group_legend_lines = []
    group_labels = []
    subgroup_legend_lines = []
    subgroup_labels = []
    subgroup_colour = {}
    for group, subgroups in gxx.items():
        style = (next(styles))[1]
        group_linestyle[group] = style
        group_legend_lines.append(Line2D([0], [0], linestyle=style))
        group_labels.append(group)

        for subgroup in subgroups:
            if subgroup not in subgroup_colour:
                colour = next(colours)
                subgroup_legend_lines.append(Line2D([0], [0], color=colour))
                subgroup_labels.append(subgroup)
                subgroup_colour[subgroup] = colour

    ax.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
    ax.set_title(plot_text["title"])
    ax.set_ylabel(plot_text["y_axis"])
    ax.set_xlabel(plot_text["x_axis"])

    for group, subgroups in gxx.items():
        for subgroup, (xs, ys) in subgroups.items():
            print(f"<<<{group}:{subgroup}:{subgroup_colour[subgroup]}>>>")
            ax.plot(xs, ys, label=subgroup, linestyle=group_linestyle[group], color=subgroup_colour[subgroup])

    ax.legend(
        subgroup_legend_lines,
        subgroup_labels,
        loc="upper left",
        title=plot_text["subgroup_title"],
        fontsize=14,
        framealpha=1,
    )
    ax.add_artist(
        Legend(
            ax,
            group_legend_lines,
            group_labels,
            title=plot_text["group_title"],
            loc="upper right",
            ncols=2,
            fontsize=14,
            framealpha=1,
        )
    )

    # fig.legend( subgroup_legend_lines, subgroup_labels, loc="upper right",title="subgroups" ,frameon=False,fontsize=14)
    # fig.add_artist(Legend(fig, group_legend_lines, group_labels, loc="upper left",title="groups" ,frameon=False,fontsize=14))

    fig.show()  # needed to force change in figure layout to accommodate legends
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
    tagged = lambda tag: lambda item: "TAG" in item and tag == item["TAG"]

    rtl = lambda item: int(item["RATETIMELIMIT"]) in [50, 100, 150, 200, 250]

    # filters = [recent, bad_targets, has_ncpus, tagged("TRIAL3")]
    filters = [recent, bad_targets, has_ncpus, rtl]

    plot_text = {"title": "continuous rate test", "x_axis": "number of BGP peers", "y_axis": "update messages / second", "group_title": "cycle duration", "subgroup_title": "target"}

    match opt:
        case "cd" | "conditioning_duration":
            y_selector = select_conditioning_duration
            plot_text["y_axis"] = "mean conditioning duration (secs.)"
        case _:
            y_selector = select_multi_rate
    group_data = group_select(summaries, filters=filters, select_group=select_ratetime)
    # group_data = make_plot(summaries, filters=filters + [n_cpus_filter(16)], select_y=y_selector)
    # group_data = make_plot(summaries, filters=filters.append(n_cpus_filter(2)), select_y=y_selector)

    plot_groups(project_y(group_data, plan=average), plot_text)


if __name__ == "__main__":
    main()
