#!/usr/bin/env python3
import os
from datetime import datetime
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from linestyle import linestyle_tuple
import matplotlib.colors as mcolors
from matplotlib.lines import Line2D
from matplotlib.legend import Legend


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
    reject_count = 0
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
        else:
            reject_count += 1

    print(f"*** rejected {reject_count}/{len(px)} !!!")

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
                yx = list(map(select_y, px))
                # print(f"<<<{group}:{subgroup}:{x}:{yx}>>>")
                vec_x.append(x)
                vec_y.append(plan(yx))
            newbase[group][subgroup] = (vec_x, vec_y)
    print(f"*** {item_count} elements in plot (raw={raw_item_count}) (@project_y)")
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
        loc="upper right",
        title=plot_text["subgroup_title"],
        fontsize=14,
        framealpha=1,
    )
    if len(gxx) > 1:
        ax.add_artist(
            Legend(
                ax,
                group_legend_lines,
                group_labels,
                title=plot_text["group_title"],
                loc="upper left",
                ncols=2,
                fontsize=14,
                framealpha=1,
            )
        )

    # fig.legend( subgroup_legend_lines, subgroup_labels, loc="upper right",title="subgroups" ,frameon=False,fontsize=14)
    # fig.add_artist(Legend(fig, group_legend_lines, group_labels, loc="upper left",title="groups" ,frameon=False,fontsize=14))
    if not ("yfloat" in plot_text and plot_text["yfloat"]):
        ax.set_ylim(bottom=0)
    ax.set_xlim(left=1)
    if "logscalex" in plot_text:
        ax.set_xscale("log")

    fig.show()  # needed to force change in figure layout to accommodate legends
    plt.show()


def graph(summaries, opt, tags, targets,host):

    # 'y' value projectors
    select_conditioning_duration = lambda item: item["conditioning_duration"] / item["sender_count"]

    # group selectors
    select_window = lambda item: item["WINDOW"]

    # define some useful filters

    recent = lambda item: item["time"] > datetime.fromisoformat("2025-03-11")
    exclude_targets = lambda targets: lambda item: item["target"] not in targets
    include_targets = lambda targets: lambda item: item["target"] in targets
    has_ncpus = lambda item: "DOCKER_NCPUS" in item
    tagged = lambda tag: lambda item: "TAG" in item and tag == item["TAG"]
    filter_on_tags = lambda item: tags == "" or ("TAG" in item and item["TAG"] in tags)
    rtl = lambda item: int(item["RATETIMELIMIT"]) in [50, 100, 150, 200, 250]
    with_tags = lambda tags: lambda item: "TAG" in item and item["TAG"] in tags
    default_target_filter = include_targets(targets) if targets else exclude_targets(["gobgpV2"])
    host_filter = lambda s: lambda item: item["host"]==s

    filters = [recent, default_target_filter, has_ncpus, filter_on_tags]
    if host:
        filters += host_filter(host)

    # defaults
    plot_text = {"title": "continuous rate test", "x_axis": "number of BGP peers", "y_axis": "update messages / second", "group_title": "cycle duration", "subgroup_title": "target"}
    select_x = select_sender_count
    select_subgroup = select_target
    select_group = select_ncpus
    plot_text["group_title"] = "# cpus"

    y_selector = select_multi_rate
    plan = average

    # overrides
    match opt:
        case "cd" | "conditioning_duration":
            y_selector = select_conditioning_duration
            plot_text["y_axis"] = "mean conditioning duration (secs.)"
        case "cpu" | "ncpus":
            select_group = select_ncpus
            filters = [recent, include_targets(["bird2", "hbgp", "gobgp"]), has_ncpus]
            plot_text["group_title"] = "# cpus"
            plan = average
        case "rtl":
            filters += [rtl]
            select_group = select_ratetime
            plot_text["yfloat"] = True
        case "w" | "window":
            select_x = select_window
            plot_text["x_axis"] = "rate window size"
            # plot_text["logscalex"] = True
            plan = max
            filters += [lambda item: item["WINDOW"] < 11]
        case "m" | "max":
            plan = max
        case "min":
            plan = min
        case "p" | "power":
            filters += [with_tags(["POWER_HIGH", "POWER_MEDIUM", "POWER_LOW", "POWER_MEDIUM_BATTERY", "POWER_SERVER"])]
            select_group = select_tags
            plot_text["group_title"] = ""

        case "" | "tags":
            pass
        case _:
            print(f"*** UNKNOWN option'{opt}'")

    group_data = group_select(summaries, filters=filters, select_x=select_x, select_subgroup=select_subgroup, select_group=select_group)
    plot_groups(project_y(group_data, select_y=y_selector, plan=plan), plot_text)
