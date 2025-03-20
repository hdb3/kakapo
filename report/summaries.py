import json
import os
from sys import argv
from datetime import datetime
from dt import string_to_datetime
from logtext import parse_logtext
import matplotlib.pyplot as plt


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
    # print(f"process_json_list skipped {ignore_count}/{count}")
    return rval


# common_keys = ["type", "start_time", "end_time", "UUID", "file_name", "LOGTEXT"]
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
        print(f"key: {k} len(vx): {len(vx)}")


def group_select_target(item):
    return item["target"]


def select_multi_rate(item):
    return item["multi_rate"]


def select_sender_count(item):
    return item["sender_count"]


# filters are true to allow / accept


def prefilter(item):
    return item["type"] == "summary"


def filter_1(item):
    return item["target"] in ["bird2", "frr"]


def make_plot(px, filters=[], select_x=select_sender_count, select_y=select_multi_rate):

    # the top level data structure constructed is a set of groups, indexed by group name
    # Each group is a set of 'y' values indexed by 'x' value.
    # In the form here 'y' values are lists, to allow duplicates
    # A transformation might be required to produce plottable 'y' values,
    # e.g. single valued, or error bars, or for scatter plot.
    # A default transformation is defined later - tail: 'take the last value seen'.

    groups = {}
    xs = {}
    for p in px:
        cond = prefilter(p)
        for fp in filters:
            cond = cond and fp(p)
        if cond:
            for fp in filters:
                if fp(p):
                    continue
            group = group_select_target(p)
            y = select_y(p)
            x = select_x(p)

            if group not in groups:
                groups[group] = {}

            if x not in xs:
                xs[x] = {}

            if x not in groups[group]:
                groups[group][x] = [y]
            else:
                groups[group][x].append(y)

    # The x values were accumulated independently, to ensure a single complete list, for the following sanity check
    # Sanity check - verify, for every group, whether all possible 'x' values are represented.

    missing_cells = set()
    duplicate_cells = set()
    for gg, xx in groups.items():
        for x in xs:
            if x in xx:
                if len(xx[x]) > 1:
                    print(f"duplicate x:{x} in group:{gg}")
                    duplicate_cells.add(gg)
            else:
                print(f"missing x:{x} in group:{gg}")
                missing_cells.add(gg)

    if len(missing_cells) == 0:
        print("***can plot!!!")
    else:
        print(f"*** missing cells from {missing_cells}, cannot plot !!!")

    if len(duplicate_cells) == 0:
        print("***no duplicate cells")
    else:
        print(f"*** duplicate cells in {duplicate_cells}")

    # flatten the lists for simple plotting

    tail = lambda ax: ax[-1]

    for group in groups.values():
        for x, y_item in group.items():
            group[x] = tail(y_item)

    return groups


def unpack(x_y):
    # unpack a dictionary of form x:y into plottable lists
    xs = sorted(x_y.keys())
    ys = []

    for x in xs:
        ys.append(x_y[x])
    return xs, ys


def plot_groups(gx, plot_text):
    plt.rcParams.update({"font.size": 22})
    plt.rcParams["savefig.directory"] = os.path.dirname(__file__)

    fig, ax = plt.subplots(figsize=(12, 8), layout="constrained")

    for group, x_y in gx.items():
        xs, ys = unpack(x_y)
        ax.plot(xs, ys, label=group)
    ax.legend()
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

    # conditioning_duration
    select_conditioning_duration = lambda item: item["conditioning_duration"] / item["sender_count"]

    recent = lambda item: item["time"] > datetime.fromisoformat("2025-03-11")
    bad_targets = lambda item: item["target"] not in ["gobgpV2"]

    filters = [recent, bad_targets]

    plot_text = {"title": "continuous rate test", "x_axis": "number of BGP peers", "y_axis": "update messages / second"}

    match opt:
        case "cd" | "conditioning_duration":
            y_selector = select_conditioning_duration
            plot_text["y_axis"] = "mean conditioning duration (secs.)"
        case _:
            y_selector = select_multi_rate

    group_data = make_plot(summaries, filters=filters, select_y=y_selector)

    plot_groups(group_data, plot_text)


if __name__ == "__main__":
    main()
