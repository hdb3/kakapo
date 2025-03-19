import json
from sys import argv
from dt import string_to_datetime
from logtext import parse_logtext


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


def group_projector(item):
    return item["target"]


def y_value(item):
    return item["multi_rate"]


def x_value(item):
    return item["sender_count"]


def prefilter(item):
    return item["type"] == "summary"


def make_plot(px):
    groups = {}
    xs = {}
    for p in px:
        if prefilter(p):
            group = group_projector(p)
            y = y_value(p)
            x = x_value(p)

            if group not in groups:
                groups[group] = {}
            if x not in xs:
                xs[x] = {}

            if x not in groups[group]:
                groups[group][x] = 1
            else:
                groups[group][x] += 1

    for gg, xx in groups.items():
        for x in xs:
            if x not in xx:
                print(f"missing x:{x} in group:{gg}")
            elif xx[x] > 1:
                print(f"duplicate x:{x} in group:{gg}")


def main():

    fn = argv[1]
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

    make_plot(summaries)


if __name__ == "__main__":
    main()
