#!/usr/bin/env python3
import os
import sys
from sys import argv
import json
from pymongo import MongoClient
from datetime import datetime, date
import matplotlib.colors as mcolors
from matplotlib.lines import Line2D
from matplotlib.legend import Legend
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import inspect
import views

"""
 from linestyle.py
"""

linestyle_tuple = [
    ("solid", "solid"),
    ("densely dashdotted", (0, (3, 1, 1, 1))),
    ("dashed", (0, (5, 5))),
    ("densely dotted", (0, (1, 1))),
    # ("loosely dotted", (0, (1, 10))),
    ("dotted", (0, (1, 5))),
    ("long dash with offset", (5, (10, 3))),
    ("loosely dashed", (0, (5, 10))),
    ("densely dashed", (0, (5, 1))),
    ("loosely dashdotted", (0, (3, 10, 1, 10))),
    ("dashdotted", (0, (3, 5, 1, 5))),
    ("dashdotdotted", (0, (3, 5, 1, 5, 1, 5))),
    ("loosely dashdotdotted", (0, (3, 10, 1, 10, 1, 10))),
    ("densely dashdotdotted", (0, (3, 1, 1, 1, 1, 1))),
]


"""
 from dt.py
"""


def string_to_datetime(date_string):
    try:
        # Attempt to parse with microseconds (up to 6 digits)
        return datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S.%f")
    except ValueError:

        try:
            # Handle up to 9 digits of fractional seconds (nanoseconds)
            # by truncating to 6 digits (microseconds) if necessary
            parts = date_string.split(".")
            if len(parts) == 2:
                date_part = parts[0]
                frac_part = parts[1]
                frac_part = frac_part[:6].ljust(6, "0")
                new_date_string = f"{date_part}.{frac_part}"

                return datetime.strptime(new_date_string, "%Y-%m-%d %H:%M:%S.%f")
            else:
                return datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

        except ValueError:
            return None


"""
 from logtext.py
"""


def parse_logtext(fn, uuid, s):
    rval = {}
    terms = s.split()
    rval["target"] = terms[0]
    for term in terms[1:]:
        units = term.split("=")
        if len(units) != 2:
            print(f"invalid term in log text:[{term}] ({fn}:{uuid})")
        else:
            rval[units[0]] = units[1]
    return rval


"""
 from summary.py
"""

found_RATEWINDOW = False


def process_summary(item):

    if "LOGTEXT" not in item or item["LOGTEXT"] == "":
        print(f"bad item: {item}")
        return None
    logtext = parse_logtext("", item["UUID"], item["LOGTEXT"])
    item |= logtext
    time = string_to_datetime(item["time"])
    item["time"] = time
    del item["LOGTEXT"]
    if "RATEWINDOW" in item:
        item["WINDOW"] = item["RATEWINDOW"]
        del item["RATEWINDOW"]
        found_RATEWINDOW = True

    return item


common_keys = ["type", "file_name", "LOGTEXT", "multi_rate", "single_rate", "exit_status"]
marker_keys = ["TAG", "test_name", "target", "SPEC"]


def report_summaries(sx):
    keys = {}
    print(f"got {len(sx)} items")
    for s in sx:
        for k, v in s.items():
            if not k in common_keys:
                if not k in keys:
                    keys[k] = {}
                if not v in keys[k]:
                    keys[k][v] = 0
                else:
                    keys[k][v] += 1

    for k, vx in keys.items():

        # test 'len(vx) < len(sx)' excludes attributes like time and uuid, which are different, for EVERY item
        # test 'len(vx) > 1' skips attributes which are constant accros a dataset (unless they are always 'interesting', and thus in marker_keys)
        if k in marker_keys or (len(vx) < len(sx) and len(vx) > 1):

            # how many discrete values, and for each discrete value, how many instances...?
            # show the top 10, summarise the rest....
            vx_tuples = sorted(vx.items(), key=lambda item: item[1], reverse=True)
            displayed_items = vx_tuples[:10]
            remaining_items = vx_tuples[10:]
            display = []

            for v, count in displayed_items:
                display.append(f"{v}({count})")

            if remaining_items:
                remaining_count = sum(count for _, count in remaining_items)
                display.append(f"{{{len(remaining_items)} more({remaining_count})}}")
            print(f"key: {k} [", ", ".join(display), "]")

    if found_RATEWINDOW:
        print("found and renamed RATEWINDOW to WINDOW")


def debug_filter(px, filters):

    print("Debug filter")

    for fp in filters:
        filter_name = fp.__name__
        if filter_name == "<lambda>":
            filter_source = inspect.getsource(fp).strip()
            filter_name = filter_source.split()[0]
        reject_count = 0
        accept_count = 0
        except_count = 0
        for p in px:
            try:
                if fp(p):
                    accept_count += 1
                else:
                    reject_count += 1
            except KeyError:
                print(f"KeyError in {p}")
                except_count += 1
        print(f"filter {filter_name} reject_count={reject_count} accept_count={accept_count} except_count={except_count}")

    print("End - Debug filter")


def main_filter(px, filters):

    filter_map = {}
    filter_rejections = {}

    for fp in filters:
        filter_name = fp.__name__
        if filter_name == "<lambda>":
            filter_source = inspect.getsource(fp).strip()
            filter_name = filter_source.split()[0]
        filter_map[filter_name] = fp
        filter_rejections[filter_name] = 0

    filter_output = []
    total_count = len(px)
    for p in px:
        for fn, fp in filter_map.items():
            try:
                if fp(p):
                    continue
                else:
                    filter_rejections[fn] += 1
                    break
            except KeyError:
                print(f"KeyError in {p}")
                filter_rejections[fn] += 1
                break
        else:
            filter_output.append(p)

    accept_count = len(filter_output)
    reject_count = total_count - accept_count
    print(f"*** rejected {reject_count}/{total_count}!!!")
    # # debug level filter analysis
    # for fn, count in filter_rejections.items():
    #     print(f"filter {fn}:{count} ({inspect.getsource(filter_map[fn]).strip()})")
    return filter_output



def process_json_list(jdata):
    rval = []
    ignore_count = 0
    error_count = 0
    count = 0
    for item in jdata:
        if not isinstance(item, dict):
            print(f"in got improper item, is not dict (object)")
            error_count += 1
        elif not "type" in item:
            print(f"in got improper item, has no type")
            error_count += 1
        elif item["type"] == "summary":
            summary_item = process_summary(item)
            if summary_item:
                rval.append(summary_item)
            else:
                error_count += 1
        else:
            ignore_count += 1
        count += 1
    if error_count:
        print(f"process_json_list - {len(rval)} summaries returned, {count} items read, {ignore_count} non-summary, {error_count} errors")
    return rval


def process_mongo_list(mdata):
    rval = []
    ignore_count = 0
    error_count = 0
    count = 0
    for item in mdata:
        if not isinstance(item, dict):
            print(f"in got improper item, is not dict (object)")
            error_count += 1
        elif not "type" in item:
            print(f"in got improper item, has no type")
            error_count += 1
        elif item["type"] == "summary":
            summary_item = process_summary(item)
            if summary_item:
                rval.append(summary_item)
            else:
                error_count += 1
        else:
            ignore_count += 1
        count += 1
    if error_count:
        print(f"process_mongo_list - {len(rval)} summaries returned, {count} items read, {ignore_count} non-summary, {error_count} errors")
    return rval


def handle_json_file_variants(fn):
    try:
        with open(fn, "r") as f:
            s = f.read()
    except FileNotFoundError as e:
        print(f"Error loading JSON data from {fn}: {e}")

    try:
        jdata = json.loads(s)
    except json.JSONDecodeError as e:
        try:
            jdata = json.loads("[" + s[:-2] + "]")
        except json.JSONDecodeError:
            print(f"Error loading JSON data from {fn}: {e}")
            exit(1)

    if isinstance(jdata, list):
        return jdata
    else:
        print(f"Error JSON was not list in file {fn}")
        exit(1)


def main():

    if len(argv) > 1:
        fn = argv[1]
    else:
        fn = "mongo"

    opt = ""
    if len(argv) > 2:
        opt = argv[2]

    tags = []
    targets = []
    host = ""
    fn_out = ""
    if len(argv) > 3:
        for arg in argv[3:]:
            match arg.split("="):
                case [a] | ["tag", a] | ["tags", a]:
                    tags = a.split(",")
                case ["targets", a]:
                    targets = a.split(",")
                case ["host", s]:
                    host = s
                case ["file", fn_out]:
                    pass
                case _:
                    print(f"'{arg}' not expected")

    if fn == "mongo":
        client = MongoClient()
        db = client["kakapo"]
        collection = db["raw"]
        mdata = collection.find({"type": "summary"})
        summaries = process_mongo_list(mdata)
    else:
        jdata = handle_json_file_variants(fn)
        summaries = process_json_list(jdata)

    # 'summaries' is now a curated list of dict objects representing single data points
    report_summaries(summaries)
    if opt == "dump":
        with open("summaries.json", "w") as f:
            json.dump(summaries, f, default=str)
        exit(0)

    filters = views.get_filters(opt, tags, targets, host)
    debug_filter(summaries, filters)
    filtered_data = main_filter(summaries, filters)
    view = views.View(opt)
    view.do_it(filtered_data)


if __name__ == "__main__":
    main()
