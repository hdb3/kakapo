#!/usr/bin/env python3
from sys import argv
import json
from pymongo import MongoClient
from datetime import datetime
import views
import filters
import time
from pathlib import Path


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

found_LOGTEXT_error = False


def process_summary(item):
    global found_LOGTEXT_error
    global found_RATEWINDOW

    del item["_id"]

    if "LOGTEXT" not in item or item["LOGTEXT"] == "":
        if not found_LOGTEXT_error:
            found_LOGTEXT_error = True
            print(f"error: LOGTEXT missing or empty in: {item} (only reported once)")
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


common_keys = ["type", "file_name", "LOGTEXT", "SEQ", "multi_rate", "single_rate", "exit_status", "conditioning_duration", "mean", "max", "min", "sd", "time", "elapsed_time", "unixtime"]
marker_keys = ["TAG", "test_name", "target", "SPEC", "HOSTNAME"]


def report_summaries(sx):
    global found_RATEWINDOW

    earliest = int(time.time())
    latest = 0

    keys = {}
    for s in sx:
        item_time = s["unixtime"]
        latest = max(latest, item_time)
        earliest = min(earliest, item_time)
        for k, v in s.items():
            if not k in common_keys:
                if not k in keys:
                    keys[k] = {}
                if not v in keys[k]:
                    keys[k][v] = 0
                else:
                    keys[k][v] += 1
    dt_earliest = datetime.fromtimestamp(earliest)
    dt_latest = datetime.fromtimestamp(latest)
    report = []
    report.append(f"summarising {len(sx)} items")
    report.append(f"sample data time window is {dt_earliest.date()} - {dt_latest.date()}")
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
            # print(f"key: {k} [", ", ".join(display), "]")
            display_str = ", ".join(display)
            report.append(f"key: {k} [{display_str}]")

    # if found_RATEWINDOW:
    #     print("found and renamed RATEWINDOW to WINDOW")

    return "\n".join(report)


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

    command_line = " ".join(argv)

    if len(argv) > 1:
        fn = argv[1]
    else:
        fn = "mongo"

    opt = "default"
    if len(argv) < 2:
        opt = "dump"

    tags = []
    targets = []
    host = ""
    fn_out = ""
    save = False
    test = None
    if len(argv) > 2:
        for arg in argv[2:]:
            match arg:
                case "save":
                    save = True
                case _:
                    match arg.split("=", 1):
                        case [a] | ["tag", a] | ["tags", a]:
                            tags = a.split(",")
                        case ["target", a] | ["targets", a]:
                            targets = a.split(",")
                        case ["opt", opt]:
                            pass
                        case ["test", test]:
                            pass
                            # host = s
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
    print(report_summaries(summaries))
    if opt == "dump":
        with open("summaries.json", "w") as f:
            json.dump(summaries, f, default=str)
        exit(0)

    filter = filters.get_filters(opt, tags, targets, host, test)
    filters.debug_filter(summaries, filter)
    filtered_data = filters.main_filter(summaries, filter)
    filtered_summary = report_summaries(filtered_data)

    print()
    print("===================")
    print("Post Filter Summary")
    print("===================")
    print(filtered_summary)
    print("===================")

    # debug code
    with open("filtered_data.json", "w") as f:
        json.dump(filtered_data, f, default=str)

    view = views.View(opt)
    path = view.do_it(filtered_data)
    if path:
        print(f"graph was saved to {path}")
        print(f'command line was "{command_line}"')

    if save:
        save_dir = Path.home() / ".kakapo" / str(int(time.time()))
        print(f"command line was {command_line}")
        print(f"save dir is  {save_dir}")
        save_dir.mkdir(parents=True)

        with open(save_dir / "command_line", "w") as f:
            f.write(command_line)
            f.write("\n")

        with open(save_dir / "filtered_summary", "w") as f:
            f.write(filtered_summary)
            f.write("\n")

        with open(save_dir / "summaries.json", "w") as f:
            json.dump(filtered_data, f, default=str)

        # last because it may fail, allowing user to fix up after successfully writing the other material
        figure = Path(path)
        figure.rename(save_dir / "figure.png")


if __name__ == "__main__":
    main()
