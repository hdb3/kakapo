import json
from sys import argv
from dt import string_to_datetime

sections = {}
files = {}


def process_raw_item(fn, jitem):

    if not "time" in jitem:
        print(f"in {fn}:{uuid} got improper item, has no time")
        return
    else:
        time = string_to_datetime(jitem["time"])
        jitem["time"] = time

    if not "UUID" in jitem:
        print(f"in {fn} got improper item, has no UUID")
        return
    else:
        uuid = jitem["UUID"]

    if not fn in files:
        files[fn] = {}
        files[fn]["start"] = time
        files[fn]["end"] = time
        files[fn]["count"] = 0
    else:
        if time > files[fn]["end"]:
            files[fn]["end"] = time
        if time < files[fn]["start"]:
            files[fn]["start"] = time

    if not uuid in sections:
        files[fn]["count"] += 1
        sections[uuid] = {}
        sections[uuid]["filename"] = fn
        sections[uuid]["start_time"] = time
        sections[uuid]["end_time"] = time
    else:
        if time > sections[uuid]["end_time"]:
            sections[uuid]["end_time"] = time
        if time < sections[uuid]["start_time"]:
            sections[uuid]["start_time"] = time

    match jitem["type"]:
        case "start":
            sections[uuid]["start"] = jitem

        case "summary":
            if not "start" in sections[uuid]:
                print(f"in {fn} summary: missing start for {uuid}")
                return
            else:
                if "exit" in sections[uuid]:
                    print(f"in {fn} summary: exit also found for {uuid}")
                sections[uuid]["summary"] = jitem

        case "exit":
            if not "start" in sections[uuid]:
                print(f"in {fn} exit: missing start for {uuid}")
                return
            else:
                if "summary" in sections[uuid]:
                    print(f"in {fn} exit: summary also found for {uuid}")
                sections[uuid]["exit"] = jitem

        case "rate_intermediate":
            if not "start" in sections[uuid]:
                print(f"in {fn} rate_intermediate: missing start for {uuid}")
                return
            else:
                if not "rate_intermediate" in sections[uuid]:
                    sections[uuid]["rate_intermediate"] = []
                sections[uuid]["rate_intermediate"].append(jitem)
        case _:
            print(f"in {fn} unexpected item ({jitem['type']}) for {uuid}")


def process_raw_list(fn, jdata):
    for item in jdata:
        if not isinstance(item, dict):
            print(f"in {fn} got improper item, is not dict (object)")
        elif not "type" in item:
            print(f"in {fn} got improper item, has no type")
        else:
            process_raw_item(fn, item)


def report_files():
    for fn, fdata in files.items():
        print(fn.ljust(15), f"count {fdata['count']}  start {fdata['start']}  end {fdata['end']}")


def process_sections():
    count = 0
    file_data = {}
    for uuid, item in sections.items():
        fn = item["filename"]
        if not fn in file_data:
            file_data[fn] = {"ignore": 0, "use": 0, "exit": 0}
        if not "summary" in item:
            if "exit" in item:
                file_data[fn]["exit"] += 1
            else:
                file_data[fn]["ignore"] += 1
        else:
            file_data[fn]["use"] += 1
            count += 1
    print(f"got {count} summaries")
    for fn, counts in file_data.items():
        fdata = files[fn]
        print(fn.ljust(15), f"use {counts['use']:4}  ignore {counts['ignore']:4}  exit {counts['exit']:4} start {fdata['start']}  end {fdata['end']}")


def main():

    for fn in argv[1:]:
        try:
            with open(fn, "r") as f:
                jdata = json.load(f)
                if isinstance(jdata, list):
                    print(f"from file {fn} got list with {len(jdata)} items")
                    process_raw_list(fn, jdata)
                else:
                    print(f"Error JSON was not list in file {fn}")
        except (FileNotFoundError, json.JSONDecodeError) as e:
            print(f"Error loading JSON data from {fn}: {e}")

    # report_files()
    process_sections()


if __name__ == "__main__":
    main()
