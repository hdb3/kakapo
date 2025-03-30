#!/usr/bin/env python3
import json
from sys import argv
from graph import graph
from summary import process_summary, report_summaries


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

    fn = argv[1]

    opt = ""
    if len(argv) > 2:
        opt = argv[2]

    tags = ""
    if len(argv) > 3:
        tags = argv[3].split(",")

    jdata = handle_json_file_variants(fn)
    summaries = process_json_list(jdata)
    report_summaries(summaries)
    graph(summaries, opt, tags)


if __name__ == "__main__":
    main()
