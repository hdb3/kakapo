#!/usr/bin/env python3
import json
from sys import argv
from graph import graph
from summary import process_summary, report_summaries
from pymongo import MongoClient


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
    if len(argv) > 3:
        for arg in argv[3:]:
            match arg.split("="):
                case [a] | ["tags", a]:
                    tags = a.split(",")
                case ["targets", a]:
                    targets = a.split(",")
                case ["host", s]:
                    host = s
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
    report_summaries(summaries)
    graph(summaries, opt, tags, targets, host)


if __name__ == "__main__":
    main()
