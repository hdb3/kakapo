#!/usr/bin/env python3

from dt import string_to_datetime
from logtext import parse_logtext


def process_summary(item):

    logtext = parse_logtext("", item["UUID"], item["LOGTEXT"])
    item |= logtext
    time = string_to_datetime(item["time"])
    item["time"] = time
    del item["LOGTEXT"]
    return item


common_keys = ["type", "file_name", "LOGTEXT"]


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
        if len(vx) < len(sx) and len(vx) > 1:

            if len(vx) < 20:
                print(f"key: {k} [", end="")
                for v in vx:
                    print(f'"{v}",', end="")
                print(" ]")
            else:
                print(f"key: {k} (N={len(vx)})")
