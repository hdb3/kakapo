#!/usr/bin/env python3

from dt import string_to_datetime
from logtext import parse_logtext

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
