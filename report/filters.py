from datetime import datetime
import inspect


def get_filters(opt, tags, targets, host, test):

    recent = lambda item: item["time"] > datetime.fromisoformat("2025-03-11")
    exclude_targets = lambda targets: lambda item: item["target"] not in targets
    include_targets = lambda targets: lambda item: item["target"] in targets
    filter_on_tags = lambda item: len(tags) == 0 or ("TAG" in item and item["TAG"] in tags)
    with_tags = lambda tags: lambda item: "TAG" in item and item["TAG"] in tags
    default_target_filter = include_targets(targets) if targets else exclude_targets(["gobgpV2"])
    host_filter = lambda s: lambda item: item["HOSTNAME"] == s
    test_filter = lambda s: lambda item: item["test_name"] == s

    filters = [recent, default_target_filter, filter_on_tags]
    if test:
        filters.append(test_filter(test))
    if host:
        filters.append(host_filter(host))

    match opt:
        case "cpu" | "ncpus":
            has_ncpus = lambda item: "DOCKER_NCPUS" in item
            filters = [recent, include_targets(["bird2", "hbgp", "gobgp"]), has_ncpus]
        case "rtl":
            rtl = lambda item: int(item["RATETIMELIMIT"]) in [50, 100, 150, 200, 250]
            filters += [rtl]
        case "w" | "window":
            filters += [lambda item: item["RATEWINDOW"] < 11]
        case "p" | "power":
            filters += [with_tags(["POWER_HIGH", "POWER_MEDIUM", "POWER_LOW", "POWER_MEDIUM_BATTERY", "POWER_SERVER"])]
        case _:
            pass

    return filters


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
            except KeyError as e:
                print(f"KeyError in {p}, missing key: {e.args[0]}")
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
            except KeyError as e:
                print(f"KeyError in {p}, missing key: {e.args[0]}")
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
