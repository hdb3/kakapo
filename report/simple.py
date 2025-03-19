import json
from sys import argv

sections = {}


def process_record(jitem):
    if not "UUID" in jitem:
        print(f"got improper item, has no UUID")
        return
    uuid = jitem["UUID"]
    jtype = jitem["type"]
    match jtype:
        case "start":
            if uuid in sections:
                print(f"duplicate start for {uuid}")
                return
            else:
                sections[uuid]={}
                sections[uuid]["start"] = jitem

        case "summary":
            if not uuid in sections:
                print(f"summary: missing start for {uuid}")
                return
            else:
                if "exit" in sections[uuid]:
                    print(f"summary: exit also found for {uuid}")
                sections[uuid]["summary"] = jitem

        case "exit":
            if not uuid in sections:
                print(f"exit: missing start for {uuid}")
                return
            else:
                if "summary" in sections[uuid]:
                    print(f"exit: summary also found for {uuid}")
                sections[uuid]["exit"] = jitem

        case "rate_intermediate":
            if not uuid in sections:
                print(f"rate_intermediate: missing start for {uuid}")
                return
            else:
                if not "rate_intermediate" in sections[uuid]:
                    sections[uuid]["rate_intermediate"] = []
                sections[uuid]["rate_intermediate"].append(jitem)
        case _:
            print(f"unexpected item ({jtype}) for {uuid}")


def display_json(jdata):
    types = {}
    for item in jdata:
        if isinstance(item, dict):
            if "type" in item:
                item_type = item["type"]
                if item_type in types:
                    types[item_type] += 1
                else:
                    types[item_type] = 1
                process_record(item)
            else:
                print(f"got improper item, has no type")
        else:
            print(f"got improper item, is not dict (object)")
    for k, v in types.items():
        print(f"type: {k} count {v}")


def main():

    for fn in argv[1:]:
        try:
            with open(fn, "r") as f:
                jdata = json.load(f)
                print(f"from file {fn} got list with {len(jdata)} items")
                display_json(jdata)
        except (FileNotFoundError, json.JSONDecodeError) as e:
            print(f"Error loading JSON data from {fn}: {e}")


if __name__ == "__main__":
    main()
