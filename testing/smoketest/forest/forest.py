#!/usr/bin/env python3
import json
import yaml
from sys import argv
import sys
from splitter import fsplit


def load_spec(fn):
    try:
        with open(fn, "r") as f:
            s = f.read()
    except FileNotFoundError as e:
        print(f"Error loading JSON/yaml data from {fn}: {e}")

    try:
        jdata = yaml.safe_load(s)
    except yaml.YAMLError as e:
        print(f"Error loading JSON/yaml data from {fn}: {e}")
        exit(1)
    return jdata


def use_spec(spec):
    print(f"{yaml.dump(spec)}")
    for k, v in spec["test"].items():
        values = fsplit(v)
        print(f"{k}:{values}")


def main():

    if len(argv) == 2:
        fn = argv[1]
        spec = load_spec(fn)
    else:
        print("no file name given")
        sys.exit(1)

    if "spec" in spec and spec["spec"] == "forest":
        use_spec(spec)
    else:
        print("invalid file, no spec or invalid spec")
        sys.exit(1)


if __name__ == "__main__":
    main()
