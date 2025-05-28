#!/usr/bin/env python3
import json
import yaml
from sys import argv
import sys

sample = {}
sample["targets"] = "bird1, bird2, bird3"
sample["MODE"] = "RATE"
sample["NOPACK"] = "0,1"
sample["N_PEERS"] = "1..10"
sample["NX_PEERS"] = "1,5..40"


def load_json(fn):
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


def main():

    if len(argv) == 2:
        fn = argv[1]
        test = load_json(fn)
    else:
        print("no file name given")
        sys.exit(1)
        test = sample

    spec = {}
    spec["spec"] = "forest"
    spec["name"] = "sample"
    spec["test"] = test

    print(f"{spec}")
    print(f"{json.dumps(spec)}")
    print(f"{yaml.dump(spec)}")


if __name__ == "__main__":
    main()
