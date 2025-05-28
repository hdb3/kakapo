#!/usr/bin/env python3
import yaml
from sys import argv
import sys
from splitter import fsplit

command = "runx.sh"


def die(s):
    print(s, file=sys.stderr)
    exit(1)


def say(s):
    print(s, file=sys.stderr)


def load_spec(fn):
    try:
        with open(fn, "r") as f:
            s = f.read()
    except FileNotFoundError as e:
        die(f"Error loading JSON/yaml data from {fn}: {e}")

    try:
        jdata = yaml.safe_load(s)
    except yaml.YAMLError as e:
        die(f"Error loading JSON/yaml data from {fn}: {e}")
    return jdata


def read_spec(spec):
    # print(f"{yaml.dump(spec)}")
    parameter_vector = []
    for k, v in spec["test"].items():
        values = fsplit(v)
        # print(f"{k}:{values}")
        parameter_vector.append((k, values))
    return parameter_vector


"""

for each level of parameter set an input string is mapped to a set of strings,
so that at every recurse the number of lines grows by N, and the string length is increased by one 'x=y'
The kernel operation takes a (label_name,label_values) tuple and a list of strings and produced a list of strings.
The wrapper/fold takes a list of [(label_name,label_values) ] and a list of strings and applies the kernel operation to the top/first (label_name,label_values) and calss itself with the result and the remainder of the list


"""


def kernel(label_name, label_values, clist):
    nclist = []
    for label_value in label_values:
        for c in clist:
            nclist.append(f"{label_name}={label_value} {c}")
    return nclist


def folder(pvx, clist):
    if len(pvx) == 0:
        return clist
    else:
        (ln, lvx), ax = pvx[0], pvx[1:]
        clist = kernel(ln, lvx, clist)
        return folder(ax, clist)


def gen_commandline_set(v_n_px):
    return folder(v_n_px, [command])


def main():

    if len(argv) == 2:
        fn = argv[1]
        spec = load_spec(fn)
    else:
        die("no file name given")

    if "spec" in spec and spec["spec"] == "forest":
        v = read_spec(spec)
        clist = gen_commandline_set(v)
        for c in clist:
            print(c)
    else:
        die("invalid file, no spec or invalid spec")


if __name__ == "__main__":
    main()
