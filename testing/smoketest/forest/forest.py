#!/usr/bin/env python3
import yaml
from sys import argv
import sys
from splitter import fsplit
import tempfile
import os


from pathlib import Path


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


def gen_commandline_set(v_n_px, name):
    pd = Path(__file__).resolve().parent.parent
    command = f"{pd}/runx.sh"  # assume that the script is in the parent dir and called runx.sh!
    clist = folder(v_n_px, [command])
    seq = 0
    newclist = []
    for c in clist:
        newclist.append(f"SEQ={seq} SPEC={name} {c}")
        seq += 1
    return newclist


def main():

    if len(argv) == 2:
        fn = argv[1]
        spec = load_spec(fn)
    else:
        die("no file name given")

    if "spec" not in spec or spec["spec"] != "forest":
        die("invalid file, no spec or invalid spec")

    if "name" in spec:
        name = spec["name"]
    else:
        name = Path(fn).stem

    v = read_spec(spec)
    clist = gen_commandline_set(v, name)
    # for c in clist:
    #     print(c)

    fd, file_name = tempfile.mkstemp(suffix=".sh", prefix="smoktest_")

    with os.fdopen(fd, "w") as tmp_file:
        for c in clist:
            print(c, file=tmp_file)

    print(f"wrote script to: {file_name}")


if __name__ == "__main__":
    main()
