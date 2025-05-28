import re

"""
# splitting rules

##  examples

  targets: bird1, bird2, bird3
  MODE: RATE
  NOPACK: 0,1
  N_PEERS: 1..10
  NX_PEERS: 1,5..40

## premise

The output in each case is a list of parameters (strings).
The shapes of term are:
- simple - no separators
- comma only list
- dot-dot only list
- comma,dot-dot list
Space and white space are not significant (meaning not changed if removed)
Only numerics can be used in dot-dot terms.
quotes are not expected,strings with white space will not be preserved.

"""

re_simple = re.compile(r"^\w+$")
re_commas = re.compile(r"^(\w+)(,\w+)+$")
re_dotdot = re.compile(r"^(\d+)\.\.(\d+)$")
re_commadotdot = re.compile(r"^(\d+),(\d+)\.\.(\d+)$")


def parse_simple(m):
    return [m[0]]


def parse_commas(m):
    validated_string = m.string
    words = re.findall(r"(\w+)", validated_string)
    return words


def parse_dotdot(m):
    m1 = int(m[1])
    m2 = int(m[2])
    int_list = list(range(m1, m2 + 1))
    str_list = list(map(str, int_list))
    return str_list


def parse_commadotdot(m):
    m1 = int(m[1])
    m2 = int(m[2])
    m3 = int(m[3])
    int_list = list(range(m1, m3 + 1, m2 - m1))
    str_list = list(map(str, int_list))
    return str_list


def fsplit(t):
    t = "".join(t.split())
    if m := re_simple.match(t):
        return parse_simple(m)
    elif m := re_commas.fullmatch(t):
        return parse_commas(m)
    elif m := re_dotdot.match(t):
        return parse_dotdot(m)
    elif m := re_commadotdot.match(t):
        return parse_commadotdot(m)
    else:
        raise Exception(f"could not parse {t}")
