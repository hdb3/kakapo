def parse_logtext(fn, uuid, s):
    rval = {}
    terms = s.split()
    rval["target"] = terms[0]
    for term in terms[1:]:
        units = term.split("=")
        if len(units) != 2:
            print(f"invalid term in log text:[{term}] ({fn}:{uuid})")
        else:
            rval[units[0]] = units[1]
    return rval
