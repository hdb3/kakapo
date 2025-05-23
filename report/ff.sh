#!/bin/bash -e
# export db="kakapo" collection="raw" loc="$HOME"
db="kakapo" collection="raw"

import() {
    if grep -q '^ \[$' $1; then
        echo "simple processing $1"
        mongoimport --db $db --collection $collection --jsonArray --file $1
    else
        echo "make json array and process $1"
        sed -e '1 i \ [' -e '$ s/,$/\]/' $1 >/tmp/kakapo.json
        mongoimport --db $db --collection $collection --jsonArray --file /tmp/kakapo.json
    fi
}

# for f in $(find $loc -name kakapo.json -not -empty); do
#     if grep -q '^ \[$' $f; then
#         echo "simple processing $f"
#         mongoimport --db $db --collection $collection --jsonArray --file $f
#     else
#         echo "indirect for $f"
#         sed -e '1 i \ [' -e '$ s/,$/\]/' $f >/tmp/kakapo.json
#         mongoimport --db $db --collection $collection --jsonArray --file /tmp/kakapo.json
#     fi
# done

if [[ -z "$1" ]]; then
    for f in $(find $loc -name kakapo.json -not -empty); do
        import $f
    done
else
    for f in $(find $1 -name \*.json -not -empty); do
        import $f
    done
fi
