#!/bin/bash -e
# export db="kakapo" collection="raw" loc="$HOME"
db="kakapo" collection="raw"

import() {
    if grep -q '^ \[$' $1; then
        echo "simple processing $1"
        mongoimport --db $db --collection $collection --jsonArray --file $1
    else
        echo "make json array and process $1"
	tmpfile=$(mktemp --suffix=".json")
        sed -e '1 i \ [' -e '$ s/,$/\]/' $1 >$tmpfile
	sed -i 's/,},/},/g' $tmpfile
        if ! mongoimport --db $db --collection $collection --jsonArray --file $tmpfile ; then
            echo "failed to import from $tmpfile (orginal source $1)"
        fi
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
        if ! import $f ; then
		echo "failed to import $f"
	fi
    done
elif [[ -d "$1" ]]; then
    for f in $(find $1 -name \*.json -not -empty); do
        import $f
    done
elif [[ -f "$1" ]]; then
        import $1
else
    echo "woe is me"
fi
