#!/bin/bash -e
db="kakapo" collection="raw"
tmpfile=$(mktemp --suffix=".json")
mongoexport --db $db --collection $collection --jsonArray | jq 'map(del(._id))' > $tmpfile
echo "exported $db:$collection to $tmpfile"
