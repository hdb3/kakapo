
echo "this will only be effective if you run with '.' or 'source'"

DIR=$( dirname "${BASH_SOURCE[0]}" )
VARS=$DIR/vars
for var in $(<${VARS})
  do
    echo "$var is ${!var}"
    export -n $var
    unset $var
  done
