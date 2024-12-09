SCRIPTDIR="$(dirname ${BASH_SOURCE[0]})"
alias cissh="ssh -l cisco -i $SCRIPTDIR/vm.key -oKexAlgorithms=diffie-hellman-group14-sha1"
