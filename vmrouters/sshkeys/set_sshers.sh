SCRIPTDIR="$(dirname ${BASH_SOURCE[0]})"
alias iossh="ssh -l cisco -i $SCRIPTDIR/vm.key -oKexAlgorithms=diffie-hellman-group14-sha1 -oHostKeyAlgorithms=ssh-rsa -oPubkeyAcceptedKeyTypes=ssh-rsa "
echo "iossh alias set"
alias jssh="ssh -l nic -i $SCRIPTDIR/junos.rsa"
echo "jssh alias set"
