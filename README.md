# kakapo

_kakapo_ is a test suite for BGP performance.

The main component is kakapo-core, which prvides the executalbel binary 'kakapo'.
The simplest invocation of kakapo-core is of the form:

_kakapo 192.168.0.1 192.168.0.1_

Or, as the executable reports for usage:

USAGE: kakapo <IP address>[,<IP address>] <IP address>[,<IP address>] [<IP address>[,<IP address>]
       note the minimum number of peers is two, of which the first is a listener and all others are senders
       many options are controlled via environment variables like SLEEP, etc...


See the README in core/ for more information about kakapo core

## kakapo relay

_kakapo-relay_ is a benchmark reference 'BGP speaker' for kakapo.
It does nothing bar accept BGP sessions and relay update streams unchanged between peers.

Use it to calibrate the test environment and evaluate whether _kakapo-core_ or the hardware may be a material performance constraint.
