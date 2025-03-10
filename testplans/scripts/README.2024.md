# usage notes

scripts/README.md and ../README.md are also relevant

# overview

BGP functional and performance tests in varying scenarios are supported

The main function and theme is a test frame work which orchestrates multiple BGP speakers to demonstrate or test specific scenarios.  In more complex cases a small cluster of BGP speakers are linked to form an autonomous system, in which different nodes may have different roles, and different implementations.  For example, a cluster of commercial vendor routers, cisco, or juniper, are configured with further BGP peers acting as route injector or spceial role such as a 'controller'.  Typically, the controller is a novel BGP implementation, and the route injector could be any well known BGP implementation such as bird or frr, or, for performance experiments, a custom BGP speaker ('kakapo').

# Principle of operation
The framework is built using linux bash scripts, docker container manager, and uses tmux to provide multiple, simultaneously visible,  windows, one for each active component.

The principle entry point is the run.sh script.  Run requires two parameters, one specifies the specific BGP vendor/project implementation to deploy in the 'Device(s) Under Test' role.  The other parameter specifies what tests are to be executed.

The vendor/project parameter is self explanatory - options include cisco, juniper, frr, bird and other projects, including of course the Haskell BGP implementation which is the topic of the work.

# Add-Path based Controller Test Scenarios
The main use of the framework is the evaluation of proposed novel solutions for mitigation of routing integrity assaults.

In this test group the devices under test are typically limited, due to the fact that most non-commercial 'project' BGP implementations are not feature rich enough to support the BGP capabilities needed, specifically, the 'Add Path' BGP extension.

To execute a full evaluation of an 'Add-Path based Controller, the following scripts are invoked:

---------------------------------

implmentationy stuff

file suffix conventions

.target
.run
.conf
.script
.test

.target
   bgpd bird2 erl exabgp frr gobgp hbgp junos relay rust vios

.run
   bgpd bird2 erl frr gobgp rust

.conf
   agent1 agent2 agent3 bgpd control2 frr frr-ris gobgp hbgp-kakapo slave

.script
    testplans/scripts/
        allsorts base bgpagent bgpping bgprelay-kakapo exabgp-kakapo ios-be-3 junos-be-3 kakapo-silent null slave3 slave
    testplans/
        addpath bestex classfull controller exa null

.test
   controller kakapo

common
------
common.sh
layouts.sh
local.sh
run.sh

other
-----
exabgp.sh
kakapo-bgprelay.sh
kakapo.sh
kakapo-test.sh
start.sh
test2.sh
test3.sh
 ---------------------

 script analysis

## addpath
use:
- simple-prefix-addpath/hbgp x control2.conf
- master/hbgp x [ agent1-3.conf, slave.conf ]
- test2.sh
## bestex
as: addpath, but
- controller/hbgp x control2.conf

## classfull
as: addpath, but
- controller/hbgp x control2.conf
- classfulPT/hbgp x [ agent1-3.conf, slave.conf ]
- test2.sh


controller
exa
null
