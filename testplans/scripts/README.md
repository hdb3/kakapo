# 'testplans/scripts' #

Scripts in this directory are integrations over various components from _testplans_.

In general these scripts are the user interface to the whole capability of testplans, albeit those which
utilise the tmux framework.  The other components of _testplans_ are standalone and could very likely be used in another context.
Also, some these scripts have other dependencies on my work, and a more logical hierarchy which includes e.g. the VR, VM and VN management capability in _testplans_
should also contain these dependencies.  The main such dependencies are the Docker VR build aspects or _kagu_, and the test agent capabilities of kakapo and even hbgp.
For more detail on all of this see <Work In Progress> in the top level directory.

## summary ##

These scripts are designed to enable various different target virtual routers to be stopped, started and configured, in similar configurations, and then test agents to be similarly instantiated and run, with appropriate network monitoring and capture, and in some cases with an interactive console to allow staged experiments to be interactively managed.
Most active elements have console interfaces, and the framework aims to provide simultaneously visible 'windows' onto as many components as is useful and practicable.
An important goal of the scripts is that they are self-contained and 'idempotent'.  By idempotent is meant that mutually incompatible test agents and target routers can be stopped and started in any order, but not force slow starting agents to reinitialise unless
strictly required; and that every test cycle can clean its execution environment before starting to ensure consistent and efficient experimentation and observation.

## operational principles ##

Every test cycle defines a single target system, a separate test system, and a suitable display and control framework.
The same test system can be applied to any compatible target system, and where possible the same 'flavour' target systems can be used with different test systems or test system variants.
So, a test run is initiated with a command which starts the generic framework ('run.sh'), and is provided with two parameters - the first defines the test target, the second defines the test system. 

## organisation ##

There is a single generic test runner - 'run.sh'.

Test targets include 'frr' 'bird2' 'gobgp' 'bgpd' 'hbgp' and 'exabgp', as well as variants of junos and ios.
There are scripts with corresponding names, such as frr.script

test systems are: kakapo and 

## .test script structure

Test scripts are bash scripts which rely on other scripts and environment variables to customise a specific test scenario.
A test script is run only after the virtual network environment and test target components have been configured and started.
The test script itself defines the test components that are required and the virtual screen layout in which the interactive components should run.
The complexity of building the screen are handled by a 'layout' script which creates a tmux windowed environment with logically named sub-windows.
The test script merely calls 'tmux respawn-pane' for each defined component. Configuration details of test targets and test network environments are not controlled by the test script itself.

## .target script structure

.target scripts complement .test scripts.
The .target script _prepares_ the execution of a target but does not actually start it.
This is to allow the test script to define the screen environment in which the target runs, and defer target start-up until the network and virtual execution environment is ready for use.
The main task of .target files is to define an environment variable '$target' which will be used by a .test script to start the target when it is needed, in a terminal window.
The detail of starting the target is not rigidly constrained and can be any single bash command, however for more complex targets the convention used is to provide a related '.run' file which contains the full bash command(s) needed.
This convention avoids problems with command escapes and substitutions which can otherwise over complicate the .target script.  This separated approach also allows the target configuration to be kept separate from the target specification.

# usage

$ ./run.sh $TEST $TARGET

where $TEST and $TARGET are as described and listed above.