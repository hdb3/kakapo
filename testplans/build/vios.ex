#!/usr/bin/expect
set vm [lindex $argv 0]
set sshconfig  [file join [file dirname [info script]] "ios/sshbase.cfg"]
set config [lindex $argv 1]
set data [read [open $config]]
set sshdata [read [open $sshconfig]]
# set 0 -> 1 for console logging of the process
log_user 0

proc ts {s} {
  set t [timestamp -format %X]
  send_user "\n$t $s "
}

spawn /usr/bin/virsh start $vm
expect "Domain $vm started"

ts "vm $vm started"

# connecting too quickly to the console causes errors which are hard to catch
sleep 10

spawn /usr/bin/virsh console $vm
ts "console started"

expect { "Escape character is ^]" { ts "console connected" } 
         -re ".*\n" { send_user . }
         timeout { send_user ~ ; exp_continue }
       }

ts "waiting for config dialog"

set timeout 120

expect {
        -re "Would you like to enter the initial configuration dialog" { ts "console active, waiting for command prompt"; sleep 1 ; send "no\r" }
         timeout { ts " timed out (120 seconds)\n" ; exit 1 }
       }

set timeout 5
send "\r"
expect {
         -re "Press RETURN to get started" { send "\r"; exp_continue }
         -re "Router>" {  ts "command prompt active" } 
         timeout {  send "\r" ; exp_continue }
       }

send "show privilege\r"
expect {
         -re ".*\r\nCurrent privilege level is 1\r\nRouter>" {  ts "priv level 1"} 
         timeout {  send "show privilege\r" ; exp_continue }
       }

send "enable\r"

send "show privilege\r"
expect {
         -re ".*\r\nCurrent privilege level is 15\r\n(\[\[:alnum:]\]*)#" {
              set rname $expect_out(1,string)
              if { $rname != "Router" } { send_user "fatal error - device name not Router" ; exit 1 }
              ; ts "enable OK"
             } 
         timeout {  send "show privilege\r" ; exp_continue }
       }

ts "initialisation complete , sending config $config"

send "\rconfig terminal\r"
send "\rterminal no monitor\r"

send "$data"
send "$sshdata"
send "\rend\r"
sleep 5
expect *
send "\r\rwrite memory\r\r"
ts "configuration complete , waiting for confirmation"

set timeout 20
expect {
         "\[OK\]" { ts "got OK"}
         -re "\n" { exp_continue }
         timeout { ts "timeout waiting for OK after save startup-config" ; exit 1 }
       }

ts "startup configuration saved, waiting for console prompt"

expect *

send "show privilege\r"
expect {
         -re ".*\r\nCurrent privilege level is 15\r\n(\[\[:alnum:]\]*)#" { set rname $expect_out(1,string) ; ts "hostname $rname"} 
         timeout {  send "show privilege\r" ; exp_continue }
       }

if { $rname == "Router" } { send_user "fatal error - device name not changed from Router" ; exit 1 }

ts "reload request"
send "\rreload\r"

expect { "[confirm]" { send "\r" } }

ts "reload request accepted"

sleep 2

send "\035"
expect eof

ts "console disconnected, stopping VM"

# delay needed before stopping VM to avoid configuration loss
sleep 5

spawn /usr/bin/virsh destroy $vm 

expect -re "Domain $vm destroyed"
ts "bootstrap complete for $vm using $config (config hostname was $rname)\n"
