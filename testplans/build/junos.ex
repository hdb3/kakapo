#!/usr/bin/expect
set vm [lindex $argv 0]

log_user 0
exp_internal 0

proc locate {s} {
  return [file join [file dirname [info script]] $s ]
}

proc ts {s} {
  set t [timestamp -format %X]
  send_user "\n$t $s "
}

proc sendfile {fn} {

  ts "sending $fn "
  set fd [open $fn r]
  set send_slow {1 .001}

  while {[gets $fd line] != -1} {
    send -s "$line\r"
    expect {
      "\[edit\]" { send_user "%" }
       timeout {  ts "configuration line failed: <$line>" ; exit 1}
    }
  }
#  send_user "\n"
}

proc unescape {s} {
  return [ regsub -all {\033..} "" $s ]
  # regsub -all {\033..} "" $s r
  # return $r
}

proc send_unescape {s} {
  regsub -all {\033..} $s "" r
  send_user $r
}

proc diag {s} {
  set hs [ binary encode hex $s] ; set us [ unescape $s ] ; send_unescape "\nbuffer: (<$us>,\[ $hs \]\n"
}

ts "starting vm $vm"

spawn "/usr/bin/virsh" "start" "$vm" "--console"

ts "spawned vm $vm"

set timeout 60

expect {
    "Domain jbr1 started\r\n"  {ts "started" ; exp_continue }
    "Connected to domain jbr1" {ts "connected" ; exp_continue }
    "Escape character is ^]"   {ts "console" ; exp_continue }
    "Consoles: serial port"    {ts "console(2)" ; exp_continue }
    "BIOS drive C: is disk0"   {ts "BIOS" ; exp_continue }
    "Booting from Junos volume" { ts "booting" ; exp_continue } 
    "/packages" { send_user "P" ; exp_continue } 
    "Mounting " { send_user "m" ; exp_continue } 
    "Loading " { send_user "l" ; exp_continue } 
    "RDM Embedded" { send_user " RDM " ; exp_internal 0 ; exp_continue } 

    "FreeBSD/amd64 (Amnesiac) (ttyu0)" { ts "FreeBSD prompt" ; exp_continue }
    "kernel trap 12 with interrupts disabled" {  ts "VM start fail - unsupported virtualisation environment\n" ; exit 1}
    "login: " { ts "login prompt"}
    timeout { ts "timeout" }
    eof { ts "eof" }
}

send "root\r"
expect {
         "root@:~ #" {  ts "logged in as root" ; sleep 2 }
         timeout {  ts "login failed" ; exit 1}
       }

set timeout 5

send "cli\r"
expect {
    "root\>"   {  ts "cli enabled"}
    timeout   {  ts "cli mode timeout" ; exp_continue }
}

send "configure\r"
expect {
    "Entering configuration mode"   {  ts "configuration mode"}
    timeout    {  ts "configuration mode timeout" ; exp_continue }
}

ts "send configuration"

foreach sf [lrange $argv 1 end] {
  sendfile [locate $sf]
}

set timeout 1
expect {
  zzzzzzzzz
}

ts "sent configuratiion"

send "\rcommit\r"
set timeout 20
expect {
    "commit complete"   {  ts "commit complete"}
    "configuration check-out failed"   {  ts "configuration check-out failed" ; send_user($expect_out(buffer)) ; exit 1}
    "commit failed"   {  ts "commit failed" ; send_user($expect_out(buffer)) ; exit 1}
    timeout   {  ts "commit mode timeout" ; exit 1 }
}

send "\rexit\r"
expect {
    "Exiting configuration mode"   {  ts "exited configuration mode"}
    timeout   {  ts "exit commit mode timeout" ; exp_continue }
}

ts "configuration complete"
sleep 1
# send "\0x1d"
# expect eof
close
spawn "/usr/bin/virsh" "destroy" "$vm"
send_user "\n\n"
