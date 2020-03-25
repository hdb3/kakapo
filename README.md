# kakapo
BGP flood tool - source and sink
A minimalistic 'C' tool which is capable of establishing BGP peer sessions and subsequently simultaneously generating and accepting very large numbers of Update messages.
## Description
BGP update messages may be sourced from preencoded binary files which allows complete flexibility in choice of payload and BGP capability options, or generated synthetically.  Synthetic streams can be flexibly defined in terms of prefix count per path, total number of prefixes and paths, and can repeat cycle with path variations to ensure that generated updates continue to be forwarded by receipient even when prefixes is reused.
## BGP FSM
The implemented state machine is basic but sufficient to bring up sessions with any type of peer.
Once the FSM has completed and sessions established _kakapo_ starts to stream pre-defined BGP Update messages.  _Kakapo_ operates a seprate receive thread which counts checks and logs received updates. 
## Update handling and Performance
Bulk essage transmission is accomplished using a single _sendfile_ system call, which ensures that the payload is sent at the fastest rate possible given the underlying hardware.
Message reception is done in two ways: the default uses libc FILE buffering which is more than fast enough for most purposes (10M updates in 0.7 seconds).  This option parses BGP messages sufficiently to count them and potentially to respond differently to specific message types or content.  A compile time option provides the capability of simply dumping all input once the initial BGP handshake is complete.  This option cannot count BGP messages: it enables a 10M Update burst to be consumed in ~0.2 seconds)
## Usage
_kakapo_ operates as an active BGP speaker only.  kakapo command line has this format:
kakapo <peer IP address>,<local IP address>,(local AS number> <peer IP address>,<local IP address>,(local AS number> [<peer IP address>,<local IP address>,(local AS number>]
  The first defined peer session is the traffic sink/monitor, subsequent peer definitions will source Update traffic. 

## Implementation
_kakapo_ is written in generic 'C' and uses the glibc library. It has no other dependenices.  It is tested on a range of common Linux distributions.
## Generating BGP binary files
_kakapo_ update payload message file encoding must be BGP compliant wireformat, which includes the 16 byte 0xff 'marker', followed by 16-bit message length and 8-bit message type (Update == 2) and then payload if any.
Simple message binary files are included with the source: they and other can be generated using the Haskell language BGP toolkit hBGP (https://github.com/hdb3/hBGP), which contains source code for the CreateUpdate tool as well as an application for extracting an Update stream from an MRT formated RIB dump.
  ## kakapo operational parameters
  kakapo operational parameters are passed as environment variables.  The most fundamental are MODE, SENDFILENAME and REPEAT.
  MODE has a wide range of permitted values, e.g. RATE, FILE and SINGLE (a burst mode).  In burst modes additional parameters are used to customise the Update stream - key parameters here are: GROUPSIZE and BLOCKSIZE.
## Canaries
  The main goal of kakapo is to measure performance, which requires timing the durations between first and last packet  on both reception and transmission.  This can be problematic for the last packet if the device-under-test drops inserts or modifies some Updates.  Kakapo resolves this problem by using 'canary updates' - uniquely identified route updates which are recognised and tracked by the packet reception thread.  After every packet burst a canary is sent, so that the last packet received can be easily identified and timestamped.  Thus, for every burst of updates kakapo can independently record the times taken to send the burst, process the first Update through to a downstream peer, and to complete processing and forwarding of all Updates in the burst.
## Logging
  kakapo writes log files to either or both of local and remote storage (using libcurl for remote).
  Every run produces a single log entry: kakapo calculates mean/max/min and SD for repeated identical tests.  Repeat tests include restarting BGP sessions.
  
 ## Building kakapo
 The kakapo binary is built form source code in the core/ sub-directory.  The build script is 'build.sh'.
 # kakapo relay
 kakapo can generate traffic _much_ faster than any BGP speaker we have tested, including bird.  Therefore the 'toy' BGP speaker kakapo relay was built as a reference 'BGP speaker' to determine how quickly kakapo itself can source and sink traffic.
 kakapo relay is even simpler that kakapo core - it has no BGP FSM, and relies on switching all BGP messages transparently.  Thus the two peers of kakapo relay actually exchange OPEN messages.  If only netcat was high enough performance there would be little need for kakapo relay.  But, netcat is anything but fast.  So kakapo relay was needed.
