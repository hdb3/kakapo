# kakapo
BGP flood tool - source and sink
A minimalistic 'C' tool which is capable of establishing BGP peer sessions and subsequently generating or accepting very large numbers of Update messages.
All used BGP messages are sourced from preencoded binary files which allows complete flexibility in choice of payload and BGP capability options.
## BGP FSM
The implemented state machine is very basic but sufficient to bring up sessions with any type of peer.  The FSM principle is to send an initial (BGP Open) message, await any reply, and then send BGP Keepalive.
_TODO_ Implment Keepalive response
Once the FSM has completed the initial exchange the program sends the provided file which should contain a stream of BGP Update messages.  Finally, the program enters an receive loop which parses, counts and discards received messages, which should also be BGP Updates.
## Update handling and Performance
Message transmission is accomplished using a single _sendfile_ system call, which ensures that the payload is sent at the fastest rate possible given the underlying hardware.
Message reception is done in two ways: the default uses libc FILE buffering which is more than fast enough for most purposes (10M updates in 0.7 seconds).  This option parses BGP messages sufficiently to count them and potentially to respond differently to specific message types or content.  A compile time option provides the capability of simply dumping all input once the initial BGP handshake is complete.  This option cannot count BGP messages: it enables a 10M Update burst to be consumed in ~0.2 seconds)
