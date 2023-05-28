unit vttycom;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 TIOCEXCL      =$2000740D; // set exclusive use of tty
 TIOCNXCL      =$2000740E; // reset exclusive use of tty
 TIOCGPTN      =$4004740F; // Get pts number.
 TIOCFLUSH     =$80047410; // flush buffers
 TIOCGETA      =$402C7413; // get termios struct
 TIOCSETA      =$802C7414; // set termios struct
 TIOCSETAW     =$802C7415; // drain output, set
 TIOCSETAF     =$802C7416; // drn out, fls in, set
 TIOCGETD      =$4004741A; // get line discipline
 TIOCSETD      =$8004741B; // set line discipline
 TIOCPTMASTER  =$2000741C; // pts master validation
 TIOCGDRAINWAIT=$40047456; // get ttywait timeout
 TIOCSDRAINWAIT=$80047457; // set ttywait timeout
 TIOCTIMESTAMP =$40107459; // enable/get timestamp
 TIOCMGDTRWAIT =$4004745A; // modem: get wait on close
 TIOCMSDTRWAIT =$8004745B; // modem: set wait on close
 TIOCDRAIN     =$2000745E; // wait till output drained
 TIOCSIG       =$2004745F; // pty: generate signal
 TIOCEXT       =$80047460; // pty: external processing
 TIOCSCTTY     =$20007461; // become controlling tty
 TIOCCONS      =$80047462; // become virtual console
 TIOCGSID      =$40047463; // get session id
 TIOCSTAT      =$20007465; // simulate ^T status message
 TIOCUCNTL     =$80047466; // pty: set/clr usr cntl mode
 //UIOCCMD(n)  _IO('u', n) // usr cntl op "n"
 TIOCSWINSZ    =$80087467; // set window size
 TIOCGWINSZ    =$40087468; // get window size
 TIOCMGET      =$4004746A; // get all modem bits

 TIOCM_LE      =&0001; // line enable
 TIOCM_DTR     =&0002; // data terminal ready
 TIOCM_RTS     =&0004; // request to send
 TIOCM_ST      =&0010; // secondary transmit
 TIOCM_SR      =&0020; // secondary receive
 TIOCM_CTS     =&0040; // clear to send
 TIOCM_DCD     =&0100; // data carrier detect
 TIOCM_RI      =&0200; // ring indicate
 TIOCM_DSR     =&0400; // data set ready
 TIOCM_CD      =TIOCM_DCD;
 TIOCM_CAR     =TIOCM_DCD;
 TIOCM_RNG     =TIOCM_RI ;

 TIOCMBIC      =$8004746B; // bic modem bits
 TIOCMBIS      =$8004746C; // bis modem bits
 TIOCMSET      =$8004746D; // set all modem bits
 TIOCSTART     =$2000746E; // start output, like ^Q
 TIOCSTOP      =$2000746F; // stop output, like ^S
 TIOCPKT       =$80047470; // pty: set/clear packet mode

 TIOCPKT_DATA      =$00; // data packet
 TIOCPKT_FLUSHREAD =$01; // flush packet
 TIOCPKT_FLUSHWRITE=$02; // flush packet
 TIOCPKT_STOP      =$04; // stop output
 TIOCPKT_START     =$08; // start output
 TIOCPKT_NOSTOP    =$10; // no more ^S, ^Q
 TIOCPKT_DOSTOP    =$20; // now do ^S ^Q
 TIOCPKT_IOCTL     =$40; // state change of pty driver

 TIOCNOTTY     =$20007471; // void tty association
 TIOCSTI       =$80017472; // simulate terminal input
 TIOCOUTQ      =$40047473; // output queue size
 TIOCSPGRP     =$80047476; // set pgrp of tty
 TIOCGPGRP     =$40047477; // get pgrp of tty
 TIOCCDTR      =$20007478; // clear data terminal ready
 TIOCSDTR      =$20007479; // set data terminal ready
 TIOCCBRK      =$2000747A; // clear break bit
 TIOCSBRK      =$2000747B; // set break bit

 TTYDISC     =0; // termios tty line discipline
 SLIPDISC    =4; // serial IP discipline
 PPPDISC     =5; // PPP discipline
 NETGRAPHDISC=6; // Netgraph tty node discipline
 H4DISC      =7; // Netgraph Bluetooth H4 discipline

implementation

end.

