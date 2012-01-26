#!/usr/bin/env perl
#
# mintty-wrapper.pl
# -----------------
# Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of Lambda-Term.

# This script allow to run a mingw-compiled lambda-term application
# under mintty. It is included at compile time into lambda-term.

use Term::ReadKey;
use IO::Socket;

# Connect to the lambda-term application
my $sock = new IO::Socket::INET (
    PeerHost => '127.0.0.1',
    PeerPort => $port,
    Proto => 'tcp',
);

die "lambda-term-mintty-helper.pl: could not connect to 127.0.0.1:$port: $!\n" unless $sock;

# Test whether input and output are ttys
$incoming_is_a_tty = -t STDIN;
$outgoing_is_a_tty = -t STDOUT;

if ((not $incoming_is_a_tty) && (not $outgoing_is_a_tty)) {
    print $sock "notty";
    exit(0);
}

if ($outgoing_is_a_tty) {
    # Get initial terminal size
    ($cols, $rows, $xpix, $ypix) = Term::ReadKey::GetTerminalSize(STDOUT);
    print $sock "$incoming_is_a_tty:$outgoing_is_a_tty:$cols:$rows\n";
    $SIG{WINCH} = \&send_size;
} else {
    print $sock "$incoming_is_a_tty:$outgoing_is_a_tty:0:0\n";
}

# Send the size when we receive a SIGWINCH
sub send_size {
    ($cols, $rows, $xpix, $ypix) = Term::ReadKey::GetTerminalSize(STDOUT);
    print $sock "size:$cols:$rows\n";
}

# Handle commands
while (<$sock>) {
    $cmd = $_;
    $cmd =~ s/\s+$//;
    if ($cmd eq "enter-raw-mode") {
        Term::ReadKey::ReadMode(4);
        print $sock "done\n";
    }
    if ($cmd eq "leave-raw-mode") {
        Term::ReadKey::ReadMode(0);
        print $sock "done\n";
    }
}
