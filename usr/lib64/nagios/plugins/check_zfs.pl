#!/usr/bin/perl -w

# check_zfs Nagios plugin for monitoring ZFS Pools
# Copyright (c) 2007 
# Written by Nathan Butcher

# Copyright (c) 2013
# By Stephan Ebelt, adopted to work on CentOS with ZFSonLinux

# Released under the GNU Public License
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Version: 0.9.3
# Date : 21st August 2013
# Adopted to run on CentOS-6 Linux with ZFS-on-Linux, see http://zfsonlinux.org/

# Version: 0.9.2
# Date : 24th July 2007
# This plugin has tested on FreeBSD 7.0-CURRENT and Solaris 10
# With a bit of fondling, it could be expanded to recognize other OSes in
# future (e.g. if FUSE Linux gets off the ground)

# Verbose levels:-
# 1 - Only alert us of zpool health and size stats
# 2 - ...also alert us of failed devices when things go bad
# 3 - ...alert us of the status of all devices regardless of health
#
# Usage:   check_zfs <zpool> <verbose level 1-3>
# Example: check_zfs zeepool 1
#	ZPOOL zeedata : ONLINE {Size:3.97G Used:183K Avail:3.97G Cap:0%}

use strict;

my %ERRORS=('DEPENDENT'=>4,'UNKNOWN'=>3,'OK'=>0,'WARNING'=>1,'CRITICAL'=>2);
my $state="UNKNOWN";
my $msg="FAILURE";

if ($#ARGV+1 != 2) {
	print "Usage: $0 <zpool name> <verbose level 1-3>\n";
	exit $ERRORS{$state};
}

#if ($^O ne 'solaris' && $^O ne 'freebsd') {
#	print "This plugin currently only works on Solaris 10, OpenSolaris distributions, and FreeBSD 7 and later.\n";
#	exit $ERRORS{$state};
#}

my $pool=$ARGV[0];
my $verbose=$ARGV[1];

my $size=""; # SIZE
my $used=""; # ALLOC
my $avail=""; # FREE
my $sizex=""; # EXPANDSZ
my $frag=""; # FRAG
my $cap=""; # CAP
my $dedup=""; # DEDUP
my $health=""; # HEALTH
my $dmge="";

if ($verbose < 1 || $verbose > 3) {
	print "Verbose levels range from 1-3\n";
	exit $ERRORS{$state};
}

my $statcommand="sudo /sbin/zpool list $pool";

if (! open STAT, "$statcommand|") {
	print ("$state '$statcommand' command returns no result! NOTE: This plugin needs OS support for ZFS, and execution with root privileges.\n");
	exit $ERRORS{$state};
}

while(<STAT>) {
	chomp;
                   NAME   SIZE   ALLOC   FREE   EXPANDSZ   FRAG   CAP   DEDUP   HEALTH   ALTROOT
	next if (/^NAME\s+SIZE\s+ALLOC\s+FREE\s+CAP\s+DEDUP\s+HEALTH\s+ALTROOT/);
        next if (/^NAME\s+SIZE\s+ALLOC\s+FREE\s+EXPANDSZ\s+FRAG\s+CAP\s+DEDUP\s+HEALTH\s+ALTROOT/); 
	if (/^${pool}\s+/) {
                ($size, $used, $avail, $sizex, $frag, $cap, $dedup, $health) = /^${pool}\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/;
	}
}

close(STAT);

## check for valid zpool list response from zpool
if (! $health ) {
	$state = "CRITICAL";
	$msg = sprintf "ZPOOL {%s} does not exist and/or is not responding!\n", $pool;
	print $state, " ", $msg;
	exit ($ERRORS{$state});
}

## determine health of zpool and subsequent error status
if ($health eq "ONLINE" ) {
	$state = "OK";
} else {
	if ($health eq "DEGRADED") {
		$state = "WARNING";
	} else {
		$state = "CRITICAL";
	}
}

## get more detail on possible device failure
## flag to detect section of zpool status involving our zpool
my $poolfind=0;

$statcommand="sudo /sbin/zpool status $pool";
if (! open STAT, "$statcommand|") {
	$state = 'CRITICAL';
	print ("$state '$statcommand' command returns no result! NOTE: This plugin needs OS support for ZFS, and execution with root privileges.\n");
	exit $ERRORS{$state};
}

## go through zfs status output to find zpool fses and devices
while(<STAT>) {
	chomp;

	if (/^\s${pool}/ && $poolfind==1) {
		$poolfind=2;
		next;
	} elsif ( $poolfind==1 ) {
		$poolfind=0;
	}

	if (/NAME\s+STATE\s+READ\s+WRITE\s+CKSUM/) {
		$poolfind=1;
	}

	if ( /^$/ ) {
		$poolfind=0;
	}

	if ($poolfind == 2) {

		## special cases pertaining to full verbose
		if (/^\sspares/) {
			next unless $verbose == 3;
			$dmge=$dmge . "[SPARES]:- ";
			next;
		}
		if (/^\s{5}spare\s/) {
			next unless $verbose == 3;
			my ($sta) = /spare\s+(\S+)/;
			$dmge=$dmge . "[SPARE:${sta}]:- ";
			next;
		}
		if (/^\s{5}replacing\s/) {
			next unless $verbose == 3;
			my $perc;
			my ($sta) = /^\s+\S+\s+(\S+)/;
			if (/%/) {
				($perc) = /([0-9]+%)/;	
			} else {
				$perc = "working";
			}
			$dmge=$dmge . "[REPLACING:${sta} (${perc})]:- ";
			next;
		}
		if (/^\scache/) {
			next unless $verbose == 3;
			$dmge=$dmge . "[CACHE]:- ";
			next;
		}

		## other cases
		my ($dev, $sta) = /^\s+(\S+)\s+(\S+)/;

		## pool online, not degraded thanks to dead/corrupted disk
		if ($state eq "OK" && $sta eq "UNAVAIL") {
			$state="WARNING";
			
			## switching to verbose level 2 to explain weirdness
			if ($verbose == 1) {
				$verbose =2;
			}
		}

		## no display for verbose level 1
		next if ($verbose==1);
		## don't display working devices for verbose level 2
		next if ($verbose==2 && $state eq "OK");
		next if ($verbose==2 && ($sta eq "ONLINE" || $sta eq "AVAIL" || $sta eq "INUSE"));
	
		## show everything else
		if (/^\s{3}(\S+)/) {
			$dmge=$dmge . "<" . $dev . ":" . $sta . "> ";
		} elsif (/^\s{7}(\S+)/) {
			$dmge=$dmge . "(" . $dev . ":" . $sta . ") ";
		} else {
			$dmge=$dmge . $dev . ":" . $sta . " ";
		}
	}
}

## calling all goats!

$msg = sprintf "ZPOOL %s : %s {Size:%s Used:%s Avail:%s Expand:%s Frag:%s Cap:%s Dedup:%s} %s\n", $pool, $health, $size, $used, $avail, $sizex, $frag, $cap, $dedup, $dmge;
print $state, " ", $msg;
exit ($ERRORS{$state});
