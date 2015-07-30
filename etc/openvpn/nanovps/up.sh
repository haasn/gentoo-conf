#!/bin/sh
ifconfig $1 add 2a01:4f8:d13:5245::3/64 mtu $2 || true
route -A inet6 add default gw 2a01:4f8:d13:5245::2 || true
route add -net 10.80.1.0/24 gw 10.80.0.1 metric 256 tap0 || true
route add default gw 10.80.0.1 metric 127 || true
