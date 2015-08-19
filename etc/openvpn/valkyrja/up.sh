#!/bin/dash
route add -net 198.41.128.0/17 gw 10.60.0.1
ip route add default via 10.60.0.1 dev tap1 table valk-route
ip rule add fwmark 0x2 table valk-route
#sysctl net.ipv4.conf.tap1.rp_filter=2
