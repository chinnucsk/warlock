#!/bin/sh

dev/dev1/bin/warlock start

dev/dev2/bin/warlock start

dev/dev3/bin/warlock start

sleep 3

dev/dev2/bin/warlock-admin join warlock1@127.0.0.1

sleep 3

dev/dev3/bin/warlock-admin join warlock1@127.0.0.1

