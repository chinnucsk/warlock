#warlock

Distributed consensus service focused on consistency

##Quick start

Get dependencies and compile the project

    $ make

Create a release in "rel" folder

    $ make rel

Start and open in console mode

    $ rel/warlock/bin/warlock console

##Usage

warlock uses "ets" based storage backend by default. It can be configured
to use other backends such as redis (driver included).

It supports two types of command

* Cluster (clu): runs the command on the cluster
* Local (loc): runs the command on the connected node

For example, set and get commands can be run as

    (warlock@127.0.0.1)1> war_server:x(clu, [set, key, value]).
    {ok,success}

    (warlock@127.0.0.1)1> war_server:x(loc, [get, key]).
    {ok,value}
    

The format is `server:x(LOCATION, COMMAND)`

    
##Development cluster

warlock can be tested by creating a 3 node dev cluster,

    $ make devrel

This creates 3 nodes in the `dev` folder.

Starting the cluster (start the 3 nodes and connect them),

    $ bin/dev_start.sh

Run commands on the cluster by running commands on any of the 3 nodes.

    $ dev/dev1/bin/warlock attach
    Attaching to /tmp//home/user/warlock/dev/dev1/erlang.pipe.1 (^D to exit)

    (warlock1@127.0.0.1)1> war_server:x(clu, [set, key, value]).
    {ok,success}

    Ctrl+D
    (warlock1@127.0.0.1)1> [Quit]

    $ dev/dev2/bin/warlock attach
    Attaching to /tmp//home/user/warlock/dev/dev2/erlang.pipe.1 (^D to exit)

    (warlock2@127.0.0.1)1> war_server:x(loc, [get, key]).
    {ok,value}

    Ctrl+D
    (warlock2@127.0.0.1)1> [Quit]
