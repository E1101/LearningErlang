%
rpc:call(Node, Mod, Function, Args) -> Result | {badrpc, Reason}

-spec spawn(Node, Fun) -> Pid
-spec spawn(Node, Mod, Func, ArgList) -> Pid

-spec spawn_link(Node, Fun) -> Pid
-spec spawn_link(Node, Mod, Func, ArgList) -> Pid

-spec disconnect_node(Node) -> bool() | ignored

-spec monitor_node(Node, Flag) -> true
%% If Flag is true , monitoring is turned on; if Flag is false , monitoring is turned
%% off. If monitoring has been turned on, then the process that evaluated
%% this BIF will be sent {nodeup, Node} and {nodedown, Node} messages if Node
%% joins or leaves the set of connected Erlang nodes.

-spec node() -> Node
%% This returns the name of the local node. nonode@nohost is returned if the
%% node is not distributed.

-spec node(Arg) -> Node
%% This returns the node where Arg is located. Arg can be a PID, a reference,
%% or a port. If the local node is not distributed, nonode@nohost is returned.

-spec nodes() -> [Node]
%% This returns a list of all other nodes in the network to which we are connected.

-spec is_alive() -> bool()
%% This returns true if the local node is alive and can be part of a distributed
%% system. Otherwise, it returns false .

{RegName, Node} ! Msg
%% sends the message Msg to the registered process RegName on the node Node.

