%% - Linked-in Drivers
%%   These programs obey the same protocol as the port drivers discussed
%%   earlier. The only difference is that the driver code is linked into the Erlang
%%   kernel and thus runs inside the Erlang OS main process.

%% - NIFS
%%   NIFs are natively implemented functions. These are functions that are
%%   written in C (or some language that compiles to native code) and that are
%%   linked into the Erlang VM. NIFs pass arguments directly onto the Erlang
%%   processes’ stacks and heaps and have direct access to all the Erlang
%%   internal data structure.

%% - C-Nodes
%%   C nodes are nodes implemented in C that obey the Erlang distribution
%%   protocol. A “real” distributed Erlang node can talk to a C-node and will
%%   think that the C-node is an Erlang node

