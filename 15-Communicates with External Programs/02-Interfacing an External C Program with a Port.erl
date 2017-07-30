% --- ports/example1.c ---------
int sum(int x, int y){
  return x+y;
}

int twice(int x){
  return 2*x;
}
%--------------------------------

%% Our final goal is to call these routines from Erlang.
X1 = example1:sum(12,23),
Y1 = example1:twice(10),

%% To implement this, we need to turn function calls such as sum(12,23) and
%% twice(10) into sequences of bytes that we send to the external program by means
%% of the port.

%% The protocol we use is very simple.
%% • All packets start with a 2-byte length code ( Len ) followed by Len bytes of
%%   data. This header is automatically added by the port when we open it with
%%   argument {packet,2} .
%% • We encode the call sum(N, M) as the byte sequence [1,N,M] .
%% • We encode the call twice(N) as the byte sequence [2,N] .
%% • Arguments and return values are assumed to be a single byte long.

%% What happens is as follows:
%%
%% 1. The driver encodes the sum(12,23) function call into the byte sequence
%%    [1,12,23] and sends the {self(), {command, [1,12,23]}} message to the port.
%% 2. The port driver adds a 2-byte length header to the message and sends
%%    the byte sequence 0,3,1,12,23 to the external program.
%% 3. The external program reads these five bytes from standard input, calls
%%    the sum function, and then writes the byte sequence 0,1,35 to standard
%%    output.
%%    The first two bytes contains the packet length. This is followed by the
%%    result, 35 , which is 1-byte long.
%% 4. The port driver removes the length header and sends a {Port, {data, [35]}}
%%    message to the connected process.
%% 5. The connected process decodes this message and returns the result to
%%    the calling program.


%% This code is specialized for handling data with a 2-byte length header, so it
%% matches up with the {packet, 2} option given to the port driver program.

% --- ports/example1_driver.c --------------------
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);
int sum(int x, int y);
int twice(int x);

int main() {
  int fn, arg1, arg2, result;
  byte buff[100];
  while (read_cmd(buff) > 0) {
    fn = buff[0];
    if (fn == 1) {
      arg1 = buff[1];
      arg2 = buff[2];
      /* debug -- you can print to stderr to debug
      fprintf(stderr,"calling sum %i %i\n",arg1,arg2); */
      result = sum(arg1, arg2);
    } else if (fn == 2) {
      arg1 = buff[1];
      result = twice(arg1);
    } else {
      /* just exit on unknown function */
      exit(EXIT_FAILURE);
    }

    buff[0] = result;
    write_cmd(buff, 1);
  }
}
%% --------------------------------------------------------------

% -- ports/erl_comm.c -------------------------------------------
/* erl_comm.c */
#include <unistd.h>
typedef unsigned char byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

int read_cmd(byte *buf)
{
  int len;
  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;
  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  li = len & 0xff;
  write_exact(&li, 1);
  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;
  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
  got += i;
} while (got<len);
  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;
  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);
    return (len);
}
%% --------------------------------------------------------------

%% ---- ports/example1.erl --------------------------------------
-module(example1).

-export([start/0, stop/0]).
-export([twice/1, sum/2]).

start() ->
  register(example1,
    spawn(fun() ->
      process_flag(trap_exit, true),
      Port = open_port({spawn, "./example1"}, [{packet, 2}]),
      loop(Port)
    end)).

stop() -> ?MODULE ! stop.

twice(X) -> call_port({twice, X}).
sum(X,Y) -> call_port({sum, X, Y}).

call_port(Msg) ->
  ?MODULE ! {call, self(), Msg},
  receive
    {?MODULE, Result} -> Result
  end.

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, encode(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {?MODULE, decode(Data)}
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      exit({port_terminated, Reason})
  end.

encode({sum, X, Y}) -> [1, X, Y];
encode({twice, X}) -> [2, X].
decode([Int]) -> Int.

%% --- ports/Makefile.mac ----------------------------
.SUFFIXES: .erl .beam .yrl

.erl.beam:
  erlc -W $<

MODS = example1 example1_lid unit_test

all: ${MODS:%=%.beam} example1 example1_drv.so
     @erl -noshell -s unit_test start

example1: example1.c erl_comm.c example1_driver.c
          gcc -o example1 example1.c erl_comm.c example1_driver.c

example1_drv.so: example1_lid.c example1.c
                 gcc -arch i386 -I /usr/local/lib/erlang/usr/include\
                 -o example1_drv.so -fPIC -bundle -flat_namespace -undefined suppress\
                 example1.c example1_lid.c

clean:
   rm example1 example1_drv.so *.beam
%% ----------------------------------------------------------