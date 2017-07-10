%% # ############## #
%% # The Bit Syntax #
%% # ############## #

%% Suppose we have three variables— X , Y , and Z —that we want to pack into a 16-bit
%% memory area. X should take 3 bits in the result, Y should take 7 bits, and Z should
%% take 6.
M = <<X:3, Y:7, Z:6>>
%% !! M is of type binary since the total bit length of the data is 16 bits,
%%    which is exactly divisible by 8.
M = <<X:2, Y:7, Z:6>>
%% !! total number of bits in M is 15 , so the resulting data structure is of type bitstring .


% > Packing and Unpacking 16-Bit Colors
Red = 2.
Green = 61.
Blue = 20.
Mem = <<Red:5, Green:6, Blue:5>>. % <<23,180>> because its 16-bit = 2 * 8-byte
<<R1:5, G1:6, B1:5>> = Mem. % <<23,180>>

R1. % 2
G1. % 61
B1. % 20

% The value of the Size can be obtained from earlier pattern matches in
% the binary.
<<Size:4, Data:Size/binary, ...>>


% > Bit Syntax have the following form:
<<E1, E2, ..., En>>

Ei = Value |
   Value:Size |
   Value/TypeSpecifierList |
   Value:Size/TypeSpecifierList

TypeSpecifierList
% TypeSpecifierList is a hyphen-separated list of items of the form End-Sign-Type-Unit .


%% # ##################################### #
%% # Bitstrings: Processing Bit-Level Data #
%% # ##################################### #

B2 = <<1:17>>.
% <<0,0,1:1>>
% mean binary literal whose third segment is a bitstring of length 1

is_binary(B2). % false
is_bitstring(B2). % true
byte_size(B2). % 3
bit_size(B2). % 17

% !! We can’t, for example, write a bitstring to
%    a file or socket (which we can do with a binary), since files and sockets work
%    in units of bytes.

% > This example shows how to extract the bits from a byte:
B = <<16#5f>>.
[ X || <<X:1>> <= B]. % [0,1,0,1,1,1,1,1]
<< <<X>> || <<X:1>> <= B >>. % <<0,1,0,1,1,1,1,1>>



%% # ##################### #
%% # Decoding TCP Segments #
%% # ##################### #

% ! Transmission Control Protocol (TCP)

decode(Segment) ->
  case Segment of
    << SourcePort:16, DestinationPort:16,
      SequenceNumber:32,
      AckNumber:32,
      DataOffset:4, _Reserved:4, Flags:8, WindowSize:16,
      Checksum:16, UrgentPointer:16,
      Payload/binary>> when DataOffset > 4 ->
      OptSize = (DataOffset - 5)*32,
      << Options:OptSize, Message/binary >> = Payload,
      <<CWR:1, ECE:1, URG:1, ACK:1, PSH:1, RST:1, SYN:1, FIN:1>> = <<Flags:8>>,
      %% Can now process the Message according to the
      %% Options (if any) and the flags CWR, ..., FIN.
      binary_to_list(Message);
    _ -> {error, bad_segment}
  end.

seg1() ->
  << 0:16, 0:16,
    0:32,
    0:32,
    5:4, 0:4, 0:8, 0:16,
    0:16, 0:16,
    "message">>.

seg2() ->
  << 0:16, 0:16,
    0:32,
    0:32,
    7:4, 0:4, 0:8, 0:16,
    0:16, 0:16,
    0:64,
    "message">>.

