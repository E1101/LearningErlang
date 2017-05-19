% > All convert atoms to strings and back. If the atom was not previously used by
% - the runtime system in the current session, calling the function
% - list_to_existing_atom/1 will fail.
atom_to_list/1 , list_to_atom/1 , list_to_existing_atom/1

% > Both convert between the two data types.
list_to_tuple/1 , tuple_to_list/1

% > Both create a float, one with an integer parameter and the other from a string.
float/1 , list_to_float/1

% > Both return strings.
float_to_list/1 , integer_to_list/1

% > All return integers.
round/1 , trunc/1 , list_to_integer/1


atom_to_list(monday). % "monday"
list_to_existing_atom("tuesday"). % ** exception error: bad argument
list_to_existing_atom("monday").  % monday
list_to_tuple(tuple_to_list({one,two,three})). % {one,two,three}

float(1). % 1.00000
round(10.5). % 11
trunc(10.5). % 10


