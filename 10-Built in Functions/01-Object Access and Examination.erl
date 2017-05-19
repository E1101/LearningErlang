% > Returns the first element of a list
hd/1

% > Returns the remaining elements when the first element has been removed
tl/1

% > Returns the length of a list
length/1

% > Returns the number of elements in a tuple
tuple_size/1

% > Returns the nth element of a tuple
element/2

% > Replaces an element in a tuple, returning the new tuple
setelement/3

% > Adds an element to the tuple, as the final element
erlang:append_element/2


List = [one,two,three,four,five].
hd(List). % one
tl(List). % [two,three,four,five]
hd(tl(List)). % two

length(List). % 5


Tuple = {1,2,3,4,5}.
tuple_size(Tuple). % 5
element(2, Tuple). % 2
setelement(3, Tuple, three). % {1,2,three,4,5}
erlang:append_element(Tuple, 6). % {1,2,3,4,5,6}


