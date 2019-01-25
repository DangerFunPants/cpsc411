/*
Read a number and print
all values from it down to 0
*/
var x: int;
var y: bool;

begin
read x;

y := x>=0;

while y do {
	print x;

	%x := x - 1;
	
	% why decrement the easy way?
	x := ((-----2) + -1*(-2*x)) / (2 - 4 + 3 + 1); 	
	
	y := x>=0;
};

end
