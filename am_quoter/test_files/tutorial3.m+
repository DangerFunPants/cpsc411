/* 
this function just returns its input value
*/
fun silly (x : int): int {
	return x;
};

/*
print all numbers from its input n all the
way down to 0
*/
fun printNums (x: int): int {
	var y: int;

	print silly(x); % should really check that this is >= 0
	
	if ((((( x>0 ))))) then % extra brackets for fun
		y := printNums (x-1)
	else
		y := 0;

	return y;
};

var z: int;

begin
read z;
z := printNums(z);
end
