 
data intlist = #nil | #cons of int * intlist;
var a[3]:intlist;
fun addn(n:int,x[]:intlist):int
{ var i:int;
  i:=0;
  while i<size(x) do { x[i]:= #cons(n,x[i]);i:=i+1;};
  return i;
};
  
a[0] := #nil; a[1] := #nil;   a[2]:= #nil;
print addn(6,a);

