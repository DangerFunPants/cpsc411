/*
  What order are you processing arguments to a function?
  What happens if one of the parameters you pass includes a function
  call that modifies a global variable?
*/
var m: int;

/*
  Updates the global variable m to a new value
*/
fun updateM (newval: int) : int {
      var x: int;
      x := m;
      m := newval;
      return x;
};

/*
  Prints a pair of values
*/
fun printPair (a,b:int) : int {
      print a;
      print b;
      return 0;
};

m := 6;
m := printPair (updateM(5),m);
m := 10;
print updateM(2)+m;
m := 10;
print m+updateM(2);
 
