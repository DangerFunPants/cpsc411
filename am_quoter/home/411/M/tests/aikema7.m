/*
  Some nested functions
*/
fun a (x: int) : int { % first function A
  fun a (x: int) : int { % second function A
    
    fun a (x: int) : int { % third function A
      return (x+2);
    };

    return (x+1);
  };

  return a(x);
};
      
};

% what should get output?
print a(5);
