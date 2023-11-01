fun sum(a, b, c) {
    return a + b + c;
  }
  
  
  // global variable 
  var g = "fred"; 
  
  
  //main code block
  {
        print sum(5, 6, 7);
        print g;
  
        //native function here.
        print DateTime();
  
  
        //native function here expecting 3 params
        print foo("bob","Fred","Brenda");
  
  
    //random while loop
    
    var i = 1;
      while (i <=10) {
         
             i = i + 1;
         }
       
      print i;
  
  }
