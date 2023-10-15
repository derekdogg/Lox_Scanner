var global = "global 1" ;
{
   var a = "1a";
   print a ;
}

print global ;



//two scope depth
var global = "global 1" ;
{
   var a = "1a";
   print a ;
   {
        var a = "1a";
        print a ;
    }
}

print global ;




var global = "global 1" ;
var i = 1;
{ 
   var a = "1a";
   while (i <=10) {
       print  (a + 1);
       i = i + 1;
   }
} 

 
//adding a number to a string;
var global = "global 1" ;
var i = 1;
{ 
   var a = "1a";
   while (i <=10) {
      a = a + i; 
      print  (a);
      i = i + 1;
   }
} 


//declare same global *2 in different scoping contexts

var someglobal;

{
    var s = "Scope 1";
    print s ;
    var a = "1a";
    print a ;
    {
        var s = "Scope 2";
        print s ;
        var a = "2a";
        print a ;
        {
	        var s = "Scope 3";
	        print s ;
	        var a = "3a";
        		print a ;	

            {
	            
		var someglobal;
		var s = "Scope 4";
	            print s ;
		        var a = "4a";
        	          print a ;	
	          print someglobal;



            }
        }
    }
}

{
    var s = "Scope 1";
     
    var a = "1a";
    print a ;
    print s ;
    {
        var s = "Scope 2";
         
        var a = "2a";
        print a ;
        print s ;
        {
	        var s = "Scope 3";
	        
	        var a = "3a";
        	print a ;	
            print s ;

            {
	            var s = "Scope 4";
	             
		        var a = "4a";
        	    print a ;	
                print s ;



            }
        }
    }
}

{
    var s = "Scope 1";
    var a = "1a";
    {


    }
    print s ;
    print a ;
    
}

{
    var s = "Scope 1";
    var a = "1a";
    {


    }
    {


    }
    {


    }
    {


    }
    print s ;
    print a ;
    
}

{
    
    {
        
        {
	        
            
            {
                
    
                {
                    var s = "Scope 5";
                    var a = "5a";
                    print a ;	
                    print s ;
    
    
    
                }
                
            }
        
        }
    }
}

{
    
    {
        
        {
	        
            
            {
                var s = "Scope 4";
                var a = "4a";    
                {
                    var s = "Scope 5";
                    var a = "5a";
                    print a ;	
                    print s ;
                }
                print a ;	
                print s ;
            }
        
        }
    }
}

var globalb = "top global";
{
    print globalb;
    
}



PRINT:1a
PRINT:global 1
PRINT:Scope 1
PRINT:1a
PRINT:Scope 2
PRINT:2a
PRINT:Scope 3
PRINT:3a
PRINT:Scope 4
PRINT:4a
PRINT:1a
PRINT:Scope 1
PRINT:2a
PRINT:Scope 2
PRINT:3a
PRINT:Scope 3
PRINT:4a
PRINT:Scope 4
PRINT:Scope 1
PRINT:1a
PRINT:Scope 1
PRINT:1a
PRINT:5a
PRINT:Scope 5
PRINT:5a
PRINT:Scope 5
PRINT:4a
PRINT:Scope 4
PRINT:top global
