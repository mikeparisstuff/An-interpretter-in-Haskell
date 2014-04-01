class Main inherits IO {
    main() : Object {
    	let a : Int <- 5,
    	b : String <- "hello",
    	c : Dog <- new Dog,
    	d : IO <- new IO,
    	e : Int <- a + 4,
    	a : Int <- 7 in 
    	{
    		out_int(a);
    		out_string(b);
    		c.out();
    		out_int(e);
    	}
       
    };
};

class Dog inherits Main { 
	x : String <- "Bark"; 
	out() : Object { out_string(x) };
};