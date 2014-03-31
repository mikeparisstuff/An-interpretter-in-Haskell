class Main inherits IO{

    main() : Object {
        let x : Int <- new Int,
            y : Int in
        {
        	y <- x;
        	y <- 5;
            out_int(x);
            out_string("\n");
            out_int(y);
            out_string("\n");
        }
    };

    blah() : Object {
    	let d : Dog <- new Dog,
        d2 : Dog <- d.get_self_t() in {
        	out_string("Doggies");
        }
    };
};

class Dog {
	get_self_t() : SELF_TYPE {
		let d : Dog <- new Dog in {
			d
		}
	};
};