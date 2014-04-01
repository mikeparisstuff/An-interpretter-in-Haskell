class Main inherits IO {
    main() : Object {
        {

        	let t : Object <-  {
        		out_string("Hey Derr\n");
                out_string("Guy\n");
        	} in
            out_string(t.type_name());
           
            let t : Object <-  {
                out_string("Hey Derr\n");
                out_string("Guy\n");
                4;
            } in
            out_string(t.type_name());
        }

    };
};

class Dog inherits Main { 
	x : String <- "Bark"; 
	out() : Object { out_string(x) };
};