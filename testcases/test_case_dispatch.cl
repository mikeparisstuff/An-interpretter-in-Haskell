class Main inherits IO {


    main() : Object {
    	{
	        case self of
	            d : Dog => d.bark();
	            d : Skip => d.bark();
	            d : Main => out_string("MAIN\n");
	        esac;

	        let t : Object <- case (new Object) of
	            d : Dog => d.bark();
	            d : Skip => d.bark();
	            d : Main => out_string("MAIN\n");
	            d : Int => out_string("INT\n");
	            d : Object => out_string("Object\n");
	        esac in

	        out_string(t.type_name());

	        let t : Object <- case (new Skip) of
	            d : Dog => d.bark();
	            d : Skip => d.bark();
	            d : Main => out_string("MAIN\n");
	            d : Int => out_string("INT\n");
	            d : Object => out_string("Object\n");
	        esac in

	        out_string(t.type_name());

	        let t : Object <- case (new Dog) of
	            d : Dog => d.bark();
	            d : Skip => d.bark();
	            d : Main => out_string("MAIN\n");
	            d : Int => out_string("INT\n");
	            d : Object => out_string("Object\n");
	        esac in

	        out_string(t.type_name());

	        let t : Object <- case (new Bottom) of
	            d : Dog => d.bark();
	            d : Skip => d.bark();
	            d : Main => out_string("MAIN\n");
	            d : Int => out_string("INT\n");
	            d : Object => out_string("Object\n");
	        esac in

	        out_string(t.type_name());
    	}
    };


};

class Dog inherits Main {
    bark() : Object { out_string("woof\n") };
};

class Skip inherits Dog {
    bark() : Object { out_string("arf\n") };
};

class Bottom inherits Skip {};
