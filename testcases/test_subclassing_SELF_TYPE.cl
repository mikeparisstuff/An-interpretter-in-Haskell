class Main inherits IO{

    main() : Object {
        let x : Dog <- (new Dog).get_self() in
        {
            let y : Main <- x in
            {
                x.setName("Doggie");
                x.bark();
                out_string(self.type_name());
                x.superBark();
                x.static_test(1,2, "Call");
            };
        }
    };

    bark() : Object { out_string("IN MAIN") };

    static_test( a: Int, b : Int, c : String) : Object { "TOTALLY STATIC YEAH\n" };

};

class Dog inherits Main {
    name : String;
    a : Int <- b + 3;
    b : Int <- 5;
    c : Int <- a + 4;
    superBark() : Object { let t : Object <- self@Main.bark() in out_string(t.type_name()) };
    static_test( a: Int, b: Int, c: String) : Object { 
        let t: Object <- (new Dog)@Main.static_test(1,2,"Test") in out_string(t.type_name()) 
    };
    setName(n : String) : Object { name <- n };
    bark() : Object { out_string(name) };
    print_ints(): Object {
        {
            out_string("A: ");
            out_int(a);
            out_string("\n");
            out_string("B: ");
            out_int(b);
            out_string("\n");
            out_string("C: ");
            out_int(c);
            out_string("\n");
        }
    };

    get_self(): SELF_TYPE {
        {
            out_string(self.type_name());
            new SELF_TYPE;
        }
    };
};