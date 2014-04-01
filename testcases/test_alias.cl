class Main inherits IO{

    main() : Object {
        let x : Dog <- new Dog,
        y : Dog in
        {
            x.setName("Doggie\n");
            y <- x;
            y.bark();
            y.setName("Yosarian\n");
            x.bark();
            x.print_ints();
            y.print_ints();
        }
    };

};

class Dog inherits Main {
    name : String;
    a : Int <- b + 3;
    b : Int <- 5;
    c : Int <- a + 4;
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
};