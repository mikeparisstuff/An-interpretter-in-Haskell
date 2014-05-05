class Main inherits IO {
    i : Int <- 2;
    main() : Object { {
        self.copy().change_i().print_i();   -- prints 3
        self.copy().change_i();
        out_int(i);                 -- prints 2
        out_string("\n");
        self.change_i();
        out_int(i);             -- prints 3
        out_string("\n");
    } };

    change_i() : Main { {
        i <- i + 1;
        self;
    } };

    print_i() : Object {
        out_int(i)
    };
};