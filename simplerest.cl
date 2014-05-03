class Main inherits IO{
    a : Int <- 8;
    c : Int;
    b : Int <- a * 2;
	main() : Object {
        let num : Int <- in_int(),
            num2 : Int <- num.copy() in
        {
            out_int(num);
            num2 <- num2 + 2;
            out_int(num);
            (new Dog).bark();
            self.iftrue();
            self.iffalse();
            wooooookie();
            test_loop();
            out_string("hello");
            out_int(num);
            case_check();
            out_string((new Dog).type_name());
            out_string(a.type_name());
            str_check();
            doh();
            self.doh();
            (new Main).doh();
            self@Main.doh();
            if cool_eq_check() then
                out_string("Equal\n")
            else
                out_string("Not Equal\n")
            fi;
        }
	};
    doh() : Int { (let i: Int <- a in { a <- a + 1; i; } ) };
    str_check() : Object {
        let ts : String <- "Lololol",
            ts2 : String <- " Rofl" in
            {
                out_string("\nShould be 7: \n");
                out_int(ts.length());
                out_string("\nShould be 5: \n");
                out_int(ts2.length());
                out_string("\nShould be Lololol Rofl: \n");
                out_string(ts.concat(ts2));
                out_string("\nShould be 12: \n");
                out_int((ts.concat(ts2)).length());
                out_string("\nShould be 123: \n");
                out_string("01234".substr(1,3));
            }
    };
    cool_eq_check() : Bool {
        let a : Dog <- new Dog,
            b : Dog <- a in
        a = b
    };
    case_check() : String {
        case (new Dog) of
            d : Cat => "Correct";
            d : Main => "Incorrect";
        esac
    };
    let_check() : Object {
        let test : Int <- 2,
            test2 : String <- "Hello",
            test3 : String,
            test4 : Dog in test4
    };
    wooooookie() : Object {
        --(a * b + 2) * 10 + c + b
        "Main"
    };
    iftrue() : Object {
        if 10 <= 10 then
            "Should be True"
        else
            "This is wrong"
        fi
    };
    iffalse() : Object {
        if 5 < 4 then
            "This is wrong"
        else
            "Should be false correct"
        fi
    };
    test_outs() : Object {
        out_string("Printing Some stuff in Main Wookie")
    };
    test_loop() : Object {
        while a < 10 loop
            a <- a + 1
        pool
    };
};

class Dog inherits Main{
    bark() : Object {
        self@Main.wooooookie()
    };
    wooooookie() : Object {
        "Dog"
    };
};

class Cat inherits Main{
    meow() : Object {
        self@Main.wooooookie()
    };
    wooooookie() : Object {
        "Dog"
    };
};