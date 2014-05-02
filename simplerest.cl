class Main inherits IO{
    a : Int <- 8;
    c : Int;
    b : Int <- a * 2;
	main() : Object {
        let num : Int <- in_int() in
        {
            (new Dog).bark();
            self.iftrue();
            self.iffalse();
            wooooookie();
            test_loop();
            out_string("hello");
            out_int(num);
        }
	};
    case_check() : Object {
        case (new Dog) of
            d : Dog => "Correct";
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