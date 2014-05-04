class Main inherits IO{
    a : Int <- 8;
    c : Int;
    b : Int <- a * 2;
    d : Dog;
    x : String <- "yo";
	main() : Object {
        let num : Int <- 3,
            num2 : Int <- num.copy(),
            y : Int <- 5,
            z : String <- "Hello" in
        {
            out_string("Enter a number: ");
            in_int();
            out_string("\n");
            out_int(1234567 * 98765432);
            out_string("LOLOLOL");
            out_int(y);
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
            case_check(d);
            out_string((new Dog).type_name());
            out_string(a.type_name());
            str_check();
            doh();
            out_string(b.type_name());
            self.doh();
            (new Main).doh();
            self@Main.doh();
            check_obj_compares();
            check_math();
            check_is_void();
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
                out_string("01234".substr(1,4));
            }
    };
    cool_eq_check() : Bool {
        let a : Dog <- new Dog,
            b : Dog <- a in
        a = b
    };
    case_check(o : Dog) : Object {
        let a : Dog <- new Dog in
        {
            case a of
                d : Cat => out_string("Cat Case\n");
                d : Main => out_string("Main Case\n");
                d : Dog => out_string("Dog Case\n");
            esac;

            case a of
                d : Cat => out_string("Cat Case\n");
                d : Main => out_string("Main Case\n");
            esac;

            case new Int of
                d : Cat => out_string("Cat Case\n");
                d : Main => out_string("Main Case\n");
            esac;
        }
    };
    let_check() : Object {
        let test : Int <- 2,
            test2 : String <- "Hello",
            test3 : String,
            test4 : Dog <- new Dog in {
                out_string(test2);
                test4.bark();
                out_string(test4@Main.wooooookie());
                out_string(test4.wooooookie());
            }
    };
    wooooookie() : String {
        --(a * b + 2) * 10 + c + b
        "Main"
    };
    check_is_void() : Object {
        let a : Dog,
            b : Dog <- new Dog in
            {
                if isvoid(a) then out_string("a is void\n") else out_string("a is not void\n") fi;
                if isvoid(b) then out_string("b is void\n") else out_string("b is not void\n") fi;
            }
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
        {
            while a < 100 loop
                a <- a + 1
            pool;
            out_string("A: \n");
            out_int(a);
        }
    };
    check_math() : Object {
        {
            if not true then out_string("Not True\n") else out_string("Totally True\n") fi;
            out_int(~5);
            out_int( 3 * 2 + 4 - 9 / 3);
            out_int(98765432*9876543);
        }
    };
    check_obj_compares() : Object {
        let a : Dog <- new Dog,
            b : Cat <- new Cat,
            c : Dog <- a in
            {
                    if a = b then out_string("A = B True\n") else out_string("A = B False\n") fi;
                    if a = a then out_string("A = A True\n") else out_string("A = A False\n") fi;
                    if a = c then out_string("A = C True\n") else out_string("A = C False\n") fi;
                    if a < b then out_string("A < B True\n") else out_string("A < B False\n") fi;
                    if c < b then out_string("C < A True\n") else out_string("C < A False\n") fi;
                    if a <= a then out_string("A <= A True\n") else out_string("A <= A False\n") fi;
                    if c <= a then out_string("C <= A True\n") else out_string("C <= A False\n") fi;
                    if b <= a then out_string("B <= A True\n") else out_string("B <= A False\n") fi;
            }
    };
};

class Dog inherits Main{
    bark() : Object {
        self@Main.wooooookie()
    };
    wooooookie() : String {
        "Dog"
    };
};

class Cat inherits Main{
    meow() : Object {
        self@Main.wooooookie()
    };
    wooooookie() : String {
        "Cat"
    };
};
