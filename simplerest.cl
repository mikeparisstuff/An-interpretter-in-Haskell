class Main inherits IO{
    a : Int <- 8;
    c : Int;
    b : Int <- a * 2;
	main() : Object {
        {
            (new Dog).bark();
            self.iftrue();
            self.iffalse();
            wooooookie();
        }
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
};

class Dog inherits Main{
    bark() : Object {
        self@Main.wooooookie()
    };
    wooooookie() : Object {
        "Dog"
    };
};