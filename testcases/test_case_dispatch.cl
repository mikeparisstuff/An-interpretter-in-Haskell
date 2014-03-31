class Main inherits IO {


    main() : Object {
        case self of
            d : Dog => d.bark();
            d : Skip => d.bark();
            d : Main => 0;
        esac
    };


};

class Dog inherits Main {
    bark() : Object { out_string("wofe") };
};

class Skip inherits Dog {
    bark() : Object { out_string("arf") };
};
