class Main inherits IO {
    main() : Object {
        {
            if isvoid( while 2 < 1 loop 5 pool) then out_string("Y\n") else out_string("N\n") fi;
            let t : Int <- 1 in 
                if isvoid( while t < 5 loop t <- t+1 pool) then out_string("Y\n") else out_string("N\n") fi;
        } 
    };
};

class Dog {};

class Cat{};