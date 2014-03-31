class Main inherits IO {
    main() : Object {
        {
            -- Looks at cools comparison operators
            if "aab" < "aac" then out_string("Y\n") else 0 fi;
            if "aac" <= "aac" then out_string("Y\n") else 0 fi;
            if "a" = "a" then out_string("Y\n") else 0 fi;
            if 10 < 20 then out_string("Y\n") else 0 fi;
            let x : Dog <- new Dog,
                y : Dog <- x,
                z : Dog <- new Dog in
                {
                    if x = y then out_string("Y\n") else 0 fi;
                    if not x = z then out_string("Y\n") else 0 fi;
                };
        } 
    };
};

class Dog {};
