class Main inherits IO {
    main() : Object {
        {
            -- Looks at cools comparison operators
            if "aab" < "aac" then out_string("Y\n") else out_string("N\n") fi;
            if "aac" <= "aac" then out_string("Y\n") else out_string("N\n") fi;
            if "a" = "a" then out_string("Y\n") else out_string("N\n") fi;
            if 10 < 20 then out_string("Y\n") else out_string("N\n") fi;
            let x : Dog <- new Dog,
                y : Dog <- x,
                z : Dog <- new Dog,
                c : Cat <- new Cat,
                void : IO,
                void1 : IO in
                {
                    if x = y then out_string("Y\n") else out_string("N\n") fi;
                    if x = z then out_string("Y\n") else out_string("N\n") fi;
                    if c < z then out_string("Y\n") else out_string("N\n") fi;
                    if c <= z then out_string("Y\n") else out_string("N\n") fi;
                    if z < c then out_string("Y\n") else out_string("N\n") fi;
                    if c = void then out_string("Y\n") else out_string("N\n") fi;
                    if void1 = void then out_string("Y\n") else out_string("N\n") fi;
                    if isvoid(void1) then out_string("Y\n") else out_string("N\n") fi;
                    if isvoid(y) then out_string("Y\n") else out_string("N\n") fi;
                    if not x < z then out_string("Y\n") else out_string("N\n") fi;
                };
        } 
    };
};

class Dog {};

class Cat{};