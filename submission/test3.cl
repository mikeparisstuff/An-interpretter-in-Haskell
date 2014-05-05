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
                    if x = y then out_string("X = Y -- Y\n") else out_string("X = Y -- N\n") fi;
                    if x = z then out_string("X = Z -- Y\n") else out_string("X = Z -- N\n") fi;
                    if c < z then out_string("X < Z -- Y\n") else out_string("X < Z -- N\n") fi;
                    if c <= z then out_string("X <= Z -- Y\n") else out_string("X <= Z -- N\n") fi;
                    if z < c then out_string("Z < X -- Y\n") else out_string("Z < X -- N\n") fi;
                    if c = void then out_string("X = void -- Y\n") else out_string("X = void -- N\n") fi;
                    if void1 = void then out_string("void1 == void -- Y\n") else out_string("void1 == void -- N\n") fi;
                    if isvoid(void1) then out_string("isvoid(void1) -- Y\n") else out_string("isvoid(void1) -- N\n") fi;
                    if isvoid(y) then out_string("isvoid(y) -- Y\n") else out_string("isvoid(y) -- N\n") fi;
                    if not x < z then out_string("not x < z -- Y\n") else out_string("not x < z -- N\n") fi;
                };
        }
    };
};

class Dog {};

class Cat{};