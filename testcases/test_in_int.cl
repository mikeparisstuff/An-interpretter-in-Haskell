class Main inherits IO {
    main() : Object {
        let x : Int <- in_int(),  -- the input has crap at the end
            y : Int <- in_int(),  -- y is a negative int
            too_big : Int <- in_int(), -- all malformed ints return 0
            too_small : Int <- in_int(),
            malformed : Int <- in_int() in
            {
                out_int(x); 
                out_int(y); 
                out_int(too_big); -- should output 0
                out_int(too_small);
                out_int(malformed);
            }

    };
};
