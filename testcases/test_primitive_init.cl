class Main inherits IO {
    i : Int;
    s : String;
    b : Bool;

    main() : Object {
        {
            -- what follows are the valid default values for each primitive
            if i = 0 then out_string("int works\n")
            else out_string("int is broken\n") fi;

            if s = "" then out_string("String works\n")
            else out_string("String is broken\n") fi;

            if not b then out_string("Bool works\n")
            else out_string("Bool is broken\n") fi;
        }
    };
};
