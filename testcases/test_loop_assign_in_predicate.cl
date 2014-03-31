class Main inherits IO {
    x : Int <- 3;

    main() : Object {
        {
            while let x : Int <- x in x = 3 loop
                x <- x + 1
            pool;
            out_int(x);
        }
    };
};
