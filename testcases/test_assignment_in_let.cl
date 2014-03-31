class Main inherits IO {
    x : Int <- 5;

    main() : Object {
        let x : Int <- 1,
            y : Int <- x + 1,
            x : Int <- y + 1 in
        {
            out_int(x);
            out_int(y);
        }   
    };
};
