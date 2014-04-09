class Main inherits IO {
    x : String <- "hello";
    y : String;
    a : Int <- ~4;
    b : Int <- 5 * 2;
    c : Bool <- true;
    d : Bool <- not false;
    e : Object <- 7 = 8;
    f : Object <- 2 < 3;
    g : Object <- 4 <= 5;
    h : Int <- 3 + 2;
    i : Int <- 8 - 2;
    j : Int <- 4 / 2;
    k : Bool <- isvoid(5);
    main() : Object {
        abort()
    };
};
