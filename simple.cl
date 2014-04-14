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
    l : Int <- j;
    m : Int <- l <- 18;
    n : Object <- new IO;
    o : Int <- if true then 5 else 8 fi;
    p : Object <- while false loop 9 pool;
    q : Int <- { 8; 0;  };
    r : Object <- { out_string("Hello"); } ;
    s : Object <- { self@Object.type_name(); }; 
    t : Object <- { self.copy(); };
    meth( a : Int ) : Int { a };
    main() : Object {
        let x1 : Int <- 4,
            y1 : String,
            b1 : Int <- h + i
        in out_string(x)
    };
};
