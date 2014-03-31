class Main inherits IO {
    -- a testcase that creates 2500 objects in the heap
    -- tests a weird corner case of activation record counting
    main() : Object {
        {
            recurse(500);
            out_string("We never exceeded 1000 objects in scope\n");
        }
    };

    recurse(left : Int) : Int {
        let x : D <- new D, 
            y : D <- new D,
            z : D <- new D,
            w : D <- new D,
            v : D <- new D in
        if left = 0 then 0
        else recurse(left - 1) fi
    };
};

class D {};
