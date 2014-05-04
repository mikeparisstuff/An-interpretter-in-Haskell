class Main inherits IO {
    main() : Object {
        let g : Object <- out_string("BLAHBLAH\n"),
            fido : Dog <- new Dog
        --    fido2 : Dog <- fido.copy()
            in
    {
         --out_int(fido2.getX());
         0;

    }
    };

};

class Dog {
    x : Int <- 17;
    
    setX() : Object {
        x <- x + 1
    };
    getX() : Int {
        x
    };
};
