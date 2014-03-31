class Main inherits IO {
    dog : Dog <- new Dog;
    dog1 : Dog <- dog.copy();
    main() : Object {
        case dog1 of 
            dog : Dog => out_string("Dog\n");
            str : String => 0;
        esac
    };
};

class Dog { x : String <- "Bark"; };