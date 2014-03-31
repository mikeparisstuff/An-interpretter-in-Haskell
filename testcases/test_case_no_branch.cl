class Main {
    dog : Dog <- new Dog;

    main() : Object {
        case dog of 
            io : IO => 0;
            str : String => 0;
        esac
    };
};

class Dog { };
