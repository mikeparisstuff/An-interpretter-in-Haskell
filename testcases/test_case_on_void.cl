class Main {
    empty : IO; -- is void

    main() : Object {
        case empty of 
            o : Object => 0;
            otha : IO => 0;
        esac
    };
};
