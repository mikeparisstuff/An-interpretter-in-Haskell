class Main inherits IO {
    main() : Object {
        {

        	let t : Object <- if true then 5 else "hello" fi in
            out_string(t.type_name());

            let t : Object <- if false then 23 else "Yossarion" fi in
            out_string(t.type_name());
        }

    };
};