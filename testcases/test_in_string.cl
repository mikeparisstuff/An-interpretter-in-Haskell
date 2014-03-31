class Main inherits IO {
    main() : Object {
        let big_s : String <- in_string(), -- a big ass string
            empty_s : String <- in_string(), -- nothing but a newline
            tb_s : String <- in_string(), -- a string with tabs in it
            nl_s: String <- "\n \\t\nThis is a \\n \n a new \n new \n string\n" in -- newlines should be compressed
            {
                out_string(big_s);
                out_string(empty_s);
                out_string(tb_s);
                out_string(nl_s);
            }
    };
};
