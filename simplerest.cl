class Main inherits IO{
	x : Int;
	y : Int <- x;
	z : Int <- 5;
	a : Int <- z + x;
	main() : Object {
		(new IO).out_int(a)
	};
};