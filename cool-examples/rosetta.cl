Class Main inherits IO {

	main() : Object {
		let 
			l: List <- new NilAction, 
			done : Bool <- false,
			math : Math <- new Math,
			t : List <- new NilAction
		in
		 {
		 	let count : Int <- 0 in
		 	let last_element : String <- "" in
		 	let s : String <- "" in
			while not done loop {
				s <- in_string ();

				if s = "" then 
					done <- true 
				else
					l <- l.insert(s)
				fi;

				if math.mod(count, 2) = 1 then
					{
						l <- l.insert_dependency(last_element, s);
					}
				else
					out_string("")
				fi;
				last_element <- s;
				count <- count + 1;
			} pool;
			--l.print_list();
			--out_string("\n");
			
			t <- l.sorted_list( (new NilAction), l.count());
			t.print_reverse();
			--l.print_list ();
		}
	};
};

(* The List type is not built in to Cool, so we'll have to define it 
 * ourselves. Cool classes can appear in any order, so we can define
 * List here _after_ our reference to it in Main. *) 
Class List inherits IO { 
        
	dependency(hd : String) : Dependency { 
	  let new_cell : Dependency <- new Dependency in
		new_cell.init(hd,self)
	};

	insert(i : String) : List { self };

	print_list() : Object { abort() };

	head() : String {"List"};

	insert_dependency(action: String, dependency : String) : List {self};

	print_sorted() : String {"Hi"};

	decrement_incoming_count(s : String) : Object {true};

	vertex_of_indegree_zero(l : List) : String {
		{
			--out_string("Call on List");
			"Blah";
		}
	};

	sorted_list(l : List, count : Int) : List {new NilDependency};

	count() : Int {0};

	contains(s : String) : Bool {false};

	remove_node(s : String) : Bool {false};

	print_reverse(): Object {true};
} ;

Class Action inherits List {
	head : String;
	tail : List;
	dependencies : List;
	incoming : Int;

	init(hd : String, tl : List, dependency : List, inc : Int) : Action {
		{
			head <- hd;
			tail <- tl;
			dependencies <- dependency;
			incoming <- inc;
			self;
		}
	};

	count() : Int {
		if	head = "NilAction" then
			1
		else
			1 + tail.count()
		fi
	};

	set_incoming(n : Int) : Int {
		incoming <- n
	};

	get_incoming() : Int {
		incoming
	};

	insert(s : String) : List {
		if (s = head) then
			{
				--out_string("Returning self on insert for node: ");
				--out_string(head);
				--out_string("\n");
				self;
			}
		else 
			if (head = "NilAction") then
			{
				--out_string("Creating new node for: ");
				--out_string(head);
				--out_string("\n");
				(new Action).init(s, (new NilAction), (new NilDependency), 0);
			}
			else
			{
				(new Action).init(head, tail.insert(s), dependencies, incoming);
			}
			fi
		fi
	};

	insert_dependency(action: String, dependency : String) : List {
		if (head = action) then 
			{
				--out_string("Inserting dependency: ");
				--out_string(dependency);
				--out_string("\n");
				--out_string("For action: ");
				--out_string(head);
				--out_string("\n");
				incoming <- incoming + 1;
				dependencies <- dependencies.insert(dependency);
				--out_int(incoming);
				--out_string("\n\n");
				self;
			}
		else
		{
			tail.insert_dependency(action, dependency);
			self;
		}
			--(new Action).init(head, tail.insert_dependency(action, dependency), (new NilDependency), incoming)
		fi
	};

	contains(s : String) : Bool {
		if head = "NilAction" then
		{
			--out_string("Item not found");
			false;
		}
		else
			if head = s then
			{
				--out_string("Item found");
				true;
			}
			else
			{
				--out_string("Searching Action");
				tail.contains(s);
			}
			fi
		fi
	};

	head() : String {head};

	vertex_of_indegree_zero(l: List) : String {
		{
			--out_string("Call on List");
			if incoming = 0 then
				if tail.head() = "NilAction" then
					{
						--l <- (new Dependency).init(head, l);
						l <- l.insert(head);
						l.head();
					}
				else
					{
						--out_string("Taking node: \n");
						--out_string(head);
						--out_string("\n\n");
						--tail.vertex_of_indegree_zero((new Dependency).init(head, l));
						tail.vertex_of_indegree_zero(l.insert(head));
					}
				fi
			else
				if tail.head() = "NilAction" then
					if l.head() = "NilDependency" then
						"CYCLE"
					else
						l.head()
					fi
				else
				{
					--out_string("Skipping Node: \n");
					--out_string(head);
					--out_string("\n\n");
					tail.vertex_of_indegree_zero(l);
				}
				fi
			fi;
		}
	};

	decrement_incoming_count(s : String) : Object {
		{
		if dependencies.contains(s) = true then
			{
				--out_string("Decrementing value of node ");
				incoming <- incoming - 1;
				--out_string(head);
				--out_string("\n");
				--out_string(" to: ");
				--out_int(incoming);
				--out_string("\n");
				tail.decrement_incoming_count(s);
				--if tail.head() = "NilAction" then
				--	self
				--else
				--	tail.decrement_incoming_count(s)
				--fi;
				--(new Action).init(head, tail, dependencies);
			}
		else
			{
				tail.decrement_incoming_count(s); --(new Action).init(head,tail.decrement_incoming_count(s), dependencies)
			}
		fi;
		}	
	};

	remove_node(s : String) : Bool {
		if head = s then
		{
			incoming <- 0 - 1;
			true;
		}
		else
		{
			tail.remove_node(s);
			false;
		}
		fi
	};

	sorted_list(l : List, count : Int) : List {
		if	count = 0 then
			l
		else
			let vertex : String <- self.vertex_of_indegree_zero(new NilDependency) in
			if	vertex = "CYCLE" then
			{
				out_string("cycle\n");
				(new NilDependency);
			}
			else
				let t : Action <- new Action in
				{
					t.init(vertex, l, (new NilDependency), incoming);
					self.decrement_incoming_count(vertex);
					self.remove_node(vertex);
					self.sorted_list(t, count - 1);
				}
			fi
		fi
	};

	print_reverse(): Object {
		if tail.head() = "NilAction" then
			{
				out_string(head);
				out_string("\n");
			}
		else
			{
				tail.print_reverse();
				out_string(head);
				out_string("\n");
			}
		fi
	};

	print_list() : Object {
		{
		     out_string(head);
		     out_string("\n");
		     dependencies.print_list();
		     tail.print_list();
		}
	};

};

Class Dependency inherits List { -- a Dependency cell is a non-empty list 
	head : String;          -- head is the contents of the list head 
	tail : List;            -- tail is the rest of the list

	init(hd : String, tl : List) : Dependency {
	  {
	    head <- hd;
	    tail <- tl;
	    self;
	  }
	};
	  
        (* insert() does insertion sort (using a reverse comparison) *) 
	insert(s : String) : List {
		if not (head < s) then          -- sort in reverse order
			(new Dependency).init(s,self)
		else
			(new Dependency).init(head,tail.insert(s))
		fi
	};

	head() : String {head};

	insert_dependency(action: String, dependency : String) : List { self };

	print_list() : Object {
		{
			out_string("== ");
		    out_string(head);
		    out_string("\n");
		    tail.print_list();
		}
	};

	contains(s : String) : Bool {
		if head = s then
			{
				--out_string("Item found");
				true;
			}
		else
			if tail.head() = "NilDependency" then
			{
				--out_string("Item not found\n");
				false;
			}
			else
			{
				--out_string("Searching  ");
				--out_string(head);
				--out_string("\n");
				tail.contains(s);
			}
			fi
		fi
	};
};

Class NilAction inherits List { -- Nil is an empty list 

	insert(s : String) : List { (new Action).init(s,self, (new NilDependency), 0) }; 

	print_list() : Object { true }; -- do nothing 

	head(): String {"NilAction"};

} ;

Class NilDependency inherits List { -- Nil is an empty list 

	insert(s : String) : List { (new Dependency).init(s,self) }; 

	print_list() : Object { true }; -- do nothing 

	head(): String {"NilDependency"};
} ;

Class Math inherits Object {
	mod(n : Int, m : Int) : Int{
		if 0 <= n - m then
			mod(n-m, m)
		else
			n
		fi
	};
};