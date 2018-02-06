(("name" . "simple-socket")
("version" . "0.0.2")
("description" . "a simple socket library for chez scheme")
("keywords")
("author" 
    ("evilbinary" "ch"))
("license" . "mit")
("private" . #f)
("scripts" 
    ("run" . "scheme --script")
    ("build" . "cd simple-socket && cd libsocket && make && make clean")
    ("repl" . "scheme")
    ("packing" . "raven pack simple-socket"))
("dependencies")
("devDependencies" 
    ("slib" . "3.2.5")
    ("scheme-lib" . "0.1.2")))