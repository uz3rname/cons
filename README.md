# cons
cons is an exprerimental, statically typed, compiled programming language with some support for type inference and polymorphism. There is no specification (yet). Syntactically it is close to Scheme and code examples should give you an idea. The compiler is implemented in Scheme, has support for tail call optimization and call/cc (though, it has never been tested heavily). It generates code for GNU assembler and uses it as a backend. Only Linux/amd64 is supported (for now).

## Build prerequisites
* Chicken Scheme (v. 5.x)
* GCC
* perl
* libelf
* boehm-gc (optional, though, there won't be any garbage collecting without it, cause internal memory manager is not implemented)

## Building
```
./configure && make
```
You may want to point script to chicken csc binary with CSC env variable. ./bin/consc is your resulting compiler binary. Running it with -h argument will give you info about usage.

```
make test
```

will build and run additional tests.
