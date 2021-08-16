## Overview

It is easy enough for Lisp to interact with the world, but how should the world interact with Lisp? There are several possibilities, such as: releasing a Lisp tool as a [command line app](https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/), making Lisp routines available via a REST API, or even interacting with a REPL directly. I'd like to instead consider making Lisp code available in the form of a *shared library*. And I want more than that: *I want the shared library to look as if it was written in reasonably idiomatic C*, even if the underyling implementation is Lisp.

In the following, we will consider a hypothetical Lisp project, `cl-calc`, which implements a small programming language for arithmetic expressions. The Lisp code includes, among other things, objects for representing syntax, and some basic routines (e.g. parsing and printing). Our goal is to distribute these in the form of a shared library, `libcalc`, along with declarations in a single header file `libcalc.h` written in idiomatic C. Although this is a toy problem, we hope that by carefully considering the implications of design choices here we may come to more broadly useful conclusions.



To allow ourselves space to dig in, this article will be split into three parts. Roughly, you can expect

1. Goofing around with ECL
2. A proposal: `ecl-simplelib`
3. Some specific issues: Python integration and Windows support

## ECL, the basics

In a certain sense, the hard work is already done for us: there is [already](https://common-lisp.net/project/ecl/) a Lisp implementation that can compile Lisp to a shared library. Let's look a bit more closely at ECL to understand how it may suit our needs. In what follows, we will consider a few example programs, demonstrating various aspects of ECL and its runtime.

#### Compiling our System

For starters, let's look at compiling `cl-calc` to a static library. We'll need to have ECL installed, and for convenience I also have quicklisp configured. On my system this means

```
brew install ecl
ecl --load ~/.quicklisp/setup.lisp
```

and then calling `(ql:add-to-init-file)` to add the standard boilerplate to my `~/.eclrc` file. Things might look a bit different on your system. In particular, Windows users will have to jump through some hoops to get the following examples to work. We defer discussion of Windows-specific behavior until a later post.



You should also have the [cl-calc repository](https://github.com/kilimanjaro/cl-calc) somewhere accessible to ASDF, e.g. in your quicklisp local projects. Then, you can easily compile the whole system to a static libary. Following the instructions from [the ECL manual](https://common-lisp.net/project/ecl/static/manual/System-building.html#Build-it-as-static-library-and-use-in-C), the incantation goes like this:

```
(asdf:make-build :cl-calc
                 :type :static-library
                 :mononithic t
                 :move-here #P"."
                 :init-name "init_calc")
```

This produces a static library named `cl-calc--all-systems.a`. As a note, `asdf:make-build` is both ECL-only and also technically deprecated (cf. [the source](https://github.com/fare/asdf/blob/490ee8c7dba3b63a2f39b8dd5faa9d4086cf976c/backward-interface.lisp#L231)), but for now it works for our purposes.



#### Using ECL

To see how this library can be used, let's consider a toy example. In what follows, we are going to simply construct and simplify an arithmetic expression, `(+ 5 6)`. 

```
/* example1.c */
#include <ecl/ecl.h>
#include <stdio.h>

extern void init_calc(cl_object);

int main(int argc, char **argv) {
  /* init */
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_calc);

  /* get symbols */
  cl_object int_literal = ecl_make_symbol("INT-LITERAL", "CL-CALC");
  cl_object int_literal_value = ecl_make_symbol("INT-LITERAL-VALUE", "CL-CALC");
  cl_object sum_expression = ecl_make_symbol("SUM-EXPRESSION", "CL-CALC");
  cl_object simplify = ecl_make_symbol("SIMPLIFY", "CL-CALC");

  /* do stuff */
  cl_object a = cl_funcall(2, int_literal, ecl_make_int(5));
  cl_object b = cl_funcall(2, int_literal, ecl_make_int(6));
  cl_object sum_expr = cl_funcall(3, sum_expression, a, b);
  cl_object reduced = cl_funcall(2, simplify, sum_expr);
  
  int sum = ecl_to_int(cl_funcall(2, int_literal_value, reduced));
  printf("5 + 6 = %d\n", sum);

  /* clean up */
  cl_shutdown();
  return 0;
}

```

To make sense of the above, note that `cl-calc` exposes a few types (cf. https://github.com/kilimanjaro/cl-calc/blob/master/src/calc.lisp). In particular, we have an `expression` type, with subclasses  `int-literal` (for representing immediate integers) and `sum-expression` (for representing sums of expressions). The high level logic of the example can be expressed in Lisp as

```
(let* ((a (int-literal 5))
    	 (b (int-literal 6))
		   (sum-expr (sum-expression a b))
		   (reduced (simplify sum-expr)))
  (format t "5 + 6 = ~D" (int-literal-value reduced)))
```



We can compile and run `example1.c` with

```
cc `ecl-config --cflags` -o example1 example1.c cl-calc--all-systems.a `ecl-config --ldflags` -lecl
./example1
```

which shows the output we expect.



#### Looking Closely at the Example

This example, although extremely simple, demonstrates a few characteristics of ECL. Let me summarize them below.

1. Lisp objects are exposed to C via the `cl_object` type. As is described in [the manual](https://common-lisp.net/project/ecl/static/manual/Manipulating-Lisp-objects.html#Manipulating-Lisp-objects), and seen here in [ecl/src/object.h](https://gitlab.com/embeddable-common-lisp/ecl/-/blob/develop/src/h/object.h#L109), this is a pointer to a C union. Since addressing on a system is usually done at 4 or 8-byte boundaries, ECL can use the bottom two bits of the pointer to encode type information about immediate values. In other words, ECL represents a fixnum as a pointer which is never dereferenced!
2. Coercing from C to Lisp values involves calling an constructor, which is often named something like `ecl_make_<type>`. In the above, we do this for both symbols and integers, using `ecl_make_symbol` and `ecl_make_int` respectively. The return value is a `cl_object`, which may represent an immediate value in a few cases, but which generically is just a pointer to some part of the Lisp managed heap.
3. Converting from Lisp values back to ordinary C values likewise usually involves a functional call, e.g. `ecl_to_int(cl_object integer)`. In some cases, we might need to do something more complicated (as we will see with strings in the next example).
4. Interaction with `cl-calc` specifically is performed by invoking `cl_funcall` with suitable symbols and arguments. In the call `cl_funcall(n, symbol, ...)`, the `n` signifies the number of arguments to `cl_funcall`. In general, ECL exposes Common Lisp functionality through ordinary C functions, with the convention that variadic Lisp functions correspond to variadic C functions which take an argument count as the first argument. 
5. Finally, we need to ensure we have correctly initialized both ECL as well as our module. We also should call `cl_shutdown` at the end (e.g. to ensure object finalizers get invoked, temporary files are deleted, etc).

It's worth remarking a bit further on (4). In particular, what ECL does not do is to provide `cl-calc` defuns as C functions. One can see this directly by inspecting the symbols exported from the shared library, using `nm` (showing only `g`lobal / exported systems, and excluding `U`ndefined symbols):

```
nm -gU cl-calc--all-systems.a
```

On my system, the result of this is

```

cl-calc--all-systems.a(eclinitUKOq0B.o):
0000000000000080 T _init_calc
0000000000000000 T _init_lib__ECLO6VIKY8DRCXVV_9W8KRX51
0000000000000100 T _main_lib__ECLO6VIKY8DRCXVV_9W8KRX51

cl-calc--all-systems.a(package.o):
0000000000000000 T __eclTv4f2M6WwBduV_ULmjRX51

cl-calc--all-systems.a(calc.o):
0000000000000000 T __eclm39yT04yhPmqV_6mmjRX51
```



### Strings and Error Handling

We now consider a second example, where we read a string, parse it to an expression, and print it.

```
/* example2.c */
#include <ecl/ecl.h>
#include <stdio.h>

extern void init_calc(cl_object);

int main(int argc, char **argv) {
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_calc);

  cl_object parse = ecl_make_symbol("PARSE", "CL-CALC");
  cl_object print = ecl_make_symbol("EXPRESSION-TO-STRING", "CL-CALC");

  char source[256];
  printf("> ");
  fgets(source, sizeof(source), stdin);

  cl_object expr = cl_funcall(2, parse, ecl_make_simple_base_string(source, -1));
  cl_object printed = cl_funcall(2, print, expr);

  char *result;
  result = ecl_base_string_pointer_safe(ecl_null_terminated_base_string(printed));

  printf("\n%s\n", result);

  cl_shutdown();
  return 0;
}

```

Note that converting an ECL string to a C `char*` type involves two stages: first we must coerce the Lisp value to a suitable form (with `ecl_null_terminated_base_string`) and then get a pointer (with `ecl_base_string_pointer_safe`). Although we don't consider the implications until the next section,  `result` ends up pointing into the Lisp heap.

As before, we can compile and run this with

```
cc `ecl-config --cflags` -o example2 example2.c cl-calc--all-systems.a `ecl-config --ldflags` -lecl
./example2
```

The following session works as expected (although the "pretty printer" could use some work):

```
> (+ (+ 3 1) 4)

(+
  (+ 3 1)
  4)
```

Note that this is not a Lisp expression, but a `cl-calc` expression, consisting of a sum of two integer literals!



Now, what happens if we make an innocent mistake, e.g. we try to enter `foo`, which is not valid `cl-calc` syntax.

```
> foo

Condition of type: SIMPLE-ERROR
Unable to parse expression: FOO
No restarts available.

Top level in: #<process TOP-LEVEL 0x10b50ef80>.
> ^D

;;;
;;; Detected access to protected memory, also known as 'bus or segmentation fault'.
;;; Jumping to the outermost toplevel prompt
;;;


```

Not good! Not only has ECL hijacked the process, leaving us in a REPL, but when we try to get out of this via Ctrl-D we end up segfaulting. 



In general, we shouldn't expect much out of this situation: it was a real error, and there is no way our program could proceed as-is. However, we could do a better job of handling this. One might consider trying to define suitable restarts on the Lisp side, but seeing as how in this example it was the caller that goofed, perhaps it's enough to print a meaningful error message and abort gracefully.



#### Handler Case (ECL style)

Below, we consider a slightly modified form of the earlier example. 

```
/* example3.c */
#include <ecl/ecl.h>
#include <stdio.h>
#include <stdlib.h>

extern void init_calc(cl_object);

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

#define ecl_to_charp(x) (ecl_base_string_pointer_safe(ecl_null_terminated_base_string(x)))

int main(int argc, char **argv) {
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_calc);

  cl_object error_list = cl_list(1, ecl_make_symbol("T", "COMMON-LISP"));

  cl_object parse = ecl_make_symbol("PARSE", "CL-CALC");
  cl_object print = ecl_make_symbol("EXPRESSION-TO-STRING", "CL-CALC");

  char source[256];
  printf("> ");
  fgets(source, sizeof(source), stdin);

  char *result;

  ECL_HANDLER_CASE_BEGIN(ecl_process_env(), error_list) {
    cl_object expr = cl_funcall(2, parse, ecl_make_simple_base_string(source, -1));
    cl_object printed = cl_funcall(2, print, expr);
    result = ecl_to_charp(printed);
  }
  ECL_HANDLER_CASE(1, condition) {
    cl_object err = cl_format(3, ECL_NIL, ecl_make_simple_base_string("~A", -1), condition);
    die(ecl_to_charp(err));
  } ECL_HANDLER_CASE_END;

  printf("\n%s\n", result);

  cl_shutdown();
  return 0;
}

```



We make a few notes about what we've done above:

1. We define a list of error names in `error_list`. For now, we will just handle any Lisp error the same: we will print the error message and `abort`.
2. The heart of the new code is that we wrap the `cl_funcall`calls in an `ECL_HANDLER_CASE` macro.  [This macro](https://common-lisp.net/project/ecl/static/manual/Conditions.html) lets us run the code in he `ECL_HANDLER_CASE_BEGIN` block  with handlers bound to the names in `error_list`.
3.  `ECL_HANDLER_CASE(1, condition)` form indicates what to do for errors matching the first name in `error_list`. Here the condition object itself is bound to the C name `condition`, and we choose to print it to a Lisp string and then coerce this to a C string with our `ecl_to_charp` macro. 

Reproducing the previous error, we get a much more reasonable result:

```
> foo
Unable to parse expression: FOO
```



### The Lisp Heap and Garbage Collection

One issue of critical importance to consider is how the Lisp heap is managed, and in particular how garbage collection works. ECL uses the [Boehm garbage collector](https://en.wikipedia.org/wiki/Boehm_garbage_collector), which has a few important properties which we note here.

1. The Boehm GC works by explicitly traversing  object references recursively, marking blocks in heap memory which are reachable from a *root set*, and then freeing blocks with no marks (cf. [these graphics](https://en.wikipedia.org/wiki/Tracing_garbage_collection#Na%C3%AFve_mark-and-sweep) for a rough example). In particular, the Boehm GC is non-moving: *live objects*, i.e. those reachable from the root set, are not moved. This means any `cl_object` values which the GC knows about are safe!

2. The root set includes the stack of the process embedding ECL. In other words, the `cl_object` values we manipulate in the C programs above are tracked by the Boehm GC. 
3. As noted in [the manual](https://common-lisp.net/project/ecl/static/manual/Memory-Management.html#Boehm_002dWeiser-garbage-collector), the GC does *not* track the data sectors by default (e.g.  `static` variables in C). Nor is the C heap tracked by default. We can, however, tell the GC to explicitly track certain addresses as root addresses, via the `ecl_register_root` function (cf. [src/h/external.h](https://github.com/asceth/ecl/blob/master/src/h/external.h#L768)).



Point (3) is very important. On the one hand, working with `cl_object`s directly is certainly convenient when they are all sitting on the C stack. In the general case, however, we must track all such references and keep the GC informed of their existence in order to not invalidate them by a garbage collection. Likewise, we must let the GC know when we are done with them.



It's also worth recalling our ultimate goal here: we want to create a shared library which looks indistinguishable from one written in reasonabbly idiomatic C. In this case, slopping `cl_object` values around is not generally a good practice. What we'd really like are *opaque values* which are manipulated by a functional interface, and which do not reveal the inner guts of implementation details (e.g. the fact that we are using ECL).

#### Handles to the Rescue

Rather than work with `cl_object` values directly, we instead prefer to introduce the notion of a *handle*. Here, a handle is a `void*` object whith the following operations

- they can be constructed from a `cl_object`
- they can be dereferenced to recover the underlying `cl_object`
- they can be *released*, which indicates to the GC that the reference is no longer live. 

One possible implementation looks something like this (with irrelevant pieces omitted):

```
...

typedef void* calc_expr;

static cl_object _handles;
static int _handle_count = 0;

void* make_handle(cl_object obj)
{
  cl_object id = ecl_make_fixnum(_handle_count++);
  si_hash_set(id, _handles, obj);
  return (void*)id;
}

#define dereference_handle(x) (cl_gethash(2, ((cl_object)x), _handles))
#define release_handle(x) (cl_remhash(dereference_handle(handle), _handles))

int main(int argc, char **argv) {
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_calc);
  
  _references = cl_make_hash_table(2, 
                  ecl_make_symbol("TEST", "KEYWORD"), 
                  ecl_make_symbol("EQ", "COMMON-LISP"));
  ecl_register_root(&_references);
  ...
}
```

A few notes

1. Our handle type is the *opaque* `calc_expr` type. This is what we want the library user to work with.
2. A *handle* will be a `void*`, which is simply a `cl_object` fixnum with the type forgotten. Recalling that for ECL a fixnum is an immediate value, the corresponding `void*` does not point to the Lisp heap. In particular, we can be sure that handles will not be invalidated unless we want them to be. 
3. We have a static `_handles` object, which we assign to a hash table. We register the address of  this with the GC as a root. This will be a table mapping fixnums to lisp objects. If the GC knows about this table, then it knows about all of the values in the table, i.e. all of the objects which we have handles for.
4. To construct a handle, we generate a unique id (using `_handle_count`), and then call `si_hash_set(id, _handles, obj)`. This is the C equivalent of `(setf (gethash id _handles) obj)`. The handle is returned as a void pointer. 
5. We have a macro `dereference_handle` which does just that: it lets us gets the object associated with a handle.
6. We have a macro `release_handle` which clears the handle from the `_handles` table.



We indicate the actual usage of handles in the context of our (still hypothetical) shared library in the next section.



## Putting it together: A Proposal



Having explored ECL a bit, let us return to our initial goal of creating a shared library, `libcalc`, which exposes the `cl-calc` functionality in a simple and idiomatic manner. We summarize a few findings we have made or conventions we will adopt:

1. We will use ECL to do the hard work of compiling to a library. ECL itself will not make nice C-style interfaces for us, but we can start with a static C library and then define shims which wrap underlying `cl_funcall` forms. 

2. We will expose a few types: either ordinary C types (e.g. `int` or `char*`) or opaque `void*` types. Under the hood, we will systematically coerce between ordinary C types and their Lisp counterparts, whereas for the opaque types we simply use handles to `cl_object` values. 

3. We expect that any handle must be explicitly released when the caller is done with it.
4. In order to deal with Lisp errors, we will wrap funcalls in a `ECL_HANDLER_CASE` macro. This requires identifying a list of error types to handle. Whereas in the above example we simply aborted on errors, ing general we will translate error types to return codes, and generate error messages from the corresponding Lisp condition.
5. We will produce the shared library via the static library + a C file, `libcalc.c`, which implements the above. We will also make available a header file, `libcalc.h`, which exposes the interface of the library.



For example, the `parse` function of `cl-calc` might be exposed as

```
/* hypothetical libcalc.h */

...

#define SET_ERROR(msg) (...) /* stash an error code somewhere */

typedef void* calc_expr;
typedef enum { ERR_SUCCESS, ERR_FAIL } err_t;

...

err_t calc_parse(char* source, calc_expr* result)
{
  ECL_HANDLER_CASE_BEGIN(ecl_process_env(), _error_list) {
    cl_object _source = ecl_make_simple_base_string(source, -1);
    cl_object _result = cl_funcall(2, _parse, _source);
    *result = make_handle(_result);
  } ECL_HANDLER_CASE(1, _condition) {
    SET_ERROR(cl_format(3, ECL_NIL, ecl_make_simple_base_string("~A", -1), _condition));
    return ERR_FAIL;
  } ECL_HANDLER_CASE_END;
  return ERR_SUCCESS;
}

...

```

where `_parse` is an actual `cl_object` storing the required symbol, etc. 



With this approach, a rewrite of `example3.c` might look like

```
/* hypothetical rewrite of example3.c */
#include "libcalc.h"
#include <stdio.h>
#include <stdlib.h>

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

int main(int argc, char **argv) {
  calc_init(argc, argv);

  char source[256];
  printf("> ");
  fgets(source, sizeof(source), stdin);
  
  calc_expr expr;
  if (calc_parse(source, &expr) != ERR_SUCCESS) {
    die("unable to parse expression");
  }
  
  char *result;
  if (calc_expression_to_string(expr, &result) != ERR_SUCCESS) {
    die("unable to print expression to string");
  }

  printf("\n%s\n", result);

  release_handle(expr);
  return 0;
}
```



In the next installment, we will take this sketch of an idea and actually implement it.