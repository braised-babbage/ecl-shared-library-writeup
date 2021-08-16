## The Story So Far

In our previous discussion, we established a problem to be solved: how can we produce a shared library from a Lisp system? We did not answer this question in that article, but instead looked at what [ECL](https://common-lisp.net/project/ecl/) can offer us. 

Although ECL is up to the task of compiling Lisp to C, by default the resulting library does not look like a library that an "ordinary" user expects. In particular, our library functions are not exported (but must instead be invoked via `cl_funcall`), Lisp types are exposed via `cl_object` values, error handling is a bit rough around the edges, and we will need to do some GC bookeeping in order to safely refer to objects in the Lisp heap. A motivation threaded through that discussion, as well as the discussion today, is that we hope to produce something reasonably idiomatic. In other words, our shared library should not be a "Lisp ghetto" with ugly code or awkward interfaces, but rather the artifacts produced should be legibile to the system at large (e.g. so that we can load them easily via the FFI of some other language).

All of this is doable with ECL, but not something we wish to do by hand. Instead, we propose to generate  C code which does this from a higher level description. 



## What We Expect to Produce

We therefore hope to produce two files, a `libcalc.c` which contains "stub functions" taking care of all of the funny-business mentioned above, and a `libcalc.h` which serves as a specification of the "interface" of our library. It's worth articulating in a bit more detail what `libcalc.h` should include.



First, we expect that it defines the *types* which our library exposes, e.g. an enum defining error codes, handles for Lisp objects, etc.



Second, we expect that it declares a set of functions which are exported from the shared library. These declarations will more or less look like (for a hypothetical function `cl-calc::foo`)

```
err_t calc_foo(type1 arg1, ..., typeN argN, rtype* result)
```

and should be called like

```
rtype result;
if (calc_foo(arg1, ..., argN, &result) != ERR_SUCCESS) {
  /* do something */
}
```

The `calc_foo` function is responsible for coercing C values to Lisp values (for the arguments) and back (for the result). It should also handle any conditions, translating them to corresponding error codes.



Third, we expect a few administrative routines (the "standard boilerplate"):

- `int calc_init()`, which starts ECL and sets up whatever state is needed
- `int calc_shutdown()`, which stops ECL
- `char* calc_error_message()` which recovers the last recorded error message
- `void calc_release_handle(void*)` which releases a handle.
- `_Bool calc_handle_eq(void*, void*)` which can be used to check whether two handles are equal.



## Enter ECL-SIMPLELIB

The basic premise of [ecl-simpelib](https://github.com/kilimanjaro/ecl-simplelib) is that we should be able to express libraries of this sort in a declarative manner, and then generate the actual artifacts (e.g. `libcalc.dylib` and `libcalc.h`) from this declarative description. Before showcasing how it can be used for `libcalc`, we discuss a few general aspects about its design and implementation.



### Types and all that jazz

Recall that one of the basic duties of our "stub functions" proposed last time is to coerce between Lisp and C types, so that a caller can provide ordinary values (e.g. `int`), but that the underlying ECL functions still receive appropriate `cl_object` values.



In general, `ecl-simplelib` represents types either via keywords (e.g. `:int`) or by more general compound objects. The details are all encoded in four methods:

- `(type-specifier <type>)` returns a C fragment specifying the C type, e.g. `"err_t"`
- `(c-to-lisp <expr> <type>)` takes a C fragment `<expr>` representing an ordinary C value of type `<type>`, and returns a new fragment with the value coerced appropriately to a `cl_object`
- `(c-from-lisp <expr> <type>)` takes a C fragment `<expr>` representing an encoded Lisp value of type `<type>`, and returns a new fragment with the value coerced back to an ordinary C value
- `(type-definition <type>)` contains the C definition of `<type>`, if necessary.

**Note**: By "C fragment", we mean a *Lisp string representing a textual fragment of a C program*. The primary job of `ecl-simplelib` is to emit C code, and it works with C expression, declarations, etc simply as Lisp strings.

For example, consider the behavior with respect to integers

```
ECL-SIMPLELIB> (type-specifier ':int)
"int"
ECL-SIMPLELIB> (c-to-lisp "foo" ':int)
"ecl_make_int(foo)"
ECL-SIMPLELIB> (c-from-lisp "foo" ':int)
"ecl_to_int(foo)"
```

Note that, since ints are built-in types in C, there is no `type-definition`.

#### Handles and Custom Types

In general, new types can be introduced simply by adding methods on the above generics. Because their use is so ubiquitous, `ecl-simplelib` provides structures for expressing handles and enums. We show an example of a handle below.

```
ECL-SIMPLELIB> (defvar expr-type
		 (make-handle-type :name "calc_expr"))
EXPR-TYPE
ECL-SIMPLELIB> (type-specifier expr-type)
"calc_expr"
ECL-SIMPLELIB> (c-to-lisp "foo" expr-type)
"__dereference_handle(foo)"
ECL-SIMPLELIB> (c-from-lisp "foo" expr-type)
"__make_handle(foo)"
ECL-SIMPLELIB> (type-definition expr-type)
"typedef void *calc_expr"
```

In the above, `__dereference_handle` and `__make_handle` refer to C macros which `ecl-simplelib` uses for handle management.



### Error Maps and Error Messages

Another duty of our stub functions is to translate Lisp conditions to C error codes. Recalling our earlier discussion, this is done by wrapping ECL's `cl_funcall` functions in the `ECL_HANDLER_CASE` macro. The simplest possible example of this, in which all errors translate to the same error code, is annotated below. We use another `ecl-simplelib` type constructor, `make-enum-type`.

```
(defvar error-type
  (make-enum-type
   :name "err_t"              ; the type name
   :alist '(("ERR_SUCCESS" 0) ; an alist mapping codes to numeric values
            ("ERR_FAIL" 1))))

(defvar error-map
  (error-map          ; a MACRO for expressing error maps
      error-type      ; the error type
      "ERR_SUCCESS"   ; a C fragment encoding the value to use on "success" (i.e. no conditions)
    (t (expr) "ERR_FAIL"))) ; HANDLER-CASE error clauses. here, regardless of the condition, we report ERR_FAIL
```

Note that in general, the error value can depend in more complicated ways on the underlying condition. In the above, `expr` is a C fragment whose value will be the corresponding `cl_object` storing the condition. We choose to ignore it, since we will be able to get an error message by other means.

In terms of C code, the above generates stubs that look roughly as follows

```
#define ERROR_LENGTH 256
static char _error_message[ERROR_LENGTH];
#define SET_ERROR(x) (strncpy(_error_message, ecl_base_string_pointer_safe(ecl_null_terminated_base_string(x)), ERROR_LENGTH-1))

...

err_t calc_foo(...)
{
  ECL_HANDLER_CASE_BEGIN(ecl_process_env(), cl_list(1, ecl_make_symbol("T", "COMMON-LISP"))) {
    ... /* coerce types, do funcall, & set result */
  } ECL_HANDLER_CASE(1, _condition) {
    SET_ERROR(cl_format(3, ECL_NIL, ecl_make_simple_base_string("~A", -1), _condition));
    return ERR_FAIL;
  } ECL_HANDLER_CASE_END;
  return ERR_SUCCESS;
}
```

Note that, in addition to returning an error code, an actual message is also available.



### Library Specification

At the heart of `ecl-simplelib` is the `library` class (cf. [simplelib.lisp](https://github.com/kilimanjaro/ecl-simplelib/blob/master/src/simplelib.lisp)). At a high level, this consists of

- some metadata (e.g. the library name, a path to store the build artifacts, an error map, etc)
- a sequence of `spec-statement` objects, which control the actual C code that is generated.



In general, the meaning of a spec statement is defined by two methods:

- `(write-to-c-header stmt lib stream)` writes a fragment of the C header corresponding to `stmt` to `stream`
- `(write-to-c-source stmt lib stream)`writes a fragment of the C source file corresponding to `stmt` to `stream`.



`ecl-simplelib` provides a variety of spec statements. In general, these may depend on the library metadata (hence the need for the `lib` argument), but some are simple enough so as not to. For example, `preprocessor-statement` specifications simply indicate C preprocessor lines to be emitted in the header but not the source:

```
ECL-SIMPLELIB> (defvar pp (preprocessor-statement "define" "FOO 0"))
PP
ECL-SIMPLELIB> (write-to-c-header PP nil *standard-output*)
#define FOO 0
NIL
ECL-SIMPLELIB> (write-to-c-source PP nil *standard-output*)
NIL
```



We briefly list the variety of spec statements below, along with their DSL syntax (to be introduced later).

| Name                     | Code-generation Behavior                                     | DSL Syntax                                 |
| ------------------------ | ------------------------------------------------------------ | ------------------------------------------ |
| `preprocessor-statement` | Emit a C preprocessor statement (header only).               | `:include`, `:ifndef`, `:define`, `:endif` |
| `literal-statement`      | Emit a C fragment (header only).                             | `:newline`, `:section`, `:text`            |
| `:standard-boilerplate`  | Emit the declarations (header) and definitions (source) of the "standard boilerplate" functions. | `:init-function`                           |
| `type-declaration`       | Emit the the declaration or definition of a custom type (header only). | `:type`                                    |
| `function-declaration`   | Emit the declaration (header) and definition (source) of a C stub, wrapping an ECL-compiled Lisp function. | `:function`                                |

Two spec statements are worth looking at in a bit more detail: `type-declaration` and `function-declaration`.

#### Type Declarations

For custom types (e.g. a handle type, or an enum), we need to emit an appropriate type declaration in the library header. This is done with the `type-declaration` spec statement, whose definition we show below

```
(defclass type-declaration (spec-statement)
  ((type :initarg :type
         :reader type-declaration-type))
  (:documentation "A type declaration."))

(defmethod write-to-c-header ((stmt type-declaration) lib stream)
  (declare (ignore lib))
  (let* ((type (type-declaration-type stmt))
         (defn (type-definition type)))
    (unless defn
      (error "Missing definition for type ~A. Either provide one, or remove the corresponding :TYPE directive." type))
    (format stream "~A;~%" defn)))

(defmethod write-to-c-source ((stmt type-declaration) lib stream)
  (declare (ignore stmt lib stream))
  nil)
```

As with preprocessor statements, type declarations are only emitted in the header. 

#### Function Declarations

The bulk of most library definitions will be function declarations, indicating to `ecl-simplelib` that a Lisp function (compiled by ECL) should have a corresponding stub function generated for export from the shared library.

```
(defclass function-declaration (spec-statement)
  ((name :initarg :name
         :reader function-declaration-name)
   (return-type :initarg :return-type
                :reader function-declaration-return-type)
   (args :initarg :args
         :reader function-declaration-args))
  (:documentation "A function declaration."))
```

Here, `args` is an association list mapping argument names to their types. Note that function declarations contain just enough information to describe a stub. In particular, we do not in any way rely on details of ECL's compilation process, but instead simply produce a stub which manipulates ECL data structures and which relies on the underlying ECL funcall for invoking the Lisp function. 

### The DEFINE-LIBRARY macro

Although libraries may be constructed explicitly via assempling suitable `ecl-simpelib` (e.g. producing spec statements explicitly), for convenience we provide a macro, `define-library`, which allows for libraries to be expressed declaratively. The body of this macro is parsed to produce a corresponding library definition. Rather than explicitly outline the macro syntax, we proceed by looking at a definition for `libcalc`.

### libcalc

Since there are now a few pieces on the board, it's worth recapitulating the high level approach for generating our shared library. Specifically, for `libcalc` we have

- an ordinary Lisp system, `cl-calc`, which implements a small language for manipulating "calculator expressions", and
- an `ecl-simplelib` library definition, `libcalc` (shown further below).

We use ECL to generate (via the `ecl-simplelib:build-static` function)

- a static library, `cl-calc--all-system.a`, containing compiled `cl-calc` code.

We use `ecl-simplelib` to generate (via `ecl-simplelib:build-bindings`)

- stub functions in `libcalc.c`, wrapping the underlying ECL funcalls (cf. the previous article)
- declarations in `libcalc.h`, for library users.

The ultimate goal, `libcalc.dylib`, will come from `libcalc.c`. 

We can summarize this with the following [Makefile](https://github.com/kilimanjaro/cl-calc/blob/master/lib/Makefile)

```
.PHONY: all clean bindings

all:  libcalc.dylib

clean:
	rm -f libcalc.c libcalc.h *.a *.dylib

cl-calc--all-systems.a: libcalc.lisp
	ecl --load "libcalc.lisp" --eval "(ecl-simplelib:build-static cl-calc::libcalc)" --eval "(quit)"

bindings: libcalc.lisp
	ecl --load "libcalc.lisp" --eval "(ecl-simplelib:build-bindings cl-calc::libcalc)" --eval "(quit)"

libcalc.dylib: cl-calc--all-systems.a bindings
	$(CC) `ecl-config --cflags` -dynamiclib -o $@ libcalc.c cl-calc--all-systems.a \
	      `ecl-config --ldflags` -lecl
```



#### The Library Definition

Now, to the heart of the matter: the library definition.

The `libcalc` definition is found in [lib/libcalc.lisp](https://github.com/kilimanjaro/cl-calc/blob/master/lib/libcalc.lisp). We reproduce this below:

```
;; lib/libcalc.lisp
(in-package :cl-calc)

(defvar expr-type
  (ecl-simplelib:make-handle-object
   :name "calc_expr")
  "The C type (a handle) associated with a cl-calc expression.")


(defvar error-type
  (ecl-simplelib:make-enum-type
   :name "err_t"
   :alist '(("ERR_SUCCESS" 0)
            ("ERR_FAIL" 1)))

  "The C type (an enum) associated with cl-calc errors.")

(defvar libcalc-errors
  (ecl-simplelib:error-map
      error-type
      "ERR_SUCCESS"
    (t (expr) "ERR_FAIL")))

(ecl-simplelib:define-library libcalc
    (:system :cl-calc
     :path (asdf:system-relative-pathname :cl-calc "lib/")
     :function-prefix "calc_"
     :error-map libcalc-errors)
  :ifndef "_libcalc_h"
  :define "_libcalc_h"

  :section "boilerplate"
  :standard-boilerplate

  :section "types"
  :type expr-type
  :type error-type

  :section "functions"
  :function int-literal expr-type ((value :int))
  :function int-literal-value :int ((expr expr-type))
  :function int-literal-p :bool ((obj expr-type))
  :function sum-expression expr-type ((left expr-type) (right expr-type))
  :function sum-expression-left-arg expr-type ((expr expr-type))
  :function sum-expression-right-arg expr-type ((expr expr-type))
  :function sum-expression-p :bool ((expr expr-type))
  :function simplify expr-type ((expr expr-type))
  :function parse expr-type ((source :string))
  :function expression-to-string :string ((expr expr-type))
  :newline
  :endif)
```

To summarize the above, we 

1. Define Lisp values encoding the types which we will use in `libcalc`.
2. Define an "error map", indicating how Lisp conditions map to error codes.
3. Define the library itself, using the `ecl-simplelib:define-library` macro. This macro expects the library name, some metadata (representing by the plist), and then one or more spec statements, expressed in a lightweight DSL.

Assuming that the requisite systems are loaded, we may generate C bindings for our shared library with

```
(ecl-simplelib:build-bindings libcalc)
```

This generates two files: `libcalc.h` and `libcalc.c` (visible [here](https://github.com/kilimanjaro/cl-calc/tree/master/lib)). Note the similarity between `libcalc.h` and the `libcalc` definition above:

```
/* generated lib/libcalc.h */
#ifndef _libcalc_h
#define _libcalc_h


/* boilerplate */
#if defined(CALC_BUILD)
#  if defined(_WIN64)
#    define CALC_API_EXPORT __declspec(dllexport)
#  elif defined(__ELF__)
#    define CALC_API_EXPORT __attribute__ ((visibility ("default")))
#  else
#    define CALC_API_EXPORT
# endif
#else
#  if defined(_WIN64)
#    define CALC_API_EXPORT __declspec(dllimport)
#  else
#  define CALC_API_EXPORT
#  endif
#endif

CALC_API_EXPORT int calc_init();
CALC_API_EXPORT int calc_shutdown();
CALC_API_EXPORT char* calc_error_message();
CALC_API_EXPORT void calc_release_handle(void* handle);
CALC_API_EXPORT _Bool calc_handle_eq(void* a, void* b);


/* types */
typedef void *calc_expr;
typedef enum { ERR_SUCCESS = 0, ERR_FAIL = 1, } err_t;


/* functions */
CALC_API_EXPORT err_t calc_int_literal(int value, calc_expr* result);
CALC_API_EXPORT err_t calc_int_literal_value(calc_expr expr, int* result);
CALC_API_EXPORT err_t calc_int_literal_p(calc_expr obj, _Bool* result);
CALC_API_EXPORT err_t calc_sum_expression(calc_expr left, calc_expr right, calc_expr* result);
CALC_API_EXPORT err_t calc_sum_expression_left_arg(calc_expr expr, calc_expr* result);
CALC_API_EXPORT err_t calc_sum_expression_right_arg(calc_expr expr, calc_expr* result);
CALC_API_EXPORT err_t calc_sum_expression_p(calc_expr expr, _Bool* result);
CALC_API_EXPORT err_t calc_simplify(calc_expr expr, calc_expr* result);
CALC_API_EXPORT err_t calc_parse(char* source, calc_expr* result);
CALC_API_EXPORT err_t calc_expression_to_string(calc_expr expr, char** result);

#endif 

```

One basic design decision of the`define-library` DSL should now be apparent: libraries are defined in a "what-you-see-is-what-you-get" manner. In particular, the library definition DSL allows you to more or less express, line by line, the resulting header file, which is taken as a prescription of the library interface. Note that Lisp style names are converted to C style names (e.g. `calc-init` vs `calc_init`).

**Note**: You may be wondering about this `CALC_API_EXPORT` macro, but we will not discuss it until next time. 

### Example Usage and Conclusion

For an example of how this library can be used, look at the toy `libcalc` client in the [example](https://github.com/kilimanjaro/cl-calc/tree/master/example) directory of the code repo.



