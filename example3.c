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
