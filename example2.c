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
