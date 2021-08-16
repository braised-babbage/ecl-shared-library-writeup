#include <ecl/ecl.h>
#include <stdio.h>

extern void init_calc(cl_object);

int main(int argc, char **argv) {
  cl_boot(argc, argv);
  ecl_init_module(NULL, init_calc);

  cl_object int_literal = ecl_make_symbol("INT-LITERAL", "CL-CALC");
  cl_object int_literal_value = ecl_make_symbol("INT-LITERAL-VALUE", "CL-CALC");
  cl_object sum_expression = ecl_make_symbol("SUM-EXPRESSION", "CL-CALC");
  cl_object simplify = ecl_make_symbol("SIMPLIFY", "CL-CALC");

  
  cl_object a = cl_funcall(2, int_literal, ecl_make_int(5));
  cl_object b = cl_funcall(2, int_literal, ecl_make_int(6));
  cl_object sum_expr = cl_funcall(3, sum_expression, a, b);
  cl_object reduced = cl_funcall(2, simplify, sum_expr);
  
  int sum = ecl_to_int(cl_funcall(2, int_literal_value, reduced));
  printf("5 + 6 = %d\n", sum);

  cl_shutdown();
  return 0;
}
