#include <stdio.h>
#include "mpc.h"

/* Declare a buffer for user input of size 2048 */
static char input[2048];

int main(int argc, char **argv)
{
  // Build the grammar
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Operator = mpc_new("operator");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "                                                \
              number   : /-?[0-9]+/ ;                        \
              operator : '+' | '-' | '*' | '/' ;             \
              expr : <number> | '(' <operator> <expr>+ ')' ; \
              lispy : /^/ <operator> <expr>+ /$/ ;           \
            ",
            Number, Operator, Expr, Lispy);

  /* Print Version and Exit Information */
  puts("Lispy Version 0.0.0.0.1");
  puts("Press Ctrl+c to Exit\n");

  while (1)
  {
    fputs("lispy> ", stdout);
    fgets(input, 2048, stdin);

    mpc_result_t r;
    if (mpc_parse("<stdin>", input, Lispy, &r))
    {
      mpc_ast_print(r.output);
      mpc_ast_delete(r.output);
    }
    else
    {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
  }

  mpc_cleanup(4, Number, Operator, Expr, Lispy);
  return 0;
}