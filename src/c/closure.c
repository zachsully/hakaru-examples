#include <stdio.h>
#include <math.h>

/* Hakaru Program:
foo = 42
foo2 = 0
bar = fn x int: foo + foo2
bar(0)
*/

struct clos_b
{
  int * fn (struct clos_b _c, int x_d);
  int a;
  int b;
};

int fn_a(struct clos_b _c, int x_d)
{
  int loc_a;
  int loc_b;
  loc_a = _c.a;
  loc_b = _c.b;
  return loc_a + loc_b;
}

int main()
{
  int result;
  int foo_e;
  int foo2_f;
  struct clos_b _g;
  foo_e = 42;
  foo2_f = 0;
  _g.fn = &fn_a;
  _g.a = foo_e;
  _g.b = foo2_f;
  result = (*_g.fn)(_g,0);
  printf("%d\n",result);
}
