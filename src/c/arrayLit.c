#include <stdio.h>
#include <stdlib.h>
#include <math.h>

struct SUSUV {
    int index;
};
struct arrayNat {
    int size; int * data;
};

int main(){

struct arrayNat result;
struct arrayNat arr_a;
int _b;
struct SUSUV eq_c;
int _d;
struct SUSUV eq_e;
int _f;
struct SUSUV eq_g;
int _h;

    arr_a.size = 4;
    arr_a.data = (int *) malloc(4 * sizeof(int));
    _b = 0;
    for (_b; _b < 4; _b++)
    {
        eq_c.index = _b == 0 ? 0 : 1;
        if (eq_c.index == 0)
        {
            _d = 4;
        }
        if (eq_c.index == 1)
        {
            eq_e.index = _b == 1 ? 0 : 1;
            if (eq_e.index == 0)
            {
                _f = 2;
            }
            if (eq_e.index == 1)
            {
                eq_g.index = _b == 2 ? 0 : 1;
                if (eq_g.index == 0)
                {
                    _h = 1;
                }
                if (eq_g.index == 1)
                {
                    _h = 0;
                }
                _f = _h;
            }
            _d = _f;
        }
        *(arr_a.data + _b) = _d;
    }
    result = arr_a;

printf("[ ");
for (int i = 0; i < result.size; i++) {
  printf("%d ",*(result.data + i));
}
printf("]\n");

return 0;
}

