#include <stdio.h>
#include <stdlib.h>
#include <math.h>

struct arrayNat {
    int size; int * data;
};

int main(){

int result;
struct arrayNat arr_a;
int i_b;

    arr_a.size = 42;
    arr_a.data = (int *) malloc(42 * sizeof(int));
    i_b = 0;
    for (i_b; i_b < 42; i_b++)
    {
        *(arr_a.data + i_b) = i_b;
    }
    result = arr_a.size;

printf("%d\n",result);
return 0;
}

