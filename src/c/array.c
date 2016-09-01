#include <stdio.h>
#include <stdlib.h>
#include <math.h>

struct arrayNat {
    int size; int * data;
};

int main(){

struct arrayNat result;
struct arrayNat arr_a;
int i_b;

    arr_a.size = 5;
    arr_a.data = (int *) malloc(5 * sizeof(int));
    i_b = 0;
    for (i_b; i_b < 5; i_b++)
    {
        *(arr_a.data + i_b) = i_b;
    }
    result = arr_a;

printf("[ ");
for (int i = 0; i < result.size; i++) {
  printf("%d ",*(result.data + i));
}
printf("]\n");

return 0;
}

