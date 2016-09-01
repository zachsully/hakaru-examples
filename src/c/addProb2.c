#include <stdio.h>
#include <stdlib.h>
#include <math.h>


int main(){

double result;
double p_a;

    p_a = log1p(4.2e-5 - 1);
    result = p_a;

printf("exp(%.17f)\n",result);
return 0;
}

