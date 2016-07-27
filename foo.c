#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

double measure(){
double result;
double a1;
double x_a2;
double a3;
double y_a4;

    a1 = 0.0 + (double) rand() / (double) RAND_MAX * (5.0 - 0.0);
    x_a2 = a1;
    a3 = 5.0 + (double) rand() / (double) RAND_MAX * (9.0 - 5.0);
    y_a4 = a3;
    result = y_a4 + x_a2;

return result;
}

int main(){
srand(time(NULL));



while(1) printf("%.17g\n",measure());
return 0;
}

