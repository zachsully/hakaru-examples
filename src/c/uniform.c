#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>


double measure(){
double result;
double _a;

    _a = 0.0 + (double) rand() / (double) RAND_MAX * (1.0 - 0.0);
    result = _a;

return result;
}

int main(){
srand(time(NULL));



while(1) printf("%.17f\n",measure());
return 0;
}


