#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>


double measure(){
double result;
double p_a;
double _b;
double _c;
double _d;
double _e;

    p_a = log1p(5.0 - 1);
    do
    {
        _b = (double) rand() / (double) RAND_MAX * 2.0 - 1.0;
        _c = (double) rand() / (double) RAND_MAX * 2.0 - 1.0;
        _d = _b * _b + _c * _c;
    }
    while (_d == 0 || _d > 1);
    _e = sqrt(-(2) * (log(_d) / _d));
    result = 0.0 + _b * (_e * p_a);

return result;
}

int main(){
srand(time(NULL));



while(1) printf("%.17f\n",measure());
return 0;
}


