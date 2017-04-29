#include <stdio.h>
#include <stdlib.h>
#include <math.h>

struct SUSUV {
    int index;
};

int main(){

int result;
struct SUSUV _a;
int _b;

    _a.index = 1;
    if (_a.index == 0)
    {
        _b = 0;
    }
    if (_a.index == 1)
    {
        _b = 42;
    }
    result = _b;

printf("%d\n",result);
return 0;
}

