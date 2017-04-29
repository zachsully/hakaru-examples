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
struct SUSUV _c;
int _d;

    _a.index = 1;
    if (_a.index == 0)
    {
        _b = 0;
    }
    if (_a.index == 1)
    {
        _c.index = 1;
        if (_c.index == 0)
        {
            _d = 0;
        }
        if (_c.index == 1)
        {
            _d = 42;
        }
        _b = _d;
    }
    result = _b;

printf("%d\n",result);
return 0;
}

