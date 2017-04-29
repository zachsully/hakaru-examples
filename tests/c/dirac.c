#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>


double measure(){
int result;

    result = 42;

return result;
}

int main(){
srand(time(NULL));



while(1) printf("%d\n",measure());
return 0;
}


