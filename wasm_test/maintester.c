#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "test1.h"


void binops(){
    int a = 5, b = 5, d = 5;
    long c = 5;
    int e = 10;
    int ans = (a + b - c) * d / e;
    int res = func0(a,b,c,d,e);
    if(ans == res)
        printf("PASS: add, sub, mul, and div work\n");
    else
        printf("FAIL: add, sub, mul, and div did not work\n");
}

void binops2(){
    int a = 1, b = 0;
    int res = func1(a,b);
    if(res == 5)
        printf("PASS: and, or, xor, and shl work\n");
}

/*𝖺𝖻𝗌 | 𝗇𝖾𝗀 | 𝗌𝗊𝗋𝗍 | 𝖼𝖾𝗂𝗅 | 𝖿𝗅𝗈𝗈𝗋 | 𝗍𝗋𝗎𝗇𝖼 | 𝗇𝖾𝖺𝗋𝖾𝗌𝗍 */
void funops(){
    float a = -6.5f;
    float b = 2.5f;
    float c = 25.0f;
    // float a = -6.5f, b = 0.0f, c = 0.0f;

    // check that abs works
    float a_abs = func2(a);
    if(fabs(a_abs - fabs(a)) < 1e-6f)
        printf("PASS: abs(a) works\n");
    else 
        printf("FAIL: abs(%f) returned %f\n", a, a_abs);


    // check that neg works
    float b_neg = func3(b);
    float exp = -b;
    if(fabs(b + exp) < 1e-6f)
        printf("PASS: neg(b) works\n");
    else
        printf("FAIL: neg(%f) returned %f\n", b, b_neg);

    // check that neg works
    float c_sqrt = func4(c);
    if(fabs(c_sqrt - 5.0f) < 1e-6f)
        printf("PASS: sqrt(c) works\n");
    else
        printf("FAIL: sqrt(%f) returned %f\n", c, c_sqrt);

    // abs(a) + neg(b) + sqrt(c)
    float res = func5(a,b,c);
    exp = fabs(a) - b + 5.0f;
    if(fabs(a_abs - exp) < 1e-6f)
        printf("PASS: abs, neg, and sqrt work\n");
    else 
        printf("FAIL: abs(%f) + neg(%f) + sqrt(%f) resulted in %f\n", a, b, c, res);
    
}

void funops2(){
    float a = 10.6f, b = 3.7f, c = 7.95f, d = 4.501f; 

    // check if ceil is correct
    float a_ceil = func6(a);
    if(fabs(a_ceil - 11.0f) < 1e-6)
        printf("PASS: ceil(a) works\n");
    else
        printf("FAIL: ceil(a) resulted in %f, should be 11.0", a_ceil);


    // ceil(a) + floor(b) + trunc(c) + ceil(d)
    //  11     +    3 .   + .   7 .  +     5
    float res = func10(a,b,c,d);
    if(fabs(res - 26.0) < 1e-6)
        printf("PASS: ceil, floor, trunc and nearest work\n");
    else 
        printf("FAIL: ceil(%f) + floor(%f) + trunc(%f) + nearest(%f) resulted in %f \n", a, b, c, d, res);

}

void unops(){
    int a = 1, b = 0;
    int res = func1(a,b);
    if(res == 5)
        printf("and, or, xor, and shl work\n");
}

int main()
{
    int num1 = 5, num2 = 10;
    /*int res = func0(num1, num2);
    printf("numbers are %d and %d, result is %d", num1, num2, res);*/
    binops();
    binops2();
    funops();
    funops2();
    return 0;
}