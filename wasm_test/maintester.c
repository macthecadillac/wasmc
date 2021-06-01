#include <stdio.h>
#include "test1.h"


void binops(){
    int a = 5, b = 5, d = 5;
    long c = 5;
    int e = 10;
    int ans = (a + b - c) * d / e;
    int res = func0(a,b,c,d,e);
    if(ans == res)
        printf("add, sub, mul, and div work\n");
}

void binops2(){
    int a = 1, b = 0;
    int res = func1(a,b);
    if(res == 5)
        printf("and, or, xor, and shl work\n");
}
/*𝖺𝖻𝗌 | 𝗇𝖾𝗀 | 𝗌𝗊𝗋𝗍 | 𝖼𝖾𝗂𝗅 | 𝖿𝗅𝗈𝗈𝗋 | 𝗍𝗋𝗎𝗇𝖼 | 𝗇𝖾𝖺𝗋𝖾𝗌𝗍
*/
void funops(){
    int a = -2, b = 32, c = 16;
    int res = func2(a,b,c);
    if(res == -26)
        printf("abs, neg, and sqrt work\n");
}
void funops2(){
    float a = 2.6f, b = 3.7f, c = 6.45f, d = 4.501f; 
    float res = func3(a,b,c,d);
    if(res == 17.0)
        printf("ceil, floor, trunc and nearest work\n");
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
    return 0;
}