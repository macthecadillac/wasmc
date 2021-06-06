#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "test1.h"


int unops(){
    printf("\n----UnOps Tests: clz, ctz, popcnt----\n");

    int failed = 0;
    int res = func18();

    if (res == 32)
        printf("PASS: clz, ctz, popcnt work\n");
    else {
        failed++;
        printf("FAIL: clz, ctz, popcnt not working. Returned: %d\n", res);
    }

    return failed;
}

int binops(){
    printf("\n----BinOps Tests: add, sub, mul, div----\n");
    int failed = 0;
    int a = 5, b = 5, d = 5;
    long c = 5;
    int e = 10;
    int ans = (a + b - c) * d / e;
    int res = func0(a,b,c,d,e);
    if(ans == res)
        printf("PASS: add, sub, mul, and div work\n");
    else {
        failed++;
        printf("FAIL: add, sub, mul, and div did not work\n");
    }
    return failed;
}

int binops2(){
    printf("\n----BinOps Tests: and, or, xor, shl----\n");
    int failed = 0;
    int a = 1, b = 0;
    int res = func1(a,b);
    if(res == 5)
        printf("PASS: and, or, xor, and shl work\n");
    else {
        failed++;
        printf("FAIL: and, or, xor, and shl did not work\n");
    }
    return failed;
}

/*𝖺𝖻𝗌 | 𝗇𝖾𝗀 | 𝗌𝗊𝗋𝗍 | 𝖼𝖾𝗂𝗅 | 𝖿𝗅𝗈𝗈𝗋 | 𝗍𝗋𝗎𝗇𝖼 | 𝗇𝖾𝖺𝗋𝖾𝗌𝗍 */
int funops(){
    printf("\n----FUnops Tests: abs, neg, sqrt----\n");
    int failed = 0;
    float a = -6.5f;
    float b = 2.5f;
    float c = 25.0f;
    // float a = -6.5f, b = 0.0f, c = 0.0f;

    // check that abs works
    float a_abs = func2(a);
    if(fabs(a_abs - fabs(a)) < 1e-6f)
        printf("PASS: abs(a) works\n");
    else {
        failed++;
        printf("FAIL: abs(%f) returned %f\n", a, a_abs);
    }


    // check that neg works
    float b_neg = func3(b);
    float exp = -b;
    if(fabs(b + exp) < 1e-6f)
        printf("PASS: neg(b) works\n");
    else {
        failed++;
        printf("FAIL: neg(%f) returned %f\n", b, b_neg);
    }

    // check that neg works
    float c_sqrt = func4(c);
    if(fabs(c_sqrt - 5.0f) < 1e-6f)
        printf("PASS: sqrt(c) works\n");
    else {
        failed++;
        printf("FAIL: sqrt(%f) returned %f\n", c, c_sqrt);
    }

    // abs(a) + neg(b) + sqrt(c)
    float res = func5(a,b,c);
    exp = fabs(a) - b + 5.0f;
    if(fabs(res - exp) < 1e-6f)
        printf("PASS: abs, neg, and sqrt work\n");
    else {
        failed++;
        printf("FAIL: abs(%f) + neg(%f) + sqrt(%f) resulted in %f\n", a, b, c, res);
    }
    return failed;
}

int funops2(){
    int failed = 0;
    printf("\n----FUnops Tests: ceil, floor, trunc, nearest----\n");
    float a = 10.6f, b = 3.7f, c = 7.95f, d = 4.501f; 

    // check if ceil is correct
    float a_ceil = func6(a);
    if(fabs(a_ceil - 11.0f) < 1e-6)
        printf("PASS: ceil(a) works\n");
    else {
        failed++;
        printf("FAIL: ceil(a) resulted in %f, should be 11.0\n", a_ceil);
    }

    // ceil(a) + floor(b) + trunc(c) + nearest(d)
    failed++;
    printf("FAIL: nearest not implemented. Won't link\n");


    // ceil(a) + floor(b) + trunc(c) + ceil(d)
    //  11     +    3 .   + .   7 .  +     5
    float res = func10(a,b,c,d);
    if(fabs(res - 26.0) < 1e-6)
        printf("PASS: ceil, floor, trunc and nearest work\n");
    else {
        failed++;
        printf("FAIL: ceil(%f) + floor(%f) + trunc(%f) + ceil(%f) resulted in %f \n", a, b, c, d, res);
    }

    return failed;
}

int control_flow_tests() {
    int failed = 0;
    printf("\n----Control Flow Tests----\n");

    long a = 0;
    long res = func15(a);
    if (res == 42)
        printf("PASS: EQZ Working\n");
    else {
        failed++;
        printf("FAIL: EQZ Not Working; Func Call Returned Unexpected Value: %ld\n", res);
    }

    a = 1;
    res = func15(a);
    if (res == 99)
        printf("PASS: EQ Working\n");
    else {
        failed++;
        printf("FAIL: EQ Not Working; Func Call Returned Unexpected Value: %ld\n", res);
    }

    res = func19(10);
    if (res == 55)
        printf("PASS: Loop Working\n");
    else {
        failed++;
        printf("FAIL: Loop Not Working; Func Call Returned Unexpected Value: %ld\n", res);
    }

    return failed;
}

int table_tests(){
    int failed = 0;
    printf("\n----Table Tests----\n");
    int n = 3;

    // res = 666 + n
    int res = func13(n);
    if (res == 666 + n)
        printf("PASS: Table 0 Func Called\n");
    else {
        failed++;
        printf("FAIL: Table 0 Func Call Returned Unexpected Value: %d\n", res);
    }

    // Nested Table Call
    res = func14(n);
    if (res == 666 + 999)
        printf("PASS: Nested Table Calls working\n");
    else {
        failed++;
        printf("FAIL: Nested Table Calls Returned Unexpected Value: %d\n", res);
    }

    return failed;
}

void segfault_sigaction(int signal, siginfo_t *si, void *arg)
{
    printf("Yeah No Memory Don't Work Nope: Caught segfault at address %p\n", si->si_addr);
    exit(0);
    // ucontext_t *context = arg;
    // context->uc_mcontext.gregs[14] += 6;
}

int memory_tests() {
    printf("\n----Memory Tests----\n");

    int failed = 0;

    // initialize memory
    // _main();

    // Nested Call + Memory Access
    int res = 0;
    res = func17();
    if (res == 300 + 47)
        printf("PASS: Nested Call + Memory Access within module working\n");
    else {
        failed++;
        printf("FAIL: Nested Call + Memory Access within module not working. Returned: %d\n", res);
    }

    // free();

    return failed;
}

int main()
{
    struct sigaction sa;

    memset(&sa, 0, sizeof(struct sigaction));
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = segfault_sigaction;
    sa.sa_flags   = SA_SIGINFO;

    sigaction(SIGSEGV, &sa, NULL);

    int failed = 0, numTests = 16;
    int num1 = 5, num2 = 10;

    /*int res = func0(num1, num2);
    printf("numbers are %d and %d, result is %d", num1, num2, res);*/
    failed += unops();
    failed += binops();
    failed += binops2();
    failed += funops();
    failed += funops2();
    failed += control_flow_tests();
    failed += table_tests();
    failed += memory_tests();
    wasmc_drop();
    printf("\nFailing (%d/%d) Tests", failed, numTests);
    return 0;
}
