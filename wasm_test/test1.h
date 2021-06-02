int func0(int a, int b, int c, int d, int e); /*Arithmetic - returns (a + b - c) * d / e */
int func1(int a, int b); /*logical - returns (a and b)+(a or b)+(a and a)+(b or b)+(a xor b)+(a xor a)+(a shl a)*/

float func2(float a); /*fun - returns abs(a)*/
float func3(float a); /*fun - returns neg(a)*/
float func4(float a); /*fun - returns sqrt(a)*/
float func5(float a, float b, float c); /*fun - returns abs(a) + neg(b) + sqrt(c)*/

float func6(float a); /*fun - returns ceil(a)*/
float func7(float a); /*fun - returns floor(a)*/
float func8(float a); /*fun - returns trunc(a)*/
float func9(float a); /*fun - returns nearest(a)*/
float func10(float a, float b, float c, float d); /*fun - returns ceil(a) + floor(b) + trunc(c) + nearest(d)*/

long func15(long a); /* testing if else statements */

int func13(int n); /*run the 0th function in the table; should return 666 + n*/
int func14(); /*run the 0th and 1st function in the table; should return 666 + 999*/

int func17(); /* calls helper that stores 300 in memory and return 47; func 17 loads and adds returned 47 */

int func18(); /* popcount(6) + ctz(16) + clz(32) = 2 + 4 + 26 = 32 */


void _main();
