module AOC2021.Day24.Main where

-- Solved by hand. My notes below:

{-
// The program is a cycle of this 14 times (* means different from cycle to cycle):
// 
// read to w
// set x to 0...
// ... and put z into x
// does mod 26 on x
// * either noop or divide z by 26
// * add various different numbers to x
// x = if x == w then 1 else 0...
// ... and negates x
// set y to 0...
// ... and set y to 25
// multiply y by x...
// and add 1 to y
// multiply z by y
// set y to 0...
// ... and put w into y
// * add various different numbers to y
// multiply y by x
// add y to z
// 
// 
// Interesting that only 3 instructions differ between the cycles!
// 
// First stab at simplifying:
// 
// w = inp();
// x = z;
// x %= 26;
// * either noop or divide z by 26
// * add various different numbers to x
// x = (int)(x != w);
// y = (25 * x) + 1;
// z *= y;
// y = w;
// * add various different numbers to y
// y *= x;
// z += y;
// 
// 
// Another simplification iteration:
// 
// w = inp();
// x = z % 26;
// if (/* some of the cycles */) {
//     z /= 26;
// }
// if (x + /* different number in different cycles */ != w) {
//     z *= 26;
//     z += w + /* different number in different cycles */;
// }
// 
// 
// Only z keeps state between cycles.
// 
// The goal is to keep z at 0 in the end.
// 
// Worth noting: In case z < 26, then z / 26 == 0.
// 
// In the last cycle, we can't enter the last is, because the "different number" is 5.
// 
// 
// Full code in C style:

//                                iteration: {   #0,     #1    #2,    #3,   #4,    #5,    #6,   #7,    #8,   #9,  #10,  #11,  #12,  #13}
const bool[] rightShifts        = new bool[] {false, false, false, false, true, false, false, true, false, true, true, true, true, true};
const int[]  checkNums          = new int[]  {   12,    12,    13,    12,   -3,    10,    14,  -16,    12,   -8,  -12,   -7,   -6,  -11};
const int[]  newLastDigitOffset = new int[]  {    7,     8,     2,    11,    6,    12,    14,   13,    15,   10,    6,   10,    8,    5};
int z = 0;
for (int i = 0; i < 14; i++) {
    const int x = z % 26;
    if (rightShifts[i]) {
        z /= 26;  // Shift base-26 z right by 1.
    }
    const int w = inp();
    if (x + checkNums[i] != w) {
        z *= 26;  // Shift base-26 z left by 1.
        z += w + newLastDigitOffset[i];
    }
}


// Let's try inputting 1s until it doesn't work out anymore. By hand.

// iteration #0 : inp() = 9,  0 +  12 != 9, then z = [16]
// iteration #1 : inp() = 7, 16 +  12 != 7, then z = [16, 15]
// iteration #2 : inp() = 9, 15 +  13 != 9, then z = [16, 15, 11]
// iteration #3 : inp() = 1, 11 +  12 != 1, then z = [16, 15, 11, 12]
// iteration #4 : inp() = 9, 12 +  -3 == 9, then z = [16, 15, 11]
// iteration #5 : inp() = 9, 11 +  10 != 9, then z = [16, 15, 11, 21]
// iteration #6 : inp() = 9, 21 +  14 != 9, then z = [16, 15, 11, 21, 23]
// iteration #7 : inp() = 7, 23 + -16 == 7, then z = [16, 15, 11, 21]
// iteration #8 : inp() = 2, 21 +  12 != 2, then z = [16, 15, 11, 21, 17]
// iteration #9 : inp() = 9, 17 +  -8 == 9, then z = [16, 15, 11, 21]
// iteration #10: inp() = 9, 21 + -12 == 9, then z = [16, 15, 11]
// iteration #11: inp() = 4, 11 +  -7 == 4, then z = [16, 15]
// iteration #12: inp() = 9, 15 +  -6 == 9, then z = [16]
// iteration #13: inp() = 5, 16 + -11 == 5, then z = []

// 97919997299495


// iteration #0 : w = 5, !=  _ +  12, w +  7 = 12, then z = [12]
// iteration #1 : w = 1, !=  _ +  12, w +  8 =  9, then z = [12, 9]
// iteration #2 : w = 6, !=  _ +  13, w +  2 =  8, then z = [12, 9, 8]
// iteration #3 : w = 1, !=  _ +  12, w + 11 = 12, then z = [12, 9, 8, 12]
// iteration #4 : w = 9, == 12 +  -3, w +  6 =  _, then z = [12, 9, 8]
// iteration #5 : w = 1, !=  _ +  10, w + 12 = 13, then z = [12, 9, 8, 13]
// iteration #6 : w = 3, !=  _ +  14, w + 14 = 17, then z = [12, 9, 8, 13, 17]
// iteration #7 : w = 1, == 17 + -16, w + 13 =  _, then z = [12, 9, 8, 13]
// iteration #8 : w = 1, !=  _ +  12, w + 15 = 16, then z = [12, 9, 8, 13, 16]
// iteration #9 : w = 8, == 16 +  -8, w + 10 =  _, then z = [12, 9, 8, 13]
// iteration #10: w = 1, == 13 + -12, w +  6 =  _, then z = [12, 9, 8]
// iteration #11: w = 1, ==  8 +  -7, w + 10 =  _, then z = [12, 9]
// iteration #12: w = 3, ==  9 +  -6, w +  8 =  _, then z = [12]
// iteration #13: w = 1, == 12 + -11, w +  5 =  _, then z = []

// 51619131181131
-}