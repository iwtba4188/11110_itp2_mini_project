#include <stdio.h>

int main() {
    int x = 3, y = 8, z = 6;
    z = x + 5;
    y = z / 10 - 7 * x;
    -y - (+z) % (z + 100);
    z = (x++) + (y--);
    x = (--y) * (++z);
    x = z - +-+-+-++y;
    ;
    x = y = z = 3 + 5;
    printf("x=%d, y=%d, z=%d\n", x, y, z);
}