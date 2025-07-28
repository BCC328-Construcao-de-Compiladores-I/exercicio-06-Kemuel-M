#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    // immutable variable definition
    {
        int x = 1;
        // assignment
        int a = (x + 1);
        // print operation
        printf("%d\n", a);
        // immutable variable definition
        {
            int x = 5;
            // assignment
            int a = (x - 1);
            // print operation
            printf("%d\n", a);
        }
        // print operation
        printf("%d\n", x);
    }
    return 0;
}
