#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    // read operation
    int a;
    printf("Digite o primeiro numero: ");
    scanf("%d", &a);
    // read operation
    int b;
    printf("Digite o segundo numero: ");
    scanf("%d", &b);
    // immutable variable definition
    {
        int x = (a + b);
        // print operation
        printf("%d\n", x);
        // assignment
        int y = (x * 2);
        // print operation
        printf("%d\n", y);
        // immutable variable definition
        {
            int x = (y - a);
            // print operation
            printf("%d\n", x);
        }
        // print operation
        printf("%d\n", x);
    }
    // print operation
    printf("%s\n", "Processo concluido");
    return 0;
}
