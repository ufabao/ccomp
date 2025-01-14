int main(void) {
    int outer = 1;
    int foo = 0;
    if (outer) {
        extern int foo;
        extern int foo;
        return foo;
    }
    return 0;
}

int foo = 3;