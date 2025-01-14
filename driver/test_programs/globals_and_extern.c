int a = 5;

int return_a(void) {
    return a;
}

int main(void) {
    int a = 3;
    {
        extern int a;
        if (a != 5)
            return 1;
        a = 4;
    }
    return a + return_a();
}