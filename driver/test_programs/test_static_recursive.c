int putchar (int ch);

int print_alphabet(void) {
    static int count = 0;
    putchar(count + 65);
    count = count + 1;
    if (count < 26) {
        print_alphabet();
    }
    return count;
}

int main(void) {
    print_alphabet();
}