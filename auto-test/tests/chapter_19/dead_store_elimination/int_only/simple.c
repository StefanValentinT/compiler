/* A basic test case for eliminating a dead store */


int target(void) {
    int x = 10; // this is a dead store
    return 3;
}

int main(void) {
    return target();
}