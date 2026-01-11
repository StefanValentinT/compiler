/* Make sure this pass removes unused label instructions */


int target(void) {
    lbl:
    return 0;
}

int main(void) {
    return target();
}