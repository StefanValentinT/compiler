/* Test case where the return type would be passed on the stack, but the callee
 * is missing a return statement This is well-defined as long as the caller
 * doesn't try to use the return value
 * */



struct big missing_return_value(int *i) {
    *i = 10;
}