// include isnan macro and export a non-macro version we can use

int double_isnan(double d) {
    return isnan(d);
}