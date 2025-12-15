#include "../optiscope.h"

// Church numerals

static struct lambda_term *
church_two(void) {
	struct lambda_term *f, *x;
	
	return lambda(f, lambda(x, apply(var(f), apply(var(f),
	 var(x)))));
}

static struct lambda_term *
church_three(void) {
	struct lambda_term *f, *x;
	
	return lambda(
	f, lambda(x, apply(var(f), apply(var(f), apply(var(f), 
	 var(x))))));
}

static struct lambda_term *
church_multiply(void) {
	struct lambda_term *m, *n, *f, *x;
	
	return lambda(
	 m,
	 lambda(
	  n,
	  lambda(
	   f,
	   lambda(
	    x, apply(apply(var(m), apply(var(n), var(f))), 
	     var(x))))));
}

static struct lambda_term *
church_three_times_two(void) {
	return apply(
	apply(church_multiply(),church_three()),church_two());
}


int main(void) {
	optiscope_open_pools();
	optiscope_algorithm(stdout, church_three_times_two());
	puts("");
	optiscope_close_pools();
}