fact(n) {
	if (n == 0)
		return 1;

	return n * fact(n-1);
}

fib(n) {
	a = 0;
	b = 1;

	while (n > 0) {
		b = b + a;
		a = b - a;
		n = n - 1;
	}

	return a;
}

add(a, b) {
	return a + b;
}

main() {
	sayn(fib(10));
	sayn(fact(10));

	return 0;
}
