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
	}

	return a;
}

main() {
	sayn(fib(10));
	sayn(fact(10));

	return 0;
}
