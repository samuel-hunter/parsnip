fib(n) {
	if (n == 0) {
		return 1;
	}

	return n * fib(n-1);
}

printfibs(n, max) {
	print(fib(n));

	if (n != max)
		printfibs(n-1, max);
}

main () {
	printfibs(10);
}
