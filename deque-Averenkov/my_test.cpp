# include <iostream>

int& bad (int x) {
	++ x;
	return x;
}

int main () {
	int y = bad(0);
	std :: cout << y ;
}