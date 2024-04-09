#include <bits/stdc++.h>
#include "string.h"

template <typename T>
void sw(T& a, T& b) {
    T c = std::move(a);
    std::cout << c[0] << '\n';
    a = std::move(b);
    std::cout << a[0] << '\n';
    b = std::move(c);
}

int main() {
    String a(1000000000, '$'), b(100000000, '%');
    double s = std::clock();
    sw(a, b);
    std::cout << a[0] << (std::clock() - s);

}
