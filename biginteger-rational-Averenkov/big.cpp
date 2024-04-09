#include <iostream>
#include "biginteger.h"

int main() {
    BigInteger bigint = 0;
    std::cout << ((--bigint) == -1);
    std::cout << ((bigint--) == -1);
    std::cout << (bigint == -2);

    std::cout << ((++bigint) == -1);
    std::cout << ((bigint++) == -1) << std::endl;
    std::cout << bigint.toString() << std::endl;
    std::cout << (bigint == 0);

    std::cout << (-bigint == bigint);
    bigint = 1;
    std::cout << (-bigint != bigint);
    std::cout << (-bigint == -1);
    std::cout << (bigint == 1);

}
