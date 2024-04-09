#include <iostream>
#include "geometry.h"
#include <math.h>

int main() {
    std::vector <Point> v = {
      {0.8, 0.8},
      {3.4, 1.8},
      {2.6, 2.8},
      {1.4, 1.8},
      {0.6, 2.2},
    };
    Polygon a(v);
    std::cout << a.area();
}
