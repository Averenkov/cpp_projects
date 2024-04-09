#include <cstring>
#include <ostream>
#include <istream>
#include <algorithm>
#include <vector>
#include <tuple>
#include <utility>
#include <cmath>
#include <cassert>

const double eps = 1e-8, pi = acos(-1.0), DEGREE = 180;

struct Point {
    double x;
    double y;
    Point() = default;
    Point(double x, double y): x(x), y(y) {}
    bool operator ==(const Point& b) const {
        return ((fabs(x - b.x) < eps) && (fabs(y - b.y) < eps));
    }
    Point& operator +=(const Point& b) {
        x += b.x;
        y += b.y;
        return *this;
    }
    Point& operator -=(const Point& b) {
        x -= b.x;
        y -= b.y;
        return *this;
    }
    Point& operator *=(const double& k) {
        x *= k;
        y *= k;
        return *this;
    }
    Point& operator /=(const double& k) {
        x /= k;
        y /= k;
        return *this;
    }
    void rotate(const double& angle) {
        double x1 = x * cos(angle) - y * sin(angle);
        double y1 = x * sin(angle) + y * cos(angle);
        x = x1;
        y = y1;
    }
    double sz() {
        return sqrt(x * x + y * y);
    }
};

double dist(const Point& a, const Point& b) {
    return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

Point operator +(Point a, const Point& b) {
    a += b;
    return a;
}

Point operator -(Point a, const Point& b) {
    a -= b;
    return a;
}

Point operator *(Point a, const double& k) {
    a *= k;
    return a;
}

Point operator /(Point a, const double& k) {
    a /= k;
    return a;
}

double dot_product(const Point& a, const Point& b) {
    return a.x * b.x + a.y * b.y;
}

double cross_product(const Point& a, const Point& b) {
    return a.x * b.y - a.y * b.x;
}

bool operator !=(const Point& a, const Point& b) {
    return !(a == b);
}

double area(const Point& a, const Point& b, const Point& c) {
	return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
}

bool intersect(double a, double b, double c, double d) {
    if (a > b) {
        std::swap(a, b);
    }
    if (c > d) {
     	std::swap(c, d);
    }
    return std::max(a, c) <= std::min(b, d);
}

bool get_seg(Point a, Point b, Point c, Point d) {
    return intersect(a.x, b.x, c.x, d.x) && intersect(a.y, b.y, c.y, d.y) && area(a,b,c) * area(a,b,d) < eps && area(c,d,a) * area(c,d,b) < eps;
}

class Line {
public:
    double A, B, C;
    Line(const Point& a, const Point& b): A(a.y - b.y), B(b.x - a.x), C(-A * a.x - B * a.y) {}
    Line(const double& k, const double& b): A(k), B(-1), C(b) {};
    Line(const Point& a, const double& k): Line(a, {a.x + 1, a.y + k}) {};
    bool operator ==(const Line& b) const {
        return std::fabs((A * b.A + B * b.B + C * b.C) * (A * b.A + B * b.B + C * b.C) - (A * A + B * B + C * C) * (b.A * b.A + b.B * b.B + b.C * b.C)) < eps;
    }
};

bool operator !=(const Line& a, const Line& b) {
    return !(a == b);
}

double dist(const Point& a, const Line& b) {
    return std::fabs(a.x * b.A + a.y * b.B + b.C) / sqrt(b.A * b.A + b.B * b.B);
}

Point operator ^(const Line& a, const Line& b) {
    return Point(-(a.C * b.B - b.C * a.B) / (a.A * b.B - a.B * b.A), -(a.A * b.C - b.A * a.C) / (a.A * b.B - a.B * b.A));
}

class Shape {
public:
    Shape() = default;
    virtual double perimeter() const = 0;
    virtual double area() const = 0;
    virtual bool operator==(const Shape& another) const = 0;
    virtual void rotate(const Point& center, double angle) = 0;
    virtual void reflect(const Point& center) = 0;
    virtual void reflect(const Line& axis) = 0;
    virtual void scale(const Point& center, double coefficient) = 0;
    virtual bool isSimilarTo(const Shape& another) const = 0;
    virtual bool isCongruentTo(const Shape& another) const = 0;
    virtual bool containsPoint(const Point& point) const = 0;
    virtual ~Shape() = default;
};

class Polygon: public Shape {
protected:
    std::vector <Point> vertex;
public:
    Polygon(const std::vector <Point> &v): vertex(v) {}
    template<typename... Args>
    Polygon(Args... args) {
        (vertex.push_back(args), ...);
    }
    Polygon(std::initializer_list<Point> c) {
        vertex.resize(c.size());
        std::copy(c.begin(), c.end(), vertex.begin());
    }
    size_t verticesCount() const {
        return vertex.size();
    }
    const std::vector<Point>& getVertices() const {
        return vertex;
    }
    bool isConvex() const {
        bool f1 = true;
        bool f2 = true;
        for (size_t i = 0; i < vertex.size(); ++i) {
            double cross_prod = cross_product((vertex[(i + 1) % vertex.size()] - vertex[i]), (vertex[(i + 2) % vertex.size()] - vertex[i]));
            f1 &= (cross_prod > 0);
            f2 &= (cross_prod < 0);
        }
        return (f1 || f2);
    }
    double perimeter() const {
        double p = 0;
        for (size_t i = 0; i < vertex.size(); ++i) {
            p += dist(vertex[i], vertex[(i + 1) % vertex.size()]);
        }
        return p;
    }
    double area() const {
        double s = 0;
        for (size_t i = 0; i < vertex.size(); ++i) {
            s += vertex[i].x * (vertex[(i + vertex.size() - 1) % vertex.size()].y - vertex[(i + 1) % vertex.size()].y);
        }
        s = fabs(s);
        s = s / 2;
        return s;
    }
    bool operator==(const Shape& another) const {
        auto p = dynamic_cast<const Polygon*>(&another);
        if (p == nullptr) {
            return false;
        }
        return operator==(*p);
    }
    bool operator==(const Polygon& other) const {
        if (vertex.size() != other.vertex.size()) {
            return false;
        }
        for (size_t i = 0; i < vertex.size(); ++i) {
            size_t ind = 0;
            for (size_t j = 0; j < other.vertex.size(); ++j) {
                if (vertex[i] == other.vertex[j]) {
                    ind = j + 1;
                    break;
                }
            }
            if (ind == 0) {
                continue;
            }
            ind -= 1;
            bool f = true;
            for (size_t j = 0; j < other.vertex.size(); ++j) {
                if (vertex[(j + i) % vertex.size()] != other.vertex[(ind + j) % other.vertex.size()]) {
                    f = false;
                    break;
                }
            }
            if (f) {
                return true;
            }
            f = true;
            for (size_t j = 0; j < other.vertex.size(); ++j) {
                if (vertex[(i + vertex.size() - j) % vertex.size()] != other.vertex[(ind + j) % other.vertex.size()]) {
                    f = false;
                    break;
                }
            }
            if (f) {
                return true;
            }
        }
        return false;
    }
    void rotate(const Point& center, double angle) {
        for (size_t i = 0; i < vertex.size(); ++i) {
            vertex[i] -= center;
            vertex[i].rotate(angle / DEGREE * pi);
            vertex[i] += center;
        }
    }
    void reflect(const Point& center) {
        for (size_t i = 0; i < vertex.size(); ++i) {
            vertex[i] -= center;
            vertex[i] = center - vertex[i];
        }
    }
    void reflect(const Line& axis) {
        Point n = {axis.A, axis.B};
        n /= n.sz();
        for (size_t i = 0; i < vertex.size(); ++i) {
            double dd = dist(vertex[i], axis);
            vertex[i] += (n * dd);
            if (dist(vertex[i], axis) < eps) {
                vertex[i] += (n * dd);
            } else {
                // NOLINTNEXTLINE(readability-magic-numbers)
                vertex[i] -= (n * dd * 3);
            }
        }
    }
    void scale(const Point& center, double coefficient) {
        for (size_t i = 0; i < vertex.size(); ++i) {
            vertex[i] -= center;
            vertex[i] *= coefficient;
            vertex[i] += center;
        }
    }
    bool isSimilarTo(const Shape& another) const {
        auto p = dynamic_cast<const Polygon*>(&another);
        if (p == nullptr) {
            return false;
        }
        return isSimilarTo(*p);
    }
    bool isCongruentTo(const Shape& another) const {
        auto p = dynamic_cast<const Polygon*>(&another);
        if (p == nullptr) {
            return false;
        }
        return isCongruentTo(*p);
    }
    bool isCongruentTo(const Polygon& other, bool fl = true) const {
        if (vertex.size() != other.vertex.size()) {
            return false;
        }
        for (size_t i = 0; i < vertex.size(); ++i) {
            double k = dist(vertex[(i + 1) % vertex.size()], vertex[i]) / dist(other.vertex[1], other.vertex[0]);
            if (fl && std::fabs(k - 1) > eps) {
                continue;
            }
            bool f = true;
            for (size_t j = 0; j < other.vertex.size(); ++j) {
                double pk = dist(vertex[(i + j + 1) % vertex.size()], vertex[(i + j) % vertex.size()]) / dist(other.vertex[(j + 1) % other.vertex.size()], other.vertex[j]);
                if (std::fabs(pk - k) > eps) {
                    f = false;
                    break;
                }
                Point v1 = vertex[(i + j + 1) % vertex.size()] - vertex[(i + j) % vertex.size()];
                Point v2 = vertex[(i + j + 2) % vertex.size()] - vertex[(i + j + 1) % vertex.size()];
                double angle1 = dot_product(v1, v2) / v1.sz() / v2.sz();
                v1 = other.vertex[(j + 1) % other.vertex.size()] - other.vertex[j];
                v2 = other.vertex[(j + 2) % other.vertex.size()] - other.vertex[(j + 1) % other.vertex.size()];
                double angle2 = dot_product(v1, v2) / v1.sz() / v2.sz();
                if (std::fabs(angle1 - angle2) > eps) {
                    f = false;
                    break;
                }
            }
            if (f)
                return f;
        }
        for (size_t i = 0; i < vertex.size(); ++i) {
            double k = dist(vertex[(i + vertex.size() - 1) % vertex.size()], vertex[i]) / dist(other.vertex[1], other.vertex[0]);
            if (fl && std::fabs(k - 1) > eps) {
                continue;
            }
            bool f = true;
            for (size_t j = 0; j < other.vertex.size(); ++j) {
                double pk = dist(vertex[(i + vertex.size() - j - 1) % vertex.size()], vertex[(i + vertex.size() - j) % vertex.size()]) / dist(other.vertex[(j + 1) % other.vertex.size()], other.vertex[j]);
                if (std::fabs(pk - k) > eps) {
                    f = false;
                    break;
                }
                Point v1 = vertex[(i + vertex.size() - j - 1) % vertex.size()] - vertex[(i + vertex.size() - j) % vertex.size()];
                Point v2 = vertex[(i + vertex.size() * 2 - j - 2) % vertex.size()] - vertex[(i + vertex.size() - j - 1) % vertex.size()];
                double angle1 = dot_product(v1, v2) / v1.sz() / v2.sz();
                v1 = other.vertex[(j + 1) % other.vertex.size()] - other.vertex[j];
                v2 = other.vertex[(j + 2) % other.vertex.size()] - other.vertex[(j + 1) % other.vertex.size()];
                double angle2 = dot_product(v1, v2) / v1.sz() / v2.sz();
                if (std::fabs(angle1 - angle2) > eps) {
                    f = false;
                    break;
                }
            }
            if (f)
                return f;
        }
        return false;
    }
    bool isSimilarTo(const Polygon& other) const {
        return isCongruentTo(other, false);
    }
    bool containsPoint(const Point& point) const {
        double s = 0;
        for (size_t i = 0; i < vertex.size(); i++) {
            if (fabs(dist(point, vertex[i]) + dist(point, vertex[(i + 1) % vertex.size()]) - dist(vertex[i], vertex[(i + 1) % vertex.size()])) < eps) {
                return true;
            }
        }
        for (size_t i = 0; i < vertex.size(); ++i) {
            if (vertex[i] == point) {
                return true;
            }
            s += atan2(cross_product((vertex[i] - point), (vertex[(i + 1) % vertex.size()] - point)), dot_product((vertex[i] - point), (vertex[(i + 1) % vertex.size()] - point))) / pi * DEGREE;
        }
        return !(std::abs(s) <= 1.0);
    }
};

class Ellipse: public Shape {
protected:
    Point f1, f2;
    double a, b, r;
public:
    Ellipse(const Point& f1, const Point& f2, double r): f1(f1), f2(f2), a(r / 2), r(r) {
        Point c = (f1 + f2) / 2;
        // NOLINTNEXTLINE(readability-magic-numbers)
        b = sqrt((r * r) / 4 - dist(c, f1) * dist(c, f1));
    }
    std::pair<Point, Point> focuses() const {
        return {f1, f2};
    }
    std::pair<Line, Line> directrices() {
        Line big(f1, f2);
        Point dr(-big.B, big.A);
        dr /= dist(dr, Point{0, 0});
        dr *= (a / eccentricity());
        Line ex1((f1 + f2) / 2 + dr, (f1 + f2) / 2 + dr + Point(big.A, big.B));
        Line ex2((f1 + f2) / 2 - dr, (f1 + f2) / 2 - dr + Point(big.A, big.B));
        return {ex1, ex2};
    }
    double eccentricity() const {
        return sqrt(a * a - b * b) / a;
    }
    Point center() const {
        return (f1 + f2) / 2;
    }
    double perimeter() const {
        // NOLINTNEXTLINE(readability-magic-numbers)
        return pi * (3 * (a + b) - sqrt((3 * a + b) * (a + 3 * b)));
    }
    double area() const {
        return pi * a * b;
    }
    bool operator==(const Shape& another) const {
        auto p = dynamic_cast<const Ellipse*>(&another);
        if (p == nullptr) {
            return false;
        }
        return operator==(*p);
    }
    bool operator==(const Ellipse& other) const {
        return (f1 == other.f1 && f2 == other.f2 && fabs(other.r - r) < eps);
    }
    void rotate(const Point& center, double angle) {
        f1 -= center;
        f1.rotate(-angle);
        f1 += center;
        f2 -= center;
        f2.rotate(-angle);
        f2 += center;
    }
    void reflect(const Point& center) {
        f1 = center - (f1 - center);
        f2 = center - (f2 - center);
    }
    void reflect(const Line& axis) {
        Point n = {axis.A, axis.B};
        n /= n.sz();
        double dd = dist(f1, axis);
        if (dist(f1 + n * dd, axis) < eps) {
            f1 += (n * dd) * 2;
        } else {
            f1 -= (n * dd) * 2;
        }
        dd = dist(f2, axis);
        if (dist(f2 + n * dd, axis) < eps) {
            f2 += (n * dd) * 2;
        } else {
            f2 -= (n * dd) * 2;
        }
    }
    void scale(const Point& center, double coefficient) {
        f1 -= center;
        f1 *= coefficient;
        f1 += center;
        f2 -= center;
        f2 *= coefficient;
        f2 += center;
        a *= coefficient;
        b *= coefficient;
        r *= coefficient;
    }
    bool isCongruentTo(const Shape& another) const {
        auto p = dynamic_cast<const Ellipse*>(&another);
        if (p == nullptr) {
            return false;
        }
        return isCongruentTo(*p);
    }
    bool isSimilarTo(const Shape& another) const {
        auto p = dynamic_cast<const Ellipse*>(&another);
        if (p == nullptr) {
            return false;
        }
        return isSimilarTo(*p);
    }
    bool isCongruentTo(const Ellipse& another) const {
        return (a == another.a && b == another.b && fabs(dist(f1, f2) - dist(another.f1, another.f2)) < eps);
    }
    bool isSimilarTo(const Ellipse& another) const {
        return (this->eccentricity() == another.eccentricity());
    }
    bool containsPoint(const Point& point) const {
        return ((dist(f1, point) + dist(f2, point) - r) < eps);
    }
};

class Circle: public Ellipse {
public:
    Circle(const Point& c, double r): Ellipse(c, c, 2 * r) {}
    double radius() const {
        return a;
    }
    double perimeter() const {
        return 2 * pi * a;
    }
    double area() const {
        return pi * a * a;
    }
};

class Rectangle: public Polygon {
public:
    Rectangle(const Point& a, const Point& b, double k) {
        if (k < 1) {
            k = 1.0 / k;
        }
        double dl = dist(a, b);
        double w = sqrt((dl * dl) / (1.0 + k * k));
        double l = w * k;
        Point c = (b - a) / dl * w;
        Point d = (b - a) / dl * l;
        c.rotate(acos(w / dl));
        d.rotate(-acos(l / dl));
        c = a + c;
        d = a + d;
        vertex = {a, c, b, d};
    }
    Point center() const {
        return (vertex[0] + vertex[2]) / 2;
    }
    std::pair<Line, Line> diagonals() const {
        // NOLINTNEXTLINE(readability-magic-numbers)
        return {Line(vertex[0], vertex[2]), Line(vertex[1], vertex[3])};
    }
    double perimeter() const {
        // NOLINTNEXTLINE(readability-magic-numbers)
        return (dist(vertex[0], vertex[1]) + dist(vertex[1], vertex[2])) * 2;
    }
    double area() const {
        return dist(vertex[0], vertex[1]) * dist(vertex[1], vertex[2]);
    }
};

class Square: public Rectangle {
public:
    Square(const Point& a, const Point& b): Rectangle(a, b, 1.0) {}
    double perimeter() const {
        // NOLINTNEXTLINE(readability-magic-numbers)
        return dist(vertex[0], vertex[1]) * 4;
    }
    double area() const {
        return dist(vertex[0], vertex[1]) * dist(vertex[0], vertex[1]);
    }
    Circle circumscribedCircle() const {
        return Circle(this->center(), dist(vertex[0], vertex[2]) / 2);
    }
    Circle inscribedCircle() const {
        return Circle(this->center(), dist(vertex[0], vertex[1]) / 2);
    }
};

class Triangle: public Polygon {
public:
    Triangle(const Point& a, const Point& b, const Point& c) {
        vertex = {a, b, c};
    }
    Circle circumscribedCircle() const {
        Line a(vertex[0], vertex[1]), b(vertex[1], vertex[2]);
        Point m1 = (vertex[0] + vertex[1]) / 2;
        Point m2 = (vertex[1] + vertex[2]) / 2;
        Line p1(m1, Point(a.A, a.B) + m1);
        Line p2(m2, Point(b.A, b.B) + m2);
        Point p = p1 ^ p2;
        return Circle(p, dist(p, vertex[0]));
    }
    Circle inscribedCircle() const {
        Point p1 = (vertex[1] - vertex[0]) / dist(vertex[1], vertex[0]);
        Point p2 = (vertex[2] - vertex[0]) / dist(vertex[2], vertex[0]);
        p1 += p2;
        Line b1(vertex[0], vertex[0] + p1);
        p1 = (vertex[0] - vertex[1]) / dist(vertex[0], vertex[1]);
        p2 = (vertex[2] - vertex[1]) / dist(vertex[2], vertex[1]);
        p1 += p2;
        Line b2(vertex[1], vertex[1] + p1);
        Point p = (b1 ^ b2);
        return Circle(p, dist(p, Line(vertex[0], vertex[1])));
    }
    Point centroid() const {
        // NOLINTNEXTLINE(readability-magic-numbers)
        return (vertex[0] + vertex[1] + vertex[2]) / 3;
    }
    Point orthocenter() const {
        Line s1(vertex[0], vertex[1]);
        Line s2(vertex[1], vertex[2]);
        Line h1(vertex[2], Point(s1.A, s1.B) + vertex[2]);
        Line h2(vertex[0], Point(s2.A, s2.B) + vertex[0]);
        return (h1 ^ h2);
    }
    Line EulerLine() const {
        return Line(orthocenter(), centroid());
    }
    Circle ninePointsCircle() const {
        Triangle centr_t({(vertex[0] + vertex[1]) / 2, (vertex[1] + vertex[2]) / 2, (vertex[2] + vertex[0]) / 2});
        return centr_t.circumscribedCircle();
    }
};
