#include <cmath>
#include <array>
#include <cassert>
#include <cstring>
#include <ostream>
#include <istream>
#include <algorithm>
#include <vector>
#include <cmath>
#include <complex>
#include <deque>

const int block = 2;
const int mod = 100;
const int DISK = 10;
const int MAXPRESITION = 18;

const double pi = acos(-1.0);
typedef std::complex<double> base;
class BigInteger;
class Rational;
BigInteger operator *(BigInteger a, const BigInteger& b);
Rational operator *(Rational a, const Rational& b);
bool operator <=(const BigInteger& a, const BigInteger& b);
bool operator ==(const BigInteger& a, const BigInteger& b);
std::ostream& operator << (std::ostream& os, const BigInteger& x);

void gen_inv(int* inv, int sz) {
    inv[0] = 0;
    for (int i = 1; i < (1 << sz); ++i) {
        inv[i] = (inv[i >> 1] >> 1) | ((i & 1) << (sz - 1));
    }
}

void fft(base* arr, int* inv, int DEG, int K, bool invert = false) {
    for (int i = 0; i < DEG; ++i) {
        if (i < inv[i]) {
            std::swap(arr[i], arr[inv[i]]);
        }
    }
    base x, y;
    for (int i = 0; i < K; ++i) {
        double ang = 2 * pi / (1 << (i + 1)) * (invert ? -1 : 1);
        base wn(cos(ang), sin(ang));
        for (int j = 0; j < DEG; j+=(1 << (i + 1))) {
            base w(1);
            for (int k = j; k < j + (1 << i); k++) {
                x = arr[k];
                y = arr[k + (1 << i)] * w;
                arr[k] = x + y;
                arr[k + (1 << i)] = x - y;
                w *= wn;
            }
        }
    }
    if (invert) {
        for (int i = 0; i < DEG; ++i) {
            arr[i] /= DEG;
        }
    }
}

class BigInteger {
friend class Rational;
friend bool operator <(const BigInteger& a, const BigInteger& b);
private:
    std::vector <int> digits;
    bool sign = false;
public:
    BigInteger(): BigInteger(0) {}
    BigInteger(const char* str) {
        if (std::strlen(str) > 0 && str[0] == '-') {
            sign = true;
        }
        int x = 0;
        for (int i = static_cast<int>(std::strlen(str)) - 1; i >= sign; i-=block) {
            x = 0;
            for (int j = std::max(i - block + 1, sign ? 1 : 0); j <= i; ++j) {
                x *= DISK;
                x += (str[j] - '0');
            }
            digits.push_back(x);
        }
        check_zero();
    }
    BigInteger(const std::string &str) {
        if (!str.empty() && str[0] == '-') {
            sign = true;
        }
        int x = 0;
        for (int i = static_cast<int>(str.size()) - 1; i >= sign; i-=block) {
            x = 0;
            for (size_t j = static_cast<size_t>(std::max(i - block + 1, sign ? 1 : 0)); j <= static_cast<size_t>(i); ++j) {
                x *= DISK;
                x += (str[j] - '0');
            }
            digits.push_back(x);
        }
        check_zero();
    }
    BigInteger(int x): sign(x < 0) {
        x = abs(x);
        while (x > 0) {
            digits.push_back(x % mod);
            x /= mod;
        }
        check_zero();
    }
    void check_zero() {
        if (digits.size() == 1 && digits[0] == 0) {
            sign = false;
        }
        if (digits.empty()) {
            digits.push_back(0);
            sign = false;
        }
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
    }
    BigInteger operator -() {
        BigInteger neg = *this;
        neg.sign ^= 1;
        neg.check_zero();
        return neg;
    }
    BigInteger& operator --() {
        *this -= 1;
        return *this;
    }
    BigInteger operator --(int) {
        BigInteger copy = *this;
        --*this;
        return copy;
    }
    BigInteger& operator ++() {
        *this += 1;
        return *this;
    }
    BigInteger operator ++(int) {
        BigInteger copy = *this;
        ++*this;
        return copy;
    }
    BigInteger& operator -=(const BigInteger& a) {
        if (this == &a) {
            *this = 0;
            return *this;
        }
        if (sign != a.sign) {
            this->sign ^= 1;
            *this += a;
            this->sign ^= 1;
        } else {
            if ((sign && a < *this) || (!sign && *this < a)) {
                while (digits.size() < a.digits.size()) {
                    digits.push_back(0);
                }
                digits.push_back(0);
                for (size_t i = 0; i < a.digits.size(); ++i) {
                    digits[i] = a.digits[i] - digits[i];
                    if (digits[i] < 0) {
                        digits[i] += mod;
                        digits[i + 1] += 1;
                    }
                }
                sign ^= 1;
            } else {
                for (size_t i = 0; i < a.digits.size(); ++i) {
                    digits[i] -= a.digits[i];
                    if (digits[i] < 0) {
                        digits[i] += mod;
                        digits[i + 1] -= 1;
                    }
                }
                for (size_t i = a.digits.size(); i < digits.size() - 1; ++i) {
                    if (digits[i] < 0) {
                        digits[i] += mod;
                        digits[i + 1] -= 1;
                    }
                }
            }
        }
        check_zero();
        return *this;
    }
    BigInteger& operator *=(int k) {
        sign ^= (k < 0);
        k = abs(k);
        int next_rang = 0;
        size_t ind = 0;
        int mid = 0;
        while (ind < digits.size()) {
            mid = digits[ind] * k;
            digits[ind] = (next_rang + mid) % mod;
            next_rang = (next_rang + mid) / mod;
            if (next_rang != 0 && ind + 1 == digits.size()) {
                digits.push_back(0);
            }
            ++ind;
        }
        check_zero();
        return *this;
    }
    BigInteger& operator +=(const BigInteger& a) {
        if (this == &a) {
            *this *= 2;
            return *this;
        }
        if (sign == a.sign) {
            while (a.digits.size() > digits.size()) {
                digits.push_back(0);
            }
            digits.push_back(0);
            for (size_t i = 0; i < a.digits.size(); ++i) {
                digits[i] += a.digits[i];
                if (digits[i] >= mod) {
                    digits[i] -= mod;
                    digits[i + 1] += 1;
                }
            }
            for (size_t i = a.digits.size(); i < digits.size() - 1; ++i) {
                if (digits[i] >= mod) {
                    digits[i] -= mod;
                    digits[i + 1] += 1;
                }
            }
        } else {
            this->sign ^= 1;
            *this -= a;
            this->sign ^= 1;
        }
        check_zero();
        return *this;
    }

    explicit operator int() const {
        int x = 0;
        size_t i = digits.size();
        do {
            --i;
            x *= mod;
            x += digits[i];
        } while (i != 0);
        return x * (sign ? -1 : 1);
    }
    explicit operator bool() const {
        return (digits[0] == 0 && digits.size() == 1) ? false : true;
    }
    BigInteger& operator <<=(int k) {
        digits.insert(digits.begin(), k);
        check_zero();
        return *this;
    }
    std::string toString() const {
        std::string s;
        if (sign) {
            s.push_back('-');
        }
        s += std::to_string(digits[digits.size() - 1]);
        int st = 0;
        if (digits.size() > 1) {
            size_t i = digits.size() - 1;
            do {
                --i;
                if (digits[i] == 0) {
                    for (int j = 0; j < block; ++j) {
                        s.push_back('0');
                    }
                } else {
                    st = 1;
                    while (digits[i] * st * DISK < mod) {
                        st *= DISK;
                        s.push_back('0');
                    }
                    s += std::to_string(digits[i]);
                }
            } while (i != 0);
        }
        return s;
    }
    BigInteger& operator *=(const BigInteger& b) {
        int K = 0;
        int DEG = 0;
        while (static_cast<int>(std::max(digits.size(), b.digits.size())) > (1 << K)) {
            K++;
        }
        K++;
        DEG = (1 << K);
        int* inv = new int[static_cast<size_t>(DEG)];
        base* arr_a = new base[static_cast<size_t>(DEG)];
        base* arr_b = new base[static_cast<size_t>(DEG)];
        for (size_t i = 0; i < digits.size(); ++i) {
            arr_a[i] = {static_cast<double>(digits[i]), 0.0};
        }
        for (size_t i = 0; i < b.digits.size(); ++i) {
            arr_b[i] = {static_cast<double>(b.digits[i]), 0.0};
        }
        gen_inv(inv, K);
        fft(arr_a, inv, DEG, K);
        fft(arr_b, inv, DEG, K);
        for (int i = 0; i < DEG; ++i) {
            arr_a[i] *= arr_b[i];
        }
        fft(arr_a, inv, DEG, K, true);
        size_t mx = 0;
        for (int i = DEG - 1; i >= 0; --i) {
            if (round(arr_a[i].real()) != 0) {
                mx = static_cast<size_t>(i);
                break;
            }
        }
        ++mx;
        while (digits.size() < mx) {
            digits.push_back(0);
        }
        while (digits.size() > mx) {
            digits.pop_back();
        }
        for (size_t i = 0; i < mx; ++i) {
            digits[i] = static_cast<int>(round(arr_a[i].real()));
        }
        int next_rang = 0;
        int mid = 0;
        size_t ind = 0;
        while (ind < digits.size()) {
            mid = digits[ind];
            digits[ind] = (next_rang + digits[ind]) % mod;
            next_rang = (next_rang + mid) / mod;
            if (next_rang != 0 && ind + 1 == digits.size()) {
                digits.push_back(0);
            }
            ++ind;
        }
        sign ^= b.sign;
        check_zero();
        delete[] arr_a;
        delete[] arr_b;
        delete[] inv;
        return *this;
    }
    BigInteger& operator /=(const BigInteger& b) {
        if (digits.size() < b.digits.size()) {
            digits = {0};
            sign = false;
            return *this;
        }
        BigInteger x;
        std::vector <int> ans;
        for (size_t i = digits.size() - 1; i > digits.size() - b.digits.size(); --i) {
            x <<= digits[i];
        }
        size_t i = digits.size() - b.digits.size() + 1;
        do {
            --i;
            x <<= digits[i];
            int l = 0, r = mod;
            while (r - l > 1) {
                int mid = (r + l) / 2;
                BigInteger res = b;
                res.sign = false;
                res *= mid;
                if (res <= x) {
                    l = mid;
                } else {
                    r = mid;
                }
            }
            ans.push_back(l);
            BigInteger res = b;
            res.sign = false;
            res *= l;
            x -= res;
        } while (i != 0);
        std::reverse(ans.begin(), ans.end());
        digits = ans;
        sign ^= b.sign;
        check_zero();
        return *this;
    }
    BigInteger& operator %=(const BigInteger& b) {
        if (digits.size() < b.digits.size()) {
            return *this;
        }
        BigInteger x;
        for (size_t i = digits.size() - 1; i > digits.size() - b.digits.size(); --i) {
            x <<= digits[i];
        }
        size_t i = digits.size() - b.digits.size() + 1;
        do {
            --i;
            x <<= digits[i];
            int l = 0, r = mod;
            while (r - l > 1) {
                int mid = (r + l) / 2;
                BigInteger res = b;
                res.sign = false;
                res *= mid;
                if (res <= x) {
                    l = mid;
                } else {
                    r = mid;
                }
            }
            BigInteger res = b;
            res.sign = false;
            res *= l;
            x -= res;
        } while (i != 0);
        *this = x;
        check_zero();
        return *this;
    }
};

bool operator <(const BigInteger& a, const BigInteger& b) {
    if (a.sign != b.sign) {
        return a.sign;
    }
    if (a.digits.size() == b.digits.size()) {
        size_t i = a.digits.size();
        do {
            --i;
            if (a.digits[i] == b.digits[i]) {
                continue;
            }
            return ((a.digits[i] < b.digits[i]) ^ a.sign);
        } while (i != 0);
        return false;
    }
    return ((a.digits.size() < b.digits.size()) ^ a.sign);
}

BigInteger gcd(BigInteger a, BigInteger b) {
	while (b) {
		a %= b;
		std::swap(a, b);
	}
	return a;
}

BigInteger operator"" _bi(unsigned long long x) {
    return BigInteger(std::to_string(x));
}

BigInteger operator *(BigInteger a, const BigInteger& b) {
    a *= b;
    return a;
}

BigInteger operator /(BigInteger a, const BigInteger& b) {
    a /= b;
    return a;
}

BigInteger operator %(BigInteger a, const BigInteger& b) {
    a %= b;
    return a;
}

bool operator >(const BigInteger& a, const BigInteger& b) {
    return (b < a);
}


BigInteger operator +(BigInteger a, const BigInteger& b) {
    a += b;
    return a;
}

BigInteger operator -(BigInteger a, const BigInteger& b) {
    a -= b;
    return a;
}

bool operator <=(const BigInteger& a, const BigInteger& b) {
    return !(a > b);
}

bool operator >=(const BigInteger& a, const BigInteger& b) {
    return !(a < b);
}

bool operator ==(const BigInteger& a, const BigInteger& b) {
    return !((a < b) || (a > b));
}

bool operator !=(const BigInteger& a, const BigInteger& b) {
    return !(a == b);
}

std::istream& operator >> (std::istream& in, BigInteger& x) {
    std::string s;
    in >> s;
    for (size_t i = 0; i < s.size(); ++i) {
        if (s[i] != '-' && (s[i] < '0' || s[i] > '9')) {
            throw std::runtime_error("is not number");
        }
        if (i != 0 && s[i] == '-') {
            throw std::runtime_error("is not number");
        }
    }
    x = s;
    return in;
}

std::ostream& operator << (std::ostream& os, const BigInteger& x) {
    os << x.toString();
    return os;
}

class Rational {
private:
    BigInteger x;
    BigInteger y;
    bool sign = false;
public:
    Rational() = default;
    Rational(const BigInteger& Bx): x(Bx), y(1), sign(x.sign) {
        x.sign = false;
    }
    Rational(int num): x(abs(num)), y(1), sign(num < 0) {}
    void reduct() {
        BigInteger g = gcd(x, y);
        x /= g;
        y /= g;
    }
    void check() {
        sign = (sign ^ x.sign ^ y.sign);
        x.sign = false;
        y.sign = false;
        reduct();
        if (x == 0) {
            sign = false;
        }
    }
    void check_zero() {
        if (x == 0) {
            sign = false;
        }
    }
    Rational& operator *=(const Rational& a) {
        x *= a.x;
        y *= a.y;
        sign ^= a.sign;
        check();
        return *this;
    }
    Rational& operator /=(const Rational& a) {
        x *= a.y;
        y *= a.x;
        sign ^= a.sign;
        check();
        return *this;
    }
    Rational& operator +=(const Rational& a) {
        x *= a.y;
        if (a.sign ^ sign) {
            x -= (a.x * y);
        } else {
            x += (a.x * y);
        }
        y *= a.y;
        check();
        return *this;
    }
    Rational& operator -=(const Rational& a) {
        x *= a.y;
        if (a.sign ^ sign) {
            x += (a.x * y);
        } else {
            x -= (a.x * y);
        }
        y *= a.y;
        check();
        return *this;
    }
    bool operator <(const Rational& a) const {
        if (sign != a.sign) {
            return sign;
        }
        if (x * a.y < a.x * y) {
            return sign ^ 1;
        } else if (x * a.y > a.x * y) {
            return sign;
        }
        return false;
    }
    Rational operator -() {
        Rational neg = *this;
        neg.sign ^= 1;
        check_zero();
        return neg;
    }
    std::string toString() {
        std::string str;
        if (sign) {
            str.push_back('-');
        }
        if (y == 1) {
            str += x.toString();
        } else {
            str += x.toString();
            str += '/';
            str += y.toString();
        }
        return str;
    }
    std::string asDecimal(size_t precision=0) const {
        std::string str;
        if (sign) {
            str.push_back('-');
        }
        str += (x / y).toString();
        if (precision == 0) {
            return str;
        }
        str.push_back('.');
        BigInteger remind = x % y;
        std::string zeroes = "1";
        for (int i = 0; i < static_cast<int>(y.digits.size()) - static_cast<int>(remind.digits.size()) - 1; ++i) {
            if (precision < 2) {
                break;
            }
            remind <<= 0;
            str += "00";
            precision -= 2;
        }
        while (precision > 0 && remind * DISK < y) {
            remind *= DISK;
            str.push_back('0');
            --precision;
        }
        for (size_t i = 0; i < precision; ++i) {
            zeroes.push_back('0');
        }
        std::string real = (remind * BigInteger(zeroes) / y).toString();
        if (real != "0") {
            str += real;
        }
        return str;
    }
    explicit operator double() const {
        return std::stod(asDecimal(MAXPRESITION));
    }
};

std::istream& operator >> (std::istream& in, Rational& a) {
    std::string s;
    in >> s;
    BigInteger x = 0, y = 1;
    bool f = false;
    size_t z = 0;
    if (s[0] == '-') {
        z = 1;
    }
    for (size_t i = z; i < s.size(); ++i) {
        if (s[i] == '.') {
            f = true;
            continue;
        }
        if (f) {
            // NOLINTNEXTLINE(readability-magic-numbers)
            y *= 10;
        }
        // NOLINTNEXTLINE(readability-magic-numbers)
        x *= 10;
        x += (s[i] - '0');
    }
    a = x;
    if (z == 1) {
        a *= -1;
    }
    a /= y;
    return in;
}

Rational operator *(Rational a, const Rational& b) {
    a *= b;
    return a;
}

Rational operator /(Rational a, const Rational& b) {
    a /= b;
    return a;
}

Rational operator +(Rational a, const Rational& b) {
    a += b;
    return a;
}

Rational operator -(Rational a, const Rational& b) {
    a -= b;
    return a;
}

bool operator >(const Rational& a, const Rational& b) {
    return b < a;
}

bool operator <=(const Rational& a, const Rational& b) {
    return !(a > b);
}

bool operator >=(const Rational& a, const Rational& b) {
    return !(a < b);
}

bool operator ==(const Rational& a, const Rational& b) {
    return !((a < b) || (a > b));
}

bool operator !=(const Rational& a, const Rational& b) {
    return !(a == b);
}

template<size_t N>
struct lower_power_of_2 {
    static const size_t value = lower_power_of_2<(N >> 1)>::value * 2;
};

template<>
struct lower_power_of_2<1> {
    static const size_t value = 1;
};

template <size_t N, size_t D>
struct is_prime_helper {
    static const bool value = (!(N % D == 0) && is_prime_helper<N, D - 1>::value);
};

template<size_t N>
struct is_prime_helper<N, 1> {
    static const bool value = true;
};

template<size_t N>
struct is_prime_helper<N, N> {
    static const bool value = true;
};

template <size_t N, size_t L = 1, size_t R = N>
struct Root {
    static const size_t mid = (L + R) / 2;
    static const bool check = ((mid * mid) >= N);
    static const size_t res = Root<N,(check ? L : mid + 1),(check ? mid : R)>::res;
};

template <size_t N, size_t Mid>
struct Root<N,Mid,Mid> {
    static const size_t res = Mid;
};

template <size_t N>
struct is_prime {
    static const bool value = is_prime_helper<N, Root<N>::res>::value;
};

long long bin_pow(long long x, int k, int mod) {
    if (k == 0) {
        return 1;
    }
    if (k % 2 == 0) {
        long long ans = bin_pow(x, k / 2, mod);
        return (ans * ans) % mod;
    } else {
        return (x * bin_pow(x, k - 1, mod)) % mod;
    }
}

template <size_t N>
class Residue {
private:
    size_t x;
public:
    Residue() = default;
    explicit Residue(int nx): x((nx % N + N) % N) {}
    Residue<N>& operator +=(const Residue<N>& b) {
        x += b.x;
        x %= N;
        return *this;
    }
    Residue<N>& operator -=(const Residue<N>& b) {
        x = (x - b.x + N) % N;
        return *this;
    }
    Residue<N>& operator *=(const Residue<N>& b) {
        x = (static_cast<long long>(x) * b.x) % N;
        x %= N;
        return *this;
    }
    explicit operator int() const {
        return x;
    }
    Residue<N>& operator /=(const Residue<N>& b) {
        static_assert(is_prime<N>::value);
        x *= bin_pow(b.x, N - 2, N);
        x %= N;
        return *this;
    }
    size_t& get_x() {
        return x;
    }
};

template <size_t N>
std::istream& operator >> (std::istream& in, Residue<N>& a) {
    in >> a.get_x();
    return in;
}

template <size_t N>
bool operator ==(const Residue<N>& a, const Residue<N>& b) {
    return !((a < b) || (a > b));
}

template <size_t N>
bool operator !=(const Residue<N>& a, const Residue<N>& b) {
    return !(a == b);
}

template <size_t N>
Residue<N> operator +(Residue<N> a, const Residue<N>& b) {
    a += b;
    return a;
}

template <size_t N>
Residue<N> operator -(Residue<N> a, const Residue<N>& b) {
    a -= b;
    return a;
}

template <size_t N>
Residue<N> operator *(Residue<N> a, const Residue<N>& b) {
    a *= b;
    return a;
}

template <size_t N>
Residue<N> operator /(Residue<N> a, const Residue<N>& b) {
    a /= b;
    return a;
}

template <size_t N, size_t M, typename Field>
class Matrix;

template <size_t A, size_t B, size_t C, typename Field_t>
Matrix <A, C, Field_t> operator *(const Matrix<A, B, Field_t>& a, const Matrix<B, C, Field_t>& b);

const size_t MIN_SIZE = 4;

template <size_t N, size_t M, typename Field = Rational>
class Matrix {
private:
    std::array <std::array<Field, M>, N> m;
    Matrix<N, M, Field> get_Gaus(int &sign, bool f = false) const {
        Matrix <N, N, Field> invm, newm = *this;
        for (size_t i = 0; i < N; ++i) {
            invm[i][i] = 1;
        }
        for (size_t i = 0; i < N; ++i) {
            if (newm[i][i] == 0) {
                bool f = false;
                for (size_t j = i + 1; j < N; ++j) {
                    if (newm[j][i] != 0) {
                        f = true;
                        sign *= -1;
                        swap(newm[j], newm[i]);
                        swap(invm[j], invm[i]);
                        break;
                    }
                }
                if (!f) {
                    continue;
                }
            }
            for (size_t j = 0; j < N; ++j) {
                if (j == i) {
                    continue;
                }
                Field d = newm[j][i] / newm[i][i];
                for (size_t k = 0; k < N; ++k) {
                    newm[j][k] -= d * newm[i][k];
                    invm[j][k] -= d * invm[i][k];
                }
            }
        }
        if (f) {
            for (size_t i = 0; i < N; ++i) {
                for (size_t j = 0; j < N; ++j) {
                    invm[i][j] /= newm[i][i];
                }
            }
            return invm;
        }
        return newm;
    }
public:
    Matrix() {
        for (size_t i = 0; i < N; ++i) {
            std::fill(m[i].begin(), m[i].end(), Field(0));
        }
    }
    Matrix(std::initializer_list<std::initializer_list<Field>> l) {
        assert(l.size() == N);
        size_t ind = 0;
        for (auto i : l) {
            assert(i.size() == M);
            for (auto j : i) {
                m[ind / M][ind % M] = j;
                ++ind;
            }
        }
    }
    Matrix<N, M, Field>& operator +=(const Matrix<N, M, Field>& b) {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                m[i][j] += b[i][j];
            }
        }
        return *this;
    }
    Matrix<N, M, Field>& operator -=(const Matrix<N, M, Field>& b) {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                m[i][j] -= b[i][j];
            }
        }
        return *this;
    }
    Matrix<N, M, Field>& operator *=(const Field& x) {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                m[i][j] *= x;
            }
        }
        return *this;
    }
    Field det() const {
        static_assert(N == M, "Matrix is not square");
        int sign = 1;
        Matrix <N, N, Field> newm = get_Gaus(sign);
        Field ans = 1;
        for (size_t i = 0; i < N; ++i) {
            ans *= newm[i][i];
        }
        return ans * sign;
    }
    size_t rank() const {
        int sign = 1;
        Matrix <N, N, Field> newm = get_Gaus(sign);
        size_t rk = 0;
        for (size_t i = 0; i < std::min(N, M); ++i) {
            rk += (newm[i][i] != 0);
        }
        return rk;
    }
    Matrix<N, N, Field>& invert() {
        static_assert(N == M, "Matrix is not square");
        int sign = 1;
        *this = get_Gaus(sign, true);
        return *this;
    }
    Matrix<N, N, Field> inverted() const {
        Matrix<N, N, Field> newm = *this;
        return newm.invert();
    }
    Matrix<M, N, Field> transposed() const {
        Matrix <M, N, Field> aT;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                aT[j][i] = m[i][j];
            }
        }
        return aT;
    }
    Field trace() {
        static_assert(N == M, "Matrix is not square");
        Field tr = 0;
        for (size_t i = 0; i < N; ++i) {
            tr += m[i][i];
        }
        return tr;
    }
    std::array<Field, N> getRow(size_t ind) const {
        return m[ind];
    }
    std::array<Field, M> getColumn(size_t ind) const {
        std::array<Field, M> a;
        for (size_t i = 0; i < M; ++i) {
            a[i] = m[i][ind];
        }
        return a;
    }
    std::array<Field, M>& operator [](size_t x) {
        return m[x];
    }
    const std::array<Field, M>& operator [](size_t x) const {
        return m[x];
    }
    bool operator ==(const Matrix<N, M, Field> &b) const {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                if (m[i][j] != b[i][j]) {
                    return false;
                }
            }
        }
        return true;
    }
    Matrix<N, M, Field>& operator *=(const Matrix<M, N, Field>& b) {
        *this = *this * b;
        return *this;
    }
};

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator +(const Matrix<N, M, Field> &a, const Matrix<N, M, Field>& b) {
    auto copy = a;
    copy += b;
    return copy;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator -(const Matrix<N, M, Field> &a, const Matrix<N, M, Field>& b) {
    auto copy = a;
    copy -= b;
    return copy;
}

template <size_t N, typename Field = Rational>
using SquareMatrix = Matrix<N, N, Field>;

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator *(const Field& x, const Matrix<N, M, Field> &a) {
    auto copy = a;
    copy *= x;
    return copy;
}

template <size_t N, typename Field>
void split_matrix(const SquareMatrix<N, Field>& m, const SquareMatrix<N * 2, Field>& temp, size_t indx, size_t indy) {
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < N; ++j) {
            m[i][j] = temp[indx + i][indy + j];
        }
    }
}

template <size_t N, typename Field>
void merge_matrix(const SquareMatrix<N, Field>& m, const SquareMatrix<N * 2, Field>& temp, size_t indx, size_t indy) {
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < N; ++j) {
            temp[indx + i][indy + j] = m[i][j];
        }
    }
}

template <size_t N, typename Field>
SquareMatrix<N, Field> Strassen(const SquareMatrix<N, Field> &a, const SquareMatrix<N, Field> &b) {
    const size_t NEW_N = N / 2;
    SquareMatrix<NEW_N, Field> a11, a12, a21, a22, b11, b12, b21, b22, p1, p2, p3, p4, p5, p6, p7, c11, c12, c21, c22;
    split_matrix(a11, a, 0, 0);
    split_matrix(a12, a, 0, NEW_N);
    split_matrix(a21, a, NEW_N, 0);
    split_matrix(a22, a, NEW_N, NEW_N);
    split_matrix(b11, b, 0, 0);
    split_matrix(b12, b, 0, NEW_N);
    split_matrix(b21, b, NEW_N, 0);
    split_matrix(b22, b, NEW_N, NEW_N);
    p1 = Strassen(a11 + a22, b11 + b22);
    p2 = Strassen(a21 + a22, b11);
    p3 = Strassen(a11, b12 - b22);
    p4 = Strassen(a22, b21 - b11);
    p5 = Strassen(a11 + a12, b22);
    p6 = Strassen(a21 - a11, b11 + b12);
    p7 = Strassen(a12 - a22, b21 + b22);
    c11 = p1 + p4 -  p5 + p7;
    c12 = p3 + p5;
    c21 = p2 + p4;
    c22 = p1 - p2 + p3 + p6;
    SquareMatrix<N, Field> ans;
    merge_matrix(c11, ans, 0, 0);
    merge_matrix(c12, ans, 0, NEW_N);
    merge_matrix(c21, ans, NEW_N, 0);
    merge_matrix(c22, ans, NEW_N, NEW_N);
    return ans;
}

template <size_t N, typename Field>
SquareMatrix<MIN_SIZE, Field> Strassen(const SquareMatrix<MIN_SIZE, Field> &a, const SquareMatrix<MIN_SIZE, Field> &b) {
    SquareMatrix<MIN_SIZE, Field> ans;
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < N; ++j) {
            for (size_t k = 0; k < N; ++k) {
                ans[i][j] += a[i][k] * b[k][j];
            }
        }
    }
    return ans;
}

template <size_t A, size_t B, size_t C, typename Field_t>
Matrix <A, C, Field_t> operator *(const Matrix<A, B, Field_t>& a, const Matrix<B, C, Field_t>& b) {
    Matrix <A, C, Field_t> ans;
    if (std::min({A, B, C}) < MIN_SIZE) {
        for (size_t i = 0; i < A; ++i) {
            for (size_t j = 0; j < C; ++j) {
                for (size_t k = 0; k < B; ++k) {
                    ans[i][j] += (a[i][k] * b[k][j]);
                }
            }
        }
        return ans;
    }
    const size_t K = lower_power_of_2<std::max({A, B, C})>::value * 2;
    SquareMatrix<K, Field_t> sha, shb, shc;
    for (size_t i = 0; i < A; ++i) {
        for (size_t j = 0; j < B; ++j) {
            sha[i][j] = a[i][j];
        }
    }
    for (size_t i = 0; i < B; ++i) {
        for (size_t j = 0; j < C; ++j) {
            shb[i][j] = b[i][j];
        }
    }
    shc = Strassen<K, Field_t>(sha, shb);
    for (size_t i = 0; i < A; ++i) {
        for (size_t j = 0; j < C; ++j) {
            ans[i][j] = shc[i][j];
        }
    }
    return ans;
}

template <size_t N, size_t M, typename Field>
std::ostream& operator << (std::ostream& os, const Matrix<N, M, Field>& a) {
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < M; ++j) {
            os << a[i][j];
            if (j + 1 != M) {
                os << ' ';
            }
        }
        if (i + 1 != N) {
            os << '\n';
        }
    }
    return os;
}

template <size_t N, size_t M, typename Field>
std::istream& operator >> (std::istream& in, Matrix<N, M, Field>& a) {
    for (size_t i = 0; i < N; ++i) {
        for (size_t j = 0; j < M; ++j) {
            in >> a[i][j];
        }
    }
    return in;
}
