#include <cstring>
#include <ostream>
#include <istream>
#include <algorithm>
#include <vector>
#include <cmath>
#include <complex>
#include <deque>
#include <compare>
#include <math.h>

typedef std::complex<double> base;
class BigInteger;
class Rational;
BigInteger operator *(const BigInteger& a, const BigInteger& b);
Rational operator *(const Rational& a, const Rational& b);
std::ostream& operator << (std::ostream& os, const BigInteger& x);

class BigInteger {
private:
    static const int BLOCK = 2;
    static const int MOD = 100;
    static const int DISK = 10;
    void gen_inv(std::vector<size_t>& inv, size_t sz) {
        inv[0] = 0;
        for (size_t i = 1; i < static_cast<size_t>((1ULL << sz)); ++i) {
            inv[i] = (inv[i >> 1] >> 1) | ((i & 1) << (sz - 1));
        }
    }

    void fft(std::vector<base>& arr, std::vector<size_t>& inv, size_t DEG, size_t K, bool invert = false) {
        for (size_t i = 0; i < DEG; ++i) {
            if (i < inv[i]) {
                std::swap(arr[i], arr[inv[i]]);
            }
        }
        base x, y;
        for (size_t i = 0; i < K; ++i) {
            double ang = 2 * M_PI / (1 << (i + 1)) * (invert ? -1 : 1);
            base wn(cos(ang), sin(ang));
            for (size_t j = 0; j < DEG; j+=(1ULL << (i + 1))) {
                base w(1);
                for (size_t k = j; k < j + (1ULL << i); k++) {
                    x = arr[k];
                    y = arr[k + (1ULL << i)] * w;
                    arr[k] = x + y;
                    arr[k + (1UL << i)] = x - y;
                    w *= wn;
                }
            }
        }
        if (invert) {
            for (size_t i = 0; i < DEG; ++i) {
                arr[i] /= static_cast<int>(DEG);
            }
        }
    }
    void check_zero() {
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }
        if (digits.size() == 1 && digits[0] == 0) {
            is_negative = false;
        }
        if (digits.empty()) {
            digits.push_back(0);
            is_negative = false;
        }
    }
    void get(const BigInteger& a, bool negative = false) {
        digits.resize(std::max(a.digits.size(), digits.size()) + 1, 0);
        for (size_t i = 0; i < digits.size() - 1; ++i) {
            if (i < a.digits.size()) {
                digits[i] += a.digits[i] * (is_negative ^ negative ^ a.is_negative ? -1 : 1);
            }
            if (digits[i] < 0) {
                digits[i] += MOD;
                --digits[i + 1];
            }
            if (digits[i] >= MOD) {
                digits[i] -= MOD;
                ++digits[i + 1];
            }
        }
        if (digits.back() == -1) {
            is_negative ^= 1;
            for (size_t i = 0; i < digits.size() - 1; ++i) {
                if (digits[i] == 0) {
                    continue;
                }
                digits[i] = MOD - digits[i];
                ++digits[i + 1];
            }
        }
        check_zero();
    }
    std::vector <int> digits;
    bool is_negative = false;
public:
    BigInteger(): BigInteger(0) {}
    BigInteger(const std::string &str) {
        if (!str.empty() && str[0] == '-') {
            is_negative = true;
        }
        int x = 0;
        for (int i = static_cast<int>(str.size()) - 1; i >= is_negative; i-=BLOCK) {
            x = 0;
            for (size_t j = static_cast<size_t>(std::max(i - BLOCK + 1, is_negative ? 1 : 0)); j <= static_cast<size_t>(i); ++j) {
                x *= DISK;
                x += (str[j] - '0');
            }
            digits.push_back(x);
        }
        check_zero();
    }
    BigInteger(const char* str, size_t s) {
        if (s != 0 && str[0] == '-') {
            is_negative = true;
        }
        int x = 0;
        for (int i = static_cast<int>(s) - 1; i >= is_negative; i-=BLOCK) {
            x = 0;
            for (size_t j = static_cast<size_t>(std::max(i - BLOCK + 1, is_negative ? 1 : 0)); j <= static_cast<size_t>(i); ++j) {
                x *= DISK;
                x += (str[j] - '0');
            }
            digits.push_back(x);
        }
        check_zero();
    }
    BigInteger(int x): is_negative(x < 0) {
        x = std::abs(x);
        while (x > 0) {
            digits.push_back(x % MOD);
            x /= MOD;
        }
        check_zero();
    }
    BigInteger operator -() {
        BigInteger neg = *this;
        neg.is_negative ^= 1;
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
    void abs() {
        is_negative = false;
    }
    BigInteger& operator +=(const BigInteger& a) {
        if (this == &a) {
            *this *= 2;
            return *this;
        }
        get(a);
        return *this;
    }
    BigInteger& operator -=(const BigInteger& a) {
        if (this == &a) {
            *this = 0;
            return *this;
        }
        get(a, true);
        return *this;
    }
    BigInteger& operator *=(int k) {
        is_negative ^= (k < 0);
        k = std::abs(k);
        int next_rang = 0;
        size_t ind = 0;
        int mid = 0;
        while (ind < digits.size()) {
            mid = digits[ind] * k;
            digits[ind] = (next_rang + mid) % MOD;
            next_rang = (next_rang + mid) / MOD;
            if (next_rang != 0 && ind + 1 == digits.size()) {
                digits.push_back(0);
            }
            ++ind;
        }
        check_zero();
        return *this;
    }
    std::strong_ordering operator<=>(const BigInteger& b) const {
        if (is_negative != b.is_negative) {
            return is_negative ? std::strong_ordering::less : std::strong_ordering::greater;
        }
        if (digits.size() != b.digits.size()) {
            return ((digits.size() < b.digits.size()) ^ is_negative) ? std::strong_ordering::less : std::strong_ordering::greater;
        }
        for (size_t i = digits.size(); i--;) {
            if (digits[i] != b.digits[i]) {
                return ((digits[i] < b.digits[i]) ^ is_negative) ? std::strong_ordering::less : std::strong_ordering::greater;
            }
        }
        return std::strong_ordering::equal;
    }
    explicit operator int() const {
        int x = 0;
        for (size_t i = digits.size(); i--;) {
            x *= MOD;
            x += digits[i];
        }
        return x * (is_negative ? -1 : 1);
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
        if (is_negative) {
            s.push_back('-');
        }
        s += std::to_string(digits[digits.size() - 1]);
        int st = 0;
        for (size_t i = digits.size() - 1; i--; ) {
            if (digits[i] == 0) {
                for (int j = 0; j < BLOCK; ++j) {
                    s.push_back('0');
                }
                continue;
            }
            st = 1;
            while (digits[i] * st * DISK < MOD) {
                st *= DISK;
                s.push_back('0');
            }
            s += std::to_string(digits[i]);
        }
        return s;
    }
    BigInteger& operator *=(const BigInteger& b) {
        size_t K = 0;
        size_t DEG = 0;
        while (std::max(digits.size(), b.digits.size()) > (1ULL << K)) {
            K++;
        }
        K++;
        DEG = (1ULL << K);
        std::vector<size_t> inv(DEG);
        std::vector<base> arr_a(DEG), arr_b(DEG);
        std::copy(digits.begin(), digits.end(), arr_a.begin());
        std::copy(b.digits.begin(), b.digits.end(), arr_b.begin());
        gen_inv(inv, K);
        fft(arr_a, inv, DEG, K);
        fft(arr_b, inv, DEG, K);
        for (size_t i = 0; i < DEG; ++i) {
            arr_a[i] *= arr_b[i];
        }
        fft(arr_a, inv, DEG, K, true);
        size_t mx = 0;
        for (size_t i = DEG; i--;) {
            if (round(arr_a[i].real()) != 0) {
                mx = i;
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
            digits[ind] = (next_rang + digits[ind]) % MOD;
            next_rang = (next_rang + mid) / MOD;
            if (next_rang != 0 && ind + 1 == digits.size()) {
                digits.push_back(0);
            }
            ++ind;
        }
        is_negative ^= b.is_negative;
        check_zero();
        return *this;
    }
    BigInteger& operator /=(const BigInteger& b) {
        if (digits.size() < b.digits.size()) {
            digits = {0};
            is_negative = false;
            return *this;
        }
        BigInteger x;
        std::vector <int> ans;
        for (size_t i = digits.size() - 1; i > digits.size() - b.digits.size(); --i) {
            x <<= digits[i];
        }
        for (size_t i = digits.size() - b.digits.size() + 1; i--;) {
            x <<= digits[i];
            int l = 0, r = MOD;
            while (r - l > 1) {
                int mid = (r + l) / 2;
                BigInteger res = b;
                res.is_negative = false;
                res *= mid;
                if (res <= x) {
                    l = mid;
                } else {
                    r = mid;
                }
            }
            ans.push_back(l);
            BigInteger res = b;
            res.is_negative = false;
            res *= l;
            x -= res;
        }
        std::reverse(ans.begin(), ans.end());
        digits = ans;
        is_negative ^= b.is_negative;
        check_zero();
        return *this;
    }
    BigInteger& operator %=(const BigInteger& b) {
        BigInteger whole_part = *this;
        whole_part /= b;
        whole_part *= b;
        *this -= whole_part;
        return *this;
    }
};

BigInteger gcd(BigInteger a, BigInteger b) {
    a.abs(), b.abs();
	while (b) {
		a %= b;
		std::swap(a, b);
	}
	return a;
}

BigInteger operator"" _bi(unsigned long long x) {
    return BigInteger(std::to_string(x));
}

BigInteger operator"" _bi(const char* x, size_t s) {
    return BigInteger(x, s);
}

BigInteger operator +(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c += b;
    return c;
}

BigInteger operator -(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c -= b;
    return c;
}

BigInteger operator *(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c *= b;
    return c;
}

BigInteger operator /(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c /= b;
    return c;
}

BigInteger operator %(const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    c %= b;
    return c;
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
    static const int DISK = 10;
    static const int MAXPRESITION = 18;
    BigInteger num;
    BigInteger den;
    void reduct() {
        BigInteger g = gcd(num, den);
        num /= g;
        den /= g;
    }
public:
    Rational() = default;
    Rational(const BigInteger& Bx): num(Bx), den(1) {}
    Rational(int num): num(num), den(1) {}
    Rational& operator *=(const Rational& a) {
        num *= a.num;
        den *= a.den;
        reduct();
        return *this;
    }
    Rational& operator /=(const Rational& a) {
        num *= a.den;
        den *= a.num;
        if (den < 0) {
            num *= -1;
            den.abs();
        }
        reduct();
        return *this;
    }
    Rational& operator +=(const Rational& a) {
        num *= a.den;
        num += (a.num * den);
        den *= a.den;
        reduct();
        return *this;
    }
    Rational& operator -=(const Rational& a) {
        num *= a.den;
        num -= (a.num * den);
        den *= a.den;
        reduct();
        return *this;
    }
    std::strong_ordering operator<=>(const Rational& b) const {
        if ((num < 0) != (b.num < 0)) {
            return (num < 0) ? std::strong_ordering::less : std::strong_ordering::greater;
        }
        BigInteger diff = num * b.den - b.num * den;
        if (diff == 0) {
            return std::strong_ordering::equal;
        }
        return (diff < 0) ? std::strong_ordering::less : std::strong_ordering::greater;
    }
    Rational operator -() {
        Rational neg = *this;
        neg.num *= -1;
        return neg;
    }
    std::string toString() const {
        std::string str;
        str += num.toString();
        if (den != 1) {
            str += '/';
            str += den.toString();
        }
        return str;
    }
    std::string asDecimal(size_t precision=0) const {
        std::string str;
        std::string sign;
        if (num < 0) {
            sign = "-";
        }
        if (num == 0) {
            str = "0.";
            for (size_t i = 0; i < precision; ++i) {
                str.push_back('0');
            }
            return str;
        }
        BigInteger new_num = num;
        new_num.abs();
        if (precision == 0) {
            str = (new_num / den).toString();
            return sign + str;
        }
        bool f = new_num < den;
        for (size_t i = 0; i < precision; ++i) {
            new_num *= DISK;
        }
        str = (new_num / den).toString();
        if (f) {
            std::string add_str = "0.";
            while (str.size() < precision) {
                add_str.push_back('0');
                --precision;
            }
            return sign + add_str + str;
        }
        str.insert(str.size() - precision, 1, '.');
        return sign + str;
    }
    explicit operator double() const {
        return std::stod(asDecimal(MAXPRESITION));
    }
};

Rational operator +(const Rational& a, const Rational& b) {
    Rational c = a;
    c += b;
    return c;
}

Rational operator -(const Rational& a, const Rational& b) {
    Rational c = a;
    c -= b;
    return c;
}

Rational operator *(const Rational& a, const Rational& b) {
    Rational c = a;
    c *= b;
    return c;
}

Rational operator /(const Rational& a, const Rational& b) {
    Rational c = a;
    c /= b;
    return c;
}

bool operator ==(const Rational& a, const Rational& b) {
    return !((a < b) || (a > b));
}

bool operator !=(const Rational& a, const Rational& b) {
    return !(a == b);
}
