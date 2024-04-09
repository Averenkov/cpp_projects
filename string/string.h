#include <cstring>
#include <ostream>
#include <istream>
#include <algorithm>

class String {
private:
    size_t sz = 0;
    size_t cap = 0;
    char* arr;
    void update_cap(size_t new_cap) {
        cap = new_cap;
        char* inter_arr = new char[cap + 1];
        memcpy(inter_arr, arr, sz + 1);
        delete[] arr;
        arr = inter_arr;
    }
    void fill_last_element() {
        arr[sz] = '\0';
    }
public:
    String(): String(static_cast<size_t>(0)) {}
    String(size_t n, char c): sz(n), cap(n), arr(new char[n+1]) {
        memset(arr, c, n);
        fill_last_element();
    }
    String(String&& str): sz(str.sz), cap(str.cap), arr(str.arr) {
        str.sz = 0;
        str.cap = 0;
        str.arr = nullptr;
        std::cout << '#';
    }
    explicit String(size_t cap): sz(cap), cap(cap), arr(new char[cap+1]) {
        fill_last_element();
    }
    String(const String& str): String(str.sz) {
        memcpy(arr, str.arr, sz);
    }
    String(char c): String(static_cast<size_t>(1)) {
        arr[0] = c;
    }
    String(const char* str): sz(std::strlen(str)), cap(sz), arr(new char[sz + 1])  {
        memcpy(arr, str, sz);
        fill_last_element();
    }
    String& operator=(String str) {
        swap(str);
        return *this;
    }
    String& operator=(String&& str) {
        std::cout << '1';
        String c = std::move(str);
        swap(c);
        return *this;
    }
    void swap(String& str) {
        std::swap(sz, str.sz);
        std::swap(cap, str.cap);
        std::swap(arr, str.arr);
    }
    char& operator[](size_t ind) {
        return arr[ind];
    }
    const char& operator[](size_t ind) const {
        return arr[ind];
    }
    size_t length() const {
        return sz;
    }
    size_t size() const {
        return sz;
    }
    size_t capacity() const {
        return cap;
    }
    void push_back(const char c) {
        if (sz == cap) {
            update_cap(std::max(sz * 2, static_cast<size_t>(1)));
        }
        arr[sz++] = c;
        fill_last_element();
    }
    void pop_back() {
        sz--;
        fill_last_element();
    }
    char& front() {
        return arr[0];
    }
    const char& front() const {
        return arr[0];
    }
    char& back() {
        return arr[sz - 1];
    }
    const char& back() const {
        if (sz == 0)
            return arr[0];
        return arr[sz - 1];
    }
    String& operator+=(const String& str) {
        if (cap < sz + str.sz) {
            update_cap(sz + str.sz);
            memcpy(arr + sz, str.arr, str.sz);
            sz = cap;
        } else {
            memcpy(arr + sz, str.arr, str.sz);
            sz += str.sz;
        }
        fill_last_element();
        return *this;
    }
    String& operator+=(const char str) {
        push_back(str);
        return *this;
    }
    size_t find(const String& substring) const {
        for (int i = 0; i <= static_cast<int>(sz) - static_cast<int>(substring.sz); ++i) {
            if (memcmp(arr + i, substring.arr, substring.sz) == 0) {
                return static_cast<size_t>(i);
            }
        }
        return sz;
    }
    size_t rfind(const String& substring) const {
        for (int i = static_cast<int>(sz) - static_cast<int>(substring.sz); i >= 0; --i) {
            if (memcmp(arr + i, substring.arr, substring.sz) == 0) {
                return static_cast<size_t>(i);
            }
	}
        return sz;
    }
    String substr(size_t start, size_t count) const {
        String ans(count);
        memcpy(ans.arr, arr + start, count);
        return ans;
    }
    bool empty() const {
        return (sz == 0);
    }
    void clear() {
        sz = 0;
        fill_last_element();
    }
    void shrink_to_fit() {
        update_cap(sz);
        fill_last_element();
    }
    char* data() {
        return arr;
    }
    const char* data() const {
        return arr;
    }
    ~String() {
        delete[] arr;
    }
};


std::ostream& operator << (std::ostream& os, const String& str) {
    for (size_t i = 0; i < str.size(); ++i) {
        os << str[i];
    }
    return os;
}

std::istream& operator >> (std::istream& in, String& str) {
    char c;
    str.clear();
    int cnt = 0;
    while (true) {
        c = static_cast<char>(in.get());
        if (in.eof()) {
            break;
        }
        if (cnt > 0 && (c == ' ' || c == '\n')) {
            break;
        }
        if (c == ' ' || c == '\n') {
            continue;
        }
        cnt++;
        str.push_back(c);
    }
    return in;
}

bool operator==(const String& a, const String& b) {
    size_t ind = 0;
    while (a[ind] == b[ind]) {
        if (a[ind] == '\0') {
            return true;
        }
        ++ind;
    }
    return false;
}

bool operator<(const String& a, const String& b) {
    size_t ind = 0;
    while (a[ind] == b[ind]) {
        if (a[ind] == '\0') {
            return false;
        }
        ++ind;
    }
    return (a[ind] < b[ind]);
}

bool operator!=(const String& a, const String& b) {
    return !(a == b);
}

bool operator>(const String& a, const String& b) {
    return b < a;
}

bool operator<=(const String& a, const String& b) {
    return !(a > b);
}

bool operator>=(const String& a, const String& b) {
    return !(a < b);
}

String operator+(String a, const String& b) {
    a += b;
    return a;
}

String operator+(String a, const char& c) {
    a.push_back(c);
    return a;
}
