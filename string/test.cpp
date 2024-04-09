#include <type_traits>
#include <algorithm>
#include <sstream>
#include "string.h"
#include "TestLib.hpp"

using testing::make_pretty_test;
using testing::TestGroup;


TestGroup all_tests[] = {
    TestGroup("static tests",
        make_pretty_test("no cheating", [](auto& test) {
            test.check(!std::is_base_of_v<std::string, String>);
        })
    ),

    TestGroup("push/pop & other", 
        make_pretty_test("push back and length", [](auto& test) {
            String str;
            for (size_t i = 0; i < 1000; ++i) {
                test.check(i == str.length());
                str.push_back('a');
            }
        }),

        make_pretty_test("correctness + pop_back", [](auto& test){
            String str;
            std::string std_str;
            for (size_t i = 0; i < 2000; ++i) {
                char c = '0';
                str.push_back(c);
                std_str.push_back(c);

                if (i % 12 == 10) {
                    str.pop_back();
                    std_str.pop_back();
                }
                test.check(str.length() == std_str.length());
            }

            for (size_t i = 0; i < str.length(); ++i) {
                test.check(str[i] == std_str[i]);
            }
        }),

        make_pretty_test("back & front", [](auto& test){
            const size_t len = 100;
            String str(len, 'q');
            char& first = str.front();
            char& last = str.back();
            first = 'a';
            last = 'b';
            test.check((str[0] == 'a' && str[len - 1] == 'b' && (&last - &first == len - 1)));
        }),

        make_pretty_test("empty&clear", [](auto& test){
            String str;
            test.check(str.empty());
            str = String(12, 's');
            test.check(!str.empty());
            str.clear();
            test.check(str.empty());
            str.clear();
        })
    ),

    TestGroup("input/output", 
        make_pretty_test("push back and length", [](auto& test) {
            String str;
            for (size_t i = 0; i < 1000; ++i) {
                test.check(i == str.length());
                str.push_back('a');
            }
        }),

        make_pretty_test("read from stream", [](auto& test) {
            std::string ref = "c++";
            String str = ref.c_str();
            std::stringstream stream(ref);
            
            String str2;
            stream >> str2;
            
            test.check(str2 == str);
        }),

        make_pretty_test("write to stream", [](auto& test) {
            std::string ref = "c++";
            String str = ref.c_str();
            std::stringstream stream;
            
            stream << str;

            std::string res = stream.str();
            String str2 = res.c_str();
            test.check( str2 == str );
        }),

        make_pretty_test("several reads", [](auto& test) {
            std::string ref = "c++ is the best";
            std::stringstream stream(ref);

            String str;
            String str2;
            String str3;

            stream >> str >> str2 >> str3;
            test.check(str == "c++");
            test.check(str2 == "is");
            test.check(str3 == "the");
        }),

        make_pretty_test("several writes", [](auto& test) {
            String lang = "c++";
            String middle = " is the";
            String status = "best";

            std::stringstream stream{};
            stream << lang << middle << ' ' << status << "!";
            std::string res = stream.str();
            test.check( res == "c++ is the best!" );
        })
    ),

    TestGroup("constructors", 
        make_pretty_test("c-style", [](auto& test) {
            auto c_str = "this is c-style";
            std::string ref = c_str;

            String str(c_str);
            test.check ( str.length() == ref.length() );

            for (size_t i = 0; i < ref.length(); ++i) {
                test.check( ref[i] == str[i] );
            }
        }),

        make_pretty_test("n chars", [](auto& test) {
            char c = '!';
            String str(123, c);
            test.check (str.length() == 123);

            for (size_t i = 0; i < str.length(); ++ i) {
                test.check (str[i] == c);
            }
        }),

        make_pretty_test("other", [](auto& test) {
            String empty;
            test.check (empty.length() == 0);

            String not_empty(11, 'a');
            String copy = not_empty;
            copy[5] = 'b';
            test.check ( copy != not_empty );

            empty = copy;
            test.check( copy == empty );
        })
    ),

    TestGroup("operators", 

        make_pretty_test("equality", [](auto& test) {
            std::string ref = "c++ is the best";
            String str = ref.c_str();
            String str2 = str;
            String str3 = ref.c_str();

            test.check(str == str2);
            test.check(str2 == str3);
            test.check(str3 == str);
            str2[3] = 'm';
            test.check(str != str2);
            test.check(str2 != str3);
            test.check(str3 == str);
        }),

        make_pretty_test("+=", [](auto& test) {
            String str = "c++ ";
            str += "is ";
            String str2 = "the";
            str += str2;
            str += ' ';
            str += "best!";
            str += str;
            test.check( str == "c++ is the best!c++ is the best!" );
        }),

        make_pretty_test("+", [](auto& test) {
            String str = "++ ";
            String str2 = "the best";
            String str3 = str + str2;
            str3 = str3 + '!';
            String str4 = 'c' + str3;
            test.check( str4 == "c++ the best!" );
        })
    ),

    TestGroup("find & substr", 
        make_pretty_test("find", [](auto& test) {
            String str = "c++ is the best!";
            test.check( str.find("c++") == 0 );
            test.check( str.find("is") == 4 );
            test.check( str.find("!") == 15 );
            // second condition is for std::string
            test.check ((str.find("java") == 16 || str.find("java") == std::string::npos));
        }),

        make_pretty_test("find vs rfind", [](auto& test) {
            String str = "abacaba";
            test.check (str.find("c") == str.rfind("c"));
            test.check (str.find("ba") == 1);
            test.check (str.rfind("ba") == 5);
            test.check (str.find("qqqq") == str.rfind("qqqq"));
        }),

        make_pretty_test("substr", [](auto& test) {
            String str = "c++ is the best!";
            String cpp = str.substr(0, 3);
            String exclamation = str.substr(15, 1);

            test.check (cpp == "c++");
            test.check (exclamation == "!");
            test.check ("is" == str.substr(str.find("is"), 2));
        }),

        make_pretty_test("bigger find", [](auto& test) {
            String str = "ab";
            String subs = "abacaba";
            test.check( (str.find(subs) == str.length() || str.find(subs) == std::string::npos) );
        })
    )
};


int main() {
    bool success = true;
    for (auto& group : all_tests) {
        success &= group.run();
    }
    return success ? 0 : 1;
}

