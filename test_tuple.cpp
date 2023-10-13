#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "tuple.hpp"

#include <array>
#include <sstream>
#include <string>
#include <vector>

TEST_CASE("Tuple simple test(same types)") {
  Tuple<int, int, int> t(1, 2, 3);
  SECTION("get<0>") {
    REQUIRE(get<0>(t) == 1);
  }
  SECTION("get<1>") {
    REQUIRE(get<1>(t) == 2);
  }
  SECTION("get<2>") {
    REQUIRE(get<2>(t) == 3);
  }
}

TEST_CASE("Tuple simple test(get<type>)") {
  Tuple<int, int, int, double> t(1, 2, 3, 4.5);
  SECTION("get<int>") {
    REQUIRE(get<int>(t) == 1);
  }
  SECTION("get<double>") {
    REQUIRE(get<double>(t) == 4.5);
  }
}

TEST_CASE("Tuple simple test(non basic types tuple) + move") {
  Tuple<int, std::string, int, double> t(1, "haha", 3, 4.5);
  SECTION("get<int> returns first int in tuple") {
    REQUIRE(get<int>(t) == 1);
  }
  SECTION("get<std::string>") {
    REQUIRE(get<std::string>(t) == "haha");
  }
  std::vector<int> v = {1, 2, 3, 4};
  Tuple<std::vector<int>, std::vector<int>, int> tv(v, v, 4);
  SECTION("Move works") {
    auto moved = std::move(tv);
    REQUIRE(get<0>(moved) == v);
    REQUIRE(get<1>(moved) == v);
    REQUIRE(get<0>(tv).empty());
    REQUIRE(get<1>(tv).empty());
  }
}

TEST_CASE("Make tuple test") {
  auto t = make_tuple(1, 2, 3, 4);
  SECTION("get<int> returns first int in tuple") {
    REQUIRE(get<int>(t) == 1);
  }
  SECTION("Get by indexes") {
    REQUIRE(get<0>(t) == 1);
    REQUIRE(get<1>(t) == 2);
    REQUIRE(get<2>(t) == 3);
    REQUIRE(get<3>(t) == 4);
  }
}

TEST_CASE("Comparsions and copy operator and constructor") {
  Tuple<int, std::string, std::string, std::string> t1(1, "kek", "lol",
                                                       "cheburek");
  auto t2 = t1 = t1;
  Tuple<int, std::string, std::string, std::string> t3(3, "kek", "lol",
                                                       "cheburek");
  SECTION("operator ==") {
    REQUIRE(t1 == t2);
  }
  SECTION("operator <=") {
    REQUIRE(t1 <= t3);
    REQUIRE(t1 <= t2);
  }
  SECTION("operator >=") {
    REQUIRE(t1 >= t2);
    REQUIRE(t3 >= t1);
  }
  SECTION("!=") {
    REQUIRE(t2 != t3);
  }
  SECTION("Copy test") {
    t2 = t3;
    REQUIRE(t2 == t3);
  }
  SECTION("Yury Gagarin test") {
    REQUIRE((t2 <=> t3) == std::strong_ordering::less);
    REQUIRE((t3 <=> t2) == std::strong_ordering::greater);
    t2 = t3;
    REQUIRE((t2 <=> t3) == std::strong_ordering::equivalent);
  }
}

TEST_CASE("Perfect forwarding test + std::swap + swap method") {
  std::vector<int> a{1, 2, 3};
  auto b = a;
  Tuple<int, std::vector<int>, int> t(1, std::move(a), 3);
  Tuple<int, std::vector<int>, int> t1(12, std::move(a), 31);
  REQUIRE(a.empty());
  REQUIRE(get<std::vector<int>>(t) == b);
  std::swap(t, t1);
  REQUIRE(get<std::vector<int>>(t1) == b);
  REQUIRE(get<std::vector<int>>(t) == a);
  t1.swap(t);
  REQUIRE(get<std::vector<int>>(t) == b);
  REQUIRE(get<std::vector<int>>(t1) == a);
}

TEST_CASE("Tuple cat") {
  Tuple<std::string, int> info1("hromosomes", 47);
  Tuple<std::string, int> info2("man", 228);
  Tuple<std::string, int> info3("deadinside", 1000 - 7);
  auto cat = tuple_cat(info1, info2, info3);
  REQUIRE(get<0>(cat) == "hromosomes");
  REQUIRE(get<1>(cat) == 47);
  REQUIRE(get<2>(cat) == "man");
  REQUIRE(get<3>(cat) == 228);
  REQUIRE(get<4>(cat) == "deadinside");
  REQUIRE(get<5>(cat) == 1000 - 7);
}

TEST_CASE("Lvalue test") {
  Tuple<std::string, int, int> ken("goul", 1000, 1000);
  while (get<1>(ken) > 0 && get<2>(ken) > 0) {
    get<1>(ken) -= 7;
    get<2>(ken) -= 7;
  }
  get<0>(ken) = "YA GOUL";
  REQUIRE(get<1>(ken) < 0);
  REQUIRE(get<2>(ken) < 0);
  REQUIRE(get<0>(ken) == "YA GOUL");
}

TEST_CASE("Empty tuples must be equal") {
  Tuple<> t1, t2;
  REQUIRE(t1 == t2);
}

TEST_CASE("One element tuples") {
  Tuple<int> t1(1), t2(2);
  auto kek = tuple_cat(t1, t1, t1, t1, t1, t2, t1, t2, t1, t2, t2, t2);
  REQUIRE(get<6>(kek) == 1);
}

TEST_CASE("Tuple size helper") {
  Tuple<int> t1(1), t2(2);
  auto kek = tuple_cat(t1, t1, t1, t1, t1, t2, t1, t2, t1, t2, t2, t2);

  SECTION("Just works") {
    REQUIRE(tuple_size_v<decltype(kek)> == 12);
  }

  SECTION("Really compile-time") {
    std::array<int, tuple_size_v<decltype(kek)>> x;
    REQUIRE(x.size() == 12);
  }
}

TEST_CASE("Structure binding support") {
  Tuple<int, double, std::string> t1(1, 2.0, "keke");
  auto [x, y, z] = t1;
  REQUIRE(x == 1);
  REQUIRE(y == 2.0);
  REQUIRE(z == "keke");
  auto& [x1, y1, z1] = t1;
  x1 = -10;
  REQUIRE(get<0>(t1) == -10);
}

TEST_CASE("Is default constructible + Stream operators") {
  Tuple<int, double, std::string, std::string, std::string, std::string> t1;
  std::istringstream is("1 1.5 Brarishka Ya Prines Pokushat");
  SECTION("Input stream") {
    is >> t1;
    REQUIRE(get<int>(t1) == 1);
    REQUIRE(get<double>(t1) == 1.5);
    REQUIRE(get<2>(t1) + get<3>(t1) + get<4>(t1) + get<5>(t1) ==
            "BrarishkaYaPrinesPokushat");
  }
  std::ostringstream out;
  Tuple<int, double> t2(1, 1.5);
  SECTION("Output stream(no extra spaces in the end)") {
    out << t2;
    REQUIRE(out.str() == "1 1.5");
  }
}
