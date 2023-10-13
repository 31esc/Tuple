#pragma once

template <typename... T>
class Tuple;

template <typename Head, typename... Tail>
class Tuple<Head, Tail...> : public Tuple<Tail...> {
 private:
  Head value_;

 public:
  Tuple() = default;

  Tuple(Head head, Tail... tail) : Tuple<Tail...>(tail...), value_(head) {}

  using first_type = Head;
  using tail_type = Tuple<Tail...>;

  template <size_t n, typename RETURN_TYPE>
  constexpr RETURN_TYPE& get_answer() {
    if constexpr (n == 0) {
      return value_;
    } else {
      return tail_type::template get_answer<n - 1, RETURN_TYPE>();
    }
  }

  template <size_t n, typename RETURN_TYPE>
  constexpr const RETURN_TYPE& get_answer() const {
    if constexpr (n == 0) {
      return value_;
    } else {
      return tail_type::template get_answer<n - 1, RETURN_TYPE>();
    }
  }

  void swap(Tuple& other) noexcept { std::swap(other, *this); }
};

template <>
class Tuple<> {};

template <typename... Args>
struct std::tuple_size<Tuple<Args...>>
    : std::integral_constant<size_t, sizeof...(Args)> {};

template <typename... Args>
struct std::tuple_size<Tuple<Args...>&>
    : std::integral_constant<size_t, sizeof...(Args)> {};

template <typename... Args>
struct std::tuple_size<const Tuple<Args...>>
    : std::integral_constant<size_t, sizeof...(Args)> {};

template <typename... Args>
struct std::tuple_size<const Tuple<Args...>&>
    : std::integral_constant<size_t, sizeof...(Args)> {};

template <typename T>
inline constexpr size_t tuple_size_v = std::tuple_size<T>::value;

template <size_t n, typename TUPLE>
struct get_n_type {
  using type = typename get_n_type<n - 1, typename TUPLE::tail_type>::type;
};

template <typename TUPLE>
struct get_n_type<0, TUPLE> {
  using type = typename TUPLE::first_type;
};

template <size_t n, typename... T>
struct std::tuple_element<n, Tuple<T...>> : get_n_type<n, Tuple<T...>> {};

template <size_t n, typename... T>
struct std::tuple_element<n, Tuple<T...>&> : get_n_type<n, Tuple<T...>> {};

template <size_t n, typename... T>
struct std::tuple_element<n, const Tuple<T...>> : get_n_type<n, Tuple<T...>> {};

template <size_t n, typename... T>
struct std::tuple_element<n, const Tuple<T...>&> : get_n_type<n, Tuple<T...>> {
};

template <size_t n, typename... T>
using tuple_element_t = typename std::tuple_element<n, T...>::type;

template <size_t n>
constexpr decltype(auto) get_support(auto&& t) {
  using type = std::remove_reference_t<tuple_element_t<n, decltype(t)>>;
  return t.template get_answer<n, type>();
}

template <typename T, size_t n = 0>
constexpr decltype(auto) get_support(auto&& t) {
  using type = std::remove_reference_t<tuple_element_t<n, decltype(t)>>;
  if constexpr (std::is_same_v<type, std::remove_reference_t<T>>) {
    return t.template get_answer<n, type>();
  } else {
    return get_support<T, n + 1>(t);
  }
}

template <size_t T, typename... Types>
constexpr auto& get(Tuple<Types...>& t) noexcept {
  return get_support<T>(t);
}

template <size_t T, typename... Types>
constexpr auto&& get(Tuple<Types...>&& t) noexcept {
  return get_support<T>(t);
}

template <size_t T, typename... Types>
constexpr const auto& get(const Tuple<Types...>& t) noexcept {
  return get_support<T>(t);
}

template <size_t T, typename... Types>
constexpr const auto&& get(const Tuple<Types...>&& t) noexcept {
  return get_support<T>(t);
}

template <typename T, typename... Types>
constexpr auto& get(Tuple<Types...>& t) noexcept {
  return get_support<T>(t);
}

template <typename T, typename... Types>
constexpr auto&& get(Tuple<Types...>&& t) noexcept {
  return get_support<T>(t);
}

template <typename T, typename... Types>
constexpr const auto& get(const Tuple<Types...>& t) noexcept {
  return get_support<T>(t);
}

template <typename T, typename... Types>
constexpr const auto&& get(const Tuple<Types...>&& t) noexcept {
  return get_support<T>(t);
}

template <typename... Args>
Tuple<Args...> make_tuple(Args&&... args) {
  return Tuple<Args...>(args...);
}

template <size_t n = 0, typename... T1, typename... T2>
std::strong_ordering operator<=>(const Tuple<T1...>& t1,
                                 const Tuple<T2...>& t2) {
  if constexpr (n >= std::min(std::tuple_size<decltype(t1)>::value,
                              std::tuple_size<decltype(t2)>::value)) {
    return std::tuple_size<decltype(t1)>::value <=>
           std::tuple_size<decltype(t2)>::value;
  } else {
    if (get<n>(t1) != get<n>(t2)) {
      return get<n>(t1) <=> get<n>(t2);
    } else {
      return operator<=><n + 1>(t1, t2);
    }
  }
}

template <typename... T1, typename... T2>
auto operator==(const Tuple<T1...>& t1, const Tuple<T2...>& t2) {
  return (t1 <=> t2) == 0;
}

template <typename T, T n>
struct simple_integral_constant {
  static constexpr T value = n;
  using value_type = T;
};

template <size_t n, typename... T>
auto add_elem(Tuple<T...>) {
  return Tuple<T..., simple_integral_constant<size_t, n>>();
}

template <size_t n>
struct struct_for_get_n_elem {
  using type =
      decltype(add_elem<n - 1>(typename struct_for_get_n_elem<n - 1>::type()));
};

template <>
struct struct_for_get_n_elem<0> {
  using type = Tuple<>;
};

template <typename... T1, typename... T2, typename... T3, typename... T4>
constexpr auto tuple_cat_for_two(const Tuple<T1...>& t1, const Tuple<T2...>& t2,
                                 Tuple<T3...>, Tuple<T4...>) {
  return Tuple<T1..., T2...>(get<T3::value>(t1)..., get<T4::value>(t2)...);
}

template <typename... T1, typename... T2>
constexpr auto operator+(const Tuple<T1...>& t1, const Tuple<T2...>& t2) {
  return tuple_cat_for_two(
      t1, t2, typename struct_for_get_n_elem<sizeof...(T1)>::type(),
      typename struct_for_get_n_elem<sizeof...(T2)>::type());
}

constexpr auto tuple_cat(auto&&... t) {
  return (t + ... + Tuple<>());
}

template <size_t n = 0, typename... Args>
auto& operator>>(std::istream& in, Tuple<Args...>& t) {
  if constexpr (n < std::tuple_size_v<decltype(t)>) {
    in >> get<n>(t);
    operator>><n + 1>(in, t);
  }
  return in;
}

template <size_t n = 0, typename... Args>
auto& operator<<(std::ostream& out, Tuple<Args...>& t) {
  if constexpr (n < std::tuple_size_v<decltype(t)>) {
    out << get<n>(t);
    if constexpr (n + 1 < std::tuple_size_v<decltype(t)>) {
      out << ' ';
    }
    operator<< <n + 1>(out, t);
  }
  return out;
}
