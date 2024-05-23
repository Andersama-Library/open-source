#pragma once

#include <map>
#include <set>
#include <multiset>
#include <multimap>
#include <vector>
#include <list>
#include <deque>
#include <priority_queue>
#include <valarray>
#include <string>

#if __cplusplus >= 201100L
#include <array>
#include <forward_list>
#include <unordered_map>
#include <unordered_set>
#include <unordered_multiset>
#include <unordered_multimap>
#endif

#if __cplusplus >= 201700L
#include <string_view>
#endif

#if __cplusplus >= 202000L
#include <span>
#endif

#if __cplusplus >= 202300L
#include <flat_set>
#include <flat_map>
#include <flat_multiset>
#include <flat_multimap>
#include <mdspan>
#endif