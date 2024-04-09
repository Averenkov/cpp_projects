#include <iostream>
#include <cstring>
#include <stdexcept> 
#include <iterator>
#include <compare>

template<typename T>
class Deque {
public:
	template<typename V>
	struct DequeIterator;
	using value_type = T;
private:
	static const size_t buc_2 = 5;
	static const size_t bucket_size = (1ULL << buc_2);
	size_t sz = 0;
	size_t bucket_cnt = 0;
	DequeIterator<T> d_begin;
	DequeIterator<T> d_end;
	T** glob_arr = nullptr;
	void memory_clear(T** arr, size_t ind) {
		for (size_t j = 0; j < ind; ++j) {
			delete[] reinterpret_cast<int8_t*>(arr[j]);
		}
		delete[] reinterpret_cast<int8_t*>(arr);
	}
	void memory_clear_arr(T*const* begin, T*const* end) {
		for (auto j = begin; j != end; ++j) {
			delete[] reinterpret_cast<int8_t*>(*j);
		}
	}
	// NOLINTNEXTLINE(readability-function-size)
	void reserve() {
		bool null_sz = (sz == 0);
		size_t true_bucket_cnt = null_sz ? static_cast<size_t>(1) : static_cast<size_t>(d_end.buc - d_begin.buc + 1);
		// NOLINTNEXTLINE(readability-magic-numbers)
		size_t new_bucket_cnt = std::max(true_bucket_cnt, static_cast<size_t>(1)) * 3 + 2;
		T** new_global_arr = nullptr;
		new_global_arr = reinterpret_cast<T**>(new int8_t[new_bucket_cnt * sizeof(T*)]);
		// NOLINTNEXTLINE(readability-identifier-length)
		size_t i = 0;
		try {
			for (; i < true_bucket_cnt + 1; ++i) {
				new_global_arr[i] = reinterpret_cast<T*>(new int8_t[bucket_size * sizeof(T)]);
			}
		} catch(...) {
			memory_clear(new_global_arr, i);
			throw;
		}
		T*const* it = d_begin.buc;
		T** true_begin_y = nullptr;
		T** true_end_y = nullptr;
		for (size_t j = true_bucket_cnt + 1; j < 1 + true_bucket_cnt * 2; ++j) {
			new_global_arr[j] = *it;
			if (it == d_begin.buc) {
				true_begin_y = new_global_arr + j;
			}
			if (it == d_end.buc) {
				true_end_y = new_global_arr + j;
			}
			++it;
		}
		i = true_bucket_cnt * 2 + 1;
		try {
			for (; i < new_bucket_cnt; ++i) {
				new_global_arr[i] = reinterpret_cast<T*>(new int8_t[bucket_size * sizeof(T)]);
			}
		} catch(...) {
			memory_clear_arr(new_global_arr, new_global_arr + true_bucket_cnt);
			memory_clear_arr(new_global_arr + true_bucket_cnt * 2, new_global_arr + i);
			delete[] reinterpret_cast<int8_t*>(new_global_arr);
			throw;
		}
		memory_clear_arr(glob_arr, d_begin.buc);
		memory_clear_arr(d_end.buc + 1, glob_arr + (bucket_cnt - 1) + 1);
		delete[] reinterpret_cast<int8_t*>(glob_arr);
		glob_arr = new_global_arr;
		bucket_cnt = new_bucket_cnt;
		d_begin.buc = true_begin_y;
		if (!null_sz) {
			d_end.buc = true_end_y;
		} else {
			d_end = (d_begin - 1);
		}
	}
	void init_bucket(size_t cnt) {
    	bucket_cnt = cnt;
    	try {
			glob_arr = reinterpret_cast<T**>(new int8_t[bucket_cnt * sizeof(T*)]);
		} catch(...) {
			delete[] reinterpret_cast<int8_t*>(glob_arr);
			throw;
		}
		// NOLINTNEXTLINE(readability-identifier-length)
		size_t i = 0;
		try {
			for (; i < bucket_cnt; ++i) {
				glob_arr[i] = reinterpret_cast<T*>(new int8_t[bucket_size * sizeof(T)]);
			}
		} catch(...) {
			for (size_t j = 0; j < i; ++j) {
				delete[] reinterpret_cast<int8_t*>(glob_arr[j]);
			}
			delete[] reinterpret_cast<int8_t*>(glob_arr);
			sz = 0;
			bucket_cnt = 0;
			throw;
		}
    }
public:
	template<typename V>
	struct DequeIterator {
	friend class Deque;
	public:
		using difference_type = std::ptrdiff_t;
		using value_type = V;
		using pointer = V*;
		using chunk = V*const*;
		using reference = value_type&;
		using iterator_category = std::random_access_iterator_tag;
	private:
		pointer ptr_elem = nullptr;
		chunk buc = nullptr;
		pointer begin_b = nullptr;
		pointer end_b = nullptr;
	public:
		DequeIterator() = default;
		explicit DequeIterator(pointer ptr, chunk ch): ptr_elem(ptr), buc(ch), begin_b(*buc), end_b(begin_b + (bucket_size - 1)) {}
		DequeIterator(const DequeIterator<typename std::remove_const<T>::type>& it): ptr_elem(it.ptr_elem), buc(it.buc), begin_b(it.begin_b), end_b(it.end_b) {};
		DequeIterator& operator=(const DequeIterator<typename std::remove_const<T>::type>& it) {
			ptr_elem = it.ptr_elem;
			buc = it.buc;
			begin_b = it.begin_b;
			end_b = it.end_b;
			return *this;
		}
		reference operator*() const {
			return *ptr_elem;
		}
		pointer operator->() const {
			return ptr_elem;
		}
		DequeIterator& operator+=(difference_type value) {
			difference_type offset = (ptr_elem - begin_b) + value;
			difference_type blocks = std::abs(offset) / static_cast<difference_type>(bucket_size);
			if (offset < 0) {
				blocks *= -1;
				if (std::abs(offset) % static_cast<difference_type>(bucket_size) != 0) {
					blocks -= 1;
				}
			}
			buc += blocks;
			begin_b = *buc;
			ptr_elem = begin_b + (offset - blocks * static_cast<difference_type>(bucket_size));
			end_b = begin_b + bucket_size;
			return *this;
		}
		DequeIterator& operator-=(difference_type value) { return operator+=(-value); }
		DequeIterator& operator++() { return *this += 1; }
		DequeIterator& operator--() { return *this -= 1; }
		DequeIterator operator++(int) {
			DequeIterator copy = *this;
			*this += 1;
			return copy;
		}
		DequeIterator operator--(int) {
			DequeIterator copy = *this;
			*this -= 1;
			return copy;
		}
		difference_type operator-(const DequeIterator& other) const {
			if (other < *this) {
				return (other.end_b - other.ptr_elem) + (ptr_elem - begin_b) + 1 + (buc - other.buc - 1) * static_cast<difference_type>(bucket_size);
			}
			return -((other.ptr_elem == ptr_elem) ? 0 : (other.ptr_elem - other.begin_b) + (end_b - ptr_elem) + (other.buc != buc) + (other.buc - buc - 1) * static_cast<difference_type>(bucket_size));
		}
		DequeIterator operator+(difference_type diff) const {
			DequeIterator copy = *this;
			copy += diff;
			return copy;
		}
		DequeIterator operator-(difference_type diff) const {
			DequeIterator copy = *this;
			copy -= diff;
			return copy;
		}
		auto operator<=>(const DequeIterator& other) const {
			if (buc != other.buc) {
				return (buc <=> other.buc);
			}
			return ptr_elem <=> other.ptr_elem;
		}
		bool operator==(const DequeIterator& other) const {
			return (ptr_elem == other.ptr_elem);
		}
		bool operator!=(const DequeIterator& other) const {
			return !(*this == other);
		}
	};
    using iterator = DequeIterator<T>;
    using const_iterator = DequeIterator<const T>;
    iterator begin() {
        return d_begin;   
    }
    iterator end() {
    	DequeIterator copy = d_end;
    	++copy;
        return copy;   
    }

    const_iterator begin() const {
        return cbegin(); 
    }
    const_iterator end() const {
    	return cend();  
    }

    const_iterator cbegin() const {
        return const_iterator(d_begin.ptr_elem, static_cast<const T* const*>(d_begin.buc));   
    }

    const_iterator cend() const {
    	DequeIterator copy = d_end;
    	++copy;
        return const_iterator(copy.ptr_elem, static_cast<const T* const*>(copy.buc));
    }
    auto rbegin() {
        return std::reverse_iterator<iterator>(end());   
    }

    auto rend() {
        return std::reverse_iterator<iterator>(begin());   
    }

    auto rbegin() const {
        return std::reverse_iterator<const_iterator>(cend());   
    }

    auto rend() const {
        return std::reverse_iterator<const_iterator>(cbegin());   
    }
	Deque(size_t sz): Deque(sz, T()) {};
	Deque(): sz(0) {
		// NOLINTNEXTLINE(readability-magic-numbers)
		init_bucket(3);
		d_begin = DequeIterator<T>(glob_arr[1], glob_arr + 1);
		// NOLINTNEXTLINE(cppcoreguidelines-prefer-member-initializer)
		d_end = d_begin - 1;
	}
	Deque(const Deque<T>& deq): Deque() {
		try {
			for (auto elem : deq) {
				push_back(elem);
			}
		} catch(...) {
			while(size() != 0) {
				pop_back();
			}
			throw;
		}
	}
	Deque& operator=(const Deque<T>& deq) {
		if (this == &deq) {
			return *this;
		}
		size_t old_sz = size();
		try {
			for (auto elem : deq) {
				push_back(elem);
			}
		} catch(...) {
			while (size() != old_sz) {
				pop_back();
			}
			throw;
		}
		while (size() != deq.size()) {
			pop_front();
		}
		return *this;
	}
	// NOLINTNEXTLINE(readability-identifier-length)
	Deque(size_t sz, const T& x): sz(sz), bucket_cnt((sz + bucket_size - 1) / bucket_size + 2) {
		init_bucket(bucket_cnt);
		// NOLINTNEXTLINE(readability-identifier-length)
		size_t i = 0;
		for (; i < bucket_cnt; ++i) {
			// NOLINTNEXTLINE(readability-identifier-length)
			size_t j = 0;
			try {
				for (; j < bucket_size; ++j) {
					new (glob_arr[i] + j) T(x);
				}
			} catch(...) {
				for (size_t k = 0; k < j; ++k) {
					(glob_arr[i] + k)->~T();
				}
				for (size_t j = 0; j < i; ++j) {
					for (size_t k = 0; k < bucket_size; ++k) {
						(glob_arr[i] + k)->~T();
					}
				}
				for (size_t j = 0; j < bucket_cnt; ++j) {
					delete[] reinterpret_cast<int8_t*>(glob_arr[j]);
				}
				delete[] reinterpret_cast<int8_t*>(glob_arr);
				sz = 0;
				bucket_cnt = 0;
				throw;
			}
		}
		d_begin = DequeIterator<T>(glob_arr[1], glob_arr + 1);
		d_end = DequeIterator<T>(glob_arr[1 + ((sz - 1) >> buc_2)] + ((sz - 1) % bucket_size), 1 + glob_arr + ((sz - 1) >> buc_2));
	}
	size_t size() const {
		return sz;
	}
	const T& operator[](size_t ind) const {
		return *(d_begin + static_cast<int>(ind));
	}
	T& operator[](size_t ind) {
		return *(d_begin + static_cast<int>(ind));
	}
	const T& at(size_t ind) const {
		if (ind >= sz) {
			throw std::out_of_range("out of range");
		}
		return *(d_begin + static_cast<int>(ind));
	}
	T& at(size_t ind) {
		if (ind >= sz) {
			throw std::out_of_range("out of range");
		}
		return *(d_begin + static_cast<int>(ind));
	}
	void push_back(const T& elem) {
		if (d_end.buc == glob_arr + (bucket_cnt - 2) && d_end.ptr_elem == glob_arr[bucket_cnt - 2] + (bucket_size - 1)) {
			reserve();
		}
		try {
			++d_end;
			*d_end.ptr_elem = elem;
		} catch(...) {
			--d_end;
			throw;
		}
		++sz;
	}
	void push_front(const T& elem) {
		bool fl = false;
		if (d_begin.buc == glob_arr + 1 && d_begin.ptr_elem == glob_arr[1]) {
			fl = true;
		}
		try {
			--d_begin;
			*d_begin.ptr_elem = elem;
		} catch(...) {
			++d_begin;
			throw;
		}
		++sz;
		if (fl) {
			try {
				reserve();
			} catch(...) {
				++d_begin;
				--sz;
				throw;
			}
		}
	}
	void pop_back() {
		--d_end;
		--sz;
	}
	void pop_front() {
		++d_begin;
		--sz;
	}
	iterator insert(iterator itr, const T& elem) {
		if (itr == end()) {
			push_back(elem);
			return d_end;
		}
		T copy = elem;
		iterator copy_itr = itr;
		iterator ans = itr;
		while (itr != end()) {
			std::swap(*itr, copy);
			++itr;
		}
		try {
			push_back(copy);
		} catch(...) {
			itr = d_end;
			while (copy_itr != itr) {
				std::swap(*itr, copy);
				--itr;
			}
			std::swap(*itr, copy);
			throw;
		}
		return ans;
	}
	void erase(iterator itr) {
		while (itr != end() - 1) {
			std::swap(*itr, (*(itr + 1)));
			++itr;
		}
		pop_back();
	}
	~Deque() {
		for (size_t i = 0; i < bucket_cnt; ++i) {
			delete[] reinterpret_cast<int8_t*>(glob_arr[i]);
		}
		delete[] reinterpret_cast<int8_t*>(glob_arr);
	}
};