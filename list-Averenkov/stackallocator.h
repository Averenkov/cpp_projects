#include <memory>
#include <type_traits>
#include <iterator>


template<typename T, typename Allocator = std::allocator<T>>
class List {
private:
    struct Node {
        Node* left = nullptr;
        Node* right = nullptr;
    };
    struct TrueNode: Node {
        T x;
        TrueNode(Node* left, Node* right, const T& val): x(val) {
            Node::left = left;
            Node::right = right;
        }
        TrueNode(Node* left, Node* right): x() {
            Node::left = left;
            Node::right = right;
        }
    };
    using NodeAlloc = typename std::allocator_traits<Allocator>::template rebind_alloc<TrueNode>;
    [[no_unique_address]] NodeAlloc alloc;
    using AllocTraits = typename std::allocator_traits<Allocator>::template rebind_traits<TrueNode>;
    Node fake_node;
    size_t sz;
    template <typename... Args>
    void emplace(Args&&... args) {
        TrueNode* ver = nullptr;
        ver = AllocTraits::allocate(alloc, 1);    
        try {
            AllocTraits::construct(alloc, ver, std::forward<Args>(args)...);
        } catch(...) {
            AllocTraits::deallocate(alloc, ver, 1);
            throw;
        }
        ver->left->right = static_cast<Node*>(ver);
        ver->right->left = static_cast<Node*>(ver);
        ++sz;
    }
public:
    List() : alloc(), sz(0) {
        fake_node.left = fake_node.right = &fake_node;
    }
    List(const Allocator& alloc): alloc(alloc), sz(0) {
        fake_node.left = fake_node.right = &fake_node;
    }
    List(size_t n, const Allocator& other_alloc = Allocator()): alloc(AllocTraits::select_on_container_copy_construction(other_alloc)), sz(0) {
        fake_node.left = fake_node.right = &fake_node;
        try {
            for (size_t i = 0; i < n; ++i) {
                emplace(fake_node.left, &fake_node);
            }
        } catch(...) {
            clear();
            throw;
        }
    }
    List(size_t n, const T& val, const Allocator& other_alloc = Allocator()): alloc(AllocTraits::select_on_container_copy_construction(other_alloc)), sz(0) {
        fake_node.left = fake_node.right = &fake_node;
        try {
            for (size_t i = 0; i < n; ++i) {
                emplace(fake_node.left, &fake_node, val);
            }
        } catch(...) {
            clear();
            throw;
        }
    }
    size_t size() const {
        return sz;
    }
    List(const List& other): alloc(AllocTraits::select_on_container_copy_construction(other.alloc)), sz(0) {
        fake_node.left = fake_node.right = &fake_node;
        try {
            for (auto i = other.begin(); i != other.end(); ++i) {
                push_back(*i);
            }
        }  catch(...) {
            clear();
            throw;
        }
    }
    List<T, Allocator>& operator=(const List<T, Allocator>& other) {
        if (&other == this) {
            return *this;
        }
        size_t old_sz = sz;
        size_t cnt = 0;
        auto copy_alloc = alloc;
        alloc = other.alloc;
        try {
            for (auto i = other.begin(); i != other.end(); ++i) {
                push_back(*i);
                ++cnt;
            }
        }  catch(...) {
            for (size_t j = 0; j < cnt; ++j) {
                pop_back();
            }
            alloc = copy_alloc;
            throw;
        }
        std::swap(alloc, copy_alloc);
        while (old_sz--) {
            pop_front();
        }
        std::swap(alloc, copy_alloc);
        return *this;
    }
    NodeAlloc& get_allocator() {
        return alloc;
    }
    template<typename V>
    struct BaseIterator {
    friend class List;
    public:
        using difference_type = std::ptrdiff_t;
        using value_type = V;
        using pointer = V*;
        using reference = value_type&;
        using iterator_category = std::bidirectional_iterator_tag;
    private:
        Node* ptr_elem = nullptr;
    public:
        BaseIterator() = default;
        BaseIterator(Node* ptr): ptr_elem(ptr) {}
        BaseIterator(const BaseIterator<typename std::remove_const<T>::type>& it): ptr_elem(it.ptr_elem) {};
        reference operator=(BaseIterator<typename std::remove_const<T>::type> it) {
            ptr_elem = it.ptr_elem;
            return static_cast<TrueNode*>(ptr_elem)->x;
        }
        reference operator*() const {
            return static_cast<TrueNode*>(ptr_elem)->x;
        }
        pointer operator->() const {
            return &(static_cast<TrueNode*>(ptr_elem)->x);
        }
        BaseIterator& operator++() {
            ptr_elem = ptr_elem->right;
            return *this;
        }
        BaseIterator& operator--() {
            ptr_elem = ptr_elem->left;
            return *this;
        }
        BaseIterator operator++(int) {
            BaseIterator copy = *this;
            ++*this;
            return copy;
        }
        BaseIterator operator--(int) {
            BaseIterator copy = *this;
            --*this;
            return copy;
        }
        bool operator!=(const BaseIterator& other) const = default;
        bool operator==(const BaseIterator& other) const = default;
    };
    using iterator = BaseIterator<T>;
    using const_iterator = BaseIterator<const T>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    iterator begin() {
        return iterator(fake_node.right);   
    }
    iterator end() {
        return iterator(const_cast<Node*>(&fake_node));   
    }

    const_iterator begin() const {
        return cbegin(); 
    }
    const_iterator end() const {
        return cend();  
    }

    const_iterator cbegin() const {
        return const_iterator(fake_node.right);   
    }

    const_iterator cend() const {
        return const_iterator(const_cast<Node*>(&fake_node));
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
    void push_back(const T& elem) {
        insert(cend(), elem);
    }
    void push_front(const T& elem) {
        insert(cbegin(), elem);
    }
    void pop_back() {
        erase(--cend());
    }
    void pop_front() {
        erase(cbegin());
    }
    iterator insert(const_iterator itr, const T& elem) {
        TrueNode* ver = nullptr;
        ver = AllocTraits::allocate(alloc, 1);    
        try {
            AllocTraits::construct(alloc, ver, itr.ptr_elem->left, itr.ptr_elem, elem);
        } catch(...) {
            AllocTraits::deallocate(alloc, ver, 1);
            throw;
        }
        itr.ptr_elem->left->right = static_cast<Node*>(ver);
        itr.ptr_elem->left = static_cast<Node*>(ver);
        ++sz;
        return iterator(itr.ptr_elem->left);
    }
    void erase(const_iterator itr) {
        Node* lv = itr.ptr_elem->left;
        Node* rv = itr.ptr_elem->right;
        lv->right = rv;
        rv->left = lv;
        AllocTraits::destroy(alloc, static_cast<TrueNode*>(itr.ptr_elem));
        AllocTraits::deallocate(alloc, static_cast<TrueNode*>(itr.ptr_elem), 1);
        --sz;
    }
    void clear() {
        while (size()) {
            pop_back();
        }
    }
    ~List() {
        clear();
    }
};

template<std::size_t N>
struct StackStorage {
    char m[N] = {};
    size_t pos = 0;
};

template<typename T, std::size_t N>
struct StackAllocator {
    StackStorage<N>& s_alloc;
    using value_type = T;
    StackAllocator(StackStorage<N>& other_alloc): s_alloc(other_alloc) {}

    template<typename U>
    StackAllocator(const StackAllocator<U, N>& other_alloc): s_alloc(other_alloc.s_alloc) {}

    T* allocate(size_t n) {
        if (s_alloc.pos + n * sizeof(T) > N) {
            throw std::bad_alloc();
        }
        while ((s_alloc.pos) % sizeof(T) != 0) {
            ++s_alloc.pos;
        }
        T* ans = reinterpret_cast<T*>(s_alloc.m + s_alloc.pos);
        s_alloc.pos += n * sizeof(T);
        return ans;
    }

    void deallocate(T* pointer, size_t n) {
        std::ignore = pointer;
        std::ignore = n;
        //return std::allocator<T>().deallocate(pointer, n);
    }

    bool operator==(const StackAllocator& other) const = default;

    template<typename U>
    //NOLINTNEXTLINE
    struct rebind {
        using other = StackAllocator<U, N>;
    };
};

