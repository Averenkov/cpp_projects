#include <algorithm>
#include <functional>
#include <iostream>

namespace my_list {
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
        List(List&& other): fake_node(other.fake_node) {
            if (other.size() == 0) {
                fake_node.left = fake_node.right = &fake_node;
            } else {
                fake_node.left->right = &fake_node;
                fake_node.right->left = &fake_node;
            }
            other.fake_node.left = other.fake_node.right = &other.fake_node;
            sz = other.sz;
            other.sz = 0;
            if (std::allocator_traits<Allocator>::propagate_on_container_copy_assignment::value) {
                alloc = std::move(other.alloc);
            }
        }
        void swap(List&& other) {
            fake_node = other.fake_node;
            if (other.size() == 0) {
                fake_node.left = fake_node.right = &fake_node;
            } else {
                fake_node.left->right = &fake_node;
                fake_node.right->left = &fake_node;
            }
            other.fake_node.left = other.fake_node.right = &other.fake_node;
            sz = other.sz;
            other.sz = 0;
            if (std::allocator_traits<Allocator>::propagate_on_container_copy_assignment::value) {
                alloc = std::move(other.alloc);
            }
        }
        List& operator=(List&& other) {
            if (this == &other) {
                return *this;
            }
            swap(std::move(other));
            return *this;
        }
        NodeAlloc& get_allocator() {
            return alloc;
        }
        template<typename V>
        struct ListBasicIterator {
        friend class List;
            using difference_type = std::ptrdiff_t;
            using value_type = V;
            using pointer = V*;
            using reference = value_type&;
            using iterator_category = std::bidirectional_iterator_tag;
            Node* ptr_val = nullptr;
            ListBasicIterator() = default;
            ListBasicIterator(Node* ptr): ptr_val(ptr) {}
            ListBasicIterator(const ListBasicIterator<typename std::remove_const<T>::type>& it): ptr_val(it.ptr_val) {};
            ListBasicIterator& operator=(ListBasicIterator<typename std::remove_const<T>::type> it) {
                ptr_val = it.ptr_val;
                return *this;
            }
            reference operator*() const {
                return static_cast<TrueNode*>(ptr_val)->x;
            }
            pointer operator->() const {
                return &(static_cast<TrueNode*>(ptr_val)->x);
            }
            ListBasicIterator& operator++() {
                ptr_val = ptr_val->right;
                return *this;
            }
            ListBasicIterator& operator--() {
                ptr_val = ptr_val->left;
                return *this;
            }
            ListBasicIterator operator++(int) {
                ListBasicIterator copy = *this;
                ++*this;
                return copy;
            }
            ListBasicIterator operator--(int) {
                ListBasicIterator copy = *this;
                --*this;
                return copy;
            }
            bool operator!=(const ListBasicIterator& other) const {
                return (ptr_val != other.ptr_val);
            }
            bool operator==(const ListBasicIterator& other) const {
                return (ptr_val == other.ptr_val);
            }
        };
        using iterator = ListBasicIterator<T>;
        using const_iterator = ListBasicIterator<const T>;
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
                AllocTraits::construct(alloc, ver, itr.ptr_val->left, itr.ptr_val, elem);
            } catch(...) {
                AllocTraits::deallocate(alloc, ver, 1);
                throw;
            }
            itr.ptr_val->left->right = static_cast<Node*>(ver);
            itr.ptr_val->left = static_cast<Node*>(ver);
            ++sz;
            return iterator(itr.ptr_val->left);
        }
        void erase(const_iterator itr) {
            Node* lv = itr.ptr_val->left;
            Node* rv = itr.ptr_val->right;
            lv->right = rv;
            rv->left = lv;
            AllocTraits::destroy(alloc, static_cast<TrueNode*>(itr.ptr_val));
            AllocTraits::deallocate(alloc, static_cast<TrueNode*>(itr.ptr_val), 1);
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
}

using namespace my_list;

template<typename Key,
         typename Value,
         typename Hash = std::hash<Key>,
         typename Equal = std::equal_to<Key>,
         typename Alloc = std::allocator< std::pair<const Key, Value> >
>
class UnorderedMap {
public:
    using NodeType = std::pair<const Key, Value>;
private:
    using listAllocType = typename std::allocator_traits<Alloc>::template rebind_alloc<NodeType*>;
    using MapList = List<NodeType*, listAllocType>;
    // удобнее же хранить именно указатели, потом при rehash нам нужно чистить list и все равно нужно будет что-то дополнительное делать,
    // чтобы элементы не удалять, а так я просто перемещаю указатели и спокойно чищу list
    using constListIterator = typename MapList::const_iterator;
    using listIterator = typename MapList::iterator;
    using listIteratorAllocType = typename std::allocator_traits<Alloc>::template rebind_alloc<std::pair<listIterator, listIterator>>;
    
    Hash hash_f;
    Equal equal_f;
    Alloc allocator;
    MapList global_list;
    std::vector<std::pair<listIterator, listIterator>, listIteratorAllocType> hash_table;

    // NOLINTNEXTLINE(readability-magic-numbers)
    float max_factor = 0.6f;

    size_t getInd(const Key& key) const {
        return hash_f(key) % hash_table.size();
    }
    void rehash(size_t sz) {
        hash_table.clear();

        MapList copy = std::move(global_list);

        hash_table.resize(sz);

        std::fill(hash_table.begin(), hash_table.end(), std::make_pair(listIterator(), listIterator()));
        for (listIterator it = copy.begin(); it != copy.end(); ++it) {
            size_t ind = getInd((*it)->first);
            if (hash_table[ind].first == listIterator()) {
                hash_table[ind].first = global_list.insert(global_list.end(), std::move(*it));
                hash_table[ind].second = hash_table[ind].first;
            } else {
                hash_table[ind].first = global_list.insert(hash_table[ind].first, *it);
            }
        }
    }

    constListIterator base_find(size_t ind, const Key& key) const {
        if (hash_table[ind].first == listIterator()) {
            return global_list.end();
        }
        auto last = hash_table[ind].second;
        ++last;
        for (auto it = hash_table[ind].first; it != last; ++it) {
            if (equal_f((*it)->first, key)) {
                return it;
            }
        }
        return global_list.end();
    }
public:
    template<bool IsConst>
    struct ListBasicIterator {
    friend class UnorderedMap;
    public:
        using difference_type = std::ptrdiff_t;
        using value_type = NodeType;
        using pointer = typename std::conditional<IsConst, const NodeType*, NodeType *>::type;
        using reference = typename std::conditional<IsConst, const NodeType&, NodeType&>::type;
        using iterator_category = std::forward_iterator_tag;
        using iterator_type = typename std::conditional<IsConst, constListIterator, listIterator>::type;
    private:
        iterator_type it;
    public:
        ListBasicIterator() = default;
        ListBasicIterator(iterator_type it): it(it) {}
        iterator_type get_listiterator() const {
            return it;
        }
        reference operator*() const {
            return *(*it);
        }
        pointer operator->() const {
            return *it;
        }
        ListBasicIterator& operator++() {
            ++it;
            return *this;
        }
        ListBasicIterator& operator--() {
            --it;
            return *this;
        }
        ListBasicIterator operator++(int) {
            ListBasicIterator copy = *this;
            ++(*this);
            return copy;
        }
        ListBasicIterator operator--(int) {
            ListBasicIterator copy = *this;
            --(*this);
            return copy;
        }
        bool operator!=(const ListBasicIterator& other) const {
            return (it != other.it);
        }
        bool operator==(const ListBasicIterator& other) const {
            return !(*this != other);
        }
    };
    using iterator = ListBasicIterator<false>;
    using const_iterator = ListBasicIterator<true>;
    UnorderedMap(): hash_table(1) {
        std::fill(hash_table.begin(), hash_table.end(), std::make_pair(listIterator(), listIterator()));
    }
    UnorderedMap(const UnorderedMap& other): allocator(std::allocator_traits<Alloc>::select_on_container_copy_construction(other.allocator)),
                                             max_factor(other.max_factor) {
        hash_table.resize(other.hash_table.size());

        std::fill(hash_table.begin(), hash_table.end(), std::make_pair(listIterator(), listIterator()));

        for (auto it : other) {
            insert(it);
        }
    }

    UnorderedMap(UnorderedMap&& other): hash_f(std::move(other.hash_f)), equal_f(std::move(other.equal_f)), allocator(std::move(other.allocator)), global_list(std::move(other.global_list)),
                                        hash_table(std::move(other.hash_table)), max_factor(std::move(other.max_factor)) {}
    // NOLINTNEXTLINE(readability-function-size)
    void swap(UnorderedMap&& mp) {
        if (std::allocator_traits<Alloc>::propagate_on_container_swap::value) {
            allocator = mp.allocator;
        }
        hash_f = std::move(mp.hash_f);
        equal_f = std::move(mp.equal_f);
        max_factor = std::move(mp.max_factor);
        for (auto it = global_list.begin(); it != global_list.end(); ++it) {
            std::allocator_traits<Alloc>::destroy(allocator, (*it));
            std::allocator_traits<Alloc>::deallocate(allocator, (*it), 1);
        }
        global_list.clear();
        if (mp.size() != 1) {
            hash_table = std::move(mp.hash_table);
            global_list = std::move(mp.global_list);
        } else {
            hash_table.clear();

            hash_table.resize(mp.hash_table.size());

            std::fill(hash_table.begin(), hash_table.end(), std::make_pair(listIterator(), listIterator()));
            for (auto it : mp.global_list) {
                size_t ind = getInd((it)->first);
                NodeType* pointer = std::allocator_traits<Alloc>::allocate(allocator, 1);
                std::allocator_traits<Alloc>::construct(allocator, pointer, std::move(*it));
                if (hash_table[ind].first == listIterator()) {
                    hash_table[ind].first = global_list.insert(global_list.end(), pointer);
                    hash_table[ind].second = hash_table[ind].first;
                } else {
                    hash_table[ind].first = global_list.insert(hash_table[ind].first, pointer);
                }
            }
            for (auto it = mp.global_list.begin(); it != mp.global_list.end(); ++it) {
                std::allocator_traits<Alloc>::destroy(mp.allocator, (*it));
                std::allocator_traits<Alloc>::deallocate(mp.allocator, (*it), 1);
            }
            mp.hash_table.clear();
            mp.global_list.clear();
        }
    }

    void swap(UnorderedMap& mp) {
        if (this == &mp) {
            return;
        }
        auto copy = std::move(*this);
        swap(std::move(mp));
        mp.swap(std::move(copy));
    }


    UnorderedMap& operator=(const UnorderedMap& mp) {
        if (this == &mp) {
            return *this;
        }
        UnorderedMap copy(mp);
        *this = std::move(copy);
        return *this;
    }

    UnorderedMap& operator=(UnorderedMap&& mp) {
        if (this == &mp) {
            return *this;
        }
        swap(std::move(mp));
        return *this;
    }

    Alloc get_allocator() const {
        return allocator;
    }

    iterator begin() {
        return iterator(global_list.begin());
    }

    iterator end() {
        return iterator(global_list.end());
    }

    const_iterator cbegin() const {
        return const_iterator(global_list.cbegin());
    }

    const_iterator cend() const {
        return const_iterator(global_list.cend());
    }

    const_iterator begin() const {
        return cbegin();
    }

    const_iterator end() const {
        return cend();
    }

    iterator find(const Key& key) {
        constListIterator ans = base_find(getInd(key), key);
        return iterator(listIterator(ans.ptr_val));
    }

    const_iterator find(const Key& key) const {
        return const_iterator(base_find(getInd(key), key));
    }

    template <typename... Args>
    std::pair<iterator, bool> emplace(Args&&... args) {
        check_load();
        NodeType* pointer = std::allocator_traits<Alloc>::allocate(allocator, 1);
        try {
            std::allocator_traits<Alloc>::construct(allocator, pointer, std::forward<Args>(args)...);
        } catch(...) {
            std::allocator_traits<Alloc>::deallocate(allocator, pointer, 1);
            throw;
        }
        auto ans = find(pointer->first);
        if (ans == end()) {
            auto ind = getInd(pointer->first);
            if (hash_table[ind].first == listIterator()) {
                hash_table[ind].first = global_list.end();
            }
            hash_table[ind].first = global_list.insert(hash_table[ind].first, pointer);
            if (hash_table[ind].second == listIterator()) {
                hash_table[ind].second = hash_table[ind].first;    
            }
            return {iterator(hash_table[ind].first), true};
        } else {
            std::allocator_traits<Alloc>::destroy(allocator, pointer);
            std::allocator_traits<Alloc>::deallocate(allocator, pointer, 1);
            return {ans, false};
        }
    }

    std::pair<iterator, bool> insert(const NodeType& val) {
        return emplace(val);
    }
    template <typename InputIterator>
    void insert(InputIterator first, InputIterator second) {
        for (auto it = first; it != second; ++it) {
            insert(*it);
        }
    }

    template<typename T>
    std::pair<iterator, bool> insert(T&& val) {
        return emplace(std::forward<T>(val));
    }

    iterator erase(iterator iter) {
        size_t ind = getInd(iter->first);
        iterator ans = ++iter;
        --iter;
        if (hash_table[ind] == iter) {
            if (hash_table[ind].first == hash_table[ind].second) {
                hash_table[ind].first = hash_table[ind].second = listIterator(nullptr);
            } else {
                ++hash_table[ind].first;
            }
        } else if (hash_table[ind].second == iter) {
            --hash_table[ind].second;
        }
        global_list.erase(iter.get_listiterator());
        return ans;
    }

    iterator erase(iterator left, iterator right) {
        iterator posl = right;
        --posl; 
        for (auto it = left; it != right; it=erase(it)) {
            if (it == posl) {
                return erase(it);
            }
        }
        return end();
    }

    Value& operator[](const Key& key) {
        std::pair<iterator, bool> ans = insert({key, Value()});
        return ans.first->second;
    }

    Value& operator[](Key&& key) {
        std::pair<iterator, bool> ans = insert({std::move(key), Value()});
        return ans.first->second;
    }

    Value& at(const Key& key) {
        iterator it = find(key);
        if (it == end()) {
            throw std::out_of_range("key is not found");
        }
        return it->second;
    }

    const Value& at(const Key& key) const {
        const_iterator it = find(key);
        if (it == end()) {
            throw std::out_of_range("key is not found");
        }
        return it->second;
    }


    float load_factor() const {
        return static_cast<float>(global_list.size() + 1) / static_cast<float>(hash_table.size());
    }

    float max_load_factor() const {
        return max_factor;
    }

    void max_load_factor(float new_load_factor) {
        max_factor = new_load_factor;
    }

    void check_load() {
        if (load_factor() >= max_load_factor()) {
            reserve(hash_table.size() * 2);
        }
    }
    void reserve(size_t n) {
        if (n > hash_table.size()) {
            rehash(static_cast<size_t>(static_cast<float>(n) / max_factor));
        }
    }
    size_t size() const {
        return global_list.size();
    }
    ~UnorderedMap() {
        for (auto it = global_list.begin(); it != global_list.end(); ++it) {
            std::allocator_traits<Alloc>::destroy(allocator, (*it));
            std::allocator_traits<Alloc>::deallocate(allocator, (*it), 1);
        }
        global_list.clear();
        hash_table.clear();
        hash_table.shrink_to_fit();
    }
};

