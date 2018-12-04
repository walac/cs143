#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <unordered_map>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <type_traits>
#include <iterator>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

typedef std::remove_pointer<Symbol>::type symbol_t;

bool operator==(const symbol_t &a, const symbol_t &b) {
    return a.equal_string(b.get_string(), b.get_len());
}

bool operator!=(const symbol_t &a, const symbol_t &b) {
    return !(a == b);
}

template<typename T>
class node_iterator : public std::iterator<std::forward_iterator_tag, T> {
public:
    typedef list_node<T> list_type;
    typedef std::iterator<std::forward_iterator_tag, T> iterator_type;

    using typename iterator_type::iterator_category;
    using typename iterator_type::value_type;
    using typename iterator_type::difference_type;
    using typename iterator_type::pointer;
    using typename iterator_type::reference;

    node_iterator(list_type *l) : l_(l), i_(l->first()) {}
    node_iterator() : l_(nullptr) {}

    node_iterator &operator++() {
        i_ = l_->next(i_);
        return *this;
    }

    node_iterator operator++(int) const {
        auto self = *this;
        i_ = l_->next(i_);
        return self;
    }

    value_type operator*() const {
        return l_->nth(i_);
    }

    value_type operator->() {
        return l_->nth(i_);
    }

    bool operator==(const node_iterator &rhs) const {
        assert(l_ != nullptr);
        return rhs.l_
            ? l_ == rhs.l_ && i_ == rhs.i_
            : !l_->more(i_);
    }

    bool operator!=(const node_iterator &rhs) const {
        assert(l_ != nullptr);
        return rhs.l_
            ? l_ != rhs.l_ || i_ != rhs.i_
            : l_->more(i_);
    }

private:
    list_type *l_;
    int i_;
};

template<typename T>
node_iterator<T> begin(list_node<T> &l) {
    return node_iterator<T>(&l);
}

template<typename T>
node_iterator<T> end(list_node<T> &) {
    return node_iterator<T>();
}

class ClassTable {
private:
    int semant_errors;
    void install_basic_classes();
    ostream& error_stream;
    std::unordered_map<Symbol, Class_> classes_;
    Class_ cls_;

public:
    std::unordered_map<Symbol, SymbolTable<Symbol, std::remove_pointer<Symbol>::type>> attrs;
    std::unordered_map<Symbol, SymbolTable<Symbol, method_class>> methods;

private:
	bool has_cycle(Class_ cls);
	void set_current_class(Class_ cls) { cls_ = cls; }

public:
    ClassTable(Classes);
    int errors() { return semant_errors; }
    ostream& semant_error();
    ostream& semant_error(Class_ c);
    ostream& semant_error(Symbol filename, tree_node *t);
	Class_ get_class(Symbol name);
	Class_ get_class() { return cls_; }
    bool leq(Symbol derived, Symbol ancestor);
    Symbol find_common_ancestor(Symbol a, Symbol b);

	template<typename Key, typename Value>
	Value *lookup(std::unordered_map<Symbol, SymbolTable<Key, Value>> &symtab, Symbol class_name, Symbol key);
};

#endif

