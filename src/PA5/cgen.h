#include <assert.h>
#include <stdio.h>
#include <vector>
#include <type_traits>
#include <iterator>
#include <algorithm>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

using namespace std;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

typedef remove_pointer<Symbol>::type symbol_t;

bool operator==(const symbol_t &a, const symbol_t &b) {
    return a.equal_string(b.get_string(), b.get_len());
}

bool operator!=(const symbol_t &a, const symbol_t &b) {
    return !(a == b);
}

bool operator<(const symbol_t &a, const symbol_t &b) {
    return std::lexicographical_compare(
        a.get_string(),
        a.get_string() + a.get_len(),
        b.get_string(),
        b.get_string() + b.get_len()
    );
}

template<typename T>
class node_iterator : public std::iterator<forward_iterator_tag, T> {
public:
    typedef list_node<T> list_type;
    typedef iterator<std::forward_iterator_tag, T> iterator_type;

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

template<typename U, typename K>
int index_of(const U& container, const K& key) {
    auto b = begin(container);
    auto e = end(container);
    auto it = find(b, e, key);
    return it != e ? distance(b, it) : -1;
}

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
    List<CgenNode> *nds;
    ostream& str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;
    vector<Symbol> tags;

    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();
    int tag(Symbol p) const;
    void class_nameTab();

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);
    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);
public:
    CgenClassTable(Classes, ostream& str);
    void code();
    CgenNodeP root();
};


class CgenNode : public class__class {
private: 
    CgenNodeP parentnd;         // Parent of class
    List<CgenNode> *children;   // Children of class
    Basicness basic_status;     // `Basic' if class is basic or `NotBasic' otherwise
    vector<Symbol> attributes;
    vector<Symbol> methods;
    Symbol find_cls_method(Symbol method);

public:
    CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }
    int lookup_attr(Symbol name) const { return index_of(attributes, name); }
    int lookup_meth(Symbol name) const { return index_of(methods, name); }
    void code_protObj(int tag, ostream &os);
    void code_dispatchTab(ostream &os);
};

class BoolConst 
{
private: 
    int val;
public:
    BoolConst(int);
    void code_def(ostream&, int boolclasstag);
    void code_ref(ostream&) const;
};

class Context {
public:
    Context(CgenNodeP so)
        : so(so) {}

    CgenNodeP get_so() const { return so; }
    int lookup_attr(Symbol name) const { return so->lookup_attr(name); }
    int lookup_var(Symbol name) const { return index_of(let_vars, name); }
    int lookup_param(Symbol name) const { 
        auto i = index_of(params, name);
        if (i == -1) return i;
        return params.size() - 1 - i;
    }

    int add_let(Symbol name) {
        let_vars.push_back(name);
        return let_vars.size() - 1;
    }

    int add_param(Symbol name) {
        params.push_back(name);
        return params.size() - 1;
    }

private:
    CgenNodeP so;
    vector<Symbol> let_vars;
    vector<Symbol> params;
};
