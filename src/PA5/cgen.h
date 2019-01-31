#include <assert.h>
#include <stdio.h>
#include <unordered_map>
#include <type_traits>
#include <iterator>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

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

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
    List<CgenNode> *nds;
    ostream& str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;


    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

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
    CgenNodeP parentnd;                        // Parent of class
    List<CgenNode> *children;                  // Children of class
    Basicness basic_status;                    // `Basic' if class is basic
                                                // `NotBasic' otherwise
    std::unordered_map<Symbol, int> attr_indexes;
    std::unordered_map<Symbol, int> meth_indexes;

public:
    CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode> *get_children() { return children; }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd() { return parentnd; }
    int basic() { return (basic_status == Basic); }
    int emit_attr_index(Symbol p) const;
    int emit_meth_index(Symbol p) const;

private:
    int max_index(const std::unordered_map<Symbol, int> &i) const;
    void build_indexes();
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

