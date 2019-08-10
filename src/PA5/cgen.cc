//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <sstream>
#include <unordered_map>
#include <list>
#include "cgen.h"
#include "cgen_gc.h"

#define OBJCOPY "Object.copy"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

unordered_map<Symbol, CgenNodeP> sym_map;

static int lnum;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    ::copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
{ "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
{ "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

    os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(const char *dest_reg, int offset, const char *source_reg, ostream& s)
{
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
        << endl;
}

static void emit_store(const char *source_reg, int offset, const char *dest_reg, ostream& s)
{
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
        << endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(const char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(const char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s)
{
    emit_partial_load_address(dest,s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream& s)
{
    emit_partial_load_address(dest,s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(const char *dest, IntEntry *i, ostream& s)
{
    emit_partial_load_address(dest,s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(const char *dest, const char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(const char *dest, const char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(const char *dest, const char *src1, const char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(const char *dest, const char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(const char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
    emit_label_ref(l,s);
    s << ":" << endl;
}

static void emit_beqz(const char *source, int label, ostream &s)
{
    s << BEQZ << source << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_beq(const char *src1, const char *src2, int label, ostream &s)
{
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bne(const char *src1, const char *src2, int label, ostream &s)
{
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bleq(const char *src1, const char *src2, int label, ostream &s)
{
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blt(const char *src1, const char *src2, int label, ostream &s)
{
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_blti(const char *src1, int imm, int label, ostream &s)
{
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_bgti(const char *src1, int imm, int label, ostream &s)
{
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label,s);
    s << endl;
}

static void emit_branch(int l, ostream& s)
{
    s << BRANCH;
    emit_label_ref(l,s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(const char *reg, ostream& str)
{
    emit_store(reg,0,SP,str);
    emit_addiu(SP,SP,-4,str);
}

static void emit_pop(const char *reg, ostream &str)
{
    emit_addiu(SP,SP,WORD_SIZE,str);
    emit_load(reg,0,SP,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(const char *dest, const char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(const char *source, const char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP,SP,4,s);
    emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
    if (source != (char*)A1) emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s  << LABEL                                             // label
        << WORD << stringclasstag << endl                                 // tag
        << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
        << WORD;
    emit_disptable_ref(Str, s);
    s << endl;


    /***** Add dispatch information for class String ******/

    s << endl;                                              // dispatch table
    s << WORD;  lensym->code_ref(s);  s << endl;            // string length
    emit_string_constant(s,str);                                // ascii string
    s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
    for (List<StringEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                // label
        << WORD << intclasstag << endl                      // class tag
        << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
        << WORD;
    emit_disptable_ref(Int, s);
    s << endl;

    /***** Add dispatch information for class Int ******/

    s << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
    s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                  // label
        << WORD << boolclasstag << endl                       // class tag
        << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
        << WORD;
    emit_disptable_ref(Bool, s);
    s << endl;

    /***** Add dispatch information for class Bool ******/

    s << endl;                                            // dispatch table
    s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
    Symbol main    = idtable.lookup_string(MAINNAME);
    Symbol string  = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc   = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
    str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
    str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
    str << GLOBAL; falsebool.code_ref(str);  str << endl;
    str << GLOBAL; truebool.code_ref(str);   str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL
        << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL 
        << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL 
        << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL 
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"),str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"),str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"),str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl << GLOBAL;
    str << CLASSOBJTAB << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
    falsebool.code_def(str,boolclasstag);
    truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str,stringclasstag);
    inttable.code_string_table(str,intclasstag);
    code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
    enterscope();
    if (cgen_debug) cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

    stringclasstag = tag(Str);
    intclasstag = tag(Int);
    boolclasstag = tag(Bool);

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes()
{

    // The tree package uses these globals to annotate the classes built below.
    //curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class,
	        new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
    addid(SELF_TYPE,
	        new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
    addid(prim_slot,
	        new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

    // 
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class name
    //        copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    install_class(
            new CgenNode(
                class_(Object, 
	                No_class,
	                append_Features(
                        append_Features(
                            single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                            single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                        single_Features(method(::copy, nil_Formals(), SELF_TYPE, no_expr()))),
	                filename),
                Basic,this));

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    install_class(
            new CgenNode(
                class_(IO, 
                    Object,
                    append_Features(
                        append_Features(
                            append_Features(
                                single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                        SELF_TYPE, no_expr())),
                                single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                        SELF_TYPE, no_expr()))),
                            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                        single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	                filename),	    
                Basic,this));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    install_class(
            new CgenNode(
                class_(Int, 
	                Object,
                    single_Features(attr(val, prim_slot, no_expr())),
	                filename),
                Basic,this));

    //
    // Bool also has only the "val" slot.
    //
    install_class(
            new CgenNode(
                class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
                Basic,this));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //       
    install_class(
            new CgenNode(
                class_(Str, 
	                Object,
                    append_Features(
                        append_Features(
                            append_Features(
                                append_Features(
                                    single_Features(attr(val, Int, no_expr())),
                                    single_Features(attr(str_field, prim_slot, no_expr()))),
                                single_Features(method(length, nil_Formals(), Int, no_expr()))),
                            single_Features(method(concat, 
				                    single_Formals(formal(arg, Str)),
				                    Str, 
				                    no_expr()))),
	                    single_Features(method(substr, 
				                append_Formals(single_Formals(formal(arg, Int)), 
						            single_Formals(formal(arg2, Int))),
				                Str, 
				                no_expr()))),
	                filename),
                Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
    Symbol name = nd->get_name();

    if (probe(name))
    {
        return;
    }

    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd,nds);
    addid(name,nd);
    tags.push_back(name);
    sym_map[name] = nd;
}

void CgenClassTable::install_classes(Classes cs)
{
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i),NotBasic,this));

    sort(begin(tags), end(tags), [](auto a, auto b) { return *a < *b; });
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
    for(List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());

    for(List<CgenNode> *l = nds; l; l = l->tl()) {
        std::list<CgenNodeP> ll;
        auto p = l->hd();
        for (auto c = p; c; c = c->get_parentnd())
            ll.push_front(c);

        for (auto c: ll) {
            for (auto feature: *c->features) {
                auto a = dynamic_cast<attr_class*>(feature);
                if (a) {
                    p->attributes.push_back(a->name);
                } else {
                    auto m = dynamic_cast<method_class&>(*feature);
                    auto e = end(p->methods);
                    if (find_if(begin(p->methods), e, [&m] (auto meth) -> bool {return *meth == *m.name;}) == e) {
                        p->methods.push_back(m.name);
                    }
                }
            }
        }
    }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

int CgenClassTable::tag(Symbol p) const
{
    auto e = end(tags);
    auto first = begin(tags);
    auto low = lower_bound(first, e, p, [](auto a, auto b) { return *a < *b; });
    assert(low != e && **low == *p);
    return distance(first, low);
}

void CgenNode::add_child(CgenNodeP n)
{
    children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::class_nameTab()
{
    str << CLASSNAMETAB << LABEL << endl;
    for (auto cls_name: tags) {
        auto entry = stringtable.lookup_string(cls_name->get_string());
        assert(entry);
        str << WORD;
        entry->code_ref(str);
        str << endl;
    }
}

void CgenClassTable::code()
{
    if (cgen_debug) cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug) cout << "coding constants" << endl;
    code_constants();

    if (cgen_debug) cout << "choosing gc" << endl;
    code_select_gc();

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //
    class_nameTab();
    for (decltype(tags.size()) i = 0; i < tags.size(); ++i) {
        auto cls = lookup(tags[i]);
        cls->code_protObj(i, str);
        cls->code_dispatchTab(str);
    }

    if (cgen_debug) cout << "coding global text" << endl;
    code_global_text();

    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...
    for (decltype(tags.size()) i = 0; i < tags.size(); ++i) {
        auto name = tags[i];
        auto cls = lookup(name);
        cls->code_init(str);
        if (*name != *Int && *name != *Str && *name != *Object && *name != *IO)
            cls->code_methods(str);
    }

    str << CLASSOBJTAB << LABEL << endl;
    for (auto it: tags) {
        str << WORD << *it << PROTOBJ_SUFFIX << endl;
        str << WORD << *it << CLASSINIT_SUFFIX << endl;
    }
}


CgenNodeP CgenClassTable::root()
{
    return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
    class__class((const class__class &) *nd),
    parentnd(NULL),
    children(NULL),
    basic_status(bstatus)
{ 
    stringtable.add_string(name->get_string());          // Add class name to string table
}

void CgenNode::protObj(Features features, ostream &os)
{
    auto default_int = inttable.lookup_string("0");
    auto default_str = stringtable.lookup_string("");

    for (auto feature: *features) {
        auto attr = dynamic_cast<attr_class *>(feature);
        if (attr == nullptr) continue;
        auto type = attr->type_decl;

        if (*type == *Str) {
            os << WORD;
            default_str->code_ref(os);
        } else if (*type == *Int) {
            os << WORD;
            default_int->code_ref(os);
        } else if (*type == *Bool) {
            os << WORD;
            falsebool.code_ref(os);
        } else {
            os << WORD << 0;
        }

        os << " # " << attr->name << endl;
    }
}

void CgenNode::code_protObj(int tag, ostream &os)
{
    os << WORD << "-1" << endl;
    emit_protobj_ref(name, os);
    os << LABEL << endl;
    os << WORD << tag << endl;
    os << WORD << 4 * (attributes.size() + DEFAULT_OBJFIELDS) << endl;
    os << WORD << name << DISPTAB_SUFFIX << endl;

    vector<CgenNodeP> cls;
    for (auto c = this; *c->get_name() != *No_class; c = c->parentnd)
        cls.push_back(c);

    for_each(crbegin(cls), crend(cls), [&os](auto class_) {
        class_->protObj(class_->features, os);
    });
}

Symbol CgenNode::find_cls_method(Symbol method)
{
    for (auto cls = this; *cls->name != *No_class; cls = cls->get_parentnd()) {
        auto e = end(*cls->features);
        auto it = find_if(begin(*cls->features), e, [=](auto feature) -> bool {
            auto meth = dynamic_cast<method_class*> (feature);
            if (meth == nullptr) return false;
            return *meth->name == *method;
        });

        if (it != e)
            return cls->name;
    }

    return nullptr;
}

void CgenNode::code_dispatchTab(ostream &os)
{
    vector<pair<Symbol, Symbol>> disp_tbl(methods.size());

    for (decltype(methods.size()) i = 0; i < methods.size(); ++i) {
        auto p = methods[i];
        auto cls = find_cls_method(p);
        assert(cls);
        disp_tbl[i] = make_pair(cls, p);
    }

    emit_disptable_ref(name, os);
    os << LABEL << endl;

    for (auto meth: disp_tbl) {
        os << WORD;
        emit_method_ref(meth.first, meth.second, os);
        os << endl;
    }
}

void CgenNode::code_methods(ostream &os)
{
    for (auto feature: *features) {
        auto method = dynamic_cast<method_class*>(feature);
        if (!method) continue;
        os << GLOBAL;
        emit_method_ref(name, method->name, os);
        os << endl;
        emit_method_ref(name, method->name, os);
        os << LABEL << endl;
        emit_push(FP, os);
        emit_move(FP, SP, os);
        emit_push(RA, os);
        Context ctx(this);
        for (auto formal: *method->formals) {
            auto param = dynamic_cast<formal_class&>(*formal);
            ctx.add_param(param.name);
        }
        method->expr->code(os, ctx);
        emit_pop(RA, os);
        emit_pop(FP, os);
        emit_addiu(SP, SP, WORD_SIZE*method->formals->len(), os);
        emit_return(os);
    }
}

void CgenNode::code_init(ostream &os)
{
    os << GLOBAL;
    emit_init_ref(name, os);
    os << endl;
    emit_init_ref(name, os);
    os << LABEL << endl;
    emit_push(RA, os);
    if (*parent != *No_class) {
        emit_partial_load_address(T1, os);
        emit_init_ref(parent, os);
        os << endl;
        emit_jalr(T1, os);
    }

    for (auto feature: *features) {
        auto attr = dynamic_cast<attr_class*>(feature);
        if (attr != nullptr) {
            Context c(this);
            if (attr->init->get_type()) {
                os << "# Initializing " << attr->name << endl;
                attr->init->code(os, c);
                emit_store(ACC, lookup_attr(attr->name) + DEFAULT_OBJFIELDS, SELF, os);
                os << "# " << attr->name << " initialized\n";
            }
        }
    }

    emit_pop(RA, os);
    emit_return(os);
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

// Activation record layout
//
// +-----------------+
// | Self Object     |
// | ARG 0           |
// | ARG 1           |
// | ...             |
// | ARG N           |
// | Frame Pointer   |
// | Return Address  |
// | LET 0           |
// | LET 1           |
// | ...             |
// | LET N           |
// +-----------------+

void assign_class::code(ostream &s, Context c) {
    int i;
    expr->code(s, c);
    if ((i = c.lookup_var(name)) != -1) {
        emit_store(ACC, -i - 1, FP, s);
    } else if ((i = c.lookup_param(name)) != -1) {
        emit_store(ACC, i + 1, FP, s);
    } else {
        i = c.lookup_attr(name);
        assert(i != -1);
        emit_store(ACC, i + DEFAULT_OBJFIELDS, SELF, s);
    }
}

void static_dispatch_class::code(ostream &s, Context c) {
    emit_push(SELF, s);
    for (auto a: *actual) {
        a->code(s, c);
        emit_push(ACC, s);
    }
    expr->code(s, c);
    emit_bne(ACC, ZERO, lnum, s);
    auto fname = c.get_so()->get_filename()->get_string();
    stringtable.add_string(fname);
    emit_load_string(ACC, stringtable.lookup_string(fname), s);
    emit_load_imm(T1, curr_lineno, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(lnum++, s);
    emit_move(SELF, ACC, s);
    auto cls = sym_map[type_name];
    emit_partial_load_address(T1, s);
    emit_disptable_ref(cls->get_name(), s);
    s << endl;
    emit_load(T1, cls->lookup_meth(name), T1, s);
    emit_jalr(T1, s);
    emit_pop(SELF, s);
}

void dispatch_class::code(ostream &s, Context c) {
    emit_push(SELF, s);
    for (auto a: *actual) {
        a->code(s, c);
        emit_push(ACC, s);
    }
    expr->code(s, c);
    emit_bne(ACC, ZERO, lnum, s);
    auto fname = c.get_so()->get_filename()->get_string();
    stringtable.add_string(fname);
    emit_load_string(ACC, stringtable.lookup_string(fname), s);
    emit_load_imm(T1, curr_lineno, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(lnum++, s);
    emit_move(SELF, ACC, s);
    auto cls = *expr->get_type() == *SELF_TYPE ? c.get_so() : sym_map[expr->get_type()];
    emit_load(T1, 2, SELF, s);
    emit_load(T1, cls->lookup_meth(name), T1, s);
    emit_jalr(T1, s);
    emit_pop(SELF, s);
}

void cond_class::code(ostream &s, Context c) {
    pred->code(s, c);
    emit_fetch_int(ACC, ACC, s);
    int elsebranch = lnum++;
    emit_beq(ACC, ZERO, elsebranch, s);
    then_exp->code(s, c);
    auto label = lnum++;
    emit_branch(label, s);
    emit_label_def(elsebranch, s);
    else_exp->code(s, c);
    emit_label_def(label, s);
}

void loop_class::code(ostream &s, Context c) {
    int looplabel = lnum++;
    emit_label_def(looplabel, s);
    pred->code(s, c);
    emit_fetch_int(ACC, ACC, s);
    auto exit = lnum++;
    emit_beq(ACC, ZERO, exit, s);
    body->code(s, c);
    emit_branch(looplabel, s);
    emit_label_def(exit, s);
    emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s, Context c) {
}

void block_class::code(ostream &s, Context c) {
    for (auto b: *body) {
        b->code(s, c);
    }
}

void let_class::code(ostream &s, Context c) {
    if (init->type != nullptr)
        init->code(s, c);
    else if (*type_decl == *Str)
        emit_load_string(ACC, stringtable.lookup_string(""), s);
    else if (*type_decl == *Int)
        emit_load_int(ACC, inttable.lookup_string("0"), s);
    else if (*type_decl == *Bool)
        emit_load_bool(ACC, falsebool, s);
    else
        emit_move(ACC, ZERO, s);

    c.add_let(identifier);
    emit_push(ACC, s);
    body->code(s, c);
    emit_pop(T1, s);
}

void plus_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_fetch_int(ACC, ACC, s);
    emit_push(ACC, s);
    e2->code(s, c);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal(OBJCOPY, s);
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_add(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void sub_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_fetch_int(ACC, ACC, s);
    emit_push(ACC, s);
    e2->code(s, c);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal(OBJCOPY, s);
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_sub(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void mul_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_fetch_int(ACC, ACC, s);
    emit_push(ACC, s);
    e2->code(s, c);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal(OBJCOPY, s);
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_mul(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void divide_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_fetch_int(ACC, ACC, s);
    emit_push(ACC, s);
    e2->code(s, c);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    emit_jal(OBJCOPY, s);
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_div(T1, T1, T2, s);
    emit_store_int(T1, ACC, s);
}

void neg_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_jal(OBJCOPY, s);
    emit_fetch_int(T1, ACC, s);
    emit_neg(T1, T1, s);
    emit_store_int(T1, ACC, s);
}

void lt_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    e2->code(s, c);
    emit_fetch_int(T2, ACC, s);
    emit_pop(T1, s);
    auto ltlabel = lnum++;
    emit_blt(T1, T2, ltlabel, s);
    emit_load_bool(ACC, falsebool, s);
    emit_branch(lnum, s);
    emit_label_def(ltlabel, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(lnum++, s);
}

void eq_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_push(ACC, s);
    e2->code(s, c);
    emit_move(T2, ACC, s);
    emit_pop(T1, s);
    emit_load_bool(ACC, truebool, s);
    emit_load_bool(A1, falsebool, s);

    if (*e1->type == *Int || *e1->type == *Bool || *e1->type == *Str) {
        emit_jal("equality_test", s);
    } else {
        emit_beq(T1, T2, lnum, s);
        emit_move(ACC, A1, s);
        emit_label_def(lnum++, s);
    }
}

void leq_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_fetch_int(T1, ACC, s);
    emit_push(T1, s);
    e2->code(s, c);
    emit_fetch_int(T2, ACC, s);
    emit_pop(T1, s);
    auto ltlabel = lnum++;
    emit_bleq(T1, T2, ltlabel, s);
    emit_load_bool(ACC, falsebool, s);
    emit_branch(lnum, s);
    emit_label_def(ltlabel, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(lnum++, s);
}

void comp_class::code(ostream &s, Context c) {
    e1->code(s, c);
    emit_fetch_int(T1, ACC, s);
    emit_load_bool(ACC, truebool, s);
    emit_fetch_int(T2, ACC, s);
    emit_bne(T1, T2, lnum, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(lnum++, s);
}

void int_const_class::code(ostream& s, Context c)  
{
    auto tk = token->get_string();
    auto entry = inttable.lookup_string(tk);
    //if (!entry) entry = inttable.add_string(tk);
    emit_load_int(ACC,entry,s);
}

void string_const_class::code(ostream& s, Context c)
{
    auto tk = token->get_string();
    auto entry = stringtable.lookup_string(tk);
    //if (!entry) entry = stringtable.add_string(tk);
    emit_load_string(ACC,entry,s);
}

void bool_const_class::code(ostream& s, Context c)
{
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, Context c) {
    if (*type_name == *SELF_TYPE) {
        emit_partial_load_address(T1, s);
        s << CLASSOBJTAB << endl;
        emit_load(T2, 0, SELF, s);
        emit_load_imm(ACC, 8, s);
        emit_mul(T2, T2, ACC, s);
        emit_add(T1, T1, T2, s);
        emit_load(ACC, 0, T1, s);
        emit_push(T1, s);
    } else {
        emit_partial_load_address(ACC, s);
        emit_protobj_ref(type_name, s);
        s << endl;
    }
    emit_jal(OBJCOPY, s);
    if (*type_name == *SELF_TYPE)
        emit_pop(T1, s);
    emit_push(SELF, s);
    emit_move(SELF, ACC, s);
    emit_push(ACC, s);
    if (*type_name != *SELF_TYPE) {
        emit_partial_load_address(T1, s);
        emit_init_ref(type_name, s);
        s << endl;
    } else {
        emit_load(T1, 1, T1, s);
    }
    emit_jalr(T1, s);
    emit_pop(ACC, s);
    emit_pop(SELF, s);
}

void isvoid_class::code(ostream &s, Context c) {
}

void no_expr_class::code(ostream &s, Context c) {
}

void object_class::code(ostream &s, Context c) {
    s << "# Load " << name << endl;
    if (*name == *self) {
        emit_move(ACC, SELF, s);
    } else {
        int i;
        if ((i = c.lookup_var(name)) != -1) {
            emit_load(ACC, -i - 1, FP, s);
        } else if ((i = c.lookup_param(name)) != -1) {
            emit_load(ACC, i + 2, FP, s);
        } else {
            i = c.lookup_attr(name);
            assert(i != -1);
            emit_load(ACC, i + DEFAULT_OBJFIELDS, SELF, s);
        }
    }
    s << "# Loaded " << name << endl;
}

