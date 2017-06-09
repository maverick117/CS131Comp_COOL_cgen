
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
#include <map>
#include <stack>
#include <sstream>
#include <vector>
#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

#define INT_TYPEID  2
#define STR_TYPEID  4
#define BOOL_TYPEID 3

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
  copy        = idtable.add_string("copy");
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

//**********************************************************
//
// Compile time environment variables
//
//**********************************************************


// Static integer for labels
static unsigned long label_count = 0;

std::string generate_label(std::string const & s) {
  std::stringstream ss;
  ss << s << "_" << label_count;
  label_count++;
  return ss.str();
}

struct methodPair {
  Symbol className;
  int offset;
  methodPair(Symbol s, int index):className(s),offset(index){}
};

static std::stack<Symbol> classStack;

static std::vector<Symbol> letVars;
static std::map<Symbol, int> argList;
static std::map<Symbol, std::map<Symbol, int> > attrTable;
static std::map<Symbol, std::map<Symbol, methodPair*> > dispTable;
static std::map<Symbol, std::vector<Symbol> > methOrder;

//**********************************************************

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

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address, ostream &s)
{ s << JAL << address << endl; }

static void emit_jal(std::string const & address, ostream &s)
{ s << JAL << address << endl; }

static void emit_jmp(char * address, ostream & s){
  s << JMP << address << endl;
}

static void emit_jmp(std::string const & address, ostream & s){
  s << JMP << address << endl;
}

static void emit_jr(char * address, ostream & s){
  s << JR << address << endl;
}

static void emit_jr(std::string const & address, ostream & s){
  s << JR << address << endl;
}

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

static void emit_label(std::string const & label, ostream & s) {
  s << label << LABEL;
}

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beqz(char * source, std::string const & label, ostream &s) {
  s << BEQZ << source << " " << label << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char * src1, char * src2, std::string const & label, ostream &s) {
  s << BEQ << src1 << " " << src2 << " " << label << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
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
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Pop a stack value into a register
//
static void emit_pop(char * reg, ostream& str) {
  emit_load(reg, 4, SP, str);
  emit_addiu(SP, SP, 4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
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

static void emit_slt(char *dest, char *src1, char *src2, ostream& s)
{ s << SLT << dest << " " << src1 << " " << src2 << endl; }

static void emit_xori(char *dest, char *src1, int imm, ostream& s)
{ s << XORI << dest << " " << src1 << " " << imm << endl; }
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

 /***** Add dispatch information for class Int ******/
      s << 0;
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
  str << endl;
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
   stringclasstag   =   STR_TYPEID  /* Change to your String class tag here */;
   intclasstag      =   INT_TYPEID  /* Change to your Int class tag here */;
   boolclasstag     =   BOOL_TYPEID /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

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
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
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
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
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

void CgenNode::code_prototype(ostream & s) {

  // GC Tag, offset -4 from the 
  s << WORD << -1 << endl;

  // Object tag
  emit_protobj_ref(name, s);
  s << LABEL;
  
  // Emit Class tag
  s << WORD << this->classTag << "\t# Class tag" << endl;

  int attrNum = 0;
  for (int i = features->first(); features->more(i); i = features->next(i)){
    Feature f = features->nth(i);    
    // if f is an attribute
    if (f->is_attr())
      attrNum += 1;
  }

  s << WORD << attrNum + 3 <<  "\t# Class entry size" << endl;
  
  // Dispatch Pointer
  s << WORD << name << "_dispTab" << "\t# Dispatch table pointer" << endl;

  // Emit attributes
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    if (f->is_attr())
      s << WORD << f->get_name() << endl;
  } 

}

void CgenNode::code_dispatchtable(std::map<Symbol, Symbol>& methodList, ostream & s) {
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature current = features->nth(i);
    this->override_func(methodList, current);
  }
  
  if (get_parent() != No_class) {
    get_parentnd()->code_dispatchtable(methodList, s);
  }

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    if(!f->is_attr()){
      if(methodList.find(f->get_name()) != methodList.end()){
        auto fn = methodList.find(f->get_name());
        if(fn->second == name)
          s << WORD << name << METHOD_SEP << f->get_name() << endl;
      }
    }
  }
}

void CgenClassTable::code()
{

  if(cgen_debug) this->str << "# Start of CgenClassTable::code() function.\n";
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  // Push base level into classStack
  classStack.push(No_class);

  // Prototype objects
  List<CgenNode> * l = nds;
  std::stack<CgenNode*> nodestack;
  for (l = nds; l != NULL; l = l->tl()){
    nodestack.push(l->hd());
  }
  int tagnum = 0;
  while(!nodestack.empty()){
    CgenNode * n = nodestack.top();
    n->set_tag(tagnum++);
    nodestack.pop();
  }

  str << "# Start of prototype objects for classes\n";
 
  for(l = nds; l != NULL; l = l->tl()){
    l->hd()->code_prototype(str);
  }

  // Class name tables
  
  str << "# Class name table\n";
  str << "class_nameTab" << LABEL;
  std::stack< std::pair<int,Symbol> > nametblstack;
  for(l = nds; l != NULL; l = l->tl()){
    nametblstack.push(std::pair<int, Symbol>(l->hd()->tag(),l->hd()->name));
  }
  while(!nametblstack.empty()){
    std::pair<int,Symbol>& ent = nametblstack.top();
    str<<WORD << ent.first << endl << WORD << ent.second << endl;
    nametblstack.pop();
  }

  // Class object tables
  std::stack<Symbol> objsblstack;
  str << "# Class object table\n";
  str << "class_objTab" << LABEL;
  for(l = nds; l != NULL; l = l->tl()){
    objsblstack.push(l->hd()->name);
  }
  while(!objsblstack.empty()){
    Symbol & sbl = objsblstack.top();
    str<<WORD << sbl << "_protObj" << endl << WORD << sbl << "_init" << endl;
    objsblstack.pop();
  }

  // Dispatch Tables

  std::map<Symbol, Symbol>  methodList;

  for(l = nds; l != NULL; l = l->tl()){
    str << l->hd()->name << "_dispTab" << LABEL;
    l->hd()->code_dispatchtable(methodList, str);
  }

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  // Object initializers
  for (l = nds; l!= NULL; l = l->tl()) {
    str << "# Initializer for Class " << l->hd()->name << endl;
    std::string className = l->hd()->name->get_string();
    // if (className == Int || className == Bool || className == Str || className == IO || className == Object) continue;
    emit_label(className + std::string(CLASSINIT_SUFFIX), str);
    str << endl;
    emit_addiu(SP, SP, -12, str);
    emit_store(FP, 3, SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, 4, str);
    emit_move(SELF, ACC, str);
    if (className != "Object")
      emit_jal(std::string("Object") + std::string(CLASSINIT_SUFFIX), str);
    emit_move(ACC, SELF, str);
    emit_load(FP, 3, SP, str);
    emit_load(SELF, 2, SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 12, str);
    emit_return(str);
  }

  // Class methods

  if (cgen_debug) cout << "Coding class methods" << endl;

  for (l = nds; l != NULL; l = l->tl()) {
    CgenNode * c = l->hd();
    classStack.push(c->name);
    c->code_methods(str);
    classStack.pop();
  }
  
  str<< "\n# This code is generated by the COOL code generator written by Andrew Jianzhong Liu and Zean Zhou.\n# All rights reserved.\n";

  if (cgen_debug) this->str << "# End of CgenClassTable::code() function.\n";
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

void CgenNode::code_methods(ostream & s) {
  if (basic()) return;
  s << "# Start coding methods for class " << name << endl;

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    Feature f = features->nth(i);
    if(!f->is_attr()){
      s << "# Code start for method " << f->get_name() << endl;
      Formals formals = f->get_formals();
      Symbol returnTyep = f->get_type();
      Expression expr = f->get_init();
     
      // Emit method label
      s << name << METHOD_SEP << f->get_name() << LABEL;

      // Emit function call prologue
      
      emit_addiu(SP, SP, -12, s);
      emit_store(FP, 3, SP, s);
      emit_store(SELF, 2, SP, s);
      emit_store(RA, 1, SP, s);
      emit_addiu(FP, SP, 4, s);
      emit_move(SELF, ACC, s);

      bool former_debug_flag = cgen_debug;
      cgen_debug = true;

      // Evaluate the expression of the function
      expr->code(s);

      cgen_debug = former_debug_flag;

      // Return value is in A0
      // Emit the epilogue
      emit_load(FP, 3, SP, s);
      emit_load(SELF, 2, SP, s);
      emit_load(RA, 1, SP, s);
      emit_addiu(SP, SP, 12, s);

      // Pop all arguments
      int add_amount = 0;
      if(formals != NULL){
        for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
          add_amount += 1;
        }
      }
      if (add_amount) {
        emit_addiu(SP, SP, add_amount, s);
      }

      s << JR << RA << endl;

      s << "# Code end for method " << f->get_name() << endl;
    }
  }

  s << "# Finished coding methods for class " << name << endl;

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

void assign_class::code(ostream &s) {
  // Assign Class Structure
  // Symbol name
  // Expression expr

  if (cgen_debug) s << "# Code start for assign class\n";
  // operational semantics: evaluate expression, get new location from environment, save evaluation to store
  
  // Evaluate expression
  this->expr->code(s);
  // Reference to evaluated expression in ACC
  
  Symbol curClass = classStack.top();
  int offset = 0;

  // See if it is in let
  for (int i = letVars.size() - 1; i >= 0; i--) {
    if(name == letVars[i]) {
      offset = letVars.size() - i;
      emit_store(ACC, offset, SP, s);
      return;
    }
  }

  // See if it is a passed in argument
  if (argList.find(name) != argList.end()){
    offset = argList[name] + 3;
    emit_store(ACC, offset, FP, s);
  }
  else {
    offset = attrTable[curClass][name] + 3;
    emit_store(ACC, offset, SELF, s);
    emit_addiu(A1, SELF, offset*4, s);
    emit_jal("_GenGC_Assign", s);
  }

  if (cgen_debug) s << "# Code end for assign class\n" << endl;
}

void static_dispatch_class::code(ostream &s) {
  // TODO: Complete static dispatch

  if (cgen_debug) s << "# Code start for static_dispatch_class::code()" << endl;  
  // Class structure
  // Expression expr
  // Symbol type_name
  // Symbol name
  // Expressions actual

  int numOfArgs = 0;

  // Evaluate all parameter expressions
  for (int i = this->actual->first(); this->actual->more(i); this->actual->next(i)) {
    this->actual->nth(i)->code(s);
    emit_push(ACC, s);

    // calculate offset
    numOfArgs++;

    // leave space for right offset
    letVars.push_back(No_type);
  }

  // Evaluate expression
  this->expr->code(s);

  // Error detection
  // emit_bne(ACC, ZERO, label_count, s);
  // s << LA << ACC << " " << "str_const0" << endl; // load file name
  // emit_load_imm(T1, 1, s);
  // emit_jal("_dispatch_abort",s);

  // TODO: Enter new scope and assign new environments to passed arguments
  // char* className = this->type_name->get_string();
  // std::string dispLabel = std::string(className) + std::string(DISPTAB_SUFFIX);

  // emit_label_def(label_count, s);
  // emit_load_address(T1, dispLabel, s);
  // emit_load(T1, dispTable[this->type_name][this->name]->offset, T1, s);
  // label_count++;
  // emit_jalr(T1, s);

  for (int i = 0; i < numOfArgs; ++i) {
    letVars.pop_back();
  }

  // Call the function
  s << JAL << type_name << METHOD_SEP << name << endl;

  if (cgen_debug) s << "# Code end for static_dispatch_class::code()" << endl;
}

void dispatch_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for dispatch_class::code()" << endl;

  int numOfArgs = 0;

  // Evaluate all parameter expressions
  for (int i = this->actual->first(); this->actual->more(i); this->actual->next(i)) {
    this->actual->nth(i)->code(s);
    emit_push(ACC, s);

    // calculate offset
    numOfArgs++;

    // leave space for right offset
    letVars.push_back(No_type);
  }

  // Evaluate expression
  this->expr->code(s);
 
  Symbol curClass = classStack.top();
  if (this->expr->get_type() != SELF_TYPE) {
    curClass = expr->get_type();
  }

  s << JAL << curClass->get_string() << METHOD_SEP << name << endl; 
  for(int i = 0; i < numOfArgs; i++){
    letVars.pop_back(); // Pop off corresponding number of arguments
  }
  if (cgen_debug) s << "# Code end for dispatch_class::code()" << endl;
}

void cond_class::code(ostream &s) {
 
  if (cgen_debug) s << "# Code start for cond_class::code()" << endl; 

  // Generate branches for falselabel and finallabel
  std::string falselabel = generate_label("condfalse");
  std::string finallabel = generate_label("condend");

  // Evaluate predicate
  this->pred->code(s);

  // Give label to this conditional jump
  emit_beq(ACC, ZERO, falselabel, s);

  // For true evaluations, evaluate e2
  this->then_exp->code(s);

  // Then jump to final label
  emit_jmp(finallabel, s);
  
  // emit falselabel
  emit_label(falselabel, s);

  // For false evaluations, evaluate e3
  this->else_exp->code(s);

  // Emit final label
  emit_label(finallabel, s);

  if (cgen_debug) s << "# Code end for cond_class::code()" << endl;
}

void loop_class::code(ostream &s) {

  if (cgen_debug) s << "# Code start for loop_class::code()" << endl;

  // Generate tags
  std::string looplabel = generate_label("looplabel");
  std::string endlabel = generate_label("endlabel");

  // Loop tag:
  emit_label(looplabel,s);

  // Evaluate pred
  this->pred->code(s);
  
  // beq acc, zero, end_tag  
  emit_beqz(ACC, endlabel, s);
  // Evaluate e2
  this->body->code(s);

  // Jump back to loop tag
  emit_jmp(looplabel, s);

  // End tag
  emit_label(endlabel, s);

  if (cgen_debug) s << "# Code end for loop_class::code()" << endl;

}

void typcase_class::code(ostream &s) {

  if (cgen_debug) s << "# Code start for typecase_class::code()" << endl;

  // TODO: Finish case expression

  // typcase_class Attributes:
  // Expression expr
  // Cases cases

  // Operational Semantics of case expressions:
  // 1. Evaluate the expression
  // 2. Choose which branch to take
  // 3. Evaluate the expression at that branch
  
  // Evaluate expression 
  expr->code(s);

 

  if (cgen_debug) s << "# Code end for type_case_class::code()" << endl;
}


void block_class::code(ostream &s) {
  // Expression Block operational semantics:
  // Evaluate expression from first to last
  // Return value is the value of the last
  
  if (cgen_debug) s << "# Code start for block_class::code()" << endl;

  for (int i = this->body->first(); this->body->more(i); i = this->body->next(i)) {
    this->body->nth(i)->code(s);
  }

  if (cgen_debug) s << "# Code end for block_class::code()" << endl;
}

void let_class::code(ostream &s) {

  // Evaluate init
  init->code(s);

  // If no initialization, load default values
  if (init->get_type() == NULL) {
    if(type_decl == Int) {
      emit_load_int(ACC, inttable.lookup_string("0"),s);
    }
    else if (type_decl == Str) {
      emit_load_string(ACC, stringtable.lookup_string(""),s);
    }
    else if (type_decl == Bool) {
      emit_load_bool(ACC, BoolConst(false), s);
    }
    else {
      emit_load_imm(ACC, 0, s);
    }
  }

  // Push new reference to evaluated expression to stack
  emit_push(ACC, s);
  // Push identifier so we may use it
  letVars.push_back(identifier);
  // Generate code for the body
  body->code(s);
  // Pop the identifier
  letVars.pop_back();
  // Pop the value from the stack
  emit_addiu(SP, SP, 4, s);
}

enum bin_op {
  OP_PLUS,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_LT,
  OP_EQ,
  OP_LEQ,

};

static void binary_op(bin_op op, Expression e1, Expression e2, ostream& s) {

  // Evaluate e1
  e1->code(s);
  
  // Load the Integer value of the evaluated expression
  emit_load(T1, 3, ACC, s);
  
  letVars.push_back(No_type); // For offset correctness
  // Push e1
  emit_push(T1, s);

  // Evaluate e2
  e2->code(s);

  // Generate a new storage area for the result
  s << JAL;
  emit_method_ref(Object, copy, s);
  s << endl;

  // Currently ACC contains a reference to the copied object

  // Load the value of e2
  emit_load(T3, 3, ACC, s);

  // Load the value of e1 into T2
  emit_pop(T2, s);
  letVars.pop_back();

  // Currently ACC contains the reference to the new object,  
  //           T2 contains the value of e1
  //           T3 contains the value of e2

  // Generate code for specific operator
  switch(op){
  case OP_PLUS:
    emit_add(T1, T2, T3, s);
    break;
  case OP_SUB:
    emit_sub(T1, T2, T3, s);
    break;
  case OP_MUL:
    emit_mul(T1, T2, T3, s);
    break;
  case OP_DIV:
    emit_div(T1, T2, T3, s);
    break;
  default:
    std::cerr << "Not yet implemented!\n";
    exit(1);
  }

  // Store the result of the operation stored in T1 back
  emit_store(T1, 3, ACC, s);

  // Reference already stored at ACC. No need for further actions.
  return;
}

void plus_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for plus class operation\n";
  binary_op(OP_PLUS, e1, e2, s);
  if (cgen_debug) s << "# Code end for plus class operation\n";
}

void sub_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for subtraction class operation\n";
  binary_op(OP_SUB, e1, e2, s);
  if (cgen_debug) s << "# Code end for subtraction class operation\n";
}

void mul_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for multiplication class operation\n";
  binary_op(OP_MUL, e1, e2, s);
  if (cgen_debug) s << "# Code end for multiplication class operation\n";
}

void divide_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for division class operation\n";
  binary_op(OP_DIV, e1, e2, s);
  if (cgen_debug) s << "# Code end for division class operation\n";
}

void neg_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for negate class operation\n";

/*
 * neg operational semantics:
 * 1. Evaluate e1
 * 2. v = Negate e1
 * 3. return v
 */

  // Expression evaluation 
  e1->code(s);

  // Create a new copy of the value of the expression
  s << JAL;
  s << "Object.copy";
  s << endl;

  // Copy the value of the expression into T1
  emit_load(T1, 3, ACC, s);

  // Negate the expression
  emit_neg(T1, T1, s);

  // Store the value of the expression back
  emit_store(T1, 3, ACC, s);
  
  if (cgen_debug) s << "# Code end for negate class operation\n";
}

static void comp_op(bin_op op, Expression e1, Expression e2, ostream &s) {

  // Evaluate e1
  e1->code(s);
  
  // Load value of e1 
  emit_load(T1, 3, ACC, s);

  // Push e1 onto stack
  emit_push(T1, s);

  letVars.push_back(No_type); // For offset correctness

  // Evaluate e2
  e2->code(s);

  // Load value of e2 into T2
  emit_load(T2, 3, ACC, s);

  // Load value of e1 into T2
  emit_pop(T1, s);

  letVars.pop_back();

  // Emit the corresponding instruction of operation
  switch (op) {
  case OP_LT:
    emit_slt(T3, T1, T2, s);
    break;
  case OP_LEQ:
    emit_slt(T3, T1, T2, s);
    emit_xori(T3, T3, 1, s);
    break;
  default:
    std::cerr << "Incorrect operation called using comp_op() function!\n";
    exit(1);
  }

  // Currently T3 contains the result of the operation

  // Emit the labels for conditional operation
  std::string falselabel = generate_label("compare_false");
  std::string endlabel = generate_label("compare_end");

  // Jump to false label if result is zero 
  emit_beqz(T3, falselabel, s); 

  // Otherwise load reference to true into ACC
  emit_load_bool(ACC, truebool, s);

  // Then jump to the end label
  emit_jmp(endlabel, s);

  emit_label(falselabel, s);

  // Load reference to false into ACC
  emit_load_bool(ACC, truebool, s);

  emit_label(endlabel, s);

}

void lt_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for less than class operation\n";
  comp_op(OP_LT, this->e1, this->e2, s);
  if (cgen_debug) s << "# Code end for less than class operation\n";
}

void eq_class::code(ostream &s) {

  if (cgen_debug) s << "# Code start for eq code generation.\n";

  // eq_class layout
  // Expression e1
  // Expression e2

  // Equality comparison operational semantics
  // Evaluate e1 and e2
  // Compare pointer values of e1 and e2
  // If different, compare contents if objects are Int, String or Bool

  // Generate corresponding labels
  std::string eq_label = generate_label("eq");
  std::string eq_end = generate_label("eq_end");
  std::string neq_label = generate_label("not_eq");

  // Evaluate expression e1
  this->e1->code(s);
  
  // Push the pointer into stack
  emit_push(ACC, s);

  letVars.push_back(No_type); // Offset correctness

  // Evaluate expression e2
  this->e2->code(s);

  // Push the pointer into stack
  emit_push(ACC, s);

  letVars.push_back(No_type);

  // Load the pointers into t1 and t2
  emit_load(T2, 1, SP, s);
  emit_load(T1, 2, SP, s);

  // Compare if the pointers are the same
  emit_beq(T1, T2, eq_label,s);

  // Load corresponding values into $a0 and $a1 and call equality_test()
  
  // Load 1 into a0 and 0 into a1
  emit_load_imm(ACC, 1, s);
  emit_load_imm(A1, 0, s);

  // Call equality_test()
  s << JAL << "equality_test" << endl;

  // Test the value in $a0
  emit_beqz(ACC, neq_label, s);
  emit_jmp(eq_label,s);

  // Label for not equal
  emit_label(neq_label, s);

  // Load false into ACC
  emit_load_bool(ACC, falsebool, s);

  // Jump to the end of equality comparison
  emit_jmp(eq_end,s);

  // Label for branching if the pointers are equal
  emit_label(eq_label,s);

  // Load true into ACC
  emit_load_bool(ACC, truebool,s);

  // End of equality comparison
  emit_label(eq_end, s);

  emit_addiu(SP, SP, 8, s);
  letVars.pop_back();
  letVars.pop_back();

  if (cgen_debug) s << "# Code end for eq code generation.\n";

}

void leq_class::code(ostream &s) {
  if (cgen_debug) s << "# Code start for leq class operation\n";
  comp_op(OP_LEQ, this->e1, this->e2, s);
  if (cgen_debug) s << "# Code end for leq class operation\n";
}

void comp_class::code(ostream &s) {
  // comp_class layout:
  // Expression e1
  
  if (cgen_debug) s << "# Start code for comp_class::code() function.\n";

  // Generate labels for conditional operations
  std::string comp_false = generate_label("comp_false");
  std::string comp_end = generate_label("comp_end");

  // Evaluate expression e1
  e1->code(s);

  // Load the Integer/Boolean value into T1
  emit_load(T1, 3, ACC, s);

  // If the value is false/0 then jump to comp_false
  emit_beqz(T1, comp_false, s);

  // Load true into ACC
  emit_load_bool(ACC, truebool, s);

  // Jump to the end
  emit_jmp(comp_end, s);

  // Emit the false label
  emit_label(comp_false, s);
 
  // Load false into ACC 
  emit_load_bool(ACC, falsebool, s);

  // Emit the end label
  emit_label(comp_end, s);
  if (cgen_debug) s << "# End code for comp_class:: code() function.\n";
}

void int_const_class::code(ostream& s)  
{
  s << "# Start of int_const_class::code()" << endl;
  if (cgen_debug) s << "# Code start for int const class operation\n";
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
  if (cgen_debug) s << "# Code end for int const class operation\n";
}

void string_const_class::code(ostream& s)
{
  if (cgen_debug) s << "# Code start for string const class operation\n";
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
  if (cgen_debug) s << "# Code end for string const class operation\n";
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  // new__class class layout:
  // Symbol type_name

  if (cgen_debug) s << "Code start for new__class::code()" << endl;

  // See if type_name is SELF_TYPE
  if (type_name == SELF_TYPE) {
    emit_load_address(T1, "class_objTab", s); // Load class_objTab address
    emit_load(T2, 0, SELF, s); // Load Class tag
    emit_sll(T2, T2, 3, s); // Double word length, four bytes per word
    emit_addu(T1,T1,T2,s); // Get address to pointer to class protObj
    emit_push(T1,s);
    emit_load(ACC, 0, T1, s); // Load proto obj address 
    s << JAL << "Object.copy" << endl; // Create a new copy of the desired object
    // Returns the copied object in ACC
    emit_pop(T1, s); // Pop the obj address back into T1
    emit_load(T1, 1, T1, s); // Load the _init method into T1
    emit_jalr(T1,s); // Call the init method
  }
  else {
    s << LA << ACC << " " << type_name << "_protObj" << endl; // Load the protObj address into ACC
    s << JAL << "Object.copy" << endl; // Create a new copy of the object
    s << JAL << type_name << "_init" << endl; // JAL the init method
  }

  if (cgen_debug) s << "Code end for new__class::code()" << endl;
}

void isvoid_class::code(ostream &s) {
  // Operational Semantics for is_void operations:
  // 1. Evaluate e1
  // 2. If e1 is void, return true
  // 3. If e1 is not void, return false
  
  // is_void_class structure:
  // Expression e1
  
  if(cgen_debug) s << "# Begin evaluation of isvoid procedure\n";

  // Evaluate e1
  this->e1->code(s);

  // Generate related labels
  std::string truelabel = generate_label("isvoid_true");
  std::string endlabel = generate_label("isvoid_endlabel"); 

  // If it is null, jump to true label
  emit_beqz(ACC, truelabel, s);

  // If it is not null, then generate false
  emit_load_bool(ACC, falsebool, s);

  // Then jump to the end of this block
  emit_jmp(endlabel,s);

  // Return true if isvoid
  emit_label(truelabel,s);

  // Load true bool
  emit_load_bool(ACC, truebool, s);

  emit_label(endlabel,s);

  if(cgen_debug) s << "# End evaluation of isvoid procedure\n";
}

void no_expr_class::code(ostream &s) {
  if(cgen_debug) s << "# Start of no_expr_class::code() function\n";
  // Load a null into the acc and return
  emit_load_imm(ACC, 0, s);
  if(cgen_debug) s << "# End of no_expr_class::code() function\n";
}

void object_class::code(ostream &s) {
  // object_class layout:
  // Symbol name

  int offset = 0;
  Symbol curClass = classStack.top();

  if (cgen_debug) s << "# Start of object_class::code() function\n";
  if (name == self) { // Self object
    emit_move(ACC, SELF, s); // Move self pointer to ACC
  }
  else{
    // declared in a let expression
    for(int i = letVars.size() - 1; i >= 0; i--) {
      if (name == letVars[i]) {
        offset = letVars.size() - i;
        emit_load(ACC, offset, SP, s);
        return;
      }
    }
    // Passed in argument
    if (argList.find(name) != argList.end()) {
      offset = argList.size() - 1 - argList[name] + 3;
      emit_load(ACC, offset, FP, s);
    }
    // Class attributes
    else {
      offset = attrTable[curClass][name] + 3;
      emit_load(ACC, offset, SELF, s);
    }
  }
  if (cgen_debug) s << "# End of object_class::code() function\n";
}



