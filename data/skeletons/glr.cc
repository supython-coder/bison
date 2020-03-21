# C++ GLR skeleton for Bison

# Copyright (C) 2002-2015, 2018-2019 Free Software Foundation, Inc.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# This skeleton produces a C++ class that encapsulates a C glr parser.
# This is in order to reduce the maintenance burden.  The glr.c
# skeleton is clean and pure enough so that there are no real
# problems.  The C++ interface is the same as that of lalr1.cc.  In
# fact, glr.c can replace yacc.c without the user noticing any
# difference, and similarly for glr.cc replacing lalr1.cc.
#
# The passing of parse-params
#
#   The additional arguments are stored as members of the parser
#   object, yyparser.  The C routines need to carry yyparser
#   throughout the C parser; that's easy: make yyparser an
#   additional parse-param.  But because the C++ skeleton needs to
#   know the "real" original parse-param, we save them
#   (b4_parse_param_orig).  Note that b4_parse_param is overquoted
#   (and c.m4 strips one level of quotes).  This is a PITA, and
#   explains why there are so many levels of quotes.
#
# The locations
#
#   We use location.cc just like lalr1.cc, but because glr.c stores
#   the locations in a union, the position and location classes
#   must not have a constructor.  Therefore, contrary to lalr1.cc, we
#   must not define "b4_location_constructors".  As a consequence the
#   user must initialize the first positions (in particular the
#   filename member).

# We require a pure interface.
m4_define([b4_pure_flag], [1])

m4_include(b4_skeletonsdir/[c++.m4])
b4_bison_locations_if([m4_include(b4_skeletonsdir/[location.cc])])

m4_define([b4_parser_class],
          [b4_percent_define_get([[api.parser.class]])])

# Save the parse parameters.
m4_define([b4_parse_param_orig], m4_defn([b4_parse_param]))

# b4_parse_param_wrap
# -------------------
# New ones.
m4_ifset([b4_parse_param],
[m4_define([b4_parse_param_wrap],
           [[b4_namespace_ref::b4_parser_class[& yyparser], [[yyparser]]],]
m4_defn([b4_parse_param]))],
[m4_define([b4_parse_param_wrap],
           [[b4_namespace_ref::b4_parser_class[& yyparser], [[yyparser]]]])
])


# b4_yy_symbol_print_define
# -------------------------
# Bypass the default implementation to generate the "yy_symbol_print"
# and "yy_symbol_value_print" functions.
m4_define([b4_yy_symbol_print_define],
[[
/*--------------------.
| Print this symbol.  |
`--------------------*/

]b4_function_define([yy_symbol_print],
    [static void],
    [[FILE *],      []],
    [[int yytype],  [yytype]],
    [[const ]b4_namespace_ref::b4_parser_class[::semantic_type *yyvaluep],
                    [yyvaluep]][]dnl
b4_locations_if([,
    [[const ]b4_namespace_ref::b4_parser_class[::location_type *yylocationp],
                    [yylocationp]]]),
    b4_parse_param)[
{
]b4_parse_param_use[]dnl
[  yyparser.yy_symbol_print_ (yytype, yyvaluep]b4_locations_if([, yylocationp])[);
}
]])[

# Hijack the initial action to initialize the locations.
]b4_bison_locations_if([m4_define([b4_initial_action],
[yylloc.initialize ();]m4_ifdef([b4_initial_action], [
m4_defn([b4_initial_action])]))])[

# Hijack the post prologue to declare yyerror.
]m4_append([b4_post_prologue],
[b4_syncline([@oline@], [@ofile@])[
]b4_function_declare([yyerror],
    [static void],b4_locations_if([
    [[const ]b4_namespace_ref::b4_parser_class[::location_type *yylocationp],
                        [yylocationp]],])
    b4_parse_param,
    [[const char* msg], [msg]])])[


#undef yynerrs
#undef yychar
#undef yylval]b4_locations_if([
#undef yylloc])

m4_if(b4_prefix, [yy], [],
[[/* Substitute the variable and function names.  */
#define yyparse ]b4_prefix[parse
#define yylex   ]b4_prefix[lex
#define yyerror ]b4_prefix[error
#define yydebug ]b4_prefix[debug]]b4_pure_if([], [[
#define yylval  ]b4_prefix[lval
#define yychar  ]b4_prefix[char
#define yynerrs ]b4_prefix[nerrs]b4_locations_if([[
#define yylloc  ]b4_prefix[lloc]])]))

# Hijack the epilogue to define implementations (yyerror, parser member
# functions etc.).
m4_append([b4_epilogue],
[b4_syncline([@oline@], [@ofile@])[

/*------------------.
| Report an error.  |
`------------------*/

]b4_function_define([yyerror],
    [static void],b4_locations_if([
    [[const ]b4_namespace_ref::b4_parser_class[::location_type *yylocationp],
                        [yylocationp]],])
    b4_parse_param,
    [[const char* msg], [msg]])[
{
]b4_parse_param_use[]dnl
[  yyparser.error (]b4_locations_if([[*yylocationp, ]])[msg);
}


]b4_namespace_open[
]dnl In this section, the parse params are the original parse_params.
m4_pushdef([b4_parse_param], m4_defn([b4_parse_param_orig]))dnl
[  /// Build a parser object.
  ]b4_parser_class::b4_parser_class[ (]b4_parse_param_decl[)]m4_ifset([b4_parse_param], [
    :])[
#if ]b4_api_PREFIX[DEBUG
    ]m4_ifset([b4_parse_param], [  ], [ :])[yycdebug_ (&std::cerr)]m4_ifset([b4_parse_param], [,])[
#endif]b4_parse_param_cons[
  {}

  ]b4_parser_class::~b4_parser_class[ ()
  {}

  ]b4_parser_class[::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  int
  ]b4_parser_class[::operator() ()
  {
    return parse ();
  }

  int
  ]b4_parser_class[::parse ()
  {
    return ::yyparse (*this]b4_user_args[);
  }

#if ]b4_api_PREFIX[DEBUG
  /*--------------------.
  | Print this symbol.  |
  `--------------------*/

  void
  ]b4_parser_class[::yy_symbol_value_print_ (int yytype,
                           const semantic_type* yyvaluep]b4_locations_if([[,
                           const location_type* yylocationp]])[)
  {]b4_locations_if([[
    YYUSE (yylocationp);]])[
    YYUSE (yyvaluep);
    std::ostream& yyo = debug_stream ();
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    ]b4_symbol_actions([printer])[
  }


  void
  ]b4_parser_class[::yy_symbol_print_ (int yytype,
                           const semantic_type* yyvaluep]b4_locations_if([[,
                           const location_type* yylocationp]])[)
  {
    *yycdebug_ << (yytype < YYNTOKENS ? "token" : "nterm")
               << ' ' << yytname[yytype] << " ("]b4_locations_if([[
               << *yylocationp << ": "]])[;
    yy_symbol_value_print_ (yytype, yyvaluep]b4_locations_if([[, yylocationp]])[);
    *yycdebug_ << ')';
  }

  std::ostream&
  ]b4_parser_class[::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  ]b4_parser_class[::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  ]b4_parser_class[::debug_level_type
  ]b4_parser_class[::debug_level () const
  {
    return yydebug;
  }

  void
  ]b4_parser_class[::set_debug_level (debug_level_type l)
  {
    // Actually, it is yydebug which is really used.
    yydebug = l;
  }

#endif
]m4_popdef([b4_parse_param])dnl
b4_namespace_close
])


# b4_shared_declarations(hh|cc)
# -----------------------------
# Declaration that might either go into the header (if --defines, $1 = hh)
# or in the implementation file.
m4_define([b4_shared_declarations],
[m4_pushdef([b4_parse_param], m4_defn([b4_parse_param_orig]))dnl
b4_percent_code_get([[requires]])[
#include <algorithm>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

template <typename Parameter>
class StrongIndexAlias
{
 public:
  static StrongIndexAlias create(size_t value) {
    StrongIndexAlias result;
    result.value_ = value;
    return result;
  }

  size_t const& get() const {return value_; }

  StrongIndexAlias operator+(size_t other) const {
    return StrongIndexAlias(get() + other);
  }

  void operator+=(size_t other) {
    value_ += other;
  }

  StrongIndexAlias operator-(size_t other) {
    return StrongIndexAlias(get() - other);
  }

  void operator-=(size_t other) {
    value_ -= other;
  }

  size_t operator-(StrongIndexAlias other) {
    return StrongIndexAlias(get() - other.get());
  }

  StrongIndexAlias& operator++() {
    ++value_;
    return *this;
  }

  bool isValid() const {
    return value_ != INVALID_INDEX;
  }

  void setInvalid() {
    value_ = INVALID_INDEX;
  }

  bool operator==(StrongIndexAlias other) {
    return get() == other.get();
  }

  bool operator!=(StrongIndexAlias other) {
    return get() != other.get();
  }

  bool operator<(StrongIndexAlias other) {
    return get() < other.get();
  }

 private:
  static const size_t INVALID_INDEX = ((size_t) -1);

  // WARNING: 0-initialized.
  size_t value_;
};

]b4_cxx_portability[
]m4_ifdef([b4_location_include],
          [[# include ]b4_location_include])[
]b4_variant_if([b4_variant_includes])[

]b4_attribute_define[
]b4_null_define[

// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif

]b4_YYDEBUG_define[

]b4_namespace_open[

]b4_bison_locations_if([m4_ifndef([b4_location_file],
                                  [b4_location_define])])[

  /// A Bison parser.
  class ]b4_parser_class[
  {
  public:
]b4_public_types_declare[

    /// Build a parser object.
    ]b4_parser_class[ (]b4_parse_param_decl[);
    virtual ~]b4_parser_class[ ();

    /// Parse.  An alias for parse ().
    /// \returns  0 iff parsing succeeded.
    int operator() ();

    /// Parse.
    /// \returns  0 iff parsing succeeded.
    virtual int parse ();

#if ]b4_api_PREFIX[DEBUG
    /// The current debugging stream.
    std::ostream& debug_stream () const;
    /// Set the current debugging stream.
    void set_debug_stream (std::ostream &);

    /// Type for debugging levels.
    typedef int debug_level_type;
    /// The current debugging level.
    debug_level_type debug_level () const;
    /// Set the current debugging level.
    void set_debug_level (debug_level_type l);
#endif

    /// Report a syntax error.]b4_locations_if([[
    /// \param loc    where the syntax error is found.]])[
    /// \param msg    a description of the syntax error.
    virtual void error (]b4_locations_if([[const location_type& loc, ]])[const std::string& msg);

# if ]b4_api_PREFIX[DEBUG
  public:
    /// \brief Report a symbol value on the debug stream.
    /// \param yytype       The token type.
    /// \param yyvaluep     Its semantic value.]b4_locations_if([[
    /// \param yylocationp  Its location.]])[
    virtual void yy_symbol_value_print_ (int yytype,
                                         const semantic_type* yyvaluep]b4_locations_if([[,
                                         const location_type* yylocationp]])[);
    /// \brief Report a symbol on the debug stream.
    /// \param yytype       The token type.
    /// \param yyvaluep     Its semantic value.]b4_locations_if([[
    /// \param yylocationp  Its location.]])[
    virtual void yy_symbol_print_ (int yytype,
                                   const semantic_type* yyvaluep]b4_locations_if([[,
                                   const location_type* yylocationp]])[);
  private:
    // Debugging.
    std::ostream* yycdebug_;
#endif

]b4_parse_param_vars[
  };

]dnl Redirections for glr.c.
b4_percent_define_flag_if([[global_tokens_and_yystype]],
[b4_token_defines])
[
#ifndef ]b4_api_PREFIX[STYPE
# define ]b4_api_PREFIX[STYPE ]b4_namespace_ref[::]b4_parser_class[::semantic_type
#endif
#ifndef ]b4_api_PREFIX[LTYPE
# define ]b4_api_PREFIX[LTYPE ]b4_namespace_ref[::]b4_parser_class[::location_type
#endif

]b4_namespace_close[
]b4_percent_code_get([[provides]])[
]m4_popdef([b4_parse_param])dnl
])

b4_defines_if(
[b4_output_begin([b4_spec_defines_file])
b4_copyright([Skeleton interface for Bison GLR parsers in C++],
             [2002-2015, 2018-2019])[
// C++ GLR parser skeleton written by Akim Demaille.

]b4_disclaimer[
]b4_cpp_guard_open([b4_spec_defines_file])[
]b4_shared_declarations[
]b4_cpp_guard_close([b4_spec_defines_file])[
]b4_output_end])

# Let glr.c (and b4_shared_declarations) believe that the user
# arguments include the parser itself.
m4_pushdef([b4_parse_param], m4_defn([b4_parse_param_wrap]))

## ---------------- ##
## Default values.  ##
## ---------------- ##

# Stack parameters.
m4_define_default([b4_stack_depth_max], [10000])
m4_define_default([b4_stack_depth_init],  [200])



## ------------------------ ##
## Pure/impure interfaces.  ##
## ------------------------ ##

b4_define_flag_if([pure])

# b4_user_formals
# ---------------
# The possible parse-params formal arguments preceded by a comma.
#
# This is not shared with yacc.c in c.m4 because  GLR relies on ISO C
# formal argument declarations.
m4_define([b4_user_formals],
[m4_ifset([b4_parse_param], [, b4_user_formals_no_comma])])

# b4_user_formals_no_comma
# ---------------
# The possible parse-params formal arguments.
m4_define([b4_user_formals_no_comma],
[m4_ifset([b4_parse_param], [b4_formals(b4_parse_param)])])

# b4_yyerror_args
# ---------------
# Optional effective arguments passed to yyerror: user args plus yylloc, and
# a trailing comma.
m4_define([b4_yyerror_args],
[b4_pure_if([b4_locations_if([yylocp, ])])dnl
m4_ifset([b4_parse_param], [b4_args(b4_parse_param), ])])


# b4_lyyerror_args
# ----------------
# Same as above, but on the lookahead, hence &yylloc instead of yylocp.
m4_define([b4_lyyerror_args],
[b4_pure_if([b4_locations_if([&yylloc, ])])dnl
m4_ifset([b4_parse_param], [b4_args(b4_parse_param), ])])


# b4_pure_args
# ------------
# Same as b4_yyerror_args, but with a leading comma.
m4_define([b4_pure_args],
[b4_pure_if([b4_locations_if([, yylocp])])[]b4_user_args])


# b4_lpure_args
# -------------
# Same as above, but on the lookahead, hence &yylloc instead of yylocp.
m4_define([b4_lpure_args],
[b4_pure_if([b4_locations_if([, &yylloc])])[]b4_user_args])



# b4_pure_formals
# ---------------
# Arguments passed to yyerror: user formals plus yylocp with leading comma.
m4_define([b4_pure_formals],
[b4_pure_if([b4_locations_if([, YYLTYPE *yylocp])])[]b4_user_formals])


# b4_locuser_formals(LOC = yylocp)
# --------------------------------
# User formal arguments, possibly preceded by location argument.
m4_define([b4_locuser_formals],
[b4_locations_if([, YYLTYPE *m4_default([$1], [yylocp])])[]b4_user_formals])


# b4_locuser_args(LOC = yylocp)
# -----------------------------
m4_define([b4_locuser_args],
[b4_locations_if([, m4_default([$1], [yylocp])])[]b4_user_args])



## ----------------- ##
## Semantic Values.  ##
## ----------------- ##


# b4_lhs_value(SYMBOL-NUM, [TYPE])
# --------------------------------
# See README.
m4_define([b4_lhs_value],
[b4_symbol_value([(*yyvalp)], [$1], [$2])])


# b4_rhs_data(RULE-LENGTH, POS)
# -----------------------------
# See README.
m4_define([b4_rhs_data],
[((yyGLRStackItem const *)yyvsp)@{YYFILL (b4_subtract([$2], [$1]))@}.yystate])


# b4_rhs_value(RULE-LENGTH, POS, SYMBOL-NUM, [TYPE])
# --------------------------------------------------
# Expansion of $$ or $<TYPE>$, for symbol SYMBOL-NUM.
m4_define([b4_rhs_value],
[b4_symbol_value([b4_rhs_data([$1], [$2]).yysemantics.yysval], [$3], [$4])])



## ----------- ##
## Locations.  ##
## ----------- ##

# b4_lhs_location()
# -----------------
# Expansion of @$.
m4_define([b4_lhs_location],
[(*yylocp)])


# b4_rhs_location(RULE-LENGTH, NUM)
# ---------------------------------
# Expansion of @NUM, where the current rule has RULE-LENGTH symbols
# on RHS.
m4_define([b4_rhs_location],
[(b4_rhs_data([$1], [$2]).yyloc)])


## -------------- ##
## Output files.  ##
## -------------- ##

# Unfortunately the order of generation between the header and the
# implementation file matters (for glr.c) because of the current
# implementation of api.value.type=union.  In that case we still use a
# union for YYSTYPE, but we generate the contents of this union when
# setting up YYSTYPE.  This is needed for other aspects, such as
# defining yy_symbol_value_print, since we need to now the name of the
# members of this union.
#
# To avoid this issue, just generate the header before the
# implementation file.  But we should also make them more independant.


# ------------------------- #
# The implementation file.  #
# ------------------------- #

b4_output_begin([b4_parser_file_name])
b4_copyright([Skeleton implementation for Bison GLR parsers in C],
             [2002-2015, 2018-2019])[
/* C GLR parser skeleton written by Paul Hilfinger.  */

]b4_disclaimer[
]b4_identification[

]b4_percent_code_get([[top]])[
]m4_if(b4_api_prefix, [yy], [],
[[/* Substitute the type names.  */
#define YYSTYPE ]b4_api_PREFIX[STYPE]b4_locations_if([[
#define YYLTYPE ]b4_api_PREFIX[LTYPE]])])[
]m4_if(b4_prefix, [yy], [],
[[/* Substitute the variable and function names.  */
#define yyparse ]b4_prefix[parse
#define yylex   ]b4_prefix[lex
#define yyerror ]b4_prefix[error
#define yydebug ]b4_prefix[debug]]b4_pure_if([], [[
#define yylval  ]b4_prefix[lval
#define yychar  ]b4_prefix[char
#define yynerrs ]b4_prefix[nerrs]b4_locations_if([[
#define yylloc  ]b4_prefix[lloc]])]))[

]b4_user_pre_prologue[

]b4_null_define[

]b4_defines_if([[#include "@basename(]b4_spec_defines_file[@)"]],
               [b4_shared_declarations])[

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE ]b4_error_verbose_if([1], [0])[
#endif

/* Default (constant) value used for initialization for null
   right-hand sides.  Unlike the standard yacc.c template, here we set
   the default value of $$ to a zeroed-out value.  Since the default
   value is undefined, this behavior is technically correct.  */
static YYSTYPE yyval_default;]b4_locations_if([[
static YYLTYPE yyloc_default][]b4_yyloc_default;])[

]b4_user_post_prologue[
]b4_percent_code_get[]dnl

[#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YYFREE
# define YYFREE free
#endif
#ifndef YYMALLOC
# define YYMALLOC malloc
#endif

#define YYSIZEMAX ((size_t) -1)

#ifndef YYSETJMP
# include <setjmp.h>
# define YYJMP_BUF jmp_buf
# define YYSETJMP(Env) setjmp (Env)
/* Pacify Clang and ICC.  */
# define YYLONGJMP(Env, Val)                    \
 do {                                           \
   longjmp (Env, Val);                          \
   YYASSERT (0);                                \
 } while (false)
#endif

]b4_attribute_define([noreturn])[

#ifndef YYASSERT
# define YYASSERT(Condition) ((void) ((Condition) || (abort (), 0)))
#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  ]b4_final_state_number[
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   ]b4_last[

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  ]b4_tokens_number[
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  ]b4_nterms_number[
/* YYNRULES -- Number of rules.  */
#define YYNRULES  ]b4_rules_number[
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  ]b4_states_number[
/* YYMAXRHS -- Maximum number of symbols on right-hand side of rule.  */
#define YYMAXRHS ]b4_r2_max[
/* YYMAXLEFT -- Maximum number of symbols to the left of a handle
   accessed by $0, $-1, etc., in any rule.  */
#define YYMAXLEFT ]b4_max_left_semantic_context[

/* YYMAXUTOK -- Last valid token number (for yychar).  */
#define YYMAXUTOK   ]b4_user_token_number_max[
/* YYFAULTYTOK -- Token number (for yychar) that denotes a
   syntax_error thrown from the scanner.  */
#define YYFAULTYTOK (YYMAXUTOK + 1)
/* YYUNDEFTOK -- Symbol number (for yytoken) that denotes an unknown
   token.  */
#define YYUNDEFTOK  ]b4_undef_token_number[

/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  ((unsigned) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const ]b4_int_type_for([b4_translate])[ yytranslate[] =
{
  ]b4_translate[
};

#if ]b4_api_PREFIX[DEBUG
/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const ]b4_int_type_for([b4_rline])[ yyrline[] =
{
  ]b4_rline[
};
#endif

#if ]b4_api_PREFIX[DEBUG || YYERROR_VERBOSE || ]b4_token_table_flag[
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  ]b4_tname[
};
#endif

#define YYPACT_NINF ]b4_pact_ninf[
#define YYTABLE_NINF ]b4_table_ninf[

]b4_parser_tables_define[

/* YYDPREC[RULE-NUM] -- Dynamic precedence of rule #RULE-NUM (0 if none).  */
static const ]b4_int_type_for([b4_dprec])[ yydprec[] =
{
  ]b4_dprec[
};

/* YYMERGER[RULE-NUM] -- Index of merging function for rule #RULE-NUM.  */
static const ]b4_int_type_for([b4_merger])[ yymerger[] =
{
  ]b4_merger[
};

/* YYIMMEDIATE[RULE-NUM] -- True iff rule #RULE-NUM is not to be deferred, as
   in the case of predicates.  */
static const bool yyimmediate[] =
{
  ]b4_immediate[
};

/* YYCONFLP[YYPACT[STATE-NUM]] -- Pointer into YYCONFL of start of
   list of conflicting reductions corresponding to action entry for
   state STATE-NUM in yytable.  0 means no conflicts.  The list in
   yyconfl is terminated by a rule number of 0.  */
static const ]b4_int_type_for([b4_conflict_list_heads])[ yyconflp[] =
{
  ]b4_conflict_list_heads[
};

/* YYCONFL[I] -- lists of conflicting rule numbers, each terminated by
   0, pointed into by YYCONFLP.  */
]dnl Do not use b4_int_type_for here, since there are places where
dnl pointers onto yyconfl are taken, whose type is "short*".
dnl We probably ought to introduce a type for confl.
[static const short yyconfl[] =
{
  ]b4_conflicting_rules[
};

/* Error token number */
#define YYTERROR 1

]b4_locations_if([[
]b4_yylloc_default_define[
# define YYRHSLOC(Rhs, K) ((Rhs)[K].yystate.yyloc)
]])[

]b4_pure_if(
[
#undef yynerrs
#define yynerrs (yystackp->yyerrcnt)
#undef yychar
#define yychar (yystackp->yyrawchar)
#undef yylval
#define yylval (yystackp->yyval)
#undef yylloc
#define yylloc (yystackp->yyloc)
m4_if(b4_prefix[], [yy], [],
[#define b4_prefix[]nerrs yynerrs
#define b4_prefix[]char yychar
#define b4_prefix[]lval yylval
#define b4_prefix[]lloc yylloc])],
[YYSTYPE yylval;]b4_locations_if([[
YYLTYPE yylloc;]])[

int yynerrs;
int yychar;])[

static const int YYEOF = 0;
static const int YYEMPTY = -2;

typedef enum { yyok, yyaccept, yyabort, yyerr } YYRESULTTAG;

#define YYCHK(YYE)                              \
  do {                                          \
    YYRESULTTAG yychk_flag = YYE;               \
    if (yychk_flag != yyok)                     \
      return yychk_flag;                        \
  } while (0)

#if ]b4_api_PREFIX[DEBUG

# ifndef YYFPRINTF
#  define YYFPRINTF fprintf
# endif

]b4_yy_location_print_define[

# define YYDPRINTF(Args)                        \
  do {                                          \
    if (yydebug)                                \
      YYFPRINTF Args;                           \
  } while (0)

]b4_yy_symbol_print_define[

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                  \
  do {                                                                  \
    if (yydebug)                                                        \
      {                                                                 \
        YYFPRINTF (stderr, "%s ", Title);                               \
        yy_symbol_print (stderr, Type, Value]b4_locuser_args([Location])[);        \
        YYFPRINTF (stderr, "\n");                                       \
      }                                                                 \
  } while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;

struct yyGLRStack;
static void yypstack (struct yyGLRStack* yystackp, size_t yyk)
  YY_ATTRIBUTE_UNUSED;
static void yypdumpstack (struct yyGLRStack* yystackp)
  YY_ATTRIBUTE_UNUSED;

#else /* !]b4_api_PREFIX[DEBUG */

# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)

#endif /* !]b4_api_PREFIX[DEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH ]b4_stack_depth_init[
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYMAXDEPTH * sizeof (GLRStackItem)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH ]b4_stack_depth_max[
#endif

/* Minimum number of free items on the stack allowed after an
   allocation.  This is to allow allocation and initialization
   to be completed by functions that call yyexpandGLRStack before the
   stack is expanded, thus insuring that all necessary pointers get
   properly redirected to new data.  */
#define YYHEADROOM 2

#ifndef YYSTACKEXPANDABLE
#  define YYSTACKEXPANDABLE 1
#endif

#if YYERROR_VERBOSE

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
template <bool YYCOPY>
static size_t
yytnamerr_count (char *yyres, const char *yystr)
{
  size_t yyn = 0;
  char const *yyp = yystr;

  for (;;)
    switch (*++yyp)
      {
      case '\'':
      case ',':
        return -1;

      case '\\':
        if (*++yyp != '\\')
          return -1;
        // fall-through;

      default:
        if (YYCOPY)
          yyres[yyn] = *yyp;
        yyn++;
        break;

      case '"':
        if (YYCOPY)
          yyres[yyn] = '\0';
        return yyn;
      }
  // Unreachable.
  return -1;
}

/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static size_t
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      size_t count = (yyres == YY_NULLPTR)
        ? yytnamerr_count<true>(yyres, yystr)
        : yytnamerr_count<false>(yyres, yystr);
      if (count != -1) return count;
    }

  if (yyres == YY_NULLPTR)
    return strlen (yystr);

  return (size_t) (yystpcpy (yyres, yystr) - yyres);
}
# endif

#endif /* !YYERROR_VERBOSE */

/** State numbers, as in LALR(1) machine */
typedef int yyStateNum;

/** Rule numbers, as in LALR(1) machine */
typedef int yyRuleNum;

/** Grammar symbol */
typedef int yySymbol;

/** Item references, as in LALR(1) machine */
typedef short yyItemNum;

// Forward declarations.
struct yyGLRState;
struct yySemanticOption;
struct yyGLRStackItem;
struct yyGLRStack;

typedef StrongIndexAlias<struct yySemanticOptionTag> yySemanticOptionIndex;

yySemanticOptionIndex yycreateOptionIndex(size_t value) {
  return yySemanticOptionIndex::create(value);
}

typedef StrongIndexAlias<struct yyStateTag> yyStateIndex;

yyStateIndex yycreateStateIndex(size_t value) {
  return yyStateIndex::create(value);
}

yyStateIndex yycreateInvalidStateIndex() {
  yyStateIndex res;
  res.setInvalid();
  return res;
}

typedef StrongIndexAlias<struct yyGLRStateSetTag> yyStateSetIndex;

yyStateSetIndex yycreateStateSetIndex(size_t value) {
  return yyStateSetIndex::create(value);
}

#define yypact_value_is_default(Yystate) \
  ]b4_table_value_equals([[pact]], [[Yystate]], [b4_pact_ninf])[

#define yytable_value_is_error(Yytable_value) \
  ]b4_table_value_equals([[table]], [[Yytable_value]], [b4_table_ninf])[

#if ]b4_api_PREFIX[DEBUG || YYERROR_VERBOSE
static inline const char*
yytokenName (yySymbol yytoken);
#endif

]b4_yydestruct_define[

]m4_define([b4_yygetToken_call],
           [[yygetToken (&yychar][]b4_pure_if([, yystackp])[]b4_user_args[)]])[

static inline yySymbol
yygetToken (int *yycharp][]b4_pure_if([, yyGLRStack* yystackp])[]b4_user_formals[);

static inline bool
yyisShiftAction (int yyaction)
{
  return 0 < yyaction;
}

static inline bool
yyisErrorAction (int yyaction)
{
  return yyaction == 0;
}

static inline int
yygetLRActions (yyStateNum yystate, yySymbol yytoken, const short** yyconflicts);

/** True iff LR state YYSTATE has only a default reduction (regardless
 *  of token).  */
static inline bool
yyisDefaultedState (yyStateNum yystate)
{
  return yypact_value_is_default (yypact[yystate]);
}

/** The default reduction for YYSTATE, assuming it has one.  */
static inline yyRuleNum
yydefaultAction (yyStateNum yystate)
{
  return yydefact[yystate];
}


struct yyGLRState {
  /** Type tag for yysemantics.  If true, yysval applies, otherwise
   *  yyfirstValIndex applies.  */
  bool yyresolved;
  /** Number of corresponding LALR(1) machine state.  */
  yyStateNum yylrState;
  /** Preceding state in this stack */
  yyStateIndex yypredIndex;
  /** Source position of the last token produced by my symbol */
  size_t yyposn;
  union {
    /** First in a chain of alternative reductions producing the
     *  nonterminal corresponding to this state, threaded through
     *  yynextIndex.  */
    yySemanticOptionIndex yyfirstValIndex;
    /** Semantic value for this state.  */
    YYSTYPE yysval;
  } yysemantics;]b4_locations_if([[
  /** Source location for this state.  */
  YYLTYPE yyloc;]])[
};

/** A stack of GLRState representing the different heads during
  * nondeterministic evaluation. */
class yyGLRStateSet {
 public:
  /** Initialize YYSET to a singleton set containing an empty stack.  */
  yyGLRStateSet()
  {
    yylastDeleted.setInvalid();
    yystates.push_back(yycreateInvalidStateIndex());
    yylookaheadNeeds.push_back(false);
  }

  // Behave like a vector of states.
  yyStateIndex& operator[](yyStateSetIndex index) {
    return yystates[index.get()];
  }

  yyStateIndex operator[](yyStateSetIndex index) const {
    return yystates[index.get()];
  }

  size_t size() const {
    return yystates.size();
  }

  std::vector<yyStateIndex>::iterator begin() {
    return yystates.begin();
  }

  std::vector<yyStateIndex>::iterator end() {
    return yystates.end();
  }


  bool lookaheadNeeds(yyStateSetIndex index) const {
    return yylookaheadNeeds[index.get()];
  }

  bool setLookaheadNeeds(yyStateSetIndex index, bool value) {
    return yylookaheadNeeds[index.get()] = value;
  }

  /** Invalidate stack #YYK.  */
  inline void
  yymarkStackDeleted (yyStateSetIndex yyk)
  {
    size_t k = yyk.get();
    if (yystates[k].isValid())
      yylastDeleted = yystates[k];
    yystates[k].setInvalid();
  }

  /** Undelete the last stack in *this that was marked as deleted.  Can
      only be done once after a deletion, and only when all other stacks have
      been deleted.  */
  void
  yyundeleteLastStack ()
  {
    if (!yylastDeleted.isValid() || !yystates.empty())
      return;
    yystates.push_back(yylastDeleted);
    YYDPRINTF ((stderr, "Restoring last deleted stack as stack #0.\n"));
    yylastDeleted.setInvalid();
  }

  /** Remove the dead stacks (yystates[i] == YY_NULLPTR) and shift the later
   * ones.  */
  inline void
  yyremoveDeletes ()
  {
    size_t newsize = yystates.size();
    /* j is the number of live stacks we have seen.  */
    for (size_t i = 0, j = 0; i < yystates.size(); ++i)
      {
        if (!yystates[i].isValid())
          {
            if (i == j)
              {
                YYDPRINTF ((stderr, "Removing dead stacks.\n"));
              }
            newsize -= 1;
          }
        else
          {
            yystates[j] = yystates[i];
            /* In the current implementation, it's unnecessary to copy
               yylookaheadNeeds[i] since, after
               yyremoveDeletes returns, the parser immediately either enters
               deterministic operation or shifts a token.  However, it doesn't
               hurt, and the code might evolve to need it.  */
            yylookaheadNeeds[j] = yylookaheadNeeds[i];
            if (j != i)
              {
                YYDPRINTF ((stderr, "Rename stack %lu -> %lu.\n",
                            (unsigned long) i, (unsigned long) j));
              }
            j += 1;
          }
        i += 1;
      }
    yystates.erase(yystates.begin() + newsize, yystates.end());
    yylookaheadNeeds.erase(yylookaheadNeeds.begin() + newsize,
                           yylookaheadNeeds.end());
  }


  yyStateSetIndex
  yysplitStack (yyStateSetIndex yyk)
  {
    size_t k = yyk.get();
    yystates.push_back(yystates[k]);
    yylookaheadNeeds.push_back(yylookaheadNeeds[k]);
    return yycreateStateSetIndex(yystates.size() - 1);
  }

  void clearLastDeleted() {
    yylastDeleted.setInvalid();
  }

 private:

  std::vector<yyStateIndex> yystates;
  /** During nondeterministic operation, yylookaheadNeeds tracks which
   *  stacks have actually needed the current lookahead.  During deterministic
   *  operation, yylookaheadNeeds[0] is not maintained since it would merely
   *  duplicate yychar != YYEMPTY.  */
  std::vector<bool> yylookaheadNeeds;

  /** The last stack we invalidated.  */
  yyStateIndex yylastDeleted;

  static const size_t INITIAL_NUMBER_STATES = 16;
};

struct yySemanticOption {
  /** Rule number for this reduction */
  yyRuleNum yyrule;
  /** The last RHS state in the list of states to be reduced.  */
  yyStateIndex yystateIndex;
  /** The lookahead for this reduction.  */
  int yyrawchar;
  YYSTYPE yyval;]b4_locations_if([[
  YYLTYPE yyloc;]])[
  /** Next sibling in chain of options.  To facilitate merging,
   *  options are chained in decreasing order by address.  */
  yySemanticOptionIndex yynextIndex;
};

/** Type of the items in the GLR stack.  The yyisState field
 *  indicates which item of the union is valid.  */
struct yyGLRStackItem {
  union {
    yyGLRState yystate;
    yySemanticOption yyoption;
  };
#if ]b4_api_PREFIX[DEBUG
  /** Type tag for the union. */
  bool yyisState;
#endif
};


static inline int
yyrhsLength (yyRuleNum yyrule);

static int
yypreference (yySemanticOption* y0, yySemanticOption* y1);

static void
yyuserMerge (int yyn, YYSTYPE* yy0, YYSTYPE* yy1);


/** Left-hand-side symbol for rule #YYRULE.  */
static inline yySymbol
yylhsNonterm (yyRuleNum yyrule)
{
  return yyr1[yyrule];
}

static inline yyStateNum
yyLRgotoState (yyStateNum yystate, yySymbol yysym);

#undef YYFILL
#define YYFILL(N) yyfill (yyvsp, &yylow, (N), yynormal)

struct yyStateStack {
 public:
  /** Initialize to a single empty stack, with total maximum
   *  capacity for all stacks of YYSIZE.  */
  yyStateStack (size_t yysize)
  {
    yyitems.reserve(yysize);
    yysplitPoint.setInvalid();
  }

#if YYSTACKEXPANDABLE
  /** Returns false if it tried to expand but could not. */
  bool
  yyexpandGLRStackIfNeeded() {
    return spaceLeft() < YYHEADROOM && yyexpandGLRStack();
  }

 private:
  /** If *this is expandable, extend it.  WARNING: Pointers into the
      stack from outside should be considered invalid after this call.
      We always expand when there are 1 or fewer items left AFTER an
      allocation, so that we can avoid having external pointers exist
      across an allocation.  */
  bool
  yyexpandGLRStack()
  {
    if (YYMAXDEPTH - YYHEADROOM < yyitems.size())
      return false;
    const size_t yynewSize = YYMAXDEPTH < 2 * yyitems.size() ? YYMAXDEPTH : 2 * yyitems.size();
    yyitems.reserve(yynewSize);
    return true;
  }
 public:
#else
  bool yyexpandGLRStackIfNeeded() {
    return yystateStack.spaceLeft() < YYHEADROOM;
  }
#endif

  static bool yyGLRStateIndexValid(yyStateIndex s) {
    return s.isValid();
  }

  bool
  reduceToOneStack() {
    const std::vector<yyStateIndex>::iterator begin =
      yytops.begin();
    const std::vector<yyStateIndex>::iterator end =
      yytops.end();
    std::vector<yyStateIndex>::iterator yyit =
      std::find_if(begin, end, yyGLRStateIndexValid);
    if (yyit == end)
      return false;
    for (yyStateSetIndex yyk = yycreateStateSetIndex(yyit + 1 - begin);
         yyk.get() != numTops(); ++yyk)
      yytops.yymarkStackDeleted (yyk);
    yytops.yyremoveDeletes ();
    yycompressStack ();
    return true;
  }

  void
  yycompressStack ()
  {
    if (yytops.size() != 1 || !isSplit())
      return;

    yyStateIndex yyr = yycreateInvalidStateIndex();
    for (yyStateIndex yyp = firstTop(), yyq = stateAt(yyp).yypredIndex;
         yyp != yysplitPoint;
         yyr = yyp, yyp = yyq, yyq = stateAt(yyp).yypredIndex)
      stateAt(yyp).yypredIndex = yyr;

    size_t yysize = yysplitPoint.get() + 1;
    yysplitPoint.setInvalid();
    yytops.clearLastDeleted();

    while (yyr.isValid())
      {
        yyGLRState& nextFreeState = stateAt(yycreateStateIndex(yysize));
        nextFreeState = stateAt(yyr);
        yyr = stateAt(yyr).yypredIndex;
        nextFreeState.yypredIndex = yycreateStateIndex(yysize - 1);
        setFirstTop(yycreateStateIndex(yysize));
        yysize += 1;
      }
    yyitems.erase(yyitems.begin() + yysize, yyitems.end());
  }

  void
  yydestroyGLRState (char const *yymsg,
                     yyGLRState *yys]b4_user_formals[)
  {
    if (yys->yyresolved)
      yydestruct (yymsg, yystos[yys->yylrState],
                  &yys->yysemantics.yysval]b4_locuser_args([&yys->yyloc])[);
    else
      {
#if ]b4_api_PREFIX[DEBUG
        if (yydebug)
          {
            if (yys->yysemantics.yyfirstValIndex.isValid())
              YYFPRINTF (stderr, "%s unresolved", yymsg);
            else
              YYFPRINTF (stderr, "%s incomplete", yymsg);
            YY_SYMBOL_PRINT ("", yystos[yys->yylrState], YY_NULLPTR, &yys->yyloc);
          }
#endif

        if (yys->yysemantics.yyfirstValIndex.isValid())
          {
            yySemanticOption *yyoption = &optionAt(yys->yysemantics.yyfirstValIndex);
            yyGLRState *yyrh;
            int yyn;
            for (yyrh = &stateAt(yyoption->yystateIndex), yyn = yyrhsLength (yyoption->yyrule);
                 yyn > 0;
                 yyrh = predState(yyrh), yyn -= 1)
              yydestroyGLRState (yymsg, yyrh]b4_user_args[);
          }
      }
  }


  bool isSplit() const {
    return yysplitPoint.isValid();
  }

  // Present the interface of a vector of yyGLRStackItem.
  std::vector<yyGLRStackItem>::const_iterator begin() const {
    return yyitems.begin();
  }

  std::vector<yyGLRStackItem>::const_iterator end() const {
    return yyitems.end();
  }

  size_t size() const {
    return yyitems.size();
  }

  yyGLRStackItem& operator[](size_t i) {
    return yyitems[i];
  }

  yySemanticOption& optionAt(yySemanticOptionIndex i) {
    YYASSERT(i.isValid());
    yyGLRStackItem& state = operator[](i.get());
#if ]b4_api_PREFIX[DEBUG
    YYASSERT (!state.yyisState);
#endif
    return state.yyoption;
  }

  yyGLRState& stateAt(yyStateIndex i) {
    YYASSERT(i.isValid());
    yyGLRStackItem& state = operator[](i.get());
#if ]b4_api_PREFIX[DEBUG
    YYASSERT (state.yyisState);
#endif
    return state.yystate;
  }

  yyGLRStackItem& stackItemAt(size_t index) {
    return yyitems[index];
  }

  yyGLRState* predState(yyGLRState* s) {
    return &stateAt(s->yypredIndex);
  }

  size_t numTops() const {
    return yytops.size();
  }

  yyStateIndex firstTop() {
    return yytops[yycreateStateSetIndex(0)];
  }

  yyStateIndex topAt(yyStateSetIndex i) {
    return yytops[i];
  }

  void setFirstTop(yyStateIndex value) {
    yytops[yycreateStateSetIndex(0)] = value;
  }

  void setTopAt(yyStateSetIndex i, yyStateIndex value) {
    yytops[i] = value;
  }

  void pop_back() {
    yyitems.pop_back();
  }

  void pop_back(size_t n) {
    yyitems.resize(yyitems.size() - n);
  }

  yyStateSetIndex
  yysplitStack (yyStateSetIndex yyk)
  {
    if (!isSplit())
      {
        YYASSERT (yyk.get() == 0);
        yysplitPoint = topAt(yyk);
      }
    return yytops.yysplitStack(yyk);
  }

  /** Assuming that YYS is a GLRState somewhere on *this, update the
   *  splitpoint of *this, if needed, so that it is at least as deep as
   *  YYS.  */
  inline void
  yyupdateSplit (yyStateIndex yys)
  {
    if (isSplit() && yys < yysplitPoint)
      yysplitPoint = yys;
  }

  /** Return a fresh GLRState.
   * Callers should call yyreserveStack afterwards to make sure there is
   * sufficient headroom.  */
  yyStateIndex yynewGLRState() {
    return yycreateStateIndex(yynewGLRStackItem(true));
  }

  /** Return a fresh SemanticOption.
   * Callers should call yyreserveStack afterwards to make sure there is
   * sufficient headroom.  */
  yySemanticOptionIndex yynewSemanticOption() {
    return yycreateOptionIndex(yynewGLRStackItem(false));
  }

  /* Do nothing if YYNORMAL or if *YYLOW <= YYLOW1.  Otherwise, fill in
   * YYVSP[YYLOW1 .. *YYLOW-1] as in yyfillin and set *YYLOW = YYLOW1.
   * For convenience, always return YYLOW1.  */
  inline int
  yyfill (yyGLRStackItem *yyvsp, int *yylow, int yylow1, bool yynormal)
  {
    if (!yynormal && yylow1 < *yylow)
      {
        yyfillin (yyvsp, *yylow, yylow1);
        *yylow = yylow1;
      }
    return yylow1;
  }

  /** Fill in YYVSP[YYLOW1 .. YYLOW0-1] from the chain of states starting
   *  at YYVSP[YYLOW0].yystate.yypredIndex.  Leaves YYVSP[YYLOW1].yystate.yypred
   *  containing the pointer to the next state in the chain.  */
  void
  yyfillin (yyGLRStackItem *yyvsp, int yylow0, int yylow1)
  {
    int i;
    yyGLRState *s = &stateAt(yyvsp[yylow0].yystate.yypredIndex);
    for (i = yylow0-1; i >= yylow1; i -= 1)
      {
#if ]b4_api_PREFIX[DEBUG
        yyvsp[i].yystate.yylrState = s->yylrState;
#endif
        yyvsp[i].yystate.yyresolved = s->yyresolved;
        if (s->yyresolved)
          yyvsp[i].yystate.yysemantics.yysval = s->yysemantics.yysval;
        else
          /* The effect of using yysval or yyloc (in an immediate rule) is
           * undefined.  */
          yyvsp[i].yystate.yysemantics.yyfirstValIndex.setInvalid();]b4_locations_if([[
        yyvsp[i].yystate.yyloc = s->yyloc;]])[
        yyvsp[i].yystate.yypredIndex = s->yypredIndex;
        s = predState(s);
      }
  }

#if !]b4_api_PREFIX[DEBUG
# define YY_REDUCE_PRINT(Args)
#else
# define YY_REDUCE_PRINT(Args)          \
  do {                                  \
    if (yydebug)                        \
      yystateStack.yy_reduce_print Args;             \
  } while (0)

  /*----------------------------------------------------------------------.
  | Report that stack #YYK of *YYSTACKP is going to be reduced by YYRULE. |
  `----------------------------------------------------------------------*/

  inline void
  yy_reduce_print (bool yynormal, yyGLRStackItem* yyvsp, yyStateSetIndex yyk,
                   yyRuleNum yyrule]b4_user_formals[)
  {
    int yynrhs = yyrhsLength (yyrule);]b4_locations_if([
    int yylow = 1;])[
    int yyi;
    YYFPRINTF (stderr, "Reducing stack %lu by rule %d (line %lu):\n",
               (unsigned long) yyk.get(), yyrule - 1,
               (unsigned long) yyrline[yyrule]);
    if (! yynormal)
      yyfillin (yyvsp, 1, -yynrhs);
    /* The symbols being reduced.  */
    for (yyi = 0; yyi < yynrhs; yyi++)
      {
        YYFPRINTF (stderr, "   $%d = ", yyi + 1);
        yy_symbol_print (stderr,
                         yystos[yyvsp[yyi - yynrhs + 1].yystate.yylrState],
                         &yyvsp[yyi - yynrhs + 1].yystate.yysemantics.yysval]b4_locations_if([,
                         &]b4_rhs_location(yynrhs, yyi + 1))[]dnl
                         b4_user_args[);
        if (!yyvsp[yyi - yynrhs + 1].yystate.yyresolved)
          YYFPRINTF (stderr, " (unresolved)");
        YYFPRINTF (stderr, "\n");
      }
  }

#endif

  /** True iff YYY0 and YYY1 represent identical options at the top level.
   *  That is, they represent the same rule applied to RHS symbols
   *  that produce the same terminal symbols.  */
  bool
  yyidenticalOptions (yySemanticOption* yyy0, yySemanticOption* yyy1)
  {
    if (yyy0->yyrule == yyy1->yyrule)
      {
        yyGLRState *yys0, *yys1;
        int yyn;
        for (yys0 = &stateAt(yyy0->yystateIndex),
             yys1 = &stateAt(yyy1->yystateIndex),
             yyn = yyrhsLength (yyy0->yyrule);
             yyn > 0;
             yys0 = predState(yys0),
             yys1 = predState(yys1), yyn -= 1)
          if (yys0->yyposn != yys1->yyposn)
            return false;
        return true;
      }
    else
      return false;
  }

  /** Assuming identicalOptions (YYY0,YYY1), destructively merge the
   *  alternative semantic values for the RHS-symbols of YYY1 and YYY0.  */
  void
  yymergeOptionSets (yySemanticOption* yyy0, yySemanticOption* yyy1)
  {
    yyGLRState *yys0, *yys1;
    int yyn;
    for (yys0 = &stateAt(yyy0->yystateIndex),
         yys1 = &stateAt(yyy1->yystateIndex),
         yyn = yyrhsLength (yyy0->yyrule);
         yyn > 0;
         yys0 = predState(yys0),
         yys1 = predState(yys1), yyn -= 1)
      {
        if (yys0 == yys1)
          break;
        else if (yys0->yyresolved)
          {
            yys1->yyresolved = true;
            yys1->yysemantics.yysval = yys0->yysemantics.yysval;
          }
        else if (yys1->yyresolved)
          {
            yys0->yyresolved = true;
            yys0->yysemantics.yysval = yys1->yysemantics.yysval;
          }
        else
          {
            yySemanticOptionIndex* yyz0p = &yys0->yysemantics.yyfirstValIndex;
            yySemanticOptionIndex yyz1 = yys1->yysemantics.yyfirstValIndex;
            while (true)
              {
                if (yyz1 == *yyz0p || !yyz1.isValid())
                  break;
                else if (!yyz0p->isValid())
                  {
                    *yyz0p = yyz1;
                    break;
                  }
                else if (*yyz0p < yyz1)
                  {
                    yySemanticOptionIndex yyz = *yyz0p;
                    *yyz0p = yyz1;
                    yyz1 = optionAt(yyz1).yynextIndex;
                    optionAt(*yyz0p).yynextIndex = yyz;
                  }
                yyz0p = &optionAt(*yyz0p).yynextIndex;
              }
            yys1->yysemantics.yyfirstValIndex = yys0->yysemantics.yyfirstValIndex;
          }
      }
  }

  YYRESULTTAG
  yyreportAmbiguity (yySemanticOption* yyx0,
                     yySemanticOption* yyx1]b4_pure_formals[)
  {
    YYUSE (yyx0);
    YYUSE (yyx1);

#if ]b4_api_PREFIX[DEBUG
    YYFPRINTF (stderr, "Ambiguity detected.\n");
    YYFPRINTF (stderr, "Option 1,\n");
    yyreportTree (yyx0, 2);
    YYFPRINTF (stderr, "\nOption 2,\n");
    yyreportTree (yyx1, 2);
    YYFPRINTF (stderr, "\n");
#endif

    yyerror (]b4_yyerror_args[YY_("syntax is ambiguous"));
    return yyabort;
  }

  /* DEBUGGING ONLY */
#if ]b4_api_PREFIX[DEBUG
  void
  yy_yypstack (yyGLRState* yys)
  {
    if (yys->yypredIndex.isValid())
      {
        yy_yypstack (predState(yys));
        YYFPRINTF (stderr, " -> ");
      }
    YYFPRINTF (stderr, "%d@@%lu", yys->yylrState,
               (unsigned long) yys->yyposn);
  }

  void
  yypstates (yyStateIndex yyst)
  {
    if (yyst.isValid())
      yy_yypstack (&stateAt(yyst));
    else
      YYFPRINTF (stderr, "<null>");
    YYFPRINTF (stderr, "\n");
  }
#endif

 private:
  size_t spaceLeft() const {
    return yyitems.capacity() - yyitems.size();
  }

  /** Return a fresh GLRStackItem in this.  The item is an LR state
   *  if YYISSTATE, and otherwise a semantic option.  Callers should call
   *  yyreserveStack afterwards to make sure there is sufficient
   *  headroom.  */
  inline size_t
  yynewGLRStackItem (bool yyisState)
  {
    YYASSERT(yyitems.size() < yyitems.capacity());
    yyitems.push_back(yyGLRStackItem());
#if ]b4_api_PREFIX[DEBUG
    yyitems.back().yyisState = yyisState;
#endif
    return yyitems.size() - 1;
  }


#if ]b4_api_PREFIX[DEBUG
  void
  yyreportTree (yySemanticOption* yyx, int yyindent)
  {
    int yynrhs = yyrhsLength (yyx->yyrule);
    int yyi;
    yyStateIndex yys;
    yyGLRState* yystates[1 + YYMAXRHS];
    yyGLRState yyleftmost_state;

    for (yyi = yynrhs, yys = yyx->yystateIndex; 0 < yyi; yyi -= 1, yys = stateAt(yys).yypredIndex)
      yystates[yyi] = &stateAt(yys);
    if (!yys.isValid())
      {
        yyleftmost_state.yyposn = 0;
        yystates[0] = &yyleftmost_state;
      }
    else
      yystates[0] = &stateAt(yys);

    if (stateAt(yyx->yystateIndex).yyposn < yystates[0]->yyposn + 1)
      YYFPRINTF (stderr, "%*s%s -> <Rule %d, empty>\n",
                 yyindent, "", yytokenName (yylhsNonterm (yyx->yyrule)),
                 yyx->yyrule - 1);
    else
      YYFPRINTF (stderr, "%*s%s -> <Rule %d, tokens %lu .. %lu>\n",
                 yyindent, "", yytokenName (yylhsNonterm (yyx->yyrule)),
                 yyx->yyrule - 1, (unsigned long) (yystates[0]->yyposn + 1),
                 (unsigned long) stateAt(yyx->yystateIndex).yyposn);
    for (yyi = 1; yyi <= yynrhs; yyi += 1)
      {
        if (yystates[yyi]->yyresolved)
          {
            if (yystates[yyi-1]->yyposn+1 > yystates[yyi]->yyposn)
              YYFPRINTF (stderr, "%*s%s <empty>\n", yyindent+2, "",
                         yytokenName (yystos[yystates[yyi]->yylrState]));
            else
              YYFPRINTF (stderr, "%*s%s <tokens %lu .. %lu>\n", yyindent+2, "",
                         yytokenName (yystos[yystates[yyi]->yylrState]),
                         (unsigned long) (yystates[yyi-1]->yyposn + 1),
                         (unsigned long) yystates[yyi]->yyposn);
          }
        else
          yyreportTree (&optionAt(yystates[yyi]->yysemantics.yyfirstValIndex),
                        yyindent+2);
      }
  }
#endif

 public:

  std::vector<yyGLRStackItem> yyitems;
  yyStateIndex yysplitPoint;
  yyGLRStateSet yytops;
};

#undef YYFILL
#define YYFILL(N) yystateStack.yyfill (yyvsp, &yylow, (N), yynormal)

#define yystackp this
struct yyGLRStack {

  yyGLRStack(size_t yysize, ]b4_parse_param_decl[) :
    yyerrState(0),
    yyerrcnt(0),
    yyrawchar(0),
    yystateStack(yysize),]b4_parse_param_cons[
  {}

  ~yyGLRStack ()
  {
    if (yychar != YYEMPTY)
      yydestruct ("Cleanup: discarding lookahead",
                  YYTRANSLATE (yychar), &yylval]b4_locuser_args([&yylloc])[);
    popall();
  }

  int yyerrState;
]b4_locations_if([[  /* To compute the location of the error token.  */
  yyGLRStackItem yyerror_range[3];]])[
]b4_pure_if(
[
  yyStateStack yystateStack;
  int yyerrcnt;
  int yyrawchar;
  YYSTYPE yyval;]b4_locations_if([[
  YYLTYPE yyloc;]])[
])[
  YYJMP_BUF yyexception_buffer;

  void yyreserveGlrStack() {
    if (yystateStack.yyexpandGLRStackIfNeeded ())
      yyMemoryExhausted();
  }

  _Noreturn void
  yyMemoryExhausted ()
  {
    YYLONGJMP (yyexception_buffer, 2);
  }

  _Noreturn void
  yyFail (const char* yymsg]b4_pure_formals[)
  {
    if (yymsg != YY_NULLPTR)
      yyerror (]b4_yyerror_args[yymsg);
    YYLONGJMP (yyexception_buffer, 1);
  }

                                /* GLRStates */


  /** Add a new semantic action that will execute the action for rule
   *  YYRULE on the semantic values in YYRHS to the list of
   *  alternative actions for YYSTATE.  Assumes that YYRHS comes from
   *  stack #YYK of *this. */
  void
  yyaddDeferredAction (yyStateSetIndex yyk, yyGLRState* yystate,
                       yyStateIndex yyrhs, yyRuleNum yyrule)
  {
    yySemanticOptionIndex yynewIndex = yystateStack.yynewSemanticOption();
    yySemanticOption& yynewOption = optionAt(yynewIndex);
    yynewOption.yystateIndex = yyrhs;
    yynewOption.yyrule = yyrule;
    if (yystateStack.yytops.lookaheadNeeds(yyk))
      {
        yynewOption.yyrawchar = yychar;
        yynewOption.yyval = yylval;]b4_locations_if([
        yynewOption.yyloc = yylloc;])[
      }
    else
      yynewOption.yyrawchar = YYEMPTY;
    yynewOption.yynextIndex = yystate->yysemantics.yyfirstValIndex;
    yystate->yysemantics.yyfirstValIndex = yynewIndex;

    yyreserveGlrStack();
  }


#if ]b4_api_PREFIX[DEBUG

  void
  yypdumpstack ()
  {
    for (size_t yyi = 0; yyi < yystateStack.size(); ++yyi)
      {
        const yyGLRStackItem& item = yystateStack[yyi];
        YYFPRINTF (stderr, "%3lu. ",
                   (unsigned long) yyi);
        if (item.yyisState)
          {
            YYFPRINTF (stderr, "Res: %d, LR State: %d, posn: %lu, pred: %ld",
                       item.yystate.yyresolved, item.yystate.yylrState,
                       (unsigned long) item.yystate.yyposn,
                       (long) item.yystate.yypredIndex.get());
            if (! item.yystate.yyresolved)
              YYFPRINTF (stderr, ", firstVal: %ld",
                         (long) item.yystate.yysemantics.yyfirstValIndex.get());
          }
        else
          {
            YYFPRINTF (stderr, "Option. rule: %d, state: %ld, next: %ld",
                       item.yyoption.yyrule - 1,
                       (long) item.yyoption.yystateIndex.get(),
                       (long) item.yyoption.yynextIndex.get());
          }
        YYFPRINTF (stderr, "\n");
      }
    YYFPRINTF (stderr, "Tops:");
    for (size_t yyi = 0; yyi < yystateStack.numTops(); ++yyi) {
      YYFPRINTF (stderr, "%lu: %ld; ",
                 (unsigned long) (yyi),
                 (long) yystateStack.topAt(yycreateStateSetIndex(yyi)).get());
    }
    YYFPRINTF (stderr, "\n");
  }
#endif

  void
  yyreportSyntaxError (]b4_user_formals_no_comma[)
  {
    if (yyerrState != 0)
      return;
#if ! YYERROR_VERBOSE
    yyerror (]b4_lyyerror_args[YY_("syntax error"));
#else
    {
    yySymbol yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);
    size_t yysize0 = yytnamerr (YY_NULLPTR, yytokenName (yytoken));
    size_t yysize = yysize0;
    bool yysize_overflow = false;
    char* yymsg = YY_NULLPTR;
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    /* Internationalized format string. */
    const char *yyformat = YY_NULLPTR;
    /* Arguments of yyformat. */
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
    /* Number of reported tokens (one for the "unexpected", one per
       "expected").  */
    int yycount = 0;

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yychar) is if
         this state is a consistent state with a default action.  Thus,
         detecting the absence of a lookahead is sufficient to determine
         that there is no unexpected or expected token to report.  In that
         case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is a
         consistent state with a default action.  There might have been a
         previous inconsistent state, consistent state with a non-default
         action, or user semantic action that manipulated yychar.
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state merging
         (from LALR or IELR) and default reductions corrupt the expected
         token list.  However, the list is correct for canonical LR with
         one exception: it will still contain any token that will not be
         accepted due to an error action in a later state.
    */
    if (yytoken != YYEMPTY)
      {
        int yyn = yypact[firstTopState().yylrState];
        yyarg[yycount++] = yytokenName (yytoken);
        if (!yypact_value_is_default (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for this
               state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            /* Stay within bounds of both yycheck and yytname.  */
            int yychecklim = YYLAST - yyn + 1;
            int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
            int yyx;
            for (yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                  && !yytable_value_is_error (yytable[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      yysize = yysize0;
                      break;
                    }
                  yyarg[yycount++] = yytokenName (yyx);
                  {
                    size_t yysz = yysize + yytnamerr (YY_NULLPTR, yytokenName (yyx));
                    if (yysz < yysize)
                      yysize_overflow = true;
                    yysize = yysz;
                  }
                }
          }
      }

    switch (yycount)
      {
#define YYCASE_(N, S)                   \
        case N:                           \
          yyformat = S;                   \
        break
      default: /* Avoid compiler warnings. */
        YYCASE_(0, YY_("syntax error"));
        YYCASE_(1, YY_("syntax error, unexpected %s"));
        YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    {
      size_t yysz = yysize + strlen (yyformat);
      if (yysz < yysize)
        yysize_overflow = true;
      yysize = yysz;
    }

    if (!yysize_overflow)
      yymsg = (char *) YYMALLOC (yysize);

    if (yymsg)
      {
        char *yyp = yymsg;
        int yyi = 0;
        while ((*yyp = *yyformat))
          {
            if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
              {
                yyp += yytnamerr (yyp, yyarg[yyi++]);
                yyformat += 2;
              }
            else
              {
                yyp++;
                yyformat++;
              }
          }
        yyerror (]b4_lyyerror_args[yymsg);
        YYFREE (yymsg);
      }
    else
      {
        yyerror (]b4_lyyerror_args[YY_("syntax error"));
        yyMemoryExhausted();
      }
    }
#endif /* YYERROR_VERBOSE */
    yynerrs += 1;
  }


  /* Recover from a syntax error on this, assuming that YYTOKENP,
     yylval, and yylloc are the syntactic category, semantic value, and location
     of the lookahead.  */
  void
  yyrecoverSyntaxError (]b4_user_formals_no_comma[)
  {
    if (yyerrState == 3)
      /* We just shifted the error token and (perhaps) took some
         reductions.  Skip tokens until we can proceed.  */
      while (true)
        {
          yySymbol yytoken;
          int yyj;
          if (yychar == YYEOF)
            yyFail (YY_NULLPTR][]b4_lpure_args[);
          if (yychar != YYEMPTY)
            {]b4_locations_if([[
              /* We throw away the lookahead, but the error range
                 of the shifted error token must take it into account.  */
              yyGLRState *yys = &firstTopState();
              yyGLRStackItem yyerror_range[3];
              yyerror_range[1].yystate.yyloc = yys->yyloc;
              yyerror_range[2].yystate.yyloc = yylloc;
              YYLLOC_DEFAULT ((yys->yyloc), yyerror_range, 2);]])[
              yytoken = YYTRANSLATE (yychar);
              yydestruct ("Error: discarding",
                          yytoken, &yylval]b4_locuser_args([&yylloc])[);
              yychar = YYEMPTY;
            }
          yytoken = ]b4_yygetToken_call[;
          yyj = yypact[firstTopState().yylrState];
          if (yypact_value_is_default (yyj))
            return;
          yyj += yytoken;
          if (yyj < 0 || YYLAST < yyj || yycheck[yyj] != yytoken)
            {
              if (yydefact[firstTopState().yylrState] != 0)
                return;
            }
          else if (! yytable_value_is_error (yytable[yyj]))
            return;
        }

    if (!yystateStack.reduceToOneStack())
      yyFail (YY_NULLPTR][]b4_lpure_args[);

    /* Now pop stack until we find a state that shifts the error token.  */
    yyerrState = 3;
    while (yystateStack.firstTop().isValid())
      {
        yyGLRState *yys = &firstTopState();
        int yyj = yypact[yys->yylrState];
        if (! yypact_value_is_default (yyj))
          {
            yyj += YYTERROR;
            if (0 <= yyj && yyj <= YYLAST && yycheck[yyj] == YYTERROR
                && yyisShiftAction (yytable[yyj]))
              {
                /* Shift the error token.  */]b4_locations_if([[
                /* First adjust its location.*/
                YYLTYPE yyerrloc;
                yyerror_range[2].yystate.yyloc = yylloc;
                YYLLOC_DEFAULT (yyerrloc, (yyerror_range), 2);]])[
                YY_SYMBOL_PRINT ("Shifting", yystos[yytable[yyj]],
                                 &yylval, &yyerrloc);
                yyglrShift (yycreateStateSetIndex(0), yytable[yyj],
                            yys->yyposn, &yylval]b4_locations_if([, &yyerrloc])[);
                yys = &firstTopState();
                break;
              }
          }]b4_locations_if([[
        yyerror_range[1].yystate.yyloc = yys->yyloc;]])[
        if (yys->yypredIndex.isValid())
          yystateStack.yydestroyGLRState ("Error: popping", yys]b4_user_args[);
        yystateStack.setFirstTop(yys->yypredIndex);
        yystateStack.pop_back();
      }
    if (!yystateStack.firstTop().isValid())
      yyFail (YY_NULLPTR][]b4_lpure_args[);
  }

  YYRESULTTAG
  yyprocessOneStack (yyStateSetIndex yyk,
                     size_t yyposn]b4_pure_formals[)
  {
    while (yystateStack.topAt(yyk).isValid())
      {
        yyStateNum yystate = topState(yyk).yylrState;
        YYDPRINTF ((stderr, "Stack %lu Entering state %d\n",
                    (unsigned long) yyk.get(), yystate));

        YYASSERT (yystate != YYFINAL);

        if (yyisDefaultedState (yystate))
          {
            yyRuleNum yyrule = yydefaultAction (yystate);
            if (yyrule == 0)
              {
                YYDPRINTF ((stderr, "Stack %lu dies.\n",
                            (unsigned long) yyk.get()));
                yystateStack.yytops.yymarkStackDeleted (yyk);
                return yyok;
              }
            YYRESULTTAG yyflag = yyglrReduce (yyk, yyrule,
                                              yyimmediate[yyrule]]b4_user_args[);
            if (yyflag == yyerr)
              {
                YYDPRINTF ((stderr,
                            "Stack %lu dies "
                            "(predicate failure or explicit user error).\n",
                            (unsigned long) yyk.get()));
                yystateStack.yytops.yymarkStackDeleted (yyk);
                return yyok;
              }
            if (yyflag != yyok)
              return yyflag;
          }
        else
          {
            yystateStack.yytops.setLookaheadNeeds(yyk, true);
            yySymbol yytoken = ]b4_yygetToken_call[;
            const short* yyconflicts;
            int yyaction = yygetLRActions (yystate, yytoken, &yyconflicts);

            for (; *yyconflicts != 0; ++yyconflicts)
              {
                yyStateSetIndex yynewStack = yystateStack.yysplitStack (yyk);
                YYDPRINTF ((stderr, "Splitting off stack %lu from %lu.\n",
                            (unsigned long) yynewStack.get(),
                            (unsigned long) yyk.get()));
                YYRESULTTAG yyflag =
                  yyglrReduce (yynewStack, *yyconflicts,
                               yyimmediate[*yyconflicts]]b4_user_args[);
                if (yyflag == yyok)
                  YYCHK (yyprocessOneStack (yynewStack,
                                            yyposn]b4_pure_args[));
                else if (yyflag == yyerr)
                  {
                    YYDPRINTF ((stderr, "Stack %lu dies.\n",
                                (unsigned long) yynewStack.get()));
                    yystateStack.yytops.yymarkStackDeleted (yynewStack);
                  }
                else
                  return yyflag;
              }

            if (yyisShiftAction (yyaction))
              break;
            else if (yyisErrorAction (yyaction))
              {
                YYDPRINTF ((stderr, "Stack %lu dies.\n",
                            (unsigned long) yyk.get()));
                yystateStack.yytops.yymarkStackDeleted (yyk);
                break;
              }
            else
              {
                YYRESULTTAG yyflag = yyglrReduce (yyk, -yyaction,
                                                  yyimmediate[-yyaction]]b4_user_args[);
                if (yyflag == yyerr)
                  {
                    YYDPRINTF ((stderr,
                                "Stack %lu dies "
                                "(predicate failure or explicit user error).\n",
                                (unsigned long) yyk.get()));
                    yystateStack.yytops.yymarkStackDeleted (yyk);
                    break;
                  }
                else if (yyflag != yyok)
                  return yyflag;
              }
          }
      }
    return yyok;
  }

  /** Perform user action for rule number YYN, with RHS length YYRHSLEN,
   *  and top stack item YYVSP.  YYVALP points to place to put semantic
   *  value ($$), and yylocp points to place for location information
   *  (@@$).  Returns yyok for normal return, yyaccept for YYACCEPT,
   *  yyerr for YYERROR, yyabort for YYABORT.  */
  YYRESULTTAG
  yyuserAction (yyRuleNum yyn, int yyrhslen, yyGLRStackItem* yyvsp,
                YYSTYPE* yyvalp]b4_locuser_formals[)
  {
    bool yynormal YY_ATTRIBUTE_UNUSED = !yystateStack.isSplit();
    int yylow;
  ]b4_parse_param_use([yyvalp], [yylocp])dnl
  [  YYUSE (yyrhslen);
  # undef yyerrok
  # define yyerrok (yyerrState = 0)
  # undef YYACCEPT
  # define YYACCEPT return yyaccept
  # undef YYABORT
  # define YYABORT return yyabort
  # undef YYERROR
  # define YYERROR return yyerrok, yyerr
  # undef YYRECOVERING
  # define YYRECOVERING() (yyerrState != 0)
  # undef yyclearin
  # define yyclearin (yychar = YYEMPTY)
  # undef YYBACKUP
  # define YYBACKUP(Token, Value)                                              \
    return yyerror (]b4_yyerror_args[YY_("syntax error: cannot back up")),     \
           yyerrok, yyerr

    yylow = 1;
    if (yyrhslen == 0)
      *yyvalp = yyval_default;
    else
      *yyvalp = yyvsp[YYFILL (1-yyrhslen)].yystate.yysemantics.yysval;]b4_locations_if([[
    /* Default location. */
    YYLLOC_DEFAULT ((*yylocp), (yyvsp - yyrhslen), yyrhslen);
    yyerror_range[1].yystate.yyloc = *yylocp;
  ]])[
  #if YY_EXCEPTIONS
    typedef ]b4_namespace_ref[::]b4_parser_class[::syntax_error syntax_error;
    try
    {
  #endif // YY_EXCEPTIONS
    switch (yyn)
      {
  ]b4_user_actions[
        default: break;
      }
  #if YY_EXCEPTIONS
    }
    catch (const syntax_error& yyexc)
      {
        YYDPRINTF ((stderr, "Caught exception: %s\n", yyexc.what()));]b4_locations_if([
        *yylocp = yyexc.location;])[
        yyerror (]b4_yyerror_args[yyexc.what ());
        YYERROR;
      }
  #endif // YY_EXCEPTIONS

    return yyok;
  # undef yyerrok
  # undef YYABORT
  # undef YYACCEPT
  # undef YYERROR
  # undef YYBACKUP
  # undef yyclearin
  # undef YYRECOVERING
  }

  YYRESULTTAG
  yyresolveStack (]b4_user_formals_no_comma[)
  {
    if (yystateStack.isSplit())
      {
        yyStateIndex yys;
        int yyn;

        for (yyn = 0, yys = yystateStack.firstTop();
             yys != yystateStack.yysplitPoint;
             yys = stateAt(yys).yypredIndex, yyn += 1)
          continue;
        YYCHK (yyresolveStates (&firstTopState(), yyn
                               ]b4_user_args[));
      }
    return yyok;
  }

  /** Pop the symbols consumed by reduction #YYRULE from the top of stack
   *  #YYK of *YYSTACKP, and perform the appropriate semantic action on their
   *  semantic values.  Assumes that all ambiguities in semantic values
   *  have been previously resolved.  Set *YYVALP to the resulting value,
   *  and *YYLOCP to the computed location (if any).  Return value is as
   *  for userAction.  */
  inline YYRESULTTAG
  yydoAction (yyStateSetIndex yyk, yyRuleNum yyrule,
              YYSTYPE* yyvalp]b4_locuser_formals[)
  {
    int yynrhs = yyrhsLength (yyrule);

    if (!yystateStack.isSplit())
      {
        /* Standard special case: single stack.  */
        yyGLRStackItem* yyrhs = &yystateStack.stackItemAt(yystateStack.topAt(yyk).get());
        YYASSERT (yyk.get() == 0);
        yystateStack.pop_back(yynrhs);
        yystateStack.setFirstTop(yycreateStateIndex(yystateStack.size() - 1));
        YY_REDUCE_PRINT ((true, yyrhs, yyk, yyrule]b4_user_args[));
        return yyuserAction (yyrule, yynrhs, yyrhs,
                             yyvalp]b4_locuser_args[);
      }
    else
      {
        yyGLRStackItem yyrhsVals[YYMAXRHS + YYMAXLEFT + 1];
        yyStateIndex yys
          = yyrhsVals[YYMAXRHS + YYMAXLEFT].yystate.yypredIndex
          = yystateStack.topAt(yyk);]b4_locations_if([[
        if (yynrhs == 0)
          /* Set default location.  */
          yyrhsVals[YYMAXRHS + YYMAXLEFT - 1].yystate.yyloc = stateAt(yys).yyloc;]])[
        for (int yyi = 0; yyi < yynrhs; yyi += 1)
          {
            yys = stateAt(yys).yypredIndex;
            YYASSERT (yys.isValid());
          }
        yystateStack.yyupdateSplit (yys);
        yystateStack.setTopAt(yyk, yys);
        YY_REDUCE_PRINT ((false, yyrhsVals + YYMAXRHS + YYMAXLEFT - 1, yyk, yyrule]b4_user_args[));
        return yyuserAction (yyrule, yynrhs, yyrhsVals + YYMAXRHS + YYMAXLEFT - 1,
                             yyvalp]b4_locuser_args[);
      }
  }

  /** Pop items off stack #YYK of *YYSTACKP according to grammar rule YYRULE,
   *  and push back on the resulting nonterminal symbol.  Perform the
   *  semantic action associated with YYRULE and store its value with the
   *  newly pushed state, if YYFORCEEVAL or if *YYSTACKP is currently
   *  unambiguous.  Otherwise, store the deferred semantic action with
   *  the new state.  If the new state would have an identical input
   *  position, LR state, and predecessor to an existing state on the stack,
   *  it is identified with that existing state, eliminating stack #YYK from
   *  *YYSTACKP.  In this case, the semantic value is
   *  added to the options for the existing state's semantic value.
   */
  inline YYRESULTTAG
  yyglrReduce (yyStateSetIndex yyk, yyRuleNum yyrule,
               bool yyforceEval]b4_user_formals[)
  {
    size_t yyposn = topState(yyk).yyposn;

    if (yyforceEval || !yystateStack.isSplit())
      {
        YYSTYPE yysval;]b4_locations_if([[
        YYLTYPE yyloc;]])[

        YYRESULTTAG yyflag = yydoAction (yyk, yyrule, &yysval]b4_locuser_args([&yyloc])[);
        if (yyflag == yyerr && yystateStack.isSplit())
          {
            YYDPRINTF ((stderr, "Parse on stack %lu rejected by rule #%d.\n",
                       (unsigned long) yyk.get(), yyrule - 1));
          }
        if (yyflag != yyok)
          return yyflag;
        YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyrule], &yysval, &yyloc);
        yyglrShift (yyk,
                    yyLRgotoState (topState(yyk).yylrState,
                                   yylhsNonterm (yyrule)),
                    yyposn, &yysval]b4_locations_if([, &yyloc])[);
      }
    else
      {
        int yyn;
        yyStateIndex yys, yys0 = yystateStack.topAt(yyk);
        yyStateNum yynewLRState;

        for (yys = yystateStack.topAt(yyk), yyn = yyrhsLength (yyrule);
             0 < yyn; yyn -= 1)
          {
            yys = stateAt(yys).yypredIndex;
            YYASSERT (yys.isValid());
          }
        yystateStack.yyupdateSplit (yys);
        yynewLRState = yyLRgotoState (stateAt(yys).yylrState, yylhsNonterm (yyrule));
        YYDPRINTF ((stderr,
                    "Reduced stack %lu by rule #%d; action deferred.  "
                    "Now in state %d.\n",
                    (unsigned long) yyk.get(), yyrule - 1, yynewLRState));
        for (yyStateSetIndex yyi; yyi.get() < yystateStack.numTops(); ++yyi)
          if (yyi != yyk && yystateStack.topAt(yyi).isValid())
            {
              yyStateIndex yysplit = yystateStack.yysplitPoint;
              yyStateIndex yyp = yystateStack.topAt(yyi);
              while (yyp != yys && yyp != yysplit
                     && stateAt(yyp).yyposn >= yyposn)
                {
                  if (stateAt(yyp).yylrState == yynewLRState
                      && stateAt(yyp).yypredIndex == yys)
                    {
                      yyaddDeferredAction (yyk, &stateAt(yyp), yys0, yyrule);
                      yystateStack.yytops.yymarkStackDeleted (yyk);
                      YYDPRINTF ((stderr, "Merging stack %lu into stack %lu.\n",
                                  (unsigned long) yyk.get(),
                                  (unsigned long) yyi.get()));
                      return yyok;
                    }
                  yyp = stateAt(yyp).yypredIndex;
                }
            }
        yystateStack.setTopAt(yyk, yys);
        yyglrShiftDefer (yyk, yynewLRState, yyposn, yys0, yyrule);
      }
    return yyok;
  }

  /** Shift stack #YYK of *YYSTACKP, to a new state corresponding to LR
   *  state YYLRSTATE, at input position YYPOSN, with the (unresolved)
   *  semantic value of YYRHS under the action for YYRULE.  */
  inline void
  yyglrShiftDefer (yyStateSetIndex yyk, yyStateNum yylrState,
                   size_t yyposn, yyStateIndex yyrhs, yyRuleNum yyrule)
  {
    yyStateIndex yynewIndex = yystateStack.yynewGLRState();
    yyGLRState& yynewState = stateAt(yynewIndex);

    yynewState.yylrState = yylrState;
    yynewState.yyposn = yyposn;
    yynewState.yyresolved = false;
    yynewState.yypredIndex = yystateStack.topAt(yyk);
    yynewState.yysemantics.yyfirstValIndex.setInvalid();
    yystateStack.setTopAt(yyk, yynewIndex);

    /* Invokes yyreserveStack.  */
    yyaddDeferredAction (yyk, &yynewState, yyrhs, yyrule);
  }

  /** Shift to a new state on stack #YYK of *YYSTACKP, corresponding to LR
   * state YYLRSTATE, at input position YYPOSN, with (resolved) semantic
   * value *YYVALP and source location *YYLOCP.  */
  inline void
  yyglrShift (yyStateSetIndex yyk, yyStateNum yylrState,
              size_t yyposn,
              YYSTYPE* yyvalp]b4_locations_if([, YYLTYPE* yylocp])[)
  {
    yyStateIndex yynewIndex = yystateStack.yynewGLRState();
    yyGLRState& yynewState = stateAt(yynewIndex);

    yynewState.yylrState = yylrState;
    yynewState.yyposn = yyposn;
    yynewState.yyresolved = true;
    yynewState.yypredIndex = yystateStack.topAt(yyk);
    yynewState.yysemantics.yysval = *yyvalp;]b4_locations_if([
    yynewState.yyloc = *yylocp;])[
    yystateStack.setTopAt(yyk, yynewIndex);

    yyreserveGlrStack();
  }

#if ]b4_api_PREFIX[DEBUG
  void
  yypstack (yyStateSetIndex yyk)
  {
    yystateStack.yypstates (yystateStack.topAt(yyk));
  }
#endif

  yySemanticOption& optionAt(yySemanticOptionIndex i) {
    return yystateStack.optionAt(i);
  }

  yyGLRState& stateAt(yyStateIndex i) {
    return yystateStack.stateAt(i);
  }

  yyGLRState& topState(yyStateSetIndex i) {
    return stateAt(yystateStack.topAt(i));
  }

  yyGLRState& firstTopState() {
    return stateAt(yystateStack.firstTop());
  }

  yyGLRState* predState(yyGLRState* s) {
    return yystateStack.predState(s);
  }

 private:

  void popall() {
    /* If the stack is well-formed, pop the stack until it is empty,
       destroying its entries as we go.  But free the stack regardless
       of whether it is well-formed.  */
    if (!yystateStack.yyitems.empty())
      {
        for (yyStateSetIndex k; k.get() < yystateStack.numTops(); k += 1)
          if (yystateStack.topAt(k).isValid())
            {
              while (yystateStack.topAt(k).isValid())
                {
                  yyGLRState* state = &topState(k);]b4_locations_if([[
                    yyerror_range[1].yystate.yyloc = state->yyloc;]])[
                  if (state->yypredIndex.isValid())
                    yystateStack.yydestroyGLRState ("Cleanup: popping", state]b4_user_args[);
                  yystateStack.setTopAt(k, state->yypredIndex);
                  yystateStack.pop_back();
                }
                break;
            }
      }
  }

  /** Resolve the previous YYN states starting at and including state YYS
   *  on *YYSTACKP. If result != yyok, some states may have been left
   *  unresolved possibly with empty semantic option chains.  Regardless
   *  of whether result = yyok, each state has been left with consistent
   *  data so that yydestroyGLRState can be invoked if necessary.  */
  YYRESULTTAG
  yyresolveStates (yyGLRState* yys, int yyn]b4_user_formals[)
  {
    if (0 < yyn)
      {
        YYASSERT (yys->yypredIndex.isValid());
        YYCHK (yyresolveStates (predState(yys), yyn-1]b4_user_args[));
        if (! yys->yyresolved)
          YYCHK (yyresolveValue (yys]b4_user_args[));
      }
    return yyok;
  }

  /** Resolve the ambiguity represented in state YYS in *YYSTACKP,
   *  perform the indicated actions, and set the semantic value of YYS.
   *  If result != yyok, the chain of semantic options in YYS has been
   *  cleared instead or it has been left unmodified except that
   *  redundant options may have been removed.  Regardless of whether
   *  result = yyok, YYS has been left with consistent data so that
   *  yydestroyGLRState can be invoked if necessary.  */
  YYRESULTTAG
  yyresolveValue (yyGLRState* yys]b4_user_formals[)
  {
    yySemanticOptionIndex yybestIndex = yys->yysemantics.yyfirstValIndex;
    bool yymerge = false;
    YYSTYPE yysval;
    YYRESULTTAG yyflag;]b4_locations_if([
    YYLTYPE *yylocp = &yys->yyloc;])[

    for (yySemanticOptionIndex* yyindex = &optionAt(yybestIndex).yynextIndex;
         yyindex->isValid(); )
      {
        yySemanticOption* const yyp = &optionAt(*yyindex);
        yySemanticOption* const yybest = &optionAt(yybestIndex);

        if (yystateStack.yyidenticalOptions (yybest, yyp))
          {
            yystateStack.yymergeOptionSets (yybest, yyp);
            *yyindex = yyp->yynextIndex;
          }
        else
          {
            switch (yypreference (yybest, yyp))
              {
              case 0:]b4_locations_if([[
                yyresolveLocations (yys, 1]b4_user_args[);]])[
                return yystateStack.yyreportAmbiguity (yybest, yyp]b4_pure_args[);
                break;
              case 1:
                yymerge = true;
                break;
              case 2:
                break;
              case 3:
                yybestIndex = *yyindex;
                yymerge = false;
                break;
              default:
                /* This cannot happen so it is not worth a YYASSERT (false),
                   but some compilers complain if the default case is
                   omitted.  */
                break;
              }
            yyindex = &yyp->yynextIndex;
          }
      }

    yySemanticOption* const yybest = &optionAt(yybestIndex);
    if (yymerge)
      {
        int yyprec = yydprec[yybest->yyrule];
        yyflag = yyresolveAction (yybest, &yysval]b4_locuser_args[);
        if (yyflag == yyok)
          for (yySemanticOptionIndex yyindex = yybest->yynextIndex;
               yyindex.isValid();
               yyindex = optionAt(yyindex).yynextIndex)
            {
              yySemanticOption* yyp = &optionAt(yyindex);
              if (yyprec == yydprec[yyp->yyrule])
                {
                  YYSTYPE yysval_other;]b4_locations_if([
                  YYLTYPE yydummy;])[
                  yyflag = yyresolveAction (yyp, &yysval_other]b4_locuser_args([&yydummy])[);
                  if (yyflag != yyok)
                    {
                      yydestruct ("Cleanup: discarding incompletely merged value for",
                                  yystos[yys->yylrState],
                                  &yysval]b4_locuser_args[);
                      break;
                    }
                  yyuserMerge (yymerger[yyp->yyrule], &yysval, &yysval_other);
                }
            }
      }
    else
      yyflag = yyresolveAction (yybest, &yysval]b4_locuser_args([yylocp])[);

    if (yyflag == yyok)
      {
        yys->yyresolved = true;
        yys->yysemantics.yysval = yysval;
      }
    else
      yys->yysemantics.yyfirstValIndex.setInvalid();
    return yyflag;
  }

  /** Resolve the states for the RHS of YYOPT on *YYSTACKP, perform its
   *  user action, and return the semantic value and location in *YYVALP
   *  and *YYLOCP.  Regardless of whether result = yyok, all RHS states
   *  have been destroyed (assuming the user action destroys all RHS
   *  semantic values if invoked).  */
  YYRESULTTAG
  yyresolveAction (yySemanticOption* yyopt, YYSTYPE* yyvalp]b4_locuser_formals[)
  {
    yyGLRState* yyoptState = &stateAt(yyopt->yystateIndex);
    yyGLRStackItem yyrhsVals[YYMAXRHS + YYMAXLEFT + 1];
    int yynrhs = yyrhsLength (yyopt->yyrule);
    YYRESULTTAG yyflag =
      yyresolveStates (yyoptState, yynrhs]b4_user_args[);
    if (yyflag != yyok)
      {
        for (yyGLRState *yys = yyoptState; yynrhs > 0; yys = predState(yys), yynrhs -= 1)
          yystateStack.yydestroyGLRState ("Cleanup: popping", yys]b4_user_args[);
        return yyflag;
      }

    yyrhsVals[YYMAXRHS + YYMAXLEFT].yystate.yypredIndex = yyopt->yystateIndex;]b4_locations_if([[
    if (yynrhs == 0)
      /* Set default location.  */
      yyrhsVals[YYMAXRHS + YYMAXLEFT - 1].yystate.yyloc = yyoptState->yyloc;]])[
    {
      int yychar_current = yychar;
      YYSTYPE yylval_current = yylval;]b4_locations_if([
      YYLTYPE yylloc_current = yylloc;])[
      yychar = yyopt->yyrawchar;
      yylval = yyopt->yyval;]b4_locations_if([
      yylloc = yyopt->yyloc;])[
      yyflag = yyuserAction (yyopt->yyrule, yynrhs,
                             yyrhsVals + YYMAXRHS + YYMAXLEFT - 1, yyvalp]b4_locuser_args[);
      yychar = yychar_current;
      yylval = yylval_current;]b4_locations_if([
      yylloc = yylloc_current;])[
    }
    return yyflag;
  }]b4_locations_if([[

  /** Resolve the locations for each of the YYN1 states in *YYSTACKP,
   *  ending at YYS1.  Has no effect on previously resolved states.
   *  The first semantic option of a state is always chosen.  */
  void
  yyresolveLocations (yyGLRState *yys1, int yyn1]b4_user_formals[)
  {
    if (0 < yyn1)
      {
        yyresolveLocations (predState(yys1), yyn1 - 1]b4_user_args[);
        if (!yys1->yyresolved)
          {
            yyGLRStackItem yyrhsloc[1 + YYMAXRHS];
            int yynrhs;
            yySemanticOptionIndex yyoptionIndex = yys1->yysemantics.yyfirstValIndex;
            YYASSERT (yyoptionIndex.isValid());
            yySemanticOption *const yyoption = &optionAt(yyoptionIndex);
            yynrhs = yyrhsLength (yyoption->yyrule);
            if (0 < yynrhs)
              {
                yyGLRState *yys;
                int yyn;
                yyresolveLocations (&stateAt(yyoption->yystateIndex), yynrhs]b4_user_args[);
                for (yys = &stateAt(yyoption->yystateIndex), yyn = yynrhs;
                     yyn > 0;
                     yys = predState(yys), yyn -= 1)
                  yyrhsloc[yyn].yystate.yyloc = yys->yyloc;
              }
            else
              {
                /* Both yyresolveAction and yyresolveLocations traverse the GSS
                   in reverse rightmost order.  It is only necessary to invoke
                   yyresolveLocations on a subforest for which yyresolveAction
                   would have been invoked next had an ambiguity not been
                   detected.  Thus the location of the previous state (but not
                   necessarily the previous state itself) is guaranteed to be
                   resolved already.  */
                yyrhsloc[0].yystate.yyloc = stateAt(yyoption->yystateIndex).yyloc;
              }
            YYLLOC_DEFAULT ((yys1->yyloc), yyrhsloc, yynrhs);
          }
      }
  }]])[

]b4_parse_param_vars[

};
#undef yystackp
#undef YYSTACKEXPANDABLE


#if ]b4_api_PREFIX[DEBUG || YYERROR_VERBOSE
/** A printable representation of TOKEN.  */
static inline const char*
yytokenName (yySymbol yytoken)
{
  if (yytoken == YYEMPTY)
    return "";

  return yytname[yytoken];
}
#endif

/** If yychar is empty, fetch the next token.  */
static inline yySymbol
yygetToken (int *yycharp][]b4_pure_if([, yyGLRStack* yystackp])[]b4_user_formals[)
{
  yySymbol yytoken;
]b4_parse_param_use()dnl
[  if (*yycharp == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
#if YY_EXCEPTIONS
      try
        {
#endif // YY_EXCEPTIONS
          *yycharp = ]b4_lex[;
#if YY_EXCEPTIONS
        }
      catch (const ]b4_namespace_ref[::]b4_parser_class[::syntax_error& yyexc)
        {
          YYDPRINTF ((stderr, "Caught exception: %s\n", yyexc.what()));]b4_locations_if([
          yylloc = yyexc.location;])[
          yyerror (]b4_lyyerror_args[yyexc.what ());
          // Map errors caught in the scanner to the undefined token
          // (YYUNDEFTOK), so that error handling is started.
          // However, record this with this special value of yychar.
          *yycharp = YYFAULTYTOK;
        }
#endif // YY_EXCEPTIONS
    }
  if (*yycharp <= YYEOF)
    {
      *yycharp = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (*yycharp);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }
  return yytoken;
}



static void
yyuserMerge (int yyn, YYSTYPE* yy0, YYSTYPE* yy1)
{
  YYUSE (yy0);
  YYUSE (yy1);

  switch (yyn)
    {
]b4_mergers[
      default: break;
    }
}

                              /* Bison grammar-table manipulation.  */

/** Number of symbols composing the right hand side of rule #RULE.  */
static inline int
yyrhsLength (yyRuleNum yyrule)
{
  return yyr2[yyrule];
}

/** The action to take in YYSTATE on seeing YYTOKEN.
 *  Result R means
 *    R < 0:  Reduce on rule -R.
 *    R = 0:  Error.
 *    R > 0:  Shift to state R.
 *  Set *YYCONFLICTS to a pointer into yyconfl to a 0-terminated list
 *  of conflicting reductions.
 */
static inline int
yygetLRActions (yyStateNum yystate, yySymbol yytoken, const short** yyconflicts)
{
  int yyindex = yypact[yystate] + yytoken;
  if (yyisDefaultedState (yystate)
      || yyindex < 0 || YYLAST < yyindex || yycheck[yyindex] != yytoken)
    {
      *yyconflicts = yyconfl;
      return -yydefact[yystate];
    }
  else if (! yytable_value_is_error (yytable[yyindex]))
    {
      *yyconflicts = yyconfl + yyconflp[yyindex];
      return yytable[yyindex];
    }
  else
    {
      *yyconflicts = yyconfl + yyconflp[yyindex];
      return 0;
    }
}

/** Compute post-reduction state.
 * \param yystate   the current state
 * \param yysym     the nonterminal to push on the stack
 */
static inline yyStateNum
yyLRgotoState (yyStateNum yystate, yySymbol yysym)
{
  int yyr = yypgoto[yysym - YYNTOKENS] + yystate;
  if (0 <= yyr && yyr <= YYLAST && yycheck[yyr] == yystate)
    return yytable[yyr];
  else
    return yydefgoto[yysym - YYNTOKENS];
}

                                /* GLRStacks */

/** Y0 and Y1 represent two possible actions to take in a given
 *  parsing state; return 0 if no combination is possible,
 *  1 if user-mergeable, 2 if Y0 is preferred, 3 if Y1 is preferred.  */
static int
yypreference (yySemanticOption* y0, yySemanticOption* y1)
{
  yyRuleNum r0 = y0->yyrule, r1 = y1->yyrule;
  int p0 = yydprec[r0], p1 = yydprec[r1];

  if (p0 == p1)
    {
      if (yymerger[r0] == 0 || yymerger[r0] != yymerger[r1])
        return 0;
      else
        return 1;
    }
  if (p0 == 0 || p1 == 0)
    return 0;
  if (p0 < p1)
    return 3;
  if (p1 < p0)
    return 2;
  return 0;
}

#define YYCHK1(YYE)                                                          \
  do {                                                                       \
    switch (YYE) {                                                           \
    case yyok:                                                               \
      break;                                                                 \
    case yyabort:                                                            \
      goto yyabortlab;                                                       \
    case yyaccept:                                                           \
      goto yyacceptlab;                                                      \
    case yyerr:                                                              \
      goto yyuser_error;                                                     \
    default:                                                                 \
      goto yybuglab;                                                         \
    }                                                                        \
  } while (0)

#define YYSTATEAT yystack.stateAt

/*----------.
| yyparse.  |
`----------*/

]b4_function_define([yyparse], [int], b4_parse_param)[
{
  int yyresult;
  yyGLRStack yystack(YYINITDEPTH]b4_user_args[);
  yyGLRStack* const yystackp = &yystack;
  size_t yyposn;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY;
  yylval = yyval_default;]b4_locations_if([
  yylloc = yyloc_default;])[
]m4_ifdef([b4_initial_action], [
b4_dollar_pushdef([yylval], [], [], [yylloc])dnl
  b4_user_initial_action
b4_dollar_popdef])[]dnl
[
  switch (YYSETJMP (yystack.yyexception_buffer))
    {
    case 0: break;
    case 1: goto yyabortlab;
    case 2: goto yyexhaustedlab;
    default: goto yybuglab;
    }
  yystack.yyglrShift (yycreateStateSetIndex(0), 0, 0, &yylval]b4_locations_if([, &yylloc])[);
  yyposn = 0;

  while (true)
    {
      /* For efficiency, we have two loops, the first of which is
         specialized to deterministic operation (single stack, no
         potential ambiguity).  */
      /* Standard mode */
      while (true)
        {
          yyStateNum yystate = YYSTATEAT(yystack.yystateStack.firstTop()).yylrState;
          YYDPRINTF ((stderr, "Entering state %d\n", yystate));
          if (yystate == YYFINAL)
            goto yyacceptlab;
          if (yyisDefaultedState (yystate))
            {
              yyRuleNum yyrule = yydefaultAction (yystate);
              if (yyrule == 0)
                {]b4_locations_if([[
                  yystack.yyerror_range[1].yystate.yyloc = yylloc;]])[
                  yystack.yyreportSyntaxError (]b4_user_args_no_comma[);
                  goto yyuser_error;
                }
              YYCHK1 (yystack.yyglrReduce (yycreateStateSetIndex(0), yyrule, true]b4_user_args[));
            }
          else
            {
              yySymbol yytoken = ]b4_yygetToken_call;[
              const short* yyconflicts;
              int yyaction = yygetLRActions (yystate, yytoken, &yyconflicts);
              if (*yyconflicts != 0)
                break;
              if (yyisShiftAction (yyaction))
                {
                  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
                  yychar = YYEMPTY;
                  yyposn += 1;
                  yystack.yyglrShift (yycreateStateSetIndex(0), yyaction, yyposn, &yylval]b4_locations_if([, &yylloc])[);
                  if (0 < yystack.yyerrState)
                    yystack.yyerrState -= 1;
                }
              else if (yyisErrorAction (yyaction))
                {]b4_locations_if([[
                  yystack.yyerror_range[1].yystate.yyloc = yylloc;]])[
                  /* Don't issue an error message again for exceptions
                     thrown from the scanner.  */
                  if (yychar != YYFAULTYTOK)
                    yystack.yyreportSyntaxError (]b4_user_args_no_comma[);
                  goto yyuser_error;
                }
              else
                YYCHK1 (yystack.yyglrReduce (yycreateStateSetIndex(0), -yyaction, true]b4_user_args[));
            }
        }

      while (true)
        {
          yySymbol yytoken_to_shift;

          for (yyStateSetIndex yys; yys.get() < yystack.yystateStack.numTops(); ++yys)
            yystackp->yystateStack.yytops.setLookaheadNeeds(yys, yychar != YYEMPTY);

          /* yyprocessOneStack returns one of three things:

              - An error flag.  If the caller is yyprocessOneStack, it
                immediately returns as well.  When the caller is finally
                yyparse, it jumps to an error label via YYCHK1.

              - yyok, but yyprocessOneStack has invoked yymarkStackDeleted
                (yys), which sets the top state of yys to NULL.  Thus,
                yyparse's following invocation of yyremoveDeletes will remove
                the stack.

              - yyok, when ready to shift a token.

             Except in the first case, yyparse will invoke yyremoveDeletes and
             then shift the next token onto all remaining stacks.  This
             synchronization of the shift (that is, after all preceding
             reductions on all stacks) helps prevent double destructor calls
             on yylval in the event of memory exhaustion.  */

          for (yyStateSetIndex yys; yys.get() < yystack.yystateStack.numTops(); ++yys)
            YYCHK1 (yystack.yyprocessOneStack (yys, yyposn]b4_lpure_args[));
          yystack.yystateStack.yytops.yyremoveDeletes ();
          if (yystack.yystateStack.yytops.size() == 0)
            {
              yystack.yystateStack.yytops.yyundeleteLastStack ();
              if (yystack.yystateStack.yytops.size() == 0)
                yystack.yyFail (YY_("syntax error")][]b4_lpure_args[);
              YYCHK1 (yystack.yyresolveStack (]b4_user_args_no_comma[));
              YYDPRINTF ((stderr, "Returning to deterministic operation.\n"));]b4_locations_if([[
              yystack.yyerror_range[1].yystate.yyloc = yylloc;]])[
              yystack.yyreportSyntaxError (]b4_user_args_no_comma[);
              goto yyuser_error;
            }

          /* If any yyglrShift call fails, it will fail after shifting.  Thus,
             a copy of yylval will already be on stack 0 in the event of a
             failure in the following loop.  Thus, yychar is set to YYEMPTY
             before the loop to make sure the user destructor for yylval isn't
             called twice.  */
          yytoken_to_shift = YYTRANSLATE (yychar);
          yychar = YYEMPTY;
          yyposn += 1;
          for (yyStateSetIndex yys; yys.get() < yystack.yystateStack.numTops(); ++yys)
            {
              yyStateNum yystate = yystack.topState(yys).yylrState;
              const short* yyconflicts;
              int yyaction = yygetLRActions (yystate, yytoken_to_shift,
                              &yyconflicts);
              /* Note that yyconflicts were handled by yyprocessOneStack.  */
              YYDPRINTF ((stderr, "On stack %lu, ", (unsigned long) yys.get()));
              YY_SYMBOL_PRINT ("shifting", yytoken_to_shift, &yylval, &yylloc);
              yystack.yyglrShift (yys, yyaction, yyposn,
                          &yylval]b4_locations_if([, &yylloc])[);
              YYDPRINTF ((stderr, "Stack %lu now in state #%d\n",
                          (unsigned long) yys.get(),
                          yystack.topState(yys).yylrState));
            }

          if (yystack.yystateStack.yytops.size() == 1)
            {
              YYCHK1 (yystack.yyresolveStack (]b4_user_args_no_comma[));
              YYDPRINTF ((stderr, "Returning to deterministic operation.\n"));
              yystack.yystateStack.yycompressStack ();
              break;
            }
        }
      continue;
    yyuser_error:
      yystack.yyrecoverSyntaxError (]b4_user_args_no_comma[);
      yyposn = yystack.firstTopState().yyposn;
    }

 yyacceptlab:
  yyresult = 0;
  goto yyreturn;

 yybuglab:
  YYASSERT (false);
  goto yyabortlab;

 yyabortlab:
  yyresult = 1;
  goto yyreturn;

 yyexhaustedlab:
  yyerror (]b4_lyyerror_args[YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturn;

 yyreturn:
  return yyresult;
}

#undef YYSTATEAT

/* DEBUGGING ONLY */
#if ]b4_api_PREFIX[DEBUG
static void
yypstack (yyGLRStack* yystackp, size_t yyk)
{
  yystackp->yypstack(yycreateStateSetIndex(yyk));
}

#endif

#undef yylval
#undef yychar
#undef yynerrs]b4_locations_if([
#undef yylloc])

m4_if(b4_prefix, [yy], [],
[[/* Substitute the variable and function names.  */
#define yyparse ]b4_prefix[parse
#define yylex   ]b4_prefix[lex
#define yyerror ]b4_prefix[error
#define yylval  ]b4_prefix[lval
#define yychar  ]b4_prefix[char
#define yydebug ]b4_prefix[debug
#define yynerrs ]b4_prefix[nerrs]b4_locations_if([[
#define yylloc  ]b4_prefix[lloc]])])[

]b4_epilogue[]dnl
b4_output_end
m4_popdef([b4_parse_param])
