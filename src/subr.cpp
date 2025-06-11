//----------------------------------------------------------------------
// vx-scheme : Scheme interpreter.
// Copyright (c) 2002,2003,2006 and onwards Colin Smith.
//
// You may distribute under the terms of the Artistic License, 
// as specified in the LICENSE file.
//
// subr.cpp : C implementations of Scheme primitives.


#include "vx-scheme.h"
#include <fstream>
#include <float.h>
#include <math.h>
#include <errno.h>
#ifdef WIN32
#include <windows.h>
#endif

//---------------------------------------------------------------------
// Utilities
//

static FILE * oport (Context * ctx, Cell * arglist)
    {
    if (arglist != nil)
        return car (arglist)->OportValue ();
    else
        return ctx->current_output ()->OportValue ();
    }

static FILE * iport (Context * ctx, Cell * arglist)
    {
    if (arglist != nil)
        return car (arglist)->IportValue ();
    else
        return ctx->current_input ()->IportValue ();
    }

// exact_list canvasses the given arglist.  If all the arguments
// are integer type, exact_p returns true (indicating that integer
// math is appropriate to combine them with.)  If at least one
// real is found, it returns false (suggesting that the args 
// should be promoted to real type before combination.  If any
// other type is encountered, an error is thrown.

static bool exact_list (Cell * arglist)
    {
    FOR_EACH (a, arglist)
	switch (car (a)->type ())
	    {
	    case Cell::Int:   continue;
            case Cell::Real:  return false;
	    default:          error ("non-numeric type encountered");
	    }

    return true;
    }

inline static double asReal (Cell * c)
    {
    if (c->type () == Cell::Int)
	return (double) c->IntValue ();
    else
	return c->RealValue ();
    }

//---------------------------------------------------------------------
// THE PRIMITIVE PROCEDURES
//

Cell * skcons (Context * ctx, Cell * arglist)
    {
    return ctx->cons (car (arglist), cadr (arglist));
    }

Cell * skplus (Context * ctx, Cell * arglist)
    {
    if (exact_list (arglist))
	{
	int result = 0;

	FOR_EACH (p, arglist)
	    result += car (p)->IntValue ();

	return ctx->make_int (result);
	}
    else
	{
	double result = 0;
	
	FOR_EACH (p, arglist)
	    result += asReal (car (p));

	return ctx->make_real (result);
	}
    }

Cell * skminus (Context * ctx, Cell * arglist)
    {
    if (exact_list (arglist))
	{
	int result = car (arglist)->IntValue ();
	arglist = cdr (arglist);

	if (arglist == nil)
	    return ctx->make_int (- result);

	FOR_EACH (a, arglist)
	    result -= car (a)->IntValue ();

	return ctx->make_int (result);
	}
    else
	{
	double result = asReal (car (arglist));
	arglist = cdr (arglist);

	if (arglist == nil)
	    return ctx->make_real (- result);

	FOR_EACH (a, arglist)
	    result -= asReal (car (a));

	return ctx->make_real (result);
	}
    }

Cell * divide (Context * ctx, Cell * arglist)
    {
    double result;

    if (cdr (arglist) != nil)
	{
	// The usual case: there are at least 2 arguments.
	// (/ a b c ...) ==> ((a / b) / c ...)
	
	result = asReal (car (arglist));

	FOR_EACH (a, cdr (arglist))
	    result = result / asReal (car (a));
	}
    else
	{
	// A single argument means take its reciprocal.

	result = 1.0 / asReal (car (arglist));
	}

    return ctx->make_real (result);
    }

Cell * times (Context * ctx, Cell * arglist)
    {
    if (exact_list (arglist))
	{
	int result = 1;

	FOR_EACH (p, arglist)
	    result *= Cell::car (p)->IntValue ();

	return ctx->make_int (result);
	}
    else
	{
	double result = 1.0;

	FOR_EACH (p, arglist)
	    result *= asReal (car (p));

	return ctx->make_real (result);
	}
    }

Cell * skmax (Context * ctx, Cell * arglist)
    {
    if (exact_list (arglist))
	{
	int         m = INT_MIN;
	int         z;

	FOR_EACH (a, arglist)
	    if ((z = Cell::car (a)->IntValue ()) > m)
		m = z;

	return ctx->make_int (m);
	}
    else
	{
	double      m = DBL_MIN;
	double      z;

	FOR_EACH (a, arglist)
	    if ((z = asReal (car (a))) > m)
		m = z;

	return ctx->make_real (m);
	}
    }

Cell * skmin (Context * ctx, Cell * arglist)
    {
    if (exact_list (arglist))
	{
	int         m = INT_MAX;
	int         z;

	FOR_EACH (a, arglist)
	    if ((z = car (a)->IntValue ()) < m)
		m = z;

	return ctx->make_int (m);
	}
    else
	{
	double      m = DBL_MAX;
	double      z;

	FOR_EACH (a, arglist)
	    if ((z = asReal (car (a))) < m)
		m = z;

	return ctx->make_real (m);
	}
    }

Cell * skabs (Context * ctx, Cell * arglist)
    {
    Cell * c = car (arglist);
    if (c->type () == Cell::Int)
	return ctx->make_int (abs (c->IntValue ()));
    else if (c->type () == Cell::Real)
	return ctx->make_real (fabs (c->RealValue ()));
    else
	error ("numeric type expected");
    return nil; // for compiler
    }

// BINOP is a macro which constructs a binary operator
// out of a fragment of C code (OP).  This works on 
// non numeric types (i.e., those that do not participate
// in coercion).

#define BINOP(name, OP, ctype, stype)                       \
    Cell * name (Context * ctx, Cell * args)         	    \
        {                                                   \
        FOR_EACH (a, args)                                  \
            if (Cell::cdr (a) != nil)                       \
                {                                           \
                ctype ia = Cell::car (a)->stype##Value ();  \
                ctype ib = Cell::cadr (a)->stype##Value (); \
                if (! OP (ia, ib))                          \
                    return &Cell::Bool_F;                   \
                }                                           \
        return &Cell::Bool_T;                               \
    }

static int strcmp_ci (char * s, char * t)
    {
    /* Derived from BSD version. */

    unsigned char u1;
    unsigned char u2;

    while (1) 
	{
	u1 = (unsigned char) tolower (*s++);
	u2 = (unsigned char) tolower (*t++);
	if (u1 != u2)
	    return u1 - u2;
	if (u1 == '\0')
	    return 0;
	}
    }

#define EQ(a,b)       ((a) == (b))
#define LE(a,b)       ((a) <= (b))
#define LT(a,b)       ((a) <  (b))
#define GE(a,b)       ((a) >= (b))
#define GT(a,b)       ((a) >  (b))
#define strEQ(a,b)    (strcmp    (a,b) == 0)
#define strLE(a,b)    (strcmp    (a,b) <= 0)
#define strLT(a,b)    (strcmp    (a,b) <  0)
#define strGE(a,b)    (strcmp    (a,b) >= 0)
#define strGT(a,b)    (strcmp    (a,b) >  0)
#define strEQci(a,b)  (strcmp_ci (a,b) == 0)
#define strLEci(a,b)  (strcmp_ci (a,b) <= 0)
#define strLTci(a,b)  (strcmp_ci (a,b) <  0)
#define strGEci(a,b)  (strcmp_ci (a,b) >= 0)
#define strGTci(a,b)  (strcmp_ci (a,b) >  0)
#define chrEQci(a,b)  (tolower(a) == tolower (b))
#define chrLEci(a,b)  (tolower(a) <= tolower (b))
#define chrLTci(a,b)  (tolower(a) <  tolower (b))
#define chrGEci(a,b)  (tolower(a) >= tolower (b))
#define chrGTci(a,b)  (tolower(a) >  tolower (b))
    
BINOP (char_eq,      EQ,      char,   Char)
BINOP (char_le,      LE,      char,   Char)
BINOP (char_lt,      LT,      char,   Char)
BINOP (char_ge,      GE,      char,   Char)
BINOP (char_gt,      GT,      char,   Char)
BINOP (string_eq,    strEQ,   char *, String)
BINOP (string_le,    strLE,   char *, String)
BINOP (string_lt,    strLT,   char *, String)
BINOP (string_ge,    strGE,   char *, String)
BINOP (string_gt,    strGT,   char *, String)
BINOP (string_eq_ci, strEQci, char *, String)
BINOP (string_le_ci, strLEci, char *, String)
BINOP (string_lt_ci, strLTci, char *, String)
BINOP (string_ge_ci, strGEci, char *, String)
BINOP (string_gt_ci, strGTci, char *, String)
BINOP (char_eq_ci,   chrEQci, char,   Char)
BINOP (char_le_ci,   chrLEci, char,   Char)
BINOP (char_lt_ci,   chrLTci, char,   Char)
BINOP (char_ge_ci,   chrGEci, char,   Char)
BINOP (char_gt_ci,   chrGTci, char,   Char)

#define NBINOP(name, OP)                                 	\
    Cell * name (Context * ctx, Cell * args)             	\
        {                                                	\
        bool exact = exact_list (args);                     	\
        FOR_EACH (a, args)                               	\
	    if (cdr (a) != nil)                         	\
                if (exact)                               	\
	            { 		 			 	\
                    int ia = car (a)->IntValue ();       	\
                    int ib = cadr (a)->IntValue ();      	\
	            if (! OP (ia, ib))			 	\
			return &Cell::Bool_F;                   \
                    }                                           \
                else                                            \
                    {                                           \
                    double da = asReal (car (a));               \
                    double db = asReal (cadr (a));		\
	            if (! OP (da, db))			 	\
			return &Cell::Bool_F;                   \
                    }                                           \
        return &Cell::Bool_T;                                   \
        }

NBINOP (number_equal, EQ)
NBINOP (le,           LE)
NBINOP (lt,           LT)
NBINOP (ge,           GE)
NBINOP (gt,           GT)

#define CHAR_CLASS(sname, cname)                                   \
    Cell * sname (Context * ctx, Cell * args)            	   \
        {                                                	   \
        Cell * charptr = Cell::car (args);                    	   \
        return ctx->make_boolean (cname (charptr->CharValue ()) != 0);  \
        }

CHAR_CLASS (alphabetic_p, isalpha)
CHAR_CLASS (lower_case_p, islower)
CHAR_CLASS (upper_case_p, isupper)
CHAR_CLASS (numeric_p,    isdigit)
CHAR_CLASS (whitespace_p, isspace)
    
Cell * negative_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (car (arglist)->IntValue () < 0);
    }

Cell * positive_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (car (arglist)->IntValue () > 0);
    }

Cell * even_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean ((car (arglist)->IntValue () & 1) == 0);
    }

Cell * odd_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean ((car (arglist)->IntValue () & 1) == 1);
    }

Cell * eq (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (car (arglist)->eq (cadr (arglist)));
    }

Cell * eqv (Context * ctx, Cell * arglist) 
    {
    // If they're both real, compare them as numbers; else use eq
    if (car (arglist)->type () == Cell::Real &&
        cadr (arglist)->type () == Cell::Real)
        return ctx->make_boolean (
	    car (arglist)->RealValue () == cadr (arglist)->RealValue ());
    return eq (ctx, arglist);
    }

Cell * equal_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (car (arglist)->equal (cadr (arglist)));
    }

Cell * length (Context * ctx, Cell * arglist)
    {
    return ctx->make_int (car (arglist)->length ());
    }

Cell * sknot (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (! car (arglist)->istrue ());
    }

Cell * display (Context * ctx, Cell * arglist)
    {
    car (arglist)->display (oport (ctx, cdr (arglist)));
    return unspecified;
    }

Cell * display_star (Context * ctx, Cell * arglist)
    {
    FOR_EACH (a, arglist) 
      car(a)->display(oport (ctx, nil));
    return unspecified;
    }


Cell * write (Context * ctx, Cell * arglist)
    {
    car (arglist)->write (oport (ctx, cdr (arglist)));
    return unspecified;
    }

Cell * write_char (Context * ctx, Cell * arglist)
    {
    fputc (car (arglist)->CharValue (), oport (ctx, cdr (arglist)));
    return unspecified;
    }

Cell * skmake_vector (Context * ctx, Cell * arglist)
    {
    int n = car (arglist)->IntValue ();

    if (cdr (arglist) != nil)
	return ctx->make_vector (n, cadr (arglist));

    return ctx->make_vector (n);
    }

Cell * vector_ref (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();
    int                n = cadr (arglist)->IntValue ();

    return v->get (n);
    }

Cell * vector_set (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();
    int                n = cadr (arglist)->IntValue ();

    v->set (n, caddr (arglist));
    return unspecified;
    }

Cell * vector_fill (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();
    Cell *             filler = cadr (arglist);
    int                sz = v->size ();

    for (int ix = 0; ix < sz; ++ix)
	v->set (ix, filler);

    return unspecified;
    }

Cell * vector_length (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();

    return ctx->make_int (v->size ());
    }

// Flexible vector functions.  These are outside the Scheme standard,
// but very useful in practice.  Essentially the following four functions
// allow the resizing of vectors via the standard deque operations.
// We borrow the nomenclature from Perl:  "vector-push!" adds a new 
// element to the right end of a vector; "vector-pop!" detaches the
// right-most element of a vector and returns it.  "vector-unshift!"
// and "vector-shift!" do the same thing at the left side of the vector.

Cell * vector_push (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();
    v->push (cadr (arglist));
    return unspecified;
    }

Cell * vector_pop (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();
    return (v->pop ());
    }

Cell * vector_shift (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();
    return v->shift ();
    }

Cell * vector_unshift (Context * ctx, Cell * arglist)
    {
    cellvector *       v = car (arglist)->VectorValue ();
    v->unshift (cadr (arglist));
    return unspecified;
    }

Cell * vector_from_list (Context * ctx, Cell * arglist)
    {
    int                n = arglist->length ();
    Cell *             v = ctx->make_vector (n);
    cellvector *       vec = v->VectorValue ();
    int                ix = 0;

    ctx->gc_protect (v);
    FOR_EACH (elt, arglist)
	vec->set (ix++, car (elt));
    ctx->gc_unprotect ();
    return v;
    }

Cell * vector_to_list (Context * ctx, Cell * arglist)
    {
    Cell::List          list;
    Cell *              elt;
    cellvector *        vec = car (arglist)->VectorValue ();
    int                 n = vec->size ();

    ctx->gc_protect (list.head ());
    for (int ix = 0; ix < n; ++ix)
	{
	elt = ctx->make (vec->get (ix));
	ctx->gc_protect (elt);
        list.append (elt);
	ctx->gc_unprotect (2);
	ctx->gc_protect (list.head ());
	}
    ctx->gc_unprotect ();
    return list.head ();
    }

Cell * list_ref (Context * ctx, Cell * arglist)
    {
    Cell *      list = car (arglist);
    int         n = cadr (arglist)->IntValue ();
    int         ix = 0;

    FOR_EACH (a, list)
        if (ix++ == n)
            return car (a);

    error ("index out of bounds");
    return unimplemented;
    }

Cell * quotient (Context * ctx, Cell * arglist)
    {
    int d = cadr (arglist)->IntValue ();
    if (d == 0)
	error ("quotient /0");

    return ctx->make_int (car (arglist)->IntValue () / d);
    }

Cell * remainder (Context * ctx, Cell * arglist)
    {
    int n = car (arglist)->IntValue ();
    int d = cadr (arglist)->IntValue ();
    if (d == 0)
	error ("remainder /0");

    return ctx->make_int (n % d);
    }

Cell * modulo (Context * ctx, Cell * arglist)
    {
    int n = car (arglist)->IntValue ();
    int d = cadr (arglist)->IntValue ();
    int m = n % d;
    if (m < 0 && d > 0) return ctx->make_int (m + d);
    if (m > 0 && d < 0) return ctx->make_int (m + d);
    return ctx->make_int (m);
    }

//---------------------------------------------------------------------
// gcd2 (u,v)
// 
// Computes the greates common divisor of the given two integers.
// This implementation is Knuth's Algorithm 4.5.2B (TAoCP 3ed. vol II
// p. 338).  The variables and label names are as in Knuth's
// presentation and we refer the reader there for further
// documentation.
//

static int gcd2 (int u, int v)
    {
    if (u == 0) 
        return abs (v);
    if (v == 0)
        return abs (u);
    u = abs (u);
    v = abs (v);

//B1:
    int k = 0, t;
    while ((u & 1) + (v & 1) == 0)
        {
        k++;
        u >>= 1;
        v >>= 1;
        }
//B2
    if (u & 1)
        {
        t = -v;
        goto B4;
        }
    t = u;
B3: t >>= 1;
B4: if ((t & 1) == 0)
        goto B3;
//B5:
    if (t > 0)
        u = t;
    else
        v = -t;
//B6:
     t = u - v;
    if (t)
        goto B3;
    return u << k;
    }

Cell * gcd (Context * ctx, Cell * arglist)
    {
    int         g = 0;

    FOR_EACH (i, arglist)
        g = gcd2 (g, car (i)->IntValue ());

    return ctx->make_int (g);
    }

Cell * lcm (Context * ctx, Cell * arglist)
    {
    int         product = 1;
    int         g = 0;

    FOR_EACH (ip, arglist)        
        { 
        int i = car (ip)->IntValue ();
        product *= i;
        g = gcd2 (g, i);
        }

    return ctx->make_int (g == 0 ? 1 : abs (product / g));
    }

Cell * null_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (car (arglist) == nil);
    }

Cell * zero_p (Context * ctx, Cell * arglist)
    {
    Cell * a = car (arglist);
    if (a->type () == Cell::Int)
	return ctx->make_boolean (a->IntValue () == 0);
    else
	return ctx->make_boolean (a->RealValue () == 0.0);
    }

Cell * skfalse (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (false);
    }

#define ACCESSOR(ac) \
    Cell * ac (Context * ctx, Cell * a) {return Cell::ac (Cell::car (a)); }

ACCESSOR (car)
ACCESSOR (cdr)
ACCESSOR (caar)
ACCESSOR (cadr)
ACCESSOR (cdar)
ACCESSOR (cddr)
ACCESSOR (caaar)
ACCESSOR (caadr)
ACCESSOR (cadar)
ACCESSOR (caddr)
ACCESSOR (cdaar)
ACCESSOR (cdadr)
ACCESSOR (cddar)
ACCESSOR (cdddr)
ACCESSOR (caaaar)
ACCESSOR (caaadr)
ACCESSOR (caadar)
ACCESSOR (caaddr)
ACCESSOR (cadaar)
ACCESSOR (cadadr)
ACCESSOR (caddar)
ACCESSOR (cadddr)
ACCESSOR (cdaaar)
ACCESSOR (cdaadr)
ACCESSOR (cdadar)
ACCESSOR (cdaddr)
ACCESSOR (cddaar)
ACCESSOR (cddadr)
ACCESSOR (cdddar)
ACCESSOR (cddddr)

#define TYPE_PREDICATE(n,t)                      		   \
    Cell * n (Context * ctx, Cell * a)                             \
    { return ctx->make_boolean (Cell::car (a)->type () == Cell::t);} 

TYPE_PREDICATE (string_p,   String);
TYPE_PREDICATE (symbol_p,   Symbol);
TYPE_PREDICATE (vector_p,   Vec);
TYPE_PREDICATE (char_p,     Char);
TYPE_PREDICATE (input_p,    Iport);
TYPE_PREDICATE (output_p,   Oport);
TYPE_PREDICATE (integer_p,  Int);
TYPE_PREDICATE (exact_p,    Int);
TYPE_PREDICATE (inexact_p,  Real);

#define IS_NUMERIC(n)							\
    Cell * n (Context * ctx, Cell * a)			                \
    {									\
    Cell::Type t = car (a)->type ();       				\
    return ctx->make_boolean (t == Cell::Int || t == Cell::Real);	\
    }

IS_NUMERIC (number_p);
IS_NUMERIC (rational_p);
IS_NUMERIC (real_p);
IS_NUMERIC (complex_p);

Cell * pair_p (Context * ctx, Cell * arglist)
    {
    Cell * a = car (arglist);
    
    return ctx->make_boolean (a->ispair());
    }

Cell * boolean_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (car (arglist)->isBoolean ());
    }

Cell * procedure_p (Context * ctx, Cell * arglist)
    {
    Cell * a = car (arglist);
    Cell::Type t = a->type ();

    return ctx->make_boolean   (t == Cell::Subr 
                            ||  t == Cell::Lambda
	                    ||  t == Cell::Cont
                            ||  t == Cell::Cproc
			    || (t == Cell::Builtin && !a->macro ()));
    }

Cell* primitive_procedure_p (Context * ctx, Cell * arglist) {
  return ctx->make_boolean (car(arglist)->type() == Cell::Subr); 
}

Cell * list_p (Context * ctx, Cell * arglist)
    {
    Cell *	p0 = car (arglist);
    Cell *      p = p0;


    while (true)
	{
	if (p == nil)
	    return ctx->make_boolean (true);

	if (p->type () != Cell::Cons)
	    return ctx->make_boolean (false);

	p = Cell::cdr (p);

        if (p == p0)
            return ctx->make_boolean (false);
	}
    }

Cell * number_to_string (Context * ctx, Cell * arglist)
    {
    Cell *      a = car (arglist);
    switch (a->type ())
	{
	case Cell::Int :
	    {
	    char *  fmt = "%d";

	    if (cdr (arglist) != nil)
		{
		int base = cadr (arglist)->IntValue ();

		if (base == 16)
		    fmt = "%x";
		else if (base == 8)
		    fmt = "%o";
		else if (base == 10)
		    fmt = "%d";
		else
		    error ("unsupported output base");  // XXX 

		}

	    char buf [80];
	    sprintf (buf, fmt, car (arglist)->IntValue ());
	    return ctx->make_string (buf);
	    }
	case Cell::Real :
	    {
	    char buf [80];
	    Cell::real_to_string (a->RealValue (), buf, sizeof (buf));
	    return ctx->make_string (buf);
	    }

        default:
	    return ctx->make_boolean (false);
	}
    }

Cell * string_length (Context * ctx, Cell * arglist)
    {
    return ctx->make_int (static_cast<int>(car (arglist)->StringLength ()));
    }

Cell * newline (Context * ctx, Cell * arglist)
    {
    fputc ('\n', oport (ctx, arglist));
    return unspecified;
    }

Cell * string_to_list (Context * ctx, Cell * arglist)
    {
    Cell::List   l;
    Cell *       elt;
    const char * s = car (arglist)->StringValue ();
    char         c;

    ctx->gc_protect (l.head ());
    while ((c = *s++))
	{
	elt = ctx->make (ctx->make_char (c));
	ctx->gc_protect (elt);
	l.append (elt);
	ctx->gc_unprotect (2);
	ctx->gc_protect (l.head ());
	}

    ctx->gc_unprotect ();
    return l.head ();
    }

Cell * sklist (Context * ctx, Cell * arglist)
    {
    return arglist;
    }

Cell * skmake_string (Context * ctx, Cell * arglist)
    {
    int         n = car (arglist)->IntValue ();
    char        ch = ' ';

    if (cdr (arglist) != nil)
        ch = cadr (arglist)->CharValue ();

    return ctx->make_string (n, ch);
    }

Cell * string_ref (Context * ctx, Cell * arglist)
    {
    Cell *      pstr = car (arglist);
    int         ix = cadr (arglist)->IntValue ();
    int         n = static_cast<int>(pstr->StringLength ());

    if (ix < 0 || ix >= n)
        error ("string index out of bounds");

    return ctx->make_char (pstr->StringValue () [ix]);
    }

Cell * append (Context * ctx, Cell * arglist)
    {
    Cell::List  alist; 
    Cell *      elt;

    if (arglist == nil)
	return nil;

    ctx->gc_protect (alist.head ());
    while (cdr (arglist) != nil)
        {
        FOR_EACH (a, car (arglist))
	    {
	    elt = ctx->make (car (a));
            alist.append (elt);
	    ctx->gc_unprotect ();
	    ctx->gc_protect (alist.head ());
	    }
        arglist = cdr (arglist);
        }

    alist.append (car (arglist));
    ctx->gc_unprotect ();

    return alist.head ();
    }

// Destructive concatenation.  Lists are spliced together and 
// will arguments will share structure.  When it is usable, it 
// is faster than append, which must clone all its arguments.

Cell* nconc(Context* ctx, Cell* arglist) {
  Cell::List alist;

  // For each argument list:  If this is the first 
  // list, install it in alist.  Otherwise, splice 
  // it to the tail of alist, by updating pointers.
  // Do not cons anything.

  if (arglist == nil) return nil;

  while(cdr(arglist) != nil) {
    Cell* list_head = car(arglist);
    if (list_head != nil) {
      Cell* list_tail = list_head;
      while(cdr(list_tail) != nil) list_tail = cdr(list_tail);
      
      alist.append_list(list_head, list_tail);
    }
    arglist = cdr(arglist);
  }
  
  alist.append(car(arglist));

  return alist.head();
}

static Cell * member_helper 
    (
    Context *       ctx,
    Cell *          arglist,
    bool            (Cell::* equality) (Cell *)
    )
    {
    Cell *      target = car (arglist);
    Cell *      list = cadr (arglist);

    FOR_EACH (l, list)
        if ((target->*equality) (Cell::car (l)))
            return l;

    return ctx->make_boolean (false);
    }

Cell * memq (Context * ctx, Cell * arglist)
    {
    return member_helper (ctx, arglist, &Cell::eq);
    }

Cell * memv (Context * ctx, Cell * arglist)
    {
    return member_helper (ctx, arglist, &Cell::eqv);
    }

Cell * member (Context * ctx, Cell * arglist)
    {
    return member_helper (ctx, arglist, &Cell::equal);
    }

static Cell * assoc_helper
    (
    Context *       ctx,
    Cell *          arglist,
    bool            (Cell::* equality) (Cell *)
    )
    {
    Cell *      target = car (arglist);
    Cell *      list = cadr (arglist);

    FOR_EACH (l, list)
        if ((target->*equality) (Cell::caar (l)))
            return Cell::car (l);

    return ctx->make_boolean (false);
    }

Cell * assq (Context * ctx, Cell * arglist)
    {
    return assoc_helper (ctx, arglist, &Cell::eq);
    }

Cell * assv (Context * ctx, Cell * arglist)
    {
    return assoc_helper (ctx, arglist, &Cell::eqv);
    }

Cell * assoc (Context * ctx, Cell * arglist)
    {
    return assoc_helper (ctx, arglist, &Cell::equal);
    }

Cell * symbol_to_string (Context * ctx, Cell * arglist)
    {
    return ctx->make_string (car (arglist)->SymbolValue ()->key);
    }

Cell * string_to_symbol (Context * ctx, Cell * arglist)
    {
    return ctx->make_symbol
	(intern_stet (car (arglist)->StringValue ()));
    }

Cell * string_to_number (Context * ctx, Cell * arglist)
    {
    char *	s = car (arglist)->StringValue ();
    char *	t;
    int         base = 0;

    if (s[0] == '\0')
	return ctx->make_boolean (false);
    
    // The standard requires that "." produce #f.  On VxWorks, 
    // strtod would give "0.0", so we must treat "." as a special
    // case.

    if (!strcmp (s, "."))
	return ctx->make_boolean (false);

    if (cdr (arglist) != nil)
        base = cadr (arglist)->IntValue ();

    errno = 0;
    int i = strtol (s, &t, base);

    if (*t != '\0' || errno == ERANGE)
	{
	// It didn't work as an integer, but it might 
	// be floating point.
	
	if (base == 0)
	    {
	    double d = strtod (s, &t);
	    if (*t == '\0')
		return ctx->make_real (d);
	    }
	
        // Scheme considers it an error if we don't consume 
        // the whole string in the conversion.

	return ctx->make_boolean (false);
	}

    return ctx->make_int (i);
    }
    
Cell * string_chars (Context * ctx, Cell * arglist)
    {
    int len = 0;

    FOR_EACH (chptr, arglist)
        ++len;

    Cell * s = ctx->make_string (len);
    char * p = s->StringValue();

    FOR_EACH (chptr, arglist)
        *p++ = car (chptr)->CharValue ();

    *p = '\0';
    return s;
    }

Cell * list_to_string (Context * ctx, Cell * arglist)
    {
    return string_chars (ctx, car (arglist));
    }

Cell * list_to_vector (Context * ctx, Cell * arglist)
    {
    return vector_from_list (ctx, car (arglist));
    }


Cell * string_set (Context * ctx, Cell * arglist)
    {
    // XXX mutability?
    Cell *      pstr = car (arglist);
    size_t      n = pstr->StringLength();
    size_t      ix = cadr (arglist)->IntValue ();

    if (ix < 0 || ix >= n)
        error ("string index out of bounds");

    char *      s = pstr->StringValue ();
    char        ch = caddr (arglist)->CharValue ();

    s [ix] = ch;
    return unspecified;
    }

Cell * string_copy (Context * ctx, Cell * arglist)
    {
    return ctx->make_string (car (arglist)->StringValue ());
    }

Cell * string_fill (Context * ctx, Cell * arglist)
    {
    Cell *      pstr = car (arglist);
    size_t      n = pstr->StringLength ();
    char *      p = pstr->StringValue ();
    char        ch = cadr (arglist)->CharValue();

    for (size_t ix = 0; ix < n; ++ix)
	p[ix] = ch;

    return unspecified;
    }

Cell * string_append (Context * ctx, Cell * arglist)
    {
    sstring     ss;
    size_t      len = 0;

    FOR_EACH (pstr, arglist)
        len += car (pstr)->StringLength ();

    Cell * s = ctx->make_string (len);
    char * p = s->StringValue ();

    FOR_EACH (pstr, arglist)
        {
        strcpy (p, car (pstr)->StringValue ());
        p += car (pstr)->StringLength ();
        }

    *p = '\0';
    return s;
    }

Cell * substring (Context * ctx, Cell * arglist)
    {
    Cell *      pstr = car (arglist);
    int         n = static_cast<int>(pstr->StringLength());
    int         ix = cadr (arglist)->IntValue ();
    int         iy = caddr (arglist)->IntValue ();

    if (ix < 0 || iy < ix || n < iy)
        error ("string index out of bounds");

    int         l = iy - ix;

    return ctx->make_string (pstr->StringValue () + ix, l);
    }


Cell * char_upcase (Context * ctx, Cell * arglist)
    {
    return ctx->make_char (toupper (car (arglist)->CharValue ()));
    }

Cell * char_downcase (Context * ctx, Cell * arglist)
    {
    return ctx->make_char (tolower (car (arglist)->CharValue ()));
    }

Cell * set_cdr (Context * ctx, Cell * arglist)
    {
    Cell::setcdr (car (arglist), cadr (arglist));
    return unspecified;
    }

Cell * set_car (Context * ctx, Cell * arglist)
    {
    Cell::setcar (car (arglist), cadr (arglist));
    return unspecified;
    }

Cell * current_input_port (Context * ctx, Cell * arglist)
    {
    return ctx->current_input ();
    }

Cell * current_output_port (Context * ctx, Cell * arglist)
    {
    return ctx->current_output ();
    }

Cell * close_input_port (Context * ctx, Cell * arglist)
    {
    //car (arglist)->IportValue ().close ();
    return unspecified;
    }

Cell * close_output_port (Context * ctx, Cell * arglist)
    {
    fflush (car (arglist)->OportValue ());
    return unspecified;
    }

Cell * integer_to_char (Context * ctx, Cell * arglist)
    {
    return ctx->make_char (car (arglist)->IntValue () & 255);
    }

Cell * char_to_integer (Context * ctx, Cell * arglist)
    {
    return ctx->make_int ((int) car (arglist)->CharValue ());
    }

Cell * open_input_file (Context * ctx, Cell * arglist)
    {
    return ctx->make_iport (car (arglist)->StringValue ());
    }

Cell * open_output_file (Context * ctx, Cell * arglist)
    {
    return ctx->make_oport (car (arglist)->StringValue ());
    }

Cell * skread (Context * ctx, Cell * arglist)
    {
    Cell * r_nu = ctx->read (iport (ctx, arglist));
    return r_nu == 0 ? &Cell::Eof_Object : r_nu;
    }

Cell * read_char (Context * ctx, Cell * arglist)
    {
    char ch;
    FILE * in = iport (ctx, arglist);

    ch = fgetc (in);

    if (feof (in))
        return &Cell::Eof_Object;

    return ctx->make_char (ch);
    }

Cell * peek_char (Context * ctx, Cell * arglist)
    {
    FILE * in = iport (ctx, arglist);
    int ch = fgetc (in);
    ungetc (ch, in);
    return (ch == -1 ? &Cell::Eof_Object : ctx->make_char (ch));
    }

Cell * eof_object_p (Context * ctx, Cell * arglist)
    {
    return ctx->make_boolean (car (arglist) == &Cell::Eof_Object);
    }

Cell * reverse (Context * ctx, Cell * arglist)
    {
    Cell *      rlist = nil; 

    ctx->gc_protect (rlist);
    FOR_EACH (elt, car (arglist))
	{
        rlist = ctx->cons (car (elt), rlist);
	ctx->gc_unprotect ();
	ctx->gc_protect (rlist);
	}
    ctx->gc_unprotect ();

    return rlist;
    }

Cell * exact_to_inexact (Context * ctx, Cell * arglist)
    {
    return ctx->make_real (asReal (car (arglist)));
    }

Cell * inexact_to_exact (Context * ctx, Cell * arglist)
    {
    Cell *        a = car (arglist);
    if (a->type () == Cell::Int)
	return ctx->make_int (a->IntValue ());
    else
	return ctx->make_int ((int) a->RealValue ());
    }

// Round to nearest int... which would be easy except that the Scheme
// standard insists that we "round toward even" when the fractional 
// part is 0.5!  If it weren't for that, we could get away with 
// floor(d+0.5).  As it is we're left with lots of cases.  This horrible
// if/else nest tries to get the job done quickly. 

double _round (double d)
    {
    double frac_part, int_part;
    frac_part = modf (d, &int_part);
    if (frac_part == 0.0)
	return d;
    if (frac_part > 0.0)
	if (frac_part > 0.5)
	    return int_part + 1.0;
	else if (frac_part == 0.5)
	    if (fmod (int_part, 2.0) != 0)
		return int_part + 1.0;
	    else
		return int_part;
	else
	    return int_part;
    else // frac_part < 0.0
	if (frac_part < -0.5)
	    return int_part - 1.0;
	else if (frac_part == -0.5)
	    if (fmod (int_part, 2.0) != 0)
		return int_part - 1.0;
	    else
		return int_part;
	else
	    return int_part;
    }

// Trunc: not ANSI, so rather than #ifdef it we just provide a 
// version here that works.

double sktrunc (double d)
    {
    double int_part;
    modf (d, &int_part);
    return int_part;
    }

// REAL_F1 and REAL_F2 are `impedance matching' macros that expose
// a C-library transcendental math function (like sin, cos) to 
// scheme. F1 is for one-argument functions, F2 for two arguments.
// The subr-function name chosen is made different from the C 
// library function to avoid name collisions.

#define REAL_F1(sname,cname)						\
    Cell * sname (Context * ctx, Cell * arglist)			\
        {								\
        return ctx->make_real (cname (asReal (car (arglist))));	\
	}

#define REAL_F2(sname,cname)						\
    Cell * sname (Context * ctx, Cell * arglist)			\
        {								\
        return ctx->make_real (cname (asReal (car (arglist)),		\
				      asReal (cadr (arglist))));	\
	}

REAL_F1 (round,  _round)
REAL_F1 (sklog,  log)
REAL_F1 (sksqrt, sqrt)
REAL_F1 (skexp,  exp)
REAL_F1 (sksin,  sin)
REAL_F1 (skcos,  cos)
REAL_F1 (sktan,  tan)
REAL_F1 (skasin, asin)
REAL_F1 (skacos, acos)
REAL_F2 (inexact_expt, pow)
REAL_F1 (skfloor, floor)
REAL_F1 (skceiling, ceil)
REAL_F1 (sktruncate, sktrunc)

static Cell * expt (Context * ctx, Cell * arglist)
    {
    // Scheme requires expt to return an exact result, if 
    // representible, when given exact arguments.  XXX:
    // we should detect overflow, and delegate to the 
    // inexact version in that event.

    if (exact_list (arglist))
	{
	// This is Knuth's Algorithm 4.6.3A (TAoCP 3ed. vol II p. 462).
	// The variable names and labels are as in Knuth's presentation;
	// the interested reader is referred there.

	// A1:

            int Z = car (arglist)->IntValue ();
	    int N = cadr (arglist)->IntValue ();
	    int Y = 1;
	    int even;

	    // handle Scheme's requirement that (expt 0 N) = 1 
	    // if N = 0 and 0 otherwise.  Also, handle the 
	    // trivial Z = 1 case.  If N < 0, that's inexact.
	    
	    if (Z == 0)
		return ctx->make_int (N == 0 ? 1 : 0);
	    if (Z == 1)
		return ctx->make_int (1);
	    if (N == 0)
		return ctx->make_int (0);
	    if (N < 0)
		return inexact_expt (ctx, arglist);

        A2: even = !(N&1);
	    N >>= 1;
	    if (even)
		goto A5;
     // A3: 
            Y = Z * Y;
     // A4: 
            if (N == 0) 
  	        return ctx->make_int (Y);
        A5: Z = Z * Z;
	    goto A2;
	}
    else
	return inexact_expt (ctx, arglist);
    }

static Cell* skatan (Context* ctx, Cell* arglist)
    {
    // If one arg, then compute atan(x), else compute atan2(y,x).

    double x = asReal (car (arglist));

    if (cdr (arglist) != nil)
	{
	double y = asReal (cadr (arglist));
	return ctx->make_real (atan2 (y, x));
	}

    return ctx->make_real (atan (x));
    }

static Cell* logand (Context* ctx, Cell* arglist)
    {
    int     value = ~0;
    
    FOR_EACH (a, arglist)
	value &= car (a)->IntValue ();
    
    return ctx->make_int (value);
    }

static Cell* logbit_p(Context* ctx, Cell* arglist)
    {
    return ctx->make_boolean ((cadr (arglist)->IntValue ()
			       & (1 << car (arglist)->IntValue ())) != 0);
    }

static Cell* logior (Context * ctx, Cell * arglist)
    {
    int     value = 0;

    FOR_EACH (a, arglist)
	value |= car (a)->IntValue ();

    return ctx->make_int (value);
    }

static Cell* logxor (Context * ctx, Cell * arglist)
    {
    int     value = 0;

    FOR_EACH (a, arglist)
	value ^= car (a)->IntValue ();

    return ctx->make_int (value);
    }

static Cell* lognot (Context * ctx, Cell * arglist)
    {
    return ctx->make_int (~ car (arglist)->IntValue ());
    }

static Cell* skerror (Context * ctx, Cell * arglist)
    {
    // Accumulate the arguments as though they were being
    // displayed

    error (car (arglist)->StringValue ());
    return unimplemented; // satisfy compiler
    }
    
static Cell* skgc (Context * ctx, Cell * arglist)
    {
    ctx->gc ();
    return unspecified;
    }

static Cell* sk_impl_type (Context * ctx, Cell * arglist)
    {
    return ctx->make_symbol (intern ("vx-scheme"));
    }

static Cell* vxs_impl_type(Context* ctx, Cell* arglist) {
  static psymbol const i_interp = intern("interp");
  static psymbol const i_vm = intern("vm");
  return ctx->make_symbol(ctx->using_vm() ? i_vm : i_interp);
}

#define __string(x) #x
#define __vstring(v) ("vx-scheme " __string(v))
#define VERSION_STRING __vstring(VERSION)

static Cell* sk_impl_ver (Context * ctx, Cell * arglist)
    {
    return ctx->make_string (VERSION_STRING);
    }

static Cell* sk_impl_page (Context * ctx, Cell * arglist)
    {
    return ctx->make_string ("http://colin-smith.net/vx-scheme/");
    }

static Cell* sk_impl_platform (Context * ctx, Cell * arglist)
    {
    psymbol s;
#if defined(__CYGWIN__)
    s = intern ("cygwin");
#elif defined (VXWORKS)
    s = intern ("VxWorks");
#elif defined (__unix__)
    s = intern ("unix");
#elif defined (WIN32) 
	s = intern ("win32");
#else
    s = intern ("unknown");
#endif
    return ctx->make_symbol (s);
    }   
    
static Cell* file_exists_p (Context * ctx, Cell * arglist)
    {
    FILE * ip = fopen (car (arglist)->StringValue (), "r");
    if (ip != NULL) fclose (ip);
    return ctx->make_boolean (ip != NULL);
    }

//
// PROPERTY LIST SUPPORT 
//

static Cell* put_property (Context * ctx, Cell * arglist) 
    {
    psymbol p = car (arglist)->SymbolValue ();
    psymbol q = cadr (arglist)->SymbolValue ();
    Cell * value = caddr (arglist);

    if (p->plist) 
	for (int ix = 0; ix < p->plist->size (); ++ix) 
	    {
	    Cell * prop = p->plist->get (ix);
	    if (car (prop)->SymbolValue () == q)
		{
		Cell::setcdr (prop, value);  // hit: plist already contains q.
		return unspecified;
		}
	    }
    else
	// time to add the plist.
	p->plist = cellvector::alloc(0);

    // miss: add a new property.  Create the plist if necessary.

    Cell * assoc = ctx->cons (cadr (arglist), value);
    p->plist->push (assoc);
    return unspecified;
    }

static Cell* get_property (Context * ctx, Cell * arglist) 
    {
    psymbol p = car (arglist)->SymbolValue ();
    psymbol q = cadr (arglist)->SymbolValue ();

    if (p->plist)
	for (int ix = 0; ix < p->plist->size (); ++ix) 
	    {
	    Cell * elt = p->plist->get (ix);
	    if (car (elt)->SymbolValue () == q)
		return cdr (elt);
	    }

    return ctx->make_boolean (false);
    }

// Imported from Common Lisp.  Returns #t if the given symbol is 
// bound in the global environment (lexical bindings are not consulted), 
// #f otherwise.

Cell* bound_p(Context* ctx, Cell* arglist) { 
  psymbol s = car(arglist)->SymbolValue();
  return ctx->make_boolean(ctx->find_var(ctx->root(), s, 0) != NULL);
}

// Imported from Common Lisp.  Retrieves the value of a symbol in the 
// global environment (not in any lexical binding).  Errors if the 
// symbol is unbound there.

Cell* symbol_value(Context* ctx, Cell* arglist) { 
  psymbol s = car(arglist)->SymbolValue();
  Cell* value = ctx->find_var(ctx->root(), s, 0);
  if (!value) { 
    error("unbound symbol");
    return unspecified;
  }
  return cdr(value);
}

// Get/Set current working directory

static Cell* sk_getcwd(Context* ctx, Cell* arglist) { 
#ifdef WIN32
  char buf[MAX_PATH];
  GetCurrentDirectory(sizeof(buf), buf);
#else
  char buf[PATH_MAX];
  getcwd(buf, sizeof(buf));
#endif
  return ctx->make_string(buf);
}

static Cell* sk_chdir(Context* ctx, Cell* arglist) { 
  const char* dir = car(arglist)->StringValue();
#ifdef WIN32
  bool ok = SetCurrentDirectory(dir) == TRUE;
#else
  bool ok = chdir(dir) == 0;
#endif
  return ctx->make_boolean(ok);
}

//
// INITIALIZATION
//

void Context::provision ()
    {
    struct
        {
        const char * n;
        subr_f       i;
        } subr [] =
        {
            { "*",                              times },
            { "+",                              skplus },
            { "-",                              skminus },
	    { "/",                              divide },
            { "<",                              lt },
            { "<=",                             le },
            { "=",                              number_equal },
            { ">",                              gt },
            { ">=",                             ge },
            { "abs",                            skabs },
            { "append",                         append },
	    { "acos",                           skacos },
	    { "asin",                           skasin },
            { "assoc",                          assoc },
            { "assq",                           assq },
            { "assv",                           assv },
	    { "atan",                           skatan },
            { "boolean?",                       boolean_p },
	    { "caaaar",                         caaaar },
	    { "caaadr",                         caaadr },
	    { "caaar",                          caaar },  
	    { "caadar",                         caadar },
	    { "caaddr",                         caaddr },
	    { "caadr",                          caadr },  
	    { "caar",                           caar },    
	    { "cadaar",                         cadaar },
	    { "cadadr",                         cadadr },
	    { "cadar",                          cadar },  
	    { "caddar",                         caddar },
	    { "cadddr",                         cadddr },
	    { "caddr",                          caddr },  
	    { "cadr",                           cadr },    
	    { "car",                            car },
	    { "cdaaar",                         cdaaar },
	    { "cdaadr",                         cdaadr },
	    { "cdaar",                          cdaar },  
	    { "cdadar",                         cdadar },
	    { "cdaddr",                         cdaddr },
	    { "cdadr",                          cdadr },  
	    { "cdar",                           cdar },    
	    { "cddaar",                         cddaar },
	    { "cddadr",                         cddadr },
	    { "cddar",                          cddar },  
	    { "cdddar",                         cdddar },
	    { "cddddr",                         cddddr },
	    { "cdddr",                          cdddr },  
	    { "cddr",                           cddr },    
	    { "cdr",                            cdr },      
            { "ceiling",                        skceiling },
            { "char->integer",                  char_to_integer },
            { "char-alphabetic?",               alphabetic_p },
            { "char-ci<=?",                     char_le_ci },
            { "char-ci<?",                      char_lt_ci },
            { "char-ci=?",                      char_eq_ci },
            { "char-ci>=?",                     char_ge_ci },
            { "char-ci>?",                      char_gt_ci },
            { "char-downcase",                  char_downcase },
            { "char-lower-case?",               lower_case_p },
            { "char-numeric?",                  numeric_p },
            { "char-upcase",                    char_upcase },
            { "char-upper-case?",               upper_case_p },
            { "char-whitespace?",               whitespace_p },
            { "char<=?",                        char_le },
            { "char<?",                         char_lt },
            { "char=?",                         char_eq },
            { "char>=?",                        char_ge },
            { "char>?",                         char_gt },
            { "char?",                          char_p },
            { "close-input-port",               close_input_port },
            { "close-output-port",              close_output_port },
            { "complex?",                       complex_p },
            { "cons",                           skcons },
	    { "cos",                            skcos },
            { "current-input-port",             current_input_port },
            { "current-output-port",            current_output_port },
            { "display",                        display },
            { "eof-object?",                    eof_object_p },
	    { "error",                          skerror }, 
            { "eq?",                            eq },
            { "equal?",                         equal_p },
            { "eqv?",                           eqv },
            { "even?",                          even_p },
            { "exact?",                         exact_p },
	    { "exact->inexact",                 exact_to_inexact },
	    { "exp",                            skexp },
	    { "expt",                           expt },
	    { "floor",                          skfloor },
	    { "inexact->exact",                 inexact_to_exact }, 
            { "gcd",                            gcd },
            { "inexact?",                       inexact_p },
            { "input-port?",                    input_p },
            { "integer->char",                  integer_to_char },
            { "integer?",                       integer_p },
            { "lcm",                            lcm },
            { "length",                         length },
            { "list",                           sklist },
            { "list->string",                   list_to_string },
            { "list->vector",                   list_to_vector },
            { "list-ref",                       list_ref },
            { "list?",                          list_p },
	    { "log",                            sklog },
	    { "logand",                         logand }, 
	    { "logbit?",                        logbit_p }, 
	    { "logior",                         logior }, 
	    { "lognot",                         lognot }, 
	    { "logxor",                         logxor }, 
            { "make-string",                    skmake_string },
            { "make-vector",                    skmake_vector },
            { "max",                            skmax },
            { "member",                         member },
            { "memq",                           memq },
            { "memv",                           memv },
            { "min",                            skmin },
            { "modulo",                         modulo },
            { "negative?",                      negative_p },
            { "newline",                        newline },
            { "not",                            sknot },
            { "null?",                          null_p },
            { "number->string",                 number_to_string },
            { "number?",                        number_p },
            { "odd?",                           odd_p },
            { "open-input-file",                open_input_file },
            { "open-output-file",               open_output_file },
            { "output-port?",                   output_p },
            { "pair?",                          pair_p },
            { "peek-char",                      peek_char },
            { "positive?",                      positive_p },
            { "procedure?",                     procedure_p },
            { "quotient",                       quotient },
            { "rational?",                      rational_p },
            { "read",                           skread },
            { "read-char",                      read_char },
            { "real?",                          real_p },
            { "remainder",                      remainder },
            { "reverse",                        reverse },
	    { "round",                          round }, 
            { "set-car!",                       set_car },
            { "set-cdr!",                       set_cdr },
	    { "sin",                            sksin },
	    { "sqrt",                           sksqrt },
            { "string",                         string_chars },
            { "string-copy",                    string_copy },          // R5
            { "string-fill!",                   string_fill },          // R5
            { "string->list",                   string_to_list },
            { "string->number",                 string_to_number },
            { "string->symbol",                 string_to_symbol },
            { "string-append",                  string_append },
            { "string-ci<=?",                   string_le_ci },
            { "string-ci<?",                    string_lt_ci },
            { "string-ci=?",                    string_eq_ci },
            { "string-ci>=?",                   string_ge_ci },
            { "string-ci>?",                    string_gt_ci },
            { "string-length",                  string_length },
            { "string-ref",                     string_ref },
            { "string-set!",                    string_set },
            { "string<=?",                      string_le },
            { "string<?",                       string_lt },
            { "string=?",                       string_eq },
            { "string>=?",                      string_ge },
            { "string>?",                       string_gt },
            { "string?",                        string_p },
            { "substring",                      substring },
            { "symbol->string",                 symbol_to_string },
            { "symbol?",                        symbol_p },
	    { "tan",                            sktan }, 
	    { "truncate",                       sktruncate }, 
            { "vector",                         vector_from_list },
            { "vector->list",                   vector_to_list },
	    { "vector-fill!",                   vector_fill },          // R5
            { "vector-length",                  vector_length },
            { "vector-ref",                     vector_ref },
            { "vector-set!",                    vector_set },
            { "vector?",                        vector_p },
            { "write",                          write },
            { "write-char",                     write_char },
            { "zero?",                          zero_p },
	    //----------------------------------------------------------------
	    //
	    // The following functions are not part of the spec, but 
	    // are peculiar to this implementation.
	    //
            { "bound?",                           bound_p },
	    { "chdir",                            sk_chdir },
            { "display*",                         display_star },
	    { "put",                              put_property },
	    { "get",                              get_property },
	    { "file-exists?",                     file_exists_p },
	    { "gc",                               skgc },
	    { "getcwd",                           sk_getcwd },
            { "nconc",                            nconc },
            { "primitive-procedure?",             primitive_procedure_p },
	    { "scheme-implementation-type",       sk_impl_type },
	    { "vx-scheme-implementation-type",    vxs_impl_type },
	    { "scheme-implementation-version",    sk_impl_ver },
	    { "scheme-implementation-home-page",  sk_impl_page },
	    { "scheme-implementation-platform",   sk_impl_platform },
            { "symbol-value",                     symbol_value },
            { "vector-push!",                     vector_push },
            { "vector-pop!",                      vector_pop },
            { "vector-unshift!",                  vector_unshift }, 
            { "vector-shift!",                    vector_shift },
	    //
	    //----------------------------------------------------------------	    
        };

    for (unsigned int ix = 0; ix < sizeof (subr) / sizeof (*subr); ++ix)
        bind_subr (subr[ix].n, subr[ix].i);

    // Source code in SICP uses the symbols `true' and `false' for 
    // boolean values instead of #t and #f as suggested by RxRS.
    // We add these symbol-bindings here.

#define BIND_VARIABLE(var,val)					\
    set_var (envt, intern (var), val)

    BIND_VARIABLE ("true", make_boolean (true));
    BIND_VARIABLE ("false", make_boolean (false));
    BIND_VARIABLE ("*version*", make_string (VERSION_STRING));

    // Load extension bindings.

    SchemeExtension::RunInstall (this, envt);
    }

void Context::bind_subr (const char * name, subr_f subr)
    {
    psymbol s = intern (name);
    set_var (envt, s, make_subr (subr, name));
    }

cellvector* SchemeExtension::extensions = 0;
SchemeExtension* SchemeExtension::main = 0;

void SchemeExtension::Register (SchemeExtension * ext) { 
  if (!extensions)
    extensions = new cellvector ();

  extensions->push (reinterpret_cast <Cell *> (ext));
}

void SchemeExtension::RunInstall (Context * ctx, Cell * envt) {
  if (!extensions)
    return;
  for (int ix = 0; ix < extensions->size(); ++ix) { 
    SchemeExtension * extension =
      reinterpret_cast <SchemeExtension *> (extensions->get (ix));
    extension->Install (ctx, envt);
  }
}
