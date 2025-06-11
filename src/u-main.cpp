//----------------------------------------------------------------------
// vx-scheme : Scheme interpreter.
// Copyright (c) 2002,2003,2006 and onwards Colin Smith.
//
// You may distribute under the terms of the Artistic License, 
// as specified in the LICENSE file.
//
// u-main.cpp : startup code for UNIX, Cygwin or Win32 environments.

#include <stdio.h>
#include <time.h>
#include "vx-scheme.h"
#ifndef WIN32
#include <sys/time.h>
#include <unistd.h>
#else
#include <windows.h>
#endif
#include <setjmp.h>

static jmp_buf jb;
static bool jmpbuf_set = false;

//----------------------------------------------------------------------------
//
// OS-SPECIFIC FEATURES
//
// This area fills in definitions for OS-specific features named 
// in class OS.  
//

double OS::get_time() {
  double sec;
#ifdef WIN32
  FILETIME filetime;
  GetSystemTimeAsFileTime(&filetime);
  ULARGE_INTEGER ul;
  ul.HighPart = filetime.dwHighDateTime;
  ul.LowPart = filetime.dwLowDateTime; 
  // FILETIMES  are in 100ns units.
  sec = ul.QuadPart / 100000000.;
  sec += ul.QuadPart % 100000000;
#else
  struct timeval t;
  gettimeofday (&t, 0);
  sec = t.tv_sec;
  sec += t.tv_usec / 1e6;
#endif
  return sec;
}

unsigned int OS::flags ()
    {
    static bool env_checked = false;
    static unsigned int f = 0;
    if (! env_checked)
	{
	char * c;
	if ((c = getenv ("T")) != NULL)
	    f = strtol (c, 0, 0);
	env_checked = true;
	}

    return f;
    }

bool OS::interactive (int fd)
    {
    return isatty (fd) != 0;
    }

Cell * OS::undef (Context * ctx, const char * name)
    {
    return 0;
    }

void OS::exception (const char * s) {
  if (jmpbuf_set) longjmp (jb, reinterpret_cast <int> (s));
  fputs(s, stderr);
  fputs("\n", stderr);
  exit(1);
}

void interact (Context * ctx)
    {
    bool interactive = OS::interactive(0);

    while (ctx->read_eval_print (stdin, stdout, interactive))
	;
    
    if (OS::flag (DEBUG_MEMSTATS_AT_EXIT)) { 
	ctx->print_mem_stats (stdout);
        Cell::stats ();
    }


    exit (0);
    }

int main (int argc, char **argv) {
  const char *jv;
  Context ctx;
  Cell* scheme_argv = ctx.gc_protect(ctx.make_vector(0));
  cellvector* argvec = scheme_argv->VectorValue();

  --argc;
  ++argv;

  while (argc > 0) { 
    argvec->push(ctx.make_string(*argv));
    --argc;
    ++argv;
  }

  // Establish *argv* in global environment

  ctx.set_var(intern("*argv*"), scheme_argv, 0);
  ctx.gc_unprotect();

  // See if we have a canned main procedure.

  Cell* result = ctx.RunMain();
  if (result) { 
    if (result != unspecified) result->write(stdout); 
  } else { 
    // Interact

    while (1) { 
      if ((jv = reinterpret_cast <const char *> (setjmp (jb))) == 0) { 
	jmpbuf_set = true;
	interact (&ctx);
      } else { 
	fprintf (stderr, "caught: %s\n", jv);
      }
    }
  }
}

