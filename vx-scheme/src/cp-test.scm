(load "compiler.scm")
(load "simulator.scm")

;(compile '(lambda () 1))

(sim-load "library.scm")
(display "foo\n")
(sim-load "r4rstest.scm")
(display "bar\n")
;
;(display "setup complete\n")
;(sim-load "r4rstest.scm")




; Note: the "add3" test in r4rstest.scm fails because we open-code +,
; so the redefinition of + is not effective in compiled code.  What 
; to do?

;(sim-load "../testcases/maze.scm")
;(sim-load "../testcases/scheme.scm")


;; run an expression in the compiler's execution path
  
;;(define (compile-file file) 
;  (let ((input (open-input-file file)))
;    (do ((form (read input) (read input)))
;	((eof-object? form) 'ok)
;      (compile form))))
;
;
;(define apply-code '#((code #((apply.)
;			      (return)))
;		      (proc)
;		      (return)))
;
;apply-code
;(link2 apply-code)
;
;(define apply (execute (link2 apply-code)))
;		      
;apply
;
;(display "---\n")
;(display* "->" (comp-run '(apply list '(1 2 3 4 5))) "<-\n")
;(display* "->" (comp-run '(apply + '(1 2 3 4 5))) "<-\n")
;(display* "->" (comp-run '(apply list 'a 'b '(1 2 3 4 5))) "<-\n")
;(display* "->" (comp-run '(apply list 'c '(1 2 3 4 5))) "<-\n")
;(display "---\n")
;
;;(define (g v) (lambda (u) (+ u v)))
;;(display ((g 4) 7))
;
;
;(comp-run '(define (f v) (lambda (u) (+ u v))))
;
;;class VmExtension : SchemeExtension
;;{ 
;;public:
;;  VmExtension () { 
;;    Register (this);
;;  }
;;  virtual void install (Context * ctx, Cell * envt) { 
;;    static struct { 
;;      const char* name;
;;      subr_f subr;
;;    } bindings[] = { 
;;      { "make-instruction",               make_instruction }, 
;;      { "make-compiled-procedure",        make_compiled_procedure },
;;      { "write-compiled-procedure",       write_compiled_procedure },
;;      { "disassemble",                    disassemble },
;;      { "execute",                        execute },
;;    };
;;    static const unsigned int n_bindings = sizeof(bindings)/sizeof(*bindings);
;;    for (unsigned int ix = 0; ix < n_bindings; ++ix) { 
;;      ctx->bind_subr(bindings[ix].name, bindings[ix].subr);
;;    }
;;    // Attach VM execution function to context, so the interpreter may
;;    // invoke compiled procedures.
;;    ctx->vm_execute = &Context::execute;
;;  }
;;};
;;
;
;;(comp-run '((f 4) 7))
;;(write-compiled-procedure compile "compile")
;
;;(comp-load "../testcases/maze.scm")
;
;(time (comp-load "../testcases/pi.scm"))
;;(time (comp-load "../testcases/boyer.scm"))
;;(time (comp-load "../testcases/maze.scm"))
;;(comp-load "compiler.scm")
;;(time (compile-file "compiler.scm"))
;;(time (comp-load "library.scm"))
;;(comp-load "../testcases/dderiv.scm")
;;(comp-load "../testcases/puzzle.scm")
;;(comp-load "../testcases/ack.scm")
;;(comp-load "library.scm")
;;(comp-load "r4rstest.scm")
;
;
;
