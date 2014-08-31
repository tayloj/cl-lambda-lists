# cl-parsing-lambda-lists

CL-parsing-lambda-lists is a lightweight library for parsing the various
types of lambda lists specified in Common Lisp.

This functionality is present in any implementation of Common Lisp
(since the compiler has to be able to process lambda lists), but has no
standardized interface.  CL-parsing-lambda-lists provides such an interface
that handles all the types of lambda lists described in
[**&sect;3.4 Lambda Lists**](http://www.lispworks.com/documentation/HyperSpec/Body/03_d.htm)
of the HyperSpec, as well as simple extensible interface for defining new types 
of lambda lists.

## Alternatives

Every Common Lisp implementation must parse the variety of types of lambda lists, 
and may make an API for that available.  That said, other authors have implemented
source processing tools as well.  A 2008 comp.lang.lisp thread,
[parsing lambda lists](https://groups.google.com/d/msg/comp.lang.lisp/0kp4c4VA-Ow/5F2rfAFACc0J)
mentions some.  Some alternatives:

* Pascal J. Bourguignon's [lisp-sexp](https://gitorious.org/com-informatimago/com-informatimago/source/c1d96bb4fcbe99ef1128bd2a94800fbc4e48529c:common-lisp/lisp-sexp).
  This is a very CLOS-based approach.
* [Alexandria](http://common-lisp.net/project/alexandria/) has **parse-body**
  and **parse-ordinary-lambda-list**.
* SLIME's **swank::decode-arglist** has a structure-based representation of argument lists, 
  but treats &whole and &environment as "known junk."  SLIME is presumably more concerned
  with presenting useful prompts to developers.
