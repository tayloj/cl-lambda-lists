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
