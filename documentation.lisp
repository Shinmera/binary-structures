(in-package #:org.shirakumo.binary-structures)

;; toolkit.lisp
(docs:define-docs
  (function unspecific-p
    "Used to determine whether a value designates a static quantity.

If any of the THINGS is NIL, returns *
If any of the THINGS is not a number, returns the thing

Generally, * is used to designate an unknown quantity. Values that
aren't a number are runtime expressions that are unknown quantities at
compile-time, to be resolved later."))

;; protocol.lisp
(docs:define-docs
  (type binary-structures-condition
    "Supertype for all conditions signalled by this library.

See NO-SUCH-IO-TYPE
See END-OF-STORAGE
See UNKNOWN-VALUE
See NO-SUCH-SLOT")

  (type no-such-io-type
    "Error signalled when an undefined IO type is referenced.

See DESIGNATOR
See IO-TYPE (type)
See IO-TYPE
See BINARY-STRUCTURES-CONDITION (type)")

  (function designator
    "Returns the designator of the io type.

See NO-SUCH-IO-TYPE")

  (type end-of-storage
    "Error signalled when the backend is not large enough to contain the requested type.

See INDEX
See END
See BINARY-STRUCTURES-CONDITION (type)")

  (function index
    "Returns the index at which the storage condition occurred.

This may be NIL if the index is not known.

See END-OF-STORAGE (type)")

  (function end
    "Returns the maximum index of the storage backend.

This may be NIL if the index is not known.

See END-OF-STORAGE (type)")

  (type unknown-value
    "Error signalled when a value is encoded or decoded that was not declared in the type.

See VALUE
See ACCEPTED
See BINARY-STRUCTURES-CONDITION (type)")

  (function value
    "Returns the value that failed the encoding/decoding.

See UNKNOWN-VALUE (type)")

  (function accepted
    "Returns the set of values that would be acceptable.

See UNKNOWN-VALUE (type)")

  (function no-such-slot
    "Error signalled when a slot is referenced that does not exist on the structure.

See NAME
See STRUCT
See BINARY-STRUCTURES-CONDITION (type)")

  (function name
    "Returns the name associated with the object.

See NO-SUCH-SLOT (type)
See IO-STRUCTURE (type)
See IO-STRUCTURE-SLOT (type)")

  (function struct
    "Returns the structure that the slot was searched in.

See NO-SUCH-SLOT (type)")

  (type io-backend
    "Superclass for all storage backends supported by this library.

A backend must implement methods for the functions READ-DEFUN,
WRITE-DEFUN, CALL-READ-FORM, CALL-WRITE-FORM, READ-FORM, WRITE-FORM,
INDEX-FORM, and SEEK-FORM to emit the correct forms to decode or
encode io types as appropriate for the backend.

The OFFSET slot is used to keep track of the current statically
accumulated octet index as generated by emitted forms. This static
slot offset is used to eliminate the spurious seeking operations.

See UNSPECIFIC-P
See OFFSET
See READ-DEFUN
See WRITE-DEFUN
See CALL-READ-FORM
See CALL-WRITE-FORM
See READ-FORM
See WRITE-FORM
See INDEX-FORM
See SEEK-FORM
See DEFINE-IO-BACKEND
See LIST-IO-BACKENDS
See DEFINE-IO-DISPATCH
See DEFINE-IO-BACKEND-FUNCTION
See DEFINE-IO-FUNCTIONS")

  (function offset
    "Accesses the octet index at which the object resides.

See IO-BACKEND
See IO-STRUCTURE-SLOT")

  (type io-type
    "Superclass for all known type classes for IO.

See READ-DEFUN
See WRITE-DEFUN
See CALL-READ-FORM
See CALL-WRITE-FORM
See READ-FORM
See WRITE-FORM
See LISP-TYPE
See DEFAULT-VALUE
See OCTET-SIZE
See READ-DEFUN
See WRITE-DEFUN
See CALL-READ-FORM
See CALL-WRITE-FORM
See READ-FORM
See WRITE-FORM
See INITARGS
See PARSE-IO-TYPE
See IO-TYPE
See DEFINE-IO-TYPE
See LIST-IO-TYPES
See DEFINE-IO-DISPATCH-FUNCTION
See DEFINE-IO-BACKEND-FUNCTION
See DEFINE-IO-FUNCTIONS")

  (function read-defun
    "Generate a reader function definition form for the given IO-TYPE under the IO-BACKEND.

This does not necessarily have to be a direct DEFUN form, but must
when evaluated have the effect of defining such a function.

Within the function definition, a call to READ-FORM of the given
IO-TYPE and IO-BACKEND must occur to provide the function's
logic.

See READ-FORM
See IO-TYPE
See IO-BACKEND")

  (function write-defun
    "Generate a writer function definition form for the given IO-TYPE under the IO-BACKEND.

This does not necessarily have to be a direct DEFUN form, but must
when evaluated have the effect of defining such a function.

Within the function definition, a call to WRITE-FORM of the given
IO-TYPE and IO-BACKEND must occur to provide the function's
logic.

See WRITE-FORM
See IO-TYPE
See IO-BACKEND")

  (function call-read-form
    "Generate a form to call the reader function associated with the given IO-TYPE under the IO-BACKEND.

The form may only be valid within the context established by
READ-DEFUN of the same IO-BACKEND and may thus make use of local
variables and functions.

See READ-DEFUN
See IO-BACKEND
See IO-TYPE")

  (function call-write-form
    "Generate a form to call the writer function associated with the given IO-TYPE under the IO-BACKEND for the VALUE-VARIABLE.

The form may only be valid within the context established by
WRITE-DEFUN of the same IO-BACKEND and may thus make use of local
variables and functions.

See WRITE-DEFUN
See IO-BACKEND
See IO-TYPE")

  (function read-form
    "Generate a form suitable for reading the given IO-TYPE under the IO-BACKEND.

The form may only be valid within the context established by
READ-DEFUN of the same IO-BACKEND and may thus make use of local
variables and functions.

It is expected that backends add methods to this function for all
primitive types, and methods for compound types that may be more
efficient than the generic variants provided by the default IO-BACKEND
method.

See READ-DEFUN
See IO-BACKEND
See IO-TYPE")

  (function write-form
    "Generate a form suitable for writing the value contained in VALUE-VARIABLE as the given IO-TYPE under the IO-BACKEND.

The form may only be valid within the context established by
WRITE-DEFUN of the same IO-BACKEND and may thus make use of local
variables and functions.

It is expected that backends add methods to this function for all
primitive types, and methods for compound types that may be more
efficient than the generic variants provided by the default IO-BACKEND
method.

See WRITE-DEFUN
See IO-BACKEND
See IO-TYPE")

  (function index-form
    "Generate a form that returns the current index.

The form may only be valid within the context established by
READ-DEFUN or WRITE-DEFUN of the same IO-BACKEND and may thus make use
of local variables and functions.

See READ-DEFUN
See WRITE-DEFUN
See IO-BACKEND")

  (function seek-form
    "Generate a form that seeks to the requested offset.

OFFSET may be a number or a form. If the backend determines that
seeking at this point in the generation to the requested offset is
unnecessary, such as when OFFSET is a number and the current static
offset as determined by the backend is the same, NIL may also be
returned.

The form may only be valid within the context established by
READ-DEFUN or WRITE-DEFUN of the same IO-BACKEND and may thus make use
of local variables and functions.

See READ-DEFUN
See WRITE-DEFUN
See IO-BACKEND")

  (function lisp-type
    "Returns a suitable lisp type expression for the given io-type.

See IO-TYPE (type)")

  (function default-value
    "Returns a suitable lisp default value form for the given io-type.

See IO-TYPE (type)")

  (function octet-size
    "Returns an octet-size estimate for the given io-type.

Note that this may return an unspecific size.

See UNSPECIFIC-P
See IO-TYPE (type)")

  (function initargs
    "Returns a list of initargs to reconstruct this type class instance.

See IO-TYPE (type)")

  (function parse-io-type
    "Parse the given type designator with the arguments to an IO-TYPE instance.

See IO-TYPE (type)")

  (function io-type
    "Access the IO-TYPE instance associated with the given name.

When reading:
If there is no type of the given name and ERRORP is non-NIL, a
condition of type NO-SUCH-IO-TYPE is signalled. The CONTINUE restart
will cause a TOP-TYPE to be returned. The USE-VALUE restart may be
used to supply an alternate type instance. If ERRORP is NIL, NIL is
returned instead.

When setting:
If the value is NIL, the name is removed from the IO-TYPE directory.

See IO-TYPE (type)
See DEFINE-IO-TYPE")

  (function define-io-type
    "Define a new IO-TYPE instance under the given name.

The io type will be available at compile-time as well as run-time.

See IO-TYPE (type)
See IO-TYPE")

  (function list-io-types
    "Returns a list of names of all known IO-TYPE instances.

See DEFINE-IO-TYPE
See IO-TYPE
See IO-TYPE (type)")

  (function list-io-backends
    "Returns a list of names of all known IO-BACKEND types.

See IO-BACKEND (type)
See DEFINE-IO-BACKEND")

  (function define-io-backend
    "Defines a new IO-BACKEND class and registers it.

See IO-BACKEND (type)
See LIST-IO-BACKENDS")

  (function define-io-type-parser
    "Defines an IO type designator parsing function for the given type name.

This will cause both DESIGNATOR and (DESIGNATOR ...) to match as type
designators and invoke your function to parse it to an IO-TYPE
instance.

See PARSE-IO-TYPE")

  (function define-io-backend-function
    "Generate a function that parses the given IO-TYPE under the given IO-BACKEND.

TYPE may be either :READ or :WRITE according to which the
corresponding function is generated.

See READ-DEFUN
See WRITE-DEFUN
See DEFINE-IO-FUNCTIONS")

  (function define-io-dispatch-function
    "Generate a dispatch function that parses the given IO-TYPE.

TYPE may be either :READ or :WRITE according to which the
corresponding function is generated.

See DEFINE-IO-DISPATCH
See DEFINE-IO-FUNCTIONS")

  (function define-io-functions
    "Generate all possible parsing functions for the given IO-TYPE.

See DEFINE-IO-BACKEND-FUNCTION
See DEFINE-IO-DISPATCH-FUNCTION")

  (function define-io-alias
    "Defines a type designator alias.

This is equivalent to
  (define-io-parser name () (parse-io-type expansion))

See DEFINE-IO-PARSER")

  (type top-type
    "Type class for an ambiguous IO-TYPE.

See IO-TYPE (type)")

  (type numeric-type
    "Type class for numerical IO-TYPEs.

Every numeric type has a fixed OCTET-SIZE and a byte ORDER, which
defaults to :LITTLE-ENDIAN.

See OCTET-SIZE
See ORDER
See IO-TYPE (type)")

  (function order
    "Accesses the byte order of the type.

May be either :LITTLE-ENDIAN or :BIG-ENDIAN.

See NUMERIC-TYPE (type)")

  (type io-integer
    "Type class for integer IO-TYPEs.

If an integer is SIGNED-P, then its representation uses two's
complement to represent the sign.

This type parses from the representation

  (INTEGER [OCTET-SIZE] [:SIGNED/:UNSIGNED] [ORDER])

The following shorthands are also defined:

  [S/U] INT [8/16/32/64/128] [/-BE]

Where UINT16-BE designates an \"unsigned 16-bit integer in
big-endian\" and SINT32 designates a \"signed 32-bit integer in
little-endian\".

See NUMERIC-TYPE (type)")

  (function signed-p
    "Accesses whether the integer type is signed or not.

See IO-INTEGER (type)")

  (type io-float
    "Type class for floating point IO-TYPEs.

Only IEEE floating point values are represented, depending on
implementation support with 2, 4, 8, and 16 octets.

This type parses from the representation

  (FLOAT [OCTET-SIZE])

With OCTET-SIZE defaulting to 4.

The following shorthands are also defined:

  FLOAT [16/32/64/128] [/-BE]

Where FLOAT16-BE designates a \"16-bit short-float in
big-endian\" and FLOAT32 designates a \"32-bit single-float in
little-endian\".

See NUMERIC-TYPE (type)")

  (type io-vector
    "Type class to represent a sequence of other values.

This type parses from the representation

  (VECTOR ELEMENT-TYPE [ELEMENT-COUNT] [ELEMENT-OFFSET])

See ELEMENT-TYPE
See ELEMENT-COUNT
See ELEMENT-OFFSET
See STRIDE
See IO-TYPE (type)")

  (function element-type
    "Accesses the type of each element in an IO-VECTOR.

The type should be some IO-TYPE designator and must be constant for
every element.

See IO-VECTOR (type)")

  (function element-count
    "Accesses the length of the IO-VECTOR.

This is a quantity (see SPECIFIC-P) and may thus be statically known,
or be a form to determine the length of the vector at runtime.

If the form is the name of another IO-TYPE, the IO-TYPE is
read/written to obtain the actual count.

Special attention must be given if the ELEMENT-COUNT of an IO-VECTOR
is set to *, in which case the vector is said to consume all remaining
available space in the storage. For IO-STRINGS, the * designates that
the string should be null-terminated.

See IO-VECTOR (type)
See IO-STRING (type)")

  (function element-offset
    "Accesses the offset of each element in the IO-VECTOR.

This may be a form that, when evaluated under a lexical
environment with I bound to the current index, yields an index at
which the element starts. STRIDE may be an expression that yields the
number of octets between any two elements in the sequence.

See IO-VECTOR (type)")

  (function stride
    "Accesses the stride between two elements in the IO-VECTOR.

This may be a form that, when evaluated, yields an integer
representing the number of octets between the start of any two values
in the sequence.

See IO-VECTOR (type)")

  (type io-string
    "Type class to represent a character string.

This type parses from the representation

  (STRING ELEMENT-COUNT [ENCODING] [NULL-TERMINATED-P])

The encoding defaults to UTF-8.

See ELEMENT-COUNT
See ENCODING
See NULL-TERMINATED-P
See IO-TYPE (type)")

  (function null-terminated-p
    "Accesses whether the string is terminated by nulls or not.

This will truncate such strings even when they have a specific length
that goes beyond the first null byte.

See IO-STRING (type)")

  (function encoding
    "Accesses the encoding of the string.

The encoding also defines the byte order. Please see BABEL for the
available encoding schemes.

See IO-STRING (type)")

  (type io-case
    "Representation of an enum-like value dispatch.

Each CASE must be a possible value of the VALUE-TYPE to match against
and either a constant to use as the associated value or an IO-TYPE
designator to describe how to parse the associated value.

This type parses from the representation

  (CASE VALUE-TYPE (MATCH FORM)*)

For example:

  (case uint8
    (1 :linux)
    (2 :windows)
    (3 :darwin))

Defines a standard enum-like association. However, you may also use
other IO-TYPEs in the FORM:

  (case uint8
    (1 image)
    (2 music-track)
    (3 video))

Note that in order for the inverse writing operation to work, the
order of the entries matters and must usually be the reverse of what
would naturally be listed. Meaning that if SUB is a subtype of SUPER,
then SUPER must be listed /after/ SUB, or else SUPER will match both,
causing SUB to never be hit.

See VALUE-TYPE
See CASES
See IO-TYPECASE (type)
See IO-TYPE (type)")

  (function value-type
    "Accesses the IO-TYPE this type encapsulates.

See IO-CASE (type)
See IO-STRUCTURE (type)
See IO-STRUCTURE-SLOT (type)")

  (function cases
    "Accesses the list of case forms.

Each entry in the list must be composed of a MATCH and a FORM.

See IO-CASE (type)
See IO-TYPECASE (type)")

  (type io-value
    "Representation of some form used purely for its runtime value.

This always has an unknown real type and size, and is the default type
representation for parsing any CONS-based IO-TYPE.

Meaning: (parse-io-type '(foo)) ==> IO-VALUE

See FORM
See IO-TYPE (type)")

  (function form
    "Accesses the form of the IO-VALUE.

See IO-VALUE (type)")

  (type io-typecase
    "Representation of some runtime type dispatch.

This is useful to implement optional segments in an IO-TYPE or to
dispatch parsing based on an earlier part of the parsing state.

See CASES
See IO-VALUE (type)")

  (type io-structure
    "Representation of some structure composed out of multiple values.

See VALUE-TYPE
See CONSTRUCTOR
See SLOTS
See NAME
See FIND-SLOT
See DEFINE-IO-STRUCTURE
See IO-TYPE (type)")

  (function constructor
    "Accesses the name of the lisp structure constructor function.

See IO-STRUCTURE (type)")

  (function slots
    "Accesses the list of IO-STRUCTURE-SLOTs that define the structure's members.

See IO-STRUCTURE (type)
See IO-STRUCTURE-SLOT (type)")

  (function find-slot
    "Returns the slot with the given name on the structure.

If no slot with the requested name exists on the structure, an error
of type NO-SUCH-SLOT is signalled.

See IO-STRUCTURE (type)
See IO-STRUCTURE-SLOT (type)")

  (type io-structure-slot
    "Representation of a single slot in an IO-STRUCTURE.

See VALUE-TYPE
See OCTET-SIZE
See OFFSET
See NAME
See IO-STRUCTURE-MAGIC (type)
See IO-STRUCTURE (type)")

  (type io-structure-magic
    "Representation of a constant / \"magic\" value within a structure or some padding.

If the VALUE-TYPE of the slot is known, a value of that type is
read. If the DEFAULT-VALUE is also known, the value is compared and if
not EQUALP, an error of type MAGIC-VALUE-MISMATCHED is signalled. If
the DEFAULT-VALUE is known but no explicit VALUE-TYPE is set, the
DEFAULT-VALUE must be a STRING or OCTET-VECTOR, a sequence of
equivalent length of which is parsed for comparison. If a VALUE-TYPE
is known but no DEFAULT-VALUE, the value is simply discarded.

See IO-STRUCTURE-SLOT (type)")

  (function define-io-structure
    "Define an IO-STRUCTURE type instance.

NAME may either be a symbol naming the structure and type, or a list
of that name and defstruct options for the resulting structure
definition.

BODY may be a number of slot definitions preceded by additional
keyword arguments to the resulting structure type definition.

This will do the following:
  1. Define a structure via DEFSTRUCT corresponding to the specified
     name and slots.
  2. Define an IO-TYPE corresponding to the specified name and slots.
  3. Define all associated functions via DEFINE-IO-FUNCTIONS.

Each slot in the body may be one of the following:
  - An :INCLUDE expression, which causes the specified structure's
    slots to be spliced in. If the :INCLUDE expression occurs first,
    the generated STRUCTURE-CLASS will be a subtype of the specified
    structure.
  - A slot expression, which is structured like:
      (NAME IO-TYPE-DESIGNATOR &key OFFSET SIZE ALIGN PAD)
    IO-TYPE-DESIGNATOR must be a designator for a type as parsed by
    PARSE-IO-TYPE. OFFSET is computed automatically based on the size
    of preceding slots. You may add padding with PAD, or ensure the
    slot offset is rounded up to a given alignment using ALIGN. You
    can also manually force a slot to take up more space via SIZE.
  - A symbol designating the name of an io-type which will be used to
    pad the structure out with the size of the type.

Note that IO-VALUEs (forms that aren't designating IO-TYPEs and are
instead used for their runtime value) are evaluated in a lexical
environment with a SLOT macro bound. This macro either takes the name
of a slot of the structure to read out, or some other value form and
the names of the slots to resolve within. For example:

  (SLOT FOO)       ==> (SLOT-VALUE %current-structure 'FOO)
  (SLOT (FOO) BAR) ==> (SLOT-VALUE (FOO) 'BAR)

Referring to prior slots allows you to dynamically size later slots or
dispatch based on the type of an earlier slot's value.

See IO-STRUCTURE (type)
See IO-STRUCTURE-SLOT (type)
See IO-STRUCTURE-MAGIC (type)
See DEFINE-IO-TYPE
See DEFINE-IO-FUNCTIONS")

  (type bounds-checked-io-backend
    "Class for IO-BACKENDs that require manual bounds checks.

A subclass of this must provide a CHECK-AVAILABLE-SPACE function of
one argument (octets to be consumed) in the lexical environment of the
functions defined via READ-DEFUN and WRITE-DEFUN.

See READ-DEFUN
See WRITE-DEFUN
See IO-BACKEND (type)"))

;; standard-types.lisp
(docs:define-docs
  (function define-io-types
    "Shorthand macro to define different permutations of IO types.

See DEFINE-IO-TYPE"))

;; types
(docs:define-docs
  (type io-stream
    "An IO-BACKEND for STREAMs.

Dispatches both from STREAMs and PATHNAMEs.

See IO-BACKEND (type)")

  (type io-octet-vector
    "An IO-BACKEND for octet / unsigned-byte 8 VECTORs.

Dispatches both from SIMPLE-ARRAYs and ARRAYs. In the latter case the
array is copied to a SIMPLE-ARRAY first, however. You may pass both a
START and END to designate a subsequence.

See IO-BACKEND (type)")

  (type io-foreign-pointer
    "An IO-BACKEND for foreign / memory pointers.

You must pass in a SIZE along with the pointer that designates the
number of octets available in the buffer.

See IO-BACKEND (type)"))
