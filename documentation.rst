=================================
CleverCSS (Haskell) Documentation
=================================

Introduction
------------

CleverCSS is a system for writing templates that are translated into CSS,
allowing to build style sheets in a clean and structured way, following the DRY
(don't repeat yourself) principle.

The syntax is slightly different from CSS in that it relies on indentation
instead of braces to indicate grouping, like the Python language does.

The two things that enable DRY are *variables* and *nesting*.  Variables are
names for CSS values; nesting prevents repeating selectors all over.  Here is an
example template file::

   bgcolor = #f0f0e4

   ul#comments, ol#comments:
      background-color: $bgcolor

      li:
         background-color: $bgcolor
         font-size: 1.2em

It would translate to (an equivalent of) this CSS stylesheet::

   ul#comments, ol#comments {
      background-color: #f0f0e4;
   }

   ul#comments li, ol#comments li {
      background-color: #f0f0e4;
      font-size: 1.2em;
   }

You can already see that the template doesn't repeat the ``ul#comments`` and
``ol#comments`` selectors.  Also, should the desired background color ever
change, you won't need to replace a zillion references to it, but only one
variable assignment.

Other than that, CleverCSS allows expressions involving CSS values as well as
grouping of similar property names, as shown below.


Syntax
------

As you have already seen, you use indentation and colons to express grouping.
There are also other small syntactic differences to CSS that make writing
templates less pain:

* Semicolons at line ends are optional.

* There are no ``/* ... */`` multiline comments, but Java-style ``//`` line
  comments.

* A line consisting only of ``__END__`` means "end of template".  Everything
  after that line is completely ignored by CleverCSS.


Constructs
----------

Variable assignments
~~~~~~~~~~~~~~~~~~~~

You can assign to variables with an equals sign.  Variable assignments are not
allowed inside blocks, only at the toplevel.  Variables can be assigned to
multiple times. ::

   foo = 1px
   body:
      margin: $foo
   foo = 2px
   p:
      margin: $foo

would result in ``body { margin: 1px }`` and ``p { margin: 2px }``.  Referencing
variables before they are defined is an error.

There's also another assignment operator, ``?=``.  It only has an effect if the
variable is not assigned to yet.  This is helpful to provide defaults that can
be overridden by e.g. command line definitions.  For example, after ::

   foo = 1px
   bar = 1px
   foo = 2px
   bar ?= 2px

``$foo`` would be replaced by ``2px``, while ``$bar`` would be replaced by
``1px``.


Selectors
~~~~~~~~~

CleverCSS does not process selectors in a special way, but as shown above you
can nest selector blocks instead of repeating parent selectors in individual
blocks::

   ul, ol:
      foo
      li, p:
         bar
         strong, em:
            baz

would result in ::

   ul, ol { foo }

   ul li, ul p, ol li, ol p { bar }

   ul li strong, ul li em, ul p strong, ul p em,
   ol li strong, ol li em, ol p strong, ol p em  { baz }


To achieve this, the selectors are currently bluntly split at commas, so
extended selectors involving string matching with a comma will be handled
incorrectly at the moment.

As you see, the above handles basic parent-child relations by joining parent and
child selectors with a space.  However, you can also explicitly determine where
to put the parent selector, using the ``&`` character::

   strong, em:
      a &, #&:
         bar

would result in ::

   a strong, #strong, a em, #em { bar }

Using this technique, you can construct selectors quite freely.  Multiple
occurrences of the ampersand are replaced.


Properties
~~~~~~~~~~

Property specifications work like in CSS: just put a colon after the property
name.  You can use expressions or variable references (see below) only in
property values, not in property names.


Property groups
~~~~~~~~~~~~~~~

Since CSS has many property names with common prefixes, CleverCSS includes one
more shortening notation, best described by an example::

   #main p:
      font->
         family: Verdana, sans-serif
         size: 1.1em
         style: italic
   
would result in ::

   #main p {
      font-family: Verdana, sans-serif;
      font-size: 1.1em;
      font-style: italic;
   }


The ``->`` symbol, followed by a block of properties, concatenates the name
before it with each of the property names in the block.


Macros
~~~~~~

Macros can help you writing often used groups of properties and sub-blocks only
once.  Define macros like this::

   @define mymacro(arg1, arg2):
       font-family: $arg1
       p:
           display: inline
           color: $arg2

Macros can have zero or more arguments.  Inside a macro definition, the
arguments are accessible like normal variables.  Macros must be defined at
top-level, and their names live in a different namespace than variables.
           
Use ("substitute") them like this::

   body:
       %mymacro("Verdana", blue)
       font-size: 1.1em

Macro substitutions are handled as if the macro's contents are placed at the
exact location of the substitution, with the argument variables replaced by the
given expressions.


Includes
~~~~~~~~

Including a file::

   @include "filename.ccs"

The contents of the included file are treated as if they occurred in the
including file at the point of the include command.


Values and expressions
----------------------

CleverCSS property values are of multiple types which also exist in CSS:

+-------------------------------------------+------------------------------------+------+
| Type                                      | Example property                   | Note |
+===========================================+====================================+======+
| barewords (also called identifiers)       | ``font-size: small``               |      |
+-------------------------------------------+------------------------------------+------+
| strings (double or single quoted)         | ``font-family: "Times"``           |      |
+-------------------------------------------+------------------------------------+------+
| numbers (integral or floating)            | ``line-height: 2``                 |      |
+-------------------------------------------+------------------------------------+------+
| dimensions (number + unit)                | ``margin-left: 2px``               |      |
+-------------------------------------------+------------------------------------+------+
| hexadecimal colors                        | ``color: #f0f0f0``                 |      |
+-------------------------------------------+------------------------------------+------+
| colors as color names                     | ``color: black``                   | \(1) |
+-------------------------------------------+------------------------------------+------+
| colors via the RGB function               | ``color: rgb(10%, 20%, 30%)``      |      |
+-------------------------------------------+------------------------------------+------+
| value sequences (separated by whitespace) | ``margin: 2px 0 2px 0``            |      |
+-------------------------------------------+------------------------------------+------+
| value lists (separated by commas)         | ``font-family: "Times", serif``    | \(2) |
+-------------------------------------------+------------------------------------+------+
| function calls                            | ``content: attr(id)``              | \(3) |
+-------------------------------------------+------------------------------------+------+

Notes:

(1)
   Color names are also valid identifiers.  CleverCSS recognizes the 140 common
   Netscape color names and treats them as values of type color, but does not
   convert them to RGB format until you do arithmetic with them.  That way, you
   can have barewords that are color names without a problem.

(2)
   This is only used in CSS for the ``font-family`` property, but you are free
   to use value lists anywhere and convert them to other values using their
   methods, see below.

(3)
   The functions recognized by CleverCSS, in addition to ``rgb``, are ``attr``,
   ``counter`` and ``url``.


CleverCSS extends CSS in that it allows writing not only these literal values,
but also expressions involving these values.  These expressions can contain
these elements, in addition to the literal expressions described above:

+-------------------------------------------+----------------------------------------+------+
| Type                                      | Example(s)                             | Note |
+===========================================+========================================+======+
| Arithmetic on numbers and values;         | ``4 % 3 = 1``, ``2px + 4px = 6px``,    | \(1) |
| operators are ``+``, ``-``, ``*``, ``/``  | ``2 * 2px = 4px``                      |      |
| and ``%`` (modulo)                        |                                        |      |
+-------------------------------------------+----------------------------------------+------+
| Arithmetic on colors with ``+`` and ``-`` | ``#f0f000 + #000030 = #f0f030``,       | \(2) |
|                                           | ``#808080 + 16 = #909090``             |      |
+-------------------------------------------+----------------------------------------+------+
| Concatenation of strings with ``+``       | ``"hello " + "world" = "hello world"`` | \(3) |
+-------------------------------------------+----------------------------------------+------+
| String multiplication                     | ``"a " * 3 = "a a a "``                |      |
+-------------------------------------------+----------------------------------------+------+
| Expression grouping with parentheses      | ``(2 + 3) * 5px``                      |      |
+-------------------------------------------+----------------------------------------+------+
| Method calls                              | ``#303030.brighten(40%) = #434343``,   | \(4) |
|                                           | ``"hello".bare() = hello``             |      |
+-------------------------------------------+----------------------------------------+------+
| Variable references                       | ``$foo = <whatever foo was assigned>`` |      |
+-------------------------------------------+----------------------------------------+------+

More explanations:

(1)
   Arithmetic on numbers works as expected.

   You can mix one number and one value in arithmetic expressions, the result
   will automatically be given the unit of the value.  This is natural with
   multiplication and division but can feel weird with addition and subtraction.

   You can add and subtract two dimensions provided their units are the same or
   convertable to one another, but you cannot multiply or divide them.

(2)
   If two colors are added or subtracted, their individual channels will be
   added or subtracted.  If one operand is a number, it will be applied to all
   channels.

(3)
   Barewords cannot be added, but you can convert strings to barewords with the
   ``bare()`` method afterwards.

(4)
   For a list of available methods, see below.


Methods
~~~~~~~

On values of all types:

* **string()**: convert to a string.

On strings:

* **bare()**: convert the string to a bareword.  It is not checked that it has the
  required format!
* **length()**: return the string's length.
* **upper()**, **lower()**: convert the string to uppercase/lowercase.
* **strip()**: return the string with all trailing and leading whitespace removed.
* **split(delim)**: return a list with substrings, split at the string *delim*.
* **eval()**: evaluate the contents as a CleverCSS expression and return the result.

On numbers and dimensions:

* **round([places])**: return the number or dimension rounded to *places* decimal
  places; *places* defaults to 0.
* **abs()**: return the absolute value.

On colors:

* **brighten([amount])**: return the color brightened by the specified amount,
  which should be a percent dimension.  *amount* defaults to 10%.
* **darken([amount])**: return the color darkened by the specified amount, which
  should be a percent dimension.  *amount* defaults to 10%.

On lists and sequences:

* **length()**: return the list or sequence's length.
* **join([delim])**: return a string consisting the items converted to strings
  and joined by *delim*, which defaults to ``", "`` for lists and ``" "`` for
  sequences.
* **list()**: return the sequence as a list, or the list unchanged.
* **seq()**: return the list as a sequence, or the sequence unchanged.


Library usage
-------------

Using the CleverCSS library is straightforward, just import ``Text.CSS.CleverCSS``
and use the ``cleverCSSConvert`` function, which is defined as ::

  cleverCSSConvert :: SourceName -> String -> [(String, String)] -> Either String String

The arguments are:

* name of input (normally file name, only used for error messages)
* input template
* initial variable assignments as ``(name, value)`` pairs; the value is evaluated
  as a CleverCSS expression when used

The return value is either ``Left errormessage`` or ``Right stylesheet``.


Command-line usage
------------------

CleverCSS also can be compiled as a standalone command-line program.  It can be
called with no arguments, in which case it will convert standard input to
standard output, or with file names as arguments, in which case it will convert
the files named to CSS and store them in a file with the same name, but the
extension replaced with ``.css`` (e.g., ``example.clevercss`` is converted to
``example.css``).

You can use the ``-D name=value`` command line option to assign initial
variables.  The value is evaluated as a CleverCSS expression when used.


How to get and install it
-------------------------

CleverCSS can be downloaded from
`Hackage <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/clevercss>`_ 
or checked out from Mercurial at <http://dev.pocoo.org/hg/clevercss-hs-main>.
It is a cabalized package, so the usual ::

   runhaskell Setup.lhs configure
   runhaskell Setup.lhs build
   sudo runhaskell Setup.lhs install

should be enough to get the ``clevercss`` binary and the ``Text.CSS.CleverCSS``
library installed.


Authors
-------

The Haskell CleverCSS library is written by Georg Brandl <georg@python.org>.
Bug reports and suggestions are welcome!

The CleverCSS template language was initially devised and implemented in Python
by Armin Ronacher, see <http://sandbox.pocoo.org/clevercss>.
