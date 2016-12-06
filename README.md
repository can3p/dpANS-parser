## Original readme

This software has a very specific purpose, namely to parse the draft
proposed American National Standard (dpANS) for the programming
language Common Lisp.

That proposal is free for anyone to use as he or she pleases.  It is
nevertheless an interesting document because the final standard for
the Common Lisp programming language is very close to the dpANS, so it
can be used for a variety of purposes:

  * It could be an excellent basis for a language reference manual for
    the Common Lisp language.

  * It could be the foundation of a revised Common Lisp standard.

  * It could be used to provide documentation strings for existing
    Common Lisp implementations.

The dpANS is written in TeX.  It does not use LaTeX, and instead
includes TeX macros that serve a similar purpose as the LaTeX macros
for structuring a document into chapters, sections, etc.  It also
contain TeX macros for specific semantic markups related to Common
Lisp.  

In order to be used for other purposes, the dpANS needs to be parsed
and translated to other formats.  This software is not a
general-purpose TeX parser, which would be very hard indeed, given
that TeX allows macros that do arbitrary computations.  Instead, we
specifically parse the macros that are used by the dpANS.

This software is not meant to be used repeatedly.  Instead, it is
meant to be used once to translate the dpANS into some other format.
For that reason, the parser does not need to be efficient.  We can
therefore choose a simple yet powerful parsing technique that is
convenient to use as opposed to extremely efficient.  We have chosen
to use so-called "combinatory parsing".  Furthermore, the size
document that we need to parse is known, and while it is not exactly
small, it is not huge either.  We can therefore tokenize a file into a
list of tokens, which we can then transform in arbitrary ways using
our usual Common Lisp tools.  Furthermore, combinatory parsing relies
on the input stream of tokens to be side-effect free, and a list of
tokens is excellent for that purpose. 

This software includes a general framework for combinatory parsing
that was extracted from the LOOP macro of SICL.  We may extract it
from this software as well and turn it into a separately-distributed
system.

Current state of this software: 

  A tokenizer exists, but it is not certain that it produces the best
  possible set of tokens, so it might be altered in the future.  It
  does, however, produce tokens that would not normally be considered
  as such, in that it includes whitespace, newlines, and comments as
  tokens.  The reason for the existence of these additional
  non-standard tokens is that we do not want to lose any information
  as a result of parsing, so that the parser can be used in a variety
  of situations.  Specific applications that do not need certain types
  of tokens can easily preprocess the complete list of tokens by using
  any Common Lisp program to filter that list. 

Next, we intend to work on the following aspect:

  We plan to use the framework for combinatory parsing in order to
  define elementary parsers for easily-recognizable markup macros.  

----

## Current Status

General idea is to generate object tree that represents a spec
and print it out as an xml, so that we can convert it later to
whatever format we find more interesting

Since we have commands, let's execute them

\input commands should be executed during parse time
So, as a result from parsing we're getting a long stream
that consists of commands and text blocks which can contains sentences and
other commands.

Commands can be of several types:

* Ones that create structural element like \beginsubsection (start structure command)
* Ones that mark the ending of current structural element like \endsubsection (end structure command)
* Ones that define variables that can be used later (definition command)
* Ones that actually generate markup. This sort of commands can be encountered inside of text blocks (inline command)

So, logic should be like following:

1. Create root level document and mark it as a current element
2. Read next element from parsed stream. If nothing is left, we're done, exit
3.a If text block process text block and add it as a child to current element
3.b if start structure command create relevant element add it to the current and mark as current
3.c If end structure command close current element and make it's parrent current element
3.d If definition command assign relevant variable as a document property.
4. Goto 2

Here is how wwe parse a text block:

2. Read next element from parsed stream. If nothing is left, we're done, exit
3.a If it's a new line - discard and go to step 2
3.b If it's not a command - read stream until we get a command and make it a string. Then go to step 2
3.c If it's a command - run relevant command and append result to a text


## License

All the code except the parts remaining from original project is in Public Domain. Enjoy!
