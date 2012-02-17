## HaskellerZ - Link Collection

This link collection was originally assembled for the
[Functional Programming and Formal Methods](http://www.infsec.ethz.ch/education/ss2012/fmfp)
taught at [ETH Zurich](http://www.infsec.ethz.ch).
It is now maintained by the 
[HaskellerZ meetup group](http://www.meetup.com/HaskellerZ/)
for the benefit of all fellow Haskellers.
The collection is organized in three parts: links for Haskell beginners, programmers, and experts.


### Haskell Beginners

Installing GHCi:

-   The easiest way to install GHCi on OS X and Windows is using the
    [Haskell platform](http://hackage.haskell.org/platform/) packages.

-   On Linux, we recommend to use GHC packages provided by your
    distribution or the binary packages provided on the [GHC
    hompage](http://www.haskell.org/ghc/download_ghc_7_0_1).

Using GHCi:

-   [Official Website](http://www.haskell.org/ghc), [User's
    Guide](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html),
    and
    [Tutorial](http://book.realworldhaskell.org/read/getting-started.html)

-   Good text editors:
    [Notepad++](http://notepad-plus.sourceforge.net/de/site.htm)
    (Windows), [Kate](http://kate-editor.org/) (Linux with KDE desktop),
    [gedit](http://projects.gnome.org/gedit/) (Linux with GNOME
    desktop), [jEdit](http://www.jedit.org/) (cross platform),
    [vim](http://www.vim.org/) (cross platform), and
    [emacs](http://www.gnu.org/software/emacs/) (cross platform with
    good [haskell-mode](http://projects.haskell.org/haskellmode-emacs/))

Learning Haskell:

-   The [official Haskell
    website](http://www.haskell.org/haskellwiki/Haskell)

-   A [style
    guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
    for writing Haskell code

-   [Hoogle:](http://www.haskell.org/hoogle) search the Haskell
    libraries by name and/or type

-   The [Haskell
    Prelude](http://www.inf.ethz.ch/personal/meiersi/teaching/fmfp08/material/Prelude.hs)
    (Hugs version):

    -   [Syntax highlighted
        version](http://www.inf.ethz.ch/personal/meiersi/teaching/fmfp08/material/Prelude.html)

    -   [Postscript file ideal for
        printing](http://www.inf.ethz.ch/personal/meiersi/teaching/fmfp08/material/Prelude.hs.ps)

    -   [Commented
        version](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html)
        (note that the source code of this version is slightly different
        from the Hugs version, as it is optimized for speed).

-   Free online version of the excellent books [**Real World
    Haskell**](http://book.realworldhaskell.org/read) and [**Learn You a
    Haskell for Great Good**](http://learnyouahaskell.com/chapters).
    Have a look at the [Haskell
    bookshelf](http://www.haskell.org/haskellwiki/Books_and_tutorials)
    for more books.

-   Have a look at the
    [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia)
    to gain more understanding of standard typeclasses like Functors and
    Monads.

-   Note that the official definition of the syntax and the semantics
    for Haskell is given in the [Haskell 2010
    report](http://www.haskell.org/onlinereport/haskell2010/).

### Haskell programmers

**Using the Glasgow Haskell Compiler (GHC)**\
As soon as you develop some real applications, you'll need a good build
environment and the speed of a compiled executable. The first is given
the the Common Architecture for Building Applications and Libraries
([cabal](http://en.wikibooks.org/wiki/Haskell/Packaging)). The later is
given by the [Glasgow Haskell Compiler
(GHC)](http://www.haskell.org/ghc/), whose interactive shell GHCi you
are already using. (See [for a
comparison](http://shootout.alioth.debian.org/gp4/benchmark.php?test=all&lang=all)
of its speed to compilers for other languages.) Apart from GHCi, GHC
also supports various extensions to the Haskell language and features an
integrated debugger.

-   The [GHCi
    debugger](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-debugger.html)

-   [Tutorial](http://cgi.cse.unsw.edu.au/~dons/blog/2007/11/14) for the
    debugger

-   support for Concurrent Haskell including [Software Transactional
    Memory](http://research.microsoft.com/~simonpj/papers/stm/index.htm)
    and a prototype of [Data Parallel
    Haskell](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell).

\
**Do not reinvent the wheel - Know the libraries**

There exists a fast growing repository of Haskell libraries that will
serve you very well during your daily programming excursions. They are
all developed collaboratively on
[Hackage](http://hackage.haskell.org/packages/hackage.html) go and check
them out. A few that you should definitively have a look at are:

-   vector: the state-of-the art array library

-   bytestring: types and functions to manipulate sequences of bytes

-   text: types and functions to manipulate strings, i.e., sequences
    Unicode characters

-   attoparsec: efficiently parsing bytestrings and text values

-   aeson: JSON decoding and encoding

-   cmdargs: command-line parsing made easy

-   criterion: benchmarking Haskell functions made easy. Note that
    criterion supports the precise measurements of functions taking just
    a few nano-seconds to execute.

Some of the libraries are also just cool and understanding them will
widen your computer science horizon.

-   [Finger
    Trees](http://www.soi.city.ac.uk/~ross/papers/FingerTree.html) -
    ["The Swiss Army Knife of
    Data-Structures"](http://www.informatik.uni-bonn.de/~ralf/talks/Oxford.pdf)

-   repa: regular parallel arrays. See this real-time raytracer
    implemented using it.

-   

\
\

**Cool Applications - Haskell in "The Real World"**

For a long time, Haskell and other functional languages like
[OCaml](http://caml.inria.fr/) or \
[SML](http://www.smlnj.org/) have been a research only programming
languages. However this is no longer the case: There are companies like
[Galois Inc.](http://www.galois.com/),
[Bluespec](http://www.bluespec.com/), \
[Credit
Suisse](http://www.mail-archive.com/haskell@haskell.org/msg22288.html)
and [quite a few
more](http://www.haskell.org/haskellwiki/Haskell_in_industry) that are
using functional languages like Haskell to solve their real world
problems. See the book\
["Real World
Haskell"](http://www.amazon.com/gp/product/0596514980?ie=UTF8&tag=reaworhas-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=0596514980)
([online](http://book.realworldhaskell.org/read)) for explanations on
how to do this best. \
\
Furthermore, there are also a number of exciting open-source project
being developed using Haskell. Most notably there are:

-   [darcs](http://www.darcs.net/) version control system

-   [xmonad](http://www.xmonad.org/) window manager

-   [happstack](http://happstack.com/) a Haskell web application server

-   [yi](http://haskell.org/haskellwiki/Yi) text editor fully scriptable
    using dynamically loaded Haskell plugins

-   [hmp3](http://www.cse.unsw.edu.au/~dons/hmp3.html) curses based mp3
    player

-   and [many
    more](http://www.haskell.org/haskellwiki/Applications_and_libraries)

All of these projects are open for contributions, not to forget that
there surely are plenty of ideas you have that could possibly be
realised using Haskell...

\
\

**Haskell Community: Blogs & Facts**

Haskell has attracted a very active and friendly community. They use the
following communication channels

-   IRC [\#haskell @
    code.freenode.net](http://www.haskell.org/haskellwiki/IRC_channel)

-   [Haskell
    Announcements](http://www.haskell.org/mailman/listinfo/haskell),
    [Haskell
    Cafe](http://www.haskell.org/mailman/listinfo/haskell-cafe),
    [mailing lists](http://www.haskell.org/haskellwiki/Mailing_Lists)

-   various blogs: a good aggregate is
    [**http://planet.haskell.org**](http://planet.haskell.org/)

Some interesting blogs (posts) are:

-   [Lambda the Ultimate](http://lambda-the-ultimate.org/) Programming
    Languages Weblog

-   Don Stewart's blog:
    [http://cgi.cse.unsw.edu.au/\~dons/blog](http://cgi.cse.unsw.edu.au/~dons/blog)

-   Epic Games programmer Tim Sweeny at POPL'05 about the [next
    mainstream programming
    language](http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/sweeny.pdf)

### Haskell Experts

There is a lot of active research going on with respect to functional
languages and more correct and more productive programming in general. A
very good starting point to delve into these topics is [Oleg Kiselyov's
homepage](http://okmij.org/ftp/). For a starting point with respect to
the implementation of functional languages see [Simon Peyton Jones
homepage](http://research.microsoft.com/~simonpj). A few topics you
might want to investigate

-   learn about [category
    theory](http://en.wikipedia.org/wiki/Category_theory)

    -   good and free lecture notes are the [Category Theory Lecture
        Notes](http://www.andrew.cmu.edu/course/80-413-713/)

    -   An in-depth explanation of the relation between Haskell's IO
        system and category theoretic monads is ["The Haskell
        Programmers Guide to the IO Monad - Don't
        Panic"](http://stefan-klinger.de/files/monadGuide.pdf)

-   learn about [type level
    programming](http://okmij.org/ftp/Haskell/types.html)

    -   statically checking correct usage of physical units (see this
        [blog
        post](http://liftm.wordpress.com/2007/06/03/scientificdimension-type-arithmetic-and-physical-units-in-haskell)
        and this
        [library](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/dimensional))

-   learn about dependent types and

    -   [lightweight dependent-type
        programming](http://okmij.org/ftp/Computation/lightweight-dependent-typing.html),
        i.e., using Haskell's type system to preserve invariants like no
        out of bounds array access *statically*)

    -   proving software correct using the calculus of constructions in
        [Coq](http://coq.inria.fr/)

    -   proving software correct using Higher Order Logic in
        [Isabelle/HOL](http://isabelle.in.tum.de/)

-   learn about the relation between Haskell and object oriented
    software construction:
    [OOHaskell](http://homepages.cwi.nl/~ralf/OOHaskell)

\

