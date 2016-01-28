% A Primer to Commercial Haskell Development
% Simon Meier, Elevence Digital Finance AG
% January 28th, 2016


# About me

- learned about Haskell during my ETH CS studies (2003)
- first "commercial" Haskell development during PhD (2007 - 2012)
- Haskell software engineer/architect at Better AG (2013 - 2014)
- post-doctoral researcher at IBM Research (2014 - 2015)
- Haskell software engineer/architect at Elevence Digital Finance AG (2015 -)


# Commercial Haskell development - my view on it


![](img/business-model.jpg)

We'll want to be conscious about our business model.

See [this post-mortem of Klimpr](https://medium.com/@adriankyburz/it-s-been-an-amazing-ride-now-my-startup-is-dead-and-here-s-what-i-ve-learned-284e14ef4ee0#.w4621izgf),
a friend's startup, for further insights.


# We are always selling a user experience

![](img/user-experience-for-startups.png)

Haskell is just a tool in our box.


# We're only done once we've shipped our code

![](img/spacex-launch.jpg)

Deployment and coding are equally important.


# We want to use the right tools for the jobs at hand

![](img/cnc_machine.jpg)

Haskell is a great tool for many software engineering tasks.

However not all problems are best solved using Haskell or FP.


# Talk Outline

- code layout
- the `stack` tool
- the custom-prelude pattern
- the .Extended-pattern
- style guidelines
- additional resources


# Code layout

Mono-repositories are great.

- transactional changes across all of your
  software artifacts
- see [this](http://danluu.com/monorepo/) and
     [that](https://github.com/babel/babel/blob/master/doc/design/monorepo.md)
  for more in-depth comparison of mono- vs multi-repo setups

**Big thanks to Elevence for publishing their
  [development repo layout](https://github.com/meiersi/HaskellerZ/tree/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence)**
  including

- their `elevence-base` library and
- their style-guide.


# The stack tool


- solid multi-library and application support
- good build reproducability
- lots of options,
  [which one should know about](http://docs.haskellstack.org/en/stable/GUIDE.html).

In my opinion, `stack` is a must use for commercial Haskell development.

([Nix](http://www.cse.chalmers.se/~bernardy/nix.html) is an alternative,
but has a way higher ramp-up cost.)


# The custom-prelude pattern

Key purposes of our [`Elevence.Prelude`](https://github.com/meiersi/HaskellerZ/tree/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence/libs/hs/elevence-base/src/Elevence/Prelude.hs):

- reduce import clutter for standard modules
- provide an insulation layer against changes in GHC/Hackage
- provide a principled way of introducing orphan instances
    - introduce `Orphans.Lib_<library>` modules for defining orphan instances
      for classes defined in `<library>`
    - import all of these in `Elevence.Prelude`
    - guarantees instance coherence, as `Elevence.Prelude` is always in scope

I believe that every Haskell company should use their own prelude.


# The .Extended pattern

Similar goals as the custom-prelude pattern,
but for individual libraries on Hackage.

- reduce import clutter
    - for libraries whose exports are too fine-grained, or
    - for functionality spread over multiple libraries
      (e.g., `lens` support for `text` values).
- provide an insulation layer for changes in Hackage libraries

See [Jasper Van der Jeugt's post for a detailed explanation](https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html).


# The Elevence Haskell style guidelines

[Let's have a look at them](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence/docs/hs-style-guide.md).


# Additional Resources

There are a lot of resources, but they are not as condensed as one might like.

I believe Stephen Diehl's
[What I wish I knew when I learned Haskell](http://dev.stephendiehl.com/hask/)
provides a great entry point.

For people that are more freshly starting with Haskell,
there's also our [HaskellerZ meetup repo](https://github.com/meiersi/HaskellerZ)
and [my talk on getting started with Haskell](https://github.com/meiersi/HaskellerZ/blob/master/meetups/2015-02-26-Writing_your_first_real_world_Haskell_application/real_world_haskell_intro.markdown).










