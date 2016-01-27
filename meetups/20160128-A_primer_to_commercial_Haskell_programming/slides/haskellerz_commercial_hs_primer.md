% A Primer to Commercial Haskell Development


# About me

- learned about Haskell during my ETH CS studies (2003)
- first "commercial" Haskell development during PhD (2007 - 2012)
- Haskell software engineer/architect at Better AG (2013 - 2014)
- post-doctoral researcher at IBM Research (2014 - 2015)
- Haskell software engineer/architect at Elevence Digital Finance AG (2015 -)


# Commercial Haskell development

Know your business model.

- Business model (link to Adrian's post)


# You are selling a user experience

[iphone image]

Haskell is just a tool in your box.


# Your only done once you've shipped your code

- Deploy Software (ship-it image)


Deployment is an integral part of commercial development.


# Use your best tools for the job

[ Not every problem can be solved using a hammer. ]


Haskell is a great tool for many software engineering tasks... provided you
know it well enough for the task at hand.



# Talk Outline

- Code layout
- "stack" the tool
  - explain mental model
  - link to documentation and initial blog posts
- The custom-prelude pattern
- The .Extended-pattern
- Style guidelines



# Code layout

- mono-repositories: link to reasoning, link to post on Facebooks move
- future proof
- link-to-github
- create a small test-project: command-line app that does word-counting
  (probably available from the last talk)


- TODO (SM): adapt elevence-base such that it uses a BSD-3 licence and has
  proper documentation. Also adapt style guide-line such that it is shared
  under a Creative-Commons-Alike license.

# The custom prelude

- Principled way of introducing orphan instances
- Code sharing, impedacne mismatching, broader requirements

# The .Extended pattern

- Impedance mismatching between Hackage and your company's codebase.
- link to Jasper's post


# Style guidelines

- [See link to github].


# Additional Resources

- stephen diehl's post on what I wish I kenw
- my talk on getting started with Haskell
  - what libraries to know
- patterns book
- jasper's posts
- my post on service pattern











