% A Primer to Commercial Haskell Development
% Simon Meier, Elevence Digital Finance AG
% January 28th, 2016


# About me

- learned about Haskell during my ETH CS studies (2003)
- first "commercial" Haskell development during PhD (2007 - 2012)
- Haskell software engineer/architect at Better AG (2013 - 2014)
- post-doctoral researcher at IBM Research (2014 - 2015)
- Haskell software engineer/architect at Elevence Digital Finance AG (2015 -)


# Commercial Haskell development


![](img/business-model.jpg)

Start with the business model.
([see](https://medium.com/@adriankyburz/it-s-been-an-amazing-ride-now-my-startup-is-dead-and-here-s-what-i-ve-learned-284e14ef4ee0#.w4621izgf)
for a enlighting post-mortem writeup of a friend's startup)


# You are selling a user experience

![](img/user-experience-for-startups.png)

Haskell is just a tool in your box.


# Your only done once you've shipped your code

![](img/spacex-launch.jpg)

Deployment is an integral part of commercial development: just factor it in.


# Use the right tool for your job

![](img/cnc_machine.jpg)

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
  (see [here](http://danluu.com/monorepo/) and
       [here](https://github.com/babel/babel/blob/master/doc/design/monorepo.md)
  )

- future proof
- link-to-github
  https://github.com/meiersi/HaskellerZ/tree/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence



# The stack tool

- http://docs.haskellstack.org/en/stable/GUIDE.html


# Our custom prelude

- Principled way of introducing orphan instances
- Code sharing, impedacne mismatching, broader requirements

# The .Extended pattern

- Impedance mismatching between Hackage and your company's codebase.
- link to Jasper's post
- https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html


# Style guidelines

- [See link to github](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence/docs/hs-style-guide.md)


# Additional Resources

- stephen diehl's post on what I wish I kenw
  http://dev.stephendiehl.com/hask/

- my talk on getting started with Haskell
  https://github.com/meiersi/HaskellerZ/blob/master/meetups/2015-02-26-Writing_your_first_real_world_Haskell_application/real_world_haskell_intro.markdown










