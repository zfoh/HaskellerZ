An excerpt of the Elevence development repository
=================================================

The code in the sibling directories provides an excerpt of the Elevence
development repository and is intended to serve as a template for other
companies that want to start using Haskell in production.

To build and test all applications we use
[`stack`](http://docs.haskellstack.org/en/stable/README.html) as follows.
```
stack test
```

Our code-layout's intended use is the following.
```
▾ apps/                 -- code for all our applications
  ▾ hs/                 --   the ones written in Haskell
    ▾ acme-wc/          --     an example Haskell application using
                        --     elevence-base
▾ docs/                 -- all documentation
    hs-style-guide.md   -- The Elevence Haskell style-guide currently
▾ dotfiles/             -- shared configuration files
    .vimrc_elevence     -- our shared .vimrc file (not a must to use)
▾ libs/                 -- code for all our libraries
  ▾ hs/                 --   the ones written in Haskell
    ▾ elevence-base/    --     our internal base library, which provides
      ▸ src-tests/      --     our custom Prelude, a home to all orphans,
      ▸ src/            --     .Extended versions for modules of Hackage
      ...               --     libraries, and some further datastructures
▾ scratch/              -- scratch directories to host experiments by
  ▾ meiersi/            -- individual employees
```


Happy coding,
the Elevence Digital Finance AG development team.
