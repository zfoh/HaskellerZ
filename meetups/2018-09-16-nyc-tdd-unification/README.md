NYC Haskell Presentation
========================

Prep
----

- disable dev-env in `~/.bash_profile`
- disable init-tags in `~/.vimr`


Notes on test-drive
-------------------

13:20

```
stack new tdd-unification sol
```

13:40

- change package.yaml to include deps top-level and `src` dir for test
  executable instead of library dep

```
ghcid -c'stack repl tdd-unification-sol:tdd-unification-sol-test' -t':main'
```

13:43

- add `hedgehog` as a test dependency
- call `stack test` to trigger install






