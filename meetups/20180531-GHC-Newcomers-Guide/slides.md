<!-- $theme: default -->
<!-- page_number: true -->
<!-- *page_number: false -->

# GHC Newcomer Guide

#### Hacking GHC

###### 31.05.2018 @HaskellerZ by [Andreas Herrmann](github.com/aherrmann)

---

## Why Hack GHC?

- Compiler is among the most important tools
- Good to know your tools
- Better understand the language
- You can improve GHC
- It's just cool ;)

---

## How to get started?

- GHC Newcomer guide
    https://ghc.haskell.org/trac/ghc/wiki/Newcomers
    Prepare your machine, fetch the code, build it, get started
- Building guide
	https://ghc.haskell.org/trac/ghc/wiki/Building
    In-depth about build system
- GHC Commentary
	https://ghc.haskell.org/trac/ghc/wiki/Commentary
    Details about concepts and source structure
- Community
	[ghc-devs](http://www.haskell.org/mailman/listinfo/ghc-devs) mailing list
    [#ghc](http://ircbrowse.net/browse/ghc) on FreeNode
- More references in the end

---

## Dependencies - Prepare your machine

- Depends on your platform, follow the guide
	https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation
- If all else fails, use Docker
	``` shell
    $ docker run --rm -i -t -v "$PWD":/home/ghc \
          gregweber/ghc-haskell-dev /bin/bash
    ```

---

## Get the sources

``` shell
$ git clone --recursive git://git.haskell.org/ghc.git
```
GitHub mirror requires extra config
``` shell
$ git config --global \
      url."git://github.com/ghc/packages-".insteadOf \
      git://github.com/ghc/packages/
$ git clone --recursive git://github.com/ghc/ghc
```

---

## Git submodules

- When pulling changes
    ``` shell
    $ git pull --recurse-submodules=yes
    ```
- When switching branches
	``` shell
    $ git checkout <other-branchname>
    $ git submodule update --init --recursive
    ```
- More details in [GHC wiki - Git submodules](https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/Git/Submodules)

---

## Git worktrees

- Work on different branches in different worktrees.
- Can use git worktrees to work on different branches.
    `~/.gitconfig`:
    ```
    [alias]
      # Adapted from https://gitlab.com/clacke/gists/blob/0c4a0b6e10f7fbf15127339750a6ff490d9aa3c8/.config/git/config#L11-12
      wta = worktree add --detach
      wtas = "!bash -ec 'if (($# != 1)); then echo >&2 \"Usage: git wtas <worktree>\"; exit 2; fi; tree=\"$1\"; git wta \"$tree\"; cd \"$(git rev-parse --git-dir)\"; for mod in $(git config --blob HEAD:.gitmodules -l --name-only|gawk -F . \"/\\.path$/ {print \\$2}\"); do [ -d modules/$mod ] && git -C modules/$mod wta \"${tree}/$(git config --blob HEAD:.gitmodules --get submodule.${mod}.path)\"; done' wtas"
    ```
    ``` shell
    $ git wtas ../ghc-my-new-feature
    $ git submodule update --init
    ```
    https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/Git

---

## Configure the build

- Default build config
    - production settings
    - slow build
- Development build
    - less optimization
    - faster build
    - debug mode
    ``` shell
    $ cp mk/build.mk.sample mk/build.mk
    ```
    `mk/build.mk`:
    ``` make
    BuildFlavour = devel2  # mk/flavours/devel2.mk
    ```

---

## GHC build stages

- Stage 0: The installed GHC (bootstrap compiler)
- Stage 1: Bootstrap builds dependencies, libraries, and stage 1
- Stage 2: stage 1 builds libraries, rts, and stage 2
- Optional stage 3: build again from stage 2 for testing

---

## First build

``` shell
$ ./boot       # generate configure scripts
$ ./configure  # configure Makefiles, etc.
$ make -j4     # builds stage 1 and stage 2
```
Go fetch coffee/tea/milk...

If it built, try it out
``` shell
$ ./inplace/bin/ghc-stage2 --interactive
```

---

## Faster rebuild

- Don't rebuild stage 1
   `mk/build.mk`:
   ``` make
   stage=2
   ```
- Run `make` where you made changes
    (`compiler`, `utils`, `ghc`, `libraries`)
- Use `make fast`
    (except after `git pull`)

---

## Sanity check

Pick an error message
``` shell
$ ./inplace/bin/ghc-stage2 --interactive
ghci> a + 2
<interactive>:2:1: error: Variable not in scope: a
```

Find and change it
``` shell
$ grep -rl '"Variable not in scope:"'
compiler/typecheck/TcErrors.hs
$ $EDITOR compiler/typecheck/TcErrors.hs
$ (cd compiler && make fast -j4)
```

And try again
``` shell
$ ./inplace/bin/ghc-stage2 --interactive
ghci> a + 2
Doesn't look like anything to me: a
```

---

## Random hint

- `make help`: List relevant targets in each subdirectory

---

## Hadrian

- New [Shake](https://shakebuild.com/) based build system
- Coming soon [(8.6?)](https://ghc.haskell.org/trac/ghc/wiki/Status/Apr18)
- See [`hadrian/README.md`](https://github.com/ghc/hadrian/blob/master/README.md)

---

## Source code overview

- Top-level files: Largely build-system & communication
    - `HACKING.md`: Hacking & contributing guide
- `libraries/`: GHC's dependencies (boot packages)
- `compiler/`: `ghc` library package
    Parser, typechecker, AST, core, STG, code-generators, ...
- `ghc/`: `ghc-bin` executable package
- `rts/`: Runtime system - C implementation
    Storage manager, garbage collector, Scheduler, ...
- `docs/`: [GHC documentation](https://downloads.haskell.org/~ghc/latest/docs/html/)
- `testsuite/`: The test suite
- See [commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree) for more details

---

## Compilation pipeline

- Parser &rarr; Parse tree
- Desugar &rarr; Core
- STGify &rarr; STG (Spineless Tagless G-machine)
- CodeGen &rarr; C--
- Backend
	- Native code generator &rarr; Assembly (default)
	- LLVM backend &rarr; LLVM IR (`-fllvm`)
	    LLVM &rarr; Assembly
	- C backend &rarr; C (`-fvia-c` outdated)
	    GCC &rarr; Assembly

---

## Picking an issue

- Newcomers guide: ["Finding a ticket"](https://ghc.haskell.org/trac/ghc/wiki/Newcomers#Findingaticket)
- Tickets [by milestone](https://ghc.haskell.org/trac/ghc/query?status=new&status=assigned&status=reopened&group=milestone&order=priority)
- Ask on [IRC or mailing-list](https://ghc.haskell.org/trac/ghc/wiki/MailingListsAndIRC)
- You found a bug?

---

## Contributing

- Have a ticket on [Trac](https://ghc.haskell.org/trac/ghc)
- Communicate that you're working on it
- [Add a test-case](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests/Adding)
- Fix the bug
- Test `make test TEST="XXX YYY"`
- Refer to ticket number in commit message
- [Validate](https://ghc.haskell.org/trac/ghc/wiki/TestingPatches)
	- Phabricator automatically validates on Harbormaster
	- Locally using `./validate`

---

### Contributing - Phabricator

- Code review and automated build tool
- Sign-up https://phabricator.haskell.org/
- Add SSH key https://phabricator.haskell.org/settings/
- Install recent [Arcanist CLI `arc`](https://ghc.haskell.org/trac/ghc/wiki/Phabricator#TheCLI:Arcanist)
- Install user certificate `arc install-certificate`
- Submit your changes `arc diff HEAD^`
    (`arc diff <rev-before-changes>`)
- Update ticket on Trac

---

# Example

---

# References

- [GHC wiki](https://ghc.haskell.org/trac/ghc/wiki)
	- [Newcomers guide](https://ghc.haskell.org/trac/ghc/wiki/Newcomers)
	- [GHC Commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary)
- Experience reports
	- [by Andrew Gibiansky](https://rawgit.com/gibiansky/4c54f767bf21a6954b23/raw/67c62c5555f40c6fb67b124307725df168201361/exp.html)
	- [by Annie Cherkaev](http://anniecherkaev.com/projects/contributing-to-ghc)
	- [by Moritz Angermann](https://medium.com/@zw3rk/contributing-to-ghc-290653b63147)
- Dive into GHC [1](http://www.stephendiehl.com/posts/ghc_01.html) [2](http://www.stephendiehl.com/posts/ghc_02.html) [3](http://www.stephendiehl.com/posts/ghc_03.html) by Stephen Diehl