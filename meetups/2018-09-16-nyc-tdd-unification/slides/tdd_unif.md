% Test-Driven Development of a Unification Algorithm
% Simon Meier, Digital Asset
% September, 2018


About me
========

- learned about Haskell during my ETH CS studies (2003)
- first "commercial" Haskell development during PhD (2007 - 2012)
- Haskell engineer at Better AG (2013 - 2014)
- post-doctoral researcher at IBM Research (2014 - 2015)
- Haskell engineer at Elevence Digital Finance AG (2015 - 2016)
- Haskell engineer and language engineering lead at Digital Asset (since 2016)


Today's Live Coding Experience
=========

 *Unification Theory + TDD + Property-based Testing*



Why unification matters
=======================

- key algorithm in symbolic reasoning: theorem provers, type inference, symbolic math

```
Prelude> let foo x = [x, Just x]

<interactive>:4:17: error:
    • Occurs check: cannot construct the infinite type: a ~ Maybe a
    • In the expression: Just x
      In the expression: [x, Just x]
      In an equation for ‘foo’: foo x = [x, Just x]
    • Relevant bindings include
        x :: a (bound at <interactive>:4:9)
        foo :: a -> [a] (bound at <interactive>:4:5)
```


Theory: Terms
=============

- Given variables *V* and constructors *C*
- Define *first-order terms T* as smallest set closed under
  - *v ∈ T* for every *v ∈ V*
  - *c(t₁, …, tₙ)* for every *c ∈ C* and *t₁, …, tₙ ∈ T*


Example: Haskell types
=============

Variables: `a`, `b`, `c`

Constructors: `Int`, `Char`, `FunArr`, `Maybe`, `Pair`

Example Terms:

```
| Term                      | Haskell type                        |
|---------------------------|-------------------------------------|
|                           |                                     |
| Int()                     | Int                                 |
|                           |                                     |
| a                         | a                                   |
|                           |                                     |
| Maybe(Pair(b,a))          | Maybe (b, a)                        |
|                           |                                     |
| FunArr(a, FunArr(b, c))   | a -> b -> c                         |
|                           |                                     |
| Char(Int(),Int())         | Char Int Int   -- ill-kinded type   |
|                           |                                     |
```


Theory: Substitutions
===========

- substitions map finite sets of variables *x₁, …, xₖ* to terms *t₁, …, tₖ*
  - written as *{ x₁ ↦ t₁, …, xₖ ↦ tₖ }*
- applying substitution *σ* to term *t*
  - postfix notation: *tσ*
  - *t { x₁ ↦ t₁, …, xₖ ↦ tₖ }* means to simultaneously replace every *xᵢ* in *t* by *tᵢ*

Example:

```
Int {b ↦ a} == Int

(Pair(a, b)){b ↦ Int}  ==  Pair(a,Int)

(Pair(a, b)){b ↦ a, c ↦ d} ==  Pair(a,a)
```


Theory: Unification Problem (Simplified)
==============

Given terms *t₁* and *t₂*,
determine existence of substitution *σ* such that *t₁σ = t₂σ*.


Example: Unification
===========

```
a ~ Int                     solved by {a ↦ Int}

Int ~ Maybe a               unsolvable

a ~ Maybe a                 unsolvable

Pair(a,b) ~ Maybe(a)        unsolvable

Pair(Int) ~ Pair(Int, Int)  unsolvable

Pair(a,b) ~ Pair(a, Int)    solved by {b ↦ Int} and {a ↦ Int, b ↦ Int} 

Pair(a,b) ~ Pair(b, Int)    solved only by {a ↦ Int, b ↦ Int}

Pair(a,b) ~ Pair(b,a)       solved by {a ↦ b} and {b ↦ a} and others

```


Theory: Unification Problem
==============

- usually one is interested in the most-general unifier
  - Given terms *t₁* and *t₂*, determine existence of substitution *σ* such that 
    1. *t₁σ = t₂σ* and
    2. for all other substitutions *ρ* with *t₁ρ = t₂ρ* there exists a substitution *τ* with *ρ = τ ∘ σ*.
- further reading [Franz Baader and Wayne Snyder (2001). "Unification Theory".](http://www.cs.bu.edu/~snyder/publications/UnifChapter.pdf)



Test-Driven Development
===========

![](img/tdd.png)


Test-Driven Development
===========

- **systematic process for writing testable code that is tested**
- caveat: tests are often an incomplete specification
  - [2017 - Automatic repair of real bugs in java: a large-scale experiment on the defects4j dataset](https://link.springer.com/article/10.1007/s10664-016-9470-4):
    - 9 correct auto-repairs out of
    - 47 test-suite passing auto-generated patches
    - out of a total of 224 failing tests


Property-Based Testing
================


- Example:
  - `unify :: Term -> Term -> Maybe Subst`
  - property: *∀ t₁ t₂. (∃ σ. t₁σ = t₂σ) ⇔ (∃ σ'. `unify` t₁ t₂ = `Just` σ' ∧ t₁σ' = t₂σ')*

- Approach
  - assume properties have many, small(ish) counter-examples
  - replace universal quantifiers by random sampling
  - replace existential quanitifers by functions computing witnesses


Property-Based Testing: Caveats
==========

- beware of sampling bias
- not all properties have small(ish) counter-examples
- corner cases can be very hard to sample
- exponentially large spaces are LARGE
- no replacement for unit tests



TDD Time!
========

- Create new project and install `ghcid`

```
stack new tdd-unif
cd tdd-unif
stack install ghcid
```

- Remove executable stanza from `package.yaml`

- Start repl, automatic test loop, and code coverage

```
stack repl --test
ghcid --command='stack repl --test' --test=':main'
stack test --coverage --file-watch
```

- Write first test :-)


Property-based testing: logic in action
==================

- original property:
  - ∀ t₁ t₂. (∃ σ. t₁σ = t₂σ) ⇔ (∃ σ'. `unify` t₁ t₂ = `Just` σ' ∧ t₁σ' = t₂σ')

- equivalent to conjunction of
  - ∀ t₁ t₂. (∃ σ. t₁σ = t₂σ) ⇒ (∃ σ'. `unify` t₁ t₂ = `Just` σ' ∧ t₁σ' = t₂σ')
  - ∀ t₁ t₂. (∃ σ. t₁σ = t₂σ) ⇐ (∃ σ'. `unify` t₁ t₂ = `Just` σ' ∧ t₁σ' = t₂σ')

- equivalent to conjunction of
  - ∀ t₁ t₂ σ. t₁σ = t₂σ ⇒ (∃ σ'. `unify` t₁ t₂ = `Just` σ' ∧ t₁σ' = t₂σ')
  - ∀ t₁ t₂. `unify` t₁ t₂ = `Nothing` ⇒ (∀ σ. t₁σ ≠ t₂σ)


Conclusions
=========


- TDD
  - works for writing testable code that is tested
  - no replacement for understanding the problem domain
- testing
  - sampling bias is real => use unit tests and property based tests
  - incomplete specifications leave room for wrong implementations
- implementing unification algorithms requires diligence and care

*If you like what you've seen in this talk: Digital Asset is hiring [language engineers](https://www.digitalasset.com/careers/978901/language-engineer) :-)*


Background: Unification Algorithm - Take 1
===================

σ ⊢ t₁₁ ~ t₁₂, ..., tₖ₁ ~ tₖ₂     there exists a substitution ρ such that (tᵢ₁σ)ρ = (tᵢ₂σ)ρ for all 1 ≤ i ≤ k

Rewriting rules

```
1. σ ⊢ t₁ ~ t₂, E   →   σ ⊢ t₁σ ~ t₂σ, E       if t₁σ ≠ t₁ or t₂σ ≠ t₂

2. σ ⊢ t₁ ~ t₂, E   →   σ ⊢ E                  if t₁ = t₂

3. σ ⊢ x₁ ~ t₂, E   →   σ, x₁ ↦ t₂ ⊢ E

4. σ ⊢ t₁ ~ x₂, E   →   σ ⊢ x₂ ~ t₁, E

4.   σ ⊢ c₁(t₁₁, …, t₁ₖ) ~ c₂(t₂₁, …, x₂ₗ), E
   → σ ⊢ t₁₁ ~ t₂₁, …, t₁ₖ ~ t₂ₗ, E             if c₁ = c₂ and k = l

5.    σ ⊢ c₁(t₁₁, …, t₁ₖ) ~ c₂(t₂₁, …, x₂ₗ), E
   →  ⊥                                        if c₁ ≠ c₂ or k ≠ l
```



Background: Unification Algorithm - Take 2
====


Rewriting rules

```
1. σ ⊢ t₁ ~ t₂, E   →   σ ⊢ t₁σ ~ t₂σ, E       if t₁σ ≠ t₁ or t₂σ ≠ t₂

2. σ ⊢ t₁ ~ t₂, E   →   σ ⊢ E                  if t₁ = t₂

3. σ ⊢ x₁ ~ t₂, E   →   {x₁ ↦ t₂} ∘ σ ⊢ E      if x₁ ∉ vars(t₂)

3. σ ⊢ x₁ ~ t₂, E   →   ⊥                      if x₁ ∈ vars(t₂)

4. σ ⊢ t₁ ~ x₂, E   →   σ ⊢ x₂ ~ t₁, E

4.   σ ⊢ c₁(t₁₁, …, t₁ₖ) ~ c₂(t₂₁, …, x₂ₗ), E
   → σ ⊢ t₁₁ ~ t₂₁, …, t₁ₖ ~ t₂ₗ, E             if c₁ = c₂ and k = l

5.    σ ⊢ c₁(t₁₁, …, t₁ₖ) ~ c₂(t₂₁, …, x₂ₗ), E
   →  ⊥                                        if c₁ ≠ c₂ or k ≠ l
```

