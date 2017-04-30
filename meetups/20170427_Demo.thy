theory Demo imports
  Main
  "~~/src/HOL/Library/Code_Target_Int"
  "~~/src/HOL/Library/Code_Target_Nat"
  "~~/src/HOL/Library/Code_Test"
  "~~/src/HOL/Library/BNF_Corec"
  "$AFP/Coinductive/Coinductive_Nat"
begin

no_syntax "_bracket" :: "types => type => type" ("([_]/ \<Rightarrow> _)" [0, 0] 0) -- \<open>For list type syntax [_].\<close>
type_notation list ("[_]" [0] 999)
declare Pure.triv_forall_equality[simp del] implies_True_equals[simp del] -- \<open>Nicer simp traces\<close>

no_type_notation list ("[_]" [0] 999)
no_translations
  "[x, xs]" == "x:[xs]"
  "[x]" == "x:[]"
no_notation
  Nil ("[]") and
  Cons (infixr "#" 65) and
  append (infixr "@" 65) and
  Set.member  ("op :") and
  Set.member  ("(_/ : _)" [51, 51] 50) and
  map_add (infixl "++" 100)
hide_const (open) rev append Nil Cons
hide_type (open) list







text    \<open>

Functional Programming and Proving in Isabelle/HOL
Andreas Lochbihler, ETH Zurich








Overview

\<^item> What is Isabelle/HOL

\<^item> Functional programming

\<^item> Equational reasoning

\<^item> Termination proofs

\<^item> Streams






Isabelle/HOL

\<^item> Isabelle = proof assistant for developping machine-checked proofs
\<^item> HOL = higher order logic

  \<and>, \<or>, \<longrightarrow>, \<not>, True, False, \<forall>, \<exists>, \<dots>

  Goldbach: \<forall>x > 2. even x \<rightarrow> (\<exists>y z. prime y \<and> prime z \<and> x = y + z)

\<^item> Isabelle's kernel checks all proof steps according to proof rules

  Theorems are an abstract datatype:
  - ML functions for every proof rule
  - Type system ensures that only proven theorems can be constructed.

\<^item> Many packages for
  - searching for proofs
  - making function definitions





The prover IDE Isabelle/jEdit

\<close>


section \<open>List\<close>

datatype (set: 'a) list      ("[_]" [0] 999)
  = Nil               ("[]")
  | Cons (head: 'a) (tail: "'a list") (infixr ":" 65)
 for map: map

lemma head: "head xs = (case xs of [] \<Rightarrow> undefined | x : _ \<Rightarrow> x)"
by(simp add: head_def)

declare [[names_short]]
thm list.rec

export_code head in Haskell

translations
  "[x, xs]" == "x : [xs]"
  "[x]"     == "x : []"

fun append :: "['a] \<Rightarrow> ['a] \<Rightarrow> ['a]" (infixr "++" 65) where
  append_Nil: "[] ++ ys = ys"
| append_Cons: "(x : xs) ++ ys = x : (xs ++ ys)"

value [GHC] "[1,2,3] ++ [4,5,6::int]"

export_code append in Haskell

lemma append_Nil2 [simp]: "xs ++ [] = xs"
by(induction xs) simp_all
(* proof(induction xs)
  case Nil
  then show ?case by(fact append_Nil)
next
  case (Cons x xs)
  then show ?case using [[simp_trace_new mode=full]] by simp
  have "(x : xs) ++ [] = x : (xs ++ [])" by(fact append_Cons)
  also have "x : (xs ++ []) = x : xs" using Cons.IH by(simp)
  finally show "(x : xs) ++ [] = x : xs" by this
qed
 *)

fun rev :: "['a] \<Rightarrow> ['a]" where
  "rev [] = []"
| "rev (x : xs) = rev xs ++ [x]"

export_code rev in Haskell

lemma append_assoc: "(xs ++ ys) ++ zs = xs ++ (ys ++ zs)"
by(induction xs) simp_all

lemma rev_append: "rev (xs ++ ys) = rev ys ++ rev xs"
proof(induction xs)
  case Nil
  then show ?case by(simp)
next
  case (Cons x xs)
  then show ?case by(simp add: append_assoc)
qed

lemma rev_rev: "rev (rev xs) = ys"
nitpick
apply(induction xs)
subgoal by simp
subgoal for x xs apply(simp add: rev_append) done
done

fun qrev :: "['a] \<Rightarrow> ['a] \<Rightarrow> ['a]" where
  "qrev a [] = a"
| "qrev a (x:xs) = qrev (x:a) xs"

lemma qrev': 
  "\<forall>A. qrev A xs = rev xs ++ A"
apply(induction xs)
apply simp
apply(simp add: append_assoc)
done

lemma rev_qrev: "rev xs = qrev [] xs"
sledgehammer
  by (simp add: qrev')
apply(simp add: qrev')
done

lemma stupid: "rev xs = rev xs" by simp

declare rev_qrev[code]

export_code rev in Haskell


text \<open>
Termination proofs

Task: Define a function merge that merges two lists:

  merge [a,b,c] [1,2,3,4] = [a,1,b,2,c,3,4]
\<close>

partial_function (lfp) problem :: "unit \<Rightarrow> enat" where "problem x = problem x + 1"


consts merge :: "['a] \<Rightarrow> ['a] \<Rightarrow> ['a]" 
specification (merge)
  merge_Nil: "merge [] ys = ys"
  merge_Cons: "merge (x:xs) ys = x : merge ys xs"
by pat_completeness auto
termination sorry (* by size_change *)
(* apply(relation "measure (\<lambda>(xs, ys). size xs + size ys)")
apply simp
apply simp
done
 *)

thm merge.induct

lemma "size (merge xs ys) = size xs + size ys"
apply(induction xs ys rule: merge.induct)
apply simp
apply simp
done



section \<open> STREAM = infinite list \<close>

codatatype 'a stream = SCons (shd: 'a) (stl: "'a stream") (infixr "#" 80)
  for map: smap

primcorec up :: "int \<Rightarrow> int stream"
where "up n = n # up (n + 1)"

print_theorems

fun stake :: "nat \<Rightarrow> 'a stream \<Rightarrow> 'a list"
where
  "stake 0 s = []"
| "stake (Suc n) s = shd s : stake n (stl s)"

value [GHC] "stake 50 (up 7)"

export_code up stake in Haskell

primcorec smerge :: "'a stream \<Rightarrow> 'a stream \<Rightarrow> 'a stream"
where "smerge xs ys = shd xs # smerge ys (stl xs)"

value [GHC] "stake 50 (smerge (up 1) (up (- 100)))"

declare stream.map_sel[simp]

lemma smap: "smap f xs = f (shd xs) # smap f (stl xs)"
by(rule stream.expand) simp

lemma smap_smerge: "smap f (smerge xs ys) = smerge (smap f xs) (smap f ys)"
apply(coinduction arbitrary: xs ys rule: stream.coinduct)
apply simp
subgoal for xs ys
  apply(rule exI[where x="ys"])
  apply(rule exI[where x="stl xs"])
  apply simp
  done
done


end
