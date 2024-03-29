# MyAbsInterp
Implementation of Rosendahl, Mads. "Introduction to abstract interpretation." Computer Science University of Copenhagen (1995).

## Overview of Abstract Interpretation

The core idea of abstract interpretation is constructing an "abstract interpretation" of a programming language in addition to its usual meaning. The abstract interpretation can be used to reason the behaviour of the program.

In this article, we denote the standard interpretation (mapping from a program to a function) as `I_1` and the abstract interpretation as `I_2`. We also denote the relation between `I_1` and `I_2` as `R`: `I_1[p] R I_2[p]`, where `p` is the input program.

### Example.1 Rule-of-Sign

Rule-of-sigin is a simple abstract interpretation problem, where the goal is estimating whether the output of an expression is positive or negative.

#### Syntax

```
exp ::= n           - number
    | exp + exp     - addition
    | exp * exp     - multiplication
```

#### Standard Interpretation

```
E_{std}[[exp]] : \mathcal{Z} 
E_{std}[[n_i]] = n_i 
E_{std}[[exp_1 + exp_2]] = E_{std}[[exp_1]] + E_{std}[[exp_2]] 
E_{std}[[exp_1 * exp_2]] = E_{std}[[exp_1]] * E_{std}[[exp_2]]
```

#### Abstract Interpretation

We introduce a set of abstract values, `Sign = {zero, pos, neg, num}`. The adition and multiplication on `Sign` can be defined as the below tables.

```
⊕ : Sign × Sign → Sign

⊕    zero pos neg num
zero zero pos neg num
pos   pos pos num num
neg   neg num neg num
num   num num num num
```

```
⊗ : Sign × Sign → Sign

⊗    zero  pos  neg  num
zero zero zero zero zero
pos  zero  pos  neg  num
neg  zero  neg  pos  num
num  zero  num  num  num
```

Then, we define the abstract interpretation with the above operators.


```
E{ros}[[exp]] : Sign 
E{ros}[[ni]] = sign(ni) 
E{ros}[[exp_1 + exp_2]] = E{ros}[[exp_1]] ⊕ E{ros}[[exp_2]] 
E{ros}[[exp_1 ∗ exp_2]] = E{ros}[[exp_1]] ⊗ E{ros}[[exp_2]] 
```

, where `sign(x) = if x > 0 then pos else if x < 0 then neg else zero`.

#### Relation

- an abstract value -> a set of integers (concretisation)

```
γ : Sign -> P(\mathcal{Z}) \setminus {∅} 
γ(zero) = {0} 
γ(pos) = {x | x > 0} 
γ(neg) = {x | x < 0} 
γ(num) = \mathcal{Z}
```

- a set of integers -> an abstract value (abstraction)

```
α : P(\mathcal{Z}) \setminus {∅} -> Sign 
α(X) = zero if  (X = {0}) 
     = pos  if  (∀{x ∈ X}. x > 0) 
     = neg  if  (∀{x ∈ X}. x < 0) 
     = num  otherwise
```

The relationship  between `γ` and `α` is as follows.

```
∀{s} ∈ Sign.  α(γ(s)) = s 
∀{X} ∈ P(\mathcal{Z}) \setminus ∅.  γ(α(X)) \supseteq X
```

Then, the additino and multiplication can be written as follows.

```
s_1 ⊕ s_2 = α({x_1 + x_2 | x_1 ∈ γ(s_1) ∧ x_2 ∈ γ(s_2)}) 
s_1 ⊗ s_2 = α({x_1 ∗ x_2 | x_1 ∈ γ(s_1) ∧ x_2 ∈ γ(s_2)}) 
```

- Excercise

Prove `∀{exp}. {E_{std}[[exp]]} \subseteq γ(E_{ros}[[exp]])`.

### Example.2 Strictness Analysis

We call a function `f` is $strict$ if it maps the bottom element (meaning $undefined$) `⊥` to the bottom element; `f(⊥) = ⊥`. Thus, the results of calling `f` by value and calling `f` by need are the same.

#### A lazy functional language

- Semantics domains

```
D = V_{⊥}         - values
φ = (D^k -> D)^n  - function denotations
```

- Semantics functions

```
E[[exp]]  : φ -> D^k -> D
P[[prog]] : φ
```

- Definition

```
E[[c_i]]φv                      = const_i                                         
E[[x_i]]φv                      = v_i                                             
E[[a_i(e_1, ..., e_k)]]φv       = strict basic_i<E[[e_1]]φv, ..., E[[e_k]]φv>
E[[if e_1 then e_2 else e_3]]φv = cond(E[[e_1]]φv, E[[e_2]]φv, E[[e_3]]φv)
E[[f_i(e_1, ..., e_k)]]φv       = f_i<E[[e_1]]φv, ..., E[[e_k]]φv>            

P[[f_1(x_1, ..., x_k) = e_1
        .
        .
   f_n(x_1, ..., x_k) = e_n]]        = fix \lambda φ. <E[[e_1]] φ, ..., E[[e_2]] φ>
```

, where

```
strict f<v_1, ..., v_k> = if v_1 = ⊥ ∨ .... v_k = ⊥ then ⊥ else f(v_1, ..., v_k)
```

Each definition corresponds to the mapping of `name of the i-th constant -> value of the i-th constant`, `name of the i-th variable -> value of the i-th variable`, `name of the i-th standard operation -> strictly evaluated value of that operation on v`, `control flow`, and `name of the i-th user-defined function -> evaluated value of that function on v`, respectively.

#### Abstract domain

To examine the strictness of a function, we use a two-point domain named `\mathcal{2} = {0, 1}` ordered by `0 ⊑ 1`.

Here, we define two utility operators.

```
d_1 ∧ d_2 = min(d_1, d_2)
d_2 ∨ d_2 = max(d_1, d_2)
```

Then, we use `\mathcal{2}` to describe if an element in `D` is defined or not, meaning that it is `⊥` or not. Specifically, we define the abstract function as follows:

```
α : D → \mathcal{2}
α(d) = if d = ⊥ then 0 else 1
```

It is obvious that `α` holds `∀d ∈ D. d = ⊥ ⇔ α(d) = 0`.

#### Strictness function

We assume that we want to investigate the strictness of a function `f : D^k -> D`.

Let `f' \mathcal{2}^k -> 2` satisifies the following.

```
∀<d_1, . . . , d_k> ∈ D^k. α(f(d_1, . . . , d_k)) = f'(α(d_1), . . . , α(d_k))
```

If `f'` exists, we can check the strictness of `f` by checking all combinations of inputs.

For example, if `f'(1, ..., 1, 0, 1, ..., 1) = 0` (only the j-th position is 0), we can say that `f` is strict in its `j`-th argument.

- Undecidable

However, we cannot always construct such functions. One example is `g(x, y) = if x = 0 then y else 0`.

```
g(0, ⊥) = ⊥
g(1, ⊥) = 0

α(g(0, ⊥)) = 0 = g'(1, 0)
α(g(1, ⊥)) = 1 = g'(1, 0)
```

The above means that strictness property is $undeciable$.

#### Approximations

As an alternative approach, we can define two functions; upper bound of lower bound of `\alpha(f(d_1, ..., d_k))`.

```
∀<d_1, . . . , d_k> ∈ D^k. α(f(d_1, . . . , d_k)) ⊑ f^♯(α(d_1), . . . , α(d_k))

∀<d_1, . . . , d_k> ∈ D^k. α(f(d_1, . . . , d_k)) >= f^♭(α(d_1), . . . , α(d_k))
```

For example, if we know that `f^♯(1, ..., 1, 0, 1, ..., 1) = 0` for the above example, we can conclude that this function is strict for the j-th argument.

As a concrete example of `f^♯`, we can construct `mul^♯` for the lifted multplication as follows.

```
mul_⊥ : N_⊥ × N_⊥ → N_⊥
mul_⊥(x, y) = if x = ⊥ ∨ y = ⊥ then ⊥ else x ∗ y

mul^♯(x, y) = x ∧ y = min(x, y)
```

Another example is if-else condition.

```
cond^♯(b, x, y) = b ∧ (x ∨ y)
```

This means that when the condition is defined, at least one of the branch should be defined to satisfy that the entire expression is defined.

#### Strictness interpretation

- Sematic functions

```
E^♯[[exp]] : (\mathcal{2}^k -> \mathcal{2})^n -> \mathcal{2}^k -> \mathcal{2}
P^♯[[exp]] : (\mathcal{2}^k -> \mathcal{2})^n
```

- Definition

```
E^♯[[c_i]]φv                      = 1
E^♯[[x_i]]φv                      = v_i
E^♯[[a_i(e_1, ..., e_k)]]φv       = E^♯[[e_1]]φv ∧ · · · ∧ E^♯[[e_k]]φv 
E^♯[[if e_1 then e_2 else e_3]]φv = E^♯[[e_1]]φv ∧ (E^♯[[e_2]]φv ∨ E^♯[[e_3]]φv)
E^♯[[f_i(e_1, ..., e_k)]]φv       = φ_i<E^♯[[e_1]]φv, ..., E^♯[[e_k]]φv> 

P^#[[f_1(x_1, ..., x_k) = e_1
               .
               .
               .
     f_n(x_1, ..., x_k) = e_n]]   = fix \lambda φ. <E^♯[[e_1]]φ, ..., E^♯[[e_k]]φ>
```

#### Correctness

We need to prove that the semantics function `P^#` meets the following:

```
∀ρ, i : α((P[[p]] ↓ i)ρ) ⊑ (P^#[[p]] ↓ i)<α(ρ_1), . . . , α(ρ_k)>
```

Recall the followings!

```
α : D → \mathcal{2}
α(d) = if d = ⊥ then 0 else 1
```

```
Def: cpo

A partial order <D, ⊑> is said to be a cpo if any w-chain of D, d_0 ⊑ d_1 ⊑ ... ⊑ d_n ⊑ ..., satisfies that its lower upperbound, ⨆_{n \in w} d_n is within D
```

```
Def: monotonic

Let D and E be cpo. Then, f: D -> E is said to be monotonic if it satisfies ∀d, d' \in D. d ⊑ d' => f(d) ⊑ f(d')
```

```
Def: continuous

A monotonic function is said to be continuous, if it satisfies ⨆_{n \in w} f(d_n) = f(⨆_{n \in w} d_n) for all w-chain in D.
```

```
Def: fixedpoint

Let f: D -> D be a continuous function on cpo D that has a least element.

fix(f) = ⨆_{n \in w} f^{n}(⊥)
```

```
Def: inclusive

Let D be cpo. Then, we say that the subset P \subseteq D is inclusive, if for any w-chain in D, d_0 ⊑ d_1 ⊑ ... ⊑ d_n ⊑ ..., we have (d_n \in P for any n \in w) => (⨆_{n \in w} d_n \in P).
```

```
Def: fixpoint induction

Let D be a cpo with the least element of ⊥, f: D -> D be a continuous, and P be an inclusive subset of D. Then, if ⊥ \in P and ∀x \in D.x \in P => F(x) \in P, we have that fix(F) \in P.
```

The proof consists of a local part for structual induction and a global part for fixpoint induction.

##### Proof

We first define a relation between real functions and strictness functions. 

```
f  : D^k -> D
f^♯: \mathcal{2}^k -> \mathcal{2}

f R f^♯ <=> ∀ρ ∈ D^k, ρ^♯ ∈ \mathcal{2}^k. α(ρ_1) ⊑ ρ_1^♯ ∧ · · · ∧ α(ρk) ⊑ ρ_k^♯ ⇒ α(f(ρ)) ⊑ f^♯(ρ^♯)
```

(Again, we want `f^♯(ρ^♯)` since we can say that `f` is strict for the j-th argument if `f^♯(ρ^♯) = 0` with `p_j^♯ = 0`).

Then, we prove `P[[p]] ↓ i R P^♯[[p]] ↓ i` for `i = 1, ..., n` and all programs `p`.

- Local part

In the local part, we will prove the following for all expression `e`:

```
∀φ ∈ (D_k → D)^n, φ^♯ ∈ (\mathcal{2}^k → \mathcal{2})^n. φ_1 R φ_1^♯ ∧ · · · ∧ φ_n R φ_n^♯ ⇒ E[[e]]φ R E^♯[[e]]φ^♯
```

We show the above by the structual induction over all possible expressions.

1) For constant `c_i`, it is obviously that

```
∀ρ ∈ D^k, ρ^♯ ∈ \mathcak{2}^k. α(ρ_1) ⊑ ρ_1^♯ ∧ · · · ∧ α(ρ_k) ⊑ ρ_k^♯ ⇒ α(c_i) ⊑ 1
```
