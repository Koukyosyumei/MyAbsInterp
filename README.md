# MyAbsInterp
Implementation of Rosendahl, Mads. "Introduction to abstract interpretation." Computer Science University of Copenhagen (1995).

## Overview of Abstract Interpretation

The core idea of abstract interpretation is constructing an "abstract interpretation" of a programming language in addition to its usual meaning. The abstract interpretation can be used to reason the behaviour of the program.

In this article, we denote the standard interpretation (mapping from a program to a function) as $I_1$ and the abstract interpretation as $I_2$. We also denote the relation between $I_1$ and $I_2$ as $R$: $I_1[p] R I_2[p]$, where $p$ is the input program.

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
E_{std}[[exp]] : \mathcal{Z} \\
E_{std}[[n_i]] = n_i \\
E_{std}[[exp_1 + exp_2]] = E_{std}[[exp_1]] + E_{std}[[exp_2]] \\
E_{std}[[exp_1 * exp_2]] = E_{std}[[exp_1]] * E_{std}[[exp_2]]
```

#### Abstract Interpretation

We introduce a set of abstract values, $Sign = \{zero, pos, neg, num\}$. The adition and multiplication on $Sign$ can be defined as the below tables.

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
E{ros}[[exp]] : Sign \\
E{ros}[[ni]] = sign(ni) \\
E{ros}[[exp_1 + exp_2]] = E{ros}[[exp_1]] ⊕ E{ros}[[exp_2]] \\
E{ros}[[exp_1 ∗ exp_2]] = E{ros}[[exp_1]] ⊗ E{ros}[[exp_2]] \\
```

, where $sign(x)$ = if $x > 0$ then $pos$ else if $x < 0$ then $neg$ else $zero$.

#### Relation

- an abstract value $\to$ a set of integers (concretisation)

```
\gamma : Sign \to P(\mathcal{Z}) \setminus \{\empty\} \\
\gamma(zero) = \{0\} \\
\gamma(pos) = \{x | x > 0\} \\
\gamma(neg) = \{x | x < 0\} \\
\gamma(num) = \mathcal{Z}
```

- a set of integers $\to$ an abstract value (abstraction)

```
\alpha : P(\mathcal{Z}) \setminus \{\empty\} \to Sign \\
\alpha(X) = zero if  (X = \{0\}) \\
          = pos  if  (\forall{x \in X}. x > 0) \\
          = neg  if  (\forall{x \in X}. x < 0) \\
          = num  otherwise
```

The relationship  between $\gamma$ and $\alpha$ is as follows.

```
\forall{s} \in Sign.  \alpha(\gamma(s)) = s \\
\forall{X} \in P(\mathcal{Z}) \setminus \empty.  \gamma(\alpha(X)) \supseteq X
```

Then, the additino and multiplication can be written as follows.

```
s_1 ⊕ s_2 = \alpha(\{x_1 + x_2 | x_1 ∈ \gamma(s_1) ∧ x_2 \in \gamma(s_2)\}) \\
s_1 ⊗ s_2 = \alpha(\{x_1 ∗ x_2 | x_1 ∈ \gamma(s_1) ∧ x_2 \in \gamma(s_2)\}) \\
```

- Excercise

Prove $\forall{exp}. \{E_{std}[[exp]]\} \subseteq \gamma(E_{ros}[[exp]])$.

### Example.2 Strictness Analysis
