# 抽象実行入門


本チュートリアルでは、"Rosendahl, Mads. Introduction to abstract interpretation. Computer Science University of Copenhagen (1995)." の内容を実装しながら解説していきます。

## 抽象実行の概要

抽象実行とは、プログラミング言語の「抽象解釈」を行うことで、その言語で書かれたプログラムの性質を推論する枠組みを意味します。

本記事では、あるプログラム $p$ について、標準解釈 $I_1$ と抽象解釈 $I_2$ の間に関係 $R$ が成立することを $I_1[p] R I_2[p]$ と表記します。

### Rule of Sign

まず簡単な例を通して、抽象実行の雰囲気をつかんでいきましょう。

Rule-of-Signとは、抽象実行の簡単な問題であり、プログラムの実行結果が正であるか負であるかを推定することを目指します。

ここで、以下のような文法を持つシンプルなプログラミング言語を考えます。

```
# syntax

exp ::= n           - 整数
    | exp + exp     - 加法
		| exp * exp     - 乗法
```

この言語の標準解釈、つまりこのプログラミング言語を実行するさいの通常の解釈を以下のように定義します。

```math
E_{std}[[exp]] : \mathcal{Z} \\\\

E_{std}[[n_i]] = n_i 				 \\
E_{std}[[exp_1 + exp_2]] = E_{std}[[exp_1]] + E_{std}[[exp_2]] \\
E_{std}[[exp_1 * exp_2]] = E_{std}[[exp_1]] * E_{std}[[exp_2]]
```

次に、Rule-of-Signを解くために必要な抽象解釈を定義していきます。

最初のステップとして、抽象値 $Sign = \{zero, pos, neg, num\}$ を導入します。この抽象値Sign上の加法・乗法は以下の通りに定義されます。

```
# 加法

⊕ : Sign × Sign → Sign

⊕    zero pos neg num
zero zero pos neg num
pos   pos pos num num
neg   neg num neg num
num   num num num num
```

```
# 乗法

⊗ : Sign × Sign → Sign

⊗    zero  pos  neg  num
zero zero zero zero zero
pos  zero  pos  neg  num
neg  zero  neg  pos  num
num  zero  num  num  num
```

次に、これらの新しい演算子を用いて、以下のような抽象解釈を定義します。


```math
E{ros}[[exp]] : Sign \\

E{ros}[[ni]] = sign(ni) \
E{ros}[[exp_1 + exp_2]] = E{ros}[[exp_1]] ⊕ E{ros}[[exp_2]] \
E{ros}[[exp_1 ∗ exp_2]] = E{ros}[[exp_1]] ⊗ E{ros}[[exp_2]] \
```

ここで、関数 `sign` は、`sign(x) = if x > 0 then pos else if x < 0 then neg else zero` と定義されます。

最後に標準解釈と抽象解釈の間にどんな関係が成り立つかを確認します。

- 抽象値 -> 整数

```math
\gamma : Sign -> P(\mathcal{Z}) \setminus \{\empty\}

\gamma(zero) = \{0\} 
\gamma(pos) = \{x | x > 0\} 
\gamma(neg) = \{x | x < 0\} 
\gamma(num) = \mathcal{Z}
```

- 整数 -> 実数

```math
\alpha : P(\mathcal{Z}) \setminus \{\empty\} -> Sign 

\alpha(X) = zero if  (X = {0}) 
     			= pos  if  (\all{x \in X}. x > 0) 
		      = neg  if  (\all{x \in X}. x < 0) 
					= num  otherwise
```

よって、標準解釈と抽象解釈の間には以下の関係が成立します。

```
\forall{s} \in Sign.  \alpha(\gamma(s)) = s 
\forall{X} \in P(\mathcal{Z}) \setminus \empty.  \gamma(\alpha(X)) \supseteq X
```
