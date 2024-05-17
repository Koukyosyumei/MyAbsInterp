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

ここで、Sign上の加法と乗法は以下のように書くことが可能です。

```
s_1 ⊕ s_2 = \alpha(\{x_1 + x_2 | x_1 \in γ(s_1) ∧ x_2 \in γ(s_2)\}) 
s_1 ⊗ s_2 = \alpha(\{x_1 ∗ x_2 | x_1 \in γ(s_1) ∧ x_2 \in γ(s_2)\}) 
```

### Strictness Analysis

次に、strictnessと呼ばれる性質についての解釈の例を扱います。

エラーや未定義出るような値をbottom要素と呼ぶことにしましょう。ここで、ある関数$f$がstrictであるとは、その関数がbottom要素`⊥`を引数に取ったとき、bottom要素を返すことを意味します (`f(⊥) = ⊥`)。

これは、関数をcalling by valueした場合とcalling by needした場合の結果が同じになることを意味します。

ここで、以下のような簡単な遅延評価言語を考えます。

- semantic domains

$$$
D = V_{⊥}         - values
\phi = (D^k -> D)^n  - 関数環境
$$$

- semantic functions

$$$
E[[exp]]  : \phi -> D^k -> D : 関数環境と値の集合をとって、値を返す
P[[prog]] : \phi
$$$

- definition

$$$
E[[c_i]] \phi v                      = const_i : i番目の定数の名前 -> i番目の定数の値                                     
E[[x_i]] \phi v                      = v_i : i番目の変数の名前 -> i番目の変数の値                                            
E[[a_i(e_1, ..., e_k)]] \phi v       = strict basic_i<E[[e_1]] \phi v, ..., E[[e_k]] \phi v> i番目の標準関数の名前 -> その標準関数をストリクとにvで評価した結果の値
E[[if e_1 then e_2 else e_3]] \phi v = cond(E[[e_1]] \phi v, E[[e_2]] \phi v, E[[e_3]] \phi v) : 制御フロー
E[[f_i(e_1, ..., e_k)]] \phi v       = f_i<E[[e_1]] \phi v, ..., E[[e_k]] \phi v> : i番目のユーザー定義関数の名前 -> その関数をvで評価した時の結果の値           

P[[f_1(x_1, ..., x_k) = e_1
        .
				.
	 f_n(x_1, ..., x_k) = e_n]]        = fix \lambda \phi. <E[[e_1]] \phi, ..., E[[e_2]] \phi>
$$$

ここで関数のストリクと評価を行う`strict`は以下のように定義される。

```
strict f<v_1, ..., v_k> = if v_1 = ⊥ ∨ .... v_k = ⊥ then ⊥ else f(v_1, ..., v_k)
```

つまり、引数に一つでもボトムが含まれている場合、ボトムを返し、そうでない場合は関数fを通常どうり評価する。


