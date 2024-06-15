本記事では、"Rosendahl, Mads. Introduction to abstract interpretation. Computer Science University of Copenhagen (1995)." の内容を実装しながら解説していきます。

## 抽象実行の概要

抽象実行とは、プログラミング言語の「抽象解釈」を行うことで、その言語で書かれたプログラムの性質を推論する枠組みを意味します。

本記事では、あるプログラム $p$ について、通常の解釈 $I_1$ と抽象解釈 $I_2$ の間に関係 $R$ が成立することを $I_1[p] R I_2[p]$ と表記します。

## Rule of Sign

まず簡単な例を通して、抽象実行の雰囲気をつかんでいきましょう。

Rule-of-signとは、抽象実行の簡単な問題であり、プログラムの実行結果が正であるか負であるかを推定することを目指します。

ここで、以下のような文法を持つシンプルなプログラミング言語を考えます。

ここで、以下のような文法を持つシンプルなプログラミング言語を考えます。

```
# syntax

exp ::= n           - 整数
	| exp + exp     - 加法
	| exp * exp     - 乗法
```

この言語の標準解釈、つまりこのプログラミング言語を実行するさいの通常の解釈を以下のように定義します。

```math
\begin{align}
    E_{std}[[exp]] &:: \mathcal{Z} \\
    E_{std}[[n_i]] &= n_i \\
    E_{std}[[exp_1 + exp_2]] &= E_{std}[[exp_1]] + E_{std}[[exp_2]] \\
    E_{std}[[exp_1 * exp_2]] &= E_{std}[[exp_1]] * E_{std}[[exp_2]]
\end{align}
```

次に、Rule-of-Signを解くために必要な抽象解釈を定義していきます。

最初のステップとして、抽象値 $Sign = \\{zero, pos, neg, num\\}$ を導入します。この抽象値$Sign$上の加法・乗法は以下の通りに定義されます。

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
\begin{align}
    E_{ros}[[exp]] &:: Sign \\
    E_{ros}[[ni]] &= sign(ni) \\
    E_{ros}[[exp_1 + exp_2]] &= E_{ros}[[exp_1]] ⊕ E_{ros}[[exp_2]] \\
    E_{ros}[[exp_1 ∗ exp_2]] &= E_{ros}[[exp_1]] ⊗ E_{ros}[[exp_2]]
\end{align}
```

ここで、関数 $sign$ は、`sign(x) = if x > 0 then pos else if x < 0 then neg else zero` と定義されます。

さて、ここで標準解釈と抽象解釈の間にはどんな関係が成立するでしょうか?

- 抽象値 -> 整数

```math
\begin{align}
    &\gamma :: Sign \rightarrow P(\mathcal{Z}) \setminus \{\emptyset\} \\
    
    &\gamma(zero) = \{0\} \\
    &\gamma(pos) = \{x | x > 0\} \\ 
    &\gamma(neg) = \{x | x < 0\} \\
    &\gamma(num) = \mathcal{Z}
\end{align}
```

- 整数 -> 実数

```math
\begin{align}
\alpha &:: P(\mathcal{Z}) \setminus \{\emptyset\} \rightarrow Sign \\
\alpha(X) &= zero \quad (X = {0}) \\
          &= pos  \quad (\forall{x \in X}. x > 0) \\
		  &= neg  \quad (\forall{x \in X}. x < 0) \\
		  &= num  \quad \text{otherwise}
\end{align}
```

よって、標準解釈と抽象解釈の間には以下の関係が成立します。

```math
\forall{s} \in Sign.  \alpha(\gamma(s)) = s 
```
```math
\forall{X} \in P(\mathcal{Z}) \setminus \emptyset.  \gamma(\alpha(X)) \supseteq X
```

ここで、Sign上の加法と乗法は以下のように書くことが可能です。

```math
\begin{align}
s_1 ⊕ s_2 &= \alpha(\{x_1 + x_2 | x_1 \in \gamma(s_1) ∧ x_2 \in \gamma(s_2)\}) \\
s_1 ⊗ s_2 &= \alpha(\{x_1 ∗ x_2 | x_1 \in \gamma(s_1) ∧ x_2 \in \gamma(s_2)\}) 
\end{align}
```

## Strictness Analysis

次に、strictnessと呼ばれる性質についての解釈の例を扱います。

エラーや未定義である値をbottom要素と呼ぶことにしましょう。ここで、ある関数$f$がstrictであるとは、その関数がbottom要素$⊥$を引数に取ったとき、bottom要素を返すことを意味します ($f(⊥) = ⊥$)。

ここで、以下のような簡単な遅延評価言語を考えます。

- semantic domains

```math
\begin{align}
    D &= V_{⊥}         \\
\phi &= (D^k \rightarrow D)^n  \\
\end{align}
```

ここで、$D$は値の集合、$\phi$は関数環境を意味します。

- semantic functions

```math
\begin{align}
    E[[exp]]  &:: \phi \rightarrow D^k \rightarrow D  \\
    P[[prog]] &:: \phi
\end{align}
```

- definition

```math
\begin{align}
    E[[c_i]] \phi v  &= const_i \quad (i番目の定数の名前 \rightarrow i番目の定数の値) \\                                     
E[[x_i]] \phi v      &= v_i \quad (i番目の変数の名前 \rightarrow i番目の変数の値) \\                                            
E[[a_i(e_1, ..., e_k)]] \phi v  &= \text{strict } basic_i(E[[e_1]] \phi v, ..., E[[e_k]] \phi v) \quad (i番目の標準関数の名前 \rightarrow その標準関数をストリクトにvで評価した結果の値) \\
E[[\text{if } e_1 \text{ then } e_2 \text{ else } e_3]] \phi v &= cond(E[[e_1]] \phi v, E[[e_2]] \phi v, E[[e_3]] \phi v) \quad (制御フロー) \\
E[[f_i(e_1, ..., e_k)]] \phi v       &= f_i(E[[e_1]] \phi v, ..., E[[e_k]] \phi v) \quad (i番目のユーザー定義関数の名前 \rightarrow その関数をvで評価した時の結果の値)          \\ 

P[[f_1(x_1, ..., x_k) = e_1, \\..\\..\\ &\\ f_n(x_1, ..., x_k) = e_n]]  &= fix \lambda \phi. (E[[e_1]] \phi, ..., E[[e_2]] \phi)
\end{align}
```

ここで関数のストリクト評価を行う`strict`は以下のように定義されます。

```math
\text{strict } f(v_1, ..., v_k) = \text{ if } v_1 = ⊥ ∨ .... v_k = ⊥ \text{ then } ⊥ \text{ else } f(v_1, ..., v_k)
```

つまり、引数に一つでもボトムが含まれている場合、ボトムを返し、そうでない場合は関数fを通常通り評価します。

- 抽象領域

関数のストリクト性を検査するために、二値から成る抽象領域 $\mathcal{2} = \\{0, 1\\}$ (ただし、$0 \sqsubseteq 1$ )を導入します。この抽象領域を、$D$ と以下のように対応付けます。

```math
\begin{align}
\alpha &: D \rightarrow \mathcal{2} \\
\alpha(d) &= \text{if } d = ⊥ \text{ then } 0 \text{ else } 1
\end{align}
```

さらに、この抽象領域上で以下の二つの演算子を導入します。

```math
\begin{align}
  d_1 \land d_2 &= min(d_1, d_2) \\
  d_1 \lor d_2 &= max(d_1, d_2)
\end{align}
```

- 理想的な抽象化

今、$f : D^k \rightarrow D$ のストリクト性を調べるとします。ここで、以下を満たすような抽象領域上の関数 $f'$ が存在すると仮定します。

```math
\forall{d_1, ..., d_k} \in D^k. \alpha(f(d_1, ..., d_k)) = f'(\alpha(d_1), ..., \alpha(d_k))
```

このような $f'$ があれば、すべての入浴の組み合わせに対して、 $f$ のストリクト性を検査することが可能です。例えばj番目の引数のみを0・他の引数を全て1にした時、 $f'(1, ..., 1, 0, 1, ..., 1) = 0$であるならば、 $f'$ はj番目の引数についてストリクトであると言えます。

しかし、このような関数が常に存在するとは限りません。例えば、 $g(x, y) = \text{ if } x = 0 \text{ then } y \text{ else } 0$ を考えると、

```math
\begin{align}
	&g(0, ⊥) = ⊥ \\
	&g(1, ⊥) = 0 \\
	&\alpha(g(0, ⊥)) = 0 = g'(1, 0) \\
	&\alpha(g(1, ⊥)) = 1 = g'(1, 0)
\end{align}
```

となり、このような $g'$ を定めることはできないことが分かります。

- 近似

そこで、 $\alpha(f(d_1, ..., d_k))$ の値そのものではなく、上界もしくは下界に注目します。

```math
\begin{align}
\forall{d_1, ..., d_k} \in D^k. \alpha(f(d_1, ..., d_k)) &\sqsubseteq f^♯(\alpha(d_1), ..., \alpha(d_k)) \\
\forall{d_1, ..., d_k} \in D^k. \alpha(f(d_1, ..., d_k)) &\sqsupseteq f^♭(\alpha(d_1), ..., \alpha(d_k))
\end{align}
```

このような上界・下界を得ることができれば、例えば $f^♯(1, ..., 1, 0, 1, ..., 1) = 0$ であるときに、 $f$ はj番目の引数についてストリクトであると言えます。

このような上界の例として、ボトム要素に対応した乗算 $mul_⊥$ に対する $mul^♯$ を以下のように定めることができる。

```math
\begin{align}
mul_⊥ &: N_⊥ \times N_⊥ \rightarrow N_⊥ \\
mul_⊥(x, y) &= \text{ if } x = ⊥ \lor y = ⊥ \text{ then } ⊥ \text{ else } x ∗ y \\

mul^♯(x, y) &= x \land y = min(x, y)
\end{align}
```

また条件分岐については、いかのように上界を定めることができる。

```math
cond^♯(b, , y) = b \land (x \lor y)
```

これは、条件分岐が定義済みの出力を返すためには、条件そのものおよび、分岐の少なくとも片方が定義済みである必要があることを意味している。

- 抽象解釈

以上の議論を基に、上界を用いた抽象解釈を以下のように定める。





