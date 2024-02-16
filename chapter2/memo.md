d1 ∧ d2 = min(d1, d2)
d1 ∨ d2 = max(d1, d2)


g(x, y) = y ∧ (x ∨ g(x, y))

- 1st iteration

00 0
10 0
01 0
11 0

- 2nd iteration

00 0
10 0
01 0
11 1

- 3nd iteration (converge)

00 0
10 0
01 0
11 1


f(x, y, z) = (y ∧ z) ∨ f(z, x, f(y, 1, 1))

- 1st iteration

0で初期化

000 0
001 0
010 0
011 0
100 0
101 0
110 0
111 0

- 2nd iteration

000 0 = (0 ∧ 0) ∨ 0
001 0 = (0 ∧ 1) ∨ 0
010 0 = (1 ∧ 0) ∨ 0
011 1 = (1 ∧ 1) ∨ 0
100 0 = (0 ∧ 0) ∨ 0
