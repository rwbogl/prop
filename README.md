# prop

A simple resolution-based theorem prover for propositional logic in Haskell.

Resolution is a simple logical inference rule. If we write `+` for "logical
or," and `*` for "logical and," then the resolution rule states that, if
`A + B` and `~A + C`, then `B + C`. In other words, you can *resolve out*
complementary variables. The rule is straightforward to prove, requiring only
the law of the excluded middle. If `A`, then `~A` is false, so `C`. If `~A`,
then `A` is false, so `B`. Therefore `B + C`.

This argument is a special case of this more general rule:

```
(A -> P) * (B -> Q) * (A + B) -> P + Q
```

Let's ask `prop` to prove this for us. In the file `tests/resolution.prop` is
an equivalent form:

```
A -> P.
B -> Q.
A + B.
? P + Q.
```

Output:

```
$ prop tests/resolution.prop
THEOREM. The clauses
	[A -> P,B -> Q,A + B]
imply the statement
	P + Q.
PROOF. Translate everything into conjunctive normal form:
	Clauses: [~A + P,~B + Q,A + B]
	Query: P + Q
Assume, for the sake of contradiction,
	~P * ~Q.
Then we may reason as follows.
The clauses
	[A + B,~A + P]
imply
	P + B.
The clauses
	[~B + Q,~Q]
imply
	~B.
The clauses
	[P + B,~P]
imply
	B.
The clauses
	[B,~B]
imply
	[].
But this is the empty clause, a contradiction!
Our original statement must follow.
		Q.E.D.
```

This proof is circular of course, but shows the power of the resolution rule.

The full algorithm is quite simple. Assume the negation of the desired
statement, then resolve until you reach a contradiction or cannot resolve any
further. If you reach a contradiction, then the original statement must have
been true. (Assuming that your initial clauses were consistent!) If you cannot
resolve any further, then the statement does not follow from your clauses. This
procedure is *complete* (it will always find an answer) and *sound* (that
answer is always correct).

In the current implementation, the program actually finds the *shortest*
possible proof by contradiction.

# Examples

All theorems in propositional logic are "routine" to prove, in the sense that
a computer can easily do it. `prop` is one such way to do this. Given a theorem
in propositional logic, we must only give `prop` the hypotheses as clauses, and
then ask if the conclusion can be proven from them. Below are two examples of
this: *Frege's theorem* and *Peirce's law*.

## Frege's theorem

In propositional logic, [Frege's
theorem](https://en.wikipedia.org/wiki/Frege%27s_theorem) is the tautology

```
(P -> (Q -> R)) -> ((P -> Q) -> (P -> R))
```

Prop can easily verify this implication. In `tests/frege.prop`:

```
P -> (Q -> R).
? (P -> Q) -> (P -> R).
```

Output:

```
$ prop tests/frege.prop
THEOREM. The clauses
	[P -> (Q -> R)]
imply the statement
	(P -> Q) -> (P -> R).
PROOF. Translate everything into conjunctive normal form:
	Clauses: [~P + (~Q + R)]
	Query: (P + (~P + R)) * (~Q + (~P + R))
Assume, for the sake of contradiction,
	(~P * (P * ~R)) + (Q * (P * ~R)).
Then we may reason as follows.
The clauses
	[~P + Q,~P + (~Q + R)]
imply
	~P + R.
The clauses
	[P,~P + R]
imply
	R.
The clauses
	[R,~R]
imply
	[].
But this is the empty clause, a contradiction!
Our original statement must follow.
		Q.E.D.
```

## Peirce's law

[Peirce's law](https://en.wikipedia.org/wiki/Peirce%27s_law) states that `((a
-> b) -> a) -> a`. This is trivial to check with `prop`. In
`tests/peirce.prop`:

```
((a -> b) -> a).
? a.
```

Output:

```
$ prop tests/peirce.prop
THEOREM. The clauses
	[(a -> b) -> a]
imply the statement
	a.
PROOF. Translate everything into conjunctive normal form:
	Clauses: [(a + a) * (~b + a)]
	Query: a
Assume, for the sake of contradiction,
	~a.
Then we may reason as follows.
The clauses
	[a + a,~a]
imply
	a.
The clauses
	[a,~a]
imply
	[].
But this is the empty clause, a contradiction!
Our original statement must follow.
		Q.E.D.
```
