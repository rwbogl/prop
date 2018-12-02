# proph

A simple resolution-based theorem prover for propositional logic in Haskell.

I started writing this small program to learn more about resolution-based
theorem proving and Haskell. I *initially* started working through [Write
Yourself a Scheme in 48
Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), but
it got boring. (Because of this starting point, much of the parsing components
are weak.)

Resolution is a simple logical inference rule. It merely states that, given `A + B`
and `~A + C`, you can conclude `B + C`, where `+` denotes "logical or" and `*`
denotes "logical and." In other words, you can *resolve out* complementary
variables.

The rule is pretty straightforward to prove, requiring only the law of the
excluded middle and De Morgan's laws. If `A`, then `~A` is false, so `C`. If
`~A`, then `A` is false, so `B`. Therefore `B + C`.

This argument is a special case of this more general rule:

```
(A -> P) * (B -> Q) * (A + B) -> P + Q
```

Let's ask `proph` to prove this for us. In the file `tests/em.ph` is an
equivalent form:

```
~A + P.
~B + Q.
A + B.
? P + Q.
```

Output:

```
$ ./main tests/em.ph
THEOREM. The clauses
	[~A + P,~B + Q,A + B]
imply the statement
	P + Q.
PROOF. Translate everything into conjunctive normal form:
	Clauses: [~A + P,~B + Q,A + B]
	Query: P + Q
Assume, for the sake of contradiction,
	~P * ~Q.
Then we may reason as follows.
The clauses
	~A + P
and
	A + B
imply
	P + B.
The clauses
	~B + Q
and
	~Q
imply
	~B.
The clauses
	~P
and
	P + B
imply
	B.
The clauses
	B
and
	~B
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

## Another example

In the file `tests/test.ph`:

```
(A + B) * (C + D).
~D.
~B.
?A * C.
?A * D.
```

Output:

```
$ ./main tests/test.ph
THEOREM. The clauses
	[(A + B) * (C + D),~D,~B]
imply the statement
	A * C.
PROOF. Translate everything into conjunctive normal form:
	Clauses: [(A + B) * (C + D),~D,~B]
	Query: A * C
Assume, for the sake of contradiction,
	~A + ~C.
Then we may reason as follows.
The clauses
	A + B
and
	~B
imply
	A.
The clauses
	C + D
and
	~D
imply
	C.
The clauses
	~A + ~C
and
	C
imply
	~A.
The clauses
	~A
and
	A
imply
	[].
But this is the empty clause, a contradiction!
Our original statement must follow.
		Q.E.D.
THEOREM. The clauses
	[(A + B) * (C + D),~D,~B]
DO NOT imply the statement
	A * D.
PROOF. It is routine to check that the resolution algorithm ends in saturation.
Therefore the statement does not follow.
(But it may follow under stronger assumptions!)
		Q.E.D.
```
