# proph

A simple resolution-based theorem prover for propositional logic in Haskell.

I started writing this small program to learn more about resolution-based
theorem proving and Haskell. I *initially* started working through [Write
Yourself a Scheme in 48
Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), but
it got boring. (Because of this starting point, much of the parsing components
are weak.)

## Example

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
PROOF. It is routine to check that the resolution algorithm ends in saturation. Therefore the statement does not follow. (But it may follow under stronger assumptions!)
		Q.E.D.
```
