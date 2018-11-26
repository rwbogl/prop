# proph

A simple resolution-based theorem prover for propositional logic in Haskell.

This small program was started to learn more about resolution-based theorem
proving and Haskell. I started it after [Write Yourself a Scheme in 48
Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
became boring. (And because of this starting point, much of the parsing
components are weak.)

## Example

In the file `test.ph`:

```
~(A) + B.
~B + C.
A.
? C.
```

Output:

```
$ ./main test.ph

THEOREM. The clauses
    [~A + B,~B + C,A]
imply the statement
    C.
PROOF. Translate everything into conjunctive normal form:
    Clauses: [~A + B,~B + C,A]
    Query: C
Assume, for the sake of contradiction,
    ~C.
Then we may reason as follows.
The clauses
    ~A + B
and
    ~B + C
imply
    ~A + C.
The clauses
    A
and
    ~A + C
imply
    C.
The clauses
    ~C
and
    C
imply
    [].
But this is the empty clause, a contradiction!
Our original statement must follow.
        Q.E.D.
```
