import Test.Tasty
import Test.Tasty.HUnit
import Logic
import Resolve

main = defaultMain unitTests

unitTests = testGroup "Unit tests"
    [ testCase "Consistent clauses are satisfiable" $
        satisfiable consistentClauses @?= True

    , testCase "Inconsistent clauses are unsatisfiable" $
          satisfiable inconsistentClauses @?= False

    , testCase "Clauses do not entail query when impossible" $
          clausesEntail weakClauses strongQuery @?= False

    , testCase "Resolution rule" $
        clausesEntail bootstrapClauses bootstrapQuery @?= True

    , testCase "Frege's theorem" $
        clausesEntail fregeClauses fregeQuery @?= True

    , testCase "Peirce's theorem" $
        clausesEntail peirceClauses peirceQuery @?= True

    , testCase "Destructive dilemma" $
        clausesEntail destructiveClauses destructiveQuery @?= True
    ]

a = Var "A"
b = Var "B"
p = Var "P"
q = Var "Q"
r = Var "R"

consistentClauses = [a, b, Neg $ Dis (Neg a) (Neg b)]

inconsistentClauses = [Neg a, a]

bootstrapClauses = [Dis (Neg a) p, Dis (Neg b) q, Dis a b]

bootstrapQuery = Query (Dis (Var "P") (Var "Q"))

weakClauses = [Dis a b]
strongQuery = Query a

fregeClauses = [Impl p (Impl q r)]
fregeQuery = Query $ Impl (Impl p q) (Impl p r)

peirceClauses = [Impl (Impl a b) a]
peirceQuery = Query a

destructiveClauses = [Impl p q, Impl a b, Dis (Neg q) (Neg b)]
destructiveQuery = Query $ Dis (neg p) (neg a)
