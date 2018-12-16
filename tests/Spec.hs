import Test.Tasty
import Test.Tasty.HUnit
import Logic
import Resolve

main = defaultMain unitTests

unitTests = testGroup "Unit tests"
    [ testCase "Consistent clauses are satisfiable" $
        satisfiable consistentCNF @?= True

    , testCase "Inconsistent clauses are unsatisfiable" $
          satisfiable inconsistentCNF @?= False

    , testCase "Can prove resolution rule" $
        clausesEntail bootstrapClauses bootstrapQuery @?= True

    , testCase "Clauses do not entail query when impossible" $
          clausesEntail weakClauses strongQuery @?= False

    , testCase "Can prove Frege's theorem" $
        clausesEntail fregeClauses fregeQuery @?= True
    ]

a = Var "A"
b = Var "B"
p = Var "P"
q = Var "Q"
r = Var "R"

consistentCNF =
    [ [a]
    , [b]
    , [Neg (Dis (Neg a) (Neg b))]
    ]

inconsistentCNF =
    [ [Neg a]
    , [a]
    ]

bootstrapClauses =
    [ [Neg a, p]
    , [Neg b, q]
    , [a, b]
    ]

bootstrapQuery = Query (Dis (Var "P") (Var "Q"))

weakClauses = [[Dis a b]]
strongQuery = Query a

fregeClauses = clauseToCNF $ Impl p (Impl q r)
fregeQuery = Query $ Impl (Impl p q) (Impl p r)
