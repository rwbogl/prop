module Proof where
import Parser
import Resolve

clausesEntailProof :: [Term] -> Term -> IO ()
clausesEntailProof clauses (Query x) = do
    let cnfClauses = clausesToCNF clauses
        negated = clauseToCNF $ neg x
        proof = sat (cnfClauses ++ negated)
    case proof of
      Just tail -> printProof clauses x tail
      Nothing -> printFailure clauses x

printProof :: [Term] -> Term -> QueueItem -> IO ()
printProof clauses query queue = do
    putStrLn "THEOREM. The clauses"
    putStrLn $ "\t" ++ show clauses
    putStrLn "imply the statement"
    putStrLn $ "\t" ++ show query ++ "."
    putStrLn "PROOF. Translate everything into conjunctive normal form:"
    putStrLn $ "\tClauses: " ++ show (map cnf clauses)
    putStrLn $ "\tQuery: " ++ show (cnf query)
    putStrLn "Assume, for the sake of contradiction,"
    putStrLn $ "\t" ++ show (neg (cnf query)) ++ "."
    putStrLn "Then we may reason as follows."
    let proof = queueToList queue
    printProof' proof

printProof' :: [QueueItem] -> IO ()
printProof' [] = do
    putStrLn "But this is the empty clause, a contradiction!"
    putStrLn "Our original statement must follow."
    putStrLn "\t\tQ.E.D."
printProof' (head:tail) = do
    putStrLn "The clauses"
    putStrLn $ "\t" ++ show (unfoldDisList $ left head)
    putStrLn "and"
    putStrLn $ "\t" ++ show (unfoldDisList $ right head)
    putStrLn "imply"
    putStrLn $ "\t" ++
        case res head of
          [] -> "[]"
          x -> show (unfoldDisList x)
        ++ "."
    printProof' tail

printFailure :: [Term] -> Term -> IO ()
printFailure clauses query = do
    putStrLn "THEOREM. The clauses"
    putStrLn $ "\t" ++ show clauses
    putStrLn "DO NOT imply the statement"
    putStrLn $ "\t" ++ show query ++ "."
    putStr "PROOF. It is routine to check that the resolution algorithm ends "
    putStrLn "in saturation.\nTherefore the statement does not follow."
    putStrLn "(But it may follow under stronger assumptions!)"
    putStrLn "\t\tQ.E.D."

queueToList :: QueueItem -> [QueueItem]
queueToList head = build head []
    where build Empty acc = acc
          build head acc = build (prev head) (head : acc)
