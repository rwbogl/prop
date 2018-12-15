module Logic
    ( Term(..)
    , cnf
    , neg
    , clauseToCNF
    , clausesToCNF
    , DisList
    , CNF
    , unfoldDisList
    ) where

data Term = Var String
          | Dis Term Term
          | Con Term Term
          | Impl Term Term
          | Neg Term
          | Query Term
          deriving (Eq, Ord)

instance Show Term where show = showTerm

-- This represents a sequence of disjunctions of literals joined with
-- conjunctions.
type DisList = [Term]
type CNF = [DisList]

showTerm (Var s) = s
showTerm (Dis x y) = wrapComplex x ++ " + " ++ wrapComplex y
showTerm (Con x y) = wrapComplex x ++ " * " ++ wrapComplex y
showTerm (Neg x) = "~" ++ wrapComplex x
showTerm (Impl x y) = wrapComplex x ++ " -> " ++ wrapComplex y
showTerm (Query x) = "?" ++ wrapComplex x

wrapComplex :: Term -> String
wrapComplex (Var x) = x
-- We don't need to wrap negations in parenthesis if they're binding to a
-- variable. Thus, just show it if that's the case.
wrapComplex exp@(Neg (Var s)) = show exp
wrapComplex other = "(" ++ show other ++ ")"

showDisList :: DisList -> String
showDisList = show . unfoldDisList

-- Negate a term.
neg :: Term -> Term
neg (Var a) = Neg (Var a)
neg (Dis x y) = Con (neg x) (neg y)
neg (Con x y) = Dis (neg x) (neg y)
neg (Query x) = Query $ neg x
neg (Impl x y) = Con x (neg y)
neg (Neg t) = t

{-| Unfold a DisList into a term. -}
unfoldDisList :: DisList -> Term
unfoldDisList [x] = x
unfoldDisList (x:xs) = Dis x (unfoldDisList xs)

-- Flatten an n-ary disjunction into a list of literals.
flattenDis :: Term -> [Term]
flattenDis (Dis x y) = flattenDis x ++ flattenDis y
flattenDis other = [other]

-- Split the top-level conjunctives off into separate lists.
splitCons :: [Term] -> [Term]
splitCons = foldr (\term acc -> splitCon term ++ acc) []
    where splitCon (Con x y) = splitCon x ++ splitCon y
          splitCon other = [other]

{-| Translate a term into conjunctive normal form. -}
cnf :: Term -> Term
-- Recursively apply De Morgan's law.
cnf (Dis left right) =
    case (left, right) of
      (x `Con` y, right) -> cnf (x `Dis` right) `Con` cnf (y `Dis` right)
      (left, x `Con` y) -> cnf (x `Dis` left) `Con` cnf (y `Dis` left)
      _ -> Dis left right
-- Swap argument order to fall into first case.
cnf (Impl x y) = cnf $ Dis (neg x) y
cnf (Query t) = Query $ cnf t
cnf oth = oth

{-| Translate a term into a CNF list. -}
clauseToCNF :: Term -> CNF
clauseToCNF = clausesToCNF . (: [])

{-| Translate a list of terms into a CNF list. -}
clausesToCNF :: [Term] -> CNF
clausesToCNF = map flattenDis . splitCons . map cnf
