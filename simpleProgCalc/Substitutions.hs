module Substitutions (
    Subst,
    unitSub,
    combine,
    apply
)

where
import Expressions
import Data.Maybe
import Utilities

type Subst = [(VarName, Expr)]

emptySub = []
unitSub v e = [(v, e)]

apply :: Subst -> Expr -> Expr
apply sub (Compose as)
    = Compose (concatMap (applyA sub) as)

applyA sub (Var v) = deCompose (binding sub v)
applyA sub (Con k es) = [Con k (map (apply sub) es)]

binding :: Subst -> VarName -> Expr
binding sub v = fromJust (lookup v sub)

combine = concatMap unifyAll . cp

unify :: Subst -> Subst -> [Subst]
unify sub1 sub2 = [sub1 `union` sub2 | compatible sub1 sub2]

compatible [] sub2 = True
compatible sub1 [] = True
compatible sub1@((v1,e1):sub1') sub2@((v2,e2):sub2')
        | v1<v2 = compatible sub1' sub2
        | v1==v2 = e1==e2 && compatible sub1' sub2'
        | v1>v2 = compatible sub1 sub2'

union [] sub2 = sub2
union sub1 [] = sub1
union sub1@((v1,e1):sub1') sub2@((v2,e2):sub2')
    | v1<v2     = (v1, e1):union sub1' sub2
    | v1==v2    = (v1, e1):union sub1' sub2'
    | v1>v2     = (v2, e2):union sub1 sub2'

unifyAll :: [Subst] -> [Subst]
unifyAll = foldr f [emptySub]
    where f sub subs = concatMap (unify sub) subs