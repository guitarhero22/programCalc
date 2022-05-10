module Matchings (
    match
)
where
import Expressions
import Utilities
import Substitutions (Subst, unitSub, combine)

alignments :: (Expr, Expr) -> [[(Atom, Expr)]]
alignments (Compose as, Compose bs)
        =   [zip as (map Compose bss) | bss <- parts n bs]
            where n = length as

match = xmatch []
xmatch sub (e1,e2) = concat [xmatchesA sub aes | aes <- alignments (e1,e2)]

xmatchesA sub [] = [sub]
xmatchesA sub (ae:aes) = concat [xmatchesA sub' aes | sub' <- xmatchA sub ae]

xmatchA sub (Var v,e) = extend sub v e
xmatchA sub (Con k1 es1,Compose [Con k2 es2]) | k1==k2 = xmatches sub (zip es1 es2)
xmatchA sub _ = []

xmatches sub [] = [sub]
xmatches sub ((e1,e2):es) = concat [xmatches sub' es | sub' <- xmatch sub (e1,e2)]

extend sub v e = case lookup v sub of 
                    Nothing -> [(v,e):sub] 
                    Just e' -> if e==e' then [sub] else []