module Calculations (
    Calculation (Calc),
    Step,
    calculate,
    paste,
)

where

import Laws
import Expressions
import Utilities
import Rewrites

data Calculation = Calc Expr [Step]
type Step = (LawName, Expr)

calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws e)
    where   rws e = [(name, e')
                |   Law name eqn <- sortedLaws,
                    e' <- rewrites eqn e,
                    e' /= e]
            sortedLaws = sortLaws laws

manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e 
    =   if null steps then []
        else step : manyStep rws (snd step)
        where   steps = rws e
                step = head steps

reverseCalc :: Calculation -> Calculation
reverseCalc (Calc e steps)
    = foldl shunt (Calc e []) steps
        where   shunt (Calc e1 steps) (why, e2)
                    = Calc e2 ((why, e1):steps)

paste :: Calculation -> Calculation -> Calculation
paste calc1@(Calc e1 steps1) calc2
    = if conc1 == conc2
        then Calc e1 (prune conc1 rsteps1 rsteps2)
        else Calc e1 (steps1 ++ (gap, conc2):rsteps2)
        where   Calc conc1 rsteps1 = reverseCalc calc1
                Calc conc2 rsteps2 = reverseCalc calc2
                gap = "... ??? ..."

prune :: Expr -> [Step] -> [Step] -> [Step]
prune e ((_, e1):steps1) ((_, e2):steps2)
    | e1 == e2 = prune e1 steps1 steps2
prune e steps1 steps2 = rsteps ++ steps2
        where Calc _ rsteps = reverseCalc (Calc e steps1)

instance Show Calculation where
    showsPrec p (Calc e ((l, e1):ss)) = 
            showString (if p == 0 then "" else "    ") 
        .   showsPrec 0 e
        .   showString "\n  = {"
        .   showString l
        .   showString "}\n"
        .   showsPrec 1 (Calc e1 ss)

    showsPrec p (Calc e []) = 
            showString (if p == 0 then "" else "    ") 
        .   showsPrec 0 e