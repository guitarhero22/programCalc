module Calculator (
    simplify,
    prove
)
where

import Calculations
import Laws
import Expressions
import Parsing

simplify :: [String] -> String -> Calculation
simplify strings string
    = let   laws = map (parse law) strings
            e = parse expr string
    in calculate laws e

prove :: [String] -> String -> Calculation
prove strings string
    = let   laws = map (parse law) strings
            (e1, e2) = parse equation string
    in paste (calculate laws e1) (calculate laws e2)