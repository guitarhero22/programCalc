import Calculator

laws1 = [
    "definition filter: filter p = concat . map (box p)",
    "definition box: box p = if p one nil",

    "if after dot: if p f g . h = if (p . h) (f . h) (g . h)",
    "dot after if: h . if p f g = if p (h . f) (h . g)",

    "nil constant: nil . f = nil",
    "map after nil: map f . nil = nil",
    "map after one: map f . one = one . f",

    "map after concat: map f . concat = concat . map (map f)",

    "map functor: map f . map g = map (f . g)",
    "map functor: map id = id"]

specification1 = "filter p . map f"

derivation1 = simplify laws specification