import Calculator

laws1 = 
    ["defn xmatch: xmatch s = cmap (unify s) . match",
    "unify of empty: unify emptySub = one",
    "cmap of one: cmap one = id"]

xmatch_emptySub = simplify laws1 "xmatch emptySub"

laws2 = 
    ["defn match: match = cmap matchesA . alignments",
    "defn xmatch: xmatch s = cmap (unify s) . match",
    -- "defn xmatchesA: xmatchesA s = cmap (unify s) . matchesA",
    "defn xmatchesA rev: cmap (unify s) . matchesA = xmatchesA s",
    "cmap after cmap: cmap f . cmap g = cmap (cmap f . g)"]

xmatch_s = simplify laws2 "xmatch s"

extra_defs = 
    ["defn matchesA: matchesA = combine . map matchA",
    "defn xmatchA: xmatchA = cup . (one * matchA)"]

defns = 
    [ "defn match: match = cmap matchesA . alignments",
    "defn xmatch: xmatch = cup . (one * match)",
    "defn xmatchesA: xmatchesA = cup . (one * matchesA)",
    "defn combine: combine = cmap unifyAll . cp"]

cmap_laws = 
    [ "cmap-cup: cmap (cup . (one * g)) . cpp = cup . (id * cmap g)",
        "cmap-cpp: cmap (cpp . (one * f)) . cpp = cpp . (id * cmap f)",
        "cmap after cmap: cmap f . cmap g = cmap (cmap f . g)",
        "cmap of one: cmap one = id",
        "cmap after map:     cmap f . map g = cmap (f . g)",
        "cmap after concat:  cmap f . concat = cmap (cmap f)",
        "cmap after nil:     cmap f . nil = nil",
        "cmap after one:     cmap f . one = f",
        "cmap after nil   : map f . nil = nil",
        "cmap after one   : map f . one = one. f",
        "cmap after cons  : map f . cons = cons . (f * map f)",
        "cmap after concat: map f . concat = concat . map (map f)"]

cup_laws = 
    [ "cup assoc: cup . (id * cup) = cup . (cup * id) .assocl",
    "cup ident: cup . (f * (one .nil)) = f . fst",
    "cup idnet: cup . ((one . nil) * g) = g . snd",
    "assocl: assocl . (f * (g * h)) = ((f * g) * h) . assocl"]

other_laws = 
    [ "cross bifunctor: (f * g) . (h * k) = (f . h) * (g . k)",
    "cross bifunctor: (id * id) = id",
    "defn cp: cp . nil = one . nil",
    "defn cp: cp . cons = map cons . cpp . (id * cp)",
    "defn unifyAll: unifyAll . nil = one . nil",
    "defn unifyAll: unifyAll . cons = cup . (one * unifyAll)",
    "unify after nil: unify . (id * nil) = one . fst"]

extra_laws = 
    ["map functor: map f . map g = map (f . g)",
    "map functor: map id = id"]

all_laws = defns ++ cmap_laws ++ cup_laws ++ other_laws ++ extra_laws ++ extra_defs
some_laws = defns ++ cmap_laws ++ cup_laws ++ other_laws ++ extra_laws

xmatchesA_base = simplify all_laws "xmatchesA . (id * nil)"
xmatchesA_ = simplify some_laws "cmap xmatchesA . cpp . (xmatchA * one) . assocl"
finally_to_show = prove all_laws "xmatchesA . (id * cons) = cup . (xmatchA * matchesA) . assocl"