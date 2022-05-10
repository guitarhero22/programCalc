import Calculator

to_prove = "filter (all nodups . boxs) . expand . pruneBy boxs = filter (all nodups . boxs) . expand"

laws = [
    "defn pruneBy: pruneBy f = f . map pruneRow . f",
    "expand after boxs: expand . boxs = map boxs . expand",
    "filter with boxs: filter (p . boxs) = map boxs . filter p . map boxs",
    "boxs involution: boxs . boxs = id",
    "map functor: map f . map g = map (f.g)",
    "map functor: map id = id",
    "defn expand: expand = cp . map cp",
    "filter after cp: filter (all p) . cp = cp . map (filter p)",
    "law of pruneRow: filter nodups . cp . pruneRow = filter nodups . cp"]

hacked_laws = "hack: map boxs . cp . map cp = cp . map cp . boxs" : laws

proof = prove laws to_prove
hacked_proof = prove hacked_laws to_prove