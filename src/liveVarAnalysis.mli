open Util
open BlockStructure

type cfg_annot = { gen : Varset.t; kill : Varset.t; live_exit : Varset.t }
    [@@deriving show]

type cfg = (cfg_entry * cfg_annot) list
    [@@deriving show]

val lva : BlockStructure.cfg -> cfg

val remove_unused_writes : cfg -> cfg
