open Util
open BlockStructure

type cfg_annot = { gen : Intset.t; kill : Intset.t; live_exit : Intset.t }
    [@@deriving show]

type cfg = (int cfg_entry * cfg_annot) list
    [@@deriving show]

val lva : int BlockStructure.cfg -> cfg

val remove_unused_writes : cfg -> cfg
