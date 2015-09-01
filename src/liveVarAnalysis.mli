open Util
open BlockStructure

type cfg_annot = { gen : Strset.t; kill : Strset.t; live_exit : Strset.t }
    [@@deriving show]

type cfg = (SourceAst.id cfg_entry * cfg_annot) list
    [@@deriving show]

val lva : SourceAst.id BlockStructure.cfg -> cfg

