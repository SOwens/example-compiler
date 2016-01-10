exception BadInput of string
exception InternalError of string

module Strmap = Map.Make(String)
module Strset' = Set.Make(String)
module Intset' = Set.Make(struct type t = int let compare = compare end)

module Strset = struct
  include Strset'
  let show s = "Set.Make(String).of_list " ^ [%show: string list] (elements s)
  let pp f s = Format.fprintf f "%s" (show s)
end

module Intset = struct
  include Intset'
  let show s = "Set.Make(struct type t = int let compare = compare end).of_list " ^ [%show: int list] (elements s)
  let pp f s = Format.fprintf f "%s" (show s)
end
