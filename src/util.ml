exception BadInput of string
exception InternalError of string

module Strmap = Map.Make(String)
module Strset' = Set.Make(String)

module Strset = struct
  include Strset'
  let show s = "Set.Make(String).of_list " ^ [%show: string list] (elements s)
  let pp f s = Format.fprintf f "%s" (show s)
end
