exception BadInput of string
exception InternalError of string

module Strmap = Map.Make(String)
