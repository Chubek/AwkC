type json =
  | JsonObject of (string * json) list
  | JsonArray of json list
  | JsonString of string

let rec pp_json fmt = function
  | JsonObject pairs ->
      Format.fprintf fmt "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (key, value) ->
             Format.fprintf fmt "%s: %a" key pp_json value))
        pairs
  | JsonArray elements ->
      Format.fprintf fmt "[ %a ]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_json)
        elements
  | JsonString s -> Format.fprintf fmt "\"%s\"" s

