type 'a pointer = NULL | Pointer of 'a ref

let ( !^ ) = function
  | NULL -> invalid_arg "Attempt to dereference the null pointer"
  | Pointer r -> !r;

let ( ^:= ) p v =
  match p with
    | NULL -> invalid_arg "Attempt to assign the null pointer"
    | Pointer r -> r := v;