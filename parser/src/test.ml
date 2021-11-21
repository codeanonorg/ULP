type literal =
  | True
  | False
  | Cst of int
  | Table of literal list


let fmt (s : literal) =
  match s with
  | True
  | False
  | Cst _
  | Table _ -> assert false