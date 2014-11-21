
type ty =
    Tunit
  | Tbool
  | Tint
  | Tfloat
  | Ttuple of ty list
  | Tarray of ty
  | Tfun of ty * ty list

