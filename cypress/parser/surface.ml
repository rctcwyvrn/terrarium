open! Core
open! Soil

(* String things *)

module rec Module_name : sig
  type t = string
end = struct
  type t = string
end

and Value_name : sig
  type t = string
end = struct
  type t = string
end

and Field_name : sig
  type t = string
end = struct
  type t = string
end

and Identifier : sig
  type t = string
end = struct
  type t = string
end

(* fixme-someday: unique identifiable types for these things that just end up as strings *)

(* Types *)
and Ty_expr : sig
  type t = Identifier.t
end = struct
  type t = Identifier.t
end

and Val : sig
  type t =
    { name : Value_name.t
    ; ty_expr : Ty_expr.t
    }
end = struct
  type t =
    { name : Value_name.t
    ; ty_expr : Ty_expr.t
    }
end

(* Impl types *)

(* Exprs *)
(* fixme-someday lilin: do we need to expose module paths here? *)
and Field : sig
  type t = Field_name.t
end = struct
  type t = Field_name.t
end

and Pattern : sig
  (* fixme *)
  type t
end = struct
  type t
end

and Record_def : sig
  type t =
    { field : Field.t
    ; equals : Expr.t
    }
end = struct
  type t =
    { field : Field.t
    ; equals : Expr.t
    }
end

and Pattern_matching : sig
  type arm =
    { pattern : Pattern.t
    ; when_ : Expr.t option
    ; expr : Expr.t
    }

  type t = arm list
end = struct
  type arm =
    { pattern : Pattern.t
    ; when_ : Expr.t option
    ; expr : Expr.t
    }

  type t = arm list
end

and Constant : sig
  type t =
    | Int of int
    | String of string
    | True
    | False
    | Unit
end = struct
  type t =
    | Int of int
    | String of string
    | True
    | False
    | Unit
end

and Infix : sig
  type t =
    | Add
    | Sub
    | Mult
    | And
    | Or
end = struct
  type t =
    | Add
    | Sub
    | Mult
    | And
    | Or
end

and Expr : sig
  (* This type is where the most corners are cut

     Hopefully I'm not cutting out anything critical *)
  type t =
    | Path of
        { modules : Module_name.t list
        ; value : Value_name.t
        }
    | Constant of Constant.t
    | Group of t
    | Cons of
        { hd : t
        ; tail : t
        }
    | Record of Record_def.t list
    | Application of
        { f : t
        ; args : t list (* fixme-someday: labelled arguments, optional arguments *)
        }
    | Infix of
        { op : Infix.t
        ; l : t
        ; r : t
        }
    | If of
        { pred : t
        ; then_ : t
        ; else_ : t option
        }
    | Sequence of t * t
    | Match of t * Pattern_matching.t
    | Fun of
        { params : Pattern.t list
            (* fixme-someday: labelled arguments, optional arguments *)
        ; body : t
        }
    | Let_expr of
        { is_rec : bool
        ; binding : Let_binding.t
        ; others : Let_binding.t list
        }
end = struct
  type t =
    | Path of
        { modules : Module_name.t list
        ; value : Value_name.t
        }
    | Constant of Constant.t
    | Group of t
    | Cons of
        { hd : t
        ; tail : t
        }
    | Record of Record_def.t list
    | Application of
        { f : t (* fixme-someday: labelled arguments *)
        ; args : t list
        }
    | Infix of
        { op : Infix.t
        ; l : t
        ; r : t
        }
    | If of
        { pred : t
        ; then_ : t
        ; else_ : t option
        }
    | Sequence of t * t
    | Match of t * Pattern_matching.t
    | Fun of
        { params : Pattern.t list
        ; body : t
        }
    | Let_expr of
        { is_rec : bool
        ; binding : Let_binding.t
        ; others : Let_binding.t list
        }
end

and Module_expr : sig
  (* fixme *)
  type t
end = struct
  type t
end

(* defns *)
and Let_binding : sig
  type t =
    { pattern : Pattern.t
    ; expr : Expr.t
    }
end = struct
  type t =
    { pattern : Pattern.t
    ; expr : Expr.t
    }
end

and Definition : sig
  type t =
    | Let of
        { is_rec : bool
        ; binding : Let_binding.t
        ; others : Let_binding.t list
        }
    | Module of
        { name : Module_name.t
        ; ty : Module_interface.module_type
        ; expr : Module_expr.t
        }
    | Module_type of
        { name : Module_name.t
        ; ty : Module_interface.module_type
        }
end = struct
  type t =
    | Let of
        { is_rec : bool
        ; binding : Let_binding.t
        ; others : Let_binding.t list
        }
    | Module of
        { name : Module_name.t
        ; ty : Module_interface.module_type
        ; expr : Module_expr.t
        }
    | Module_type of
        { name : Module_name.t
        ; ty : Module_interface.module_type
        }
end

and Module_impl : sig
  type pair =
    { def : Definition.t
    ; expr : Expr.t
    }

  type t = pair list
end = struct
  type pair =
    { def : Definition.t
    ; expr : Expr.t
    }

  type t = pair list
end

(* Modules *)
and Module_interface : sig
  type specification =
    | Val of Val.t
    | Module of
        { name : Module_name.t
        ; ty : module_type
        }
    | Open of Module_name.t
    | Include of Module_name.t

  and module_type =
    | Sig of specification list
    | Functor of
        { arg_name : Module_name.t
        ; arg_type : module_type
        ; result_type : module_type
        }
end = struct
  type specification =
    | Val of Val.t
    | Module of
        { name : Module_name.t
        ; ty : module_type
        }
    | Open of Module_name.t
    | Include of Module_name.t

  and module_type =
    | Sig of specification list
    | Functor of
        { arg_name : Module_name.t
        ; arg_type : module_type
        ; result_type : module_type
        }
end

and Compilation_unit : sig
  type t =
    { interface : Module_interface.module_type
    ; impl : Module_impl.t
    }
end = struct
  type t =
    { interface : Module_interface.module_type
    ; impl : Module_impl.t
    }
end
