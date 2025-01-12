open! Core
open! Soil

(* String things *)

module rec Module_name : sig
  type t = string
  type twr = t With_report.t
end = struct
  type t = string
  type twr = t With_report.t
end

and Value_name : sig
  type t = string
  type twr = t With_report.t
end = struct
  type t = string
  type twr = t With_report.t
end

and Field_name : sig
  type t = string
  type twr = t With_report.t
end = struct
  type t = string
  type twr = t With_report.t
end

and Identifier : sig
  type t = string
  type twr = t With_report.t
end = struct
  type t = string
  type twr = t With_report.t
end

and Constructor : sig
  type t =
    { path : Module_name.twr list
    ; name : string
    }

  type twr = t With_report.t
end = struct
  type t =
    { path : Module_name.twr list
    ; name : string
    }

  type twr = t With_report.t
end

(* fixme-someday: unique identifiable types for these things that just end up as strings *)

(* Types *)
and Ty_expr : sig
  type t = Identifier.twr
  type twr = t With_report.t
end = struct
  type t = Identifier.twr
  type twr = t With_report.t
end

and Val : sig
  type t =
    { name : Value_name.twr
    ; ty_expr : Ty_expr.twr
    }

  type twr = t With_report.t
end = struct
  type t =
    { name : Value_name.twr
    ; ty_expr : Ty_expr.twr
    }

  type twr = t With_report.t
end

(* Impl types *)

(* Exprs *)
(* fixme-someday lilin: do we need to expose module paths here? *)
and Field : sig
  type t = Field_name.twr
  type twr = t With_report.t
end = struct
  type t = Field_name.twr
  type twr = t With_report.t
end

and Pattern : sig
  module Record_pattern_item : sig
    type t =
      { field : Field.twr
      ; equals : Pattern.twr
      }

    type twr = t With_report.t
  end

  type t =
    | Binding of Value_name.twr
    | Constant of Constant.twr
    | Cons of
        { hd : Pattern.twr
        ; rest : Pattern.twr
        }
    | Constructor of Constructor.twr
    | Record of Record_pattern_item.twr list
    | Catchall

  type twr = t With_report.t
end = struct
  module Record_pattern_item = struct
    type t =
      { field : Field.twr
      ; equals : Pattern.twr
      }

    type twr = t With_report.t
  end

  type t =
    | Binding of Value_name.twr
    | Constant of Constant.twr
    | Cons of
        { hd : Pattern.twr
        ; rest : Pattern.twr
        }
    | Constructor of Constructor.twr
    | Record of Record_pattern_item.twr list
    | Catchall

  type twr = t With_report.t
end

and Record_def : sig
  type t =
    { field : Field.twr
    ; equals : Expr.twr
    }

  type twr = t With_report.t
end = struct
  type t =
    { field : Field.twr
    ; equals : Expr.twr
    }

  type twr = t With_report.t
end

and Pattern_matching : sig
  type arm =
    { pattern : Pattern.twr
    ; when_ : Expr.twr option
    ; expr : Expr.twr
    }

  type t = arm list
  type twr = t With_report.t
end = struct
  type arm =
    { pattern : Pattern.twr
    ; when_ : Expr.twr option
    ; expr : Expr.twr
    }

  type t = arm list
  type twr = t With_report.t
end

and Constant : sig
  type t =
    | Int of int
    | String of string
    | True
    | False
    | Unit

  type twr = t With_report.t
end = struct
  type t =
    | Int of int
    | String of string
    | True
    | False
    | Unit

  type twr = t With_report.t
end

and Infix : sig
  type t =
    | Add
    | Sub
    | Mult
    | And
    | Or

  type twr = t With_report.t
end = struct
  type t =
    | Add
    | Sub
    | Mult
    | And
    | Or

  type twr = t With_report.t
end

and Expr : sig
  (* This type is where the most corners are cut

     Hopefully I'm not cutting out anything critical *)
  type t =
    | Path of
        { modules : Module_name.twr list
        ; value : Value_name.twr
        }
    | Constant of Constant.twr
    | Group of Expr.twr
    | Cons of
        { hd : Expr.twr
        ; tail : Expr.twr
        }
    | Record of Record_def.twr list
    | Application of
        { f : Expr.twr
        ; args : Expr.twr list (* fixme-someday: labelled arguments, optional arguments *)
        }
    | Infix of
        { op : Infix.twr
        ; l : Expr.twr
        ; r : Expr.twr
        }
    | If of
        { pred : Expr.twr
        ; then_ : Expr.twr
        ; else_ : Expr.twr option
        }
    | Sequence of Expr.twr * Expr.twr
    | Match of Expr.twr * Pattern_matching.twr
    | Fun of
        { params : Pattern.twr list
            (* fixme-someday: labelled arguments, optional arguments *)
        ; body : Expr.twr
        }
    | Let_expr of
        { is_rec : bool
        ; binding : Let_binding.twr
        ; others : Let_binding.twr list
        }

  type twr = t With_report.t
end = struct
  type t =
    | Path of
        { modules : Module_name.twr list
        ; value : Value_name.twr
        }
    | Constant of Constant.twr
    | Group of Expr.twr
    | Cons of
        { hd : Expr.twr
        ; tail : Expr.twr
        }
    | Record of Record_def.twr list
    | Application of
        { f : Expr.twr (* fixme-someday: labelled arguments *)
        ; args : Expr.twr list
        }
    | Infix of
        { op : Infix.twr
        ; l : Expr.twr
        ; r : Expr.twr
        }
    | If of
        { pred : Expr.twr
        ; then_ : Expr.twr
        ; else_ : Expr.twr option
        }
    | Sequence of Expr.twr * Expr.twr
    | Match of Expr.twr * Pattern_matching.twr
    | Fun of
        { params : Pattern.twr list
        ; body : Expr.twr
        }
    | Let_expr of
        { is_rec : bool
        ; binding : Let_binding.twr
        ; others : Let_binding.twr list
        }

  type twr = t With_report.t
end

and Module_expr : sig
  type t =
    | Struct of Module_impl.twr
    | Functor of
        { arg_name : Module_name.twr
        ; arg_type : Module_interface.module_type_wr
        ; expr : Module_expr.twr
        }
    | App of
        { f : Module_expr.twr
        ; arg : Module_expr.twr
        }

  type twr = t With_report.t
end = struct
  type t =
    | Struct of Module_impl.twr
    | Functor of
        { arg_name : Module_name.twr
        ; arg_type : Module_interface.module_type_wr
        ; expr : Module_expr.twr
        }
    | App of
        { f : Module_expr.twr
        ; arg : Module_expr.twr
        }

  type twr = t With_report.t
end

(* defns *)
and Let_binding : sig
  type t =
    { pattern : Pattern.twr
    ; expr : Expr.twr
    }

  type twr = t With_report.t
end = struct
  type t =
    { pattern : Pattern.twr
    ; expr : Expr.twr
    }

  type twr = t With_report.t
end

and Definition : sig
  type t =
    | Let of
        { is_rec : bool
        ; binding : Let_binding.twr
        ; others : Let_binding.twr list
        }
    | Module of
        { name : Module_name.twr
        ; ty : Module_interface.module_type_wr
        ; expr : Module_expr.twr
        }
    | Module_type of
        { name : Module_name.twr
        ; ty : Module_interface.module_type_wr
        }
    | Open of Module_name.twr list
    | Include of Module_expr.twr

  type twr = t With_report.t
end = struct
  type t =
    | Let of
        { is_rec : bool
        ; binding : Let_binding.twr
        ; others : Let_binding.twr list
        }
    | Module of
        { name : Module_name.twr
        ; ty : Module_interface.module_type_wr
        ; expr : Module_expr.twr
        }
    | Module_type of
        { name : Module_name.twr
        ; ty : Module_interface.module_type_wr
        }
    | Open of Module_name.twr list
    | Include of Module_expr.twr

  type twr = t With_report.t
end

and Module_impl : sig
  type item =
    | Def of Definition.twr
    | Expr of Expr.twr

  type t = item list
  type twr = t With_report.t
end = struct
  type item =
    | Def of Definition.twr
    | Expr of Expr.twr

  type t = item list
  type twr = t With_report.t
end

(* Modules *)
and Module_interface : sig
  type specification =
    | Val of Val.twr
    | Module of
        { name : Module_name.twr
        ; ty : Module_interface.module_type_wr
        }
    | Open of Module_name.twr
    | Include of Module_name.twr

  and module_type =
    | Sig of Module_interface.specification_wr list
    | Functor of
        { arg_name : Module_name.twr
        ; arg_type : Module_interface.module_type_wr
        ; result_type : Module_interface.module_type_wr
        }

  type specification_wr = specification With_report.t
  type module_type_wr = module_type With_report.t
end = struct
  type specification =
    | Val of Val.twr
    | Module of
        { name : Module_name.twr
        ; ty : Module_interface.module_type_wr
        }
    | Open of Module_name.twr
    | Include of Module_name.twr

  and module_type =
    | Sig of Module_interface.specification_wr list
    | Functor of
        { arg_name : Module_name.twr
        ; arg_type : Module_interface.module_type_wr
        ; result_type : Module_interface.module_type_wr
        }

  type specification_wr = specification With_report.t
  type module_type_wr = module_type With_report.t
end

and Compilation_unit : sig
  type t =
    { interface : Module_interface.module_type_wr
    ; impl : Module_impl.twr
    }

  type twr = t With_report.t
end = struct
  type t =
    { interface : Module_interface.module_type_wr
    ; impl : Module_impl.twr
    }

  type twr = t With_report.t
end
