open! Core
open! Soil

(* String things *)

module rec Module_name : sig
  type t = string
  type t_with_report = t With_report.t
end = struct
  type t = string
  type t_with_report = t With_report.t
end

and Value_name : sig
  type t = string
  type t_with_report = t With_report.t
end = struct
  type t = string
  type t_with_report = t With_report.t
end

and Field_name : sig
  type t = string
  type t_with_report = t With_report.t
end = struct
  type t = string
  type t_with_report = t With_report.t
end

and Identifier : sig
  type t = string
  type t_with_report = t With_report.t
end = struct
  type t = string
  type t_with_report = t With_report.t
end

and Constructor : sig
  type t =
    { path : Module_name.t_with_report list
    ; name : string
    }

  type t_with_report = t With_report.t
end = struct
  type t =
    { path : Module_name.t_with_report list
    ; name : string
    }

  type t_with_report = t With_report.t
end

(* fixme-someday: unique identifiable types for these things that just end up as strings *)

(* Types *)
and Ty_expr : sig
  type t = Identifier.t_with_report
  type t_with_report = t With_report.t
end = struct
  type t = Identifier.t_with_report
  type t_with_report = t With_report.t
end

and Val : sig
  type t =
    { name : Value_name.t_with_report
    ; ty_expr : Ty_expr.t_with_report
    }

  type t_with_report = t With_report.t
end = struct
  type t =
    { name : Value_name.t_with_report
    ; ty_expr : Ty_expr.t_with_report
    }

  type t_with_report = t With_report.t
end

(* Impl types *)

(* Exprs *)
(* fixme-someday lilin: do we need to expose module paths here? *)
and Field : sig
  type t = Field_name.t_with_report
  type t_with_report = t With_report.t
end = struct
  type t = Field_name.t_with_report
  type t_with_report = t With_report.t
end

and Pattern : sig
  module Record_pattern_item : sig
    type t =
      { field : Field.t_with_report
      ; equals : Pattern.t_with_report
      }

    type t_with_report = t With_report.t
  end

  type t =
    | Binding of Value_name.t_with_report
    | Constant of Constant.t_with_report
    | Cons of
        { hd : Pattern.t_with_report
        ; rest : Pattern.t_with_report
        }
    | Constructor of Constructor.t_with_report
    | Record of Record_pattern_item.t_with_report list
    | Catchall

  type t_with_report = t With_report.t
end = struct
  module Record_pattern_item = struct
    type t =
      { field : Field.t_with_report
      ; equals : Pattern.t_with_report
      }

    type t_with_report = t With_report.t
  end

  type t =
    | Binding of Value_name.t_with_report
    | Constant of Constant.t_with_report
    | Cons of
        { hd : Pattern.t_with_report
        ; rest : Pattern.t_with_report
        }
    | Constructor of Constructor.t_with_report
    | Record of Record_pattern_item.t_with_report list
    | Catchall

  type t_with_report = t With_report.t
end

and Record_def : sig
  type t =
    { field : Field.t_with_report
    ; equals : Expr.t_with_report
    }

  type t_with_report = t With_report.t
end = struct
  type t =
    { field : Field.t_with_report
    ; equals : Expr.t_with_report
    }

  type t_with_report = t With_report.t
end

and Pattern_matching : sig
  type arm =
    { pattern : Pattern.t_with_report
    ; when_ : Expr.t_with_report option
    ; expr : Expr.t_with_report
    }

  type t = arm list
  type t_with_report = t With_report.t
end = struct
  type arm =
    { pattern : Pattern.t_with_report
    ; when_ : Expr.t_with_report option
    ; expr : Expr.t_with_report
    }

  type t = arm list
  type t_with_report = t With_report.t
end

and Constant : sig
  type t =
    | Int of int
    | String of string
    | True
    | False
    | Unit

  type t_with_report = t With_report.t
end = struct
  type t =
    | Int of int
    | String of string
    | True
    | False
    | Unit

  type t_with_report = t With_report.t
end

and Infix : sig
  type t =
    | Add
    | Sub
    | Mult
    | And
    | Or

  type t_with_report = t With_report.t
end = struct
  type t =
    | Add
    | Sub
    | Mult
    | And
    | Or

  type t_with_report = t With_report.t
end

and Expr : sig
  (* This type is where the most corners are cut

     Hopefully I'm not cutting out anything critical *)
  type t =
    | Path of
        { modules : Module_name.t_with_report list
        ; value : Value_name.t_with_report
        }
    | Constant of Constant.t_with_report
    | Group of Expr.t_with_report
    | Cons of
        { hd : Expr.t_with_report
        ; tail : Expr.t_with_report
        }
    | Record of Record_def.t_with_report list
    | Application of
        { f : Expr.t_with_report
        ; args : Expr.t_with_report list
        (* fixme-someday: labelled arguments, optional arguments *)
        }
    | Infix of
        { op : Infix.t_with_report
        ; l : Expr.t_with_report
        ; r : Expr.t_with_report
        }
    | If of
        { pred : Expr.t_with_report
        ; then_ : Expr.t_with_report
        ; else_ : Expr.t_with_report option
        }
    | Sequence of Expr.t_with_report * Expr.t_with_report
    | Match of Expr.t_with_report * Pattern_matching.t_with_report
    | Fun of
        { params : Pattern.t_with_report list
            (* fixme-someday: labelled arguments, optional arguments *)
        ; body : Expr.t_with_report
        }
    | Let_expr of
        { is_rec : bool
        ; binding : Let_binding.t_with_report
        ; others : Let_binding.t_with_report list
        }

  type t_with_report = t With_report.t
end = struct
  type t =
    | Path of
        { modules : Module_name.t_with_report list
        ; value : Value_name.t_with_report
        }
    | Constant of Constant.t_with_report
    | Group of Expr.t_with_report
    | Cons of
        { hd : Expr.t_with_report
        ; tail : Expr.t_with_report
        }
    | Record of Record_def.t_with_report list
    | Application of
        { f : Expr.t_with_report (* fixme-someday: labelled arguments *)
        ; args : Expr.t_with_report list
        }
    | Infix of
        { op : Infix.t_with_report
        ; l : Expr.t_with_report
        ; r : Expr.t_with_report
        }
    | If of
        { pred : Expr.t_with_report
        ; then_ : Expr.t_with_report
        ; else_ : Expr.t_with_report option
        }
    | Sequence of Expr.t_with_report * Expr.t_with_report
    | Match of Expr.t_with_report * Pattern_matching.t_with_report
    | Fun of
        { params : Pattern.t_with_report list
        ; body : Expr.t_with_report
        }
    | Let_expr of
        { is_rec : bool
        ; binding : Let_binding.t_with_report
        ; others : Let_binding.t_with_report list
        }

  type t_with_report = t With_report.t
end

and Module_expr : sig
  type t =
    | Struct of Module_impl.t_with_report
    | Functor of
        { arg_name : Module_name.t_with_report
        ; arg_type : Module_interface.module_type_with_report
        ; expr : Module_expr.t_with_report
        }
    | App of
        { f : Module_expr.t_with_report
        ; arg : Module_expr.t_with_report
        }

  type t_with_report = t With_report.t
end = struct
  type t =
    | Struct of Module_impl.t_with_report
    | Functor of
        { arg_name : Module_name.t_with_report
        ; arg_type : Module_interface.module_type_with_report
        ; expr : Module_expr.t_with_report
        }
    | App of
        { f : Module_expr.t_with_report
        ; arg : Module_expr.t_with_report
        }

  type t_with_report = t With_report.t
end

(* defns *)
and Let_binding : sig
  type t =
    { pattern : Pattern.t_with_report
    ; expr : Expr.t_with_report
    }

  type t_with_report = t With_report.t
end = struct
  type t =
    { pattern : Pattern.t_with_report
    ; expr : Expr.t_with_report
    }

  type t_with_report = t With_report.t
end

and Definition : sig
  type t =
    | Let of
        { is_rec : bool
        ; binding : Let_binding.t_with_report
        ; others : Let_binding.t_with_report list
        }
    | Module of
        { name : Module_name.t_with_report
        ; ty : Module_interface.module_type_with_report
        ; expr : Module_expr.t_with_report
        }
    | Module_type of
        { name : Module_name.t_with_report
        ; ty : Module_interface.module_type_with_report
        }
    | Open of Module_name.t_with_report list
    | Include of Module_expr.t_with_report

  type t_with_report = t With_report.t
end = struct
  type t =
    | Let of
        { is_rec : bool
        ; binding : Let_binding.t_with_report
        ; others : Let_binding.t_with_report list
        }
    | Module of
        { name : Module_name.t_with_report
        ; ty : Module_interface.module_type_with_report
        ; expr : Module_expr.t_with_report
        }
    | Module_type of
        { name : Module_name.t_with_report
        ; ty : Module_interface.module_type_with_report
        }
    | Open of Module_name.t_with_report list
    | Include of Module_expr.t_with_report

  type t_with_report = t With_report.t
end

and Module_impl : sig
  type item =
    | Def of Definition.t_with_report
    | Expr of Expr.t_with_report

  type t = item list
  type t_with_report = t With_report.t
end = struct
  type item =
    | Def of Definition.t_with_report
    | Expr of Expr.t_with_report

  type t = item list
  type t_with_report = t With_report.t
end

(* Modules *)
and Module_interface : sig
  type specification =
    | Val of Val.t_with_report
    | Module of
        { name : Module_name.t_with_report
        ; ty : Module_interface.module_type_with_report
        }
    | Open of Module_name.t_with_report
    | Include of Module_name.t_with_report

  and module_type =
    | Sig of Module_interface.specification_with_report list
    | Functor of
        { arg_name : Module_name.t_with_report
        ; arg_type : Module_interface.module_type_with_report
        ; result_type : Module_interface.module_type_with_report
        }

  type specification_with_report = specification With_report.t
  type module_type_with_report = module_type With_report.t
end = struct
  type specification =
    | Val of Val.t_with_report
    | Module of
        { name : Module_name.t_with_report
        ; ty : Module_interface.module_type_with_report
        }
    | Open of Module_name.t_with_report
    | Include of Module_name.t_with_report

  and module_type =
    | Sig of Module_interface.specification_with_report list
    | Functor of
        { arg_name : Module_name.t_with_report
        ; arg_type : Module_interface.module_type_with_report
        ; result_type : Module_interface.module_type_with_report
        }

  type specification_with_report = specification With_report.t
  type module_type_with_report = module_type With_report.t
end

and Compilation_unit : sig
  type t =
    { interface : Module_interface.module_type_with_report
    ; impl : Module_impl.t_with_report
    }

  type t_with_report = t With_report.t
end = struct
  type t =
    { interface : Module_interface.module_type_with_report
    ; impl : Module_impl.t_with_report
    }

  type t_with_report = t With_report.t
end
