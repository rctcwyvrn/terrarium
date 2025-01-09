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
and Pattern : sig
  type t
end = struct
  type t
end

and Expr : sig
  type t
end = struct
  type t
end

and Module_expr : sig
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
