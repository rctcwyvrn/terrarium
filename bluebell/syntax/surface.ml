open! Core

type constant =
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | True
  | False
  | Unit
(* idk lists? empty list as a constant and :: as a builtin expr *)

and pattern = Identifier of string
(* many more patterns, need a thing for product and sum types*)

and binding =
  { lhs : pattern
  ; rhs : expr
  }

and pattern_match =
  { pat : pattern list
  ; in_ : expr
  }

and label
(* opaque type, just a string *)

and prefix
and infix

and argument =
  | Expr of expr
  | Just_label of label
  | Label_with_expr of (label * expr)
  | Just_opt_label of label
  | Opt_label_with_expr of (label * expr)

and param =
  | Pattern of pattern
  | Labelled of label
  | Optional of label
(* lots of extra variations here with patterns/type args, gonna just do the basic ones for now *)

and let_binding =
  { pattern : pattern
  ; expr : expr
  }

and record_binding =
  { ident : string
  ; expr : expr
  }

and expr =
  | Constant of constant
  | Grouped of expr
  | Group_with_type of (expr * type_expr)
  | Tuple of expr list
  | Application of (expr * argument list)
  | Prefix of (prefix * expr)
  | Infix of (expr * infix * expr)
  | If_then of
      { if_ : expr
      ; then_ : expr
      }
  | If_then_else of
      { if_ : expr
      ; then_ : expr
      ; else_ : expr
      }
  | Sequence of (expr * expr)
  | Match_with of
      { expr : expr
      ; patterns : pattern_match list
      }
  | Fun of
      { params : param list
      ; with_type : type_expr option
      ; body : expr
      }
  | Let_binding of
      { is_rec : bool
      ; bindings : binding list
      ; in_ : expr
      }
  | Product_init of record_binding list
(* also need constructor inits for sum types? *)
(* poly vars, exceptions, modules, subtyping, lazy, assert, local open *)

and arg_kind =
  | Anon
  | Labelled
  | Optional

(** Types *)

and type_expr =
  | Typevar of string
  | Hole
  (* is this needed? *)
  | Group of type_expr
  | Function of
      { arg_kind : arg_kind
      ; arg : type_expr
      ; result : type_expr
      }
    (* need identifier?? *)
  | Tuple of type_expr list
(* product and sum types cannot be type_exprs and must be type_defs *)

and constructor_decl =
  { name : string (* wait shouldnt record be an option here *)
  ; args : type_expr list
  }

and field_decl =
  { name : string
  ; ty : type_expr (* apparantely poly is an option here? *)
  }

and type_repr =
  | Sum of constructor_decl list
  | Product of field_decl list

and type_def =
  { (* type t = thing = { record : int } *)
    name : string
  ; extra_equation : type_expr option
  ; def : type_repr
  }

and module_type_spec =
  | Include of module_type
  | Val of (string * type_expr)
  | Module_spec of
      { name : string
      ; ty : module_type
      }
  | Module_type_spec of
      { name : string
      ; equals : module_type option
      }
(* open? *)

(* with sig end *)
and module_type = module_type_spec list
(* functors? *)

and module_item =
  | Def of definition
  | Expr of expr

and module_expr = Struct of module_item list
(* functors *)

and definition =
  | Let of
      { bindings : let_binding list
      ; is_rec : bool
      }
  | Typedef of type_def
  | Module_def of
      { name : string
      ; functor_args : (string * module_type) list
      ; ty : module_type
      ; expr : module_expr
      }
  | Module_type of
      { name : string
      ; ty : module_type
      }
(* fixme-soon: open, include *)

and program = module_item list
