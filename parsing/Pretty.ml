(*----------------------------------------------------
----------------------PRINTING------------------------
----------------------------------------------------*)

open Printf
open Hiptypes

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

exception Foo of string

let colours : [`Html|`Ansi|`None] ref = ref `None

let col ~ansi ~html ~title text =
  (match !colours with
  | `None when title -> "==== "
  | `None -> ""
  | `Ansi -> ansi
  | `Html -> html) ^ text ^
  (match !colours with
  | `None when title -> " ===="
  | `None -> ""
  | `Ansi -> "\u{001b}[0m"
  | `Html -> "</span>")

let red text = col ~ansi:"\u{001b}[31m" ~html:"<span class=\"output-error\">" ~title:false text
let green text = col ~ansi:"\u{001b}[32m" ~html:"<span class=\"output-ok\">" ~title:false text
let yellow text = col ~ansi:"\u{001b}[33m" ~html:"<span class=\"output-emph\">" ~title:true text

let verifier_counter: int ref = ref 0;;

(* only for testing! to make tests deterministic *)
let verifier_counter_reset () = verifier_counter := 0
let verifier_counter_reset_to n = verifier_counter := n

let end_of_var = Str.regexp "_?[0-9]+$"
let verifier_getAfreeVar _from :string  =
  (* this prefix shows provenance, but that turned out to be useless *)
  (* let prefix = from |> Option.map (fun v -> v ^ "_") |> Option.value ~default:"_f" in *)
  let prefix =
    (* match from with *)
    (* | None -> "_f" *)
    (* | Some f -> *)
      (* Str.global_replace end_of_var "" from *)
      "v"
  in
  let x = prefix ^ string_of_int (!verifier_counter) in
  incr verifier_counter;
  x

(* let%expect_test _ =
  let p = print_endline in
  verifier_counter_reset ();
  p (verifier_getAfreeVar "v");
  p (verifier_getAfreeVar "v");
  p (verifier_getAfreeVar "a");
  let v = verifier_getAfreeVar "a" in
  p v;
  p (verifier_getAfreeVar v);
  [%expect
  {|
    v0
    v1
    v2
    v3
    v4
  |}] *)


let string_of_args pp args =
  match args with
  | [] -> "()"
  | _ ->
    let a = String.concat ", " (List.map pp args) in
    Format.asprintf "(%s)" a

let rec iter f = function
  | [] -> ()
  | [x] ->
      f true x
  | x :: tl ->
      f false x;
      iter f tl

let to_buffer ?(line_prefix = "") ~get_name ~get_children buf x =
  let rec print_root indent x =
    bprintf buf "%s\n" (get_name x);
    let children = get_children x in
    iter (print_child indent) children
  and print_child indent is_last x =
    let line =
      if is_last then
        "└── "
      else
        "├── "
    in
    bprintf buf "%s%s" indent line;
    let extra_indent =
      if is_last then
        "    "
      else
        "│   "
    in
    print_root (indent ^ extra_indent) x
  in
  Buffer.add_string buf line_prefix;
  print_root line_prefix x

let printTree ?line_prefix ~get_name ~get_children x =
  let buf = Buffer.create 1000 in
  to_buffer ?line_prefix ~get_name ~get_children buf x;
  Buffer.contents buf

type binary_tree =
  | Node of string * (binary_tree  list )
  | Leaf

let get_name = function
    | Leaf -> "."
    | Node (name, _) -> name;;

let get_children = function
    | Leaf -> []
    | Node (_, li) -> List.filter ((<>) Leaf) li;;

let rule ?(children=[]) ?(success=true) ~name fmt = Format.kasprintf (fun s -> Node (Format.asprintf "[%s]%s %s" name (if success then "" else red " FAIL") s, children)) fmt

type proof = binary_tree

let string_of_proof tree = printTree
~line_prefix:"│"
(* ~line_prefix:" " *)
~get_name ~get_children tree

let rec input_lines file =
  match try [input_line file] with End_of_file -> [] with
   [] -> []
  | [line] -> (String.trim line) :: input_lines file
  | _ -> failwith "Weird input_line return value"

  ;;

let rec separate li f sep : string =
  match li with
  | [] -> ""
  | [x] -> f x
  | x ::xs -> f x ^ sep ^ separate xs f sep
  ;;




let string_of_bin_op op : string =
  match op with
  | GT -> ">"
  | LT -> "<"
  | EQ -> "="
  | GTEQ -> ">="
  | LTEQ -> "<="

let rec string_of_term t : string =
  match t with
  | Num i -> string_of_int i
  | UNIT -> "()"
  | Nil -> "[]"
  | TTrue -> "true"
  | TFalse -> "false"
  | TNot a -> Format.asprintf "not(%s)" (string_of_term a)
  | TAnd (a, b) -> Format.asprintf "%s && %s" (string_of_term a) (string_of_term b)
  | TOr (a, b) -> Format.asprintf "%s || %s" (string_of_term a) (string_of_term b)
  | Var str -> str
  | Rel (bop, t1, t2) ->
    string_of_term t1 ^ (match bop with | EQ -> "==" | _ -> string_of_bin_op bop) ^ string_of_term t2
  | Plus (t1, t2) -> string_of_term t1 ^ "+" ^ string_of_term t2
  | Minus (t1, t2) -> string_of_term t1 ^ "-" ^ string_of_term t2
  | TPower (t1, t2) -> "(" ^string_of_term t1 ^ "^(" ^ string_of_term t2 ^ "))"
  | TApp (op, args) -> Format.asprintf "%s%s" op (string_of_args string_of_term args)
  | TLambda (_name, params, sp) -> Format.asprintf "lambda(\\%s -> %s)" (String.concat " " params) (string_of_disj_spec sp)
  | TTupple nLi ->
    let rec helper li =
      match li with
      | [] -> ""
      | [x] -> string_of_term x
      | x:: xs -> string_of_term x ^","^ helper xs
    in "(" ^ helper nLi ^ ")"

  | TList nLi ->
    let rec helper li =
      match li with
      | [] -> ""
      | [x] -> string_of_term x
      | x:: xs -> string_of_term x ^";"^ helper xs
    in "[" ^ helper nLi ^ "]"

and string_of_staged_spec (st:stagedSpec) : string =
  match st with
  | Require (p, h) ->
    Format.asprintf "req %s" (string_of_state (p, h))
  | HigherOrder (pi, h, (f, args), ret) ->
    begin match pi, h with
    | True, EmptyHeap ->
      Format.asprintf "%s(%s)" f (String.concat ", " (List.map string_of_term args @ ([string_of_term ret]))) 
    | _ ->
      Format.asprintf "ens %s; %s(%s)" (string_of_state (pi, h)) f (String.concat ", " (List.map string_of_term args @ ([string_of_term ret]))) 
    end
  | NormalReturn (pi, heap) ->
    Format.asprintf "Norm(%s)" (string_of_state (pi, heap))
  | RaisingEff (pi, heap, (name, args), ret) ->
    Format.asprintf "%s(%s, %s, %s)" name (string_of_state (pi, heap)) (string_of_args string_of_term args) (string_of_term ret)
  | Exists vs ->
    Format.asprintf "ex %s" (String.concat " " vs)
  (* | IndPred {name; args} -> *)
    (* Format.asprintf "%s(%s)" name (String.concat " " (List.map string_of_term args)) *)

and string_of_spec (spec:spec) :string =
  match spec with
  | [] -> "<empty spec>"
  | _ ->
    spec
    (* |> List.filter (function Exists [] -> false | _ -> true) *)
    |> List.map string_of_staged_spec |> String.concat "; "

and string_of_spec_list (specs:spec list) : string =
  match specs with
  | [] -> "<empty disj>"
  | _ :: _ -> List.map string_of_spec specs |> String.concat " \\/ "

and string_of_disj_spec (s:disj_spec) : string = string_of_spec_list s

and string_of_instant (str, ar_Li): string =
  (* syntax is like OCaml type constructors, e.g. Foo, Foo (), Foo (1, ()) *)
  let args =
    match ar_Li with
    | [] -> ""
    | [t] -> Format.sprintf "(%s)" (string_of_term t)
    | _ -> Format.sprintf "(%s)" (separate (ar_Li) (string_of_term) (","));
  in
  Format.sprintf "%s%s" str args


and string_of_kappa (k:kappa) : string =
  match k with
  | EmptyHeap -> "emp"
  | PointsTo  (str, args) -> Format.sprintf "%s->%s" str (List.map string_of_term [args] |> String.concat ", ")
  | SepConj (k1, k2) -> string_of_kappa k1 ^ "*" ^ string_of_kappa k2
  (*| MagicWand (k1, k2) -> "(" ^ string_of_kappa k1 ^ "-*" ^ string_of_kappa k2  ^ ")" *)
  (* | Implication (k1, k2) -> string_of_kappa k1 ^ "-*" ^ string_of_kappa k2  *)

and string_of_state (p, h) : string =
  match h, p with
  | _, True -> string_of_kappa h
  | EmptyHeap, _ -> string_of_pi p
  | _ ->
    Format.asprintf "%s/\\%s" (string_of_kappa h) (string_of_pi p)
    (* Format.asprintf "%s*%s" (string_of_kappa h) (string_of_pi p) *)

and string_of_pi pi : string =
  match pi with
  | True -> "T"
  | False -> "F"
  | Atomic (op, t1, t2) -> string_of_term t1 ^ string_of_bin_op op ^ string_of_term t2
  | And   (p1, p2) -> string_of_pi p1 ^ "/\\" ^ string_of_pi p2
  | Or     (p1, p2) -> string_of_pi p1 ^ "\\/" ^ string_of_pi p2
  | Imply  (p1, p2) -> string_of_pi p1 ^ "=>" ^ string_of_pi p2
  | Not    p -> "not(" ^ string_of_pi p ^ ")"
  | Predicate (str, t) -> str ^ "(" ^ string_of_term t ^ ")"



let rec stricTcompareTerm (term1:term) (term2:term) : bool =
  match (term1, term2) with
    (Var s1, Var s2) -> String.compare s1 s2 == 0
  | (Num n1, Num n2) -> n1 == n2
  | (Plus (tIn1, num1), Plus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | (Minus (tIn1, num1), Minus (tIn2, num2)) -> stricTcompareTerm tIn1 tIn2 && stricTcompareTerm num1  num2
  | (UNIT, UNIT) -> true
  | _ -> false
  ;;


let rec comparePure (pi1:pi) (pi2:pi):bool =
  match (pi1 , pi2) with
    (True, True) -> true
  | (False, False) -> true
  | (Atomic(GT, t1, t11),  Atomic(GT, t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Atomic (LT, t1, t11),  Atomic(LT, t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Atomic (GTEQ, t1, t11),  Atomic(GTEQ, t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Atomic (LTEQ, t1, t11),  Atomic(LTEQ, t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Atomic (EQ, t1, t11),  Atomic(EQ, t2, t22)) -> stricTcompareTerm t1 t2 && stricTcompareTerm t11  t22
  | (Or (p1, p2), Or (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (And (p1, p2), And (p3, p4)) ->
      (comparePure p1 p3 && comparePure p2 p4) || (comparePure p1 p4 && comparePure p2 p3)
  | (Not p1, Not p2) -> comparePure p1 p2
  | _ -> false
  ;;

let rec getAllPi piIn acc=
    (match piIn with
      And (pi1, pi2) -> (getAllPi pi1 acc ) @ (getAllPi pi2 acc )
    | _ -> acc  @[piIn]
    )
    ;;

let rec existPi pi li =
    (match li with
      [] -> false
    | x :: xs -> if comparePure pi x then true else existPi pi xs
    )
    ;;


(**********************************************)
exception FooAskz3 of string



let rec getAllVarFromTerm (t:term) (acc:string list):string list =
  match t with
| Var ( name) -> List.append acc [name]
| Plus (t1, t2) ->
    let cur = getAllVarFromTerm t1 acc in
    getAllVarFromTerm t2 cur
| Minus (t1, t2) ->
    let cur = getAllVarFromTerm t1 acc in
    getAllVarFromTerm t2 cur
| _ -> acc
;;





let addAssert (str:string) :string =
  "(assert " ^ str ^ " ) \n (check-sat) \n"
  ;;










let rec kappaToPure kappa : pi =
  match kappa with
  | EmptyHeap -> True
  | PointsTo (str, t) -> Atomic(EQ, Var str, t)
  | SepConj (k1, k2) -> And (kappaToPure k1, kappaToPure k2)

  (* | Implication (k1, k2) -> Imply (kappaToPure k1, kappaToPure k2) *)







let string_of_pred ({ p_name; p_params; p_body } : pred_def) : string =
  Format.asprintf "%s(%s) == %s" p_name (String.concat "," p_params) (string_of_spec_list p_body)

let string_of_inclusion (lhs:spec list) (rhs:spec list) :string =
  string_of_spec_list lhs ^" |- " ^string_of_spec_list rhs

let rec string_of_normalisedStagedSpec (spec:normalisedStagedSpec) : string =
  let (effS, normalS) = spec in
  match effS with
  | [] ->
    let (existiental, (p1, h1), (p2, h2)) = normalS in
    let ex = match existiental with [] -> [] | _ -> [Exists existiental] in
    let current = ex @ [Require(p1, h1); NormalReturn(p2, h2)] in
    string_of_spec current
  | x :: xs  ->
    (let {e_pre = (p1, h1); e_post = (p2, h2); _} = x in
    let ex = match x.e_evars with [] -> [] | _ -> [Exists x.e_evars] in
    let current = ex @ [Require(p1, h1);
    (match x.e_typ with
    | `Eff -> RaisingEff(p2, h2, x.e_constr, x.e_ret)
    | `Fn -> HigherOrder(p2, h2, x.e_constr, x.e_ret))] in
    string_of_spec current )
    ^ "; " ^ string_of_normalisedStagedSpec (xs, normalS)

let string_of_constr_call n args =
  match n, args with
  | "[]", _ -> "[]"
  | "::", [a1; a2] -> Format.asprintf "%s :: %s" a1 a2
  | _ -> Format.asprintf "%s(%s)" n (String.concat ", " args)

let rec string_of_core_lang (e:core_lang) :string =
  match e with
  | CValue v -> string_of_term v
  | CLet (v, e, e1) -> Format.sprintf "let %s = %s in\n%s" v (string_of_core_lang e) (string_of_core_lang e1)
  | CIfELse (pi, t, e) -> Format.sprintf "if %s then %s else %s" (string_of_pi pi)  (string_of_core_lang t) (string_of_core_lang e)
  | CFunCall (f, [a; b]) when not (is_alpha (String.get f 0)) -> Format.sprintf "%s %s %s" (string_of_term a) f (string_of_term b)
  | CFunCall (f, xs) -> Format.sprintf "%s %s" f (List.map string_of_term xs |> String.concat " ")
  | CWrite (v, e) -> Format.sprintf "%s := %s" v (string_of_term e)
  | CRef v -> Format.sprintf "ref %s" (string_of_term v)
  | CRead v -> Format.sprintf "!%s" v
  | CAssert (p, h) -> Format.sprintf "assert (%s && %s)" (string_of_pi p) (string_of_kappa h)
  | CPerform (eff, Some arg) -> Format.sprintf "perform %s %s" eff (string_of_term arg)
  | CPerform (eff, None) -> Format.sprintf "perform %s" eff
  | CMatch (None, e, vs, hs, cs) -> Format.sprintf "match %s with\n%s%s\n%s" (string_of_core_lang e) (match vs with | Some (v, norm) -> Format.asprintf "| %s -> %s\n" v (string_of_core_lang norm) | _ -> "") (string_of_core_handler_ops hs) (string_of_constr_cases cs)
  | CMatch (Some spec, e, vs, hs, cs) -> Format.sprintf "match %s%s with\n%s%s\n%s" (string_of_disj_spec spec) (string_of_core_lang e) (match vs with | Some (v, norm) -> Format.asprintf "| %s -> %s\n" v (string_of_core_lang norm) | _ -> "") (string_of_core_handler_ops hs) (string_of_constr_cases cs)
  | CResume v -> Format.sprintf "continue k %s" (string_of_term v)
  | CLambda (xs, spec, e) -> Format.sprintf "fun %s%s -> %s" (String.concat " " xs) (match spec with None -> "" | Some ds -> Format.asprintf " (*@@ %s @@*)" (string_of_disj_spec ds)) (string_of_core_lang e)

and string_of_constr_cases cs =
  cs |> List.map (fun (n, args, body) -> Format.asprintf "| %s -> %s" (string_of_constr_call n args) (string_of_core_lang body)) |> String.concat "\n"

and string_of_core_handler_ops hs =
  List.map (fun (name, v, spec, body) ->
    let spec = spec |> Option.map (fun s -> Format.asprintf " (*@@ %s @@*)" (string_of_disj_spec s)) |> Option.value ~default:"" in
    Format.asprintf "| effect %s k%s -> %s"
      (match v with None -> name | Some v -> Format.asprintf "(%s %s)" name v) spec (string_of_core_lang body)) hs |> String.concat "\n"

let string_of_effect_stage1 (vs, pre, post, eff, ret) =
  Format.asprintf "ex %s. req %s; ens %s /\\ %s /\\ res=%s" (String.concat " " vs) (string_of_state pre) (string_of_state post) (string_of_instant eff) (string_of_term ret)

let string_of_effect_stage {e_evars = vs; e_pre = pre; e_post = post; e_constr = eff; e_ret = ret; _} =
  Format.asprintf "ex %s. req %s; ens %s /\\ %s /\\ res=%s" (String.concat " " vs) (string_of_state pre) (string_of_state post) (string_of_instant eff) (string_of_term ret)

let string_of_normal_stage (vs, pre, post, ret) =
  Format.asprintf "ex %s. req %s; ens %s /\\ res=%s" (String.concat " " vs) (string_of_state pre) (string_of_state post) (string_of_term ret)

let string_of_existentials vs =
  match vs with
  | [] -> ""
  | _ :: _ -> Format.asprintf "ex %s. " (String.concat "," vs)

let string_of_res b = if b then green "true" else red "false"

let string_of_option to_s o : string =
  match o with Some a -> "Some " ^ to_s a | None -> "None"

let string_of_lemma l =
  Format.asprintf "%s: forall %s, %s <: %s" l.l_name (string_of_list Fun.id l.l_params) (string_of_instant l.l_left) (string_of_spec l.l_right)

(* let string_of_time = string_of_float *)
let string_of_time = Format.asprintf "%.0f"

let string_of_sset s =
  Format.asprintf "{%s}" (String.concat ", " (SSet.elements s))

let string_of_smap pp s =
  Format.asprintf "{%s}" (String.concat ", " (List.map (fun (k, v) -> Format.asprintf "%s -> %s" k (pp v)) (SMap.bindings s)))

let conj xs =
  match xs with
  | [] -> True
  | x :: xs -> List.fold_right (fun c t -> And (c, t)) xs x

let string_of_type t =
  match t with
  | Int -> "int"
  | Unit -> "unit"
  | List_int -> "intlist"
  | Bool -> "bool"
  | Lamb -> "lambda"
  | TVar v -> Format.asprintf "'%s" v

let string_of_tmap pp s =
  Format.asprintf "{%s}" (String.concat ", " (List.map (fun (k, v) -> Format.asprintf "%s -> %s" (string_of_type k) (pp v)) (TMap.bindings s)))

let string_of_abs_env t =
  Format.asprintf "%s, %s" (string_of_smap string_of_type t.vartypes) 
  "<opaque>"
(* (string_of_tmap string_of_type (TMap.map (fun t -> U.get t) !(t.equalities))) *)

let string_of_typ_env t =
  Format.asprintf "%s" (string_of_smap string_of_type t)


let string_of_quantified to_s (vs, e) =
  match vs with
  | [] -> to_s e
  | _ :: _ -> Format.asprintf "ex %s. %s" (String.concat " " vs) (to_s e)

let string_of_instantiations pv kvs =
  List.map (fun (k, v) -> Format.asprintf "%s := %s" k (pv v)) kvs
  |> String.concat ", " |> Format.asprintf "[%s]"

let string_of_bindings = string_of_instantiations

let string_of_meth_def m =
  Format.asprintf "let rec %s %s\n%s=\n%s" m.m_name
    (match m.m_params with | [] -> "()" | _ -> String.concat " " m.m_params)
    ((match m.m_spec with None -> "" | Some s -> s |> List.map string_of_spec |> List.map (Format.asprintf "(*@@ %s @@*)") |> String.concat "\n\\/\n" |> fun s -> s ^ "\n"))
    (string_of_core_lang m.m_body)

let string_of_program (cp:core_program) :string =
  List.map string_of_meth_def cp.cp_methods |> String.concat "\n\n"

(* implements the [pi = pi_other and pi_res] split from the ho paper *)
let rec split_res p =
  match p with
  | True -> True, []
  | False -> False, []
  | Atomic (_, Var "res", _)
  | Atomic (_, _, Var "res") -> True, [p]
  | Atomic (_, _, _) -> p, []
  | And (a, b) ->
    let l, r = split_res a in
    let l1, r1 = split_res b in
    And (l, l1), r @ r1
  | Or (a, b) ->
    let l, r = split_res a in
    let l1, r1 = split_res b in
    Or (l, l1), r @ r1
  | Imply (a, b) ->
    let l, r = split_res a in
    let l1, r1 = split_res b in
    Imply (l, l1), r @ r1
  | Not a ->
    let l, r = split_res a in
    Not l, r
  | Predicate (_, _) -> failwith (Format.asprintf "NYI: predicate")

let split_res_fml p =
  let rest, constrs = split_res p in
  let fml = List.fold_right (fun c t -> And (c, t)) constrs True in
  rest, fml

let get_res_value p =
  let _rest, eqs = split_res p in
  match eqs with
  | [Atomic (EQ, Var "res", t)]
  | [Atomic (EQ, t, Var "res")] -> t
  | [Atomic (_, _, _)] ->
    failwith (Format.asprintf "not an equality on res: %s" (string_of_pi p))
  | [] ->
    failwith (Format.asprintf "no mention of res: %s" (string_of_pi p))
  | _ ->
    failwith (Format.asprintf "many possible res values: %s" (string_of_pi p))

let is_just_res_of pi t =
  try
    (get_res_value pi) = t
  with _ -> false

let quantify_res p =
  let r, rez = split_res_fml p in
  let nv = verifier_getAfreeVar "split" in
  And (r, Subst.instantiatePure ["res", Var nv] rez), nv

(** existentially quantify, i.e. replace with fresh variable *)
let quantify_res_state (p, h) =
  let p1, nv = quantify_res p in
  (p1, h), nv

  let contains_res_state (p, h) =
    SSet.mem "res" (Subst.used_vars_state (p, h))

let lambda_to_pred_def name t =
  match t with
  | TLambda (_lid, params, body) ->
    {
      p_name = name;
      p_params = params;
      p_body = body;
    }
  | _ ->
    failwith
      (Format.asprintf "cannot convert term to predicate: %s" (string_of_term t))

let rec local_lambda_defs pi =
  match pi with
  | Atomic (EQ, (TLambda _ as l), Var v) | Atomic (EQ, Var v, (TLambda _ as l))
    ->
    [(v, lambda_to_pred_def v l)]
  | Atomic (_, _, _) | True | False -> []
  | And (a, b) | Or (a, b) | Imply (a, b) ->
    local_lambda_defs a @ local_lambda_defs b
  | Not a -> local_lambda_defs a
  | Predicate (_, _) -> failwith "NYI: predicate"

let local_lambda_defs_state (p, _h) =
  local_lambda_defs p |> List.to_seq |> SMap.of_seq