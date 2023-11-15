(* McCarthy's locally angelic choice operator (angelic modulo nontermination). *)
(* The following examples are adapted from Oleg Kiselyov
   "Non-deterministic choice amb"
   (c.f. https://okmij.org/ftp/ML/ML.html#amb) *)


effect Choose : (unit -> bool) list -> bool
effect Success : int -> unit 
effect Failure : int -> int 

let amb (xs : (unit -> bool) list) counter : bool
(*@ ex r; Choose(emp, (xs), r); ex x; req counter->x; Norm(counter->x+1 /\ res = r) @*)
= let b = perform (Choose xs) in counter := !counter +1; b

let rec iter (f:((unit -> bool) -> unit))  (xs:(unit -> bool) list) :  unit
(*@ ex r; Norm(is_nil(xs)=true/\res=r /\ r=()) 
\/ ex r1 h t; ens head(xs)=h/\tail(xs)=t/\is_cons(xs)=true; f(h, r1); ex r2; iter(f, t, r2); Norm(res=r2) @*)
= match xs with
  | [] -> ()
  | h::t -> f h; iter f t

(*@ predicate containRetTrue (xs, c) = 
   ex r; Norm(is_nil(xs)=true/\res=r /\ r=false/\ c =0 ) 
\/ ex r1 h t r; ens head(xs)=h/\tail(xs)=t/\is_cons(xs)=true; h((), r1); Norm(c = 1 /\ r1=true /\ res=r /\ r=true)
\/ ex r1 h t; ens head(xs)=h/\tail(xs)=t/\is_cons(xs)=true; h((), r1); 
   Norm(r1=false); ex r c'; containRetTrue (t,c',r) ; Norm(c = c'+1 /\ res=r) @*)

let m xs counter
(*@ ex r1; Choose(emp, (xs), r1); ex x r; req counter->x; Norm(counter->x+1 /\ r1= true /\ res =r /\ r=7 ) 
\/ ex r1; Choose(emp, (xs), r1); ex r2 x;req counter->x;  Failure(counter->x+1 /\r1=false, 500, r2); Norm(res =r2 ) @*)
= if amb xs counter then 7 else perform (Failure 500)


let handle (xs:(unit -> bool) list) counter : int 
(*@ ex r c a r1; req counter -> a; containRetTrue (xs, c, r1); ens counter->a+c /\ r1 = true /\ res = r /\ r=7 
\/ ex r c a r1; req counter -> a; containRetTrue (xs, c, r1); Failure(counter->a+c /\ r1= false, 404, r); Norm (res=r)
@*)
= match (m xs counter) with
  | x -> x
  | effect (Choose xs) k ->
    let f h = 
      match
      (*let k = Obj.clone_continuation k in*)
      let (temp:int) = continue k (h ()) in
      perform (Success (temp))
      with
      | effect (Success x) k -> perform (Success (x))
      | effect (Failure x) k -> ()
      | x -> () 
    in 
    match iter f xs; perform (Failure 404) with 
    (*@ ex r c a r1; req counter -> a; containRetTrue (xs, c, r1); ens counter->a+c /\ r1 = true /\ res = r /\ r=7 
     \/ ex r c a r1; req counter -> a; containRetTrue (xs, c, r1); Failure(counter->a+c /\ r1= false, 404, r); Norm (res=r)
    @*)
    | effect (Success r) k -> r
    | x -> x  

(*
let branch_example_generic (xs: (unit -> bool) list) counter : int
= handle xs counter

let _ =
  let counter  = ref 0 in
  let v = branch_example_generic [(fun () -> false); (fun () -> false); (fun () -> true); (fun () -> false)] counter in
  Printf.printf "(%d)\n%!" v;
  Printf.printf "(counter = %d)\n%!" !counter
  *)