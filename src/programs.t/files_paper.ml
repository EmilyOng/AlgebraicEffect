effect Open : int -> unit
effect Close: int -> unit

let open_file n
(*@ requires _^* @*)
(*@ ensures Open(n) @*)
= perform (Open n)

let close_file n
(*@ requires _^*.Open(n).(!Close(n))^* @*)
(*@ ensures Close(n) @*)
= perform (Close n)

let file_9 () 
(*@ requires emp @*)
(*@ ensures Open(9).Close(9) @*)
= open_file 9;
  close_file 9

let main = 
  match file_9 () with 
  | _ -> ()
  | effect (Open n) k -> continue k ()
  | effect (Close n) k -> continue k ()