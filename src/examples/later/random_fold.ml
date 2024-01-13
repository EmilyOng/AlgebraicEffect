
let rec foldr f li acc =
  match li with 
  | [] -> acc 
  | x :: xs -> 
    let acc' = f x acc in 
    foldr f xs acc'

let rec sum li = 
  match li with 
  | [] -> 0
  | x :: xs -> x + sum xs

let foldr_sum1 xs k
(*@ ex r; sum(xs, r); ens res=r+k @*)
= let g c t = c + t in
  foldr g xs k

let foldr_sum2 xs init
(*@ ex r; sum(xs, r); ens res=r+init @*)
= let g c t = c + t in
  foldr g xs init
