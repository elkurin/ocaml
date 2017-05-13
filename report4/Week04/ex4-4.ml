type 'a m = int -> ('a * int)

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) = 
	fun init -> let (a, b) = x init in f a b

(** return : 'a -> 'a m *)
let return x = fun init -> (x, init)

(** memo : (int -> int m) -> int -> int m *)
let memo (f : int -> int m) n = 
	fun i -> (f n) i

(** runMemo : 'a m -> 'a *)
let runMemo (x : 'a m) = x ()
	
let rec fib n =
  if n <= 1 then 
    return n
  else
    (memo fib (n-2)) >>= (fun r1 ->
    (memo fib (n-1)) >>= (fun r2 ->
      return (r1 + r2)))
			   
let _ =
  if runMemo (fib 80) = 23416728348467685 && runMemo (fib 10) = 55 then
    print_string "ok\n"
  else
    print_string "wrong\n"
