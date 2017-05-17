type 'a m = (int * int) list -> ('a * (int * int) list)
type 'a option = Some of 'a | None

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) = 
	fun init -> let (a, b) = x init in (f a) b

(** return : 'a -> 'a m *)
let return x = fun init -> (x, init)

let rec lookup init n =
    match init with
    | [] -> None
    | (x, y) :: z -> if x = n then Some y else lookup z n

(** memo : (int -> int m) -> int -> int m *)
let memo (f : int -> int m) n = 
    fun init -> 
        match lookup init n with
        | None -> let (a, b) = (f n) init in (a, (n, a) :: b)
        | Some y -> (y, init)

(** runMemo : 'a m -> 'a *)
let runMemo (x : 'a m) = let (a, _) = x [] in a
	
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
