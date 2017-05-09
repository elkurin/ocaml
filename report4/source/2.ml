(* Definition of "the" list monad *)
type 'a m = 'a list

(** (>>=) : 'a m -> ('a -> 'b m) -> 'b m *)
let (>>=) (x : 'a m) (f : 'a -> 'b m) =
  List.concat (List.map f x)

(** return : 'a -> 'a m *)
let return (x : 'a) = [x]

(** guard : bool -> unit m *)
let guard (x : bool) =
  if x then return () else []

(** check if "banana + banana = sinamon" *)
let test_banana ba na si mo n =
  (100 * ba + 10 * na + na
   + 100 * ba + 10 * na + na
   = 1000 * si + 100 * na + 10 * mo + n)

(** check if "send + more = money" *)
let test_money s e n d m o r y =
  (1000 * s + 100 * e + 10 * n + d
   + 1000 * m + 100 * o + 10 * r + e
   = 10000 * m + 1000 * o + 100 * n + 10 * e + y)

let numlist = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;
let rec remove v l =
	match l with
	| [] -> []
	| x :: xs -> if x = v then xs else x :: remove v xs;;

let banana =
	numlist >>= (fun ba ->
	numlist >>= (fun na ->
	numlist >>= (fun si ->
	numlist >>= (fun mo ->
	numlist >>= (fun n  ->
	(guard (test_banana ba na si mo n)) >>= (fun _ ->
		return (ba, na, si, mo, n)))))));;

let money =
	let l = (remove 0 numlist) in l >>= (fun s ->
	let l = (remove s l) in l >>= (fun m ->
	let l = 0 :: (remove m l) in l >>= (fun e ->
	let l = (remove e l) in l >>= (fun n ->
	let l = (remove n l) in l >>= (fun d ->
	let l = (remove d l) in l >>= (fun o ->
	let l = (remove o l) in l >>= (fun r ->
	let l = (remove r l) in l >>= (fun y ->
	(guard (test_money s e n d m o r y)) >>= (fun _ ->
		return (s, e, n, d, m, o, r, y))))))))));;
	
