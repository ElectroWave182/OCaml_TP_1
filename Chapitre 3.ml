Exercice 1 :
------------

1)
fonction int -> bool

let pair = fun n -> n mod 2 == 0 ;;

pair (3) ;; -> false
pair (-2) ;; -> true

2)
fonction char -> bool

let minus = fun c -> `a` <= c && c <= `z` ;;

minus (`z`) ;; -> true
minus (`A`) ;; -> false
minus (`0`) ;; -> false
minus (` `) ;; -> false

3)
fonction float -> float

let f = fun x -> if x == 0. then 1. else sin (x) /. x ;;

f (0.) ;; -> 1.
f (3.14) ;; -> 0.000507214304614


Exercice 2 :
------------

2)
fonction float -> float

let decimales = fun x -> abs_float (x -. float_of_int (int_of_float (x))) ;;

decimales (-42.45) ;; -> 0.45
decimales (42.45) ;; -> 0.45
decimales (0.) ;; -> 0.0

6.1)
fonction float -> float

let arrCent = fun x -> float_of_int (int_of_float (x *. 100.)) /. 100. ;;

6.2)
fonction float -> float

let francsEnEuros = fun x -> arrCent (x /. 6.55957) ;;

francsEnEuros (7.) ;; -> 1.06
francsEnEuros (7.11) ;; -> 1.08
francsEnEuros (0.) ;; -> 0.0


Exercice 3 :
------------

1.1)
fonction float -> int

let heures = fun t -> int_of_float (t) ;;

heures (23.42) ;; -> 23

1.2)
fonction float -> int

let minutes = fun t -> int_of_float (t *. 100.) mod 100 ;;

minutes (23.42) ;; -> 42

2)
fonction float -> string

let quelleHeureEstIl = fun
t ->
    let h = heures (t) in
    let m = minutes (t) in
    if h == 0
	then if m == 0
	    then "Il est minuit pile."
	    else "Il est minuit " ^ string_of_int (m) ^ "."
	else if h == 12
	    then if m == 0
		then "Il est midi pile."
		else "Il est midi " ^ string_of_int (m) ^ "."
	    else if m == 0
		then "Il est " ^ string_of_int (h) ^ " heure pile."
		else "Il est " ^ string_of_int (h) ^ " heure " ^ string_of_int (m) ^ "." ;;

quelleHeureEstIl (23.42) ;; -> "Il est 23 heure 42."
quelleHeureEstIl (23.) ;; -> "Il est 23 heure pile."
quelleHeureEstIl (0.42) ;; -> "Il est minuit 42."
quelleHeureEstIl (12.) ;; -> "Il est midi pile."
quelleHeureEstIl (0.) ;; -> "Il est minuit pile."


Exercice 4 :
------------

1)
fonction int * int -> int

let min = fun
(x, y) ->
if x > y
	then y
	else x
;;

min (4, 5) ;; -> 4
min (-6, -10) ;; -> -10
min (0, 0) ;; -> 0

2)
fonction int * int * int -> int

let norme = fun
(x, y, z) -> x * x + y * y + z * z
;;

norme (-6, 2, 0) ;; -> 40
norme (1, 1, 1) ;; -> 3


Exercice 5 :
------------

1)
fonction int * int -> int

let max = fun
(x, y) ->
if x < y
	then y
	else x
;;

max (4, 5) ;; -> 5
max (-6, -10) ;; -> -6
max (0, 0) ;; -> 0

2)
fonction int * int * float -> float

let reel = fun
(a, b, x) ->
	let decimales = abs_float (x -. float_of_int (int_of_float (x))) in
		decimales +. float_of_int (max (a, b))
;;

reel (4, 5, -4.123) ;; -> 5.123
reel (7, 5, 4.123) ;; -> 7.123


Exercice 6 :
------------

1)
fonction int -> int

let chiffre = fun
n ->
	let expr = n mod 10 in
		int_of_float (abs_float (float_of_int (expr)))
;;

chiffre (456) ;; -> 6
chiffre (-11) ;; -> 1
chiffre (0) ;; -> 0

2)
fonction int * int -> int

let echange = fun
(n, p) ->
	let tronc = n / 10 * 10 in
		if tronc = 0
		then chiffre (p)
		else
			let signe = int_of_float (abs_float (float_of_int (tronc))) / tronc in
				tronc + signe * chiffre (p)
;;

echange (456, 23) ;; -> 453
echange (-47, -985) ;; -> -45
echange (0, 0) ;; -> 0


Exercice 8 :
------------

fonction float * float * float -> int

let nb_sol = fun
(a, b, c) ->
	let delta = b *. b -. 4. *. a *. c in
		if delta > 0.
		then 2
		else if delta = 0.
			then 1
			else 0
;;

nb_sol (1., 2., 1.5) ;; -> 0
nb_sol (-4.5, 0., 0.) ;; -> 1
nb_sol (11.8, 0., -1.2) ;; -> 2
nb_sol (-486.9, 0.0008, 0.) ;; -> 2
