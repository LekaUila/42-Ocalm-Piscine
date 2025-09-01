(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex03.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/01 10:17:55 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/01 14:27:28 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* include stdlib *)

module type FIXED = sig
  type t 
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig
  val bits : int
end

module Make = functor (F : FRACTIONNAL_BITS) -> struct
    type t = (int * int)

    let of_float (f : float) =
      let x  = int_of_float (2. ** (float_of_int F.bits)) in
      if ((f *. (float_of_int x)) -. float_of_int (int_of_float (f *. (float_of_int x)))) >= 0.5 then
        ((int_of_float(f *. (float_of_int x))) + 1, x)
      else
        ((int_of_float(f *. (float_of_int x))), x)

    let of_int i =
      let x  = int_of_float (2. ** (float_of_int F.bits)) in
      ((i / 1) * x, x)
      
    let to_float (fp : t) = ((float_of_int (fst fp)) /. (float_of_int (snd fp)))
    let to_int (fp : t) = ( (fst fp) /  (snd fp))
    let to_string (fp : t) = string_of_float ((float_of_int (fst fp)) /. (float_of_int (snd fp)))
    let zero = let x = int_of_float (2. ** (float_of_int F.bits)) in (0, x)
    let one = let x = int_of_float (2. ** (float_of_int F.bits)) in (1 * x, x)
    
    let succ (fp : t) = ((fst fp) + 1, (snd fp))
    let pred (fp : t) = ((fst fp) - 1, (snd fp))
    let min x1 x2 = if (fst x1) <= (fst x2) then x1 else x2
    let max x1 x2 = if (fst x1) >= (fst x2) then x1 else x2
    let gth x1 x2 = if (fst x1) > (fst x2) then true else false
    let lth x1 x2 = if (fst x1) < (fst x2) then true else false
    let gte x1 x2 = if (fst x1) >= (fst x2) then true else false
    let lte x1 x2 = if (fst x1) <= (fst x2) then true else false
    let eqp x1 x2 = if (fst x1) == (fst x2) then true else false (** physical equality *)
    let eqs x1 x2 = if x1 == x2 then true else false (** structural equality *)
    let add x1 x2 = ((fst x1) + (fst x2), (snd x1))
    let sub x1 x2 = ((fst x1) - (fst x2), (snd x1))
    let mul x1 x2 = ((fst x1) * (fst x2), (snd x1))
    let div x1 x2 = ((fst x1) / (fst x2), (snd x1))
    
    let rec foreach x1 x2 func =
      if (fst x1) != (fst x2) then
        (func x1; foreach (((fst x1) + 1), (snd x1)) x2 func)
      else func x1
end




module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
let x8 = Fixed8.of_float 21.10 in
let y8 = Fixed8.of_float 21.32 in
let r8 = Fixed8.add x8 y8 in
print_endline (Fixed8.to_string r8);
Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))


;
print_endline "test function :";
print_endline ""

let print_endline_bool b = print_endline (match b with | true -> "true"| false -> "false")

let test1 = print_endline  (Fixed4.to_string (Fixed4.of_float 25.10))
let test1 = print_endline  (Fixed4.to_string (Fixed4.of_int 25))
let test1 = print_endline (string_of_float  (Fixed4.to_float (Fixed4.of_float 25.10)))
let test1 = print_endline (string_of_int  (Fixed4.to_int (Fixed4.of_float 25.10)))
let test1 = print_endline (Fixed4.to_string (Fixed4.succ Fixed4.one))
let test1 = print_endline (Fixed4.to_string (Fixed4.pred Fixed4.one));
print_endline ""
let test1 = print_endline_bool (Fixed4.gth Fixed4.one Fixed4.one)
let test1 = print_endline_bool (Fixed4.gth Fixed4.zero Fixed4.one)
let test1 = print_endline_bool (Fixed4.gth Fixed4.one Fixed4.zero);
print_endline ""
let test1 = print_endline_bool (Fixed4.lth Fixed4.one Fixed4.one)
let test1 = print_endline_bool (Fixed4.lth Fixed4.zero Fixed4.one)
let test1 = print_endline_bool (Fixed4.lth Fixed4.one Fixed4.zero);
print_endline ""
let test1 = print_endline_bool (Fixed4.gte Fixed4.one Fixed4.one)
let test1 = print_endline_bool (Fixed4.gte Fixed4.zero Fixed4.one)
let test1 = print_endline_bool (Fixed4.gte Fixed4.one Fixed4.zero);
print_endline ""
let test1 = print_endline_bool (Fixed4.lte Fixed4.one Fixed4.one)
let test1 = print_endline_bool (Fixed4.lte Fixed4.zero Fixed4.one)
let test1 = print_endline_bool (Fixed4.lte Fixed4.one Fixed4.zero);
print_endline ""
let test1 = print_endline_bool (Fixed4.eqp Fixed4.one Fixed4.one)
let test1 = print_endline_bool (Fixed4.eqp Fixed4.zero Fixed4.one)
let test1 = print_endline_bool (Fixed4.eqp Fixed4.one Fixed4.zero);
print_endline ""
let test1 = print_endline_bool (Fixed4.eqs Fixed4.one Fixed4.one)
let test1 = print_endline_bool (Fixed4.eqs Fixed4.zero Fixed4.one)
let test1 = print_endline_bool (Fixed4.eqs Fixed4.one Fixed4.zero);
print_endline ""