(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/12/04 11:21:58 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/12/10 15:52:41 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


module type MONOID =
    sig
        type element
        val zero1 : element
        val zero2 : element
        val mul : element -> element -> element
        val add : element -> element -> element
        val div : element -> element -> element
        val sub : element -> element -> element
    end

module Calc =
functor (M:MONOID) ->
  struct
    let add e1 e2 = M.add e1 e2
    let sub e1 e2 = M.sub e1 e2
    let mul e1 e2 = M.mul e1 e2
    let div e1 e2 = M.div e1 e2
    let rec power e1 (e2:int) = if (e2 <= 0) then M.zero2 else M.mul e1 (power e1 (e2 - 1))
    let rec fact e1  = if e1 <= M.zero2 then M.zero2 else M.mul e1 (fact (M.sub e1 M.zero2 ))
  end

  exception DivByZero of string

module INT =
  struct
    type element = int
    let zero1 = 0
    let zero2 = 1
    let add e1 e2  = e1 + e2
    let sub e1 e2  = e1 - e2
    let mul e1 e2  = e1 * e2
    let div e1 e2  = if e2 = 0 then raise (DivByZero "Error : division by zero") else e1 / e2
  end


module FLOAT =
  struct
    type element = float
    let zero1 = 0.0
    let zero2 = 1.0
    let add e1 e2  = e1 +. e2
    let sub e1 e2  = e1 -. e2
    let mul e1 e2  = e1 *. e2
    let div e1 e2  = if e2 = 0.0 then raise (DivByZero "Error : division by zero") else e1 /. e2
  end




module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let main () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
  print_endline (string_of_int (Calc_int.fact 3));
  print_endline (string_of_float (Calc_float.fact 3.0));
  print_endline (string_of_int (Calc_int.div (Calc_int.sub (-20) 1) 2));
  print_endline (string_of_float (Calc_float.div (Calc_float.sub (-20.0) 1.0) 2.0));
  let test =   try (string_of_int (Calc_int.div 5 0)) with e -> Printexc.to_string e in  print_endline test;
  let test =   try (string_of_float (Calc_float.div 5.0 0.0)) with e -> Printexc.to_string e in  print_endline test




let () = main ()