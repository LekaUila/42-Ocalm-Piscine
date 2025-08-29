(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/08/29 16:57:08 by lflandri          #+#    #+#             *)
(*   Updated: 2025/08/29 18:51:15 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig
  val pair : (int * int)
end

module type VAL = sig val x : int end

(* module MAKEPROJECTION = functor (P : Pair) -> sig val x : int end *)

(* module type X = sig
  val x : int
end

module IncX = functor (M : X) -> struct
  let x = M.x + 1
end *)

module MakeFst = functor (P : PAIR) -> struct
  let (a , b) = P 
(* in let x = a *)
end


module MakeSnd = functor (P : PAIR) -> struct
  let x = P
    (* match P with
    | x , y -> y *)
end



module Pair : PAIR = struct let pair = ( 21, 42 ) end
module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)
let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x