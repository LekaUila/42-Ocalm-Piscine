(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/08/29 16:57:08 by lflandri          #+#    #+#             *)
(*   Updated: 2025/08/31 15:08:48 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig
  val pair : (int * int)
end

module type VAL = sig val x : int end

module type MAKEPROJECTION = functor (P : PAIR) -> VAL

module MakeFst : MAKEPROJECTION = functor (P : PAIR) -> struct
  let x = 
  match P.pair with
  | (a , b) -> a
end


module MakeSnd :MAKEPROJECTION = functor (P : PAIR) -> struct
  let x = 
  match P.pair with
  | (a , b) -> b
end



module Pair : PAIR = struct let pair = ( 21, 42 ) end
module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)
let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x