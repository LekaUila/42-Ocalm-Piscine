(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   molecule.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 14:14:44 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/15 16:31:51 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Hydrogen
include String

let hillNotation (al: atom list) =
  let need_inversion (a1: atom) (a2: atom) =
    if (String.compare a1#symbol "C") == 0
      then false
      else if (String.compare a2#symbol "C") == 0
              then true
              else if (String.compare a1#symbol "H") == 0
                  then false
                  else if ((String.compare a2#symbol "H" == 0))
                          then true
                          else ((String.compare a1#symbol a2#symbol) > 0)    
  in
  let rec getListLen l =
    match l with
    | [] -> 0
    | hd :: tl -> (getListLen tl) + 1
  in
  let rec sortList l =
    match l with
    | hd :: mid :: tl -> if need_inversion hd mid 
      then mid :: (sortList (hd :: tl))
      else hd :: (sortList (mid :: tl))
    | e -> l
  in
  let rec getSortList nb l =
    match nb with
    | 0 -> l
    | e -> getSortList (nb - 1) (sortList l)
  in
    getSortList (getListLen al) al



class molecule (n: string) (al: atom list) =
  object (self)
  val atom_list = (hillNotation al)
  method name = n
  method formula =
    let rec calculFormula str actual nb list =
      match list with
      | hd :: tl -> if hd#equals actual
          then calculFormula str actual (nb + 1) tl
          else if nb == 1 
            then calculFormula (str ^ actual#symbol) hd 1 tl
            else calculFormula (str ^ actual#symbol ^ (string_of_int nb)) hd 1 tl
      | [] -> if nb == 1 
            then (str ^ actual#symbol)
            else (str ^ actual#symbol ^ (string_of_int nb))
    in
    let launchCalcul list =
      match list with
      | hd :: tl -> calculFormula "" hd  1 tl
      | [] -> "ERROR_FORMULA"
    in launchCalcul atom_list
  method to_string = "class molecule : (name=" ^ self#name ^ ", formula=" ^ self#formula ^ ")"
  method equals (other: molecule) = (self#name == other#name && self#formula == other#formula)   
  end