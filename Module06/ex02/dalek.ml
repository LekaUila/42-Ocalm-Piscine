(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/09 11:59:16 by lflandri          #+#    #+#             *)
(*   Updated: 2025/12/02 18:42:26 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include People
include Random

let getLetterLower nb = 
  match nb with
  | 0 -> "a"
  | 1 -> "b"
  | 2 -> "c"
  | 3 -> "d"
  | 4 -> "e"
  | 5 -> "f"
  | 6 -> "g"
  | 7 -> "h"
  | 8 -> "i"
  | 9 -> "j"
  | 10 -> "k"
  | 11 -> "l"
  | 12 -> "m"
  | 13 -> "n"
  | 14 -> "o"
  | 15 -> "p"
  | 16 -> "q"
  | 17 -> "r"
  | 18 -> "s"
  | 19 -> "t"
  | 20 -> "u"
  | 21 -> "v"
  | 22 -> "w"
  | 23 -> "x"
  | 24 -> "y"
  | x -> "z"

let getLetterUpper nb = 
  match nb with
  | 0 -> "A"
  | 1 -> "B"
  | 2 -> "C"
  | 3 -> "D"
  | 4 -> "E"
  | 5 -> "F"
  | 6 -> "G"
  | 7 -> "H"
  | 8 -> "I"
  | 9 -> "J"
  | 10 -> "K"
  | 11 -> "L"
  | 12 -> "M"
  | 13 -> "N"
  | 14 -> "O"
  | 15 -> "P"
  | 16 -> "Q"
  | 17 -> "R"
  | 18 -> "S"
  | 19 -> "T"
  | 20 -> "U"
  | 21 -> "V"
  | 22 -> "W"
  | 23 -> "X"
  | 24 -> "Y"
  | x -> "Z"

class dalek =
  object (self)
    val mutable name = "Dalek" ^ (getLetterUpper (Random.int 26)) ^ (getLetterLower (Random.int 26)) ^ (getLetterLower (Random.int 26)) 
    val mutable hp = 100
    val mutable shield = true
        
    method to_string = "class people : (name=" ^ name ^ ", hp=" ^ (string_of_int hp) ^ ", shield=" ^ (if shield then "true" else "false") ^  ")"
    method talk = match Random.int 4 with
                      | 0 -> print_endline "Explain! Explain!"
                      | 1 -> print_endline "Exterminate! Exterminate!"
                      | 2 -> print_endline "I obey!"
                      | x -> print_endline "You are the Doctor! You are the enemy of the Daleks!"
    method die = print_endline "Emergency Temporal Shift!"
    method exterminate (p: people) = p#die; shield <- (if shield then false else true)
    initializer print_string "Dalek "; print_string name; print_endline " created."
  end