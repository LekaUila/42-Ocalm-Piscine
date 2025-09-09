(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dalek.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/09 11:59:16 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/09 12:48:06 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include People
include Random

class dalek =
  object (self)
    val mutable name = "Dalek" ^ (string_of_int (Random.int 9)) ^ (string_of_int (Random.int 9)) ^ (string_of_int (Random.int 9)) 
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