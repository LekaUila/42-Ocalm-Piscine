(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/01 15:05:10 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/01 15:38:01 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people =
  object (self)
    val mutable name = ""    
    val mutable hp = 100
        
    method to_string = "class people : (name=" ^ name ^ ", hp=" ^ (string_of_int hp) ^ ")"
    method talk = print_endline ("I'm " ^ name ^ "! Do you know the doctor?")
    method die = print_endline "Aaaarghh!"
  end