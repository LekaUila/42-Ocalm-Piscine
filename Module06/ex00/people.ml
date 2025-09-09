(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   people.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/01 15:05:10 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/09 10:45:09 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class people (n:string) =
  object (self)
    val mutable name = n
    val mutable hp = 100
        
    method to_string = "class people : (name=" ^ name ^ ", hp=" ^ (string_of_int hp) ^ ")"
    method talk = print_endline ("I'm " ^ name ^ "! Do you know the doctor?")
    method die = print_endline "Aaaarghh!"
    initializer print_string "People "; print_string name; print_endline " created."
  end