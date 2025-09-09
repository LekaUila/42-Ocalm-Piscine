(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/09 11:11:45 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/09 12:01:21 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include People

class doctor =
  object (self)
    val mutable name = "Doctor"
    val mutable hp = 100
    val mutable sidekick = new people "Clara"
    val mutable age = 25
        
    method to_string = "class people : (name=" ^ name ^ ", hp=" ^ (string_of_int hp) ^ ", age=" ^ (string_of_int age) ^ ", sidekick=" ^ sidekick#to_string ^ ")"
    method talk = print_endline ("Hi! I'm the Doctor!")
    method private regenerate = hp <- 100
    method use_sonic_screwdrive = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method travel_in_time (d1: int) (d2: int) = age <- (d2 - d1 + age);
   print_endline "        ___";
   print_endline "        | |";
   print_endline "        | |";
   print_endline "-------------------";
   print_endline "-------------------";
   print_endline " |  ___  |  ___  |";
   print_endline " | | | | | | | | |";
   print_endline " | |-+-| | |-+-| |";
   print_endline " | |_|_| | |_|_| |";
   print_endline " |  ___  |  ___  |";
   print_endline " | |   | | |   | |";
   print_endline " | |   | | |   | |";
   print_endline " | |___| | |___| |";
   print_endline " |  ___  |  ___  |";
   print_endline " | |   | | |   | |";
   print_endline " | |   | | |   | |";
   print_endline " | |___| | |___| |";
   print_endline " |       |       |";
   print_endline "==================="
    method use_private_regenerate = self#regenerate
    method set_hp_to_50 = hp <- 50
    initializer print_string "Doctor "; print_string name; print_endline " created."
  end