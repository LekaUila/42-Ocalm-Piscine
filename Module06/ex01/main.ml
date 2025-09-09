(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/01 15:05:10 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/09 11:35:17 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Doctor
include People



let main () =
  let p = new doctor in
  p#talk;
  print_endline p#to_string;
  p#use_sonic_screwdrive;
  p#travel_in_time 2077 2080;
  print_endline p#to_string;
  p#set_hp_to_50;
  print_endline p#to_string;
  p#use_private_regenerate;
  print_endline p#to_string



let () = main ()