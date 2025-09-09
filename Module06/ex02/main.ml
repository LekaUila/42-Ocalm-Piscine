(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/01 15:05:10 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/09 12:49:20 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Doctor
include People
include Dalek
include Random



let main () =
  Random.self_init ();
  let p = new people "Random Human" in
  let d = new dalek in
  d#talk;
  d#talk;
  d#talk;
  print_endline d#to_string;
  d#exterminate p;
  print_endline d#to_string;
  d#die




let () = main ()