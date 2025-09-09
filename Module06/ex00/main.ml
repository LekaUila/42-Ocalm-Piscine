(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/01 15:05:10 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/09 11:04:23 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include People



let main () =
  (* print_endline "test" *)
  let p = new people "Clara" in
  p#talk;
  print_endline p#to_string;
  p#die


let () = main ()