(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/10 14:39:34 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/10 15:57:09 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Aluminium
include Carbon
include Copper
include Hydrogen
include Nickel
include Oxygen

let main () =
  let alu = new aluminium in
  let ca = new carbon in
  let co = new copper in
  let hy = new hydrogen in
  let nik = new nickel in
  let oxy = new oxygen in
  let oxy2 = new oxygen in
   print_endline alu#to_string;
   print_endline ca#to_string;
   print_endline co#to_string;
   print_endline hy#to_string;
   print_endline nik#to_string;
   print_endline oxy#to_string;
   print_endline (if alu#equals co then "true" else "false");
   print_endline (if oxy#equals oxy2 then "true" else "false")
   




let () = main ()