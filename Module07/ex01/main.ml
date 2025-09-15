(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/10 14:39:34 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/15 16:39:29 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Aluminium
include Carbon
include Copper
include Hydrogen
include Nickel
include Nitrogen
include Oxygen

include Molecule
include Water
include Carbon_dioxide
include Carbon_monoxide
include Trinitroluene
include Dioxygen


let main () =
  let alu = new aluminium in
  let ca = new carbon in
  let co = new copper in
  let hy = new hydrogen in
  let nik = new nickel in
  let nitro = new nitrogen in
  let oxy = new oxygen in
  let oxy2 = new oxygen in
   print_endline alu#to_string;
   print_endline ca#to_string;
   print_endline co#to_string;
   print_endline hy#to_string;
   print_endline nik#to_string;
   print_endline nitro#to_string;
   print_endline oxy#to_string;
   print_endline (if alu#equals co then "true" else "false");
   print_endline (if oxy#equals oxy2 then "true" else "false");
  print_endline "---------------------------------------------------------------";
   (* let mol = new molecule "TEST" (oxy :: (alu :: (oxy :: (hy :: (ca :: (hy :: (oxy :: [])))))))
   in print_endline mol#to_string; *)
  let glouglou = new water in
  let ca_2 = new carbon_dioxide in
  let ca_1 = new carbon_monoxide in
  let tnt = new trinitroluene in
  let o2 = new dioxygen in
    print_endline glouglou#to_string;
    print_endline ca_2#to_string;
    print_endline ca_1#to_string;
    print_endline tnt#to_string;
    print_endline o2#to_string

   




let () = main ()