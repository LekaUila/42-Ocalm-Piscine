(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/10 14:39:34 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/16 14:55:37 by Leka Uïla        ###   ########.fr       *)
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

include Alkane
include Methane
include Ethane
include Octane



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
  let glouglou = new water in
  let ca_2 = new carbon_dioxide in
  let ca_1 = new carbon_monoxide in
  let tnt = new trinitroluene in
  let tnt2 = new trinitroluene in
  let o2 = new dioxygen in
    print_endline glouglou#to_string;
    print_endline ca_2#to_string;
    print_endline ca_1#to_string;
    print_endline tnt#to_string;
    print_endline o2#to_string;
    print_endline (if tnt#equals o2 then "true" else "false");
    print_endline (if tnt#equals tnt2 then "true" else "false");
  print_endline "---------------------------------------------------------------";
  let alk7 = new alkane 7 in
  let alk0 = new alkane 0 in
  let alk13 = new alkane 13 in
  let methalk = new alkane 1 in
  let meth = new methane in
  let oct = new octane in
  let eth = new ethane in
    print_endline alk7#to_string;
    print_endline alk13#to_string;
    print_endline alk0#to_string;
    print_endline meth#to_string;
    print_endline eth#to_string;
    print_endline oct#to_string;
    print_endline (if meth#equals methalk then "true" else "false");
    print_endline (if meth#equals oct then "true" else "false");
    print_endline (if meth#equals glouglou then "true" else "false")




let () = main ()