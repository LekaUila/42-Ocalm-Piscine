(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/10 14:39:34 by lflandri          #+#    #+#             *)
(*   Updated: 2025/11/07 17:10:57 by Leka Uïla        ###   ########.fr       *)
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

include Reaction
include Alkane_combustion

let print_alkane_combustion (ac : Alkane_combustion.alkane_combustion) =
  let rec printlist list =
      match list with
      | [] -> ()
      | hd :: tl -> match hd with
        | molec, mult -> print_int mult;
                         print_string molec#formula;
                         print_char ' ';
                         printlist tl
  in
  printlist ac#get_start;
  print_string " -> ";
  printlist ac#get_result;
  print_char '\n'
  

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
  let alk13 = new alkane 13 in
  let methalk = new alkane 1 in
  let meth = new methane in
  let oct = new octane in
  let eth = new ethane in
    print_endline alk7#to_string;
    print_endline alk13#to_string;
    print_endline meth#to_string;
    print_endline eth#to_string;
    print_endline oct#to_string;
    print_endline (if meth#equals methalk then "true" else "false");
    print_endline (if meth#equals oct then "true" else "false");
    print_endline (if meth#equals glouglou then "true" else "false");
  print_endline "---------------------------------------------------------------";
  print_endline "all test in ex04 because of virtual class which does nothing";
  print_endline "---------------------------------------------------------------";
  let alkaneComb1 = new alkane_combustion (((new alkane 1) , 2) :: ([(new dioxygen , 5)])) ((new carbon_dioxide , 3) :: ([(new water , 5)])) in (*not balanced*)
  let alkaneComb2 = new alkane_combustion (((new alkane 1) , 1) :: ([(new dioxygen , 2)])) ((new carbon_dioxide , 1) :: ([(new water , 2)])) in(*balanced*)
  let alkaneComb3 = new alkane_combustion (((new alkane 1) , 1) :: ([(new dioxygen , 2)])) ((new carbon_monoxide , 1) :: ([(new water , 2)])) in (*not balanced*)
  let alkaneComb4 = new alkane_combustion (((new alkane 2) , 2) :: (((new alkane 7) , 2) :: ([(new dioxygen , 5)]))) ((new carbon_dioxide , 3) :: ([(new water , 5)])) in (*not balanced*)
  print_endline "----------------TEST IS BALANCED----------------- :";
  print_endline "test alkaneComb1 is_balanced :";
  if alkaneComb1#is_balanced
    then print_endline "balanced alkane combustion"
    else print_endline "unbalanced alkane combustion";
  print_endline "test alkaneComb1 get_start :";
  let checktext1 = try alkaneComb1#get_start; () with e -> print_endline (Printexc.to_string e) in
  print_endline "test alkaneComb2 is_balanced :";
  if alkaneComb2#is_balanced
    then print_endline "balanced alkane combustion"
    else print_endline "unbalanced alkane combustion";
  print_endline "test alkaneComb3 :";
  if alkaneComb1#is_balanced
    then print_endline "balanced alkane combustion"
    else print_endline "unbalanced alkane combustion";
  print_endline "test alkaneComb4 :";
  if alkaneComb4#is_balanced
    then print_endline "balanced alkane combustion"
    else print_endline "unbalanced alkane combustion";
 print_endline "------------------TEST BALANCED------------------- :";
  print_endline "balancing alkaneComb1 :";
  print_alkane_combustion alkaneComb1#balance;
  print_endline "balancing alkaneComb2 :";
  print_alkane_combustion alkaneComb2#balance;
  print_endline "balancing alkaneComb3 :";
  let checktext2 = try alkaneComb3#balance; () with e -> print_endline (Printexc.to_string e) in
  print_endline "balancing alkaneComb4 :";
  print_alkane_combustion alkaneComb4#balance


let () = main ()