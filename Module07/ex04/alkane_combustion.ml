(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/10/17 11:40:13 by lflandri          #+#    #+#             *)
(*   Updated: 2025/11/07 17:08:52 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Reaction
include Carbon
include Hydrogen
include Dioxygen
include Carbon_dioxide
include Water

  let rec printlist list =
      match list with
      | [] -> true
      | hd :: tl -> match hd with
        | molec, mult -> print_int mult;
                         print_string molec#formula;
                         print_char ' ';
                         printlist tl

let getNameAlkane n =
  match n with
  | 1 -> "méthane"
  | 2 -> "éthane"
  | 3 -> "proane"
  | 4 -> "butane"
  | 5 -> "pentane"
  | 6 -> "hexane"
  | 7 -> "heptane"
  | 8 -> "octane"
  | 9 -> "nonane"
  | 10 -> "décane"
  | 11 -> "undéane"
  | 12 -> "dodécanane"
  | i -> "no-named alkane"

let isMoleculeAnAlkane (m: molecule) =
  let rec tryAll m nb =
    if nb == 13
      then false
  else if String.compare m#name (getNameAlkane nb) == 0
        then true
      else tryAll m (nb + 1)
  in
  tryAll m 1

let checkListMolecule (starlist: (molecule * int) list) (endlist: (molecule * int) list) =
  let isEndMole (m: molecule) =
    if String.compare m#name "Carbon Dioxide" == 0 || String.compare m#name "Water" == 0
      then true
      else false
  in
  let isStartMole (m: molecule) =
      if isMoleculeAnAlkane m
        then true
    else if String.compare m#name "Dioxygen" == 0
            then true
            else false
  in
  let rec checklist f list =
      match list with
      | [] -> true
      | hd :: tl -> match hd with
        | molec, mult -> f molec && checklist f tl
  in
  checklist isStartMole starlist && checklist isEndMole endlist
    
let checkListComponent (starlist: (molecule * int) list) (endlist: (molecule * int) list) =
  let isEndMole (m: molecule) =
    if String.compare m#name "Carbon Dioxide" == 0 
      then (true, false)
      else
        if String.compare m#name "Water" == 0
          then (false, true)
          else (false, false)
  in
  let isStartMole (m: molecule) =
     if isMoleculeAnAlkane m
      then (true, false)
      else if String.compare m#name "Dioxygen" == 0
            then (false, true)
            else (false, false)
  in
  let rec checklist f list result : (bool * bool) =
      match list with
      | [] -> result
      | hd :: tl -> match hd with
        | molec, mult -> match f molec with
          | p1, p2 -> match result with
            | p3, p4 -> checklist f tl (p1 || p3, p2 || p4)
  in
  match checklist isStartMole starlist (false, false) with
  | p1, p2 -> match checklist isEndMole endlist (false, false) with
              | p3, p4 -> p1 && p3 && p2 && p4

class alkane_combustion (starlist: (molecule * int) list) (endlist: (molecule * int) list) =
  object (self)
    inherit reaction starlist endlist

    method get_start = if self#is_balanced
                        then starlist
                        else raise (BalancedError "Alkane Combustion not balanced")

    method get_result = if self#is_balanced
                        then endlist
                        else raise (BalancedError "Alkane Combustion not balanced")
                        
    method balance = 
        let rec getCarbAndHydroFromAlkane atomLst carb hydro =
          match atomLst with
          | [] -> (carb, hydro)
          | hd :: tl -> if String.compare "carbon" hd#name == 0
            then getCarbAndHydroFromAlkane tl (carb + 1) hydro
            else getCarbAndHydroFromAlkane tl carb (hydro + 1)
        in
        let rec getAlkane starlist returnValue : (int * int *  (molecule * int) list) =
          match starlist with
          | [] -> returnValue
          | hd :: tl -> match hd with
            | molec, mult -> if not(isMoleculeAnAlkane molec)
              then getAlkane tl returnValue
              else let carb1, hydro1, lst = returnValue in
              let carb2, hydro2 = getCarbAndHydroFromAlkane molec#getAtomList 0 0 in
              let carb3, hydro3, lst3 = getAlkane tl (carb1 + carb2, hydro1 + hydro2, lst) in
              if (hydro3 / 2) mod 2 == 0
                then (carb3, hydro3, (molec, 1) :: lst3)
                else if (hydro2 / 2) mod 2 == 0
                  then (carb3, hydro3, (molec, 1) :: lst3)
                  else (carb3 + carb2, hydro3 + hydro2, (molec, 2) :: lst3)
      in
        if checkListMolecule starlist endlist && checkListComponent starlist endlist
          then
          let carb, hydro, lst = getAlkane starlist (0, 0, []) in
          new alkane_combustion ((new dioxygen, (carb * 2 + hydro / 2) / 2) :: lst) ((new carbon_dioxide, carb) :: [(new water, hydro / 2)])
        else
            raise (BadReaction "No way to balance this equation as an alkane combustion")
        
    method is_balanced =
          if checkListMolecule starlist endlist
          then
          let rec count_atoms oxy carb hydro (list: (molecule * int) list) : (int * int * int) =
                let rec count_atom (name_a : char) (name_m : string) (ind : int) : int =
                          let rec create_number str nb (ind : int) : int =
                            if String.length str > ind 
                              then let chr = String.get str ind in
                                if chr == '0' || chr == '1' || chr == '2' || chr == '3' || chr == '4' || chr == '5' || chr == '6' || chr == '7' || chr == '8' || chr == '9'
                                  then create_number str (nb * 10 + (int_of_char chr - int_of_char '0')) (ind + 1)
                                  else if nb == 0
                                    then 1
                                    else nb
                              else if nb == 0
                                    then 1
                                    else nb

                  in
                  if String.length name_m == ind
                    then 0
                    else let chr = String.get name_m ind in
                          if chr != name_a
                            then count_atom name_a name_m (ind + 1)
                            else create_number name_m 0 (ind + 1)
            in
            match list with
            | [] -> (oxy, carb, hydro)
            | hd :: tl -> 
              match hd with
              | molec, mult -> count_atoms (oxy + (count_atom 'O' molec#formula 0) * mult)
                                          (carb + (count_atom 'C' molec#formula 0) * mult)
                                          (hydro + (count_atom 'H' molec#formula 0) * mult)
                                          tl
          in
          let o1, c1, h1 = count_atoms 0 0 0 starlist in
          let o2, c2, h2 = count_atoms 0 0 0 endlist in
          if o1 == o2
            then if c1 == c2
              then if h1 == h2
                then true
                else false
              else false
            else false
      else raise (BadReaction "unpossible molecule in reaction")
  end