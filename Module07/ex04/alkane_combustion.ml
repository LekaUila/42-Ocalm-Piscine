(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane_combustion.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/10/17 11:40:13 by lflandri          #+#    #+#             *)
(*   Updated: 2025/10/31 12:54:45 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Reaction



let checkListMolecule (starlist: (molecule * int) list) (endlist: (molecule * int) list) =
  let isEndMole (m: molecule) =
    (* print_endline m#name; *)
    if String.compare m#name "Carbon Dioxide" == 0 || String.compare m#name "Water" == 0
      then true
      else false
  in
  let isStartMole (m: molecule) =
    (* print_endline m#name; *)
      match m with
      | (a: Alkane.alkane) -> true
      | e -> if String.compare m#name "Dioxygen" == 0
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
      
    

class alkane_combustion (starlist: (molecule * int) list) (endlist: (molecule * int) list) =
  object (self)
    inherit reaction starlist endlist

    method get_start = if self#is_balanced
                        then starlist
                        else raise (BalancedError "Alkane Combustion not balanced")

    method get_result = if self#is_balanced
                        then endlist
                        else raise (BalancedError "Alkane Combustion not balanced")
                        
    method balance = new alkane_combustion starlist endlist
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