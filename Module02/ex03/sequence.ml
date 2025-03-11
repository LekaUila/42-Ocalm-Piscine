(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sequence.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/11 15:29:56 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/11 16:57:07 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec list_get_elt_n list n   =
  match list with
  | [] -> ""
  | hd :: tl ->
    if n = 0 then
      hd
    else
      list_get_elt_n tl (n - 1)

let rec int_to_string n =
  let dic =  ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"]
  in
    if n < 10 then
      list_get_elt_n dic n
    else
      int_to_string (n / 10) ^ list_get_elt_n dic (n mod 10)

let rec list_to_string list  = 
  match list with
  | [] -> ""
  | hd :: tl -> (int_to_string hd) ^ list_to_string tl

let rec encode_modify list : int list  = 
  match list with
  | [] -> []
  | hd :: tl ->
    let rec get_number_of list letter =
      match list with
      | [] -> 0
      | hd :: tl -> 
        if hd = letter then 
          1 + get_number_of (tl) letter 
        else
          0
    in
      let rec ntl list nb =
        match list with
        | [] -> []
        | hd :: tl ->
          if nb = 0 then
            list
          else
            ntl (tl) (nb - 1)
      in
        let letter_nb = get_number_of list (hd)
        in
        [letter_nb] @ [hd] @ (encode_modify (ntl list letter_nb))

let sequence n  =
  if n < 1 then
    ""
  else
    let rec sequence_intern n = 
      match n with
      | 1 -> [1]
      | n -> encode_modify (sequence_intern (n - 1))
    in
    list_to_string (sequence_intern n)







let main () : unit =
  print_string ((sequence (-1)) ^ "\n") ;
  print_string ((sequence 0) ^ "\n") ;
  print_string ((sequence 1) ^ "\n") ;
  print_string ((sequence 2) ^ "\n") ;
  print_string ((sequence 3) ^ "\n") ;
  print_string ((sequence 4) ^ "\n") ;
  print_string ((sequence 5) ^ "\n") ;
  print_string ((sequence 6) ^ "\n") ;
  print_string ((sequence 7) ^ "\n") ;
  print_string ((sequence 8) ^ "\n") ;
  print_string ((sequence 9) ^ "\n") ;
  print_string ((sequence 10) ^ "\n") ;
  print_string ((sequence 11) ^ "\n") ;
  print_string ((sequence 12) ^ "\n") ;
  print_string ((sequence 13) ^ "\n") ;
  print_string ((sequence 14) ^ "\n") 

let x = main()