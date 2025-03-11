(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   encode.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/11 11:45:53 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/11 13:22:50 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let rec encode list :  (int * 'a) list = 
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
          [(letter_nb, (hd))] @ (encode (ntl list letter_nb))



let rec print_tuple t = 
  match t with
  | ((first : int),(second : string)) -> print_int first; print_string second

let rec print_encode_list list =
  match list with
  | [] -> print_newline ()
  | hd :: tl ->
    print_char '(';
    print_tuple hd;
    print_char ')';
    print_encode_list tl
    


let main () : unit =
  print_endline("Test pour   [\"a\";\"a\";\"a\";\"b\";\"b\";\"b\"] :");
  print_encode_list (encode  ["a";"a";"a";"b";"b";"b"]);
  print_newline ();
  print_endline("Test pour   [] :");
  print_encode_list (encode  []);
  print_newline ();
  print_endline("Test pour   [\"a\";\"a\";\"a\";\"b\";\"c\";\"b\"] :");
  print_encode_list (encode  ["a";"a";"a";"b";"c";"b"]);
  print_newline ()

let x = main()