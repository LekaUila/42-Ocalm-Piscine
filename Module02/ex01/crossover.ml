(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   crossover.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/11 13:24:04 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/11 14:09:41 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec supr_elt_of_list list elt =
  match list with
  | [] -> []
  | hd :: tl ->
    if hd = elt then
      supr_elt_of_list tl elt
    else
      hd :: supr_elt_of_list tl elt

let rec is_elt_in_list list elt =
  match list with
  | [] -> []
  | hd :: tl ->
    if hd = elt then
      [hd]
    else
      is_elt_in_list tl elt

let rec crossover l1 l2 =
  match l1 with
  | [] -> []
  | hd :: tl ->
    is_elt_in_list l2 hd @ crossover (supr_elt_of_list tl hd) l2


















let rec print_list_int list =
  match list with
  | [] -> print_newline ()
  | hd :: tl ->
    print_char '"';
    print_int hd;
    print_char '"';
    print_list_int tl

let rec print_list_char list =
  match list with
  | [] -> print_newline ()
  | hd :: tl ->
    print_char '"';
    print_char hd;
    print_char '"';
    print_list_char tl

let rec print_list_string list =
  match list with
  | [] -> print_newline ()
  | hd :: tl ->
    print_char '"';
    print_string hd;
    print_char '"';
    print_list_string tl

let main () : unit =
  print_endline("Test pour   ['a';'a';'a';'b';'b';'b'] et ['a';'a';'a';'b';'b';'b'] :");
  print_list_char (crossover  ['a';'a';'a';'b';'b';'b'] ['a';'a';'a';'b';'b';'b']);
  print_newline ();
  print_endline("Test pour   ['a';'a';'a';'b';'b';'b'] et [] :");
  print_list_char (crossover  ['a';'a';'a';'b';'b';'b'] []);
  print_newline ();
  print_endline("Test pour   [] et ['a';'a';'a';'b';'b';'b'] :");
  print_list_char (crossover  [] ['a';'a';'a';'b';'b';'b']);
  print_newline ();
  print_endline("Test pour   [] []:");
  print_list_string (crossover  [] []);
  print_newline ();
  print_endline("Test pour   [\"lol\";\"pizza\";\"test\"] [\"cuicui\";\"pizza\";\"tset\"]:");
  print_list_string (crossover  ["lol";"pizza";"test"] ["cuicui";"pizza";"tset"]);
  print_newline ();
  print_endline("Test pour   [ 1 ; 2 ; 3 ] [ 4 ; 5 ; 6 ]:");
  print_list_int (crossover  [1;2;1] [4;5;6]);
  print_newline ()

let x = main()