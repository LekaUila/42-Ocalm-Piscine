(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/21 13:13:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/21 15:34:56 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Card


let rec print_card_list lst =
  match lst with
  | [] -> print_char '\n';
  | hd::tl ->
    print_string (Card.toStringVerbose hd);
    print_newline ();
    print_card_list tl

let main () =
  print_card_list Card.all;
  print_newline ();
  print_endline (Card.toStringVerbose (Card.best Card.all));
  if Card.isSpade (Card.best Card.all) then
    print_endline "Success"
  else
    print_endline "Echec"


let () = main ()