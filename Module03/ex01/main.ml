(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/21 13:13:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/21 14:18:06 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Value


let rec print_value_list lst =
  match lst with
  | [] -> print_char '\n';
  | hd::tl ->
    print_string (Value.toString hd);
    print_newline ();
    print_string "Verbose : ";
    print_endline (Value.toStringVerbose hd);
    print_string "Int value : ";
    print_int (Value.toInt hd);
    print_newline ();
    print_string "Next : ";
    print_endline (try Value.toString (Value.next hd) with invalid_arg -> "error");
    print_string "Previous : ";
    print_endline (try Value.toString (Value.previous hd)  with invalid_arg -> "error");
    print_value_list tl

let main () =
  print_value_list Value.all
  

let () = main ()