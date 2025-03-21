(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/21 13:13:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/21 13:44:38 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Color


let rec print_color_list lst =
  match lst with
  | [] -> print_char '\n';
  | hd::tl ->
    print_char '|';
    print_string (Color.toString hd);
    print_char ' ';
    print_string (Color.toStringVerbose hd);
    print_char '|';
    print_color_list tl

let main () =
  print_color_list Color.all
  

let () = main ()