(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tak.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/27 13:12:15 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/27 13:24:57 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec tak x y z : int = 
  if y < x then
    tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  else
    z


let main () : unit =
  print_endline("Test pour 1 2 3:");
  print_int (tak 1 2 3);
  print_newline ();
  print_endline("Test pour 5 23 7:");
  print_int (tak 5 23 7);
  print_newline ();
  print_endline("Test pour 9 1 0:");
  print_int (tak 9 1 0);
  print_newline ();
  print_endline("Test pour 1 1 1:");
  print_int (tak 1 1 1);
  print_newline ();
  print_endline("Test pour 0 42 0:");
  print_int (tak 0 42 0);
  print_newline ();
  print_endline("Test pour 23498 98734 98776:");
  print_int (tak 23498 98734 98776);
  print_newline ()

let x = main()