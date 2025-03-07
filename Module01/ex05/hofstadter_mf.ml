(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   hofstadter_mf.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 12:09:09 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/07 12:28:02 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec
  hfs_f n =
    if n < 0 then
      -1
    else if n = 0 then
      1
    else
      n - hfs_m(hfs_f(n - 1))
  and 
  hfs_m n =
    if n < 0 then
      -1
    else if n = 0 then
      0
    else
      n - hfs_f(hfs_m(n - 1))




let main () : unit =
  print_endline("Test pour M 0:");
  print_int (hfs_m 0);
  print_newline ();
  print_endline("Test pour F 0:");
  print_int (hfs_f 0);
  print_newline ();
  print_endline("Test pour M 4:");
  print_int (hfs_m 4);
  print_newline ();
  print_endline("Test pour F 4:");
  print_int (hfs_f 4);
  print_newline ()


let x = main()