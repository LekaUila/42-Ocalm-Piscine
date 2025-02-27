(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ackermann.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/27 12:44:02 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/27 12:53:28 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ackermann m n =
  if m < 0 || n < 0 then
    -1
  else
  (
    if m = 0 then
      n + 1
    else
    (
      if n = 0 then
        ackermann (m - 1) 1
      else
        ackermann (m - 1) (ackermann m (n - 1)) 
     )
  )

let main () : unit =
  print_endline("Test pour (-1) 7:");
  print_int (ackermann (-1) 7);
  print_newline ();
  print_endline("Test pour 7 (-1):");
  print_int (ackermann 7 (-1));
  print_newline ();
  print_endline("Test pour (-1) (-1):");
  print_int (ackermann (-1) (-1));
  print_newline ();
  print_endline("Test pour 0 0:");
  print_int (ackermann 0 0);
  print_newline ();
  print_endline("Test pour 0 1:");
  print_int (ackermann 0 1);
  print_newline ();
  print_endline("Test pour 1 0:");
  print_int (ackermann 1 0);
  print_newline ();
  print_endline("Test pour 2 3:");
  print_int (ackermann 2 3);
  print_newline ();
  print_endline("Test pour 4 1:");
  print_int (ackermann 4 1);
  print_newline ()

let x = main()