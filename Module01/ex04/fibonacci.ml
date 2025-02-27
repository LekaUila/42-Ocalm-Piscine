(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   fibonacci.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/27 13:30:37 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/27 14:18:56 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec fibonacci n =
  if n < 0 then
    -1
  else
    begin
      let rec fib_intern n e1 e2 =
        if n = 0 then
          e1
        else
          fib_intern (n - 1) e2 (e2 + e1)
    in
      fib_intern n 0 1
    end
         




let main () : unit =

  print_endline("Test pour -1:");
  print_int (fibonacci (-1));
  print_newline ();
  print_endline("Test pour 0:");
  print_int (fibonacci 0);
  print_newline ();
  print_endline("Test pour 1:");
  print_int (fibonacci 1);
  print_newline ();
  print_endline("Test pour 3:");
  print_int (fibonacci 3);
  print_newline ();
  print_endline("Test pour 6:");
  print_int (fibonacci 6);
  print_newline ()

let () = main()