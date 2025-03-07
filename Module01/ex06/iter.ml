(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   iter.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 12:31:17 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/07 12:36:48 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec iter f x n : int = 
  if n < 0 then
    -1
  else if n = 0 then
    x
  else
    f (iter f x (n - 1))







let main () : unit =
  print_endline("Test pour  (fun x -> x * x) 2 4:");
  print_int (iter  (fun x -> x * x) 2 4);
  print_newline ();
  print_endline("Test pour  (fun x -> x * 2) 2 4:");
  print_int (iter (fun x -> x * 2) 2 4);
  print_newline ();
  print_endline("Test pour  (fun x -> x * 2) 2 -1:");
  print_int (iter  (fun x -> x * 2) 2 (-1));
  print_newline ()


let x = main()