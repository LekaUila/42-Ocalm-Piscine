(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sum.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/04 15:02:52 by lflandri          #+#    #+#             *)
(*   Updated: 2025/06/04 15:15:22 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Float



let sum a b : float =
  Float.add a b

let main () : unit =
  print_string "1. + 2. = ";
  print_string (Float.to_string (sum 1. 2.));
  print_char '\n';
  print_string "21.21 + 21.21 = ";
  print_string (Float.to_string (sum 21.21 21.21)); 
  print_char '\n';
  print_string "-1. + 0.6 = ";
  print_string (Float.to_string (sum Float.minus_one 0.6));
  print_char '\n'



let x = main ()
