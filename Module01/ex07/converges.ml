(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   converges.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 12:40:07 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/07 13:18:44 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec converges f x n : bool = 
  if n < 0 then
    false
  else
    let y = f x in 
    if y = x then
      true
    else if n = 0 then
      false
    else     
      converges f y (n - 1)




let main () : unit =
  print_endline("Test pour  (( * ) 2) 2 5:");
	if converges  (( * ) 2) 2 5 then
		print_endline "    true"
	else
		print_endline "    false";

  print_endline("Test pour   (fun x -> x / 2) 2 3:");
	if converges  (fun x -> x / 2) 2 3 then
		print_endline "    true"
	else
		print_endline "    false";

  print_endline("Test pour   (fun x -> x / 2) 2 2:");
	if converges  (fun x -> x / 2) 2 2 then
		print_endline "    true"
	else
		print_endline "    false";

  print_endline("Test pour   (fun x -> x / 2) 2 1:");
	if converges  (fun x -> x / 2) 2 1 then
		print_endline "    true"
	else
		print_endline "    false";

  print_endline("Test pour   (fun x -> x / 2) 2 -1:");
	if converges   (fun x -> x / 2) 2 (-1) then
		print_endline "    true"
	else
		print_endline "    false"


let x = main()