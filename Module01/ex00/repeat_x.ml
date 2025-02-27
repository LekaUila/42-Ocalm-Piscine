(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_x.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/27 12:11:56 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/27 12:23:23 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec repeat_x n : string =
	if n < 0 then
		"error"
	else
	(
		if n = 0 then
			""
		else
			"x" ^ repeat_x (n - 1)
	)



(* main test *)

let main () : unit =
	print_endline("Test pour -1 :");
	print_endline (repeat_x (-1));
	print_endline("Test pour 0 :");
	print_endline (repeat_x 0);
	print_endline("Test pour 1 :");
	print_endline (repeat_x (1));
	print_endline("Test pour 2 :");
	print_endline (repeat_x (2));
	print_endline("Test pour 5 :");
	print_endline (repeat_x (5))
	

let x = main()
