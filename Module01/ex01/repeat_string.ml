(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   repeat_string.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/27 12:11:56 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/27 12:37:26 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec repeat_string ?(str = "x") n : string =
	if n < 0 then
		"error"
	else
	(
		if n = 0 then
			""
		else
			str ^ repeat_string ~str: str (n - 1)
	)



(* main test *)

let main () : unit =
	print_endline("Test pour -1 :");
	print_endline (repeat_string (-1));
	print_endline("Test pour 0 :");
	print_endline (repeat_string 0);
	print_endline("Test pour 1 :");
	print_endline (repeat_string (1));
	print_endline("Test pour 2 :");
	print_endline (repeat_string (2));
	print_endline("Test pour 5 :");
	print_endline (repeat_string (5));

	print_endline("Test pour test -1 :");
	print_endline (repeat_string ~str: "test" (-1));
	print_endline("Test pour lol 0 :");
	print_endline (repeat_string ~str: "lol" 0);
	print_endline("Test pour toto 1 :");
	print_endline (repeat_string ~str: "toto" (1));
	print_endline("Test pour what 3 :");
	print_endline (repeat_string ~str: "what" (3));
	print_endline("Test pour a 5 :");
	print_endline (repeat_string ~str: "a" (5))
	

let x = main()
