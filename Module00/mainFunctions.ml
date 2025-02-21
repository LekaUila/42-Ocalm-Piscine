(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   mainFunctions.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 19:28:17 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 13:58:31 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* EX00 main test *)

let main () : unit =
	print_endline("Test pour 42 :");
	ft_test_sign 42;
	print_endline("Test pour 0 :");
	ft_test_sign 0;
	print_endline("Test pour -42 :");
	ft_test_sign (-42) 

let x = main()



(* EX01 main test *)

let main () : unit =
	print_endline("Test pour 3 :");
	ft_countdown 3;
	print_endline("Test pour 0 :");
	ft_countdown 0;
	print_endline("Test pour -1 :");
	ft_countdown (-1)

let x = main()



(* EX02 main test *)

let main () : unit =
	print_endline("Test pour 2 4 :");
	print_int (ft_power 2 4);
	print_char '\n';
	print_endline("Test pour 3 0 :");
	print_int (ft_power 3 0);
	print_char '\n';
	print_endline("Test pour 0 5 :");
	print_int (ft_power 0 5);
	print_char '\n'
  
  let x = main()



(* EX03 main test *)

let main () : unit =
	ft_print_alphabet ()


let x = main()



(* EX04 main test *)

let main () : unit =
	ft_print_comb ()


let x = main()



(* EX05 main test *)

let main () : unit =
	print_endline("Test pour \"Hello world !\" :");
	ft_print_rev("Hello world !");
	print_endline("Test pour \"\" :");
	ft_print_rev("");
	print_endline("Test pour \"24\" :");
	ft_print_rev("24")

let x = main()