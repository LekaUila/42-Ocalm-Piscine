(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   mainFunctions.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 19:28:17 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 15:41:45 by lflandri         ###   ########.fr       *)
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



(* EX06 main test *)

let is_digit c = c >= '0' && c <= '9'
let is_not_digit c = c < '0' || c > '9'


let main () : unit =
	print_endline "Test pour \"Hello world !\" :";
	print_endline "  with is_digit :";
	if ft_string_all is_digit "Hello world !" then
    print_endline "    true"
  else
    print_endline "    false";
	print_endline "  with is_not_digit :";
  if ft_string_all is_not_digit "Hello world !" then
    print_endline "    true"
  else
    (print_endline "    false");
  print_newline ();
  print_endline "Test pour \"\" :";
  print_endline "  with is_digit :";
  if ft_string_all is_digit "" then
    print_endline "    true"
  else
    print_endline "    false";
  print_endline "  with is_not_digit :";
  if ft_string_all is_not_digit "" then
    print_endline "    true"
  else
    (print_endline "    false");
    print_newline ();
  print_endline "Test pour \"42\" :";
  print_endline "  with is_digit :";
  if ft_string_all is_digit "42" then
    print_endline "    true"
  else
    print_endline "    false";
  print_endline "  with is_not_digit :";
  if ft_string_all is_not_digit "42" then
    print_endline "    true"
  else
    (print_endline "    false")

let x = main()



  (* EX07 main test *)

let main () : unit =
	print_endline "Test pour \"radar\" :";
	if ft_ispalindrome "radar" then
		print_endline "    true"
	else
		print_endline "    false";
	
	print_endline "Test pour \"madam\" :";
	if ft_ispalindrome "madam" then
		print_endline "    true"
	else
		print_endline "    false";

	print_endline "Test pour \"car\" :";
	if ft_ispalindrome "car" then
		print_endline "    true"
	else
		print_endline "    false";

	print_endline "Test pour \"\" :";
	if ft_ispalindrome "" then
		print_endline "    true"
	else
		print_endline "    false";

	print_endline "Test pour \"lool\" :";
	if ft_ispalindrome "lool" then
		print_endline "    true"
	else
		print_endline "    false";

	print_endline "Test pour \"loul\" :";
	if ft_ispalindrome "loul" then
		print_endline "    true"
	else
		print_endline "    false"

let x = main()




(* EX08 main test *)

let main () : unit =
	print_endline "Test pour 1 \"abcdefghijklmnopqrstuvwxyz\" :";
	print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" );
	print_endline "Test pour 13 \"abcdefghijklmnopqrstuvwxyz\" :";
	print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" );
	print_endline "Test pour 42 \"0123456789\" :";
	print_endline (ft_rot_n 42 "0123456789" );
	print_endline "Test pour 2 \"0123456789\" :";
	print_endline (ft_rot_n 2 "0123456789" );
	print_endline "Test pour 0 \"Dammed!\" :";
	print_endline (ft_rot_n 0 "Dammed !" );
	print_endline "Test pour 42 \"\" :";
	print_endline (ft_rot_n 42 "" );
	print_endline "Test pour 1 \"NBzlk qnbjr !\" :";
	print_endline (ft_rot_n 1 "NBzlk qnbjr !") 

let x = main()