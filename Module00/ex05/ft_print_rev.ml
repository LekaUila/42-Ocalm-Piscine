(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/21 12:13:32 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 14:05:46 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)



let rec ft_print_rev_from s i : unit =
  match i with
  | 0 ->
    print_char(String.get s i);
    print_char('\n')
  | e ->
    print_char(String.get s i);
    ft_print_rev_from s (i - 1)

let ft_print_rev s : unit =
  match s with
  | "" -> print_char '\n'
  | e -> ft_print_rev_from s (String.length(s) - 1)


let main () : unit =
  print_endline("Test pour \"Hello world !\" :");
  ft_print_rev "Hello world !";
  print_endline("Test pour \"\" :");
  ft_print_rev "";
  print_endline("Test pour \"24\" :");
  ft_print_rev "24"

let x = main()