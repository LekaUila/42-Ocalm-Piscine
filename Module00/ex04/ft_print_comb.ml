(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 20:30:15 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 14:03:58 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_print_comb_of_third_number nb1 nb2 nb3 : unit =
  match nb3 with
  | 9 ->
    print_int(nb1);
    print_int(nb2);
    print_int(nb3);
    if nb1 = 7 then
      print_string("\n")
    else
      print_string(", ");
  | e ->
    print_int(nb1);
    print_int(nb2);
    print_int(nb3);
    print_string(", ");
    ft_print_comb_of_third_number nb1 nb2 (nb3 + 1)

let rec ft_print_comb_of_second_number nb1 nb2 nb3 : unit =
  match nb2 with
  | 9 -> ()
  | e ->
    ft_print_comb_of_third_number nb1 nb2 nb3;
    ft_print_comb_of_second_number nb1 (nb2 + 1) (nb3 + 1)

let rec ft_print_comb_of_first_number nb1 nb2 nb3 : unit =
match nb1 with
| 8 -> ()
| e ->
  ft_print_comb_of_second_number nb1 nb2 nb3;
  ft_print_comb_of_first_number (nb1 + 1) (nb2 + 1) (nb3 + 1)

let ft_print_comb () : unit = ft_print_comb_of_first_number 0 1 2

