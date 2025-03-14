(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_comb2.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <lflandri@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/21 15:51:44 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/14 16:37:44 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_print_number_correct nb : unit =
  if nb < 10 then
    (print_int(0);print_int(nb))
  else
    print_int(nb)

let rec ft_print_comb_of_second_number nb1 nb2 : unit =
  match nb2 with
  | 99 ->
    ft_print_number_correct(nb1);
    print_char(' ');
    ft_print_number_correct(nb2);
    if nb1 = 98 then
      print_string("\n")
    else
      print_string(", ");
  | e ->
    ft_print_number_correct(nb1);
    print_char(' ');
    ft_print_number_correct(nb2);
    print_string(", ");
    ft_print_comb_of_second_number nb1 (nb2 + 1)

let rec ft_print_comb_of_first_number nb1 nb2 : unit =
match nb1 with
| 99 -> ()
| e ->
  ft_print_comb_of_second_number nb1 nb2;
  ft_print_comb_of_first_number (nb1 + 1) (nb2 + 1)

let ft_print_comb2 () : unit = ft_print_comb_of_first_number 0 1


