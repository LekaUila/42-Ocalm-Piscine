(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_alphabet.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 20:05:31 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/20 20:21:59 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_print_alphabet_from(c : char) : unit =
  match int_of_char(c) with
  | 123 -> print_char('\n')
  | e ->
    print_char(char_of_int(e));
    ft_print_alphabet_from(char_of_int(e + 1))

let ft_print_alphabet (unit) : unit = ft_print_alphabet_from('a')
  