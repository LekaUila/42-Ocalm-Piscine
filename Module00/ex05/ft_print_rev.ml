(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_print_rev.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <lflandri@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/21 12:13:32 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/14 16:29:19 by lflandri         ###   ########.fr       *)
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
