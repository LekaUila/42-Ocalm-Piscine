(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_string_all.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/21 14:11:11 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 14:32:09 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_string_all_from func s i : bool =
  match i with
  | 0 -> func (String.get s i)
  | e -> func (String.get s i) && ft_string_all_from func s (i - 1)

let ft_string_all func s : bool = 
  match s with
  | "" -> true
  | e -> ft_string_all_from func s (String.length(s) - 1)
