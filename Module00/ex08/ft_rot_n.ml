(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_rot_n.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/21 15:02:37 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 15:41:31 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_rot_n n str : string =
  match n with
  | 0 -> str
  | e ->
    ft_rot_n
      (n - 1) 
      (String.map
        (fun c ->
          if c >= 'a' && c <= 'z' then
            (char_of_int (((int_of_char c) - 96) mod 26 + 97))
          else if c >= 'A' && c <= 'Z' then
            (char_of_int (((int_of_char c) - 64) mod 26 + 65))
          else
            c)
        str)

