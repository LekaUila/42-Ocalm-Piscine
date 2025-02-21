(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ispalindrome.ml                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/21 14:36:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 15:01:20 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_ispalindrome_from s i : bool =
  if i = (String.length s) / 2 then
    (String.get s i) = String.get s ((String.length s) - i - 1)
  else
    (String.get s i) = String.get s ((String.length s) - i - 1) && ft_ispalindrome_from s (i + 1)

let ft_ispalindrome s : bool =
  match s with
  | "" -> true
  | e -> ft_ispalindrome_from s 0 
