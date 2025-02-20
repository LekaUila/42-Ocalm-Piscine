(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 15:23:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/20 19:45:19 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign (nb : int) : unit = 
	if nb >= 0 then
		print_endline("positive")
	else
		print_endline("negative")
