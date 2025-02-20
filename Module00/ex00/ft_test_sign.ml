(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_test_sign.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <lflandri@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 15:23:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/20 17:35:15 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let ft_test_sign (nb : int) : unit = 
	if nb >= 0 then
		print_endline("positive")
	else
		print_endline("negative")
