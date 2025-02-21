(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 15:23:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/21 13:58:25 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let ft_countdown nb : unit =
	if nb < 0 then
	(
		print_char '0';
		print_char '\n'
	)
	else
	(
		for value = 0 to nb do
			print_int (nb - value);
			print_char '\n'
		done
	)
