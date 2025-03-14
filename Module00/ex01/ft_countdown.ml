(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <lflandri@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 15:23:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/14 16:32:27 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let rec ft_countdown nb : unit =
	if nb <= 0 then
	(
		print_char ('0');
		print_char '\n'
	)
	else
	(
		print_int (nb);
		print_char '\n';
		ft_countdown (nb - 1)
	)

	
	