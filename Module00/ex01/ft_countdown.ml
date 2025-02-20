(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_countdown.ml                                    :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <lflandri@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/02/20 15:23:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/02/20 18:09:04 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let ft_countdown (nb : int) : unit =
	for value = 0 to nb do
		print_int (nb - value);
		print_char '\n'
	done
