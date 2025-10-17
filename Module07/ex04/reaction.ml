(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/16 15:54:41 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/10/17 16:20:58 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Molecule

exception BalancedError of string
exception BadReaction of string

class virtual reaction (starlist: (molecule * int) list) (endlist: (molecule * int) list) =
  object (self)
    method virtual get_start: (molecule * int) list
    method virtual get_result: (molecule * int) list
    method virtual balance: reaction
    method virtual is_balanced: bool
  end