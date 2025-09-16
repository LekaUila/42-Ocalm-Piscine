(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   reaction.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/16 15:54:41 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/16 15:59:44 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Molecule

class virtual reaction (starlist: (molecule * int) list) (endlist: (molecule * int) list) =
  object (self)
    method virtual get_start: (molecule * int) list
    method virtual get_result: (molecule * int) list
    method virtual balance: reaction
    method virtual is_balanced: bool
  end