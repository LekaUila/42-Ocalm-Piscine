(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   carbon_dioxide.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 15:50:26 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/10/17 14:40:37 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Carbon
include Oxygen
include Molecule

class carbon_dioxide  =
  object 
    inherit molecule "Carbon Dioxide" (new carbon :: (new oxygen :: (new oxygen :: [])))
  end