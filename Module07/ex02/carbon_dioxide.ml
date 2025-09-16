(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   carbon_dioxide.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 15:50:26 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/15 16:17:59 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Carbon
include Oxygen
include Molecule

class carbon_dioxide  =
  object 
    inherit molecule "Carbon Dioxide" (new carbon :: (new oxygen :: (new carbon :: [])))
  end