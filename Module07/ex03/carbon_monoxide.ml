(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Carbon_Monoxide.ml                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 15:50:22 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/15 16:14:27 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Carbon
include Oxygen
include Molecule

class carbon_monoxide =
  object 
    inherit molecule "Carbon Dioxide" (new oxygen :: (new carbon :: []))
  end