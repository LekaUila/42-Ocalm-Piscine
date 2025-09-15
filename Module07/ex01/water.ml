(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   water.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 15:49:37 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/15 16:13:01 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Hydrogen
include Oxygen
include Molecule

class water  =
  object 
    inherit molecule "Water" (new hydrogen :: (new oxygen :: (new hydrogen :: [])))
  end