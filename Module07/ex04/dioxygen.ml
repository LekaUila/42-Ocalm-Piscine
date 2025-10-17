(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   dioxygen.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 16:32:46 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/15 16:33:56 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Oxygen
include Molecule

class dioxygen =
  object 
    inherit molecule "Dioxygen" (new oxygen :: (new oxygen :: []))
  end