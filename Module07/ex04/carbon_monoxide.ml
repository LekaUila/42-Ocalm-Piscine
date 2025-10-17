(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   carbon_monoxide.ml                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 15:50:22 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/10/17 16:24:59 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Carbon
include Oxygen
include Molecule

class carbon_monoxide =
  object 
    inherit molecule "Carbon Monoxide" (new oxygen :: (new carbon :: []))
  end