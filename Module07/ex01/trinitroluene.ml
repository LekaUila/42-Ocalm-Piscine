(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   trinitroluene.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 16:23:07 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/15 16:30:56 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Carbon
include Oxygen
include Hydrogen
include Nitrogen
include Molecule

class trinitroluene  =
  object 
    inherit molecule "Trinitroluene"
    (new hydrogen :: (new oxygen :: (new hydrogen :: (new nitrogen ::
    (new hydrogen :: (new oxygen :: (new hydrogen :: (new nitrogen ::
    (new carbon :: (new carbon :: (new carbon :: (new carbon :: (new carbon ::
    (new oxygen :: (new oxygen :: (new oxygen :: (new oxygen :: (new hydrogen ::
    (new carbon :: (new carbon ::  (new nitrogen :: [])))))))))))))))))))))
  end