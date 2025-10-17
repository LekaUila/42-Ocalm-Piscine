(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   alkane.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/15 17:16:25 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/09/16 14:36:21 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Atom
include Carbon
include Hydrogen
include Molecule

let getNameAlkane n =
  match n with
  | 0 -> "dihydrogen"
  | 1 -> "méthane"
  | 2 -> "éthane"
  | 3 -> "proane"
  | 4 -> "butane"
  | 5 -> "pentane"
  | 6 -> "hexane"
  | 7 -> "heptane"
  | 8 -> "octane"
  | 9 -> "nonane"
  | 10 -> "décane"
  | 11 -> "undéane"
  | 12 -> "dodécanane"
  | i -> "no-named alkane"

let createListAtomAlkane n =
  let rec addCarbon n =
    match n with
    | 0 -> []
    | e -> new carbon :: (addCarbon (n - 1))
  in
  let rec addHydrogen n nb = 
    match n with
    | 0 -> new hydrogen :: (new hydrogen :: (addCarbon nb))
    | e -> new hydrogen :: (new hydrogen :: (addHydrogen (n - 1) nb))
  in addHydrogen n n

class alkane nb  =
  object 
    inherit molecule (getNameAlkane nb) (createListAtomAlkane nb)
  end
