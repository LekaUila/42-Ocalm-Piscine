(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   atom.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/09/10 14:39:36 by lflandri          #+#    #+#             *)
(*   Updated: 2025/09/16 14:04:47 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


class virtual atom (n: string) (s: string) (a_nb: int) =
  object (self)
  method name = n
  method symbol = s
  method atomic_number = a_nb
  method to_string = "class atom : (name=" ^ self#name ^ ", symbol=" ^ self#symbol ^ ", atomic_number=" ^ (string_of_int self#atomic_number) ^  ")"
  method equals (other: atom) = (0 == String.compare self#name other#name && 0 == String.compare self#symbol other#symbol && self#atomic_number == other#atomic_number)   
  end