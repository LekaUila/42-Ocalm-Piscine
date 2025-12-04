(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   watchtower.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/12/04 11:23:21 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/12/04 13:17:42 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


module Watchtower =
  struct
    type hour = int
    let zero = 0
    let add (h1:hour) (h2:hour) = (h1 + h2) mod 12
    let sub (h1:hour) (h2:hour) = if (h1 < h2) then
                      (h1 + (((h2 - h1) / 12 + 1) * 12) - h2) mod 12
                    else
                      (h1 - h2) mod 12  
  end



