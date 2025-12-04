(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   app.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/12/04 11:23:21 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/12/04 14:29:24 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


module App =
  struct
    type project = string * string * int
    let zero = "", "", 0
    let combine (p1:project) (p2:project) : project =
      let type1, status1, grade1 = p1 in
      let type2, status2, grade2 = p2 in
      let average = (grade1 + grade2) / 2 in
      type1 ^ type2, (if average >= 80 then "succeed" else "failed"), average 
    let fail (p:project) : project = 
      let typ, status, grade = p in
      typ, "failed", 0
    let success (p:project) : project = 
      let typ, status, grade = p in
      (typ, "succeed", 80)
  end

