(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex00.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/05 13:48:10 by lflandri          #+#    #+#             *)
(*   Updated: 2025/11/26 16:43:25 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Set
include List
include String

let comparestr s1 s2 = 
  if s1 = s2 then 0
  else (if s1 < s2 then -1 else 1)

module Newstring =
  struct
    type t = string
    let compare (s1) (s2) = comparestr s1  s2
  end

module StringSet = Set.Make(Newstring)

let () =
let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
StringSet.iter print_endline set;
print_endline (StringSet.fold ( ^ ) set "")