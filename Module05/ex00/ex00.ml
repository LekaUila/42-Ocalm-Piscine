(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex00.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/05 13:48:10 by lflandri          #+#    #+#             *)
(*   Updated: 2025/06/11 16:44:52 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Set
include List
include String

module Newstring =
  struct
    type t = string
    let compare (s1) (s2) = String.compare s1 s2
  end

module StringSet = Set.Make(Newstring)

let () =
let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
StringSet.iter print_endline set;
print_endline (StringSet.fold ( ^ ) set "")