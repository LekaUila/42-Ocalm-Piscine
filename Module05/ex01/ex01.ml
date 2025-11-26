(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/08/29 11:30:36 by lflandri          #+#    #+#             *)
(*   Updated: 2025/11/26 16:39:48 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Hashtbl
include String

module Newhash =
  struct
    type t = string
    let equal t1 t2 = (t1 = t2)
    let hash t = Hashtbl.hash t
  end

module StringHashtbl = Hashtbl.Make(Newhash)

let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht