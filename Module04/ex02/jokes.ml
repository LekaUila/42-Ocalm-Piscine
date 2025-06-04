(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/04 13:01:01 by lflandri          #+#    #+#             *)
(*   Updated: 2025/06/04 13:22:06 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Array
include Random


let createJokeArray () : string array =
  let a = Array.make 5 "" in
  Array.set a 0 "Que fait un démon pour expliquer quelque chose ? une demon-stration";
  Array.set a 1 "What does a baby computer call its father? Data.";
  Array.set a 2 "Which knight invented King Arthur's Round Table? Sir Cumference.";
  Array.set a 3 "What do you call a fish with no eye? Fsh";
  Array.set a 4 "What do you call a boomerang that doesn't come back? A stick!";
  a



let main () : unit =
  Random.self_init ();
  print_string (Array.get (createJokeArray ()) (Random.int 5 ));
  print_char '\n'



let x = main ()