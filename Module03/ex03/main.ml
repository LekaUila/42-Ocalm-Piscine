(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/21 13:13:19 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/25 16:51:32 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Deck

let main () =
  Random.self_init ();
  let d = Deck.newDeck in
  print_endline (Deck.toStringListVerbose d);
  print_endline (Deck.toStringList d);
  let c,d = Deck.drawCard d in
    print_endline (Deck.Card.toString c);
    print_endline (Deck.toStringList d)


let () = main ()