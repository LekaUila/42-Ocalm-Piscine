(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/12/04 11:21:58 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/12/04 13:18:38 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Watchtower


let print_result (nb:int) =
  print_int nb;
  print_char '\n'

let main () =
  print_result Watchtower.zero;
  print_result (Watchtower.add 4 5);
  print_result (Watchtower.add Watchtower.zero 9);
  print_result (Watchtower.add 10 13);
  print_result (Watchtower.sub 5 4);
  print_result (Watchtower.sub Watchtower.zero 9);
  print_result (Watchtower.sub 13 19)




let () = main ()