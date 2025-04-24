(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   micronap.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 14:38:32 by lflandri          #+#    #+#             *)
(*   Updated: 2025/04/24 15:16:25 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* compilation with -I +unix unix.cmxa *)

include Sys
include Array
(* module Unix = UnixLabels *)




let my_sleep () = Unix.sleep 1






let main () : unit =
  if Array.length Sys.argv > 1 then
    for i = 1 to int_of_string Sys.argv.(1)  do
      print_int i;
      my_sleep ()
    done








let x = main()