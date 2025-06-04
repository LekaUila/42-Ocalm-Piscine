(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   eu_dist.ml                                         :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/04 15:22:04 by lflandri          #+#    #+#             *)
(*   Updated: 2025/06/04 16:59:53 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Float
include Array

let rec eu_dist_intern (array1:floatarray) (array2:floatarray) (nb:int) : float =
  if (Array.length array1) > (nb + 1) then
    Float.add (eu_dist_intern array1 array2 (nb + 1)) (Float.pow (Float.sub (Array.get array1 nb) (Array.get array2 nb)) 2.)
  else
    Float.pow ( Float.sub (Array.get array1 nb) (Array.get array2 nb)) 2.


let eu_dist (array1:floatarray) (array2:floatarray) : float =
  Float.sqrt (eu_dist_intern array1 array2 0)



let main () : unit =
  let arr1 = Array.make 5 2. in
  let arr2 = Array.make 5 1. in
  print_string (Float.to_string (eu_dist arr1 arr2));
  print_char '\n';
  Array.set arr2 4 8.;
  print_string (Float.to_string (eu_dist arr1 arr2));
  print_char '\n'



let x = main ()