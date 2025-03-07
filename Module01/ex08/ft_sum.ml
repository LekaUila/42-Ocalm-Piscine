(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_sum.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 13:22:49 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/07 13:33:15 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let rec ft_sum f i_l i_u : float =
  if i_l > i_u then
    nan
  else if i_l = i_u then
    f i_l 
  else
    (ft_sum f (i_l + 1) i_u) +. (f i_l)






let main () : unit =
  print_endline("Test pour   (fun i -> float_of_int (i * i)) 1 10:");
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_newline ();
  print_endline("Test pour  (fun x -> x * 2) 2 4:");
  print_float (ft_sum (fun x -> float_of_int(x * 2)) 2 4);
  print_newline ();
  print_endline("Test pour  (fun x -> x * 2) 2 -1:");
  print_float (ft_sum  (fun x -> float_of_int(x * 2)) 2 (-1));
  print_newline ()



let x = main()
