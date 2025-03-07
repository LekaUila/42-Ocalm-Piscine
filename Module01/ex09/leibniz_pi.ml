(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   leibniz_pi.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/07 13:39:43 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/07 14:40:40 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let rec leibniz_pi delta : int =
  if delta < 0. then
    -1
  else
    let pi_tend_value = (4. *. (atan 1.))
    in
    let correct_operation nb1 nb2 : float =
      (
        if nb1 > nb2 then
          nb1 -. nb2
        else
            nb2 -. nb1
      )
    in
    let rec ft_test_depth delta i sum : int =
    (
      let new_sum : float = 
        ((fun x -> (if x mod 2 = 1 then -1. else 1.) /. (2. *. (float_of_int x) +. 1.)) i) +. sum
      in
        if (correct_operation pi_tend_value  (4. *. new_sum)) < delta then
          i
        else
          (
          ft_test_depth delta (i + 1) new_sum
          )

    )
    in
    ft_test_depth delta 0 0.










let main () : unit =
  print_endline("Test pour   0.1:");
  print_int (leibniz_pi  0.1);
  print_newline ();
  print_endline("Test pour   0.01:");
  print_int (leibniz_pi  0.01);
  print_newline ();
  print_endline("Test pour   0.001:");
  print_int (leibniz_pi  0.001);
  print_newline ();
  print_endline("Test pour  -1. :");
  print_int (leibniz_pi (-1.));
  print_newline ()



let x = main()