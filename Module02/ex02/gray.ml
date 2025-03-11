(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gray.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/11 14:34:11 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/11 16:53:28 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


let print_list_char_with_ret list =
  let rec print_list_char_intern list =
    match list with
    | [] -> ()
    | hd :: tl ->
      print_list_char_intern tl;
      print_char hd
  in
    print_list_char_intern list;
    print_char ' ';
    list

let print_list_char list =
  let rec print_list_char_intern list =
    match list with
    | [] -> ()
    | hd :: tl ->
      print_list_char_intern tl;
      print_char hd
  in
    print_list_char_intern list;
    print_char ' '
    

let gray  n =
  if n = 0 then
    ()
  else
    let rec create_list_of_0_char n =
      match n with
      | 1 -> ['0']
      | n -> '0' :: create_list_of_0_char (n - 1)
    in
      let base = create_list_of_0_char n
      in
        let rec inverse_elt_n_of_list n list =
          match list with
          | [] -> []
          | hd :: tl ->
            if n = 0 then
            (
              if hd = '1' then
                '0' :: tl
              else
                '1' :: tl
            )
            else
              hd :: inverse_elt_n_of_list (n - 1) tl
        in
          let rec gray_intern n list =
            match n with
            | 1 -> print_list_char_with_ret (inverse_elt_n_of_list 0 list)
            | x ->
              gray_intern (n - 1)
                (print_list_char_with_ret
                    (inverse_elt_n_of_list (n - 1)
                      (gray_intern (n - 1) list)
                    )
                )
            in
              let rec gray_intern_last n list =
                match n with
                | 1 -> print_list_char (inverse_elt_n_of_list 0 list)
                | x ->
                  gray_intern_last (n - 1)
                    (print_list_char_with_ret
                        (inverse_elt_n_of_list (n - 1)
                          (gray_intern (n - 1) list)
                        )
                    )
                in
                  print_list_char base;
                  gray_intern_last n base;
                  print_char '\n'




let main () : unit =
  gray 0;
  gray 1;
  gray 2;
  gray 3;
  gray 4;
  gray 5



let x = main()