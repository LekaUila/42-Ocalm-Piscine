(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/04 13:01:01 by lflandri          #+#    #+#             *)
(*   Updated: 2025/06/04 14:54:02 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Array
include Random

type 'a ft_ref = {mutable content: 'a}

let return l : 'a ft_ref  = 
  {content= l}

let set (l:'a ft_ref)(a:'a) : unit =
  l.content <- a

let get l : 'a =
  l.content

let main () : unit =
  if Array.length Sys.argv > 1 then
    let a = return  (Array.make 0 "") in 

    let ic = open_in Sys.argv.(1) in
    try
      while true do
        let line = input_line ic in
        set a (Array.append (get a) (Array.make 1 line));
      done
    with e ->
      close_in_noerr ic;
      if Array.length (get a) = 0 then
        raise e
      else
        Random.self_init ();
        print_string (Array.get (get a) (Random.int (Array.length (get a))));
        print_char '\n'

        



  (* done *)



let x = main ()