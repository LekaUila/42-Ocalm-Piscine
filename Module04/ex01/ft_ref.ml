(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 15:27:34 by lflandri          #+#    #+#             *)
(*   Updated: 2025/05/27 13:11:04 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable content: 'a}


let return l : 'a ft_ref  = 
  {content= l}

let printcontent l : unit = 
    print_int l.content;
    print_char '\n'

let set (l:'a ft_ref)(a:'a) : unit =
  l.content <- a

let get l : 'a =
  l.content

let bind (l:'a ft_ref)(func:('a -> 'b ft_ref)) : 'b ft_ref =
  func l.content

let testbindfunc (a:'a) : 'b ft_ref =
  {content= string_of_int a}

let main () : unit =
  let x = return 5 in
  let y = x in
  printcontent x;
  printcontent y;
  set x 3;
  printcontent x;
  printcontent y;
  let t = bind x testbindfunc in
  print_string (get t);
  print_char '\n';
  set t "42 is life !";
  print_string (get t);
  print_char '\n'


let x = main ()

