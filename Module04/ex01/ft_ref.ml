(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/04/24 15:27:34 by lflandri          #+#    #+#             *)
(*   Updated: 2025/04/24 16:55:46 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {content: 'a}


let creacontent l : 'a ft_ref  = 
  {content= l}

let printcontent l : unit = 
  let {content} = l in
    print_int content

let setcontent l a : unit =
  let {content} = l in
    let content = a in ()

let x = creacontent 5;;
let y = x;;
printcontent x;;
printcontent y;;
let x = setcontent x, 3;;
printcontent x;;
printcontent y;;


