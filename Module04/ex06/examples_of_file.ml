(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   examples_of_file.ml                                :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/06/05 11:55:03 by lflandri          #+#    #+#             *)
(*   Updated: 2025/11/25 15:39:45 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)



(*==================================== REF ===================================*)
type 'a ft_ref = {mutable content: 'a}

let return l : 'a ft_ref  = 
  {content= l}

let set (l:'a ft_ref)(a:'a) : unit =
  l.content <- a

let get l : 'a =
  l.content

(*==================================== CODE ==================================*)

let rec getNewNumber line a nb len : int =
  if String.get line (nb + len) = ',' then
  (
    set a (Array.append (get a) (Array.make 1 (Float.of_string (String.sub line nb len))));
    (nb + len)
  )
  else
    getNewNumber line a nb (len + 1)

let rec createArrayFromLine_intern line a nb : int =
  if String.get line nb <= 'z' && String.get line nb >= 'a' then 
    nb
  else
    let newNb = getNewNumber line a nb 0 in
      createArrayFromLine_intern line a (newNb + 1)

let createArrayFromLine line : (float array * string) =
  let a = return (Array.make 0 1.) in
    let stringpos = createArrayFromLine_intern line a 0 in
      ((get a), String.sub line stringpos ((String.length line) - stringpos))

let rec examples_of_file_intern ic : (float array * string) list =
  try
      let line = input_line ic in
       (createArrayFromLine line) :: (examples_of_file_intern ic)
  with e ->
    close_in_noerr ic;
    if e = End_of_file then
      []
    else
      raise e

let examples_of_file ic : (float array * string) list = 
  let ic = open_in Sys.argv.(1) in
  
  examples_of_file_intern ic

(*=================================== PRINT ==================================*)

let rec printArrayFloat a ind : unit =
  if ind < Array.length a then
  (
    print_string (Float.to_string (Array.get a ind));
    print_string ", ";
    printArrayFloat a (ind + 1)
  )

let printContentElt elt : unit =
  match elt with
  | a , s ->
    print_char '[';
    printArrayFloat a 0;
    print_string "] \"";
    print_string s;
    print_char '"';
    print_char '\n'

let rec printList l : unit =
  match l with
  | [] -> ()
  | hd :: tl ->
    printContentElt hd;
    printList tl
    


(*=================================== MAIN ===================================*)


let main () : unit =
  if Array.length Sys.argv > 1 then
    let ic = open_in Sys.argv.(1) in
      printList (examples_of_file ic)

        



  (* done *)



let x = main ()