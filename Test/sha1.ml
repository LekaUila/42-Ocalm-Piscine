(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   sha1.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/08/29 12:16:32 by lflandri          #+#    #+#             *)
(*   Updated: 2025/11/27 17:48:58 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* https://www.youtube.com/watch?v=kmHojGMUn0Q *)
(* continue at step 11 *)

(*
Step 1 : convert text into array of ascii code
step 2 : convert to binary
step 3 : make it 8 bits long by staking 0 in front
step 4 : join all binary code et add a one at end
step 5 : fill the end with zeo until lenght = 512 mod 448
step 6 : take binary join lenth from step 3, convert lenth in binary
step 7 : add zero in front of lenth converted in binary until lenth of binary = 64
step 8 : append step 7 to end of step 5
step 9 : break message into array of "chunks" of 512 char
step 10 : break each chunk into a subarray of 16  32-bit list
step 11 : extend each subarray by looping chunks and making bitwise operation on 32-bit list
step 12 : initialise const
step 13 : use bitwise and variables assignement to do some shit
step 14 : convert variables into hexadecimal
step 15 : join them
*)

let charToBinaryString c : (int list) = 
  (*convert a char to a int list*)
  match c with
  | ' '  -> 0 :: (0 :: (1 :: (0 :: (0 :: (0 :: (0 :: [0]))))))
  | '!'  -> 0 :: (0 :: (1 :: (0 :: (0 :: (0 :: (0 :: [1]))))))
  | '"'  -> 0 :: (0 :: (1 :: (0 :: (0 :: (0 :: (1 :: [0]))))))
  | '#'  -> 0 :: (0 :: (1 :: (0 :: (0 :: (0 :: (1 :: [1]))))))
  | '$'  -> 0 :: (0 :: (1 :: (0 :: (0 :: (1 :: (0 :: [0]))))))
  | '%'  -> 0 :: (0 :: (1 :: (0 :: (0 :: (1 :: (0 :: [1]))))))
  | '&'  -> 0 :: (0 :: (1 :: (0 :: (0 :: (1 :: (1 :: [0]))))))
  | '\'' -> 0 :: (0 :: (1 :: (0 :: (0 :: (1 :: (1 :: [1]))))))
  | '('  -> 0 :: (0 :: (1 :: (0 :: (1 :: (0 :: (0 :: [0]))))))
  | ')'  -> 0 :: (0 :: (1 :: (0 :: (1 :: (0 :: (0 :: [1]))))))
  | '*'  -> 0 :: (0 :: (1 :: (0 :: (1 :: (0 :: (1 :: [0]))))))
  | '+'  -> 0 :: (0 :: (1 :: (0 :: (1 :: (0 :: (1 :: [1]))))))
  | ','  -> 0 :: (0 :: (1 :: (0 :: (1 :: (1 :: (0 :: [0]))))))
  | '-'  -> 0 :: (0 :: (1 :: (0 :: (1 :: (1 :: (0 :: [1]))))))
  | '.'  -> 0 :: (0 :: (1 :: (0 :: (1 :: (1 :: (1 :: [0]))))))
  | '/'  -> 0 :: (0 :: (1 :: (0 :: (1 :: (1 :: (1 :: [1]))))))
  | '0'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (0 :: (0 :: [0]))))))
  | '1'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (0 :: (0 :: [1]))))))
  | '2'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (0 :: (1 :: [0]))))))
  | '3'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (0 :: (1 :: [1]))))))
  | '4'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (1 :: (0 :: [0]))))))
  | '5'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (1 :: (0 :: [1]))))))
  | '6'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (1 :: (1 :: [0]))))))
  | '7'  -> 0 :: (0 :: (1 :: (1 :: (0 :: (1 :: (1 :: [1]))))))
  | '8'  -> 0 :: (0 :: (1 :: (1 :: (1 :: (0 :: (0 :: [0]))))))
  | '9'  -> 0 :: (0 :: (1 :: (1 :: (1 :: (0 :: (0 :: [1]))))))
  | ':'  -> 0 :: (0 :: (1 :: (1 :: (1 :: (0 :: (1 :: [0]))))))
  | ';'  -> 0 :: (0 :: (1 :: (1 :: (1 :: (0 :: (1 :: [1]))))))
  | '<'  -> 0 :: (0 :: (1 :: (1 :: (1 :: (1 :: (0 :: [0]))))))
  | '='  -> 0 :: (0 :: (1 :: (1 :: (1 :: (1 :: (0 :: [1]))))))
  | '>'  -> 0 :: (0 :: (1 :: (1 :: (1 :: (1 :: (1 :: [0]))))))
  | '?'  -> 0 :: (0 :: (1 :: (1 :: (1 :: (1 :: (1 :: [1]))))))
  | '@'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (0 :: (0 :: [0]))))))
  | 'A'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (0 :: (0 :: [1]))))))
  | 'B'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (0 :: (1 :: [0]))))))
  | 'C'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (0 :: (1 :: [1]))))))
  | 'D'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (1 :: (0 :: [0]))))))
  | 'E'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (1 :: (0 :: [1]))))))
  | 'F'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (1 :: (1 :: [0]))))))
  | 'G'  -> 0 :: (1 :: (0 :: (0 :: (0 :: (1 :: (1 :: [1]))))))
  | 'H'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (0 :: (0 :: [0]))))))
  | 'I'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (0 :: (0 :: [1]))))))
  | 'J'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (0 :: (1 :: [0]))))))
  | 'K'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (0 :: (1 :: [1]))))))
  | 'L'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (1 :: (0 :: [0]))))))
  | 'M'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (1 :: (0 :: [1]))))))
  | 'N'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (1 :: (1 :: [0]))))))
  | 'O'  -> 0 :: (1 :: (0 :: (0 :: (1 :: (1 :: (1 :: [1]))))))
  | 'P'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (0 :: (0 :: [0]))))))
  | 'Q'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (0 :: (0 :: [1]))))))
  | 'R'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (0 :: (1 :: [0]))))))
  | 'S'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (0 :: (1 :: [1]))))))
  | 'T'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (1 :: (0 :: [0]))))))
  | 'U'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (1 :: (0 :: [1]))))))
  | 'V'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (1 :: (1 :: [0]))))))
  | 'W'  -> 0 :: (1 :: (0 :: (1 :: (0 :: (1 :: (1 :: [1]))))))
  | 'X'  -> 0 :: (1 :: (0 :: (1 :: (1 :: (0 :: (0 :: [0]))))))
  | 'Y'  -> 0 :: (1 :: (0 :: (1 :: (1 :: (0 :: (0 :: [1]))))))
  | 'Z'  -> 0 :: (1 :: (0 :: (1 :: (1 :: (0 :: (1 :: [0]))))))
  | '['  -> 0 :: (1 :: (0 :: (1 :: (1 :: (0 :: (1 :: [1]))))))
  | ']'  -> 0 :: (1 :: (0 :: (1 :: (1 :: (1 :: (0 :: [0]))))))
  | '\\' -> 0 :: (1 :: (0 :: (1 :: (1 :: (1 :: (0 :: [1]))))))
  | '^'  -> 0 :: (1 :: (0 :: (1 :: (1 :: (1 :: (1 :: [0]))))))
  | '_'  -> 0 :: (1 :: (0 :: (1 :: (1 :: (1 :: (1 :: [1]))))))
  | '`'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (0 :: (0 :: [0]))))))
  | 'a'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (0 :: (0 :: [1]))))))
  | 'b'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (0 :: (1 :: [0]))))))
  | 'c'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (0 :: (1 :: [1]))))))
  | 'd'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (1 :: (0 :: [0]))))))
  | 'e'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (1 :: (0 :: [1]))))))
  | 'f'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (1 :: (1 :: [0]))))))
  | 'g'  -> 0 :: (1 :: (1 :: (0 :: (0 :: (1 :: (1 :: [1]))))))
  | 'h'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (0 :: (0 :: [0]))))))
  | 'i'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (0 :: (0 :: [1]))))))
  | 'j'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (0 :: (1 :: [0]))))))
  | 'k'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (0 :: (1 :: [1]))))))
  | 'l'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (1 :: (0 :: [0]))))))
  | 'm'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (1 :: (0 :: [1]))))))
  | 'n'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (1 :: (1 :: [0]))))))
  | 'o'  -> 0 :: (1 :: (1 :: (0 :: (1 :: (1 :: (1 :: [1]))))))
  | 'p'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (0 :: (0 :: [0]))))))
  | 'q'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (0 :: (0 :: [1]))))))
  | 'r'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (0 :: (1 :: [0]))))))
  | 's'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (0 :: (1 :: [1]))))))
  | 't'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (1 :: (0 :: [0]))))))
  | 'u'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (1 :: (0 :: [1]))))))
  | 'v'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (1 :: (1 :: [0]))))))
  | 'w'  -> 0 :: (1 :: (1 :: (1 :: (0 :: (1 :: (1 :: [1]))))))
  | 'x'  -> 0 :: (1 :: (1 :: (1 :: (1 :: (0 :: (0 :: [0]))))))
  | 'y'  -> 0 :: (1 :: (1 :: (1 :: (1 :: (0 :: (0 :: [1]))))))
  | 'z'  -> 0 :: (1 :: (1 :: (1 :: (1 :: (0 :: (1 :: [0]))))))
  | '{'  -> 0 :: (1 :: (1 :: (1 :: (1 :: (0 :: (1 :: [1]))))))
  | '|'  -> 0 :: (1 :: (1 :: (1 :: (1 :: (1 :: (0 :: [0]))))))
  | '}'  -> 0 :: (1 :: (1 :: (1 :: (1 :: (1 :: (0 :: [1]))))))
  | '~'  -> 0 :: (1 :: (1 :: (1 :: (1 :: (1 :: (1 :: [0]))))))
  | e    -> 0 :: (0 :: (0 :: (0 :: (0 :: (0 :: (0 :: [0]))))))

let rec concatList l1 l2 : 'a list =
  match l1 with
  | [] -> l2
  | hd :: tl -> hd :: concatList tl l2

let rec stringToList s nb : (int list list) =
  (*convert a string to a list of int list which represent a char*)
  if (String.length s) <= nb then
    []
  else
    (charToBinaryString (String.get s nb)) :: (stringToList s (nb + 1))

let rec concatListList l : 'a list =
  match l with
  | [] -> 1 :: []
  | hd :: tl -> concatList hd (concatListList tl)

let rec findNbZeroToAdd s nb = 
  if ((String.length s) * 8 + 1 + nb) mod 512 != 448 then
    findNbZeroToAdd s (nb + 1)
  else
    nb

let rec lenList l =
  match l with
  | [] -> 0
  | hd :: tl -> 1 + (lenList tl)

let rec createZeroList nb =
  match nb with
  | 0 -> []
  | e -> 0 :: createZeroList (nb - 1)

let rec intToBinary i l =
  if i == 0 then
    l
  else  
    match (i mod 2) with
    | 0 ->  intToBinary (i / 2) (0 :: l)
    | 1 ->  intToBinary (i / 2) (1 :: l)
    | e -> [] (* What's the fuck bro ?*)

let rec splitIntoBlockOf nb l =
  let rec getBlockOf nb count l =
    match l with
    | [] -> []
    | hd :: tl ->
      if nb == count then
        []
      else
        hd :: getBlockOf nb (count + 1) tl
  in
    let rec goNbFarAway nb l =
      match l with
      | [] -> []
      | hd :: tl ->
        if nb == 0 then
          tl
        else
          goNbFarAway (nb - 1)  tl
    in
      match goNbFarAway (nb - 1) l with
      | [] -> [getBlockOf nb 0 l]
      | hd :: tl -> (getBlockOf nb 0 l) :: (splitIntoBlockOf nb (goNbFarAway (nb - 1) l))

let rec splitBlockIntoBlockOf nb l =
  match l with 
  | [] -> []
  | hd :: tl -> (splitIntoBlockOf nb hd) :: (splitBlockIntoBlockOf nb tl)


let rec xOR l1 l2 =
  match l1 with
  | [] -> []
  | hd1 :: tl1 ->
    match l2 with
    | [] -> []
    | hd2 :: tl2 -> ((hd2 + hd1) mod 2) :: (xOR tl1 tl2)

let rec printList (l:'a list) =
  match l with
  | [] -> ()
  | hd :: tl ->
    print_int hd;
    printList tl

let rec printListList (l:'a list) =
  match l with
  | [] -> ()
  | hd :: tl ->
    printList hd;
    print_char '\n';
    printListList tl

let rec printListListList (l:'a list) =
  match l with
  | [] -> ()
  | hd :: tl ->
    printListList hd;
    print_char '\n';print_char '\n';
    printListListList tl

let parseStringForOperation (str : string) = 
  (*call all necessary function to do step 1 to 10 and have
  a converted list of chunk to do proper hash*)
  splitBlockIntoBlockOf 32 (*step 10*)
              (splitIntoBlockOf 512 (*step 9*)
                    (concatList (*step 8*)
                        (concatList (*step 5*)
                            (concatListList (stringToList str 0)) (*step  1, 2, 3, 4*)
                            (createZeroList (findNbZeroToAdd str 0))) (*step 5*)
                        (concatList (*step 7*)
                            (createZeroList (64 - (lenList (intToBinary (String.length str) [])))) (*step 7*)
                            (intToBinary (String.length str) [])))) (*step 6*)


let  x = printListListList (parseStringForOperation "42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test 42 test");      
        print_char '\n';print_char '\n'

