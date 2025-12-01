(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/08/29 11:30:36 by lflandri          #+#    #+#             *)
(*   Updated: 2025/12/01 12:13:47 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Hashtbl
include String

(* https://www.youtube.com/watch?v=kmHojGMUn0Q *)

(*
Step 1 : convert text into array of ascii code
step 2 : convert to binary
step 3 : make it 8 bits long by staking 0 in front
step 4 : join all binary code et add a one at end
step 5 : fill the end with zeo until lenght = something
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
(*==================================================BINAY OPERATOR==============================================*)

let rec xOR l1 l2 : int list =
  match l1 with
  | [] -> []
  | hd1 :: tl1 ->
    match l2 with
    | [] -> []
    | hd2 :: tl2 -> ((hd2 + hd1) mod 2) :: (xOR tl1 tl2)

let rec _AND l1 l2 : int list =
  match l1 with
  | [] -> []
  | hd1 :: tl1 ->
    match l2 with
    | [] -> []
    | hd2 :: tl2 -> (hd2 * hd1) :: (_AND tl1 tl2)

let rec _OR l1 l2 : int list =
  match l1 with
  | [] -> []
  | hd1 :: tl1 ->
    match l2 with
    | [] -> []
    | hd2 :: tl2 -> ((hd2 * hd1) + ((hd2 + hd1) mod 2)) :: (_OR tl1 tl2)

let rec not l : int list =
    match l with
    | [] -> []
    | hd :: tl -> ((hd + 1) mod 2) :: not tl

let binaryAddition l1 l2 : int list =
  let rec reverseList arg ret : int list =
    match arg with
    | [] -> ret
    | hd :: tl -> reverseList tl (hd :: ret)
  in
  let rec binaryAdditionIntern l1 l2 toAdd : int list  =
    let endCase l1 l2 toAdd = 
      match l2 with
      | [] -> [toAdd]
      | hd1 :: tl1 -> binaryAdditionIntern l2 l1 toAdd
    in
      match l1 with
      | [] -> endCase l1 l2 toAdd
      | hd1 :: tl1 ->
        match l2 with
        | [] -> ((hd1 + toAdd) mod 2) :: (binaryAdditionIntern tl1 l2 ((hd1 + toAdd) / 2))
        | hd2 :: tl2 -> 
          ((hd2 + hd1 + toAdd) mod 2) :: (binaryAdditionIntern tl1 tl2 ((hd2 + hd1 + toAdd) / 2))
          
          (*let newl, toAdd = binaryAdditionIntern tl1 tl2 in
          (((hd2 + hd1 + toAdd) mod 2) :: newl, if hd2 * hd1 + toAdd * hd1 + hd2 * toAdd >= 1 then 1 else 0)*)
  in
    let revl1 = reverseList l1 [] in
    let revl2 = reverseList l2 [] in
    let newL = binaryAdditionIntern revl1 revl2 0 in
    reverseList newL []

let rec truncate l nb : int list =
  match l with
  | [] -> []
  | hd :: tl -> if nb <= 0 then [] else hd :: truncate tl (nb - 1)


(*==================================================MINOR FUNCTION==============================================*)









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

let rec leftRotate l nb =
  let leftRotateIntern l =
    match l with
    | [] -> []
    | hd :: tl -> concatList tl [hd]
  in
  if nb >= 1 then
    leftRotate (leftRotateIntern l) (nb - 1)
  else
    l
    

let rec getBlockFromIndex (l:'a list) nb =
  if nb <= 0 then
    (
    match l with
      | [] -> []
      | hd :: tl -> hd
    )
  else
    (
    match l with
      | [] -> []
      | hd :: tl -> getBlockFromIndex tl (nb - 1)
    )


let rec createNewBlocks (l:'a list) nb =
  if nb >= 80 then
    l
  else
    let wordA = getBlockFromIndex l (nb - 3) in
    let wordB = getBlockFromIndex l (nb - 8) in
    let wordC = getBlockFromIndex l (nb - 14) in
    let wordD = getBlockFromIndex l (nb - 16) in
    let xorA = xOR wordA wordB in
    let xorB = xOR wordC xorA in
    let xorC = xOR wordD xorB in
    createNewBlocks (concatList l [leftRotate xorC 1]) (nb + 1)


let rec extendChunks (l:'a list) =
  match l with
  | [] -> []
  | hd :: tl ->
    (createNewBlocks hd 16) :: extendChunks tl

let rec chunkBitwiseOperation l a b c d e j : int list * int list * int list * int list * int list  =
  if j < 20 then
    (
      let bandC = _AND b c in
      let notB = _AND (not b) d in
      let f = _OR bandC notB in
      let k = [0;1;0;1;1;0;1;0;1;0;0;0;0;0;1;0;0;1;1;1;1;0;0;1;1;0;0;1;1;0;0;1] in

      let word = getBlockFromIndex l j in
      let tempA = binaryAddition (leftRotate a 5) f in
      let tempB = binaryAddition tempA e in
      let tempC = binaryAddition tempB k in
      let temp = truncate (binaryAddition tempC word) 32 in
      chunkBitwiseOperation l temp a (leftRotate b 30) c d (j + 1)
    )
  else
    (
      if j < 40 then
        (
          let bxorC = xOR b c in
          let f = xOR bxorC d in
          let k = [0;1;1;0;1;1;1;0;1;1;0;1;1;0;0;1;1;1;1;0;1;0;1;1;1;0;1;0;0;0;0;1] in

          let word = getBlockFromIndex l j in
          let tempA = binaryAddition (leftRotate a 5) f in
          let tempB = binaryAddition tempA e in
          let tempC = binaryAddition tempB k in
          let temp = truncate (binaryAddition tempC word) 32 in
          chunkBitwiseOperation l temp a (leftRotate b 30) c d (j + 1)
        )
      else
        (
          if j < 60 then
            (
              let bandC = _AND b c in
              let bandD = _AND b d in
              let candD = _AND c d in
              let bandCorbandD = _OR bandC bandD in
              let f = _OR bandCorbandD candD in
              let k = [1;0;0;0;1;1;1;1;0;0;0;1;1;0;1;1;1;0;1;1;1;1;0;0;1;1;0;1;1;1;0;0] in

              let word = getBlockFromIndex l j in
              let tempA = binaryAddition (leftRotate a 5) f in
              let tempB = binaryAddition tempA e in 
              let tempC = binaryAddition tempB k in 
              let temp = truncate (binaryAddition tempC word) 32 in
              chunkBitwiseOperation l temp a (leftRotate b 30) c d (j + 1)
            )
          else
            (
              if j < 60 then
                (
                  let bxorC = xOR b c in
                  let f = xOR bxorC d in
                  let k = [1;1;0;0;1;0;1;0;0;1;1;0;0;0;1;0;1;1;0;0;0;0;0;1;1;1;0;1;0;1;1;0] in

                  let word = getBlockFromIndex l j in
                  let tempA = binaryAddition (leftRotate a 5) f in
                  let tempB = binaryAddition tempA e in 
                  let tempC = binaryAddition tempB k in 
                  let temp = truncate (binaryAddition tempC word) 32 in
                  chunkBitwiseOperation l temp a (leftRotate b 30) c d (j + 1)
                )
              else a, b, c, d, e
                
            )
        )
    )

let rec chunksBitwiseOperation (l:'a list) h0 h1 h2 h3 h4 a b c d e : int list * int list * int list * int list * int list =
  match l with
  | [] -> h0, h1, h2, h3, h4
  | hd :: tl -> let a, b, c, d, e = chunkBitwiseOperation hd a b c d e 0 in
    chunksBitwiseOperation  tl
                            (truncate (binaryAddition h0 a) 32)
                            (truncate (binaryAddition h1 b) 32)
                            (truncate (binaryAddition h2 c) 32)
                            (truncate (binaryAddition h3 d) 32)
                            (truncate (binaryAddition h4 e) 32)
                            a b c d e

(*==================================================PRINT FUNCTION==============================================*)

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

(*==================================================MAJOR FUNCTION==============================================*)

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

let hashFunction l = 
  let newL = extendChunks l in (*step 11*)
  let h0 = [0;1;1;0;0;1;1;1;0;1;0;0;0;1;0;1;0;0;1;0;0;0;1;1;0;0;0;0;0;0;0;1] in (*step 12*)
  let h1 = [1;1;1;0;1;1;1;1;1;1;0;0;1;1;0;1;1;0;1;0;1;0;1;1;1;0;0;0;1;0;0;1] in
  let h2 = [1;0;0;1;1;0;0;0;1;0;1;1;1;0;1;0;1;1;0;1;1;1;0;0;1;1;1;1;1;1;1;0] in
  let h3 = [0;0;0;1;0;0;0;0;0;0;1;1;0;0;1;0;0;1;0;1;0;1;0;0;0;1;1;1;0;1;1;0] in
  let h4 = [1;1;0;0;0;0;1;1;1;1;0;1;0;0;1;0;1;1;1;0;0;0;0;1;1;1;1;1;0;0;0;0] in

  let a = [0;1;1;0;0;1;1;1;0;1;0;0;0;1;0;1;0;0;1;0;0;0;1;1;0;0;0;0;0;0;0;1] in
  let b = [1;1;1;0;1;1;1;1;1;1;0;0;1;1;0;1;1;0;1;0;1;0;1;1;1;0;0;0;1;0;0;1] in
  let c = [1;0;0;1;1;0;0;0;1;0;1;1;1;0;1;0;1;1;0;1;1;1;0;0;1;1;1;1;1;1;1;0] in
  let d = [0;0;0;1;0;0;0;0;0;0;1;1;0;0;1;0;0;1;0;1;0;1;0;0;0;1;1;1;0;1;1;0] in
  let e = [1;1;0;0;0;0;1;1;1;1;0;1;0;0;1;0;1;1;1;0;0;0;0;1;1;1;1;1;0;0;0;0] in

  chunksBitwiseOperation newL h0 h1 h2 h3 h4 a b c d e (*step 13*)

let convertBinaryListToInt h0 h1 h2 h3 h4 : int =
  let rec convertBlockToHexa l nb =
    match l with
    | [] -> nb
    | hd :: tl -> 
      match hd with
      | p1 :: p2 :: p3 :: p4 :: other -> convertBlockToHexa tl ((nb * 16) + (p4 + p3 * 2 + p2 * 4 + p1 * 8))
      | e -> convertBlockToHexa tl ((nb * 16)) (*How did you get there again ?*)
  in
    let n0 = convertBlockToHexa (splitIntoBlockOf 4 h0) 0 in
    let n1 = convertBlockToHexa (splitIntoBlockOf 4 h1) 0 in
    let n2 = convertBlockToHexa (splitIntoBlockOf 4 h2) 0 in
    let n3 = convertBlockToHexa (splitIntoBlockOf 4 h3) 0 in
    let n4 = convertBlockToHexa (splitIntoBlockOf 4 h4) 0 in
    n0 + n1 + n2 + n3 + n4


let hashF str : int =
  let h0, h1, h2, h3, h4 = hashFunction (parseStringForOperation str) in
    convertBinaryListToInt h0 h1 h2 h3 h4 

module Newhash =
  struct
    type t = string
    let equal t1 t2 = (t1 = t2)
    let hash t = hashF t
  end

module StringHashtbl = Hashtbl.Make(Newhash)

let () =
let ht = StringHashtbl.create 5 in
let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
let pairs = List.map (fun s -> (s, String.length s)) values in
List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht