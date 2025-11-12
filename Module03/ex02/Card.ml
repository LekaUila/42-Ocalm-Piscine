(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Card.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/21 14:23:36 by lflandri          #+#    #+#             *)
(*   Updated: 2025/11/12 17:46:11 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


module Color =
  struct
    type t = Spade | Heart | Diamond | Club

    let all = [Spade; Heart; Diamond; Club]
    
    let toString typ =
      match typ with
      | Spade -> "S"
      | Heart -> "H"
      | Diamond -> "D"
      | Club -> "C"
    
    let toStringVerbose typ = 
      match typ with
      | Spade -> "Spade"
      | Heart -> "Heart"
      | Diamond -> "Diamond"
      | Club -> "Club"
end

module Value =
  struct
    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
    let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

    let toInt typ =
      match typ with
      | T2 -> 1
      | T3 -> 2
      | T4 -> 3
      | T5 -> 4
      | T6 -> 5
      | T7 -> 6
      | T8 -> 7
      | T9 -> 8
      | T10 -> 9
      | Jack -> 10
      | Queen -> 11
      | King -> 12
      | As -> 13

    let toString typ =
      match typ with
      | T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "J"
      | Queen -> "Q"
      | King -> "K"
      | As -> "A"

    let toStringVerbose typ = 
      match typ with
      | T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "Jack"
      | Queen -> "Queen"
      | King -> "King"
      | As -> "As"

    let next typ = 
      match typ with
      | T2 -> T3
      | T3 -> T4
      | T4 -> T5
      | T5 -> T6
      | T6 -> T7
      | T7 -> T8
      | T8 -> T9
      | T9 -> T10
      | T10 -> Jack
      | Jack -> Queen
      | Queen -> King
      | King -> As
      | As -> invalid_arg "error"

    let previous typ = 
      match typ with
      | T2 -> invalid_arg "error"
      | T3 -> T2
      | T4 -> T3
      | T5 -> T4
      | T6 -> T5
      | T7 -> T6
      | T8 -> T7
      | T9 -> T8
      | T10 -> T9
      | Jack -> T10
      | Queen -> Jack
      | King -> Queen
      | As -> King
end





type t = (Value.t * Color.t)

let newCard v c : t =
  (v, c)

let rec create_list_card_value_from_of_c_color lst c =
  match lst with
  | [] -> []
  | hd::tl -> (newCard hd c)  :: create_list_card_value_from_of_c_color tl c

let allSpades = create_list_card_value_from_of_c_color Value.all (Color.Spade)
let allHearts = create_list_card_value_from_of_c_color Value.all (Color.Heart)
let allDiamonds = create_list_card_value_from_of_c_color Value.all (Color.Diamond)
let allClubs = create_list_card_value_from_of_c_color Value.all (Color.Club)
let all =
  create_list_card_value_from_of_c_color Value.all (Color.Spade) @
  create_list_card_value_from_of_c_color Value.all (Color.Heart) @
  create_list_card_value_from_of_c_color Value.all (Color.Diamond) @
  create_list_card_value_from_of_c_color Value.all (Color.Club)


let getValue typ : Value.t =
  match typ with
  | (v , c) -> v

let getColor typ : Color.t =
  match typ with
  | (v , c) -> c

let toString typ  : string =
  match typ with
  | (v , c) -> Value.toString v  ^ Color.toString c

let toStringVerbose typ : string =
  match typ with
  | (v , c) -> "Card(" ^ Value.toStringVerbose v ^ ", " ^ Color.toStringVerbose c ^ ")"

  
let compare card1 card2 : int =
  match card1 with
  | (v1 , c1) -> 
    match card2 with
    | (v2 , c2) -> (Value.toInt v1) - (Value.toInt v2)

let max card1 card2 : t =
    if compare card1 card2 > -1 then
      card1
    else
      card2

let min card1 card2 : t =
  if compare card1 card2 < 1 then
    card1
  else
    card2


let best lst : t =
    match lst with
    | [] -> invalid_arg "Empty list"
    | hd :: tl -> List.fold_left max hd tl

let isOf card color : bool =
  match card with
  | (v , c) -> c = color

let isSpade card : bool =
  isOf card Color.Spade

let isHeart card : bool =
isOf card Color.Heart

let isDiamond card : bool =
isOf card Color.Diamond

let isClub card : bool =
isOf card Color.Club
