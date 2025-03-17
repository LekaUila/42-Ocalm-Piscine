(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   life.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/14 12:32:33 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/17 15:07:23 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open Random

type phostate = string
type deoxyribose = string
type nucleobase =
| A
| T
| C
| G
| U
| None 
type nucleotide = {
  p : phostate;
  d : deoxyribose;
  base : nucleobase;
}
type aminoacid =
| Stop
| Ala
| Arg
| Asn
| Asp
| Cys
| Gln
| Glu
| Gly
| His
| Ile
| Leu
| Lys
| Met
| Phe
| Pro
| Ser
| Thr
| Trp
| Tyr
| Val

type helix = nucleotide list
type rna = nucleobase list
type protein = aminoacid list


let generate_nucleotide b =
  match b with
  | 'A' | 'a' -> {p= "phosphate"; d= "deoxyribose"; base= A}
  | 'T' | 't' -> {p= "phosphate"; d= "deoxyribose"; base= T}
  | 'C' | 'c' -> {p= "phosphate"; d= "deoxyribose"; base= C}
  | 'G' | 'g' -> {p= "phosphate"; d= "deoxyribose"; base= G}
  | 'U' | 'u' -> {p= "phosphate"; d= "deoxyribose"; base= U}
  | x -> {p= "phosphate"; d= "deoxyribose"; base= None}

let nucleotide_to_string n =
  let { p; d; base } = n
  in
    match base with
    | A -> "|P = " ^ p ^ ", D = " ^ d ^ ", base = " ^ "A|"
    | T -> "|P = " ^ p ^ ", D = " ^ d ^ ", base = " ^ "T|"
    | C -> "|P = " ^ p ^ ", D = " ^ d ^ ", base = " ^ "C|"
    | G -> "|P = " ^ p ^ ", D = " ^ d ^ ", base = " ^ "G|"
    | U -> "|P = " ^ p ^ ", D = " ^ d ^ ", base = " ^ "U|"
    | None -> "|P = " ^ p ^ ", D = " ^ d ^ ", base = " ^ "None|"

let rec generate_helix n =
  if n < 1 then
    []
  else if n = 1 then
    match Random.int 4 with
    | 0 -> [generate_nucleotide 'a']
    | 1 -> [generate_nucleotide 't']
    | 2 -> [generate_nucleotide 'g']
    | 3 -> [generate_nucleotide 'c']
    | x -> [generate_nucleotide 'z']
  else
    match Random.int 4 with
    | 0 -> (generate_nucleotide 'a') :: generate_helix (n - 1)
    | 1 -> (generate_nucleotide 't') :: generate_helix (n - 1)
    | 2 -> (generate_nucleotide 'g') :: generate_helix (n - 1)
    | 3 -> (generate_nucleotide 'c') :: generate_helix (n - 1)
    | x -> (generate_nucleotide 'z') :: generate_helix (n - 1)


  
let rec helix_to_string h = 
  match h with
  | [] -> ""
  | hd :: tl -> nucleotide_to_string hd ^ " " ^helix_to_string tl

let rec complementary_helix h =
  let associated_nucleotide n =
    let {base} = n
    in
      match base with
      | A -> generate_nucleotide 'T'
      | T -> generate_nucleotide 'A'
      | C -> generate_nucleotide 'G'
      | G -> generate_nucleotide 'C'
      | None -> generate_nucleotide 'N'
      | U -> generate_nucleotide 'N'
  in
  match h with
  | [] -> []
  | hd :: tl -> associated_nucleotide hd :: complementary_helix tl

let generate_rna h =
  let rec complementary_helix_spe h =
    let associated_nucleotide n =
      let {base} = n
      in
        match base with
        | A -> generate_nucleotide 'U'
        | T -> generate_nucleotide 'A'
        | C -> generate_nucleotide 'G'
        | G -> generate_nucleotide 'C'
        | None -> generate_nucleotide 'N'
        | U -> generate_nucleotide 'U'
    in
      match h with
      | [] -> []
      | hd :: tl -> associated_nucleotide hd :: complementary_helix_spe tl
  in
    let rec complementary_helix_spe_to_rna h =
      let associated_nucleotide n =
        let {base} = n
        in
          match base with
          | A -> A
          | T -> T
          | C -> C
          | G -> G
          | U -> U
          | None -> None
      in
        match h with
        | [] -> []
        | hd :: tl -> associated_nucleotide hd :: complementary_helix_spe_to_rna tl
    in
      complementary_helix_spe_to_rna (complementary_helix_spe h)

let rec rna_to_string h = 
  match h with
  | [] -> ""
  | hd :: tl ->
    match hd with
    | A -> "A " ^ rna_to_string tl
    | T -> "T " ^ rna_to_string tl
    | C -> "C " ^ rna_to_string tl
    | G -> "G " ^ rna_to_string tl
    | U -> "U " ^ rna_to_string tl
    | None -> "None " ^ rna_to_string tl


let rec generate_bases_triplets r = 
  match r with
  | [] -> []
  | hd1 :: hd2 :: hd3 :: tl-> (hd1, hd2, hd3) :: generate_bases_triplets tl
  | e -> []

let rec string_of_protein p =
match p with
| [] -> ""
| hd :: tl ->
  match hd with
  | Stop -> "End of translation " ^ string_of_protein tl
  | Ala -> "Alanine " ^ string_of_protein tl
  | Arg -> "Arginine " ^ string_of_protein tl
  | Asn -> "Asparagine " ^ string_of_protein tl
  | Asp -> "Aspartique " ^ string_of_protein tl
  | Cys -> "Cysteine " ^ string_of_protein tl
  | Gln -> "Glutamine " ^ string_of_protein tl
  | Glu -> "Glutamique " ^ string_of_protein tl
  | Gly -> "Glycine " ^ string_of_protein tl
  | His -> "Histidine " ^ string_of_protein tl
  | Ile -> "Isoleucine " ^ string_of_protein tl
  | Leu -> "Leucine " ^ string_of_protein tl
  | Lys -> "Lysine " ^ string_of_protein tl
  | Met -> "Methionine " ^ string_of_protein tl
  | Phe -> "Phenylalanine " ^ string_of_protein tl
  | Pro -> "Proline " ^ string_of_protein tl
  | Ser -> "Serine " ^ string_of_protein tl
  | Thr -> "Threonine " ^ string_of_protein tl
  | Trp -> "Tryptophane " ^ string_of_protein tl
  | Tyr -> "Tyrosine " ^ string_of_protein tl
  | Val -> "Valine " ^ string_of_protein tl

  
let rec decode_arm r =
  let triplet_list = generate_bases_triplets r
  in
  let rec triplet_list_to_protein lst =
    match lst with
    | [] -> [Stop]
    | hd :: tl ->
      match hd with
      |(U,A,A) | (U,A,G) | (U,G,A) -> [Stop]
      |(G,C,A) | (G,C,C) | (G,C,G) | (G,C,U) -> Ala :: triplet_list_to_protein tl
      |(A,G,A) | (A,G,G) | (C,G,A) | (C,G,C) | (C,G,G) | (C,G,U) -> Arg :: triplet_list_to_protein tl
      |(A,A,C) | (A,A,U) -> Asn :: triplet_list_to_protein tl
      |(G,A,C) | (G,A,U) -> Asp :: triplet_list_to_protein tl
      |(U,G,C) | (U,G,U) -> Cys :: triplet_list_to_protein tl
      |(C,A,A) | (C,A,G) -> Gln :: triplet_list_to_protein tl
      |(G,A,A) | (G,A,G) -> Glu :: triplet_list_to_protein tl
      |(G,G,A) | (G,G,C) | (G,G,G) | (G,G,U) -> Gly :: triplet_list_to_protein tl
      |(C,A,C) | (C,A,U) -> His :: triplet_list_to_protein tl
      |(A,U,A) | (A,U,C) | (A,U,U) -> Ile :: triplet_list_to_protein tl
      |(C,U,A) | (C,U,C) | (C,U,G) | (C,U,U) | (U,U,A) | (U,U,G) -> Leu :: triplet_list_to_protein tl
      |(A,A,A) | (A,A,G) -> Lys :: triplet_list_to_protein tl
      |(A,U,G) -> Met :: triplet_list_to_protein tl
      |(U,U,C) | (U,U,U) -> Phe :: triplet_list_to_protein tl
      |(C,C,C) | (C,C,A) | (C,C,G) | (C,C,U) -> Pro :: triplet_list_to_protein tl
      |(U,C,A) | (U,C,C) | (U,C,G) | (U,C,U) | (A,G,U) | (A,G,C) -> Ser :: triplet_list_to_protein tl
      |(A,C,A) | (A,C,C) | (A,C,G) | (A,C,U) -> Thr :: triplet_list_to_protein tl
      |(U,G,G)  -> Trp :: triplet_list_to_protein tl
      |(U,A,C) | (U,A,U) -> Tyr :: triplet_list_to_protein tl
      |(G,U,A) | (G,U,C) | (G,U,G) | (G,U,U) -> Val :: triplet_list_to_protein tl
      |e -> [Stop]
    in
    triplet_list_to_protein triplet_list
    
  
let rec generate_helix_from_string s i : helix  =
  if i = 0 then
    [generate_nucleotide (String.get s i)]
  else
    generate_helix_from_string s (i - 1) @ [generate_nucleotide (String.get s i)]


let life n =
  let new_helix = (generate_helix_from_string n ((String.length n) - 1))
  in
    print_endline ("Helix : " ^ (helix_to_string new_helix));
    let new_rna = generate_rna new_helix
    in
      print_endline ("rna : " ^ (rna_to_string new_rna));
      print_endline("protein : " ^ ( string_of_protein (decode_arm new_rna)))




let main () : unit =
  life "TTACGGGCTATGCATGCGCTAGCTACGTATGCTACTTGACTAGCTACGTATGCTATGCTAGCTATGCATCGTAGCTAGCTAGCTAGCATGCTATGCTAGCTATGCTACGTAGCTACGTACGTATCGTAGCTATCTAGCATCTGGATT"



let x = main()