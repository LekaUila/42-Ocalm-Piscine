(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   rna.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/14 12:32:33 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/25 17:11:55 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include Random

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
type helix = nucleotide list
type rna = nucleobase list



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
    
let main () : unit =
  Random.self_init ();
  let new_helix = generate_helix 3
  in
    print_endline ("New Helix : " ^ (helix_to_string new_helix));
    print_endline ("rna : " ^ (rna_to_string (generate_rna new_helix)));
  let new_helix = generate_helix 0
  in
    print_endline ("New Helix : " ^ (helix_to_string new_helix));
    print_endline ("rna : " ^ (rna_to_string (generate_rna new_helix)))

let x = main()