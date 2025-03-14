(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <lflandri@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/03/14 12:32:33 by lflandri          #+#    #+#             *)
(*   Updated: 2025/03/14 13:23:11 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phostate = string
type deoxyribose = string
type nucleobase =
| A
| T
| C
| G
| None 
type nucleotide = {
  p : phostate;
  d : deoxyribose;
  base : nucleobase;
}



let generate_nucleotide b =
  match b with
  | 'A' | 'a' -> {p= "phosphate"; d= "deoxyribose"; base= A}
  | 'T' | 't' -> {p= "phosphate"; d= "deoxyribose"; base= T}
  | 'C' | 'c' -> {p= "phosphate"; d= "deoxyribose"; base= C}
  | 'G' | 'g' -> {p= "phosphate"; d= "deoxyribose"; base= G}
  | x -> {p= "phosphate"; d= "deoxyribose"; base= None}







let print_nucleotide n =
  let { p; d; base } = n
  in
    print_string "Nucleotide : P = ";
    print_string p;
    print_string "D = ";
    print_string d;
    print_string "base = ";
    match base with
    | A -> print_char 'A'; print_newline ()
    | T -> print_char 'T'; print_newline ()
    | C -> print_char 'C'; print_newline ()
    | G -> print_char 'G'; print_newline ()
    | None -> print_string "None"; print_newline ()

let main () : unit =
  print_endline "Test pour a :";
  print_nucleotide (generate_nucleotide 'a');
  print_endline "Test pour A :";
  print_nucleotide (generate_nucleotide 'A');
  print_endline "Test pour t :";
  print_nucleotide (generate_nucleotide 't');
  print_endline "Test pour T :";
  print_nucleotide (generate_nucleotide 'T');
  print_endline "Test pour c :";
  print_nucleotide (generate_nucleotide 'c');
  print_endline "Test pour C :";
  print_nucleotide (generate_nucleotide 'C');
  print_endline "Test pour g :";
  print_nucleotide (generate_nucleotide 'g');
  print_endline "Test pour G :";
  print_nucleotide (generate_nucleotide 'G');
  print_endline "Test pour E :";
  print_nucleotide (generate_nucleotide 'E');
  print_endline "Test pour z :";
  print_nucleotide (generate_nucleotide 'z');
  print_endline "Test pour 1 :";
  print_nucleotide (generate_nucleotide '1')

let x = main()