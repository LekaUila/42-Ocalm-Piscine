(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/12/04 11:21:58 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/12/04 14:28:53 by Leka Uïla        ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

include App


let print_proj (p:App.project) =
  let typ, status, grade = p in
  print_string (typ ^ " : " ^ status ^ " (");
  print_int grade;
  print_endline ")"

let main () =
  let trans = "Transcendance", "success", 120 in
  let mod1 = "Mod1", "failed", 42 in
  print_proj trans;
  print_proj mod1;
  print_proj App.zero;
  
  print_proj (App.fail App.zero);
  print_proj (App.success App.zero);

  print_proj (App.fail trans);
  print_proj (App.success trans);
  print_proj (App.combine App.zero trans);

  print_proj (App.fail mod1);
  print_proj (App.success mod1);
  print_proj (App.combine App.zero mod1);

  print_proj (App.combine trans mod1)





let () = main ()