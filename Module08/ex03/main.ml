(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: lflandri <liam.flandrinck.58@gmail.com>    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/12/04 11:21:58 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/12/15 14:05:26 by lflandri         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Try =
    struct
     exception BadFilter of string
     exception NotAnError of string
      type 'a t = {
                content : 'a ;
                state : bool;
                error : exn;
              }
      let return (input:'a) : ('a t) =
        {
          content = input;
          state   = true;
          error   = NotAnError "nothing went wrong"
        }
      let bind (x:'a t) (f:('a -> 'b t)) : 'b t =
        let {content} = x in
        try
          (f content)
        with e ->           {
            content = content;
            state   = false;
            error   = e
          }
      
      let recover (x:'a t) (f:exn -> 'a t) : 'a t = 
        let {state} = x in
        if state then
          x
        else let {error} = x in f error
      let filter (x:'a t) (f:'a -> bool) : 'a t =
        let {content} = x in
        if f content then
          x
        else
          {
            content = content;
            state   = false;
            error   = BadFilter "content doesn't satisfy the filter function"
          }
      let flatten (x:'a t t) : 'a t =
        let {content} = x in
        let {state} = x in
        let {error} = x in
        if state then
            content
        else
            let {content} = content in
              {
              content = content;
              state   = false;
              error   = error
            }
          
    end



(*=========================================================================================================================*)

exception FakeError of string
exception NotAFakeError of string


let intfakefunction nb = 
  if nb = 1 then
    Try.return (nb + 2)
  else
    raise (FakeError "i don't want other than 1")

let intfakefunctionRevover err = 
  if err = (FakeError "i don't want other than 1") then
    Try.return 1
  else
    Try.return 42

let isOne nb = 
  if nb = 1 then
    true
  else
    false

let printTry (x:'a Try.t) = 
    let content = x.content in
    let state = x.state in
    let error = x.error in
    print_int content;
    print_string "  ";
    if state then
      print_string "true  "
    else
      print_string "false  ";
    print_endline (Printexc.to_string error)

let main () =
  let i1 =  Try.return 1 in
  let i2 =  Try.return 2 in
  printTry i1;
  printTry i2;
  let i3 = Try.bind i1 intfakefunction in
  let ei2 = Try.bind i2 intfakefunction in
  printTry i3;
  printTry ei2;
  let i3 = Try.recover i3 intfakefunctionRevover in
  let ei2 = Try.recover ei2 intfakefunctionRevover in
  printTry i3;
  printTry ei2;
  let ei3 = Try.filter i3 isOne in
  let i1 = Try.filter ei2 isOne in
  printTry ei3;
  printTry i1;
  let t1 = Try.flatten (Try.return ei3) in
  let t2 = Try.flatten (Try.return ei2) in
  printTry t1;
  printTry t2

let () = main ()