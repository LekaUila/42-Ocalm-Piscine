(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: Leka Uïla <liam.flandrinck.58@gmail.com    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2025/12/04 11:21:58 by Leka Uïla         #+#    #+#             *)
(*   Updated: 2025/12/10 17:20:02 by Leka Uïla        ###   ########.fr       *)
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
        let retValue = (f content) in
        let {state} = retValue  in
        let {error} = retValue  in
        if state then
          retValue
        else
          {
            content = content;
            state   = state;
            error   = error
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
      let flattenn (x:'a t t) : 'a t =
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

let main () =
  let i = Try.return 1 in print_endline "tset"



let () = main ()