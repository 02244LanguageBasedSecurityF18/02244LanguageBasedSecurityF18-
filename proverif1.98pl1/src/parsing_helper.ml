(*************************************************************
 *                                                           *
 *  Cryptographic protocol verifier                          *
 *                                                           *
 *  Bruno Blanchet, Vincent Cheval, and Marc Sylvestre       *
 *                                                           *
 *  Copyright (C) INRIA, CNRS 2000-2017                      *
 *                                                           *
 *************************************************************)

(*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details (in file LICENSE).

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)
open Lexing

let internal_error mess =
  print_string ("Internal error: " ^ mess ^ "\nPlease report bug to Bruno.Blanchet@inria.fr, including input file and output\n");
  exit 3

(* extent, for error messages *)

type extent = Lexing.position * Lexing.position

let dummy_ext = (Lexing.dummy_pos, Lexing.dummy_pos)

let merge_ext (p1,_) (_,p2) = (p1,p2)

let next_line lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
			 pos_bol = lexbuf.lex_curr_p.pos_cnum;
			 pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1 }

let extent lexbuf =
  (Lexing.lexeme_start_p lexbuf,
   Lexing.lexeme_end_p lexbuf)

let parse_extent () =
  (Parsing.symbol_start_pos(),
   Parsing.symbol_end_pos())

let combine_extent ((outer_start, _) as outer_ext) ((inner_start, inner_end) as inner_ext) =
  if inner_ext == dummy_ext then outer_ext else
  if outer_ext == dummy_ext then inner_ext else
  ({ outer_start with
     pos_cnum = outer_start.pos_cnum + inner_start.pos_cnum + 1 },
   { outer_start with
     pos_cnum = outer_start.pos_cnum + inner_end.pos_cnum + 1 })

exception InputError of string * extent

(* Add a point at the end of mess if neccessary *)
let add_point_if_necessary mess =
  if (String.length mess > 0) &&
    (let end_char = String.get mess (String.length mess - 1) in
    end_char != '.' && end_char != '?' && end_char != '!')
  then
    "."
else
    ""

(* Raise InputError *)
let input_error mess extent =
  raise (InputError (mess, extent))

(* Get the message to write from mess and ext. Verbose if v is true *)
let get_mess_from v prefix mess (loc_start, loc_end) =
  let message = prefix ^  mess ^ add_point_if_necessary mess in
  if loc_start.pos_cnum = -1 then
    message
  else if (not v) then
    "Character " ^ (string_of_int  (loc_start.pos_cnum - loc_start.pos_bol +1))
    ^ " - " ^ string_of_int (loc_end.pos_cnum - loc_end.pos_bol) ^ ":\n"
    ^  message
  else
    "File \"" ^ loc_start.pos_fname ^ "\", "
    ^ "line " ^ (string_of_int (loc_start.pos_lnum))
    ^ ", character " ^ (string_of_int  (loc_start.pos_cnum - loc_start.pos_bol +1))
    ^ " - line " ^ (string_of_int (loc_end.pos_lnum))
    ^ ", character " ^ string_of_int (loc_end.pos_cnum - loc_end.pos_bol (* + 1 *)) ^ ":\n"
    ^ message

(* Print the message with the location of the error, and a point at the end if needed. *)
let display_input_error mess ext =
  print_endline (get_mess_from true "Error: " mess ext);
  exit 2

(* print a warning message with the location of the error, and a point at the end if needed *)

(*for interactive mode *)
let interactive_mode = ref false

let warning_list = ref []

let get_warning_list() =
  let result = !warning_list in
  warning_list := [];
  result
    
let input_warning mess ext =
  if !interactive_mode then
    warning_list := (mess, ext) :: (!warning_list)
  else
    print_endline (get_mess_from true "Warning: " mess ext)

(* raise InputError with unknown extent *)
let user_error mess =
  raise (InputError (mess, dummy_ext))
