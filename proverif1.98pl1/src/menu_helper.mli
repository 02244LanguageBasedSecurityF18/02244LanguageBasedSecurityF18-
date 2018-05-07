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
open Types
exception WrongChoice
exception WarningsAsError
(* Functions for !cur_state *)
(* [no_more_porc ()] Return true if the maximal number of process that can be displayed is hit *)
val no_more_proc: unit -> bool
val inc_proc_nb: unit -> unit
val dec_proc_nb: unit -> unit
val get_proc_nb: unit -> int
(* [reduc_state_of_proc proc] Return the term Pitypes.reduc_state build from [proc] *)
val reduc_state_of_proc: process -> term Pitypes.reduc_state
(* [get_proc state n] Return the n-th process in state.subprocess *)
val get_proc: term Pitypes.reduc_state -> int -> process
(* [delete_NamedProcess state] Apply all the possible NamedProcess reductions to state *)
val delete_NamedProcess: 'a Pitypes.reduc_state -> 'a Pitypes.reduc_state
(* [is_auto_reductible state p] Return true if [p] is auto reductible according to [state], *)
(* false otherwise *)
val is_auto_reductible: term Pitypes.reduc_state -> process -> bool
val equal_io_oi : Pitypes.io_r_t -> Pitypes.io_r_t -> bool
(* [reset_env ()] Reset the global environment, clear tables, restore some parameters. *)
(* Used to load a new file *)
val reset_env: unit -> unit
(* [input_error_box b mess ext] Create a message box with title "Error in your Recipe", and one *)
(* button. The message displayed is comming from an InputError(mess, ext) exception. If [b] *)
(* is true, the the message display the line number and the character number of the error. *)
(* Otherwise, its only display the character number *)
val input_error_box: bool -> string -> Parsing_helper.extent -> unit
(* [parse_term string] Return the term corresponding to the parsing of [string]. *)
(* Raise InputError if the parsing went wrong *)
val parse_term: string -> term
(* [dialog_box title string ()] Create a dialog box with title [title], displaying the string *)
(* string [string]. Return the string enter by the user, raise WrongChoice if no string is entered. *)
val dialog_box:  string -> string -> unit -> string
(* [question_box title buttons string] Create a question box with title [title], displaying *)
(* the string [string]. This box has a list of buttons labelled by the string in the string list *)
(* [buttons]. It returns the number of the clicked button, starting from 1, 0 otherwise. *)
val question_box : string -> string list -> string -> unit -> int
(* [proc_of_sub proc] Return the process corresponding to [sub] *)
val proc_of_sub: term Pitypes.sub_proc -> process
(* [sub_of_proc proc] Return the subprocess corresponding to [proc] *)

val sub_of_proc: process -> term Pitypes.sub_proc
(* [update_cur_state state] Update the current state with state *)
val update_cur_state: term Pitypes.reduc_state -> unit
(* [exists_auto ()] Return true if there exists a auto-reductible process in one of the *)
(* subprocess of the current state, false otherwise *)
val exists_auto: unit -> bool
(* [is_first_step ()] Return true if the current state is the initialise state, false otherwise *)
val is_first_step: unit -> bool
(* [get_state ()] Return the current state *)
val get_state: unit -> term Pitypes.reduc_state
(* [get_data ()] Return the current data associated to the current state *)
val get_data: unit -> Pitypes.data_model
(* [set_model_build b] Set the reference model_build to b *)
val set_model_build: bool -> unit
(* [model_is_build ()] Return true if a file has already been load, false otherwise *)
val model_is_build: unit -> bool
(* [frz ()] Freeze the necessary elements in the main window *)
val frz: unit -> unit
(* [dfrz ()] De-freeze the necessary elements in the main window *)
val dfrz: unit -> unit
(* [sets_button b bool] Set the sensitivity of a button [b] according to [bool] *)
val sets_button:< coerce : < misc : < set_sensitive : 'a -> 'b; .. >; .. >; .. > ->
'a -> unit
(* Getter functions for the interface *)
val get_b_bstep: unit -> GButton.button
val get_b_new_name: unit -> GButton.button
val get_b_step_auto: unit -> GButton.button
val get_b_new_public: unit -> GButton.button
val get_b_all_auto:  unit -> GButton.button
(* Getter functions for the interface *)
val get_window_p: unit -> GWindow.window
val get_view: unit -> GTree.view
val get_cols: unit -> GTree.column_list
val get_col_lst: unit -> string GTree.column list
val get_menu_bar: unit -> GMenu.menu_shell
val get_menu_box: unit -> GPack.box
val get_factory: unit -> GMenu.menu_shell GMenu.factory
val get_file_select_item: unit -> GMenu.menu_item
val get_quit_item: unit -> GMenu.menu_item
val get_next_auto_item: unit -> GMenu.menu_item
val get_all_auto_item: unit -> GMenu.menu_item
val get_step_back_item: unit -> GMenu.menu_item
val get_create_nonce_item: unit -> GMenu.menu_item
val get_create_public_item: unit -> GMenu.menu_item
val get_recipe : string -> string -> term
val expand_recipe : (Types.term * Types.term) list -> Types.term -> Types.term
val get_display_trace_item: unit -> GMenu.menu_item
val get_show_tables_item: unit -> GMenu.menu_item
val get_show_events_item: unit -> GMenu.menu_item
(* Set or get the reference [io_c] used to make RIO reductions *)
val set_io_c: Pitypes.io_r_t -> unit
val get_io_c: unit -> Pitypes.io_r_t
