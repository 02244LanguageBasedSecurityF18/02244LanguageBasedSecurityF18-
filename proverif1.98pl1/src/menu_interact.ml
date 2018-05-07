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
open GMain
open GdkKeysyms
open Pitypes
open Types

let debug_print s = ()
  (* print_endline s *)

let window_p = Menu_helper.get_window_p ()

let view = Menu_helper.get_view ()

let cols = Menu_helper.get_cols ()

let col_lst = Menu_helper.get_col_lst ()

let b_bstep = Menu_helper.get_b_bstep ()

let b_new_name = Menu_helper.get_b_new_name ()

let b_new_public = Menu_helper.get_b_new_public ()

let b_step_auto = Menu_helper.get_b_step_auto ()

let b_all_auto = Menu_helper.get_b_all_auto ()

let state_destroyed = ref false

(* [i_choice] is equal to 1 if the user choice left when there is choice on his input file, *)
(* 2 if he choices the right, 0 if no choice has been made yet *)
let i_choice = ref 0

(* [choice_in_term t] Return the term t, without the choices if there are some in it, *)
(* according to the user choice *)
let rec choice_in_term = function
    (Var _) as t -> t
  | FunApp(f, ([t1; t2] as l)) when f.f_cat == Choice ->
      choice_in_term (List.nth l ((!i_choice) - 1))
  | FunApp(f, l) ->
      FunApp(f, List.map choice_in_term l)

let rec choice_in_pat = function
    (PatVar _) as pat -> pat
  | PatTuple(f,l) -> PatTuple(f, List.map choice_in_pat l)
  | PatEqual t -> PatEqual (choice_in_term t)

(* [choice_in_proc p] Return the process p without all the choices in terms that might be present *)
let rec choice_in_proc = function
    | Nil -> Nil
    | Par(p, q) -> Par(choice_in_proc p, choice_in_proc q)
    | Repl(p, occ) -> Repl(choice_in_proc p, occ)
    | Restr(f, args, p, occ) -> Restr(f, args, choice_in_proc p, occ)
    | Test(t, p, q, occ) -> Test (choice_in_term t, choice_in_proc p, choice_in_proc q, occ)
    | Input(t, pat, p, occ) -> Input(choice_in_term t, choice_in_pat pat, choice_in_proc p, occ)
    | Output(t, t', p, occ) -> Output(choice_in_term t, choice_in_term t', choice_in_proc p, occ)
    | Let(pat, t, p, q, occ) -> Let(choice_in_pat pat, choice_in_term t, choice_in_proc p, choice_in_proc q, occ)
    | LetFilter(bl, f, p, q, occ) -> LetFilter(bl, f, choice_in_proc p, choice_in_proc q, occ)
    | Event(t, args, p, occ) -> Event(choice_in_term t, args, choice_in_proc p, occ)
    | Insert(t, p, occ) -> Insert(choice_in_term t, choice_in_proc p, occ)
    | Get(pat, t, p, q, occ) -> Get(choice_in_pat pat, choice_in_term t, choice_in_proc p, choice_in_proc q, occ)
    | Phase(i, p, occ) -> Phase(i, choice_in_proc p, occ)
    | Barrier(i, s, p, occ) -> Barrier(i, s, choice_in_proc p, occ)
    | AnnBarrier(i, so, f, f', btl, p, occ) -> AnnBarrier(i, so, f, f', List.map (fun (b, t) -> (b, choice_in_term t)) btl, choice_in_proc p, occ)
    | NamedProcess(s, tl, p) -> NamedProcess(s, List.map choice_in_term tl, choice_in_proc p)

(* [check_supported p] verifies that all needed features are supported by the simulator *)
let rec check_supported = function
    | Nil -> ()
    | Par(p, q)
    | Let(_, _, p, q, _)
    | Get(_, _, p, q, _)
    | Test(_, p, q, _) -> check_supported p; check_supported q
    | Restr(_,_,p, _)
    | Repl(p, _)
    | Input(_, _, p, _)
    | Event(_,_, p, _)
    | Insert(_, p, _)
    | Phase(_, p, _)
    | NamedProcess(_, _, p)
    | Output(_, _, p, _) -> check_supported p
    | LetFilter _ -> Parsing_helper.user_error "let...suchthat not supported by the simulator"
    | Barrier _ | AnnBarrier _ -> Parsing_helper.user_error "sync not supported by the simulator"

(* [anal_file s] Parse the file of path [s]. Update the current state with the result of this *)
(* parsing. Show a dialog box with an error message if there is a problem during the parsing, *)
(* and raise [Menu_helper.WrongChoice] *)
let rec anal_file s =
  Menu_helper.reset_env();
  state_destroyed := true;
  let build_state_from p =
    check_supported p;
    let p1 =
      if !Param.has_choice then
	begin
	  i_choice := Menu_helper.question_box "Choice/diff present in your process" ["first"; "second"] "Which component of choice/diff would you like to simulate?" ();
	  if !i_choice = 0 then
	    raise Menu_helper.WrongChoice;
	  choice_in_proc p
	end
      else
	p
    in
    let p0 = Simplify.prepare_process p1 in
    TermsEq.record_eqs_with_destr();
    Destructor.check_deterministic !Pitsyntax.destructors_check_deterministic;
    Menu_helper.update_cur_state (Menu_helper.delete_NamedProcess (Menu_helper.reduc_state_of_proc p0))
  in
  try
    Param.typed_frontend := true;
    let p0, second_p0 = Pitsyntax.parse_file s in
    (* Delete trailling [-1] in binders *)
    (* Allow the user to choose which process simulates in case second_p0 is not Null *)
    (* (i.e. when equivalence between process is made *)
    let warnings = Parsing_helper.get_warning_list() in
    if warnings != [] then
      begin
	let messages = String.concat "" (List.map (fun (mess, ext) ->
	  (Parsing_helper.get_mess_from true "Warning: " mess ext) ^ "\n") warnings) in
	ignore (Menu_helper.question_box "Warnings" ["OK"] messages ())
      end;
    begin
      match p0, second_p0 with
        p0, None -> build_state_from p0
      | p0, Some (p') ->
         begin
           match Menu_helper.question_box "Choose a process to simulate" ["First"; "Second"] "Simulate first or second process ?" () with
           | 1 -> build_state_from p0;
           | 2 -> build_state_from p';
           | _ -> raise Menu_helper.WrongChoice
         end
    end;
    state_destroyed := false
  with
  | Parsing_helper.InputError (mess, ext) ->
     Menu_helper.input_error_box true mess ext;
     Menu_helper.reset_env();
     raise Menu_helper.WrongChoice

(* [do_all_auto_reduction ()] Do all possible auto reductions on the current state. *)
(* Return the modified state *)
let rec do_all_auto_reduction () =
  let state = Menu_helper.get_state () in
  let n_state =  Reduction_interact.do_all_auto_reduction state in
  Menu_helper.dfrz();
  n_state

(* [create_model ()] Create the list_store from the current state and fill it. *)
(* Return [(data, store)], where data is the data obtained from *)
(*  the function [Menu_helper.get_data ()] *)
(* and store is the [GTree.list_store] containing these data *)
let create_model () =
  let _ = Menu_helper.set_model_build true in
  let data = Menu_helper.get_data () in
  if Menu_helper.exists_auto() then
    Menu_helper.sets_button b_all_auto true
  else
    Menu_helper.sets_button b_all_auto false;
  begin
        match (Menu_helper.get_state()).previous_state with
        | None -> Menu_helper.sets_button b_bstep false
        | Some _ -> Menu_helper.sets_button b_bstep true
  end;
  let store = GTree.list_store cols in
  let iter = ref (store#append ()) in
  let all_empty = ref true in
  let rec fill_store acc n lls =
    match lls with
      [] ->
        begin
          match acc with
            [] -> ()
          | acc -> if !all_empty then () else
              begin
                all_empty:= true;
                iter := store#append ();
                fill_store [] 0 (List.rev acc)
              end
        end
    | ls::tl_lls ->
       begin
         match ls with
           [] ->
           store#set ~row:!iter ~column:(List.nth col_lst n) "";
           fill_store ([]::acc) (n + 1) tl_lls
         | s::tl_s ->
            begin
              all_empty:= false;
              store#set ~row:!iter ~column:(List.nth col_lst n) s;
              fill_store (tl_s::acc) (n + 1) tl_lls
            end
       end
  in
  let lls = data.tables_lst::data.events_lst::data.public_lst::data.proc_llst in
  fill_store [] 0 lls;
  (data, store)

(* [do_one_reduction_step n ()] Do one reduction step on the nth subprocess of the current state. *)
(* Modify the current state *)
let do_one_reduction_step n () =
  let state = Menu_helper.get_state () in
  try
    Reduction_interact.do_one_reduction_step state n
  with
    Menu_helper.WrongChoice -> state
  | Parsing_helper.InputError(mess, extent) -> Menu_helper.input_error_box false mess extent; state

(* [next_auto_step ()] Callback function to make the first next auto reduction step. *)
(* Modify the current state *)
let next_auto_step () =
  let state = Menu_helper.get_state () in
  let rec aux state n = function
      [] -> assert false
    | sub::tl ->
        let p = Menu_helper.proc_of_sub sub in
        if Menu_helper.is_auto_reductible state p then
          do_one_reduction_step n ()
        else
          aux state (n + 1) tl
  in
  aux state 0 state.subprocess

(* [create_new_nonce ()] Callback function to create a nonce and add it to the current state. *)
(* Modify the current state *)
let create_new_nonce () =
  let state = Menu_helper.get_state () in
  let return state ty =
    let id = (Terms.fresh_id "n") in
    let n = Terms.create_name id ([], ty) false in
    let t = FunApp(n, []) in
    { state with
      public = (t, t)::state.public;
      previous_state = Some(state);
      comment = RRestrAtt}
  in
  try
    if not (Param.get_ignore_types ()) then
      let t =
        Menu_helper.dialog_box "Enter the type" "Enter the type of the new nonce" ()
      in
      let ty = List.find (fun {tname = t'} -> t = t') (!Param.all_types) in
      return state ty
    else
      return state Param.any_type
  with
    Not_found ->
     let _ = Menu_helper.question_box "Error" ["Ok"] "Type not defined" () in
     state
  | Menu_helper.WrongChoice -> state

let create_new_public () =
  let state = Menu_helper.get_state () in
  begin
    try
      let r = Menu_helper.get_recipe "" "Give a recipe to add to the public elements of the current state" in
      let exp = Menu_helper.expand_recipe state.public r in
      let t = Evaluation_helper.term_evaluation_fail exp in
      let new_recipe = Terms.new_var "~M" (Terms.get_term_type t) in
      let r = Terms.copy_term4 (Var new_recipe) in
      if List.exists
           (fun (_, t') -> Reduction_helper.equal_terms_modulo t t') state.public then
        let _ = Menu_helper.question_box "Error" ["Ok"] "Recipe already in the public elements of the current state" () in
        state
      else
      { state with
        public = (r, t)::state.public;
        previous_state = Some state;
        comment = RAddpub
      }
    with
    | Terms.Unify -> ignore (Menu_helper.question_box "Error" ["Ok"] "The evaluation of the recipe fails" ()); state
    | _ -> state
  end

(* [one_step_backward ()] Callback function to make one backward reduction step .*)
(* Modify the current state. *)
let rec one_step_backward () =
  Menu_helper.set_io_c Other;
  let state = Menu_helper.get_state () in
  match state.previous_state, state.comment with
  | None, _ -> state
  | Some state', RNamedProcess(_, _, _) ->
     (* If the current state is a NamedProcess step, we call the function on the previous state *)
     begin
       Menu_helper.update_cur_state state';
       one_step_backward ()
     end
  | Some state', REvent1(_, t, _)
  | Some state', REvent2(_, t)->
     { state' with
       events = List.tl state.events
     }
  | Some state', _ -> state'


let show_tables_bool = ref false

let show_events_bool = ref false

let click_on_show view bool n =
  let col = view#get_column n in
  col#set_visible (not bool);
  not bool

(* [update_titles view data] Update titles of the view according to data. *)
(* RIO reductions are made in two steps. First the user click on a column containing a private *)
(* input (respectively output). [Menu_helper.get_io_c ()] is then equal to [I_O(tou, _, _, _)], *)
(* (resp. [O_I(tin, _, _, _)]) and the call to [update_titles view data] will only show  *)
(* the output processes on [tou] (resp. input on [tin]). *)
let update_titles view data =
  let t1 = Menu_helper.get_io_c () in
  List.iteri (fun n (title, t2) ->
    let col = view#get_column n in
    col#set_title title;
    if (t1 = Other) || (Menu_helper.equal_io_oi t1 t2) then
      begin
        if n = 0 then
          col#set_visible !show_tables_bool
        else
          if n = 1 then
            col#set_visible !show_events_bool
          else
            col#set_visible true
      end
    else
      col#set_visible false
	) data.titles

(* [ends_with s sub] Return true if [sub] if a suffix of [s]. *)
let ends_with s sub =
  let l_s = String.length s in
  let l_sub = String.length sub in
  (l_s >= l_sub) && (String.sub s (l_s - l_sub) l_sub = sub)

(* For displaying traces *)
(* Main window to display trace *)
let win_trace = ref (GWindow.window ~title:"Trace" ())

let file_names = ref None

let get_file_names() =
  match !file_names with
    None ->
      let dot_file_name = Filename.temp_file "trace" ".dot" in
      let png_file_name = Filename.temp_file "trace" ".png" in
      file_names := Some (dot_file_name, png_file_name);
      (dot_file_name, png_file_name)
  | Some (dot_file_name, png_file_name) ->
      (dot_file_name, png_file_name)

(* [delete_trace_files ()] Delete the files associated to the trace if it's open *)
let delete_trace_files () =
  match !file_names with
    None -> ()
  | Some (dot_file_name, png_file_name) ->
      Unix.unlink(dot_file_name);
      Unix.unlink(png_file_name);
      file_names := None

(* [destroy_win_trace ()] Callback function to destroy trace window. *)
let destroy_win_trace () =
  if !Param.trace_win_open then
    begin
      delete_trace_files ();
      !win_trace#destroy();
      Param.trace_win_open := false;
      Menu_helper.dfrz()
    end

let destroy_main_win () =
  if !Param.trace_win_open then
      delete_trace_files ();
  exit 0

let _ = window_p#connect#destroy ~callback:destroy_main_win
let _ = window_p#set_default_size ~width:800 ~height:480

(* [img] Image to put inside the trace box (the vbox inside the trace window). *)
let img = ref (GMisc.image ())

let filter string =
  let patterns = "*." ^ string in
  GFile.filter ~name:patterns ~patterns:[patterns] ()

let dir = ref (Sys.getcwd ())

let update_dir = function
  | None -> ()
  | Some s -> dir := s


(* [display_trace ()] Callback function to display graph trace. *)
let display_trace () =
  Param.trace_win_open := true;
  let (dot_file_name, png_file_name) = get_file_names() in
  (* [callback_save()] Callback dialog function to save the trace file in .png. *)
  let rec callback_save () =
    let ok_cb dialog  () =
      match dialog#filename with
        None ->
          let _ = Menu_helper.question_box "Error" ["Ok"] "Please use .png, .pdf, .jpg, or .eps format to save the file." () in
          dialog#destroy ();
          callback_save()
      | Some s ->
         begin
           update_dir (dialog#current_folder);
           let s_up = String.uppercase s in
           if (ends_with s_up ".PNG") || (ends_with s_up ".PDF") || (ends_with s_up ".JPG") || (ends_with s_up ".EPS") then
             begin
	       let last_3 = String.lowercase (String.sub s (String.length s - 3) 3) in
               match Sys.command ((!Param.interactive_dot_command) ^ " -T" ^ last_3
                                  ^ " " ^ dot_file_name ^ " -o "
                                  ^ s)
               with
                 0 -> dialog#destroy ();
               | _ ->
		   let _ = Menu_helper.question_box "Error" ["Ok"] ("The call to " ^ (!Param.interactive_dot_command) ^ " failed. Please check that it is correctly installed.\n(The program \"" ^ (!Param.interactive_dot_command) ^ "\" must be in the PATH.)") () in
                   dialog#destroy ()
             end
           else
             let _ = Menu_helper.question_box "Error" ["Ok"] "Please use .png, .pdf, .jpg, or .eps format to save the file." () in
             dialog#destroy();
             update_dir (dialog#current_folder );
             callback_save ()
         end
    in
    let dialog = GWindow.file_chooser_dialog ~action:`SAVE  ~title:"Save trace. Please specify the extension (.png, .pdf, .jpg, or .eps) in the file name" () in
    update_dir (dialog#current_folder);
    let _ = dialog#set_current_folder (!dir) in
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_select_button_stock `SAVE `SAVE;
    dialog#add_filter (filter "png");
    dialog#add_filter (filter "pdf");
    dialog#add_filter (filter "jpg");
    dialog#add_filter (filter "eps");
    begin
      match dialog#run () with
      | `SAVE ->
         begin
           match dialog#filename with
           | None -> ()
           | Some s ->
              ok_cb dialog ();
         end
    | `DELETE_EVENT | `CANCEL -> dialog#destroy (); Menu_helper.dfrz()
    end
  in
  (* [create_win_trace ()] Create the main window trace, return [trace_box], the vbox *)
  (* inside the window trace which contains the image of the trace *)
  let create_win_trace () =
    Param.trace_win_open := true;
    (* Set the sensitivity of display trace item to false *)
    (Menu_helper.get_display_trace_item ())#misc#set_sensitive false;
    win_trace := GWindow.window ~title:"Trace" ();
    let _ = !win_trace#connect#destroy ~callback:destroy_win_trace in
    !win_trace#set_default_size ~width:800 ~height:480;
    let win_scrolled_trace = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:(!win_trace#add)  () in
    let trace_box = GPack.vbox ~packing:(win_scrolled_trace#add_with_viewport)  () in
    img := GMisc.image ();
    let menu_bar = GMenu.menu_bar ~packing:trace_box#pack () in
    let factory = new GMenu.factory menu_bar in
    let accel_group = factory#accel_group in
    let _ = !win_trace#add_accel_group accel_group in
    let save_menu = factory#add_submenu "Save" in
    let factory_save = new GMenu.factory save_menu ~accel_group in
    let _ = factory_save#add_item "Save File" ~key:_S ~callback:callback_save in
    trace_box
  in
  (* [create_trace state trace_box] Create the trace from cur_state, insert it in trace_box, *)
  (* display the main windows. If graphviz is not installed, display an error dialog box. *)
  let create_trace trace_box =
    let state = Menu_helper.get_state () in
    Display.AttGraph.write_state_to_dot_file dot_file_name Display.term_to_term (fun _ -> "") state false;
    match Sys.command ((!Param.interactive_dot_command) ^ " -Tpng " ^ dot_file_name ^ " -o " ^ png_file_name) with
      0 ->
	let pixbuf = GdkPixbuf.from_file png_file_name in
        !img#set_pixbuf pixbuf;
        trace_box !img#coerce;
        !win_trace#show ()
    | _ ->
       let _ = Menu_helper.question_box "Error" ["Ok"] ("The call to " ^ (!Param.interactive_dot_command) ^ " failed. Please check that it is correctly installed.\n(The program \"" ^ (!Param.interactive_dot_command) ^ "\" must be in the PATH.)") () in
       ()
  in
  let trace_box = create_win_trace () in
  create_trace (trace_box#pack ~expand:true ~fill:true)

(* [refresh_img ()] Refresh the trace if the trace window is open. *)
let refresh_img () =
  let state = Menu_helper.get_state () in
  if !Param.trace_win_open = true then
    begin
      let (dot_file_name, png_file_name) = get_file_names() in
      Display.AttGraph.write_state_to_dot_file dot_file_name Display.term_to_term (fun _ -> "") state false;
      match Sys.command ("dot -Tpng " ^ dot_file_name ^ " -o " ^ png_file_name) with
        0 ->
          let pixbuf = GdkPixbuf.from_file png_file_name  in
          !img#set_pixbuf pixbuf;
      | _ ->
          let _ = Menu_helper.question_box "Error" ["Ok"] "The call to Graphviz failed. Please check that Graphviz is correctly installed.\n(The program \"dot\" must be in the PATH.)" in
          ()
    end

(* [update update_view up_fun view ()] Update the current state applying *)
(* [update_cur_state (up_fun ())]. Update the view according to the new state *)
(* using [update_view]. Create the new model, and associate it with view. *)
(* Refresh the trace is the trace window is open. *)
let rec update update_view up_fun view () =
  let proc_nb_cur state = List.length state.subprocess in
  Menu_helper.frz();
  let old_state = Menu_helper.get_state () in
  let n_state = up_fun () in
  Menu_helper.update_cur_state n_state;
  let diff = proc_nb_cur n_state - proc_nb_cur old_state in
  update_view view diff;
  let (data, n_model) = create_model () in
  view#set_model None;
  update_titles view data;
  Menu_helper.dfrz();
  let _ = view#set_model (Some(n_model#coerce)) in
  refresh_img ()

(* [update_view view diff] Add or suppress [diff] columns to [view]. *)
let rec update_view view diff  =
  (* [add_column_at n] Add a column at position n *)
  let add_column_at n =
    (* [markup] attribute means that we use Pango markup language in the cell, instead *)
    (* of a simple string. Used to color keywords when displaying processes *)
    let col = GTree.view_column ~renderer:(GTree.cell_renderer_text [], ["markup", List.nth col_lst n]) () in
    col#set_clickable true;
    let _ = col#connect#clicked
             ~callback:(update update_view (do_one_reduction_step (n - 3)) view) in
    let _ = view#append_column col in
    col#set_resizable true;
  in
  (* [add_n_columns n] Add n columns at the end of the view. *)
  (* Update the current number of processes *)
  let rec add_n_columns n  = match n with
    | 0 -> ()
    | n ->
       begin
         Menu_helper.inc_proc_nb();
         add_column_at (Menu_helper.get_proc_nb () + 2);
         add_n_columns (n - 1)
       end
  in
  (* [remove_n_columns n] Remove n columns at the end of the view. *)
  (* Update the current number of processes *)
  let rec remove_n_columns n = match n with
    | 0 -> ()
    | n ->
       begin
         let _ = view#remove_column (view#get_column (Menu_helper.get_proc_nb () + 2 )) in
         Menu_helper.dec_proc_nb ();
         remove_n_columns (n - 1)
       end
  in
  begin
    match diff with
    | n when n < 0  ->
       remove_n_columns (-diff)
    | _ ->
       add_n_columns diff
  end

(* [callback_when_view_exists view filew ()] Callback function used by  *)
(* [file_selection_window callback ()] to create a new model and associated to *)
(* to the existing view.  *)
let rec callback_when_view_exists view filew () =
  Menu_helper.set_io_c Other;
  anal_file filew;
  let nop = Menu_helper.get_proc_nb () in
  let nnop = 1 in
  let diff = nnop - nop in
  update_view view diff;
  let (data, n_model) = create_model () in
  update_titles view data;
  view#set_model None;
  let _ = view#set_model (Some(n_model#coerce)) in
  Menu_helper.dfrz()

(* [callback_create_view create_view filew ()] Callback function used by  *)
(* [file_selection_window callback ()] to create the first view and the model associated to *)
(* it after the parsing of the input file. *)
let rec callback_create_view create_view filew () =
  Menu_helper.set_model_build true;
  try
    anal_file filew;
    let (data, model) = create_model () in
    let _ = create_view data model in
    ()
  with
  | exc ->  Menu_helper.set_model_build false; raise exc

(* [file_selection_window callback ()] Open a file selection *)
(* dialog window. The [callback] function *)
(*  is instanced either by [callback_create_view create_view] *)
(* to create the view, or by *)
(* [callback_when_view_exists view] if the view already exists. *)
let rec file_selection_window callback () =
  Menu_helper.frz();
  (* Close trace windows if it's open *)
  if !Param.trace_win_open then
    begin
      !win_trace#destroy ();
      Param.trace_win_open := false
    end;
  let dialog = GWindow.file_chooser_dialog ~action:`OPEN ~title:"Open File" () in
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_select_button_stock `OPEN `OPEN;
  let _ = dialog#set_current_folder (!dir) in
  dialog#add_filter (filter "pv");
  begin
    match dialog#run () with
    | `OPEN ->
       begin
         match dialog#filename with
         | None -> ()
         | Some s ->
            update_dir (dialog#current_folder);
            dialog#destroy();
           begin
             try
               callback s ();

             with
             | _ ->  dialog#destroy ();
               file_selection_window callback ()
           end;
       end
    | `DELETE_EVENT | `CANCEL ->
	Menu_helper.dfrz();
	if !state_destroyed then
	  destroy_main_win ()
	else
	  dialog#destroy ()
  end

(* [create_button_box direction title spacing child_width child_height layout view] Use *)
(* to add buttons in the main window. *)
let create_button_box direction title spacing child_width child_height layout view =
  let frame = GBin.frame  () in
  let bbox = GPack.button_box direction ~border_width:5 ~layout
    ~child_height ~child_width  ~spacing ~packing:frame#add () in
  (* add buttons to bbox. *)
  bbox#add b_step_auto#coerce;
  bbox#add b_all_auto#coerce;
  bbox#add b_bstep#coerce;
  bbox#add b_new_name#coerce;
  bbox#add b_new_public#coerce;
  (* add callbacks to buttons. *)
  let _ = b_all_auto#connect#clicked ~callback:(update update_view (do_all_auto_reduction) view) in
  let _ = b_bstep#connect#clicked ~callback:(update update_view one_step_backward view) in
  let _ = b_step_auto#connect#clicked ~callback:(update update_view (next_auto_step)  view) in
  let _ = b_new_name#connect#clicked ~callback:(update update_view create_new_nonce view) in
  let _ = b_new_public#connect#clicked ~callback:(update update_view create_new_public view) in
  frame#coerce

(* [click_on_tables view ()] Callback function for Show/hide tables item *)
let click_on_tables view () =
  show_tables_bool := click_on_show view !show_tables_bool 0


(* [click_on_events view ()] Callback function for Show/hide events item *)
let click_on_events view () =
  show_events_bool := click_on_show view !show_events_bool 1

let set_menu_items view =
  (* Connect callbacks to menu items *)
  let _ = (Menu_helper.get_file_select_item ())#connect#activate (file_selection_window  (callback_when_view_exists view)) in
  let _ = (Menu_helper.get_next_auto_item ())#connect#activate (update update_view (next_auto_step) view)  in
  let _ = (Menu_helper.get_quit_item ())#connect#activate destroy_main_win  in
  let _ = (Menu_helper.get_all_auto_item ())#connect#activate (update update_view (do_all_auto_reduction) view) in
  let _ = (Menu_helper.get_step_back_item ())#connect#activate (update update_view one_step_backward view) in
  let _ = (Menu_helper.get_create_nonce_item ())#connect#activate (update update_view create_new_nonce view) in
  let _ = (Menu_helper.get_create_public_item ())#connect#activate (update update_view create_new_public view) in
  let _ = (Menu_helper.get_display_trace_item ())#connect#activate display_trace in
  let _ = (Menu_helper.get_show_tables_item ())#connect#activate (click_on_tables view) in
  let _ = (Menu_helper.get_show_events_item ())#connect#activate (click_on_events view) in
  ()
(* [window_menu view] Add buttons, menu, and view in the main window. *)
let window_menu view =
  set_menu_items view;
  (* Create buttons *)
  let buttons = create_button_box `HORIZONTAL "" 5 5 5 `START view in
  let box_menu = Menu_helper.get_menu_box () in
  (* Pack buttons and view to box_menu *)
  box_menu#pack buttons#coerce;
  box_menu#pack ~expand:true view#coerce

(* Create the view and fill the column titles with the good information, set all parameters for *)
(* the view. The first column contains public elements and is not clickable. *)
(* Any Other nth-column is  clickable and each callback allow to do a reduction step? *)
let rec create_view data model =
  view#set_model (Some model#coerce);
  view#selection#set_mode  `NONE;
  let _ = window_menu view in
  (* Create the view columns. "markup" is used for Pango language. *)
  ignore(List.iteri (fun n d ->
    let col = GTree.view_column ~title:d ~renderer:(GTree.cell_renderer_text [], ["markup", List.nth col_lst n]) () in
    if n <= 2 then
      begin
        (* Tables, events, and public columns are not clickable *)
        col#set_clickable false;
        if n <= 1 then
          (* By default, events and public columns are not visible *)
          col#set_visible false
      end
    else
      begin
        col#set_clickable true;
        (* The callback reduction step is done on n - 3 since there is the column public and *)
        (* the columns events, and tables. *)
        ignore(col#connect#clicked ~callback:(update update_view  (do_one_reduction_step (n - 3)) view));
      end;
    ignore (view#append_column col);
    col#set_resizable true;
  ) (List.map fst data.titles));
  window_p#show();
  Menu_helper.dfrz ();
  GMain.Main.main ();
  view

(* [main_window fileopt] If [fileopt = None] launch the file *)
(* dialog box which allows the user to choose the *)
(* file containing the process to emulate. *)
(* Otherwise [fileopt = Some s], and the emulator starts *)
(* emulating the process represented in file of path [s]. *)
(* If [s] does not exists, or the parsing went wrong, acts *)
(* like in the case [fileopt = None] *)
let main_window fileopt =
  Param.allow_tilde := true;
  Parsing_helper.interactive_mode := true;
  match fileopt with
    None ->
      let _ = file_selection_window (callback_create_view create_view) () in
      ()
  | Some file ->
     try
       callback_create_view create_view file ()
     with
     | _ -> file_selection_window (callback_create_view create_view) ()
