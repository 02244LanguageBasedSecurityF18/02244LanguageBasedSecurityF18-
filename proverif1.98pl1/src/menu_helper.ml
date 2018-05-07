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
open Types
open Pitypes
open Terms

exception WrongChoice

let debug_print s = ()
  (* print_endline s *)

(* [initialize_state ()] Use to initialize or reset [cur_state] *)
let initialize_state () =
  { goal = NoGoal;
    subprocess = [];
    public = [];
    tables = [];
    prepared_attacker_rule = [];
    io_rule = [];
    previous_state = None;
    hyp_not_matched = [];
    assumed_false = [];
    current_phase = 0;
    comment = RInit;
    events = [];
    barriers = []}

let max_proc_nb = 1024

let cur_proc_nb = ref 1

let get_proc_nb () = !cur_proc_nb

let no_more_proc () = !cur_proc_nb >= max_proc_nb

let inc_proc_nb () = cur_proc_nb := !cur_proc_nb + 1

let dec_proc_nb () = cur_proc_nb := !cur_proc_nb - 1

(* [cur_state] A reference on the curent state given first by the parsing of the selected file. *)
(* This reference changes after every reduction step *)
let cur_state = ref (initialize_state ())

(* [get_state ()] Return the current state *)
let get_state () = !cur_state

(* [sub_of_proc proc] Return the subprocess corresponding to [proc] *)
let sub_of_proc proc = (proc, [], [], [], Nothing)

(* [proc_of_sub proc] Return the process corresponding to [sub] *)
let proc_of_sub sub =
  let (p, _, _, _, _) = sub in
  p

(* [get_proc state n] Return the n-th process in state.subprocess *)
let get_proc state n =
  let (proc, _, _, _, _) = List.nth state.subprocess n in
  proc

(* [get_proc_list state] Return the list of process obtain from state.subprocess *)
let get_proc_list state =
  let rec aux acc = function
      [] -> List.rev acc
    | (proc, _, _, _, _)::tl -> aux (proc::acc) tl
  in
  aux [] state.subprocess

(* [only_one_proc state] Return true if state.suprocess contains only one process, false otherwise. *)
let only_one_proc state = match state.subprocess with
  | p::[] -> true
  | _ -> false

(* [is_auto_reductible state p] Return true if the p is auto reductible, according to state, *)
(* false otherwise *)
let is_auto_reductible state p =
  match p with
  | Nil | Par(_, _) | Restr(_, _, _, _) | Let(_, _, _, _, _)
  | NamedProcess(_ , _, _) | Event( _, _, _, _) | Test(_, _, _, _) -> true
  | Output(tc, t, _, _)  ->
    begin
      try
        let tc' = Evaluation_helper.term_evaluation_fail tc in
        let _ = Evaluation_helper.term_evaluation_fail t in
        match Evaluation_helper.is_in_public state.public tc' with
        | None -> false
        | Some _ -> true
      with Unify ->
        true
    end
  | Input(tc, _, _, _) ->
     begin
      try
        let _ = Evaluation_helper.term_evaluation_fail tc in
	false
      with Unify ->
        true
     end
  | Insert (t, _, _)->
      begin
	try
	  let _ = Evaluation_helper.term_evaluation_fail t in
	  only_one_proc state
	with Unify ->
	  true
      end
  | _ -> false

(* [string_of_proc_first_line state proc] Return the string witch will be displayed in the first *)
(* column of the store to represent [proc], in respect to [state] *)
let string_of_proc_first_line state proc =
  match proc with
  | Nil -> "Nil"
  | Par(_, _) ->  "Par"
  | Repl(_, _)-> "Replication"
  | Restr(_, _, _ , _) -> "Restriction"
  | Test(_, _, _, _) -> "Test"
  | Input(tin, _, _, _) ->
    begin
      try
        let tin' = Evaluation_helper.term_evaluation_fail tin in
        match Evaluation_helper.is_in_public state.public tin' with
        | None -> "Input (private)"
        | Some _ -> "Input (public)"
      with Unify ->
        "Input (channel fails)"
    end
  | Output(tc, _, _, _) ->
    begin
      try
        let tc' = Evaluation_helper.term_evaluation_fail tc in
        match Evaluation_helper.is_in_public state.public tc' with
        | None -> "Output (private)"
        | Some _ -> "Output (public)"
      with Unify ->
        "Output (channel fails)"
    end
  | Let(_, _, _, _, _) -> "Let"
  | LetFilter(_, _, _, _, _) -> "let...suchthat"
  | Event(_, _, _, _) -> "Event"
  | Insert(_, _, _)-> "Insert"
  | Get(_, _, _, _, _) -> "Get"
  | Phase(_, _, _) -> "Phase"
  | NamedProcess(s, _, _) -> "Process" ^ s
  | _ -> "Other"


let equal_io_oi x y =
  match x, y with
  | (I_O (cin, _, _, _), O_I (cout, _, t, _))
  | (O_I (cout, _, t, _), I_O (cin, _, _, _)) ->
      begin
	try
          let cin' = Evaluation_helper.term_evaluation_fail cin in
          let cout' = Evaluation_helper.term_evaluation_fail cout in
          let _ = Evaluation_helper.term_evaluation_fail t in
	  Reduction_helper.equal_terms_modulo cin' cout'
	with Unify ->
	  false
      end
  | _ -> false

(* [public_build l] Initial attacker knowledge *)
let rec public_build l =
  match l with
  | [] -> []
  | h::l' ->
    if not h.f_private then
      let t = (FunApp(h,[])) in
      (t, t)::(public_build l')
    else
      public_build l'

let string_of_events t = Display_interact.GtkInteract.display_term t

(* [reduc_state_of_proc proc] Return the term Pitypes.reduc_state build from [proc] *)
let reduc_state_of_proc proc =
  {goal = NoGoal;
   subprocess = [sub_of_proc proc];
   public = public_build !Param.freenames;
   tables = [];
   prepared_attacker_rule = [];
   io_rule = [];
   previous_state = None; hyp_not_matched = [];
   assumed_false = [];
   current_phase = 0;
   comment = RInit;
   events = [];
   barriers = []
  }

(* [get_data_from_state state] Return the data which allow to create the model from [state]. *)
let get_data_from_state () =
  let string_lst_of_barriers barriers = [] (* TO DO *)
  in
  let state = get_state () in
  let exists_auto = ref false in
  let plst = get_proc_list state in
  let rec aux n tlst plst = function
    | [] -> (List.rev tlst, List.rev plst)
    | p::tl ->
       begin
         if is_auto_reductible state p then
           exists_auto := true;
         let pt = string_of_proc_first_line state p in
         let sp = List.rev (Display_interact.GtkInteract.display_process p) in
         let is_io_p = match p with
             Input (tin, pat, p, _) -> I_O (tin, n, pat, p)
           | Output (tou, t, q, _) -> O_I (tou, n, t, q)
           | _ -> Other in
         aux (n + 1) ((pt,is_io_p)::tlst) (sp::plst) tl
       end
  in
  let last_id = ref None in
  let add_space_if_needed tables =
    let rec aux acc = function
      [] -> List.rev acc
    | hd::tl ->
       begin
         match hd with
           FunApp(f, _) as t ->
            let t' = Display_interact.GtkInteract.display_term t in
            begin
              match !last_id with
                None ->
               last_id := Some f.f_name;
               aux (t'::acc) tl
              | Some(f') ->
                 if not (f.f_name = f') then
                   begin
                     last_id := Some f.f_name;
                     aux (t'::""::acc) tl
                   end
                 else
                   aux (t'::acc) tl
            end
         | _ -> failwith "add_space_if_needed"
       end
    in
    aux [] tables
  in
  let tables_lst = add_space_if_needed state.tables in
  let public_lst = Display_interact.GtkInteract.display_cpl_lst state.public in
  let barriers_lst = string_lst_of_barriers () in
  let (titles, proc_llst) = aux (-3) [] [] plst in (* -3 since there is columns titles, events and tables *)
  let events_lst = List.map string_of_events state.events in
  {tables_lst; public_lst; titles =("Tables", Other)::("Events", Other)::("Public", Other)::titles; proc_llst; no_auto = not !exists_auto; events_lst; barriers_lst}


(* [cur_data] Reference on the current data associated to [!cur_state] *)
let cur_data = ref (get_data_from_state ())

(* [get_data ()] Return the current data associated to the current state *)
let get_data () = !cur_data

(* [is_first_step ()] Return true if the current state is the initial state, false otherwise *)
let is_first_step () = !cur_state.previous_state = None

(* [exists_auto ()] Return true if there exists a auto-reductible process in one of the *)
(* subprocess of the current state, false otherwise *)
let exists_auto () = not (!cur_data.no_auto)

(* [update_cur_state state events_ barriers_] Update the current state with state events barriers, and then update the current data *)
let update_cur_state state =
  cur_state := state;
  cur_data := get_data_from_state ()

let model_build = ref false

(* [set_model_build b] Set the reference model_build to b *)
let set_model_build b = model_build := b

(* [model_is_build ()] Return true if a file has already been load, false otherwise *)
let model_is_build () = !model_build

(* TreeView *)

let _ = GtkMain.Main.init ()

(* [window_p] Main Window of the interactive mode *)
let window_p = GWindow.window ~title:"Proverif Interact" ()

(* [get_window_p ()] Return the main window of the interactive mode *)
let get_window_p () = window_p


(* [view] The treeview used to represent the data. It contains a column list *)
let view = GTree.view ()
let _ = view#set_resize_mode `QUEUE

(* [get_view ()] Return the treview used to represent the data *)
let get_view () = view

let cols = new GTree.column_list

(* [get_cols ()] Return the colum list linked to the view *)
let get_cols () = cols

let col_lst =
  let rec create_n_cols acc = function
    | 0 -> List.rev acc
    | n -> let col = cols#add Gobject.Data.string in create_n_cols (col::acc) (n - 1)
  in create_n_cols [] (max_proc_nb + 3)

(* [get_col_lst ()] Return the list of Gtree.column of type string associated to our model *)
let get_col_lst () = col_lst

let scrolled_window = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:window_p#add  ()

(* Box for the menu *)
let menu_box = GPack.vbox ~homogeneous:false ~packing:scrolled_window#add_with_viewport ()
  (* Menu bar *)

let get_menu_box () = menu_box

let menu_bar = GMenu.menu_bar ~packing:menu_box#pack ()

let get_menu_bar () = menu_bar

let factory = new GMenu.factory menu_bar

let get_factory () = factory

let accel_group = factory#accel_group

let _ = window_p#add_accel_group accel_group

let file_menu = factory#add_submenu "File"

(* Widgets and getter functions for the interface *)

(* File factory. Callbacks are defined in menu_interact for some items *)
let factory_file = new GMenu.factory file_menu ~accel_group

let get_factory_file () = factory_file

let file_select_item = factory_file#add_item "Load File" ~key:_L

let get_file_select_item () = file_select_item

let quit_item = factory_file#add_item "Quit" ~key:_Q

let get_quit_item () = quit_item

let reduct_menu = factory#add_submenu "Reduction"

(* Reduct factory *)
let factory_reduct = new GMenu.factory reduct_menu ~accel_group

let next_auto_item = factory_reduct#add_item "Next auto-reduction" ~key:_N

let get_next_auto_item () = next_auto_item

let all_auto_item = factory_reduct#add_item "Make auto-reductions" ~key:_A

let get_all_auto_item () = all_auto_item

let step_back_item = factory_reduct#add_item "One step backward" ~key:_B

let get_step_back_item () = step_back_item

let create_nonce_item = factory_reduct#add_item "Create a nonce" ~key:_C

let get_create_nonce_item () = create_nonce_item

let create_public_item = factory_reduct#add_item "Add a public term" ~key:_P

let get_create_public_item () = create_public_item


let display_trace_item = factory_reduct#add_item "Display trace" ~key:_D

let get_display_trace_item () = display_trace_item

let show_menu = factory#add_submenu "Show"

(* Widgets and getter functions for the interface *)

(* File factory. Callbacks are defined in menu_interact for some items *)
let factory_show = new GMenu.factory show_menu ~accel_group

let show_tables_item = factory_show#add_item "Show/hide tables" ~key:_T

let get_show_tables_item () = show_tables_item

let show_events_item = factory_show#add_item "Show/hide events" ~key:_E

let get_show_events_item () = show_events_item

(* Button [b_all_auto] Allow to make all auto-reduction *)
let b_all_auto = GButton.button ~label:"Make auto-reductions" ()

let get_b_all_auto () = b_all_auto

(* Button [b_bstep] Allow to go a reduction step Backward *)
let b_bstep = GButton.button ~label:"One step backward" ()

let get_b_bstep () = b_bstep

(* Button [b_step_auto] Allow to make one step of auto *)
let b_step_auto = GButton.button ~label:"Next auto-reduction" ()

let get_b_step_auto () = b_step_auto

(* Button [b_new_name] Allow to create a new name and add it into state.public *)
let b_new_name = GButton.button ~label:"Create a nonce" ()

let get_b_new_name () = b_new_name

(* Button [b_new_public] Allow to create a new public term and add it into state.public *)
let b_new_public = GButton.button ~label:"Add a public term" ()

let get_b_new_public () = b_new_public

(* [sets_button b bool] Set the sensitivity of a button [b] according to [bool] *)
let sets_button b bool =
  ignore(b#coerce#misc#set_sensitive bool)

let sets_obj obj bool =
  ignore(obj#misc#set_sensitive bool)

let _ = sets_button b_bstep false

(* |sets_b_bool b] Set the sensitivity of the main windows widgets according to [b] *)
(* and the current state *)
let sets_b_bool b =
  sets_button b_all_auto b;
  sets_button b_step_auto b;
  sets_button b_new_name b;
  sets_button b_new_public b;
  sets_obj menu_bar b;
  sets_obj next_auto_item b;
  sets_obj all_auto_item b;
  sets_obj step_back_item b;
  sets_obj file_select_item b;
  sets_obj display_trace_item b;
  sets_obj create_nonce_item b;
  sets_obj create_public_item b;
  sets_obj quit_item b;
  sets_obj show_tables_item b;
  sets_obj show_events_item b;
  if not (exists_auto ()) then
    (* When there is no auto reduction possible, all buttons to make such reductions *)
    (* become insensitives *)
    begin
      sets_button b_all_auto false;
      sets_button b_step_auto false;
      sets_obj next_auto_item false;
      sets_obj all_auto_item false;
    end;
  sets_button b_bstep b;
  if is_first_step () then
    (* When it's the first step, all buttons to go forward become in-sensitives *)
    begin
      sets_button b_bstep false;
      sets_obj step_back_item false;
      sets_obj display_trace_item false;
    end;
  (* When the trace window is open, we locked the display_trace_item *)
  if !Param.trace_win_open = true then
    sets_obj display_trace_item false;
  let rec sets_columns n = match n with
    | -1 -> ()
    | n ->
       begin
         let col = view#get_column (n) in
         if n < 3 then
           col#set_clickable false
         else
           col#set_clickable b;
         sets_columns (n - 1)
       end
  in
  if !model_build then
    sets_columns (get_proc_nb () + 2)

(* [frz ()] Freeze the necessary elements in the main window *)
let frz () = sets_b_bool false

(* [dfrz ()] De-freeze the necessary elements in the main window *)
let dfrz () = sets_b_bool true

(* [dialog_box title string ()] Create a dialog box with title [title], displaying the string *)
(* string [string]. Return the string enter by the user, raise WrongChoice if no string is entered. *)
let dialog_box title string () =
  let db = GToolbox.input_string ~title ~text:"" string in
  match db  with
    Some s -> s
  | None -> dfrz(); raise WrongChoice

(* [question_box title buttons string] Create a question box with title [title], displaying *)
(* tshe string [string]. This box has a list of buttons labelled by the string in the string list *)
(* [buttons]. It returns the number of the clicked button, starting from 1, 0 otherwise. *)
let question_box title buttons string () =
  match GToolbox.question_box title buttons string with
    | 0 -> dfrz(); 0
    | n -> n

(* [reset_env ()] Reset the global environment, clear tables, restore some parameters. *)
(* Used to load a new file *)
let reset_env () =
  cur_state := initialize_state ()

(* [input_error_box b mess ext] Create a message box with title "Error in your Recipe", and one *)
(* button. The message displayed is comming from an InputError(mess, ext) exception. If [b] *)
(* is true, the the message display the line number and the character number of the error. *)
(* Otherwise, its only display the character number *)
let input_error_box b mess ext =
  let mess' = Parsing_helper.get_mess_from b "Error: " mess ext in
  let _ = question_box "Error" ["OK"] mess' () in
  ()

(* [get_binder_name b] Return the string associated to the binder b *)
let get_binder_name b =
  if b.vname = 0 then b.sname else  b.sname ^ "_" ^ (string_of_int b.vname)

(* [add_var_env env b] Add the binder b to env *)
let add_var_env env b =
  let s = get_binder_name b in
  Stringmap.StringMap.add s (EVar b) env

let args_to_string tl =
  let l = List.length tl in
  if l=0 then
    "0 argument"
  else if l=1 then
    "1 argument of type " ^ (Terms.tl_to_string ", " tl)
  else
    (string_of_int l) ^ " arguments of types " ^ (Terms.tl_to_string ", " tl)

let type_compatible ty1 ty2 =
  ty1 == ty2 || (Param.get_ignore_types() && (ty1 == Param.any_type || ty2 == Param.any_type))

let rec compatible_lists l1 l2 =
  match l1,l2 with
    [],[] -> true
  | a1::q1,a2::q2 -> (type_compatible a1 a2) && (compatible_lists q1 q2)
  | _,_ -> false

let type_error mess ext =
  if Param.get_ignore_types() then
    Parsing_helper.input_warning (mess ^ (Parsing_helper.add_point_if_necessary mess) ^
				  "\nThis is acceptable because types are ignored.") ext
  else
    Parsing_helper.input_error mess ext

let rec split_string s =
  try
    let pos_first_dash = String.index s '-' in
    let s1 = String.sub s 0 pos_first_dash in
    let s2 = String.sub s (pos_first_dash+1) (String.length s -pos_first_dash-1) in
    s1 :: split_string s2
  with Not_found ->
    [s]

let rec id_list_to_types = function
    [] -> raise Not_found
  | ["tuple"] -> []
  | [_] -> raise Not_found
  | a::r ->
      let ty = List.find (fun t -> t.tname = a) (!Param.all_types) in
      let ty_r = id_list_to_types r in
      ty::ty_r



let rec check_eq_term env (term,ext) =
  match term with
  | Pitptree.PFail -> None
  | (Pitptree.PIdent (s,ext)) ->
    let t =
      try
	match Stringmap.StringMap.find s env with
	| EVar v -> Var v
	| EFun f ->
	    if fst f.f_type <> [] then
	      Parsing_helper.input_error ("function " ^ s ^ " expects " ^
					  (string_of_int (List.length (fst f.f_type))) ^
					  " arguments but is used without arguments") ext;
            if f.f_private then
              Parsing_helper.input_error ("identifier " ^ f.f_name ^ " is private") ext;
            FunApp(f, [])
        | EName r ->
            if r.f_private then
              Parsing_helper.input_error ("identifier " ^ r.f_name ^ " is private") ext;
            FunApp(r, [])
	| _ -> Parsing_helper.input_error ("identifier " ^ s ^ " should be a function, a variable, or a name") ext
      with Not_found ->
	Parsing_helper.input_error ("identifier " ^ s ^ " not defined") ext
    in
    Some (t, Terms.get_term_type t)
  | (Pitptree.PFunApp ((f,ext), tlist)) ->
     begin
       let tol = List.map (check_eq_term env) tlist in
       match f, tol with
	 "=", [to1;to2] ->
	   begin
             match to1, to2 with
             | Some (t1, ty1), Some (t2, ty2) ->
		 if not (type_compatible ty1 ty2) then
		   type_error ("function " ^ f ^ " expects two arguments of same type but is here given " ^ args_to_string [ty1;ty2]) ext;
		 Some(FunApp(Terms.equal_fun ty1, [t1; t2]), Param.bool_type)
             | _ ->
		 Some(get_fail_term Param.bool_type, Param.bool_type)
	   end
       | "<>", [to1;to2] ->
	   begin
	     match to1, to2 with
	     | Some (t1, ty1), Some (t2, ty2) ->
		 if not (type_compatible ty1 ty2) then
		   type_error ("function " ^ f ^ " expects two arguments of same type but is here given " ^
    		          args_to_string [ty1;ty2]) ext;
		 Some(FunApp(Terms.diff_fun ty1, [t1; t2]), Param.bool_type)
             | _ ->
		 Some(get_fail_term Param.bool_type, Param.bool_type)
	   end
       | ("=" | "<>"), _ ->
	   Parsing_helper.input_error (f ^ " expects two arguments") ext
       | "choice", _ ->
	   Parsing_helper.input_error "choice is not allowed in recipes" ext;
       | _ ->
	   try
	     match Stringmap.StringMap.find f env with
               EFun r ->
		 let (tl', tyl) =
		   List.split (List.map2 (fun ty t ->
		     match t with
		     | None -> (get_fail_term ty, ty)
		     | Some (t, ty') -> (t, ty')
			   ) (fst r.f_type) tol)
		 in
		 if (List.length (fst r.f_type)) != List.length tyl then
      		   Parsing_helper.input_error ("function " ^ f ^ " expects " ^
        				       args_to_string (fst r.f_type) ^
        				       " but is here given " ^
        				       args_to_string tyl) ext;
      		 if not (compatible_lists (fst r.f_type) tyl) then
      		   type_error ("function " ^ f ^ " expects " ^
        		       args_to_string (fst r.f_type) ^
        		       " but is here given " ^
        		       args_to_string tyl) ext;
		 if r.f_private then
		   Parsing_helper.input_error ("identifier " ^ r.f_name ^ " is private") ext;
		 if (r.f_options land Param.fun_TYPECONVERTER != 0) && (Param.get_ignore_types()) then
		   match tl' with
		     [t] -> Some (t, snd r.f_type)
		   | _ -> Parsing_helper.input_error ("type converter functions should always be unary") ext
		 else
		   Some (FunApp(r, tl'), snd r.f_type)
	     | x -> Parsing_helper.input_error (f ^ " should be a function") ext
	   with Not_found ->
	     Parsing_helper.input_error (f ^ " not defined") ext
     end
  | (Pitptree.PTuple tlist) ->
    let tl' = List.map (check_eq_term env) tlist in
    let (tl, tyl) =
      List.split (List.map (function
	| None ->
	    let ty = Param.bitstring_type in
	    (get_fail_term ty, ty)
	| Some (t',ty) ->
	    (t', ty)) tl')
    in
    Some (FunApp (Terms.get_tuple_fun tyl, tl), Param.bitstring_type)

  | (Pitptree.PProj((f, ext), t)) ->
      let t' = check_eq_term env t in
      (* f is of the form "<integer>-proj-<identifier>"
	 <integer> is the position of the element extracted by the projection
	 <identifier> is the corresponding tuple function *)
      let f_split = split_string f in
      match f_split with
	n_str :: "proj" :: id_list ->
	  begin
	    let n = int_of_string n_str in
	    let tuple_fun =
	      match id_list with
		[fun_name] ->
		  begin
		    try
		      match Stringmap.StringMap.find fun_name env with
			EFun r when r.f_cat == Tuple ->
			  r
		      | _ ->
			  Parsing_helper.input_error ("Projection " ^ f ^ " should refer to a [data] function") ext
		    with Not_found ->
		      Parsing_helper.input_error ("Function " ^ fun_name ^ " not defined. Projection " ^ f ^ " should refer to a [data] function") ext
		  end
	      | _ ->
		  if Param.get_ignore_types() then
		    (* The default tuple functions are written <n'>-tuple *)
		    match id_list with
		      [n_str'; "tuple"] ->
			begin
			  try
			    let n' = int_of_string n_str' in
			    let tl = Terms.copy_n n' Param.any_type in
			    Terms.get_tuple_fun tl
			  with _ ->
			    Parsing_helper.input_error "After -proj-, we accept either an existing [data] function or a default tuple function written <n>-tuple" ext
			end
		    | _ ->
			Parsing_helper.input_error "After -proj-, we accept either an existing [data] function or a default tuple function written <n>-tuple" ext
		  else
		    (* The default tuple functions are written <typelist>-tuple *)
		    try
		      let tl = id_list_to_types id_list in
		      Terms.get_tuple_fun tl
		    with Not_found ->
		      Parsing_helper.input_error "After -proj-, we accept either an existing [data] function or a default tuple function written <type-list>-tuple" ext
	    in
	    if (n < 1) || (n > List.length (fst tuple_fun.f_type)) then
	      Parsing_helper.input_error ("Component does not exist in projection " ^ f) ext;
	    let proj_fun = Terms.projection_fun (tuple_fun, n) in
	    match t' with
	      Some(t'', ty) ->
		if not (type_compatible ty (snd tuple_fun.f_type)) then
		  type_error ("function " ^ f ^ " expects " ^
        		      args_to_string (fst proj_fun.f_type) ^
        		      " but is here given " ^
        		      args_to_string [ty]) ext;
		Some (FunApp(proj_fun, [t'']), snd proj_fun.f_type)
	    | None ->
		let t'' = Terms.get_fail_term (snd tuple_fun.f_type) in
		Some (FunApp(proj_fun, [t'']), snd proj_fun.f_type)
	  end
      | _ -> Parsing_helper.internal_error "Bad projection name"

exception WarningsAsError

(* [parse_term string] Return the term corresponding to the parsing of [string]. *)
(* Call input_error if the parsing went wrong *)
let parse_term s =
  let lexbuf = Lexing.from_string s in
  let ptree =
    try
      Pitparser.term Pitlexer.token lexbuf
    with
      Parsing.Parse_error -> Parsing_helper.input_error ("Syntax error") (Parsing_helper.extent lexbuf)
  in
  let global_env = Pitsyntax.get_global_env () in
  let public = (get_state ()).public in
  let env = List.fold_left (fun accu (recipe,_) ->
    match recipe with
      Var x -> add_var_env accu x
    | FunApp(n,[]) ->
	begin
	  match n.f_cat with
	    Name _ -> Stringmap.StringMap.add n.f_name (EName n) accu
	  | _ -> accu
	end
    | _ -> accu) global_env public
  in
  let term =
    match check_eq_term env ptree with
    | None -> get_fail_term Param.bitstring_type
    | Some(t, _) -> t
  in
  let warnings = Parsing_helper.get_warning_list() in
  if warnings != [] then
    begin
      let messages = String.concat "" (List.map (fun (mess, ext) ->
	(Parsing_helper.get_mess_from false "Warning: " mess ext) ^ "\n") warnings) in
      let messages = messages ^ "Do you want to continue?" in
      match (question_box "Warnings" ["Yes"; "No"] messages ()) with
	0 | 1 -> ()
      | _ -> raise WarningsAsError
    end;
  term

(* [delete_NamedProcess state] Apply all the possible NamedProcess reductions to state *)
let delete_NamedProcess state =
  let proc_nb = List.length state.subprocess  in
  let rec aux state n =
    if n = proc_nb then state
    else
      let proc = get_proc state n in
          match proc with
          | NamedProcess (name, l, p') ->
                 let n_sub = sub_of_proc p' in
                 let n_state =
                   {state with
                     subprocess = Reduction_helper.replace_at n n_sub state.subprocess;
                     comment = RNamedProcess (n, name, l);
                     previous_state = Some state
                   } in
                   aux n_state n;
          | _ -> aux state (n + 1)
  in
  aux state 0

(* Use for RIO reductions *)
let io_c = ref Pitypes.Other

let set_io_c c = io_c := c

let get_io_c () = !io_c


(* [get_recipe pref text] Display a dialog box with title "Give Recipe". *)
(* [pref] is used to display error messages if the user makes a mistake. *)
(* [text] is the message displayed in the dialog box *)
let rec get_recipe pref text =
  let s = dialog_box  "Give Recipe" (pref ^ "\n" ^ text)  () in
     try
       parse_term s
     with
       Parsing_helper.InputError(mess, extent) ->
         let mess' = Parsing_helper.get_mess_from false "Error: " mess extent in
         if s = "" then
           get_recipe "" text
         else
           get_recipe mess' text
     | WarningsAsError -> get_recipe "" text

(* [expand_recipe public recipe] expand the [recipe] according to equations in public, *)
(* and returns the obtained term *)
let expand_recipe public recipe =
  Terms.auto_cleanup (fun () ->
    List.iter (function
	(Var x,t') -> link x (TLink t')
      | _ -> ()) public;
    copy_term3 recipe)
