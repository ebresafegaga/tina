open Syntax
open Naming
module A = Ast
module T = Type

let report_expected_at ~expected ~got ~loc =
  let msg =
    Printf.sprintf "Expected a %s but got a %s at %s" (T.pp_ty expected)
      (T.pp_ty got) (Loc.pp loc)
  in
  Errors.runtime msg

let report_error_at msg loc =
  let msg = Printf.sprintf "%s at %s" msg (Loc.pp loc) in
  Errors.runtime msg

let rec synth ctx term =
  match term with
  | A.LitTodo _ | A.Absurd _ ->
    Errors.runtime "can't infer a type for TODO or absurd"
  | A.Variable (_loc, name) -> Ctx.lookup ctx name
  | A.LitUnit _loc -> T.TyUnit
  | A.LitBool (_loc, _b) -> T.TyBool
  | A.LitFloat (_loc, _f) -> T.TyFloat
  | A.LitInteger (_loc, _i) -> T.TyInt
  | A.LitString (_loc, _s) -> T.TyString
  | A.If (loc, _p, _pt, _pf) ->
    report_error_at "Can't infer a type for the if expression" loc
  | A.Sequence (loc, _, _) ->
    report_error_at "Can't infer a type for the sequence expression" loc
  | A.Fn (loc, _, _) ->
    report_error_at "Can't infer the type of the function" loc
  | A.Application (loc, f, args) -> (
      let f_ty = synth ctx f in
      match f_ty with
      | T.TyArrow (args_ty, ret_ty) ->
        List.combine args args_ty |> List.iter (fun (a, t) -> check ctx a t);
        ret_ty
      | _ -> report_error_at "Expected a function" loc)
  | A.Annotated (_loc, term, ty) ->
    check ctx term ty;
    ty
  | A.Tuple (_loc, es) ->
    let ts = List.map (synth ctx) es in
    T.TyTuple ts
  | A.Record (_loc, name, fields) ->
    let fields_ty = Ctx.lookup_record ctx name in
    (* check that all labels of fields_ty is present in fields *)
    fields_ty
    |> List.iter (fun (label, ty) ->
        match List.assoc label fields with
        | expr -> check ctx expr ty
        | exception Not_found ->
          let msg =
            Printf.sprintf "This record is expected to have field %s"
              (FieldName.to_string label)
          in
          Errors.runtime msg);
    T.TyRecord fields_ty
  | A.RecordIndex (loc, record, name) -> (
      let rec_ty = synth ctx record in
      match rec_ty with
      | T.TyRecord fields -> (
          match List.assoc_opt name fields with
          | Some t -> t
          | None ->
            let msg = Printf.sprintf "The field name %s is not defined on this record" (FieldName.to_string name) in
            Errors.runtime msg)
      | _ -> report_error_at "expected a record type" loc)
  | A.Variant (loc, name, es) ->
    let vt, ts = Ctx.lookup_variant ctx name in
    if List.length ts <> List.length es then
      report_error_at "Variant argument length mismatch" loc
    else
      List.combine es ts
      |> List.iter (fun (e, t) -> check ctx e t);
    T.TyVariant vt

and check ctx term ty =
  match term with
  | A.LitBool (loc, _) -> (
      match ty with
      | T.TyBool -> ()
      | _ -> report_expected_at ~expected:ty ~got:T.TyBool ~loc)
  | A.LitInteger (loc, _) -> (
      match ty with
      | T.TyInt -> ()
      | _ -> report_expected_at ~expected:ty ~got:T.TyInt ~loc)
  | A.LitString (loc, _) -> (
      match ty with
      | T.TyString -> ()
      | _ -> report_expected_at ~expected:ty ~got:T.TyString ~loc)
  | A.LitFloat (loc, _) -> (
      match ty with
      | T.TyFloat -> ()
      | _ -> report_expected_at ~expected:ty ~got:T.TyFloat ~loc)
  | A.LitUnit loc -> (
      match ty with
      | T.TyUnit -> ()
      | _ -> report_expected_at ~expected:ty ~got:T.TyUnit ~loc)
  | A.Variable (loc, name) ->
    let ty_i = Ctx.lookup ctx name in
    let msg =
      Printf.sprintf "Expected a %s but got a %s" (T.pp_ty ty) (T.pp_ty ty_i)
    in
    if not (T.tyequal ty ty_i) then report_error_at msg loc
  | A.Absurd _ | LitTodo _ -> ()
  | A.If (_loc, p, pt, pf) ->
    check ctx p T.TyBool;
    check ctx pt ty;
    check ctx pf ty
  | A.Fn (loc, args, body) -> (
      match ty with
      | T.TyArrow (args_ty, ret_ty) when List.length args_ty = List.length args
        ->
        let body_ctx = Ctx.assume_list ctx args args_ty in
        check body_ctx body ret_ty
      | T.TyArrow _ -> report_error_at "Function argument mismatch" loc
      | _ ->
        report_error_at "A function expression must have a function type" loc)
  | A.Sequence (_loc, a, b) ->
    check ctx a T.TyUnit;
    check ctx b ty
  | A.Application (loc, _, _) ->
    let t_app = synth ctx term in
    if not (T.tyequal ty t_app) then
      report_expected_at ~expected:ty ~got:t_app ~loc
  | A.Record (loc, _, _) ->
    let t_rec = synth ctx term in
    if not (T.tyequal ty t_rec) then
      report_expected_at ~expected:ty ~got:t_rec ~loc
  | A.RecordIndex (loc, _, _) ->
    let t_rec = synth ctx term in
    if not (T.tyequal ty t_rec) then
      report_expected_at ~expected:ty ~got:t_rec ~loc
  | A.Annotated (loc, _, _) ->
    let t_rec = synth ctx term in
    if not (T.tyequal ty t_rec) then
      report_expected_at ~expected:ty ~got:t_rec ~loc
  | A.Variant (loc, _, _) ->
    let t_vrt = synth ctx term in
    if not (T.tyequal ty t_vrt) then
      report_expected_at ~expected:ty ~got:t_vrt ~loc
  | A.Tuple (loc, _) ->
    let t_rec = synth ctx term in
    if not (T.tyequal ty t_rec) then
      report_expected_at ~expected:ty ~got:t_rec ~loc
