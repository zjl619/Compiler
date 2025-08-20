(*
   ToyC 编译器代码生成模块
  将 AST 翻译为 RISC-V 32位汇编代码
*)

open Ast

(* 全局标签计数器 *)
let global_label_counter = ref 0

(* 代码生成上下文 *)
type codegen_context =
  { mutable temp_counter : int (* 临时寄存器计数器 *)
  ; mutable stack_offset : int (* 当前栈偏移 *)
  ; mutable break_labels : string list (* break 跳转标签栈 *)
  ; mutable continue_labels : string list (* continue 跳转标签栈 *)
  ; mutable local_vars : (string * int) list (* 局部变量映射到栈偏移 *)
  ; mutable used_callee_saved : reg list (* 使用的被调用者保存寄存器 *)
  ; mutable spill_counter : int (* spill栈位置计数器 *)
  }

(* 创建新的代码生成上下文 *)
let create_context _symbol_table =
  { temp_counter = 0
  ; stack_offset = 0 (* fp-based offset, starts from 0 and goes down *)
  ; break_labels = []
  ; continue_labels = []
  ; local_vars = []
  ; used_callee_saved = []
  ; spill_counter = 0
  }
;;

(* 生成新标签 *)
let new_label _ctx prefix =
  let label = Printf.sprintf "%s%d" prefix !global_label_counter in
  global_label_counter := !global_label_counter + 1;
  label
;;

(* 获取临时寄存器，优先使用T寄存器，必要时合理使用S寄存器 *)
let get_temp_reg ctx =
  let temp_regs = [ T0; T1; T2; T3; T4; T5; T6 ] in
  let num_temp_regs = List.length temp_regs in
  (* 首先尝试使用T寄存器 *)
  if ctx.temp_counter < num_temp_regs * 3
  then (
    (* 在前几轮中主要使用T寄存器，允许一定程度的重用 *)
    let reg = List.nth temp_regs (ctx.temp_counter mod num_temp_regs) in
    ctx.temp_counter <- ctx.temp_counter + 1;
    reg)
  else (
    (* 当T寄存器不够用时，分配一个新的S寄存器 *)
    let available_s_regs = [ S2; S3; S4; S5; S6; S7; S8; S9; S10 ] in
    let rec find_unused regs =
      match regs with
      | [] ->
        (* 如果所有S寄存器都用完了，回退到T寄存器重用 *)
        let reg = List.nth temp_regs (ctx.temp_counter mod num_temp_regs) in
        ctx.temp_counter <- ctx.temp_counter + 1;
        reg
      | reg :: rest ->
        if List.mem reg ctx.used_callee_saved
        then find_unused rest
        else (
          ctx.used_callee_saved <- reg :: ctx.used_callee_saved;
          ctx.temp_counter <- ctx.temp_counter + 1;
          reg)
    in
    find_unused available_s_regs)
;;

(* 获取一个spill栈位置来保存寄存器内容 *)
let get_spill_location ctx =
  ctx.spill_counter <- ctx.spill_counter + 1;
  (* spill位置在栈的最底部，偏移为负数且比局部变量更远 *)
  ctx.stack_offset - (ctx.spill_counter * 4)
;;

(* 生成将寄存器内容spill到栈的指令 *)
let spill_reg_to_stack ctx reg =
  let spill_offset = get_spill_location ctx in
  spill_offset, [ Sw (reg, spill_offset, Fp) ]
;;

(* 生成从栈恢复寄存器内容的指令 *)
let restore_reg_from_stack _ctx spill_offset target_reg =
  [ Lw (target_reg, spill_offset, Fp) ]
;;

(* 将变量添加到栈中 *)
let add_local_var ctx name =
  ctx.stack_offset <- ctx.stack_offset - 4;
  ctx.local_vars <- (name, ctx.stack_offset) :: ctx.local_vars;
  ctx.stack_offset
;;

(* 获取变量的栈偏移 *)
let get_var_offset ctx name =
  try List.assoc name ctx.local_vars with
  | Not_found -> failwith ("Variable not found: " ^ name)
;;

(* Helper function to check if an expression might use A0 *)
let rec expr_might_use_a0 = function
  | Ast.Call _ -> true
  | Ast.BinaryOp (_, e1, e2) -> expr_might_use_a0 e1 || expr_might_use_a0 e2
  | Ast.UnaryOp (_, e) -> expr_might_use_a0 e
  | _ -> false
;;

(* Generate expression code with proper A0 handling *)
let rec gen_expr ctx (expr : Ast.expr) : reg * instruction list =
  match expr with
  | Ast.Int n ->
    let reg = get_temp_reg ctx in
    let instr = [ Li (reg, n) ] in
    reg, instr
  | Ast.Var name ->
    let reg = get_temp_reg ctx in
    let offset = get_var_offset ctx name in
    let instr = [ Lw (reg, offset, Fp) ] in
    reg, instr
  | Ast.UnaryOp (op, e) ->
    let e_reg, e_instrs = gen_expr ctx e in
    let result_reg = get_temp_reg ctx in
    let instrs =
      match op with
      | Ast.Pos -> e_instrs @ [ Mv (result_reg, e_reg) ]
      | Ast.Neg -> e_instrs @ [ Sub (result_reg, Zero, e_reg) ]
      | Ast.Not -> e_instrs @ [ Sltiu (result_reg, e_reg, 1) ]
    in
    result_reg, instrs
  | Ast.BinaryOp (op, e1, e2) ->
    (* 检查是否需要更小心的寄存器管理 *)
    let e2_has_call = expr_might_use_a0 e2 in
    let e1_reg, e1_instrs = gen_expr ctx e1 in
    (* 对于复杂的二元表达式，特别是算术运算，我们需要保护e1的值 *)
    let needs_protection =
      e2_has_call
      ||
      match op with
      | Ast.Div | Ast.Mod | Ast.Add | Ast.Sub | Ast.Mul ->
        (* 检查e2是否是复杂表达式 *)
        (match e2 with
         | Ast.BinaryOp _ | Ast.Call _ -> true
         | _ -> false)
      | _ -> false
    in
    let e1_save_strategy =
      if needs_protection
      then (
        match e1 with
        | Ast.Var var_name ->
          (* 对于变量，从栈重新加载更安全 *)
          `ReloadFromStack var_name
        | _ ->
          (* 对于表达式结果，保存到栈上 *)
          let spill_offset, spill_instrs = spill_reg_to_stack ctx e1_reg in
          `SpillToStack (spill_instrs, spill_offset))
      else (
        match e1, e2_has_call with
        | _, true ->
          let temp_reg = get_temp_reg ctx in
          `SaveToReg ([ Mv (temp_reg, e1_reg) ], temp_reg)
        | _, _ when op = Ast.And || op = Ast.Or ->
          let temp_reg = get_temp_reg ctx in
          `SaveToReg ([ Mv (temp_reg, e1_reg) ], temp_reg)
        | _ -> `NoSave e1_reg)
    in
    (* Apply save strategy BEFORE computing e2 *)
    let e1_save_instrs, e1_final_reg =
      match e1_save_strategy with
      | `ReloadFromStack _var_name -> [], e1_reg (* Will reload later *)
      | `SaveToReg (instrs, reg) -> instrs, reg
      | `SpillToStack (instrs, _offset) -> instrs, e1_reg (* Will reload later *)
      | `NoSave reg -> [], reg
    in
    let e2_reg, e2_instrs = gen_expr ctx e2 in
    (* Reload e1 value if needed AFTER e2 computation *)
    let e1_reload_instrs, e1_actual_reg =
      match e1_save_strategy with
      | `ReloadFromStack var_name ->
        let offset = get_var_offset ctx var_name in
        let reload_reg = get_temp_reg ctx in
        [ Lw (reload_reg, offset, Fp) ], reload_reg
      | `SpillToStack (_instrs, spill_offset) ->
        let reload_reg = get_temp_reg ctx in
        restore_reg_from_stack ctx spill_offset reload_reg, reload_reg
      | _ -> [], e1_final_reg
    in
    let result_reg = get_temp_reg ctx in
    let op_instrs =
      match op with
      | Ast.Add -> [ Add (result_reg, e1_actual_reg, e2_reg) ]
      | Ast.Sub -> [ Sub (result_reg, e1_actual_reg, e2_reg) ]
      | Ast.Mul -> [ Mul (result_reg, e1_actual_reg, e2_reg) ]
      | Ast.Div -> [ Div (result_reg, e1_actual_reg, e2_reg) ]
      | Ast.Mod -> [ Rem (result_reg, e1_actual_reg, e2_reg) ]
      | Ast.Eq ->
        [ Sub (result_reg, e1_actual_reg, e2_reg); Sltiu (result_reg, result_reg, 1) ]
      | Ast.Neq ->
        [ Sub (result_reg, e1_actual_reg, e2_reg); Sltu (result_reg, Zero, result_reg) ]
      | Ast.Lt -> [ Slt (result_reg, e1_actual_reg, e2_reg) ]
      | Ast.Leq ->
        [ Slt (result_reg, e2_reg, e1_actual_reg); Xori (result_reg, result_reg, 1) ]
      | Ast.Gt -> [ Slt (result_reg, e2_reg, e1_actual_reg) ]
      | Ast.Geq ->
        [ Slt (result_reg, e1_actual_reg, e2_reg); Xori (result_reg, result_reg, 1) ]
      | Ast.And | Ast.Or ->
        (* Handled separately in instrs construction *)
        []
    in
    let instrs =
      match op with
      | Ast.And | Ast.Or ->
        (* 对于逻辑运算符，实现正确的计算逻辑，将所有非零值转换为1 *)
        (* 重要：为了避免寄存器分配冲突，我们需要确保中间结果被正确保存 *)

        (* 首先计算e1，并立即将其转换为布尔值 *)
        let e1_bool_reg = get_temp_reg ctx in
        let e1_bool_instrs = [ Sltu (e1_bool_reg, Zero, e1_actual_reg) ] in
        (* 如果e2的计算可能破坏e1_bool_reg，则需要将其保存到栈上 *)
        let e2_might_clobber =
          match e2 with
          | Ast.BinaryOp _ | Ast.Call _ -> true
          | _ -> false
        in
        let e1_bool_save_strategy =
          if e2_might_clobber
          then (
            let spill_offset, spill_instrs = spill_reg_to_stack ctx e1_bool_reg in
            `SpillToStack (spill_instrs, spill_offset))
          else `NoSave e1_bool_reg
        in
        (* 应用e1布尔值保存策略 *)
        let e1_bool_save_instrs =
          match e1_bool_save_strategy with
          | `SpillToStack (instrs, _) -> instrs
          | `NoSave _ -> []
        in
        (* 计算e2并转换为布尔值 *)
        let e2_bool_reg = get_temp_reg ctx in
        let e2_bool_instrs = [ Sltu (e2_bool_reg, Zero, e2_reg) ] in
        (* 如果需要，从栈中恢复e1布尔值 *)
        let e1_bool_restore_instrs, e1_bool_final_reg =
          match e1_bool_save_strategy with
          | `SpillToStack (_, spill_offset) ->
            let restore_reg = get_temp_reg ctx in
            restore_reg_from_stack ctx spill_offset restore_reg, restore_reg
          | `NoSave reg -> [], reg
        in
        (* 执行最终的逻辑运算 *)
        let logical_op =
          match op with
          | Ast.And -> [ And (result_reg, e1_bool_final_reg, e2_bool_reg) ]
          | Ast.Or -> [ Or (result_reg, e1_bool_final_reg, e2_bool_reg) ]
          | _ -> failwith "Impossible"
        in
        (* 正确的指令顺序 *)
        e1_instrs
        @ e1_save_instrs (* Save e1 before e2 computation *)
        @ e1_reload_instrs (* Reload e1 after saving if needed *)
        @ e1_bool_instrs (* Convert e1 to boolean *)
        @ e1_bool_save_instrs (* Save e1 boolean value if needed *)
        @ e2_instrs (* Compute e2 *)
        @ e2_bool_instrs (* Convert e2 to boolean *)
        @ e1_bool_restore_instrs (* Restore e1 boolean value if needed *)
        @ logical_op
      | _ ->
        e1_instrs
        @ e1_save_instrs (* Save e1 before e2 computation *)
        @ e2_instrs
        @ e1_reload_instrs (* Reload e1 after e2 computation if needed *)
        @ op_instrs
    in
    result_reg, instrs
  | Ast.Call (fname, args) ->
    let result_reg = A0 in
    (* 计算栈参数数量 *)
    let num_args = List.length args in
    let num_stack_args = max 0 (num_args - 8) in
    
    (* 为栈参数分配空间 (16字节对齐) *)
    let stack_space = (num_stack_args + 1) / 2 * 2 * 4 in  (* 确保是8的倍数 *)
    let pre_call_instrs = 
        if stack_space > 0 then [ Addi (Sp, Sp, -stack_space) ] else [] 
    in
    
    (* 生成参数代码 *)
    let arg_instrs = ref [] in
    let saved_results = ref [] in
    
    List.iteri (fun i arg ->
        let arg_reg, arg_code = gen_expr ctx arg in
        arg_instrs := !arg_instrs @ arg_code;
        
        if i < 8 then
            (* 前8个参数放入寄存器 *)
            let target_reg = 
                match i with
                | 0 -> A0
                | 1 -> A1
                | 2 -> A2
                | 3 -> A3
                | 4 -> A4
                | 5 -> A5
                | 6 -> A6
                | 7 -> A7
                | _ -> failwith "Impossible"
            in
            
            (* 如果参数包含函数调用，需要保存结果 *)
            if expr_might_use_a0 arg && i < num_args - 1 then
                let save_reg = 
                    match i with
                    | 0 -> S2
                    | 1 -> S3
                    | 2 -> S4
                    | 3 -> S5
                    | 4 -> S6
                    | 5 -> S7
                    | 6 -> S8
                    | 7 -> S9
                    | _ -> failwith "Impossible"
                in
                if not (List.mem save_reg ctx.used_callee_saved) then
                    ctx.used_callee_saved <- save_reg :: ctx.used_callee_saved;
                arg_instrs := !arg_instrs @ [ Mv (save_reg, arg_reg) ];
                saved_results := (target_reg, save_reg) :: !saved_results
            else
                arg_instrs := !arg_instrs @ [ Mv (target_reg, arg_reg) ]
        else
            (* 第8个以后的参数放到栈上 *)
            let stack_offset = (i - 8) * 4 in
            arg_instrs := !arg_instrs @ [ Sw (arg_reg, stack_offset, Sp) ]
    ) args;
    
    (* 恢复保存的结果到正确的寄存器位置 *)
    let restore_instrs = 
        List.map (fun (target_reg, save_reg) -> Mv (target_reg, save_reg)) 
                 (List.rev !saved_results)
    in
    
    (* 函数调用 *)
    let call_instr = [ Jal (Ra, fname) ] in
    
    (* 释放栈空间 *)
    let post_call_instrs = 
        if stack_space > 0 then [ Addi (Sp, Sp, stack_space) ] else [] 
    in
    
    (result_reg, pre_call_instrs @ !arg_instrs @ restore_instrs @ call_instr @ post_call_instrs)
;;

(* Generate epilogue with callee-saved register restoration *)
let gen_epilogue_instrs frame_size used_callee_saved =
  (* Restore callee-saved registers in the same order as saved *)
  let restore_callee_saved =
    List.mapi
      (fun i reg ->
         let offset = frame_size - 12 - (i * 4) in
         (* Same offset as save *)
         Lw (reg, offset, Sp))
      used_callee_saved (* Same order as save *)
  in
  (* 注意：S1寄存器不应该在这里恢复，因为它是在used_callee_saved列表中处理的 *)
  restore_callee_saved
  @ [ (* Restore registers from correct stack positions before releasing stack frame *)
      Lw (Ra, frame_size - 4, Sp) (* Restore return address from sp + (frame_size - 4) *)
    ; Lw (T0, frame_size - 8, Sp)
      (* Load old frame pointer from sp + (frame_size - 8) using T0 暂存 *)
    ; Addi (Sp, Sp, frame_size) (* Release stack frame *)
    ; Mv (Fp, T0) (* Restore old frame pointer using T0 *)
    ; Ret (* Return to caller *)
    ]
;;

(* 生成语句代码 *)
let rec gen_stmt ctx frame_size (stmt : Ast.stmt) : asm_item list =
  match stmt with
  | Ast.Empty -> []
  | Ast.Expr e ->
    let _, instrs = gen_expr ctx e in
    List.map (fun i -> Instruction i) instrs
  | Ast.Block stmts ->
    let old_vars = ctx.local_vars in
    let old_offset = ctx.stack_offset in
    let items = List.map (gen_stmt ctx frame_size) stmts |> List.flatten in
    ctx.local_vars <- old_vars;
    ctx.stack_offset <- old_offset;
    items
  | Ast.Return (Some e) ->
    (* Optimize for simple constant 0 *)
    (match e with
     | Ast.Int 0 ->
       let all_instrs =
         [ Li (A0, 0) ] @ gen_epilogue_instrs frame_size ctx.used_callee_saved
       in
       List.map (fun i -> Instruction i) all_instrs
     | _ ->
       let e_reg, e_instrs = gen_expr ctx e in
       let all_instrs =
         e_instrs
         @ [ Mv (A0, e_reg) ]
         @ gen_epilogue_instrs frame_size ctx.used_callee_saved
       in
       List.map (fun i -> Instruction i) all_instrs)
  | Ast.Return None ->
    List.map
      (fun i -> Instruction i)
      (gen_epilogue_instrs frame_size ctx.used_callee_saved)
  | Ast.If (cond, then_stmt, else_stmt) ->
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let else_label = new_label ctx "else" in
    let end_label = new_label ctx "endif" in
    let then_items = gen_stmt ctx frame_size then_stmt in
    let else_items =
      match else_stmt with
      | Some s -> gen_stmt ctx frame_size s
      | None -> []
    in
    List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, else_label)) ]
    @ then_items
    @ [ Instruction (J end_label); Label else_label ]
    @ else_items
    @ [ Label end_label ]
  | Ast.While (cond, body) ->
    let loop_label = new_label ctx "loop" in
    let end_label = new_label ctx "endloop" in
    ctx.break_labels <- end_label :: ctx.break_labels;
    ctx.continue_labels <- loop_label :: ctx.continue_labels;
    let cond_reg, cond_instrs = gen_expr ctx cond in
    let body_items = gen_stmt ctx frame_size body in
    ctx.break_labels <- List.tl ctx.break_labels;
    ctx.continue_labels <- List.tl ctx.continue_labels;
    [ Label loop_label ]
    @ List.map (fun i -> Instruction i) cond_instrs
    @ [ Instruction (Beq (cond_reg, Zero, end_label)) ]
    @ body_items
    @ [ Instruction (J loop_label); Label end_label ]
  | Ast.Break ->
    (match ctx.break_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Break outside loop")
  | Ast.Continue ->
    (match ctx.continue_labels with
     | label :: _ -> [ Instruction (J label) ]
     | [] -> failwith "Continue outside loop")
  | Ast.Declare (name, e) ->
    let offset = add_local_var ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    List.map (fun i -> Instruction i) all_instrs
  | Ast.Assign (name, e) ->
    let offset = get_var_offset ctx name in
    let e_reg, e_instrs = gen_expr ctx e in
    let all_instrs = e_instrs @ [ Sw (e_reg, offset, Fp) ] in
    List.map (fun i -> Instruction i) all_instrs
;;

(* 计算函数所需的栈帧大小，包括被调用者保存寄存器和spill空间 *)
let calculate_frame_size (func_def : Ast.func_def) used_callee_saved ctx =
  (* Helper to count all declarations including nested blocks *)
  let rec count_all_decls_in_stmt stmt =
    match stmt with
    | Declare _ -> 1
    | Block stmts ->
      (* For nested blocks, all variables are live simultaneously *)
      List.fold_left (fun acc s -> acc + count_all_decls_in_stmt s) 0 stmts
    | If (_, s1, Some s2) -> max (count_all_decls_in_stmt s1) (count_all_decls_in_stmt s2)
    | If (_, s1, None) -> count_all_decls_in_stmt s1
    | While (_, s) -> count_all_decls_in_stmt s
    | _ -> 0
  in
  let num_locals = count_all_decls_in_stmt func_def.body in
  let num_params = List.length func_def.params in
  let num_stack_params = max 0 (num_params - 8) in  (* 栈上传参的数量 *)
  let num_callee_saved = List.length used_callee_saved in
  let num_spills = ctx.spill_counter in
  (* Calculate required space:
     - 8 bytes for ra and fp
     - 4 bytes per callee-saved register
     - 4 bytes per parameter (all parameters need to be stored as local variables)
     - 4 bytes per local variable
     - 4 bytes per spill slot
     - 额外空间用于栈上传参 (如果需要)
  *)
  let base_space =
    8 + (num_callee_saved * 4) + (num_params * 4) + 
    (num_locals * 4) + (num_spills * 4) + (num_stack_params * 4)
  in
  (* 16-byte alignment *)
  let aligned_space = (base_space + 15) / 16 * 16 in
  aligned_space
;;

(* 生成函数代码 *)
let gen_function symbol_table (func_def : Ast.func_def) : asm_item list =
  let ctx = create_context symbol_table in
  (* 生成函数体以确定使用的被调用者保存寄存器 *)
  (* 先设置参数以便生成函数体 *)
  ctx.stack_offset <- -8;
  List.iteri
    (fun _i param ->
       match param with
       | Ast.Param name ->
         let _offset = add_local_var ctx name in
         ())
    func_def.params;
  let temp_frame_size = 1000 in
  (* 临时的大frame_size *)
  let _temp_body_items = gen_stmt ctx temp_frame_size func_def.body in
  (* 确保S1寄存器被添加到被调用者保存寄存器列表中，但不要重复添加 *)
  (* 注意：必须在计算frame_size和生成序言指令之前添加S1 *)
  if not (List.mem S1 ctx.used_callee_saved)
  then ctx.used_callee_saved <- S1 :: ctx.used_callee_saved;
  (* 现在我们知道了使用的被调用者保存寄存器，计算真正的frame_size *)
  let frame_size = calculate_frame_size func_def ctx.used_callee_saved ctx in
  (* 生成函数序言，包括保存被调用者保存寄存器 *)
  let save_callee_saved =
    List.mapi
      (fun i reg ->
         let offset = frame_size - 12 - (i * 4) in
         (* After ra(-4) and fp(-8) *)
         Instruction (Sw (reg, offset, Sp)))
      ctx.used_callee_saved
  in
  let prologue =
    [ Comment "prologue"
    ; Instruction (Addi (Sp, Sp, -frame_size))
    ; Instruction (Sw (Ra, frame_size - 4, Sp))
    ; Instruction (Sw (Fp, frame_size - 8, Sp))
    ; Instruction (Addi (Fp, Sp, frame_size))
    ]
    @ save_callee_saved
  in
  (* 重新设置参数变量的栈偏移，考虑被调用者保存寄存器 *)
  ctx.temp_counter <- 0;
  ctx.stack_offset <- -8 - (List.length ctx.used_callee_saved * 4);
  ctx.local_vars <- [];
  ctx.spill_counter <- 0;
  (* 重置spill计数器，避免在real run中使用错误的偏移 *)
  let param_instrs =
    List.mapi (fun i param ->
        match param with
        | Ast.Param name ->
            let offset = add_local_var ctx name in
            if i < 8 then
                (* 前8个参数来自寄存器 *)
                let arg_reg = 
                    match i with
                    | 0 -> A0
                    | 1 -> A1
                    | 2 -> A2
                    | 3 -> A3
                    | 4 -> A4
                    | 5 -> A5
                    | 6 -> A6
                    | 7 -> A7
                    | _ -> failwith "Impossible"
                in
                [ Instruction (Sw (arg_reg, offset, Fp)) ]
            else
                (* 额外的参数来自调用者的栈帧 *)
                (* 这些参数位于 fp + (i-8)*4 + 16 (跳过保存的ra和fp) *)
                let stack_param_offset = (i - 8) * 4 + 16 in
                let temp_reg = get_temp_reg ctx in
                [ Instruction (Lw (temp_reg, stack_param_offset, Fp));
                  Instruction (Sw (temp_reg, offset, Fp)) ]
    ) func_def.params |> List.flatten
  in
  (* 重新生成函数体，使用正确的frame_size和变量映射 *)
  let body_items = gen_stmt ctx frame_size func_def.body in
  (* 函数尾声（如果函数没有显式 return） *)
  let epilogue =
    let has_ret =
      List.exists
        (function
          | Instruction Ret -> true
          | _ -> false)
        body_items
    in
    if has_ret
    then []
    else
      List.map
        (fun i -> Instruction i)
        (gen_epilogue_instrs frame_size ctx.used_callee_saved)
  in
  prologue @ param_instrs @ body_items @ epilogue
;;

(* 生成程序代码 *)
let gen_program symbol_table (program : Ast.comp_unit) =
  (* 重置全局标签计数器 *)
  global_label_counter := 0;
  (* 全局声明 *)
  let header =
    [ Directive ".text"; Directive ".globl main"; Comment "ToyC Compiler Generated Code" ]
  in
  (* 生成所有函数 *)
  let func_asm_items =
    List.map
      (fun func_def ->
         let items = gen_function symbol_table func_def in
         [ Label func_def.name; Comment ("Function: " ^ func_def.name) ] @ items)
      program
    |> List.flatten
  in
  header @ func_asm_items
;;

(* 主入口函数：编译程序并输出汇编文件 *)
let compile_to_riscv symbol_table program output_file =
  let asm_items = gen_program symbol_table program in
  Riscv.emit_asm_to_file output_file asm_items
;;
