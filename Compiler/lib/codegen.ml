(* codegen.ml *)
open Ast

(* 寄存器分类 *)
type reg_type = 
  | CallerSaved   (* 调用者保存寄存器 *)
  | CalleeSaved   (* 被调用者保存寄存器 *)
  | TempReg       (* 临时寄存器 *)

(* 寄存器表示 *)
type reg =
  | Physical of string  (* 物理寄存器 *)
  | Spill of int        (* 栈偏移位置 *)

(* 循环上下文 *)
type loop_ctx = {
    break_label: string;
    continue_label: string
}

(* 上下文类型 *)
type context = {
    current_func: string;
    local_offset: int;
    frame_size: int;
    var_offset: (string * int) list list;
    next_temp: int;
    label_counter: int;
    loop_stack: loop_ctx list;  (* 循环上下文栈 *)
    saved_regs: string list;
    reg_map: (string * reg_type) list;
    param_count: int;
    used_regs: reg list;        (* 跟踪正在使用的寄存器 *)
    max_local_offset: int;      (* 最大局部变量偏移 *)
    return_label: string;       (* 统一返回点标签 *)
    saved_area_size: int;
}

(* 创建新上下文 *)
let create_context func_name =
    let reg_map = [
        ("a0", CallerSaved); ("a1", CallerSaved); ("a2", CallerSaved); ("a3", CallerSaved);
        ("a4", CallerSaved); ("a5", CallerSaved); ("a6", CallerSaved); ("a7", CallerSaved);
        ("s0", CalleeSaved); ("s1", CalleeSaved); ("s2", CalleeSaved); ("s3", CalleeSaved);
        ("s4", CalleeSaved); ("s5", CalleeSaved); ("s6", CalleeSaved); ("s7", CalleeSaved);
        ("s8", CalleeSaved); ("s9", CalleeSaved); ("s10", CalleeSaved); ("s11", CalleeSaved);
        ("t0", TempReg); ("t1", TempReg); ("t2", TempReg); ("t3", TempReg);
        ("t4", TempReg); ("t5", TempReg); ("t6", TempReg)
    ] in
    let return_label = Printf.sprintf ".L%s_return" func_name in
    { current_func = func_name;
      local_offset = 0;
      frame_size = 0;
      var_offset = [[]];
      next_temp = 0;
      label_counter = 0;
      loop_stack = [];
      saved_regs = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"];
      reg_map = reg_map;
      param_count = 0;
      used_regs = [];  (* 初始化为空列表 *)
      max_local_offset = 0;
      return_label = return_label;
      saved_area_size = 52 } (* 4(ra) + 12*4(regs) = 52 *)

(* 栈对齐常量 *)
let stack_align = 16

(* 获取唯一标签 - 使用函数名作为前缀 *)
let fresh_label ctx prefix =
    let n = ctx.label_counter in
    { ctx with label_counter = n + 1 }, 
    Printf.sprintf ".L%s_%s%d" ctx.current_func prefix n

(* 获取变量偏移量 - 支持嵌套作用域查找 *)
let get_var_offset ctx name =
    let rec search = function
        | [] -> None
        | scope :: rest -> 
            try Some (List.assoc name scope)
            with Not_found -> search rest
    in
    match search ctx.var_offset with
    | Some offset -> offset
    | None -> failwith (Printf.sprintf "Undefined variable: %s" name)

(* 添加变量到当前作用域 *)
let add_var ctx name size =
  match ctx.var_offset with
  | current_scope :: rest_scopes ->
      let offset = ctx.saved_area_size + ctx.local_offset in
      let new_scope = (name, offset) :: current_scope in
      { ctx with 
          var_offset = new_scope :: rest_scopes;
          local_offset = ctx.local_offset + size;
          max_local_offset = max ctx.max_local_offset (ctx.local_offset + size)
      }
  | [] -> failwith "No active scope"

(* 优化的寄存器分配函数 *)
let alloc_temp_reg ctx =
    (* 优先分配临时寄存器 *)
    let temp_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"] in (* 保留t6用于大偏移量处理 *)
    let is_physical_reg_available reg =
        not (List.exists (function Physical r -> r = reg | _ -> false) ctx.used_regs)
    in
    
    match List.find_opt is_physical_reg_available temp_regs with
    | Some reg -> 
        let new_reg = Physical reg in
        { ctx with used_regs = new_reg :: ctx.used_regs }, new_reg
    | None ->
        (* 其次尝试分配保存寄存器 *)
        let saved_regs = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"] in
        match List.find_opt is_physical_reg_available saved_regs with
        | Some reg -> 
            let new_reg = Physical reg in
            { ctx with used_regs = new_reg :: ctx.used_regs }, new_reg
        | None ->
            (* 最后溢出到栈 *)
            let spill_offset = ctx.saved_area_size + ctx.max_local_offset + (List.length ctx.used_regs) * 4 in
            let spill_reg = Spill spill_offset in
            { ctx with 
                used_regs = spill_reg :: ctx.used_regs;
                max_local_offset = max ctx.max_local_offset (spill_offset + 4)
            }, spill_reg

(* 寄存器释放函数 *)
let free_temp_reg ctx reg =
    { ctx with used_regs = List.filter (fun r -> r <> reg) ctx.used_regs }

(* 计算栈对齐 *)
let align_stack size align =
    let remainder = size mod align in
    if remainder = 0 then size else size + (align - remainder)

(* 生成大立即数加载代码 - 修复立即数范围问题 *)
let gen_large_offset_access base offset access_op reg =
    if offset >= -2048 && offset <= 2047 then
        Printf.sprintf "    %s %s, %d(%s)" access_op reg offset base
    else
        (* 使用t6处理大偏移量 *)
        let hi = (offset asr 12) + if offset land 0x800 != 0 then 1 else 0 in  (* 符号扩展处理 *)
        let lo = offset land 0xFFF in
        (* 处理负数的正确转换 *)
        let hi, lo = 
            if lo >= 2048 then (hi + 1, lo - 4096)
            else (hi, lo)
        in
        Printf.sprintf "    lui t6, %d\n    addi t6, t6, %d\n    add t6, %s, t6\n    %s %s, 0(t6)"
            hi lo base access_op reg

(* 函数序言生成 *)
let gen_prologue ctx func =
    let total_size = align_stack (ctx.saved_area_size + ctx.max_local_offset) stack_align in
    
    (* 处理大栈帧调整 - 修复立即数范围问题 *)
    let stack_adjust_asm = 
        if total_size <= 2047 then
            Printf.sprintf "    addi sp, sp, -%d" total_size
        else
            let hi = (total_size asr 12) + if total_size land 0x800 != 0 then 1 else 0 in
            let lo = total_size land 0xFFF in
            let hi, lo = 
                if lo >= 2048 then (hi + 1, lo - 4096)
                else (hi, lo)
            in
            Printf.sprintf "    lui t6, %d\n    addi t6, t6, %d\n    sub sp, sp, t6" hi lo
    in
    
    let save_regs_asm = 
        let save_instrs = 
            List.mapi (fun i reg -> 
                Printf.sprintf "    sw %s, %d(sp)" reg (i * 4)
            ) ctx.saved_regs
            @ [Printf.sprintf "    sw ra, %d(sp)" (List.length ctx.saved_regs * 4)]
        in
        String.concat "\n" save_instrs
    in
    
    let asm = Printf.sprintf "
    .globl %s
%s:
%s
%s
" func.name func.name stack_adjust_asm save_regs_asm in
    (asm, { ctx with frame_size = total_size })

(* 函数结语生成 - 修复立即数范围问题 *)
let gen_epilogue ctx =
    let restore_regs_asm = 
        let ra_restore = Printf.sprintf "    lw ra, %d(sp)" (List.length ctx.saved_regs * 4) in
        let s_regs_restore = 
            List.rev ctx.saved_regs
            |> List.mapi (fun i reg ->
                let offset = (List.length ctx.saved_regs - 1 - i) * 4 in
                Printf.sprintf "    lw %s, %d(sp)" reg offset)
            |> String.concat "\n" in
        ra_restore ^ "\n" ^ s_regs_restore
    in
    
    (* 处理大栈帧恢复 - 修复立即数范围问题 *)
    let stack_restore_asm = 
        if ctx.frame_size <= 2047 then
            Printf.sprintf "    addi sp, sp, %d" ctx.frame_size
        else
            let hi = (ctx.frame_size asr 12) + if ctx.frame_size land 0x800 != 0 then 1 else 0 in
            let lo = ctx.frame_size land 0xFFF in
            let hi, lo = 
                if lo >= 2048 then (hi + 1, lo - 4096)
                else (hi, lo)
            in
            Printf.sprintf "    lui t6, %d\n    addi t6, t6, %d\n    add sp, sp, t6" hi lo
    in
    
    Printf.sprintf "
%s
%s
    ret
" restore_regs_asm stack_restore_asm

(* 表达式代码生成 - 修复二元运算符处理逻辑 *)
let rec gen_expr ctx expr =
    match expr with
    | IntLit n -> 
        let (ctx, reg) = alloc_temp_reg ctx in
        let asm = match reg with
            | Physical r -> Printf.sprintf "    li %s, %d" r n
            | Spill offset ->
                (* 使用大偏移量处理 *)
                Printf.sprintf "%s\n    li t0, %d" (gen_large_offset_access "sp" offset "sw" "t0") n
        in
        (ctx, asm, reg)
    | Var name ->
        let offset = get_var_offset ctx name in
        let (ctx, reg) = alloc_temp_reg ctx in
        let asm = match reg with
            | Physical r -> 
                gen_large_offset_access "sp" offset "lw" r
            | Spill spill_offset ->
                Printf.sprintf "%s\n%s" 
                    (gen_large_offset_access "sp" offset "lw" "t0")
                    (gen_large_offset_access "sp" spill_offset "sw" "t0")
        in
        (ctx, asm, reg)
    | BinOp (e1, op, e2) ->
        let (ctx, asm1, reg1) = gen_expr ctx e1 in
        let (ctx, asm2, reg2) = gen_expr ctx e2 in
    
        (* 处理寄存器冲突 - 不再重用左操作数寄存器 *)
        let (ctx, temp_reg) = alloc_temp_reg ctx in
        
        (* 获取实际使用的寄存器 *)
        let get_actual_reg reg temp_reg =
            match reg with
            | Physical r -> r
            | Spill offset -> 
                let _ = gen_large_offset_access "sp" offset "lw" temp_reg in
                temp_reg
        in
    
        let actual_reg1 = get_actual_reg reg1 "t0" in
        let actual_reg2 = get_actual_reg reg2 "t1" in
        
        let actual_temp = match temp_reg with
            | Physical r -> r
            | Spill offset -> 
                let _ = gen_large_offset_access "sp" offset "lw" "t2" in
                "t2"
        in
        
        let instr = match op with
            | Add -> Printf.sprintf "    add %s, %s, %s" actual_temp actual_reg1 actual_reg2
            | Sub -> Printf.sprintf "    sub %s, %s, %s" actual_temp actual_reg1 actual_reg2
            | Mul -> Printf.sprintf "    mul %s, %s, %s" actual_temp actual_reg1 actual_reg2
            | Div -> Printf.sprintf "    div %s, %s, %s" actual_temp actual_reg1 actual_reg2
            | Mod -> Printf.sprintf "    rem %s, %s, %s" actual_temp actual_reg1 actual_reg2
            | Lt  -> Printf.sprintf "    slt %s, %s, %s" actual_temp actual_reg1 actual_reg2
            | Le  -> Printf.sprintf "    slt %s, %s, %s\n    xori %s, %s, 1" 
                        actual_temp actual_reg2 actual_reg1 actual_temp actual_temp
            | Gt  -> Printf.sprintf "    slt %s, %s, %s" actual_temp actual_reg2 actual_reg1
            | Ge  -> Printf.sprintf "    slt %s, %s, %s\n    xori %s, %s, 1" 
                        actual_temp actual_reg1 actual_reg2 actual_temp actual_temp
            | Eq  -> Printf.sprintf "    sub %s, %s, %s\n    seqz %s, %s" 
                        actual_temp actual_reg1 actual_reg2 actual_temp actual_temp
            | Ne  -> Printf.sprintf "    sub %s, %s, %s\n    snez %s, %s" 
                        actual_temp actual_reg1 actual_reg2 actual_temp actual_temp
            | And -> Printf.sprintf "    and %s, %s, %s" actual_temp actual_reg1 actual_reg2
            | Or  -> Printf.sprintf "    or %s, %s, %s" actual_temp actual_reg1 actual_reg2
        in
        
        (* 存储结果 *)
        let store_result = match temp_reg with
            | Physical _ -> ""
            | Spill offset -> gen_large_offset_access "sp" offset "sw" actual_temp
        in
    
        let full_asm = 
            let parts = [asm1; asm2] @
                     [instr] @
                     (if store_result = "" then [] else [store_result]) in
            String.concat "\n" (List.filter (fun s -> s <> "") parts) in
    
        (* 释放操作数寄存器 *)
        let ctx = free_temp_reg ctx reg1 in
        let ctx = free_temp_reg ctx reg2 in
        
        (ctx, full_asm, temp_reg)
        
    | UnOp (op, e) ->
        let (ctx, asm, reg) = gen_expr ctx e in
        let actual_reg = match reg with
            | Physical r -> r
            | Spill offset -> 
                let _ = gen_large_offset_access "sp" offset "lw" "t0" in
                "t0"
        in
        let instr = match op with
        | UPlus  -> ""  (* 不需要操作 *)
        | UMinus -> Printf.sprintf "    neg %s, %s" actual_reg actual_reg
        | Not    -> Printf.sprintf "    seqz %s, %s" actual_reg actual_reg
        in
        let store_asm = match reg with
            | Physical _ -> ""
            | Spill offset -> gen_large_offset_access "sp" offset "sw" actual_reg
        in
        let full_asm = 
          let parts = [asm] @
                     [instr] @
                     (if store_asm = "" then [] else [store_asm]) in
          String.concat "\n" (List.filter (fun s -> s <> "") parts) in
        (ctx, full_asm, reg)

    | FuncCall (name, args) ->
        let (ctx, arg_asm, arg_regs) = gen_args ctx args in
        
        let n_extra = max (List.length args - 8) 0 in
        let temp_space = 28 + n_extra * 4 in
        let aligned_temp_space = align_stack temp_space stack_align in
        
        let stack_adj_asm = 
          if aligned_temp_space > 0 then 
            if aligned_temp_space <= 2047 then
                Printf.sprintf "    addi sp, sp, -%d" aligned_temp_space
            else
                let hi = (aligned_temp_space asr 12) + if aligned_temp_space land 0x800 != 0 then 1 else 0 in
                let lo = aligned_temp_space land 0xFFF in
                let hi, lo = 
                    if lo >= 2048 then (hi + 1, lo - 4096)
                    else (hi, lo)
                in
                Printf.sprintf "    lui t6, %d\n    addi t6, t6, %d\n    sub sp, sp, t6" hi lo
          else "" in
        
        let save_temps_asm = 
          List.init 7 (fun i -> 
            Printf.sprintf "    sw t%d, %d(sp)" i (i * 4))
          |> String.concat "\n" in
        
        let move_args_asm = 
          let rec move_args regs index parts =
            match regs with
            | [] -> String.concat "\n" (List.filter (fun s -> s <> "") parts)
            | reg::rest when index < 8 ->
                let target = Printf.sprintf "a%d" index in
                let actual_src = match reg with
                    | Physical r -> r
                    | Spill offset -> 
                        gen_large_offset_access "sp" offset "lw" "t0" |> ignore;
                        "t0"
                in
                let move_instr = 
                  if actual_src = target then ""
                  else Printf.sprintf "    mv %s, %s" target actual_src
                in
                let new_parts = parts @ [move_instr] in
                move_args rest (index+1) new_parts
            | reg::rest ->
                let stack_offset = 28 + (index - 8) * 4 in
                let actual_src = match reg with
                    | Physical r -> r
                    | Spill offset -> 
                        gen_large_offset_access "sp" offset "lw" "t0" |> ignore;
                        "t0"
                in
                let store_instr = gen_large_offset_access "sp" stack_offset "sw" actual_src in
                let new_parts = parts @ [store_instr] in
                move_args rest (index+1) new_parts
          in
          move_args arg_regs 0 []
        in
        
        let call_asm = Printf.sprintf "    call %s" name in
        
        let restore_temps_asm = 
          List.init 7 (fun i -> 
            Printf.sprintf "    lw t%d, %d(sp)" i (i * 4))
          |> String.concat "\n" in
        
        let restore_stack_asm = 
          if aligned_temp_space > 0 then 
            if aligned_temp_space <= 2047 then
                Printf.sprintf "    addi sp, sp, %d" aligned_temp_space
            else
                let hi = (aligned_temp_space asr 12) + if aligned_temp_space land 0x800 != 0 then 1 else 0 in
                let lo = aligned_temp_space land 0xFFF in
                let hi, lo = 
                    if lo >= 2048 then (hi + 1, lo - 4096)
                    else (hi, lo)
                in
                Printf.sprintf "    lui t6, %d\n    addi t6, t6, %d\n    add sp, sp, t6" hi lo
          else "" in
        
        (* 分配结果寄存器 *)
        let (ctx, reg_dest) = alloc_temp_reg ctx in
        
        let move_result = match reg_dest with
            | Physical r -> Printf.sprintf "    mv %s, a0" r
            | Spill offset -> gen_large_offset_access "sp" offset "sw" "a0"
        in
        
        (* 确保在恢复临时寄存器前保存结果 *)
        let asm = 
          let parts = [arg_asm] @
                     (if stack_adj_asm = "" then [] else [stack_adj_asm]) @
                     (if save_temps_asm = "" then [] else [save_temps_asm]) @
                     (if move_args_asm = "" then [] else [move_args_asm]) @
                     [call_asm] @
                     [move_result] @  (* 先保存结果再恢复临时寄存器 *)
                     (if restore_temps_asm = "" then [] else [restore_temps_asm]) @
                     (if restore_stack_asm = "" then [] else [restore_stack_asm]) 
          in
          String.concat "\n" (List.filter (fun s -> s <> "") parts) in
        
        (* 释放参数寄存器 *)
        let ctx = List.fold_left free_temp_reg ctx arg_regs in
        
        (ctx, asm, reg_dest)

(* 生成参数代码 *)
and gen_args ctx args =
  let rec process_args ctx asm regs count = function
    | [] -> (ctx, asm, List.rev regs)
    | arg::rest ->
        let (ctx, arg_asm, reg) = gen_expr ctx arg in
        let new_asm = if asm = "" then arg_asm 
                     else asm ^ "\n" ^ arg_asm in
        process_args ctx new_asm (reg::regs) (count+1) rest
  in
  process_args ctx "" [] 0 args

(* 处理语句列表的辅助函数 *)
let rec gen_stmts ctx stmts =
    List.fold_left (fun (ctx, asm) stmt ->
        let (ctx', stmt_asm) = gen_stmt ctx stmt in
        let new_asm = if asm = "" then stmt_asm
                     else if stmt_asm = "" then asm
                     else asm ^ "\n" ^ stmt_asm in
        (ctx', new_asm)
    ) (ctx, "") stmts

(* 语句代码生成 *)
and gen_stmt ctx stmt =
    match stmt with
    | Block stmts ->
        let new_ctx = { ctx with var_offset = [] :: ctx.var_offset } in
        let (ctx_after, asm) = gen_stmts new_ctx stmts in
        let popped_ctx = 
            match ctx_after.var_offset with
            | _ :: outer_scopes -> { ctx_after with var_offset = outer_scopes }
            | [] -> failwith "Scope stack underflow"
        in
        (popped_ctx, asm)
    
    | VarDecl (name, expr) ->
        let (ctx, expr_asm, reg) = gen_expr ctx expr in
        let ctx = add_var ctx name 4 in
        let offset = get_var_offset ctx name in
        
        let store_asm = match reg with
            | Physical r -> gen_large_offset_access "sp" offset "sw" r
            | Spill spill_offset -> 
                Printf.sprintf "%s\n%s"
                    (gen_large_offset_access "sp" spill_offset "lw" "t0")
                    (gen_large_offset_access "sp" offset "sw" "t0")
        in
        
        let asm = expr_asm ^ "\n" ^ store_asm in
        (free_temp_reg ctx reg, asm)
    
    | VarAssign (name, expr) ->
        let offset = get_var_offset ctx name in
        let (ctx, expr_asm, reg) = gen_expr ctx expr in
        
        let store_asm = match reg with
            | Physical r -> gen_large_offset_access "sp" offset "sw" r
            | Spill spill_offset -> 
                Printf.sprintf "%s\n%s"
                    (gen_large_offset_access "sp" spill_offset "lw" "t0")
                    (gen_large_offset_access "sp" offset "sw" "t0")
        in
        
        let asm = expr_asm ^ "\n" ^ store_asm in
        (free_temp_reg ctx reg, asm)
    
     | If (cond, then_stmt, else_stmt) ->
        let (ctx, cond_asm, cond_reg) = gen_expr ctx cond in
        let (ctx, then_label) = fresh_label ctx "if_then" in
        let (ctx, else_label) = fresh_label ctx "if_else" in
        let (ctx, end_label) = fresh_label ctx "if_end" in
        
        let cond_value = match cond_reg with
            | Physical r -> r
            | Spill offset -> 
                gen_large_offset_access "sp" offset "lw" "t0" |> ignore;
                "t0"
        in
        
        let (ctx, then_asm) = gen_stmt ctx then_stmt in
        let (ctx, else_asm) = match else_stmt with
            | Some s -> gen_stmt ctx s
            | None -> (ctx, "") in
        
        let asm = 
          let parts = [cond_asm] @
                     [Printf.sprintf "    beqz %s, %s" cond_value else_label] @
                     [Printf.sprintf "    j %s" then_label] @
                     [Printf.sprintf "%s:" else_label] @
                     (if else_asm = "" then [] else [else_asm]) @
                     [Printf.sprintf "    j %s" end_label] @
                     [Printf.sprintf "%s:" then_label] @
                     (if then_asm = "" then [] else [then_asm]) @
                     [Printf.sprintf "    j %s" end_label] @
                     [Printf.sprintf "%s:" end_label] in
          String.concat "\n" (List.filter (fun s -> s <> "") parts) in
        (free_temp_reg ctx cond_reg, asm)
    
    | While (cond, body) ->
        let (ctx, begin_label) = fresh_label ctx "loop_begin" in
        let (ctx, next_label) = fresh_label ctx "loop_next" in
        let (ctx, end_label) = fresh_label ctx "loop_end" in
        
        let loop_ctx = {
            break_label = end_label;
            continue_label = next_label
        } in
        
        let loop_stack = loop_ctx :: ctx.loop_stack in
        let ctx_with_loop = { ctx with loop_stack } in
        
        let (ctx_after_body, body_asm) = gen_stmt ctx_with_loop body in
        
        (* 修复：使用_忽略未使用的ctx变量 *)
        let (_, cond_asm, cond_reg) = gen_expr ctx_after_body cond in
        
        let cond_value = match cond_reg with
            | Physical r -> r
            | Spill offset -> 
                gen_large_offset_access "sp" offset "lw" "t0" |> ignore;
                "t0"
        in
        
        let ctx_after_loop = { ctx_after_body with 
            loop_stack = List.tl ctx_after_body.loop_stack } in
        
        let asm = 
          let parts = [Printf.sprintf "%s:" begin_label] @
                     [cond_asm] @
                     [Printf.sprintf "    beqz %s, %s" cond_value end_label] @
                     (if body_asm = "" then [] else [body_asm]) @
                     [Printf.sprintf "%s:" next_label] @
                     [Printf.sprintf "    j %s" begin_label] @
                     [Printf.sprintf "%s:" end_label] in
          String.concat "\n" (List.filter (fun s -> s <> "") parts) in
        (free_temp_reg ctx_after_loop cond_reg, asm)
    
    | Break ->
        (match ctx.loop_stack with
        | loop_ctx::_ -> 
            (ctx, Printf.sprintf "    j %s" loop_ctx.break_label)
        | [] -> failwith "break outside loop")
    
    | Continue ->
        (match ctx.loop_stack with
        | loop_ctx::_ ->
            (ctx, Printf.sprintf "    j %s" loop_ctx.continue_label)
        | [] -> failwith "continue outside loop")
    
    | Return expr_opt ->
        (* 修复：使用_忽略未使用的reg变量 *)
        let (ctx, expr_asm, _) = 
            match expr_opt with
            | Some expr -> 
                let (ctx, asm, r) = gen_expr ctx expr in
                let result_asm = match r with
                    | Physical "a0" -> asm
                    | Physical r -> asm ^ "\n" ^ Printf.sprintf "    mv a0, %s" r
                    | Spill offset -> asm ^ "\n" ^ gen_large_offset_access "sp" offset "lw" "a0"
                in
                (free_temp_reg ctx r, result_asm, r)
            | None -> (ctx, "", Physical "a0")
        in
        (* 跳转到统一的返回点 *)
        (ctx, expr_asm ^ "\n    j " ^ ctx.return_label)
    
    | EmptyStmt -> (ctx, "")
    | ExprStmt e -> 
        let (ctx, asm, reg) = gen_expr ctx e in 
        (free_temp_reg ctx reg, asm)

(* 函数代码生成 *)
let gen_function func =
    let ctx = create_context func.name in
    let ctx =
        List.fold_left (fun ctx param ->
            add_var ctx param 4
        ) ctx func.params
    in
    
    (* 生成参数保存代码 *)
    let save_params_asm =
        let rec gen_save params index asm =
            match params with
            | [] -> asm
            | param::rest ->
                let offset = get_var_offset ctx param in
                if index < 8 then (
                    let reg = Printf.sprintf "a%d" index in
                    let store_instr = gen_large_offset_access "sp" offset "sw" reg in
                    gen_save rest (index + 1) (asm ^ store_instr ^ "\n")
                ) else (
                    let stack_offset = (index - 8) * 4 in
                    let load_asm = gen_large_offset_access "sp" stack_offset "lw" "t0" in
                    let store_asm = gen_large_offset_access "sp" offset "sw" "t0" in
                    gen_save rest (index + 1) (asm ^ load_asm ^ "\n" ^ store_asm ^ "\n")
                )
        in
        gen_save func.params 0 ""
    in
    
    (* 生成函数体 *)
    let (ctx, body_asm) = gen_stmts ctx [func.body] in
    
    (* 计算实际帧大小 *)
    let frame_size = align_stack (ctx.saved_area_size + ctx.max_local_offset) stack_align in
    let ctx = { ctx with frame_size } in
    
    (* 生成序言和结语 *)
    let prologue_asm =
        let save_regs_asm =
            let reg_saves = List.mapi (fun i reg ->
                gen_large_offset_access "sp" (i * 4) "sw" reg
                ) ctx.saved_regs
            in
            let ra_save = gen_large_offset_access "sp" (List.length ctx.saved_regs * 4) "sw" "ra" in
            String.concat "\n" (reg_saves @ [ra_save])
        in
        Printf.sprintf ".globl %s\n%s:\n%s\n%s"
            func.name func.name 
            (if ctx.frame_size <= 2047 then 
                Printf.sprintf "    addi sp, sp, -%d" ctx.frame_size
             else
                let hi = (ctx.frame_size asr 12) + if ctx.frame_size land 0x800 != 0 then 1 else 0 in
                let lo = ctx.frame_size land 0xFFF in
                (* 如果低12位超过2047，调整高位 *)
                let hi, lo = 
                    if lo >= 2048 then (hi + 1, lo - 4096)
                    else (hi, lo)
                in
                Printf.sprintf "    lui t6, %d\n    addi t6, t6, %d\n    sub sp, sp, t6" hi lo)
            save_regs_asm
    in
    
    let epilogue_asm = gen_epilogue ctx in
    
    (* 组合完整函数代码 *)
    prologue_asm ^ "\n" ^ 
    save_params_asm ^ 
    body_asm ^ "\n" ^ 
    ctx.return_label ^ ":\n" ^ 
    epilogue_asm
    
(* 编译单元代码生成 *)
let compile cu =
    let main_exists = ref false in
    let funcs_asm = List.map (fun func ->
        if func.name = "main" then main_exists := true;
        gen_function func
    ) cu in
    
    if not !main_exists then
        failwith "Missing main function";
    
    String.concat "\n\n" funcs_asm
