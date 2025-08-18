(* codegen.ml *)
open Ast

(* 寄存器分类 *)
type reg_type = 
  | CallerSaved   (* 调用者保存寄存器 *)
  | CalleeSaved   (* 被调用者保存寄存器 *)
  | TempReg       (* 临时寄存器 *)

(* 上下文类型 *)
type context = {
    current_func: string;
    local_offset: int;
    frame_size: int;
    var_offset: (string * int) list list;
    next_temp: int;
    label_counter: int;
    loop_stack: (string * string * string) list;
    saved_regs: string list;
    reg_map: (string * reg_type) list;
    param_count: int;
    used_regs: string list;
    saved_area_size: int;
    max_local_offset: int;
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
      used_regs = [];
      max_local_offset = 0; 
      saved_area_size = 0 }

(* 栈对齐常量 *)
let stack_align = 16

(* 获取唯一标签 *)
let fresh_label ctx prefix =
    let n = ctx.label_counter in
    { ctx with label_counter = n + 1 }, 
    Printf.sprintf ".L%s_%s%d" ctx.current_func prefix n

(* 获取变量偏移量 *)
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

(* 寄存器分配函数 *)
let alloc_temp_reg ctx =
    let temp_regs = ["t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"] in
    let saved_regs_list = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"] in
    let all_regs = temp_regs @ saved_regs_list in
    
    let available_reg = List.find_opt (fun reg -> 
        not (List.mem reg ctx.used_regs)) all_regs in
    
    match available_reg with
    | Some reg -> 
        { ctx with used_regs = reg :: ctx.used_regs }, reg
    | None ->
        let spill_offset = ctx.saved_area_size + ctx.max_local_offset + (List.length ctx.used_regs) * 4 in
        let spill_reg = Printf.sprintf "SPILL_%d" spill_offset in
        { ctx with used_regs = spill_reg :: ctx.used_regs }, spill_reg

(* 寄存器释放函数 *)
let free_temp_reg ctx reg =
    { ctx with used_regs = List.filter (fun r -> r <> reg) ctx.used_regs }

(* 计算栈对齐 *)
let align_stack size align =
    let remainder = size mod align in
    if remainder = 0 then size else size + (align - remainder)

(* 函数序言生成 - 关键修复 *)
let gen_prologue ctx func =
    (* 动态计算保存区域大小 *)
    let saved_regs_count = List.length ctx.saved_regs in
    let saved_area_size = (saved_regs_count + 1) * 4 in  (* +1 for ra *)
    
    (* 使用实际局部变量大小计算帧大小 *)
    let total_size = align_stack (saved_area_size + ctx.max_local_offset) stack_align in
    
    (* 生成保存寄存器的汇编 *)
    let save_regs_asm = 
        let save_instrs = 
            List.mapi (fun i reg -> 
                Printf.sprintf "    sw %s, %d(sp)" reg (i * 4)
            ) ctx.saved_regs
            @ [Printf.sprintf "    sw ra, %d(sp)" (saved_regs_count * 4)]
        in
        String.concat "\n" save_instrs
    in
    
    let asm = Printf.sprintf "
    .globl %s
%s:
    addi sp, sp, -%d
%s
" func.name func.name total_size save_regs_asm in
    
    (asm, { ctx with 
        frame_size = total_size;
        saved_area_size = saved_area_size;
    })

(* 函数结语生成 - 关键修复 *)
let gen_epilogue ctx =
    let restore_regs_asm = 
        let ra_restore = Printf.sprintf "    lw ra, %d(sp)" (ctx.saved_area_size - 4) in
        let s_regs_restore = 
            List.rev ctx.saved_regs
            |> List.mapi (fun i reg ->
                let offset = i * 4 in
                Printf.sprintf "    lw %s, %d(sp)" reg offset)
            |> String.concat "\n" in
        ra_restore ^ "\n" ^ s_regs_restore
    in
    
    Printf.sprintf "
%s
    addi sp, sp, %d
    ret
" restore_regs_asm ctx.frame_size

(* 溢出寄存器处理 *)
let is_spill_reg reg = 
    String.length reg > 5 && String.sub reg 0 5 = "SPILL"

let get_spill_offset reg =
    if is_spill_reg reg then
        int_of_string (String.sub reg 6 (String.length reg - 6))
    else
        failwith "Not a spill register"

let gen_load_spill reg temp_reg =
    if is_spill_reg reg then
        let offset = get_spill_offset reg in
        Printf.sprintf "    lw %s, %d(sp)" temp_reg offset
    else ""

let gen_store_spill reg temp_reg =
    if is_spill_reg reg then
        let offset = get_spill_offset reg in
        Printf.sprintf "    sw %s, %d(sp)" temp_reg offset
    else ""

(* 表达式代码生成 *)
let rec gen_expr ctx expr =
    (* 保持不变 *)
    (* ... *)

(* 函数调用处理 - 关键修复 *)
| FuncCall (name, args) ->
    let (ctx, arg_asm, arg_regs) = gen_args ctx args in
    
    (* 计算栈参数数量 *)
    let n_extra = max (List.length args - 8) 0 in
    let stack_args_size = n_extra * 4 in
    
    (* 16字节对齐的栈空间 *)
    let aligned_stack_args_size = align_stack stack_args_size stack_align in
    
    (* 调整栈指针 *)
    let stack_adj_asm = 
      if aligned_stack_args_size > 0 then 
        Printf.sprintf "    addi sp, sp, -%d" aligned_stack_args_size
      else "" in
    
    (* 传递参数 *)
    let move_args_asm = 
      let rec move_args regs index parts =
        match regs with
        | [] -> String.concat "\n" (List.filter (fun s -> s <> "") parts)
        | reg::rest when index < 8 ->
            let target = Printf.sprintf "a%d" index in
            let load_src = gen_load_spill reg "t0" in
            let actual_src = if is_spill_reg reg then "t0" else reg in
            let move_instr = 
              if actual_src = target then ""
              else Printf.sprintf "    mv %s, %s" target actual_src
            in
            let new_parts = parts @ 
              (if load_src = "" then [] else [load_src]) @
              (if move_instr = "" then [] else [move_instr]) in
            move_args rest (index+1) new_parts
        | reg::rest ->
            (* 栈参数存储在调用者栈帧顶部 *)
            let stack_offset = (index - 8) * 4 in
            let load_src = gen_load_spill reg "t0" in
            let actual_src = if is_spill_reg reg then "t0" else reg in
            let store_instr = Printf.sprintf "    sw %s, %d(sp)" actual_src stack_offset in
            let new_parts = parts @
              (if load_src = "" then [] else [load_src]) @
              [store_instr] in
            move_args rest (index+1) new_parts
      in
      move_args arg_regs 0 []
    in
    
    let call_asm = Printf.sprintf "    call %s" name in
    
    (* 恢复栈指针 *)
    let restore_stack_asm = 
      if aligned_stack_args_size > 0 then 
        Printf.sprintf "    addi sp, sp, %d" aligned_stack_args_size
      else "" in
    
    (* 处理结果 *)
    let (ctx, reg_dest) = alloc_temp_reg ctx in
    let move_result = 
      if is_spill_reg reg_dest then
        Printf.sprintf "    mv t0, a0\n%s" (gen_store_spill reg_dest "t0")
      else
        Printf.sprintf "    mv %s, a0" reg_dest
    in
    
    let asm = 
      let parts = [arg_asm] @
                 (if stack_adj_asm = "" then [] else [stack_adj_asm]) @
                 [move_args_asm] @
                 [call_asm] @
                 [restore_stack_asm] @
                 [move_result] in
      String.concat "\n" (List.filter (fun s -> s <> "") parts) in
    
    (* 释放参数寄存器 *)
    let ctx = List.fold_left (fun ctx reg -> free_temp_reg ctx reg) ctx arg_regs in
    
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

(* 语句代码生成 *)
(* 保持不变 *)
(* ... *)

(* 函数代码生成 - 关键修复栈参数访问 *)
let gen_function func =
    let ctx = create_context func.name in
    (* 添加参数到作用域 *)
    let ctx = List.fold_left (fun ctx param -> add_var ctx param 4) ctx func.params in
    
    (* 生成序言 *)
    let (prologue_asm, ctx) = gen_prologue ctx func in
    
    (* 保存参数到局部变量 - 修复栈参数访问 */
    let save_params_asm =
        let buf = Buffer.create 256 in
        List.iteri (fun i param ->
            let offset = get_var_offset ctx param in
            if i < 8 then (
                (* 寄存器参数直接保存 *)
                let reg = Printf.sprintf "a%d" i in
                Buffer.add_string buf (Printf.sprintf "    sw %s, %d(sp)\n" reg offset)
            ) else (
                (* 关键修复：栈参数从调用者栈帧访问 *)
                let param_offset = (i - 8) * 4 in
                Buffer.add_string buf (Printf.sprintf "    lw t0, %d(sp)\n" (ctx.frame_size + param_offset));
                Buffer.add_string buf (Printf.sprintf "    sw t0, %d(sp)\n" offset)
            )
        ) func.params;
        Buffer.contents buf
    in
    
    (* 生成函数体 *)
    let (_, body_asm) =
        match func.body with
        | Block stmts -> gen_stmts ctx stmts
        | _ -> gen_stmt ctx func.body
    in
    
    (* 只在函数体没有显式return时添加epilogue *)
    let needs_epilogue = 
        let rec has_return = function
            | Return _ -> true
            | Block stmts -> List.exists has_return stmts
            | If (_, then_stmt, Some else_stmt) -> has_return then_stmt || has_return else_stmt
            | If (_, _, None) -> false
            | While (_, _) -> false
            | _ -> false
        in
        not (has_return func.body)
    in
    
    let epilogue_asm = 
        if needs_epilogue then gen_epilogue ctx 
        else ""
    in
    
    prologue_asm ^ "\n" ^ save_params_asm ^ body_asm ^ epilogue_asm
    
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
