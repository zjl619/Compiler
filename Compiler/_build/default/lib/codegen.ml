open Ast
(* 寄存器分类 *)
type reg_type = 
  | CallerSaved   (* 调用者保存寄存器 *)
  | CalleeSaved   (* 被调用者保存寄存器 *)
  | TempReg       (* 临时寄存器 *)

(* 上下文类型 *)
type context = {
    current_func: string;          (* 当前函数名 *)
    local_offset: int;             (* 局部变量区当前偏移量 *)
    frame_size: int;               (* 栈帧总大小（计算后设置） *)
    var_offset: (string, int) Hashtbl.t; (* 变量名 -> 栈偏移量 *)
    next_temp: int;                (* 下一个临时寄存器编号 *)
    label_counter: int;            (* 标签计数器 *)
    loop_stack: (string * string) list; (* 循环标签栈 (begin_label, end_label) *)
    saved_regs: string list;       (* 需要保存的寄存器列表 *)
    reg_map: (string * reg_type) list; (* 寄存器映射 *)
    param_count: int;              (* 当前参数计数 *)
    temp_regs_used: int;           (* 已使用的临时寄存器数量 *)
    saved_area_size: int;          (* 保存区域大小（包含RA和保存的寄存器） *)
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
    (* 初始保存区域大小：RA(4字节) + 所有被调用者保存寄存器(12*4=48字节) *)
    { current_func = func_name;
      local_offset = 0;
      frame_size = 0;
      var_offset = Hashtbl.create 16;
      next_temp = 0;
      label_counter = 0;
      loop_stack = [];
      saved_regs = ["s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"];
      reg_map = reg_map;
      param_count = 0;
      temp_regs_used = 0;
      saved_area_size = 52 } (* 4(ra) + 12*4(regs) = 52 *)

(* 栈对齐常量 *)
let stack_align = 16

(* 获取唯一标签 *)
let fresh_label ctx prefix =
    let n = ctx.label_counter in
    { ctx with label_counter = n + 1 }, 
    Printf.sprintf ".L%s%d" prefix n

(* 获取变量偏移量 *)
let get_var_offset ctx name =
    try Hashtbl.find ctx.var_offset name
    with Not_found -> 
        failwith (Printf.sprintf "Undefined variable: %s" name)

(* 添加变量到符号表 *)
let add_var ctx name size =
    let offset = ctx.saved_area_size + ctx.local_offset in
    Hashtbl.add ctx.var_offset name offset;
    { ctx with local_offset = ctx.local_offset + size }

(* 分配临时寄存器 *)
let alloc_temp_reg ctx =
    if ctx.temp_regs_used >= 7 then
        failwith "No more temporary registers available";
    let reg = Printf.sprintf "t%d" ctx.temp_regs_used in
    { ctx with temp_regs_used = ctx.temp_regs_used + 1 }, reg

(* 释放临时寄存器 *)
let free_temp_reg ctx =
    { ctx with temp_regs_used = ctx.temp_regs_used - 1 }

(* 计算栈对齐 *)
let align_stack size align =
    let remainder = size mod align in
    if remainder = 0 then size else size + (align - remainder)

(* 函数序言生成 *)
let gen_prologue ctx func =
    (* 总栈大小 = 保存区域 + 局部变量区域 *)
    let total_size = align_stack (ctx.saved_area_size + ctx.local_offset) stack_align in
    (* 生成保存寄存器的汇编代码 - 顺序保存s0-s11，最后保存ra *)
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
    addi sp, sp, -%d
%s
" func.name func.name total_size save_regs_asm in
    (asm, { ctx with frame_size = total_size })
let gen_epilogue ctx =
    (* 生成恢复寄存器的汇编代码 - 逆序恢复：先恢复ra，然后s11-s0 *)
    let restore_regs_asm = 
        (* 关键修复：按入栈顺序的逆序恢复 *)
        let restore_list = 
            ["ra"] @ (List.rev ctx.saved_regs) (* 恢复顺序：ra, s11, s10, ..., s0 *)
        in
        (* 关键修复：偏移量计算与保存时完全匹配 *)
        List.mapi (fun i reg ->
            let offset = (List.length ctx.saved_regs * 4) - (i * 4) in
            Printf.sprintf "    lw %s, %d(sp)" reg offset
        ) restore_list
        |> String.concat "\n"
    in
    
    Printf.sprintf "
%s
    addi sp, sp, %d
    ret
" restore_regs_asm ctx.frame_size

(* 表达式代码生成 *)
let rec gen_expr ctx expr =
    match expr with
    | IntLit n -> 
        let (ctx, reg) = alloc_temp_reg ctx in
        (ctx, Printf.sprintf "    li %s, %d" reg n, reg)
    | Var name ->
        let offset = get_var_offset ctx name in
        let (ctx, reg) = alloc_temp_reg ctx in
        (ctx, Printf.sprintf "    lw %s, %d(sp)" reg offset, reg)
    | BinOp (e1, op, e2) ->
        let (ctx, asm1, reg1) = gen_expr ctx e1 in
        let (ctx, asm2, reg2) = gen_expr ctx e2 in
        let (ctx, reg_dest) = alloc_temp_reg ctx in
        let instr = match op with
        | Add -> Printf.sprintf "add %s, %s, %s" reg_dest reg1 reg2
        | Sub -> Printf.sprintf "sub %s, %s, %s" reg_dest reg1 reg2
        | Mul -> Printf.sprintf "mul %s, %s, %s" reg_dest reg1 reg2
        | Div -> Printf.sprintf "div %s, %s, %s" reg_dest reg1 reg2
        | Mod -> Printf.sprintf "rem %s, %s, %s" reg_dest reg1 reg2
        | Lt  -> Printf.sprintf "slt %s, %s, %s" reg_dest reg1 reg2
        | Le  -> Printf.sprintf "slt %s, %s, %s\n    xori %s, %s, 1" reg_dest reg2 reg1 reg_dest reg_dest
        | Gt  -> Printf.sprintf "slt %s, %s, %s" reg_dest reg2 reg1
        | Ge  -> Printf.sprintf "slt %s, %s, %s\n    xori %s, %s, 1" reg_dest reg1 reg2 reg_dest reg_dest
        | Eq  -> Printf.sprintf "sub %s, %s, %s\n    seqz %s, %s" reg_dest reg1 reg2 reg_dest reg_dest
        | Ne  -> Printf.sprintf "sub %s, %s, %s\n    snez %s, %s" reg_dest reg1 reg2 reg_dest reg_dest
        | And -> Printf.sprintf "and %s, %s, %s" reg_dest reg1 reg2
        | Or  -> Printf.sprintf "or %s, %s, %s" reg_dest reg1 reg2
        in
        (* 释放临时寄存器 *)
        let ctx = free_temp_reg (free_temp_reg ctx) in
        (ctx, asm1 ^ "\n" ^ asm2 ^ "\n" ^ instr, reg_dest)
    | UnOp (op, e) ->
        let (ctx, asm, reg) = gen_expr ctx e in
        let (ctx, reg_dest) = alloc_temp_reg ctx in
        let instr = match op with
        | UPlus  -> Printf.sprintf "mv %s, %s" reg_dest reg
        | UMinus -> Printf.sprintf "neg %s, %s" reg_dest reg
        | Not    -> Printf.sprintf "seqz %s, %s" reg_dest reg
        in
        let ctx = free_temp_reg ctx in
        (ctx, asm ^ "\n" ^ instr, reg_dest)
    | FuncCall (name, args) ->
        (* 计算额外参数数量 *)
        let n_extra = max (List.length args - 8) 0 in
        (* 总临时空间 = 临时寄存器保存区(28字节) + 额外参数区 *)
        let temp_space = 28 + n_extra * 4 in  (* 固定7个临时寄存器*4=28 *)
        let aligned_temp_space = align_stack temp_space stack_align in
        
        (* 调整栈指针 *)
        let stack_adj_asm = 
            if aligned_temp_space > 0 then 
                Printf.sprintf "    addi sp, sp, -%d\n" aligned_temp_space
            else ""
        in
        
        (* 保存临时寄存器 *)
        let save_temps_asm = 
            List.init 7 (fun i -> 
                Printf.sprintf "    sw t%d, %d(sp)" i (i * 4))
            |> String.concat "\n"
        in
        
        (* 生成参数代码 *)
        let (ctx, arg_asm, _arg_count) = gen_args ctx args aligned_temp_space in
        
        (* 函数调用 *)
        let call_asm = Printf.sprintf "    call %s" name in
        
        (* 恢复临时寄存器 *)
        let restore_temps_asm = 
            List.init 7 (fun i -> 
                Printf.sprintf "    lw t%d, %d(sp)" i (i * 4))
            |> String.concat "\n"
        in
        
        (* 恢复栈指针 *)
        let restore_stack_asm = 
            if aligned_temp_space > 0 then 
                Printf.sprintf "    addi sp, sp, %d" aligned_temp_space
            else ""
        in
        
        (* 将返回值移动到目标寄存器 *)
        let (ctx, reg_dest) = alloc_temp_reg ctx in
        let move_result = Printf.sprintf "    mv %s, a0" reg_dest in
        
        (* 组合汇编代码 *)
        let asm = stack_adj_asm ^ save_temps_asm ^ "\n" ^ arg_asm ^ call_asm ^ "\n" ^ 
                  restore_temps_asm ^ "\n" ^ restore_stack_asm ^ "\n" ^ move_result in
        
        (ctx, asm, reg_dest)

and gen_args ctx args _temp_space =
    let rec process_args ctx asm count = function
        | [] -> (ctx, asm, count)
        | arg::rest when count < 8 ->  (* 前8个参数使用寄存器 *)
            let (ctx, arg_asm, reg) = gen_expr ctx arg in
            let target_reg = Printf.sprintf "a%d" count in
            let new_asm = 
                if reg = target_reg then 
                    asm ^ arg_asm 
                else 
                    asm ^ arg_asm ^ Printf.sprintf "\n    mv %s, %s" target_reg reg
            in
            let ctx = free_temp_reg ctx in  (* 释放表达式寄存器 *)
            process_args ctx new_asm (count + 1) rest
        | arg::rest ->  (* 额外参数使用栈传递 *)
            (* 栈传递参数位置：临时空间基址 + 28(临时寄存器区) + (count-8)*4 *)
            let stack_offset = 28 + (count - 8) * 4 in
            let (ctx, arg_asm, reg) = gen_expr ctx arg in
            let new_asm = asm ^ arg_asm ^ 
                         Printf.sprintf "\n    sw %s, %d(sp)" reg stack_offset in
            let ctx = free_temp_reg ctx in  (* 释放表达式寄存器 *)
            process_args ctx new_asm (count + 1) rest
    in
    
    process_args ctx "" 0 args

(* 语句代码生成 *)
let rec gen_stmt ctx stmt =
    match stmt with
    | Block stmts ->
        List.fold_left (fun (ctx, asm) s ->
            let (ctx, s_asm) = gen_stmt ctx s in
            (ctx, asm ^ "\n" ^ s_asm)
        ) (ctx, "") stmts
    
    | VarDecl (name, expr) ->
        let (ctx, expr_asm, reg) = gen_expr ctx expr in
        let ctx = add_var ctx name 4 in
        let offset = get_var_offset ctx name in
        let asm = expr_asm ^ Printf.sprintf "\n    sw %s, %d(sp)" reg offset in
        (free_temp_reg ctx, asm)
    
    | VarAssign (name, expr) ->
        let offset = get_var_offset ctx name in
        let (ctx, expr_asm, reg) = gen_expr ctx expr in
        let asm = expr_asm ^ Printf.sprintf "\n    sw %s, %d(sp)" reg offset in
        (free_temp_reg ctx, asm)
    
    | If (cond, then_stmt, else_stmt) ->
        let (ctx, cond_asm, cond_reg) = gen_expr ctx cond in
        let (ctx, then_label) = fresh_label ctx "if_then" in
        let (ctx, else_label) = fresh_label ctx "if_else" in
        let (ctx, end_label) = fresh_label ctx "if_end" in
        
        let (ctx, then_asm) = gen_stmt ctx then_stmt in
        let (ctx, else_asm) = match else_stmt with
            | Some s -> gen_stmt ctx s
            | None -> (ctx, "") in
        
        let asm = cond_asm ^
                Printf.sprintf "\n    beqz %s, %s" cond_reg else_label ^
                Printf.sprintf "\n    j %s" then_label ^
                Printf.sprintf "\n%s:" else_label ^
                else_asm ^
                Printf.sprintf "\n    j %s" end_label ^
                Printf.sprintf "\n%s:" then_label ^
                then_asm ^
                Printf.sprintf "\n%s:" end_label in
        (free_temp_reg ctx, asm)
    
    | While (cond, body) ->
        let (ctx, begin_label) = fresh_label ctx "loop_begin" in
        let (ctx, end_label) = fresh_label ctx "loop_end" in
        let (ctx, cond_asm, cond_reg) = gen_expr ctx cond in
        
        let loop_ctx = { ctx with 
            loop_stack = (begin_label, end_label) :: ctx.loop_stack } in
        let (ctx_after_body, body_asm) = gen_stmt loop_ctx body in
        
        (* 仅弹出循环栈，保留其他字段 *)
        let ctx_after_loop = { ctx_after_body with 
            loop_stack = List.tl ctx_after_body.loop_stack } in
        
        let asm = Printf.sprintf "%s:" begin_label ^
                cond_asm ^
                Printf.sprintf "\n    beqz %s, %s" cond_reg end_label ^
                body_asm ^
                Printf.sprintf "\n    j %s" begin_label ^
                Printf.sprintf "\n%s:" end_label in
        (free_temp_reg ctx_after_loop, asm)
    
    | Break ->
        (match ctx.loop_stack with
        | (_, end_label)::_ -> 
            (ctx, Printf.sprintf "    j %s" end_label)
        | [] -> failwith "break outside loop")
    
    | Continue ->
        (match ctx.loop_stack with
        | (begin_label, _)::_ -> 
            (ctx, Printf.sprintf "    j %s" begin_label)
        | [] -> failwith "continue outside loop")
    
    | Return expr_opt ->
        let (ctx, expr_asm, _reg) = 
            match expr_opt with
            | Some expr -> 
                let (ctx, asm, r) = gen_expr ctx expr in
                if r = "a0" then (ctx, asm, r)
                else (ctx, asm ^ Printf.sprintf "\n    mv a0, %s" r, "a0")
            | None -> (ctx, "", "a0")
        in
        (free_temp_reg ctx, expr_asm)
    
    | EmptyStmt -> (ctx, "")
    | ExprStmt e -> 
        let (ctx, asm, _) = gen_expr ctx e in 
        (free_temp_reg ctx, asm)

(* 函数代码生成 *)
let gen_function func =
    let ctx = create_context func.name in
    
    (* 处理参数 - 在局部变量区分配空间 *)
    let ctx = 
        List.fold_left (fun ctx param ->
            let ctx = add_var ctx param 4 in
            { ctx with param_count = ctx.param_count + 1 }
        ) ctx func.params
    in
    
    (* 生成函数序言 *)
    let (prologue_asm, ctx) = gen_prologue ctx func in
    
    (* 保存参数到局部变量区 - 关键修复：栈传递参数偏移量 *)
    let save_params_asm = 
        let rec gen_save params index asm =
            match params with
            | [] -> asm
            | param::rest ->
                let offset = get_var_offset ctx param in
                if index < 8 then (
                    (* 寄存器参数 *)
                    let reg = Printf.sprintf "a%d" index in
                    gen_save rest (index + 1) 
                        (asm ^ Printf.sprintf "    sw %s, %d(sp)\n" reg offset)
                ) else (
                    (* 关键修复：栈传递参数在调用者栈帧中，不在当前栈帧 *)
                    let stack_offset = (index - 8) * 4 in  (* 移除ctx.frame_size *)
                    let (_, reg) = alloc_temp_reg ctx in  (* 使用临时寄存器 *)
                    let load_asm = Printf.sprintf "    lw %s, %d(sp)\n" reg stack_offset in
                    let store_asm = Printf.sprintf "    sw %s, %d(sp)" reg offset in
                    gen_save rest (index + 1) 
                        (asm ^ load_asm ^ store_asm ^ "\n")
                )
        in
        gen_save func.params 0 ""
    in
    
    (* 生成函数体 *)
    let (_, body_asm) = gen_stmt ctx func.body in
    
    (* 生成函数结语 *)
    let epilogue_asm = gen_epilogue ctx in
    
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
    
    ".text\n" ^ String.concat "\n\n" funcs_asm