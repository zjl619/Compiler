(* codegen.ml *)
open Ast

(* ... 前面的代码保持不变 ... *)

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

(* ... 中间代码保持不变 ... *)

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

(* 表达式代码生成 - 修复函数调用参数处理 *)
let rec gen_expr ctx expr =
    match expr with
    (* ... 其他表达式类型保持不变 ... *)
    
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
        
        (* 修复：确保在恢复临时寄存器前保存结果 *)
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

(* ... 后面的代码保持不变 ... *)
