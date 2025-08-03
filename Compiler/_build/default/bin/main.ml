open ToyClib
open Ast
open Semantic
open Codegen

(* 解析字符串为AST *)
let parse s : comp_unit =
  let lexbuf = Lexing.from_string s in
    let ast = Parser.comp_unit Lexer.token lexbuf in
    ast

(* 从文件读取并解析 *)
let parse_file filename : comp_unit =
  let in_channel = open_in filename in
  let file_content = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;
  parse file_content

(* 测试单个文件 *)
(* 更新 test_file 函数 *)
let test_file filename =
  Printf.printf "=== Testing file: %s ===\n" filename;
  try
    (* 1. 解析 *)
    let ast = parse_file filename in
    Printf.printf "AST:\n%s\n\n" (string_of_comp_unit ast);
    Printf.printf "Parse successful!\n";

    (* 2. 语义分析 *)
    analyze ast; (* 新增调用 *)
    Printf.printf "Semantic analysis successful!\n\n";

   (* 3. 代码生成 *)
    let asm_code = compile ast in
    Printf.printf "\nGenerated Assembly:\n%s\n" asm_code;
    
    (* 4. 写入.s文件 *)
    let output_filename = (Filename.remove_extension filename) ^ ".s" in
    let oc = open_out output_filename in
    output_string oc asm_code;
    close_out oc;
    Printf.printf "Assembly saved to: %s\n\n" output_filename;

  with
  | Sys_error msg ->
      Printf.eprintf "File error: %s\n\n" msg
  | Lexer.LexError msg ->
      Printf.eprintf "Lexer error: %s\n\n" msg
  | Parsing.Parse_error ->
      Printf.eprintf "Parser error\n\n"
  | SemanticError msg -> (* 新增错误捕获 *)
      Printf.eprintf "Semantic error: %s\n\n" msg
  | e ->
      Printf.eprintf "An unexpected error occurred: %s\n\n" (Printexc.to_string e)

let () =
  
  (* 测试文件 *)
  test_file "test/01_minimal.tc";
  test_file "test/02_assignment.tc";
  test_file "test/03_if_else.tc";
  test_file "test/04_while_break.tc";
  test_file "test/05_function_call.tc";
  test_file "test/06_continue.tc";
  test_file "test/07_scope_shadow.tc";
  test_file "test/08_short_circuit.tc";
  test_file "test/09_recursion.tc";
  test_file "test/10_void_fn.tc";
  test_file "test/11_precedence.tc";
  test_file "test/12_division_check.tc";
  test_file "test/13_scope_block.tc";
  test_file "test/14_nested_if_while.tc";
  test_file "test/15_multiple_return_paths.tc";
  test_file "test/16_complex_syntax.tc";
  test_file "test/17_complex_expressions.tc";
  test_file "test/18_many_variables.tc";
  test_file "test/19_many_arguments.tc";
  test_file "test/20_comprehensive.tc";

  Printf.printf "All tests completed!\n"

  