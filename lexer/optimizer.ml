open Llvm
open Llvm_scalar_opts
open Llvm_passmgr_builder

let optimize m = 
  let pm = PassManager.create () in 
  let opts = [
    add_memory_to_register_promotion;
    add_instruction_combination; 
    add_reassociation;
    add_cfg_simplification; 
    add_gvn;
    add_aggressive_dce] in
  List.iter (( |> ) pm) opts;
  ignore (PassManager.run_module m pm);
  match Llvm_analysis.verify_module m with 
  | None -> ()
  | Some s -> failwith "Cannot do the optimizations"