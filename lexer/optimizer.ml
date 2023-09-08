open Llvm
open Llvm_scalar_opts
open Llvm_passmgr_builder

let optimize m = 
  let pm = PassManager.create () in 
  let opts = [
    Llvm_scalar_opts.add_memory_to_register_promotion;
    Llvm_scalar_opts.add_instruction_combination; 
    Llvm_scalar_opts.add_reassociation;
    Llvm_scalar_opts.add_cfg_simplification; 
    Llvm_scalar_opts.add_gvn;
    Llvm_scalar_opts.add_aggressive_dce] in
  List.iter (( |> ) pm) opts;
  ignore (PassManager.run_module m pm);
  match Llvm_analysis.verify_module m with 
  | None -> ()
  | Some s -> failwith "Cannot do the optimizations"