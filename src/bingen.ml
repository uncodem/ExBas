
let resolve_labels emit_buffer = 
    let tbl = Hashtbl.create 32 in
    let aux acc = function
        | Emitter.LabelDef label ->
            Hashtbl.add tbl label acc;
            acc
        | Emitter.RawValS _ -> acc+2
        | _ -> acc+1
    in
    let _ = List.fold_left aux 0 emit_buffer in tbl

let rec emit_binary labels acc pos = function
    | [] -> List.rev acc 
    | Emitter.RawOp op :: rest -> emit_binary labels (Opcodes.int_of_exbvm_op op :: acc) (pos+1) rest
    | Emitter.RawValB x :: rest -> emit_binary labels (x land 0xff :: acc) (pos+1) rest
    | Emitter.RawValS x :: rest -> 
        let h = (x lsr 8) land 0xff in 
        let l = x land 0xff in
        emit_binary labels (h :: l :: acc) (pos+2) rest 
    | Emitter.LabelRef name :: rest ->
        let offset = pos-(Hashtbl.find labels name) in
        let h = (offset lsr 8) land 0xff in 
        let l = offset land 0xff in
        emit_binary labels (h :: l :: acc) (pos+2) rest
    | Emitter.LabelDef _ :: rest | Emitter.NoEmit :: rest -> emit_binary labels acc pos rest

