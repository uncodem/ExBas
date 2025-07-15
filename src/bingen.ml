
type bingen_state = {
    buffer: int list;
    emit_buffer: Emitter.emit_me list;
    
    labels: (string, int) Hashtbl.t;
}
 
let bingen_init buff = {
    buffer = [];
    emit_buffer = buff;
    labels = Hashtbl.create 32
}

let resolve_labels state = 
    let aux acc = function
        | Emitter.LabelDef label ->
            Hashtbl.add state.labels label acc;
            acc
        | _ -> acc+1
    in
    let _ = List.fold_left aux 0 state.emit_buffer in ()

