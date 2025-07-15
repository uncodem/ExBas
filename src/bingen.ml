
let calc_labels emit_buffer = 
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
        let offset = (Hashtbl.find labels name)-pos in
        let h = (offset lsr 8) land 0xff in 
        let l = offset land 0xff in
        emit_binary labels (h :: l :: acc) (pos+2) rest
    | Emitter.LabelDef _ :: rest | Emitter.NoEmit :: rest -> emit_binary labels acc pos rest


let int_to_le_bytes n byte_count =
    let rec aux acc i =
        if i = 0 then List.rev acc
        else aux ((n lsr ((i - 1) * 8)) land 0xFF :: acc) (i - 1)
    in
    List.rev (aux [] byte_count)

let le_bytes_of_const const = 
    match const with
    | Emitter.IntConst x -> 0 :: int_to_le_bytes x 4
    | Emitter.StrConst s -> 
        let chars = List.of_seq (String.to_seq s) in
        1 :: List.map Char.code chars @ [0]
    | Emitter.BoolConst b -> 2 :: [if b then 1 else 0]
    | Emitter.FloatConst f ->
        3 :: int_to_le_bytes (Int32.bits_of_float f |> Int32.to_int) 4

let emit_consts const_pool = 
     let raw_consts = List.map le_bytes_of_const const_pool in
     List.flatten raw_consts

let emit_final emitted const_pool = 
    let labels = calc_labels emitted in
    let binary = emit_binary labels [] 0 emitted in
    let header = [0xef; 0xbe; 0xad; 0xde] @ int_to_le_bytes (List.length binary) 4 in
    header @ binary @ emit_consts const_pool

