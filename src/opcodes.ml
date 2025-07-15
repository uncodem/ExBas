type exbvm_opcode =
    | OP_const
    | OP_copy
    | OP_cast
    | OP_size
    | OP_startscope
    | OP_endscope
    | OP_defvar
    | OP_pushvar
    | OP_popvar
    | OP_swap
    | OP_dup
    | OP_drop
    | OP_createarray
    | OP_createarray_nd
    | OP_initarray
    | OP_cget
    | OP_rget
    | OP_cset
    | OP_rset
    | OP_rget_nd
    | OP_rset_nd
    | OP_add
    | OP_sub
    | OP_div
    | OP_mul
    | OP_mod
    | OP_neg
    | OP_and
    | OP_or
    | OP_not
    | OP_eql
    | OP_neql
    | OP_more
    | OP_less
    | OP_eqmore
    | OP_eqless
    | OP_jmp
    | OP_call
    | OP_tjmp
    | OP_tcall
    | OP_dump
    | OP_native
    | OP_ret
[@@deriving show]

let int_of_exbvm_op = function
    | OP_const -> 0
    | OP_copy -> 1
    | OP_cast -> 2
    | OP_size -> 3
    | OP_startscope -> 4
    | OP_endscope -> 5
    | OP_defvar -> 6
    | OP_pushvar -> 7
    | OP_popvar -> 8
    | OP_swap -> 9
    | OP_dup -> 10
    | OP_drop -> 11
    | OP_createarray -> 12
    | OP_createarray_nd -> 13
    | OP_initarray -> 14
    | OP_cget -> 15
    | OP_rget -> 16
    | OP_cset -> 17
    | OP_rset -> 18
    | OP_rget_nd -> 19
    | OP_rset_nd -> 20
    | OP_add -> 21
    | OP_sub -> 22
    | OP_div -> 23
    | OP_mul -> 24
    | OP_mod -> 25
    | OP_neg -> 26
    | OP_and -> 27
    | OP_or -> 28
    | OP_not -> 29
    | OP_eql -> 30
    | OP_neql -> 31
    | OP_more -> 32
    | OP_less -> 33
    | OP_eqmore -> 34
    | OP_eqless -> 35
    | OP_jmp -> 36
    | OP_call -> 37
    | OP_tjmp -> 38
    | OP_tcall -> 39
    | OP_dump -> 40
    | OP_native -> 41
    | OP_ret -> 42
