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
    | OP_initarray
    | OP_cget
    | OP_rget
    | OP_cset
    | OP_rset
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
    | OP_initarray -> 13
    | OP_cget -> 14
    | OP_rget -> 15
    | OP_cset -> 16
    | OP_rset -> 17
    | OP_add -> 18
    | OP_sub -> 19
    | OP_div -> 20
    | OP_mul -> 21
    | OP_mod -> 22
    | OP_neg -> 23
    | OP_and -> 24
    | OP_or -> 25
    | OP_not -> 26
    | OP_eql -> 27
    | OP_neql -> 28
    | OP_more -> 29
    | OP_less -> 30
    | OP_eqmore -> 31
    | OP_eqless -> 32
    | OP_jmp -> 33
    | OP_call -> 34
    | OP_tjmp -> 35
    | OP_tcall -> 36
    | OP_dump -> 37
    | OP_native -> 38
    | OP_ret -> 39
