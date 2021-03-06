; x86 stdlib for tellurium
section .data

    global exit_status
exit_status:
    dw 0

section .text
    global syscall_exit
    global syscall_exit_status
    global asm_add

syscall_exit:
    mov eax, 1
    mov ebx, [exit_status]
    int 0x80

syscall_exit_status:
    ; arg is in eax
    mov ebx, eax   ; move to ebx
    mov eax, 1
    int 0x80

asm_add:
   ; args in eax, ecx
   add eax, ecx
   ret
    
