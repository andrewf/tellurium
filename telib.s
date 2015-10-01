; x86 stdlib for tellurium
section .text
    global syscall_exit

syscall_exit:
    mov eax, 1
    mov ebx, 23
    int 0x80

syscall_exit_status:
    ; arg is in eax
    mov ebx, eax   ; move to ebx
    mov eax, 1
    int 0x80
    
