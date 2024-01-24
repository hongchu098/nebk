SECTION .data
    LOADER_PE_HELLO_TEXT db 'Here is loader win32 PE-COFF version!', 0

SECTION .text
global _start
[BITS 16]
_start:
    mov ax, cs
    mov ds, ax
    mov eax, LOADER_PE_HELLO_TEXT
    mov si, ax
    call Println
    jmp $

Println:
    push di
    mov ah, 0x07
    .loop:
        mov al, [si]
        inc si
        mov [gs:di], ax
        add di, 2
        test al, 0xff
        jnz .loop
    pop di
    add di, 0xa0
    ret