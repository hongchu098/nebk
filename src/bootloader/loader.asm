org 0x0

[BITS 16]
LOADER_START:
    mov ax, cs
    mov ds, ax
    mov si, LOADER_HELLO_TEXT
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

LOADER_HELLO_TEXT db 'Here is loader!', 0