org 0x7c00

[BITS 16]

    jmp short BOOT_START
    nop

    BS_OEMNAME db 'ABCDEFGH'
    BPB_BytesPerSec dw 512
    BPB_SecPerClus db 1
    BPB_RsvdSecCnt dw 1
    BPB_NumFATs db 2
    BPB_RootEntCnt dw 0x00E0
    BPB_TotSec16 dw 0x0B40
    BPB_Media db 0xF0
    BPB_FATSz16 dw 9
    BPB_SecPerTrk dw 18
    BPB_NumHeads dw 2
    BPB_HiddSec dd 0
    BPB_TotSec32 dd 0x0B40
    BS_DrvNum db 0
    BS_Reserved1 db 0
    BS_BootSig db 0x29
    BS_VolID dd 0
    BS_VOlLab db 'NEBKFLO0.01'
    BS_FileSysType db 'FAT12   '

BOOT_START:
    mov ax, cs
    mov ds, ax
    mov ss, ax
    mov sp, 0x7c00
    mov bp, sp
    mov ax, 0xb800
    mov gs, ax
    mov ax, 0x1000
    mov es, ax

    dec sp
    mov [bp-1], dl ; BIOS use dl to store BIOS device ID
    mov ax, 0x0600
    mov bh, 0x07
    xor cx, cx
    mov dx, 0x1950
    int 0x10

FindLoader:
    xor edi, edi
    mov si, BOOTING_TEXT
    call Println
    xor ax, ax
    mov dl, [bp-1]
    int 0x13 ; reset floppy system
    test ah, 0xff
    jnz ErrorAndStop
FindLoaderEntry:
    ; read root directory region into memory, staring at CHS(0,1,2)
    mov ax, 0x020e ; al = number of sectors to read (BPB_RootEntCnt*32/512 = 0xe)
    mov cx, 0x0002 ; ch = cylinder number, cl = sector number (since 1)
    mov dh, 0x01   ; dh = head number
    mov dl, [bp-1] ; dl = drive number
    mov bx, 0x2600 ; [es:bx] is data buffer
    int 13h        ; READ SECTOR(S) INTO MEMORY
    test ah, 0xff
    jnz ErrorAndStop
    push di
    cld
    mov di, 0x2600
    mov cx, [BPB_RootEntCnt]
    .loop_entry:
        mov si, LOADER_FILENAME
        test byte [es:di], 0xff
        jz short .not_found
        mov dx, 11
        .loop_strcmp:
            cmpsb
            jnz short .next_entry
            dec dx
            jnz short .loop_strcmp
            jmp short ReadLoaderData
        .next_entry:
            and di, 0xffe0
            add di, byte 0x20
        loop .loop_entry
    .not_found:
        pop di
        jmp ErrorAndStop
Println:
    push di
    mov ah, 0x07
    .loop:
        mov al, [si]
        inc si
        mov [gs:di], ax
        add di, byte 2
        test al, 0xff
        jnz .loop
    pop di
    add di, 0xa0
    ret
ErrorAndStop:
    mov si, ERROR_TEXT
    call Println
    jmp $
ReadLoaderData:
    and di, 0xffe0
    mov dx, es:[di + 0x1A]   ; Start of file in clusters 
    ;mov eax, es:[di + 0x1C]  ; File size in bytes
    pop di
    ;push eax
    push dx
        ; read FAT1 into memory, staring at CHS(0,0,2)
    mov ax, 0x0209 ; al = number of sectors to read (BPB_FATSz16 = 0x9)
    mov cx, 0x0002 ; ch = cylinder number, cl = sector number (since 1)
    xor dx, dx     ; dh = head number
    mov dl, [bp-1] ; dl = drive number
    mov bx, 0x0200 ; [es:bx] is data buffer
    int 13h        ; READ SECTOR(S) INTO MEMORY
    test ah, 0xff
    jnz ErrorAndStop
    
    mov ax, 0x2000 ; data buffer of loader.bin
    mov es, ax
    mov ax, 0x1020
    mov fs, ax
    xor bx, bx
    pop dx
    .loop_cluster:
        StartSectorOfDataRegion equ 33
        mov ax, dx
        add ax, StartSectorOfDataRegion-2 ; data region start at cluster #2
        call ReadSectorFromLBA
        add bx, 0x0200
        .next_cluster:
            mov ax, dx
            shr ax, 1
            add ax, dx  ; ax = dx + (dx >> 2)  i.e.  ax = floor(3/2*dx)
            mov si, ax
            mov ax, fs:[si]
            test dx, 0x0001
            jnz short .odd_cluster
            and ax, 0x0fff
            jmp short .if_final
            .odd_cluster:
                shr ax, byte 4
        .if_final:
            mov dx, ax
            cmp dx, 0xff8
            jc .loop_cluster

ParsePE:
    mov ax, 0x2000                  ; data buffer of loader.exe
    mov es, ax                  
    mov ax, 0x1000                  ; image base of loader.exe
    mov fs, ax
    push di
    PEHeaderOffset equ 0x3c
    mov edi, [es:PEHeaderOffset]
    mov ax, [es:di + 4+16]          ; SizeOfOptionalHeader
    add ax, di
    add ax, byte 4+20
    push ax
    mov si, TEXT_SECTION_NAME
    call near FindSection           
    mov esi, [es:di + 12]           ; VirtualAddress
    mov ecx, [es:di + 16]           ; SizeOfRawData
    mov edi, [es:di + 20]           ; PointerToRawData
    push si
    call near Memcpy_ESDI_to_FSSI
    mov ax, [bp-5]
    mov si, DATA_SECTION_NAME
    call near FindSection
    mov esi, [es:di + 12]           ; VirtualAddress
    mov ecx, [es:di + 16]           ; SizeOfRawData
    mov edi, [es:di + 20]           ; PointerToRawData
    call near Memcpy_ESDI_to_FSSI
    
    mov di, [bp-3]
    push fs
    add sp, 8
    jmp far word [bp-9]

FindSection:
    ; [es:ax] is section table
    ; [ds:si] is section name (8 bytes)
    ; ret: [es:di] is section item
    mov [.si], si
    .loop:
        mov di, ax
        mov cx, 8
        repz cmpsb
        jz short .ret
        add ax, byte 40
        mov si, [.si]
        jmp short .loop
        .si dw 0
    .ret:
        mov di, ax
    ret

Memcpy_ESDI_to_FSSI:
    ; [es:di] and [fs:si] above
    ; cx is length
    .loop:
        mov al, [es:di]
        mov [fs:si], al
        inc di
        inc si
        loop .loop
    ret

ReadSectorFromLBA:
    ; ax = LBA of sector to read
    ; [es:bx] is data buffer
    push dx
    push cx
    mov cl, 18
    div cl          ; divide ax, al := Q, ah := R
    xor dx, dx
    shr al, 1
    rcl dh, 1       ; dh = head number
    mov dl, [bp-1]  ; dl = drive number
    mov ch, al      ; ch = cylinder number
    mov cl, ah      
    inc cl          ; cl = sector number (since 1)
    mov ax, 0x0201  ; al = number of sectors to read (only one)
    int 13h
    test ah, 0xff
    jnz ErrorAndStop
    pop cx
    pop dx
    ret


    BOOTING_TEXT db 'Booting...', 0
    ERROR_TEXT db 'ERROR!', 0
    LOADER_FILENAME db 'LOADER  EXE'
    TEXT_SECTION_NAME db '.text', 0, 0, 0
    DATA_SECTION_NAME db '.data', 0, 0, 0

    times 510-($-$$) db 0
    dw 0xAA55