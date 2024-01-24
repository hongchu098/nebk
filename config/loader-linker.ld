OUTPUT_FORMAT("pei-x86-64")
ENTRY(_start)

SECTIONS
{
    . = SIZEOF_HEADERS;
    . = ALIGN(__section_alignment__);
    .text  __image_base__ + ( __section_alignment__ < 0x1000 ? . : __section_alignment__ ) : ALIGN(4K)
    {
        *(.text)
    }
    .data BLOCK(4K) : ALIGN(4K)
    {
        *(.data)
    }
    .reloc BLOCK(__section_alignment__) :
    {
        *(.reloc)
    }
}