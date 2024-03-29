# 
# Copyright (c) 2016, The Linux Foundation. All rights reserved.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 and
# only version 2 as published by the Free Software Foundation.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# The next line should be uncommented if it is desired to link
# without libstart.o and directly enter main.

# ENTRY=_main

test -z "$ENTRY" && ENTRY=_start
cat <<EOF

/* Example Linker Script for linking NS CR16 elf32 files. */

/* The next line forces the entry point (${ENTRY} in this script)
   to be entered in the output file as an undefined symbol.
   It is needed in case the entry point is not called explicitly
   (which is the usual case) AND is in an archive.  */

OUTPUT_FORMAT("${OUTPUT_FORMAT}")
OUTPUT_ARCH(${ARCH})
EXTERN(${ENTRY})
ENTRY(${ENTRY})

/* Define memory regions.  */
MEMORY
{
        rom         : ORIGIN = 0,           LENGTH = 3M
        ram         : ORIGIN = 4M,          LENGTH = 10M
}

/*  Many sections come in three flavours.  There is the 'real' section,
    like ".data".  Then there are the per-procedure or per-variable
    sections, generated by -ffunction-sections and -fdata-sections in GCC,
    and useful for --gc-sections, which for a variable "foo" might be
    ".data.foo".  Then there are the linkonce sections, for which the linker
    eliminates duplicates, which are named like ".gnu.linkonce.d.foo".
    The exact correspondences are:

    Section Linkonce section
    .text   .gnu.linkonce.t.foo
    .rdata  .gnu.linkonce.r.foo
    .data   .gnu.linkonce.d.foo
    .bss    .gnu.linkonce.b.foo
    .debug_info .gnu.linkonce.wi.foo  */

SECTIONS
{
  .init :
  { 
    __INIT_START = .; 
    KEEP (*(.init))
    __INIT_END = .; 
  } > rom

  .fini :
  { 
    __FINI_START = .; 
    KEEP (*(.fini))
    __FINI_END = .; 
  } > rom

  .jcr :
  { 
    KEEP (*(.jcr))
  } > rom

  .text : 
  {
    __TEXT_START = .;
    *(.text) *(.text.*) *(.gnu.linkonce.t.*)
    __TEXT_END = .;
  } > rom

  .rdata :
  {
    __RDATA_START = .;
    *(.rdata_4) *(.rdata_2) *(.rdata_1) *(.rdata.*) *(.gnu.linkonce.r.*) *(.rodata*)
    __RDATA_END = .;
  } > rom

  .ctor ALIGN(4) : 
  { 
    __CTOR_START = .; 
    /* The compiler uses crtbegin.o to find the start
       of the constructors, so we make sure it is
       first.  Because this is a wildcard, it
       doesn't matter if the user does not
       actually link against crtbegin.o; the
       linker won't look for a file to match a
       wildcard.  The wildcard also means that it
       doesn't matter which directory crtbegin.o
       is in.  */

    KEEP (*crtbegin*.o(.ctors))

    /* We don't want to include the .ctor section from
       the crtend.o file until after the sorted ctors.
       The .ctor section from the crtend file contains the
       end of ctors marker and it must be last */

    KEEP (*(EXCLUDE_FILE (*crtend*.o) .ctors))
    KEEP (*(SORT(.ctors.*)))
    KEEP (*(.ctors))
    __CTOR_END = .; 
  } > rom

  .dtor ALIGN(4) : 
  { 
    __DTOR_START = .; 
    KEEP (*crtbegin*.o(.dtors))
    KEEP (*(EXCLUDE_FILE (*crtend*.o) .dtors))
    KEEP (*(SORT(.dtors.*)))
    KEEP (*(.dtors))
    __DTOR_END = .; 
  } > rom

  .data :
  {
    __DATA_START = .;
    *(.data_4) *(.data_2) *(.data_1) *(.data) *(.data.*) *(.gnu.linkonce.d.*)
    __DATA_END = .;
  } > ram AT > rom

  .bss (NOLOAD) :
  {
    __BSS_START = .;
    *(.bss_4) *(.bss_2) *(.bss_1) *(.bss) *(COMMON) *(.bss.*) *(.gnu.linkonce.b.*)
    __BSS_END = .;
  } > ram

/* You may change the sizes of the following sections to fit the actual
   size your program requires.

   The heap and stack are aligned to the bus width, as a speed optimization
   for accessing data located there.  */

  .heap :
  {
    . = ALIGN(4);
    __HEAP_START = .;
    . += 0x2000; __HEAP_MAX = .;
  } > ram

  .stack :
  {
    . = ALIGN(4);
    . += 0x6000;
    __STACK_START = .;
  } > ram

  .istack :
  {
    . = ALIGN(4);
    . += 0x100;
    __ISTACK_START = .;
  } > ram

  .comment        0 : { *(.comment) }

  /* DWARF debug sections.
     Symbols in the DWARF debugging sections are relative to the beginning
     of the section so we begin them at 0.  */

  .debug_aranges  0 : { *(.debug_aranges) }
  .debug_pubnames 0 : { *(.debug_pubnames) }
  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
  .debug_abbrev   0 : { *(.debug_abbrev) }
  .debug_line     0 : { *(.debug_line) }
  .debug_frame    0 : { *(.debug_frame) }
  .debug_str      0 : { *(.debug_str) }
  .debug_loc      0 : { *(.debug_loc) }
  .debug_macinfo  0 : { *(.debug_macinfo) }
}

__DATA_IMAGE_START = LOADADDR(.data);
EOF

