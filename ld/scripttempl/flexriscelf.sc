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

#   Linker Script for Zoran flexRISC
   
#   Brian Dodge

# The next line should be uncommented if it is desired to link
# without libstart.o and directly enter main.

# ENTRY=_main

test -z "$ENTRY" && ENTRY=_start
cat <<EOF

/* Example Linker Script for linking Zoran flexRISC elf32 files. */

/* The next line forces the entry point (${ENTRY} in this script)
   to be entered in the output file as an undefined symbol.
   It is needed in case the entry point is not called explicitly
   (which is the usual case) AND is in an archive.
*/

OUTPUT_FORMAT("${OUTPUT_FORMAT}")
OUTPUT_ARCH(${ARCH})
EXTERN(${ENTRY})
ENTRY(${ENTRY})

/* Define memory regions.
 *
 * the flexRISC has separate data and instructions memories,
 * both of which start at 0!!
 */
MEMORY
{
        imem        : ORIGIN = 0,           LENGTH = 0x3FFF
        dmem        : ORIGIN = 0,           LENGTH = 0x3FFF
}


SECTIONS
{
		.text : 
		{
		__TEXT_START = .;
		*(.text) *(.text.*) *(.code)
		__TEXT_END = .;
		} > imem
		
		.data :
		{
		__DATA_START = .;
		*(.data) *(.data.*)
		__DATA_END = .;
		} > dmem
}

EOF

