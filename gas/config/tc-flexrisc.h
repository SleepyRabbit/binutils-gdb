/* This file is tc-arm.h
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rwe@pegasus.esprit.ec.org)
	Modified by David Taylor (dtaylor@armltd.co.uk)

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  

   Copyright (c) 2016, The Linux Foundation. All rights reserved.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2 and
   only version 2 as published by the Free Software Foundation.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

*/

/* 
  This file created by Brian Dodge, Zoran Inc
*/
#define TC_QUATRO 1

#ifndef TARGET_BYTES_BIG_ENDIAN
#define TARGET_BYTES_BIG_ENDIAN 0
#endif

#define TARGET_FORMAT flexrisc_target_format ()
extern const char *flexrisc_target_format (void);

#define WORKING_DOT_WORD

#define COFF_MAGIC 	FLEXRISCMAGIC
#define TARGET_ARCH 	bfd_arch_flexrisc

/*
#define AOUT_MACHTYPE 	0

#define DIFF_EXPR_OK


#ifdef  LITTLE_ENDIAN
#undef  LITTLE_ENDIAN
#endif

#ifdef  BIG_ENDIAN
#undef  BIG_ENDIAN
#endif


#define LITTLE_ENDIAN 	1234
#define BIG_ENDIAN 	4321
*/

/* Call md_pcrel_from_section(), not md_pcrel_from().  */
extern long md_pcrel_from_section PARAMS ((struct fix *, segT));
#define MD_PCREL_FROM_SECTION(FIXP, SEC) md_pcrel_from_section (FIXP, SEC)

#define md_number_to_chars number_to_chars_littleendian

#define md_relax_frag(segment, fragP, stretch) \
flexrisc_relax_frag (segment, fragP, stretch)
extern int flexrisc_relax_frag (segT    segment, fragS * fragP, long    stretch);

extern int tc_equal_in_insn(char c, char *line);

#define TC_EQUAL_IN_INSN(C, PTR)  tc_equal_in_insn(C, PTR)

#undef IGNORE_OPCODE_CASE
#define TC_CASE_SENSITIVE 1


#define LISTING_HEADER "Quatro flexRISC GAS"

#define md_operand(x)

extern const struct relax_type md_relax_table[];
#define TC_GENERIC_RELAX_TABLE md_relax_table

extern const char *md_shortopts;




