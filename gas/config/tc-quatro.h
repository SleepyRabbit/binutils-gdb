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

   Copyright (c) 2015 Linux Foundation. All rights reserved.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2 and
   only version 2 as published by the Free Software Foundation.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

*/

#define TC_QUATRO 1

// the QUATRO DSP itself is big endian, but the ARM is little endian
//

#ifndef TARGET_BYTES_BIG_ENDIAN
	#if defined OBJ_ELF
		#define TARGET_BYTES_BIG_ENDIAN 0
		/* bdd - the target is "really" a big-endian device, but the 
		 * bus is swapped, so we need to store the file little-endian
		 * I handle this by swapping back to big ending if treated as
		 * a number and back to little endian if treated as storage
		 * but I only do that inside tc-quatro, so the regular
		 * assembler just uses the regular ones
		 */
		#ifdef TCQUATRO_INTERNAL
		#define md_number_to_chars number_to_chars_swapendian
		#define md_chars_to_number chars_to_number_swapendian
		#else
		#define md_number_to_chars number_to_chars_littleendian
		#endif
	#else
		#define TARGET_BYTES_BIG_ENDIAN 1
		#define md_number_to_chars number_to_chars_bigendian
		#define md_chars_to_number chars_to_number_bigendian
	#endif
#endif

#define TARGET_FORMAT quatro_target_format ()
extern const char *quatro_target_format (void);

#define DOUBLESLASH_LINE_COMMENTS	/* allow //-style comments */
#define WORKING_DOT_WORD

#define COFF_MAGIC 	QUATROMAGIC
#define TARGET_ARCH 	bfd_arch_quatro


/* Call md_pcrel_from_section(), not md_pcrel_from().  */
extern long md_pcrel_from_section PARAMS ((struct fix *, segT));
#define MD_PCREL_FROM_SECTION(FIXP, SEC) md_pcrel_from_section (FIXP, SEC)

#define md_relax_frag(segment, fragP, stretch) \
quatro_relax_frag (segment, fragP, stretch)
extern int quatro_relax_frag (segT    segment, fragS * fragP, long    stretch);

extern int tc_equal_in_insn(char c, char *line);

#define TC_EQUAL_IN_INSN(C, PTR)  tc_equal_in_insn(C, PTR)

#undef IGNORE_OPCODE_CASE
#define TC_CASE_SENSITIVE 1


#define LISTING_HEADER "Quatro GAS"

#define md_operand(x)

extern const struct relax_type md_relax_table[];
#define TC_GENERIC_RELAX_TABLE md_relax_table

extern const char *md_shortopts;




