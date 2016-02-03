/* BFD back-end for ARM COFF files.
   Copyright 1990, 91, 92, 93, 94, 95, 96, 97, 98, 99, 2000
   Free Software Foundation, Inc.
   Written by Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

Copyright (c) 2016, The Linux Foundation. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2 and
only version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

*/

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "bfdlink.h"
#include "coff/quatro.h"
#include "coff/internal.h"
#include "libcoff.h"


#define BADMAG(x) QUATROBADMAG(x)
#define COFF_DEFAULT_SECTION_ALIGNMENT_POWER (4)
#define COFF_LONG_FILENAMES
#define COFF_LONG_SECTION_NAMES

#ifndef false
#define false 0
#define true  1
#endif

static bfd_reloc_status_type coff_quatro_reloc
PARAMS ((bfd *, arelent *, asymbol *, PTR, asection *, bfd *, char **));


static   reloc_howto_type reloc_memlit =
    HOWTO (BFD_RELOC_QUATRO_MEMLIT, /* type */
           2, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           16, /* bitsize */
           false, /* pc_relative */
           0, /* bitpos */
           0,  /* dont complain_on_overflow */
           NULL, /* special_function */
           "quatro_reloc_memlit", /* name */
           false, /* partial_inplace */
           0x0000FFFF, /* src_mask */
           0x00383ff7, /* dst_mask */
           false); /* pcrel_offset */

static   reloc_howto_type reloc_16 =
    HOWTO (BFD_RELOC_16, /* type */
           0, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           16, /* bitsize */
           false, /* pc_relative */
           0, /* bitpos */
           0,  /* dont complain_on_overflow */
           NULL, /* special_function */
           "quatro_reloc_16", /* name */
           false, /* partial_inplace */
           0x0000FFFF, /* src_mask */
           0x0000FFFF, /* dst_mask */
           false); /* pcrel_offset */

static   reloc_howto_type reloc_24 =
    HOWTO (BFD_RELOC_24, /* type */
           2, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           24, /* bitsize */
           false, /* pc_relative */
           0, /* bitpos */
           0,  /* dont complain_on_overflow */
           NULL, /* special_function */
           "quatro_reloc_24", /* name */
           false, /* partial_inplace */
           0x00FFFFFF, /* src_mask */
           0x00FFFFFF, /* dst_mask */
           false); /* pcrel_offset */

static   reloc_howto_type reloc_24_pcrel =
    HOWTO (BFD_RELOC_24_PCREL, /* type */
           2, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           24, /* bitsize */
           true, /* pc_relative */
           0, /* bitpos */
           complain_overflow_bitfield,  /* dont complain_on_overflow */
           coff_quatro_reloc, /* special_function */
           "quatro_reloc_24_pcrel", /* name */
           false, /* partial_inplace */
           0x00, /* src_mask */
           0x00FFFFFF, /* dst_mask */
           true); /* pcrel_offset */


static   reloc_howto_type reloc_quatro_11_pcrel =
    HOWTO (BFD_RELOC_QUATRO_11_PCREL, /* type */
           2, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           11, /* bitsize */
           true, /* pc_relative */
           0, /* bitpos */
           complain_overflow_bitfield,  /* dont complain_on_overflow */
           coff_quatro_reloc, /* special_function */
           "quatro_reloc_11_pcrel", /* name */
           false, /* partial_inplace */
           0x000007FF, /* src_mask */
           0x000007FF, /* dst_mask */
           true); /* pcrel_offset */

static   reloc_howto_type reloc_quatro_9_pcrel =
    HOWTO (BFD_RELOC_QUATRO_9_PCREL, /* type */
           2, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           9, /* bitsize */
           true, /* pc_relative */
           0, /* bitpos */
           complain_overflow_bitfield,  /* dont complain_on_overflow */
           coff_quatro_reloc, /* special_function */
           "quatro_reloc_9_pcrel", /* name */
           false, /* partial_inplace */
           0x000001FF, /* src_mask */
           0x000001FF, /* dst_mask */
           true); /* pcrel_offset */


static   reloc_howto_type reloc_32 =
    HOWTO (BFD_RELOC_32, /* type */
           0, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           32, /* bitsize */
           false,  /* pc_relative */
           0, /* bitpos */
           0, /* dont complain_on_overflow */
           NULL,/* special_function */
           "quatro_reloc_32", /* name */
           false, /* partial_inplace */
           0xFFFFFFFF,/* src_mask */ 
           0xFFFFFFFF,  /* dst_mask */
           false); /* pcrel_offset */

static   reloc_howto_type reloc_16_pcrel = 
    HOWTO (BFD_RELOC_16_PCREL, /* type */
           0, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           16, /* bitsize */
           true, /* pc_relative */
           0, /* bitpos */
           0,  /* dont complain_on_overflow */
           NULL, /* special_function */
           "quatro_reloc_16_pcrel", /* name */
           false, /* partial_inplace */
           0x0000FFFF, /* src_mask */
           0x0000FFFF, /* dst_mask */
           true); /* pcrel_offset */


static   reloc_howto_type reloc_8 = 
HOWTO (BFD_RELOC_8,  /* type */
       0, /* rightshift */
           2, /* size (0 = byte, 1 = short, 2 = long) */
           8, /* bitsize */
           false, /* pc_relative */
           0, /* bitpos */
           0,  /* dont complain_on_overflow */
           NULL, /* special_function */
           "quatro_reloc_8", /* name */
           false, /* partial_inplace */
           0x000000FF, /* src_mask */
           0x000000FF, /* dst_mask */
           false); /* pcrel_offset */


static void
rtype2howto (
     arelent *internal,
     struct internal_reloc *dst
)
{
  switch (dst->r_type)
    {
    case BFD_RELOC_8:
      internal->howto = &reloc_8;
      break;
    case BFD_RELOC_16:
      internal->howto = &reloc_16;
      break;
    case BFD_RELOC_QUATRO_MEMLIT:
      internal->howto = &reloc_memlit;
      break;
    case BFD_RELOC_16_PCREL:
      internal->howto = &reloc_16_pcrel;
      break;
    case BFD_RELOC_32:
      internal->howto = &reloc_32;
      break;
    case BFD_RELOC_24:
      internal->howto = &reloc_24;
      break;
    case BFD_RELOC_24_PCREL:
      internal->howto = &reloc_24_pcrel;
      break;
    case BFD_RELOC_QUATRO_9_PCREL:
      internal->howto = &reloc_quatro_9_pcrel;
      break;
    case BFD_RELOC_QUATRO_11_PCREL:
      internal->howto = &reloc_quatro_11_pcrel;
      break;


default:
      abort ();
      break;
    }
}

#define RTYPE2HOWTO(internal, relocentry) rtype2howto(internal,relocentry)

#define coff_bfd_reloc_type_lookup quatro_coff_reloc_type_lookup

reloc_howto_type* quatro_coff_reloc_type_lookup (bfd *abfd, bfd_reloc_code_real_type code);

static volatile void* wasteuse(volatile void* crap)
{
	return crap;
}

/* For the case statement use the code values used in tc_gen_reloc to
   map to the howto table entries that match those in both the aout
   and coff implementations.  */
reloc_howto_type *
quatro_coff_reloc_type_lookup (bfd *abfd, bfd_reloc_code_real_type code)
{
	wasteuse(abfd);
	
  /* _bfd_error_handler("quatro_coff_reloc_type_lookup 0x%X 0x%X\n", abfd, code);*/

  switch (code)
    {
    case BFD_RELOC_8:
      return &reloc_8;
      break;
    case BFD_RELOC_QUATRO_MEMLIT:
      return &reloc_memlit;
      break;
    case BFD_RELOC_16:
      return &reloc_16;
      break;
    case BFD_RELOC_16_PCREL:
      return &reloc_16_pcrel;
      break;
    case BFD_RELOC_32:
      return &reloc_32;
      break;
    case BFD_RELOC_24:
      return &reloc_24;
      break;
    case BFD_RELOC_24_PCREL:
      return &reloc_24_pcrel;
      break;
    case BFD_RELOC_QUATRO_9_PCREL:
      return &reloc_quatro_9_pcrel;
      break;
    case BFD_RELOC_QUATRO_11_PCREL:
      return &reloc_quatro_11_pcrel;
      break;
     default:
      abort ();
      return (reloc_howto_type *) NULL;
      break;
    }
}


#include "coffcode.h"


/* Target vectors.  */
CREATE_BIG_COFF_TARGET_VEC (quatro_coff_vec, 
                            "coff-quatro", 
                            0, 
                            0, 
                            1, 
							NULL,
                            COFF_SWAP_TABLE
							);



static bfd_reloc_status_type
coff_quatro_reloc (
     bfd *abfd,
     arelent *reloc_entry,
     asymbol *symbol,
     PTR data,
     asection *input_section,
     bfd *output_bfd,
     char **error_message
)
{
  bfd_vma relocation, where_we_are, where_we_want_to_go;
  reloc_howto_type *howto = reloc_entry->howto;
//  asection *reloc_target_output_section;
//  bfd_vma output_base = 0;
  unsigned long x;
  int branch_delay;

  unsigned long mach = bfd_get_mach(abfd);

  *error_message = "";
  relocation = 0;
  
  if(mach==bfd_mach_quatro_amber)
    branch_delay = 2*4;
  else
    branch_delay = 3*4;

    
  /* If this is an undefined symbol, return error.  */
  if (symbol->section == &bfd_und_section
      && (symbol->flags & BSF_WEAK) == 0)
    return output_bfd ? bfd_reloc_continue : bfd_reloc_undefined;

  if ((output_bfd != (bfd *) NULL) &&
      (howto->partial_inplace == false))
    {
      /* This is a partial relocation, and we want to apply the relocation
         to the reloc entry rather than the raw data. Modify the reloc
         inplace to reflect what we now know.  */
      reloc_entry->addend = relocation;
      reloc_entry->address += input_section->output_offset;
      return bfd_reloc_ok;
    }



    /*where we want to go equals the address of the symbol relative to
      the file plus the offset of the file relative to the image.*/

    where_we_want_to_go = symbol->section->output_offset + symbol->value;

    /*where we are equals the offset of this file plus the offset of were
     we are writing the reloc */
    where_we_are = input_section->output_offset + reloc_entry->address + branch_delay;

    relocation = where_we_want_to_go - where_we_are;
    
#if 0
        fprintf (stderr, "\n\n%s\t0x%X\t0x%X\t0x%X\n", 
             symbol->name, where_we_are,  where_we_want_to_go, relocation);
    
    /*    relocation = relocation - 12;*/ //for branch delay
#endif

    relocation >>= (bfd_vma) howto->rightshift;

    /* Shift everything up to where it's going to be used.  */
    relocation <<= (bfd_vma) howto->bitpos;


  x = bfd_get_32 (abfd, (bfd_byte *) data + reloc_entry->address);
  /*  fprintf (stderr, "\t<=0x%8X\n", x );*/
  x = ( (x & ~howto->dst_mask) | (((x & howto->src_mask) +  relocation) & howto->dst_mask));
  /*  fprintf (stderr, "\t=>0x%8X\n\n\n", x );*/
  bfd_put_32 (abfd, (bfd_vma) x, (bfd_byte *) data + reloc_entry->address);

  return bfd_reloc_ok;
}








