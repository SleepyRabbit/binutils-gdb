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

#ifndef QUATRO
# define QUATRO
#endif


#define	QUATROMAGIC	0xbeaf  

#define QUATROBADMAG(x) (((x).f_magic != QUATROMAGIC))

#define F_QUATRO_FIRE    0x1000
#define F_QUATRO_AMBER    0x2000
#define F_QUATRO_MACHMASK 0xf000

#define OMAGIC          0404    /* object files, eg as output */
#define ZMAGIC          0413    /* demand load format, eg normal ld output */


/********************** RELOCATION DIRECTIVES **********************/
#include "elf/reloc-macros.h"

/* Relocations.  */
START_RELOC_NUMBERS (elf_quatro_reloc_type)
  RELOC_NUMBER (R_QUATRO_NONE,          0)
  RELOC_NUMBER (R_QUATRO_8,             1)
  RELOC_NUMBER (R_QUATRO_16,            2)
  RELOC_NUMBER (R_QUATRO_16_PCREL,      3)
  RELOC_NUMBER (R_QUATRO_24,            4)
  RELOC_NUMBER (R_QUATRO_24_PCREL,      5)
  RELOC_NUMBER (R_QUATRO_32,            6)
  RELOC_NUMBER (R_QUATRO_9_PCREL,       7)
  RELOC_NUMBER (R_QUATRO_11_PCREL,      8)
//  RELOC_NUMBER (R_QUATRO_MV16,          9)
  RELOC_NUMBER (R_QUATRO_MEMLIT,        9)
END_RELOC_NUMBERS (R_QUATRO_MAX)
