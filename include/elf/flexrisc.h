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

#ifndef FLEXRISC
# define FLEXRISC
#endif


#define	FLEXRISCMAGIC	0xbeaf  

#define FLEXRISCBADMAG(x) (((x).f_magic != FLEXRISCMAGIC))

#define F_FLEXRISC_FIRE    0x1000
#define F_FLEXRISC_MACHMASK 0xf000

#define OMAGIC          0404    /* object files, eg as output */
#define ZMAGIC          0413    /* demand load format, eg normal ld output */


/********************** RELOCATION DIRECTIVES **********************/
#include "elf/reloc-macros.h"

/* Relocations.  */
START_RELOC_NUMBERS (elf_FLEXRISC_reloc_type)
  RELOC_NUMBER (R_FLEXRISC_NONE,          0)
  RELOC_NUMBER (R_FLEXRISC_8,             1)
  RELOC_NUMBER (R_FLEXRISC_8_PCREL,       2)
  RELOC_NUMBER (R_FLEXRISC_16,            3)
  RELOC_NUMBER (R_FLEXRISC_16L,           4)
  RELOC_NUMBER (R_FLEXRISC_16H,           5)
  RELOC_NUMBER (R_FLEXRISC_3,             6)
END_RELOC_NUMBERS (R_FLEXRISC_MAX)


