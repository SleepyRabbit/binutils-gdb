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

#define L_LNNO_SIZE 2
#include "coff/external.h"

#define	QUATROMAGIC	0xbeaf  

#define QUATROBADMAG(x) (((x).f_magic != QUATROMAGIC))

#define F_QUATRO_FIRE    0x1000
#define F_QUATRO_AMBER    0x2000
#define F_QUATRO_MACHMASK 0xf000

#define OMAGIC          0404    /* object files, eg as output */
#define ZMAGIC          0413    /* demand load format, eg normal ld output */


/********************** RELOCATION DIRECTIVES **********************/

struct external_reloc
{
  char r_vaddr[4];
  char r_symndx[4];
  char r_type[2];
};

#define RELOC struct external_reloc
#define RELSZ 10
