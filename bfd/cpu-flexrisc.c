/* BFD support for the Axis CRIS architecture.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Axis Communications AB.
   Written by Hans-Peter Nilsson.

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

const bfd_arch_info_type
bfd_flexrisc_arch[] =
{
{
  16,   						/* There's 16 bits_per_word.  */
  16,			            /* There's 16 bits_per_address.  */
  8,							/* There's 8 bits_per_byte.  */
  bfd_arch_flexrisc,		/* One of enum bfd_architecture, defined
								   in archures.c and provided in
								   generated header files.  */
  bfd_mach_flexrisc,			
  "flexrisc",				/* The arch_name.  */
  "flexrisc",				/* The printable name is the same.  */
  1,							/* Section alignment power; each section
								   is aligned to (only) 2^1 bytes.  */
  1,							/* This is the default "machine", since
								   there's only one.  */
  bfd_default_compatible,	/* A default function for testing
								   "machine" compatibility of two
								   bfd_arch_info_type.  */
  bfd_default_scan,		/* Check if an bfd_arch_info_type is a
								   match.  */
  NULL  						/* Pointer to next bfd_arch_info_type in
								   the same family.  */
},
};

