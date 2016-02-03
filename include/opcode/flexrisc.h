/* 
   Copyright (C) 1989, Free Software Foundation, Inc.

This file is part of GDB and GAS.

GDB and GAS are free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GDB and GAS are distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GDB or GAS; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  

Copyright (c) 2016, The Linux Foundation. All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License version 2 and
only version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

*/


/* Describes the format of a flexrisc machine instruction */

#ifndef FLEXRISC_H
#define FLEXRISC_H

#define FLEXRISC_MAX_OPCODEN     8
#define FLEXRISC_MAX_OPERAND    16
#define FLEXRISC_MAX_OPANDTYPE   9

/*       mnemonic  numBytes  opcVal  opcMask  numOps  type1    type2    type3  type4  src1   src2   src3   src4 */

struct flexrisc_opcode
{
  /* The opcode name.  */
  char			name[8];
  
  int           numBytes;           /* bytes for this instruction */
  unsigned char opcVal;             /* actual opcode bits in first byte */
  unsigned char opcMask;            /* where the bits of inherent operand are */
  int           numOps;             /* number of operands */
  char          type1[8];           /* type of operand 1 */
  char          type2[8];           /* type of operand 2 */
  char          type3[8];           /* type of operand 3 */
  char          type4[8];           /* type of operand 4 */
  char          src1[8];            /* source of operand 1 */
  char          src2[8];            /* source of operand 2 */
  char          src3[8];            /* source of operand 3 */
  char          src4[8];            /* source of operand 4 */
};

/* The table itself is sorted by major opcode number, and is otherwise
   in the order in which the disassembler should consider
   instructions.  */
extern void flexrisc_init_opcodes(void);
extern struct flexrisc_opcode flexrisc_opcodes[256];
extern int flexrisc_num_opcodes;

/* A macro to extract the major opcode from an instruction.  */
#define FLEXRISC_OP(i) ((i) & 0xF8)

#endif /* FLEXRISC_H */
