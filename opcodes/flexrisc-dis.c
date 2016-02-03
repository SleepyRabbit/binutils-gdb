/* Instruction printing code for the ARM
   Copyright (C) 1994, 95, 96, 97, 98, 99, 2000 Free Software Foundation, Inc. 
   Contributed by Richard Earnshaw (rwe@pegasus.esprit.ec.org)
   Modification by James G. Smith (jsmith@cygnus.co.uk)

This file is part of libopcodes. 

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version. 

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details. 

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

/* This flexrisc version was originally written be Brian Dodge
*/
#include "sysdep.h"
#include "dis-asm.h"
#include "coff/internal.h"
#include "libcoff.h"
#include "opintl.h"
#include <stdlib.h>
#include <string.h>
#include "opcode/flexrisc.h"

static int branch_delay = 0;

static void flexrisc_print_operand(
			bfd_vma pc,
			char* type,
			unsigned char val, unsigned char val1,
			struct disassemble_info* info
		)
{
	/* possible types of operand
	 *     val3         3bit value (0-7)
	 *     val8         8bit value (0-255)
	 *     val8imm      8bit immediate value (0-255) leaded by '#'
	 *     val16imm     16bit immediate value (0-65535) leaded by '#' (but only 1 byte is used)
	 *     reg8         8bit register name (a1, a2, b1, b2, c1, c2, d1, d3)
	 *     reg16        16bit register name (a, b, c, d)
	 *     reg16id      16bit register used indirect (a, b, c, d in parantheses)
	 *     bit<text>    constant text followed by '.' and a 3bit value (0-7) 
	 *     absAddr      16bit address
	 *     relAddr      16bit address
	 *     @<text>      constant text
	 */
	if(type[0] == '@')
	{
		(*info->fprintf_func)(info->stream, "%s", type+1);
	}
	else if(type[0] == 'b' && type[1] == 'i' && type[2] == 't')
	{
		(*info->fprintf_func)(info->stream, "%s.%d", type+3, val);
	}
	else if(! strcmp(type, "val3"))
	{
		(*info->fprintf_func)(info->stream, "0x%-4x", val);
	}
	else if(! strcmp(type, "val8"))
	{
		(*info->fprintf_func)(info->stream, "0x%-4x", val);
	}
	else if(! strcmp(type, "val8imm") || ! strcmp(type, "val16imm"))
	{
		(*info->fprintf_func)(info->stream, "#0x%-4x", val);
	}
	else if(! strcmp(type, "reg8"))
	{
		(*info->fprintf_func)(info->stream, "%c%d", 'a' + val / 2, 1 + (val & 1));
	}
	else if(! strcmp(type, "reg16"))
	{
		(*info->fprintf_func)(info->stream, "%c", 'a' + val / 2);
	}
	else if(! strcmp(type, "reg16id"))
	{
		(*info->fprintf_func)(info->stream, "(%c)", 'a' + val / 2);
	}
	else if(! strcmp(type, "absAddr"))
	{
		// note the byte swap!
		unsigned short uav = (unsigned short)val + 256 * (unsigned short)val1;
		
		(*info->fprintf_func)(info->stream, "0x%-4x\t; ", uav);
        info->print_address_func(uav, info);
	}
	else if(! strcmp(type, "relAddr"))
	{
		int rv = (int)(signed char)val;
		
		(*info->fprintf_func)(info->stream,
						"0x%-4x\t; pc%c0x%-4x = ",
						rv,
						(rv > 0) ? '+':'-',
						(rv > 0) ? rv : -rv);
        info->print_address_func(pc + rv + 2, info);
	}
	else
	{
		(*info->fprintf_func)(info->stream, "???");
	}
}


/*
static int extract_uint(unsigned long insn, char *s, char **ptr);
static int extract_int(unsigned long insn, char *s, char **ptr);
static long SIGN_EXTEND(unsigned long Val, unsigned int Width);
static long EXT_BIT_FIELD(long Val, long StartBit, long EndBit);
*/
int print_insn_flexrisc ( bfd_vma pc, struct disassemble_info * info)
{ 
  int status = 0;
  bfd_byte opcode, operands[4];
  int opdex;
  struct flexrisc_opcode* pcode;

  branch_delay = 0;
  
  if(flexrisc_num_opcodes <= 0)
  {
 	flexrisc_init_opcodes();
  } 

  status = info->read_memory_func (pc, &opcode, 1, info);

  if (status != 0)
  {
	  info->memory_error_func (status, pc, info);
	  return -1;
  }
  /* find opcode in table, first by matching entire byte (to catch B8-BF, and "macros insns")
  */
  for(opdex = 0, pcode = flexrisc_opcodes; opdex < flexrisc_num_opcodes; opdex++, pcode++)
  {
	  if(pcode->opcVal == opcode)
	  {
		  /* found the opcode exactly
		  */
		  break;
	  }
  }
  /* if not found by exact byte match, try just the "official" opcodes bits */
  if(opdex >= flexrisc_num_opcodes)
  {
	  for(opdex = 0, pcode = flexrisc_opcodes; opdex < flexrisc_num_opcodes; opdex++, pcode++)
	  {
		  if(pcode->opcVal == (opcode & FLEXRISC_OP(opcode)))
		  {
			  /* found the opcode bits
			  */
			  break;
		  }
	  }
  }
  if(opdex >= flexrisc_num_opcodes)
  {
      (*info->fprintf_func)(info->stream, "UNKNOWN OPCODE 0x%02X", opcode);
      return -1;
  }
  if(pcode->numBytes > 1)
  {
	  status = info->read_memory_func (pc, operands, pcode->numBytes, info);

	  if (status != 0)
	  {
		  info->memory_error_func (status, pc, info);
		  return -1;
	  }
  }
  /* dump opcode mnemonic */
  (*info->fprintf_func)(info->stream, "%s\t", pcode->name);
  
  operands[0] = opcode & 0x7;
  opdex = 0;

  if(pcode->numOps > 0)
  {
	  if(! strcmp(pcode->type1, "absAddr"))
		  opdex++;
	  flexrisc_print_operand(pc, pcode->type1, operands[opdex], operands[opdex + 1], info);
	  if(pcode->numBytes > opdex && pcode->type1[0] != '@')
		  opdex+= 1;
  }
  if(pcode->numOps > 1)
  {
	  (*info->fprintf_func)(info->stream, ",");
	  flexrisc_print_operand(pc, pcode->type2, operands[opdex], operands[opdex + 1], info);
	  if(pcode->numBytes > opdex && pcode->type2[0] != '@')
		  opdex+= 1;
  }
  if(pcode->numOps > 2)
  {
	  (*info->fprintf_func)(info->stream, ",");
	  flexrisc_print_operand(pc, pcode->type3, operands[opdex], operands[opdex + 1], info);
	  if(pcode->numBytes > opdex && pcode->type3[0] != '@')
		  opdex+= 1;
  }
  return pcode->numBytes;
}

struct flexrisc_opcode flexrisc_opcodes[256];
int flexrisc_num_opcodes = 0;

/*
 * This is the Quatro flexrisc instruction definition.  It is in this format because
 * this is GENERATED from TCL scripts from the actual h/w, so it is nice that I read
 * it in in this format, so that in the future it would be possible to read this
 * from an external file as well
 
# mnemonic: Mnemonic of the assembler instruction. Multiple instructions
#           with the same name are valid, but must have a different
#           parameter set.
# numBytes: Number of instruction bytes after assembly. Up to 4 are
#           supported.
# opcVal:   Opcode value for instruction byte0.
# opcMask:  Bits to be used for a inherent operand. Eg. 7 uses bit 2-0
#           for a inherent operand.
# numOps:   Number of operands of the instruction.
# type<n>:  Type of the corresponding operand
#           val3         3bit value (0-7)
#           val8         8bit value (0-255)
#           val8imm      8bit immediate value (0-255) leaded by '#'
#           reg8         8bit register name (a1, a2, b1, b2, c1, c2, d1, d3)
#           reg16        16bit register name (a, b, c, d)
#           reg16id      16bit register used indirect (a, b, c, d in parantheses)
#           bit<text>    constant text followed by '.' and a 3bit value (0-7) 
#           absAddr      16bit address
#           relAddr      16bit address
#           @<text>      constant text
# src<n>:   Operand source of corresponding byte. Eg. 1 means that the respective
#           byte is defined by operand 1. A zero is only valid for byte1
#          (the operator byte). 

*/
/*
#  Real instructions
#         mnemonic  numBytes  opcVal  opcMask  numOps  type1    type2    type3  type4  src1   src2   src3   src4
*/
static const char* instbl[] =
{
"addInstr  inc       1           0     7        1       reg8                            1                          ",	/*;# INC   <reg8>*/
"addInstr  adc       1           8     7        2       @a1      reg8                   1                          ",	/*;# ADC   a1, <reg8>*/
"addInstr  mov       1          16     7        2       @a1      reg8                   1                          ",	/*;# MOV   a1, <reg8>*/
"addInstr  or        1          24     7        2       @a1      reg8                   1                          ",	/*;# OR    a1, <reg8>*/
"addInstr  and       1          32     7        2       @a1      reg8                   1                          ",	/*;# AND   a1, <reg8>*/
"addInstr  xor       1          40     7        2       @a1      reg8                   1                          ",	/*;# XOR   a1, <reg8>*/
"addInstr  rol       1          48     7        1       reg8                            1                          ",	/*;# ROL   <reg8>*/
"addInstr  ror       1          56     7        1       reg8                            1                          ",	/*;# ROR   <reg8>*/
"addInstr  dec       1          64     7        1       reg8                            1                          ",	/*;# DEC   <reg8>*/
"addInstr  sbc       1          72     7        2       @a1      reg8                   1                          ",	/*;# SBC   a1, <reg8>*/
"addInstr  add       1          80     7        2       @a1      reg8                   1                          ",	/*;# ADD   a1, <reg8>*/
"addInstr  set       1          88     7        1       bitpsr                          1                          ",	/*;# SET   psr.<bit>*/
"addInstr  tst       1          96     7        1       bita1                           1                          ",	/*;# TST   a1.<bit>*/
"addInstr  clr       1         104     7        1       bitpsr                          1                          ",	/*;# CLR   psr.<bit>*/
"addInstr  mov       1         112     7        2       reg8     @a1                    1                          ",	/*;# MOV   <reg8>, a1*/
"addInstr  cmp       1         120     7        2       @a1      reg8                   1                          ",	/*;# CMP   a1, <reg8>*/
"addInstr  psh       1         128     7        1       reg8                            1                          ",	/*;# PSH   <reg8>*/
"addInstr  pop       1         136     7        1       reg8                            1                          ",	/*;# POP   <reg8>*/
"addInstr  brc       2         144     7        2       bitpsr   relAddr                1      rel(2)              ",	/*;# BRC   psr.<bit>, <addr>*/
"addInstr  brs       2         152     7        2       bitpsr   relAddr                1      rel(2)              ",	/*;# BRS   psr.<bit>, <addr>*/
"addInstr  dec       1         160     7        1       reg16                           1                          ",	/*;# DEC   <reg16>*/
"addInstr  int       1         168     7        1       val3                            1                          ",	/*;# INT   <val3>*/
"addInstr  xcg       1         184     0        2       @a       @sp                    0                          ",	/*;# XCG   a, sp*/
"addInstr  rts       1         185     0        0                                       0                          ",	/*;# RTS*/
"addInstr  rti       1         186     0        0                                       0                          ",	/*;# RTI*/
"addInstr  mul       1         187     0        2       @a1      @a2                    0                          ",	/*;# MUL   a1, a2*/
"addInstr  jmp       3         188     0        1       absAddr                         0      lo(1)  hi(1)        ",	/*;# JMP   <addr>*/
"addInstr  jsr       3         191     0        1       absAddr                         0      lo(1)  hi(1)        ",	/*;# JSR   <addr>*/
"addInstr  inc       1         192     7        1       reg16                           1                          ",	/*;# INC   <reg16>*/
"addInstr  st        3         200     7        2       reg8     absAddr                1      lo(2)  hi(2)        ",	/*;# ST    <reg8>, <addr>*/
"addInstr  st        1         208     7        2       @a1      reg16id                1                          ",	/*;# ST    a1, (<reg16>)*/
"addInstr  st        2         216     7        3       @a1      reg16id  val8          1      2                   ",	/*;# ST    a1, (<reg16>), <val8>*/
"addInstr  ld        2         224     7        2       reg8     val8imm                1      2                   ",	/*;# LD    <reg8>, #<val8>*/
"addInstr  ld        3         232     7        2       reg8     absAddr                1      lo(2)  hi(2)        ",	/*;# LD    <reg8>, <addr>*/
"addInstr  ld        1         240     7        2       @a1      reg16id                1                          ",	/*;# LD    a1, (<reg16>)*/
"addInstr  ld        2         248     7        3       @a1      reg16id  val8          1      2                   ",	/*;# LD    a1, (<reg16>), <val8>*/
NULL
};

/*
#  Pseudo instructions (real ones with fixed parameters)
#         mnemonic  numBytes  opcVal  opcMask  numOps  type1    type2    type3  type4  src1   src2   src3   src4
*/
static const char* mactbl[] =
{
"addInstr  nop       1          24     0        0                                       0                          ",	/*;# NOP*/
"addInstr  clr       1          40     0        1       @a1                             0                          ",	/*;# CLR   a1*/
"addInstr  mul2      1          80     0        1       @a1                             0                          ",	/*;# MUL2  a1*/
"addInstr  stz       1          88     0        0                                       0                          ",	/*;# STZ*/
"addInstr  stc       1          89     0        0                                       0                          ",	/*;# STC*/
"addInstr  stn       1          90     0        0                                       0                          ",	/*;# STN*/
"addInstr  sti       1          91     0        0                                       0                          ",	/*;# STI*/
"addInstr  clz       1         104     0        0                                       0                          ",	/*;# CLZ*/
"addInstr  clc       1         105     0        0                                       0                          ",	/*;# CLC*/
"addInstr  cln       1         106     0        0                                       0                          ",	/*;# CLN*/
"addInstr  cli       1         107     0        0                                       0                          ",	/*;# CLI*/
"addInstr  brne      2         144     0        1       relAddr                         0      rel(1)              ",	/*;# BRNE  <addr>*/
"addInstr  brcc      2         145     0        1       relAddr                         0      rel(1)              ",	/*;# BRCC  <addr>*/
"addInstr  brlt      2         145     0        1       relAddr                         0      rel(1)              ",	/*;# BRLT  <addr>*/
"addInstr  brpl      2         146     0        1       relAddr                         0      rel(1)              ",	/*;# BRPL  <addr>*/
"addInstr  breq      2         152     0        1       relAddr                         0      rel(1)              ",	/*;# BREQ  <addr>*/
"addInstr  brcs      2         153     0        1       relAddr                         0      rel(1)              ",	/*;# BRCS  <addr>*/
"addInstr  brge      2         153     0        1       relAddr                         0      rel(1)              ",	/*;# BRGE  <addr>*/
"addInstr  brmi      2         154     0        1       relAddr                         0      rel(1)              ",	/*;# BRMI  <addr>*/
NULL
};

/*
#  Pseudo instructions (fake ones that get handled special)
#         mnemonic  numBytes  opcVal  opcMask  numOps  type1    type2    type3  type4  src1   src2   src3   src4
*/
static const char* exttbl[] =
{
"addInstr  ldl       2         224     7        2       reg8     val16imm                1      2                   ",	/*;# LD    <reg8>, #<val8>*/
"addInstr  ldh       2         224     7        2       reg8     val16imm                1      2                   ",	/*;# LD    <reg8>, #<val8>*/
NULL
};

static void add_fr_opcode(int n, const char* pt)
{
	struct flexrisc_opcode* pc;
	char* pd;
	int   i;
	
	if(n < 0 || n > 256) return;
	
	pd = (char*)pt;
	pc = &flexrisc_opcodes[n];
	
	if(! strncmp(pd, "addInstr", 8))
	{
		while(*pd && *pd != ' ' && *pd != '\t' && *pd != '"')
			pd++;
		while(*pd && (*pd == ' ' || *pd == '\t'))
			pd++;
		if(! *pd)
		{
			fprintf(stderr, "flexrisc addInstr missing opcode name\n");
			return;
		}
		for(i = 0; i < FLEXRISC_MAX_OPCODEN; i++)
		{
			if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
				break;
			pc->name[i] = *pd++;
		}
		pc->name[i] = '\0';
		
		while(*pd && (*pd == ' ' || *pd == '\t'))
			pd++;
		if(! *pd || (*pd == '"'))
		{
			fprintf(stderr, "flexrisc addInstr missing inst length\n");
			return;
		}
		pc->numBytes = strtol(pd, &pd, 0);
		
		while(*pd && (*pd == ' ' || *pd == '\t'))
			pd++;
		if(! *pd || (*pd == '"'))
		{
			fprintf(stderr, "flexrisc addInstr missing inst opcVal\n");
			return;
		}
		pc->opcVal = strtol(pd, &pd, 0);
		
		while(*pd && (*pd == ' ' || *pd == '\t'))
			pd++;
		if(! *pd || (*pd == '"'))
		{
			fprintf(stderr, "flexrisc addInstr missing inst opcMask\n");
			return;
		}
		pc->opcMask = strtol(pd, &pd, 0);
		
		while(*pd && (*pd == ' ' || *pd == '\t'))
			pd++;
		if(! *pd || (*pd == '"'))
		{
			fprintf(stderr, "flexrisc addInstr missing inst numOps\n");
			return;
		}
		pc->numOps = strtol(pd, &pd, 0);
		if(pc->numOps < 0 || pc->numOps > 4)
		{
			fprintf(stderr, "flexrisc addInstr numOps=%d is bad\n", pc->numOps);
			return;
		}
		while(*pd && (*pd == ' ' || *pd == '\t'))
			pd++;
		
		pc->type1[0] = pc->type2[0] = pc->type3[0] = pc->type4[0] = '\0';
		pc->src1[0] = pc->src2[0] = pc->src3[0] = pc->src4[0] = '\0';
		
		if(pc->numOps > 0)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing type for operand 1\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->type1[i] = *pd++;
			}
			pc->type1[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
		if(pc->numOps > 1)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing type for operand 2\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->type2[i] = *pd++;
			}
			pc->type2[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
		if(pc->numOps > 2)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing type for operand 3\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->type3[i] = *pd++;
			}
			pc->type3[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
		if(pc->numOps > 3)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing type for operand 4\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->type4[i] = *pd++;
			}
			pc->type4[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
		
		/**/
		if(pc->numBytes > 0)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing source for operand 1\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->src1[i] = *pd++;
			}
			pc->src1[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
		if(pc->numBytes > 1)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing source for operand 2\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->src2[i] = *pd++;
			}
			pc->src2[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
		if(pc->numBytes > 2)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing source for operand 3\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->src3[i] = *pd++;
			}
			pc->src3[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
		if(pc->numBytes > 3)
		{
			if(! *pd || (*pd == '"'))
			{
				fprintf(stderr, "flexrisc addInstr %s missing source for operand 4\n", pc->name);
				return;
			}
			for(i = 0; i < FLEXRISC_MAX_OPANDTYPE-1; i++)
			{
				if(*pd == ' ' || *pd == '\t' || *pd == '"' || *pd == '\r' || *pd == '\n')
					break;
				pc->src4[i] = *pd++;
			}
			pc->src4[i] = '\0';
			while(*pd && (*pd == ' ' || *pd == '\t'))
				pd++;
		}
	}
	else
	{
		fprintf(stderr, "Unknown flexrisc init type %s\n", pd);
	}
	return;
}

void flexrisc_init_opcodes()
{
	int n;
	const char** pt;
	
	/* add instructions in instbl as opcodes
	*/
	for(n = 0, pt = instbl; *pt != NULL && n < 256; n++, pt++)
	{
		add_fr_opcode(n, *pt);
	}
	/* add instructions in mactbl as opcodes
	*/
	for(pt = mactbl; *pt != NULL && n < 256; n++, pt++)
	{
		add_fr_opcode(n, *pt);
	}
	/* add instructions in exttbl as opcodes
	*/
	for(pt = exttbl; *pt != NULL && n < 256; n++, pt++)
	{
		add_fr_opcode(n, *pt);
	}
	flexrisc_num_opcodes = n;
}
