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


/* Describes the format of an quatro machine instruction */

/* Operate (with Literal)

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |rw            |ra      |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |ra   |rb            |lit                       |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

   This is the form for operations that use a literal.
   
   RW (rw) = Register to which result is written.  If RW = 0, LA is loaded instead.
   RA (ra) = First operand (the one to which extracts are applied).  RA = 0 is undefined.
   RB (rb) = Second operand.  The effect of RB = 0 varies depending on the instruction
   Literal (lit) = 9 bit signed literal.  This literal is ALWAYS 9 bits wide.
   
*/
struct oprtimd { 
  unsigned lit:9, rb:5, ra:5, rw:5, op:8;
  };

/*Operate (with concurrent memory read)

  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |opcode                 |rw            |ra      |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |ra   |rb            |rm            |ar      | 1|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

  
  All operations that don't require a literal can have a longword memory
  read (MEMP) in the same cycle.
  
  RM = MEMPL(AR), AR+=IR
  
  RM (pmem_rm) = Register to which result is written.
  AR (pmem_ar) = Address (and increment) register to use
  PMEM (pmem) = 1, indicating parallel memory access.  */

struct oprtmem { 
  unsigned bit0:1, ar:3, rm:5, rb:5, ra:5, rw:5, op:8;
   };


/*Operate (EXTL in extract stage)
static int operate_with_iextl(char op, char rw, char ra, char rb, char rm, char ar);


  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |opcode                 |rw            |ra      |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |ra   |rb            |ewid       |epos          |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  
  struct op_w_extl { unsigned op:8, rw:5, ra:5, rb:5, ewid:4, epos:5;} raw;
  
  
  EWidth (ewid) = Extract width (encoded value)   15 = do left shift
  EPos (epos) = Extract pos (0-31), unless EWidth = 15, in which case it specifies the left shift count.



*/
struct oprtextl { 
  unsigned epos:5, ewid:4, rb:5, ra:5, rw:5, op:8;
   };

/*Operate (with EXTL in Inserter stage)

  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |opcode                 |rw            |ra      |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |ra   |rb            |iepos            |iewid| 0|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  
  All operations that don't require a literal can do an EXTL in the
  inserter stage (bit 0 distinguishes this from a parallel memory
  read).  Setting Width to 63 and Pos to 3 gives EXTL 'bypass', which
  simply passes the lower 32 bits unchanged.

  IEWidth (iewid) = Extractor width (encoded value: 8, 16, 24 or 32)
  IEPos (iepos) = Extactor position.  63 = bypass.  */

struct oprtiextl { 
  unsigned bit0:1, iewid:2, iepos:6, rb:5, ra:5, rw:5, op:8;
   };


/*Select

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |rw            |ra      |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |ra   |rb            |cond    |sel_lit          |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

	The select literal is smaller than before.  It is only 6 bits (signed) now.

    Cond (sel_cond) = Select condition
    Literal (sel_lit) = Select literal
*/
struct movcc { 
  unsigned lit:6, cc:3, rb:5, ra:5, rw:5, op:8;
   };

/*Register Move/Broadcast

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |rw            |ra      |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |ra   |b |sdp  |ddp  |             0            |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

	B (broad) = Broadcast (DDp ignored)
	SDp (sdp) = Source Dp
	DDp (ddp) = Dest Dp (= 0 if B is set)
*/
struct mov { 
  unsigned bits0_8:9, ddp:2, sdp:2, b:1, rb:5, ra:5, rw:5, op:8;
   };

/*	Memory Op with Register Increment

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |dp   |ar      |ra/rm   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |rb            |0    |In|Wid  |Q |B |P |M |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

	Memory writes:  MEMx(AR+RB) = RA or MEMx(AR)=RA, AR+= IR
	Memory reads:    RM = MEMx(AR+RB) or RM = MEMx(AR), AR += IR
	  
 (For post-increment, the RB field is ignored.  It is set to 0) CMEM
 instructions use the same form, but the AR field is ignored (AR is
 assumed to be 0) XMEM instructions also use this form, except the P
 bit is always 0.

	DP (mem_dp): Memory Datapath (ignored if 'quad' is set)
	AR (mem_ar): Memory address register 
	RM (mem_rm): Register that receives result (memory reads)
    In (mem_in):  0 = Post-increment mode, 1 = Index mode
	Wid (mem_wid):  Width:  0 = DWORD, 1 = Word, 2 = Byte
	Q (mem_quad): 0 = Single register, 1 = Quad
	B (mem_broad) 0 = Normal, 1 = broadcast (read data goes to all DPs)
	P (mem_par)  0 = Serial memory access, 1 = Parallel access
	M (mem_req) 0 = Just update address reg, 1 = Request memory access */

struct memrr { 
  unsigned m:1,p:1,b:1,q:1, wid:1, idx:1, bits7_8:2, rb:5, rm:5, ar:3, dp:2, op:8;
   };



/*Memory Op with Literal Increment

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |dp   |ar      |ra/rm   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |lit                 |In|Wid  |Q |B |P |M |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

	Memory writes:  MEMx(AR+Lit) =  RA or MEMx(AR) = RA, AR+= Lit
	Memory reads:    RM = MEMx(AR+Lit) or RM = MEMx(AR), AR += Lit
	XMEM instructions also use this form (with the P bit set to 0).

	Lit (memli_lit): Literal increment (signed: +63/-64)

DP (mem_dp): Memory Datapath (ignored if ‘quad’ is set)
AR (mem_ar): Memory address register
RM (mem_rm): Register that receives result (memory reads)
In (mem_in): 0 = Post-increment mode, 1 = Index mode
Wid (mem_wid): Width: 0 = DWORD, 1 = Word, 2 = Byte
Q (mem_quad): 0 = Single register, 1 = Quad
B (mem_broad) 0 = Normal, 1 = broadcast (read data goes to all DPs)
P (mem_par) 0 = Serial memory access, 1 = Parallel access
M (mem_req) 0 = Just update address reg, 1 = Request memory access

*/	
struct memri   { 
  unsigned m:1,p:1,b:1,q:1, wid:1, idx:1, lit:7, rm:5, ar:3, dp:2,op:8;
   };


/*Literal Memory Access
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |dp   |   lit  |  rm    |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |lit                          |q |  lit   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

   Lit (memlit): Unsigned literal address
   This always does a longword access.
   This instruction is quad & parallel if Q is set, otherwise it's a single.
   The CMEM instruction also uses this form (with the Q bit set to 0).
*/

struct memi   { 
  unsigned litlo:3, q:1, litmid:10, rm:5, lithi:3, dp:2, op:8;
   };

/*Special Register Load Literal

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |dp   |q |lit  |sreg    |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |lit                                      |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

	SPECREG(Reg.s) = Lit // if Q = 0   (single register load)
	SPECREG(Reg)  = Lit   // if Q = 1     (multi-register load)

	DP (sregld_dp):  Datapath (0 if Q = 1)
	Q (sregld_quad):  0 = Single, 1 = Quad
	SPECREG (sreg):  Register to load
	Lit (sregld_lit):   Literal value to load into register
*/
struct srldi  { 
  unsigned litlo:14, sreg:5, lithi:2, q:1, dp:2, op:8;
   };

/*Special Register Load From Register

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |dp   |q |  0  |sreg    |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |rb            |            0             |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

   SPECREG(Reg.s) = RB.s  //(Q = 0)
   SPECREG(Reg) = RB  // (Q = 1)
   
   Address register load/store uses the same fields (with a different
   op-code).  It replaces the SReg field with 2 other fields:
   
   Bits 16-14:  AReg (ar_ldst)
   Bits 18-17: AType (ar_type)  0 = Address register, 1 = increment register
*/   
struct srld      { 
  unsigned bits0_8:9, rb:5, sreg:5, bits19_20:2, q:1, dp:2, op:8;
   };

struct arld      { 
  unsigned bits0_8:9, rb:5, areg:3, inc:2, bits19_20:2, q:1, dp:2, op:8;
   };



/*Special Register Store Register

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |rw            |sreg    |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |dp   |q |                0               |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

   RW = SPECREG(Reg) // (Q = 0) 
   RW.s = SPECREG(Reg.s) // (Q = 1)

   Address register load/store uses the same fields (with a different
   op-code)
*/   
struct srst      { 
  unsigned bits0_8:9, q:1, dp:2, sreg:5, rw:5, op:8;
   };
struct arst      { 
  unsigned bits0_8:9, q:1, dp:2, areg:5, rw:5, op:8;
   };


/*Conditional Branch

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |dp   |cond |q |ra      |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |brlit         |offset                    |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+   


   if (RA (cond) brlit) cond: =, !=, >, < Q = Do quad compare (all 4
   conditions must be true)

   DP (br_dp)
   Cond (br_cond)
   Quad (br_quad)
   Offset (br_offset)*/


struct br { 
  unsigned offset:9, lit:5, ra:5, q:1, cc:2, dp:2, op:8;
   };


/*Conditional Branch with increment

   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |opcode                 |dp   |cond |q |ra      |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |     |inc     |offset                          |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+   

   Form: if (RA (condition) 0, RA += Inc) goto PC + offset (offset is
   11 bits) Inc ranges from -4 to 3

   Inc (bri_inc)
   Cond (bri_cond)
   Offset (bri_offset)
*/

struct brinc     { 
  unsigned  offset:11, inc:3, ra:5, q:1, cc:2, dp:2, op:8;
    };

/*
  Subroutine Call/Return, Unconditional branch

  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |opcode                 |        offset         |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    offset                     |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+   

  

  offset - 0 for return 1 for returnI

*/
struct jmp 
{ 
    unsigned offset:24, op:8; 
};

/*
  Load Literal:  RW = Lit
  
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |opcode                 |rw            |dp   |q |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   | lit                                           |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   
   2 instructions:  One prepends the 16 bit 'LIT_HIGH' register.	
   Q (ldlit_quad) = Quad
   DP (ldlit_dp)
   Literal (ldlit_lit):  16 bit literal to load
   
*/
struct ldi 
{ 
  unsigned lit:16, q:1, dp:2, rw:5, op:8;
};

struct condex {
  unsigned c1:1, bit1_8:8, lit:5, ra:5, bit19:1, cc:2, bits22_23:2, op:8;
  };














