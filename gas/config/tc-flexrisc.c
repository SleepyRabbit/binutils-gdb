/* tc-flexrisc.c 
 *
 * Copyright (c) 2016, The Linux Foundation. All rights reserved.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 and
 * only version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Brian Dodge <brian.dodge@zoran.com> 
 */

#include <stdio.h>
#include <ctype.h>
#define  NO_RELOC 0
#include "as.h"

#include "config.h"
#include "subsegs.h"
#include "obstack.h"
#include "symbols.h"
#include "listing.h"
#include "opcode/flexrisc.h"
#include "sb.h"

const char *md_shortopts = "";
struct option md_longopts[] =
{
#define OPTION_LIST_FIXES (OPTION_MD_BASE + 6)
	{ "list_fixes", no_argument, NULL, OPTION_LIST_FIXES },
	{NULL, no_argument, NULL, 0}
	
};
size_t md_longopts_size = sizeof (md_longopts);

const pseudo_typeS	md_pseudo_table[] =
{
	{"align", s_align_bytes, 32},
	{NULL, 0, 0},
};


/* Characters which start a comment.  */
const char comment_chars[] = ";";

/* Characters which start a comment at the beginning of a line.  */
const char line_comment_chars[] = ";";

/* Characters which may be used to separate multiple commands on a
   single line.  */
const char line_separator_chars[] = "";

/* Characters which are used to indicate an exponent in a floating
   point number.  */
const char EXP_CHARS[] = "eE";

/* Characters which mean that a number is a floating point constant,
   as in 0d1.0.  */
const char FLT_CHARS[] = "dD";

/*insn_t struct and mutators.*/
typedef struct {
	unsigned long raw;
	/*Relocation info*/
	struct
	{
		bfd_reloc_code_real_type type;
		expressionS exp;
		int pcrel;
	} reloc;
	int zeroed;
	int fraged;
}
insn_t;


const char* flexrisc_target_format()
{
	switch(OUTPUT_FLAVOR)
	{
	case bfd_target_elf_flavour:
		return "elf32-flexrisc";
	default:
		return "coff-flexrisc";
	}
}

void
md_show_usage (FILE * stream)
{
  fputs ("FLEXRISC assembler",	 stream);
}


int md_parse_option (int c, char *arg ATTRIBUTE_UNUSED)
{
	switch (c)
	{
	default:
		as_fatal ("Unrecognized option (%d) in md_parse_option.", c );
		return 0;
	}
	return 1;
}

void md_begin ()
{
	/* build table of opcodes from def file (opcodes/flexrisc-dis.c)
	*/
	flexrisc_init_opcodes();
}

static int get_expr_from_str(char *s,  expressionS *e)
{
  char *save = input_line_pointer;

  input_line_pointer = s;
  expression (e);
  input_line_pointer = save;

  return 0;
}

/* Interface to relax_segment.  */

const relax_typeS md_relax_table[] =
{
/* The fields are:
   1) most positive reach of this state,
   2) most negative reach of this state,
   3) how many bytes this mode will add to the size of the current frag
   4) which index into the table to try if we can't fit into this one.  */

  /* The first entry must be unused because an `rlx_more' value of zero ends
     each list.  */
  {1, 1, 0, 0},
  {127, -128, 0, 2 },
  {16384, -16385, 0, 0 },
};

int flexrisc_relax_frag (
     segT    segment,
     fragS * fragP,
     long    stretch
	)
{
  /* Address of branch insn.  */
  long growth = 0;
  growth = relax_frag (segment, fragP, stretch);
  return growth;
}

/* Perform post-processing of machine-dependent frags after relaxation.
   Called after relaxation is finished.
   In:	Address of frag.
	fr_type == rs_machine_dependent.
	fr_subtype is what the address relaxed to.

   Out: Any fixS:s and constants are set up.

   The caller will turn the frag into a ".space 0".  */

void
md_convert_frag (
     bfd *abfd ATTRIBUTE_UNUSED,
     segT sec ATTRIBUTE_UNUSED,
     fragS *fragP ATTRIBUTE_UNUSED
		 )
{
}

/*Functions to assemble instructions.*/

typedef enum
{
	itNONE,
	itVAL3,
	itVAL8,
	itVAL8IMM,
	itVAL16IMM,
	itREG8,
	itREG16,
	itREG16ID,
	itBITN,
	itABSADDR,
	itRELADDR,
	itTEXT
}
OPANDTYPE;

static unsigned char regnum(char* regname)
{
	unsigned char rn = 0;
	
	rn = 2 * (regname[0] - 'a');
	switch(regname[1])
	{
	case '1': /* reg8 */
		break;
	case '2': /* reg8 */
		rn++;
		break;
	case '\0': case ')': /* reg16 or reg16id */
		break;
	}
	return rn;
}

static int relocate_relative(char* frag, char *exp)
{
  expressionS offset;

  get_expr_from_str(exp, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
  {
	  as_bad(_("Bad expression for relocation: %s\n"), exp);
      return -1;
  }

  fix_new_exp (
	  		frag_now,
			frag - frag_now->fr_literal,	/* where */
            1,								/* size */
            &offset,
			1,
            BFD_RELOC_8_PCREL
			);
  return 0;
}

static int relocate_absolute(char* frag, char *exp)
{
  expressionS offset;

  get_expr_from_str(exp, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
  {
	  as_bad(_("Bad expression for relocation: %s\n"), exp);
      return -1;
  }
  
  fix_new_exp (
	  		frag_now,
			frag - frag_now->fr_literal,	/* where */
            2,								/* size */
            &offset,
			0,
			BFD_RELOC_16
			);
  return 0;
}

static int relocate_immediate(char* frag, char *exp, int type)
{
  expressionS offset;

  get_expr_from_str(exp, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
  {
	  as_bad(_("Bad expression for immediate: %s\n"), exp);
      return -1;
  }
    
  fix_new_exp (
	  		frag_now,
			frag - frag_now->fr_literal,	/* where */
            1,								/* size */
            &offset,
			0,
			type
			);
  return 0;
}

static void
output_opand(char* dst, OPANDTYPE code, char* opand, char* opcode)
{
	switch(code)
	{
	case itNONE:
		*dst = 0;
		break;
	case itVAL3:
		if(opand[1] < '0' || opand[1] > '7')
			relocate_immediate(dst, opand + 1, BFD_RELOC_8);
		else
			*dst |= 0x7 & strtoul(opand, NULL, 0);
		break;
	case itVAL8:
		if(opand[1] < '0' || opand[1] > '9')
			relocate_immediate(dst, (opand[0] == '#') ? (opand + 1) : opand, BFD_RELOC_8);
		else
			*dst = 0xFF & strtoul(opand, NULL, 0);
		break;
	case itVAL8IMM:
		if(opand[1] < '0' || opand[1] > '9')
			relocate_immediate(dst, opand + 1, BFD_RELOC_8);
		else
			*dst = 0xFF & strtoul(opand + 1, NULL, 0);
		break;
	case itVAL16IMM: /* only one byte is used really, this is for pseudo ops ldl, ldh */
		relocate_immediate(dst, opand + 1, (! strcmp(opcode, "ldl")) ? 
					BFD_RELOC_QUATRO_16 : BFD_RELOC_QUATRO_MV16);
		break;
	case itREG8:
		*dst |= regnum(opand);
		break;
	case itREG16:
		*dst |= regnum(opand);
		break;
	case itREG16ID:
		*dst |= regnum(opand+1);
		break;
	case itBITN:		
		while(*opand && (*opand != '.'))
			opand++;
		if(*opand)
			opand++;
		if(opand[0] < '0' || opand[0] > '7')
		{
			relocate_immediate(dst, opand, BFD_RELOC_FLEXRISC_BITN);
		}
		else
		{
			*dst |= 0x7 & strtoul(opand, NULL, 0);
		}
		break;
	case itABSADDR:
		if(opand[0] < '0' || opand[0] > '9')
		{
			relocate_absolute(dst, opand);
		}
		else 
		{
			unsigned short xv;
			
			xv = 0xFFFF & strtoul(opand, NULL, 0);
			/* remember absAddr are lobyte-highbyte */
			*dst++ = (xv & 0xFF);
			*dst   = (xv & 0xFF00) >> 8;
		}
		break;		
	case itRELADDR:
		/* a hard-number here *should* be a relative
		 * offset period, but, to match the "TCL" version
		 * of the assembler, I relocate that too, to make it
		 * pc-relative
		 */
		//if(opand[0] < '0' || opand[0] > '9')
			relocate_relative(dst, opand);
		//else
		//	*dst = 0xFF & strtoul(opand, NULL, 0);
		break;
	case itTEXT:
		break;
	}
}

static void 
output_insn(
			struct flexrisc_opcode* pcode,
			OPANDTYPE code1, OPANDTYPE code2, OPANDTYPE code3,
			char* val1, char *val2, char *val3
			)
{
	char *dst, *base;
	
	base = dst = frag_more (pcode->numBytes);
	
	/* opcode */
	dst[0] = pcode->opcVal;
	
	if(pcode->numOps > 0)
	{
		if(! pcode->opcMask)
		{
			 /* operand1 is NOT in opcode byte */
			dst++;
		}
		output_opand(dst, code1, val1, pcode->name);
	}
	if(pcode->numOps > 1)
	{
		if(pcode->type1[0] != '@')
			dst++;
		output_opand(dst, code2, val2, pcode->name);
	}
	if(pcode->numOps > 2)
	{
		if(pcode->type2[0] != '@')
			dst++;
		output_opand(dst, code3, val3, pcode->name);
	}
}
	
static int flexrisc_typematch(
				char* ftype,
				char* vtype,
				int insist,
				OPANDTYPE* r_type)
{
	char *pf, *pv;
	unsigned  val;
	
	/* possible types of operand
	 *     val3         3bit value (0-7)
	 *     val8         8bit value (0-255)
	 *     val8imm      8bit immediate value (0-255) leaded by '#'
	 *     reg8         8bit register name (a1, a2, b1, b2, c1, c2, d1, d3)
	 *     reg16        16bit register name (a, b, c, d)
	 *     reg16id      16bit register used indirect (a, b, c, d in parantheses)
	 *     bit<text>    constant text followed by '.' and a 3bit value (0-7) 
	 *     absAddr      16bit address
	 *     relAddr      8bit address offset
	 *     @<text>      constant text
	 */
	pf = ftype;
	pv = vtype;

	*r_type = itNONE;
	
	if(ftype[0] == '@')
	{
		/* match vtype and text after @
		*/
		pf++;
		while(*pf && *pv)
		{
			if(*pf != *pv)
				break;
			pf++;
			pv++;
		}
		if(! *pf && ! *pv)
		{
			*r_type = itTEXT;
			return 1;
		}
		if(insist)
			as_bad(_("Expected text \"%s\", but got \"%s\"\n"), ftype + 1, vtype);
	}
	else if(ftype[0] == 'b' && ftype[1] == 'i' && ftype[2] == 't')
	{		
		/* bitTEXT.N */
		pf+= 3;
		while(*pf && *pv && (*pv != '.'))
		{
			if(*pf != *pv)
			{
				if(insist)
				{
					as_bad(_("Wrong argument, expected %s.N, got %s\n"), ftype+3, vtype);
				}
				return 0;
			}
			pf++;
			pv++;
		}
		if(*pf || (*pv != '.'))
		{
			if(insist)
			{
				as_bad(_("Expected %s.0-7 but got %s\n"), ftype+3, vtype);
			}
			return 0;
		}
		pv++;
		if(*pv >= '0' && *pv <= '7')
		{
			val = strtoul(pv, &pv, 0);
			if(*pv || (val > 7))
			{
				if(insist)
					as_bad(_("Expected a bit number 0-7, got %s\n"), pv);
				return 0;
			}
		}
		*r_type = itBITN;
		return 1;
	}
	else if(! strcmp(ftype, "val3"))
	{
		if(*pv >= '0' && *pv <= '7')
		{
			val = strtoul(vtype, &pv, 0);
			if(*pv || (val > 7))
			{
				if(insist)
					as_bad(_("Expected number 0-7, got %s\n"), vtype);
				return 0;
			}
		}
		*r_type = itVAL3;
		return 1;
	}
	else if(! strcmp(ftype, "val8"))
	{
		/* skip optional (implied) immediate char if present
		*/
		if(*pv == '#')
			pv++;
		
		if(*pv >= '0' && *pv <= '9')
		{
			val = strtoul(vtype, &pv, 0);
			if(*pv || (val > 255))
			{
				if(insist)
					as_bad(_("Expected number 0-255, got %s\n"), vtype);
				return 0;
			}
		}
		*r_type = itVAL8;
		return 1;
	}
	else if(! strcmp(ftype, "val8imm"))
	{
		if(*pv++ != '#')
		{
			if(insist)
				as_bad(_("Expected immediate specification #0-#255, got %s\n"), vtype);
			return 0;
		}
		if(*pv >= '0' && *pv <= '9')
		{
			val = strtoul(pv, &pv, 0);
			if(*pv || (val > 255))
			{
				if(insist)
					as_bad(_("Expected immed. number 0-255, got %s\n"), pv);
				return 0;
			}
		}
		*r_type = itVAL8IMM;
		return 1;
	}
	else if(! strcmp(ftype, "val16imm"))
	{
		if(*pv++ != '#')
		{
			if(insist)
				as_bad(_("Expected immediate specification #0-#65535, got %s\n"), vtype);
			return 0;
		}
		*r_type = itVAL16IMM;
		return 1;
	}
	else if(! strcmp(ftype, "reg8"))
	{
		if(vtype[0] >= 'a' && vtype[0] <= 'd')
		{
			if(vtype[1] >= '1' && vtype[1] <= '2')
			{
				if(vtype[2] == '\0')
				{
					*r_type = itREG8;
					return 1;
				}
			}
		}
		if(insist)
			as_bad(_("Expected a1,a2,b1,b2,c1,c2,d1, or d2, but got %s\n"), vtype);
	}
	else if(! strcmp(ftype, "reg16"))
	{
		if(vtype[0] >= 'a' && vtype[0] <= 'd')
		{
			if(vtype[1] == '\0')
			{
				*r_type = itREG16;
				return 1;
			}
		}
		if(insist)
			as_bad(_("Expected a,b,c, or d, but got %s\n"), vtype);
	}
	else if(! strcmp(ftype, "reg16id"))
	{
		if(vtype[0] == '(')
		{
			if(vtype[1] >= 'a' && vtype[1] <= 'd')
			{
				if(vtype[2] == ')')
				{
					if(vtype[3] == '\0')
					{
						*r_type = itREG16ID;
						return 1;
					}
				}
			}
		}
		if(insist)
			as_bad(_("Expected a,b,c, or d, but got %s\n"), vtype);
	}
	else if(! strcmp(ftype, "absAddr"))
	{
		while(*pv)
		{
			/* check for special chars to disqualify vtype as an address
			*/
			if(*pv == '(' || *pv == '#')
			{
				break;
			}
			pv++;
		}
		if(! *pv)
		{
			*r_type = itABSADDR;
			return 1;
		}
		if(insist)
			as_bad(_("Expected absolute address, but got %s\n"), vtype);
	}
	else if(! strcmp(ftype, "relAddr"))
	{
		while(*pv)
		{
			/* check for special chars to disqualify vtype as an address
			*/
			if(*pv == '(' || *pv == '#')
			{
				break;
			}
			pv++;
		}
		if(! *pv)
		{
			*r_type = itRELADDR;
			return 1;
		}
		if(insist)
			as_bad(_("Expected relative address, but got %s\n"), vtype);
	}
	else
	{
		as_bad(_("Internal: bad operand typespec %s\n"), ftype);
	}
	return 0;
}


/*****************************************************************************
   md_assemble:  Assemble an instruction

   Assumptions about the passed-in text:
  	- all comments, labels removed
  	- text is an instruction
  	- all white space compressed to single blanks
  	- all character constants have been replaced with decimal

  *************************************************************************** */
void
md_assemble (char *line)		/* Source text of instruction */
{
	char *pi, *po1, *po2, *po3, *pl;

	struct flexrisc_opcode* pcode = NULL, *lastpcode;
	int nparms;
	int opn;
	int opmatch, typematch;
	OPANDTYPE ot1, ot2, ot3;
	
	//printf("=%s=\n", line);
	
	nparms = 0;
	pi  = NULL;
	po1 = NULL;
	po2 = NULL;
	
	/* normalize and terminate instruction 
	*/
	pi = pl = line;
	if(*pl)
	{
		while(*pl && *pl != ' ' && *pl != '\t' && *pl != '\r' && *pl != '\n')
			pl++;
		if(*pl)
			*pl++ = '\0';
	}
	/* see if there is an operand 1
	*/
	while(*pl == ' ' || *pl == '\t' || *pl == '\r' || *pl == '\n')
		pl++;
	po1 = pl;
	if(*po1)
	{
		nparms++;
		while(*pl && *pl != ',' && *pl != ' ' && *pl != '\t' && *pl != '\r' && *pl != '\n')
			pl++;
		if(*pl)
			*pl++ = '\0';
	}
	/* and see if there is an operand 2
	*/
	while(*pl == ' ' || *pl == '\t' || *pl == '\r' || *pl == '\n')
		pl++;
	po2 = pl;	
	if(*po2)
	{
		nparms++;
		while(*pl && *pl != ',' && *pl != ' ' && *pl != '\t' && *pl != '\r' && *pl != '\n')
			pl++;
		if(*pl)
			*pl++ = '\0';
	}
	/* and see if there is an operand 3
	*/
	while(*pl == ' ' || *pl == '\t' || *pl == '\r' || *pl == '\n')
		pl++;
	po3 = pl;	
	if(*po3)
	{
		nparms++;
		while(*pl && *pl != ',' && *pl != ' ' && *pl != '\t' && *pl != '\r' && *pl != '\n')
			pl++;
		if(*pl)
			*pl++ = '\0';
	}
	
	/* lookup opcode based on concat of opcode and parmtyped
	*/
	opmatch = 0;

	ot1 = ot2 = ot3 = itNONE;
	lastpcode = NULL;
	
	for(opn = 0, pcode = flexrisc_opcodes; opn < flexrisc_num_opcodes; opn++, pcode++)
	{
		if(! strcmp(pcode->name, pi))
		{
			opmatch = 1;
			typematch = 0;
			
			/*
			printf("mda opcode match %d  np=%d types=%s,%s,%s,%s\n",
				opn, pcode->numOps,
				pcode->type1,
				pcode->type2,
				pcode->type3,
				pcode->type4);
			*/
			if(pcode->numOps == nparms)
			{				
				typematch = 1;
				lastpcode = pcode;
				
				switch(nparms)
				{
					/*
				case 4:
					if(! flexrisc_typematch(pcode->type4, po4, 0, &ot4))
					{
						typematch = 0;
						break;
					}
					*/
					/* fall into*/
				case 3:
					if(! flexrisc_typematch(pcode->type3, po3, 0, &ot3))
					{
						typematch = 0;
						break;
					}
					/* fall into*/
				case 2:
					if(! flexrisc_typematch(pcode->type2, po2, 0, &ot2))
					{
						typematch = 0;
						break;
					}
					/* fall into*/
				case 1:
					if(! flexrisc_typematch(pcode->type1, po1, 0, &ot1))
					{
						typematch = 0;
						break;
					}
					/* fall into*/
				case 0:
					break;
				default:
					as_bad(_("Bad number of operands (%d) in inst %s\n"), nparms, pi);
					return;
				}
			}
			if(typematch)
			{
				/*
				printf("op matches %s ", pcode->name);
				if(pcode->numOps > 0)
					printf("[%s]", pcode->type1);
				if(pcode->numOps > 1)
					printf(",[%s]", pcode->type2);
				if(pcode->numOps > 2)
					printf(",[%s]", pcode->type3);
				if(pcode->numOps > 3)
					printf(",[%s]", pcode->type4);
				printf("\n");
				*/
				break;
			}	
		}
	}
	if(opn >= flexrisc_num_opcodes)
	{
		if(! opmatch)
		{
			as_bad (_("Unrecognized instruction: `%s'"), line);
		}
		else
		{
			// just typematch again, and print mis-match errors
			//
			if(lastpcode)
			{
				switch(nparms)
				{
					/*
				case 4:
					typematch = flexrisc_typematch(lastpcode->type4, po4, 1, &ot4);
					*/
					/* fall into*/
				case 3:
					typematch = flexrisc_typematch(lastpcode->type3, po3, 1, &ot3);
					/* fall into*/
				case 2:
					typematch = flexrisc_typematch(lastpcode->type2, po2, 1, &ot2);
					/* fall into*/
				case 1:
					typematch = flexrisc_typematch(lastpcode->type1, po1, 1, &ot1);
				}
			}
			else
			{
				as_bad (_("Wrong number of operands for instruction: `%s'"), line);
			}
		}
		return;
	}
	/* build opcode
	*/
    output_insn(pcode, ot1, ot2, ot3, po1, po2, po3);
	
	return;
}				/* md_assemble() */


int tc_equal_in_insn(char c ATTRIBUTE_UNUSED, char *line ATTRIBUTE_UNUSED)
{
  return(1);
}


/* Turn a string in input_line_pointer into a floating point constant
   of type type, and store the appropriate bytes in *litP.  The number
   of LITTLENUMS emitted is stored in *sizeP.  An error message is
   returned, or NULL on OK.  */
/* Turn a string in input_line_pointer into a floating point constant
   of type TYPE, and store the appropriate bytes in *LITP.  The number
   of LITTLENUMS emitted is stored in *SIZEP.  An error message is
   returned, or NULL on OK.  */

/* Equal to MAX_PRECISION in atof-ieee.c  */
#define MAX_LITTLENUMS 6

char *
md_atof (type, litP, sizeP)
     int type;
     char *litP;
     int *sizeP;
{
  int prec;
  LITTLENUM_TYPE words[MAX_LITTLENUMS];
  LITTLENUM_TYPE *wordP;
  char *t;
  char * atof_ieee PARAMS ((char *, int, LITTLENUM_TYPE *));

  switch (type)
    {
    case 'f':
    case 'F':
      prec = 2;
      break;

    case 'd':
    case 'D':
      prec = 4;
      break;

    default:
      *sizeP = 0;
      return "bad call to md_atof";
    }

  t = atof_ieee (input_line_pointer, type, words);
  if (t)
    input_line_pointer = t;
  *sizeP = prec * sizeof (LITTLENUM_TYPE);
  for (wordP = words; prec--;)
    {
      md_number_to_chars (litP, (valueT) (*wordP++), sizeof (LITTLENUM_TYPE));
      litP += sizeof (LITTLENUM_TYPE);
    }

  return NULL;
}

void
md_apply_fix (fixS *   fixP,
               valueT * value,
               segT     seg ATTRIBUTE_UNUSED);
void
md_apply_fix (fixS *   fixP,
               valueT * value,
               segT     seg ATTRIBUTE_UNUSED)
{
	char *buf = fixP->fx_where + fixP->fx_frag->fr_literal;
	valueT val = *value;
	
	/* printf("md_apply_fix 0x%X\t%X\n", fixP->fx_r_type, (unsigned int)val); */
	
	if (fixP->fx_addsy == (symbolS *) NULL)
	{
		/*long valx = (long)val;
		fprintf(stderr, "---------  marking reloc done for %08lx\n", valx);
		*/
		fixP->fx_done = 1;
	}
	switch (fixP->fx_r_type)
	{
	case BFD_RELOC_FLEXRISC_BITN:
		if(val > 7)
		    as_bad ("value of %d too large for bit number", (int)val);		
		*value = (valueT)(val & 0x7);
		buf[0] = (buf[0] & ~7) | (val & 0x7);
		break;
	case BFD_RELOC_8:
		if(val > 255)
		    as_bad ("Relocation %d exceeds 8 bits", (int)val);
		*value = (valueT)(val & 0xFF);
		buf[0] = val & 0xFF;
		break;
	case BFD_RELOC_8_PCREL:
		if((int)val > 0 && (int)val > 127) {
			/* it might work if val == 128, but that depends on the flexrisc being right 
			*  so I error 1 byte earlier here */
		    as_bad ("PC-Relative relocation +%d overflows, 127 bytes is maximum forward distance", (int)val);
		}
		else if((int)val < 0 && (int)val < -127) {
		    as_bad ("PC-Relative relocation -%d overflows, 127 bytes is maximum backward distance", (int)val);
		}
		else {
			/* sub one from val, since it is relative to the end of frag byte, not the opcode
			*/
			buf[0] = (val - 1) & 0xFF;
		//	fixP->fx_done = 1;
		}
		break;
	case BFD_RELOC_QUATRO_16:
		*value = (valueT)(val & 0xFF);
		buf[0] = (char)(*value);
		break;
	case BFD_RELOC_QUATRO_MV16:
		*value = (valueT)((val & 0xFF00) >> 8);
		buf[0] = (char)(*value);
		break;
	case BFD_RELOC_16:
		buf[0] = val & 0xFF;
		buf[1] = (val & 0xFF00) >> 8;
		break;
	case NO_RELOC:
	default:
		as_bad ("Bad relocation type: 0x%02x", fixP->fx_r_type);
		break;
	}
	return;
}

/* should never be called for pm4x.  */
void
md_create_short_jump (char *ptr ATTRIBUTE_UNUSED, addressT from_addr ATTRIBUTE_UNUSED, addressT to_addr ATTRIBUTE_UNUSED,
		      fragS * frag ATTRIBUTE_UNUSED, symbolS * to_symbol ATTRIBUTE_UNUSED)
{
  as_fatal ("md_create_short_jmp\n");
}

/* should never be called for pm4x.  */
void
md_create_long_jump (char *ptr ATTRIBUTE_UNUSED, addressT from_addr ATTRIBUTE_UNUSED, addressT to_addr ATTRIBUTE_UNUSED,
		     fragS * frag ATTRIBUTE_UNUSED, symbolS * to_symbol ATTRIBUTE_UNUSED)
{
  as_fatal ("md_create_long_jump\n");
}

/* Prepare machine-dependent frags for relaxation.

   Called just before relaxation starts. Any symbol that is now undefined
   will not become defined.

   Return the correct fr_subtype in the frag.

   Return the initial "guess for fr_var" to caller.  The guess for fr_var
   is *actually* the growth beyond fr_fix. Whatever we do to grow fr_fix
   or fr_var contributes to our returned value.

   Although it may not be explicit in the frag, pretend
   fr_var starts with a value.  */

int
md_estimate_size_before_relax (fragP, segment)
     fragS *fragP;
     segT segment;
{
  /* The only thing we have to handle here are symbols outside of the
     current segment.  They may be undefined or in a different segment in
     which case linker scripts may place them anywhere.
     However, we can't finish the fragment here and emit the reloc as insn
     alignment requirements may move the insn about.  */

  if (S_GET_SEGMENT (fragP->fr_symbol) != segment)
    {
      /* The symbol is undefined in this segment.
         Change the relaxation subtype to the max allowable and leave
         all further handling to md_convert_frag.  */
      fragP->fr_subtype = 2;
      as_warn (_("External symbol in machine_dependant frag!"));

    }
  return md_relax_table[fragP->fr_subtype].rlx_length;
}

/* Handle local labels peculiar to us referred to in an expression.  */
symbolS *
md_undefined_symbol (char *name ATTRIBUTE_UNUSED)
{
  return NULL;
}
/* Translate internal representation of relocation info to BFD target
   format.  */

arelent *
tc_gen_reloc (
     asection *section ATTRIBUTE_UNUSED,
     fixS *fixP
		 )
{
  arelent *reloc;

  bfd_reloc_code_real_type code = fixP->fx_r_type;
  asymbol *sym = symbol_get_bfdsym (fixP->fx_addsy);

  reloc               = xmalloc (sizeof (arelent));
  reloc->sym_ptr_ptr  = xmalloc (sizeof (asymbol *));
  *reloc->sym_ptr_ptr = sym;
  reloc->address      = fixP->fx_frag->fr_address + fixP->fx_where;
  reloc->howto        = bfd_reloc_type_lookup (stdoutput, code);
  reloc->addend       = fixP->fx_offset;
  
  if(!reloc->howto)
  {
	  const char *name = S_GET_NAME (fixP->fx_addsy);
	  if (name == NULL)
		  name = "<unknown>";
	  as_fatal ("Cannot generate relocation type for symbol %s, code %s",
			  name, bfd_get_reloc_code_name (code));
	  return NULL;
  }
#if 0
  if(0)
  {
	  valueT val = S_GET_VALUE(fixP->fx_addsy);
	  unsigned long valx = (unsigned long)val;
	  fprintf(stderr, "gen-reloc ty=%d sym %s val=%8lx off=%08llx dn=%d\n", code, 
			  S_GET_NAME(fixP->fx_addsy), valx, fixP->fx_offset, fixP->fx_done);
  }
#endif
  return reloc;
}

/* GAS will call this function for each section at the end of the assembly,
   to permit the CPU backend to adjust the alignment of a section.  */

valueT
md_section_align (seg, addr)
     asection *seg;
     valueT addr;
{
  int align = bfd_get_section_alignment (stdoutput, seg);
  return ((addr + (1 << align) - 1) & (-1 << align));
}

/* The location from which a PC relative jump should be calculated,
   given a PC relative reloc.  */

long
md_pcrel_from_section (fixP, sec)
     fixS * fixP;
     segT   sec;
{
  if (fixP->fx_addsy != (symbolS *) NULL
      && (! S_IS_DEFINED (fixP->fx_addsy)
	  || S_GET_SEGMENT (fixP->fx_addsy) != sec))
    {
      /* The symbol is undefined (or is defined but not in this section).
	 Let the linker figure it out.  */
      return 0;
    }

  return (fixP->fx_frag->fr_address + fixP->fx_where);
}

