/*
 *
 * Copyright (c) 2014, 2015 Linux Foundation. All rights reserved.
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
 */


/* tc-quatro.c */
#define TCQUATRO_INTERNAL 1 /*redefine md_number_to...*/
#include <stdio.h>
#include <ctype.h>
#define  NO_RELOC 0
#include "as.h"
#include "write.h"
#include "config.h"
#include "subsegs.h"
#include "obstack.h"
#include "symbols.h"
#include "listing.h"
#include "opcode/quatro.h"
#include "sb.h"

/* If we are using ELF, then we probably can support dwarf2 debug
   records.  Furthermore, if we are supporting dwarf2 debug records,
   then we want to use the assembler support for compact line numbers.  */
#ifdef OBJ_ELF
#include "dwarf2dbg.h"
#endif /* OBJ_ELF */

/*const*/
/*
int md_short_jump_size = 4;
int md_long_jump_size = 4;
const int md_reloc_size = RELSZ;	
*/

#ifndef false
#define false 0
#define true 1
#endif

static valueT chars_to_number_swapendian (char* buf, int n);
void number_to_chars_swapendian (
    char*  buf,
    valueT i,
    int    n
    );

#define Swap32(x)  ((((x) & 0x000000FF) << 24) | \
                    (((x) & 0x0000FF00) <<  8) | \
		            (((x) & 0x00FF0000) >>  8) | \
                    (((x) & 0xFF000000) >> 24))



static int amber = false;
static int amber_fixes = false;
static int branch_delay = 3;
static int list_fixes = false;

static int debuginfo = 0;

char next_rw = -1;
char last_rw = -1;
char regstall = 0;

#define SIZE_OF_LONG_COND_BRANCH (11 * 4)

const char comment_chars[] = "#";
const char line_comment_chars[] = "#";

/* We needed an unused char for line separation to work around the
   lack of macros, using sed and such.  */
const char line_separator_chars[] = ";";


/* Chars that can be used to separate mant from exp in floating point nums.  */
const char EXP_CHARS[] = "eE";

/* Chars that mean this number is a floating point constant.  */
const char FLT_CHARS[] = "fF";

#define ISSPACE(a) isspace((int)(unsigned)(a))
#define ISALPHA(a) isalpha((int)(unsigned)(a))
#define ISALNUM(a) isalnum((int)(unsigned)(a))

/*insn_t struct and mutators.*/
typedef struct{
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
}insn_t;


//PPA stuff

enum TOK { TOKNO, TOKLIT, TOKID, TOKOP, TOKKEY, TOKIREG, 
           TOKAREG, TOKGPREG, TOKDEC, TOKALIAS, TOKADDNEG, TOKEQNEG, TOKOPNEG };

static struct hash_control *ppa;
static struct hash_control *keyword;
static struct hash_control *gregs;
static struct hash_control *ireg;
static struct hash_control *areg;
static struct hash_control *fields;
static struct hash_control *extwidths;
static struct hash_control *iextwidths;
static struct hash_control *aliases;
static struct hash_control *alias_keys;

typedef struct {
  const char *key;
  const void *value;
}hash_t;

static hash_t fire_insn_template[];
static hash_t fire_keywords[];
static hash_t amber_insn_template[];
static hash_t amber_keywords[];
static hash_t alias_keywords[];
static hash_t field_desc[];
static hash_t general_registers[];
static hash_t address_registers[];
static hash_t index_registers[];
static hash_t ext_width_table[];
static hash_t iext_width_table[];

char yytext[256];

static int isletter(char c);
static int isnumber(char c);
static char *iskeyword(char *id);
static int isgpreg(char *id);
static int isareg(char *id);
static int isireg(char *id);
static enum TOK gettok(char **line);
static char *ppa_match(char *search);
static int ppa_parse(char *_line);
int ppa_register_alias(char *line);
int ppa_pack(insn_t *i, char *fmt, sb *arg);
static int ppa_simplify(sb *str, sb *arg);
static int ppa_setup(void);

int set_ldlit_lit(insn_t *i, char *str);
int set_ir_ldst(insn_t *i, char *str);
int set_ar_ldst(insn_t *i, char *str);
int set_memlit(insn_t *i, char *label);
int reloc_memlit(insn_t *i, expressionS *e);
int set_memli_lit(insn_t *i, char *str);
int set_mem_ir(insn_t *i, char *str);
int set_mem_rm(insn_t *i, char *str);
int set_mem_ar(insn_t *i, char *str);
int set_pmem_rm(insn_t *i, char *str);
int set_pmem_ir(insn_t *i, char *str);
int set_pmem_ar(insn_t *i, char *str);
int set_bri_offset(insn_t *i, char *label);
int set_uncond_offset(insn_t *i, char *label);
int set_bri_inc(insn_t *i, char *increment);
int set_br_lit(insn_t *i, char *str);
int set_br_offset(insn_t *i, char *label);
int set_epos(insn_t *i, char *epos);

int set_ewid(insn_t *i, char *str);
int set_lit(insn_t *i, char *l);
int set_rb(insn_t *i, char *r);
int set_ra(insn_t *i, char *r);
int set_rw(insn_t *i, char *r);
int match_zero(insn_t *i, char *str);

char *new_string_from_sb(sb *src);
sb *argsnew(void);
void argspush(char *arg);
void argskill(void);
void argsreset(void);


const char* quatro_target_format()
{
	switch(OUTPUT_FLAVOR)
	{
	case bfd_target_elf_flavour:
		return "elf32-quatro";
	default:
		return "coff-quatro";
	}
}

#define MEMLITBITS 0x00383ff7UL

#if 0
static unsigned long stuff(unsigned long mask, unsigned long value)
{
  unsigned long bit = 1;
  unsigned long stuffed = 0;

  while(bit)
    {
      if(mask & 1)
        {
          if(value & 1)
            stuffed = stuffed | bit;
          value = value >> 1;
        }
      mask = mask >> 1;
      bit = bit << 1;

      
    }  

  return stuffed;
} 
#endif




static int is_cmem(unsigned long insn)
{
  unsigned char amber_cmem_insn[] = {0xde,
                                     0xdf,
                                     0xe0,
                                     0xe1};
  unsigned char op = insn >> 24;
  int cmem;
  
  cmem = (amber_cmem_insn[0] == op || 
          amber_cmem_insn[1] == op || 
          amber_cmem_insn[2] == op || 
          amber_cmem_insn[3] == op );

  return(cmem);
    
}

static int is_branch(unsigned long insn)
{
  unsigned char amber_br_insn[] = {0xcc, 
                                   0xcd, 
                                   0xce, 
                                   0xcf, 
                                   0xd0, 
                                   0xd1};

  unsigned char op = insn >> 24;
  int br;
  
  br = (amber_br_insn[0] == op || 
        amber_br_insn[1] == op || 
        amber_br_insn[2] == op || 
        amber_br_insn[3] == op || 
        amber_br_insn[4] == op || 
        amber_br_insn[5] == op );

  return(br);
    
  

}


static void zero_insn_t(insn_t *i)
{
  memset((char *)i, 0, sizeof(insn_t));
  i->raw = 0xdeadbeaf;
  i->reloc.type = NO_RELOC;
  i->zeroed = (long)i;
  i->fraged = 0;
}
#if TARGET_BYTES_BIG_ENDIAN
static valueT
md_chars_to_number_bigendian (
     char * buf,
     int    n
    )
{
  valueT result = 0;
  unsigned char * where = (unsigned char *) buf;

  while (n--)
  {
      result <<= 8;
      result |= (*where++ & 255);
  }
  return result;
}
#else
static valueT
chars_to_number_swapendian (
     char * buf,
     int    n
    )
{
  valueT result = 0;
  unsigned char * where = (unsigned char *) buf;

  while (n--)
  {
      result <<= 8;
      result |= (*where++ & 255);
  }
  return Swap32(result);
}

void number_to_chars_swapendian (
    char*  buf,
    valueT i,
    int    n
    )    
{
    number_to_chars_bigendian(buf, Swap32(i), n);
}
#endif

#if 0
static int reloc16(insn_t *i, expressionS *e)
{
  i->reloc.exp = *e;
  i->reloc.type = BFD_RELOC_16;
  i->reloc.pcrel = 0;
  return 0;
}
#endif

static int reloc_br(insn_t *i, expressionS *e)
{
  i->reloc.exp = *e;
  i->reloc.type = BFD_RELOC_QUATRO_9_PCREL;
  i->reloc.pcrel = 1;
  return 0;
}
static int reloc_bri(insn_t *i, expressionS *e)
{
  i->reloc.exp = *e;
  i->reloc.type = BFD_RELOC_QUATRO_11_PCREL;
  i->reloc.pcrel = 1;
  return 0;
}

static int reloc_jmp(insn_t *i, expressionS *e)
{
  i->reloc.exp = *e;
  i->reloc.type = BFD_RELOC_24_PCREL;
  i->reloc.pcrel = 1;
  return 0;
}

const pseudo_typeS
  md_pseudo_table[] =
{
  {"align", s_align_bytes, 32},
  {NULL, 0, 0},
};


void
md_show_usage (FILE * stream)
{
  fputs ("Quatro DSP assembler - use \"as [-g] filename\"", stream);
}


//extern int machine;
//extern int coff_flags;

const char *md_shortopts = "";
struct option md_longopts[] =
{
#define OPTION_AMBER (OPTION_MD_BASE + 6)
#define OPTION_AMBER_FIXES (OPTION_MD_BASE + 7)
#define OPTION_LIST_FIXES (OPTION_MD_BASE + 8)
#define OPTION_DEBUG (OPTION_MD_BASE + 9)
  { "amber", no_argument, NULL, OPTION_AMBER },
  { "no_fixes", no_argument, NULL, OPTION_AMBER_FIXES },
  { "list_fixes", no_argument, NULL, OPTION_LIST_FIXES },
  {NULL, no_argument, NULL, 0}

};

size_t md_longopts_size = sizeof (md_longopts);

int md_parse_option (int c, char *arg ATTRIBUTE_UNUSED)
{
 switch (c)
    {
    case OPTION_AMBER:
      as_warn(_("assembling for amber.\n"));
      amber = true;
      branch_delay = 2;
      //machine = bfd_mach_quatro_amber;
      //      coff_flags = F_QUATRO_AMBER;
      break;
    case OPTION_AMBER_FIXES:
      as_warn(_("No amber bug fixes.\n"));
      amber_fixes = false;
      break;
    case OPTION_LIST_FIXES:
      list_fixes = true;
      break;
    case 'g':
      if(debug_type != DEBUG_DWARF2)
      {
          as_warn(("Setting debug type to -gdwarf-2, you should add that to the cmd line"));
          debug_type = DEBUG_DWARF2;
      }
    //  printf("adding line-number info for each instruction.\n");
      debuginfo = 1;
      break;
    default:
      as_fatal ("Unrecognized option (%d) in md_parse_option.", c );
      return 0;
    }
    return 1;
}

void
md_begin ()
{

  if(amber)
    {
      bfd_set_arch_mach (stdoutput, TARGET_ARCH, bfd_mach_quatro_amber);
    }

  ppa_setup();
}

/*Functions to assemble instructions.*/



static int get_expr_from_str(char *s,  expressionS *e)
{
  char *save = input_line_pointer;

  input_line_pointer = s;
  expression (e);
  input_line_pointer = save;

  return 0;
}
/* Interface to relax_segment.  */

/* FIXME: Look through this.  */

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
  {1024*4, -1023*4, 0, 2 },
  {0x2000000, -0x2000000, 44, 0 },
};

/* Perform post-processing of machine-dependent frags after relaxation.
   Called after relaxation is finished.
   In:	Address of frag.
	fr_type == rs_machine_dependent.
	fr_subtype is what the address relaxed to.

   Out: Any fixS:s and constants are set up.

   The caller will turn the frag into a ".space 0".  */

void
md_convert_frag (abfd, sec, fragP)
     bfd *abfd ATTRIBUTE_UNUSED;
     segT sec ATTRIBUTE_UNUSED;
     fragS *fragP;
{

//  int old_fr_fix;
  long int value;
  int offset;
  unsigned long i;

  know (fragP->fr_type == rs_machine_dependent);
  know (fragP->fr_subtype==1 || fragP->fr_subtype == 2);

  resolve_symbol_value (fragP->fr_symbol);
  /*
    as_warn("\nconverting  %s @ %lx,fr_offset %lx,fr_address %lx,fr_fix %lx\n", 
          S_GET_NAME(fragP->fr_symbol),
          S_GET_VALUE (fragP->fr_symbol),
          fragP->fr_offset,
          fragP->fr_address,
          fragP->fr_fix);
  */
  /*The -4 is because fr_fix points to the instruction after the branch.*/
  value = S_GET_VALUE (fragP->fr_symbol) -
      (fragP->fr_address + (fragP->fr_fix - 4));   

  offset = value >> 2;
  offset -= branch_delay;

  if (fragP->fr_subtype==1)
    {     
      i = md_chars_to_number (fragP->fr_opcode, 4);
      offset &= 0x7FF;
      i |= offset;
      md_number_to_chars(fragP->fr_opcode, i, 4);
    }
  else if(fragP->fr_subtype == 2)
    {

     /* Word or dword displacement.  */
      unsigned int *l ;
      
      i = md_chars_to_number (fragP->fr_opcode, 4);
	  i |= 5;
      md_number_to_chars(fragP->fr_opcode, i, 4);
      
      l = (unsigned int*)(fragP->fr_literal + fragP->fr_fix);
      /*
        00000000 <__foo>:
        0:   e2 38 40 05     IF(R1<0)GOTO pc + 32 =20 <__true>
        4:   00 00 00 00     NOP
        8:   00 00 00 00     NOP
        c:   00 00 00 00     NOP
        10:   df 00 00 05     GOTO pc + 32 =30 <__false>
        14:   00 00 00 00     NOP
        18:   00 00 00 00     NOP
        1c:   00 00 00 00     NOP
        
        00000020 <__true>:
        20:   df ff ff f5     GOTO pc + -32 =0 <__foo>
        24:   00 00 00 00     NOP
        28:   00 00 00 00     NOP
        2c:   00 00 00 00     NOP
        
        00000030 <__false>:
        30:   00 00 00 00     NOP
        34:   00 00 00 00     NOP
      */
      md_number_to_chars((char*)&l[0], 0, 4);
      md_number_to_chars((char*)&l[1], 0, 4);
      md_number_to_chars((char*)&l[2], 0, 4);
      md_number_to_chars((char*)&l[3], 0xdf000005, 4);
      md_number_to_chars((char*)&l[4], 0, 4);
      md_number_to_chars((char*)&l[5], 0, 4);
      md_number_to_chars((char*)&l[6], 0, 4);
      offset -= 8; /*The eight instruction from the if to here.*/
      offset &= 0xFFFFFF;
      md_number_to_chars((char*)&l[7], 0xdf000000 | offset, 4);
      md_number_to_chars((char*)&l[8], 0x0, 4);
      md_number_to_chars((char*)&l[9], 0x0, 4);
      md_number_to_chars((char*)&l[10], 0x0, 4);
      fragP->fr_fix += 44;
    }
}

/* Relax a machine dependent frag.  This returns the amount by which
   the current size of the frag should change.  */
/*md_relax_frag*/
int quatro_relax_frag (
     segT    segment,
     fragS * fragP,
     long    stretch
	)
{
  /* Address of branch insn.  */
  //long address = fragP->fr_address + fragP->fr_fix - 2;
  long growth = 0;
  growth = relax_frag (segment, fragP, stretch);
  return growth;
}
#if 0
int quatro_relax_frag (segment, fragP, stretch)
     segT    segment;
     fragS * fragP;
     long    stretch;
{
  int old_fr_fix;
  long int value;
  int offset;
  unsigned long i;

  know (fragP->fr_subtype==1 || fragP->fr_subtype == 2);

  old_fr_fix = fragP->fr_fix;

  value = S_GET_VALUE (fragP->fr_symbol) + fragP->fr_offset;
  value = S_GET_VALUE (fragP->fr_symbol) -
      (fragP->fr_address + fragP->fr_fix);   

  offset = value >> 2;
  offset -= branch_delay;
  /*
    as_warn("\nrelaxing %s @ %x,fr_offset %x,fr_address %x,fr_fix %x,fr_var %x\n", 
    S_GET_NAME(fragP->fr_symbol),
    S_GET_VALUE (fragP->fr_symbol),
    fragP->fr_offset,
    fragP->fr_address,
    fragP->fr_fix,
    fragP->fr_var);
  */
  if (offset >= -1024 && offset <= 1023)
    {
      if(fragP->fr_var)
        return (0 - fragP->fr_var);
    }
  else
    {
      //      fragP->fr_fix += SIZE_OF_LONG_COND_BRANCH;
      return 0; //SIZE_OF_LONG_COND_BRANCH;
    }

  //  frag_wane (fragP);

  //  return fragP->fr_var + (fragP->fr_fix - old_fr_fix);
}
#endif
/* Generate code and fixes for a 32-bit conditional branch instruction
   created by "extending" an existing 8-bit branch instruction.

   opcodep    Pointer to the word containing the original 8-bit branch
	      instruction.

   writep     Pointer to "extension area" following the first instruction
	      word.

   fragP      Pointer to the frag containing the instruction.

   add_symP,  Parts of the destination address expression.
   sub_symP,
   add_num.  */
/*
if(cond) goto foo;

if(cond) goto +8:
NOP;
NOP;
NOP;
GOTO +8;
NOP;
NOP;
NOP;
__foo:
GOTO foo;
NOP;
NOP;
NOP;
__cont;*/

static void 
output_insn (insn_t * insn);

void 
output_insn(insn_t * insn)
{
  unsigned long raw = insn->raw;
  char *dst;
  unsigned long nop = 0;

  if(insn->fraged==1) 
    {
      return;
    }

  dst = frag_more (4);


  if(amber && amber_fixes)
    {
      /*
        
        Amber fix:  
        
        Due to a bug in the first pass of the Amber chip, certain instruction
        sequences can cause an improper instruction to be executed.  These 
        sequences (and their solutions) are:
        
        1 - Two successive CMEM instructions or Two instructions that
        depend on each other (i.e.:  R2 = R1 followed by R3 = R2)
        Solution: Insert a NOP between them
        
        2 - All PC-modifying instructions (branches, calls and returns)
        must be the last entry in a cache line
        
        Solution: Insert NOPs until this is true.
        
      */
      if(regstall)
        {
          if(list_fixes)
            as_warn(_("found regstall!\n"));
          md_number_to_chars (dst, nop, 4);
          dst = frag_more(4);
        }

      if(is_cmem(raw))
        {
          unsigned long offset = dst - frag_now->fr_literal;
          if(offset >= sizeof(nop)) 
            {
              unsigned last = md_chars_to_number (dst - 4, 4);
              if(is_cmem(last))
                {
                  if(list_fixes)
                    as_warn(_("fouand sequential CMEM!\n"));
                  md_number_to_chars (dst, nop, 4);
                  dst = frag_more(4);
                }
            }
        }

      if(is_branch(raw))
        {
          unsigned long offset = dst - frag_now->fr_literal;
          int nops;
          
          
          nops =  7 - ((offset/4) % 8);
          
          if(list_fixes)
            as_warn(_("found branch adding %d nops!\n"), nops);
          while(nops--)
            {
              md_number_to_chars (dst, nop, 4);
              dst = frag_more(4);
            }
          
        }
    }
      md_number_to_chars (dst, raw, 4);
      
  /* Put out the symbol-dependent stuff.  */
  if (insn->reloc.type != NO_RELOC)
    {
      bfd_reloc_code_real_type type = insn->reloc.type;
      expressionS exp = insn->reloc.exp;
      int pcrel = insn->reloc.pcrel;
      /* Where is the offset into the fragment for this instruction.  */
      fix_new_exp (
              frag_now,
    		   dst - frag_now->fr_literal,	/* where */
               4,	/* size */
               &exp,
               pcrel,
               type
            );
    }
  /* put in line number info */
  if (debuginfo)
    {
      dwarf2_emit_insn(4);
    }
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
  int err;

  last_rw = next_rw;
  regstall = 0;

  err = ppa_parse(line);

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
  valueT fixed;

  /*
  printf("md_apply_fix 0x%X\t%X\n", (unsigned)fixP->fx_r_type, (unsigned)val);
  if(fixP->fx_addsy)
    {
        printf("  s=%s\n",  S_GET_NAME(fixP->fx_addsy));
    }
  */
   /*
   printf("md_apply_fix 0x%X\t%X\n", (unsigned)fixP->fx_r_type, (unsigned)val);
   if(fixP->fx_addsy)
     {
         printf("  s=%s\n",  S_GET_NAME(fixP->fx_addsy));
     }
   */

  fixed = md_chars_to_number (buf, 4);

  if (fixP->fx_addsy == (symbolS *) NULL)
    fixP->fx_done = 1;

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_32:
      fixed = val;
      break;
    case BFD_RELOC_24:
      fixed = fixed | (((val >> 2)) & 0x00FFFFFF);
      break;
    case BFD_RELOC_24_PCREL:
      fixed = fixed | (((val >> 2)- branch_delay) & 0x00FFFFFF);
      break;
    case BFD_RELOC_16:
      fixed = fixed | (val & 0x0000FFFF);
     break;
    case BFD_RELOC_16_PCREL:
      fixed = fixed | (val & 0x0000FFFF);
     break;
    case BFD_RELOC_QUATRO_MEMLIT:
      {
        //lit:0..2,4..13,19..21:p:0:65535
  //    valueT _fixed = fixed;
  //    valueT _val = val;
        val = val/4;
        val = val & 0xFFFF;
        val = (val & 7) | (((val >> 3) & 0x7FF) << 4) | (((val >> 13) & 7) << 19);
        fixed = fixed | val;
        // as_warn("md_apply_fix BFD_RELOC_QUATRO_MEMLIT \n0x%lx 0x%lX\n0x%lx 0x%lX", _val, _fixed, val, fixed);
        break;
      }
    case BFD_RELOC_QUATRO_9_PCREL:
      {
      int _offset = val/4;
      _offset -= branch_delay;
      _offset &= 0x1FF;
   //   printf("BFD_RELOC_QUATRO_9_PCREL @ %lx %lx %lx %lx %x\n", fixP->fx_where, fixed, val, (unsigned long)branch_delay, _offset);
      val = _offset;
      fixed = fixed | val;
      }
     break;
    case BFD_RELOC_QUATRO_11_PCREL:
      {
      int _offset = val >> 2;
      _offset -= branch_delay;
      _offset &= 0x7FF;

      if(-1024 > _offset || _offset > 1023)
          as_fatal ("BFD_RELOC_QUATRO_11_PCREL offset is too big!\n");

      fixed = fixed | _offset;
      }
     break;

    case NO_RELOC:
    default:
      as_bad ("Bad relocation type: 0x%02x", fixP->fx_r_type);
      break;
    }
  md_number_to_chars (buf, fixed, 4);
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
  arelent *rel;

  bfd_reloc_code_real_type code = fixP->fx_r_type;
  asymbol *sym = symbol_get_bfdsym (fixP->fx_addsy);

  rel = (arelent *) xmalloc (sizeof (arelent));
  rel->sym_ptr_ptr = (asymbol **) xmalloc (sizeof (asymbol *));
  *rel->sym_ptr_ptr = sym;
  /* We assume that all rel->address are host byte offsets.  */
  rel->address = fixP->fx_frag->fr_address + fixP->fx_where;
  rel->address /= OCTETS_PER_BYTE;
  rel->howto = bfd_reloc_type_lookup (stdoutput, code);
  rel->addend = fixP->fx_offset;

  if (!rel->howto)
    {
      const char *name = S_GET_NAME (fixP->fx_addsy);
      if (name == NULL)
        name = "<unknown>";
      as_fatal ("Cannot generate relocation type for symbol %s, code %s",
                name, bfd_get_reloc_code_name (code));
      return NULL;
    }
  return rel;
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

  return (fixP->fx_frag->fr_address + fixP->fx_where) & ~1;
}










static int isletter(char c)
{
  return (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_';
}
static int isnumber(char c)
{
  return (c>='0' && c<='9');
}

static int ishexnumber(char c)
{
  return (c>='0' && c<='9') | (c>='a' && c<='f') | (c>='A' && c<='F');
}

static char *iskeyword(char *id)
{
  return(hash_find (keyword, id));
}


static int isgpreg(char *id)
{
  char *r = hash_find (gregs, id);
  if(r)
    return atoi(r);
  else 
    return -1;
}

static int isareg(char *id)
{
  char *r = hash_find (areg, id);
  if(r)
    return atoi(r);
  else 
    return -1;
}

static int isireg(char *id)
{
  char *r = hash_find (ireg, id);
  if(r)
    return atoi(r);
  else 
    return -1;
}



static enum TOK gettok(char **line)
{
  char *l = *line;
  enum TOK tok = 0;
  char *t = &yytext[0];
  char *alias;

  if(isletter(*l))
    {
      
      while(isletter(*l) || isnumber(*l)) *t++ = *l++;
      
      *t++ = 0;
      
      alias = hash_find(aliases, yytext);
      
      if(alias) 
        {
          strcpy(yytext, alias);
          tok = TOKALIAS;
        }
      else if(iskeyword(yytext))
        tok = TOKKEY;
      else if(isgpreg(yytext)>=0)
        tok = TOKGPREG;
      else if(isareg(yytext)>=0)
        tok = TOKAREG;
      else if(isireg(yytext)>=0)
        tok = TOKIREG;
      else
        tok = TOKID;
    }

  else if(*l=='0'&&l[1]=='x')
    {
      /*Hex number*/
      *t++ = *l++;
      *t++ = *l++;
      while(ishexnumber(*l)) *t++ = *l++;
      *t++ = 0;
      //      as_warn("Got hex number %s, 0x%X\n", yytext, strtoul(yytext,NULL,0));
      tok = TOKLIT;
    }
  else if(isnumber(*l))
    {
      while(isnumber(*l)) *t++ = *l++;
      *t++ = 0;
      tok = TOKLIT;
    }
  else if(l[0]=='.' && 
          (l[1] == 'a' || l[1] == 'b' || l[1] == 'c' || l[1] == 'd' ))
    {
      *t++ = *l++;
      *t++ = *l++;
      *t++ = 0;
      tok = TOKOP;
    }
  else if(ISSPACE((l[0])))/*white space*/ 
    {
      while(ISSPACE((*l)))l++;
      *t++ = ' ';
      *t++ = 0;
      tok = TOKNO;
    }
  else if(l[0]=='-' && l[1]=='-')/*decrement*/
    {
      l++;
      l++;
      tok = TOKDEC;
    }
  else if((l[0]=='|' ||
           l[0]=='+' ||
           l[0]=='&' ||
           l[0]=='*' ||
           l[0]=='=' ||
           l[0]=='>' ||
           l[0]=='<' ||
           l[0]=='^' ||
           l[0]==',')
          && l[1]=='-' && isnumber(l[2]))
    {
      *t++ = *l++;
      *t++ = *l++;
      while(isnumber(*l)) *t++ = *l++;
      *t++ = 0;
      tok = TOKOPNEG;
    }
  else if(l[0]=='-' && isnumber(l[1]))
    {
      *t++ = '+';
      *t++ = *l++;
      while(isnumber(*l)) *t++ = *l++;
      *t++ = 0;
      tok = TOKOPNEG;
    }
  else
    {
      *t++ = *l++;
      *t++ = 0;
      tok = TOKOP;
    }

  *line = l;
  return(tok);
}



static char *ppa_match(char *search)
{

  char *fmt;
  fmt = hash_find (ppa, search);
  return fmt;
}


static const int nargs = 10;
static sb args[10];
static sb *argp;


sb *argsnew(void)
{
  int i;
  argp = args;
  for(i=0; i < nargs; i++)
    sb_new(&args[i]);

  return(args);

}

void argspush(char *arg)
{
  if(argp >= &args[nargs])
    as_fatal ("Too many arguments in exprestion.");

  sb_add_string(argp++, arg);
}

void argskill(void)
{
  int i;
  for(i=0; i < nargs; i++)
    sb_kill(&args[i]);
}

void argsreset(void)
{
  int i;
  for(i=0; i < nargs; i++)
    sb_reset(&args[i]);
  argp = args;
}

static char * getid(sb *tok, char *l)
{

  /*eat any white space*/
  while(ISSPACE(*l)) l++;
  
  if(!ISALPHA(*l)) return 0;
  
  while(ISALNUM(*l))
    sb_add_char(tok, *l++);

  return l;
}


static char * getreg(sb *tok, char *l)
{

  /*eat any white space*/
  while(ISSPACE(*l)) l++;
  
  if(!ISALPHA(*l)) return 0;
  
  while(ISALNUM(*l) || *l=='.')
    sb_add_char(tok, *l++);

  return l;
}



static char * getequal(char *l)
{
  /*eat any white space*/
  while(ISSPACE(*l)) l++;
  
  if(*l++ == '=') return l;
  
  return 0;
}




int ppa_register_alias(char *line)
{
  
  sb id;
  sb alias;
  char *retval;
  sb key;
  
  sb_new(&key);
  line = getid(&key, line);

  if(hash_find(alias_keys, sb_terminate(&key)))
    {

      sb_new(&id);
      sb_new(&alias);

      line = getid(&id, line);
      line = getequal(line);
      line = getreg(&alias, line);

      retval = (char*)hash_insert(aliases, new_string_from_sb(&id), new_string_from_sb(&alias));
      
      if (retval)
        {
          as_bad ("%s adding register alias \"%s\".", retval, sb_terminate(&alias));
        }
      
      sb_kill(&id);
      sb_kill(&alias);
      
      return(1);
    }

  sb_kill(&key);
  return(0);
}

static void ppa_scrub(sb *line)
{
  char *l;
  sb scratch;

  sb_new(&scratch);
  
   for(l = sb_terminate(line); *l ; l++) 
    {
      sb_add_char(&scratch, *l);
      if(!ISALNUM(l[0]) && ISSPACE(l[1])) l++;
    }

  sb_reset(line);
  sb_add_sb(line, &scratch);
  sb_kill(&scratch);

}

int ppa_parse(char *_line)
{

  sb search;
  sb scratch;
  sb line;
  char *fmt;
  insn_t i;
  enum TOK tok;

  zero_insn_t(&i);
  
  argsreset();
  
  sb_new(&search);
  sb_new(&scratch);
  sb_new(&line);

  sb_add_string(&line, _line);

  ppa_scrub(&line);
  
  if(!ppa_register_alias(sb_terminate(&line)))
    {
      while(*sb_terminate(&line))
        {
          char *l;
          
          sb_reset(&scratch);
          sb_add_sb(&scratch, &line);
          
          l = sb_terminate(&scratch);
          
          tok = gettok(&l);
          
          sb_reset(&line);
          sb_add_string(&line, l);
      
          switch(tok)
            {
            case TOKALIAS:
              sb_reset(&scratch);
              sb_add_string(&scratch, yytext);
              sb_add_sb(&scratch, &line);
              sb_reset(&line);
              sb_add_sb(&line, &scratch);
              break;
            case TOKGPREG:
              sb_add_string(&search, "reg");
              argspush(yytext);
              break;
            case TOKAREG:
              sb_add_string(&search, "areg");
              argspush(yytext);
              break;
            case TOKIREG:
              sb_add_string(&search, "ireg");
              argspush(yytext);
              break;
            case TOKLIT:
              sb_add_string(&search, "lit");
              argspush(yytext);
              break;
            case TOKID:
              sb_add_string(&search, "lit");
              argspush(yytext);
              break;
            case TOKKEY:
              sb_add_string(&search, yytext);
              break;
            case TOKOP:
              sb_add_string(&search, yytext);
              break;
            case TOKDEC:
              sb_add_string(&search, "+=lit");
              argspush("-1");
              break;
            case TOKOPNEG:
              sb_add_char(&search, yytext[0]);
              sb_add_string(&search, "lit");
              argspush(&yytext[1]);
              break;
            case TOKNO:
            default:
              break;
            }
          
        }
  
      ppa_simplify(&search, args);


      fmt = ppa_match(sb_terminate(&search));

      //    as_warn(_("%s, %s, %s\n"), _line, sb_terminate(&search), fmt);

      if(fmt) 
        {
          if(ppa_pack(&i, fmt, args)<0)
            as_bad ("Bad args to \"%s\".\n",_line);
          output_insn(&i);
        }
      else
        as_bad ("Bad instruction: \n\t\"%s\".\n\t\"%s\".\n", _line, sb_terminate(&search));
    }
  sb_kill(&search);
  sb_kill(&scratch);
  sb_kill(&line);

  return 0;
}

char *new_string_from_sb(sb *src)
{
  long len;
  char *dst;

  len = strlen(sb_terminate(src)) + 1;
  dst = xmalloc(len);
  strcpy(dst, sb_terminate(src));

  return(dst);
}

/*Pack the arguments in arg into the machine instuction i as specified by
  fmt.*/
int ppa_pack(insn_t *i, char *fmt, sb *arg)
{
  sb scratch;
  
  sb_new(&scratch);

  while(*fmt != 0 && *fmt !=':')
    {
      sb_add_char(&scratch, *fmt++);
    }

  i->raw = strtoul(sb_terminate(&scratch),NULL,16);
    

  while(*fmt++ == ':')
    {
      int (*pack)(insn_t *, char *);
      int sts = -1;
      sb *a;

      sb_reset(&scratch);
      
      while(*fmt != ':' && *fmt != 0) 
        sb_add_char(&scratch, *fmt++);

      pack = hash_find(fields, sb_terminate(&scratch));
      
      a = arg++;

      if(pack)
        {
          sts = pack(i,sb_terminate(a));
          if(sts<0)
            as_warn("Couldn't pack arg: %s = %s.\n", sb_terminate(&scratch), sb_terminate(a));
        }
      else
        {
          as_bad("Unknown arument: %s, %s.\n", sb_terminate(a), sb_terminate(&scratch));
        }
    }
  
  
  sb_kill(&scratch);

  return 0;
}


/*reduce search pattern*/
static int ppa_simplify(sb *str ATTRIBUTE_UNUSED, sb *arg ATTRIBUTE_UNUSED)
{

  return 0;
  
}


static struct hash_control *new_hash_with_hash_t(const char *n, 
                  hash_t *e)
{
  struct hash_control *hsh;
  const char *retval;
  hsh = hash_new();
  
  for(retval = NULL; e->key && !retval; e++)
    retval = hash_jam (hsh, e->key, (PTR)e->value);
  
  if (retval)
    as_fatal ("Hashing %s returned \"%s\".", n, retval);
  
  return(hsh);
}

static int ppa_setup(void)
{


  if(amber)
    {
      as_warn("Using amber insn tables!");
      ppa = new_hash_with_hash_t("ppa", amber_insn_template);
      keyword = new_hash_with_hash_t("keyword", amber_keywords);
    }
  else
    {
      ppa = new_hash_with_hash_t("ppa", fire_insn_template);
      keyword = new_hash_with_hash_t("keyword", fire_keywords);
    }
  
  alias_keys = new_hash_with_hash_t("alias_keys", alias_keywords);
  fields = new_hash_with_hash_t("fields", field_desc);
  areg  = new_hash_with_hash_t("areg", address_registers);
  ireg  = new_hash_with_hash_t("ireg", index_registers);
  gregs  = new_hash_with_hash_t("gregs", general_registers);
  extwidths  = new_hash_with_hash_t("extwidths", ext_width_table);
  iextwidths  = new_hash_with_hash_t("iextwidths", iext_width_table);

  aliases = hash_new();


  argsnew();

  return 0;
}

static hash_t general_registers[] = {
  {"R1", "1"},
  {"R2", "2"},
  {"R3", "3"},
  {"R4", "4"},
  {"R5", "5"},
  { "R6", "6"},
  { "R7", "7"},
  {"R8", "8"},
  { "R9", "9"},
  { "R10", "10"},
  { "R11", "11"},
  { "R12", "12"},
  {  "R13", "13"},
  { "R14", "14"},
  { "R15", "15"},
  { "R16", "16"},
  { "R17", "17"},
  { "R18", "18"},
  { "R19", "19"},
  { "R20", "20"},
  {"R21", "21"},
  {"R22", "22"},
  { "R23", "23"},
  { "R24", "24"},
  { "R25", "25"},
  { "R26", "26"},
  { "R27", "27"},
  { "R28", "28"},
  { "R29", "29"},
  { "R30",  "30"},
  {"R31",  "31"},
  { 0,0} };

static hash_t address_registers[] = {
    {"A0", "0"},
    {"A1", "1"},
    {"A2", "2"},
    {"A3", "3"},
    {"A4", "4"},
    {"A5", "5"},
   { "A6", "6"},
   { "A7", "7"},
 { 0,0} };


static hash_t index_registers[] = {
    {"I0", "0"},
    {"I1", "1"},
    {"I2", "2"},
    {"I3", "3"},
    {"I4", "4"},
    {"I5", "5"},
   { "I6", "6"},
   { "I7", "7"},
 { 0,0} };




static void reg_written(char r)
{
  if(r==last_rw) regstall=1;
  next_rw = r;
}

static void reg_used(char r)
{
  if(r==last_rw) regstall=1;
}


//
// Description for all fields used by the DSP Core instruction set.  This
// field description is:
//
//   "field_name",   "generic_name:bits_used:type"
//
//    field_name:  Name of the field
//
//    generic_name: Replacement for the field when building a 'generic'
//                    instruction description (for the assembler)
//    bits_used:   Bit locations used, from LSB to MSB - 0..3,4..5
//                    describes a 6 bit field (bits 0-3 and 4-5)
//    type:        Field type.  Valid types are:
//                    u:min:max  - Unsigned int in range min <= int <= max
//                    s:min:max  - Signed int in the range min <= int <= max
//                    p:min:max  - Longword "Pointer".  The field represents
//                                 the pointer value / 4.  Min/max apply to 
//                                 the field, not the pointer.  The two 
//                                 pointer LSBs must be 0, and val is >= 0.
//                    t:table_name - Table lookup
//                    a:field_name - This field is an alias for another
//                                     field in the instruction (i.e it must
//                                     have the same value as that other fld)
//

int match_zero(insn_t *i ATTRIBUTE_UNUSED, char *str)
{
  int zero = strtol(str, 0, 0);

  if(zero==0)
    return 0;
  else
    return -1;
}


//  "rw",             "reg:19..23:u:1:31",    # RW isn't allowed to be 0 


int set_rw(insn_t *i, char *r)
{
  int rw = isgpreg(r);

  if(rw<0)
    as_bad ("Bad register in set_rw \"%s\".", r);

  i->raw |= (rw << 19);

  reg_written(rw);
  return 0;

}

//  "ra",             "reg:14..18:u:1:31",

int set_ra(insn_t *i, char *r)
{
  int ra = isgpreg(r);

  if(ra<0)
    as_bad ("Bad register in set_ra \"%s\".", r);

  i->raw |= (ra << 14);
  reg_used(ra);
  return 0;

}
//  "rb",             "reg:9..13:u:1:31",     # RB isn't allowed to be 0
int set_rb(insn_t *i, char *r)
{
  int rb = isgpreg(r);

  if(rb<0)
    as_bad ("Bad register in set_rb \"%s\".", r);

  i->raw |= (rb << 9);
  reg_used(rb);
  return 0;

}

//  "lit",            "lit:0..8:s:-256:255",

int set_lit(insn_t *i, char *l)
{
  int lit = strtol(l, 0, 0);
  if(lit >= -256 && lit <= 255)
    {
      i->raw |= (lit & 0x1ff);
      return 0;
    }
  return -1;
}


//  "iewid",          "lit:1..2:t:iext_width_table"},
static int set_iewid(insn_t *i, char *str)
{
  
  char *ewid = hash_find(iextwidths, str);
  int wid;

  if(ewid==NULL)
    {
      as_bad ("Bad i-extract width, \"%s\".", str);
      return -1;
    }

  wid = strtol(ewid, 0, 0);

  i->raw |= (wid << 1);
  
  return 0;
}

//  "iepos",          "lit:3..8:u:0:63",   # Actually only goes to 55 for assem.
static int set_iepos(insn_t *i, char *epos)
{
  int pos = strtol(epos, 0, 0);

  if(pos < 0 || pos > 55)
    return -1;

  i->raw |= (pos << 3);
  
  return 0;
}

int set_ewid(insn_t *i, char *str){

  
  char *ewid = hash_find(extwidths, str);
  int wid;

  if(ewid==NULL)
    {
      as_bad ("Bad extract width, \"%s\".", str);
      return -1;
    }

  wid = strtol(ewid, 0, 0);

  i->raw |= (wid << 5);
  
  return 0;
}

/*"lit:0..4:u:0:31"*/
int set_epos(insn_t *i, char *epos)
{
  int pos = strtol(epos, 0, 0);

  i->raw |= (pos & 0x1F);
  
  return 0;
}

//  "br_offset",      "lit:0..8:s:-256:255"},
int set_br_offset(insn_t *i, char *label)
{
  expressionS offset;

  get_expr_from_str(label, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
    return -1;
    
  reloc_br(i, &offset);

  return 0;
}

//"br_lit",         "lit:9..13:s:-16:15"},
int set_br_lit(insn_t *i, char *str)
{
  int lit = strtol(str, 0, 0);
  if(lit >= -16 && lit <=15)
    {
      i->raw |= ((lit & 0x1f) << 9);
      return 0;
    }
  return -1;

}


//  "bri_inc",         "lit:11..13:s:-4:3"},
int set_bri_inc(insn_t *i, char *increment)
{
  int inc = strtol(increment, 0, 0);
  if(inc >= -4 && inc <=3)
    {
      i->raw |= ((inc & 0x7) << 11);
      return 0;
    }
  return -1;

}

//"uncond_offset",  "lit:0..23:s:-8388608:8388607"},
int set_uncond_offset(insn_t *i, char *label)
{
  expressionS offset;

  get_expr_from_str(label, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
    return -1;
  reloc_jmp(i, &offset);

  return 0;
}



// "bri_offset",     "lit:0..10:s:-1024:1023"},

int set_bri_offset(insn_t *i, char *label)
{
  expressionS offset;

  get_expr_from_str(label, &offset);
    
  if(offset.X_op == O_constant)
    {
      reloc_bri(i, &offset);
      return 0;
    }
  else if(offset.X_op == O_symbol)
    {
      expressionS *exprP = &offset;
      /* Handle complex expressions.  */
      valueT addvalue
        = exprP->X_op_symbol != NULL ? 0 : exprP->X_add_number;
      symbolS *sym
        = (exprP->X_op_symbol != NULL
           ? make_expr_symbol (exprP) : exprP->X_add_symbol);
      char *opcodep = frag_more (4);
      md_number_to_chars (opcodep, i->raw, 4);
      /* The expression is not defined yet but may become absolute.  We
         make it a relocation to be relaxed.  */
      frag_var (rs_machine_dependent, SIZE_OF_LONG_COND_BRANCH, 0,
                1,
                sym, addvalue, opcodep);
      i->fraged = 1;
      
      return 0;
    }
  return -1;
}

//  "pmem_ar",        "areg:1..3:u:0:7"},
int set_pmem_ar(insn_t *i, char *str)
{
  unsigned long r = isareg(str);

  i->raw |= (r << 1);
  return 0;

}


//  "pmem_ir",        "ireg:1..3:a:pmem_ar",  
int set_pmem_ir(insn_t *i, char *str)
{
  unsigned long r = isireg(str);
  
  /*Just make sure that the AREG is the same as this IREG.*/
  if(((i->raw >> 1) & 0x7) != r)
    return -1;
  else
    return 0;
}


//  "pmem_rm",        "reg:4..8:u:1:31"},
int set_pmem_rm(insn_t *i, char *str)
{
  unsigned long r = isgpreg(str);

  i->raw |= (r << 4);

  return 0;
}

//  "mem_ar",         "areg:19..21:u:0:7"},
int set_mem_ar(insn_t *i, char *str)
{
  unsigned long r = isareg(str);

  i->raw |= (r << 19);
  return 0;

}
//  "mem_rm",         "reg:14..18:u:1:31"},
int set_mem_rm(insn_t *i, char *str)
{
  unsigned long r = isgpreg(str);

  i->raw |= (r << 14);

  reg_written(r);

  return 0;
}
//"mem_ir",         "ireg:19..21:a:mem_ar",  # mem_ir must match mem_ar
int set_mem_ir(insn_t *i, char *str)
{
  unsigned long r  = isireg(str);
  /*Just make sure that the AREG is the same as this IREG.*/
  if(((i->raw >> 19) & 0x7) != r)
    return -1;
  else
    return 0;
}

//    "memli_lit",      "lit:7..13:s:-64:63"}
int set_memli_lit(insn_t *i, char *str)
{
  int lit = strtol(str, 0, 0);
  if(lit >= -64 && lit <=63)
    {
      i->raw |= ((lit & 0x7f) << 7);
      return 0;
    }
  return -1;

}


int reloc_memlit(insn_t *i, expressionS *e)
{
  i->reloc.exp = *e;
  i->reloc.type = BFD_RELOC_QUATRO_MEMLIT;
  i->reloc.pcrel = 0;
  return 0;
}



static int reloc_ldlit_lit(insn_t *i, expressionS *e)
{
  i->reloc.exp = *e;
  i->reloc.type = BFD_RELOC_16;
  i->reloc.pcrel = 0;
  return 0;
}

#if 0
static int get_u16(char *str)
{
  int lit = strtoul(str, 0, 0);
  return(lit);
}

static int get_s16(char *str)
{
  int lit = strtol(str, 0, 0);
  return(lit);
}

static int const_ok_for_u16(long l)
{
  return(l>=0 && l<=65535);  
}
#endif

static int const_ok_for_s16(long l)
{
  return(l >= -32768 && l <= 65535);
}

//  "memlit",         "lit:0..2,4..13,19..21:p:0:65535"},
int set_memlit(insn_t *i, char *label)
{
  expressionS offset;

  get_expr_from_str(label, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
    return -1;
    
  reloc_memlit(i, &offset);

  return 0;
}

//  "ar_ldst",        "areg:14..16:u:0:7"},
int set_ar_ldst(insn_t *i, char *str)
{
  int r = isareg(str);

  if(r<0)
    as_bad ("Bad register in set_ar_ldst \"%s\".", str);

  i->raw |= (r << 14);
  return 0;

}
//  "ir_ldst",        "ireg:14..16:u:0:7"},
int set_ir_ldst(insn_t *i, char *str)
{
   int r = isireg(str);

  if(r<0)
    as_bad ("Bad register in set_ir_ldst \"%s\".", str);

  i->raw |= (r << 14);
  return 0;
}
//  "sregld_lit",     "lit:0..13,19..20:u:-32768:65535"},
static int set_sregld_lit(insn_t *i, char *str)
{
  int lit = strtol(str, 0, 0);
  if(const_ok_for_s16(lit))
    {
      i->raw |= (lit & 0x3fff);
      i->raw |= ((lit >> 14) & 0x3) << 19;
      return 0;
    }
  return -1;
}

//  "ldlitw_lit",     "lit:0..15:u:0:65535"},
static int set_ldlitw_lit(insn_t *i, char *str)
{
  expressionS offset;

  get_expr_from_str(str, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
    return -1;
    
  reloc_ldlit_lit(i, &offset);

  return 0;
#if 0
  int lit = strtol(str, 0, 0);
  if(const_ok_for_u16(lit))
    {
      i->raw |= (lit & 0xffff);
      return 0;
    }
  return -1;
#endif
}

int set_ldlit_lit(insn_t *i, char *label)
{
  expressionS offset;

  get_expr_from_str(label, &offset);
    
  if(offset.X_op != O_symbol &&
     offset.X_op != O_constant)
    return -1;
    
  reloc_ldlit_lit(i, &offset);

  return 0;
}


#if 0
//  "ldlit_lit",      "lit:0..15:s:-32768:65535"},
static int set_ldlit_lit_non_reloc(insn_t *i, char *str)
{

  int lit = strtol(str, 0, 0);
  if(const_ok_for_s16(lit))
    {
      i->raw |= (lit & 0xffff);
      return 0;
    }
  return -1;
}
#endif

//  "sel_lit",        "lit:0..5:s:-32:31"},
static int set_sel_lit(insn_t *i, char *str)
{
  int lit = strtol(str, 0, 0);
  if(lit >= -32 && lit <=31)
    {
      i->raw |= (lit & 0x3f);
      return 0;
    }
  return -1;

}

//  "sdp_lit",        "lit:0..6:s:-64:63"},
static int set_sdp_lit(insn_t *i, char *str)
{
  int lit = strtol(str, 0, 0);
  if(lit >= -64 && lit <=63)
    {
      i->raw |= (lit & 0x7f);
      return 0;
    }
  return -1;

}

static hash_t ext_width_table[] = {
  {"1" , "0"},
  {"2" ,  "1"},
  {"3" ,  "2"},
  {"4" ,  "3"},
  {"5" ,  "4"},
  {"6" ,  "5"},
  {"8" ,  "6"},
  {"10" ,  "7"},
  {"12" ,  "8"},
  {"14" ,  "9"},
  {"16" ,  "10"},
  {"20" ,  "11"},
  {"24" ,  "12"},
  {"28" ,  "13"},
  {"32" ,  "14"},
  {"LS" ,  "15"},/*Value used for left-shift*/
  {0,  0}};

static hash_t iext_width_table[] = {
  {"8" ,  "0"},
  {"16" ,  "1"},
  {"24" ,  "2"},
  {"32" ,  "3"},
  {0,  0}};

static hash_t field_desc[] = 
{
  {"0", &match_zero},
  {"rw", &set_rw},
  {"ra", &set_ra},
  {"rb", &set_rb},
  {"lit", &set_lit},
  {"ewid", &set_ewid},
  {"epos", &set_epos},
  {"iewid", &set_iewid},
  {"iepos", &set_iepos},
  {"bri_offset",&set_bri_offset},
  {"bri_inc",   &set_bri_inc},   
  {"br_offset",&set_br_offset},
  {"br_lit",   &set_br_lit},   
  {"uncond_offset",  &set_uncond_offset},
  {"pmem_ar", &set_pmem_ar },
  {"pmem_ir", &set_pmem_ir },
  {"pmem_rm", &set_pmem_rm },
  {"mem_ar", &set_mem_ar },
  {"mem_ir", &set_mem_ir },
  {"mem_rm", &set_mem_rm },
  {"memli_lit",  &set_memli_lit},
  {"memlit",    &set_memlit},
  {"ar_ldst",  &set_ar_ldst},
  {"ir_ldst",    &set_ir_ldst},
  {"ldlit_lit",    &set_ldlit_lit},
  {"ldlitw_lit",    &set_ldlitw_lit},
  {"sregld_lit",    &set_sregld_lit},
  {"sdp_lit", &set_sdp_lit},
  {"sel_lit", &set_sel_lit},
  {0,0}
};

/*
  "pmem",           ":0:u:0:1"},
  "pmem_ar",        "areg:1..3:u:0:7"},
  "pmem_ir",        "ireg:1..3:a:pmem_ar",  
  "pmem_rm",        "reg:4..8:u:1:31"},

  "iewid",          "lit:1..2:t:iext_width_table"},
  "iepos",          "lit:3..8:u:0:63",   # Actually only goes to 55 for assem.

  "sel_cond",       ":6..8:u:0:5"},
  "sel_lit",        "lit:0..5:s:-32:31"},

  "broad",          ":13:u:0:1"},
  "dp",             ":7..8:t:dp_table"},
  "sdp_lit",        "lit:0..6:s:-64:63"},
  "ddp",            ":9..10:t:dp_table"},
  "sdp",            ":11..12:t:dp_table"},

  "mem_dp",         ":22..23:t:dp_table"},
  "mem_ar",         "areg:19..21:u:0:7"},
  "mem_rm",         "reg:14..18:u:1:31"},
  "mem_ir",         "ireg:19..21:a:mem_ar",  # mem_ir must match mem_ar
  "mem_in",         ":6:u:0:1"},
  "mem_wid",        ":4..5:u:0:2"},
  "mem_quad",       ":3:u:0:1", 
  "mem_broad",      ":2:u:0:1"},
  "mem_par",        ":1:u:0:1"},
  "mem_req",        ":0:u:0:1"},

  "sreg",           "sreg:14..18:u:0:31"},
  "sregld_dp",      ":22..23:t:dp_table"},
  "sregld_quad",    ":21:u:0:1"},
  # sregld_lit is defined as unsigned to prevent sign-extension
  "sregld_lit",     "lit:0..13,19..20:u:-32768:65535"},
  "sregst_dp",      ":12..13:t:dp_table"},
  "sregst_quad",    ":11:u:0:1"},

  "ar_type",        ":17..18:u:0:3"},

  "br_dp",          ":22..23:t:dp_table"},
  "br_cond",        ":20..21:u:0:3"},
  "br_quad",        ":19:u:0:1"},
  "br_offset",      "lit:0..8:s:-256:255"},
  "br_lit",         "lit:9..13:s:-16:15"},
  "bri_offset",     "lit:0..10:s:-1024:1023"},
  "bri_inc",         "lit:11..13:s:-4:3"},
  "bri_cond",       ":19..21:u:0:5"},
  "uncond_offset",  "lit:0..23:s:-8388608:8388607"},
  "condex_s",       ":0:u:0:1"},

  "ldlit_quad",     ":16:u:0:1"},
  "ldlit_dp",       ":17..18:t:dp_table"},
  "ldlit_lit",      "lit:0..15:s:-32768:65535"},

  "trap_lit",       "lit:0..4:u:0:31"},
 );


*/

static hash_t alias_keywords[] = {
  {"MREG", "1"},
  {"SREG", "1"},
  {"MADR", "1"},
  {"MINC", "1"},
  {0, 0},
};

//fire keywords and instructions

static hash_t fire_keywords[] = {
    {"ADDMB" , "1"},
    {"BYTE" , "1"},
    {"CALL" , "1"},
    {"CMEML" , "1"},
    {"CNTLD" , "1"},
    {"CNTLO" , "1"},
    {"CNTLS" , "1"},
    {"CNTLZ" , "1"},
    {"CONDEX" , "1"},
    {"CONDEX_S" , "1"},
    {"DP_NUM" , "1"},
    {"ENDCONDEX" , "1"},
    {"EXT" , "1"},
    {"EXT2" , "1"},
    {"EXT2_ALL" , "1"},
    {"EXTI" , "1"},
    {"EXTI2" , "1"},
    {"EXTL" , "1"},
    {"EXTR" , "1"},
    {"EXT_ALL" , "1"},
    {"EXT_INC" , "1"},
    {"EXT_INIT" , "1"},
    {"EXT_NXT" , "1"},
    {"EXT_POS" , "1"},
    {"EXT_SIGNED" , "1"},
    {"EXT_WIDTH" , "1"},
    {"GOTO" , "1"},
    {"HALT" , "1"},
    {"IF" , "1"},
    {"INS" , "1"},
    {"INS_ADD" , "1"},
    {"INS_ALL" , "1"},
    {"INS_EPOS" , "1"},
    {"INS_INC" , "1"},
    {"INS_INIT" , "1"},
    {"INS_IPOS" , "1"},
    {"INS_MODE" , "1"},
    {"INS_WIDTH" , "1"},
    {"LA" , "1"},
    {"LI" , "1"},
    {"LIT_HIGH" , "1"},
    {"LOAD_LA" , "1"},
    {"LONG" , "1"},
    {"MEMB" , "1"},
    {"MEML" , "1"},
    {"MEMPB" , "1"},
    {"MEMPL" , "1"},
    {"MEMPW" , "1"},
    {"MEMSL" , "1"},
    {"MEMW" , "1"},
    {"NOP" , "1"},
    {"PBYTE" , "1"},
    {"PLONG" , "1"},
    {"PWORD" , "1"},
    {"QCOMP" , "1"},
    {"QCOMPB" , "1"},
    {"QMULA" , "1"},
    {"QMULA_SIGNED" , "1"},
    {"RETURN" , "1"},
    {"RETURNI" , "1"},
    {"RSVD" , "1"},
    {"SELECT" , "1"},
    {"SPECREG" , "1"},
    {"WORD" , "1"},
    {"XMEMB" , "1"},
    {"XMEML" , "1"},
    {"XMEMW" , "1"},
    {0, 0},
};


static hash_t fire_insn_template[] = {
  
   // Op code = 0 (0x0)
  {"NOP", "00000000"},

   // Op code = 1 (0x1)
  {"HALT", "01000000"},

   // Op code = 2 (0x2)
  {"reg=reg*lit", "02000000:rw:ra:lit"},
  {"LA=reg*lit", "02000000:ra:lit"},

   // Op code = 3 (0x3)
  {"reg=EXT(reg)*lit", "03000000:rw:ra:lit"},
  {"LA=EXT(reg)*lit", "03000000:ra:lit"},

   // Op code = 4 (0x4)
  {"reg=EXTI(reg)*lit", "04000000:rw:ra:lit"},
  {"LA=EXTI(reg)*lit", "04000000:ra:lit"},

   // Op code = 5 (0x5)
  {"reg=reg*lit+reg", "05000000:rw:ra:lit:rb"},
  {"reg=reg*lit+LA", "05000000:rw:ra:lit"},
  {"LA=reg*lit+reg", "05000000:ra:lit:rb"},
  {"LA=reg*lit+LA", "05000000:ra:lit"},

   // Op code = 6 (0x6)
  {"reg=reg*lit-reg", "06000000:rw:ra:lit:rb"},
  {"reg=reg*lit-LA", "06000000:rw:ra:lit"},
  {"LA=reg*lit-reg", "06000000:ra:lit:rb"},
  {"LA=reg*lit-LA", "06000000:ra:lit"},

   // Op code = 7 (0x7)
  {"reg=reg*lit&reg", "07000000:rw:ra:lit:rb"},
  {"reg=reg*lit&LA", "07000000:rw:ra:lit"},
  {"LA=reg*lit&reg", "07000000:ra:lit:rb"},
  {"LA=reg*lit&LA", "07000000:ra:lit"},

   // Op code = 8 (0x8)
  {"reg=reg*lit|reg", "08000000:rw:ra:lit:rb"},
  {"reg=reg*lit|LA", "08000000:rw:ra:lit"},
  {"LA=reg*lit|reg", "08000000:ra:lit:rb"},
  {"LA=reg*lit|LA", "08000000:ra:lit"},

   // Op code = 9 (0x9)
  {"reg=reg*lit^reg", "09000000:rw:ra:lit:rb"},
  {"reg=reg*lit^LA", "09000000:rw:ra:lit"},
  {"LA=reg*lit^reg", "09000000:ra:lit:rb"},
  {"LA=reg*lit^LA", "09000000:ra:lit"},

   // Op code = 10 (0xa)
  {"reg=EXT(reg)*lit+reg", "0a000000:rw:ra:lit:rb"},
  {"reg=EXT(reg)*lit+LA", "0a000000:rw:ra:lit"},
  {"LA=EXT(reg)*lit+reg", "0a000000:ra:lit:rb"},
  {"LA=EXT(reg)*lit+LA", "0a000000:ra:lit"},

   // Op code = 11 (0xb)
  {"reg=EXT(reg)*lit-reg", "0b000000:rw:ra:lit:rb"},
  {"reg=EXT(reg)*lit-LA", "0b000000:rw:ra:lit"},
  {"LA=EXT(reg)*lit-reg", "0b000000:ra:lit:rb"},
  {"LA=EXT(reg)*lit-LA", "0b000000:ra:lit"},

   // Op code = 12 (0xc)
  {"reg=EXT(reg)*lit&reg", "0c000000:rw:ra:lit:rb"},
  {"reg=EXT(reg)*lit&LA", "0c000000:rw:ra:lit"},
  {"LA=EXT(reg)*lit&reg", "0c000000:ra:lit:rb"},
  {"LA=EXT(reg)*lit&LA", "0c000000:ra:lit"},

   // Op code = 13 (0xd)
  {"reg=EXT(reg)*lit|reg", "0d000000:rw:ra:lit:rb"},
  {"reg=EXT(reg)*lit|LA", "0d000000:rw:ra:lit"},
  {"LA=EXT(reg)*lit|reg", "0d000000:ra:lit:rb"},
  {"LA=EXT(reg)*lit|LA", "0d000000:ra:lit"},

   // Op code = 14 (0xe)
  {"reg=EXT(reg)*lit^reg", "0e000000:rw:ra:lit:rb"},
  {"reg=EXT(reg)*lit^LA", "0e000000:rw:ra:lit"},
  {"LA=EXT(reg)*lit^reg", "0e000000:ra:lit:rb"},
  {"LA=EXT(reg)*lit^LA", "0e000000:ra:lit"},

   // Op code = 15 (0xf)
  {"reg=EXTI(reg)*lit+reg", "0f000000:rw:ra:lit:rb"},
  {"reg=EXTI(reg)*lit+LA", "0f000000:rw:ra:lit"},
  {"LA=EXTI(reg)*lit+reg", "0f000000:ra:lit:rb"},
  {"LA=EXTI(reg)*lit+LA", "0f000000:ra:lit"},

   // Op code = 16 (0x10)
  {"reg=EXTI(reg)*lit-reg", "10000000:rw:ra:lit:rb"},
  {"reg=EXTI(reg)*lit-LA", "10000000:rw:ra:lit"},
  {"LA=EXTI(reg)*lit-reg", "10000000:ra:lit:rb"},
  {"LA=EXTI(reg)*lit-LA", "10000000:ra:lit"},

   // Op code = 17 (0x11)
  {"reg=EXTI(reg)*lit&reg", "11000000:rw:ra:lit:rb"},
  {"reg=EXTI(reg)*lit&LA", "11000000:rw:ra:lit"},
  {"LA=EXTI(reg)*lit&reg", "11000000:ra:lit:rb"},
  {"LA=EXTI(reg)*lit&LA", "11000000:ra:lit"},

   // Op code = 18 (0x12)
  {"reg=EXTI(reg)*lit|reg", "12000000:rw:ra:lit:rb"},
  {"reg=EXTI(reg)*lit|LA", "12000000:rw:ra:lit"},
  {"LA=EXTI(reg)*lit|reg", "12000000:ra:lit:rb"},
  {"LA=EXTI(reg)*lit|LA", "12000000:ra:lit"},

   // Op code = 19 (0x13)
  {"reg=EXTI(reg)*lit^reg", "13000000:rw:ra:lit:rb"},
  {"reg=EXTI(reg)*lit^LA", "13000000:rw:ra:lit"},
  {"LA=EXTI(reg)*lit^reg", "13000000:ra:lit:rb"},
  {"LA=EXTI(reg)*lit^LA", "13000000:ra:lit"},

   // Op code = 20 (0x14)
  {"reg=reg*reg+lit", "14000000:rw:ra:rb:lit"},
  {"reg=reg+lit", "14000000:rw:ra:lit"},
  {"LA=reg*reg+lit", "14000000:ra:rb:lit"},
  {"LA=reg+lit", "14000000:ra:lit"},

   // Op code = 21 (0x15)
  {"reg=reg*reg&lit", "15000000:rw:ra:rb:lit"},
  {"reg=reg&lit", "15000000:rw:ra:lit"},
  {"LA=reg*reg&lit", "15000000:ra:rb:lit"},
  {"LA=reg&lit", "15000000:ra:lit"},

   // Op code = 22 (0x16)
  {"reg=reg*reg|lit", "16000000:rw:ra:rb:lit"},
  {"reg=reg|lit", "16000000:rw:ra:lit"},
  {"LA=reg*reg|lit", "16000000:ra:rb:lit"},
  {"LA=reg|lit", "16000000:ra:lit"},

   // Op code = 23 (0x17)
  {"reg=reg*reg^lit", "17000000:rw:ra:rb:lit"},
  {"reg=reg^lit", "17000000:rw:ra:lit"},
  {"LA=reg*reg^lit", "17000000:ra:rb:lit"},
  {"LA=reg^lit", "17000000:ra:lit"},

   // Op code = 24 (0x18)
  {"reg=EXT(reg)*reg+lit", "18000000:rw:ra:rb:lit"},
  {"reg=EXT(reg)+lit", "18000000:rw:ra:lit"},
  {"LA=EXT(reg)*reg+lit", "18000000:ra:rb:lit"},
  {"LA=EXT(reg)+lit", "18000000:ra:lit"},

   // Op code = 25 (0x19)
  {"reg=EXT(reg)*reg&lit", "19000000:rw:ra:rb:lit"},
  {"reg=EXT(reg)&lit", "19000000:rw:ra:lit"},
  {"LA=EXT(reg)*reg&lit", "19000000:ra:rb:lit"},
  {"LA=EXT(reg)&lit", "19000000:ra:lit"},

   // Op code = 26 (0x1a)
  {"reg=EXT(reg)*reg|lit", "1a000000:rw:ra:rb:lit"},
  {"reg=EXT(reg)|lit", "1a000000:rw:ra:lit"},
  {"LA=EXT(reg)*reg|lit", "1a000000:ra:rb:lit"},
  {"LA=EXT(reg)|lit", "1a000000:ra:lit"},

   // Op code = 27 (0x1b)
  {"reg=EXT(reg)*reg^lit", "1b000000:rw:ra:rb:lit"},
  {"reg=EXT(reg)^lit", "1b000000:rw:ra:lit"},
  {"LA=EXT(reg)*reg^lit", "1b000000:ra:rb:lit"},
  {"LA=EXT(reg)^lit", "1b000000:ra:lit"},

   // Op code = 28 (0x1c)
  {"reg=EXTI(reg)*reg+lit", "1c000000:rw:ra:rb:lit"},
  {"reg=EXTI(reg)+lit", "1c000000:rw:ra:lit"},
  {"LA=EXTI(reg)*reg+lit", "1c000000:ra:rb:lit"},
  {"LA=EXTI(reg)+lit", "1c000000:ra:lit"},

   // Op code = 29 (0x1d)
  {"reg=EXTI(reg)*reg&lit", "1d000000:rw:ra:rb:lit"},
  {"reg=EXTI(reg)&lit", "1d000000:rw:ra:lit"},
  {"LA=EXTI(reg)*reg&lit", "1d000000:ra:rb:lit"},
  {"LA=EXTI(reg)&lit", "1d000000:ra:lit"},

   // Op code = 30 (0x1e)
  {"reg=EXTI(reg)*reg|lit", "1e000000:rw:ra:rb:lit"},
  {"reg=EXTI(reg)|lit", "1e000000:rw:ra:lit"},
  {"LA=EXTI(reg)*reg|lit", "1e000000:ra:rb:lit"},
  {"LA=EXTI(reg)|lit", "1e000000:ra:lit"},

   // Op code = 31 (0x1f)
  {"reg=EXTI(reg)*reg^lit", "1f000000:rw:ra:rb:lit"},
  {"reg=EXTI(reg)^lit", "1f000000:rw:ra:lit"},
  {"LA=EXTI(reg)*reg^lit", "1f000000:ra:rb:lit"},
  {"LA=EXTI(reg)^lit", "1f000000:ra:lit"},

   // Op code = 32 (0x20)
  {"reg=reg*reg+LA", "200001fe:rw:ra:rb"},
  {"reg=reg+LA", "200001fe:rw:ra"},
  {"reg=reg*reg+LA,reg=MEMSL(areg)", "20000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=reg+LA,reg=MEMSL(areg)", "20000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=reg*reg+LA", "200001fe:ra:rb"},
  {"LA=reg+LA", "200001fe:ra"},
  {"LA=reg*reg+LA,reg=MEMSL(areg)", "20000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg+LA,reg=MEMSL(areg)", "20000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg*reg+LA,lit,lit)", "20000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(reg+LA,lit,lit)", "20000000:rw:ra:iepos:iewid"},

   // Op code = 33 (0x21)
  {"reg=reg*reg-LA", "210001fe:rw:ra:rb"},
  {"reg=reg-LA", "210001fe:rw:ra"},
  {"reg=reg*reg-LA,reg=MEMSL(areg)", "21000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=reg-LA,reg=MEMSL(areg)", "21000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=reg*reg-LA", "210001fe:ra:rb"},
  {"LA=reg-LA", "210001fe:ra"},
  {"LA=reg*reg-LA,reg=MEMSL(areg)", "21000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg-LA,reg=MEMSL(areg)", "21000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg*reg-LA,lit,lit)", "21000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(reg-LA,lit,lit)", "21000000:rw:ra:iepos:iewid"},

   // Op code = 34 (0x22)
  {"reg=reg*reg&LA", "220001fe:rw:ra:rb"},
  {"reg=reg&LA", "220001fe:rw:ra"},
  {"reg=reg*reg&LA,reg=MEMSL(areg)", "22000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=reg&LA,reg=MEMSL(areg)", "22000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=reg*reg&LA", "220001fe:ra:rb"},
  {"LA=reg&LA", "220001fe:ra"},
  {"LA=reg*reg&LA,reg=MEMSL(areg)", "22000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg&LA,reg=MEMSL(areg)", "22000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg*reg&LA,lit,lit)", "22000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(reg&LA,lit,lit)", "22000000:rw:ra:iepos:iewid"},

   // Op code = 35 (0x23)
  {"reg=reg*reg|LA", "230001fe:rw:ra:rb"},
  {"reg=reg|LA", "230001fe:rw:ra"},
  {"reg=reg*reg|LA,reg=MEMSL(areg)", "23000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=reg|LA,reg=MEMSL(areg)", "23000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=reg*reg|LA", "230001fe:ra:rb"},
  {"LA=reg|LA", "230001fe:ra"},
  {"LA=reg*reg|LA,reg=MEMSL(areg)", "23000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg|LA,reg=MEMSL(areg)", "23000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg*reg|LA,lit,lit)", "23000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(reg|LA,lit,lit)", "23000000:rw:ra:iepos:iewid"},

   // Op code = 36 (0x24)
  {"reg=reg*reg^LA", "240001fe:rw:ra:rb"},
  {"reg=reg^LA", "240001fe:rw:ra"},
  {"reg=reg*reg^LA,reg=MEMSL(areg)", "24000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=reg^LA,reg=MEMSL(areg)", "24000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=reg*reg^LA", "240001fe:ra:rb"},
  {"LA=reg^LA", "240001fe:ra"},
  {"LA=reg*reg^LA,reg=MEMSL(areg)", "24000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg^LA,reg=MEMSL(areg)", "24000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg*reg^LA,lit,lit)", "24000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(reg^LA,lit,lit)", "24000000:rw:ra:iepos:iewid"},

   // Op code = 37 (0x25)
  {"reg=EXT(reg)*reg+LA", "250001fe:rw:ra:rb"},
  {"reg=EXT(reg)+LA", "250001fe:rw:ra"},
  {"reg=EXT(reg)*reg+LA,reg=MEMSL(areg)", "25000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXT(reg)+LA,reg=MEMSL(areg)", "25000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*reg+LA", "250001fe:ra:rb"},
  {"LA=EXT(reg)+LA", "250001fe:ra"},
  {"LA=EXT(reg)*reg+LA,reg=MEMSL(areg)", "25000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)+LA,reg=MEMSL(areg)", "25000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*reg+LA,lit,lit)", "25000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXT(reg)+LA,lit,lit)", "25000000:rw:ra:iepos:iewid"},

   // Op code = 38 (0x26)
  {"reg=EXT(reg)*reg-LA", "260001fe:rw:ra:rb"},
  {"reg=EXT(reg)-LA", "260001fe:rw:ra"},
  {"reg=EXT(reg)*reg-LA,reg=MEMSL(areg)", "26000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXT(reg)-LA,reg=MEMSL(areg)", "26000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*reg-LA", "260001fe:ra:rb"},
  {"LA=EXT(reg)-LA", "260001fe:ra"},
  {"LA=EXT(reg)*reg-LA,reg=MEMSL(areg)", "26000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)-LA,reg=MEMSL(areg)", "26000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*reg-LA,lit,lit)", "26000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXT(reg)-LA,lit,lit)", "26000000:rw:ra:iepos:iewid"},

   // Op code = 39 (0x27)
  {"reg=EXT(reg)*reg&LA", "270001fe:rw:ra:rb"},
  {"reg=EXT(reg)&LA", "270001fe:rw:ra"},
  {"reg=EXT(reg)*reg&LA,reg=MEMSL(areg)", "27000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXT(reg)&LA,reg=MEMSL(areg)", "27000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*reg&LA", "270001fe:ra:rb"},
  {"LA=EXT(reg)&LA", "270001fe:ra"},
  {"LA=EXT(reg)*reg&LA,reg=MEMSL(areg)", "27000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)&LA,reg=MEMSL(areg)", "27000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*reg&LA,lit,lit)", "27000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXT(reg)&LA,lit,lit)", "27000000:rw:ra:iepos:iewid"},

   // Op code = 40 (0x28)
  {"reg=EXT(reg)*reg|LA", "280001fe:rw:ra:rb"},
  {"reg=EXT(reg)|LA", "280001fe:rw:ra"},
  {"reg=EXT(reg)*reg|LA,reg=MEMSL(areg)", "28000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXT(reg)|LA,reg=MEMSL(areg)", "28000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*reg|LA", "280001fe:ra:rb"},
  {"LA=EXT(reg)|LA", "280001fe:ra"},
  {"LA=EXT(reg)*reg|LA,reg=MEMSL(areg)", "28000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)|LA,reg=MEMSL(areg)", "28000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*reg|LA,lit,lit)", "28000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXT(reg)|LA,lit,lit)", "28000000:rw:ra:iepos:iewid"},

   // Op code = 41 (0x29)
  {"reg=EXT(reg)*reg^LA", "290001fe:rw:ra:rb"},
  {"reg=EXT(reg)^LA", "290001fe:rw:ra"},
  {"reg=EXT(reg)*reg^LA,reg=MEMSL(areg)", "29000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXT(reg)^LA,reg=MEMSL(areg)", "29000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*reg^LA", "290001fe:ra:rb"},
  {"LA=EXT(reg)^LA", "290001fe:ra"},
  {"LA=EXT(reg)*reg^LA,reg=MEMSL(areg)", "29000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)^LA,reg=MEMSL(areg)", "29000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*reg^LA,lit,lit)", "29000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXT(reg)^LA,lit,lit)", "29000000:rw:ra:iepos:iewid"},

   // Op code = 42 (0x2a)
  {"reg=EXTI(reg)*reg+LA", "2a0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)+LA", "2a0001fe:rw:ra"},
  {"reg=EXTI(reg)*reg+LA,reg=MEMSL(areg)", "2a000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTI(reg)+LA,reg=MEMSL(areg)", "2a000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*reg+LA", "2a0001fe:ra:rb"},
  {"LA=EXTI(reg)+LA", "2a0001fe:ra"},
  {"LA=EXTI(reg)*reg+LA,reg=MEMSL(areg)", "2a000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)+LA,reg=MEMSL(areg)", "2a000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*reg+LA,lit,lit)", "2a000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXTI(reg)+LA,lit,lit)", "2a000000:rw:ra:iepos:iewid"},

   // Op code = 43 (0x2b)
  {"reg=EXTI(reg)*reg-LA", "2b0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)-LA", "2b0001fe:rw:ra"},
  {"reg=EXTI(reg)*reg-LA,reg=MEMSL(areg)", "2b000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTI(reg)-LA,reg=MEMSL(areg)", "2b000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*reg-LA", "2b0001fe:ra:rb"},
  {"LA=EXTI(reg)-LA", "2b0001fe:ra"},
  {"LA=EXTI(reg)*reg-LA,reg=MEMSL(areg)", "2b000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)-LA,reg=MEMSL(areg)", "2b000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*reg-LA,lit,lit)", "2b000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXTI(reg)-LA,lit,lit)", "2b000000:rw:ra:iepos:iewid"},

   // Op code = 44 (0x2c)
  {"reg=EXTI(reg)*reg&LA", "2c0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)&LA", "2c0001fe:rw:ra"},
  {"reg=EXTI(reg)*reg&LA,reg=MEMSL(areg)", "2c000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTI(reg)&LA,reg=MEMSL(areg)", "2c000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*reg&LA", "2c0001fe:ra:rb"},
  {"LA=EXTI(reg)&LA", "2c0001fe:ra"},
  {"LA=EXTI(reg)*reg&LA,reg=MEMSL(areg)", "2c000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)&LA,reg=MEMSL(areg)", "2c000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*reg&LA,lit,lit)", "2c000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXTI(reg)&LA,lit,lit)", "2c000000:rw:ra:iepos:iewid"},

   // Op code = 45 (0x2d)
  {"reg=EXTI(reg)*reg|LA", "2d0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)|LA", "2d0001fe:rw:ra"},
  {"reg=EXTI(reg)*reg|LA,reg=MEMSL(areg)", "2d000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTI(reg)|LA,reg=MEMSL(areg)", "2d000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*reg|LA", "2d0001fe:ra:rb"},
  {"LA=EXTI(reg)|LA", "2d0001fe:ra"},
  {"LA=EXTI(reg)*reg|LA,reg=MEMSL(areg)", "2d000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)|LA,reg=MEMSL(areg)", "2d000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*reg|LA,lit,lit)", "2d000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXTI(reg)|LA,lit,lit)", "2d000000:rw:ra:iepos:iewid"},

   // Op code = 46 (0x2e)
  {"reg=EXTI(reg)*reg^LA", "2e0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)^LA", "2e0001fe:rw:ra"},
  {"reg=EXTI(reg)*reg^LA,reg=MEMSL(areg)", "2e000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTI(reg)^LA,reg=MEMSL(areg)", "2e000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*reg^LA", "2e0001fe:ra:rb"},
  {"LA=EXTI(reg)^LA", "2e0001fe:ra"},
  {"LA=EXTI(reg)*reg^LA,reg=MEMSL(areg)", "2e000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)^LA,reg=MEMSL(areg)", "2e000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*reg^LA,lit,lit)", "2e000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXTI(reg)^LA,lit,lit)", "2e000000:rw:ra:iepos:iewid"},

   // Op code = 47 (0x2f)
  {"reg=EXTL(reg,lit,lit)*reg+LA", "2f000000:rw:ra:epos:ewid:rb"},
  {"reg=EXTL(reg,lit,lit)+LA", "2f000000:rw:ra:epos:ewid"},
  {"LA=EXTL(reg,lit,lit)*reg+LA", "2f000000:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)+LA", "2f000000:ra:epos:ewid"},
  {"reg=(reg<<lit)*reg+LA", "2f0001e0:rw:ra:epos:rb"},
  {"reg=(reg<<lit)+LA", "2f0001e0:rw:ra:epos"},
  {"LA=(reg<<lit)*reg+LA", "2f0001e0:ra:epos:rb"},
  {"LA=(reg<<lit)+LA", "2f0001e0:ra:epos"},

   // Op code = 48 (0x30)
  {"reg=EXTL(reg,lit,lit)*reg-LA", "30000000:rw:ra:epos:ewid:rb"},
  {"reg=EXTL(reg,lit,lit)-LA", "30000000:rw:ra:epos:ewid"},
  {"LA=EXTL(reg,lit,lit)*reg-LA", "30000000:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)-LA", "30000000:ra:epos:ewid"},
  {"reg=(reg<<lit)*reg-LA", "300001e0:rw:ra:epos:rb"},
  {"reg=(reg<<lit)-LA", "300001e0:rw:ra:epos"},
  {"LA=(reg<<lit)*reg-LA", "300001e0:ra:epos:rb"},
  {"LA=(reg<<lit)-LA", "300001e0:ra:epos"},

   // Op code = 49 (0x31)
  {"reg=EXTL(reg,lit,lit)*reg&LA", "31000000:rw:ra:epos:ewid:rb"},
  {"reg=EXTL(reg,lit,lit)&LA", "31000000:rw:ra:epos:ewid"},
  {"LA=EXTL(reg,lit,lit)*reg&LA", "31000000:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)&LA", "31000000:ra:epos:ewid"},
  {"reg=(reg<<lit)*reg&LA", "310001e0:rw:ra:epos:rb"},
  {"reg=(reg<<lit)&LA", "310001e0:rw:ra:epos"},
  {"LA=(reg<<lit)*reg&LA", "310001e0:ra:epos:rb"},
  {"LA=(reg<<lit)&LA", "310001e0:ra:epos"},

   // Op code = 50 (0x32)
  {"reg=EXTL(reg,lit,lit)*reg|LA", "32000000:rw:ra:epos:ewid:rb"},
  {"reg=EXTL(reg,lit,lit)|LA", "32000000:rw:ra:epos:ewid"},
  {"LA=EXTL(reg,lit,lit)*reg|LA", "32000000:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)|LA", "32000000:ra:epos:ewid"},
  {"reg=(reg<<lit)*reg|LA", "320001e0:rw:ra:epos:rb"},
  {"reg=(reg<<lit)|LA", "320001e0:rw:ra:epos"},
  {"LA=(reg<<lit)*reg|LA", "320001e0:ra:epos:rb"},
  {"LA=(reg<<lit)|LA", "320001e0:ra:epos"},

   // Op code = 51 (0x33)
  {"reg=EXTL(reg,lit,lit)*reg^LA", "33000000:rw:ra:epos:ewid:rb"},
  {"reg=EXTL(reg,lit,lit)^LA", "33000000:rw:ra:epos:ewid"},
  {"LA=EXTL(reg,lit,lit)*reg^LA", "33000000:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)^LA", "33000000:ra:epos:ewid"},
  {"reg=(reg<<lit)*reg^LA", "330001e0:rw:ra:epos:rb"},
  {"reg=(reg<<lit)^LA", "330001e0:rw:ra:epos"},
  {"LA=(reg<<lit)*reg^LA", "330001e0:ra:epos:rb"},
  {"LA=(reg<<lit)^LA", "330001e0:ra:epos"},

   // Op code = 52 (0x34)
  {"reg=reg*reg", "340001fe:rw:ra:rb"},
  {"reg=reg", "340001fe:rw:ra"},
  {"reg=reg*reg,reg=MEMSL(areg)", "34000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=reg,reg=MEMSL(areg)", "34000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=reg*reg", "340001fe:ra:rb"},
  {"LA=reg", "340001fe:ra"},
  {"LA=reg*reg,reg=MEMSL(areg)", "34000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg,reg=MEMSL(areg)", "34000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg*reg,lit,lit)", "34000000:rw:ra:rb:iepos:iewid"},

   // Op code = 53 (0x35)
  {"reg=EXT(reg)*reg", "350001fe:rw:ra:rb"},
  {"reg=EXT(reg)", "350001fe:rw:ra"},
  {"reg=EXT(reg)*reg,reg=MEMSL(areg)", "35000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXT(reg),reg=MEMSL(areg)", "35000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*reg", "350001fe:ra:rb"},
  {"LA=EXT(reg)", "350001fe:ra"},
  {"LA=EXT(reg)*reg,reg=MEMSL(areg)", "35000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg),reg=MEMSL(areg)", "35000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*reg,lit,lit)", "35000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXT(reg),lit,lit)", "35000000:rw:ra:iepos:iewid"},

   // Op code = 54 (0x36)
  {"reg=EXTI(reg)*reg", "360001fe:rw:ra:rb"},
  {"reg=EXTI(reg)", "360001fe:rw:ra"},
  {"reg=EXTI(reg)*reg,reg=MEMSL(areg)", "36000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTI(reg),reg=MEMSL(areg)", "36000001:rw:ra:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*reg", "360001fe:ra:rb"},
  {"LA=EXTI(reg)", "360001fe:ra"},
  {"LA=EXTI(reg)*reg,reg=MEMSL(areg)", "36000001:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg),reg=MEMSL(areg)", "36000001:ra:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*reg,lit,lit)", "36000000:rw:ra:rb:iepos:iewid"},
  {"reg=EXTL(EXTI(reg),lit,lit)", "36000000:rw:ra:iepos:iewid"},

   // Op code = 55 (0x37)
  {"reg=EXTL(reg,lit,lit)*reg", "37000000:rw:ra:epos:ewid:rb"},
  {"reg=EXTL(reg,lit,lit)", "37000000:rw:ra:epos:ewid"},
  {"LA=EXTL(reg,lit,lit)*reg", "37000000:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)", "37000000:ra:epos:ewid"},
  {"reg=(reg<<lit)*reg", "370001e0:rw:ra:epos:rb"},
  {"reg=(reg<<lit)", "370001e0:rw:ra:epos"},
  {"LA=(reg<<lit)*reg", "370001e0:ra:epos:rb"},
  {"LA=(reg<<lit)", "370001e0:ra:epos"},

   // Op code = 56 (0x38)
  {"reg=reg+reg", "380001fe:rw:ra:rb"},
  {"reg=reg+reg,reg=MEMSL(areg)", "38000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg+reg", "380001fe:ra:rb"},
  {"LA=reg+reg,reg=MEMSL(areg)", "38000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg+reg,lit,lit)", "38000000:rw:ra:rb:iepos:iewid"},

   // Op code = 57 (0x39)
  {"reg=reg-reg", "390001fe:rw:ra:rb"},
  {"reg=reg-reg,reg=MEMSL(areg)", "39000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg-reg", "390001fe:ra:rb"},
  {"LA=reg-reg,reg=MEMSL(areg)", "39000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg-reg,lit,lit)", "39000000:rw:ra:rb:iepos:iewid"},

   // Op code = 58 (0x3a)
  {"reg=reg&reg", "3a0001fe:rw:ra:rb"},
  {"reg=reg&reg,reg=MEMSL(areg)", "3a000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg&reg", "3a0001fe:ra:rb"},
  {"LA=reg&reg,reg=MEMSL(areg)", "3a000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg&reg,lit,lit)", "3a000000:rw:ra:rb:iepos:iewid"},

   // Op code = 59 (0x3b)
  {"reg=reg|reg", "3b0001fe:rw:ra:rb"},
  {"reg=reg|reg,reg=MEMSL(areg)", "3b000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg|reg", "3b0001fe:ra:rb"},
  {"LA=reg|reg,reg=MEMSL(areg)", "3b000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg|reg,lit,lit)", "3b000000:rw:ra:rb:iepos:iewid"},

   // Op code = 60 (0x3c)
  {"reg=reg^reg", "3c0001fe:rw:ra:rb"},
  {"reg=reg^reg,reg=MEMSL(areg)", "3c000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=reg^reg", "3c0001fe:ra:rb"},
  {"LA=reg^reg,reg=MEMSL(areg)", "3c000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(reg^reg,lit,lit)", "3c000000:rw:ra:rb:iepos:iewid"},

   // Op code = 61 (0x3d)
  {"reg=EXT(reg)+reg", "3d0001fe:rw:ra:rb"},
  {"reg=EXT(reg)+reg,reg=MEMSL(areg)", "3d000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)+reg", "3d0001fe:ra:rb"},
  {"LA=EXT(reg)+reg,reg=MEMSL(areg)", "3d000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)+reg,lit,lit)", "3d000000:rw:ra:rb:iepos:iewid"},

   // Op code = 62 (0x3e)
  {"reg=EXT(reg)-reg", "3e0001fe:rw:ra:rb"},
  {"reg=EXT(reg)-reg,reg=MEMSL(areg)", "3e000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)-reg", "3e0001fe:ra:rb"},
  {"LA=EXT(reg)-reg,reg=MEMSL(areg)", "3e000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)-reg,lit,lit)", "3e000000:rw:ra:rb:iepos:iewid"},

   // Op code = 63 (0x3f)
  {"reg=EXT(reg)&reg", "3f0001fe:rw:ra:rb"},
  {"reg=EXT(reg)&reg,reg=MEMSL(areg)", "3f000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)&reg", "3f0001fe:ra:rb"},
  {"LA=EXT(reg)&reg,reg=MEMSL(areg)", "3f000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)&reg,lit,lit)", "3f000000:rw:ra:rb:iepos:iewid"},

   // Op code = 64 (0x40)
  {"reg=EXT(reg)|reg", "400001fe:rw:ra:rb"},
  {"reg=EXT(reg)|reg,reg=MEMSL(areg)", "40000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)|reg", "400001fe:ra:rb"},
  {"LA=EXT(reg)|reg,reg=MEMSL(areg)", "40000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)|reg,lit,lit)", "40000000:rw:ra:rb:iepos:iewid"},

   // Op code = 65 (0x41)
  {"reg=EXT(reg)^reg", "410001fe:rw:ra:rb"},
  {"reg=EXT(reg)^reg,reg=MEMSL(areg)", "41000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)^reg", "410001fe:ra:rb"},
  {"LA=EXT(reg)^reg,reg=MEMSL(areg)", "41000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)^reg,lit,lit)", "41000000:rw:ra:rb:iepos:iewid"},

   // Op code = 66 (0x42)
  {"reg=EXTI(reg)+reg", "420001fe:rw:ra:rb"},
  {"reg=EXTI(reg)+reg,reg=MEMSL(areg)", "42000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)+reg", "420001fe:ra:rb"},
  {"LA=EXTI(reg)+reg,reg=MEMSL(areg)", "42000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)+reg,lit,lit)", "42000000:rw:ra:rb:iepos:iewid"},

   // Op code = 67 (0x43)
  {"reg=EXTI(reg)-reg", "430001fe:rw:ra:rb"},
  {"reg=EXTI(reg)-reg,reg=MEMSL(areg)", "43000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)-reg", "430001fe:ra:rb"},
  {"LA=EXTI(reg)-reg,reg=MEMSL(areg)", "43000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)-reg,lit,lit)", "43000000:rw:ra:rb:iepos:iewid"},

   // Op code = 68 (0x44)
  {"reg=EXTI(reg)&reg", "440001fe:rw:ra:rb"},
  {"reg=EXTI(reg)&reg,reg=MEMSL(areg)", "44000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)&reg", "440001fe:ra:rb"},
  {"LA=EXTI(reg)&reg,reg=MEMSL(areg)", "44000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)&reg,lit,lit)", "44000000:rw:ra:rb:iepos:iewid"},

   // Op code = 69 (0x45)
  {"reg=EXTI(reg)|reg", "450001fe:rw:ra:rb"},
  {"reg=EXTI(reg)|reg,reg=MEMSL(areg)", "45000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)|reg", "450001fe:ra:rb"},
  {"LA=EXTI(reg)|reg,reg=MEMSL(areg)", "45000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)|reg,lit,lit)", "45000000:rw:ra:rb:iepos:iewid"},

   // Op code = 70 (0x46)
  {"reg=EXTI(reg)^reg", "460001fe:rw:ra:rb"},
  {"reg=EXTI(reg)^reg,reg=MEMSL(areg)", "46000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)^reg", "460001fe:ra:rb"},
  {"LA=EXTI(reg)^reg,reg=MEMSL(areg)", "46000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)^reg,lit,lit)", "46000000:rw:ra:rb:iepos:iewid"},

   // Op code = 71 (0x47)
  {"reg=EXTL(reg,lit,lit)+reg", "47000000:rw:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)+reg", "47000000:ra:epos:ewid:rb"},
  {"reg=(reg<<lit)+reg", "470001e0:rw:ra:epos:rb"},
  {"LA=(reg<<lit)+reg", "470001e0:ra:epos:rb"},

   // Op code = 72 (0x48)
  {"reg=EXTL(reg,lit,lit)-reg", "48000000:rw:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)-reg", "48000000:ra:epos:ewid:rb"},
  {"reg=(reg<<lit)-reg", "480001e0:rw:ra:epos:rb"},
  {"LA=(reg<<lit)-reg", "480001e0:ra:epos:rb"},

   // Op code = 73 (0x49)
  {"reg=EXTL(reg,lit,lit)&reg", "49000000:rw:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)&reg", "49000000:ra:epos:ewid:rb"},
  {"reg=(reg<<lit)&reg", "490001e0:rw:ra:epos:rb"},
  {"LA=(reg<<lit)&reg", "490001e0:ra:epos:rb"},

   // Op code = 74 (0x4a)
  {"reg=EXTL(reg,lit,lit)|reg", "4a000000:rw:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)|reg", "4a000000:ra:epos:ewid:rb"},
  {"reg=(reg<<lit)|reg", "4a0001e0:rw:ra:epos:rb"},
  {"LA=(reg<<lit)|reg", "4a0001e0:ra:epos:rb"},

   // Op code = 75 (0x4b)
  {"reg=EXTL(reg,lit,lit)^reg", "4b000000:rw:ra:epos:ewid:rb"},
  {"LA=EXTL(reg,lit,lit)^reg", "4b000000:ra:epos:ewid:rb"},
  {"reg=(reg<<lit)^reg", "4b0001e0:rw:ra:epos:rb"},
  {"LA=(reg<<lit)^reg", "4b0001e0:ra:epos:rb"},

   // Op code = 76 (0x4c)
  {"reg=lit-reg", "4c000000:rw:lit:ra"},
  {"LA=lit-reg", "4c000000:lit:ra"},

   // Op code = 77 (0x4d)
  {"reg=lit-EXT(reg)", "4d000000:rw:lit:ra"},
  {"LA=lit-EXT(reg)", "4d000000:lit:ra"},

   // Op code = 78 (0x4e)
  {"reg=lit-EXTI(reg)", "4e000000:rw:lit:ra"},
  {"LA=lit-EXTI(reg)", "4e000000:lit:ra"},

   // Op code = 79 (0x4f)
  {"reg=INS(reg*lit,LI)", "4f000000:rw:ra:lit"},
  {"INS(LA=reg*lit,LI)", "4f000000:ra:lit"},

   // Op code = 80 (0x50)
  {"reg=INS(EXT(reg)*lit,LI)", "50000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*lit,LI)", "50000000:ra:lit"},

   // Op code = 81 (0x51)
  {"reg=INS(EXTI(reg)*lit,LI)", "51000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*lit,LI)", "51000000:ra:lit"},

   // Op code = 82 (0x52)
  {"reg=INS(reg*lit+reg,LI)", "52000000:rw:ra:lit:rb"},
  {"reg=INS(reg*lit+LA,LI)", "52000000:rw:ra:lit"},
  {"INS(LA=reg*lit+reg,LI)", "52000000:ra:lit:rb"},
  {"INS(LA=reg*lit+LA,LI)", "52000000:ra:lit"},

   // Op code = 83 (0x53)
  {"reg=INS(reg*lit-reg,LI)", "53000000:rw:ra:lit:rb"},
  {"reg=INS(reg*lit-LA,LI)", "53000000:rw:ra:lit"},
  {"INS(LA=reg*lit-reg,LI)", "53000000:ra:lit:rb"},
  {"INS(LA=reg*lit-LA,LI)", "53000000:ra:lit"},

   // Op code = 84 (0x54)
  {"reg=INS(reg*lit&reg,LI)", "54000000:rw:ra:lit:rb"},
  {"reg=INS(reg*lit&LA,LI)", "54000000:rw:ra:lit"},
  {"INS(LA=reg*lit&reg,LI)", "54000000:ra:lit:rb"},
  {"INS(LA=reg*lit&LA,LI)", "54000000:ra:lit"},

   // Op code = 85 (0x55)
  {"reg=INS(reg*lit|reg,LI)", "55000000:rw:ra:lit:rb"},
  {"reg=INS(reg*lit|LA,LI)", "55000000:rw:ra:lit"},
  {"INS(LA=reg*lit|reg,LI)", "55000000:ra:lit:rb"},
  {"INS(LA=reg*lit|LA,LI)", "55000000:ra:lit"},

   // Op code = 86 (0x56)
  {"reg=INS(reg*lit^reg,LI)", "56000000:rw:ra:lit:rb"},
  {"reg=INS(reg*lit^LA,LI)", "56000000:rw:ra:lit"},
  {"INS(LA=reg*lit^reg,LI)", "56000000:ra:lit:rb"},
  {"INS(LA=reg*lit^LA,LI)", "56000000:ra:lit"},

   // Op code = 87 (0x57)
  {"reg=INS(EXT(reg)*lit+reg,LI)", "57000000:rw:ra:lit:rb"},
  {"reg=INS(EXT(reg)*lit+LA,LI)", "57000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*lit+reg,LI)", "57000000:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit+LA,LI)", "57000000:ra:lit"},

   // Op code = 88 (0x58)
  {"reg=INS(EXT(reg)*lit-reg,LI)", "58000000:rw:ra:lit:rb"},
  {"reg=INS(EXT(reg)*lit-LA,LI)", "58000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*lit-reg,LI)", "58000000:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit-LA,LI)", "58000000:ra:lit"},

   // Op code = 89 (0x59)
  {"reg=INS(EXT(reg)*lit&reg,LI)", "59000000:rw:ra:lit:rb"},
  {"reg=INS(EXT(reg)*lit&LA,LI)", "59000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*lit&reg,LI)", "59000000:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit&LA,LI)", "59000000:ra:lit"},

   // Op code = 90 (0x5a)
  {"reg=INS(EXT(reg)*lit|reg,LI)", "5a000000:rw:ra:lit:rb"},
  {"reg=INS(EXT(reg)*lit|LA,LI)", "5a000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*lit|reg,LI)", "5a000000:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit|LA,LI)", "5a000000:ra:lit"},

   // Op code = 91 (0x5b)
  {"reg=INS(EXT(reg)*lit^reg,LI)", "5b000000:rw:ra:lit:rb"},
  {"reg=INS(EXT(reg)*lit^LA,LI)", "5b000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*lit^reg,LI)", "5b000000:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit^LA,LI)", "5b000000:ra:lit"},

   // Op code = 92 (0x5c)
  {"reg=INS(EXTI(reg)*lit+reg,LI)", "5c000000:rw:ra:lit:rb"},
  {"reg=INS(EXTI(reg)*lit+LA,LI)", "5c000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*lit+reg,LI)", "5c000000:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit+LA,LI)", "5c000000:ra:lit"},

   // Op code = 93 (0x5d)
  {"reg=INS(EXTI(reg)*lit-reg,LI)", "5d000000:rw:ra:lit:rb"},
  {"reg=INS(EXTI(reg)*lit-LA,LI)", "5d000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*lit-reg,LI)", "5d000000:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit-LA,LI)", "5d000000:ra:lit"},

   // Op code = 94 (0x5e)
  {"reg=INS(EXTI(reg)*lit&reg,LI)", "5e000000:rw:ra:lit:rb"},
  {"reg=INS(EXTI(reg)*lit&LA,LI)", "5e000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*lit&reg,LI)", "5e000000:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit&LA,LI)", "5e000000:ra:lit"},

   // Op code = 95 (0x5f)
  {"reg=INS(EXTI(reg)*lit|reg,LI)", "5f000000:rw:ra:lit:rb"},
  {"reg=INS(EXTI(reg)*lit|LA,LI)", "5f000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*lit|reg,LI)", "5f000000:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit|LA,LI)", "5f000000:ra:lit"},

   // Op code = 96 (0x60)
  {"reg=INS(EXTI(reg)*lit^reg,LI)", "60000000:rw:ra:lit:rb"},
  {"reg=INS(EXTI(reg)*lit^LA,LI)", "60000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*lit^reg,LI)", "60000000:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit^LA,LI)", "60000000:ra:lit"},

   // Op code = 97 (0x61)
  {"reg=INS(reg*reg+lit,LI)", "61000000:rw:ra:rb:lit"},
  {"reg=INS(reg+lit,LI)", "61000000:rw:ra:lit"},
  {"INS(LA=reg*reg+lit,LI)", "61000000:ra:rb:lit"},
  {"INS(LA=reg+lit,LI)", "61000000:ra:lit"},

   // Op code = 98 (0x62)
  {"reg=INS(reg*reg&lit,LI)", "62000000:rw:ra:rb:lit"},
  {"reg=INS(reg&lit,LI)", "62000000:rw:ra:lit"},
  {"INS(LA=reg*reg&lit,LI)", "62000000:ra:rb:lit"},
  {"INS(LA=reg&lit,LI)", "62000000:ra:lit"},

   // Op code = 99 (0x63)
  {"reg=INS(reg*reg|lit,LI)", "63000000:rw:ra:rb:lit"},
  {"reg=INS(reg|lit,LI)", "63000000:rw:ra:lit"},
  {"INS(LA=reg*reg|lit,LI)", "63000000:ra:rb:lit"},
  {"INS(LA=reg|lit,LI)", "63000000:ra:lit"},

   // Op code = 100 (0x64)
  {"reg=INS(reg*reg^lit,LI)", "64000000:rw:ra:rb:lit"},
  {"reg=INS(reg^lit,LI)", "64000000:rw:ra:lit"},
  {"INS(LA=reg*reg^lit,LI)", "64000000:ra:rb:lit"},
  {"INS(LA=reg^lit,LI)", "64000000:ra:lit"},

   // Op code = 101 (0x65)
  {"reg=INS(EXT(reg)*reg+lit,LI)", "65000000:rw:ra:rb:lit"},
  {"reg=INS(EXT(reg)+lit,LI)", "65000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*reg+lit,LI)", "65000000:ra:rb:lit"},
  {"INS(LA=EXT(reg)+lit,LI)", "65000000:ra:lit"},

   // Op code = 102 (0x66)
  {"reg=INS(EXT(reg)*reg&lit,LI)", "66000000:rw:ra:rb:lit"},
  {"reg=INS(EXT(reg)&lit,LI)", "66000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*reg&lit,LI)", "66000000:ra:rb:lit"},
  {"INS(LA=EXT(reg)&lit,LI)", "66000000:ra:lit"},

   // Op code = 103 (0x67)
  {"reg=INS(EXT(reg)*reg|lit,LI)", "67000000:rw:ra:rb:lit"},
  {"reg=INS(EXT(reg)|lit,LI)", "67000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*reg|lit,LI)", "67000000:ra:rb:lit"},
  {"INS(LA=EXT(reg)|lit,LI)", "67000000:ra:lit"},

   // Op code = 104 (0x68)
  {"reg=INS(EXT(reg)*reg^lit,LI)", "68000000:rw:ra:rb:lit"},
  {"reg=INS(EXT(reg)^lit,LI)", "68000000:rw:ra:lit"},
  {"INS(LA=EXT(reg)*reg^lit,LI)", "68000000:ra:rb:lit"},
  {"INS(LA=EXT(reg)^lit,LI)", "68000000:ra:lit"},

   // Op code = 105 (0x69)
  {"reg=INS(EXTI(reg)*reg+lit,LI)", "69000000:rw:ra:rb:lit"},
  {"reg=INS(EXTI(reg)+lit,LI)", "69000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*reg+lit,LI)", "69000000:ra:rb:lit"},
  {"INS(LA=EXTI(reg)+lit,LI)", "69000000:ra:lit"},

   // Op code = 106 (0x6a)
  {"reg=INS(EXTI(reg)*reg&lit,LI)", "6a000000:rw:ra:rb:lit"},
  {"reg=INS(EXTI(reg)&lit,LI)", "6a000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*reg&lit,LI)", "6a000000:ra:rb:lit"},
  {"INS(LA=EXTI(reg)&lit,LI)", "6a000000:ra:lit"},

   // Op code = 107 (0x6b)
  {"reg=INS(EXTI(reg)*reg|lit,LI)", "6b000000:rw:ra:rb:lit"},
  {"reg=INS(EXTI(reg)|lit,LI)", "6b000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*reg|lit,LI)", "6b000000:ra:rb:lit"},
  {"INS(LA=EXTI(reg)|lit,LI)", "6b000000:ra:lit"},

   // Op code = 108 (0x6c)
  {"reg=INS(EXTI(reg)*reg^lit,LI)", "6c000000:rw:ra:rb:lit"},
  {"reg=INS(EXTI(reg)^lit,LI)", "6c000000:rw:ra:lit"},
  {"INS(LA=EXTI(reg)*reg^lit,LI)", "6c000000:ra:rb:lit"},
  {"INS(LA=EXTI(reg)^lit,LI)", "6c000000:ra:lit"},

   // Op code = 109 (0x6d)
  {"reg=INS(reg*reg+LA,LI)", "6d000000:rw:ra:rb"},
  {"reg=INS(reg+LA,LI)", "6d000000:rw:ra"},
  {"reg=INS(reg*reg+LA,LI),reg=MEMSL(areg)", "6d000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(reg+LA,LI),reg=MEMSL(areg)", "6d000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=reg*reg+LA,LI)", "6d000000:ra:rb"},
  {"INS(LA=reg+LA,LI)", "6d000000:ra"},
  {"INS(LA=reg*reg+LA,LI),reg=MEMSL(areg)", "6d000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg+LA,LI),reg=MEMSL(areg)", "6d000001:ra:pmem_rm:pmem_ar"},

   // Op code = 110 (0x6e)
  {"reg=INS(reg*reg-LA,LI)", "6e000000:rw:ra:rb"},
  {"reg=INS(reg-LA,LI)", "6e000000:rw:ra"},
  {"reg=INS(reg*reg-LA,LI),reg=MEMSL(areg)", "6e000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(reg-LA,LI),reg=MEMSL(areg)", "6e000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=reg*reg-LA,LI)", "6e000000:ra:rb"},
  {"INS(LA=reg-LA,LI)", "6e000000:ra"},
  {"INS(LA=reg*reg-LA,LI),reg=MEMSL(areg)", "6e000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg-LA,LI),reg=MEMSL(areg)", "6e000001:ra:pmem_rm:pmem_ar"},

   // Op code = 111 (0x6f)
  {"reg=INS(reg*reg&LA,LI)", "6f000000:rw:ra:rb"},
  {"reg=INS(reg&LA,LI)", "6f000000:rw:ra"},
  {"reg=INS(reg*reg&LA,LI),reg=MEMSL(areg)", "6f000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(reg&LA,LI),reg=MEMSL(areg)", "6f000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=reg*reg&LA,LI)", "6f000000:ra:rb"},
  {"INS(LA=reg&LA,LI)", "6f000000:ra"},
  {"INS(LA=reg*reg&LA,LI),reg=MEMSL(areg)", "6f000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg&LA,LI),reg=MEMSL(areg)", "6f000001:ra:pmem_rm:pmem_ar"},

   // Op code = 112 (0x70)
  {"reg=INS(reg*reg|LA,LI)", "70000000:rw:ra:rb"},
  {"reg=INS(reg|LA,LI)", "70000000:rw:ra"},
  {"reg=INS(reg*reg|LA,LI),reg=MEMSL(areg)", "70000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(reg|LA,LI),reg=MEMSL(areg)", "70000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=reg*reg|LA,LI)", "70000000:ra:rb"},
  {"INS(LA=reg|LA,LI)", "70000000:ra"},
  {"INS(LA=reg*reg|LA,LI),reg=MEMSL(areg)", "70000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg|LA,LI),reg=MEMSL(areg)", "70000001:ra:pmem_rm:pmem_ar"},

   // Op code = 113 (0x71)
  {"reg=INS(reg*reg^LA,LI)", "71000000:rw:ra:rb"},
  {"reg=INS(reg^LA,LI)", "71000000:rw:ra"},
  {"reg=INS(reg*reg^LA,LI),reg=MEMSL(areg)", "71000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(reg^LA,LI),reg=MEMSL(areg)", "71000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=reg*reg^LA,LI)", "71000000:ra:rb"},
  {"INS(LA=reg^LA,LI)", "71000000:ra"},
  {"INS(LA=reg*reg^LA,LI),reg=MEMSL(areg)", "71000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg^LA,LI),reg=MEMSL(areg)", "71000001:ra:pmem_rm:pmem_ar"},

   // Op code = 114 (0x72)
  {"reg=INS(EXT(reg)*reg+LA,LI)", "72000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)+LA,LI)", "72000000:rw:ra"},
  {"reg=INS(EXT(reg)*reg+LA,LI),reg=MEMSL(areg)", "72000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXT(reg)+LA,LI),reg=MEMSL(areg)", "72000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*reg+LA,LI)", "72000000:ra:rb"},
  {"INS(LA=EXT(reg)+LA,LI)", "72000000:ra"},
  {"INS(LA=EXT(reg)*reg+LA,LI),reg=MEMSL(areg)", "72000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)+LA,LI),reg=MEMSL(areg)", "72000001:ra:pmem_rm:pmem_ar"},

   // Op code = 115 (0x73)
  {"reg=INS(EXT(reg)*reg-LA,LI)", "73000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)-LA,LI)", "73000000:rw:ra"},
  {"reg=INS(EXT(reg)*reg-LA,LI),reg=MEMSL(areg)", "73000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXT(reg)-LA,LI),reg=MEMSL(areg)", "73000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*reg-LA,LI)", "73000000:ra:rb"},
  {"INS(LA=EXT(reg)-LA,LI)", "73000000:ra"},
  {"INS(LA=EXT(reg)*reg-LA,LI),reg=MEMSL(areg)", "73000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)-LA,LI),reg=MEMSL(areg)", "73000001:ra:pmem_rm:pmem_ar"},

   // Op code = 116 (0x74)
  {"reg=INS(EXT(reg)*reg&LA,LI)", "74000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)&LA,LI)", "74000000:rw:ra"},
  {"reg=INS(EXT(reg)*reg&LA,LI),reg=MEMSL(areg)", "74000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXT(reg)&LA,LI),reg=MEMSL(areg)", "74000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*reg&LA,LI)", "74000000:ra:rb"},
  {"INS(LA=EXT(reg)&LA,LI)", "74000000:ra"},
  {"INS(LA=EXT(reg)*reg&LA,LI),reg=MEMSL(areg)", "74000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)&LA,LI),reg=MEMSL(areg)", "74000001:ra:pmem_rm:pmem_ar"},

   // Op code = 117 (0x75)
  {"reg=INS(EXT(reg)*reg|LA,LI)", "75000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)|LA,LI)", "75000000:rw:ra"},
  {"reg=INS(EXT(reg)*reg|LA,LI),reg=MEMSL(areg)", "75000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXT(reg)|LA,LI),reg=MEMSL(areg)", "75000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*reg|LA,LI)", "75000000:ra:rb"},
  {"INS(LA=EXT(reg)|LA,LI)", "75000000:ra"},
  {"INS(LA=EXT(reg)*reg|LA,LI),reg=MEMSL(areg)", "75000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)|LA,LI),reg=MEMSL(areg)", "75000001:ra:pmem_rm:pmem_ar"},

   // Op code = 118 (0x76)
  {"reg=INS(EXT(reg)*reg^LA,LI)", "76000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)^LA,LI)", "76000000:rw:ra"},
  {"reg=INS(EXT(reg)*reg^LA,LI),reg=MEMSL(areg)", "76000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXT(reg)^LA,LI),reg=MEMSL(areg)", "76000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*reg^LA,LI)", "76000000:ra:rb"},
  {"INS(LA=EXT(reg)^LA,LI)", "76000000:ra"},
  {"INS(LA=EXT(reg)*reg^LA,LI),reg=MEMSL(areg)", "76000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)^LA,LI),reg=MEMSL(areg)", "76000001:ra:pmem_rm:pmem_ar"},

   // Op code = 119 (0x77)
  {"reg=INS(EXTI(reg)*reg+LA,LI)", "77000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)+LA,LI)", "77000000:rw:ra"},
  {"reg=INS(EXTI(reg)*reg+LA,LI),reg=MEMSL(areg)", "77000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXTI(reg)+LA,LI),reg=MEMSL(areg)", "77000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*reg+LA,LI)", "77000000:ra:rb"},
  {"INS(LA=EXTI(reg)+LA,LI)", "77000000:ra"},
  {"INS(LA=EXTI(reg)*reg+LA,LI),reg=MEMSL(areg)", "77000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)+LA,LI),reg=MEMSL(areg)", "77000001:ra:pmem_rm:pmem_ar"},

   // Op code = 120 (0x78)
  {"reg=INS(EXTI(reg)*reg-LA,LI)", "78000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)-LA,LI)", "78000000:rw:ra"},
  {"reg=INS(EXTI(reg)*reg-LA,LI),reg=MEMSL(areg)", "78000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXTI(reg)-LA,LI),reg=MEMSL(areg)", "78000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*reg-LA,LI)", "78000000:ra:rb"},
  {"INS(LA=EXTI(reg)-LA,LI)", "78000000:ra"},
  {"INS(LA=EXTI(reg)*reg-LA,LI),reg=MEMSL(areg)", "78000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)-LA,LI),reg=MEMSL(areg)", "78000001:ra:pmem_rm:pmem_ar"},

   // Op code = 121 (0x79)
  {"reg=INS(EXTI(reg)*reg&LA,LI)", "79000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)&LA,LI)", "79000000:rw:ra"},
  {"reg=INS(EXTI(reg)*reg&LA,LI),reg=MEMSL(areg)", "79000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXTI(reg)&LA,LI),reg=MEMSL(areg)", "79000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*reg&LA,LI)", "79000000:ra:rb"},
  {"INS(LA=EXTI(reg)&LA,LI)", "79000000:ra"},
  {"INS(LA=EXTI(reg)*reg&LA,LI),reg=MEMSL(areg)", "79000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)&LA,LI),reg=MEMSL(areg)", "79000001:ra:pmem_rm:pmem_ar"},

   // Op code = 122 (0x7a)
  {"reg=INS(EXTI(reg)*reg|LA,LI)", "7a000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)|LA,LI)", "7a000000:rw:ra"},
  {"reg=INS(EXTI(reg)*reg|LA,LI),reg=MEMSL(areg)", "7a000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXTI(reg)|LA,LI),reg=MEMSL(areg)", "7a000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*reg|LA,LI)", "7a000000:ra:rb"},
  {"INS(LA=EXTI(reg)|LA,LI)", "7a000000:ra"},
  {"INS(LA=EXTI(reg)*reg|LA,LI),reg=MEMSL(areg)", "7a000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)|LA,LI),reg=MEMSL(areg)", "7a000001:ra:pmem_rm:pmem_ar"},

   // Op code = 123 (0x7b)
  {"reg=INS(EXTI(reg)*reg^LA,LI)", "7b000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)^LA,LI)", "7b000000:rw:ra"},
  {"reg=INS(EXTI(reg)*reg^LA,LI),reg=MEMSL(areg)", "7b000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXTI(reg)^LA,LI),reg=MEMSL(areg)", "7b000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*reg^LA,LI)", "7b000000:ra:rb"},
  {"INS(LA=EXTI(reg)^LA,LI)", "7b000000:ra"},
  {"INS(LA=EXTI(reg)*reg^LA,LI),reg=MEMSL(areg)", "7b000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)^LA,LI),reg=MEMSL(areg)", "7b000001:ra:pmem_rm:pmem_ar"},

   // Op code = 124 (0x7c)
  {"reg=INS(EXTL(reg,lit,lit)*reg+LA,LI)", "7c000000:rw:ra:epos:ewid:rb"},
  {"reg=INS(EXTL(reg,lit,lit)+LA,LI)", "7c000000:rw:ra:epos:ewid"},
  {"INS(LA=EXTL(reg,lit,lit)*reg+LA,LI)", "7c000000:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)+LA,LI)", "7c000000:ra:epos:ewid"},
  {"reg=INS((reg<<lit)*reg+LA,LI)", "7c0001e0:rw:ra:epos:rb"},
  {"reg=INS((reg<<lit)+LA,LI)", "7c0001e0:rw:ra:epos"},
  {"INS(LA=(reg<<lit)*reg+LA,LI)", "7c0001e0:ra:epos:rb"},
  {"INS(LA=(reg<<lit)+LA,LI)", "7c0001e0:ra:epos"},

   // Op code = 125 (0x7d)
  {"reg=INS(EXTL(reg,lit,lit)*reg-LA,LI)", "7d000000:rw:ra:epos:ewid:rb"},
  {"reg=INS(EXTL(reg,lit,lit)-LA,LI)", "7d000000:rw:ra:epos:ewid"},
  {"INS(LA=EXTL(reg,lit,lit)*reg-LA,LI)", "7d000000:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)-LA,LI)", "7d000000:ra:epos:ewid"},
  {"reg=INS((reg<<lit)*reg-LA,LI)", "7d0001e0:rw:ra:epos:rb"},
  {"reg=INS((reg<<lit)-LA,LI)", "7d0001e0:rw:ra:epos"},
  {"INS(LA=(reg<<lit)*reg-LA,LI)", "7d0001e0:ra:epos:rb"},
  {"INS(LA=(reg<<lit)-LA,LI)", "7d0001e0:ra:epos"},

   // Op code = 126 (0x7e)
  {"reg=INS(EXTL(reg,lit,lit)*reg&LA,LI)", "7e000000:rw:ra:epos:ewid:rb"},
  {"reg=INS(EXTL(reg,lit,lit)&LA,LI)", "7e000000:rw:ra:epos:ewid"},
  {"INS(LA=EXTL(reg,lit,lit)*reg&LA,LI)", "7e000000:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)&LA,LI)", "7e000000:ra:epos:ewid"},
  {"reg=INS((reg<<lit)*reg&LA,LI)", "7e0001e0:rw:ra:epos:rb"},
  {"reg=INS((reg<<lit)&LA,LI)", "7e0001e0:rw:ra:epos"},
  {"INS(LA=(reg<<lit)*reg&LA,LI)", "7e0001e0:ra:epos:rb"},
  {"INS(LA=(reg<<lit)&LA,LI)", "7e0001e0:ra:epos"},

   // Op code = 127 (0x7f)
  {"reg=INS(EXTL(reg,lit,lit)*reg|LA,LI)", "7f000000:rw:ra:epos:ewid:rb"},
  {"reg=INS(EXTL(reg,lit,lit)|LA,LI)", "7f000000:rw:ra:epos:ewid"},
  {"INS(LA=EXTL(reg,lit,lit)*reg|LA,LI)", "7f000000:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)|LA,LI)", "7f000000:ra:epos:ewid"},
  {"reg=INS((reg<<lit)*reg|LA,LI)", "7f0001e0:rw:ra:epos:rb"},
  {"reg=INS((reg<<lit)|LA,LI)", "7f0001e0:rw:ra:epos"},
  {"INS(LA=(reg<<lit)*reg|LA,LI)", "7f0001e0:ra:epos:rb"},
  {"INS(LA=(reg<<lit)|LA,LI)", "7f0001e0:ra:epos"},

   // Op code = 128 (0x80)
  {"reg=INS(EXTL(reg,lit,lit)*reg^LA,LI)", "80000000:rw:ra:epos:ewid:rb"},
  {"reg=INS(EXTL(reg,lit,lit)^LA,LI)", "80000000:rw:ra:epos:ewid"},
  {"INS(LA=EXTL(reg,lit,lit)*reg^LA,LI)", "80000000:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)^LA,LI)", "80000000:ra:epos:ewid"},
  {"reg=INS((reg<<lit)*reg^LA,LI)", "800001e0:rw:ra:epos:rb"},
  {"reg=INS((reg<<lit)^LA,LI)", "800001e0:rw:ra:epos"},
  {"INS(LA=(reg<<lit)*reg^LA,LI)", "800001e0:ra:epos:rb"},
  {"INS(LA=(reg<<lit)^LA,LI)", "800001e0:ra:epos"},

   // Op code = 129 (0x81)
  {"reg=INS(reg*reg,LI)", "81000000:rw:ra:rb"},
  {"reg=INS(reg,LI)", "81000000:rw:ra"},
  {"reg=INS(reg*reg,LI),reg=MEMSL(areg)", "81000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(reg,LI),reg=MEMSL(areg)", "81000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=reg*reg,LI)", "81000000:ra:rb"},
  {"INS(LA=reg,LI)", "81000000:ra"},
  {"INS(LA=reg*reg,LI),reg=MEMSL(areg)", "81000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg,LI),reg=MEMSL(areg)", "81000001:ra:pmem_rm:pmem_ar"},

   // Op code = 130 (0x82)
  {"reg=INS(EXT(reg)*reg,LI)", "82000000:rw:ra:rb"},
  {"reg=INS(EXT(reg),LI)", "82000000:rw:ra"},
  {"reg=INS(EXT(reg)*reg,LI),reg=MEMSL(areg)", "82000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXT(reg),LI),reg=MEMSL(areg)", "82000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*reg,LI)", "82000000:ra:rb"},
  {"INS(LA=EXT(reg),LI)", "82000000:ra"},
  {"INS(LA=EXT(reg)*reg,LI),reg=MEMSL(areg)", "82000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg),LI),reg=MEMSL(areg)", "82000001:ra:pmem_rm:pmem_ar"},

   // Op code = 131 (0x83)
  {"reg=INS(EXTI(reg)*reg,LI)", "83000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg),LI)", "83000000:rw:ra"},
  {"reg=INS(EXTI(reg)*reg,LI),reg=MEMSL(areg)", "83000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"reg=INS(EXTI(reg),LI),reg=MEMSL(areg)", "83000001:rw:ra:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*reg,LI)", "83000000:ra:rb"},
  {"INS(LA=EXTI(reg),LI)", "83000000:ra"},
  {"INS(LA=EXTI(reg)*reg,LI),reg=MEMSL(areg)", "83000001:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg),LI),reg=MEMSL(areg)", "83000001:ra:pmem_rm:pmem_ar"},

   // Op code = 132 (0x84)
  {"reg=INS(EXTL(reg,lit,lit)*reg,LI)", "84000000:rw:ra:epos:ewid:rb"},
  {"reg=INS(EXTL(reg,lit,lit),LI)", "84000000:rw:ra:epos:ewid"},
  {"INS(LA=EXTL(reg,lit,lit)*reg,LI)", "84000000:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit),LI)", "84000000:ra:epos:ewid"},
  {"reg=INS((reg<<lit)*reg,LI)", "840001e0:rw:ra:epos:rb"},
  {"reg=INS((reg<<lit),LI)", "840001e0:rw:ra:epos"},
  {"INS(LA=(reg<<lit)*reg,LI)", "840001e0:ra:epos:rb"},
  {"INS(LA=(reg<<lit),LI)", "840001e0:ra:epos"},

   // Op code = 133 (0x85)
  {"reg=INS(reg+reg,LI)", "85000000:rw:ra:rb"},
  {"reg=INS(reg+reg,LI),reg=MEMSL(areg)", "85000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg+reg,LI)", "85000000:ra:rb"},
  {"INS(LA=reg+reg,LI),reg=MEMSL(areg)", "85000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 134 (0x86)
  {"reg=INS(reg-reg,LI)", "86000000:rw:ra:rb"},
  {"reg=INS(reg-reg,LI),reg=MEMSL(areg)", "86000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg-reg,LI)", "86000000:ra:rb"},
  {"INS(LA=reg-reg,LI),reg=MEMSL(areg)", "86000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 135 (0x87)
  {"reg=INS(reg&reg,LI)", "87000000:rw:ra:rb"},
  {"reg=INS(reg&reg,LI),reg=MEMSL(areg)", "87000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg&reg,LI)", "87000000:ra:rb"},
  {"INS(LA=reg&reg,LI),reg=MEMSL(areg)", "87000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 136 (0x88)
  {"reg=INS(reg|reg,LI)", "88000000:rw:ra:rb"},
  {"reg=INS(reg|reg,LI),reg=MEMSL(areg)", "88000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg|reg,LI)", "88000000:ra:rb"},
  {"INS(LA=reg|reg,LI),reg=MEMSL(areg)", "88000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 137 (0x89)
  {"reg=INS(reg^reg,LI)", "89000000:rw:ra:rb"},
  {"reg=INS(reg^reg,LI),reg=MEMSL(areg)", "89000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg^reg,LI)", "89000000:ra:rb"},
  {"INS(LA=reg^reg,LI),reg=MEMSL(areg)", "89000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 138 (0x8a)
  {"reg=INS(EXT(reg)+reg,LI)", "8a000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)+reg,LI),reg=MEMSL(areg)", "8a000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)+reg,LI)", "8a000000:ra:rb"},
  {"INS(LA=EXT(reg)+reg,LI),reg=MEMSL(areg)", "8a000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 139 (0x8b)
  {"reg=INS(EXT(reg)-reg,LI)", "8b000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)-reg,LI),reg=MEMSL(areg)", "8b000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)-reg,LI)", "8b000000:ra:rb"},
  {"INS(LA=EXT(reg)-reg,LI),reg=MEMSL(areg)", "8b000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 140 (0x8c)
  {"reg=INS(EXT(reg)&reg,LI)", "8c000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)&reg,LI),reg=MEMSL(areg)", "8c000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)&reg,LI)", "8c000000:ra:rb"},
  {"INS(LA=EXT(reg)&reg,LI),reg=MEMSL(areg)", "8c000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 141 (0x8d)
  {"reg=INS(EXT(reg)|reg,LI)", "8d000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)|reg,LI),reg=MEMSL(areg)", "8d000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)|reg,LI)", "8d000000:ra:rb"},
  {"INS(LA=EXT(reg)|reg,LI),reg=MEMSL(areg)", "8d000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 142 (0x8e)
  {"reg=INS(EXT(reg)^reg,LI)", "8e000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)^reg,LI),reg=MEMSL(areg)", "8e000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)^reg,LI)", "8e000000:ra:rb"},
  {"INS(LA=EXT(reg)^reg,LI),reg=MEMSL(areg)", "8e000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 143 (0x8f)
  {"reg=INS(EXTI(reg)+reg,LI)", "8f000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)+reg,LI),reg=MEMSL(areg)", "8f000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)+reg,LI)", "8f000000:ra:rb"},
  {"INS(LA=EXTI(reg)+reg,LI),reg=MEMSL(areg)", "8f000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 144 (0x90)
  {"reg=INS(EXTI(reg)-reg,LI)", "90000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)-reg,LI),reg=MEMSL(areg)", "90000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)-reg,LI)", "90000000:ra:rb"},
  {"INS(LA=EXTI(reg)-reg,LI),reg=MEMSL(areg)", "90000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 145 (0x91)
  {"reg=INS(EXTI(reg)&reg,LI)", "91000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)&reg,LI),reg=MEMSL(areg)", "91000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)&reg,LI)", "91000000:ra:rb"},
  {"INS(LA=EXTI(reg)&reg,LI),reg=MEMSL(areg)", "91000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 146 (0x92)
  {"reg=INS(EXTI(reg)|reg,LI)", "92000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)|reg,LI),reg=MEMSL(areg)", "92000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)|reg,LI)", "92000000:ra:rb"},
  {"INS(LA=EXTI(reg)|reg,LI),reg=MEMSL(areg)", "92000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 147 (0x93)
  {"reg=INS(EXTI(reg)^reg,LI)", "93000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)^reg,LI),reg=MEMSL(areg)", "93000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)^reg,LI)", "93000000:ra:rb"},
  {"INS(LA=EXTI(reg)^reg,LI),reg=MEMSL(areg)", "93000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 148 (0x94)
  {"reg=INS(EXTL(reg,lit,lit)+reg,LI)", "94000000:rw:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)+reg,LI)", "94000000:ra:epos:ewid:rb"},
  {"reg=INS((reg<<lit)+reg,LI)", "940001e0:rw:ra:epos:rb"},
  {"INS(LA=(reg<<lit)+reg,LI)", "940001e0:ra:epos:rb"},

   // Op code = 149 (0x95)
  {"reg=INS(EXTL(reg,lit,lit)-reg,LI)", "95000000:rw:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)-reg,LI)", "95000000:ra:epos:ewid:rb"},
  {"reg=INS((reg<<lit)-reg,LI)", "950001e0:rw:ra:epos:rb"},
  {"INS(LA=(reg<<lit)-reg,LI)", "950001e0:ra:epos:rb"},

   // Op code = 150 (0x96)
  {"reg=INS(EXTL(reg,lit,lit)&reg,LI)", "96000000:rw:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)&reg,LI)", "96000000:ra:epos:ewid:rb"},
  {"reg=INS((reg<<lit)&reg,LI)", "960001e0:rw:ra:epos:rb"},
  {"INS(LA=(reg<<lit)&reg,LI)", "960001e0:ra:epos:rb"},

   // Op code = 151 (0x97)
  {"reg=INS(EXTL(reg,lit,lit)|reg,LI)", "97000000:rw:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)|reg,LI)", "97000000:ra:epos:ewid:rb"},
  {"reg=INS((reg<<lit)|reg,LI)", "970001e0:rw:ra:epos:rb"},
  {"INS(LA=(reg<<lit)|reg,LI)", "970001e0:ra:epos:rb"},

   // Op code = 152 (0x98)
  {"reg=INS(EXTL(reg,lit,lit)^reg,LI)", "98000000:rw:ra:epos:ewid:rb"},
  {"INS(LA=EXTL(reg,lit,lit)^reg,LI)", "98000000:ra:epos:ewid:rb"},
  {"reg=INS((reg<<lit)^reg,LI)", "980001e0:rw:ra:epos:rb"},
  {"INS(LA=(reg<<lit)^reg,LI)", "980001e0:ra:epos:rb"},

   // Op code = 153 (0x99)
  {"reg=INS(lit-reg,LI)", "99000000:rw:lit:ra"},
  {"INS(LA=lit-reg,LI)", "99000000:lit:ra"},

   // Op code = 154 (0x9a)
  {"reg=INS(lit-EXT(reg),LI)", "9a000000:rw:lit:ra"},
  {"INS(LA=lit-EXT(reg),LI)", "9a000000:lit:ra"},

   // Op code = 155 (0x9b)
  {"reg=INS(lit-EXTI(reg),LI)", "9b000000:rw:lit:ra"},
  {"INS(LA=lit-EXTI(reg),LI)", "9b000000:lit:ra"},

   // Op code = 156 (0x9c)
  {"reg=LA", "9c0001fe:rw"},
  {"reg=LA,reg=MEMSL(areg)", "9c000001:rw:pmem_rm:pmem_ar"},
  {"reg=EXTL(LA,lit,lit)", "9c000000:rw:iepos:iewid"},

   // Op code = 157 (0x9d)
  {"reg=INS(LA,LI)", "9d000000:rw"},
  {"reg=INS(LA,LI),reg=MEMSL(areg)", "9d000001:rw:pmem_rm:pmem_ar"},

   // Op code = 158 (0x9e)
  {"reg=INS(reg,reg)", "9e000000:rw:ra:rb"},
  {"reg=INS(reg,reg),reg=MEMSL(areg)", "9e000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg,reg)", "9e000000:ra:rb"},
  {"INS(LA=reg,reg),reg=MEMSL(areg)", "9e000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 159 (0x9f)
  {"reg=INS(reg|LA,reg)", "9f000000:rw:ra:rb"},
  {"reg=INS(reg|LA,reg),reg=MEMSL(areg)", "9f000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=reg|LA,reg)", "9f000000:ra:rb"},
  {"INS(LA=reg|LA,reg),reg=MEMSL(areg)", "9f000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 160 (0xa0)
  {"reg=INS(reg*lit,reg)", "a0000000:rw:ra:lit:rb"},
  {"INS(LA=reg*lit,reg)", "a0000000:ra:lit:rb"},

   // Op code = 161 (0xa1)
  {"reg=INS(reg|lit,reg)", "a1000000:rw:ra:lit:rb"},
  {"INS(LA=reg|lit,reg)", "a1000000:ra:lit:rb"},

   // Op code = 162 (0xa2)
  {"reg=INS(reg+lit,reg)", "a2000000:rw:ra:lit:rb"},
  {"INS(LA=reg+lit,reg)", "a2000000:ra:lit:rb"},

   // Op code = 163 (0xa3)
  {"reg=INS(reg*lit+LA,reg)", "a3000000:rw:ra:lit:rb"},
  {"INS(LA=reg*lit+LA,reg)", "a3000000:ra:lit:rb"},

   // Op code = 164 (0xa4)
  {"reg=INS(reg*lit-LA,reg)", "a4000000:rw:ra:lit:rb"},
  {"INS(LA=reg*lit-LA,reg)", "a4000000:ra:lit:rb"},

   // Op code = 165 (0xa5)
  {"reg=INS(EXT(reg),reg)", "a5000000:rw:ra:rb"},
  {"reg=INS(EXT(reg),reg),reg=MEMSL(areg)", "a5000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg),reg)", "a5000000:ra:rb"},
  {"INS(LA=EXT(reg),reg),reg=MEMSL(areg)", "a5000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 166 (0xa6)
  {"reg=INS(EXT(reg)|LA,reg)", "a6000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)|LA,reg),reg=MEMSL(areg)", "a6000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)|LA,reg)", "a6000000:ra:rb"},
  {"INS(LA=EXT(reg)|LA,reg),reg=MEMSL(areg)", "a6000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 167 (0xa7)
  {"reg=INS(EXT(reg)*lit,reg)", "a7000000:rw:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit,reg)", "a7000000:ra:lit:rb"},

   // Op code = 168 (0xa8)
  {"reg=INS(EXT(reg)|lit,reg)", "a8000000:rw:ra:lit:rb"},
  {"INS(LA=EXT(reg)|lit,reg)", "a8000000:ra:lit:rb"},

   // Op code = 169 (0xa9)
  {"reg=INS(EXT(reg)+lit,reg)", "a9000000:rw:ra:lit:rb"},
  {"INS(LA=EXT(reg)+lit,reg)", "a9000000:ra:lit:rb"},

   // Op code = 170 (0xaa)
  {"reg=INS(EXT(reg)*lit+LA,reg)", "aa000000:rw:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit+LA,reg)", "aa000000:ra:lit:rb"},

   // Op code = 171 (0xab)
  {"reg=INS(EXT(reg)*lit-LA,reg)", "ab000000:rw:ra:lit:rb"},
  {"INS(LA=EXT(reg)*lit-LA,reg)", "ab000000:ra:lit:rb"},

   // Op code = 172 (0xac)
  {"reg=INS(EXTI(reg),reg)", "ac000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg),reg),reg=MEMSL(areg)", "ac000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg),reg)", "ac000000:ra:rb"},
  {"INS(LA=EXTI(reg),reg),reg=MEMSL(areg)", "ac000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 173 (0xad)
  {"reg=INS(EXTI(reg)|LA,reg)", "ad000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)|LA,reg),reg=MEMSL(areg)", "ad000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)|LA,reg)", "ad000000:ra:rb"},
  {"INS(LA=EXTI(reg)|LA,reg),reg=MEMSL(areg)", "ad000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 174 (0xae)
  {"reg=INS(EXTI(reg)*lit,reg)", "ae000000:rw:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit,reg)", "ae000000:ra:lit:rb"},

   // Op code = 175 (0xaf)
  {"reg=INS(EXTI(reg)|lit,reg)", "af000000:rw:ra:lit:rb"},
  {"INS(LA=EXTI(reg)|lit,reg)", "af000000:ra:lit:rb"},

   // Op code = 176 (0xb0)
  {"reg=INS(EXTI(reg)+lit,reg)", "b0000000:rw:ra:lit:rb"},
  {"INS(LA=EXTI(reg)+lit,reg)", "b0000000:ra:lit:rb"},

   // Op code = 177 (0xb1)
  {"reg=INS(EXTI(reg)*lit+LA,reg)", "b1000000:rw:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit+LA,reg)", "b1000000:ra:lit:rb"},

   // Op code = 178 (0xb2)
  {"reg=INS(EXTI(reg)*lit-LA,reg)", "b2000000:rw:ra:lit:rb"},
  {"INS(LA=EXTI(reg)*lit-LA,reg)", "b2000000:ra:lit:rb"},

   // Op code = 179 (0xb3)
  {"reg=EXTR(reg,reg)*lit", "b3000000:rw:ra:rb:lit"},
  {"LA=EXTR(reg,reg)*lit", "b3000000:ra:rb:lit"},

   // Op code = 180 (0xb4)
  {"reg=EXTR(reg,reg)+lit", "b4000000:rw:ra:rb:lit"},
  {"LA=EXTR(reg,reg)+lit", "b4000000:ra:rb:lit"},

   // Op code = 181 (0xb5)
  {"reg=EXTR(reg,reg)&lit", "b5000000:rw:ra:rb:lit"},
  {"LA=EXTR(reg,reg)&lit", "b5000000:ra:rb:lit"},

   // Op code = 182 (0xb6)
  {"reg=EXTR(reg,reg)&LA", "b60001fe:rw:ra:rb"},
  {"reg=EXTR(reg,reg)&LA,reg=MEMSL(areg)", "b6000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTR(reg,reg)&LA", "b60001fe:ra:rb"},
  {"LA=EXTR(reg,reg)&LA,reg=MEMSL(areg)", "b6000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTR(reg,reg)&LA,lit,lit)", "b6000000:rw:ra:rb:iepos:iewid"},

   // Op code = 183 (0xb7)
  {"reg=EXT(reg)*EXT2(reg)", "b70001fe:rw:ra:rb"},
  {"reg=EXT(reg)*EXT2(reg),reg=MEMSL(areg)", "b7000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*EXT2(reg)", "b70001fe:ra:rb"},
  {"LA=EXT(reg)*EXT2(reg),reg=MEMSL(areg)", "b7000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*EXT2(reg),lit,lit)", "b7000000:rw:ra:rb:iepos:iewid"},

   // Op code = 184 (0xb8)
  {"reg=EXT(reg)*EXT2(reg)+LA", "b80001fe:rw:ra:rb"},
  {"reg=EXT(reg)*EXT2(reg)+LA,reg=MEMSL(areg)", "b8000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*EXT2(reg)+LA", "b80001fe:ra:rb"},
  {"LA=EXT(reg)*EXT2(reg)+LA,reg=MEMSL(areg)", "b8000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*EXT2(reg)+LA,lit,lit)", "b8000000:rw:ra:rb:iepos:iewid"},

   // Op code = 185 (0xb9)
  {"reg=EXT(reg)*EXTI2(reg)", "b90001fe:rw:ra:rb"},
  {"reg=EXT(reg)*EXTI2(reg),reg=MEMSL(areg)", "b9000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*EXTI2(reg)", "b90001fe:ra:rb"},
  {"LA=EXT(reg)*EXTI2(reg),reg=MEMSL(areg)", "b9000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*EXTI2(reg),lit,lit)", "b9000000:rw:ra:rb:iepos:iewid"},

   // Op code = 186 (0xba)
  {"reg=EXT(reg)*EXTI2(reg)+LA", "ba0001fe:rw:ra:rb"},
  {"reg=EXT(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)", "ba000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXT(reg)*EXTI2(reg)+LA", "ba0001fe:ra:rb"},
  {"LA=EXT(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)", "ba000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXT(reg)*EXTI2(reg)+LA,lit,lit)", "ba000000:rw:ra:rb:iepos:iewid"},

   // Op code = 187 (0xbb)
  {"reg=EXTI(reg)*EXT2(reg)", "bb0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)*EXT2(reg),reg=MEMSL(areg)", "bb000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*EXT2(reg)", "bb0001fe:ra:rb"},
  {"LA=EXTI(reg)*EXT2(reg),reg=MEMSL(areg)", "bb000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*EXT2(reg),lit,lit)", "bb000000:rw:ra:rb:iepos:iewid"},

   // Op code = 188 (0xbc)
  {"reg=EXTI(reg)*EXT2(reg)+LA", "bc0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)*EXT2(reg)+LA,reg=MEMSL(areg)", "bc000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*EXT2(reg)+LA", "bc0001fe:ra:rb"},
  {"LA=EXTI(reg)*EXT2(reg)+LA,reg=MEMSL(areg)", "bc000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*EXT2(reg)+LA,lit,lit)", "bc000000:rw:ra:rb:iepos:iewid"},

   // Op code = 189 (0xbd)
  {"reg=EXTI(reg)*EXTI2(reg)", "bd0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)*EXTI2(reg),reg=MEMSL(areg)", "bd000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*EXTI2(reg)", "bd0001fe:ra:rb"},
  {"LA=EXTI(reg)*EXTI2(reg),reg=MEMSL(areg)", "bd000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*EXTI2(reg),lit,lit)", "bd000000:rw:ra:rb:iepos:iewid"},

   // Op code = 190 (0xbe)
  {"reg=EXTI(reg)*EXTI2(reg)+LA", "be0001fe:rw:ra:rb"},
  {"reg=EXTI(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)", "be000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=EXTI(reg)*EXTI2(reg)+LA", "be0001fe:ra:rb"},
  {"LA=EXTI(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)", "be000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(EXTI(reg)*EXTI2(reg)+LA,lit,lit)", "be000000:rw:ra:rb:iepos:iewid"},

   // Op code = 191 (0xbf)
  {"reg=INS(EXT(reg)*EXT2(reg),LI)", "bf000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)*EXT2(reg),LI),reg=MEMSL(areg)", "bf000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*EXT2(reg),LI)", "bf000000:ra:rb"},
  {"INS(LA=EXT(reg)*EXT2(reg),LI),reg=MEMSL(areg)", "bf000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 192 (0xc0)
  {"reg=INS(EXT(reg)*EXT2(reg)+LA,LI)", "c0000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)*EXT2(reg)+LA,LI),reg=MEMSL(areg)", "c0000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*EXT2(reg)+LA,LI)", "c0000000:ra:rb"},
  {"INS(LA=EXT(reg)*EXT2(reg)+LA,LI),reg=MEMSL(areg)", "c0000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 193 (0xc1)
  {"reg=INS(EXT(reg)*EXTI2(reg),LI)", "c1000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)*EXTI2(reg),LI),reg=MEMSL(areg)", "c1000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*EXTI2(reg),LI)", "c1000000:ra:rb"},
  {"INS(LA=EXT(reg)*EXTI2(reg),LI),reg=MEMSL(areg)", "c1000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 194 (0xc2)
  {"reg=INS(EXT(reg)*EXTI2(reg)+LA,LI)", "c2000000:rw:ra:rb"},
  {"reg=INS(EXT(reg)*EXTI2(reg)+LA,LI),reg=MEMSL(areg)", "c2000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXT(reg)*EXTI2(reg)+LA,LI)", "c2000000:ra:rb"},
  {"INS(LA=EXT(reg)*EXTI2(reg)+LA,LI),reg=MEMSL(areg)", "c2000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 195 (0xc3)
  {"reg=INS(EXTI(reg)*EXT2(reg),LI)", "c3000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)*EXT2(reg),LI),reg=MEMSL(areg)", "c3000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*EXT2(reg),LI)", "c3000000:ra:rb"},
  {"INS(LA=EXTI(reg)*EXT2(reg),LI),reg=MEMSL(areg)", "c3000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 196 (0xc4)
  {"reg=INS(EXTI(reg)*EXT2(reg)+LA,LI)", "c4000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)*EXT2(reg)+LA,LI),reg=MEMSL(areg)", "c4000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*EXT2(reg)+LA,LI)", "c4000000:ra:rb"},
  {"INS(LA=EXTI(reg)*EXT2(reg)+LA,LI),reg=MEMSL(areg)", "c4000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 197 (0xc5)
  {"reg=INS(EXTI(reg)*EXTI2(reg),LI)", "c5000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)*EXTI2(reg),LI),reg=MEMSL(areg)", "c5000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*EXTI2(reg),LI)", "c5000000:ra:rb"},
  {"INS(LA=EXTI(reg)*EXTI2(reg),LI),reg=MEMSL(areg)", "c5000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 198 (0xc6)
  {"reg=INS(EXTI(reg)*EXTI2(reg)+LA,LI)", "c6000000:rw:ra:rb"},
  {"reg=INS(EXTI(reg)*EXTI2(reg)+LA,LI),reg=MEMSL(areg)", "c6000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=EXTI(reg)*EXTI2(reg)+LA,LI)", "c6000000:ra:rb"},
  {"INS(LA=EXTI(reg)*EXTI2(reg)+LA,LI),reg=MEMSL(areg)", "c6000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 199 (0xc7)
  {"reg=QMULA(reg,reg)", "c70001fe:rw:ra:rb"},
  {"reg=QMULA(reg,reg),reg=MEMSL(areg)", "c7000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=QMULA(reg,reg)", "c70001fe:ra:rb"},
  {"LA=QMULA(reg,reg),reg=MEMSL(areg)", "c7000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(QMULA(reg,reg),lit,lit)", "c7000000:rw:ra:rb:iepos:iewid"},

   // Op code = 200 (0xc8)
  {"reg=QMULA(reg,reg)+LA", "c80001fe:rw:ra:rb"},
  {"reg=QMULA(reg,reg)+LA,reg=MEMSL(areg)", "c8000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"LA=QMULA(reg,reg)+LA", "c80001fe:ra:rb"},
  {"LA=QMULA(reg,reg)+LA,reg=MEMSL(areg)", "c8000001:ra:rb:pmem_rm:pmem_ar"},
  {"reg=EXTL(QMULA(reg,reg)+LA,lit,lit)", "c8000000:rw:ra:rb:iepos:iewid"},

   // Op code = 201 (0xc9)
  {"reg=INS(QMULA(reg,reg),LI)", "c9000000:rw:ra:rb"},
  {"reg=INS(QMULA(reg,reg),LI),reg=MEMSL(areg)", "c9000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=QMULA(reg,reg),LI)", "c9000000:ra:rb"},
  {"INS(LA=QMULA(reg,reg),LI),reg=MEMSL(areg)", "c9000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 202 (0xca)
  {"reg=INS(QMULA(reg,reg)+LA,LI)", "ca000000:rw:ra:rb"},
  {"reg=INS(QMULA(reg,reg)+LA,LI),reg=MEMSL(areg)", "ca000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=QMULA(reg,reg)+LA,LI)", "ca000000:ra:rb"},
  {"INS(LA=QMULA(reg,reg)+LA,LI),reg=MEMSL(areg)", "ca000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 203 (0xcb)
  {"reg=INS(QCOMPB(reg,reg),LI)", "cb000000:rw:ra:rb"},
  {"reg=INS(QCOMPB(reg,reg),LI),reg=MEMSL(areg)", "cb000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=QCOMPB(reg,reg),LI)", "cb000000:ra:rb"},
  {"INS(LA=QCOMPB(reg,reg),LI),reg=MEMSL(areg)", "cb000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 204 (0xcc)
  {"reg=INS(QCOMP(reg,reg),LI)", "cc000000:rw:ra:rb"},
  {"reg=INS(QCOMP(reg,reg),LI),reg=MEMSL(areg)", "cc000001:rw:ra:rb:pmem_rm:pmem_ar"},
  {"INS(LA=QCOMP(reg,reg),LI)", "cc000000:ra:rb"},
  {"INS(LA=QCOMP(reg,reg),LI),reg=MEMSL(areg)", "cc000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 205 (0xcd)
  {"reg=ADDMB(reg,lit)", "cd000000:rw:ra:lit"},
  {"LA=ADDMB(reg,lit)", "cd000000:ra:lit"},

   // Op code = 206 (0xce)
  {"reg=ADDMB(reg,lit)+reg", "ce000000:rw:ra:lit:rb"},
  {"reg=ADDMB(reg,lit)+LA", "ce000000:rw:ra:lit"},
  {"LA=ADDMB(reg,lit)+reg", "ce000000:ra:lit:rb"},
  {"LA=ADDMB(reg,lit)+LA", "ce000000:ra:lit"},

   // Op code = 207 (0xcf)
  {"reg=ADDMB(reg,lit)-reg", "cf000000:rw:ra:lit:rb"},
  {"reg=ADDMB(reg,lit)-LA", "cf000000:rw:ra:lit"},
  {"LA=ADDMB(reg,lit)-reg", "cf000000:ra:lit:rb"},
  {"LA=ADDMB(reg,lit)-LA", "cf000000:ra:lit"},

   // Op code = 208 (0xd0)
  {"reg=ADDMB(reg,reg)+lit", "d0000000:rw:ra:rb:lit"},
  {"LA=ADDMB(reg,reg)+lit", "d0000000:ra:rb:lit"},

   // Op code = 209 (0xd1)
  {"reg=SELECT(LA==lit,reg,reg)", "d1000000:rw:0:ra:rb"},
  {"reg=SELECT(LA!=lit,reg,reg)", "d1000040:rw:0:ra:rb"},
  {"reg=SELECT(LA>lit,reg,reg)", "d1000080:rw:0:ra:rb"},
  {"reg=SELECT(LA>=lit,reg,reg)", "d10000c0:rw:0:ra:rb"},
  {"reg=SELECT(LA<lit,reg,reg)", "d1000100:rw:0:ra:rb"},
  {"reg=SELECT(LA<=lit,reg,reg)", "d1000140:rw:0:ra:rb"},
  {"reg=SELECT(LA==lit,LA,reg)", "d1000000:rw:0:rb"},
  {"reg=SELECT(LA!=lit,LA,reg)", "d1000040:rw:0:rb"},
  {"reg=SELECT(LA>lit,LA,reg)", "d1000080:rw:0:rb"},
  {"reg=SELECT(LA>=lit,LA,reg)", "d10000c0:rw:0:rb"},
  {"reg=SELECT(LA<lit,LA,reg)", "d1000100:rw:0:rb"},
  {"reg=SELECT(LA<=lit,LA,reg)", "d1000140:rw:0:rb"},
  {"LA=SELECT(LA==lit,reg,reg)", "d1000000:0:ra:rb"},
  {"LA=SELECT(LA!=lit,reg,reg)", "d1000040:0:ra:rb"},
  {"LA=SELECT(LA>lit,reg,reg)", "d1000080:0:ra:rb"},
  {"LA=SELECT(LA>=lit,reg,reg)", "d10000c0:0:ra:rb"},
  {"LA=SELECT(LA<lit,reg,reg)", "d1000100:0:ra:rb"},
  {"LA=SELECT(LA<=lit,reg,reg)", "d1000140:0:ra:rb"},
  {"LA=SELECT(LA==lit,LA,reg)", "d1000000:0:rb"},
  {"LA=SELECT(LA!=lit,LA,reg)", "d1000040:0:rb"},
  {"LA=SELECT(LA>lit,LA,reg)", "d1000080:0:rb"},
  {"LA=SELECT(LA>=lit,LA,reg)", "d10000c0:0:rb"},
  {"LA=SELECT(LA<lit,LA,reg)", "d1000100:0:rb"},
  {"LA=SELECT(LA<=lit,LA,reg)", "d1000140:0:rb"},

   // Op code = 210 (0xd2)
  {"reg=SELECT(LA==lit,reg,lit)", "d2000000:rw:0:ra:sel_lit"},
  {"reg=SELECT(LA!=lit,reg,lit)", "d2000040:rw:0:ra:sel_lit"},
  {"reg=SELECT(LA>lit,reg,lit)", "d2000080:rw:0:ra:sel_lit"},
  {"reg=SELECT(LA>=lit,reg,lit)", "d20000c0:rw:0:ra:sel_lit"},
  {"reg=SELECT(LA<lit,reg,lit)", "d2000100:rw:0:ra:sel_lit"},
  {"reg=SELECT(LA<=lit,reg,lit)", "d2000140:rw:0:ra:sel_lit"},
  {"reg=SELECT(LA==lit,LA,lit)", "d2000000:rw:0:sel_lit"},
  {"reg=SELECT(LA!=lit,LA,lit)", "d2000040:rw:0:sel_lit"},
  {"reg=SELECT(LA>lit,LA,lit)", "d2000080:rw:0:sel_lit"},
  {"reg=SELECT(LA>=lit,LA,lit)", "d20000c0:rw:0:sel_lit"},
  {"reg=SELECT(LA<lit,LA,lit)", "d2000100:rw:0:sel_lit"},
  {"reg=SELECT(LA<=lit,LA,lit)", "d2000140:rw:0:sel_lit"},
  {"LA=SELECT(LA==lit,reg,lit)", "d2000000:0:ra:sel_lit"},
  {"LA=SELECT(LA!=lit,reg,lit)", "d2000040:0:ra:sel_lit"},
  {"LA=SELECT(LA>lit,reg,lit)", "d2000080:0:ra:sel_lit"},
  {"LA=SELECT(LA>=lit,reg,lit)", "d20000c0:0:ra:sel_lit"},
  {"LA=SELECT(LA<lit,reg,lit)", "d2000100:0:ra:sel_lit"},
  {"LA=SELECT(LA<=lit,reg,lit)", "d2000140:0:ra:sel_lit"},
  {"LA=SELECT(LA==lit,LA,lit)", "d2000000:0:sel_lit"},
  {"LA=SELECT(LA!=lit,LA,lit)", "d2000040:0:sel_lit"},
  {"LA=SELECT(LA>lit,LA,lit)", "d2000080:0:sel_lit"},
  {"LA=SELECT(LA>=lit,LA,lit)", "d20000c0:0:sel_lit"},
  {"LA=SELECT(LA<lit,LA,lit)", "d2000100:0:sel_lit"},
  {"LA=SELECT(LA<=lit,LA,lit)", "d2000140:0:sel_lit"},

   // Op code = 211 (0xd3)
  {"reg=SELECT(reg==lit,reg,reg)", "d3000000:rw:rb:0:ra:rb"},
  {"reg=SELECT(reg!=lit,reg,reg)", "d3000040:rw:rb:0:ra:rb"},
  {"reg=SELECT(reg>lit,reg,reg)", "d3000080:rw:rb:0:ra:rb"},
  {"reg=SELECT(reg>=lit,reg,reg)", "d30000c0:rw:rb:0:ra:rb"},
  {"reg=SELECT(reg<lit,reg,reg)", "d3000100:rw:rb:0:ra:rb"},
  {"reg=SELECT(reg<=lit,reg,reg)", "d3000140:rw:rb:0:ra:rb"},
  {"reg=SELECT(reg==lit,LA,reg)", "d3000000:rw:rb:0:rb"},
  {"reg=SELECT(reg!=lit,LA,reg)", "d3000040:rw:rb:0:rb"},
  {"reg=SELECT(reg>lit,LA,reg)", "d3000080:rw:rb:0:rb"},
  {"reg=SELECT(reg>=lit,LA,reg)", "d30000c0:rw:rb:0:rb"},
  {"reg=SELECT(reg<lit,LA,reg)", "d3000100:rw:rb:0:rb"},
  {"reg=SELECT(reg<=lit,LA,reg)", "d3000140:rw:rb:0:rb"},
  {"LA=SELECT(reg==lit,reg,reg)", "d3000000:rb:0:ra:rb"},
  {"LA=SELECT(reg!=lit,reg,reg)", "d3000040:rb:0:ra:rb"},
  {"LA=SELECT(reg>lit,reg,reg)", "d3000080:rb:0:ra:rb"},
  {"LA=SELECT(reg>=lit,reg,reg)", "d30000c0:rb:0:ra:rb"},
  {"LA=SELECT(reg<lit,reg,reg)", "d3000100:rb:0:ra:rb"},
  {"LA=SELECT(reg<=lit,reg,reg)", "d3000140:rb:0:ra:rb"},
  {"LA=SELECT(reg==lit,LA,reg)", "d3000000:rb:0:rb"},
  {"LA=SELECT(reg!=lit,LA,reg)", "d3000040:rb:0:rb"},
  {"LA=SELECT(reg>lit,LA,reg)", "d3000080:rb:0:rb"},
  {"LA=SELECT(reg>=lit,LA,reg)", "d30000c0:rb:0:rb"},
  {"LA=SELECT(reg<lit,LA,reg)", "d3000100:rb:0:rb"},
  {"LA=SELECT(reg<=lit,LA,reg)", "d3000140:rb:0:rb"},

   // Op code = 212 (0xd4)
  {"reg=SELECT(reg==lit,reg,lit)", "d4000000:rw:rb:0:ra:sel_lit"},
  {"reg=SELECT(reg!=lit,reg,lit)", "d4000040:rw:rb:0:ra:sel_lit"},
  {"reg=SELECT(reg>lit,reg,lit)", "d4000080:rw:rb:0:ra:sel_lit"},
  {"reg=SELECT(reg>=lit,reg,lit)", "d40000c0:rw:rb:0:ra:sel_lit"},
  {"reg=SELECT(reg<lit,reg,lit)", "d4000100:rw:rb:0:ra:sel_lit"},
  {"reg=SELECT(reg<=lit,reg,lit)", "d4000140:rw:rb:0:ra:sel_lit"},
  {"reg=SELECT(reg==lit,LA,lit)", "d4000000:rw:rb:0:sel_lit"},
  {"reg=SELECT(reg!=lit,LA,lit)", "d4000040:rw:rb:0:sel_lit"},
  {"reg=SELECT(reg>lit,LA,lit)", "d4000080:rw:rb:0:sel_lit"},
  {"reg=SELECT(reg>=lit,LA,lit)", "d40000c0:rw:rb:0:sel_lit"},
  {"reg=SELECT(reg<lit,LA,lit)", "d4000100:rw:rb:0:sel_lit"},
  {"reg=SELECT(reg<=lit,LA,lit)", "d4000140:rw:rb:0:sel_lit"},
  {"LA=SELECT(reg==lit,reg,lit)", "d4000000:rb:0:ra:sel_lit"},
  {"LA=SELECT(reg!=lit,reg,lit)", "d4000040:rb:0:ra:sel_lit"},
  {"LA=SELECT(reg>lit,reg,lit)", "d4000080:rb:0:ra:sel_lit"},
  {"LA=SELECT(reg>=lit,reg,lit)", "d40000c0:rb:0:ra:sel_lit"},
  {"LA=SELECT(reg<lit,reg,lit)", "d4000100:rb:0:ra:sel_lit"},
  {"LA=SELECT(reg<=lit,reg,lit)", "d4000140:rb:0:ra:sel_lit"},
  {"LA=SELECT(reg==lit,LA,lit)", "d4000000:rb:0:sel_lit"},
  {"LA=SELECT(reg!=lit,LA,lit)", "d4000040:rb:0:sel_lit"},
  {"LA=SELECT(reg>lit,LA,lit)", "d4000080:rb:0:sel_lit"},
  {"LA=SELECT(reg>=lit,LA,lit)", "d40000c0:rb:0:sel_lit"},
  {"LA=SELECT(reg<lit,LA,lit)", "d4000100:rb:0:sel_lit"},
  {"LA=SELECT(reg<=lit,LA,lit)", "d4000140:rb:0:sel_lit"},

   // Op code = 213 (0xd5)
  {"reg=CNTLD(reg,lit)", "d5000000:rw:rb:lit"},
  {"reg=CNTLZ(reg)", "d5000000:rw:rb"},
  {"reg=CNTLO(reg)", "d5000001:rw:rb"},
  {"reg=CNTLS(reg)", "d5000002:rw:rb"},
  {"reg=CNTLD(LA,lit)", "d5000000:rw:lit"},
  {"reg=CNTLZ(LA)", "d5000000:rw"},
  {"reg=CNTLO(LA)", "d5000001:rw"},
  {"reg=CNTLS(LA)", "d5000002:rw"},
  {"LA=CNTLD(reg,lit)", "d5000000:rb:lit"},
  {"LA=CNTLZ(reg)", "d5000000:rb"},
  {"LA=CNTLO(reg)", "d5000001:rb"},
  {"LA=CNTLS(reg)", "d5000002:rb"},
  {"LA=CNTLD(LA,lit)", "d5000000:lit"},
  {"LA=CNTLZ(LA)", "d5000000"},
  {"LA=CNTLO(LA)", "d5000001"},
  {"LA=CNTLS(LA)", "d5000002"},

   // Op code = 214 (0xd6)
  {"LA=LOAD_LA(reg,reg)", "d6000000:rb:ra"},

   // Op code = 215 (0xd7)
  {"reg.a=reg.a*lit+reg.a", "d7000000:rw:ra:sdp_lit:rb"},
  {"reg.a=reg.a+reg.a", "d7000001:rw:ra:rb"},
  {"reg.a=reg.a*lit+LA.a", "d7000000:rw:ra:sdp_lit"},
  {"reg.a=reg.a+LA.a", "d7000001:rw:ra"},
  {"reg.b=reg.b*lit+reg.b", "d7000080:rw:ra:sdp_lit:rb"},
  {"reg.b=reg.b+reg.b", "d7000081:rw:ra:rb"},
  {"reg.b=reg.b*lit+LA.b", "d7000080:rw:ra:sdp_lit"},
  {"reg.b=reg.b+LA.b", "d7000081:rw:ra"},
  {"reg.c=reg.c*lit+reg.c", "d7000100:rw:ra:sdp_lit:rb"},
  {"reg.c=reg.c+reg.c", "d7000101:rw:ra:rb"},
  {"reg.c=reg.c*lit+LA.c", "d7000100:rw:ra:sdp_lit"},
  {"reg.c=reg.c+LA.c", "d7000101:rw:ra"},
  {"reg.d=reg.d*lit+reg.d", "d7000180:rw:ra:sdp_lit:rb"},
  {"reg.d=reg.d+reg.d", "d7000181:rw:ra:rb"},
  {"reg.d=reg.d*lit+LA.d", "d7000180:rw:ra:sdp_lit"},
  {"reg.d=reg.d+LA.d", "d7000181:rw:ra"},
  {"LA.a=reg.a*lit+reg.a", "d7000000:ra:sdp_lit:rb"},
  {"LA.a=reg.a+reg.a", "d7000001:ra:rb"},
  {"LA.a=reg.a*lit+LA.a", "d7000000:ra:sdp_lit"},
  {"LA.a=reg.a+LA.a", "d7000001:ra"},
  {"LA.b=reg.b*lit+reg.b", "d7000080:ra:sdp_lit:rb"},
  {"LA.b=reg.b+reg.b", "d7000081:ra:rb"},
  {"LA.b=reg.b*lit+LA.b", "d7000080:ra:sdp_lit"},
  {"LA.b=reg.b+LA.b", "d7000081:ra"},
  {"LA.c=reg.c*lit+reg.c", "d7000100:ra:sdp_lit:rb"},
  {"LA.c=reg.c+reg.c", "d7000101:ra:rb"},
  {"LA.c=reg.c*lit+LA.c", "d7000100:ra:sdp_lit"},
  {"LA.c=reg.c+LA.c", "d7000101:ra"},
  {"LA.d=reg.d*lit+reg.d", "d7000180:ra:sdp_lit:rb"},
  {"LA.d=reg.d+reg.d", "d7000181:ra:rb"},
  {"LA.d=reg.d*lit+LA.d", "d7000180:ra:sdp_lit"},
  {"LA.d=reg.d+LA.d", "d7000181:ra"},

   // Op code = 216 (0xd8)
  {"reg.a=reg.a*reg.a+lit", "d8000000:rw:ra:rb:sdp_lit"},
  {"reg.a=reg.a*reg.a", "d8000000:rw:ra:rb"},
  {"reg.a=reg.a+lit", "d8000000:rw:ra:sdp_lit"},
  {"reg.b=reg.b*reg.b+lit", "d8000080:rw:ra:rb:sdp_lit"},
  {"reg.b=reg.b*reg.b", "d8000080:rw:ra:rb"},
  {"reg.b=reg.b+lit", "d8000080:rw:ra:sdp_lit"},
  {"reg.c=reg.c*reg.c+lit", "d8000100:rw:ra:rb:sdp_lit"},
  {"reg.c=reg.c*reg.c", "d8000100:rw:ra:rb"},
  {"reg.c=reg.c+lit", "d8000100:rw:ra:sdp_lit"},
  {"reg.d=reg.d*reg.d+lit", "d8000180:rw:ra:rb:sdp_lit"},
  {"reg.d=reg.d*reg.d", "d8000180:rw:ra:rb"},
  {"reg.d=reg.d+lit", "d8000180:rw:ra:sdp_lit"},
  {"LA.a=reg.a*reg.a+lit", "d8000000:ra:rb:sdp_lit"},
  {"LA.a=reg.a*reg.a", "d8000000:ra:rb"},
  {"LA.a=reg.a+lit", "d8000000:ra:sdp_lit"},
  {"LA.b=reg.b*reg.b+lit", "d8000080:ra:rb:sdp_lit"},
  {"LA.b=reg.b*reg.b", "d8000080:ra:rb"},
  {"LA.b=reg.b+lit", "d8000080:ra:sdp_lit"},
  {"LA.c=reg.c*reg.c+lit", "d8000100:ra:rb:sdp_lit"},
  {"LA.c=reg.c*reg.c", "d8000100:ra:rb"},
  {"LA.c=reg.c+lit", "d8000100:ra:sdp_lit"},
  {"LA.d=reg.d*reg.d+lit", "d8000180:ra:rb:sdp_lit"},
  {"LA.d=reg.d*reg.d", "d8000180:ra:rb"},
  {"LA.d=reg.d+lit", "d8000180:ra:sdp_lit"},

   // Op code = 217 (0xd9)
  {"reg.a=reg.a", "d9000000:rw:ra"},
  {"reg.b=reg.a", "d9000200:rw:ra"},
  {"reg.c=reg.a", "d9000400:rw:ra"},
  {"reg.d=reg.a", "d9000600:rw:ra"},
  {"reg.a=reg.b", "d9000800:rw:ra"},
  {"reg.b=reg.b", "d9000a00:rw:ra"},
  {"reg.c=reg.b", "d9000c00:rw:ra"},
  {"reg.d=reg.b", "d9000e00:rw:ra"},
  {"reg.a=reg.c", "d9001000:rw:ra"},
  {"reg.b=reg.c", "d9001200:rw:ra"},
  {"reg.c=reg.c", "d9001400:rw:ra"},
  {"reg.d=reg.c", "d9001600:rw:ra"},
  {"reg.a=reg.d", "d9001800:rw:ra"},
  {"reg.b=reg.d", "d9001a00:rw:ra"},
  {"reg.c=reg.d", "d9001c00:rw:ra"},
  {"reg.d=reg.d", "d9001e00:rw:ra"},
  {"reg=reg.a", "d9002000:rw:ra"},
  {"reg=reg.b", "d9002800:rw:ra"},
  {"reg=reg.c", "d9003000:rw:ra"},
  {"reg=reg.d", "d9003800:rw:ra"},
  {"LA.a=reg.a", "d9000000:ra"},
  {"LA.b=reg.a", "d9000200:ra"},
  {"LA.c=reg.a", "d9000400:ra"},
  {"LA.d=reg.a", "d9000600:ra"},
  {"LA.a=reg.b", "d9000800:ra"},
  {"LA.b=reg.b", "d9000a00:ra"},
  {"LA.c=reg.b", "d9000c00:ra"},
  {"LA.d=reg.b", "d9000e00:ra"},
  {"LA.a=reg.c", "d9001000:ra"},
  {"LA.b=reg.c", "d9001200:ra"},
  {"LA.c=reg.c", "d9001400:ra"},
  {"LA.d=reg.c", "d9001600:ra"},
  {"LA.a=reg.d", "d9001800:ra"},
  {"LA.b=reg.d", "d9001a00:ra"},
  {"LA.c=reg.d", "d9001c00:ra"},
  {"LA.d=reg.d", "d9001e00:ra"},
  {"LA=reg.a", "d9002000:ra"},
  {"LA=reg.b", "d9002800:ra"},
  {"LA=reg.c", "d9003000:ra"},
  {"LA=reg.d", "d9003800:ra"},

   // Op code = 218 (0xda)
  {"reg=lit", "da010000:rw:ldlit_lit"},
  {"reg.a=lit", "da000000:rw:ldlit_lit"},
  {"reg.b=lit", "da020000:rw:ldlit_lit"},
  {"reg.c=lit", "da040000:rw:ldlit_lit"},
  {"reg.d=lit", "da060000:rw:ldlit_lit"},
  {"LA=lit", "da010000:ldlit_lit"},
  {"LA.a=lit", "da000000:ldlit_lit"},
  {"LA.b=lit", "da020000:ldlit_lit"},
  {"LA.c=lit", "da040000:ldlit_lit"},
  {"LA.d=lit", "da060000:ldlit_lit"},

   // Op code = 219 (0xdb)
  {"reg=LIT_HIGH+lit", "db010000:rw:ldlitw_lit"},
  {"reg.a=LIT_HIGH.a+lit", "db000000:rw:ldlitw_lit"},
  {"reg.b=LIT_HIGH.b+lit", "db020000:rw:ldlitw_lit"},
  {"reg.c=LIT_HIGH.c+lit", "db040000:rw:ldlitw_lit"},
  {"reg.d=LIT_HIGH.d+lit", "db060000:rw:ldlitw_lit"},
  {"LA=LIT_HIGH+lit", "db010000:ldlitw_lit"},
  {"LA.a=LIT_HIGH.a+lit", "db000000:ldlitw_lit"},
  {"LA.b=LIT_HIGH.b+lit", "db020000:ldlitw_lit"},
  {"LA.c=LIT_HIGH.c+lit", "db040000:ldlitw_lit"},
  {"LA.d=LIT_HIGH.d+lit", "db060000:ldlitw_lit"},

   // Op code = 220 (0xdc)
  {"SPECREG(INS_EPOS)=reg", "dc220000:rb"},
  {"SPECREG(LI)=reg", "dc240000:rb"},
  {"SPECREG(INS_ALL)=reg", "dc23c000:rb"},
  {"SPECREG(EXT_POS)=reg", "dc20c000:rb"},
  {"SPECREG(QMULA_SIGNED)=reg", "dc24c000:rb"},
  {"SPECREG(INS_WIDTH)=reg", "dc224000:rb"},
  {"SPECREG(EXT_INIT)=reg", "dc200000:rb"},
  {"SPECREG(EXT_ALL)=reg", "dc214000:rb"},
  {"SPECREG(INS_ADD)=reg", "dc230000:rb"},
  {"SPECREG(EXT_SIGNED)=reg", "dc218000:rb"},
  {"SPECREG(INS_IPOS)=reg", "dc228000:rb"},
  {"SPECREG(EXT_NXT)=reg", "dc210000:rb"},
  {"SPECREG(LIT_HIGH)=reg", "dc248000:rb"},
  {"SPECREG(EXT2_ALL)=reg", "dc21c000:rb"},
  {"SPECREG(INS_MODE)=reg", "dc234000:rb"},
  {"SPECREG(INS_INC)=reg", "dc22c000:rb"},
  {"SPECREG(EXT_WIDTH)=reg", "dc208000:rb"},
  {"SPECREG(EXT_INC)=reg", "dc204000:rb"},
  {"SPECREG(INS_INIT)=reg", "dc238000:rb"},
  {"SPECREG(INS_EPOS.a)=reg.a", "dc020000:rb"},
  {"SPECREG(LI.a)=reg.a", "dc040000:rb"},
  {"SPECREG(INS_ALL.a)=reg.a", "dc03c000:rb"},
  {"SPECREG(EXT_POS.a)=reg.a", "dc00c000:rb"},
  {"SPECREG(QMULA_SIGNED.a)=reg.a", "dc04c000:rb"},
  {"SPECREG(INS_WIDTH.a)=reg.a", "dc024000:rb"},
  {"SPECREG(EXT_INIT.a)=reg.a", "dc000000:rb"},
  {"SPECREG(EXT_ALL.a)=reg.a", "dc014000:rb"},
  {"SPECREG(INS_ADD.a)=reg.a", "dc030000:rb"},
  {"SPECREG(EXT_SIGNED.a)=reg.a", "dc018000:rb"},
  {"SPECREG(INS_IPOS.a)=reg.a", "dc028000:rb"},
  {"SPECREG(EXT_NXT.a)=reg.a", "dc010000:rb"},
  {"SPECREG(LIT_HIGH.a)=reg.a", "dc048000:rb"},
  {"SPECREG(EXT2_ALL.a)=reg.a", "dc01c000:rb"},
  {"SPECREG(INS_MODE.a)=reg.a", "dc034000:rb"},
  {"SPECREG(INS_INC.a)=reg.a", "dc02c000:rb"},
  {"SPECREG(EXT_WIDTH.a)=reg.a", "dc008000:rb"},
  {"SPECREG(EXT_INC.a)=reg.a", "dc004000:rb"},
  {"SPECREG(INS_INIT.a)=reg.a", "dc038000:rb"},
  {"SPECREG(INS_EPOS.b)=reg.b", "dc420000:rb"},
  {"SPECREG(LI.b)=reg.b", "dc440000:rb"},
  {"SPECREG(INS_ALL.b)=reg.b", "dc43c000:rb"},
  {"SPECREG(EXT_POS.b)=reg.b", "dc40c000:rb"},
  {"SPECREG(QMULA_SIGNED.b)=reg.b", "dc44c000:rb"},
  {"SPECREG(INS_WIDTH.b)=reg.b", "dc424000:rb"},
  {"SPECREG(EXT_INIT.b)=reg.b", "dc400000:rb"},
  {"SPECREG(EXT_ALL.b)=reg.b", "dc414000:rb"},
  {"SPECREG(INS_ADD.b)=reg.b", "dc430000:rb"},
  {"SPECREG(EXT_SIGNED.b)=reg.b", "dc418000:rb"},
  {"SPECREG(INS_IPOS.b)=reg.b", "dc428000:rb"},
  {"SPECREG(EXT_NXT.b)=reg.b", "dc410000:rb"},
  {"SPECREG(LIT_HIGH.b)=reg.b", "dc448000:rb"},
  {"SPECREG(EXT2_ALL.b)=reg.b", "dc41c000:rb"},
  {"SPECREG(INS_MODE.b)=reg.b", "dc434000:rb"},
  {"SPECREG(INS_INC.b)=reg.b", "dc42c000:rb"},
  {"SPECREG(EXT_WIDTH.b)=reg.b", "dc408000:rb"},
  {"SPECREG(EXT_INC.b)=reg.b", "dc404000:rb"},
  {"SPECREG(INS_INIT.b)=reg.b", "dc438000:rb"},
  {"SPECREG(INS_EPOS.c)=reg.c", "dc820000:rb"},
  {"SPECREG(LI.c)=reg.c", "dc840000:rb"},
  {"SPECREG(INS_ALL.c)=reg.c", "dc83c000:rb"},
  {"SPECREG(EXT_POS.c)=reg.c", "dc80c000:rb"},
  {"SPECREG(QMULA_SIGNED.c)=reg.c", "dc84c000:rb"},
  {"SPECREG(INS_WIDTH.c)=reg.c", "dc824000:rb"},
  {"SPECREG(EXT_INIT.c)=reg.c", "dc800000:rb"},
  {"SPECREG(EXT_ALL.c)=reg.c", "dc814000:rb"},
  {"SPECREG(INS_ADD.c)=reg.c", "dc830000:rb"},
  {"SPECREG(EXT_SIGNED.c)=reg.c", "dc818000:rb"},
  {"SPECREG(INS_IPOS.c)=reg.c", "dc828000:rb"},
  {"SPECREG(EXT_NXT.c)=reg.c", "dc810000:rb"},
  {"SPECREG(LIT_HIGH.c)=reg.c", "dc848000:rb"},
  {"SPECREG(EXT2_ALL.c)=reg.c", "dc81c000:rb"},
  {"SPECREG(INS_MODE.c)=reg.c", "dc834000:rb"},
  {"SPECREG(INS_INC.c)=reg.c", "dc82c000:rb"},
  {"SPECREG(EXT_WIDTH.c)=reg.c", "dc808000:rb"},
  {"SPECREG(EXT_INC.c)=reg.c", "dc804000:rb"},
  {"SPECREG(INS_INIT.c)=reg.c", "dc838000:rb"},
  {"SPECREG(INS_EPOS.d)=reg.d", "dcc20000:rb"},
  {"SPECREG(LI.d)=reg.d", "dcc40000:rb"},
  {"SPECREG(INS_ALL.d)=reg.d", "dcc3c000:rb"},
  {"SPECREG(EXT_POS.d)=reg.d", "dcc0c000:rb"},
  {"SPECREG(QMULA_SIGNED.d)=reg.d", "dcc4c000:rb"},
  {"SPECREG(INS_WIDTH.d)=reg.d", "dcc24000:rb"},
  {"SPECREG(EXT_INIT.d)=reg.d", "dcc00000:rb"},
  {"SPECREG(EXT_ALL.d)=reg.d", "dcc14000:rb"},
  {"SPECREG(INS_ADD.d)=reg.d", "dcc30000:rb"},
  {"SPECREG(EXT_SIGNED.d)=reg.d", "dcc18000:rb"},
  {"SPECREG(INS_IPOS.d)=reg.d", "dcc28000:rb"},
  {"SPECREG(EXT_NXT.d)=reg.d", "dcc10000:rb"},
  {"SPECREG(LIT_HIGH.d)=reg.d", "dcc48000:rb"},
  {"SPECREG(EXT2_ALL.d)=reg.d", "dcc1c000:rb"},
  {"SPECREG(INS_MODE.d)=reg.d", "dcc34000:rb"},
  {"SPECREG(INS_INC.d)=reg.d", "dcc2c000:rb"},
  {"SPECREG(EXT_WIDTH.d)=reg.d", "dcc08000:rb"},
  {"SPECREG(EXT_INC.d)=reg.d", "dcc04000:rb"},
  {"SPECREG(INS_INIT.d)=reg.d", "dcc38000:rb"},

   // Op code = 221 (0xdd)
  {"SPECREG(INS_EPOS)=lit", "dd220000:sregld_lit"},
  {"SPECREG(LI)=lit", "dd240000:sregld_lit"},
  {"SPECREG(INS_ALL)=lit", "dd23c000:sregld_lit"},
  {"SPECREG(EXT_POS)=lit", "dd20c000:sregld_lit"},
  {"SPECREG(QMULA_SIGNED)=lit", "dd24c000:sregld_lit"},
  {"SPECREG(INS_WIDTH)=lit", "dd224000:sregld_lit"},
  {"SPECREG(EXT_INIT)=lit", "dd200000:sregld_lit"},
  {"SPECREG(EXT_ALL)=lit", "dd214000:sregld_lit"},
  {"SPECREG(INS_ADD)=lit", "dd230000:sregld_lit"},
  {"SPECREG(EXT_SIGNED)=lit", "dd218000:sregld_lit"},
  {"SPECREG(INS_IPOS)=lit", "dd228000:sregld_lit"},
  {"SPECREG(EXT_NXT)=lit", "dd210000:sregld_lit"},
  {"SPECREG(LIT_HIGH)=lit", "dd248000:sregld_lit"},
  {"SPECREG(EXT2_ALL)=lit", "dd21c000:sregld_lit"},
  {"SPECREG(INS_MODE)=lit", "dd234000:sregld_lit"},
  {"SPECREG(INS_INC)=lit", "dd22c000:sregld_lit"},
  {"SPECREG(EXT_WIDTH)=lit", "dd208000:sregld_lit"},
  {"SPECREG(EXT_INC)=lit", "dd204000:sregld_lit"},
  {"SPECREG(INS_INIT)=lit", "dd238000:sregld_lit"},
  {"SPECREG(INS_EPOS.a)=lit", "dd020000:sregld_lit"},
  {"SPECREG(LI.a)=lit", "dd040000:sregld_lit"},
  {"SPECREG(INS_ALL.a)=lit", "dd03c000:sregld_lit"},
  {"SPECREG(EXT_POS.a)=lit", "dd00c000:sregld_lit"},
  {"SPECREG(QMULA_SIGNED.a)=lit", "dd04c000:sregld_lit"},
  {"SPECREG(INS_WIDTH.a)=lit", "dd024000:sregld_lit"},
  {"SPECREG(EXT_INIT.a)=lit", "dd000000:sregld_lit"},
  {"SPECREG(EXT_ALL.a)=lit", "dd014000:sregld_lit"},
  {"SPECREG(INS_ADD.a)=lit", "dd030000:sregld_lit"},
  {"SPECREG(EXT_SIGNED.a)=lit", "dd018000:sregld_lit"},
  {"SPECREG(INS_IPOS.a)=lit", "dd028000:sregld_lit"},
  {"SPECREG(EXT_NXT.a)=lit", "dd010000:sregld_lit"},
  {"SPECREG(LIT_HIGH.a)=lit", "dd048000:sregld_lit"},
  {"SPECREG(EXT2_ALL.a)=lit", "dd01c000:sregld_lit"},
  {"SPECREG(INS_MODE.a)=lit", "dd034000:sregld_lit"},
  {"SPECREG(INS_INC.a)=lit", "dd02c000:sregld_lit"},
  {"SPECREG(EXT_WIDTH.a)=lit", "dd008000:sregld_lit"},
  {"SPECREG(EXT_INC.a)=lit", "dd004000:sregld_lit"},
  {"SPECREG(INS_INIT.a)=lit", "dd038000:sregld_lit"},
  {"SPECREG(INS_EPOS.b)=lit", "dd420000:sregld_lit"},
  {"SPECREG(LI.b)=lit", "dd440000:sregld_lit"},
  {"SPECREG(INS_ALL.b)=lit", "dd43c000:sregld_lit"},
  {"SPECREG(EXT_POS.b)=lit", "dd40c000:sregld_lit"},
  {"SPECREG(QMULA_SIGNED.b)=lit", "dd44c000:sregld_lit"},
  {"SPECREG(INS_WIDTH.b)=lit", "dd424000:sregld_lit"},
  {"SPECREG(EXT_INIT.b)=lit", "dd400000:sregld_lit"},
  {"SPECREG(EXT_ALL.b)=lit", "dd414000:sregld_lit"},
  {"SPECREG(INS_ADD.b)=lit", "dd430000:sregld_lit"},
  {"SPECREG(EXT_SIGNED.b)=lit", "dd418000:sregld_lit"},
  {"SPECREG(INS_IPOS.b)=lit", "dd428000:sregld_lit"},
  {"SPECREG(EXT_NXT.b)=lit", "dd410000:sregld_lit"},
  {"SPECREG(LIT_HIGH.b)=lit", "dd448000:sregld_lit"},
  {"SPECREG(EXT2_ALL.b)=lit", "dd41c000:sregld_lit"},
  {"SPECREG(INS_MODE.b)=lit", "dd434000:sregld_lit"},
  {"SPECREG(INS_INC.b)=lit", "dd42c000:sregld_lit"},
  {"SPECREG(EXT_WIDTH.b)=lit", "dd408000:sregld_lit"},
  {"SPECREG(EXT_INC.b)=lit", "dd404000:sregld_lit"},
  {"SPECREG(INS_INIT.b)=lit", "dd438000:sregld_lit"},
  {"SPECREG(INS_EPOS.c)=lit", "dd820000:sregld_lit"},
  {"SPECREG(LI.c)=lit", "dd840000:sregld_lit"},
  {"SPECREG(INS_ALL.c)=lit", "dd83c000:sregld_lit"},
  {"SPECREG(EXT_POS.c)=lit", "dd80c000:sregld_lit"},
  {"SPECREG(QMULA_SIGNED.c)=lit", "dd84c000:sregld_lit"},
  {"SPECREG(INS_WIDTH.c)=lit", "dd824000:sregld_lit"},
  {"SPECREG(EXT_INIT.c)=lit", "dd800000:sregld_lit"},
  {"SPECREG(EXT_ALL.c)=lit", "dd814000:sregld_lit"},
  {"SPECREG(INS_ADD.c)=lit", "dd830000:sregld_lit"},
  {"SPECREG(EXT_SIGNED.c)=lit", "dd818000:sregld_lit"},
  {"SPECREG(INS_IPOS.c)=lit", "dd828000:sregld_lit"},
  {"SPECREG(EXT_NXT.c)=lit", "dd810000:sregld_lit"},
  {"SPECREG(LIT_HIGH.c)=lit", "dd848000:sregld_lit"},
  {"SPECREG(EXT2_ALL.c)=lit", "dd81c000:sregld_lit"},
  {"SPECREG(INS_MODE.c)=lit", "dd834000:sregld_lit"},
  {"SPECREG(INS_INC.c)=lit", "dd82c000:sregld_lit"},
  {"SPECREG(EXT_WIDTH.c)=lit", "dd808000:sregld_lit"},
  {"SPECREG(EXT_INC.c)=lit", "dd804000:sregld_lit"},
  {"SPECREG(INS_INIT.c)=lit", "dd838000:sregld_lit"},
  {"SPECREG(INS_EPOS.d)=lit", "ddc20000:sregld_lit"},
  {"SPECREG(LI.d)=lit", "ddc40000:sregld_lit"},
  {"SPECREG(INS_ALL.d)=lit", "ddc3c000:sregld_lit"},
  {"SPECREG(EXT_POS.d)=lit", "ddc0c000:sregld_lit"},
  {"SPECREG(QMULA_SIGNED.d)=lit", "ddc4c000:sregld_lit"},
  {"SPECREG(INS_WIDTH.d)=lit", "ddc24000:sregld_lit"},
  {"SPECREG(EXT_INIT.d)=lit", "ddc00000:sregld_lit"},
  {"SPECREG(EXT_ALL.d)=lit", "ddc14000:sregld_lit"},
  {"SPECREG(INS_ADD.d)=lit", "ddc30000:sregld_lit"},
  {"SPECREG(EXT_SIGNED.d)=lit", "ddc18000:sregld_lit"},
  {"SPECREG(INS_IPOS.d)=lit", "ddc28000:sregld_lit"},
  {"SPECREG(EXT_NXT.d)=lit", "ddc10000:sregld_lit"},
  {"SPECREG(LIT_HIGH.d)=lit", "ddc48000:sregld_lit"},
  {"SPECREG(EXT2_ALL.d)=lit", "ddc1c000:sregld_lit"},
  {"SPECREG(INS_MODE.d)=lit", "ddc34000:sregld_lit"},
  {"SPECREG(INS_INC.d)=lit", "ddc2c000:sregld_lit"},
  {"SPECREG(EXT_WIDTH.d)=lit", "ddc08000:sregld_lit"},
  {"SPECREG(EXT_INC.d)=lit", "ddc04000:sregld_lit"},
  {"SPECREG(INS_INIT.d)=lit", "ddc38000:sregld_lit"},

   // Op code = 222 (0xde)
  {"reg=SPECREG(INS_EPOS)", "de020800:rw"},
  {"reg=SPECREG(LI)", "de040800:rw"},
  {"reg=SPECREG(INS_ALL)", "de03c800:rw"},
  {"reg=SPECREG(EXT_POS)", "de00c800:rw"},
  {"reg=SPECREG(QMULA_SIGNED)", "de04c800:rw"},
  {"reg=SPECREG(INS_WIDTH)", "de024800:rw"},
  {"reg=SPECREG(EXT_ALL)", "de014800:rw"},
  {"reg=SPECREG(EXT_SIGNED)", "de018800:rw"},
  {"reg=SPECREG(INS_IPOS)", "de028800:rw"},
  {"reg=SPECREG(EXT_NXT)", "de010800:rw"},
  {"reg=SPECREG(DP_NUM)", "de044800:rw"},
  {"reg=SPECREG(LIT_HIGH)", "de048800:rw"},
  {"reg=SPECREG(EXT2_ALL)", "de01c800:rw"},
  {"reg=SPECREG(INS_MODE)", "de034800:rw"},
  {"reg=SPECREG(INS_INC)", "de02c800:rw"},
  {"reg=SPECREG(EXT_WIDTH)", "de008800:rw"},
  {"reg=SPECREG(EXT_INC)", "de004800:rw"},
  {"reg.a=SPECREG(INS_EPOS.a)", "de020000:rw"},
  {"reg.a=SPECREG(LI.a)", "de040000:rw"},
  {"reg.a=SPECREG(INS_ALL.a)", "de03c000:rw"},
  {"reg.a=SPECREG(EXT_POS.a)", "de00c000:rw"},
  {"reg.a=SPECREG(QMULA_SIGNED.a)", "de04c000:rw"},
  {"reg.a=SPECREG(INS_WIDTH.a)", "de024000:rw"},
  {"reg.a=SPECREG(EXT_ALL.a)", "de014000:rw"},
  {"reg.a=SPECREG(EXT_SIGNED.a)", "de018000:rw"},
  {"reg.a=SPECREG(INS_IPOS.a)", "de028000:rw"},
  {"reg.a=SPECREG(EXT_NXT.a)", "de010000:rw"},
  {"reg.a=SPECREG(DP_NUM.a)", "de044000:rw"},
  {"reg.a=SPECREG(LIT_HIGH.a)", "de048000:rw"},
  {"reg.a=SPECREG(EXT2_ALL.a)", "de01c000:rw"},
  {"reg.a=SPECREG(INS_MODE.a)", "de034000:rw"},
  {"reg.a=SPECREG(INS_INC.a)", "de02c000:rw"},
  {"reg.a=SPECREG(EXT_WIDTH.a)", "de008000:rw"},
  {"reg.a=SPECREG(EXT_INC.a)", "de004000:rw"},
  {"reg.b=SPECREG(INS_EPOS.b)", "de021000:rw"},
  {"reg.b=SPECREG(LI.b)", "de041000:rw"},
  {"reg.b=SPECREG(INS_ALL.b)", "de03d000:rw"},
  {"reg.b=SPECREG(EXT_POS.b)", "de00d000:rw"},
  {"reg.b=SPECREG(QMULA_SIGNED.b)", "de04d000:rw"},
  {"reg.b=SPECREG(INS_WIDTH.b)", "de025000:rw"},
  {"reg.b=SPECREG(EXT_ALL.b)", "de015000:rw"},
  {"reg.b=SPECREG(EXT_SIGNED.b)", "de019000:rw"},
  {"reg.b=SPECREG(INS_IPOS.b)", "de029000:rw"},
  {"reg.b=SPECREG(EXT_NXT.b)", "de011000:rw"},
  {"reg.b=SPECREG(DP_NUM.b)", "de045000:rw"},
  {"reg.b=SPECREG(LIT_HIGH.b)", "de049000:rw"},
  {"reg.b=SPECREG(EXT2_ALL.b)", "de01d000:rw"},
  {"reg.b=SPECREG(INS_MODE.b)", "de035000:rw"},
  {"reg.b=SPECREG(INS_INC.b)", "de02d000:rw"},
  {"reg.b=SPECREG(EXT_WIDTH.b)", "de009000:rw"},
  {"reg.b=SPECREG(EXT_INC.b)", "de005000:rw"},
  {"reg.c=SPECREG(INS_EPOS.c)", "de022000:rw"},
  {"reg.c=SPECREG(LI.c)", "de042000:rw"},
  {"reg.c=SPECREG(INS_ALL.c)", "de03e000:rw"},
  {"reg.c=SPECREG(EXT_POS.c)", "de00e000:rw"},
  {"reg.c=SPECREG(QMULA_SIGNED.c)", "de04e000:rw"},
  {"reg.c=SPECREG(INS_WIDTH.c)", "de026000:rw"},
  {"reg.c=SPECREG(EXT_ALL.c)", "de016000:rw"},
  {"reg.c=SPECREG(EXT_SIGNED.c)", "de01a000:rw"},
  {"reg.c=SPECREG(INS_IPOS.c)", "de02a000:rw"},
  {"reg.c=SPECREG(EXT_NXT.c)", "de012000:rw"},
  {"reg.c=SPECREG(DP_NUM.c)", "de046000:rw"},
  {"reg.c=SPECREG(LIT_HIGH.c)", "de04a000:rw"},
  {"reg.c=SPECREG(EXT2_ALL.c)", "de01e000:rw"},
  {"reg.c=SPECREG(INS_MODE.c)", "de036000:rw"},
  {"reg.c=SPECREG(INS_INC.c)", "de02e000:rw"},
  {"reg.c=SPECREG(EXT_WIDTH.c)", "de00a000:rw"},
  {"reg.c=SPECREG(EXT_INC.c)", "de006000:rw"},
  {"reg.d=SPECREG(INS_EPOS.d)", "de023000:rw"},
  {"reg.d=SPECREG(LI.d)", "de043000:rw"},
  {"reg.d=SPECREG(INS_ALL.d)", "de03f000:rw"},
  {"reg.d=SPECREG(EXT_POS.d)", "de00f000:rw"},
  {"reg.d=SPECREG(QMULA_SIGNED.d)", "de04f000:rw"},
  {"reg.d=SPECREG(INS_WIDTH.d)", "de027000:rw"},
  {"reg.d=SPECREG(EXT_ALL.d)", "de017000:rw"},
  {"reg.d=SPECREG(EXT_SIGNED.d)", "de01b000:rw"},
  {"reg.d=SPECREG(INS_IPOS.d)", "de02b000:rw"},
  {"reg.d=SPECREG(EXT_NXT.d)", "de013000:rw"},
  {"reg.d=SPECREG(DP_NUM.d)", "de047000:rw"},
  {"reg.d=SPECREG(LIT_HIGH.d)", "de04b000:rw"},
  {"reg.d=SPECREG(EXT2_ALL.d)", "de01f000:rw"},
  {"reg.d=SPECREG(INS_MODE.d)", "de037000:rw"},
  {"reg.d=SPECREG(INS_INC.d)", "de02f000:rw"},
  {"reg.d=SPECREG(EXT_WIDTH.d)", "de00b000:rw"},
  {"reg.d=SPECREG(EXT_INC.d)", "de007000:rw"},

   // Op code = 223 (0xdf)
  {"GOTOlit", "df000000:uncond_offset"},

   // Op code = 224 (0xe0)
  {"CALL(lit)", "e0000000:uncond_offset"},

   // Op code = 225 (0xe1)
  {"RETURN", "e1000000"},
  {"RETURNI", "e1000001"},

   // Op code = 226 (0xe2)
  {"IF(reg.a==lit)GOTOlit", "e2000000:ra:br_lit:br_offset"},
  {"IF(reg.a!=lit)GOTOlit", "e2100000:ra:br_lit:br_offset"},
  {"IF(reg.a>lit)GOTOlit", "e2200000:ra:br_lit:br_offset"},
  {"IF(reg.a<lit)GOTOlit", "e2300000:ra:br_lit:br_offset"},
  {"IF(reg.b==lit)GOTOlit", "e2400000:ra:br_lit:br_offset"},
  {"IF(reg.b!=lit)GOTOlit", "e2500000:ra:br_lit:br_offset"},
  {"IF(reg.b>lit)GOTOlit", "e2600000:ra:br_lit:br_offset"},
  {"IF(reg.b<lit)GOTOlit", "e2700000:ra:br_lit:br_offset"},
  {"IF(reg.c==lit)GOTOlit", "e2800000:ra:br_lit:br_offset"},
  {"IF(reg.c!=lit)GOTOlit", "e2900000:ra:br_lit:br_offset"},
  {"IF(reg.c>lit)GOTOlit", "e2a00000:ra:br_lit:br_offset"},
  {"IF(reg.c<lit)GOTOlit", "e2b00000:ra:br_lit:br_offset"},
  {"IF(reg.d==lit)GOTOlit", "e2c00000:ra:br_lit:br_offset"},
  {"IF(reg.d!=lit)GOTOlit", "e2d00000:ra:br_lit:br_offset"},
  {"IF(reg.d>lit)GOTOlit", "e2e00000:ra:br_lit:br_offset"},
  {"IF(reg.d<lit)GOTOlit", "e2f00000:ra:br_lit:br_offset"},
  {"IF(reg==lit)GOTOlit", "e2080000:ra:br_lit:br_offset"},
  {"IF(reg!=lit)GOTOlit", "e2180000:ra:br_lit:br_offset"},
  {"IF(reg>lit)GOTOlit", "e2280000:ra:br_lit:br_offset"},
  {"IF(reg<lit)GOTOlit", "e2380000:ra:br_lit:br_offset"},

   // Op code = 227 (0xe3)
  {"IF(reg.a==lit,reg.a+=lit)GOTOlit", "e3000000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.a!=lit,reg.a+=lit)GOTOlit", "e3080000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.a>lit,reg.a+=lit)GOTOlit", "e3100000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.a>=lit,reg.a+=lit)GOTOlit", "e3180000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.a<lit,reg.a+=lit)GOTOlit", "e3200000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.a<=lit,reg.a+=lit)GOTOlit", "e3280000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.b==lit,reg.b+=lit)GOTOlit", "e3400000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.b!=lit,reg.b+=lit)GOTOlit", "e3480000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.b>lit,reg.b+=lit)GOTOlit", "e3500000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.b>=lit,reg.b+=lit)GOTOlit", "e3580000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.b<lit,reg.b+=lit)GOTOlit", "e3600000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.b<=lit,reg.b+=lit)GOTOlit", "e3680000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.c==lit,reg.c+=lit)GOTOlit", "e3800000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.c!=lit,reg.c+=lit)GOTOlit", "e3880000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.c>lit,reg.c+=lit)GOTOlit", "e3900000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.c>=lit,reg.c+=lit)GOTOlit", "e3980000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.c<lit,reg.c+=lit)GOTOlit", "e3a00000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.c<=lit,reg.c+=lit)GOTOlit", "e3a80000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.d==lit,reg.d+=lit)GOTOlit", "e3c00000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.d!=lit,reg.d+=lit)GOTOlit", "e3c80000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.d>lit,reg.d+=lit)GOTOlit", "e3d00000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.d>=lit,reg.d+=lit)GOTOlit", "e3d80000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.d<lit,reg.d+=lit)GOTOlit", "e3e00000:ra:0:ra:bri_inc:bri_offset"},
  {"IF(reg.d<=lit,reg.d+=lit)GOTOlit", "e3e80000:ra:0:ra:bri_inc:bri_offset"},

   // Op code = 228 (0xe4)
  {"CONDEX(reg==lit)", "e4000000:ra:br_lit"},
  {"CONDEX(reg!=lit)", "e4100000:ra:br_lit"},
  {"CONDEX(reg>lit)", "e4200000:ra:br_lit"},
  {"CONDEX(reg<lit)", "e4300000:ra:br_lit"},
  {"CONDEX_S(reg==lit)", "e4000001:ra:br_lit"},
  {"CONDEX_S(reg!=lit)", "e4100001:ra:br_lit"},
  {"CONDEX_S(reg>lit)", "e4200001:ra:br_lit"},
  {"CONDEX_S(reg<lit)", "e4300001:ra:br_lit"},

   // Op code = 229 (0xe5)
  {"ENDCONDEX", "e5000000"},

   // Op code = 230 (0xe6)
  {"reg.a=MEMB(areg.a+lit)", "e6000061:mem_rm:mem_ar:memli_lit"},
  {"reg.a=MEMB(areg.a)", "e6000061:mem_rm:mem_ar"},
  {"reg.b=MEMB(areg.b+lit)", "e6400061:mem_rm:mem_ar:memli_lit"},
  {"reg.b=MEMB(areg.b)", "e6400061:mem_rm:mem_ar"},
  {"reg.c=MEMB(areg.c+lit)", "e6800061:mem_rm:mem_ar:memli_lit"},
  {"reg.c=MEMB(areg.c)", "e6800061:mem_rm:mem_ar"},
  {"reg.d=MEMB(areg.d+lit)", "e6c00061:mem_rm:mem_ar:memli_lit"},
  {"reg.d=MEMB(areg.d)", "e6c00061:mem_rm:mem_ar"},
  {"reg.a=MEMB(areg.a),areg.a+=lit", "e6000021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=MEMB(areg.b),areg.b+=lit", "e6400021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=MEMB(areg.c),areg.c+=lit", "e6800021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=MEMB(areg.d),areg.d+=lit", "e6c00021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.a=MEMW(areg.a+lit)", "e6000051:mem_rm:mem_ar:memli_lit"},
  {"reg.a=MEMW(areg.a)", "e6000051:mem_rm:mem_ar"},
  {"reg.b=MEMW(areg.b+lit)", "e6400051:mem_rm:mem_ar:memli_lit"},
  {"reg.b=MEMW(areg.b)", "e6400051:mem_rm:mem_ar"},
  {"reg.c=MEMW(areg.c+lit)", "e6800051:mem_rm:mem_ar:memli_lit"},
  {"reg.c=MEMW(areg.c)", "e6800051:mem_rm:mem_ar"},
  {"reg.d=MEMW(areg.d+lit)", "e6c00051:mem_rm:mem_ar:memli_lit"},
  {"reg.d=MEMW(areg.d)", "e6c00051:mem_rm:mem_ar"},
  {"reg.a=MEMW(areg.a),areg.a+=lit", "e6000011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=MEMW(areg.b),areg.b+=lit", "e6400011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=MEMW(areg.c),areg.c+=lit", "e6800011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=MEMW(areg.d),areg.d+=lit", "e6c00011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.a=MEML(areg.a+lit)", "e6000041:mem_rm:mem_ar:memli_lit"},
  {"reg.a=MEML(areg.a)", "e6000041:mem_rm:mem_ar"},
  {"reg.b=MEML(areg.b+lit)", "e6400041:mem_rm:mem_ar:memli_lit"},
  {"reg.b=MEML(areg.b)", "e6400041:mem_rm:mem_ar"},
  {"reg.c=MEML(areg.c+lit)", "e6800041:mem_rm:mem_ar:memli_lit"},
  {"reg.c=MEML(areg.c)", "e6800041:mem_rm:mem_ar"},
  {"reg.d=MEML(areg.d+lit)", "e6c00041:mem_rm:mem_ar:memli_lit"},
  {"reg.d=MEML(areg.d)", "e6c00041:mem_rm:mem_ar"},
  {"reg.a=MEML(areg.a),areg.a+=lit", "e6000001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=MEML(areg.b),areg.b+=lit", "e6400001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=MEML(areg.c),areg.c+=lit", "e6800001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=MEML(areg.d),areg.d+=lit", "e6c00001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMB(areg.a+lit)", "e6000065:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMB(areg.a)", "e6000065:mem_rm:mem_ar"},
  {"reg=MEMB(areg.b+lit)", "e6400065:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMB(areg.b)", "e6400065:mem_rm:mem_ar"},
  {"reg=MEMB(areg.c+lit)", "e6800065:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMB(areg.c)", "e6800065:mem_rm:mem_ar"},
  {"reg=MEMB(areg.d+lit)", "e6c00065:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMB(areg.d)", "e6c00065:mem_rm:mem_ar"},
  {"reg=MEMB(areg.a),areg.a+=lit", "e6000025:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMB(areg.b),areg.b+=lit", "e6400025:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMB(areg.c),areg.c+=lit", "e6800025:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMB(areg.d),areg.d+=lit", "e6c00025:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMW(areg.a+lit)", "e6000055:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMW(areg.a)", "e6000055:mem_rm:mem_ar"},
  {"reg=MEMW(areg.b+lit)", "e6400055:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMW(areg.b)", "e6400055:mem_rm:mem_ar"},
  {"reg=MEMW(areg.c+lit)", "e6800055:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMW(areg.c)", "e6800055:mem_rm:mem_ar"},
  {"reg=MEMW(areg.d+lit)", "e6c00055:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMW(areg.d)", "e6c00055:mem_rm:mem_ar"},
  {"reg=MEMW(areg.a),areg.a+=lit", "e6000015:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMW(areg.b),areg.b+=lit", "e6400015:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMW(areg.c),areg.c+=lit", "e6800015:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMW(areg.d),areg.d+=lit", "e6c00015:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEML(areg.a+lit)", "e6000045:mem_rm:mem_ar:memli_lit"},
  {"reg=MEML(areg.a)", "e6000045:mem_rm:mem_ar"},
  {"reg=MEML(areg.b+lit)", "e6400045:mem_rm:mem_ar:memli_lit"},
  {"reg=MEML(areg.b)", "e6400045:mem_rm:mem_ar"},
  {"reg=MEML(areg.c+lit)", "e6800045:mem_rm:mem_ar:memli_lit"},
  {"reg=MEML(areg.c)", "e6800045:mem_rm:mem_ar"},
  {"reg=MEML(areg.d+lit)", "e6c00045:mem_rm:mem_ar:memli_lit"},
  {"reg=MEML(areg.d)", "e6c00045:mem_rm:mem_ar"},
  {"reg=MEML(areg.a),areg.a+=lit", "e6000005:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEML(areg.b),areg.b+=lit", "e6400005:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEML(areg.c),areg.c+=lit", "e6800005:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEML(areg.d),areg.d+=lit", "e6c00005:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMB(areg+lit)", "e6000069:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMB(areg)", "e6000069:mem_rm:mem_ar"},
  {"reg=MEMB(areg),areg+=lit", "e6000029:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMW(areg+lit)", "e6000059:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMW(areg)", "e6000059:mem_rm:mem_ar"},
  {"reg=MEMW(areg),areg+=lit", "e6000019:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEML(areg+lit)", "e6000049:mem_rm:mem_ar:memli_lit"},
  {"reg=MEML(areg)", "e6000049:mem_rm:mem_ar"},
  {"reg=MEML(areg),areg+=lit", "e6000009:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.a=MEMPB(areg.a+lit)", "e6000063:mem_rm:mem_ar:memli_lit"},
  {"reg.a=MEMPB(areg.a)", "e6000063:mem_rm:mem_ar"},
  {"reg.b=MEMPB(areg.b+lit)", "e6400063:mem_rm:mem_ar:memli_lit"},
  {"reg.b=MEMPB(areg.b)", "e6400063:mem_rm:mem_ar"},
  {"reg.c=MEMPB(areg.c+lit)", "e6800063:mem_rm:mem_ar:memli_lit"},
  {"reg.c=MEMPB(areg.c)", "e6800063:mem_rm:mem_ar"},
  {"reg.d=MEMPB(areg.d+lit)", "e6c00063:mem_rm:mem_ar:memli_lit"},
  {"reg.d=MEMPB(areg.d)", "e6c00063:mem_rm:mem_ar"},
  {"reg.a=MEMPB(areg.a),areg.a+=lit", "e6000023:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=MEMPB(areg.b),areg.b+=lit", "e6400023:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=MEMPB(areg.c),areg.c+=lit", "e6800023:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=MEMPB(areg.d),areg.d+=lit", "e6c00023:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.a=MEMPW(areg.a+lit)", "e6000053:mem_rm:mem_ar:memli_lit"},
  {"reg.a=MEMPW(areg.a)", "e6000053:mem_rm:mem_ar"},
  {"reg.b=MEMPW(areg.b+lit)", "e6400053:mem_rm:mem_ar:memli_lit"},
  {"reg.b=MEMPW(areg.b)", "e6400053:mem_rm:mem_ar"},
  {"reg.c=MEMPW(areg.c+lit)", "e6800053:mem_rm:mem_ar:memli_lit"},
  {"reg.c=MEMPW(areg.c)", "e6800053:mem_rm:mem_ar"},
  {"reg.d=MEMPW(areg.d+lit)", "e6c00053:mem_rm:mem_ar:memli_lit"},
  {"reg.d=MEMPW(areg.d)", "e6c00053:mem_rm:mem_ar"},
  {"reg.a=MEMPW(areg.a),areg.a+=lit", "e6000013:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=MEMPW(areg.b),areg.b+=lit", "e6400013:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=MEMPW(areg.c),areg.c+=lit", "e6800013:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=MEMPW(areg.d),areg.d+=lit", "e6c00013:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.a=MEMPL(areg.a+lit)", "e6000043:mem_rm:mem_ar:memli_lit"},
  {"reg.a=MEMPL(areg.a)", "e6000043:mem_rm:mem_ar"},
  {"reg.b=MEMPL(areg.b+lit)", "e6400043:mem_rm:mem_ar:memli_lit"},
  {"reg.b=MEMPL(areg.b)", "e6400043:mem_rm:mem_ar"},
  {"reg.c=MEMPL(areg.c+lit)", "e6800043:mem_rm:mem_ar:memli_lit"},
  {"reg.c=MEMPL(areg.c)", "e6800043:mem_rm:mem_ar"},
  {"reg.d=MEMPL(areg.d+lit)", "e6c00043:mem_rm:mem_ar:memli_lit"},
  {"reg.d=MEMPL(areg.d)", "e6c00043:mem_rm:mem_ar"},
  {"reg.a=MEMPL(areg.a),areg.a+=lit", "e6000003:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=MEMPL(areg.b),areg.b+=lit", "e6400003:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=MEMPL(areg.c),areg.c+=lit", "e6800003:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=MEMPL(areg.d),areg.d+=lit", "e6c00003:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.a+lit)", "e6000067:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.a)", "e6000067:mem_rm:mem_ar"},
  {"reg=MEMPB(areg.b+lit)", "e6400067:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.b)", "e6400067:mem_rm:mem_ar"},
  {"reg=MEMPB(areg.c+lit)", "e6800067:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.c)", "e6800067:mem_rm:mem_ar"},
  {"reg=MEMPB(areg.d+lit)", "e6c00067:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.d)", "e6c00067:mem_rm:mem_ar"},
  {"reg=MEMPB(areg.a),areg.a+=lit", "e6000027:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.b),areg.b+=lit", "e6400027:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.c),areg.c+=lit", "e6800027:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPB(areg.d),areg.d+=lit", "e6c00027:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.a+lit)", "e6000057:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.a)", "e6000057:mem_rm:mem_ar"},
  {"reg=MEMPW(areg.b+lit)", "e6400057:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.b)", "e6400057:mem_rm:mem_ar"},
  {"reg=MEMPW(areg.c+lit)", "e6800057:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.c)", "e6800057:mem_rm:mem_ar"},
  {"reg=MEMPW(areg.d+lit)", "e6c00057:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.d)", "e6c00057:mem_rm:mem_ar"},
  {"reg=MEMPW(areg.a),areg.a+=lit", "e6000017:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.b),areg.b+=lit", "e6400017:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.c),areg.c+=lit", "e6800017:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPW(areg.d),areg.d+=lit", "e6c00017:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.a+lit)", "e6000047:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.a)", "e6000047:mem_rm:mem_ar"},
  {"reg=MEMPL(areg.b+lit)", "e6400047:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.b)", "e6400047:mem_rm:mem_ar"},
  {"reg=MEMPL(areg.c+lit)", "e6800047:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.c)", "e6800047:mem_rm:mem_ar"},
  {"reg=MEMPL(areg.d+lit)", "e6c00047:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.d)", "e6c00047:mem_rm:mem_ar"},
  {"reg=MEMPL(areg.a),areg.a+=lit", "e6000007:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.b),areg.b+=lit", "e6400007:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.c),areg.c+=lit", "e6800007:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPL(areg.d),areg.d+=lit", "e6c00007:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPB(areg+lit)", "e600006b:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPB(areg)", "e600006b:mem_rm:mem_ar"},
  {"reg=MEMPB(areg),areg+=lit", "e600002b:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPW(areg+lit)", "e600005b:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPW(areg)", "e600005b:mem_rm:mem_ar"},
  {"reg=MEMPW(areg),areg+=lit", "e600001b:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=MEMPL(areg+lit)", "e600004b:mem_rm:mem_ar:memli_lit"},
  {"reg=MEMPL(areg)", "e600004b:mem_rm:mem_ar"},
  {"reg=MEMPL(areg),areg+=lit", "e600000b:mem_rm:mem_ar:mem_ar:memli_lit"},

   // Op code = 231 (0xe7)
  {"reg.a=MEMB(areg.a+reg.a)", "e7000061:mem_rm:mem_ar:rb"},
  {"reg.b=MEMB(areg.b+reg.b)", "e7400061:mem_rm:mem_ar:rb"},
  {"reg.c=MEMB(areg.c+reg.c)", "e7800061:mem_rm:mem_ar:rb"},
  {"reg.d=MEMB(areg.d+reg.d)", "e7c00061:mem_rm:mem_ar:rb"},
  {"reg.a=MEMB(areg.a),areg.a+=ireg.a", "e7000021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=MEMB(areg.b),areg.b+=ireg.b", "e7400021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=MEMB(areg.c),areg.c+=ireg.c", "e7800021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=MEMB(areg.d),areg.d+=ireg.d", "e7c00021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.a=MEMW(areg.a+reg.a)", "e7000051:mem_rm:mem_ar:rb"},
  {"reg.b=MEMW(areg.b+reg.b)", "e7400051:mem_rm:mem_ar:rb"},
  {"reg.c=MEMW(areg.c+reg.c)", "e7800051:mem_rm:mem_ar:rb"},
  {"reg.d=MEMW(areg.d+reg.d)", "e7c00051:mem_rm:mem_ar:rb"},
  {"reg.a=MEMW(areg.a),areg.a+=ireg.a", "e7000011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=MEMW(areg.b),areg.b+=ireg.b", "e7400011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=MEMW(areg.c),areg.c+=ireg.c", "e7800011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=MEMW(areg.d),areg.d+=ireg.d", "e7c00011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.a=MEML(areg.a+reg.a)", "e7000041:mem_rm:mem_ar:rb"},
  {"reg.b=MEML(areg.b+reg.b)", "e7400041:mem_rm:mem_ar:rb"},
  {"reg.c=MEML(areg.c+reg.c)", "e7800041:mem_rm:mem_ar:rb"},
  {"reg.d=MEML(areg.d+reg.d)", "e7c00041:mem_rm:mem_ar:rb"},
  {"reg.a=MEML(areg.a),areg.a+=ireg.a", "e7000001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=MEML(areg.b),areg.b+=ireg.b", "e7400001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=MEML(areg.c),areg.c+=ireg.c", "e7800001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=MEML(areg.d),areg.d+=ireg.d", "e7c00001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMB(areg.a+reg.a)", "e7000065:mem_rm:mem_ar:rb"},
  {"reg=MEMB(areg.b+reg.b)", "e7400065:mem_rm:mem_ar:rb"},
  {"reg=MEMB(areg.c+reg.c)", "e7800065:mem_rm:mem_ar:rb"},
  {"reg=MEMB(areg.d+reg.d)", "e7c00065:mem_rm:mem_ar:rb"},
  {"reg=MEMB(areg.a),areg.a+=ireg.a", "e7000025:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMB(areg.b),areg.b+=ireg.b", "e7400025:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMB(areg.c),areg.c+=ireg.c", "e7800025:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMB(areg.d),areg.d+=ireg.d", "e7c00025:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMW(areg.a+reg.a)", "e7000055:mem_rm:mem_ar:rb"},
  {"reg=MEMW(areg.b+reg.b)", "e7400055:mem_rm:mem_ar:rb"},
  {"reg=MEMW(areg.c+reg.c)", "e7800055:mem_rm:mem_ar:rb"},
  {"reg=MEMW(areg.d+reg.d)", "e7c00055:mem_rm:mem_ar:rb"},
  {"reg=MEMW(areg.a),areg.a+=ireg.a", "e7000015:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMW(areg.b),areg.b+=ireg.b", "e7400015:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMW(areg.c),areg.c+=ireg.c", "e7800015:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMW(areg.d),areg.d+=ireg.d", "e7c00015:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEML(areg.a+reg.a)", "e7000045:mem_rm:mem_ar:rb"},
  {"reg=MEML(areg.b+reg.b)", "e7400045:mem_rm:mem_ar:rb"},
  {"reg=MEML(areg.c+reg.c)", "e7800045:mem_rm:mem_ar:rb"},
  {"reg=MEML(areg.d+reg.d)", "e7c00045:mem_rm:mem_ar:rb"},
  {"reg=MEML(areg.a),areg.a+=ireg.a", "e7000005:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEML(areg.b),areg.b+=ireg.b", "e7400005:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEML(areg.c),areg.c+=ireg.c", "e7800005:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEML(areg.d),areg.d+=ireg.d", "e7c00005:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMB(areg+reg)", "e7000069:mem_rm:mem_ar:rb"},
  {"reg=MEMB(areg),areg+=ireg", "e7000029:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMW(areg+reg)", "e7000059:mem_rm:mem_ar:rb"},
  {"reg=MEMW(areg),areg+=ireg", "e7000019:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEML(areg+reg)", "e7000049:mem_rm:mem_ar:rb"},
  {"reg=MEML(areg),areg+=ireg", "e7000009:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.a=MEMPB(areg.a+reg.a)", "e7000063:mem_rm:mem_ar:rb"},
  {"reg.b=MEMPB(areg.b+reg.b)", "e7400063:mem_rm:mem_ar:rb"},
  {"reg.c=MEMPB(areg.c+reg.c)", "e7800063:mem_rm:mem_ar:rb"},
  {"reg.d=MEMPB(areg.d+reg.d)", "e7c00063:mem_rm:mem_ar:rb"},
  {"reg.a=MEMPB(areg.a),areg.a+=ireg.a", "e7000023:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=MEMPB(areg.b),areg.b+=ireg.b", "e7400023:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=MEMPB(areg.c),areg.c+=ireg.c", "e7800023:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=MEMPB(areg.d),areg.d+=ireg.d", "e7c00023:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.a=MEMPW(areg.a+reg.a)", "e7000053:mem_rm:mem_ar:rb"},
  {"reg.b=MEMPW(areg.b+reg.b)", "e7400053:mem_rm:mem_ar:rb"},
  {"reg.c=MEMPW(areg.c+reg.c)", "e7800053:mem_rm:mem_ar:rb"},
  {"reg.d=MEMPW(areg.d+reg.d)", "e7c00053:mem_rm:mem_ar:rb"},
  {"reg.a=MEMPW(areg.a),areg.a+=ireg.a", "e7000013:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=MEMPW(areg.b),areg.b+=ireg.b", "e7400013:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=MEMPW(areg.c),areg.c+=ireg.c", "e7800013:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=MEMPW(areg.d),areg.d+=ireg.d", "e7c00013:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.a=MEMPL(areg.a+reg.a)", "e7000043:mem_rm:mem_ar:rb"},
  {"reg.b=MEMPL(areg.b+reg.b)", "e7400043:mem_rm:mem_ar:rb"},
  {"reg.c=MEMPL(areg.c+reg.c)", "e7800043:mem_rm:mem_ar:rb"},
  {"reg.d=MEMPL(areg.d+reg.d)", "e7c00043:mem_rm:mem_ar:rb"},
  {"reg.a=MEMPL(areg.a),areg.a+=ireg.a", "e7000003:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=MEMPL(areg.b),areg.b+=ireg.b", "e7400003:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=MEMPL(areg.c),areg.c+=ireg.c", "e7800003:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=MEMPL(areg.d),areg.d+=ireg.d", "e7c00003:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPB(areg.a+reg.a)", "e7000067:mem_rm:mem_ar:rb"},
  {"reg=MEMPB(areg.b+reg.b)", "e7400067:mem_rm:mem_ar:rb"},
  {"reg=MEMPB(areg.c+reg.c)", "e7800067:mem_rm:mem_ar:rb"},
  {"reg=MEMPB(areg.d+reg.d)", "e7c00067:mem_rm:mem_ar:rb"},
  {"reg=MEMPB(areg.a),areg.a+=ireg.a", "e7000027:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPB(areg.b),areg.b+=ireg.b", "e7400027:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPB(areg.c),areg.c+=ireg.c", "e7800027:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPB(areg.d),areg.d+=ireg.d", "e7c00027:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPW(areg.a+reg.a)", "e7000057:mem_rm:mem_ar:rb"},
  {"reg=MEMPW(areg.b+reg.b)", "e7400057:mem_rm:mem_ar:rb"},
  {"reg=MEMPW(areg.c+reg.c)", "e7800057:mem_rm:mem_ar:rb"},
  {"reg=MEMPW(areg.d+reg.d)", "e7c00057:mem_rm:mem_ar:rb"},
  {"reg=MEMPW(areg.a),areg.a+=ireg.a", "e7000017:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPW(areg.b),areg.b+=ireg.b", "e7400017:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPW(areg.c),areg.c+=ireg.c", "e7800017:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPW(areg.d),areg.d+=ireg.d", "e7c00017:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPL(areg.a+reg.a)", "e7000047:mem_rm:mem_ar:rb"},
  {"reg=MEMPL(areg.b+reg.b)", "e7400047:mem_rm:mem_ar:rb"},
  {"reg=MEMPL(areg.c+reg.c)", "e7800047:mem_rm:mem_ar:rb"},
  {"reg=MEMPL(areg.d+reg.d)", "e7c00047:mem_rm:mem_ar:rb"},
  {"reg=MEMPL(areg.a),areg.a+=ireg.a", "e7000007:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPL(areg.b),areg.b+=ireg.b", "e7400007:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPL(areg.c),areg.c+=ireg.c", "e7800007:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPL(areg.d),areg.d+=ireg.d", "e7c00007:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPB(areg+reg)", "e700006b:mem_rm:mem_ar:rb"},
  {"reg=MEMPB(areg),areg+=ireg", "e700002b:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPW(areg+reg)", "e700005b:mem_rm:mem_ar:rb"},
  {"reg=MEMPW(areg),areg+=ireg", "e700001b:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=MEMPL(areg+reg)", "e700004b:mem_rm:mem_ar:rb"},
  {"reg=MEMPL(areg),areg+=ireg", "e700000b:mem_rm:mem_ar:mem_ar:mem_ir"},

   // Op code = 232 (0xe8)
  {"reg.a=MEML(lit)", "e8000000:mem_rm:memlit"},
  {"reg.b=MEML(lit)", "e8400000:mem_rm:memlit"},
  {"reg.c=MEML(lit)", "e8800000:mem_rm:memlit"},
  {"reg.d=MEML(lit)", "e8c00000:mem_rm:memlit"},
  {"reg=MEMPL(lit)", "e8000008:mem_rm:memlit"},

   // Op code = 233 (0xe9)
  {"MEMB(areg.a+lit)=reg.a", "e9000061:mem_ar:memli_lit:ra"},
  {"MEMB(areg.a)=reg.a", "e9000061:mem_ar:ra"},
  {"MEMB(areg.b+lit)=reg.b", "e9400061:mem_ar:memli_lit:ra"},
  {"MEMB(areg.b)=reg.b", "e9400061:mem_ar:ra"},
  {"MEMB(areg.c+lit)=reg.c", "e9800061:mem_ar:memli_lit:ra"},
  {"MEMB(areg.c)=reg.c", "e9800061:mem_ar:ra"},
  {"MEMB(areg.d+lit)=reg.d", "e9c00061:mem_ar:memli_lit:ra"},
  {"MEMB(areg.d)=reg.d", "e9c00061:mem_ar:ra"},
  {"MEMB(areg.a)=reg.a,areg.a+=lit", "e9000021:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.a=BYTE(areg.a+lit)", "e9000020:mem_ar:mem_ar:memli_lit"},
  {"MEMB(areg.b)=reg.b,areg.b+=lit", "e9400021:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.b=BYTE(areg.b+lit)", "e9400020:mem_ar:mem_ar:memli_lit"},
  {"MEMB(areg.c)=reg.c,areg.c+=lit", "e9800021:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.c=BYTE(areg.c+lit)", "e9800020:mem_ar:mem_ar:memli_lit"},
  {"MEMB(areg.d)=reg.d,areg.d+=lit", "e9c00021:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.d=BYTE(areg.d+lit)", "e9c00020:mem_ar:mem_ar:memli_lit"},
  {"MEMW(areg.a+lit)=reg.a", "e9000051:mem_ar:memli_lit:ra"},
  {"MEMW(areg.a)=reg.a", "e9000051:mem_ar:ra"},
  {"MEMW(areg.b+lit)=reg.b", "e9400051:mem_ar:memli_lit:ra"},
  {"MEMW(areg.b)=reg.b", "e9400051:mem_ar:ra"},
  {"MEMW(areg.c+lit)=reg.c", "e9800051:mem_ar:memli_lit:ra"},
  {"MEMW(areg.c)=reg.c", "e9800051:mem_ar:ra"},
  {"MEMW(areg.d+lit)=reg.d", "e9c00051:mem_ar:memli_lit:ra"},
  {"MEMW(areg.d)=reg.d", "e9c00051:mem_ar:ra"},
  {"MEMW(areg.a)=reg.a,areg.a+=lit", "e9000011:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.a=WORD(areg.a+lit)", "e9000010:mem_ar:mem_ar:memli_lit"},
  {"MEMW(areg.b)=reg.b,areg.b+=lit", "e9400011:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.b=WORD(areg.b+lit)", "e9400010:mem_ar:mem_ar:memli_lit"},
  {"MEMW(areg.c)=reg.c,areg.c+=lit", "e9800011:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.c=WORD(areg.c+lit)", "e9800010:mem_ar:mem_ar:memli_lit"},
  {"MEMW(areg.d)=reg.d,areg.d+=lit", "e9c00011:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.d=WORD(areg.d+lit)", "e9c00010:mem_ar:mem_ar:memli_lit"},
  {"MEML(areg.a+lit)=reg.a", "e9000041:mem_ar:memli_lit:ra"},
  {"MEML(areg.a)=reg.a", "e9000041:mem_ar:ra"},
  {"MEML(areg.b+lit)=reg.b", "e9400041:mem_ar:memli_lit:ra"},
  {"MEML(areg.b)=reg.b", "e9400041:mem_ar:ra"},
  {"MEML(areg.c+lit)=reg.c", "e9800041:mem_ar:memli_lit:ra"},
  {"MEML(areg.c)=reg.c", "e9800041:mem_ar:ra"},
  {"MEML(areg.d+lit)=reg.d", "e9c00041:mem_ar:memli_lit:ra"},
  {"MEML(areg.d)=reg.d", "e9c00041:mem_ar:ra"},
  {"MEML(areg.a)=reg.a,areg.a+=lit", "e9000001:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.a=LONG(areg.a+lit)", "e9000000:mem_ar:mem_ar:memli_lit"},
  {"MEML(areg.b)=reg.b,areg.b+=lit", "e9400001:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.b=LONG(areg.b+lit)", "e9400000:mem_ar:mem_ar:memli_lit"},
  {"MEML(areg.c)=reg.c,areg.c+=lit", "e9800001:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.c=LONG(areg.c+lit)", "e9800000:mem_ar:mem_ar:memli_lit"},
  {"MEML(areg.d)=reg.d,areg.d+=lit", "e9c00001:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.d=LONG(areg.d+lit)", "e9c00000:mem_ar:mem_ar:memli_lit"},
  {"MEMB(areg+lit)=reg", "e9000069:mem_ar:memli_lit:ra"},
  {"MEMB(areg)=reg", "e9000069:mem_ar:ra"},
  {"MEMB(areg)=reg,areg+=lit", "e9000029:mem_ar:ra:mem_ar:memli_lit"},
  {"areg=BYTE(areg+lit)", "e9000028:mem_ar:mem_ar:memli_lit"},
  {"MEMW(areg+lit)=reg", "e9000059:mem_ar:memli_lit:ra"},
  {"MEMW(areg)=reg", "e9000059:mem_ar:ra"},
  {"MEMW(areg)=reg,areg+=lit", "e9000019:mem_ar:ra:mem_ar:memli_lit"},
  {"areg=WORD(areg+lit)", "e9000018:mem_ar:mem_ar:memli_lit"},
  {"MEML(areg+lit)=reg", "e9000049:mem_ar:memli_lit:ra"},
  {"MEML(areg)=reg", "e9000049:mem_ar:ra"},
  {"MEML(areg)=reg,areg+=lit", "e9000009:mem_ar:ra:mem_ar:memli_lit"},
  {"areg=LONG(areg+lit)", "e9000008:mem_ar:mem_ar:memli_lit"},
  {"MEMPB(areg.a+lit)=reg.a", "e9000063:mem_ar:memli_lit:ra"},
  {"MEMPB(areg.a)=reg.a", "e9000063:mem_ar:ra"},
  {"MEMPB(areg.b+lit)=reg.b", "e9400063:mem_ar:memli_lit:ra"},
  {"MEMPB(areg.b)=reg.b", "e9400063:mem_ar:ra"},
  {"MEMPB(areg.c+lit)=reg.c", "e9800063:mem_ar:memli_lit:ra"},
  {"MEMPB(areg.c)=reg.c", "e9800063:mem_ar:ra"},
  {"MEMPB(areg.d+lit)=reg.d", "e9c00063:mem_ar:memli_lit:ra"},
  {"MEMPB(areg.d)=reg.d", "e9c00063:mem_ar:ra"},
  {"MEMPB(areg.a)=reg.a,areg.a+=lit", "e9000023:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.a=PBYTE(areg.a+lit)", "e9000022:mem_ar:mem_ar:memli_lit"},
  {"MEMPB(areg.b)=reg.b,areg.b+=lit", "e9400023:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.b=PBYTE(areg.b+lit)", "e9400022:mem_ar:mem_ar:memli_lit"},
  {"MEMPB(areg.c)=reg.c,areg.c+=lit", "e9800023:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.c=PBYTE(areg.c+lit)", "e9800022:mem_ar:mem_ar:memli_lit"},
  {"MEMPB(areg.d)=reg.d,areg.d+=lit", "e9c00023:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.d=PBYTE(areg.d+lit)", "e9c00022:mem_ar:mem_ar:memli_lit"},
  {"MEMPW(areg.a+lit)=reg.a", "e9000053:mem_ar:memli_lit:ra"},
  {"MEMPW(areg.a)=reg.a", "e9000053:mem_ar:ra"},
  {"MEMPW(areg.b+lit)=reg.b", "e9400053:mem_ar:memli_lit:ra"},
  {"MEMPW(areg.b)=reg.b", "e9400053:mem_ar:ra"},
  {"MEMPW(areg.c+lit)=reg.c", "e9800053:mem_ar:memli_lit:ra"},
  {"MEMPW(areg.c)=reg.c", "e9800053:mem_ar:ra"},
  {"MEMPW(areg.d+lit)=reg.d", "e9c00053:mem_ar:memli_lit:ra"},
  {"MEMPW(areg.d)=reg.d", "e9c00053:mem_ar:ra"},
  {"MEMPW(areg.a)=reg.a,areg.a+=lit", "e9000013:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.a=PWORD(areg.a+lit)", "e9000012:mem_ar:mem_ar:memli_lit"},
  {"MEMPW(areg.b)=reg.b,areg.b+=lit", "e9400013:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.b=PWORD(areg.b+lit)", "e9400012:mem_ar:mem_ar:memli_lit"},
  {"MEMPW(areg.c)=reg.c,areg.c+=lit", "e9800013:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.c=PWORD(areg.c+lit)", "e9800012:mem_ar:mem_ar:memli_lit"},
  {"MEMPW(areg.d)=reg.d,areg.d+=lit", "e9c00013:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.d=PWORD(areg.d+lit)", "e9c00012:mem_ar:mem_ar:memli_lit"},
  {"MEMPL(areg.a+lit)=reg.a", "e9000043:mem_ar:memli_lit:ra"},
  {"MEMPL(areg.a)=reg.a", "e9000043:mem_ar:ra"},
  {"MEMPL(areg.b+lit)=reg.b", "e9400043:mem_ar:memli_lit:ra"},
  {"MEMPL(areg.b)=reg.b", "e9400043:mem_ar:ra"},
  {"MEMPL(areg.c+lit)=reg.c", "e9800043:mem_ar:memli_lit:ra"},
  {"MEMPL(areg.c)=reg.c", "e9800043:mem_ar:ra"},
  {"MEMPL(areg.d+lit)=reg.d", "e9c00043:mem_ar:memli_lit:ra"},
  {"MEMPL(areg.d)=reg.d", "e9c00043:mem_ar:ra"},
  {"MEMPL(areg.a)=reg.a,areg.a+=lit", "e9000003:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.a=PLONG(areg.a+lit)", "e9000002:mem_ar:mem_ar:memli_lit"},
  {"MEMPL(areg.b)=reg.b,areg.b+=lit", "e9400003:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.b=PLONG(areg.b+lit)", "e9400002:mem_ar:mem_ar:memli_lit"},
  {"MEMPL(areg.c)=reg.c,areg.c+=lit", "e9800003:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.c=PLONG(areg.c+lit)", "e9800002:mem_ar:mem_ar:memli_lit"},
  {"MEMPL(areg.d)=reg.d,areg.d+=lit", "e9c00003:mem_ar:ra:mem_ar:memli_lit"},
  {"areg.d=PLONG(areg.d+lit)", "e9c00002:mem_ar:mem_ar:memli_lit"},
  {"MEMPB(areg+lit)=reg", "e900006b:mem_ar:memli_lit:ra"},
  {"MEMPB(areg)=reg", "e900006b:mem_ar:ra"},
  {"MEMPB(areg)=reg,areg+=lit", "e900002b:mem_ar:ra:mem_ar:memli_lit"},
  {"areg=PBYTE(areg+lit)", "e900002a:mem_ar:mem_ar:memli_lit"},
  {"MEMPW(areg+lit)=reg", "e900005b:mem_ar:memli_lit:ra"},
  {"MEMPW(areg)=reg", "e900005b:mem_ar:ra"},
  {"MEMPW(areg)=reg,areg+=lit", "e900001b:mem_ar:ra:mem_ar:memli_lit"},
  {"areg=PWORD(areg+lit)", "e900001a:mem_ar:mem_ar:memli_lit"},
  {"MEMPL(areg+lit)=reg", "e900004b:mem_ar:memli_lit:ra"},
  {"MEMPL(areg)=reg", "e900004b:mem_ar:ra"},
  {"MEMPL(areg)=reg,areg+=lit", "e900000b:mem_ar:ra:mem_ar:memli_lit"},
  {"areg=PLONG(areg+lit)", "e900000a:mem_ar:mem_ar:memli_lit"},

   // Op code = 234 (0xea)
  {"MEMB(areg.a+reg.a)=reg.a", "ea000061:mem_ar:rb:ra"},
  {"MEMB(areg.b+reg.b)=reg.b", "ea400061:mem_ar:rb:ra"},
  {"MEMB(areg.c+reg.c)=reg.c", "ea800061:mem_ar:rb:ra"},
  {"MEMB(areg.d+reg.d)=reg.d", "eac00061:mem_ar:rb:ra"},
  {"MEMB(areg.a)=reg.a,areg.a+=ireg.a", "ea000021:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.a=BYTE(areg.a+ireg.a)", "ea000020:mem_ar:mem_ar:mem_ir"},
  {"MEMB(areg.b)=reg.b,areg.b+=ireg.b", "ea400021:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.b=BYTE(areg.b+ireg.b)", "ea400020:mem_ar:mem_ar:mem_ir"},
  {"MEMB(areg.c)=reg.c,areg.c+=ireg.c", "ea800021:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.c=BYTE(areg.c+ireg.c)", "ea800020:mem_ar:mem_ar:mem_ir"},
  {"MEMB(areg.d)=reg.d,areg.d+=ireg.d", "eac00021:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.d=BYTE(areg.d+ireg.d)", "eac00020:mem_ar:mem_ar:mem_ir"},
  {"MEMW(areg.a+reg.a)=reg.a", "ea000051:mem_ar:rb:ra"},
  {"MEMW(areg.b+reg.b)=reg.b", "ea400051:mem_ar:rb:ra"},
  {"MEMW(areg.c+reg.c)=reg.c", "ea800051:mem_ar:rb:ra"},
  {"MEMW(areg.d+reg.d)=reg.d", "eac00051:mem_ar:rb:ra"},
  {"MEMW(areg.a)=reg.a,areg.a+=ireg.a", "ea000011:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.a=WORD(areg.a+ireg.a)", "ea000010:mem_ar:mem_ar:mem_ir"},
  {"MEMW(areg.b)=reg.b,areg.b+=ireg.b", "ea400011:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.b=WORD(areg.b+ireg.b)", "ea400010:mem_ar:mem_ar:mem_ir"},
  {"MEMW(areg.c)=reg.c,areg.c+=ireg.c", "ea800011:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.c=WORD(areg.c+ireg.c)", "ea800010:mem_ar:mem_ar:mem_ir"},
  {"MEMW(areg.d)=reg.d,areg.d+=ireg.d", "eac00011:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.d=WORD(areg.d+ireg.d)", "eac00010:mem_ar:mem_ar:mem_ir"},
  {"MEML(areg.a+reg.a)=reg.a", "ea000041:mem_ar:rb:ra"},
  {"MEML(areg.b+reg.b)=reg.b", "ea400041:mem_ar:rb:ra"},
  {"MEML(areg.c+reg.c)=reg.c", "ea800041:mem_ar:rb:ra"},
  {"MEML(areg.d+reg.d)=reg.d", "eac00041:mem_ar:rb:ra"},
  {"MEML(areg.a)=reg.a,areg.a+=ireg.a", "ea000001:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.a=LONG(areg.a+ireg.a)", "ea000000:mem_ar:mem_ar:mem_ir"},
  {"MEML(areg.b)=reg.b,areg.b+=ireg.b", "ea400001:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.b=LONG(areg.b+ireg.b)", "ea400000:mem_ar:mem_ar:mem_ir"},
  {"MEML(areg.c)=reg.c,areg.c+=ireg.c", "ea800001:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.c=LONG(areg.c+ireg.c)", "ea800000:mem_ar:mem_ar:mem_ir"},
  {"MEML(areg.d)=reg.d,areg.d+=ireg.d", "eac00001:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.d=LONG(areg.d+ireg.d)", "eac00000:mem_ar:mem_ar:mem_ir"},
  {"MEMB(areg+reg)=reg", "ea000069:mem_ar:rb:ra"},
  {"MEMB(areg)=reg,areg+=ireg", "ea000029:mem_ar:ra:mem_ar:mem_ir"},
  {"areg=BYTE(areg+ireg)", "ea000028:mem_ar:mem_ar:mem_ir"},
  {"MEMW(areg+reg)=reg", "ea000059:mem_ar:rb:ra"},
  {"MEMW(areg)=reg,areg+=ireg", "ea000019:mem_ar:ra:mem_ar:mem_ir"},
  {"areg=WORD(areg+ireg)", "ea000018:mem_ar:mem_ar:mem_ir"},
  {"MEML(areg+reg)=reg", "ea000049:mem_ar:rb:ra"},
  {"MEML(areg)=reg,areg+=ireg", "ea000009:mem_ar:ra:mem_ar:mem_ir"},
  {"areg=LONG(areg+ireg)", "ea000008:mem_ar:mem_ar:mem_ir"},
  {"MEMPB(areg.a+reg.a)=reg.a", "ea000063:mem_ar:rb:ra"},
  {"MEMPB(areg.b+reg.b)=reg.b", "ea400063:mem_ar:rb:ra"},
  {"MEMPB(areg.c+reg.c)=reg.c", "ea800063:mem_ar:rb:ra"},
  {"MEMPB(areg.d+reg.d)=reg.d", "eac00063:mem_ar:rb:ra"},
  {"MEMPB(areg.a)=reg.a,areg.a+=ireg.a", "ea000023:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.a=PBYTE(areg.a+ireg.a)", "ea000022:mem_ar:mem_ar:mem_ir"},
  {"MEMPB(areg.b)=reg.b,areg.b+=ireg.b", "ea400023:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.b=PBYTE(areg.b+ireg.b)", "ea400022:mem_ar:mem_ar:mem_ir"},
  {"MEMPB(areg.c)=reg.c,areg.c+=ireg.c", "ea800023:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.c=PBYTE(areg.c+ireg.c)", "ea800022:mem_ar:mem_ar:mem_ir"},
  {"MEMPB(areg.d)=reg.d,areg.d+=ireg.d", "eac00023:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.d=PBYTE(areg.d+ireg.d)", "eac00022:mem_ar:mem_ar:mem_ir"},
  {"MEMPW(areg.a+reg.a)=reg.a", "ea000053:mem_ar:rb:ra"},
  {"MEMPW(areg.b+reg.b)=reg.b", "ea400053:mem_ar:rb:ra"},
  {"MEMPW(areg.c+reg.c)=reg.c", "ea800053:mem_ar:rb:ra"},
  {"MEMPW(areg.d+reg.d)=reg.d", "eac00053:mem_ar:rb:ra"},
  {"MEMPW(areg.a)=reg.a,areg.a+=ireg.a", "ea000013:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.a=PWORD(areg.a+ireg.a)", "ea000012:mem_ar:mem_ar:mem_ir"},
  {"MEMPW(areg.b)=reg.b,areg.b+=ireg.b", "ea400013:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.b=PWORD(areg.b+ireg.b)", "ea400012:mem_ar:mem_ar:mem_ir"},
  {"MEMPW(areg.c)=reg.c,areg.c+=ireg.c", "ea800013:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.c=PWORD(areg.c+ireg.c)", "ea800012:mem_ar:mem_ar:mem_ir"},
  {"MEMPW(areg.d)=reg.d,areg.d+=ireg.d", "eac00013:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.d=PWORD(areg.d+ireg.d)", "eac00012:mem_ar:mem_ar:mem_ir"},
  {"MEMPL(areg.a+reg.a)=reg.a", "ea000043:mem_ar:rb:ra"},
  {"MEMPL(areg.b+reg.b)=reg.b", "ea400043:mem_ar:rb:ra"},
  {"MEMPL(areg.c+reg.c)=reg.c", "ea800043:mem_ar:rb:ra"},
  {"MEMPL(areg.d+reg.d)=reg.d", "eac00043:mem_ar:rb:ra"},
  {"MEMPL(areg.a)=reg.a,areg.a+=ireg.a", "ea000003:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.a=PLONG(areg.a+ireg.a)", "ea000002:mem_ar:mem_ar:mem_ir"},
  {"MEMPL(areg.b)=reg.b,areg.b+=ireg.b", "ea400003:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.b=PLONG(areg.b+ireg.b)", "ea400002:mem_ar:mem_ar:mem_ir"},
  {"MEMPL(areg.c)=reg.c,areg.c+=ireg.c", "ea800003:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.c=PLONG(areg.c+ireg.c)", "ea800002:mem_ar:mem_ar:mem_ir"},
  {"MEMPL(areg.d)=reg.d,areg.d+=ireg.d", "eac00003:mem_ar:ra:mem_ar:mem_ir"},
  {"areg.d=PLONG(areg.d+ireg.d)", "eac00002:mem_ar:mem_ar:mem_ir"},
  {"MEMPB(areg+reg)=reg", "ea00006b:mem_ar:rb:ra"},
  {"MEMPB(areg)=reg,areg+=ireg", "ea00002b:mem_ar:ra:mem_ar:mem_ir"},
  {"areg=PBYTE(areg+ireg)", "ea00002a:mem_ar:mem_ar:mem_ir"},
  {"MEMPW(areg+reg)=reg", "ea00005b:mem_ar:rb:ra"},
  {"MEMPW(areg)=reg,areg+=ireg", "ea00001b:mem_ar:ra:mem_ar:mem_ir"},
  {"areg=PWORD(areg+ireg)", "ea00001a:mem_ar:mem_ar:mem_ir"},
  {"MEMPL(areg+reg)=reg", "ea00004b:mem_ar:rb:ra"},
  {"MEMPL(areg)=reg,areg+=ireg", "ea00000b:mem_ar:ra:mem_ar:mem_ir"},
  {"areg=PLONG(areg+ireg)", "ea00000a:mem_ar:mem_ar:mem_ir"},

   // Op code = 235 (0xeb)
  {"MEML(lit)=reg.a", "eb000000:memlit:ra"},
  {"MEML(lit)=reg.b", "eb400000:memlit:ra"},
  {"MEML(lit)=reg.c", "eb800000:memlit:ra"},
  {"MEML(lit)=reg.d", "ebc00000:memlit:ra"},
  {"MEMPL(lit)=reg", "eb000008:memlit:ra"},

   // Op code = 236 (0xec)
  {"areg.a=reg.a", "ec000000:ar_ldst:rb"},
  {"ireg.a=reg.a", "ec020000:ir_ldst:rb"},
  {"areg.b=reg.b", "ec400000:ar_ldst:rb"},
  {"ireg.b=reg.b", "ec420000:ir_ldst:rb"},
  {"areg.c=reg.c", "ec800000:ar_ldst:rb"},
  {"ireg.c=reg.c", "ec820000:ir_ldst:rb"},
  {"areg.d=reg.d", "ecc00000:ar_ldst:rb"},
  {"ireg.d=reg.d", "ecc20000:ir_ldst:rb"},
  {"areg=reg", "ec200000:ar_ldst:rb"},
  {"ireg=reg", "ec220000:ir_ldst:rb"},

   // Op code = 237 (0xed)
  {"reg.a=areg.a", "ed000000:rw:ar_ldst"},
  {"reg.a=ireg.a", "ed020000:rw:ir_ldst"},
  {"reg.b=areg.b", "ed001000:rw:ar_ldst"},
  {"reg.b=ireg.b", "ed021000:rw:ir_ldst"},
  {"reg.c=areg.c", "ed002000:rw:ar_ldst"},
  {"reg.c=ireg.c", "ed022000:rw:ir_ldst"},
  {"reg.d=areg.d", "ed003000:rw:ar_ldst"},
  {"reg.d=ireg.d", "ed023000:rw:ir_ldst"},
  {"reg=areg", "ed000800:rw:ar_ldst"},
  {"reg=ireg", "ed020800:rw:ir_ldst"},

   // Op code = 238 (0xee)
  {"reg.a=CMEML(reg.a)", "ee000041:mem_rm:rb"},
  {"reg.b=CMEML(reg.b)", "ee400041:mem_rm:rb"},
  {"reg.c=CMEML(reg.c)", "ee800041:mem_rm:rb"},
  {"reg.d=CMEML(reg.d)", "eec00041:mem_rm:rb"},

   // Op code = 239 (0xef)
  {"reg.a=CMEML(lit)", "ef000000:mem_rm:memlit"},
  {"reg.b=CMEML(lit)", "ef400000:mem_rm:memlit"},
  {"reg.c=CMEML(lit)", "ef800000:mem_rm:memlit"},
  {"reg.d=CMEML(lit)", "efc00000:mem_rm:memlit"},

   // Op code = 240 (0xf0)
  {"CMEML(reg.a)=reg.a", "f0000041:rb:ra"},
  {"CMEML(reg.b)=reg.b", "f0400041:rb:ra"},
  {"CMEML(reg.c)=reg.c", "f0800041:rb:ra"},
  {"CMEML(reg.d)=reg.d", "f0c00041:rb:ra"},

   // Op code = 241 (0xf1)
  {"CMEML(lit)=reg.a", "f1000000:memlit:ra"},
  {"CMEML(lit)=reg.b", "f1400000:memlit:ra"},
  {"CMEML(lit)=reg.c", "f1800000:memlit:ra"},
  {"CMEML(lit)=reg.d", "f1c00000:memlit:ra"},

   // Op code = 242 (0xf2)
  {"reg.a=XMEMB(areg.a+lit)", "f2000061:mem_rm:mem_ar:memli_lit"},
  {"reg.a=XMEMB(areg.a)", "f2000061:mem_rm:mem_ar"},
  {"reg.b=XMEMB(areg.b+lit)", "f2400061:mem_rm:mem_ar:memli_lit"},
  {"reg.b=XMEMB(areg.b)", "f2400061:mem_rm:mem_ar"},
  {"reg.c=XMEMB(areg.c+lit)", "f2800061:mem_rm:mem_ar:memli_lit"},
  {"reg.c=XMEMB(areg.c)", "f2800061:mem_rm:mem_ar"},
  {"reg.d=XMEMB(areg.d+lit)", "f2c00061:mem_rm:mem_ar:memli_lit"},
  {"reg.d=XMEMB(areg.d)", "f2c00061:mem_rm:mem_ar"},
  {"reg.a=XMEMB(areg.a),areg.a+=lit", "f2000021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=XMEMB(areg.b),areg.b+=lit", "f2400021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=XMEMB(areg.c),areg.c+=lit", "f2800021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=XMEMB(areg.d),areg.d+=lit", "f2c00021:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.a=XMEMW(areg.a+lit)", "f2000051:mem_rm:mem_ar:memli_lit"},
  {"reg.a=XMEMW(areg.a)", "f2000051:mem_rm:mem_ar"},
  {"reg.b=XMEMW(areg.b+lit)", "f2400051:mem_rm:mem_ar:memli_lit"},
  {"reg.b=XMEMW(areg.b)", "f2400051:mem_rm:mem_ar"},
  {"reg.c=XMEMW(areg.c+lit)", "f2800051:mem_rm:mem_ar:memli_lit"},
  {"reg.c=XMEMW(areg.c)", "f2800051:mem_rm:mem_ar"},
  {"reg.d=XMEMW(areg.d+lit)", "f2c00051:mem_rm:mem_ar:memli_lit"},
  {"reg.d=XMEMW(areg.d)", "f2c00051:mem_rm:mem_ar"},
  {"reg.a=XMEMW(areg.a),areg.a+=lit", "f2000011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=XMEMW(areg.b),areg.b+=lit", "f2400011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=XMEMW(areg.c),areg.c+=lit", "f2800011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=XMEMW(areg.d),areg.d+=lit", "f2c00011:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.a=XMEML(areg.a+lit)", "f2000041:mem_rm:mem_ar:memli_lit"},
  {"reg.a=XMEML(areg.a)", "f2000041:mem_rm:mem_ar"},
  {"reg.b=XMEML(areg.b+lit)", "f2400041:mem_rm:mem_ar:memli_lit"},
  {"reg.b=XMEML(areg.b)", "f2400041:mem_rm:mem_ar"},
  {"reg.c=XMEML(areg.c+lit)", "f2800041:mem_rm:mem_ar:memli_lit"},
  {"reg.c=XMEML(areg.c)", "f2800041:mem_rm:mem_ar"},
  {"reg.d=XMEML(areg.d+lit)", "f2c00041:mem_rm:mem_ar:memli_lit"},
  {"reg.d=XMEML(areg.d)", "f2c00041:mem_rm:mem_ar"},
  {"reg.a=XMEML(areg.a),areg.a+=lit", "f2000001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.b=XMEML(areg.b),areg.b+=lit", "f2400001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.c=XMEML(areg.c),areg.c+=lit", "f2800001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg.d=XMEML(areg.d),areg.d+=lit", "f2c00001:mem_rm:mem_ar:mem_ar:memli_lit"},
  {"reg=XMEML(areg+lit)", "f2000049:mem_rm:mem_ar:memli_lit"},
  {"reg=XMEML(areg)", "f2000049:mem_rm:mem_ar"},
  {"reg=XMEML(areg),areg+=lit", "f2000009:mem_rm:mem_ar:mem_ar:memli_lit"},

   // Op code = 243 (0xf3)
  {"reg.a=XMEMB(areg.a+reg.a)", "f3000061:mem_rm:mem_ar:rb"},
  {"reg.b=XMEMB(areg.b+reg.b)", "f3400061:mem_rm:mem_ar:rb"},
  {"reg.c=XMEMB(areg.c+reg.c)", "f3800061:mem_rm:mem_ar:rb"},
  {"reg.d=XMEMB(areg.d+reg.d)", "f3c00061:mem_rm:mem_ar:rb"},
  {"reg.a=XMEMB(areg.a),areg.a+=ireg.a", "f3000021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=XMEMB(areg.b),areg.b+=ireg.b", "f3400021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=XMEMB(areg.c),areg.c+=ireg.c", "f3800021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=XMEMB(areg.d),areg.d+=ireg.d", "f3c00021:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.a=XMEMW(areg.a+reg.a)", "f3000051:mem_rm:mem_ar:rb"},
  {"reg.b=XMEMW(areg.b+reg.b)", "f3400051:mem_rm:mem_ar:rb"},
  {"reg.c=XMEMW(areg.c+reg.c)", "f3800051:mem_rm:mem_ar:rb"},
  {"reg.d=XMEMW(areg.d+reg.d)", "f3c00051:mem_rm:mem_ar:rb"},
  {"reg.a=XMEMW(areg.a),areg.a+=ireg.a", "f3000011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=XMEMW(areg.b),areg.b+=ireg.b", "f3400011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=XMEMW(areg.c),areg.c+=ireg.c", "f3800011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=XMEMW(areg.d),areg.d+=ireg.d", "f3c00011:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.a=XMEML(areg.a+reg.a)", "f3000041:mem_rm:mem_ar:rb"},
  {"reg.b=XMEML(areg.b+reg.b)", "f3400041:mem_rm:mem_ar:rb"},
  {"reg.c=XMEML(areg.c+reg.c)", "f3800041:mem_rm:mem_ar:rb"},
  {"reg.d=XMEML(areg.d+reg.d)", "f3c00041:mem_rm:mem_ar:rb"},
  {"reg.a=XMEML(areg.a),areg.a+=ireg.a", "f3000001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.b=XMEML(areg.b),areg.b+=ireg.b", "f3400001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.c=XMEML(areg.c),areg.c+=ireg.c", "f3800001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg.d=XMEML(areg.d),areg.d+=ireg.d", "f3c00001:mem_rm:mem_ar:mem_ar:mem_ir"},
  {"reg=XMEML(areg+reg)", "f3000049:mem_rm:mem_ar:rb"},
  {"reg=XMEML(areg),areg+=ireg", "f3000009:mem_rm:mem_ar:mem_ar:mem_ir"},

   // Op code = 244 (0xf4)
  {"XMEMB(areg.a+lit)=reg.a", "f4000061:mem_ar:memli_lit:ra"},
  {"XMEMB(areg.a)=reg.a", "f4000061:mem_ar:ra"},
  {"XMEMB(areg.b+lit)=reg.b", "f4400061:mem_ar:memli_lit:ra"},
  {"XMEMB(areg.b)=reg.b", "f4400061:mem_ar:ra"},
  {"XMEMB(areg.c+lit)=reg.c", "f4800061:mem_ar:memli_lit:ra"},
  {"XMEMB(areg.c)=reg.c", "f4800061:mem_ar:ra"},
  {"XMEMB(areg.d+lit)=reg.d", "f4c00061:mem_ar:memli_lit:ra"},
  {"XMEMB(areg.d)=reg.d", "f4c00061:mem_ar:ra"},
  {"XMEMB(areg.a)=reg.a,areg.a+=lit", "f4000021:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEMB(areg.b)=reg.b,areg.b+=lit", "f4400021:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEMB(areg.c)=reg.c,areg.c+=lit", "f4800021:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEMB(areg.d)=reg.d,areg.d+=lit", "f4c00021:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEMW(areg.a+lit)=reg.a", "f4000051:mem_ar:memli_lit:ra"},
  {"XMEMW(areg.a)=reg.a", "f4000051:mem_ar:ra"},
  {"XMEMW(areg.b+lit)=reg.b", "f4400051:mem_ar:memli_lit:ra"},
  {"XMEMW(areg.b)=reg.b", "f4400051:mem_ar:ra"},
  {"XMEMW(areg.c+lit)=reg.c", "f4800051:mem_ar:memli_lit:ra"},
  {"XMEMW(areg.c)=reg.c", "f4800051:mem_ar:ra"},
  {"XMEMW(areg.d+lit)=reg.d", "f4c00051:mem_ar:memli_lit:ra"},
  {"XMEMW(areg.d)=reg.d", "f4c00051:mem_ar:ra"},
  {"XMEMW(areg.a)=reg.a,areg.a+=lit", "f4000011:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEMW(areg.b)=reg.b,areg.b+=lit", "f4400011:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEMW(areg.c)=reg.c,areg.c+=lit", "f4800011:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEMW(areg.d)=reg.d,areg.d+=lit", "f4c00011:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEML(areg.a+lit)=reg.a", "f4000041:mem_ar:memli_lit:ra"},
  {"XMEML(areg.a)=reg.a", "f4000041:mem_ar:ra"},
  {"XMEML(areg.b+lit)=reg.b", "f4400041:mem_ar:memli_lit:ra"},
  {"XMEML(areg.b)=reg.b", "f4400041:mem_ar:ra"},
  {"XMEML(areg.c+lit)=reg.c", "f4800041:mem_ar:memli_lit:ra"},
  {"XMEML(areg.c)=reg.c", "f4800041:mem_ar:ra"},
  {"XMEML(areg.d+lit)=reg.d", "f4c00041:mem_ar:memli_lit:ra"},
  {"XMEML(areg.d)=reg.d", "f4c00041:mem_ar:ra"},
  {"XMEML(areg.a)=reg.a,areg.a+=lit", "f4000001:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEML(areg.b)=reg.b,areg.b+=lit", "f4400001:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEML(areg.c)=reg.c,areg.c+=lit", "f4800001:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEML(areg.d)=reg.d,areg.d+=lit", "f4c00001:mem_ar:ra:mem_ar:memli_lit"},
  {"XMEML(areg+lit)=reg", "f4000049:mem_ar:memli_lit:ra"},
  {"XMEML(areg)=reg", "f4000049:mem_ar:ra"},
  {"XMEML(areg)=reg,areg+=lit", "f4000009:mem_ar:ra:mem_ar:memli_lit"},

   // Op code = 245 (0xf5)
  {"XMEMB(areg.a+reg.a)=reg.a", "f5000061:mem_ar:rb:ra"},
  {"XMEMB(areg.b+reg.b)=reg.b", "f5400061:mem_ar:rb:ra"},
  {"XMEMB(areg.c+reg.c)=reg.c", "f5800061:mem_ar:rb:ra"},
  {"XMEMB(areg.d+reg.d)=reg.d", "f5c00061:mem_ar:rb:ra"},
  {"XMEMB(areg.a)=reg.a,areg.a+=ireg.a", "f5000021:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEMB(areg.b)=reg.b,areg.b+=ireg.b", "f5400021:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEMB(areg.c)=reg.c,areg.c+=ireg.c", "f5800021:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEMB(areg.d)=reg.d,areg.d+=ireg.d", "f5c00021:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEMW(areg.a+reg.a)=reg.a", "f5000051:mem_ar:rb:ra"},
  {"XMEMW(areg.b+reg.b)=reg.b", "f5400051:mem_ar:rb:ra"},
  {"XMEMW(areg.c+reg.c)=reg.c", "f5800051:mem_ar:rb:ra"},
  {"XMEMW(areg.d+reg.d)=reg.d", "f5c00051:mem_ar:rb:ra"},
  {"XMEMW(areg.a)=reg.a,areg.a+=ireg.a", "f5000011:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEMW(areg.b)=reg.b,areg.b+=ireg.b", "f5400011:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEMW(areg.c)=reg.c,areg.c+=ireg.c", "f5800011:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEMW(areg.d)=reg.d,areg.d+=ireg.d", "f5c00011:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEML(areg.a+reg.a)=reg.a", "f5000041:mem_ar:rb:ra"},
  {"XMEML(areg.b+reg.b)=reg.b", "f5400041:mem_ar:rb:ra"},
  {"XMEML(areg.c+reg.c)=reg.c", "f5800041:mem_ar:rb:ra"},
  {"XMEML(areg.d+reg.d)=reg.d", "f5c00041:mem_ar:rb:ra"},
  {"XMEML(areg.a)=reg.a,areg.a+=ireg.a", "f5000001:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEML(areg.b)=reg.b,areg.b+=ireg.b", "f5400001:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEML(areg.c)=reg.c,areg.c+=ireg.c", "f5800001:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEML(areg.d)=reg.d,areg.d+=ireg.d", "f5c00001:mem_ar:ra:mem_ar:mem_ir"},
  {"XMEML(areg+reg)=reg", "f5000049:mem_ar:rb:ra"},
  {"XMEML(areg)=reg,areg+=ireg", "f5000009:mem_ar:ra:mem_ar:mem_ir"},

   // Op code = 246 (0xf6)
  {"RSVD", "f6000000"},

   // Op code = 247 (0xf7)
  {"RSVD", "f7000000"},

   // Op code = 248 (0xf8)
  {"RSVD", "f8000000"},

   // Op code = 249 (0xf9)
  {"RSVD", "f9000000"},

   // Op code = 250 (0xfa)
  {"RSVD", "fa000000"},

   // Op code = 251 (0xfb)
  {"RSVD", "fb000000"},

   // Op code = 252 (0xfc)
  {"RSVD", "fc000000"},

   // Op code = 253 (0xfd)
  {"RSVD", "fd000000"},

   // Op code = 254 (0xfe)
  {"RSVD_254", "fe000000"},

   // Op code = 255 (0xff)
  {"RSVD", "ff000000"},

  {  NULL, NULL }
};


//amber keywords and instructions


static hash_t amber_insn_template[] = {
   // Op code = 0 (0x0)
   {"NOP"
       , "00000000"},

   // Op code = 1 (0x1)
   {"HALT"
       , "01000000"},

   // Op code = 2 (0x2)
   {"reg=reg*lit"
       , "02000000:rw:ra:lit"},
   {"LA=reg*lit"
       , "02000000:ra:lit"},

   // Op code = 3 (0x3)
   {"reg=EXT(reg)*lit"
       , "03000000:rw:ra:lit"},
   {"LA=EXT(reg)*lit"
       , "03000000:ra:lit"},

   // Op code = 4 (0x4)
   {"reg=EXTI(reg)*lit"
       , "04000000:rw:ra:lit"},
   {"LA=EXTI(reg)*lit"
       , "04000000:ra:lit"},

   // Op code = 5 (0x5)
   {"reg=reg*lit+reg"
       , "05000000:rw:ra:lit:rb"},
   {"reg=reg*lit+LA"
       , "05000000:rw:ra:lit"},
   {"LA=reg*lit+reg"
       , "05000000:ra:lit:rb"},
   {"LA=reg*lit+LA"
       , "05000000:ra:lit"},

   // Op code = 6 (0x6)
   {"reg=reg*lit-reg"
       , "06000000:rw:ra:lit:rb"},
   {"reg=reg*lit-LA"
       , "06000000:rw:ra:lit"},
   {"LA=reg*lit-reg"
       , "06000000:ra:lit:rb"},
   {"LA=reg*lit-LA"
       , "06000000:ra:lit"},

   // Op code = 7 (0x7)
   {"reg=reg*lit&reg"
       , "07000000:rw:ra:lit:rb"},
   {"reg=reg*lit&LA"
       , "07000000:rw:ra:lit"},
   {"LA=reg*lit&reg"
       , "07000000:ra:lit:rb"},
   {"LA=reg*lit&LA"
       , "07000000:ra:lit"},

   // Op code = 8 (0x8)
   {"reg=reg*lit|reg"
       , "08000000:rw:ra:lit:rb"},
   {"reg=reg*lit|LA"
       , "08000000:rw:ra:lit"},
   {"LA=reg*lit|reg"
       , "08000000:ra:lit:rb"},
   {"LA=reg*lit|LA"
       , "08000000:ra:lit"},

   // Op code = 9 (0x9)
   {"reg=reg*lit^reg"
       , "09000000:rw:ra:lit:rb"},
   {"reg=reg*lit^LA"
       , "09000000:rw:ra:lit"},
   {"LA=reg*lit^reg"
       , "09000000:ra:lit:rb"},
   {"LA=reg*lit^LA"
       , "09000000:ra:lit"},

   // Op code = 10 (0xa)
   {"reg=EXT(reg)*lit+reg"
       , "0a000000:rw:ra:lit:rb"},
   {"reg=EXT(reg)*lit+LA"
       , "0a000000:rw:ra:lit"},
   {"LA=EXT(reg)*lit+reg"
       , "0a000000:ra:lit:rb"},
   {"LA=EXT(reg)*lit+LA"
       , "0a000000:ra:lit"},

   // Op code = 11 (0xb)
   {"reg=EXT(reg)*lit-reg"
       , "0b000000:rw:ra:lit:rb"},
   {"reg=EXT(reg)*lit-LA"
       , "0b000000:rw:ra:lit"},
   {"LA=EXT(reg)*lit-reg"
       , "0b000000:ra:lit:rb"},
   {"LA=EXT(reg)*lit-LA"
       , "0b000000:ra:lit"},

   // Op code = 12 (0xc)
   {"reg=EXT(reg)*lit&reg"
       , "0c000000:rw:ra:lit:rb"},
   {"reg=EXT(reg)*lit&LA"
       , "0c000000:rw:ra:lit"},
   {"LA=EXT(reg)*lit&reg"
       , "0c000000:ra:lit:rb"},
   {"LA=EXT(reg)*lit&LA"
       , "0c000000:ra:lit"},

   // Op code = 13 (0xd)
   {"reg=EXT(reg)*lit|reg"
       , "0d000000:rw:ra:lit:rb"},
   {"reg=EXT(reg)*lit|LA"
       , "0d000000:rw:ra:lit"},
   {"LA=EXT(reg)*lit|reg"
       , "0d000000:ra:lit:rb"},
   {"LA=EXT(reg)*lit|LA"
       , "0d000000:ra:lit"},

   // Op code = 14 (0xe)
   {"reg=EXT(reg)*lit^reg"
       , "0e000000:rw:ra:lit:rb"},
   {"reg=EXT(reg)*lit^LA"
       , "0e000000:rw:ra:lit"},
   {"LA=EXT(reg)*lit^reg"
       , "0e000000:ra:lit:rb"},
   {"LA=EXT(reg)*lit^LA"
       , "0e000000:ra:lit"},

   // Op code = 15 (0xf)
   {"reg=EXTI(reg)*lit+reg"
       , "0f000000:rw:ra:lit:rb"},
   {"reg=EXTI(reg)*lit+LA"
       , "0f000000:rw:ra:lit"},
   {"LA=EXTI(reg)*lit+reg"
       , "0f000000:ra:lit:rb"},
   {"LA=EXTI(reg)*lit+LA"
       , "0f000000:ra:lit"},

   // Op code = 16 (0x10)
   {"reg=EXTI(reg)*lit-reg"
       , "10000000:rw:ra:lit:rb"},
   {"reg=EXTI(reg)*lit-LA"
       , "10000000:rw:ra:lit"},
   {"LA=EXTI(reg)*lit-reg"
       , "10000000:ra:lit:rb"},
   {"LA=EXTI(reg)*lit-LA"
       , "10000000:ra:lit"},

   // Op code = 17 (0x11)
   {"reg=EXTI(reg)*lit&reg"
       , "11000000:rw:ra:lit:rb"},
   {"reg=EXTI(reg)*lit&LA"
       , "11000000:rw:ra:lit"},
   {"LA=EXTI(reg)*lit&reg"
       , "11000000:ra:lit:rb"},
   {"LA=EXTI(reg)*lit&LA"
       , "11000000:ra:lit"},

   // Op code = 18 (0x12)
   {"reg=EXTI(reg)*lit|reg"
       , "12000000:rw:ra:lit:rb"},
   {"reg=EXTI(reg)*lit|LA"
       , "12000000:rw:ra:lit"},
   {"LA=EXTI(reg)*lit|reg"
       , "12000000:ra:lit:rb"},
   {"LA=EXTI(reg)*lit|LA"
       , "12000000:ra:lit"},

   // Op code = 19 (0x13)
   {"reg=EXTI(reg)*lit^reg"
       , "13000000:rw:ra:lit:rb"},
   {"reg=EXTI(reg)*lit^LA"
       , "13000000:rw:ra:lit"},
   {"LA=EXTI(reg)*lit^reg"
       , "13000000:ra:lit:rb"},
   {"LA=EXTI(reg)*lit^LA"
       , "13000000:ra:lit"},

   // Op code = 20 (0x14)
   {"reg=reg*reg+lit"
       , "14000000:rw:ra:rb:lit"},
   {"reg=reg+lit"
       , "14000000:rw:ra:lit"},
   {"LA=reg*reg+lit"
       , "14000000:ra:rb:lit"},
   {"LA=reg+lit"
       , "14000000:ra:lit"},

   // Op code = 21 (0x15)
   {"reg=reg*reg&lit"
       , "15000000:rw:ra:rb:lit"},
   {"reg=reg&lit"
       , "15000000:rw:ra:lit"},
   {"LA=reg*reg&lit"
       , "15000000:ra:rb:lit"},
   {"LA=reg&lit"
       , "15000000:ra:lit"},

   // Op code = 22 (0x16)
   {"reg=reg*reg|lit"
       , "16000000:rw:ra:rb:lit"},
   {"reg=reg|lit"
       , "16000000:rw:ra:lit"},
   {"LA=reg*reg|lit"
       , "16000000:ra:rb:lit"},
   {"LA=reg|lit"
       , "16000000:ra:lit"},

   // Op code = 23 (0x17)
   {"reg=reg*reg^lit"
       , "17000000:rw:ra:rb:lit"},
   {"reg=reg^lit"
       , "17000000:rw:ra:lit"},
   {"LA=reg*reg^lit"
       , "17000000:ra:rb:lit"},
   {"LA=reg^lit"
       , "17000000:ra:lit"},

   // Op code = 24 (0x18)
   {"reg=EXT(reg)*reg+lit"
       , "18000000:rw:ra:rb:lit"},
   {"reg=EXT(reg)+lit"
       , "18000000:rw:ra:lit"},
   {"LA=EXT(reg)*reg+lit"
       , "18000000:ra:rb:lit"},
   {"LA=EXT(reg)+lit"
       , "18000000:ra:lit"},

   // Op code = 25 (0x19)
   {"reg=EXT(reg)*reg&lit"
       , "19000000:rw:ra:rb:lit"},
   {"reg=EXT(reg)&lit"
       , "19000000:rw:ra:lit"},
   {"LA=EXT(reg)*reg&lit"
       , "19000000:ra:rb:lit"},
   {"LA=EXT(reg)&lit"
       , "19000000:ra:lit"},

   // Op code = 26 (0x1a)
   {"reg=EXT(reg)*reg|lit"
       , "1a000000:rw:ra:rb:lit"},
   {"reg=EXT(reg)|lit"
       , "1a000000:rw:ra:lit"},
   {"LA=EXT(reg)*reg|lit"
       , "1a000000:ra:rb:lit"},
   {"LA=EXT(reg)|lit"
       , "1a000000:ra:lit"},

   // Op code = 27 (0x1b)
   {"reg=EXT(reg)*reg^lit"
       , "1b000000:rw:ra:rb:lit"},
   {"reg=EXT(reg)^lit"
       , "1b000000:rw:ra:lit"},
   {"LA=EXT(reg)*reg^lit"
       , "1b000000:ra:rb:lit"},
   {"LA=EXT(reg)^lit"
       , "1b000000:ra:lit"},

   // Op code = 28 (0x1c)
   {"reg=EXTI(reg)*reg+lit"
       , "1c000000:rw:ra:rb:lit"},
   {"reg=EXTI(reg)+lit"
       , "1c000000:rw:ra:lit"},
   {"LA=EXTI(reg)*reg+lit"
       , "1c000000:ra:rb:lit"},
   {"LA=EXTI(reg)+lit"
       , "1c000000:ra:lit"},

   // Op code = 29 (0x1d)
   {"reg=EXTI(reg)*reg&lit"
       , "1d000000:rw:ra:rb:lit"},
   {"reg=EXTI(reg)&lit"
       , "1d000000:rw:ra:lit"},
   {"LA=EXTI(reg)*reg&lit"
       , "1d000000:ra:rb:lit"},
   {"LA=EXTI(reg)&lit"
       , "1d000000:ra:lit"},

   // Op code = 30 (0x1e)
   {"reg=EXTI(reg)*reg|lit"
       , "1e000000:rw:ra:rb:lit"},
   {"reg=EXTI(reg)|lit"
       , "1e000000:rw:ra:lit"},
   {"LA=EXTI(reg)*reg|lit"
       , "1e000000:ra:rb:lit"},
   {"LA=EXTI(reg)|lit"
       , "1e000000:ra:lit"},

   // Op code = 31 (0x1f)
   {"reg=EXTI(reg)*reg^lit"
       , "1f000000:rw:ra:rb:lit"},
   {"reg=EXTI(reg)^lit"
       , "1f000000:rw:ra:lit"},
   {"LA=EXTI(reg)*reg^lit"
       , "1f000000:ra:rb:lit"},
   {"LA=EXTI(reg)^lit"
       , "1f000000:ra:lit"},

   // Op code = 32 (0x20)
   {"reg=reg*reg+LA"
       , "200001fe:rw:ra:rb"},
   {"reg=reg+LA"
       , "200001fe:rw:ra"},
   {"reg=reg*reg+LA,reg=MEMSL(areg)"
       , "20000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=reg+LA,reg=MEMSL(areg)"
       , "20000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=reg*reg+LA"
       , "200001fe:ra:rb"},
   {"LA=reg+LA"
       , "200001fe:ra"},
   {"LA=reg*reg+LA,reg=MEMSL(areg)"
       , "20000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg+LA,reg=MEMSL(areg)"
       , "20000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg*reg+LA,lit,lit)"
       , "20000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(reg+LA,lit,lit)"
       , "20000000:rw:ra:iepos:iewid"},

   // Op code = 33 (0x21)
   {"reg=reg*reg-LA"
       , "210001fe:rw:ra:rb"},
   {"reg=reg-LA"
       , "210001fe:rw:ra"},
   {"reg=reg*reg-LA,reg=MEMSL(areg)"
       , "21000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=reg-LA,reg=MEMSL(areg)"
       , "21000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=reg*reg-LA"
       , "210001fe:ra:rb"},
   {"LA=reg-LA"
       , "210001fe:ra"},
   {"LA=reg*reg-LA,reg=MEMSL(areg)"
       , "21000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg-LA,reg=MEMSL(areg)"
       , "21000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg*reg-LA,lit,lit)"
       , "21000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(reg-LA,lit,lit)"
       , "21000000:rw:ra:iepos:iewid"},

   // Op code = 34 (0x22)
   {"reg=reg*reg&LA"
       , "220001fe:rw:ra:rb"},
   {"reg=reg&LA"
       , "220001fe:rw:ra"},
   {"reg=reg*reg&LA,reg=MEMSL(areg)"
       , "22000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=reg&LA,reg=MEMSL(areg)"
       , "22000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=reg*reg&LA"
       , "220001fe:ra:rb"},
   {"LA=reg&LA"
       , "220001fe:ra"},
   {"LA=reg*reg&LA,reg=MEMSL(areg)"
       , "22000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg&LA,reg=MEMSL(areg)"
       , "22000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg*reg&LA,lit,lit)"
       , "22000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(reg&LA,lit,lit)"
       , "22000000:rw:ra:iepos:iewid"},

   // Op code = 35 (0x23)
   {"reg=reg*reg|LA"
       , "230001fe:rw:ra:rb"},
   {"reg=reg|LA"
       , "230001fe:rw:ra"},
   {"reg=reg*reg|LA,reg=MEMSL(areg)"
       , "23000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=reg|LA,reg=MEMSL(areg)"
       , "23000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=reg*reg|LA"
       , "230001fe:ra:rb"},
   {"LA=reg|LA"
       , "230001fe:ra"},
   {"LA=reg*reg|LA,reg=MEMSL(areg)"
       , "23000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg|LA,reg=MEMSL(areg)"
       , "23000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg*reg|LA,lit,lit)"
       , "23000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(reg|LA,lit,lit)"
       , "23000000:rw:ra:iepos:iewid"},

   // Op code = 36 (0x24)
   {"reg=reg*reg^LA"
       , "240001fe:rw:ra:rb"},
   {"reg=reg^LA"
       , "240001fe:rw:ra"},
   {"reg=reg*reg^LA,reg=MEMSL(areg)"
       , "24000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=reg^LA,reg=MEMSL(areg)"
       , "24000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=reg*reg^LA"
       , "240001fe:ra:rb"},
   {"LA=reg^LA"
       , "240001fe:ra"},
   {"LA=reg*reg^LA,reg=MEMSL(areg)"
       , "24000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg^LA,reg=MEMSL(areg)"
       , "24000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg*reg^LA,lit,lit)"
       , "24000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(reg^LA,lit,lit)"
       , "24000000:rw:ra:iepos:iewid"},

   // Op code = 37 (0x25)
   {"reg=EXT(reg)*reg+LA"
       , "250001fe:rw:ra:rb"},
   {"reg=EXT(reg)+LA"
       , "250001fe:rw:ra"},
   {"reg=EXT(reg)*reg+LA,reg=MEMSL(areg)"
       , "25000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXT(reg)+LA,reg=MEMSL(areg)"
       , "25000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*reg+LA"
       , "250001fe:ra:rb"},
   {"LA=EXT(reg)+LA"
       , "250001fe:ra"},
   {"LA=EXT(reg)*reg+LA,reg=MEMSL(areg)"
       , "25000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)+LA,reg=MEMSL(areg)"
       , "25000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*reg+LA,lit,lit)"
       , "25000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXT(reg)+LA,lit,lit)"
       , "25000000:rw:ra:iepos:iewid"},

   // Op code = 38 (0x26)
   {"reg=EXT(reg)*reg-LA"
       , "260001fe:rw:ra:rb"},
   {"reg=EXT(reg)-LA"
       , "260001fe:rw:ra"},
   {"reg=EXT(reg)*reg-LA,reg=MEMSL(areg)"
       , "26000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXT(reg)-LA,reg=MEMSL(areg)"
       , "26000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*reg-LA"
       , "260001fe:ra:rb"},
   {"LA=EXT(reg)-LA"
       , "260001fe:ra"},
   {"LA=EXT(reg)*reg-LA,reg=MEMSL(areg)"
       , "26000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)-LA,reg=MEMSL(areg)"
       , "26000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*reg-LA,lit,lit)"
       , "26000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXT(reg)-LA,lit,lit)"
       , "26000000:rw:ra:iepos:iewid"},

   // Op code = 39 (0x27)
   {"reg=EXT(reg)*reg&LA"
       , "270001fe:rw:ra:rb"},
   {"reg=EXT(reg)&LA"
       , "270001fe:rw:ra"},
   {"reg=EXT(reg)*reg&LA,reg=MEMSL(areg)"
       , "27000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXT(reg)&LA,reg=MEMSL(areg)"
       , "27000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*reg&LA"
       , "270001fe:ra:rb"},
   {"LA=EXT(reg)&LA"
       , "270001fe:ra"},
   {"LA=EXT(reg)*reg&LA,reg=MEMSL(areg)"
       , "27000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)&LA,reg=MEMSL(areg)"
       , "27000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*reg&LA,lit,lit)"
       , "27000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXT(reg)&LA,lit,lit)"
       , "27000000:rw:ra:iepos:iewid"},

   // Op code = 40 (0x28)
   {"reg=EXT(reg)*reg|LA"
       , "280001fe:rw:ra:rb"},
   {"reg=EXT(reg)|LA"
       , "280001fe:rw:ra"},
   {"reg=EXT(reg)*reg|LA,reg=MEMSL(areg)"
       , "28000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXT(reg)|LA,reg=MEMSL(areg)"
       , "28000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*reg|LA"
       , "280001fe:ra:rb"},
   {"LA=EXT(reg)|LA"
       , "280001fe:ra"},
   {"LA=EXT(reg)*reg|LA,reg=MEMSL(areg)"
       , "28000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)|LA,reg=MEMSL(areg)"
       , "28000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*reg|LA,lit,lit)"
       , "28000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXT(reg)|LA,lit,lit)"
       , "28000000:rw:ra:iepos:iewid"},

   // Op code = 41 (0x29)
   {"reg=EXT(reg)*reg^LA"
       , "290001fe:rw:ra:rb"},
   {"reg=EXT(reg)^LA"
       , "290001fe:rw:ra"},
   {"reg=EXT(reg)*reg^LA,reg=MEMSL(areg)"
       , "29000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXT(reg)^LA,reg=MEMSL(areg)"
       , "29000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*reg^LA"
       , "290001fe:ra:rb"},
   {"LA=EXT(reg)^LA"
       , "290001fe:ra"},
   {"LA=EXT(reg)*reg^LA,reg=MEMSL(areg)"
       , "29000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)^LA,reg=MEMSL(areg)"
       , "29000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*reg^LA,lit,lit)"
       , "29000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXT(reg)^LA,lit,lit)"
       , "29000000:rw:ra:iepos:iewid"},

   // Op code = 42 (0x2a)
   {"reg=EXTI(reg)*reg+LA"
       , "2a0001fe:rw:ra:rb"},
   {"reg=EXTI(reg)+LA"
       , "2a0001fe:rw:ra"},
   {"reg=EXTI(reg)*reg+LA,reg=MEMSL(areg)"
       , "2a000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTI(reg)+LA,reg=MEMSL(areg)"
       , "2a000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*reg+LA"
       , "2a0001fe:ra:rb"},
   {"LA=EXTI(reg)+LA"
       , "2a0001fe:ra"},
   {"LA=EXTI(reg)*reg+LA,reg=MEMSL(areg)"
       , "2a000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)+LA,reg=MEMSL(areg)"
       , "2a000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*reg+LA,lit,lit)"
       , "2a000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXTI(reg)+LA,lit,lit)"
       , "2a000000:rw:ra:iepos:iewid"},

   // Op code = 43 (0x2b)
   {"reg=EXTI(reg)*reg-LA"
       , "2b0001fe:rw:ra:rb"},
   {"reg=EXTI(reg)-LA"
       , "2b0001fe:rw:ra"},
   {"reg=EXTI(reg)*reg-LA,reg=MEMSL(areg)"
       , "2b000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTI(reg)-LA,reg=MEMSL(areg)"
       , "2b000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*reg-LA"
       , "2b0001fe:ra:rb"},
   {"LA=EXTI(reg)-LA"
       , "2b0001fe:ra"},
   {"LA=EXTI(reg)*reg-LA,reg=MEMSL(areg)"
       , "2b000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)-LA,reg=MEMSL(areg)"
       , "2b000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*reg-LA,lit,lit)"
       , "2b000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXTI(reg)-LA,lit,lit)"
       , "2b000000:rw:ra:iepos:iewid"},

   // Op code = 44 (0x2c)
   {"reg=EXTI(reg)*reg&LA"
       , "2c0001fe:rw:ra:rb"},
   {"reg=EXTI(reg)&LA"
       , "2c0001fe:rw:ra"},
   {"reg=EXTI(reg)*reg&LA,reg=MEMSL(areg)"
       , "2c000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTI(reg)&LA,reg=MEMSL(areg)"
       , "2c000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*reg&LA"
       , "2c0001fe:ra:rb"},
   {"LA=EXTI(reg)&LA"
       , "2c0001fe:ra"},
   {"LA=EXTI(reg)*reg&LA,reg=MEMSL(areg)"
       , "2c000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)&LA,reg=MEMSL(areg)"
       , "2c000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*reg&LA,lit,lit)"
       , "2c000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXTI(reg)&LA,lit,lit)"
       , "2c000000:rw:ra:iepos:iewid"},

   // Op code = 45 (0x2d)
   {"reg=EXTI(reg)*reg|LA"
       , "2d0001fe:rw:ra:rb"},
   {"reg=EXTI(reg)|LA"
       , "2d0001fe:rw:ra"},
   {"reg=EXTI(reg)*reg|LA,reg=MEMSL(areg)"
       , "2d000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTI(reg)|LA,reg=MEMSL(areg)"
       , "2d000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*reg|LA"
       , "2d0001fe:ra:rb"},
   {"LA=EXTI(reg)|LA"
       , "2d0001fe:ra"},
   {"LA=EXTI(reg)*reg|LA,reg=MEMSL(areg)"
       , "2d000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)|LA,reg=MEMSL(areg)"
       , "2d000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*reg|LA,lit,lit)"
       , "2d000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXTI(reg)|LA,lit,lit)"
       , "2d000000:rw:ra:iepos:iewid"},

   // Op code = 46 (0x2e)
   {"reg=EXTI(reg)*reg^LA"
       , "2e0001fe:rw:ra:rb"},
   {"reg=EXTI(reg)^LA"
       , "2e0001fe:rw:ra"},
   {"reg=EXTI(reg)*reg^LA,reg=MEMSL(areg)"
       , "2e000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTI(reg)^LA,reg=MEMSL(areg)"
       , "2e000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*reg^LA"
       , "2e0001fe:ra:rb"},
   {"LA=EXTI(reg)^LA"
       , "2e0001fe:ra"},
   {"LA=EXTI(reg)*reg^LA,reg=MEMSL(areg)"
       , "2e000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)^LA,reg=MEMSL(areg)"
       , "2e000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*reg^LA,lit,lit)"
       , "2e000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXTI(reg)^LA,lit,lit)"
       , "2e000000:rw:ra:iepos:iewid"},

   // Op code = 47 (0x2f)
   {"reg=EXTL(reg,lit,lit)*reg+LA"
       , "2f000000:rw:ra:epos:ewid:rb"},
   {"reg=EXTL(reg,lit,lit)+LA"
       , "2f000000:rw:ra:epos:ewid"},
   {"LA=EXTL(reg,lit,lit)*reg+LA"
       , "2f000000:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)+LA"
       , "2f000000:ra:epos:ewid"},
   {"reg=(reg<<lit)*reg+LA"
       , "2f0001e0:rw:ra:epos:rb"},
   {"reg=(reg<<lit)+LA"
       , "2f0001e0:rw:ra:epos"},
   {"LA=(reg<<lit)*reg+LA"
       , "2f0001e0:ra:epos:rb"},
   {"LA=(reg<<lit)+LA"
       , "2f0001e0:ra:epos"},

   // Op code = 48 (0x30)
   {"reg=EXTL(reg,lit,lit)*reg-LA"
       , "30000000:rw:ra:epos:ewid:rb"},
   {"reg=EXTL(reg,lit,lit)-LA"
       , "30000000:rw:ra:epos:ewid"},
   {"LA=EXTL(reg,lit,lit)*reg-LA"
       , "30000000:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)-LA"
       , "30000000:ra:epos:ewid"},
   {"reg=(reg<<lit)*reg-LA"
       , "300001e0:rw:ra:epos:rb"},
   {"reg=(reg<<lit)-LA"
       , "300001e0:rw:ra:epos"},
   {"LA=(reg<<lit)*reg-LA"
       , "300001e0:ra:epos:rb"},
   {"LA=(reg<<lit)-LA"
       , "300001e0:ra:epos"},

   // Op code = 49 (0x31)
   {"reg=EXTL(reg,lit,lit)*reg&LA"
       , "31000000:rw:ra:epos:ewid:rb"},
   {"reg=EXTL(reg,lit,lit)&LA"
       , "31000000:rw:ra:epos:ewid"},
   {"LA=EXTL(reg,lit,lit)*reg&LA"
       , "31000000:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)&LA"
       , "31000000:ra:epos:ewid"},
   {"reg=(reg<<lit)*reg&LA"
       , "310001e0:rw:ra:epos:rb"},
   {"reg=(reg<<lit)&LA"
       , "310001e0:rw:ra:epos"},
   {"LA=(reg<<lit)*reg&LA"
       , "310001e0:ra:epos:rb"},
   {"LA=(reg<<lit)&LA"
       , "310001e0:ra:epos"},

   // Op code = 50 (0x32)
   {"reg=EXTL(reg,lit,lit)*reg|LA"
       , "32000000:rw:ra:epos:ewid:rb"},
   {"reg=EXTL(reg,lit,lit)|LA"
       , "32000000:rw:ra:epos:ewid"},
   {"LA=EXTL(reg,lit,lit)*reg|LA"
       , "32000000:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)|LA"
       , "32000000:ra:epos:ewid"},
   {"reg=(reg<<lit)*reg|LA"
       , "320001e0:rw:ra:epos:rb"},
   {"reg=(reg<<lit)|LA"
       , "320001e0:rw:ra:epos"},
   {"LA=(reg<<lit)*reg|LA"
       , "320001e0:ra:epos:rb"},
   {"LA=(reg<<lit)|LA"
       , "320001e0:ra:epos"},

   // Op code = 51 (0x33)
   {"reg=EXTL(reg,lit,lit)*reg^LA"
       , "33000000:rw:ra:epos:ewid:rb"},
   {"reg=EXTL(reg,lit,lit)^LA"
       , "33000000:rw:ra:epos:ewid"},
   {"LA=EXTL(reg,lit,lit)*reg^LA"
       , "33000000:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)^LA"
       , "33000000:ra:epos:ewid"},
   {"reg=(reg<<lit)*reg^LA"
       , "330001e0:rw:ra:epos:rb"},
   {"reg=(reg<<lit)^LA"
       , "330001e0:rw:ra:epos"},
   {"LA=(reg<<lit)*reg^LA"
       , "330001e0:ra:epos:rb"},
   {"LA=(reg<<lit)^LA"
       , "330001e0:ra:epos"},

   // Op code = 52 (0x34)
   {"reg=reg*reg"
       , "340001fe:rw:ra:rb"},
   {"reg=reg"
       , "340001fe:rw:ra"},
   {"reg=reg*reg,reg=MEMSL(areg)"
       , "34000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=reg,reg=MEMSL(areg)"
       , "34000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=reg*reg"
       , "340001fe:ra:rb"},
   {"LA=reg"
       , "340001fe:ra"},
   {"LA=reg*reg,reg=MEMSL(areg)"
       , "34000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg,reg=MEMSL(areg)"
       , "34000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg*reg,lit,lit)"
       , "34000000:rw:ra:rb:iepos:iewid"},

   // Op code = 53 (0x35)
   {"reg=EXT(reg)*reg"
       , "350001fe:rw:ra:rb"},
   {"reg=EXT(reg)"
       , "350001fe:rw:ra"},
   {"reg=EXT(reg)*reg,reg=MEMSL(areg)"
       , "35000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXT(reg),reg=MEMSL(areg)"
       , "35000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*reg"
       , "350001fe:ra:rb"},
   {"LA=EXT(reg)"
       , "350001fe:ra"},
   {"LA=EXT(reg)*reg,reg=MEMSL(areg)"
       , "35000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg),reg=MEMSL(areg)"
       , "35000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*reg,lit,lit)"
       , "35000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXT(reg),lit,lit)"
       , "35000000:rw:ra:iepos:iewid"},

   // Op code = 54 (0x36)
   {"reg=EXTI(reg)*reg"
       , "360001fe:rw:ra:rb"},
   {"reg=EXTI(reg)"
       , "360001fe:rw:ra"},
   {"reg=EXTI(reg)*reg,reg=MEMSL(areg)"
       , "36000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTI(reg),reg=MEMSL(areg)"
       , "36000001:rw:ra:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*reg"
       , "360001fe:ra:rb"},
   {"LA=EXTI(reg)"
       , "360001fe:ra"},
   {"LA=EXTI(reg)*reg,reg=MEMSL(areg)"
       , "36000001:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg),reg=MEMSL(areg)"
       , "36000001:ra:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*reg,lit,lit)"
       , "36000000:rw:ra:rb:iepos:iewid"},
   {"reg=EXTL(EXTI(reg),lit,lit)"
       , "36000000:rw:ra:iepos:iewid"},

   // Op code = 55 (0x37)
   {"reg=EXTL(reg,lit,lit)*reg"
       , "37000000:rw:ra:epos:ewid:rb"},
   {"reg=EXTL(reg,lit,lit)"
       , "37000000:rw:ra:epos:ewid"},
   {"LA=EXTL(reg,lit,lit)*reg"
       , "37000000:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)"
       , "37000000:ra:epos:ewid"},
   {"reg=(reg<<lit)*reg"
       , "370001e0:rw:ra:epos:rb"},
   {"reg=(reg<<lit)"
       , "370001e0:rw:ra:epos"},
   {"LA=(reg<<lit)*reg"
       , "370001e0:ra:epos:rb"},
   {"LA=(reg<<lit)"
       , "370001e0:ra:epos"},

   // Op code = 56 (0x38)
   {"reg=reg+reg"
       , "380001fe:rw:ra:rb"},
   {"reg=reg+reg,reg=MEMSL(areg)"
       , "38000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg+reg"
       , "380001fe:ra:rb"},
   {"LA=reg+reg,reg=MEMSL(areg)"
       , "38000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg+reg,lit,lit)"
       , "38000000:rw:ra:rb:iepos:iewid"},

   // Op code = 57 (0x39)
   {"reg=reg-reg"
       , "390001fe:rw:ra:rb"},
   {"reg=reg-reg,reg=MEMSL(areg)"
       , "39000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg-reg"
       , "390001fe:ra:rb"},
   {"LA=reg-reg,reg=MEMSL(areg)"
       , "39000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg-reg,lit,lit)"
       , "39000000:rw:ra:rb:iepos:iewid"},

   // Op code = 58 (0x3a)
   {"reg=reg&reg"
       , "3a0001fe:rw:ra:rb"},
   {"reg=reg&reg,reg=MEMSL(areg)"
       , "3a000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg&reg"
       , "3a0001fe:ra:rb"},
   {"LA=reg&reg,reg=MEMSL(areg)"
       , "3a000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg&reg,lit,lit)"
       , "3a000000:rw:ra:rb:iepos:iewid"},

   // Op code = 59 (0x3b)
   {"reg=reg|reg"
       , "3b0001fe:rw:ra:rb"},
   {"reg=reg|reg,reg=MEMSL(areg)"
       , "3b000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg|reg"
       , "3b0001fe:ra:rb"},
   {"LA=reg|reg,reg=MEMSL(areg)"
       , "3b000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg|reg,lit,lit)"
       , "3b000000:rw:ra:rb:iepos:iewid"},

   // Op code = 60 (0x3c)
   {"reg=reg^reg"
       , "3c0001fe:rw:ra:rb"},
   {"reg=reg^reg,reg=MEMSL(areg)"
       , "3c000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=reg^reg"
       , "3c0001fe:ra:rb"},
   {"LA=reg^reg,reg=MEMSL(areg)"
       , "3c000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(reg^reg,lit,lit)"
       , "3c000000:rw:ra:rb:iepos:iewid"},

   // Op code = 61 (0x3d)
   {"reg=EXT(reg)+reg"
       , "3d0001fe:rw:ra:rb"},
   {"reg=EXT(reg)+reg,reg=MEMSL(areg)"
       , "3d000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)+reg"
       , "3d0001fe:ra:rb"},
   {"LA=EXT(reg)+reg,reg=MEMSL(areg)"
       , "3d000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)+reg,lit,lit)"
       , "3d000000:rw:ra:rb:iepos:iewid"},

   // Op code = 62 (0x3e)
   {"reg=EXT(reg)-reg"
       , "3e0001fe:rw:ra:rb"},
   {"reg=EXT(reg)-reg,reg=MEMSL(areg)"
       , "3e000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)-reg"
       , "3e0001fe:ra:rb"},
   {"LA=EXT(reg)-reg,reg=MEMSL(areg)"
       , "3e000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)-reg,lit,lit)"
       , "3e000000:rw:ra:rb:iepos:iewid"},

   // Op code = 63 (0x3f)
   {"reg=EXT(reg)&reg"
       , "3f0001fe:rw:ra:rb"},
   {"reg=EXT(reg)&reg,reg=MEMSL(areg)"
       , "3f000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)&reg"
       , "3f0001fe:ra:rb"},
   {"LA=EXT(reg)&reg,reg=MEMSL(areg)"
       , "3f000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)&reg,lit,lit)"
       , "3f000000:rw:ra:rb:iepos:iewid"},

   // Op code = 64 (0x40)
   {"reg=EXT(reg)|reg"
       , "400001fe:rw:ra:rb"},
   {"reg=EXT(reg)|reg,reg=MEMSL(areg)"
       , "40000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)|reg"
       , "400001fe:ra:rb"},
   {"LA=EXT(reg)|reg,reg=MEMSL(areg)"
       , "40000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)|reg,lit,lit)"
       , "40000000:rw:ra:rb:iepos:iewid"},

   // Op code = 65 (0x41)
   {"reg=EXT(reg)^reg"
       , "410001fe:rw:ra:rb"},
   {"reg=EXT(reg)^reg,reg=MEMSL(areg)"
       , "41000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)^reg"
       , "410001fe:ra:rb"},
   {"LA=EXT(reg)^reg,reg=MEMSL(areg)"
       , "41000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)^reg,lit,lit)"
       , "41000000:rw:ra:rb:iepos:iewid"},

   // Op code = 66 (0x42)
   {"reg=EXTI(reg)+reg"
       , "420001fe:rw:ra:rb"},
   {"reg=EXTI(reg)+reg,reg=MEMSL(areg)"
       , "42000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)+reg"
       , "420001fe:ra:rb"},
   {"LA=EXTI(reg)+reg,reg=MEMSL(areg)"
       , "42000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)+reg,lit,lit)"
       , "42000000:rw:ra:rb:iepos:iewid"},

   // Op code = 67 (0x43)
   {"reg=EXTI(reg)-reg"
       , "430001fe:rw:ra:rb"},
   {"reg=EXTI(reg)-reg,reg=MEMSL(areg)"
       , "43000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)-reg"
       , "430001fe:ra:rb"},
   {"LA=EXTI(reg)-reg,reg=MEMSL(areg)"
       , "43000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)-reg,lit,lit)"
       , "43000000:rw:ra:rb:iepos:iewid"},

   // Op code = 68 (0x44)
   {"reg=EXTI(reg)&reg"
       , "440001fe:rw:ra:rb"},
   {"reg=EXTI(reg)&reg,reg=MEMSL(areg)"
       , "44000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)&reg"
       , "440001fe:ra:rb"},
   {"LA=EXTI(reg)&reg,reg=MEMSL(areg)"
       , "44000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)&reg,lit,lit)"
       , "44000000:rw:ra:rb:iepos:iewid"},

   // Op code = 69 (0x45)
   {"reg=EXTI(reg)|reg"
       , "450001fe:rw:ra:rb"},
   {"reg=EXTI(reg)|reg,reg=MEMSL(areg)"
       , "45000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)|reg"
       , "450001fe:ra:rb"},
   {"LA=EXTI(reg)|reg,reg=MEMSL(areg)"
       , "45000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)|reg,lit,lit)"
       , "45000000:rw:ra:rb:iepos:iewid"},

   // Op code = 70 (0x46)
   {"reg=EXTI(reg)^reg"
       , "460001fe:rw:ra:rb"},
   {"reg=EXTI(reg)^reg,reg=MEMSL(areg)"
       , "46000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)^reg"
       , "460001fe:ra:rb"},
   {"LA=EXTI(reg)^reg,reg=MEMSL(areg)"
       , "46000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)^reg,lit,lit)"
       , "46000000:rw:ra:rb:iepos:iewid"},

   // Op code = 71 (0x47)
   {"reg=EXTL(reg,lit,lit)+reg"
       , "47000000:rw:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)+reg"
       , "47000000:ra:epos:ewid:rb"},
   {"reg=(reg<<lit)+reg"
       , "470001e0:rw:ra:epos:rb"},
   {"LA=(reg<<lit)+reg"
       , "470001e0:ra:epos:rb"},

   // Op code = 72 (0x48)
   {"reg=EXTL(reg,lit,lit)-reg"
       , "48000000:rw:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)-reg"
       , "48000000:ra:epos:ewid:rb"},
   {"reg=(reg<<lit)-reg"
       , "480001e0:rw:ra:epos:rb"},
   {"LA=(reg<<lit)-reg"
       , "480001e0:ra:epos:rb"},

   // Op code = 73 (0x49)
   {"reg=EXTL(reg,lit,lit)&reg"
       , "49000000:rw:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)&reg"
       , "49000000:ra:epos:ewid:rb"},
   {"reg=(reg<<lit)&reg"
       , "490001e0:rw:ra:epos:rb"},
   {"LA=(reg<<lit)&reg"
       , "490001e0:ra:epos:rb"},

   // Op code = 74 (0x4a)
   {"reg=EXTL(reg,lit,lit)|reg"
       , "4a000000:rw:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)|reg"
       , "4a000000:ra:epos:ewid:rb"},
   {"reg=(reg<<lit)|reg"
       , "4a0001e0:rw:ra:epos:rb"},
   {"LA=(reg<<lit)|reg"
       , "4a0001e0:ra:epos:rb"},

   // Op code = 75 (0x4b)
   {"reg=EXTL(reg,lit,lit)^reg"
       , "4b000000:rw:ra:epos:ewid:rb"},
   {"LA=EXTL(reg,lit,lit)^reg"
       , "4b000000:ra:epos:ewid:rb"},
   {"reg=(reg<<lit)^reg"
       , "4b0001e0:rw:ra:epos:rb"},
   {"LA=(reg<<lit)^reg"
       , "4b0001e0:ra:epos:rb"},

   // Op code = 76 (0x4c)
   {"reg=lit-reg"
       , "4c000000:rw:lit:ra"},
   {"LA=lit-reg"
       , "4c000000:lit:ra"},

   // Op code = 77 (0x4d)
   {"reg=lit-EXT(reg)"
       , "4d000000:rw:lit:ra"},
   {"LA=lit-EXT(reg)"
       , "4d000000:lit:ra"},

   // Op code = 78 (0x4e)
   {"reg=lit-EXTI(reg)"
       , "4e000000:rw:lit:ra"},
   {"LA=lit-EXTI(reg)"
       , "4e000000:lit:ra"},

   // Op code = 79 (0x4f)
   {"reg=INS(reg*lit,LI)"
       , "4f000000:rw:ra:lit"},
   {"INS(LA=reg*lit,LI)"
       , "4f000000:ra:lit"},

   // Op code = 80 (0x50)
   {"reg=INS(EXT(reg)*lit,LI)"
       , "50000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*lit,LI)"
       , "50000000:ra:lit"},

   // Op code = 81 (0x51)
   {"reg=INS(EXTI(reg)*lit,LI)"
       , "51000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*lit,LI)"
       , "51000000:ra:lit"},

   // Op code = 82 (0x52)
   {"reg=INS(reg*lit+reg,LI)"
       , "52000000:rw:ra:lit:rb"},
   {"reg=INS(reg*lit+LA,LI)"
       , "52000000:rw:ra:lit"},
   {"INS(LA=reg*lit+reg,LI)"
       , "52000000:ra:lit:rb"},
   {"INS(LA=reg*lit+LA,LI)"
       , "52000000:ra:lit"},

   // Op code = 83 (0x53)
   {"reg=INS(reg*lit-reg,LI)"
       , "53000000:rw:ra:lit:rb"},
   {"reg=INS(reg*lit-LA,LI)"
       , "53000000:rw:ra:lit"},
   {"INS(LA=reg*lit-reg,LI)"
       , "53000000:ra:lit:rb"},
   {"INS(LA=reg*lit-LA,LI)"
       , "53000000:ra:lit"},

   // Op code = 84 (0x54)
   {"reg=INS(reg*lit&reg,LI)"
       , "54000000:rw:ra:lit:rb"},
   {"reg=INS(reg*lit&LA,LI)"
       , "54000000:rw:ra:lit"},
   {"INS(LA=reg*lit&reg,LI)"
       , "54000000:ra:lit:rb"},
   {"INS(LA=reg*lit&LA,LI)"
       , "54000000:ra:lit"},

   // Op code = 85 (0x55)
   {"reg=INS(reg*lit|reg,LI)"
       , "55000000:rw:ra:lit:rb"},
   {"reg=INS(reg*lit|LA,LI)"
       , "55000000:rw:ra:lit"},
   {"INS(LA=reg*lit|reg,LI)"
       , "55000000:ra:lit:rb"},
   {"INS(LA=reg*lit|LA,LI)"
       , "55000000:ra:lit"},

   // Op code = 86 (0x56)
   {"reg=INS(reg*lit^reg,LI)"
       , "56000000:rw:ra:lit:rb"},
   {"reg=INS(reg*lit^LA,LI)"
       , "56000000:rw:ra:lit"},
   {"INS(LA=reg*lit^reg,LI)"
       , "56000000:ra:lit:rb"},
   {"INS(LA=reg*lit^LA,LI)"
       , "56000000:ra:lit"},

   // Op code = 87 (0x57)
   {"reg=INS(EXT(reg)*lit+reg,LI)"
       , "57000000:rw:ra:lit:rb"},
   {"reg=INS(EXT(reg)*lit+LA,LI)"
       , "57000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*lit+reg,LI)"
       , "57000000:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit+LA,LI)"
       , "57000000:ra:lit"},

   // Op code = 88 (0x58)
   {"reg=INS(EXT(reg)*lit-reg,LI)"
       , "58000000:rw:ra:lit:rb"},
   {"reg=INS(EXT(reg)*lit-LA,LI)"
       , "58000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*lit-reg,LI)"
       , "58000000:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit-LA,LI)"
       , "58000000:ra:lit"},

   // Op code = 89 (0x59)
   {"reg=INS(EXT(reg)*lit&reg,LI)"
       , "59000000:rw:ra:lit:rb"},
   {"reg=INS(EXT(reg)*lit&LA,LI)"
       , "59000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*lit&reg,LI)"
       , "59000000:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit&LA,LI)"
       , "59000000:ra:lit"},

   // Op code = 90 (0x5a)
   {"reg=INS(EXT(reg)*lit|reg,LI)"
       , "5a000000:rw:ra:lit:rb"},
   {"reg=INS(EXT(reg)*lit|LA,LI)"
       , "5a000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*lit|reg,LI)"
       , "5a000000:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit|LA,LI)"
       , "5a000000:ra:lit"},

   // Op code = 91 (0x5b)
   {"reg=INS(EXT(reg)*lit^reg,LI)"
       , "5b000000:rw:ra:lit:rb"},
   {"reg=INS(EXT(reg)*lit^LA,LI)"
       , "5b000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*lit^reg,LI)"
       , "5b000000:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit^LA,LI)"
       , "5b000000:ra:lit"},

   // Op code = 92 (0x5c)
   {"reg=INS(EXTI(reg)*lit+reg,LI)"
       , "5c000000:rw:ra:lit:rb"},
   {"reg=INS(EXTI(reg)*lit+LA,LI)"
       , "5c000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*lit+reg,LI)"
       , "5c000000:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit+LA,LI)"
       , "5c000000:ra:lit"},

   // Op code = 93 (0x5d)
   {"reg=INS(EXTI(reg)*lit-reg,LI)"
       , "5d000000:rw:ra:lit:rb"},
   {"reg=INS(EXTI(reg)*lit-LA,LI)"
       , "5d000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*lit-reg,LI)"
       , "5d000000:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit-LA,LI)"
       , "5d000000:ra:lit"},

   // Op code = 94 (0x5e)
   {"reg=INS(EXTI(reg)*lit&reg,LI)"
       , "5e000000:rw:ra:lit:rb"},
   {"reg=INS(EXTI(reg)*lit&LA,LI)"
       , "5e000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*lit&reg,LI)"
       , "5e000000:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit&LA,LI)"
       , "5e000000:ra:lit"},

   // Op code = 95 (0x5f)
   {"reg=INS(EXTI(reg)*lit|reg,LI)"
       , "5f000000:rw:ra:lit:rb"},
   {"reg=INS(EXTI(reg)*lit|LA,LI)"
       , "5f000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*lit|reg,LI)"
       , "5f000000:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit|LA,LI)"
       , "5f000000:ra:lit"},

   // Op code = 96 (0x60)
   {"reg=INS(EXTI(reg)*lit^reg,LI)"
       , "60000000:rw:ra:lit:rb"},
   {"reg=INS(EXTI(reg)*lit^LA,LI)"
       , "60000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*lit^reg,LI)"
       , "60000000:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit^LA,LI)"
       , "60000000:ra:lit"},

   // Op code = 97 (0x61)
   {"reg=INS(reg*reg+lit,LI)"
       , "61000000:rw:ra:rb:lit"},
   {"reg=INS(reg+lit,LI)"
       , "61000000:rw:ra:lit"},
   {"INS(LA=reg*reg+lit,LI)"
       , "61000000:ra:rb:lit"},
   {"INS(LA=reg+lit,LI)"
       , "61000000:ra:lit"},

   // Op code = 98 (0x62)
   {"reg=INS(reg*reg&lit,LI)"
       , "62000000:rw:ra:rb:lit"},
   {"reg=INS(reg&lit,LI)"
       , "62000000:rw:ra:lit"},
   {"INS(LA=reg*reg&lit,LI)"
       , "62000000:ra:rb:lit"},
   {"INS(LA=reg&lit,LI)"
       , "62000000:ra:lit"},

   // Op code = 99 (0x63)
   {"reg=INS(reg*reg|lit,LI)"
       , "63000000:rw:ra:rb:lit"},
   {"reg=INS(reg|lit,LI)"
       , "63000000:rw:ra:lit"},
   {"INS(LA=reg*reg|lit,LI)"
       , "63000000:ra:rb:lit"},
   {"INS(LA=reg|lit,LI)"
       , "63000000:ra:lit"},

   // Op code = 100 (0x64)
   {"reg=INS(reg*reg^lit,LI)"
       , "64000000:rw:ra:rb:lit"},
   {"reg=INS(reg^lit,LI)"
       , "64000000:rw:ra:lit"},
   {"INS(LA=reg*reg^lit,LI)"
       , "64000000:ra:rb:lit"},
   {"INS(LA=reg^lit,LI)"
       , "64000000:ra:lit"},

   // Op code = 101 (0x65)
   {"reg=INS(EXT(reg)*reg+lit,LI)"
       , "65000000:rw:ra:rb:lit"},
   {"reg=INS(EXT(reg)+lit,LI)"
       , "65000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*reg+lit,LI)"
       , "65000000:ra:rb:lit"},
   {"INS(LA=EXT(reg)+lit,LI)"
       , "65000000:ra:lit"},

   // Op code = 102 (0x66)
   {"reg=INS(EXT(reg)*reg&lit,LI)"
       , "66000000:rw:ra:rb:lit"},
   {"reg=INS(EXT(reg)&lit,LI)"
       , "66000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*reg&lit,LI)"
       , "66000000:ra:rb:lit"},
   {"INS(LA=EXT(reg)&lit,LI)"
       , "66000000:ra:lit"},

   // Op code = 103 (0x67)
   {"reg=INS(EXT(reg)*reg|lit,LI)"
       , "67000000:rw:ra:rb:lit"},
   {"reg=INS(EXT(reg)|lit,LI)"
       , "67000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*reg|lit,LI)"
       , "67000000:ra:rb:lit"},
   {"INS(LA=EXT(reg)|lit,LI)"
       , "67000000:ra:lit"},

   // Op code = 104 (0x68)
   {"reg=INS(EXT(reg)*reg^lit,LI)"
       , "68000000:rw:ra:rb:lit"},
   {"reg=INS(EXT(reg)^lit,LI)"
       , "68000000:rw:ra:lit"},
   {"INS(LA=EXT(reg)*reg^lit,LI)"
       , "68000000:ra:rb:lit"},
   {"INS(LA=EXT(reg)^lit,LI)"
       , "68000000:ra:lit"},

   // Op code = 105 (0x69)
   {"reg=INS(EXTI(reg)*reg+lit,LI)"
       , "69000000:rw:ra:rb:lit"},
   {"reg=INS(EXTI(reg)+lit,LI)"
       , "69000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*reg+lit,LI)"
       , "69000000:ra:rb:lit"},
   {"INS(LA=EXTI(reg)+lit,LI)"
       , "69000000:ra:lit"},

   // Op code = 106 (0x6a)
   {"reg=INS(EXTI(reg)*reg&lit,LI)"
       , "6a000000:rw:ra:rb:lit"},
   {"reg=INS(EXTI(reg)&lit,LI)"
       , "6a000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*reg&lit,LI)"
       , "6a000000:ra:rb:lit"},
   {"INS(LA=EXTI(reg)&lit,LI)"
       , "6a000000:ra:lit"},

   // Op code = 107 (0x6b)
   {"reg=INS(EXTI(reg)*reg|lit,LI)"
       , "6b000000:rw:ra:rb:lit"},
   {"reg=INS(EXTI(reg)|lit,LI)"
       , "6b000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*reg|lit,LI)"
       , "6b000000:ra:rb:lit"},
   {"INS(LA=EXTI(reg)|lit,LI)"
       , "6b000000:ra:lit"},

   // Op code = 108 (0x6c)
   {"reg=INS(EXTI(reg)*reg^lit,LI)"
       , "6c000000:rw:ra:rb:lit"},
   {"reg=INS(EXTI(reg)^lit,LI)"
       , "6c000000:rw:ra:lit"},
   {"INS(LA=EXTI(reg)*reg^lit,LI)"
       , "6c000000:ra:rb:lit"},
   {"INS(LA=EXTI(reg)^lit,LI)"
       , "6c000000:ra:lit"},

   // Op code = 109 (0x6d)
   {"reg=INS(reg*reg+LA,LI)"
       , "6d000000:rw:ra:rb"},
   {"reg=INS(reg+LA,LI)"
       , "6d000000:rw:ra"},
   {"reg=INS(reg*reg+LA,LI),reg=MEMSL(areg)"
       , "6d000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(reg+LA,LI),reg=MEMSL(areg)"
       , "6d000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=reg*reg+LA,LI)"
       , "6d000000:ra:rb"},
   {"INS(LA=reg+LA,LI)"
       , "6d000000:ra"},
   {"INS(LA=reg*reg+LA,LI),reg=MEMSL(areg)"
       , "6d000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg+LA,LI),reg=MEMSL(areg)"
       , "6d000001:ra:pmem_rm:pmem_ar"},

   // Op code = 110 (0x6e)
   {"reg=INS(reg*reg-LA,LI)"
       , "6e000000:rw:ra:rb"},
   {"reg=INS(reg-LA,LI)"
       , "6e000000:rw:ra"},
   {"reg=INS(reg*reg-LA,LI),reg=MEMSL(areg)"
       , "6e000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(reg-LA,LI),reg=MEMSL(areg)"
       , "6e000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=reg*reg-LA,LI)"
       , "6e000000:ra:rb"},
   {"INS(LA=reg-LA,LI)"
       , "6e000000:ra"},
   {"INS(LA=reg*reg-LA,LI),reg=MEMSL(areg)"
       , "6e000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg-LA,LI),reg=MEMSL(areg)"
       , "6e000001:ra:pmem_rm:pmem_ar"},

   // Op code = 111 (0x6f)
   {"reg=INS(reg*reg&LA,LI)"
       , "6f000000:rw:ra:rb"},
   {"reg=INS(reg&LA,LI)"
       , "6f000000:rw:ra"},
   {"reg=INS(reg*reg&LA,LI),reg=MEMSL(areg)"
       , "6f000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(reg&LA,LI),reg=MEMSL(areg)"
       , "6f000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=reg*reg&LA,LI)"
       , "6f000000:ra:rb"},
   {"INS(LA=reg&LA,LI)"
       , "6f000000:ra"},
   {"INS(LA=reg*reg&LA,LI),reg=MEMSL(areg)"
       , "6f000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg&LA,LI),reg=MEMSL(areg)"
       , "6f000001:ra:pmem_rm:pmem_ar"},

   // Op code = 112 (0x70)
   {"reg=INS(reg*reg|LA,LI)"
       , "70000000:rw:ra:rb"},
   {"reg=INS(reg|LA,LI)"
       , "70000000:rw:ra"},
   {"reg=INS(reg*reg|LA,LI),reg=MEMSL(areg)"
       , "70000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(reg|LA,LI),reg=MEMSL(areg)"
       , "70000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=reg*reg|LA,LI)"
       , "70000000:ra:rb"},
   {"INS(LA=reg|LA,LI)"
       , "70000000:ra"},
   {"INS(LA=reg*reg|LA,LI),reg=MEMSL(areg)"
       , "70000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg|LA,LI),reg=MEMSL(areg)"
       , "70000001:ra:pmem_rm:pmem_ar"},

   // Op code = 113 (0x71)
   {"reg=INS(reg*reg^LA,LI)"
       , "71000000:rw:ra:rb"},
   {"reg=INS(reg^LA,LI)"
       , "71000000:rw:ra"},
   {"reg=INS(reg*reg^LA,LI),reg=MEMSL(areg)"
       , "71000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(reg^LA,LI),reg=MEMSL(areg)"
       , "71000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=reg*reg^LA,LI)"
       , "71000000:ra:rb"},
   {"INS(LA=reg^LA,LI)"
       , "71000000:ra"},
   {"INS(LA=reg*reg^LA,LI),reg=MEMSL(areg)"
       , "71000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg^LA,LI),reg=MEMSL(areg)"
       , "71000001:ra:pmem_rm:pmem_ar"},

   // Op code = 114 (0x72)
   {"reg=INS(EXT(reg)*reg+LA,LI)"
       , "72000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)+LA,LI)"
       , "72000000:rw:ra"},
   {"reg=INS(EXT(reg)*reg+LA,LI),reg=MEMSL(areg)"
       , "72000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXT(reg)+LA,LI),reg=MEMSL(areg)"
       , "72000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)*reg+LA,LI)"
       , "72000000:ra:rb"},
   {"INS(LA=EXT(reg)+LA,LI)"
       , "72000000:ra"},
   {"INS(LA=EXT(reg)*reg+LA,LI),reg=MEMSL(areg)"
       , "72000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)+LA,LI),reg=MEMSL(areg)"
       , "72000001:ra:pmem_rm:pmem_ar"},

   // Op code = 115 (0x73)
   {"reg=INS(EXT(reg)*reg-LA,LI)"
       , "73000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)-LA,LI)"
       , "73000000:rw:ra"},
   {"reg=INS(EXT(reg)*reg-LA,LI),reg=MEMSL(areg)"
       , "73000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXT(reg)-LA,LI),reg=MEMSL(areg)"
       , "73000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)*reg-LA,LI)"
       , "73000000:ra:rb"},
   {"INS(LA=EXT(reg)-LA,LI)"
       , "73000000:ra"},
   {"INS(LA=EXT(reg)*reg-LA,LI),reg=MEMSL(areg)"
       , "73000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)-LA,LI),reg=MEMSL(areg)"
       , "73000001:ra:pmem_rm:pmem_ar"},

   // Op code = 116 (0x74)
   {"reg=INS(EXT(reg)*reg&LA,LI)"
       , "74000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)&LA,LI)"
       , "74000000:rw:ra"},
   {"reg=INS(EXT(reg)*reg&LA,LI),reg=MEMSL(areg)"
       , "74000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXT(reg)&LA,LI),reg=MEMSL(areg)"
       , "74000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)*reg&LA,LI)"
       , "74000000:ra:rb"},
   {"INS(LA=EXT(reg)&LA,LI)"
       , "74000000:ra"},
   {"INS(LA=EXT(reg)*reg&LA,LI),reg=MEMSL(areg)"
       , "74000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)&LA,LI),reg=MEMSL(areg)"
       , "74000001:ra:pmem_rm:pmem_ar"},

   // Op code = 117 (0x75)
   {"reg=INS(EXT(reg)*reg|LA,LI)"
       , "75000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)|LA,LI)"
       , "75000000:rw:ra"},
   {"reg=INS(EXT(reg)*reg|LA,LI),reg=MEMSL(areg)"
       , "75000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXT(reg)|LA,LI),reg=MEMSL(areg)"
       , "75000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)*reg|LA,LI)"
       , "75000000:ra:rb"},
   {"INS(LA=EXT(reg)|LA,LI)"
       , "75000000:ra"},
   {"INS(LA=EXT(reg)*reg|LA,LI),reg=MEMSL(areg)"
       , "75000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)|LA,LI),reg=MEMSL(areg)"
       , "75000001:ra:pmem_rm:pmem_ar"},

   // Op code = 118 (0x76)
   {"reg=INS(EXT(reg)*reg^LA,LI)"
       , "76000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)^LA,LI)"
       , "76000000:rw:ra"},
   {"reg=INS(EXT(reg)*reg^LA,LI),reg=MEMSL(areg)"
       , "76000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXT(reg)^LA,LI),reg=MEMSL(areg)"
       , "76000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)*reg^LA,LI)"
       , "76000000:ra:rb"},
   {"INS(LA=EXT(reg)^LA,LI)"
       , "76000000:ra"},
   {"INS(LA=EXT(reg)*reg^LA,LI),reg=MEMSL(areg)"
       , "76000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)^LA,LI),reg=MEMSL(areg)"
       , "76000001:ra:pmem_rm:pmem_ar"},

   // Op code = 119 (0x77)
   {"reg=INS(EXTI(reg)*reg+LA,LI)"
       , "77000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)+LA,LI)"
       , "77000000:rw:ra"},
   {"reg=INS(EXTI(reg)*reg+LA,LI),reg=MEMSL(areg)"
       , "77000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXTI(reg)+LA,LI),reg=MEMSL(areg)"
       , "77000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)*reg+LA,LI)"
       , "77000000:ra:rb"},
   {"INS(LA=EXTI(reg)+LA,LI)"
       , "77000000:ra"},
   {"INS(LA=EXTI(reg)*reg+LA,LI),reg=MEMSL(areg)"
       , "77000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)+LA,LI),reg=MEMSL(areg)"
       , "77000001:ra:pmem_rm:pmem_ar"},

   // Op code = 120 (0x78)
   {"reg=INS(EXTI(reg)*reg-LA,LI)"
       , "78000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)-LA,LI)"
       , "78000000:rw:ra"},
   {"reg=INS(EXTI(reg)*reg-LA,LI),reg=MEMSL(areg)"
       , "78000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXTI(reg)-LA,LI),reg=MEMSL(areg)"
       , "78000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)*reg-LA,LI)"
       , "78000000:ra:rb"},
   {"INS(LA=EXTI(reg)-LA,LI)"
       , "78000000:ra"},
   {"INS(LA=EXTI(reg)*reg-LA,LI),reg=MEMSL(areg)"
       , "78000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)-LA,LI),reg=MEMSL(areg)"
       , "78000001:ra:pmem_rm:pmem_ar"},

   // Op code = 121 (0x79)
   {"reg=INS(EXTI(reg)*reg&LA,LI)"
       , "79000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)&LA,LI)"
       , "79000000:rw:ra"},
   {"reg=INS(EXTI(reg)*reg&LA,LI),reg=MEMSL(areg)"
       , "79000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXTI(reg)&LA,LI),reg=MEMSL(areg)"
       , "79000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)*reg&LA,LI)"
       , "79000000:ra:rb"},
   {"INS(LA=EXTI(reg)&LA,LI)"
       , "79000000:ra"},
   {"INS(LA=EXTI(reg)*reg&LA,LI),reg=MEMSL(areg)"
       , "79000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)&LA,LI),reg=MEMSL(areg)"
       , "79000001:ra:pmem_rm:pmem_ar"},

   // Op code = 122 (0x7a)
   {"reg=INS(EXTI(reg)*reg|LA,LI)"
       , "7a000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)|LA,LI)"
       , "7a000000:rw:ra"},
   {"reg=INS(EXTI(reg)*reg|LA,LI),reg=MEMSL(areg)"
       , "7a000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXTI(reg)|LA,LI),reg=MEMSL(areg)"
       , "7a000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)*reg|LA,LI)"
       , "7a000000:ra:rb"},
   {"INS(LA=EXTI(reg)|LA,LI)"
       , "7a000000:ra"},
   {"INS(LA=EXTI(reg)*reg|LA,LI),reg=MEMSL(areg)"
       , "7a000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)|LA,LI),reg=MEMSL(areg)"
       , "7a000001:ra:pmem_rm:pmem_ar"},

   // Op code = 123 (0x7b)
   {"reg=INS(EXTI(reg)*reg^LA,LI)"
       , "7b000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)^LA,LI)"
       , "7b000000:rw:ra"},
   {"reg=INS(EXTI(reg)*reg^LA,LI),reg=MEMSL(areg)"
       , "7b000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXTI(reg)^LA,LI),reg=MEMSL(areg)"
       , "7b000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)*reg^LA,LI)"
       , "7b000000:ra:rb"},
   {"INS(LA=EXTI(reg)^LA,LI)"
       , "7b000000:ra"},
   {"INS(LA=EXTI(reg)*reg^LA,LI),reg=MEMSL(areg)"
       , "7b000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)^LA,LI),reg=MEMSL(areg)"
       , "7b000001:ra:pmem_rm:pmem_ar"},

   // Op code = 124 (0x7c)
   {"reg=INS(EXTL(reg,lit,lit)*reg+LA,LI)"
       , "7c000000:rw:ra:epos:ewid:rb"},
   {"reg=INS(EXTL(reg,lit,lit)+LA,LI)"
       , "7c000000:rw:ra:epos:ewid"},
   {"INS(LA=EXTL(reg,lit,lit)*reg+LA,LI)"
       , "7c000000:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)+LA,LI)"
       , "7c000000:ra:epos:ewid"},
   {"reg=INS((reg<<lit)*reg+LA,LI)"
       , "7c0001e0:rw:ra:epos:rb"},
   {"reg=INS((reg<<lit)+LA,LI)"
       , "7c0001e0:rw:ra:epos"},
   {"INS(LA=(reg<<lit)*reg+LA,LI)"
       , "7c0001e0:ra:epos:rb"},
   {"INS(LA=(reg<<lit)+LA,LI)"
       , "7c0001e0:ra:epos"},

   // Op code = 125 (0x7d)
   {"reg=INS(EXTL(reg,lit,lit)*reg-LA,LI)"
       , "7d000000:rw:ra:epos:ewid:rb"},
   {"reg=INS(EXTL(reg,lit,lit)-LA,LI)"
       , "7d000000:rw:ra:epos:ewid"},
   {"INS(LA=EXTL(reg,lit,lit)*reg-LA,LI)"
       , "7d000000:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)-LA,LI)"
       , "7d000000:ra:epos:ewid"},
   {"reg=INS((reg<<lit)*reg-LA,LI)"
       , "7d0001e0:rw:ra:epos:rb"},
   {"reg=INS((reg<<lit)-LA,LI)"
       , "7d0001e0:rw:ra:epos"},
   {"INS(LA=(reg<<lit)*reg-LA,LI)"
       , "7d0001e0:ra:epos:rb"},
   {"INS(LA=(reg<<lit)-LA,LI)"
       , "7d0001e0:ra:epos"},

   // Op code = 126 (0x7e)
   {"reg=INS(EXTL(reg,lit,lit)*reg&LA,LI)"
       , "7e000000:rw:ra:epos:ewid:rb"},
   {"reg=INS(EXTL(reg,lit,lit)&LA,LI)"
       , "7e000000:rw:ra:epos:ewid"},
   {"INS(LA=EXTL(reg,lit,lit)*reg&LA,LI)"
       , "7e000000:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)&LA,LI)"
       , "7e000000:ra:epos:ewid"},
   {"reg=INS((reg<<lit)*reg&LA,LI)"
       , "7e0001e0:rw:ra:epos:rb"},
   {"reg=INS((reg<<lit)&LA,LI)"
       , "7e0001e0:rw:ra:epos"},
   {"INS(LA=(reg<<lit)*reg&LA,LI)"
       , "7e0001e0:ra:epos:rb"},
   {"INS(LA=(reg<<lit)&LA,LI)"
       , "7e0001e0:ra:epos"},

   // Op code = 127 (0x7f)
   {"reg=INS(EXTL(reg,lit,lit)*reg|LA,LI)"
       , "7f000000:rw:ra:epos:ewid:rb"},
   {"reg=INS(EXTL(reg,lit,lit)|LA,LI)"
       , "7f000000:rw:ra:epos:ewid"},
   {"INS(LA=EXTL(reg,lit,lit)*reg|LA,LI)"
       , "7f000000:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)|LA,LI)"
       , "7f000000:ra:epos:ewid"},
   {"reg=INS((reg<<lit)*reg|LA,LI)"
       , "7f0001e0:rw:ra:epos:rb"},
   {"reg=INS((reg<<lit)|LA,LI)"
       , "7f0001e0:rw:ra:epos"},
   {"INS(LA=(reg<<lit)*reg|LA,LI)"
       , "7f0001e0:ra:epos:rb"},
   {"INS(LA=(reg<<lit)|LA,LI)"
       , "7f0001e0:ra:epos"},

   // Op code = 128 (0x80)
   {"reg=INS(EXTL(reg,lit,lit)*reg^LA,LI)"
       , "80000000:rw:ra:epos:ewid:rb"},
   {"reg=INS(EXTL(reg,lit,lit)^LA,LI)"
       , "80000000:rw:ra:epos:ewid"},
   {"INS(LA=EXTL(reg,lit,lit)*reg^LA,LI)"
       , "80000000:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)^LA,LI)"
       , "80000000:ra:epos:ewid"},
   {"reg=INS((reg<<lit)*reg^LA,LI)"
       , "800001e0:rw:ra:epos:rb"},
   {"reg=INS((reg<<lit)^LA,LI)"
       , "800001e0:rw:ra:epos"},
   {"INS(LA=(reg<<lit)*reg^LA,LI)"
       , "800001e0:ra:epos:rb"},
   {"INS(LA=(reg<<lit)^LA,LI)"
       , "800001e0:ra:epos"},

   // Op code = 129 (0x81)
   {"reg=INS(reg*reg,LI)"
       , "81000000:rw:ra:rb"},
   {"reg=INS(reg,LI)"
       , "81000000:rw:ra"},
   {"reg=INS(reg*reg,LI),reg=MEMSL(areg)"
       , "81000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(reg,LI),reg=MEMSL(areg)"
       , "81000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=reg*reg,LI)"
       , "81000000:ra:rb"},
   {"INS(LA=reg,LI)"
       , "81000000:ra"},
   {"INS(LA=reg*reg,LI),reg=MEMSL(areg)"
       , "81000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg,LI),reg=MEMSL(areg)"
       , "81000001:ra:pmem_rm:pmem_ar"},

   // Op code = 130 (0x82)
   {"reg=INS(EXT(reg)*reg,LI)"
       , "82000000:rw:ra:rb"},
   {"reg=INS(EXT(reg),LI)"
       , "82000000:rw:ra"},
   {"reg=INS(EXT(reg)*reg,LI),reg=MEMSL(areg)"
       , "82000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXT(reg),LI),reg=MEMSL(areg)"
       , "82000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)*reg,LI)"
       , "82000000:ra:rb"},
   {"INS(LA=EXT(reg),LI)"
       , "82000000:ra"},
   {"INS(LA=EXT(reg)*reg,LI),reg=MEMSL(areg)"
       , "82000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg),LI),reg=MEMSL(areg)"
       , "82000001:ra:pmem_rm:pmem_ar"},

   // Op code = 131 (0x83)
   {"reg=INS(EXTI(reg)*reg,LI)"
       , "83000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg),LI)"
       , "83000000:rw:ra"},
   {"reg=INS(EXTI(reg)*reg,LI),reg=MEMSL(areg)"
       , "83000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"reg=INS(EXTI(reg),LI),reg=MEMSL(areg)"
       , "83000001:rw:ra:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)*reg,LI)"
       , "83000000:ra:rb"},
   {"INS(LA=EXTI(reg),LI)"
       , "83000000:ra"},
   {"INS(LA=EXTI(reg)*reg,LI),reg=MEMSL(areg)"
       , "83000001:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg),LI),reg=MEMSL(areg)"
       , "83000001:ra:pmem_rm:pmem_ar"},

   // Op code = 132 (0x84)
   {"reg=INS(EXTL(reg,lit,lit)*reg,LI)"
       , "84000000:rw:ra:epos:ewid:rb"},
   {"reg=INS(EXTL(reg,lit,lit),LI)"
       , "84000000:rw:ra:epos:ewid"},
   {"INS(LA=EXTL(reg,lit,lit)*reg,LI)"
       , "84000000:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit),LI)"
       , "84000000:ra:epos:ewid"},
   {"reg=INS((reg<<lit)*reg,LI)"
       , "840001e0:rw:ra:epos:rb"},
   {"reg=INS((reg<<lit),LI)"
       , "840001e0:rw:ra:epos"},
   {"INS(LA=(reg<<lit)*reg,LI)"
       , "840001e0:ra:epos:rb"},
   {"INS(LA=(reg<<lit),LI)"
       , "840001e0:ra:epos"},

   // Op code = 133 (0x85)
   {"reg=INS(reg+reg,LI)"
       , "85000000:rw:ra:rb"},
   {"reg=INS(reg+reg,LI),reg=MEMSL(areg)"
       , "85000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg+reg,LI)"
       , "85000000:ra:rb"},
   {"INS(LA=reg+reg,LI),reg=MEMSL(areg)"
       , "85000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 134 (0x86)
   {"reg=INS(reg-reg,LI)"
       , "86000000:rw:ra:rb"},
   {"reg=INS(reg-reg,LI),reg=MEMSL(areg)"
       , "86000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg-reg,LI)"
       , "86000000:ra:rb"},
   {"INS(LA=reg-reg,LI),reg=MEMSL(areg)"
       , "86000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 135 (0x87)
   {"reg=INS(reg&reg,LI)"
       , "87000000:rw:ra:rb"},
   {"reg=INS(reg&reg,LI),reg=MEMSL(areg)"
       , "87000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg&reg,LI)"
       , "87000000:ra:rb"},
   {"INS(LA=reg&reg,LI),reg=MEMSL(areg)"
       , "87000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 136 (0x88)
   {"reg=INS(reg|reg,LI)"
       , "88000000:rw:ra:rb"},
   {"reg=INS(reg|reg,LI),reg=MEMSL(areg)"
       , "88000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg|reg,LI)"
       , "88000000:ra:rb"},
   {"INS(LA=reg|reg,LI),reg=MEMSL(areg)"
       , "88000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 137 (0x89)
   {"reg=INS(reg^reg,LI)"
       , "89000000:rw:ra:rb"},
   {"reg=INS(reg^reg,LI),reg=MEMSL(areg)"
       , "89000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg^reg,LI)"
       , "89000000:ra:rb"},
   {"INS(LA=reg^reg,LI),reg=MEMSL(areg)"
       , "89000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 138 (0x8a)
   {"reg=INS(EXT(reg)+reg,LI)"
       , "8a000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)+reg,LI),reg=MEMSL(areg)"
       , "8a000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)+reg,LI)"
       , "8a000000:ra:rb"},
   {"INS(LA=EXT(reg)+reg,LI),reg=MEMSL(areg)"
       , "8a000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 139 (0x8b)
   {"reg=INS(EXT(reg)-reg,LI)"
       , "8b000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)-reg,LI),reg=MEMSL(areg)"
       , "8b000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)-reg,LI)"
       , "8b000000:ra:rb"},
   {"INS(LA=EXT(reg)-reg,LI),reg=MEMSL(areg)"
       , "8b000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 140 (0x8c)
   {"reg=INS(EXT(reg)&reg,LI)"
       , "8c000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)&reg,LI),reg=MEMSL(areg)"
       , "8c000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)&reg,LI)"
       , "8c000000:ra:rb"},
   {"INS(LA=EXT(reg)&reg,LI),reg=MEMSL(areg)"
       , "8c000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 141 (0x8d)
   {"reg=INS(EXT(reg)|reg,LI)"
       , "8d000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)|reg,LI),reg=MEMSL(areg)"
       , "8d000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)|reg,LI)"
       , "8d000000:ra:rb"},
   {"INS(LA=EXT(reg)|reg,LI),reg=MEMSL(areg)"
       , "8d000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 142 (0x8e)
   {"reg=INS(EXT(reg)^reg,LI)"
       , "8e000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)^reg,LI),reg=MEMSL(areg)"
       , "8e000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)^reg,LI)"
       , "8e000000:ra:rb"},
   {"INS(LA=EXT(reg)^reg,LI),reg=MEMSL(areg)"
       , "8e000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 143 (0x8f)
   {"reg=INS(EXTI(reg)+reg,LI)"
       , "8f000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)+reg,LI),reg=MEMSL(areg)"
       , "8f000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)+reg,LI)"
       , "8f000000:ra:rb"},
   {"INS(LA=EXTI(reg)+reg,LI),reg=MEMSL(areg)"
       , "8f000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 144 (0x90)
   {"reg=INS(EXTI(reg)-reg,LI)"
       , "90000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)-reg,LI),reg=MEMSL(areg)"
       , "90000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)-reg,LI)"
       , "90000000:ra:rb"},
   {"INS(LA=EXTI(reg)-reg,LI),reg=MEMSL(areg)"
       , "90000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 145 (0x91)
   {"reg=INS(EXTI(reg)&reg,LI)"
       , "91000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)&reg,LI),reg=MEMSL(areg)"
       , "91000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)&reg,LI)"
       , "91000000:ra:rb"},
   {"INS(LA=EXTI(reg)&reg,LI),reg=MEMSL(areg)"
       , "91000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 146 (0x92)
   {"reg=INS(EXTI(reg)|reg,LI)"
       , "92000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)|reg,LI),reg=MEMSL(areg)"
       , "92000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)|reg,LI)"
       , "92000000:ra:rb"},
   {"INS(LA=EXTI(reg)|reg,LI),reg=MEMSL(areg)"
       , "92000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 147 (0x93)
   {"reg=INS(EXTI(reg)^reg,LI)"
       , "93000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)^reg,LI),reg=MEMSL(areg)"
       , "93000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)^reg,LI)"
       , "93000000:ra:rb"},
   {"INS(LA=EXTI(reg)^reg,LI),reg=MEMSL(areg)"
       , "93000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 148 (0x94)
   {"reg=INS(EXTL(reg,lit,lit)+reg,LI)"
       , "94000000:rw:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)+reg,LI)"
       , "94000000:ra:epos:ewid:rb"},
   {"reg=INS((reg<<lit)+reg,LI)"
       , "940001e0:rw:ra:epos:rb"},
   {"INS(LA=(reg<<lit)+reg,LI)"
       , "940001e0:ra:epos:rb"},

   // Op code = 149 (0x95)
   {"reg=INS(EXTL(reg,lit,lit)-reg,LI)"
       , "95000000:rw:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)-reg,LI)"
       , "95000000:ra:epos:ewid:rb"},
   {"reg=INS((reg<<lit)-reg,LI)"
       , "950001e0:rw:ra:epos:rb"},
   {"INS(LA=(reg<<lit)-reg,LI)"
       , "950001e0:ra:epos:rb"},

   // Op code = 150 (0x96)
   {"reg=INS(EXTL(reg,lit,lit)&reg,LI)"
       , "96000000:rw:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)&reg,LI)"
       , "96000000:ra:epos:ewid:rb"},
   {"reg=INS((reg<<lit)&reg,LI)"
       , "960001e0:rw:ra:epos:rb"},
   {"INS(LA=(reg<<lit)&reg,LI)"
       , "960001e0:ra:epos:rb"},

   // Op code = 151 (0x97)
   {"reg=INS(EXTL(reg,lit,lit)|reg,LI)"
       , "97000000:rw:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)|reg,LI)"
       , "97000000:ra:epos:ewid:rb"},
   {"reg=INS((reg<<lit)|reg,LI)"
       , "970001e0:rw:ra:epos:rb"},
   {"INS(LA=(reg<<lit)|reg,LI)"
       , "970001e0:ra:epos:rb"},

   // Op code = 152 (0x98)
   {"reg=INS(EXTL(reg,lit,lit)^reg,LI)"
       , "98000000:rw:ra:epos:ewid:rb"},
   {"INS(LA=EXTL(reg,lit,lit)^reg,LI)"
       , "98000000:ra:epos:ewid:rb"},
   {"reg=INS((reg<<lit)^reg,LI)"
       , "980001e0:rw:ra:epos:rb"},
   {"INS(LA=(reg<<lit)^reg,LI)"
       , "980001e0:ra:epos:rb"},

   // Op code = 153 (0x99)
   {"reg=INS(lit-reg,LI)"
       , "99000000:rw:lit:ra"},
   {"INS(LA=lit-reg,LI)"
       , "99000000:lit:ra"},

   // Op code = 154 (0x9a)
   {"reg=INS(lit-EXT(reg),LI)"
       , "9a000000:rw:lit:ra"},
   {"INS(LA=lit-EXT(reg),LI)"
       , "9a000000:lit:ra"},

   // Op code = 155 (0x9b)
   {"reg=INS(lit-EXTI(reg),LI)"
       , "9b000000:rw:lit:ra"},
   {"INS(LA=lit-EXTI(reg),LI)"
       , "9b000000:lit:ra"},

   // Op code = 156 (0x9c)
   {"reg=LA"
       , "9c0001fe:rw"},
   {"reg=LA,reg=MEMSL(areg)"
       , "9c000001:rw:pmem_rm:pmem_ar"},
   {"reg=EXTL(LA,lit,lit)"
       , "9c000000:rw:iepos:iewid"},

   // Op code = 157 (0x9d)
   {"reg=INS(LA,LI)"
       , "9d000000:rw"},
   {"reg=INS(LA,LI),reg=MEMSL(areg)"
       , "9d000001:rw:pmem_rm:pmem_ar"},

   // Op code = 158 (0x9e)
   {"reg=INS(reg*lit,reg)"
       , "9e000000:rw:ra:lit:rb"},
   {"INS(LA=reg*lit,reg)"
       , "9e000000:ra:lit:rb"},

   // Op code = 159 (0x9f)
   {"reg=INS(reg|lit,reg)"
       , "9f000000:rw:ra:lit:rb"},
   {"INS(LA=reg|lit,reg)"
       , "9f000000:ra:lit:rb"},

   // Op code = 160 (0xa0)
   {"reg=INS(reg+lit,reg)"
       , "a0000000:rw:ra:lit:rb"},
   {"INS(LA=reg+lit,reg)"
       , "a0000000:ra:lit:rb"},

   // Op code = 161 (0xa1)
   {"reg=INS(reg|LA,reg)"
       , "a1000000:rw:ra:rb"},
   {"reg=INS(reg|LA,reg),reg=MEMSL(areg)"
       , "a1000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=reg|LA,reg)"
       , "a1000000:ra:rb"},
   {"INS(LA=reg|LA,reg),reg=MEMSL(areg)"
       , "a1000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 162 (0xa2)
   {"reg=INS(reg*lit+LA,reg)"
       , "a2000000:rw:ra:lit:rb"},
   {"INS(LA=reg*lit+LA,reg)"
       , "a2000000:ra:lit:rb"},

   // Op code = 163 (0xa3)
   {"reg=INS(reg*lit-LA,reg)"
       , "a3000000:rw:ra:lit:rb"},
   {"INS(LA=reg*lit-LA,reg)"
       , "a3000000:ra:lit:rb"},

   // Op code = 164 (0xa4)
   {"reg=INS(EXT(reg)*lit,reg)"
       , "a4000000:rw:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit,reg)"
       , "a4000000:ra:lit:rb"},

   // Op code = 165 (0xa5)
   {"reg=INS(EXT(reg)|lit,reg)"
       , "a5000000:rw:ra:lit:rb"},
   {"INS(LA=EXT(reg)|lit,reg)"
       , "a5000000:ra:lit:rb"},

   // Op code = 166 (0xa6)
   {"reg=INS(EXT(reg)+lit,reg)"
       , "a6000000:rw:ra:lit:rb"},
   {"INS(LA=EXT(reg)+lit,reg)"
       , "a6000000:ra:lit:rb"},

   // Op code = 167 (0xa7)
   {"reg=INS(EXT(reg)|LA,reg)"
       , "a7000000:rw:ra:rb"},
   {"reg=INS(EXT(reg)|LA,reg),reg=MEMSL(areg)"
       , "a7000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXT(reg)|LA,reg)"
       , "a7000000:ra:rb"},
   {"INS(LA=EXT(reg)|LA,reg),reg=MEMSL(areg)"
       , "a7000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 168 (0xa8)
   {"reg=INS(EXT(reg)*lit+LA,reg)"
       , "a8000000:rw:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit+LA,reg)"
       , "a8000000:ra:lit:rb"},

   // Op code = 169 (0xa9)
   {"reg=INS(EXT(reg)*lit-LA,reg)"
       , "a9000000:rw:ra:lit:rb"},
   {"INS(LA=EXT(reg)*lit-LA,reg)"
       , "a9000000:ra:lit:rb"},

   // Op code = 170 (0xaa)
   {"reg=INS(EXTI(reg)*lit,reg)"
       , "aa000000:rw:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit,reg)"
       , "aa000000:ra:lit:rb"},

   // Op code = 171 (0xab)
   {"reg=INS(EXTI(reg)|lit,reg)"
       , "ab000000:rw:ra:lit:rb"},
   {"INS(LA=EXTI(reg)|lit,reg)"
       , "ab000000:ra:lit:rb"},

   // Op code = 172 (0xac)
   {"reg=INS(EXTI(reg)+lit,reg)"
       , "ac000000:rw:ra:lit:rb"},
   {"INS(LA=EXTI(reg)+lit,reg)"
       , "ac000000:ra:lit:rb"},

   // Op code = 173 (0xad)
   {"reg=INS(EXTI(reg)|LA,reg)"
       , "ad000000:rw:ra:rb"},
   {"reg=INS(EXTI(reg)|LA,reg),reg=MEMSL(areg)"
       , "ad000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=EXTI(reg)|LA,reg)"
       , "ad000000:ra:rb"},
   {"INS(LA=EXTI(reg)|LA,reg),reg=MEMSL(areg)"
       , "ad000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 174 (0xae)
   {"reg=INS(EXTI(reg)*lit+LA,reg)"
       , "ae000000:rw:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit+LA,reg)"
       , "ae000000:ra:lit:rb"},

   // Op code = 175 (0xaf)
   {"reg=INS(EXTI(reg)*lit-LA,reg)"
       , "af000000:rw:ra:lit:rb"},
   {"INS(LA=EXTI(reg)*lit-LA,reg)"
       , "af000000:ra:lit:rb"},

   // Op code = 176 (0xb0)
   {"reg=EXTR(reg,reg)*lit"
       , "b0000000:rw:ra:rb:lit"},
   {"LA=EXTR(reg,reg)*lit"
       , "b0000000:ra:rb:lit"},

   // Op code = 177 (0xb1)
   {"reg=EXTR(reg,reg)+lit"
       , "b1000000:rw:ra:rb:lit"},
   {"LA=EXTR(reg,reg)+lit"
       , "b1000000:ra:rb:lit"},

   // Op code = 178 (0xb2)
   {"reg=EXTR(reg,reg)&lit"
       , "b2000000:rw:ra:rb:lit"},
   {"LA=EXTR(reg,reg)&lit"
       , "b2000000:ra:rb:lit"},

   // Op code = 179 (0xb3)
   {"reg=EXTR(reg,reg)&LA"
       , "b30001fe:rw:ra:rb"},
   {"reg=EXTR(reg,reg)&LA,reg=MEMSL(areg)"
       , "b3000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTR(reg,reg)&LA"
       , "b30001fe:ra:rb"},
   {"LA=EXTR(reg,reg)&LA,reg=MEMSL(areg)"
       , "b3000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTR(reg,reg)&LA,lit,lit)"
       , "b3000000:rw:ra:rb:iepos:iewid"},

   // Op code = 180 (0xb4)
   {"reg=EXT(reg)*EXT2(reg)+LA"
       , "b40001fe:rw:ra:rb"},
   {"reg=EXT(reg)*EXT2(reg)+LA,reg=MEMSL(areg)"
       , "b4000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*EXT2(reg)+LA"
       , "b40001fe:ra:rb"},
   {"LA=EXT(reg)*EXT2(reg)+LA,reg=MEMSL(areg)"
       , "b4000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*EXT2(reg)+LA,lit,lit)"
       , "b4000000:rw:ra:rb:iepos:iewid"},

   // Op code = 181 (0xb5)
   {"reg=EXT(reg)*EXTI2(reg)+LA"
       , "b50001fe:rw:ra:rb"},
   {"reg=EXT(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)"
       , "b5000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXT(reg)*EXTI2(reg)+LA"
       , "b50001fe:ra:rb"},
   {"LA=EXT(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)"
       , "b5000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXT(reg)*EXTI2(reg)+LA,lit,lit)"
       , "b5000000:rw:ra:rb:iepos:iewid"},

   // Op code = 182 (0xb6)
   {"reg=EXTI(reg)*EXT2(reg)+LA"
       , "b60001fe:rw:ra:rb"},
   {"reg=EXTI(reg)*EXT2(reg)+LA,reg=MEMSL(areg)"
       , "b6000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*EXT2(reg)+LA"
       , "b60001fe:ra:rb"},
   {"LA=EXTI(reg)*EXT2(reg)+LA,reg=MEMSL(areg)"
       , "b6000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*EXT2(reg)+LA,lit,lit)"
       , "b6000000:rw:ra:rb:iepos:iewid"},

   // Op code = 183 (0xb7)
   {"reg=EXTI(reg)*EXTI2(reg)+LA"
       , "b70001fe:rw:ra:rb"},
   {"reg=EXTI(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)"
       , "b7000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"LA=EXTI(reg)*EXTI2(reg)+LA"
       , "b70001fe:ra:rb"},
   {"LA=EXTI(reg)*EXTI2(reg)+LA,reg=MEMSL(areg)"
       , "b7000001:ra:rb:pmem_rm:pmem_ar"},
   {"reg=EXTL(EXTI(reg)*EXTI2(reg)+LA,lit,lit)"
       , "b7000000:rw:ra:rb:iepos:iewid"},

   // Op code = 184 (0xb8)
   {"reg=SELECT(LA==lit,reg,reg)"
       , "b8000000:rw:0:ra:rb"},
   {"reg=SELECT(LA!=lit,reg,reg)"
       , "b8000040:rw:0:ra:rb"},
   {"reg=SELECT(LA>lit,reg,reg)"
       , "b8000080:rw:0:ra:rb"},
   {"reg=SELECT(LA>=lit,reg,reg)"
       , "b80000c0:rw:0:ra:rb"},
   {"reg=SELECT(LA<lit,reg,reg)"
       , "b8000100:rw:0:ra:rb"},
   {"reg=SELECT(LA<=lit,reg,reg)"
       , "b8000140:rw:0:ra:rb"},
   {"reg=SELECT(LA==lit,LA,reg)"
       , "b8000000:rw:0:rb"},
   {"reg=SELECT(LA!=lit,LA,reg)"
       , "b8000040:rw:0:rb"},
   {"reg=SELECT(LA>lit,LA,reg)"
       , "b8000080:rw:0:rb"},
   {"reg=SELECT(LA>=lit,LA,reg)"
       , "b80000c0:rw:0:rb"},
   {"reg=SELECT(LA<lit,LA,reg)"
       , "b8000100:rw:0:rb"},
   {"reg=SELECT(LA<=lit,LA,reg)"
       , "b8000140:rw:0:rb"},
   {"LA=SELECT(LA==lit,reg,reg)"
       , "b8000000:0:ra:rb"},
   {"LA=SELECT(LA!=lit,reg,reg)"
       , "b8000040:0:ra:rb"},
   {"LA=SELECT(LA>lit,reg,reg)"
       , "b8000080:0:ra:rb"},
   {"LA=SELECT(LA>=lit,reg,reg)"
       , "b80000c0:0:ra:rb"},
   {"LA=SELECT(LA<lit,reg,reg)"
       , "b8000100:0:ra:rb"},
   {"LA=SELECT(LA<=lit,reg,reg)"
       , "b8000140:0:ra:rb"},
   {"LA=SELECT(LA==lit,LA,reg)"
       , "b8000000:0:rb"},
   {"LA=SELECT(LA!=lit,LA,reg)"
       , "b8000040:0:rb"},
   {"LA=SELECT(LA>lit,LA,reg)"
       , "b8000080:0:rb"},
   {"LA=SELECT(LA>=lit,LA,reg)"
       , "b80000c0:0:rb"},
   {"LA=SELECT(LA<lit,LA,reg)"
       , "b8000100:0:rb"},
   {"LA=SELECT(LA<=lit,LA,reg)"
       , "b8000140:0:rb"},

   // Op code = 185 (0xb9)
   {"reg=SELECT(LA==lit,reg,lit)"
       , "b9000000:rw:0:ra:sel_lit"},
   {"reg=SELECT(LA!=lit,reg,lit)"
       , "b9000040:rw:0:ra:sel_lit"},
   {"reg=SELECT(LA>lit,reg,lit)"
       , "b9000080:rw:0:ra:sel_lit"},
   {"reg=SELECT(LA>=lit,reg,lit)"
       , "b90000c0:rw:0:ra:sel_lit"},
   {"reg=SELECT(LA<lit,reg,lit)"
       , "b9000100:rw:0:ra:sel_lit"},
   {"reg=SELECT(LA<=lit,reg,lit)"
       , "b9000140:rw:0:ra:sel_lit"},
   {"reg=SELECT(LA==lit,LA,lit)"
       , "b9000000:rw:0:sel_lit"},
   {"reg=SELECT(LA!=lit,LA,lit)"
       , "b9000040:rw:0:sel_lit"},
   {"reg=SELECT(LA>lit,LA,lit)"
       , "b9000080:rw:0:sel_lit"},
   {"reg=SELECT(LA>=lit,LA,lit)"
       , "b90000c0:rw:0:sel_lit"},
   {"reg=SELECT(LA<lit,LA,lit)"
       , "b9000100:rw:0:sel_lit"},
   {"reg=SELECT(LA<=lit,LA,lit)"
       , "b9000140:rw:0:sel_lit"},
   {"LA=SELECT(LA==lit,reg,lit)"
       , "b9000000:0:ra:sel_lit"},
   {"LA=SELECT(LA!=lit,reg,lit)"
       , "b9000040:0:ra:sel_lit"},
   {"LA=SELECT(LA>lit,reg,lit)"
       , "b9000080:0:ra:sel_lit"},
   {"LA=SELECT(LA>=lit,reg,lit)"
       , "b90000c0:0:ra:sel_lit"},
   {"LA=SELECT(LA<lit,reg,lit)"
       , "b9000100:0:ra:sel_lit"},
   {"LA=SELECT(LA<=lit,reg,lit)"
       , "b9000140:0:ra:sel_lit"},
   {"LA=SELECT(LA==lit,LA,lit)"
       , "b9000000:0:sel_lit"},
   {"LA=SELECT(LA!=lit,LA,lit)"
       , "b9000040:0:sel_lit"},
   {"LA=SELECT(LA>lit,LA,lit)"
       , "b9000080:0:sel_lit"},
   {"LA=SELECT(LA>=lit,LA,lit)"
       , "b90000c0:0:sel_lit"},
   {"LA=SELECT(LA<lit,LA,lit)"
       , "b9000100:0:sel_lit"},
   {"LA=SELECT(LA<=lit,LA,lit)"
       , "b9000140:0:sel_lit"},

   // Op code = 186 (0xba)
   {"reg=SELECT(reg==lit,reg,reg)"
       , "ba000000:rw:rb:0:ra:rb"},
   {"reg=SELECT(reg!=lit,reg,reg)"
       , "ba000040:rw:rb:0:ra:rb"},
   {"reg=SELECT(reg>lit,reg,reg)"
       , "ba000080:rw:rb:0:ra:rb"},
   {"reg=SELECT(reg>=lit,reg,reg)"
       , "ba0000c0:rw:rb:0:ra:rb"},
   {"reg=SELECT(reg<lit,reg,reg)"
       , "ba000100:rw:rb:0:ra:rb"},
   {"reg=SELECT(reg<=lit,reg,reg)"
       , "ba000140:rw:rb:0:ra:rb"},
   {"reg=SELECT(reg==lit,LA,reg)"
       , "ba000000:rw:rb:0:rb"},
   {"reg=SELECT(reg!=lit,LA,reg)"
       , "ba000040:rw:rb:0:rb"},
   {"reg=SELECT(reg>lit,LA,reg)"
       , "ba000080:rw:rb:0:rb"},
   {"reg=SELECT(reg>=lit,LA,reg)"
       , "ba0000c0:rw:rb:0:rb"},
   {"reg=SELECT(reg<lit,LA,reg)"
       , "ba000100:rw:rb:0:rb"},
   {"reg=SELECT(reg<=lit,LA,reg)"
       , "ba000140:rw:rb:0:rb"},
   {"LA=SELECT(reg==lit,reg,reg)"
       , "ba000000:rb:0:ra:rb"},
   {"LA=SELECT(reg!=lit,reg,reg)"
       , "ba000040:rb:0:ra:rb"},
   {"LA=SELECT(reg>lit,reg,reg)"
       , "ba000080:rb:0:ra:rb"},
   {"LA=SELECT(reg>=lit,reg,reg)"
       , "ba0000c0:rb:0:ra:rb"},
   {"LA=SELECT(reg<lit,reg,reg)"
       , "ba000100:rb:0:ra:rb"},
   {"LA=SELECT(reg<=lit,reg,reg)"
       , "ba000140:rb:0:ra:rb"},
   {"LA=SELECT(reg==lit,LA,reg)"
       , "ba000000:rb:0:rb"},
   {"LA=SELECT(reg!=lit,LA,reg)"
       , "ba000040:rb:0:rb"},
   {"LA=SELECT(reg>lit,LA,reg)"
       , "ba000080:rb:0:rb"},
   {"LA=SELECT(reg>=lit,LA,reg)"
       , "ba0000c0:rb:0:rb"},
   {"LA=SELECT(reg<lit,LA,reg)"
       , "ba000100:rb:0:rb"},
   {"LA=SELECT(reg<=lit,LA,reg)"
       , "ba000140:rb:0:rb"},

   // Op code = 187 (0xbb)
   {"reg=SELECT(reg==lit,reg,lit)"
       , "bb000000:rw:rb:0:ra:sel_lit"},
   {"reg=SELECT(reg!=lit,reg,lit)"
       , "bb000040:rw:rb:0:ra:sel_lit"},
   {"reg=SELECT(reg>lit,reg,lit)"
       , "bb000080:rw:rb:0:ra:sel_lit"},
   {"reg=SELECT(reg>=lit,reg,lit)"
       , "bb0000c0:rw:rb:0:ra:sel_lit"},
   {"reg=SELECT(reg<lit,reg,lit)"
       , "bb000100:rw:rb:0:ra:sel_lit"},
   {"reg=SELECT(reg<=lit,reg,lit)"
       , "bb000140:rw:rb:0:ra:sel_lit"},
   {"reg=SELECT(reg==lit,LA,lit)"
       , "bb000000:rw:rb:0:sel_lit"},
   {"reg=SELECT(reg!=lit,LA,lit)"
       , "bb000040:rw:rb:0:sel_lit"},
   {"reg=SELECT(reg>lit,LA,lit)"
       , "bb000080:rw:rb:0:sel_lit"},
   {"reg=SELECT(reg>=lit,LA,lit)"
       , "bb0000c0:rw:rb:0:sel_lit"},
   {"reg=SELECT(reg<lit,LA,lit)"
       , "bb000100:rw:rb:0:sel_lit"},
   {"reg=SELECT(reg<=lit,LA,lit)"
       , "bb000140:rw:rb:0:sel_lit"},
   {"LA=SELECT(reg==lit,reg,lit)"
       , "bb000000:rb:0:ra:sel_lit"},
   {"LA=SELECT(reg!=lit,reg,lit)"
       , "bb000040:rb:0:ra:sel_lit"},
   {"LA=SELECT(reg>lit,reg,lit)"
       , "bb000080:rb:0:ra:sel_lit"},
   {"LA=SELECT(reg>=lit,reg,lit)"
       , "bb0000c0:rb:0:ra:sel_lit"},
   {"LA=SELECT(reg<lit,reg,lit)"
       , "bb000100:rb:0:ra:sel_lit"},
   {"LA=SELECT(reg<=lit,reg,lit)"
       , "bb000140:rb:0:ra:sel_lit"},
   {"LA=SELECT(reg==lit,LA,lit)"
       , "bb000000:rb:0:sel_lit"},
   {"LA=SELECT(reg!=lit,LA,lit)"
       , "bb000040:rb:0:sel_lit"},
   {"LA=SELECT(reg>lit,LA,lit)"
       , "bb000080:rb:0:sel_lit"},
   {"LA=SELECT(reg>=lit,LA,lit)"
       , "bb0000c0:rb:0:sel_lit"},
   {"LA=SELECT(reg<lit,LA,lit)"
       , "bb000100:rb:0:sel_lit"},
   {"LA=SELECT(reg<=lit,LA,lit)"
       , "bb000140:rb:0:sel_lit"},

   // Op code = 188 (0xbc)
   {"reg=ADDMB(reg,lit)"
       , "bc000000:rw:ra:lit"},
   {"LA=ADDMB(reg,lit)"
       , "bc000000:ra:lit"},

   // Op code = 189 (0xbd)
   {"reg=ADDMB(reg,lit)+reg"
       , "bd000000:rw:ra:lit:rb"},
   {"reg=ADDMB(reg,lit)+LA"
       , "bd000000:rw:ra:lit"},
   {"LA=ADDMB(reg,lit)+reg"
       , "bd000000:ra:lit:rb"},
   {"LA=ADDMB(reg,lit)+LA"
       , "bd000000:ra:lit"},

   // Op code = 190 (0xbe)
   {"reg=ADDMB(reg,lit)-reg"
       , "be000000:rw:ra:lit:rb"},
   {"reg=ADDMB(reg,lit)-LA"
       , "be000000:rw:ra:lit"},
   {"LA=ADDMB(reg,lit)-reg"
       , "be000000:ra:lit:rb"},
   {"LA=ADDMB(reg,lit)-LA"
       , "be000000:ra:lit"},

   // Op code = 191 (0xbf)
   {"reg=ADDMB(reg,reg)+lit"
       , "bf000000:rw:ra:rb:lit"},
   {"LA=ADDMB(reg,reg)+lit"
       , "bf000000:ra:rb:lit"},

   // Op code = 192 (0xc0)
   {"reg=INS(QCOMPB(reg,reg),LI)"
       , "c0000000:rw:ra:rb"},
   {"reg=INS(QCOMPB(reg,reg),LI),reg=MEMSL(areg)"
       , "c0000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=QCOMPB(reg,reg),LI)"
       , "c0000000:ra:rb"},
   {"INS(LA=QCOMPB(reg,reg),LI),reg=MEMSL(areg)"
       , "c0000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 193 (0xc1)
   {"reg=INS(QCOMP(reg,reg),LI)"
       , "c1000000:rw:ra:rb"},
   {"reg=INS(QCOMP(reg,reg),LI),reg=MEMSL(areg)"
       , "c1000001:rw:ra:rb:pmem_rm:pmem_ar"},
   {"INS(LA=QCOMP(reg,reg),LI)"
       , "c1000000:ra:rb"},
   {"INS(LA=QCOMP(reg,reg),LI),reg=MEMSL(areg)"
       , "c1000001:ra:rb:pmem_rm:pmem_ar"},

   // Op code = 194 (0xc2)
   {"reg=CNTLD(reg,lit)"
       , "c2000000:rw:rb:lit"},
   {"reg=CNTLD(LA,lit)"
       , "c2000000:rw:lit"},
   {"LA=CNTLD(reg,lit)"
       , "c2000000:rb:lit"},
   {"LA=CNTLD(LA,lit)"
       , "c2000000:lit"},

   // Op code = 195 (0xc3)
   {"LA=LOAD_LA(reg,reg)"
       , "c3000000:rb:ra"},

   // Op code = 196 (0xc4)
   {"reg.a=reg.a*lit+reg.a"
       , "c4000000:rw:ra:sdp_lit:rb"},
   {"reg.a=reg.a*lit+LA.a"
       , "c4000000:rw:ra:sdp_lit"},
   {"reg.b=reg.b*lit+reg.b"
       , "c4000080:rw:ra:sdp_lit:rb"},
   {"reg.b=reg.b*lit+LA.b"
       , "c4000080:rw:ra:sdp_lit"},
   {"reg.c=reg.c*lit+reg.c"
       , "c4000100:rw:ra:sdp_lit:rb"},
   {"reg.c=reg.c*lit+LA.c"
       , "c4000100:rw:ra:sdp_lit"},
   {"reg.d=reg.d*lit+reg.d"
       , "c4000180:rw:ra:sdp_lit:rb"},
   {"reg.d=reg.d*lit+LA.d"
       , "c4000180:rw:ra:sdp_lit"},
   {"LA.a=reg.a*lit+reg.a"
       , "c4000000:ra:sdp_lit:rb"},
   {"LA.a=reg.a*lit+LA.a"
       , "c4000000:ra:sdp_lit"},
   {"LA.b=reg.b*lit+reg.b"
       , "c4000080:ra:sdp_lit:rb"},
   {"LA.b=reg.b*lit+LA.b"
       , "c4000080:ra:sdp_lit"},
   {"LA.c=reg.c*lit+reg.c"
       , "c4000100:ra:sdp_lit:rb"},
   {"LA.c=reg.c*lit+LA.c"
       , "c4000100:ra:sdp_lit"},
   {"LA.d=reg.d*lit+reg.d"
       , "c4000180:ra:sdp_lit:rb"},
   {"LA.d=reg.d*lit+LA.d"
       , "c4000180:ra:sdp_lit"},

   // Op code = 197 (0xc5)
   {"reg.a=reg.a*reg.a+lit"
       , "c5000000:rw:ra:rb:sdp_lit"},
   {"reg.a=reg.a+lit"
       , "c5000000:rw:ra:sdp_lit"},
   {"reg.b=reg.b*reg.b+lit"
       , "c5000080:rw:ra:rb:sdp_lit"},
   {"reg.b=reg.b+lit"
       , "c5000080:rw:ra:sdp_lit"},
   {"reg.c=reg.c*reg.c+lit"
       , "c5000100:rw:ra:rb:sdp_lit"},
   {"reg.c=reg.c+lit"
       , "c5000100:rw:ra:sdp_lit"},
   {"reg.d=reg.d*reg.d+lit"
       , "c5000180:rw:ra:rb:sdp_lit"},
   {"reg.d=reg.d+lit"
       , "c5000180:rw:ra:sdp_lit"},
   {"LA.a=reg.a*reg.a+lit"
       , "c5000000:ra:rb:sdp_lit"},
   {"LA.a=reg.a+lit"
       , "c5000000:ra:sdp_lit"},
   {"LA.b=reg.b*reg.b+lit"
       , "c5000080:ra:rb:sdp_lit"},
   {"LA.b=reg.b+lit"
       , "c5000080:ra:sdp_lit"},
   {"LA.c=reg.c*reg.c+lit"
       , "c5000100:ra:rb:sdp_lit"},
   {"LA.c=reg.c+lit"
       , "c5000100:ra:sdp_lit"},
   {"LA.d=reg.d*reg.d+lit"
       , "c5000180:ra:rb:sdp_lit"},
   {"LA.d=reg.d+lit"
       , "c5000180:ra:sdp_lit"},

   // Op code = 198 (0xc6)
   {"reg.a=reg.a"
       , "c6000000:rw:ra"},
   {"reg.b=reg.a"
       , "c6000200:rw:ra"},
   {"reg.c=reg.a"
       , "c6000400:rw:ra"},
   {"reg.d=reg.a"
       , "c6000600:rw:ra"},
   {"reg.a=reg.b"
       , "c6000800:rw:ra"},
   {"reg.b=reg.b"
       , "c6000a00:rw:ra"},
   {"reg.c=reg.b"
       , "c6000c00:rw:ra"},
   {"reg.d=reg.b"
       , "c6000e00:rw:ra"},
   {"reg.a=reg.c"
       , "c6001000:rw:ra"},
   {"reg.b=reg.c"
       , "c6001200:rw:ra"},
   {"reg.c=reg.c"
       , "c6001400:rw:ra"},
   {"reg.d=reg.c"
       , "c6001600:rw:ra"},
   {"reg.a=reg.d"
       , "c6001800:rw:ra"},
   {"reg.b=reg.d"
       , "c6001a00:rw:ra"},
   {"reg.c=reg.d"
       , "c6001c00:rw:ra"},
   {"reg.d=reg.d"
       , "c6001e00:rw:ra"},
   {"reg=reg.a"
       , "c6002000:rw:ra"},
   {"reg=reg.b"
       , "c6002800:rw:ra"},
   {"reg=reg.c"
       , "c6003000:rw:ra"},
   {"reg=reg.d"
       , "c6003800:rw:ra"},
   {"LA.a=reg.a"
       , "c6000000:ra"},
   {"LA.b=reg.a"
       , "c6000200:ra"},
   {"LA.c=reg.a"
       , "c6000400:ra"},
   {"LA.d=reg.a"
       , "c6000600:ra"},
   {"LA.a=reg.b"
       , "c6000800:ra"},
   {"LA.b=reg.b"
       , "c6000a00:ra"},
   {"LA.c=reg.b"
       , "c6000c00:ra"},
   {"LA.d=reg.b"
       , "c6000e00:ra"},
   {"LA.a=reg.c"
       , "c6001000:ra"},
   {"LA.b=reg.c"
       , "c6001200:ra"},
   {"LA.c=reg.c"
       , "c6001400:ra"},
   {"LA.d=reg.c"
       , "c6001600:ra"},
   {"LA.a=reg.d"
       , "c6001800:ra"},
   {"LA.b=reg.d"
       , "c6001a00:ra"},
   {"LA.c=reg.d"
       , "c6001c00:ra"},
   {"LA.d=reg.d"
       , "c6001e00:ra"},
   {"LA=reg.a"
       , "c6002000:ra"},
   {"LA=reg.b"
       , "c6002800:ra"},
   {"LA=reg.c"
       , "c6003000:ra"},
   {"LA=reg.d"
       , "c6003800:ra"},

   // Op code = 199 (0xc7)
   {"reg=lit"
       , "c7010000:rw:ldlit_lit"},
   {"reg.a=lit"
       , "c7000000:rw:ldlit_lit"},
   {"reg.b=lit"
       , "c7020000:rw:ldlit_lit"},
   {"reg.c=lit"
       , "c7040000:rw:ldlit_lit"},
   {"reg.d=lit"
       , "c7060000:rw:ldlit_lit"},
   {"LA=lit"
       , "c7010000:ldlit_lit"},
   {"LA.a=lit"
       , "c7000000:ldlit_lit"},
   {"LA.b=lit"
       , "c7020000:ldlit_lit"},
   {"LA.c=lit"
       , "c7040000:ldlit_lit"},
   {"LA.d=lit"
       , "c7060000:ldlit_lit"},

   // Op code = 200 (0xc8)
   {"reg=LIT_HIGH+lit"
       , "c8010000:rw:ldlitw_lit"},
   {"reg.a=LIT_HIGH.a+lit"
       , "c8000000:rw:ldlitw_lit"},
   {"reg.b=LIT_HIGH.b+lit"
       , "c8020000:rw:ldlitw_lit"},
   {"reg.c=LIT_HIGH.c+lit"
       , "c8040000:rw:ldlitw_lit"},
   {"reg.d=LIT_HIGH.d+lit"
       , "c8060000:rw:ldlitw_lit"},
   {"LA=LIT_HIGH+lit"
       , "c8010000:ldlitw_lit"},
   {"LA.a=LIT_HIGH.a+lit"
       , "c8000000:ldlitw_lit"},
   {"LA.b=LIT_HIGH.b+lit"
       , "c8020000:ldlitw_lit"},
   {"LA.c=LIT_HIGH.c+lit"
       , "c8040000:ldlitw_lit"},
   {"LA.d=LIT_HIGH.d+lit"
       , "c8060000:ldlitw_lit"},

   // Op code = 201 (0xc9)
   {"SPECREG(INS_EPOS)=reg"
       , "c9220000:rb"},
   {"SPECREG(LI)=reg"
       , "c9240000:rb"},
   {"SPECREG(INS_ALL)=reg"
       , "c923c000:rb"},
   {"SPECREG(EXT_POS)=reg"
       , "c920c000:rb"},
   {"SPECREG(INS_WIDTH)=reg"
       , "c9224000:rb"},
   {"SPECREG(EXT_INIT)=reg"
       , "c9200000:rb"},
   {"SPECREG(EXT_ALL)=reg"
       , "c9214000:rb"},
   {"SPECREG(INS_ADD)=reg"
       , "c9230000:rb"},
   {"SPECREG(EXT_SIGNED)=reg"
       , "c9218000:rb"},
   {"SPECREG(INS_IPOS)=reg"
       , "c9228000:rb"},
   {"SPECREG(EXT_NXT)=reg"
       , "c9210000:rb"},
   {"SPECREG(LIT_HIGH)=reg"
       , "c9248000:rb"},
   {"SPECREG(EXT2_ALL)=reg"
       , "c921c000:rb"},
   {"SPECREG(INS_MODE)=reg"
       , "c9234000:rb"},
   {"SPECREG(INS_INC)=reg"
       , "c922c000:rb"},
   {"SPECREG(EXT_WIDTH)=reg"
       , "c9208000:rb"},
   {"SPECREG(EXT_INC)=reg"
       , "c9204000:rb"},
   {"SPECREG(INS_INIT)=reg"
       , "c9238000:rb"},
   {"SPECREG(INS_EPOS.a)=reg.a"
       , "c9020000:rb"},
   {"SPECREG(LI.a)=reg.a"
       , "c9040000:rb"},
   {"SPECREG(INS_ALL.a)=reg.a"
       , "c903c000:rb"},
   {"SPECREG(EXT_POS.a)=reg.a"
       , "c900c000:rb"},
   {"SPECREG(INS_WIDTH.a)=reg.a"
       , "c9024000:rb"},
   {"SPECREG(EXT_INIT.a)=reg.a"
       , "c9000000:rb"},
   {"SPECREG(EXT_ALL.a)=reg.a"
       , "c9014000:rb"},
   {"SPECREG(INS_ADD.a)=reg.a"
       , "c9030000:rb"},
   {"SPECREG(EXT_SIGNED.a)=reg.a"
       , "c9018000:rb"},
   {"SPECREG(INS_IPOS.a)=reg.a"
       , "c9028000:rb"},
   {"SPECREG(EXT_NXT.a)=reg.a"
       , "c9010000:rb"},
   {"SPECREG(LIT_HIGH.a)=reg.a"
       , "c9048000:rb"},
   {"SPECREG(EXT2_ALL.a)=reg.a"
       , "c901c000:rb"},
   {"SPECREG(INS_MODE.a)=reg.a"
       , "c9034000:rb"},
   {"SPECREG(INS_INC.a)=reg.a"
       , "c902c000:rb"},
   {"SPECREG(EXT_WIDTH.a)=reg.a"
       , "c9008000:rb"},
   {"SPECREG(EXT_INC.a)=reg.a"
       , "c9004000:rb"},
   {"SPECREG(INS_INIT.a)=reg.a"
       , "c9038000:rb"},
   {"SPECREG(INS_EPOS.b)=reg.b"
       , "c9420000:rb"},
   {"SPECREG(LI.b)=reg.b"
       , "c9440000:rb"},
   {"SPECREG(INS_ALL.b)=reg.b"
       , "c943c000:rb"},
   {"SPECREG(EXT_POS.b)=reg.b"
       , "c940c000:rb"},
   {"SPECREG(INS_WIDTH.b)=reg.b"
       , "c9424000:rb"},
   {"SPECREG(EXT_INIT.b)=reg.b"
       , "c9400000:rb"},
   {"SPECREG(EXT_ALL.b)=reg.b"
       , "c9414000:rb"},
   {"SPECREG(INS_ADD.b)=reg.b"
       , "c9430000:rb"},
   {"SPECREG(EXT_SIGNED.b)=reg.b"
       , "c9418000:rb"},
   {"SPECREG(INS_IPOS.b)=reg.b"
       , "c9428000:rb"},
   {"SPECREG(EXT_NXT.b)=reg.b"
       , "c9410000:rb"},
   {"SPECREG(LIT_HIGH.b)=reg.b"
       , "c9448000:rb"},
   {"SPECREG(EXT2_ALL.b)=reg.b"
       , "c941c000:rb"},
   {"SPECREG(INS_MODE.b)=reg.b"
       , "c9434000:rb"},
   {"SPECREG(INS_INC.b)=reg.b"
       , "c942c000:rb"},
   {"SPECREG(EXT_WIDTH.b)=reg.b"
       , "c9408000:rb"},
   {"SPECREG(EXT_INC.b)=reg.b"
       , "c9404000:rb"},
   {"SPECREG(INS_INIT.b)=reg.b"
       , "c9438000:rb"},
   {"SPECREG(INS_EPOS.c)=reg.c"
       , "c9820000:rb"},
   {"SPECREG(LI.c)=reg.c"
       , "c9840000:rb"},
   {"SPECREG(INS_ALL.c)=reg.c"
       , "c983c000:rb"},
   {"SPECREG(EXT_POS.c)=reg.c"
       , "c980c000:rb"},
   {"SPECREG(INS_WIDTH.c)=reg.c"
       , "c9824000:rb"},
   {"SPECREG(EXT_INIT.c)=reg.c"
       , "c9800000:rb"},
   {"SPECREG(EXT_ALL.c)=reg.c"
       , "c9814000:rb"},
   {"SPECREG(INS_ADD.c)=reg.c"
       , "c9830000:rb"},
   {"SPECREG(EXT_SIGNED.c)=reg.c"
       , "c9818000:rb"},
   {"SPECREG(INS_IPOS.c)=reg.c"
       , "c9828000:rb"},
   {"SPECREG(EXT_NXT.c)=reg.c"
       , "c9810000:rb"},
   {"SPECREG(LIT_HIGH.c)=reg.c"
       , "c9848000:rb"},
   {"SPECREG(EXT2_ALL.c)=reg.c"
       , "c981c000:rb"},
   {"SPECREG(INS_MODE.c)=reg.c"
       , "c9834000:rb"},
   {"SPECREG(INS_INC.c)=reg.c"
       , "c982c000:rb"},
   {"SPECREG(EXT_WIDTH.c)=reg.c"
       , "c9808000:rb"},
   {"SPECREG(EXT_INC.c)=reg.c"
       , "c9804000:rb"},
   {"SPECREG(INS_INIT.c)=reg.c"
       , "c9838000:rb"},
   {"SPECREG(INS_EPOS.d)=reg.d"
       , "c9c20000:rb"},
   {"SPECREG(LI.d)=reg.d"
       , "c9c40000:rb"},
   {"SPECREG(INS_ALL.d)=reg.d"
       , "c9c3c000:rb"},
   {"SPECREG(EXT_POS.d)=reg.d"
       , "c9c0c000:rb"},
   {"SPECREG(INS_WIDTH.d)=reg.d"
       , "c9c24000:rb"},
   {"SPECREG(EXT_INIT.d)=reg.d"
       , "c9c00000:rb"},
   {"SPECREG(EXT_ALL.d)=reg.d"
       , "c9c14000:rb"},
   {"SPECREG(INS_ADD.d)=reg.d"
       , "c9c30000:rb"},
   {"SPECREG(EXT_SIGNED.d)=reg.d"
       , "c9c18000:rb"},
   {"SPECREG(INS_IPOS.d)=reg.d"
       , "c9c28000:rb"},
   {"SPECREG(EXT_NXT.d)=reg.d"
       , "c9c10000:rb"},
   {"SPECREG(LIT_HIGH.d)=reg.d"
       , "c9c48000:rb"},
   {"SPECREG(EXT2_ALL.d)=reg.d"
       , "c9c1c000:rb"},
   {"SPECREG(INS_MODE.d)=reg.d"
       , "c9c34000:rb"},
   {"SPECREG(INS_INC.d)=reg.d"
       , "c9c2c000:rb"},
   {"SPECREG(EXT_WIDTH.d)=reg.d"
       , "c9c08000:rb"},
   {"SPECREG(EXT_INC.d)=reg.d"
       , "c9c04000:rb"},
   {"SPECREG(INS_INIT.d)=reg.d"
       , "c9c38000:rb"},

   // Op code = 202 (0xca)
   {"SPECREG(INS_EPOS)=lit"
       , "ca220000:sregld_lit"},
   {"SPECREG(LI)=lit"
       , "ca240000:sregld_lit"},
   {"SPECREG(INS_ALL)=lit"
       , "ca23c000:sregld_lit"},
   {"SPECREG(EXT_POS)=lit"
       , "ca20c000:sregld_lit"},
   {"SPECREG(INS_WIDTH)=lit"
       , "ca224000:sregld_lit"},
   {"SPECREG(EXT_INIT)=lit"
       , "ca200000:sregld_lit"},
   {"SPECREG(EXT_ALL)=lit"
       , "ca214000:sregld_lit"},
   {"SPECREG(INS_ADD)=lit"
       , "ca230000:sregld_lit"},
   {"SPECREG(EXT_SIGNED)=lit"
       , "ca218000:sregld_lit"},
   {"SPECREG(INS_IPOS)=lit"
       , "ca228000:sregld_lit"},
   {"SPECREG(EXT_NXT)=lit"
       , "ca210000:sregld_lit"},
   {"SPECREG(LIT_HIGH)=lit"
       , "ca248000:sregld_lit"},
   {"SPECREG(EXT2_ALL)=lit"
       , "ca21c000:sregld_lit"},
   {"SPECREG(INS_MODE)=lit"
       , "ca234000:sregld_lit"},
   {"SPECREG(INS_INC)=lit"
       , "ca22c000:sregld_lit"},
   {"SPECREG(EXT_WIDTH)=lit"
       , "ca208000:sregld_lit"},
   {"SPECREG(EXT_INC)=lit"
       , "ca204000:sregld_lit"},
   {"SPECREG(INS_INIT)=lit"
       , "ca238000:sregld_lit"},
   {"SPECREG(INS_EPOS.a)=lit"
       , "ca020000:sregld_lit"},
   {"SPECREG(LI.a)=lit"
       , "ca040000:sregld_lit"},
   {"SPECREG(INS_ALL.a)=lit"
       , "ca03c000:sregld_lit"},
   {"SPECREG(EXT_POS.a)=lit"
       , "ca00c000:sregld_lit"},
   {"SPECREG(INS_WIDTH.a)=lit"
       , "ca024000:sregld_lit"},
   {"SPECREG(EXT_INIT.a)=lit"
       , "ca000000:sregld_lit"},
   {"SPECREG(EXT_ALL.a)=lit"
       , "ca014000:sregld_lit"},
   {"SPECREG(INS_ADD.a)=lit"
       , "ca030000:sregld_lit"},
   {"SPECREG(EXT_SIGNED.a)=lit"
       , "ca018000:sregld_lit"},
   {"SPECREG(INS_IPOS.a)=lit"
       , "ca028000:sregld_lit"},
   {"SPECREG(EXT_NXT.a)=lit"
       , "ca010000:sregld_lit"},
   {"SPECREG(LIT_HIGH.a)=lit"
       , "ca048000:sregld_lit"},
   {"SPECREG(EXT2_ALL.a)=lit"
       , "ca01c000:sregld_lit"},
   {"SPECREG(INS_MODE.a)=lit"
       , "ca034000:sregld_lit"},
   {"SPECREG(INS_INC.a)=lit"
       , "ca02c000:sregld_lit"},
   {"SPECREG(EXT_WIDTH.a)=lit"
       , "ca008000:sregld_lit"},
   {"SPECREG(EXT_INC.a)=lit"
       , "ca004000:sregld_lit"},
   {"SPECREG(INS_INIT.a)=lit"
       , "ca038000:sregld_lit"},
   {"SPECREG(INS_EPOS.b)=lit"
       , "ca420000:sregld_lit"},
   {"SPECREG(LI.b)=lit"
       , "ca440000:sregld_lit"},
   {"SPECREG(INS_ALL.b)=lit"
       , "ca43c000:sregld_lit"},
   {"SPECREG(EXT_POS.b)=lit"
       , "ca40c000:sregld_lit"},
   {"SPECREG(INS_WIDTH.b)=lit"
       , "ca424000:sregld_lit"},
   {"SPECREG(EXT_INIT.b)=lit"
       , "ca400000:sregld_lit"},
   {"SPECREG(EXT_ALL.b)=lit"
       , "ca414000:sregld_lit"},
   {"SPECREG(INS_ADD.b)=lit"
       , "ca430000:sregld_lit"},
   {"SPECREG(EXT_SIGNED.b)=lit"
       , "ca418000:sregld_lit"},
   {"SPECREG(INS_IPOS.b)=lit"
       , "ca428000:sregld_lit"},
   {"SPECREG(EXT_NXT.b)=lit"
       , "ca410000:sregld_lit"},
   {"SPECREG(LIT_HIGH.b)=lit"
       , "ca448000:sregld_lit"},
   {"SPECREG(EXT2_ALL.b)=lit"
       , "ca41c000:sregld_lit"},
   {"SPECREG(INS_MODE.b)=lit"
       , "ca434000:sregld_lit"},
   {"SPECREG(INS_INC.b)=lit"
       , "ca42c000:sregld_lit"},
   {"SPECREG(EXT_WIDTH.b)=lit"
       , "ca408000:sregld_lit"},
   {"SPECREG(EXT_INC.b)=lit"
       , "ca404000:sregld_lit"},
   {"SPECREG(INS_INIT.b)=lit"
       , "ca438000:sregld_lit"},
   {"SPECREG(INS_EPOS.c)=lit"
       , "ca820000:sregld_lit"},
   {"SPECREG(LI.c)=lit"
       , "ca840000:sregld_lit"},
   {"SPECREG(INS_ALL.c)=lit"
       , "ca83c000:sregld_lit"},
   {"SPECREG(EXT_POS.c)=lit"
       , "ca80c000:sregld_lit"},
   {"SPECREG(INS_WIDTH.c)=lit"
       , "ca824000:sregld_lit"},
   {"SPECREG(EXT_INIT.c)=lit"
       , "ca800000:sregld_lit"},
   {"SPECREG(EXT_ALL.c)=lit"
       , "ca814000:sregld_lit"},
   {"SPECREG(INS_ADD.c)=lit"
       , "ca830000:sregld_lit"},
   {"SPECREG(EXT_SIGNED.c)=lit"
       , "ca818000:sregld_lit"},
   {"SPECREG(INS_IPOS.c)=lit"
       , "ca828000:sregld_lit"},
   {"SPECREG(EXT_NXT.c)=lit"
       , "ca810000:sregld_lit"},
   {"SPECREG(LIT_HIGH.c)=lit"
       , "ca848000:sregld_lit"},
   {"SPECREG(EXT2_ALL.c)=lit"
       , "ca81c000:sregld_lit"},
   {"SPECREG(INS_MODE.c)=lit"
       , "ca834000:sregld_lit"},
   {"SPECREG(INS_INC.c)=lit"
       , "ca82c000:sregld_lit"},
   {"SPECREG(EXT_WIDTH.c)=lit"
       , "ca808000:sregld_lit"},
   {"SPECREG(EXT_INC.c)=lit"
       , "ca804000:sregld_lit"},
   {"SPECREG(INS_INIT.c)=lit"
       , "ca838000:sregld_lit"},
   {"SPECREG(INS_EPOS.d)=lit"
       , "cac20000:sregld_lit"},
   {"SPECREG(LI.d)=lit"
       , "cac40000:sregld_lit"},
   {"SPECREG(INS_ALL.d)=lit"
       , "cac3c000:sregld_lit"},
   {"SPECREG(EXT_POS.d)=lit"
       , "cac0c000:sregld_lit"},
   {"SPECREG(INS_WIDTH.d)=lit"
       , "cac24000:sregld_lit"},
   {"SPECREG(EXT_INIT.d)=lit"
       , "cac00000:sregld_lit"},
   {"SPECREG(EXT_ALL.d)=lit"
       , "cac14000:sregld_lit"},
   {"SPECREG(INS_ADD.d)=lit"
       , "cac30000:sregld_lit"},
   {"SPECREG(EXT_SIGNED.d)=lit"
       , "cac18000:sregld_lit"},
   {"SPECREG(INS_IPOS.d)=lit"
       , "cac28000:sregld_lit"},
   {"SPECREG(EXT_NXT.d)=lit"
       , "cac10000:sregld_lit"},
   {"SPECREG(LIT_HIGH.d)=lit"
       , "cac48000:sregld_lit"},
   {"SPECREG(EXT2_ALL.d)=lit"
       , "cac1c000:sregld_lit"},
   {"SPECREG(INS_MODE.d)=lit"
       , "cac34000:sregld_lit"},
   {"SPECREG(INS_INC.d)=lit"
       , "cac2c000:sregld_lit"},
   {"SPECREG(EXT_WIDTH.d)=lit"
       , "cac08000:sregld_lit"},
   {"SPECREG(EXT_INC.d)=lit"
       , "cac04000:sregld_lit"},
   {"SPECREG(INS_INIT.d)=lit"
       , "cac38000:sregld_lit"},

   // Op code = 203 (0xcb)
   {"reg=SPECREG(INS_EPOS)"
       , "cb020800:rw"},
   {"reg=SPECREG(LI)"
       , "cb040800:rw"},
   {"reg=SPECREG(INS_ALL)"
       , "cb03c800:rw"},
   {"reg=SPECREG(EXT_POS)"
       , "cb00c800:rw"},
   {"reg=SPECREG(INS_WIDTH)"
       , "cb024800:rw"},
   {"reg=SPECREG(EXT_ALL)"
       , "cb014800:rw"},
   {"reg=SPECREG(EXT_SIGNED)"
       , "cb018800:rw"},
   {"reg=SPECREG(INS_IPOS)"
       , "cb028800:rw"},
   {"reg=SPECREG(EXT_NXT)"
       , "cb010800:rw"},
   {"reg=SPECREG(DP_NUM)"
       , "cb044800:rw"},
   {"reg=SPECREG(LIT_HIGH)"
       , "cb048800:rw"},
   {"reg=SPECREG(EXT2_ALL)"
       , "cb01c800:rw"},
   {"reg=SPECREG(INS_MODE)"
       , "cb034800:rw"},
   {"reg=SPECREG(INS_INC)"
       , "cb02c800:rw"},
   {"reg=SPECREG(EXT_WIDTH)"
       , "cb008800:rw"},
   {"reg=SPECREG(EXT_INC)"
       , "cb004800:rw"},
   {"reg.a=SPECREG(INS_EPOS.a)"
       , "cb020000:rw"},
   {"reg.a=SPECREG(LI.a)"
       , "cb040000:rw"},
   {"reg.a=SPECREG(INS_ALL.a)"
       , "cb03c000:rw"},
   {"reg.a=SPECREG(EXT_POS.a)"
       , "cb00c000:rw"},
   {"reg.a=SPECREG(INS_WIDTH.a)"
       , "cb024000:rw"},
   {"reg.a=SPECREG(EXT_ALL.a)"
       , "cb014000:rw"},
   {"reg.a=SPECREG(EXT_SIGNED.a)"
       , "cb018000:rw"},
   {"reg.a=SPECREG(INS_IPOS.a)"
       , "cb028000:rw"},
   {"reg.a=SPECREG(EXT_NXT.a)"
       , "cb010000:rw"},
   {"reg.a=SPECREG(DP_NUM.a)"
       , "cb044000:rw"},
   {"reg.a=SPECREG(LIT_HIGH.a)"
       , "cb048000:rw"},
   {"reg.a=SPECREG(EXT2_ALL.a)"
       , "cb01c000:rw"},
   {"reg.a=SPECREG(INS_MODE.a)"
       , "cb034000:rw"},
   {"reg.a=SPECREG(INS_INC.a)"
       , "cb02c000:rw"},
   {"reg.a=SPECREG(EXT_WIDTH.a)"
       , "cb008000:rw"},
   {"reg.a=SPECREG(EXT_INC.a)"
       , "cb004000:rw"},
   {"reg.b=SPECREG(INS_EPOS.b)"
       , "cb021000:rw"},
   {"reg.b=SPECREG(LI.b)"
       , "cb041000:rw"},
   {"reg.b=SPECREG(INS_ALL.b)"
       , "cb03d000:rw"},
   {"reg.b=SPECREG(EXT_POS.b)"
       , "cb00d000:rw"},
   {"reg.b=SPECREG(INS_WIDTH.b)"
       , "cb025000:rw"},
   {"reg.b=SPECREG(EXT_ALL.b)"
       , "cb015000:rw"},
   {"reg.b=SPECREG(EXT_SIGNED.b)"
       , "cb019000:rw"},
   {"reg.b=SPECREG(INS_IPOS.b)"
       , "cb029000:rw"},
   {"reg.b=SPECREG(EXT_NXT.b)"
       , "cb011000:rw"},
   {"reg.b=SPECREG(DP_NUM.b)"
       , "cb045000:rw"},
   {"reg.b=SPECREG(LIT_HIGH.b)"
       , "cb049000:rw"},
   {"reg.b=SPECREG(EXT2_ALL.b)"
       , "cb01d000:rw"},
   {"reg.b=SPECREG(INS_MODE.b)"
       , "cb035000:rw"},
   {"reg.b=SPECREG(INS_INC.b)"
       , "cb02d000:rw"},
   {"reg.b=SPECREG(EXT_WIDTH.b)"
       , "cb009000:rw"},
   {"reg.b=SPECREG(EXT_INC.b)"
       , "cb005000:rw"},
   {"reg.c=SPECREG(INS_EPOS.c)"
       , "cb022000:rw"},
   {"reg.c=SPECREG(LI.c)"
       , "cb042000:rw"},
   {"reg.c=SPECREG(INS_ALL.c)"
       , "cb03e000:rw"},
   {"reg.c=SPECREG(EXT_POS.c)"
       , "cb00e000:rw"},
   {"reg.c=SPECREG(INS_WIDTH.c)"
       , "cb026000:rw"},
   {"reg.c=SPECREG(EXT_ALL.c)"
       , "cb016000:rw"},
   {"reg.c=SPECREG(EXT_SIGNED.c)"
       , "cb01a000:rw"},
   {"reg.c=SPECREG(INS_IPOS.c)"
       , "cb02a000:rw"},
   {"reg.c=SPECREG(EXT_NXT.c)"
       , "cb012000:rw"},
   {"reg.c=SPECREG(DP_NUM.c)"
       , "cb046000:rw"},
   {"reg.c=SPECREG(LIT_HIGH.c)"
       , "cb04a000:rw"},
   {"reg.c=SPECREG(EXT2_ALL.c)"
       , "cb01e000:rw"},
   {"reg.c=SPECREG(INS_MODE.c)"
       , "cb036000:rw"},
   {"reg.c=SPECREG(INS_INC.c)"
       , "cb02e000:rw"},
   {"reg.c=SPECREG(EXT_WIDTH.c)"
       , "cb00a000:rw"},
   {"reg.c=SPECREG(EXT_INC.c)"
       , "cb006000:rw"},
   {"reg.d=SPECREG(INS_EPOS.d)"
       , "cb023000:rw"},
   {"reg.d=SPECREG(LI.d)"
       , "cb043000:rw"},
   {"reg.d=SPECREG(INS_ALL.d)"
       , "cb03f000:rw"},
   {"reg.d=SPECREG(EXT_POS.d)"
       , "cb00f000:rw"},
   {"reg.d=SPECREG(INS_WIDTH.d)"
       , "cb027000:rw"},
   {"reg.d=SPECREG(EXT_ALL.d)"
       , "cb017000:rw"},
   {"reg.d=SPECREG(EXT_SIGNED.d)"
       , "cb01b000:rw"},
   {"reg.d=SPECREG(INS_IPOS.d)"
       , "cb02b000:rw"},
   {"reg.d=SPECREG(EXT_NXT.d)"
       , "cb013000:rw"},
   {"reg.d=SPECREG(DP_NUM.d)"
       , "cb047000:rw"},
   {"reg.d=SPECREG(LIT_HIGH.d)"
       , "cb04b000:rw"},
   {"reg.d=SPECREG(EXT2_ALL.d)"
       , "cb01f000:rw"},
   {"reg.d=SPECREG(INS_MODE.d)"
       , "cb037000:rw"},
   {"reg.d=SPECREG(INS_INC.d)"
       , "cb02f000:rw"},
   {"reg.d=SPECREG(EXT_WIDTH.d)"
       , "cb00b000:rw"},
   {"reg.d=SPECREG(EXT_INC.d)"
       , "cb007000:rw"},

   // Op code = 204 (0xcc)
   {"GOTOlit"
       , "cc000000:uncond_offset"},

   // Op code = 205 (0xcd)
   {"CALL(lit)"
       , "cd000000:uncond_offset"},

   // Op code = 206 (0xce)
   {"RETURN"
       , "ce000000"},
   {"RETURNI"
       , "ce000001"},

   // Op code = 207 (0xcf)
   {"IF(reg.a==reg.a)GOTOlit"
       , "cf000000:ra:rb:br_offset"},
   {"IF(reg.a!=reg.a)GOTOlit"
       , "cf100000:ra:rb:br_offset"},
   {"IF(reg.a>reg.a)GOTOlit"
       , "cf200000:ra:rb:br_offset"},
   {"IF(reg.a>=reg.a)GOTOlit"
       , "cf300000:ra:rb:br_offset"},
   {"IF(reg.b==reg.b)GOTOlit"
       , "cf400000:ra:rb:br_offset"},
   {"IF(reg.b!=reg.b)GOTOlit"
       , "cf500000:ra:rb:br_offset"},
   {"IF(reg.b>reg.b)GOTOlit"
       , "cf600000:ra:rb:br_offset"},
   {"IF(reg.b>=reg.b)GOTOlit"
       , "cf700000:ra:rb:br_offset"},
   {"IF(reg.c==reg.c)GOTOlit"
       , "cf800000:ra:rb:br_offset"},
   {"IF(reg.c!=reg.c)GOTOlit"
       , "cf900000:ra:rb:br_offset"},
   {"IF(reg.c>reg.c)GOTOlit"
       , "cfa00000:ra:rb:br_offset"},
   {"IF(reg.c>=reg.c)GOTOlit"
       , "cfb00000:ra:rb:br_offset"},
   {"IF(reg.d==reg.d)GOTOlit"
       , "cfc00000:ra:rb:br_offset"},
   {"IF(reg.d!=reg.d)GOTOlit"
       , "cfd00000:ra:rb:br_offset"},
   {"IF(reg.d>reg.d)GOTOlit"
       , "cfe00000:ra:rb:br_offset"},
   {"IF(reg.d>=reg.d)GOTOlit"
       , "cff00000:ra:rb:br_offset"},
   {"IF(reg==reg)GOTOlit"
       , "cf080000:ra:rb:br_offset"},
   {"IF(reg!=reg)GOTOlit"
       , "cf180000:ra:rb:br_offset"},
   {"IF(reg>reg)GOTOlit"
       , "cf280000:ra:rb:br_offset"},
   {"IF(reg>=reg)GOTOlit"
       , "cf380000:ra:rb:br_offset"},

   // Op code = 208 (0xd0)
   {"IF(reg.a==lit)GOTOlit"
       , "d0000000:ra:br_lit:br_offset"},
   {"IF(reg.a!=lit)GOTOlit"
       , "d0100000:ra:br_lit:br_offset"},
   {"IF(reg.a>lit)GOTOlit"
       , "d0200000:ra:br_lit:br_offset"},
   {"IF(reg.a<lit)GOTOlit"
       , "d0300000:ra:br_lit:br_offset"},
   {"IF(reg.b==lit)GOTOlit"
       , "d0400000:ra:br_lit:br_offset"},
   {"IF(reg.b!=lit)GOTOlit"
       , "d0500000:ra:br_lit:br_offset"},
   {"IF(reg.b>lit)GOTOlit"
       , "d0600000:ra:br_lit:br_offset"},
   {"IF(reg.b<lit)GOTOlit"
       , "d0700000:ra:br_lit:br_offset"},
   {"IF(reg.c==lit)GOTOlit"
       , "d0800000:ra:br_lit:br_offset"},
   {"IF(reg.c!=lit)GOTOlit"
       , "d0900000:ra:br_lit:br_offset"},
   {"IF(reg.c>lit)GOTOlit"
       , "d0a00000:ra:br_lit:br_offset"},
   {"IF(reg.c<lit)GOTOlit"
       , "d0b00000:ra:br_lit:br_offset"},
   {"IF(reg.d==lit)GOTOlit"
       , "d0c00000:ra:br_lit:br_offset"},
   {"IF(reg.d!=lit)GOTOlit"
       , "d0d00000:ra:br_lit:br_offset"},
   {"IF(reg.d>lit)GOTOlit"
       , "d0e00000:ra:br_lit:br_offset"},
   {"IF(reg.d<lit)GOTOlit"
       , "d0f00000:ra:br_lit:br_offset"},
   {"IF(reg==lit)GOTOlit"
       , "d0080000:ra:br_lit:br_offset"},
   {"IF(reg!=lit)GOTOlit"
       , "d0180000:ra:br_lit:br_offset"},
   {"IF(reg>lit)GOTOlit"
       , "d0280000:ra:br_lit:br_offset"},
   {"IF(reg<lit)GOTOlit"
       , "d0380000:ra:br_lit:br_offset"},

   // Op code = 209 (0xd1)
   {"IF(reg.a==lit,reg.a+=lit)GOTOlit"
       , "d1000000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.a!=lit,reg.a+=lit)GOTOlit"
       , "d1080000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.a>lit,reg.a+=lit)GOTOlit"
       , "d1100000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.a>=lit,reg.a+=lit)GOTOlit"
       , "d1180000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.a<lit,reg.a+=lit)GOTOlit"
       , "d1200000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.a<=lit,reg.a+=lit)GOTOlit"
       , "d1280000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.b==lit,reg.b+=lit)GOTOlit"
       , "d1400000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.b!=lit,reg.b+=lit)GOTOlit"
       , "d1480000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.b>lit,reg.b+=lit)GOTOlit"
       , "d1500000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.b>=lit,reg.b+=lit)GOTOlit"
       , "d1580000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.b<lit,reg.b+=lit)GOTOlit"
       , "d1600000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.b<=lit,reg.b+=lit)GOTOlit"
       , "d1680000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.c==lit,reg.c+=lit)GOTOlit"
       , "d1800000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.c!=lit,reg.c+=lit)GOTOlit"
       , "d1880000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.c>lit,reg.c+=lit)GOTOlit"
       , "d1900000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.c>=lit,reg.c+=lit)GOTOlit"
       , "d1980000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.c<lit,reg.c+=lit)GOTOlit"
       , "d1a00000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.c<=lit,reg.c+=lit)GOTOlit"
       , "d1a80000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.d==lit,reg.d+=lit)GOTOlit"
       , "d1c00000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.d!=lit,reg.d+=lit)GOTOlit"
       , "d1c80000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.d>lit,reg.d+=lit)GOTOlit"
       , "d1d00000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.d>=lit,reg.d+=lit)GOTOlit"
       , "d1d80000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.d<lit,reg.d+=lit)GOTOlit"
       , "d1e00000:ra:0:ra:bri_inc:bri_offset"},
   {"IF(reg.d<=lit,reg.d+=lit)GOTOlit"
       , "d1e80000:ra:0:ra:bri_inc:bri_offset"},

   // Op code = 210 (0xd2)
   {"TRAPlit"
       , "d2000000:trap_lit"},

   // Op code = 211 (0xd3)
   {"CONDEX(reg==reg)"
       , "d3000000:ra:rb"},
   {"CONDEX(reg!=reg)"
       , "d3100000:ra:rb"},
   {"CONDEX(reg>reg)"
       , "d3200000:ra:rb"},
   {"CONDEX(reg>=reg)"
       , "d3300000:ra:rb"},
   {"CONDEX_S(reg==reg)"
       , "d3000001:ra:rb"},
   {"CONDEX_S(reg!=reg)"
       , "d3100001:ra:rb"},
   {"CONDEX_S(reg>reg)"
       , "d3200001:ra:rb"},
   {"CONDEX_S(reg>=reg)"
       , "d3300001:ra:rb"},

   // Op code = 212 (0xd4)
   {"CONDEX(reg==lit)"
       , "d4000000:ra:br_lit"},
   {"CONDEX(reg!=lit)"
       , "d4100000:ra:br_lit"},
   {"CONDEX(reg>lit)"
       , "d4200000:ra:br_lit"},
   {"CONDEX(reg<lit)"
       , "d4300000:ra:br_lit"},
   {"CONDEX_S(reg==lit)"
       , "d4000001:ra:br_lit"},
   {"CONDEX_S(reg!=lit)"
       , "d4100001:ra:br_lit"},
   {"CONDEX_S(reg>lit)"
       , "d4200001:ra:br_lit"},
   {"CONDEX_S(reg<lit)"
       , "d4300001:ra:br_lit"},

   // Op code = 213 (0xd5)
   {"ENDCONDEX"
       , "d5000000"},

   // Op code = 214 (0xd6)
   {"reg.a=MEMB(areg.a+lit)"
       , "d6000061:mem_rm:mem_ar:memli_lit"},
   {"reg.a=MEMB(areg.a)"
       , "d6000061:mem_rm:mem_ar"},
   {"reg.b=MEMB(areg.b+lit)"
       , "d6400061:mem_rm:mem_ar:memli_lit"},
   {"reg.b=MEMB(areg.b)"
       , "d6400061:mem_rm:mem_ar"},
   {"reg.c=MEMB(areg.c+lit)"
       , "d6800061:mem_rm:mem_ar:memli_lit"},
   {"reg.c=MEMB(areg.c)"
       , "d6800061:mem_rm:mem_ar"},
   {"reg.d=MEMB(areg.d+lit)"
       , "d6c00061:mem_rm:mem_ar:memli_lit"},
   {"reg.d=MEMB(areg.d)"
       , "d6c00061:mem_rm:mem_ar"},
   {"reg.a=MEMB(areg.a),areg.a+=lit"
       , "d6000021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=MEMB(areg.b),areg.b+=lit"
       , "d6400021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=MEMB(areg.c),areg.c+=lit"
       , "d6800021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=MEMB(areg.d),areg.d+=lit"
       , "d6c00021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.a=MEMW(areg.a+lit)"
       , "d6000051:mem_rm:mem_ar:memli_lit"},
   {"reg.a=MEMW(areg.a)"
       , "d6000051:mem_rm:mem_ar"},
   {"reg.b=MEMW(areg.b+lit)"
       , "d6400051:mem_rm:mem_ar:memli_lit"},
   {"reg.b=MEMW(areg.b)"
       , "d6400051:mem_rm:mem_ar"},
   {"reg.c=MEMW(areg.c+lit)"
       , "d6800051:mem_rm:mem_ar:memli_lit"},
   {"reg.c=MEMW(areg.c)"
       , "d6800051:mem_rm:mem_ar"},
   {"reg.d=MEMW(areg.d+lit)"
       , "d6c00051:mem_rm:mem_ar:memli_lit"},
   {"reg.d=MEMW(areg.d)"
       , "d6c00051:mem_rm:mem_ar"},
   {"reg.a=MEMW(areg.a),areg.a+=lit"
       , "d6000011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=MEMW(areg.b),areg.b+=lit"
       , "d6400011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=MEMW(areg.c),areg.c+=lit"
       , "d6800011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=MEMW(areg.d),areg.d+=lit"
       , "d6c00011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.a=MEML(areg.a+lit)"
       , "d6000041:mem_rm:mem_ar:memli_lit"},
   {"reg.a=MEML(areg.a)"
       , "d6000041:mem_rm:mem_ar"},
   {"reg.b=MEML(areg.b+lit)"
       , "d6400041:mem_rm:mem_ar:memli_lit"},
   {"reg.b=MEML(areg.b)"
       , "d6400041:mem_rm:mem_ar"},
   {"reg.c=MEML(areg.c+lit)"
       , "d6800041:mem_rm:mem_ar:memli_lit"},
   {"reg.c=MEML(areg.c)"
       , "d6800041:mem_rm:mem_ar"},
   {"reg.d=MEML(areg.d+lit)"
       , "d6c00041:mem_rm:mem_ar:memli_lit"},
   {"reg.d=MEML(areg.d)"
       , "d6c00041:mem_rm:mem_ar"},
   {"reg.a=MEML(areg.a),areg.a+=lit"
       , "d6000001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=MEML(areg.b),areg.b+=lit"
       , "d6400001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=MEML(areg.c),areg.c+=lit"
       , "d6800001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=MEML(areg.d),areg.d+=lit"
       , "d6c00001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMB(areg.a+lit)"
       , "d6000065:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMB(areg.a)"
       , "d6000065:mem_rm:mem_ar"},
   {"reg=MEMB(areg.b+lit)"
       , "d6400065:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMB(areg.b)"
       , "d6400065:mem_rm:mem_ar"},
   {"reg=MEMB(areg.c+lit)"
       , "d6800065:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMB(areg.c)"
       , "d6800065:mem_rm:mem_ar"},
   {"reg=MEMB(areg.d+lit)"
       , "d6c00065:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMB(areg.d)"
       , "d6c00065:mem_rm:mem_ar"},
   {"reg=MEMB(areg.a),areg.a+=lit"
       , "d6000025:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMB(areg.b),areg.b+=lit"
       , "d6400025:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMB(areg.c),areg.c+=lit"
       , "d6800025:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMB(areg.d),areg.d+=lit"
       , "d6c00025:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMW(areg.a+lit)"
       , "d6000055:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMW(areg.a)"
       , "d6000055:mem_rm:mem_ar"},
   {"reg=MEMW(areg.b+lit)"
       , "d6400055:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMW(areg.b)"
       , "d6400055:mem_rm:mem_ar"},
   {"reg=MEMW(areg.c+lit)"
       , "d6800055:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMW(areg.c)"
       , "d6800055:mem_rm:mem_ar"},
   {"reg=MEMW(areg.d+lit)"
       , "d6c00055:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMW(areg.d)"
       , "d6c00055:mem_rm:mem_ar"},
   {"reg=MEMW(areg.a),areg.a+=lit"
       , "d6000015:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMW(areg.b),areg.b+=lit"
       , "d6400015:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMW(areg.c),areg.c+=lit"
       , "d6800015:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMW(areg.d),areg.d+=lit"
       , "d6c00015:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEML(areg.a+lit)"
       , "d6000045:mem_rm:mem_ar:memli_lit"},
   {"reg=MEML(areg.a)"
       , "d6000045:mem_rm:mem_ar"},
   {"reg=MEML(areg.b+lit)"
       , "d6400045:mem_rm:mem_ar:memli_lit"},
   {"reg=MEML(areg.b)"
       , "d6400045:mem_rm:mem_ar"},
   {"reg=MEML(areg.c+lit)"
       , "d6800045:mem_rm:mem_ar:memli_lit"},
   {"reg=MEML(areg.c)"
       , "d6800045:mem_rm:mem_ar"},
   {"reg=MEML(areg.d+lit)"
       , "d6c00045:mem_rm:mem_ar:memli_lit"},
   {"reg=MEML(areg.d)"
       , "d6c00045:mem_rm:mem_ar"},
   {"reg=MEML(areg.a),areg.a+=lit"
       , "d6000005:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEML(areg.b),areg.b+=lit"
       , "d6400005:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEML(areg.c),areg.c+=lit"
       , "d6800005:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEML(areg.d),areg.d+=lit"
       , "d6c00005:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMB(areg+lit)"
       , "d6000069:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMB(areg)"
       , "d6000069:mem_rm:mem_ar"},
   {"reg=MEMB(areg),areg+=lit"
       , "d6000029:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMW(areg+lit)"
       , "d6000059:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMW(areg)"
       , "d6000059:mem_rm:mem_ar"},
   {"reg=MEMW(areg),areg+=lit"
       , "d6000019:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEML(areg+lit)"
       , "d6000049:mem_rm:mem_ar:memli_lit"},
   {"reg=MEML(areg)"
       , "d6000049:mem_rm:mem_ar"},
   {"reg=MEML(areg),areg+=lit"
       , "d6000009:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.a=MEMPB(areg.a+lit)"
       , "d6000063:mem_rm:mem_ar:memli_lit"},
   {"reg.a=MEMPB(areg.a)"
       , "d6000063:mem_rm:mem_ar"},
   {"reg.b=MEMPB(areg.b+lit)"
       , "d6400063:mem_rm:mem_ar:memli_lit"},
   {"reg.b=MEMPB(areg.b)"
       , "d6400063:mem_rm:mem_ar"},
   {"reg.c=MEMPB(areg.c+lit)"
       , "d6800063:mem_rm:mem_ar:memli_lit"},
   {"reg.c=MEMPB(areg.c)"
       , "d6800063:mem_rm:mem_ar"},
   {"reg.d=MEMPB(areg.d+lit)"
       , "d6c00063:mem_rm:mem_ar:memli_lit"},
   {"reg.d=MEMPB(areg.d)"
       , "d6c00063:mem_rm:mem_ar"},
   {"reg.a=MEMPB(areg.a),areg.a+=lit"
       , "d6000023:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=MEMPB(areg.b),areg.b+=lit"
       , "d6400023:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=MEMPB(areg.c),areg.c+=lit"
       , "d6800023:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=MEMPB(areg.d),areg.d+=lit"
       , "d6c00023:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.a=MEMPW(areg.a+lit)"
       , "d6000053:mem_rm:mem_ar:memli_lit"},
   {"reg.a=MEMPW(areg.a)"
       , "d6000053:mem_rm:mem_ar"},
   {"reg.b=MEMPW(areg.b+lit)"
       , "d6400053:mem_rm:mem_ar:memli_lit"},
   {"reg.b=MEMPW(areg.b)"
       , "d6400053:mem_rm:mem_ar"},
   {"reg.c=MEMPW(areg.c+lit)"
       , "d6800053:mem_rm:mem_ar:memli_lit"},
   {"reg.c=MEMPW(areg.c)"
       , "d6800053:mem_rm:mem_ar"},
   {"reg.d=MEMPW(areg.d+lit)"
       , "d6c00053:mem_rm:mem_ar:memli_lit"},
   {"reg.d=MEMPW(areg.d)"
       , "d6c00053:mem_rm:mem_ar"},
   {"reg.a=MEMPW(areg.a),areg.a+=lit"
       , "d6000013:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=MEMPW(areg.b),areg.b+=lit"
       , "d6400013:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=MEMPW(areg.c),areg.c+=lit"
       , "d6800013:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=MEMPW(areg.d),areg.d+=lit"
       , "d6c00013:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.a=MEMPL(areg.a+lit)"
       , "d6000043:mem_rm:mem_ar:memli_lit"},
   {"reg.a=MEMPL(areg.a)"
       , "d6000043:mem_rm:mem_ar"},
   {"reg.b=MEMPL(areg.b+lit)"
       , "d6400043:mem_rm:mem_ar:memli_lit"},
   {"reg.b=MEMPL(areg.b)"
       , "d6400043:mem_rm:mem_ar"},
   {"reg.c=MEMPL(areg.c+lit)"
       , "d6800043:mem_rm:mem_ar:memli_lit"},
   {"reg.c=MEMPL(areg.c)"
       , "d6800043:mem_rm:mem_ar"},
   {"reg.d=MEMPL(areg.d+lit)"
       , "d6c00043:mem_rm:mem_ar:memli_lit"},
   {"reg.d=MEMPL(areg.d)"
       , "d6c00043:mem_rm:mem_ar"},
   {"reg.a=MEMPL(areg.a),areg.a+=lit"
       , "d6000003:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=MEMPL(areg.b),areg.b+=lit"
       , "d6400003:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=MEMPL(areg.c),areg.c+=lit"
       , "d6800003:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=MEMPL(areg.d),areg.d+=lit"
       , "d6c00003:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.a+lit)"
       , "d6000067:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.a)"
       , "d6000067:mem_rm:mem_ar"},
   {"reg=MEMPB(areg.b+lit)"
       , "d6400067:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.b)"
       , "d6400067:mem_rm:mem_ar"},
   {"reg=MEMPB(areg.c+lit)"
       , "d6800067:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.c)"
       , "d6800067:mem_rm:mem_ar"},
   {"reg=MEMPB(areg.d+lit)"
       , "d6c00067:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.d)"
       , "d6c00067:mem_rm:mem_ar"},
   {"reg=MEMPB(areg.a),areg.a+=lit"
       , "d6000027:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.b),areg.b+=lit"
       , "d6400027:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.c),areg.c+=lit"
       , "d6800027:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPB(areg.d),areg.d+=lit"
       , "d6c00027:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.a+lit)"
       , "d6000057:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.a)"
       , "d6000057:mem_rm:mem_ar"},
   {"reg=MEMPW(areg.b+lit)"
       , "d6400057:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.b)"
       , "d6400057:mem_rm:mem_ar"},
   {"reg=MEMPW(areg.c+lit)"
       , "d6800057:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.c)"
       , "d6800057:mem_rm:mem_ar"},
   {"reg=MEMPW(areg.d+lit)"
       , "d6c00057:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.d)"
       , "d6c00057:mem_rm:mem_ar"},
   {"reg=MEMPW(areg.a),areg.a+=lit"
       , "d6000017:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.b),areg.b+=lit"
       , "d6400017:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.c),areg.c+=lit"
       , "d6800017:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPW(areg.d),areg.d+=lit"
       , "d6c00017:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.a+lit)"
       , "d6000047:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.a)"
       , "d6000047:mem_rm:mem_ar"},
   {"reg=MEMPL(areg.b+lit)"
       , "d6400047:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.b)"
       , "d6400047:mem_rm:mem_ar"},
   {"reg=MEMPL(areg.c+lit)"
       , "d6800047:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.c)"
       , "d6800047:mem_rm:mem_ar"},
   {"reg=MEMPL(areg.d+lit)"
       , "d6c00047:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.d)"
       , "d6c00047:mem_rm:mem_ar"},
   {"reg=MEMPL(areg.a),areg.a+=lit"
       , "d6000007:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.b),areg.b+=lit"
       , "d6400007:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.c),areg.c+=lit"
       , "d6800007:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPL(areg.d),areg.d+=lit"
       , "d6c00007:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPB(areg+lit)"
       , "d600006b:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPB(areg)"
       , "d600006b:mem_rm:mem_ar"},
   {"reg=MEMPB(areg),areg+=lit"
       , "d600002b:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPW(areg+lit)"
       , "d600005b:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPW(areg)"
       , "d600005b:mem_rm:mem_ar"},
   {"reg=MEMPW(areg),areg+=lit"
       , "d600001b:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=MEMPL(areg+lit)"
       , "d600004b:mem_rm:mem_ar:memli_lit"},
   {"reg=MEMPL(areg)"
       , "d600004b:mem_rm:mem_ar"},
   {"reg=MEMPL(areg),areg+=lit"
       , "d600000b:mem_rm:mem_ar:mem_ar:memli_lit"},

   // Op code = 215 (0xd7)
   {"reg.a=MEMB(areg.a+reg.a)"
       , "d7000061:mem_rm:mem_ar:rb"},
   {"reg.b=MEMB(areg.b+reg.b)"
       , "d7400061:mem_rm:mem_ar:rb"},
   {"reg.c=MEMB(areg.c+reg.c)"
       , "d7800061:mem_rm:mem_ar:rb"},
   {"reg.d=MEMB(areg.d+reg.d)"
       , "d7c00061:mem_rm:mem_ar:rb"},
   {"reg.a=MEMB(areg.a),areg.a+=ireg.a"
       , "d7000021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=MEMB(areg.b),areg.b+=ireg.b"
       , "d7400021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=MEMB(areg.c),areg.c+=ireg.c"
       , "d7800021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=MEMB(areg.d),areg.d+=ireg.d"
       , "d7c00021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.a=MEMW(areg.a+reg.a)"
       , "d7000051:mem_rm:mem_ar:rb"},
   {"reg.b=MEMW(areg.b+reg.b)"
       , "d7400051:mem_rm:mem_ar:rb"},
   {"reg.c=MEMW(areg.c+reg.c)"
       , "d7800051:mem_rm:mem_ar:rb"},
   {"reg.d=MEMW(areg.d+reg.d)"
       , "d7c00051:mem_rm:mem_ar:rb"},
   {"reg.a=MEMW(areg.a),areg.a+=ireg.a"
       , "d7000011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=MEMW(areg.b),areg.b+=ireg.b"
       , "d7400011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=MEMW(areg.c),areg.c+=ireg.c"
       , "d7800011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=MEMW(areg.d),areg.d+=ireg.d"
       , "d7c00011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.a=MEML(areg.a+reg.a)"
       , "d7000041:mem_rm:mem_ar:rb"},
   {"reg.b=MEML(areg.b+reg.b)"
       , "d7400041:mem_rm:mem_ar:rb"},
   {"reg.c=MEML(areg.c+reg.c)"
       , "d7800041:mem_rm:mem_ar:rb"},
   {"reg.d=MEML(areg.d+reg.d)"
       , "d7c00041:mem_rm:mem_ar:rb"},
   {"reg.a=MEML(areg.a),areg.a+=ireg.a"
       , "d7000001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=MEML(areg.b),areg.b+=ireg.b"
       , "d7400001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=MEML(areg.c),areg.c+=ireg.c"
       , "d7800001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=MEML(areg.d),areg.d+=ireg.d"
       , "d7c00001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMB(areg.a+reg.a)"
       , "d7000065:mem_rm:mem_ar:rb"},
   {"reg=MEMB(areg.b+reg.b)"
       , "d7400065:mem_rm:mem_ar:rb"},
   {"reg=MEMB(areg.c+reg.c)"
       , "d7800065:mem_rm:mem_ar:rb"},
   {"reg=MEMB(areg.d+reg.d)"
       , "d7c00065:mem_rm:mem_ar:rb"},
   {"reg=MEMB(areg.a),areg.a+=ireg.a"
       , "d7000025:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMB(areg.b),areg.b+=ireg.b"
       , "d7400025:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMB(areg.c),areg.c+=ireg.c"
       , "d7800025:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMB(areg.d),areg.d+=ireg.d"
       , "d7c00025:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMW(areg.a+reg.a)"
       , "d7000055:mem_rm:mem_ar:rb"},
   {"reg=MEMW(areg.b+reg.b)"
       , "d7400055:mem_rm:mem_ar:rb"},
   {"reg=MEMW(areg.c+reg.c)"
       , "d7800055:mem_rm:mem_ar:rb"},
   {"reg=MEMW(areg.d+reg.d)"
       , "d7c00055:mem_rm:mem_ar:rb"},
   {"reg=MEMW(areg.a),areg.a+=ireg.a"
       , "d7000015:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMW(areg.b),areg.b+=ireg.b"
       , "d7400015:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMW(areg.c),areg.c+=ireg.c"
       , "d7800015:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMW(areg.d),areg.d+=ireg.d"
       , "d7c00015:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEML(areg.a+reg.a)"
       , "d7000045:mem_rm:mem_ar:rb"},
   {"reg=MEML(areg.b+reg.b)"
       , "d7400045:mem_rm:mem_ar:rb"},
   {"reg=MEML(areg.c+reg.c)"
       , "d7800045:mem_rm:mem_ar:rb"},
   {"reg=MEML(areg.d+reg.d)"
       , "d7c00045:mem_rm:mem_ar:rb"},
   {"reg=MEML(areg.a),areg.a+=ireg.a"
       , "d7000005:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEML(areg.b),areg.b+=ireg.b"
       , "d7400005:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEML(areg.c),areg.c+=ireg.c"
       , "d7800005:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEML(areg.d),areg.d+=ireg.d"
       , "d7c00005:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMB(areg+reg)"
       , "d7000069:mem_rm:mem_ar:rb"},
   {"reg=MEMB(areg),areg+=ireg"
       , "d7000029:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMW(areg+reg)"
       , "d7000059:mem_rm:mem_ar:rb"},
   {"reg=MEMW(areg),areg+=ireg"
       , "d7000019:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEML(areg+reg)"
       , "d7000049:mem_rm:mem_ar:rb"},
   {"reg=MEML(areg),areg+=ireg"
       , "d7000009:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.a=MEMPB(areg.a+reg.a)"
       , "d7000063:mem_rm:mem_ar:rb"},
   {"reg.b=MEMPB(areg.b+reg.b)"
       , "d7400063:mem_rm:mem_ar:rb"},
   {"reg.c=MEMPB(areg.c+reg.c)"
       , "d7800063:mem_rm:mem_ar:rb"},
   {"reg.d=MEMPB(areg.d+reg.d)"
       , "d7c00063:mem_rm:mem_ar:rb"},
   {"reg.a=MEMPB(areg.a),areg.a+=ireg.a"
       , "d7000023:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=MEMPB(areg.b),areg.b+=ireg.b"
       , "d7400023:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=MEMPB(areg.c),areg.c+=ireg.c"
       , "d7800023:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=MEMPB(areg.d),areg.d+=ireg.d"
       , "d7c00023:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.a=MEMPW(areg.a+reg.a)"
       , "d7000053:mem_rm:mem_ar:rb"},
   {"reg.b=MEMPW(areg.b+reg.b)"
       , "d7400053:mem_rm:mem_ar:rb"},
   {"reg.c=MEMPW(areg.c+reg.c)"
       , "d7800053:mem_rm:mem_ar:rb"},
   {"reg.d=MEMPW(areg.d+reg.d)"
       , "d7c00053:mem_rm:mem_ar:rb"},
   {"reg.a=MEMPW(areg.a),areg.a+=ireg.a"
       , "d7000013:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=MEMPW(areg.b),areg.b+=ireg.b"
       , "d7400013:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=MEMPW(areg.c),areg.c+=ireg.c"
       , "d7800013:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=MEMPW(areg.d),areg.d+=ireg.d"
       , "d7c00013:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.a=MEMPL(areg.a+reg.a)"
       , "d7000043:mem_rm:mem_ar:rb"},
   {"reg.b=MEMPL(areg.b+reg.b)"
       , "d7400043:mem_rm:mem_ar:rb"},
   {"reg.c=MEMPL(areg.c+reg.c)"
       , "d7800043:mem_rm:mem_ar:rb"},
   {"reg.d=MEMPL(areg.d+reg.d)"
       , "d7c00043:mem_rm:mem_ar:rb"},
   {"reg.a=MEMPL(areg.a),areg.a+=ireg.a"
       , "d7000003:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=MEMPL(areg.b),areg.b+=ireg.b"
       , "d7400003:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=MEMPL(areg.c),areg.c+=ireg.c"
       , "d7800003:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=MEMPL(areg.d),areg.d+=ireg.d"
       , "d7c00003:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPB(areg.a+reg.a)"
       , "d7000067:mem_rm:mem_ar:rb"},
   {"reg=MEMPB(areg.b+reg.b)"
       , "d7400067:mem_rm:mem_ar:rb"},
   {"reg=MEMPB(areg.c+reg.c)"
       , "d7800067:mem_rm:mem_ar:rb"},
   {"reg=MEMPB(areg.d+reg.d)"
       , "d7c00067:mem_rm:mem_ar:rb"},
   {"reg=MEMPB(areg.a),areg.a+=ireg.a"
       , "d7000027:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPB(areg.b),areg.b+=ireg.b"
       , "d7400027:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPB(areg.c),areg.c+=ireg.c"
       , "d7800027:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPB(areg.d),areg.d+=ireg.d"
       , "d7c00027:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPW(areg.a+reg.a)"
       , "d7000057:mem_rm:mem_ar:rb"},
   {"reg=MEMPW(areg.b+reg.b)"
       , "d7400057:mem_rm:mem_ar:rb"},
   {"reg=MEMPW(areg.c+reg.c)"
       , "d7800057:mem_rm:mem_ar:rb"},
   {"reg=MEMPW(areg.d+reg.d)"
       , "d7c00057:mem_rm:mem_ar:rb"},
   {"reg=MEMPW(areg.a),areg.a+=ireg.a"
       , "d7000017:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPW(areg.b),areg.b+=ireg.b"
       , "d7400017:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPW(areg.c),areg.c+=ireg.c"
       , "d7800017:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPW(areg.d),areg.d+=ireg.d"
       , "d7c00017:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPL(areg.a+reg.a)"
       , "d7000047:mem_rm:mem_ar:rb"},
   {"reg=MEMPL(areg.b+reg.b)"
       , "d7400047:mem_rm:mem_ar:rb"},
   {"reg=MEMPL(areg.c+reg.c)"
       , "d7800047:mem_rm:mem_ar:rb"},
   {"reg=MEMPL(areg.d+reg.d)"
       , "d7c00047:mem_rm:mem_ar:rb"},
   {"reg=MEMPL(areg.a),areg.a+=ireg.a"
       , "d7000007:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPL(areg.b),areg.b+=ireg.b"
       , "d7400007:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPL(areg.c),areg.c+=ireg.c"
       , "d7800007:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPL(areg.d),areg.d+=ireg.d"
       , "d7c00007:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPB(areg+reg)"
       , "d700006b:mem_rm:mem_ar:rb"},
   {"reg=MEMPB(areg),areg+=ireg"
       , "d700002b:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPW(areg+reg)"
       , "d700005b:mem_rm:mem_ar:rb"},
   {"reg=MEMPW(areg),areg+=ireg"
       , "d700001b:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=MEMPL(areg+reg)"
       , "d700004b:mem_rm:mem_ar:rb"},
   {"reg=MEMPL(areg),areg+=ireg"
       , "d700000b:mem_rm:mem_ar:mem_ar:mem_ir"},

   // Op code = 216 (0xd8)
   {"reg.a=MEML(lit)"
       , "d8000000:mem_rm:memlit"},
   {"reg.b=MEML(lit)"
       , "d8400000:mem_rm:memlit"},
   {"reg.c=MEML(lit)"
       , "d8800000:mem_rm:memlit"},
   {"reg.d=MEML(lit)"
       , "d8c00000:mem_rm:memlit"},
   {"reg=MEMPL(lit)"
       , "d8000008:mem_rm:memlit"},

   // Op code = 217 (0xd9)
   {"MEMB(areg.a+lit)=reg.a"
       , "d9000061:mem_ar:memli_lit:ra"},
   {"MEMB(areg.a)=reg.a"
       , "d9000061:mem_ar:ra"},
   {"MEMB(areg.b+lit)=reg.b"
       , "d9400061:mem_ar:memli_lit:ra"},
   {"MEMB(areg.b)=reg.b"
       , "d9400061:mem_ar:ra"},
   {"MEMB(areg.c+lit)=reg.c"
       , "d9800061:mem_ar:memli_lit:ra"},
   {"MEMB(areg.c)=reg.c"
       , "d9800061:mem_ar:ra"},
   {"MEMB(areg.d+lit)=reg.d"
       , "d9c00061:mem_ar:memli_lit:ra"},
   {"MEMB(areg.d)=reg.d"
       , "d9c00061:mem_ar:ra"},
   {"MEMB(areg.a)=reg.a,areg.a+=lit"
       , "d9000021:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.a=BYTE(areg.a+lit)"
       , "d9000020:mem_ar:mem_ar:memli_lit"},
   {"MEMB(areg.b)=reg.b,areg.b+=lit"
       , "d9400021:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.b=BYTE(areg.b+lit)"
       , "d9400020:mem_ar:mem_ar:memli_lit"},
   {"MEMB(areg.c)=reg.c,areg.c+=lit"
       , "d9800021:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.c=BYTE(areg.c+lit)"
       , "d9800020:mem_ar:mem_ar:memli_lit"},
   {"MEMB(areg.d)=reg.d,areg.d+=lit"
       , "d9c00021:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.d=BYTE(areg.d+lit)"
       , "d9c00020:mem_ar:mem_ar:memli_lit"},
   {"MEMW(areg.a+lit)=reg.a"
       , "d9000051:mem_ar:memli_lit:ra"},
   {"MEMW(areg.a)=reg.a"
       , "d9000051:mem_ar:ra"},
   {"MEMW(areg.b+lit)=reg.b"
       , "d9400051:mem_ar:memli_lit:ra"},
   {"MEMW(areg.b)=reg.b"
       , "d9400051:mem_ar:ra"},
   {"MEMW(areg.c+lit)=reg.c"
       , "d9800051:mem_ar:memli_lit:ra"},
   {"MEMW(areg.c)=reg.c"
       , "d9800051:mem_ar:ra"},
   {"MEMW(areg.d+lit)=reg.d"
       , "d9c00051:mem_ar:memli_lit:ra"},
   {"MEMW(areg.d)=reg.d"
       , "d9c00051:mem_ar:ra"},
   {"MEMW(areg.a)=reg.a,areg.a+=lit"
       , "d9000011:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.a=WORD(areg.a+lit)"
       , "d9000010:mem_ar:mem_ar:memli_lit"},
   {"MEMW(areg.b)=reg.b,areg.b+=lit"
       , "d9400011:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.b=WORD(areg.b+lit)"
       , "d9400010:mem_ar:mem_ar:memli_lit"},
   {"MEMW(areg.c)=reg.c,areg.c+=lit"
       , "d9800011:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.c=WORD(areg.c+lit)"
       , "d9800010:mem_ar:mem_ar:memli_lit"},
   {"MEMW(areg.d)=reg.d,areg.d+=lit"
       , "d9c00011:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.d=WORD(areg.d+lit)"
       , "d9c00010:mem_ar:mem_ar:memli_lit"},
   {"MEML(areg.a+lit)=reg.a"
       , "d9000041:mem_ar:memli_lit:ra"},
   {"MEML(areg.a)=reg.a"
       , "d9000041:mem_ar:ra"},
   {"MEML(areg.b+lit)=reg.b"
       , "d9400041:mem_ar:memli_lit:ra"},
   {"MEML(areg.b)=reg.b"
       , "d9400041:mem_ar:ra"},
   {"MEML(areg.c+lit)=reg.c"
       , "d9800041:mem_ar:memli_lit:ra"},
   {"MEML(areg.c)=reg.c"
       , "d9800041:mem_ar:ra"},
   {"MEML(areg.d+lit)=reg.d"
       , "d9c00041:mem_ar:memli_lit:ra"},
   {"MEML(areg.d)=reg.d"
       , "d9c00041:mem_ar:ra"},
   {"MEML(areg.a)=reg.a,areg.a+=lit"
       , "d9000001:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.a=LONG(areg.a+lit)"
       , "d9000000:mem_ar:mem_ar:memli_lit"},
   {"MEML(areg.b)=reg.b,areg.b+=lit"
       , "d9400001:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.b=LONG(areg.b+lit)"
       , "d9400000:mem_ar:mem_ar:memli_lit"},
   {"MEML(areg.c)=reg.c,areg.c+=lit"
       , "d9800001:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.c=LONG(areg.c+lit)"
       , "d9800000:mem_ar:mem_ar:memli_lit"},
   {"MEML(areg.d)=reg.d,areg.d+=lit"
       , "d9c00001:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.d=LONG(areg.d+lit)"
       , "d9c00000:mem_ar:mem_ar:memli_lit"},
   {"MEMB(areg+lit)=reg"
       , "d9000069:mem_ar:memli_lit:ra"},
   {"MEMB(areg)=reg"
       , "d9000069:mem_ar:ra"},
   {"MEMB(areg)=reg,areg+=lit"
       , "d9000029:mem_ar:ra:mem_ar:memli_lit"},
   {"areg=BYTE(areg+lit)"
       , "d9000028:mem_ar:mem_ar:memli_lit"},
   {"MEMW(areg+lit)=reg"
       , "d9000059:mem_ar:memli_lit:ra"},
   {"MEMW(areg)=reg"
       , "d9000059:mem_ar:ra"},
   {"MEMW(areg)=reg,areg+=lit"
       , "d9000019:mem_ar:ra:mem_ar:memli_lit"},
   {"areg=WORD(areg+lit)"
       , "d9000018:mem_ar:mem_ar:memli_lit"},
   {"MEML(areg+lit)=reg"
       , "d9000049:mem_ar:memli_lit:ra"},
   {"MEML(areg)=reg"
       , "d9000049:mem_ar:ra"},
   {"MEML(areg)=reg,areg+=lit"
       , "d9000009:mem_ar:ra:mem_ar:memli_lit"},
   {"areg=LONG(areg+lit)"
       , "d9000008:mem_ar:mem_ar:memli_lit"},
   {"MEMPB(areg.a+lit)=reg.a"
       , "d9000063:mem_ar:memli_lit:ra"},
   {"MEMPB(areg.a)=reg.a"
       , "d9000063:mem_ar:ra"},
   {"MEMPB(areg.b+lit)=reg.b"
       , "d9400063:mem_ar:memli_lit:ra"},
   {"MEMPB(areg.b)=reg.b"
       , "d9400063:mem_ar:ra"},
   {"MEMPB(areg.c+lit)=reg.c"
       , "d9800063:mem_ar:memli_lit:ra"},
   {"MEMPB(areg.c)=reg.c"
       , "d9800063:mem_ar:ra"},
   {"MEMPB(areg.d+lit)=reg.d"
       , "d9c00063:mem_ar:memli_lit:ra"},
   {"MEMPB(areg.d)=reg.d"
       , "d9c00063:mem_ar:ra"},
   {"MEMPB(areg.a)=reg.a,areg.a+=lit"
       , "d9000023:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.a=PBYTE(areg.a+lit)"
       , "d9000022:mem_ar:mem_ar:memli_lit"},
   {"MEMPB(areg.b)=reg.b,areg.b+=lit"
       , "d9400023:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.b=PBYTE(areg.b+lit)"
       , "d9400022:mem_ar:mem_ar:memli_lit"},
   {"MEMPB(areg.c)=reg.c,areg.c+=lit"
       , "d9800023:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.c=PBYTE(areg.c+lit)"
       , "d9800022:mem_ar:mem_ar:memli_lit"},
   {"MEMPB(areg.d)=reg.d,areg.d+=lit"
       , "d9c00023:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.d=PBYTE(areg.d+lit)"
       , "d9c00022:mem_ar:mem_ar:memli_lit"},
   {"MEMPW(areg.a+lit)=reg.a"
       , "d9000053:mem_ar:memli_lit:ra"},
   {"MEMPW(areg.a)=reg.a"
       , "d9000053:mem_ar:ra"},
   {"MEMPW(areg.b+lit)=reg.b"
       , "d9400053:mem_ar:memli_lit:ra"},
   {"MEMPW(areg.b)=reg.b"
       , "d9400053:mem_ar:ra"},
   {"MEMPW(areg.c+lit)=reg.c"
       , "d9800053:mem_ar:memli_lit:ra"},
   {"MEMPW(areg.c)=reg.c"
       , "d9800053:mem_ar:ra"},
   {"MEMPW(areg.d+lit)=reg.d"
       , "d9c00053:mem_ar:memli_lit:ra"},
   {"MEMPW(areg.d)=reg.d"
       , "d9c00053:mem_ar:ra"},
   {"MEMPW(areg.a)=reg.a,areg.a+=lit"
       , "d9000013:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.a=PWORD(areg.a+lit)"
       , "d9000012:mem_ar:mem_ar:memli_lit"},
   {"MEMPW(areg.b)=reg.b,areg.b+=lit"
       , "d9400013:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.b=PWORD(areg.b+lit)"
       , "d9400012:mem_ar:mem_ar:memli_lit"},
   {"MEMPW(areg.c)=reg.c,areg.c+=lit"
       , "d9800013:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.c=PWORD(areg.c+lit)"
       , "d9800012:mem_ar:mem_ar:memli_lit"},
   {"MEMPW(areg.d)=reg.d,areg.d+=lit"
       , "d9c00013:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.d=PWORD(areg.d+lit)"
       , "d9c00012:mem_ar:mem_ar:memli_lit"},
   {"MEMPL(areg.a+lit)=reg.a"
       , "d9000043:mem_ar:memli_lit:ra"},
   {"MEMPL(areg.a)=reg.a"
       , "d9000043:mem_ar:ra"},
   {"MEMPL(areg.b+lit)=reg.b"
       , "d9400043:mem_ar:memli_lit:ra"},
   {"MEMPL(areg.b)=reg.b"
       , "d9400043:mem_ar:ra"},
   {"MEMPL(areg.c+lit)=reg.c"
       , "d9800043:mem_ar:memli_lit:ra"},
   {"MEMPL(areg.c)=reg.c"
       , "d9800043:mem_ar:ra"},
   {"MEMPL(areg.d+lit)=reg.d"
       , "d9c00043:mem_ar:memli_lit:ra"},
   {"MEMPL(areg.d)=reg.d"
       , "d9c00043:mem_ar:ra"},
   {"MEMPL(areg.a)=reg.a,areg.a+=lit"
       , "d9000003:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.a=PLONG(areg.a+lit)"
       , "d9000002:mem_ar:mem_ar:memli_lit"},
   {"MEMPL(areg.b)=reg.b,areg.b+=lit"
       , "d9400003:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.b=PLONG(areg.b+lit)"
       , "d9400002:mem_ar:mem_ar:memli_lit"},
   {"MEMPL(areg.c)=reg.c,areg.c+=lit"
       , "d9800003:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.c=PLONG(areg.c+lit)"
       , "d9800002:mem_ar:mem_ar:memli_lit"},
   {"MEMPL(areg.d)=reg.d,areg.d+=lit"
       , "d9c00003:mem_ar:ra:mem_ar:memli_lit"},
   {"areg.d=PLONG(areg.d+lit)"
       , "d9c00002:mem_ar:mem_ar:memli_lit"},
   {"MEMPB(areg+lit)=reg"
       , "d900006b:mem_ar:memli_lit:ra"},
   {"MEMPB(areg)=reg"
       , "d900006b:mem_ar:ra"},
   {"MEMPB(areg)=reg,areg+=lit"
       , "d900002b:mem_ar:ra:mem_ar:memli_lit"},
   {"areg=PBYTE(areg+lit)"
       , "d900002a:mem_ar:mem_ar:memli_lit"},
   {"MEMPW(areg+lit)=reg"
       , "d900005b:mem_ar:memli_lit:ra"},
   {"MEMPW(areg)=reg"
       , "d900005b:mem_ar:ra"},
   {"MEMPW(areg)=reg,areg+=lit"
       , "d900001b:mem_ar:ra:mem_ar:memli_lit"},
   {"areg=PWORD(areg+lit)"
       , "d900001a:mem_ar:mem_ar:memli_lit"},
   {"MEMPL(areg+lit)=reg"
       , "d900004b:mem_ar:memli_lit:ra"},
   {"MEMPL(areg)=reg"
       , "d900004b:mem_ar:ra"},
   {"MEMPL(areg)=reg,areg+=lit"
       , "d900000b:mem_ar:ra:mem_ar:memli_lit"},
   {"areg=PLONG(areg+lit)"
       , "d900000a:mem_ar:mem_ar:memli_lit"},

   // Op code = 218 (0xda)
   {"MEMB(areg.a+reg.a)=reg.a"
       , "da000061:mem_ar:rb:ra"},
   {"MEMB(areg.b+reg.b)=reg.b"
       , "da400061:mem_ar:rb:ra"},
   {"MEMB(areg.c+reg.c)=reg.c"
       , "da800061:mem_ar:rb:ra"},
   {"MEMB(areg.d+reg.d)=reg.d"
       , "dac00061:mem_ar:rb:ra"},
   {"MEMB(areg.a)=reg.a,areg.a+=ireg.a"
       , "da000021:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.a=BYTE(areg.a+ireg.a)"
       , "da000020:mem_ar:mem_ar:mem_ir"},
   {"MEMB(areg.b)=reg.b,areg.b+=ireg.b"
       , "da400021:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.b=BYTE(areg.b+ireg.b)"
       , "da400020:mem_ar:mem_ar:mem_ir"},
   {"MEMB(areg.c)=reg.c,areg.c+=ireg.c"
       , "da800021:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.c=BYTE(areg.c+ireg.c)"
       , "da800020:mem_ar:mem_ar:mem_ir"},
   {"MEMB(areg.d)=reg.d,areg.d+=ireg.d"
       , "dac00021:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.d=BYTE(areg.d+ireg.d)"
       , "dac00020:mem_ar:mem_ar:mem_ir"},
   {"MEMW(areg.a+reg.a)=reg.a"
       , "da000051:mem_ar:rb:ra"},
   {"MEMW(areg.b+reg.b)=reg.b"
       , "da400051:mem_ar:rb:ra"},
   {"MEMW(areg.c+reg.c)=reg.c"
       , "da800051:mem_ar:rb:ra"},
   {"MEMW(areg.d+reg.d)=reg.d"
       , "dac00051:mem_ar:rb:ra"},
   {"MEMW(areg.a)=reg.a,areg.a+=ireg.a"
       , "da000011:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.a=WORD(areg.a+ireg.a)"
       , "da000010:mem_ar:mem_ar:mem_ir"},
   {"MEMW(areg.b)=reg.b,areg.b+=ireg.b"
       , "da400011:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.b=WORD(areg.b+ireg.b)"
       , "da400010:mem_ar:mem_ar:mem_ir"},
   {"MEMW(areg.c)=reg.c,areg.c+=ireg.c"
       , "da800011:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.c=WORD(areg.c+ireg.c)"
       , "da800010:mem_ar:mem_ar:mem_ir"},
   {"MEMW(areg.d)=reg.d,areg.d+=ireg.d"
       , "dac00011:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.d=WORD(areg.d+ireg.d)"
       , "dac00010:mem_ar:mem_ar:mem_ir"},
   {"MEML(areg.a+reg.a)=reg.a"
       , "da000041:mem_ar:rb:ra"},
   {"MEML(areg.b+reg.b)=reg.b"
       , "da400041:mem_ar:rb:ra"},
   {"MEML(areg.c+reg.c)=reg.c"
       , "da800041:mem_ar:rb:ra"},
   {"MEML(areg.d+reg.d)=reg.d"
       , "dac00041:mem_ar:rb:ra"},
   {"MEML(areg.a)=reg.a,areg.a+=ireg.a"
       , "da000001:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.a=LONG(areg.a+ireg.a)"
       , "da000000:mem_ar:mem_ar:mem_ir"},
   {"MEML(areg.b)=reg.b,areg.b+=ireg.b"
       , "da400001:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.b=LONG(areg.b+ireg.b)"
       , "da400000:mem_ar:mem_ar:mem_ir"},
   {"MEML(areg.c)=reg.c,areg.c+=ireg.c"
       , "da800001:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.c=LONG(areg.c+ireg.c)"
       , "da800000:mem_ar:mem_ar:mem_ir"},
   {"MEML(areg.d)=reg.d,areg.d+=ireg.d"
       , "dac00001:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.d=LONG(areg.d+ireg.d)"
       , "dac00000:mem_ar:mem_ar:mem_ir"},
   {"MEMB(areg+reg)=reg"
       , "da000069:mem_ar:rb:ra"},
   {"MEMB(areg)=reg,areg+=ireg"
       , "da000029:mem_ar:ra:mem_ar:mem_ir"},
   {"areg=BYTE(areg+ireg)"
       , "da000028:mem_ar:mem_ar:mem_ir"},
   {"MEMW(areg+reg)=reg"
       , "da000059:mem_ar:rb:ra"},
   {"MEMW(areg)=reg,areg+=ireg"
       , "da000019:mem_ar:ra:mem_ar:mem_ir"},
   {"areg=WORD(areg+ireg)"
       , "da000018:mem_ar:mem_ar:mem_ir"},
   {"MEML(areg+reg)=reg"
       , "da000049:mem_ar:rb:ra"},
   {"MEML(areg)=reg,areg+=ireg"
       , "da000009:mem_ar:ra:mem_ar:mem_ir"},
   {"areg=LONG(areg+ireg)"
       , "da000008:mem_ar:mem_ar:mem_ir"},
   {"MEMPB(areg.a+reg.a)=reg.a"
       , "da000063:mem_ar:rb:ra"},
   {"MEMPB(areg.b+reg.b)=reg.b"
       , "da400063:mem_ar:rb:ra"},
   {"MEMPB(areg.c+reg.c)=reg.c"
       , "da800063:mem_ar:rb:ra"},
   {"MEMPB(areg.d+reg.d)=reg.d"
       , "dac00063:mem_ar:rb:ra"},
   {"MEMPB(areg.a)=reg.a,areg.a+=ireg.a"
       , "da000023:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.a=PBYTE(areg.a+ireg.a)"
       , "da000022:mem_ar:mem_ar:mem_ir"},
   {"MEMPB(areg.b)=reg.b,areg.b+=ireg.b"
       , "da400023:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.b=PBYTE(areg.b+ireg.b)"
       , "da400022:mem_ar:mem_ar:mem_ir"},
   {"MEMPB(areg.c)=reg.c,areg.c+=ireg.c"
       , "da800023:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.c=PBYTE(areg.c+ireg.c)"
       , "da800022:mem_ar:mem_ar:mem_ir"},
   {"MEMPB(areg.d)=reg.d,areg.d+=ireg.d"
       , "dac00023:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.d=PBYTE(areg.d+ireg.d)"
       , "dac00022:mem_ar:mem_ar:mem_ir"},
   {"MEMPW(areg.a+reg.a)=reg.a"
       , "da000053:mem_ar:rb:ra"},
   {"MEMPW(areg.b+reg.b)=reg.b"
       , "da400053:mem_ar:rb:ra"},
   {"MEMPW(areg.c+reg.c)=reg.c"
       , "da800053:mem_ar:rb:ra"},
   {"MEMPW(areg.d+reg.d)=reg.d"
       , "dac00053:mem_ar:rb:ra"},
   {"MEMPW(areg.a)=reg.a,areg.a+=ireg.a"
       , "da000013:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.a=PWORD(areg.a+ireg.a)"
       , "da000012:mem_ar:mem_ar:mem_ir"},
   {"MEMPW(areg.b)=reg.b,areg.b+=ireg.b"
       , "da400013:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.b=PWORD(areg.b+ireg.b)"
       , "da400012:mem_ar:mem_ar:mem_ir"},
   {"MEMPW(areg.c)=reg.c,areg.c+=ireg.c"
       , "da800013:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.c=PWORD(areg.c+ireg.c)"
       , "da800012:mem_ar:mem_ar:mem_ir"},
   {"MEMPW(areg.d)=reg.d,areg.d+=ireg.d"
       , "dac00013:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.d=PWORD(areg.d+ireg.d)"
       , "dac00012:mem_ar:mem_ar:mem_ir"},
   {"MEMPL(areg.a+reg.a)=reg.a"
       , "da000043:mem_ar:rb:ra"},
   {"MEMPL(areg.b+reg.b)=reg.b"
       , "da400043:mem_ar:rb:ra"},
   {"MEMPL(areg.c+reg.c)=reg.c"
       , "da800043:mem_ar:rb:ra"},
   {"MEMPL(areg.d+reg.d)=reg.d"
       , "dac00043:mem_ar:rb:ra"},
   {"MEMPL(areg.a)=reg.a,areg.a+=ireg.a"
       , "da000003:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.a=PLONG(areg.a+ireg.a)"
       , "da000002:mem_ar:mem_ar:mem_ir"},
   {"MEMPL(areg.b)=reg.b,areg.b+=ireg.b"
       , "da400003:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.b=PLONG(areg.b+ireg.b)"
       , "da400002:mem_ar:mem_ar:mem_ir"},
   {"MEMPL(areg.c)=reg.c,areg.c+=ireg.c"
       , "da800003:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.c=PLONG(areg.c+ireg.c)"
       , "da800002:mem_ar:mem_ar:mem_ir"},
   {"MEMPL(areg.d)=reg.d,areg.d+=ireg.d"
       , "dac00003:mem_ar:ra:mem_ar:mem_ir"},
   {"areg.d=PLONG(areg.d+ireg.d)"
       , "dac00002:mem_ar:mem_ar:mem_ir"},
   {"MEMPB(areg+reg)=reg"
       , "da00006b:mem_ar:rb:ra"},
   {"MEMPB(areg)=reg,areg+=ireg"
       , "da00002b:mem_ar:ra:mem_ar:mem_ir"},
   {"areg=PBYTE(areg+ireg)"
       , "da00002a:mem_ar:mem_ar:mem_ir"},
   {"MEMPW(areg+reg)=reg"
       , "da00005b:mem_ar:rb:ra"},
   {"MEMPW(areg)=reg,areg+=ireg"
       , "da00001b:mem_ar:ra:mem_ar:mem_ir"},
   {"areg=PWORD(areg+ireg)"
       , "da00001a:mem_ar:mem_ar:mem_ir"},
   {"MEMPL(areg+reg)=reg"
       , "da00004b:mem_ar:rb:ra"},
   {"MEMPL(areg)=reg,areg+=ireg"
       , "da00000b:mem_ar:ra:mem_ar:mem_ir"},
   {"areg=PLONG(areg+ireg)"
       , "da00000a:mem_ar:mem_ar:mem_ir"},

   // Op code = 219 (0xdb)
   {"MEML(lit)=reg.a"
       , "db000000:memlit:ra"},
   {"MEML(lit)=reg.b"
       , "db400000:memlit:ra"},
   {"MEML(lit)=reg.c"
       , "db800000:memlit:ra"},
   {"MEML(lit)=reg.d"
       , "dbc00000:memlit:ra"},
   {"MEMPL(lit)=reg"
       , "db000008:memlit:ra"},

   // Op code = 220 (0xdc)
   {"areg.a=reg.a"
       , "dc000000:ar_ldst:rb"},
   {"ireg.a=reg.a"
       , "dc020000:ir_ldst:rb"},
   {"areg.b=reg.b"
       , "dc400000:ar_ldst:rb"},
   {"ireg.b=reg.b"
       , "dc420000:ir_ldst:rb"},
   {"areg.c=reg.c"
       , "dc800000:ar_ldst:rb"},
   {"ireg.c=reg.c"
       , "dc820000:ir_ldst:rb"},
   {"areg.d=reg.d"
       , "dcc00000:ar_ldst:rb"},
   {"ireg.d=reg.d"
       , "dcc20000:ir_ldst:rb"},
   {"areg=reg"
       , "dc200000:ar_ldst:rb"},
   {"ireg=reg"
       , "dc220000:ir_ldst:rb"},

   // Op code = 221 (0xdd)
   {"reg.a=areg.a"
       , "dd000000:rw:ar_ldst"},
   {"reg.a=ireg.a"
       , "dd020000:rw:ir_ldst"},
   {"reg.b=areg.b"
       , "dd001000:rw:ar_ldst"},
   {"reg.b=ireg.b"
       , "dd021000:rw:ir_ldst"},
   {"reg.c=areg.c"
       , "dd002000:rw:ar_ldst"},
   {"reg.c=ireg.c"
       , "dd022000:rw:ir_ldst"},
   {"reg.d=areg.d"
       , "dd003000:rw:ar_ldst"},
   {"reg.d=ireg.d"
       , "dd023000:rw:ir_ldst"},
   {"reg=areg"
       , "dd000800:rw:ar_ldst"},
   {"reg=ireg"
       , "dd020800:rw:ir_ldst"},

   // Op code = 222 (0xde)
   {"reg.a=CMEML(reg.a)"
       , "de000041:mem_rm:rb"},
   {"reg.b=CMEML(reg.b)"
       , "de400041:mem_rm:rb"},
   {"reg.c=CMEML(reg.c)"
       , "de800041:mem_rm:rb"},
   {"reg.d=CMEML(reg.d)"
       , "dec00041:mem_rm:rb"},

   // Op code = 223 (0xdf)
   {"reg.a=CMEML(lit)"
       , "df000000:mem_rm:memlit"},
   {"reg.b=CMEML(lit)"
       , "df400000:mem_rm:memlit"},
   {"reg.c=CMEML(lit)"
       , "df800000:mem_rm:memlit"},
   {"reg.d=CMEML(lit)"
       , "dfc00000:mem_rm:memlit"},

   // Op code = 224 (0xe0)
   {"CMEML(reg.a)=reg.a"
       , "e0000041:rb:ra"},
   {"CMEML(reg.b)=reg.b"
       , "e0400041:rb:ra"},
   {"CMEML(reg.c)=reg.c"
       , "e0800041:rb:ra"},
   {"CMEML(reg.d)=reg.d"
       , "e0c00041:rb:ra"},

   // Op code = 225 (0xe1)
   {"CMEML(lit)=reg.a"
       , "e1000000:memlit:ra"},
   {"CMEML(lit)=reg.b"
       , "e1400000:memlit:ra"},
   {"CMEML(lit)=reg.c"
       , "e1800000:memlit:ra"},
   {"CMEML(lit)=reg.d"
       , "e1c00000:memlit:ra"},

   // Op code = 226 (0xe2)
   {"reg.a=XMEMB(areg.a+lit)"
       , "e2000061:mem_rm:mem_ar:memli_lit"},
   {"reg.a=XMEMB(areg.a)"
       , "e2000061:mem_rm:mem_ar"},
   {"reg.b=XMEMB(areg.b+lit)"
       , "e2400061:mem_rm:mem_ar:memli_lit"},
   {"reg.b=XMEMB(areg.b)"
       , "e2400061:mem_rm:mem_ar"},
   {"reg.c=XMEMB(areg.c+lit)"
       , "e2800061:mem_rm:mem_ar:memli_lit"},
   {"reg.c=XMEMB(areg.c)"
       , "e2800061:mem_rm:mem_ar"},
   {"reg.d=XMEMB(areg.d+lit)"
       , "e2c00061:mem_rm:mem_ar:memli_lit"},
   {"reg.d=XMEMB(areg.d)"
       , "e2c00061:mem_rm:mem_ar"},
   {"reg.a=XMEMB(areg.a),areg.a+=lit"
       , "e2000021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=XMEMB(areg.b),areg.b+=lit"
       , "e2400021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=XMEMB(areg.c),areg.c+=lit"
       , "e2800021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=XMEMB(areg.d),areg.d+=lit"
       , "e2c00021:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.a=XMEMW(areg.a+lit)"
       , "e2000051:mem_rm:mem_ar:memli_lit"},
   {"reg.a=XMEMW(areg.a)"
       , "e2000051:mem_rm:mem_ar"},
   {"reg.b=XMEMW(areg.b+lit)"
       , "e2400051:mem_rm:mem_ar:memli_lit"},
   {"reg.b=XMEMW(areg.b)"
       , "e2400051:mem_rm:mem_ar"},
   {"reg.c=XMEMW(areg.c+lit)"
       , "e2800051:mem_rm:mem_ar:memli_lit"},
   {"reg.c=XMEMW(areg.c)"
       , "e2800051:mem_rm:mem_ar"},
   {"reg.d=XMEMW(areg.d+lit)"
       , "e2c00051:mem_rm:mem_ar:memli_lit"},
   {"reg.d=XMEMW(areg.d)"
       , "e2c00051:mem_rm:mem_ar"},
   {"reg.a=XMEMW(areg.a),areg.a+=lit"
       , "e2000011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=XMEMW(areg.b),areg.b+=lit"
       , "e2400011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=XMEMW(areg.c),areg.c+=lit"
       , "e2800011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=XMEMW(areg.d),areg.d+=lit"
       , "e2c00011:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.a=XMEML(areg.a+lit)"
       , "e2000041:mem_rm:mem_ar:memli_lit"},
   {"reg.a=XMEML(areg.a)"
       , "e2000041:mem_rm:mem_ar"},
   {"reg.b=XMEML(areg.b+lit)"
       , "e2400041:mem_rm:mem_ar:memli_lit"},
   {"reg.b=XMEML(areg.b)"
       , "e2400041:mem_rm:mem_ar"},
   {"reg.c=XMEML(areg.c+lit)"
       , "e2800041:mem_rm:mem_ar:memli_lit"},
   {"reg.c=XMEML(areg.c)"
       , "e2800041:mem_rm:mem_ar"},
   {"reg.d=XMEML(areg.d+lit)"
       , "e2c00041:mem_rm:mem_ar:memli_lit"},
   {"reg.d=XMEML(areg.d)"
       , "e2c00041:mem_rm:mem_ar"},
   {"reg.a=XMEML(areg.a),areg.a+=lit"
       , "e2000001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.b=XMEML(areg.b),areg.b+=lit"
       , "e2400001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.c=XMEML(areg.c),areg.c+=lit"
       , "e2800001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg.d=XMEML(areg.d),areg.d+=lit"
       , "e2c00001:mem_rm:mem_ar:mem_ar:memli_lit"},
   {"reg=XMEML(areg+lit)"
       , "e2000049:mem_rm:mem_ar:memli_lit"},
   {"reg=XMEML(areg)"
       , "e2000049:mem_rm:mem_ar"},
   {"reg=XMEML(areg),areg+=lit"
       , "e2000009:mem_rm:mem_ar:mem_ar:memli_lit"},

   // Op code = 227 (0xe3)
   {"reg.a=XMEMB(areg.a+reg.a)"
       , "e3000061:mem_rm:mem_ar:rb"},
   {"reg.b=XMEMB(areg.b+reg.b)"
       , "e3400061:mem_rm:mem_ar:rb"},
   {"reg.c=XMEMB(areg.c+reg.c)"
       , "e3800061:mem_rm:mem_ar:rb"},
   {"reg.d=XMEMB(areg.d+reg.d)"
       , "e3c00061:mem_rm:mem_ar:rb"},
   {"reg.a=XMEMB(areg.a),areg.a+=ireg.a"
       , "e3000021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=XMEMB(areg.b),areg.b+=ireg.b"
       , "e3400021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=XMEMB(areg.c),areg.c+=ireg.c"
       , "e3800021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=XMEMB(areg.d),areg.d+=ireg.d"
       , "e3c00021:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.a=XMEMW(areg.a+reg.a)"
       , "e3000051:mem_rm:mem_ar:rb"},
   {"reg.b=XMEMW(areg.b+reg.b)"
       , "e3400051:mem_rm:mem_ar:rb"},
   {"reg.c=XMEMW(areg.c+reg.c)"
       , "e3800051:mem_rm:mem_ar:rb"},
   {"reg.d=XMEMW(areg.d+reg.d)"
       , "e3c00051:mem_rm:mem_ar:rb"},
   {"reg.a=XMEMW(areg.a),areg.a+=ireg.a"
       , "e3000011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=XMEMW(areg.b),areg.b+=ireg.b"
       , "e3400011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=XMEMW(areg.c),areg.c+=ireg.c"
       , "e3800011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=XMEMW(areg.d),areg.d+=ireg.d"
       , "e3c00011:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.a=XMEML(areg.a+reg.a)"
       , "e3000041:mem_rm:mem_ar:rb"},
   {"reg.b=XMEML(areg.b+reg.b)"
       , "e3400041:mem_rm:mem_ar:rb"},
   {"reg.c=XMEML(areg.c+reg.c)"
       , "e3800041:mem_rm:mem_ar:rb"},
   {"reg.d=XMEML(areg.d+reg.d)"
       , "e3c00041:mem_rm:mem_ar:rb"},
   {"reg.a=XMEML(areg.a),areg.a+=ireg.a"
       , "e3000001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.b=XMEML(areg.b),areg.b+=ireg.b"
       , "e3400001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.c=XMEML(areg.c),areg.c+=ireg.c"
       , "e3800001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg.d=XMEML(areg.d),areg.d+=ireg.d"
       , "e3c00001:mem_rm:mem_ar:mem_ar:mem_ir"},
   {"reg=XMEML(areg+reg)"
       , "e3000049:mem_rm:mem_ar:rb"},
   {"reg=XMEML(areg),areg+=ireg"
       , "e3000009:mem_rm:mem_ar:mem_ar:mem_ir"},

   // Op code = 228 (0xe4)
   {"XMEMB(areg.a+lit)=reg.a"
       , "e4000061:mem_ar:memli_lit:ra"},
   {"XMEMB(areg.a)=reg.a"
       , "e4000061:mem_ar:ra"},
   {"XMEMB(areg.b+lit)=reg.b"
       , "e4400061:mem_ar:memli_lit:ra"},
   {"XMEMB(areg.b)=reg.b"
       , "e4400061:mem_ar:ra"},
   {"XMEMB(areg.c+lit)=reg.c"
       , "e4800061:mem_ar:memli_lit:ra"},
   {"XMEMB(areg.c)=reg.c"
       , "e4800061:mem_ar:ra"},
   {"XMEMB(areg.d+lit)=reg.d"
       , "e4c00061:mem_ar:memli_lit:ra"},
   {"XMEMB(areg.d)=reg.d"
       , "e4c00061:mem_ar:ra"},
   {"XMEMB(areg.a)=reg.a,areg.a+=lit"
       , "e4000021:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEMB(areg.b)=reg.b,areg.b+=lit"
       , "e4400021:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEMB(areg.c)=reg.c,areg.c+=lit"
       , "e4800021:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEMB(areg.d)=reg.d,areg.d+=lit"
       , "e4c00021:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEMW(areg.a+lit)=reg.a"
       , "e4000051:mem_ar:memli_lit:ra"},
   {"XMEMW(areg.a)=reg.a"
       , "e4000051:mem_ar:ra"},
   {"XMEMW(areg.b+lit)=reg.b"
       , "e4400051:mem_ar:memli_lit:ra"},
   {"XMEMW(areg.b)=reg.b"
       , "e4400051:mem_ar:ra"},
   {"XMEMW(areg.c+lit)=reg.c"
       , "e4800051:mem_ar:memli_lit:ra"},
   {"XMEMW(areg.c)=reg.c"
       , "e4800051:mem_ar:ra"},
   {"XMEMW(areg.d+lit)=reg.d"
       , "e4c00051:mem_ar:memli_lit:ra"},
   {"XMEMW(areg.d)=reg.d"
       , "e4c00051:mem_ar:ra"},
   {"XMEMW(areg.a)=reg.a,areg.a+=lit"
       , "e4000011:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEMW(areg.b)=reg.b,areg.b+=lit"
       , "e4400011:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEMW(areg.c)=reg.c,areg.c+=lit"
       , "e4800011:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEMW(areg.d)=reg.d,areg.d+=lit"
       , "e4c00011:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEML(areg.a+lit)=reg.a"
       , "e4000041:mem_ar:memli_lit:ra"},
   {"XMEML(areg.a)=reg.a"
       , "e4000041:mem_ar:ra"},
   {"XMEML(areg.b+lit)=reg.b"
       , "e4400041:mem_ar:memli_lit:ra"},
   {"XMEML(areg.b)=reg.b"
       , "e4400041:mem_ar:ra"},
   {"XMEML(areg.c+lit)=reg.c"
       , "e4800041:mem_ar:memli_lit:ra"},
   {"XMEML(areg.c)=reg.c"
       , "e4800041:mem_ar:ra"},
   {"XMEML(areg.d+lit)=reg.d"
       , "e4c00041:mem_ar:memli_lit:ra"},
   {"XMEML(areg.d)=reg.d"
       , "e4c00041:mem_ar:ra"},
   {"XMEML(areg.a)=reg.a,areg.a+=lit"
       , "e4000001:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEML(areg.b)=reg.b,areg.b+=lit"
       , "e4400001:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEML(areg.c)=reg.c,areg.c+=lit"
       , "e4800001:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEML(areg.d)=reg.d,areg.d+=lit"
       , "e4c00001:mem_ar:ra:mem_ar:memli_lit"},
   {"XMEML(areg+lit)=reg"
       , "e4000049:mem_ar:memli_lit:ra"},
   {"XMEML(areg)=reg"
       , "e4000049:mem_ar:ra"},
   {"XMEML(areg)=reg,areg+=lit"
       , "e4000009:mem_ar:ra:mem_ar:memli_lit"},

   // Op code = 229 (0xe5)
   {"XMEMB(areg.a+reg.a)=reg.a"
       , "e5000061:mem_ar:rb:ra"},
   {"XMEMB(areg.b+reg.b)=reg.b"
       , "e5400061:mem_ar:rb:ra"},
   {"XMEMB(areg.c+reg.c)=reg.c"
       , "e5800061:mem_ar:rb:ra"},
   {"XMEMB(areg.d+reg.d)=reg.d"
       , "e5c00061:mem_ar:rb:ra"},
   {"XMEMB(areg.a)=reg.a,areg.a+=ireg.a"
       , "e5000021:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEMB(areg.b)=reg.b,areg.b+=ireg.b"
       , "e5400021:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEMB(areg.c)=reg.c,areg.c+=ireg.c"
       , "e5800021:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEMB(areg.d)=reg.d,areg.d+=ireg.d"
       , "e5c00021:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEMW(areg.a+reg.a)=reg.a"
       , "e5000051:mem_ar:rb:ra"},
   {"XMEMW(areg.b+reg.b)=reg.b"
       , "e5400051:mem_ar:rb:ra"},
   {"XMEMW(areg.c+reg.c)=reg.c"
       , "e5800051:mem_ar:rb:ra"},
   {"XMEMW(areg.d+reg.d)=reg.d"
       , "e5c00051:mem_ar:rb:ra"},
   {"XMEMW(areg.a)=reg.a,areg.a+=ireg.a"
       , "e5000011:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEMW(areg.b)=reg.b,areg.b+=ireg.b"
       , "e5400011:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEMW(areg.c)=reg.c,areg.c+=ireg.c"
       , "e5800011:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEMW(areg.d)=reg.d,areg.d+=ireg.d"
       , "e5c00011:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEML(areg.a+reg.a)=reg.a"
       , "e5000041:mem_ar:rb:ra"},
   {"XMEML(areg.b+reg.b)=reg.b"
       , "e5400041:mem_ar:rb:ra"},
   {"XMEML(areg.c+reg.c)=reg.c"
       , "e5800041:mem_ar:rb:ra"},
   {"XMEML(areg.d+reg.d)=reg.d"
       , "e5c00041:mem_ar:rb:ra"},
   {"XMEML(areg.a)=reg.a,areg.a+=ireg.a"
       , "e5000001:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEML(areg.b)=reg.b,areg.b+=ireg.b"
       , "e5400001:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEML(areg.c)=reg.c,areg.c+=ireg.c"
       , "e5800001:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEML(areg.d)=reg.d,areg.d+=ireg.d"
       , "e5c00001:mem_ar:ra:mem_ar:mem_ir"},
   {"XMEML(areg+reg)=reg"
       , "e5000049:mem_ar:rb:ra"},
   {"XMEML(areg)=reg,areg+=ireg"
       , "e5000009:mem_ar:ra:mem_ar:mem_ir"},

   // Op code = 230 (0xe6)
   {"RSVD"
       , "e6000000"},

   // Op code = 231 (0xe7)
   {"RSVD"
       , "e7000000"},

   // Op code = 232 (0xe8)
   {"RSVD"
       , "e8000000"},

   // Op code = 233 (0xe9)
   {"RSVD"
       , "e9000000"},

   // Op code = 234 (0xea)
   {"RSVD"
       , "ea000000"},

   // Op code = 235 (0xeb)
   {"RSVD"
       , "eb000000"},

   // Op code = 236 (0xec)
   {"RSVD"
       , "ec000000"},

   // Op code = 237 (0xed)
   {"RSVD"
       , "ed000000"},

   // Op code = 238 (0xee)
   {"RSVD"
       , "ee000000"},

   // Op code = 239 (0xef)
   {"RSVD"
       , "ef000000"},

   // Op code = 240 (0xf0)
   {"RSVD"
       , "f0000000"},

   // Op code = 241 (0xf1)
   {"RSVD"
       , "f1000000"},

   // Op code = 242 (0xf2)
   {"RSVD"
       , "f2000000"},

   // Op code = 243 (0xf3)
   {"RSVD"
       , "f3000000"},

   // Op code = 244 (0xf4)
   {"RSVD"
       , "f4000000"},

   // Op code = 245 (0xf5)
   {"RSVD"
       , "f5000000"},

   // Op code = 246 (0xf6)
   {"RSVD"
       , "f6000000"},

   // Op code = 247 (0xf7)
   {"RSVD"
       , "f7000000"},

   // Op code = 248 (0xf8)
   {"RSVD"
       , "f8000000"},

   // Op code = 249 (0xf9)
   {"RSVD"
       , "f9000000"},

   // Op code = 250 (0xfa)
   {"RSVD"
       , "fa000000"},

   // Op code = 251 (0xfb)
   {"RSVD"
       , "fb000000"},

   // Op code = 252 (0xfc)
   {"RSVD"
       , "fc000000"},

   // Op code = 253 (0xfd)
   {"RSVD"
       , "fd000000"},

   // Op code = 254 (0xfe)
   {"RSVD_254"
       , "fe000000"},

   // Op code = 255 (0xff)
   {"RSVD"
       , "ff000000"},

{0,0}
};

static hash_t amber_keywords[] = {
    {"ADDMB" , "1"},
    {"BYTE" , "1"},
    {"CALL" , "1"},
    {"CMEML" , "1"},
    {"CNTLD" , "1"},
    {"CONDEX" , "1"},
    {"CONDEX_S" , "1"},
    {"DP_NUM" , "1"},
    {"ENDCONDEX" , "1"},
    {"EXT" , "1"},
    {"EXT2" , "1"},
    {"EXT2_ALL" , "1"},
    {"EXTI" , "1"},
    {"EXTI2" , "1"},
    {"EXTL" , "1"},
    {"EXTR" , "1"},
    {"EXT_ALL" , "1"},
    {"EXT_INC" , "1"},
    {"EXT_INIT" , "1"},
    {"EXT_NXT" , "1"},
    {"EXT_POS" , "1"},
    {"EXT_SIGNED" , "1"},
    {"EXT_WIDTH" , "1"},
    {"GOTO" , "1"},
    {"HALT" , "1"},
    {"IF" , "1"},
    {"INS" , "1"},
    {"INS_ADD" , "1"},
    {"INS_ALL" , "1"},
    {"INS_EPOS" , "1"},
    {"INS_INC" , "1"},
    {"INS_INIT" , "1"},
    {"INS_IPOS" , "1"},
    {"INS_MODE" , "1"},
    {"INS_WIDTH" , "1"},
    {"LA" , "1"},
    {"LI" , "1"},
    {"LIT_HIGH" , "1"},
    {"LOAD_LA" , "1"},
    {"LONG" , "1"},
    {"MEMB" , "1"},
    {"MEML" , "1"},
    {"MEMPB" , "1"},
    {"MEMPL" , "1"},
    {"MEMPW" , "1"},
    {"MEMSL" , "1"},
    {"MEMW" , "1"},
    {"NOP" , "1"},
    {"PBYTE" , "1"},
    {"PLONG" , "1"},
    {"PWORD" , "1"},
    {"QCOMP" , "1"},
    {"QCOMPB" , "1"},
    {"RETURN" , "1"},
    {"RETURNI" , "1"},
    {"RSVD" , "1"},
    {"SELECT" , "1"},
    {"SPECREG" , "1"},
    {"TRAP" , "1"},
    {"WORD" , "1"},
    {"XMEMB" , "1"},
    {"XMEML" , "1"},
    {"XMEMW" , "1"},
  {0,0}
};
