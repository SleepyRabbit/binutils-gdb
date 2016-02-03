/* BFD back-end for Quatro flexRisc Elf32 files.
   Copyright 2007-2009 Zoran, Inc.
   Free Software Foundation, Inc.
   Written by Brian Dodge <brian.dodge@csr.com>

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

/* This flexrisc version was originally written be Brian Dodge
*/
#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "bfdlink.h"
#include "elf-bfd.h"
#include "elf/flexrisc.h"

//#define VERBOSE_RELOCS 1

#define BADMAG(x) FLEXRISCBADMAG(x)

/* flexrisc_reloc_map array maps BFD relocation enum into a CRGAS relocation type.  */

typedef struct tag_FLEXRISC_reloc_map
{
	bfd_reloc_code_real_type bfd_reloc_enum; /* BFD relocation enum.  */
	unsigned int flexrisc_reloc_type;        /* FLEXRISC relocation type.  */
} FRELOCMAP, *PFRELOCMAP;

static const FRELOCMAP flexrisc_reloc_map[R_FLEXRISC_MAX] =
{
	{BFD_RELOC_NONE,				R_FLEXRISC_NONE},
	{BFD_RELOC_8,					R_FLEXRISC_8},
	{BFD_RELOC_8_PCREL,				R_FLEXRISC_8_PCREL},
	{BFD_RELOC_16,					R_FLEXRISC_16},
	{BFD_RELOC_QUATRO_16,			R_FLEXRISC_16L},
	{BFD_RELOC_QUATRO_MV16,			R_FLEXRISC_16H},
	{BFD_RELOC_FLEXRISC_BITN,		R_FLEXRISC_3},
};

static bfd_reloc_status_type elf32_flexrisc_reloc
PARAMS ((bfd *, arelent *, asymbol *, PTR, asection *, bfd *, char **));

// this table needs to be ordered the SAME way as the enumeration in 
// include/elf/flexrisc.h (see reloc map above to get order too)
//
static reloc_howto_type flexrisc_elf_howto_table[] =
{
	HOWTO (R_FLEXRISC_NONE,           /* type */
			0,                        /* rightshift */
			2,                        /* size */
			32,                       /* bitsize */
			0,                    /* pc_relative */
			0,                        /* bitpos */
			complain_overflow_dont,   /* complain_on_overflow */
			elf32_flexrisc_reloc,     /* special_function */
			"R_FLEXRISC_NONE",        /* name */
			0,                    /* partial_inplace */
			0,                        /* src_mask */
			0,                        /* dst_mask */
			0),                   /* pcrel_offset */

	HOWTO (R_FLEXRISC_8,			  /* type */
			0, 						  /* rightshift */
			0,						  /* size (0 = byte, 1 = short, 2 = long) */
			8,						  /* bitsize */
			0,						  /* pc_relative */
			0,						  /* bitpos */
			0,						  /* dont complain_on_overflow */
			NULL,					  /* special_function */
			"flexrisc_reloc_8",       /* name */
			0,						  /* partial_inplace */
			0x000000FF,				  /* src_mask */
			0x000000FF,				  /* dst_mask */
			0),					  /* pcrel_offset */
			
	HOWTO (R_FLEXRISC_8_PCREL,		  /* type */
			0, 						  /* rightshift */
			0,						  /* size (0 = byte, 1 = short, 2 = long) */
			8,						  /* bitsize */
			1,						  /* pc_relative */
			0,						  /* bitpos */
			0,						  /* dont complain_on_overflow */
			NULL,					  /* special_function */
			"flexrisc_reloc_8_pcrel", /* name */
			0,						  /* partial_inplace */
			0x000000FF,				  /* src_mask */
			0x000000FF,				  /* dst_mask */
			1),					  /* pcrel_offset */
			
    HOWTO (R_FLEXRISC_16, /* type */
			0,						  /* rightshift */
			0,						  /* size (0 = byte, 1 = short, 2 = long) */
			16,						  /* bitsize */
			0,						  /* pc_relative */
			0,						  /* bitpos */
			0,						  /* dont complain_on_overflow */
			NULL,					  /* special_function */
			"flexrisc_reloc_16",	  /* name */
			0,						  /* partial_inplace */
			0x0000FFFF,				  /* src_mask */
			0x0000FFFF,				  /* dst_mask */
			0),						  /* pcrel_offset */

    HOWTO (R_FLEXRISC_16L, /* type */
			0,						  /* rightshift */
			0,						  /* size (0 = byte, 1 = short, 2 = long) */
			8,						  /* bitsize */
			0,						  /* pc_relative */
			0,						  /* bitpos */
			0,						  /* dont complain_on_overflow */
			NULL,					  /* special_function */
			"flexrisc_reloc_16_lowbyte",	  /* name */
			0,						  /* partial_inplace */
			0x000000FF,				  /* src_mask */
			0x000000FF,				  /* dst_mask */
			0),						  /* pcrel_offset */

    HOWTO (R_FLEXRISC_16H, /* type */
			8,						  /* rightshift */
			0,						  /* size (0 = byte, 1 = short, 2 = long) */
			8,						  /* bitsize */
			0,						  /* pc_relative */
			0,						  /* bitpos */
			0,						  /* dont complain_on_overflow */
			NULL,					  /* special_function */
			"flexrisc_reloc_16_highbyte",	  /* name */
			0,						  /* partial_inplace */
			0x0000FF00,				  /* src_mask */
			0x000000FF,				  /* dst_mask */
			0),						  /* pcrel_offset */

	HOWTO (R_FLEXRISC_3,			  /* type */
			0, 						  /* rightshift */
			0,						  /* size (0 = byte, 1 = short, 2 = long) */
			3,						  /* bitsize */
			0,						  /* pc_relative */
			0,						  /* bitpos */
			0,						  /* dont complain_on_overflow */
			NULL,					  /* special_function */
			"flexrisc_reloc_bitn",    /* name */
			0,						  /* partial_inplace */
			0x00000007,				  /* src_mask */
			0x00000007,				  /* dst_mask */
			0),					  /* pcrel_offset */
			
};

/* Retrieve a howto ptr using an internal relocation entry. 
 */
static void
elf_flexrisc_info_to_howto(bfd *abfd ATTRIBUTE_UNUSED, arelent *cache_ptr,
                        Elf_Internal_Rela *dst)
{
	unsigned int r_type = ELF32_R_TYPE (dst->r_info);
	
	BFD_ASSERT (r_type < (unsigned int) R_FLEXRISC_MAX);
	cache_ptr->howto = &flexrisc_elf_howto_table[r_type];
}


static bfd_reloc_status_type
elf32_flexrisc_reloc (
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
	
	//unsigned long mach = bfd_get_mach(abfd);
	
	*error_message = "";
	relocation = 0;
	
	
	/* If this is an undefined symbol, return error.  */
	if (
			symbol->section == &bfd_und_section
		&& (symbol->flags & BSF_WEAK) == 0
	)
	{
		return output_bfd ? bfd_reloc_continue : bfd_reloc_undefined;
	}
	if (
			(output_bfd != (bfd *) NULL)
		&&
			(howto->partial_inplace == 0)
	)
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
	where_we_are = input_section->output_offset + reloc_entry->address;
	
	relocation = where_we_want_to_go - where_we_are;
	
#ifdef VERBOSE_RELOCS
	fprintf (stderr, "\n\n%s\t%lx\t%lx\t%lx\n", 
		symbol->name, where_we_are,  where_we_want_to_go, relocation);		
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

/* Perform a relocation as part of a final link.  */

static bfd_reloc_status_type
flexrisc_elf_final_link_relocate (
	reloc_howto_type *howto,
	bfd *input_bfd ATTRIBUTE_UNUSED,
	bfd *output_bfd ATTRIBUTE_UNUSED,
	asection *input_section,
	bfd_byte *contents ATTRIBUTE_UNUSED,
	bfd_vma offset,
	bfd_vma Rvalue,
	bfd_vma addend,
	struct bfd_link_info *info ATTRIBUTE_UNUSED,
	asection *sec ATTRIBUTE_UNUSED,
	int is_local ATTRIBUTE_UNUSED)
{
	unsigned short r_type = howto->type;
	bfd_byte *hit_data = contents + offset;
	
	unsigned char rx;
	unsigned short ax;
	
	//unsigned long mach = bfd_get_mach(input_bfd);
	if (howto->pc_relative)
	{		
		/* Subtract the address of the section containing the location.	*/
		/*
		fprintf(stderr, "Rval=%08lX, soff=%08lx\n",
				Rvalue, (input_section->output_section->vma
							+ input_section->output_offset));
		*/				
		Rvalue -= (input_section->output_section->vma
				+ input_section->output_offset);

		/* Subtract the position of the location within the section. */
		Rvalue -= offset;
		Rvalue -= addend;

		#ifdef VERBOSE_RELOCS
		fprintf(stderr, "pcrel: ");
		#endif
	}
	else
	{
		#ifdef VERBOSE_RELOCS
		fprintf(stderr, "absolute: ");
		#endif
		if(addend)
			Rvalue = 0; /* addend should be all that's needed */
	}
	
	switch (r_type)
	{
	case R_FLEXRISC_NONE:
		return bfd_reloc_ok;
		
	case R_FLEXRISC_8_PCREL:
		
#ifdef VERBOSE_RELOCS
		fprintf(stderr, "\t8 pcrel Rvalue=%08lX, offset=%08lX  addend=%08lX\n", Rvalue, offset, addend);
#endif	
		/* Add in supplied addend.  */
		Rvalue += addend;
		
		/* sub a byte since val is relative to after frag byte not opcode */
		Rvalue--;
		
		/* Drop unwanted bits from the value we are relocating to.  */
		Rvalue >>= (bfd_vma) howto->rightshift;
		
		/* Apply dst_mask to select only relocatable part of the insn.  */
		Rvalue &= howto->dst_mask;
		
		/* get 8 bits at relocation target */
		rx = bfd_get_8 (input_bfd, hit_data);
		
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t<=0x%02X\n", rx);
#endif		
		/* Mask off address portion */
		rx &= ~howto->dst_mask;
		
		/* put in relocated address */
		rx |= Rvalue;
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t=>0x%02X\n\n", rx);
#endif		
		bfd_put_8(input_bfd, rx, hit_data);
		break;
		
	case R_FLEXRISC_8:
	case R_FLEXRISC_3:

#ifdef VERBOSE_RELOCS
		fprintf(stderr, "\t8 immed Rvalue=%08lX, offset=%08lX  addend=%08lX\n", Rvalue, offset, addend);
#endif
		/* Add in supplied addend.  */
		Rvalue += addend;
		
		/* Drop unwanted bits from the value we are relocating to.  */
		Rvalue >>= (bfd_vma) howto->rightshift;
		
		/* Apply dst_mask to select only relocatable part of the insn.  */
		Rvalue &= howto->dst_mask;
		
		/* get 8 bits at relocation target */
		rx = bfd_get_8 (input_bfd, hit_data);
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t<=0x%02X\n", rx);
#endif		
		/* Mask off address portion */
		rx &= ~howto->dst_mask;
		
		/* put in relocated address */
		rx |= Rvalue;
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t=>0x%02X\n\n", rx);
#endif		
		bfd_put_8(input_bfd, rx, hit_data);
		break;
		
	case R_FLEXRISC_16:
		
#ifdef VERBOSE_RELOCS
		fprintf(stderr, "\t16 abs Rvalue=%08lX, offset=%08lX  addend=%08lX\n", Rvalue, offset, addend);
#endif
		Rvalue += addend;
		
		/* Drop unwanted bits from the value we are relocating to.  */
		Rvalue >>= (bfd_vma) howto->rightshift;
		
		/* Apply dst_mask to select only relocatable part of the insn.  */
		Rvalue &= howto->dst_mask;
		
		/* get 16 bits at relocation target */
		ax = bfd_get_16 (input_bfd, hit_data);
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t<=0x%04X\n", ax);
#endif		
		/* Mask off address portion */
		ax &= ~howto->dst_mask;
		
		/* put in relocated address */
		ax |= Rvalue;
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t=>0x%04X\n\n", ax);
#endif		
		bfd_put_16(input_bfd, ax, hit_data);
		break;
		
	case R_FLEXRISC_16L:
	case R_FLEXRISC_16H:
		
#ifdef VERBOSE_RELOCS
		fprintf(stderr, "\t16->8 abs Rvalue=%08lX, offset=%08lX  addend=%08lX\n", Rvalue, offset, addend);
#endif
		// fix done, just add section relocation
		Rvalue += addend;
		
		/* Drop unwanted bits from the value we are relocating to.  */
		Rvalue >>= (bfd_vma) howto->rightshift;
		
		/* Apply dst_mask to select only relocatable part of the insn.  */
		Rvalue &= howto->dst_mask;
		
		/* get 8 bits at relocation target */
		ax = bfd_get_8 (input_bfd, hit_data);
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t<=0x%02X\n", ax);
#endif		
		/* Mask off address portion */
		ax &= ~howto->dst_mask;
		
		/* put in relocated address */
		ax |= Rvalue;
#ifdef VERBOSE_RELOCS
		fprintf (stderr, "\t=>0x%02X\n\n", ax);
#endif		
		bfd_put_8(input_bfd, ax, hit_data);
		break;

	default:
		break;
	}
	return bfd_reloc_ok;
}


/* Relocate a flexrisc ELF section.  */

static bfd_boolean
elf32_flexrisc_relocate_section (bfd *output_bfd, struct bfd_link_info *info,
                            bfd *input_bfd, asection *input_section,
                            bfd_byte *contents, Elf_Internal_Rela *relocs,
                            Elf_Internal_Sym *local_syms,
                            asection **local_sections)
{
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  Elf_Internal_Rela *rel, *relend;

  if (info->relocatable)
    return 1;

  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (input_bfd);

  rel = relocs;
  relend = relocs + input_section->reloc_count;
  for (; rel < relend; rel++)
    {
      int r_type;
      reloc_howto_type *howto;
      unsigned long r_symndx;
      Elf_Internal_Sym *sym;
      asection *sec;
      struct elf_link_hash_entry *h;
      bfd_vma relocation;
      bfd_reloc_status_type r;

      r_symndx = ELF32_R_SYM (rel->r_info);
      r_type = ELF32_R_TYPE (rel->r_info);
      howto = flexrisc_elf_howto_table + (r_type);

      h = NULL;
      sym = NULL;
      sec = NULL;
      if (r_symndx < symtab_hdr->sh_info)
        {
          sym = local_syms + r_symndx;
          sec = local_sections[r_symndx];
          relocation = _bfd_elf_rela_local_sym (output_bfd, sym, &sec, rel);
        }
      else
        {
          bfd_boolean unresolved_reloc, warned;

          RELOC_FOR_GLOBAL_SYMBOL (info, input_bfd, input_section, rel,
                                   r_symndx, symtab_hdr, sym_hashes,
                                   h, sec, relocation,
                                   unresolved_reloc, warned);
        }
      r = flexrisc_elf_final_link_relocate (howto, input_bfd, output_bfd,
                                        input_section,
                                        contents, rel->r_offset,
                                        relocation, rel->r_addend,
                                        info, sec, h == NULL);

      if (r != bfd_reloc_ok)
        {
          const char *name;
          const char *msg = NULL;

          if (h != NULL)
            name = h->root.root.string;
          else
            {
              name = (bfd_elf_string_from_elf_section
                      (input_bfd, symtab_hdr->sh_link, sym->st_name));
              if (name == NULL || *name == '\0')
                name = bfd_section_name (input_bfd, sec);
            }

          switch (r)
            {
             case bfd_reloc_overflow:
               if (!((*info->callbacks->reloc_overflow)
                     (info, (h ? &h->root : NULL), name, howto->name,
                      (bfd_vma) 0, input_bfd, input_section,
                      rel->r_offset)))
                 return 0;
               break;

             case bfd_reloc_undefined:
               if (!((*info->callbacks->undefined_symbol)
                     (info, name, input_bfd, input_section,
                      rel->r_offset, 1)))
                 return 0;
               break;

             case bfd_reloc_outofrange:
               msg = _("internal error: out of range error");
               goto common_error;

             case bfd_reloc_notsupported:
               msg = _("internal error: unsupported relocation error");
               goto common_error;

             case bfd_reloc_dangerous:
               msg = _("internal error: dangerous error");
               goto common_error;

             default:
               msg = _("internal error: unknown error");
               /* Fall through.  */

             common_error:
               if (!((*info->callbacks->warning)
                     (info, msg, name, input_bfd, input_section,
                      rel->r_offset)))
                 return 0;
               break;
            }
        }
    }
  return 1;
}

/* This is a version of bfd_generic_get_relocated_section_contents
   which uses elf32_flexrisc_relocate_section.  */

static bfd_byte *
elf32_flexrisc_get_relocated_section_contents (bfd *output_bfd,
                                           struct bfd_link_info *link_info,
                                           struct bfd_link_order *link_order,
                                           bfd_byte *data,
                                           bfd_boolean relocatable,
                                           asymbol **symbols)
{
  Elf_Internal_Shdr *symtab_hdr;
  asection *input_section = link_order->u.indirect.section;
  bfd *input_bfd = input_section->owner;
  asection **sections = NULL;
  Elf_Internal_Rela *internal_relocs = NULL;
  Elf_Internal_Sym *isymbuf = NULL;

  /* We only need to handle the case of relaxing, or of having a
     particular set of section contents, specially.  */
  if (relocatable
      || elf_section_data (input_section)->this_hdr.contents == NULL)
    return bfd_generic_get_relocated_section_contents (output_bfd, link_info,
                                                       link_order, data,
                                                       relocatable,
                                                       symbols);

  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;

  memcpy (data, elf_section_data (input_section)->this_hdr.contents,
          (size_t) input_section->size);

  if ((input_section->flags & SEC_RELOC) != 0
      && input_section->reloc_count > 0)
    {
      Elf_Internal_Sym *isym;
      Elf_Internal_Sym *isymend;
      asection **secpp;
      bfd_size_type amt;

      internal_relocs = _bfd_elf_link_read_relocs (input_bfd, input_section,
						   NULL, NULL, 0);
      if (internal_relocs == NULL)
        goto error_return;

      if (symtab_hdr->sh_info != 0)
        {
          isymbuf = (Elf_Internal_Sym *) symtab_hdr->contents;
          if (isymbuf == NULL)
            isymbuf = bfd_elf_get_elf_syms (input_bfd, symtab_hdr,
                                            symtab_hdr->sh_info, 0,
                                            NULL, NULL, NULL);
          if (isymbuf == NULL)
            goto error_return;
        }

      amt = symtab_hdr->sh_info;
      amt *= sizeof (asection *);
      sections = bfd_malloc (amt);
      if (sections == NULL && amt != 0)
        goto error_return;

      isymend = isymbuf + symtab_hdr->sh_info;
      for (isym = isymbuf, secpp = sections; isym < isymend; ++isym, ++secpp)
        {
          asection *isec;

          if (isym->st_shndx == SHN_UNDEF)
            isec = bfd_und_section_ptr;
          else if (isym->st_shndx == SHN_ABS)
            isec = bfd_abs_section_ptr;
          else if (isym->st_shndx == SHN_COMMON)
            isec = bfd_com_section_ptr;
          else
            isec = bfd_section_from_elf_index (input_bfd, isym->st_shndx);

          *secpp = isec;
        }

      if (! elf32_flexrisc_relocate_section (output_bfd, link_info, input_bfd,
                                     input_section, data, internal_relocs,
                                     isymbuf, sections))
        goto error_return;

      if (sections != NULL)
        free (sections);
      if (isymbuf != NULL
          && symtab_hdr->contents != (unsigned char *) isymbuf)
        free (isymbuf);
      if (elf_section_data (input_section)->relocs != internal_relocs)
        free (internal_relocs);
    }

  return data;

 error_return:
  if (sections != NULL)
    free (sections);
  if (isymbuf != NULL
      && symtab_hdr->contents != (unsigned char *) isymbuf)
    free (isymbuf);
  if (internal_relocs != NULL
      && elf_section_data (input_section)->relocs != internal_relocs)
    free (internal_relocs);
  return NULL;
}


static reloc_howto_type *
elf32_flexrisc_reloc_type_lookup (bfd *abfd  ATTRIBUTE_UNUSED, bfd_reloc_code_real_type code)
{
	unsigned int i;
	
	for(i = 0; i < R_FLEXRISC_MAX; i++)
		if(code == flexrisc_reloc_map[i].bfd_reloc_enum)
			return &flexrisc_elf_howto_table[flexrisc_reloc_map[i].flexrisc_reloc_type];
	_bfd_error_handler ("Unsupported FLEXRISC relocation type: 0x%x\n", code);
	return NULL;
}

static reloc_howto_type *
elf32_flexrisc_reloc_name_lookup (bfd *abfd  ATTRIBUTE_UNUSED, const char* r_name)
{
	unsigned int i;

	for(i = 0; sizeof(flexrisc_elf_howto_table)/sizeof(reloc_howto_type); i++)
	{
		if(
				flexrisc_elf_howto_table[i].name != NULL
			&&	strcasecmp (flexrisc_elf_howto_table[i].name, r_name) == 0
		)
		{
			return flexrisc_elf_howto_table + i;
		}
	}
	return NULL;
}

#if 1 // little-endian
#define TARGET_LITTLE_SYM			bfd_elf32_flexrisc_vec
#define TARGET_LITTLE_NAME			"elf32-flexrisc"
#else
#define TARGET_BIG_SYM			bfd_elf32_flexrisc_vec
#define TARGET_BIG_NAME			"elf32-flexrisc"
#endif
#define ELF_ARCH				bfd_arch_flexrisc
#define ELF_MACHINE_CODE		EM_FLEXRISC
#define ELF_MAXPAGESIZE			0x1000
//#define elf_symbol_leading_char           '_'

#define bfd_elf32_bfd_reloc_type_lookup	elf32_flexrisc_reloc_type_lookup
#define bfd_elf32_bfd_reloc_name_lookup	elf32_flexrisc_reloc_name_lookup
#define elf_info_to_howto				elf_flexrisc_info_to_howto
#define elf_info_to_howto_rel			0 
										//elf_flexrisc_info_to_howto_rel

#define bfd_elf32_bfd_get_relocated_section_contents \
    		                            elf32_flexrisc_get_relocated_section_contents
#define elf_backend_relocate_section    elf32_flexrisc_relocate_section

#define elf_backend_can_gc_sections     1
#define elf_backend_rela_normal         1
											
#include "elf32-target.h"
