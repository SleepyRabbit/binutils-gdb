/* Low level interface for debugging AIX 4.3+ pthreads.

   Copyright 1999, 2000, 2002 Free Software Foundation, Inc.
   Written by Nick Duffek <nsd@redhat.com>.

   This file is part of GDB.

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
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */


/* This module uses the libpthdebug.a library provided by AIX 4.3+ for
   debugging pthread applications.

   Some name prefix conventions:
     pthdb_	provided by libpthdebug.a
     pdc_	callbacks that this module provides to libpthdebug.a
     pd_	variables or functions interfacing with libpthdebug.a

   libpthdebug peculiarities:

     - pthdb_ptid_pthread() is prototyped in <sys/pthdebug.h>, but it's not
       documented, and after several calls it stops working and causes other
       libpthdebug functions to fail.

     - pthdb_tid_pthread() doesn't always work after pthdb_session_update(),
       but it does work after cycling through all threads using
       pthdb_pthread().

     */

#include "defs.h"
#include "gdbthread.h"
#include "target.h"
#include "inferior.h"
#include "regcache.h"

#if 0
#include "coff/internal.h"	/* for libcoff.h */
#include "bfd/libcoff.h"	/* for xcoff_data */
#endif

#include <procinfo.h>
#include <sys/types.h>
#include <sys/ptrace.h>
#include <sys/reg.h>
#if 0
#include <pthread.h>
#endif
#include <sched.h>
#include <sys/pthdebug.h>

/* Whether to emit debugging output. */

#define DEBUG 0

/* Default debugging output file, overridden by envvar UWTHR_DEBUG. */

#define DEBUG_FILE "/dev/tty"

/* #if DEBUG, write string S to the debugging output channel. */

#if !DEBUG
# define DBG(fmt_and_args)
# define DBG2(fmt_and_args)
#else
# define DBG(fmt_and_args) dbg fmt_and_args
# define DBG2(fmt_and_args) dbg fmt_and_args
#endif

/* in AIX 5.1, functions use pthdb_tid_t instead of tid_t */
#ifndef PTHDB_VERSION_3
#define pthdb_tid_t	tid_t
#endif

/* Return whether to treat PID as a debuggable thread id. */

#define PD_TID(ptid)	(pd_active && ptid_get_tid (ptid) != 0)

/* Build a thread ptid.  */
#define BUILD_THREAD(TID, PID) ptid_build (PID, 0, TID)

/* Build and lwp ptid.  */
#define BUILD_LWP(LWP, PID) MERGEPID (PID, LWP)

/* Call error() with a message indicating that libpthdebug FUNC failed with
   STATUS. */

#define PD_ERROR(func, status) \
  error ("aix-thread: %s returned %s", func, pd_status2str (status))

/* pthdb_user_t value that we pass to pthdb functions.  0 causes
   PTHDB_BAD_USER errors, so use 1. */

#define PD_USER	1

/* Success and failure values returned by pthdb callbacks. */

#define PDC_SUCCESS	PTHDB_SUCCESS
#define PDC_FAILURE	PTHDB_CALLBACK

/* Convert composite process/thread inferior_ptid to a process id, evaluate
   base_ops function CALL, and then restore inferior_ptid. */

#define CALL_BASE(call)					\
  do {							\
    struct cleanup *cleanup = save_inferior_ptid ();	\
    inferior_ptid = pid_to_ptid (PIDGET (inferior_ptid)); \
    call;						\
    do_cleanups (cleanup);				\
  } while (0)

/* Private data attached to each element in GDB's thread list. */

struct private_thread_info {
  pthdb_pthread_t pdtid;	/* thread's libpthdebug id */
  pthdb_tid_t tid;			/* kernel thread id */
};

/* Information about a thread of which libpthdebug is aware. */

struct pd_thread {
  pthdb_pthread_t pdtid;
  pthread_t pthid;
  pthdb_tid_t tid;
};

/* This module's target-specific operations, active while pd_able is true. */

static struct target_ops ops;

/* Copy of the target over which ops is pushed.  This is
   more convenient than a pointer to child_ops or core_ops, because
   they lack current_target's default callbacks. */

static struct target_ops base_ops;

/* Address of the function that libpthread will call when libpthdebug is
   ready to be initialized. */

static CORE_ADDR pd_brk_addr;

/* Whether the current application is debuggable by pthdb. */

static int pd_able = 0;

/* Whether a threaded application is being debugged. */

static int pd_active = 0;

/* Whether the current architecture is 64-bit.  Only valid when pd_able is
   true. */

static int arch64;

/* Saved pointer to previous owner of target_new_objfile_hook. */

static void (*target_new_objfile_chain)(struct objfile *);

/* Forward declarations for pthdb callbacks. */

static int pdc_symbol_addrs (pthdb_user_t, pthdb_symbol_t *, int);
static int pdc_read_data (pthdb_user_t, void *, pthdb_addr_t, size_t);
static int pdc_write_data (pthdb_user_t, void *, pthdb_addr_t, size_t);
static int pdc_read_regs (pthdb_user_t user, pthdb_tid_t tid,
			  unsigned long long flags, pthdb_context_t *context);
static int pdc_write_regs (pthdb_user_t user, pthdb_tid_t tid,
			   unsigned long long flags, pthdb_context_t *context);
static int pdc_alloc (pthdb_user_t, size_t, void **);
static int pdc_realloc (pthdb_user_t, void *, size_t, void **);
static int pdc_dealloc (pthdb_user_t, void *);

/* pthdb callbacks. */

static pthdb_callbacks_t pd_callbacks = {
  pdc_symbol_addrs,
  pdc_read_data,
  pdc_write_data,
  pdc_read_regs,
  pdc_write_regs,
  pdc_alloc,
  pdc_realloc,
  pdc_dealloc,
  NULL
};

/* Current pthdb session. */

static pthdb_session_t pd_session;

#if DEBUG
/* DBG() helper: if printf-style FMT is non-null, format it with args and
   display the result on the debugging output channel. */

static void
dbg (char *fmt, ...)
{
  static int fd = -1, len;
  va_list args;
  char buf[1024];
  char *path;

  if (!fmt)
    return;

  if (fd < 0)
    {
      path = getenv ("UWTHR_DEBUG");
      if (!path)
	path = DEBUG_FILE;
      if ((fd = open (path, O_WRONLY | O_CREAT | O_TRUNC, 0664)) < 0)
	error ("can't open %s\n", path);
    }

  va_start (args, fmt);
  vsprintf (buf, fmt, args);
  va_end (args);

  len = strlen (buf);
  buf[len] = '\n';
  (void)write (fd, buf, len + 1);
}
#endif   /* DEBUG */

/* Return a printable representation of pthdebug function return STATUS. */

static char *
pd_status2str (int status)
{
  switch (status)
    {
    case PTHDB_SUCCESS:		return "SUCCESS";
    case PTHDB_NOSYS:		return "NOSYS";
    case PTHDB_NOTSUP:		return "NOTSUP";
    case PTHDB_BAD_VERSION:	return "BAD_VERSION";
    case PTHDB_BAD_USER:	return "BAD_USER";
    case PTHDB_BAD_SESSION:	return "BAD_SESSION";
    case PTHDB_BAD_MODE:	return "BAD_MODE";
    case PTHDB_BAD_FLAGS:	return "BAD_FLAGS";
    case PTHDB_BAD_CALLBACK:	return "BAD_CALLBACK";
    case PTHDB_BAD_POINTER:	return "BAD_POINTER";
    case PTHDB_BAD_CMD:		return "BAD_CMD";
    case PTHDB_BAD_PTHREAD:	return "BAD_PTHREAD";
    case PTHDB_BAD_ATTR:	return "BAD_ATTR";
    case PTHDB_BAD_MUTEX:	return "BAD_MUTEX";
    case PTHDB_BAD_MUTEXATTR:	return "BAD_MUTEXATTR";
    case PTHDB_BAD_COND:	return "BAD_COND";
    case PTHDB_BAD_CONDATTR:	return "BAD_CONDATTR";
    case PTHDB_BAD_RWLOCK:	return "BAD_RWLOCK";
    case PTHDB_BAD_RWLOCKATTR:	return "BAD_RWLOCKATTR";
    case PTHDB_BAD_KEY:		return "BAD_KEY";
    case PTHDB_BAD_PTID:	return "BAD_PTID";
    case PTHDB_BAD_TID:		return "BAD_TID";
    case PTHDB_CALLBACK:	return "CALLBACK";
    case PTHDB_CONTEXT:		return "CONTEXT";
    case PTHDB_HELD:		return "HELD";
    case PTHDB_NOT_HELD:	return "NOT_HELD";
    case PTHDB_MEMORY:		return "MEMORY";
    case PTHDB_NOT_PTHREADED:	return "NOT_PTHREADED";
    case PTHDB_SYMBOL:		return "SYMBOL";
    case PTHDB_NOT_AVAIL:	return "NOT_AVAIL";
    case PTHDB_INTERNAL:	return "INTERNAL";
    default:			return "UNKNOWN";
    }
}

/* A call to ptrace(REQ, ID, ...) just returned RET.  Check for exceptional
   conditions and either return nonlocally or else return 1 for success and 0
   for failure. */

static int
ptrace_check (int req, int id, int ret)
{
  if (ret == 0 && !errno)
    return 1;

  /* According to ptrace(2), ptrace may fail with EPERM if "the Identifier
     parameter corresponds to a kernel thread which is stopped in kernel mode
     and whose computational state cannot be read or written."  This happens
     quite often with register reads. */

  switch (req)
    {
    case PTT_READ_GPRS:
    case PTT_READ_FPRS:
    case PTT_READ_SPRS:
      if (ret == -1 && errno == EPERM)
	goto strange;
      break;
    }
  error ("aix-thread: ptrace (%d, %d) returned %d (errno = %d %s)",
	 req, id, ret, errno, strerror (errno));

 strange:
  DBG2(("ptrace (%d, %d) = %d (errno = %d)", req, id, ret, errno));
  return ret == -1 ? 0 : 1;
}

/* Call ptracex(REQ, ID, ADDR, DATA, BUF).  Return success. */

static int
ptrace64aix (int req, int id, long long addr, int data, int *buf)
{
  errno = 0;
  return ptrace_check (req, id, ptracex (req, id, addr, data, buf));
}

/* Call ptrace(REQ, ID, ADDR, DATA, BUF).  Return success. */

static int
ptrace32 (int req, int id, int *addr, int data, int *buf)
{
  errno = 0;
  return ptrace_check (req, id, ptrace (req, id, (int *)addr, data, buf));
}

/* If *PIDP is a composite process/thread id, convert it to a process id. */

static void
pid_to_prc (ptid_t *ptidp)
{
  ptid_t ptid;

  ptid = *ptidp;
  if (PD_TID (ptid))
    *ptidp = pid_to_ptid (PIDGET (ptid));
}

/* pthdb callback: for <i> from 0 to COUNT, set SYMBOLS[<i>].addr to the
   address of SYMBOLS[<i>].name. */

static int
pdc_symbol_addrs (pthdb_user_t user, pthdb_symbol_t *symbols, int count)
{
  struct minimal_symbol *ms;
  int i;
  char *name;

  DBG2(("pdc_symbol_addrs (user = %ld, symbols = 0x%x, count = %d)",
	user, symbols, count));

  for (i = 0; i < count; i++)
    {
      name = symbols[i].name;
      DBG2(("  symbols[%d].name = \"%s\"", i, name));

      if (!*name)
	symbols[i].addr = 0;
      else
	{
	  if (!(ms = lookup_minimal_symbol (name, NULL, NULL)))
	    {
	      DBG2((" returning PDC_FAILURE"));
	      return PDC_FAILURE;
	    }
	  symbols[i].addr = SYMBOL_VALUE_ADDRESS (ms);
	}
      DBG2(("  symbols[%d].addr = 0x%llx", i, symbols[i].addr));
    }
  DBG2((" returning PDC_SUCCESS"));
  return PDC_SUCCESS;
}

/* Read registers call back function should be able to read the context */
/* information of a debuggee kernel thread from an active process or from */
/* a core file. The information should be formatted in context64 form for */
/* both 32-bit and 64-bit process. If successful return 0, else non-zero */
/* is returned. */
static int
pdc_read_regs (pthdb_user_t user, 
	       pthdb_tid_t tid,
	       unsigned long long flags,
	       pthdb_context_t *context)
{
  /* this function doesn't appear to be used, so we could probably just */
  /* return 0 here.  HOWEVER, if it is not defined, the OS will complain */
  /* and several thread debug functions will fail. In case this is needed, */
  /* I have implemented what I think it should do, however this code is */
  /* untested. */
  uint64_t gprs64[32];
  uint32_t gprs32[32];
  double fprs[32];
  struct ptxsprs sprs64;
  struct ptsprs sprs32;
  
  DBG2(("pdc_read_regs tid=%d flags=%llx\n", (int)tid, flags));

  /* General-purpose registers. */
  if (flags & PTHDB_FLAG_GPRS)
    {
      if (arch64)
	{
	  if (!ptrace64aix (PTT_READ_GPRS, tid, (unsigned long) gprs64, 0, NULL))
	    memset (gprs64, 0, sizeof (gprs64));
	  memcpy (context->gpr, gprs64, sizeof(gprs64));
	}
      else
	{
	  if (!ptrace32 (PTT_READ_GPRS, tid, gprs32, 0, NULL))
	    memset (gprs32, 0, sizeof (gprs32));
	  memcpy (context->gpr, gprs32, sizeof(gprs32));
	}
    }

  /* Floating-point registers. */
  if (flags & PTHDB_FLAG_FPRS)
    {
      if (!ptrace32 (PTT_READ_FPRS, tid, (int *) fprs, 0, NULL))
	memset (fprs, 0, sizeof (fprs));
      	  memcpy (context->fpr, fprs, sizeof(fprs));
    }

  /* Special-purpose registers. */
  if (flags & PTHDB_FLAG_SPRS)
    {
      if (arch64)
	{
	  if (!ptrace64aix (PTT_READ_SPRS, tid, (unsigned long) &sprs64, 0, NULL))
	    memset (&sprs64, 0, sizeof (sprs64));
      	  memcpy (&context->msr, &sprs64, sizeof(sprs64));
	}
      else
	{
	  if (!ptrace32 (PTT_READ_SPRS, tid, (int *) &sprs32, 0, NULL))
	    memset (&sprs32, 0, sizeof (sprs32));
      	  memcpy (&context->msr, &sprs32, sizeof(sprs32));
	}
    }  
  return 0;
}

/* Write register function should be able to write requested context */
/* information to specified debuggee's kernel thread id. If successful */
/* return 0, else non-zero is returned. */
static int
pdc_write_regs (pthdb_user_t user,
		pthdb_tid_t tid,
		unsigned long long flags,
		pthdb_context_t *context)
{ 
  /* this function doesn't appear to be used, so we could probably just */
  /* return 0 here.  HOWEVER, if it is not defined, the OS will complain */
  /* and several thread debug functions will fail. In case this is needed, */
  /* I have implemented what I think it should do, however this code is */
  /* untested. */

  DBG2(("pdc_write_regs tid=%d flags=%llx\n", (int)tid, flags));

  /* General-purpose registers. */
  if (flags & PTHDB_FLAG_GPRS)
    {
      if (arch64)
	ptrace64aix (PTT_WRITE_GPRS, tid, (unsigned long)context->gpr, 0, NULL);
      else
	ptrace32 (PTT_WRITE_GPRS, tid, (int *)context->gpr, 0, NULL);
    }

 /* Floating-point registers. */
  if (flags & PTHDB_FLAG_FPRS)
    {
      ptrace32 (PTT_WRITE_FPRS, tid, (int *)context->fpr, 0, NULL);
    }

  /* Special-purpose registers. */
  if (flags & PTHDB_FLAG_SPRS)
    {
      if (arch64)
	{
	  ptrace64aix (PTT_WRITE_SPRS, tid, (unsigned long) &context->msr, 0, NULL);
	}
      else
	{
	  ptrace32 (PTT_WRITE_SPRS, tid, (int *)&context->msr, 0, NULL);
	}
    }
  return 0;
}

/* pthdb callback: read LEN bytes from process ADDR into BUF. */

static int
pdc_read_data (pthdb_user_t user, void *buf, pthdb_addr_t addr, size_t len)
{
  int status, ret;

  DBG2(("pdc_read_data (user = %ld, buf = 0x%x, addr = 0x%llx, len = %d)",
	user, buf, addr, len));

  status = target_read_memory (addr, buf, len);
  ret = status == 0 ? PDC_SUCCESS : PDC_FAILURE;

  DBG2(("  status=%d, returning %s", status, pd_status2str (ret)));
  return ret;
}

/* pthdb callback: write LEN bytes from BUF to process ADDR. */

static int
pdc_write_data (pthdb_user_t user, void *buf, pthdb_addr_t addr, size_t len)
{
  int status, ret;

  DBG2(("pdc_write_data (user = %ld, buf = 0x%x, addr = 0x%llx, len = %d)",
	user, buf, addr, len));

  status = target_write_memory (addr, buf, len);
  ret = status == 0 ? PDC_SUCCESS : PDC_FAILURE;

  DBG2(("  status=%d, returning %s", status, pd_status2str (ret)));
  return ret;
}

/* pthdb callback: allocate a LEN-byte buffer and store a pointer to it in
   BUFP. */

static int
pdc_alloc (pthdb_user_t user, size_t len, void **bufp)
{
  DBG2(("pdc_alloc (user = %ld, len = %d, bufp = 0x%x)", user, len, bufp));
  *bufp = xmalloc (len);
  DBG2(("  malloc returned 0x%x", *bufp));
  /* Note: xmalloc() can't return 0; therefore PDC_FAILURE will never be
     returned.  */
  return *bufp ? PDC_SUCCESS : PDC_FAILURE;
}

/* pthdb callback: reallocate BUF, which was allocated by the alloc or realloc
   callback, so that it contains LEN bytes, and store a pointer to the result
   in BUFP. */

static int
pdc_realloc (pthdb_user_t user, void *buf, size_t len, void **bufp)
{
  DBG2(("pdc_realloc (user = %ld, buf = 0x%x, len = %d, bufp = 0x%x)",
	user, buf, len, bufp));
  *bufp = realloc (buf, len);
  DBG2(("  realloc returned 0x%x", *bufp));
  return *bufp ? PDC_SUCCESS : PDC_FAILURE;
}

/* pthdb callback: free BUF, which was allocated by the alloc or realloc
   callback. */

static int
pdc_dealloc (pthdb_user_t user, void *buf)
{
  DBG2(("pdc_free (user = %ld, buf = 0x%x)", user, buf));
  xfree (buf);
  return PDC_SUCCESS;
}

/* Return a printable representation of pthread STATE. */

static char *
state2str (pthdb_state_t state)
{
  switch (state)
    {
    case PST_IDLE:	return "idle";		/* being created */
    case PST_RUN:	return "running";	/* running */
    case PST_SLEEP:	return "sleeping";	/* awaiting an event */
    case PST_READY:	return "ready";		/* runnable */
    case PST_TERM:	return "finished";	/* awaiting a join/detach */
    default:		return "unknown";
    }
}

/* qsort() comparison function for sorting pd_thread structs by pthid. */

static int
pcmp (const void *p1v, const void *p2v)
{
  struct pd_thread *p1 = (struct pd_thread *) p1v;
  struct pd_thread *p2 = (struct pd_thread *) p2v;
  return p1->pthid < p2->pthid ? -1 : p1->pthid > p2->pthid;
}

/* iterate_over_threads() callback for counting GDB threads. */

static int
giter_count (struct thread_info *thread, void *countp)
{
  (*(int *) countp)++;
  return 0;
}

/* iterate_over_threads() callback for accumulating GDB thread pids. */

static int
giter_accum (struct thread_info *thread, void *bufp)
{
  **(struct thread_info ***) bufp = thread;
  (*(struct thread_info ***) bufp)++;
  return 0;
}

/* ptid comparison function */
static int
ptid_cmp (ptid_t ptid1, ptid_t ptid2)
{
  int pid1, pid2;

  if (ptid_get_pid (ptid1) < ptid_get_pid (ptid2))
    return -1;
  else if (ptid_get_pid (ptid1) > ptid_get_pid (ptid2))
    return 1;
  else if (ptid_get_tid (ptid1) < ptid_get_tid (ptid2))
    return -1;
  else if (ptid_get_tid (ptid1) > ptid_get_tid (ptid2))
    return 1;
  else if (ptid_get_lwp (ptid1) < ptid_get_lwp (ptid2))
    return -1;
  else if (ptid_get_lwp (ptid1) > ptid_get_lwp (ptid2))
    return 1;
  else
    return 0;
}

/* qsort() comparison function for sorting thread_info structs by pid. */

static int
gcmp (const void *t1v, const void *t2v)
{
  struct thread_info *t1 = *(struct thread_info **) t1v;
  struct thread_info *t2 = *(struct thread_info **) t2v;
  return ptid_cmp (t1->ptid, t2->ptid);
}

/* Synchronize GDB's thread list with libpthdebug's.

   There are some benefits of doing this every time the inferior stops:

     - allows users to run thread-specific commands without needing to run
       "info threads" first

     - helps pthdb_tid_pthread() work properly (see "libpthdebug
       peculiarities" at the top of this module)

     - simplifies the demands placed on libpthdebug, which seems to have
       difficulty with certain call patterns */

static void
sync_threadlists (void)
{
  int cmd, status, infpid;
  int pcount, psize, pi, gcount, gi;
  struct pd_thread *pbuf;
  struct thread_info **gbuf, **g, *thread;
  pthdb_pthread_t pdtid;
  pthread_t pthid;
  pthdb_tid_t tid;
  ptid_t pptid, gptid;

  /* Accumulate an array of libpthdebug threads sorted by pthread id. */

  pcount = 0;
  psize = 1;
  pbuf = (struct pd_thread *) xmalloc (psize * sizeof *pbuf);

  for (cmd = PTHDB_LIST_FIRST;; cmd = PTHDB_LIST_NEXT)
    {
      status = pthdb_pthread (pd_session, &pdtid, cmd);
      if (status != PTHDB_SUCCESS || pdtid == PTHDB_INVALID_PTHREAD)
	break;

      status = pthdb_pthread_ptid (pd_session, pdtid, &pthid);
      if (status != PTHDB_SUCCESS || pthid == PTHDB_INVALID_PTID)
	continue;

      if (pcount == psize)
	{
	  psize *= 2;
	  pbuf = (struct pd_thread *) xrealloc (pbuf, psize * sizeof *pbuf);
	}
      pbuf[pcount].pdtid = pdtid;
      pbuf[pcount].pthid = pthid;
      pcount++;
    }

  for (pi = 0; pi < pcount; pi++)
    {
      status = pthdb_pthread_tid (pd_session, pbuf[pi].pdtid, &tid);
      if (status != PTHDB_SUCCESS)
	tid = PTHDB_INVALID_TID;
      pbuf[pi].tid = tid;
    }

  qsort (pbuf, pcount, sizeof *pbuf, pcmp);

  /* Accumulate an array of GDB threads sorted by pid. */

  gcount = 0;
  iterate_over_threads (giter_count, &gcount);
  g = gbuf = (struct thread_info **) xmalloc (gcount * sizeof *gbuf);
  iterate_over_threads (giter_accum, &g);
  qsort (gbuf, gcount, sizeof *gbuf, gcmp);

  /* Apply differences between the two arrays to GDB's thread list. */

  infpid = PIDGET (inferior_ptid);
  for (pi = gi = 0; pi < pcount || gi < gcount;)
    {
      pptid = BUILD_THREAD (pbuf[pi].pthid, infpid);
      gptid = gbuf[gi]->ptid;
      pdtid = pbuf[pi].pdtid;
      tid = pbuf[pi].tid;

      if (pi == pcount)
	goto del;
      if (gi == gcount)
	goto add;

      if (ptid_equal (pptid, gptid))
	{
	  gbuf[gi]->private->pdtid = pdtid;
	  gbuf[gi]->private->tid = tid;
	  pi++;
	  gi++;
	}
      else if (ptid_cmp (pptid, gptid) > 0)
	{
	del:
	  delete_thread (gptid);
	  gi++;
	}
      else
	{
	add:
	  thread = add_thread (pptid);
	  thread->private = xmalloc (sizeof (struct private_thread_info));
	  thread->private->pdtid = pdtid;
	  thread->private->tid = tid;
	  pi++;
	}

    }

  xfree (pbuf);
  xfree (gbuf);
}

/* iterate_over_threads() callback for locating a thread whose kernel thread
   just received a trap signal. */

static int
iter_trap (struct thread_info *thread, void *unused)
{
  struct thrdsinfo64 thrinf;
  pthdb_tid_t tid;

  /* getthrds(3) isn't prototyped in any AIX 4.3.3 #include file. */
  extern int getthrds (pid_t, struct thrdsinfo64 *, int, pthdb_tid_t *, int);

  tid = thread->private->tid;
  if (tid == PTHDB_INVALID_TID)
    return 0;

  if (getthrds (PIDGET (inferior_ptid), &thrinf, sizeof (thrinf), &tid, 1) != 1)
    return 0;

  return thrinf.ti_cursig == SIGTRAP;
}

/* Synchronize libpthdebug's state with the inferior and with GDB, generate a
   composite process/thread <pid> for the current thread, set inferior_ptid to
   <pid> if SET_INFPID, and return <pid>. */

static ptid_t
pd_update (int set_infpid)
{
  int status;
  ptid_t ptid;
  struct thread_info *thread;

  if (!pd_active)
    return inferior_ptid;

  status = pthdb_session_update (pd_session);
  if (status != PTHDB_SUCCESS)
    return inferior_ptid;

  sync_threadlists ();

  /* Define "current thread" as one that just received a trap signal. */

  thread = iterate_over_threads (iter_trap, NULL);
  if (!thread)
    ptid = inferior_ptid;
  else
    {
      ptid = thread->ptid;
      if (set_infpid)
	inferior_ptid = ptid;
    }
  return ptid;
}

/* Try to start debugging threads in the current process.  If successful and
   SET_INFPID, set inferior_ptid to reflect the current thread. */

static ptid_t
pd_activate (int set_infpid)
{
  int status;
		
  status = pthdb_session_init (PD_USER, arch64 ? PEM_64BIT : PEM_32BIT,
			       PTHDB_FLAG_REGS, &pd_callbacks, &pd_session);
  if (status != PTHDB_SUCCESS)
    {
      return inferior_ptid;
    }
  pd_active = 1;
  return pd_update (set_infpid);
}

/* Undo the effects of pd_activate(). */

static void
pd_deactivate (void)
{
  if (!pd_active)
    return;
  pthdb_session_destroy (pd_session);
  
  pid_to_prc (&inferior_ptid);
  pd_active = 0;
}

/* An object file has just been loaded.  Check whether the current application
   is pthreaded, and if so, prepare for thread debugging. */

static void
pd_enable (void)
{
  int status;
  char *stub_name;
  struct minimal_symbol *ms;

  /* Don't initialize twice. */
  if (pd_able)
    return;

  /* Check application word size. */
  arch64 = REGISTER_RAW_SIZE (0) == 8;

  /* Check whether the application is pthreaded. */
  stub_name = NULL;
  status = pthdb_session_pthreaded (PD_USER, PTHDB_FLAG_REGS, &pd_callbacks,
				    &stub_name);
  if ((status != PTHDB_SUCCESS && status != PTHDB_NOT_PTHREADED) || !stub_name)
    return;

  /* Set a breakpoint on the returned stub function. */
  if (!(ms = lookup_minimal_symbol (stub_name, NULL, NULL)))
    return;
  pd_brk_addr = SYMBOL_VALUE_ADDRESS (ms);
  if (!create_thread_event_breakpoint (pd_brk_addr))
    return;

  /* Prepare for thread debugging. */
  base_ops = current_target;
  push_target (&ops);
  pd_able = 1;

  /* If we're debugging a core file or an attached inferior, the pthread
     library may already have been initialized, so try to activate thread
     debugging. */
  pd_activate (1);
}

/* Undo the effects of pd_enable(). */

static void
pd_disable (void)
{
  if (!pd_able)
    return;
  if (pd_active)
    pd_deactivate ();
  pd_able = 0;
  unpush_target (&ops);
}

/* target_new_objfile_hook callback.

   If OBJFILE is non-null, check whether a threaded application is being
   debugged, and if so, prepare for thread debugging.

   If OBJFILE is null, stop debugging threads. */

static void
new_objfile (struct objfile *objfile)
{
  if (objfile)
    pd_enable ();
  else
    pd_disable ();

  if (target_new_objfile_chain)
    target_new_objfile_chain (objfile);
}

/* Attach to process specified by ARGS. */

static void
ops_attach (char *args, int from_tty)
{
  base_ops.to_attach (args, from_tty);
  pd_activate (1);
}

/* Detach from the process attached to by ops_attach(). */

static void
ops_detach (char *args, int from_tty)
{
  pd_deactivate ();
  base_ops.to_detach (args, from_tty);
}

/* Tell the inferior process to continue running thread PID if != -1
   and all threads otherwise. */

static void
ops_resume (ptid_t ptid, int step, enum target_signal sig)
{
  struct thread_info *thread;
  pthdb_tid_t tid[2];

  if (!PD_TID (ptid))
    CALL_BASE (base_ops.to_resume (ptid, step, sig));
  else
    {
      thread = find_thread_pid (ptid);
      if (!thread)
	error ("aix-thread resume: unknown pthread %ld", TIDGET (ptid));

      tid[0] = thread->private->tid;
      if (tid[0] == PTHDB_INVALID_TID)
	error ("aix-thread resume: no tid for pthread %ld", TIDGET (ptid));
      tid[1] = 0;

      if (arch64)
	ptrace64aix (PTT_CONTINUE, tid[0], 1, target_signal_to_host (sig), (int *)tid);
      else
	ptrace32 (PTT_CONTINUE, tid[0], (int *) 1,
		  target_signal_to_host (sig), (int *)tid);
    }
}

/* Wait for thread/process ID if != -1 or for any thread otherwise.  If an
   error occurs, return -1, else return the pid of the stopped thread. */

static ptid_t
ops_wait (ptid_t ptid, struct target_waitstatus *status)
{
  pid_to_prc (&ptid);
  CALL_BASE (ptid = base_ops.to_wait (ptid, status));
  if (PIDGET (ptid) == -1)
    return pid_to_ptid (-1);

  /* Check whether libpthdebug might be ready to be initialized. */
  if (!pd_active && status->kind == TARGET_WAITKIND_STOPPED &&
      status->value.sig == TARGET_SIGNAL_TRAP &&
      read_pc_pid (ptid) - DECR_PC_AFTER_BREAK == pd_brk_addr)
    return pd_activate (0);

  return pd_update (0);
}

/* Record that the 64-bit general-purpose registers contain VALS. */

static void
supply_gprs64 (uint64_t *vals)
{
  int regno;

  for (regno = 0; regno < 32; regno++)
    supply_register (regno, (char *) (vals + regno));
}

/* Record that 32-bit register REGNO contains VAL. */

static void
supply_reg32 (int regno, uint32_t val)
{
  supply_register (regno, (char *) &val);
}

/* Record that the floating-point registers contain VALS. */

static void
supply_fprs (double *vals)
{
  int regno;

  for (regno = 0; regno < 32; regno++)
    supply_register (regno + FP0_REGNUM, (char *) (vals + regno));
}

/* Record that the special registers contain the specified 64-bit and 32-bit
   values. */

static void
supply_sprs64 (uint64_t iar, uint64_t msr, uint32_t cr,
	       uint64_t lr, uint64_t ctr, uint32_t xer)
{
  int regno = FIRST_UISA_SP_REGNUM;
  supply_register (regno, (char *) &iar);
  supply_register (regno + 1, (char *) &msr);
  supply_register (regno + 2, (char *) &cr);
  supply_register (regno + 3, (char *) &lr);
  supply_register (regno + 4, (char *) &ctr);
  supply_register (regno + 5, (char *) &xer);
}

/* Record that the special registers contain the specified 32-bit values. */

static void
supply_sprs32 (uint32_t iar, uint32_t msr, uint32_t cr,
	       uint32_t lr, uint32_t ctr, uint32_t xer)
{
  int regno = FIRST_UISA_SP_REGNUM;
  supply_register (regno, (char *) &iar);
  supply_register (regno + 1, (char *) &msr);
  supply_register (regno + 2, (char *) &cr);
  supply_register (regno + 3, (char *) &lr);
  supply_register (regno + 4, (char *) &ctr);
  supply_register (regno + 5, (char *) &xer);
}

/* Fetch all registers from pthread PDTID, which doesn't have a kernel
   thread.

   There's no way to query a single register from a non-kernel pthread,
   so there's no need for a single-register version of this function. */

static void
fetch_regs_lib (pthdb_pthread_t pdtid)
{
  int status, i;
  pthdb_context_t ctx;

  DBG2 (("fetch_regs_lib %lx\n", (long)pdtid));
  status = pthdb_pthread_context (pd_session, pdtid, &ctx);
  if (status != PTHDB_SUCCESS)
    PD_ERROR ("fetch_registers: pthdb_pthread_context", status);

  /* General-purpose registers. */

  if (arch64)
    supply_gprs64 (ctx.gpr);
  else
    for (i = 0; i < 32; i++)
      supply_reg32 (i, ctx.gpr[i]);

  /* Floating-point registers. */

  supply_fprs (ctx.fpr);

  /* Special registers. */

  if (arch64)
    supply_sprs64 (ctx.iar, ctx.msr, ctx.cr, ctx.lr, ctx.ctr, ctx.xer);
  else
    supply_sprs32 (ctx.iar, ctx.msr, ctx.cr, ctx.lr, ctx.ctr, ctx.xer);
}

/* Fetch register REGNO if != -1 or all registers otherwise from kernel thread
   TID.

   AIX provides a way to query all of a kernel thread's GPRs, FPRs, or SPRs,
   but there's no way to query individual registers within those groups.
   Therefore, if REGNO != -1, this function fetches an entire group.

   Unfortunately, kernel thread register queries often fail with EPERM,
   indicating that the thread is in kernel space.  This breaks backtraces of
   threads other than the current one.  To make that breakage obvious without
   throwing an error to top level (which is bad e.g. during "info threads"
   output), zero registers that can't be retrieved. */

static void
fetch_regs_kern (int regno, pthdb_tid_t tid)
{
  uint64_t gprs64[32];
  uint32_t gprs32[32];
  double fprs[32];
  struct ptxsprs sprs64;
  struct ptsprs sprs32;
  int i;

  DBG2 (("fetch_regs_kern tid=%lx regno=%d arch64=%d\n", (long)tid, regno, arch64));

  /* General-purpose registers. */
  if (regno == -1 || regno < FP0_REGNUM)
    {
      if (arch64)
	{
	  if (!ptrace64aix (PTT_READ_GPRS, tid, (unsigned long) gprs64, 0, NULL))
	    memset (gprs64, 0, sizeof (gprs64));
	  supply_gprs64 (gprs64);
	}
      else
	{
	  if (!ptrace32 (PTT_READ_GPRS, tid, gprs32, 0, NULL))
	    memset (gprs32, 0, sizeof (gprs32));
	  for (i = 0; i < 32; i++)
	    supply_reg32 (i, gprs32[i]);
	}
    }

  /* Floating-point registers. */

  if (regno == -1 || (regno >= FP0_REGNUM && regno <= FPLAST_REGNUM))
    {
      if (!ptrace32 (PTT_READ_FPRS, tid, (int *) fprs, 0, NULL))
	memset (fprs, 0, sizeof (fprs));
      supply_fprs (fprs);
    }

  /* Special-purpose registers. */

  if (regno == -1 || (regno > FPLAST_REGNUM && regno <= LAST_UISA_SP_REGNUM))
    {
      if (arch64)
	{
	  if (!ptrace64aix (PTT_READ_SPRS, tid, (unsigned long) &sprs64, 0, NULL))
	    memset (&sprs64, 0, sizeof (sprs64));
	  supply_sprs64 (sprs64.pt_iar, sprs64.pt_msr, sprs64.pt_cr,
			 sprs64.pt_lr, sprs64.pt_ctr, sprs64.pt_xer);
	}
      else
	{
	  if (!ptrace32 (PTT_READ_SPRS, tid, (int *) &sprs32, 0, NULL))
	    memset (&sprs32, 0, sizeof (sprs32));
	  supply_sprs32 (sprs32.pt_iar, sprs32.pt_msr, sprs32.pt_cr,
			 sprs32.pt_lr, sprs32.pt_ctr, sprs32.pt_xer);

	  if (REGISTER_RAW_SIZE (LAST_UISA_SP_REGNUM))
	    supply_register (LAST_UISA_SP_REGNUM, (char *) &sprs32.pt_mq);
	}
    }
}

/* Fetch register REGNO if != -1 or all registers otherwise in the
   thread/process specified by inferior_ptid. */

static void
ops_fetch_registers (int regno)
{
  struct thread_info *thread;
  pthdb_tid_t tid;

  if (!PD_TID (inferior_ptid))
    base_ops.to_fetch_registers (regno);
  else
    {
      thread = find_thread_pid (inferior_ptid);
      tid = thread->private->tid;

      if (tid == PTHDB_INVALID_TID)
	fetch_regs_lib (thread->private->pdtid);
      else
	fetch_regs_kern (regno, tid);
    }
}

/* Store the special registers into the specified 64-bit and 32-bit
   locations. */

static void
fill_sprs64 (uint64_t *iar, uint64_t *msr, uint32_t *cr,
	     uint64_t *lr, uint64_t *ctr, uint32_t *xer)
{
  int regno = FIRST_UISA_SP_REGNUM;
  *iar = read_register (regno);
  *msr = read_register (regno + 1);
  *cr = read_register (regno + 2);
  *lr = read_register (regno + 3);
  *ctr = read_register (regno + 4);
  *xer = read_register (regno + 5);
}

/* Store all registers into pthread PDTID, which doesn't have a kernel
   thread.

   It's possible to store a single register into a non-kernel pthread, but I
   doubt it's worth the effort. */

static void
store_regs_lib (pthdb_pthread_t pdtid)
{
  int status, i;
  pthdb_context_t ctx;

  DBG2 (("store_regs_lib %lx\n", (long)pdtid));

  /* Retrieve the thread's current context for its non-register values. */
  status = pthdb_pthread_context (pd_session, pdtid, &ctx);
  if (status != PTHDB_SUCCESS)
    PD_ERROR ("store_registers: pthdb_pthread_context", status);

  /* General-purpose registers. */

  for (i = 0; i < 32; i++)
    ctx.gpr[i] = read_register (i);

  /* Floating-point registers. */

  for (i = 0; i < 32; i++)
    ctx.fpr[i] = *(double *) &registers[REGISTER_BYTE (FP0_REGNUM + i)];

  /* Special registers. */

  fill_sprs64 (&ctx.iar, &ctx.msr, &ctx.cr, &ctx.lr, &ctx.ctr, &ctx.xer);

  status = pthdb_pthread_setcontext (pd_session, pdtid, &ctx);
  if (status != PTHDB_SUCCESS)
    PD_ERROR ("store_registers: pthdb_pthread_setcontext", status);
}

/* Store register REGNO if != -1 or all registers otherwise into kernel
   thread TID.

   AIX provides a way to set all of a kernel thread's GPRs, FPRs, or SPRs, but
   there's no way to set individual registers within those groups.  Therefore,
   if REGNO != -1, this function stores an entire group. */

static void
store_regs_kern (int regno, pthdb_tid_t tid)
{
  struct ptxsprs sprs64;
  struct ptsprs sprs32;
  char *regp;

  DBG2 (("store_regs_kern tid=%lx regno=%d\n", (long)tid, regno));

  /* General-purpose registers. */
  if (regno == -1 || regno < FP0_REGNUM)
    {
      regp = &registers[REGISTER_BYTE (0)];
      if (arch64)
	ptrace64aix (PTT_WRITE_GPRS, tid, (unsigned long) regp, 0, NULL);
      else
	ptrace32 (PTT_WRITE_GPRS, tid, (int *) regp, 0, NULL);
    }

  /* Floating-point registers. */

  if (regno == -1 || (regno >= FP0_REGNUM && regno <= FPLAST_REGNUM))
    {
      regp = &registers[REGISTER_BYTE (FP0_REGNUM)];
      ptrace32 (PTT_WRITE_FPRS, tid, (int *) regp, 0, NULL);
    }

  /* Special-purpose registers. */

  if (regno == -1 || (regno > FPLAST_REGNUM && regno <= LAST_UISA_SP_REGNUM))
    {
      if (arch64)
	{
	  ptrace64aix (PTT_READ_SPRS, tid, (unsigned long) &sprs64, 0, NULL);
	  fill_sprs64 (&sprs64.pt_iar, &sprs64.pt_msr, &sprs64.pt_cr,
		       &sprs64.pt_lr, &sprs64.pt_ctr, &sprs64.pt_xer);
	  ptrace64aix (PTT_WRITE_SPRS, tid, (unsigned long) &sprs64, 0, NULL);
	}
      else
	{
	  ptrace32 (PTT_READ_SPRS, tid, (int *) &sprs32, 0, NULL);

	  regno = FIRST_UISA_SP_REGNUM;
	  sprs32.pt_iar = read_register (regno);
	  sprs32.pt_msr = read_register (regno + 1);
	  sprs32.pt_cr = read_register (regno + 2);
	  sprs32.pt_lr = read_register (regno + 3);
	  sprs32.pt_ctr = read_register (regno + 4);
	  sprs32.pt_xer = read_register (regno + 5);

	  if (REGISTER_RAW_SIZE (LAST_UISA_SP_REGNUM))
	    sprs32.pt_mq = read_register (LAST_UISA_SP_REGNUM);

	  ptrace32 (PTT_WRITE_SPRS, tid, (int *) &sprs32, 0, NULL);
	}
    }
}

/* Store gdb's current view of the register set into the thread/process
   specified by inferior_ptid. */

static void
ops_store_registers (int regno)
{
  struct thread_info *thread;
  pthdb_tid_t tid;

  if (!PD_TID (inferior_ptid))
    base_ops.to_store_registers (regno);
  else
    {
      thread = find_thread_pid (inferior_ptid);
      tid = thread->private->tid;

      if (tid == PTHDB_INVALID_TID)
	store_regs_lib (thread->private->pdtid);
      else
	store_regs_kern (regno, tid);
    }
}

/* Prepare to modify the registers array. */

static void
ops_prepare_to_store (void)
{
  if (!PD_TID (inferior_ptid))
    base_ops.to_prepare_to_store ();
  else
    read_register_bytes (0, NULL, REGISTER_BYTES);
}

/* Transfer LEN bytes of memory from GDB address MYADDR to target address
   MEMADDR if WRITE and vice versa otherwise. */

static int
ops_xfer_memory (CORE_ADDR memaddr, char *myaddr, int len, int write,
                 struct mem_attrib *attrib,
		 struct target_ops *target)
{
  int n;

  CALL_BASE (n = base_ops.to_xfer_memory (memaddr, myaddr, len, write,
					  attrib, &base_ops));
  return n;
}

/* Kill and forget about the inferior process. */

static void
ops_kill (void)
{
  CALL_BASE (base_ops.to_kill ());
}

/* Clean up after the inferior exits. */

static void
ops_mourn_inferior (void)
{
  pd_deactivate ();
  base_ops.to_mourn_inferior ();
}

/* Return whether thread PID is still valid. */

static int
ops_thread_alive (ptid_t ptid)
{
  if (!PD_TID (ptid))
    return base_ops.to_thread_alive (ptid);

  /* We update the thread list every time the child stops, so all valid
     threads should be in the thread list. */
  return in_thread_list (ptid);
}

/* Return a printable representation of composite PID for use in "info
   threads" output. */

static char *
ops_pid_to_str (ptid_t ptid)
{
  static char *ret = NULL;

  if (!PD_TID (ptid))
    return base_ops.to_pid_to_str (ptid);

  /* Free previous return value; a new one will be allocated by
     xasprintf().  */
  xfree (ret);

  xasprintf (&ret, "Thread %ld", ptid_get_tid (ptid));
  return ret;
}

/* Return a printable representation of extra information about THREAD, for
   use in "info threads" output. */

static char *
ops_extra_thread_info (struct thread_info *thread)
{
  struct ui_file *buf;
  int status;
  pthdb_pthread_t pdtid;
  pthdb_tid_t tid;
  pthdb_state_t state;
  pthdb_suspendstate_t suspendstate;
  pthdb_detachstate_t detachstate;
  int cancelpend;
  long length;
  static char *ret = NULL;

  if (!PD_TID (thread->ptid))
    return NULL;

  buf = mem_fileopen ();

  pdtid = thread->private->pdtid;
  tid = thread->private->tid;

  if (tid != PTHDB_INVALID_TID)
    fprintf_unfiltered (buf, "tid %d", tid);

  status = pthdb_pthread_state (pd_session, pdtid, &state);
  if (status != PTHDB_SUCCESS)
    state = PST_NOTSUP;
  fprintf_unfiltered (buf, ", %s", state2str (state));

  status = pthdb_pthread_suspendstate (pd_session, pdtid, &suspendstate);
  if (status == PTHDB_SUCCESS && suspendstate == PSS_SUSPENDED)
    fprintf_unfiltered (buf, ", suspended");

  status = pthdb_pthread_detachstate (pd_session, pdtid, &detachstate);
  if (status == PTHDB_SUCCESS && detachstate == PDS_DETACHED)
    fprintf_unfiltered (buf, ", detached");

  pthdb_pthread_cancelpend (pd_session, pdtid, &cancelpend);
  if (status == PTHDB_SUCCESS && cancelpend)
    fprintf_unfiltered (buf, ", cancel pending");

  ui_file_write (buf, "", 1);

  xfree (ret);			/* Free old buffer.  */

  ret = ui_file_xstrdup (buf, &length);
  ui_file_delete (buf);

  return ret;
}

/* Initialize target ops. */

static void
init_ops (void)
{
  ops.to_shortname          = "aix-threads";
  ops.to_longname           = "AIX pthread support";
  ops.to_doc                = "AIX pthread support";

  ops.to_attach             = ops_attach;
  ops.to_detach             = ops_detach;
  ops.to_resume             = ops_resume;
  ops.to_wait               = ops_wait;
  ops.to_fetch_registers    = ops_fetch_registers;
  ops.to_store_registers    = ops_store_registers;
  ops.to_prepare_to_store   = ops_prepare_to_store;
  ops.to_xfer_memory        = ops_xfer_memory;
  /* No need for ops.to_create_inferior, because we activate thread debugging
     when the inferior reaches pd_brk_addr. */
  ops.to_kill               = ops_kill;
  ops.to_mourn_inferior     = ops_mourn_inferior;
  ops.to_thread_alive       = ops_thread_alive;
  ops.to_pid_to_str         = ops_pid_to_str;
  ops.to_extra_thread_info  = ops_extra_thread_info;
  ops.to_stratum            = thread_stratum;
  ops.to_magic              = OPS_MAGIC;
}

/* Module startup initialization function, automagically called by
   init.c. */

void
_initialize_aix_thread (void)
{
  init_ops ();
  add_target (&ops);

  /* Notice when object files get loaded and unloaded. */
  target_new_objfile_chain = target_new_objfile_hook;
  target_new_objfile_hook = new_objfile;
}
