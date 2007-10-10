/*	Ideal DUP DROP * 1000 assuming perfect inlining.
	$Id: perf_dupdrop.c,v 1.1 2007-10-10 13:01:05 rich Exp $
*/

#include <stdio.h>
#include <stdlib.h>

#define DUP					\
  asm volatile ("mov (%%esp),%%eax\n"		\
		"\tpush %%eax"			\
		: : : "eax")
#define DROP					\
  asm volatile ("pop %%eax"			\
		: : : "eax")

#define DUPDROP DUP; DROP;
#define DUPDROP10 DUPDROP DUPDROP DUPDROP DUPDROP DUPDROP DUPDROP DUPDROP DUPDROP DUPDROP DUPDROP
#define DUPDROP100 DUPDROP10 DUPDROP10 DUPDROP10 DUPDROP10 DUPDROP10 DUPDROP10 DUPDROP10 DUPDROP10 DUPDROP10 DUPDROP10
#define DUPDROP1000 DUPDROP100 DUPDROP100 DUPDROP100 DUPDROP100 DUPDROP100 DUPDROP100 DUPDROP100 DUPDROP100 DUPDROP100 DUPDROP100

int
main (int argc, char *argv[])
{
  unsigned long long start_time, end_time;

  asm volatile ("rdtsc" : "=A" (start_time));
  DUPDROP1000
  asm volatile ("rdtsc" : "=A" (end_time));

  printf ("%llu\n", end_time - start_time);

  exit (0);
}
