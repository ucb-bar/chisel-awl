// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

#include "verilated.h"
#include <iostream>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

static uint64_t trace_count = 0;
bool verbose;
bool done_reset;

double sc_time_stamp()
{
  return trace_count;
}

int main(int argc, char** argv)
{
  int random_seed = 3;
  int max_cycles = 100000;
  int ret = 1;
  if (verbose)
    fprintf(stderr, "using random seed %u\n", random_seed);

  srand(random_seed);
  srand48(random_seed);

  Verilated::randReset(2);
  Verilated::commandArgs(argc, argv);
  TEST_HARNESS *tile = new TEST_HARNESS;

  //signal(SIGTERM, handle_sigterm);

  bool dump;
  // reset for several cycles to handle pipelined reset
  for (int i = 0; i < 10; i++) {
    tile->reset = 1;
    tile->clock = 0;
    tile->eval();
    tile->clock = 1;
    tile->eval();
    trace_count ++;
  }
  tile->reset = 0;
  done_reset = true;

  while (!tile->io_success && trace_count < max_cycles) {
    tile->clock = 0;
    tile->eval();
    tile->clock = 1;
    tile->eval();
    trace_count++;
  }


  if (trace_count == max_cycles)
  {
    fprintf(stderr, "*** FAILED *** via trace_count (timeout, seed %d) after %ld cycles\n", random_seed, trace_count);
    ret = 2;
  }
  else if (verbose)
  {
    fprintf(stderr, "*** PASSED *** Completed after %ld cycles\n", trace_count);
    ret = 0;
  }

  if (tile) delete tile;
  return ret;
}
