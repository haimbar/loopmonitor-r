#!/usr/bin/env Rscript
# fork_workers.R — 3 parallel workers monitored by loopmonitor.
#
# Run this script, then in another terminal:
#
#   ipc list                    # shows 3 registered R processes
#   ipc peek   <pid>            # progress + tracked values for one worker
#   ipc plot   <pid>            # live loss curve for one worker
#   ipc set    <pid> lr=0.001   # inject a new learning rate mid-run
#   ipc continue <pid>          # stop one worker cleanly
#
# Requires: R package loopmonitor, base package parallel

library(loopmonitor)
library(parallel)

worker <- function(worker_id, n_steps = 300L) {
  loss <- 1.0 - worker_id * 0.05
  ipc_for(i, seq_len(n_steps), {
    lr   <- ipc_get("lr", default = 0.01)
    Sys.sleep(0.2)
    loss <- loss * (1.0 - lr * (1.0 + 0.1 * sin(i * 0.3 * worker_id)))
    loss <- max(1e-6, loss)
    ipc_track(loss = round(loss, 6L), lr = lr)
  }, label = paste0("worker-", worker_id))
}

n_workers <- 3L
cat(sprintf("Starting %d workers.  Run `ipc list` to monitor them.\n", n_workers))

jobs <- vector("list", n_workers)
for (id in seq_len(n_workers)) {
  jobs[[id]] <- parallel::mcparallel(worker(id), name = paste0("worker-", id))
}

# Print PIDs now that children are forked.
pids <- vapply(jobs, function(j) j$pid, integer(1L))
for (id in seq_len(n_workers)) {
  cat(sprintf("  worker-%d  PID %d\n", id, pids[[id]]))
}

cat("\nWaiting for all workers to finish (Ctrl-C to abort)...\n")
parallel::mccollect(jobs, wait = TRUE)
cat("Done.\n")
