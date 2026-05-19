library(loopmonitor)

# ── helpers ───────────────────────────────────────────────────────────────────

skip_if(
  .Platform$OS.type == "windows",
  "parallel::mcparallel (fork) not available on Windows"
)

.worker <- function(worker_id, n_steps, delay_sec) {
  loss <- 1.0 - worker_id * 0.05
  ipc_for(i, seq_len(n_steps), {
    Sys.sleep(delay_sec)
    loss <- loss * (1.0 - 0.01 * (1.0 + 0.1 * sin(i * 0.3)))
    loss <- max(1e-6, loss)
    ipc_track(loss = round(loss, 6L), worker = worker_id)
  }, label = paste0("worker-", worker_id))
}

.fork_workers <- function(n = 3L, n_steps = 200L, delay_sec = 0.1) {
  lapply(seq_len(n), function(id) {
    parallel::mcparallel(.worker(id, n_steps, delay_sec),
                         name = paste0("worker-", id))
  })
}

.wait_all_registered <- function(pids, timeout_sec = 15) {
  reg_path <- .ipc_registry_path()
  deadline <- proc.time()[["elapsed"]] + timeout_sec
  repeat {
    if (file.exists(reg_path)) {
      reg <- tryCatch(
        jsonlite::fromJSON(reg_path, simplifyVector = FALSE),
        error = function(e) list()
      )
      if (all(as.character(pids) %in% names(reg))) return(TRUE)
    }
    if (proc.time()[["elapsed"]] > deadline) return(FALSE)
    Sys.sleep(0.25)
  }
}

.setup_tmp <- function() {
  tmp_dir <- tempfile("loopmonitor_test_")
  dir.create(tmp_dir, recursive = TRUE)
  old_loopmonitor_dir <- Sys.getenv("LOOPMONITOR_DIR", unset = NA_character_)
  old_pythonpath <- Sys.getenv("PYTHONPATH", unset = NA_character_)

  # Ensure system2("python3", c("-m", "loopmonitor.cli", ...)) can find the module
  # (it is not installed system-wide; add the source tree to PYTHONPATH).
  # During devtools::test(), getwd() is <pkg>/tests/testthat — go up 3 levels.
  py_src <- file.path(dirname(dirname(dirname(getwd()))), "loopmonitor")
  existing_pp <- Sys.getenv("PYTHONPATH", unset = "")
  new_pp <- if (nzchar(existing_pp)) paste(py_src, existing_pp, sep = ":") else py_src

  Sys.setenv(LOOPMONITOR_DIR = tmp_dir, PYTHONPATH = new_pp)
  list(dir = tmp_dir, old_loopmonitor_dir = old_loopmonitor_dir, old_pythonpath = old_pythonpath)
}

.teardown_tmp <- function(ctx) {
  Sys.unsetenv("LOOPMONITOR_DIR")
  if (!is.na(ctx$old_loopmonitor_dir)) Sys.setenv(LOOPMONITOR_DIR = ctx$old_loopmonitor_dir)
  Sys.unsetenv("PYTHONPATH")
  if (!is.na(ctx$old_pythonpath)) Sys.setenv(PYTHONPATH = ctx$old_pythonpath)
  unlink(ctx$dir, recursive = TRUE)
}

# ── tests ─────────────────────────────────────────────────────────────────────

test_that("three mcparallel workers register with correct PPID", {
  env <- .setup_tmp()
  on.exit(.teardown_tmp(env), add = TRUE)

  parent_pid <- Sys.getpid()
  jobs <- .fork_workers()
  pids <- vapply(jobs, function(j) j$pid, integer(1L))
  on.exit(suppressWarnings(
    parallel::mccollect(jobs, wait = FALSE, timeout = 2)
  ), add = TRUE)

  expect_true(.wait_all_registered(pids),
              label = "all 3 workers registered within timeout")

  reg <- jsonlite::fromJSON(.ipc_registry_path(), simplifyVector = FALSE)
  expect_equal(length(reg), 3L)

  for (pid in pids) {
    entry <- reg[[as.character(pid)]]
    expect_false(is.null(entry))
    expect_equal(entry[["language"]], "R")
    expect_true(grepl("^worker-", entry[["label"]]))
    # PPID should be stored and equal the current (parent) PID.
    expect_equal(as.integer(entry[["ppid"]]), parent_pid,
                 label = sprintf("ppid for worker PID %d", pid))
  }

  for (pid in pids) writeLines("continue", file.path(env$dir, paste0(pid, ".cmd")))
  suppressWarnings(parallel::mccollect(jobs, wait = TRUE, timeout = 10))
})


test_that("each worker writes a state file with real progress", {
  env <- .setup_tmp()
  on.exit(.teardown_tmp(env), add = TRUE)

  jobs <- .fork_workers()
  pids <- vapply(jobs, function(j) j$pid, integer(1L))
  on.exit(suppressWarnings(
    parallel::mccollect(jobs, wait = FALSE, timeout = 2)
  ), add = TRUE)

  expect_true(.wait_all_registered(pids))
  Sys.sleep(1.5)

  for (pid in pids) {
    state_path <- file.path(env$dir, paste0(pid, ".state.json"))
    expect_true(file.exists(state_path),
                label = sprintf("state file for PID %d", pid))
    state <- jsonlite::fromJSON(state_path, simplifyVector = FALSE)
    expect_true(state[["iteration"]] >= 1L,
                label = sprintf("PID %d: at least 1 iteration", pid))
    expect_false(is.null(state[["tracked"]][["loss"]]),
                 label = sprintf("PID %d tracks 'loss'", pid))
  }

  for (pid in pids) writeLines("continue", file.path(env$dir, paste0(pid, ".cmd")))
  suppressWarnings(parallel::mccollect(jobs, wait = TRUE, timeout = 10))
})


test_that("broadcasting 'continue' via cmd files stops all workers", {
  env <- .setup_tmp()
  on.exit(.teardown_tmp(env), add = TRUE)

  jobs <- .fork_workers(n_steps = 500L, delay_sec = 0.15)
  pids <- vapply(jobs, function(j) j$pid, integer(1L))
  on.exit({
    for (pid in pids) {
      tryCatch(tools::pskill(pid, signal = 9L), error = function(e) NULL)
    }
    suppressWarnings(parallel::mccollect(jobs, wait = FALSE, timeout = 2))
  }, add = TRUE)

  expect_true(.wait_all_registered(pids))
  Sys.sleep(0.5)

  # Broadcast 'continue' to all workers via their cmd files.
  for (pid in pids) {
    writeLines("continue", file.path(env$dir, paste0(pid, ".cmd")))
  }

  # Wait for all workers to deregister.
  reg_path <- .ipc_registry_path()
  deadline <- proc.time()[["elapsed"]] + 20
  all_gone <- FALSE
  repeat {
    if (file.exists(reg_path)) {
      reg <- tryCatch(
        jsonlite::fromJSON(reg_path, simplifyVector = FALSE),
        error = function(e) list()
      )
      if (!any(as.character(pids) %in% names(reg))) { all_gone <- TRUE; break }
    }
    if (proc.time()[["elapsed"]] > deadline) break
    Sys.sleep(0.3)
  }
  expect_true(all_gone, label = "all workers deregistered after broadcast continue")

  suppressWarnings(parallel::mccollect(jobs, wait = TRUE, timeout = 5))
})


test_that("ipc list --group <ppid> shows only children of that parent (via CLI)", {
  env <- .setup_tmp()
  on.exit(.teardown_tmp(env), add = TRUE)

  parent_pid <- Sys.getpid()
  jobs <- .fork_workers()
  pids <- vapply(jobs, function(j) j$pid, integer(1L))
  on.exit(suppressWarnings(
    parallel::mccollect(jobs, wait = FALSE, timeout = 2)
  ), add = TRUE)

  expect_true(.wait_all_registered(pids))

  # `ipc list --group <parent_pid>` should show all 3 workers.
  # LOOPMONITOR_DIR is already in the inherited environment from Sys.setenv() above.
  result <- system2(
    "python3",
    c("-m", "loopmonitor.cli", "list", "--group", as.character(parent_pid)),
    stdout = TRUE, stderr = FALSE
  )
  output <- paste(result, collapse = "\n")
  for (pid in pids) {
    expect_true(grepl(as.character(pid), output, fixed = TRUE),
                label = sprintf("PID %d in ipc list --group output", pid))
  }

  # A different group (init PID 1) should not contain any of these workers.
  result2 <- system2(
    "python3",
    c("-m", "loopmonitor.cli", "list", "--group", "1"),
    stdout = TRUE, stderr = FALSE
  )
  output2 <- paste(result2, collapse = "\n")
  for (pid in pids) {
    expect_false(grepl(as.character(pid), output2, fixed = TRUE),
                 label = sprintf("PID %d absent from wrong group", pid))
  }

  for (pid in pids) writeLines("continue", file.path(env$dir, paste0(pid, ".cmd")))
  suppressWarnings(parallel::mccollect(jobs, wait = TRUE, timeout = 10))
})
