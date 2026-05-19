# Command file polling and dispatch — R equivalent of _handler.py.
#
# R batch mode repurposes SIGUSR1 (save-and-quit), so loopmonitor uses a polled
# command file instead of a signal handler.  The CLI writes one command per
# line to ~/.ipc/<pid>.cmd; ipc_for/while/repeat call .ipc_process_commands()
# at each iteration boundary.

# ── package-level context store ───────────────────────────────────────────────

.ipc_envs <- new.env(parent = emptyenv())

.ipc_ctx <- function(pid) {
  .ipc_envs[[as.character(pid)]]
}

.ipc_set_ctx <- function(pid, ctx) {
  .ipc_envs[[as.character(pid)]] <- ctx
}

.ipc_remove_ctx <- function(pid) {
  key <- as.character(pid)
  if (exists(key, envir = .ipc_envs, inherits = FALSE))
    rm(list = key, envir = .ipc_envs)
}

# ── command file reader ───────────────────────────────────────────────────────

.ipc_process_commands <- function(pid, ctx, env) {
  path <- .ipc_cmd_path(pid)
  if (!file.exists(path)) return(invisible(NULL))

  # Rename atomically so we "claim" the file; new commands go to a fresh file.
  tmp <- paste0(path, ".reading")
  if (!file.rename(path, tmp)) return(invisible(NULL))

  lines <- tryCatch(readLines(tmp, warn = FALSE), error = function(e) character(0L))
  file.remove(tmp)

  for (cmd in lines) {
    cmd <- trimws(cmd)
    if (nchar(cmd) > 0L) .ipc_dispatch(pid, cmd, ctx, env)
  }
  invisible(NULL)
}

# ── cleanup ───────────────────────────────────────────────────────────────────

.ipc_cleanup_cmd <- function(pid) {
  path <- .ipc_cmd_path(pid)
  if (file.exists(path)) file.remove(path)
  tmp <- paste0(path, ".reading")
  if (file.exists(tmp))  file.remove(tmp)
  invisible(NULL)
}

# ── dispatcher ────────────────────────────────────────────────────────────────

.ipc_dispatch <- function(pid, cmd, ctx, env) {
  if (cmd == "peek") {
    .ipc_do_peek(pid)
  } else if (cmd == "plot" || startsWith(cmd, "plot ")) {
    last <- if (nchar(cmd) > 4L) suppressWarnings(as.integer(trimws(substring(cmd, 6L)))) else 0L
    if (is.na(last)) last <- 0L
    .ipc_do_plot(pid, last)
  } else if (cmd == "continue") {
    .ipc_do_continue(ctx)
  } else if (cmd == "break") {
    .ipc_do_break(pid, ctx)
  } else if (startsWith(cmd, "set ")) {
    .ipc_do_set(substring(cmd, 5L), ctx)
  } else if (cmd == "checkpoint") {
    .ipc_do_checkpoint(pid, ctx)
  } else if (cmd == "stack") {
    .ipc_do_stack(pid, env)
  } else if (cmd == "memory") {
    .ipc_do_memory(pid)
  }
  # unknown commands are silently ignored
}

# ── command handlers ──────────────────────────────────────────────────────────

.ipc_do_peek <- function(pid) {
  state <- .ipc_read_state(pid)
  if (!is.null(state)) {
    cat(.ipc_format_peek(state), "\n", sep = "", flush = TRUE)
  } else {
    cat(sprintf("[loopmonitor] No state available for PID %d\n", pid), flush = TRUE)
  }
}

.ipc_do_plot <- function(pid, last = 0L) {
  state <- .ipc_read_state(pid)
  if (is.null(state)) {
    cat(sprintf("[loopmonitor] No state available for PID %d\n", pid), flush = TRUE)
    return(invisible(NULL))
  }
  tracked <- state[["tracked"]]
  if (length(tracked) == 0L) {
    cat("[loopmonitor] No tracked values to plot.\n", flush = TRUE)
    return(invisible(NULL))
  }

  # Spawn a background Rscript that opens a plot window — same subprocess
  # approach as the Python package uses for matplotlib.
  tmp_data   <- tempfile(fileext = ".rds")
  tmp_script <- tempfile(fileext = ".R")
  saveRDS(state, tmp_data)

  script <- paste0(
    'state <- readRDS(', shQuote(tmp_data), ')\n',
    'last  <- ', as.integer(last), 'L\n',
    'tracked <- state[["tracked"]]\n',
    'n <- length(tracked)\n',
    'if (n == 0L) quit(save="no")\n',
    'grDevices::dev.new()\n',
    'graphics::par(mfrow = c(n, 1L), mar = c(4,3,2,1))\n',
    'title_str <- paste0("PID ", state[["pid"]],
                          "  iter ", state[["iteration"]],
                          if (!is.null(state[["total"]]))
                            paste0("/", state[["total"]]) else "",
                          if (last > 0L) paste0("  (last ", last, " steps)") else "")\n',
    'for (nm in names(tracked)) {\n',
    '  val <- tracked[[nm]]\n',
    '  if (is.numeric(val) && length(val) > 1L) {\n',
    '    total_n <- length(val)\n',
    '    data    <- if (last > 0L) tail(val, last) else val\n',
    '    xlab    <- if (last > 0L && total_n > last)\n',
    '                 paste0("step (last ", last, " of ", total_n, ")")\n',
    '               else "index"\n',
    '    graphics::plot(data, type="l", main=nm, ylab=nm, xlab=xlab)\n',
    '  } else {\n',
    '    graphics::plot.new()\n',
    '    graphics::text(0.5, 0.5, paste0(nm, " = ", val[[1L]]), cex=2)\n',
    '  }\n',
    '}\n',
    'graphics::mtext(title_str, outer=TRUE, line=-1, cex=0.8)\n',
    'on.exit({\n',
    '  suppressWarnings(file.remove(', shQuote(tmp_data), '))\n',
    '  suppressWarnings(file.remove(', shQuote(tmp_script), '))\n',
    '})\n',
    'cat("Close the plot window to dismiss.\\n")\n',
    'invisible(readline())\n'
  )
  writeLines(script, tmp_script)
  system2("Rscript", shQuote(tmp_script), wait = FALSE,
          stdout = NULL, stderr = NULL)
}

.ipc_do_continue <- function(ctx) {
  ctx$should_stop <- TRUE
  cat("[loopmonitor] 'continue' received — loop will exit after this iteration.\n",
      flush = TRUE)
}

.ipc_do_break <- function(pid, ctx) {
  state <- .ipc_read_state(pid)
  if (!is.null(state)) cat(.ipc_format_peek(state), "\n", sep = "", flush = TRUE)

  ts   <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
  path <- paste0("loopmonitor_break_", pid, "_", ts, ".json")
  tryCatch({
    jsonlite::write_json(state %||% list(), path,
                         auto_unbox = TRUE, null = "null", pretty = TRUE)
    cat(sprintf("[loopmonitor] State written to %s\n", path), flush = TRUE)
  }, error = function(e) {
    cat(sprintf("[loopmonitor] Could not write state: %s\n", conditionMessage(e)),
        flush = TRUE)
  })
  quit(save = "no", status = 0L)
}

.ipc_do_set <- function(assignment, ctx) {
  tryCatch({
    eq <- regexpr("=", assignment, fixed = TRUE)
    if (eq < 0L) stop("missing '='")
    key     <- trimws(substring(assignment, 1L, eq - 1L))
    val_str <- trimws(substring(assignment, eq + 1L))
    if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", key))
      stop(sprintf("invalid identifier: %s", key))
    val <- .safe_eval(val_str)
    ctx$injected[[key]] <- val
    cat(sprintf("[loopmonitor] set %s = %s\n", key, deparse(val)[[1L]]), flush = TRUE)
  }, error = function(e) {
    cat(sprintf("[loopmonitor] 'set' failed: %s\n", conditionMessage(e)), flush = TRUE)
  })
}

# Evaluate only R literals: numbers, strings, logicals, NULL, NA, Inf, NaN,
# and calls to c() and list() — mirrors Python's ast.literal_eval().
.safe_eval <- function(text) {
  expr <- tryCatch(
    parse(text = text, keep.source = FALSE)[[1L]],
    error = function(e) stop(sprintf("parse error: %s", conditionMessage(e)))
  )
  .check_literal(expr)
  # Evaluate in a minimal environment that contains only safe primitives.
  # (Reserved words TRUE/FALSE/NULL/NA/Inf/NaN are always visible in R
  #  regardless of the environment, so we only need to add c and list.)
  safe_env <- new.env(parent = baseenv())
  safe_env$c    <- base::c
  safe_env$list <- base::list
  eval(expr, envir = safe_env)
}

.check_literal <- function(expr) {
  if (is.numeric(expr) || is.character(expr) ||
      is.logical(expr) || is.null(expr)) return(invisible(NULL))
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (nm %in% c("TRUE", "FALSE", "NULL", "NA", "Inf", "NaN", "T", "F"))
      return(invisible(NULL))
    stop(sprintf("symbol not allowed: %s", nm))
  }
  if (is.call(expr)) {
    fn <- as.character(expr[[1L]])
    allowed <- c("c", "list", "-", "+")
    if (!fn %in% allowed)
      stop(sprintf("function not allowed: %s()", fn))
    lapply(as.list(expr)[-1L], .check_literal)
  }
  invisible(NULL)
}

.ipc_do_checkpoint <- function(pid, ctx) {
  state <- .ipc_read_state(pid)
  if (is.null(state)) {
    cat(sprintf("[loopmonitor] No state available for PID %d\n", pid), flush = TRUE)
    return(invisible(NULL))
  }
  ts   <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
  path <- paste0("loopmonitor_checkpoint_", pid, "_", ts, ".json")
  tryCatch({
    jsonlite::write_json(state, path,
                         auto_unbox = TRUE, null = "null", pretty = TRUE)
    cat(sprintf("[loopmonitor] Checkpoint saved to %s\n", path), flush = TRUE)
  }, error = function(e) {
    cat(sprintf("[loopmonitor] Checkpoint failed: %s\n", conditionMessage(e)),
        flush = TRUE)
  })
}

.ipc_do_stack <- function(pid, env) {
  calls <- sys.calls()
  cat(sprintf("[loopmonitor] Stack trace for PID %d:\n", pid), flush = TRUE)
  # Drop loopmonitor-internal frames from the bottom of the stack
  for (i in seq_along(calls)) {
    cat(sprintf("  [%d] %s\n", i, deparse(calls[[i]])[[1L]]), flush = TRUE)
  }
}

.ipc_do_memory <- function(pid) {
  tryCatch({
    if (file.exists("/proc/self/status")) {
      # Linux: read VmRSS from /proc
      lines  <- readLines("/proc/self/status", warn = FALSE)
      vmrss  <- grep("^VmRSS", lines, value = TRUE)
      if (length(vmrss) > 0L) {
        kb <- as.numeric(gsub("[^0-9]", "", vmrss[[1L]]))
        cat(sprintf("[loopmonitor] PID %d memory — RSS: %.1f MB\n", pid, kb / 1024),
            flush = TRUE)
      }
    } else {
      # macOS / other: ask ps
      kb <- as.numeric(
        system2("ps", c("-o", "rss=", "-p", as.character(pid)),
                stdout = TRUE, stderr = FALSE)[[1L]]
      )
      cat(sprintf("[loopmonitor] PID %d memory — RSS: %.1f MB\n", pid, kb / 1024),
          flush = TRUE)
    }
  }, error = function(e) {
    cat(sprintf("[loopmonitor] Memory info unavailable: %s\n", conditionMessage(e)),
        flush = TRUE)
  })
}

# Null-coalescing helper (base R doesn't have %||% before R 4.4)
`%||%` <- function(a, b) if (!is.null(a)) a else b
