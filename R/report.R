# Terminal formatting — mirrors loopmonitor/loopmonitor/_report.py

.fmt_time <- function(secs) {
  if (is.null(secs) || is.na(secs)) return("?")
  secs <- as.integer(secs)
  days  <- secs %/% 86400L
  hrs   <- (secs %% 86400L) %/% 3600L
  mins  <- (secs %% 3600L)  %/% 60L
  sec   <- secs %% 60L
  if (days > 0L) {
    sprintf("%dd %02d:%02d:%02d", days, hrs, mins, sec)
  } else if (hrs > 0L) {
    sprintf("%d:%02d:%02d", hrs, mins, sec)
  } else {
    sprintf("%02d:%02d", mins, sec)
  }
}

.ipc_format_peek <- function(state) {
  pid     <- state[["pid"]]
  it      <- state[["iteration"]]
  total   <- state[["total"]]
  elapsed <- state[["elapsed_sec"]]
  eta     <- state[["eta_sec"]]
  tracked <- state[["tracked"]]

  pct_str   <- if (!is.null(total) && is.numeric(it) && total > 0)
    sprintf("  (%.1f%%)", 100 * it / total) else ""
  total_str <- if (!is.null(total)) as.character(total) else "?"
  progress  <- if (!is.null(total))
    paste0("iter ", it, "/", total_str, pct_str)
  else
    paste0("iter ", it)

  lines <- c(
    sprintf("[loopmonitor] PID %s  %s", pid, progress),
    sprintf("         elapsed %s  ETA %s", .fmt_time(elapsed), .fmt_time(eta))
  )
  if (length(tracked) > 0L) {
    vals <- paste(mapply(function(k, v) paste0(k, "=", v),
                         names(tracked), tracked),
                  collapse = "  ")
    lines <- c(lines, paste0("         ", vals))
  }
  paste(lines, collapse = "\n")
}
