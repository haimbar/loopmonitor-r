# Per-process state file — identical JSON format to the Python package.

.ipc_write_state <- function(pid, iteration, total, start_ts, tracked) {
  elapsed <- as.numeric(Sys.time()) - start_ts
  eta <- if (!is.null(total) && iteration > 0L) {
    elapsed / iteration * (total - iteration)
  } else NULL

  payload <- list(
    pid         = pid,
    iteration   = iteration,
    total       = total,
    elapsed_sec = round(elapsed, 1),
    eta_sec     = if (is.null(eta)) NULL else round(eta, 1),
    tracked     = if (length(tracked) == 0L) {
      setNames(list(), character(0L))
    } else tracked,
    updated     = paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"),
                         "+00:00")
  )

  jsonlite::write_json(payload, .ipc_state_path(pid),
                       auto_unbox = TRUE, null = "null", pretty = TRUE)
}

.ipc_read_state <- function(pid) {
  path <- .ipc_state_path(pid)
  if (!file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

.ipc_remove_state <- function(pid) {
  path <- .ipc_state_path(pid)
  if (file.exists(path)) file.remove(path)
  invisible(NULL)
}
