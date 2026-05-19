# PID registry — same JSON file as the Python package, with "language":"R".

# ── inter-process mutex via atomic mkdir ──────────────────────────────────────
# dir.create() calls the atomic mkdir(2) syscall: it returns TRUE only if the
# directory did not previously exist.  Two concurrent processes trying to
# "create" the same lock directory will serialize naturally — exactly one gets
# TRUE and proceeds; the other spins until the lock directory is removed.

.registry_lock <- function() {
  lock <- paste0(.ipc_registry_path(), ".lock.d")
  deadline <- proc.time()[["elapsed"]] + 5  # give up after 5 s
  repeat {
    if (isTRUE(dir.create(lock, showWarnings = FALSE))) return(lock)
    if (proc.time()[["elapsed"]] > deadline) {
      # Stale lock (crash before unlock) — remove and retry once.
      unlink(lock, recursive = TRUE)
      if (isTRUE(dir.create(lock, showWarnings = FALSE))) return(lock)
    }
    Sys.sleep(runif(1L, 0.002, 0.015))
  }
}

.registry_unlock <- function(lock) unlink(lock, recursive = TRUE)

# ── read / write helpers ──────────────────────────────────────────────────────

.ipc_read_registry <- function() {
  path <- .ipc_registry_path()
  if (!file.exists(path)) return(list())
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) list()
  )
}

.ipc_write_registry <- function(entries) {
  jsonlite::write_json(entries, .ipc_registry_path(),
                       auto_unbox = TRUE, pretty = TRUE, null = "null")
}

# ── public API ────────────────────────────────────────────────────────────────

.get_ppid <- function() {
  tryCatch({
    if (.Platform$OS.type == "windows") return(NA_integer_)
    if (file.exists("/proc/self/status")) {
      lines <- readLines("/proc/self/status", warn = FALSE)
      m <- grep("^PPid:", lines, value = TRUE)
      as.integer(gsub("[^0-9]", "", m[[1L]]))
    } else {
      as.integer(trimws(
        system2("ps", c("-o", "ppid=", "-p", as.character(Sys.getpid())),
                stdout = TRUE, stderr = FALSE)[[1L]]
      ))
    }
  }, error = function(e) NA_integer_)
}

.ipc_register <- function(pid, label) {
  lock <- .registry_lock()
  on.exit(.registry_unlock(lock), add = TRUE)

  ppid <- .get_ppid()
  entry <- list(
    pid        = pid,
    label      = label,
    language   = "R",
    ppid       = if (is.na(ppid)) NULL else ppid,
    script     = tryCatch(commandArgs(FALSE)[1L], error = function(e) ""),
    start_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )
  entries <- .ipc_read_registry()
  entries[[as.character(pid)]] <- entry
  .ipc_write_registry(entries)
  invisible(NULL)
}

.ipc_deregister <- function(pid) {
  lock <- .registry_lock()
  on.exit(.registry_unlock(lock), add = TRUE)

  entries <- .ipc_read_registry()
  entries[[as.character(pid)]] <- NULL
  .ipc_write_registry(entries)
  invisible(NULL)
}
