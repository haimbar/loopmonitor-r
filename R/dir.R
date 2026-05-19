# Path helpers — mirrors loopmonitor/loopmonitor/_dir.py

.ipc_dir <- function() {
  # Mirror the Python package: honour LOOPCTL_DIR first (shared override),
  # fall back to LOOPMONITOR_DIR (R-only legacy), then ~/.ipc.
  custom <- Sys.getenv("LOOPCTL_DIR", unset = "")
  if (!nzchar(custom)) custom <- Sys.getenv("LOOPMONITOR_DIR", unset = "")
  d <- if (nzchar(custom)) custom else file.path(Sys.getenv("HOME"), ".ipc")
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    Sys.chmod(d, mode = "700")
  }
  d
}

.ipc_state_path <- function(pid) {
  file.path(.ipc_dir(), paste0(pid, ".state.json"))
}

.ipc_cmd_path <- function(pid) {
  file.path(.ipc_dir(), paste0(pid, ".cmd"))
}

.ipc_registry_path <- function() {
  file.path(.ipc_dir(), "registry.json")
}
