#' Track a variable's current value
#'
#' Call inside an \code{ipc_for}, \code{ipc_while}, or \code{ipc_repeat} body
#' to record values that are displayed by \code{ipc peek} and \code{ipc plot}.
#' Multiple calls per iteration accumulate; the most recent value for each key
#' is kept.
#'
#' @param ... Named values to track, e.g. \code{loss = 0.42, lr = 0.01}.
#' @return Invisibly \code{NULL}.
#' @export
ipc_track <- function(...) {
  ctx <- .ipc_ctx(Sys.getpid())
  if (is.null(ctx)) return(invisible(NULL))
  args <- list(...)
  for (nm in names(args)) ctx$tracked[[nm]] <- args[[nm]]
  invisible(NULL)
}

#' Read a value injected by \code{ipc set}
#'
#' Returns the value most recently written by \code{ipc set <pid> key=value},
#' or \code{default} if no value has been injected for that key yet.
#'
#' @param key   Character string — the variable name used in \code{ipc set}.
#' @param default Value returned when \code{key} has not been set (default \code{NULL}).
#' @return The injected value, or \code{default}.
#' @export
ipc_get <- function(key, default = NULL) {
  ctx <- .ipc_ctx(Sys.getpid())
  if (is.null(ctx)) return(default)
  val <- ctx$injected[[key]]
  if (is.null(val)) default else val
}
