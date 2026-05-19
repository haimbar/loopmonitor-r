# ipc_for / ipc_while / ipc_repeat
#
# Design note on break/next:
#   R's break/next are C-level LONGJMP calls. When the body is evaluated via
#   eval(body_expr, envir = caller_env), the C context chain visible to break
#   does not include the enclosing for/while/repeat inside the ipc_* function,
#   so a bare break would error "no loop for break/next".
#
#   Fix: preprocess the body AST before evaluation.  Top-level break/next
#   (not inside nested loops or function definitions in the body) are replaced
#   with stop(ipc_break_cond) / stop(ipc_next_cond).  A tryCatch wrapper in
#   the main loop catches these custom conditions and executes a real break/next
#   in the ipc_* function's own loop, where the context IS visible.

# ── AST preprocessing ─────────────────────────────────────────────────────────

.rewrite_break_next <- function(expr, nested = FALSE) {
    # In R's AST, break and next are calls (not names), so test with identical()
    if (!nested && identical(expr, quote(break))) return(.ipc_break_expr)
    if (!nested && identical(expr, quote(next)))  return(.ipc_next_expr)
    if (is.call(expr)) {
        fn_name <- tryCatch(as.character(expr[[1L]]), error = function(e) "")
        # Nested loops and function definitions create their own break/next scope
        new_nested <- nested || fn_name %in% c("for", "while", "repeat", "function")
        result <- as.call(lapply(seq_along(expr), function(i) {
            if (i == 1L) expr[[i]]
            else .rewrite_break_next(expr[[i]], new_nested)
        }))
        # as.call() drops argument names — restore them
        if (!is.null(names(expr))) names(result) <- names(expr)
        result
    } else {
        expr
    }
}

# Pre-built AST nodes for the replacement expressions (built once at load time)
.ipc_break_expr <- quote(
    stop(structure(list(message = "", call = NULL),
                   class = c("ipc_break", "error", "condition")))
)
.ipc_next_expr <- quote(
    stop(structure(list(message = "", call = NULL),
                   class = c("ipc_next", "error", "condition")))
)

# Evaluate a preprocessed body; return "break", "next", or "continue".
.ipc_eval_body <- function(rewritten, env) {
    tryCatch(
        { eval(rewritten, envir = env); "continue" },
        ipc_break = function(e) "break",
        ipc_next  = function(e) "next"
    )
}

# ── shared setup / teardown ───────────────────────────────────────────────────

.ipc_new_ctx <- function() {
    ctx <- new.env(parent = emptyenv())
    ctx$should_stop <- FALSE
    ctx$tracked     <- setNames(list(), character(0L))
    ctx$injected    <- setNames(list(), character(0L))
    ctx
}

.ipc_install <- function(pid, label) {
    ctx <- .ipc_new_ctx()
    .ipc_set_ctx(pid, ctx)
    .ipc_register(pid, label)
    ctx
}

.ipc_teardown <- function(pid, iter_count, total, start_ts, ctx) {
    .ipc_write_state(pid, iter_count, total, start_ts, ctx$tracked)
    .ipc_cleanup_cmd(pid)
    .ipc_remove_state(pid)
    .ipc_deregister(pid)
    .ipc_remove_ctx(pid)
}

.default_label <- function() {
    tryCatch(basename(sys.frames()[[1L]]$ofile), error = function(e) "R")
}

# ── ipc_for ───────────────────────────────────────────────────────────────────

#' Monitored for loop
#'
#' Drop-in replacement for \code{for}.  Registers the process in the loopmonitor
#' registry, writes a state file after every \code{state_every} iterations,
#' and polls for commands from the \code{ipc} CLI at each iteration boundary.
#' \code{break} and \code{next} work exactly as in an ordinary \code{for} loop.
#'
#' @param var         Loop variable (unquoted symbol, e.g. \code{i}).
#' @param iter        Iterable (vector, list, or any object with \code{length()}
#'                    and numeric indexing).
#' @param body        Loop body expression (wrapped in \code{\{...\}}).
#' @param label       Label shown by \code{ipc list} (default: script name).
#' @param state_every Write state every \emph{n} iterations (default 1).
#' @return Invisibly \code{NULL}. Side effects only.
#' @export
#'
#' @examples
#' \dontrun{
#' library(loopmonitor)
#'
#' # Stand-in for any real per-iteration computation (model step, simulation, …)
#' make_loss <- function(i, n) max(0, exp(-3*(i-1)/n) + rnorm(1, sd=0.05))
#'
#' ipc_for(i, 1:1000, {
#'   loss <- make_loss(i, 1000)
#'   ipc_track(loss = loss)
#'   if (loss < 0.01) break
#' }, label = "training")
#' }
ipc_for <- function(var, iter, body, label = "", state_every = 1L) {
    var_name  <- as.character(substitute(var))
    body_raw  <- substitute(body)
    body_rw   <- .rewrite_break_next(body_raw)
    caller    <- parent.frame()
    pid       <- Sys.getpid()

    total <- tryCatch({
        n <- length(iter)
        if (is.finite(n) && n >= 0L) as.integer(n) else NULL
    }, error = function(e) NULL)

    if (!nzchar(label)) label <- .default_label()
    ctx         <- .ipc_install(pid, label)
    start_ts    <- as.numeric(Sys.time())
    iter_count  <- 0L
    state_every <- max(1L, as.integer(state_every))

    on.exit(.ipc_teardown(pid, iter_count, total, start_ts, ctx), add = TRUE)

    for (.loopmonitor_item in iter) {
        assign(var_name, .loopmonitor_item, envir = caller)
        .ipc_process_commands(pid, ctx, caller)
        if (isTRUE(ctx$should_stop)) break
        iter_count <- iter_count + 1L
        result <- .ipc_eval_body(body_rw, caller)
        if (identical(result, "break")) break
        if (identical(result, "next"))  next
        if (iter_count %% state_every == 0L)
            .ipc_write_state(pid, iter_count, total, start_ts, ctx$tracked)
    }

    invisible(NULL)
}

# ── ipc_while ─────────────────────────────────────────────────────────────────

#' Monitored while loop
#'
#' Drop-in replacement for \code{while}.  The condition expression is
#' re-evaluated each iteration in the caller's environment.
#' \code{break} and \code{next} work as in an ordinary \code{while} loop.
#'
#' @param condition Condition expression (re-evaluated each iteration).
#' @param body      Loop body expression.
#' @param label     Label shown by \code{ipc list}.
#' @param state_every Write state every \emph{n} iterations (default 1).
#' @return Invisibly \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' make_loss <- function(i, n) max(0, exp(-3*(i-1)/n) + rnorm(1, sd=0.05))
#'
#' i <- 0L; loss <- Inf
#' ipc_while(loss > 0.01, {
#'   i    <- i + 1L
#'   loss <- make_loss(i, 200)
#'   ipc_track(loss = loss)
#' }, label = "training")
#' }
ipc_while <- function(condition, body, label = "", state_every = 1L) {
    cond_raw  <- substitute(condition)
    body_raw  <- substitute(body)
    body_rw   <- .rewrite_break_next(body_raw)
    caller    <- parent.frame()
    pid       <- Sys.getpid()

    if (!nzchar(label)) label <- .default_label()
    ctx         <- .ipc_install(pid, label)
    start_ts    <- as.numeric(Sys.time())
    iter_count  <- 0L
    state_every <- max(1L, as.integer(state_every))

    on.exit(.ipc_teardown(pid, iter_count, NULL, start_ts, ctx), add = TRUE)

    while (isTRUE(eval(cond_raw, envir = caller))) {
        .ipc_process_commands(pid, ctx, caller)
        if (isTRUE(ctx$should_stop)) break
        iter_count <- iter_count + 1L
        result <- .ipc_eval_body(body_rw, caller)
        if (identical(result, "break")) break
        if (identical(result, "next"))  next
        if (iter_count %% state_every == 0L)
            .ipc_write_state(pid, iter_count, NULL, start_ts, ctx$tracked)
    }

    invisible(NULL)
}

# ── ipc_repeat ────────────────────────────────────────────────────────────────

#' Monitored repeat loop
#'
#' Drop-in replacement for \code{repeat}.  Use \code{break} inside the body
#' to exit; \code{ipc continue} also exits cleanly after the current iteration.
#'
#' @param body      Loop body expression.
#' @param label     Label shown by \code{ipc list}.
#' @param state_every Write state every \emph{n} iterations (default 1).
#' @return Invisibly \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' make_loss <- function(i, n) max(0, exp(-3*(i-1)/n) + rnorm(1, sd=0.05))
#'
#' i <- 0L
#' ipc_repeat({
#'   i    <- i + 1L
#'   loss <- make_loss(i, 200)
#'   ipc_track(loss = loss)
#'   if (loss < 0.01) break
#' }, label = "training")
#' }
ipc_repeat <- function(body, label = "", state_every = 1L) {
    body_raw  <- substitute(body)
    body_rw   <- .rewrite_break_next(body_raw)
    caller    <- parent.frame()
    pid       <- Sys.getpid()

    if (!nzchar(label)) label <- .default_label()
    ctx         <- .ipc_install(pid, label)
    start_ts    <- as.numeric(Sys.time())
    iter_count  <- 0L
    state_every <- max(1L, as.integer(state_every))

    on.exit(.ipc_teardown(pid, iter_count, NULL, start_ts, ctx), add = TRUE)

    repeat {
        .ipc_process_commands(pid, ctx, caller)
        if (isTRUE(ctx$should_stop)) break
        iter_count <- iter_count + 1L
        result <- .ipc_eval_body(body_rw, caller)
        if (identical(result, "break")) break
        if (identical(result, "next"))  next
        if (iter_count %% state_every == 0L)
            .ipc_write_state(pid, iter_count, NULL, start_ts, ctx$tracked)
    }

    invisible(NULL)
}
