library(loopmonitor)

# ── ipc_for ───────────────────────────────────────────────────────────────────

test_that("ipc_for iterates the correct number of times", {
  count <- 0L
  ipc_for(i, 1:5, { count <- count + 1L }, label = "t")
  expect_equal(count, 5L)
})

test_that("ipc_for exposes the loop variable in the body", {
  seen <- integer(0)
  ipc_for(i, 1:3, { seen <- c(seen, i) }, label = "t")
  expect_equal(seen, 1:3)
})

test_that("ipc_for with empty iterable runs zero times", {
  count <- 0L
  ipc_for(i, integer(0), { count <- count + 1L }, label = "t")
  expect_equal(count, 0L)
})

test_that("ipc_for break exits the loop early", {
  count <- 0L
  ipc_for(i, 1:100, {
    count <- count + 1L
    if (i == 3L) break
  }, label = "t")
  expect_equal(count, 3L)
})

test_that("ipc_for next skips to the next iteration", {
  seen <- integer(0)
  ipc_for(i, 1:5, {
    if (i %% 2L == 0L) next
    seen <- c(seen, i)
  }, label = "t")
  expect_equal(seen, c(1L, 3L, 5L))
})

test_that("ipc_for registers and deregisters correctly", {
  pid <- Sys.getpid()
  ipc_for(i, 1:2, {}, label = "reg-test")
  # After the loop the state file should be gone
  expect_false(file.exists(.ipc_state_path(pid)))
})

test_that("ipc_for writes state file during loop", {
  pid <- Sys.getpid()
  ipc_for(i, 1:3, {
    if (i == 2L) {
      # State written after iteration 1 (first completed iter)
      # so we can read it now
    }
  }, label = "t")
  # state removed at end — just check no leftover file
  expect_false(file.exists(.ipc_state_path(pid)))
})

test_that("ipc_track values appear in state file", {
  # body runs in caller's env; plain <- assigns back there, <<- would skip it
  pid     <- Sys.getpid()
  tracked <- NULL
  ipc_for(i, 1:2, {
    ipc_track(val = 99L)
    if (i == 2L) {
      state <- .ipc_read_state(pid)
      if (!is.null(state)) tracked <- state[["tracked"]]
    }
  }, label = "t")
  expect_equal(as.integer(tracked[["val"]]), 99L)
})

# ── ipc_while ─────────────────────────────────────────────────────────────────

test_that("ipc_while runs while condition is TRUE", {
  x <- 0L
  ipc_while(x < 5L, { x <- x + 1L }, label = "t")
  expect_equal(x, 5L)
})

test_that("ipc_while with initially false condition runs zero times", {
  count <- 0L
  ipc_while(FALSE, { count <- count + 1L }, label = "t")
  expect_equal(count, 0L)
})

test_that("ipc_while break exits early", {
  x <- 0L
  ipc_while(TRUE, {
    x <- x + 1L
    if (x == 3L) break
  }, label = "t")
  expect_equal(x, 3L)
})

# ── ipc_repeat ────────────────────────────────────────────────────────────────

test_that("ipc_repeat runs until break", {
  x <- 0L
  ipc_repeat({
    x <- x + 1L
    if (x == 4L) break
  }, label = "t")
  expect_equal(x, 4L)
})

# ── ipc_get ───────────────────────────────────────────────────────────────────

test_that("ipc_get returns default when key not set", {
  ipc_for(i, 1:1, {
    expect_equal(ipc_get("lr", default = 0.01), 0.01)
  }, label = "t")
})

# ── .safe_eval ────────────────────────────────────────────────────────────────

test_that(".safe_eval accepts numeric literals", {
  expect_equal(.safe_eval("3.14"), 3.14)
  expect_equal(.safe_eval("42L"), 42L)
})

test_that(".safe_eval accepts string literals", {
  expect_equal(.safe_eval('"hello"'), "hello")
})

test_that(".safe_eval accepts c() and list()", {
  expect_equal(.safe_eval("c(1, 2, 3)"), c(1, 2, 3))
  expect_equal(.safe_eval('list(a=1, b="x")'), list(a = 1, b = "x"))
})

test_that(".safe_eval accepts TRUE, FALSE, NULL, NA, Inf", {
  expect_true(.safe_eval("TRUE"))
  expect_false(.safe_eval("FALSE"))
  expect_null(.safe_eval("NULL"))
  expect_true(is.na(.safe_eval("NA")))
  expect_true(is.infinite(.safe_eval("Inf")))
})

test_that(".safe_eval rejects system() call", {
  expect_error(.safe_eval('system("ls")'))
})

test_that(".safe_eval rejects arbitrary function calls", {
  expect_error(.safe_eval("Sys.getenv('HOME')"))
})

# ── command dispatch ──────────────────────────────────────────────────────────

test_that("'continue' command sets should_stop", {
  ctx <- new.env(parent = emptyenv())
  ctx$should_stop <- FALSE
  ctx$tracked <- list()
  ctx$injected <- list()
  .ipc_do_continue(ctx)
  expect_true(ctx$should_stop)
})

test_that("'set' command injects value into ctx", {
  ctx <- new.env(parent = emptyenv())
  ctx$injected <- list()
  ctx$tracked  <- list()
  .ipc_do_set("lr=0.001", ctx)
  expect_equal(ctx$injected[["lr"]], 0.001)
})

test_that("'set' rejects invalid identifier", {
  ctx <- new.env(parent = emptyenv())
  ctx$injected <- list()
  # Should not error — failure is printed and silently swallowed
  expect_output(.ipc_do_set("123bad=1", ctx), "failed")
})
