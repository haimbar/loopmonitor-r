# loopmonitor (R package)

R package providing drop-in loop wrappers (`ipc_for`, `ipc_while`,
`ipc_repeat`) that let you inspect and control long-running R loops from a
second terminal — the same way you would with a Python process.

From any terminal, while the loop is running:

```bash
ipc list                    # see all monitored processes
ipc peek <pid>              # print current iteration, tracked values, ETA
ipc set  <pid> lr=1e-4      # inject a new hyperparameter mid-run
ipc continue <pid>          # exit cleanly after the current iteration
ipc break <pid>             # save a state snapshot and exit immediately
ipc plot <pid>              # open a live line-chart of tracked values
ipc checkpoint <pid>        # save snapshot without stopping
```

---

## Prerequisites

### Python ≥ 3.8 and the `loopmonitor` CLI

The `ipc` command-line tool that drives this package is part of the Python
`loopmonitor` package.  Install it first:

```bash
pip install loopmonitor
```

This makes the `ipc` command available on your `PATH`.  Verify with:

```bash
ipc --help
```

> **Note for conda/virtualenv users:** install into the environment that is
> active in the terminal you will use to issue `ipc` commands.  The R process
> itself does not need to run inside that environment.

### R ≥ 4.1

The package depends on [`jsonlite`](https://cran.r-project.org/package=jsonlite)
(pulled in automatically by the install steps below).

---

## Installation

### From GitHub (recommended)

```r
# install devtools if you don't have it
install.packages("devtools")

devtools::install_github("haimbar/loopmonitor-r")
```

Or with [pak](https://pak.r-lib.org/):

```r
pak::pkg_install("haimbar/loopmonitor-r")
```

### From a local clone

```r
# after: git clone https://github.com/haimbar/loopmonitor-r
devtools::install("loopmonitor-r")
```

---

## Quick start

Replace ordinary loops with the `ipc_*` equivalents:

```r
library(loopmonitor)

ipc_for(epoch, 1:100, {
  loss <- train_one_epoch(epoch)
  ipc_track(loss = loss, lr = current_lr())
}, label = "training")
```

From another terminal:

```bash
ipc list              # find the PID
ipc peek <pid>        # current iteration, loss, lr, ETA
ipc set  <pid> lr=1e-4
ipc continue <pid>
```

---

## Loop functions

### `ipc_for(var, iter, body, label = "", state_every = 1L)`

Drop-in for `for`.  `break` and `next` work as expected.

```r
ipc_for(i, 1:1000, {
  loss <- train_step(i)
  ipc_track(loss = loss)
  if (loss < 1e-4) break
}, label = "training", state_every = 10L)
```

`state_every` controls how often the state file is written (default: every
iteration).  Set it higher for tight inner loops.

### `ipc_while(condition, body, label = "", state_every = 1L)`

Drop-in for `while`.  The condition is re-evaluated in the caller's
environment each iteration.

```r
loss <- Inf
ipc_while(loss > 1e-4, {
  loss <- train_step()
  ipc_track(loss = loss)
}, label = "training")
```

### `ipc_repeat(body, label = "", state_every = 1L)`

Drop-in for `repeat`.  Use `break` in the body to exit, or send
`ipc continue` from the terminal.

```r
ipc_repeat({
  loss <- train_step()
  ipc_track(loss = loss)
  if (loss < 1e-4) break
}, label = "training")
```

---

## Helper functions

### `ipc_track(...)`

Record named values for display with `ipc peek` and `ipc plot`.  Call it
anywhere inside the loop body; multiple calls per iteration are merged.

```r
ipc_track(loss = 0.42, accuracy = 0.91, lr = 0.001)
```

Scalar values appear as text in `ipc peek`; vectors appear as line charts in
`ipc plot`.

### `ipc_get(key, default = NULL)`

Read a value injected by `ipc set`.  Use this to pick up hyperparameter
changes mid-run:

```r
ipc_for(i, 1:1000, {
  lr <- ipc_get("lr", default = 0.01)   # updated live by: ipc set <pid> lr=1e-4
  loss <- train_step(lr = lr)
  ipc_track(loss = loss, lr = lr)
}, label = "training")
```

---

## Available `ipc` commands

All commands work identically for R and Python processes.

| Command | Effect |
|---------|--------|
| `ipc list` | List all monitored processes (LANG column shows Py / R) |
| `ipc peek <pid>` | Print current state: iteration, tracked values, ETA |
| `ipc tail <pid>` | Stream live status to your terminal every N seconds |
| `ipc plot <pid>` | Open a plot window of tracked values over time |
| `ipc set <pid> key=value` | Inject a value readable via `ipc_get()` |
| `ipc continue <pid>` | Exit loop cleanly after the current iteration |
| `ipc break <pid>` | Save state to JSON and exit immediately |
| `ipc checkpoint <pid>` | Save state snapshot without stopping |
| `ipc stack <pid>` | Print the R call stack |
| `ipc memory <pid>` | Print current RSS memory usage |
| `ipc notify <pid> "expr"` | Desktop alert when a condition is satisfied |
| `ipc pause <pid>` | Suspend the process (SIGSTOP) |
| `ipc resume <pid>` | Resume a suspended process (SIGCONT) |

### How commands are delivered to R

Python processes receive commands via `SIGUSR1` + a named FIFO.  R's batch
mode repurposes `SIGUSR1` (save-workspace-and-quit), so R processes use a
**polled command file** instead: the CLI writes one command per line to
`~/.ipc/<pid>.cmd`; `ipc_for` / `ipc_while` / `ipc_repeat` reads and acts on
it at each iteration boundary without needing a signal.

---

## `break` and `next`

Both work exactly as in ordinary R loops.  Internally, `break` and `next` are
rewritten in the body AST before evaluation (R's C-level `LONGJMP` for
`break`/`next` cannot cross the `eval()` boundary used by the loop wrappers).
Nested loops and function definitions inside the body are left unchanged: their
own `break`/`next` remain native.

---

## State file format

Same JSON layout as the Python package (`~/.ipc/<pid>.state.json`):

```json
{
  "pid": 12345,
  "iteration": 42,
  "total": 100,
  "elapsed_sec": 18.3,
  "eta_sec": 24.9,
  "tracked": { "loss": 0.38, "lr": 0.001 },
  "updated": "2025-06-01T12:00:00.000000+00:00"
}
```

---

## Windows

Run R inside WSL2 and install the Python `loopmonitor` package there.  The
`~/.ipc/` directory is shared within the same WSL2 instance, so `ipc` and the
R process communicate normally.

---

## Related

- [loopmonitor (Python)](https://github.com/haimbar/IPC) — the companion Python
  package and the source of the `ipc` CLI tool
- [PyPI: loopmonitor](https://pypi.org/project/loopmonitor/)
