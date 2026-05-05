#' Portfolio runs over many policies
#'
#' A *portfolio* is a directory of policies, each in its own subfolder. Every
#' policy folder has an `inputs/` subdirectory with one CSV per input table
#' (e.g. `locations.csv`, `claims.csv`) and an `outputs/` subdirectory which
#' [run_portfolio()] populates with one CSV per result table (currently just
#' `premium.csv`). Once every policy is run, [run_portfolio()] also writes a
#' single portfolio-root `premium.csv` that binds every per-policy result
#' with an added `policy_id` column.
#'
#' ```
#' my-portfolio/
#'   premium.csv                       # portfolio aggregate (added by run_portfolio)
#'   policy-001/
#'     inputs/locations.csv
#'     inputs/claims.csv
#'     outputs/premium.csv
#'   policy-002/
#'     ...
#' ```
#'
#' The functions in this file are pure R (no Shiny). The blockr wrappers that
#' expose them in a dashboard live in `R/portfolio_blocks.R`.
#'
#' @name portfolio
NULL

#' Path to the bundled fixture portfolios
#'
#' Convenience helpers used as default `dir` arguments by the portfolio
#' blocks and the `portfolio-explorer.R` example.
#'
#' @return File path to one of the bundled fixture portfolios under
#'   `inst/extdata/`.
#'
#' @export
default_portfolio_dir <- function() {
  system.file("extdata", "portfolio-property", package = "blockr.insurance")
}

#' @rdname default_portfolio_dir
#' @export
default_comparison_portfolio_dir <- function() {
  system.file("extdata", "portfolio-property-comparison",
              package = "blockr.insurance")
}

#' List policies in a portfolio directory
#'
#' @param dir Portfolio directory.
#'
#' @return Character vector of policy ids (the subfolder names) in
#'   alphabetical order.
#'
#' @export
list_policies <- function(dir) {
  stopifnot(is.character(dir), length(dir) == 1L)
  if (!dir.exists(dir)) {
    stop("Portfolio directory does not exist: ", dir)
  }
  entries <- list.files(dir, full.names = FALSE, no.. = TRUE)
  keep <- vapply(entries, function(x) {
    dir.exists(file.path(dir, x, "inputs"))
  }, logical(1L))
  sort(entries[keep])
}

#' Read one policy's input or output tables
#'
#' Reads every CSV under `inputs/` and `outputs/` for a single policy. Date
#' columns named `date_of_loss` are parsed as [Date]; everything else is
#' left to [utils::read.csv()] defaults.
#'
#' @param dir Portfolio directory.
#' @param policy_id Policy folder name.
#' @param which One of `"all"`, `"inputs"`, `"outputs"`. `"outputs"` returns
#'   an empty list if the policy has not been run yet.
#'
#' @return Named list of data frames.
#'
#' @export
read_policy <- function(dir, policy_id, which = c("all", "inputs", "outputs")) {
  which <- match.arg(which)
  pdir <- file.path(dir, policy_id)
  if (!dir.exists(pdir)) {
    stop("Policy folder does not exist: ", pdir)
  }

  read_one <- function(sub) {
    sdir <- file.path(pdir, sub)
    if (!dir.exists(sdir)) {
      return(list())
    }
    files <- list.files(sdir, pattern = "\\.csv$", full.names = TRUE)
    if (!length(files)) {
      return(list())
    }
    nms <- tools::file_path_sans_ext(basename(files))
    out <- lapply(files, function(f) {
      df <- utils::read.csv(f, stringsAsFactors = FALSE)
      if ("date_of_loss" %in% names(df)) {
        df$date_of_loss <- as.Date(df$date_of_loss)
      }
      df
    })
    stats::setNames(out, nms)
  }

  switch(
    which,
    inputs  = read_one("inputs"),
    outputs = read_one("outputs"),
    all     = c(read_one("inputs"), read_one("outputs"))
  )
}

#' Read all policies' inputs as a long-format named list
#'
#' Reads every policy's `inputs/` CSVs (locations + claims) and row-binds
#' them across policies, adding a `policy_id` column to each table. The
#' result feeds [engine_property()] / [engine_property_v2()] in their
#' multi-policy mode (the engine partitions by `policy_id` and runs once
#' per policy). Use this with [new_price_block()] to drive the SAA
#' workbench from the bundled portfolio fixtures.
#'
#' @param dir Portfolio directory. Defaults to the bundled
#'   `inst/extdata/portfolio-property/` 5-policy fixture.
#'
#' @return Named list with two data frames, `locations` and `claims`,
#'   each carrying a `policy_id` column.
#'
#' @export
read_portfolio_inputs <- function(dir = default_portfolio_dir()) {
  pids <- list_policies(dir)
  if (!length(pids)) {
    stop("Portfolio directory has no policies: ", dir)
  }

  locs <- vector("list", length(pids))
  clms <- vector("list", length(pids))
  for (i in seq_along(pids)) {
    inp <- read_policy(dir, pids[[i]], which = "inputs")
    if (!is.null(inp$locations)) {
      inp$locations$policy_id <- pids[[i]]
      locs[[i]] <- inp$locations
    }
    if (!is.null(inp$claims)) {
      inp$claims$policy_id <- pids[[i]]
      clms[[i]] <- inp$claims
    }
  }

  list(
    locations = do.call(rbind, locs),
    claims    = do.call(rbind, clms)
  )
}

#' Read the portfolio-wide premium table
#'
#' Returns the bound `premium.csv` at the portfolio root (one row per
#' location across every policy, with a `policy_id` column). This is the
#' table the `portfolio-explorer` example loads.
#'
#' @param dir Portfolio directory.
#'
#' @return A data frame, or `NULL` if `premium.csv` does not exist.
#'
#' @export
read_portfolio_premium <- function(dir) {
  f <- file.path(dir, "premium.csv")
  if (!file.exists(f)) return(NULL)
  utils::read.csv(f, stringsAsFactors = FALSE)
}

#' Run a rating engine over every policy in a portfolio
#'
#' For each policy folder, reads the `inputs/` CSVs, calls `engine(inputs,
#' params)`, and writes the resulting `premium` table to
#' `<policy>/outputs/premium.csv`. Once every policy is run, also writes a
#' portfolio-root `premium.csv` that row-binds every per-policy result with
#' an added `policy_id` column.
#'
#' @param dir Portfolio directory.
#' @param engine Engine function with signature `engine(inputs, params)`
#'   returning `list(premium = <data.frame>)`. Defaults to
#'   [engine_property()].
#' @param params Optional named list of parameter tables shared across all
#'   policies. If `NULL`, the engine's own defaults are used.
#' @param overwrite If `FALSE` (default), policies whose `outputs/` folder
#'   already contains CSVs are skipped (their cached output is still picked
#'   up for the portfolio aggregate).
#'
#' @return Invisibly, a character vector of policy ids that were (re)run.
#'
#' @export
run_portfolio <- function(dir,
                          engine = engine_property,
                          params = NULL,
                          overwrite = FALSE) {
  stopifnot(is.function(engine))
  policies <- list_policies(dir)
  done <- character()
  for (pid in policies) {
    out_dir <- file.path(dir, pid, "outputs")
    if (!overwrite && dir.exists(out_dir) &&
        length(list.files(out_dir, pattern = "\\.csv$"))) {
      next
    }
    inputs <- read_policy(dir, pid, which = "inputs")
    if (!length(inputs)) {
      warning("Policy ", pid, " has no inputs; skipping.")
      next
    }
    res <- engine(inputs, params)
    if (!is.list(res) || !length(res$premium)) {
      warning("Engine returned no `premium` table for policy ", pid,
              "; skipping.")
      next
    }
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(
      res$premium,
      file = file.path(out_dir, "premium.csv"),
      row.names = FALSE
    )
    done <- c(done, pid)
  }

  # Portfolio-root aggregate ------------------------------------------------
  parts <- lapply(policies, function(pid) {
    f <- file.path(dir, pid, "outputs", "premium.csv")
    if (!file.exists(f)) return(NULL)
    df <- utils::read.csv(f, stringsAsFactors = FALSE)
    cbind(policy_id = pid, df)
  })
  parts <- parts[!vapply(parts, is.null, logical(1L))]
  if (length(parts)) {
    utils::write.csv(
      do.call(rbind, parts),
      file = file.path(dir, "premium.csv"),
      row.names = FALSE
    )
  }

  invisible(done)
}

#' One-row-per-policy portfolio overview
#'
#' Summarises the portfolio-root `premium.csv` into one row per policy
#' (counts, total TIV, mean of broadcast aggregate columns). Reads
#' [read_portfolio_premium()] under the hood; returns an empty frame if the
#' aggregate hasn't been written yet.
#'
#' Columns: `policy_id`, `n_locations`, `total_tiv`, `total_incurred`,
#' `experience_premium`, `exposure_premium_total`, `risk_premium`,
#' `model_price`.
#'
#' @param dir Portfolio directory.
#'
#' @return A data frame with one row per policy.
#'
#' @export
read_portfolio_overview <- function(dir) {
  prem <- read_portfolio_premium(dir)
  if (is.null(prem) || !nrow(prem)) {
    return(empty_overview())
  }
  out <- prem |>
    dplyr::group_by(.data$policy_id) |>
    dplyr::summarise(
      n_locations            = dplyr::n(),
      total_tiv              = sum(.data$tiv, na.rm = TRUE),
      # `total_incurred` and `exposure_premium_total` are constant per
      # policy (broadcast metadata), so `first()` pulls the policy value.
      total_incurred         = dplyr::first(.data$total_incurred),
      exposure_premium_total = dplyr::first(.data$exposure_premium_total),
      # `experience_premium`, `risk_premium`, `model_price` are now
      # allocated per-location pro-rata; summing reproduces the policy
      # aggregate.
      experience_premium     = sum(.data$experience_premium, na.rm = TRUE),
      risk_premium           = sum(.data$risk_premium,       na.rm = TRUE),
      model_price            = sum(.data$model_price,        na.rm = TRUE),
      .groups = "drop"
    )
  as.data.frame(out)
}

empty_overview <- function() {
  data.frame(
    policy_id              = character(),
    n_locations            = integer(),
    total_tiv              = numeric(),
    total_incurred         = numeric(),
    experience_premium     = numeric(),
    exposure_premium_total = numeric(),
    risk_premium           = numeric(),
    model_price            = numeric(),
    stringsAsFactors       = FALSE
  )
}
