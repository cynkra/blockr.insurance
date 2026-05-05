#' @importFrom blockr.core register_blocks
#' @importFrom rlang .data
.onLoad <- function(libname, pkgname) {
  blockr.core::register_blocks(
    "new_price_block",
    name = "Price",
    description = paste(
      "Run a rating engine - any function with the signature",
      "engine(inputs, params) -> outputs - on a list of input tables",
      "and an optional list of parameter tables. The engine version is",
      "selectable at runtime via a dropdown in the block UI."
    ),
    category = "transform",
    package = pkgname,
    overwrite = TRUE
  )
  blockr.core::register_blocks(
    "new_portfolio_inputs_block",
    name = "Portfolio inputs",
    description = paste(
      "Reads every policy's inputs/ CSVs (locations + claims) and",
      "exposes them as a single dm with a policy_id column on each",
      "table. Pair with new_price_block to run an engine across the",
      "whole portfolio."
    ),
    category = "input",
    package = pkgname,
    overwrite = TRUE
  )
  blockr.core::register_blocks(
    "new_portfolio_overview_block",
    name = "Portfolio overview",
    description = paste(
      "One-row-per-policy summary of a portfolio directory: counts,",
      "premiums, model price."
    ),
    category = "input",
    package = pkgname,
    overwrite = TRUE
  )
  blockr.core::register_blocks(
    "new_portfolio_premium_block",
    name = "Portfolio premium",
    description = paste(
      "Returns the portfolio-wide premium table (one row per insured",
      "location across every policy, with a policy_id column)."
    ),
    category = "input",
    package = pkgname,
    overwrite = TRUE
  )
  blockr.core::register_blocks(
    "new_policy_loader_block",
    name = "Policy loader",
    description = paste(
      "Reads all inputs/ + outputs/ CSVs for one policy and exposes",
      "them as a dm. Pair with dm-pull blocks downstream."
    ),
    category = "input",
    package = pkgname,
    overwrite = TRUE
  )
  invisible(NULL)
}
