#' @importFrom blockr.core register_blocks
.onLoad <- function(libname, pkgname) {
  blockr.core::register_blocks(
    "new_rating_engine_block",
    name = "Rating engine",
    description = paste(
      "Run a rating engine — any function with the signature",
      "engine(inputs, params) -> outputs — on a list of input tables",
      "and an optional list of parameter tables."
    ),
    category = "transform",
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
