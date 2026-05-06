#' Property pricing engine
#'
#' A minimal property-insurance rating engine. Pure R, no Shiny: takes a list
#' of input tables (locations and claims) plus an optional list of parameter
#' tables, and returns a single wide table at the location grain. Per-location
#' columns (e.g. `base_premium`, `exposure_premium`) carry their location
#' value; portfolio-aggregate columns (e.g. `risk_premium`, `model_price`) are
#' broadcast onto every row so a single crossfilter slice keeps the full
#' comparison schema available downstream.
#'
#' This is the standard rating-engine signature used in this package: a pure
#' function `engine(inputs, params) -> outputs` where `outputs` is a named
#' list of data frames. [new_price_block()] turns any such function
#' into a blockr block.
#'
#' @param inputs Named list (or `dm`) with two tables:
#'   - `locations`: one row per insured location.
#'     Required columns: `location_id`, `country`, `tiv`, `sra_adj`,
#'     `deductible`, `limit`.
#'   - `claims`: one row per claim.
#'     Required columns: `claim_id`, `date_of_loss`, `country`, `paid`,
#'     `outstanding`.
#' @param params Named list (or `dm`) of parameter tables. If `NULL`, the
#'   package defaults from [property_params] are used. Expected tables:
#'   - `country_factor`: `country`, `country_factor`.
#'   - `base_rate`: `country`, `base_rate`.
#'   - `expenses`: a single-row table with `expenses_factor`.
#'
#' @return A named list with one element, `premium`: a data frame with one
#'   row per insured location and the following columns:
#'   - Location attributes carried through (any column on `locations` is
#'     passed through): `location_id`, `country`, `peril`, `postcode`,
#'     `tiv`, `sra_adj`, `deductible`, `limit`.
#'   - Joined parameters: `country_factor`, `base_rate`.
#'   - Per-location: `base_premium`, `layer_share`, `exposure_premium`.
#'   - Per-location, allocated from policy-grain aggregates pro-rata by
#'     `exposure_premium`: `experience_premium`, `risk_premium`,
#'     `model_price`. Summing each across all locations of a policy
#'     reproduces the policy aggregate.
#'   - Broadcast scalars (constant per policy): `total_incurred`,
#'     `n_claim_years`, `exposure_premium_total`, `credibility`,
#'     `expenses_factor`. Carried through as metadata; not intended to be
#'     summed across locations.
#'
#' @examples
#' inp <- list(
#'   locations = blockr.insurance::property_locations,
#'   claims    = blockr.insurance::property_claims
#' )
#' out <- engine_property(inp)
#' head(out$premium)
#'
#' @export
engine_property <- function(inputs, params = NULL) {

  if (inherits(inputs, "dm")) inputs <- as.list(dm::dm_get_tables(inputs))
  if (inherits(params, "dm")) params <- as.list(dm::dm_get_tables(params))
  if (is.null(params)) {
    e <- new.env()
    utils::data("property_params",
                package = "blockr.insurance", envir = e)
    params <- e$property_params
  }

  out <- if ("policy_id" %in% names(as.data.frame(inputs[["locations"]]))) {
    partition_by_policy(engine_property_one, inputs, params)
  } else {
    engine_property_one(inputs, params)
  }
  out$premium <- apply_labels(out$premium)
  out
}

engine_property_one <- function(inputs, params) {

  locations <- as.data.frame(inputs[["locations"]])
  claims    <- as.data.frame(inputs[["claims"]])

  country_factor <- as.data.frame(params[["country_factor"]])
  base_rate      <- as.data.frame(params[["base_rate"]])
  expenses       <- as.data.frame(params[["expenses"]])

  # Per-location: base + layer share + exposure premium --------------------
  per_location <- locations |>
    dplyr::left_join(country_factor, by = "country") |>
    dplyr::left_join(base_rate,      by = "country") |>
    dplyr::mutate(
      country_factor = dplyr::coalesce(.data$country_factor, 1),
      base_rate      = dplyr::coalesce(.data$base_rate,      0.001),
      base_premium   = .data$tiv * .data$country_factor *
                       .data$base_rate * .data$sra_adj,
      layer_share    = pmax(
        0,
        (pmin(.data$limit, .data$tiv) - .data$deductible) /
          pmax(.data$tiv, 1)
      ),
      exposure_premium = .data$base_premium * .data$layer_share
    )

  # Aggregate quantities (broadcast onto every row) ------------------------
  exposure_total <- sum(per_location$exposure_premium, na.rm = TRUE)

  if (nrow(claims) > 0L) {
    claims_yrs   <- as.integer(format(claims$date_of_loss, "%Y"))
    incurred     <- claims$paid + claims$outstanding
    n_claim_yrs  <- length(unique(stats::na.omit(claims_yrs)))
    total_incrd  <- sum(incurred, na.rm = TRUE)
  } else {
    n_claim_yrs <- 0L
    total_incrd <- 0
  }
  experience_total <- total_incrd / max(n_claim_yrs, 1L)

  credibility <- 0.5
  risk_premium_total <- credibility * experience_total +
                        (1 - credibility) * exposure_total

  expenses_factor <- if (nrow(expenses) > 0L) {
    expenses$expenses_factor[1L]
  } else {
    1.25
  }

  model_price_total <- risk_premium_total * expenses_factor

  # Allocate policy-grain aggregates back to locations pro-rata by
  # `exposure_premium`. Summing the per-location columns across all
  # locations reproduces the policy aggregate. When all locations have
  # zero exposure (degenerate input), we fall back to equal allocation
  # so the totals still propagate.
  alloc_w <- if (exposure_total > 0) {
    per_location$exposure_premium / exposure_total
  } else {
    rep_len(1 / max(nrow(per_location), 1L), nrow(per_location))
  }

  premium <- per_location |>
    dplyr::mutate(
      experience_premium     = experience_total   * alloc_w,
      risk_premium           = risk_premium_total * alloc_w,
      model_price            = model_price_total  * alloc_w,
      total_incurred         = total_incrd,
      n_claim_years          = as.integer(n_claim_yrs),
      exposure_premium_total = exposure_total,
      credibility            = credibility,
      expenses_factor        = expenses_factor
    )

  list(premium = as.data.frame(premium))
}
