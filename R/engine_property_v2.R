#' Property pricing engine — v2 with CAT loading
#'
#' Sibling of [engine_property()] (not a successor). Identical signature and
#' output schema; the only difference is a per-country natural-catastrophe
#' loading applied to `exposure_premium` before experience aggregation.
#'
#' Use the two engines side-by-side via [new_price_block()] (gear popover) or
#' through `compare(v1, v2)` workflows. Demo target: SAA 2026 property
#' workbench, where the CAT loading is the lever the walkthrough toggles to
#' show how a new engine version reprices a book.
#'
#' @inheritParams engine_property
#'
#' @return A named list with one element, `premium` — same schema as
#'   [engine_property()]. v2's `risk_premium` and `model_price` are
#'   loaded per-location: `column_v2 = column_v1 * cat_factor`. The
#'   pre-loading columns (`base_premium`, `layer_share`, `exposure_premium`,
#'   `experience_premium`) are identical to v1. The waterfall step from
#'   `risk_premium` to `model_price` therefore shows the CAT effect on top
#'   of the standard `expenses_factor` markup.
#'
#' @details
#' The CAT loading reads `params$cat_factor` (a `country` x `cat_factor`
#' table). If absent or the country isn't in the table, the loading defaults
#' to 1 — i.e. v2 collapses to v1 numerically. This makes the engine safe to
#' run against the older [property_params_comparison] fixture (which doesn't
#' include `cat_factor`).
#'
#' Implementation note: CAT is applied **after** the v1 allocation step,
#' not as a multiplier on `exposure_premium`. The proposal's calculation
#' flow (Fig 2.1) places `mod_cat` between layering and experience, but
#' applying it there would interfere with v1's exposure-weighted allocation
#' of policy-grain aggregates back to locations (a high-CAT country could
#' end up with a *smaller* per-location share if its loading is below the
#' portfolio-weighted-average loading). Applying CAT post-allocation gives
#' the cleaner per-location story the demo needs: Italy locations on v2
#' are exactly `cat_factor[Italy]` times their v1 model_price.
#'
#' @examples
#' inp <- list(
#'   locations = blockr.insurance::property_locations,
#'   claims    = blockr.insurance::property_claims
#' )
#' out_v1 <- engine_property(inp)
#' out_v2 <- engine_property_v2(inp)
#' # Italy locations get a 1.30 cat_factor; UK 1.15; etc.
#' merge(
#'   out_v1$premium[, c("location_id", "country", "exposure_premium")],
#'   out_v2$premium[, c("location_id", "exposure_premium")],
#'   by = "location_id", suffixes = c("_v1", "_v2")
#' ) |> head()
#'
#' @seealso [engine_property()] (v1), [new_price_block()], [property_params].
#' @export
engine_property_v2 <- function(inputs, params = NULL) {

  if (inherits(inputs, "dm")) inputs <- as.list(dm::dm_get_tables(inputs))
  if (inherits(params, "dm")) params <- as.list(dm::dm_get_tables(params))
  if (is.null(params)) {
    e <- new.env()
    utils::data("property_params",
                package = "blockr.insurance", envir = e)
    params <- e$property_params
  }

  if ("policy_id" %in% names(as.data.frame(inputs[["locations"]]))) {
    return(partition_by_policy(engine_property_v2_one, inputs, params))
  }

  engine_property_v2_one(inputs, params)
}

engine_property_v2_one <- function(inputs, params) {

  locations <- as.data.frame(inputs[["locations"]])
  claims    <- as.data.frame(inputs[["claims"]])

  country_factor <- as.data.frame(params[["country_factor"]])
  base_rate      <- as.data.frame(params[["base_rate"]])
  expenses       <- as.data.frame(params[["expenses"]])

  cat_factor_tbl <- if (!is.null(params[["cat_factor"]])) {
    as.data.frame(params[["cat_factor"]])
  } else {
    data.frame(country = character(), cat_factor = numeric())
  }

  # Per-location: base + layer share + exposure premium (v1-identical) -----
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
    ) |>
    dplyr::left_join(cat_factor_tbl, by = "country") |>
    dplyr::mutate(
      cat_factor   = dplyr::coalesce(.data$cat_factor, 1),
      risk_premium = .data$risk_premium * .data$cat_factor,
      model_price  = .data$model_price  * .data$cat_factor
    )

  list(premium = as.data.frame(premium))
}
