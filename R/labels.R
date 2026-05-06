#' Apply ADaM-style column labels to a data frame
#'
#' Sets `attr(<col>, "label")` on every column for which a label is known
#' in [property_labels()]. Surfaces friendly column names in blocks that
#' read column labels (e.g. `drilldown_chart_block` reads
#' `attr(values, "label")` and shows it next to the column name in
#' selectors).
#'
#' @param x A data frame (typically the engine `premium` output).
#' @param labels Named character vector — names are column names, values
#'   are the labels. Defaults to [property_labels()].
#'
#' @return `x`, with `attr(<col>, "label")` set for known columns.
#'
#' @examples
#' inp <- list(
#'   locations = blockr.insurance::property_locations,
#'   claims    = blockr.insurance::property_claims
#' )
#' out <- engine_property(inp)$premium
#' attr(out$model_price, "label")
#' # "Model Price"
#'
#' @export
apply_labels <- function(x, labels = property_labels()) {
  if (!is.data.frame(x)) return(x)
  cols <- intersect(names(x), names(labels))
  for (col in cols) {
    attr(x[[col]], "label") <- labels[[col]]
  }
  x
}

#' Column labels for the property pricing engine
#'
#' Named character vector mapping column names produced by
#' [engine_property()] / [engine_property_v2()] / [read_portfolio_inputs()]
#' to human-readable labels (ADaM convention — short, capitalised).
#'
#' @return Named character vector.
#'
#' @examples
#' property_labels()[c("model_price", "tiv")]
#'
#' @export
property_labels <- function() {
  c(
    # Identifiers
    location_id            = "Location",
    policy_id              = "Policy",
    claim_id               = "Claim",
    country                = "Country",
    peril                  = "Peril",
    postcode               = "Postcode",
    date_of_loss           = "Date of Loss",

    # Inputs
    tiv                    = "Total Insured Value",
    sra_adj                = "Site Risk Adjustment",
    deductible             = "Deductible",
    limit                  = "Limit",
    paid                   = "Paid",
    outstanding            = "Outstanding",

    # Engine factors
    country_factor         = "Country Factor",
    base_rate              = "Base Rate",
    cat_factor             = "CAT Factor",
    layer_share            = "Layer Share",
    expenses_factor        = "Expenses Factor",
    credibility            = "Credibility",

    # Premium cascade
    base_premium           = "Base Premium",
    exposure_premium       = "Exposure Premium",
    experience_premium     = "Experience Premium",
    risk_premium           = "Risk Premium",
    model_price            = "Model Price",

    # Aggregates broadcast onto every row
    total_incurred         = "Total Incurred",
    n_claim_years          = "Claim Years",
    exposure_premium_total = "Total Exposure Premium"
  )
}
