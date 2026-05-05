#' Partition a multi-policy engine call by `policy_id`
#'
#' Internal helper used by [engine_property()] and [engine_property_v2()] to
#' transparently support both single-policy and multi-policy inputs.
#'
#' If `inputs$locations` has a `policy_id` column, the engine is invoked
#' once per distinct policy (with that policy's slice of `locations` and
#' `claims`, where `policy_id` is preserved in claims if present), and the
#' per-policy `premium` tables are row-bound with a `policy_id` column on
#' each row. If there is no `policy_id` column, the engine is called once
#' on the full inputs (the single-policy / pooled-portfolio path).
#'
#' Not exported — engines call this internally via
#' [engine_property()] / [engine_property_v2()].
#'
#' @param engine_fn The pure rating-engine function (e.g.
#'   [engine_property()]'s body, factored out as `engine_property_one`).
#' @param inputs Named list with `locations` (data frame) and `claims`
#'   (data frame).
#' @param params Named list of parameter tables, or NULL.
#'
#' @return A named list with `premium` — a data frame, possibly carrying a
#'   `policy_id` column when partitioned.
#'
#' @keywords internal
#' @noRd
partition_by_policy <- function(engine_fn, inputs, params) {

  locations <- as.data.frame(inputs[["locations"]])
  claims    <- as.data.frame(inputs[["claims"]])

  if (!"policy_id" %in% names(locations)) {
    return(engine_fn(inputs, params))
  }

  pids <- unique(locations$policy_id)
  claims_has_pid <- "policy_id" %in% names(claims)

  per_policy <- lapply(pids, function(pid) {
    loc_p <- locations[locations$policy_id == pid, , drop = FALSE]
    cls_p <- if (claims_has_pid) {
      claims[claims$policy_id == pid, , drop = FALSE]
    } else {
      claims[0L, , drop = FALSE]  # no claims for this policy
    }
    out <- engine_fn(
      list(locations = loc_p, claims = cls_p),
      params
    )$premium
    out$policy_id <- pid
    out
  })

  list(premium = do.call(rbind, per_policy))
}
