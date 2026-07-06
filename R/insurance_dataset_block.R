#' Insurance dataset block
#'
#' Thin wrapper around [blockr.core::new_dataset_block()] with the dataset
#' package locked to `blockr.insurance`. The base `new_dataset_block()`
#' exposes only the dataset dropdown — `package` is a constructor argument
#' with no UI counterpart, so an end user adding the stock block in a
#' running app cannot switch away from the `datasets` default. This wrapper
#' fixes `package = "blockr.insurance"` and defaults `dataset` to
#' `motor_portfolio` so the block can be added interactively and immediately
#' yields a usable insurance dataset.
#'
#' @param dataset Character; default dataset. Must be one of the
#'   `data.frame`-eligible datasets shipped by `blockr.insurance`.
#' @param ... Forwarded to [blockr.core::new_dataset_block()].
#'
#' @return A blockr data block.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.insurance)
#'   serve(
#'     new_board(
#'       blocks = list(motor = new_insurance_dataset_block())
#'     )
#'   )
#' }
#'
#' @export
new_insurance_dataset_block <- function(dataset = "motor_portfolio", ...) {
  dots <- list(...)
  # `package` is locked below; drop any value coming through `...` (notably
  # from the deserialised state payload) to avoid a duplicate-arg error in
  # `blockr_deser.block()`.
  dots$package <- NULL
  if (is.null(dots$ctor)) {
    dots$ctor <- sys.function()
  }

  blk <- do.call(
    blockr.core::new_dataset_block,
    c(list(dataset = dataset, package = blockr.core::pkg_name()), dots)
  )
  class(blk) <- c("insurance_dataset_block", class(blk))
  blk
}
