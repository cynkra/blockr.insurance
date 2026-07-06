#' HTML dependencies for `new_price_block()`
#'
#' Loads the blockr.dplyr select widget (`Blockr.Select`) and the
#' price-block-local Shiny binding that wires it to the engine selector.
#'
#' @return `htmltools::tagList` of dependencies.
#'
#' @keywords internal
#' @noRd
price_block_dep <- function() {
  htmltools::tagList(
    blockr.dplyr::blockr_core_js_dep(),
    blockr.dplyr::blockr_blocks_css_dep(),
    blockr.dplyr::blockr_select_dep(),
    htmltools::htmlDependency(
      name = "blockr-insurance-price-block",
      version = utils::packageVersion("blockr.insurance"),
      src = system.file("js", package = "blockr.insurance"),
      script = "price-block.js"
    )
  )
}
