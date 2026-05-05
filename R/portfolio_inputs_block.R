#' Portfolio inputs block
#'
#' Data block that reads every policy's `inputs/` CSVs (locations + claims)
#' and exposes them as a single `dm` with a `policy_id` column on each
#' table. Use this with [new_price_block()] to drive a SAA-style workbench
#' where the engine partitions by `policy_id` and runs once per policy.
#'
#' Pair with the bundled fixtures via [default_portfolio_dir()] (5 policies,
#' 69 locations) — or point at any directory with the same shape.
#'
#' @param dir Portfolio directory. Defaults to the bundled
#'   `inst/extdata/portfolio-property/` 5-policy fixture.
#' @param ... Forwarded to [blockr.core::new_data_block()].
#'
#' @return A blockr data block.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   library(blockr.insurance)
#'   serve(
#'     new_board(
#'       blocks = list(
#'         inputs  = new_portfolio_inputs_block(),
#'         pricing = new_price_block()
#'       ),
#'       links = c(
#'         new_link("inputs", "pricing", "inputs")
#'       )
#'     )
#'   )
#' }
#'
#' @export
new_portfolio_inputs_block <- function(dir = default_portfolio_dir(),
                                       ...) {
  stopifnot(is.character(dir), length(dir) == 1L)

  blockr.core::new_data_block(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        r_dir <- shiny::reactiveVal(dir)
        list(
          expr = shiny::reactive({
            bquote(
              local({
                .lst <- blockr.insurance::read_portfolio_inputs(.(d))
                dm::as_dm(.lst)
              }),
              list(d = r_dir())
            )
          }),
          state = list(dir = r_dir)
        )
      })
    },
    function(id) {
      shiny::tagList(
        shiny::tags$p(
          class = "text-muted mb-0",
          "Portfolio: ", shiny::tags$code(dir)
        )
      )
    },
    class = c("portfolio_inputs_block", "dm_block"),
    allow_empty_state = TRUE,
    ...
  )
}
