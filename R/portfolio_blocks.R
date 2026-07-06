#' Portfolio overview block
#'
#' Data block exposing [read_portfolio_overview()] as a blockr root. Has no
#' upstream inputs; the portfolio directory is fixed at construction time and
#' the block re-reads it on every render.
#'
#' Pair with a downstream `new_select_block()` / table view to drive a
#' per-policy drilldown — pass the chosen `policy_id` on to a
#' [new_policy_loader_block()] in another workspace.
#'
#' @param dir Portfolio directory. Defaults to the bundled
#'   `inst/extdata/portfolio-property/` 5-policy fixture.
#' @param ... Forwarded to [blockr.core::new_data_block()].
#'
#' @export
new_portfolio_overview_block <- function(dir = default_portfolio_dir(), ...) {
  stopifnot(is.character(dir), length(dir) == 1L)

  blockr.core::new_data_block(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        r_dir <- shiny::reactiveVal(dir)
        list(
          expr = shiny::reactive({
            bquote(
              blockr.insurance::read_portfolio_overview(.(d)),
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
          "Portfolio: ",
          shiny::tags$code(dir)
        )
      )
    },
    class = "portfolio_overview_block",
    allow_empty_state = TRUE,
    ...
  )
}

#' Portfolio premium block
#'
#' Data block that returns the portfolio-wide `premium.csv` (one row per
#' insured location across every policy, with a `policy_id` column). This
#' is the canonical input for the `portfolio-explorer.R` example.
#'
#' @param dir Portfolio directory. Defaults to the bundled
#'   `inst/extdata/portfolio-property/` fixture.
#' @param ... Forwarded to [blockr.core::new_data_block()].
#'
#' @export
new_portfolio_premium_block <- function(dir = default_portfolio_dir(), ...) {
  stopifnot(is.character(dir), length(dir) == 1L)

  blockr.core::new_data_block(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        r_dir <- shiny::reactiveVal(dir)
        list(
          expr = shiny::reactive({
            bquote(
              blockr.insurance::read_portfolio_premium(.(d)),
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
          "Portfolio: ",
          shiny::tags$code(dir)
        )
      )
    },
    class = "portfolio_premium_block",
    allow_empty_state = TRUE,
    ...
  )
}

#' Policy loader block
#'
#' Data block that reads every input and output CSV for a single policy and
#' exposes them as a `dm`. The policy id is picked from a `selectInput`
#' populated by [list_policies()] at construction time. Wire downstream
#' [blockr.dm::new_dm_pull_block()] blocks to pull individual tables
#' (`locations`, `claims`, `base_premium`, `model_price`, ...) for display.
#'
#' @param dir Portfolio directory. Defaults to the bundled
#'   `inst/extdata/portfolio-property/` 5-policy fixture.
#' @param policy_id Initially selected policy id. Defaults to the first
#'   policy in `dir`.
#' @param ... Forwarded to [blockr.core::new_data_block()].
#'
#' @export
new_policy_loader_block <- function(dir = default_portfolio_dir(),
                                    policy_id = NULL,
                                    ...) {
  stopifnot(is.character(dir), length(dir) == 1L)

  policies <- list_policies(dir)
  if (!length(policies)) {
    stop("Portfolio directory has no policies: ", dir)
  }
  if (is.null(policy_id) || !length(policy_id) || !nzchar(policy_id)) {
    policy_id <- policies[1L]
  }

  blockr.core::new_data_block(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {

        r_dir       <- shiny::reactiveVal(dir)
        r_policy_id <- shiny::reactiveVal(policy_id)

        shiny::observeEvent(shiny::req(input$policy_id), {
          r_policy_id(input$policy_id)
        })

        shiny::observeEvent(shiny::req(r_policy_id()), {
          if (!identical(r_policy_id(), input$policy_id)) {
            shiny::updateSelectInput(
              session,
              "policy_id",
              choices  = list_policies(r_dir()),
              selected = r_policy_id()
            )
          }
        })

        list(
          expr = shiny::reactive({
            shiny::req(r_policy_id())
            bquote(
              local({
                .tbls <- blockr.insurance::read_policy(
                  .(d), .(p), which = "all"
                )
                dm::as_dm(.tbls)
              }),
              list(d = r_dir(), p = r_policy_id())
            )
          }),
          state = list(
            dir       = r_dir,
            policy_id = r_policy_id
          )
        )
      })
    },
    function(id) {
      shiny::tagList(
        shiny::selectInput(
          inputId  = shiny::NS(id, "policy_id"),
          label    = "Policy",
          choices  = policies,
          selected = policy_id
        ),
        shiny::tags$p(
          class = "text-muted small mb-0",
          "Reads inputs/ + outputs/ CSVs for the selected policy"
        )
      )
    },
    class = c("policy_loader_block", "dm_block"),
    allow_empty_state = TRUE,
    external_ctrl = "policy_id",
    ...
  )
}
