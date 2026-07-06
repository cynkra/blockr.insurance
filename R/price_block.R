#' Price block — pick an engine, run on inputs (and optional parameters)
#'
#' Generic blockr wrapper around a *rating engine* — any function with the
#' signature `engine(inputs, params) -> outputs`, where `inputs` and
#' `outputs` are named lists of data frames (or `dm` objects) and `params` is
#' an optional named list of parameter tables.
#'
#' The engine is **selectable at runtime** via a dropdown in the block UI.
#' Toggling the engine re-evaluates the block — same inputs, different
#' calculation. This is the lever the SAA workbench demo points at when
#' walking through "version 1 vs version 2" of a pricing engine.
#'
#' The block is variadic and accepts up to two upstream inputs:
#'
#' - `inputs` (required): a `dm` or named list with the engine's input
#'   tables. Wire it with `new_link(..., input = "inputs")`.
#' - `params` (optional): a `dm` or named list with the engine's parameter
#'   tables. Wire it with `new_link(..., input = "params")`. If absent,
#'   the engine's own default parameters are used.
#'
#' The engine's output (a named list of tables) is wrapped in a `dm` so the
#' block can be chained with `new_dm_pull_block()` to extract individual
#' result tables for inspection or downstream visualisation.
#'
#' @param engine Character; default engine function name. Must be one of
#'   [available_engines()]. Defaults to the first available
#'   (`"engine_property"`).
#' @param package Character; package the engine lives in. Defaults to
#'   `"blockr.insurance"`. May be `""` for a function in the global
#'   environment.
#' @param ... Forwarded to [blockr.core::new_transform_block()].
#'
#' @return A blockr transform block.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   library(blockr.insurance)
#'   serve(
#'     new_board(
#'       blocks = list(
#'         locations = new_dataset_block("property_locations",
#'                                       "blockr.insurance"),
#'         claims    = new_dataset_block("property_claims",
#'                                       "blockr.insurance"),
#'         inputs    = new_dm_block(infer_keys = FALSE),
#'         pricing   = new_price_block()
#'       ),
#'       links = c(
#'         new_link("locations", "inputs",  "locations"),
#'         new_link("claims",    "inputs",  "claims"),
#'         new_link("inputs",    "pricing", "inputs")
#'       )
#'     )
#'   )
#' }
#'
#' @export
new_price_block <- function(
  engine  = available_engines()[[1L]],
  package = "blockr.insurance",
  ...
) {

  stopifnot(
    is.character(engine),  length(engine)  == 1L, nzchar(engine),
    is.character(package), length(package) == 1L
  )

  engines <- available_engines()
  if (!engine %in% engines) {
    stop("`engine` must be one of: ", paste(engines, collapse = ", "))
  }

  blockr.core::new_transform_block(

    server = function(id, ...args) { # nolint object_name_linter
      shiny::moduleServer(id, function(input, output, session) {

        r_engine  <- shiny::reactiveVal(engine)
        r_package <- shiny::reactiveVal(package)

        shiny::observeEvent(input$engine_select, {
          if (!is.null(input$engine_select) &&
              nzchar(input$engine_select) &&
              input$engine_select != r_engine()) {
            r_engine(input$engine_select)
          }
        })

        list(
          expr = shiny::reactive({
            shiny::req(length(...args) >= 1L)

            slot_ids    <- names(...args)
            display_nms <- dot_args_names(...args)

            if (is.null(display_nms)) {
              display_nms <- if (length(slot_ids) == 1L) {
                "inputs"
              } else {
                c("inputs", rep_len("params", length(slot_ids) - 1L))
              }
            }

            data_args <- stats::setNames(
              lapply(slot_ids, as_dot_call),
              display_nms
            )

            engine_call_head <- if (nzchar(r_package())) {
              call("::", as.name(r_package()), as.name(r_engine()))
            } else {
              as.name(r_engine())
            }

            engine_call <- as.call(c(list(engine_call_head), data_args))

            bquote(
              local({
                .res <- .(eng)
                if (inherits(.res, "dm")) .res else dm::as_dm(.res)
              }),
              list(eng = engine_call)
            )
          }),
          state = list(
            engine  = r_engine,
            package = r_package
          )
        )
      })
    },

    ui = function(id) {
      ns <- shiny::NS(id)
      engines <- available_engines()
      options_json <- jsonlite::toJSON(
        lapply(engines, function(e) list(value = e)),
        auto_unbox = TRUE
      )
      shiny::tagList(
        price_block_dep(),
        shiny::div(
          class = "block-container",
          # Aesthetic-row layout — same shape as blockr.viz::aesthetic_row in
          # tile-block.R: flex row, no border on the outer container, label
          # on the left as plain text, bordered control on the right. The
          # Blockr.Select inside carries the only visible border.
          shiny::div(
            style = paste("display: flex; gap: 8px; flex-wrap: wrap;",
                          "align-items: center; margin-bottom: 6px;"),
            shiny::tags$label(
              "Engine version",
              `for` = ns("engine_select"),
              style = paste("flex: 0 0 auto; margin: 0;",
                            "font-size: 0.8125rem; color: #6b7280;",
                            "font-weight: 500; white-space: nowrap;")
            ),
            shiny::div(
              style = "flex: 1 1 140px; min-width: 0;",
              shiny::div(
                id   = ns("engine_select"),
                `data-blockr-price-select` = "",
                `data-options`             = options_json,
                `data-selected`            = engine
              )
            )
          )
        )
      )
    },

    dat_valid = function(...args) { # nolint object_name_linter
      if (length(...args) < 1L) {
        stop("`inputs` slot must be wired")
      }
      for (a in ...args) {
        if (!is.data.frame(a) && !is.list(a) && !inherits(a, "dm")) {
          stop("Each input must be a data frame, list, or dm object")
        }
      }
    },

    allow_empty_state = TRUE,
    expr_type = "bquoted",
    class = c("price_block", "dm_block"),
    ...
  )
}

#' Available engines for `new_price_block()`
#'
#' List of engine function names exposed by `blockr.insurance` to
#' [new_price_block()]'s engine dropdown. Add a new engine here when shipping
#' a new property pricing variant.
#'
#' @return Character vector of engine function names.
#'
#' @examples
#' available_engines()
#'
#' @export
available_engines <- function() {
  c("engine_property", "engine_property_v2")
}

# Helper copied from blockr.core (not exported there)
dot_args_names <- function(x) {
  res <- names(x)
  unnamed <- grepl("^[1-9][0-9]*$", res)
  if (all(unnamed)) {
    return(NULL)
  }
  if (any(unnamed)) {
    return(replace(res, unnamed, ""))
  }
  res
}

# Mirror of blockr.core::as_dot_call (not exported)
as_dot_call <- function(x) {
  call(".", as.name(x))
}
