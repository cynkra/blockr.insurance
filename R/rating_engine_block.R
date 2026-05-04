#' Rating engine block
#'
#' Generic blockr wrapper around a *rating engine* — any function with the
#' signature `engine(inputs, params) -> outputs`, where `inputs` and
#' `outputs` are named lists of data frames (or `dm` objects) and `params` is
#' an optional named list of parameter tables.
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
#' @param engine Character; name of the engine function. Defaults to
#'   `"engine_property"` (see [engine_property()]).
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
#'         pricing   = new_rating_engine_block()
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
new_rating_engine_block <- function(
  engine  = "engine_property",
  package = "blockr.insurance",
  ...
) {

  stopifnot(
    is.character(engine),  length(engine)  == 1L, nzchar(engine),
    is.character(package), length(package) == 1L
  )

  engine_call_head <- if (nzchar(package)) {
    call("::", as.name(package), as.name(engine))
  } else {
    as.name(engine)
  }

  blockr.core::new_transform_block(

    server = function(id, ...args) { # nolint object_name_linter
      shiny::moduleServer(id, function(input, output, session) {

        r_engine  <- shiny::reactiveVal(engine)
        r_package <- shiny::reactiveVal(package)

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
      shiny::tagList(
        shiny::div(
          class = "block-container",
          shiny::tags$p(
            class = "text-muted mb-0",
            "Rating engine: ",
            shiny::tags$code(
              if (nzchar(package)) paste0(package, "::", engine) else engine
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
    class = c("rating_engine_block", "dm_block"),
    ...
  )
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
