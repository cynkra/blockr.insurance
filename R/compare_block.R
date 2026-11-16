utils::globalVariables(c(".", "kc", "mc", "jt", "mt"))

#' Compare Block
#'
#' Compares two data frames with the same shape by joining on key columns
#' and computing a diff metric on measurement columns. Output contains key
#' columns plus one diff column per measurement, named identically to the
#' original measurement columns.
#'
#' The UI follows the blockr.dplyr design system (JS-driven pickers built on
#' the shared `Blockr.Select` component), so both column pickers are
#' drag-reorderable and their pick order is the output column order.
#'
#' @param key_cols Character vector of columns to join on, in output order
#' @param measure_cols Character vector of numeric columns to compare, in
#'   output order
#' @param join_type Join type: `"inner"` (default) or `"full"`
#' @param metric Diff metric: `"diff"`, `"abs_diff"`, `"rel_diff"`,
#'   `"ratio"`, or `"pct_change"`
#' @param ... Forwarded to [blockr.core::new_transform_block()]
#'
#' @return A transform block of class `compare_block`.
#'
#' @export
new_compare_block <- function(
    key_cols = character(),
    measure_cols = character(),
    join_type = c("inner", "full"),
    metric = c("diff", "abs_diff", "rel_diff", "ratio", "pct_change"),
    ...) {

  join_type <- match.arg(join_type)
  metric <- match.arg(metric)

  blockr.core::new_transform_block(
    function(id, x, y) {
      shiny::moduleServer(
        id,
        function(input, output, session) {

          r_key_cols <- shiny::reactiveVal(key_cols)
          r_measure_cols <- shiny::reactiveVal(measure_cols)
          r_join_type <- shiny::reactiveVal(join_type)
          r_metric <- shiny::reactiveVal(metric)

          # Push the columns common to both inputs on every data change —
          # blockr.dplyr's column-metadata protocol, so the pickers show the
          # same name + label secondary text as every other block.
          send_columns <- function() {
            # `x()`/`y()` throw while an upstream block is unset or erroring;
            # the next data change re-sends, so swallow it here rather than
            # take down the announce observer.
            tryCatch(
              {
                common <- intersect(colnames(x()), colnames(y()))
                session$sendCustomMessage(
                  "compare-columns",
                  list(
                    id = session$ns("compare_input"),
                    columns = blockr.dplyr::build_column_picker_meta(
                      x()[, common, drop = FALSE]
                    )
                  )
                )
              },
              error = function(e) NULL
            )
          }

          shiny::observeEvent(list(x(), y()), send_columns())

          # One-shot heuristic: fill defaults on first data load
          shiny::observe({
            h <- classify_columns(x(), y())
            if (length(r_key_cols()) == 0) r_key_cols(h$key_cols)
            if (length(r_measure_cols()) == 0) r_measure_cols(h$measure_cols)
          }) |> shiny::bindEvent(x(), y(), once = TRUE)

          self_write <- new.env(parent = emptyenv())
          self_write$active <- FALSE

          # JS -> R: decompose the single state blob into the per-field
          # reactiveVals blockr.core serializes and external controllers set.
          shiny::observeEvent(input$compare_input, {
            self_write$active <- TRUE
            blob <- input$compare_input
            r_key_cols(as.character(unlist(blob$key_cols)))
            r_measure_cols(as.character(unlist(blob$measure_cols)))
            r_join_type(blob$join_type)
            r_metric(blob$metric)
          })

          r_state <- shiny::reactive(
            list(
              # as.list() so a single picked column stays a JSON array rather
              # than auto_unboxing to a scalar the JS side would mis-slice.
              key_cols = as.list(r_key_cols()),
              measure_cols = as.list(r_measure_cols()),
              join_type = r_join_type(),
              metric = r_metric()
            )
          )

          send_state <- function() {
            session$sendCustomMessage(
              "compare-block-update",
              list(
                id = session$ns("compare_input"),
                state = shiny::isolate(r_state())
              )
            )
          }

          # R -> JS: restore, external control, and the heuristic seed. Fires
          # on init; blockr-core.js queues it until the element binds.
          shiny::observeEvent(r_state(), {
            if (self_write$active) {
              self_write$active <- FALSE
            } else {
              send_state()
            }
          })

          # Deferred dock panel: both pushes above went out at boot, before
          # this block's JS existed on the page (it ships with the panel), and
          # Shiny drops a custom message that has no handler. The client
          # announces on bind when nothing was queued for it, which is the
          # signal to send again — without this the block comes up with empty
          # pickers and the JS default metric, and the first edit writes that
          # over the restored state. Same fix as blockr.dplyr 0.2.0.9005.
          #
          # `<container id>_ready` is what `Blockr.registerBlock()` sets; the
          # container is `compare_input`, declared in the ui() below.
          shiny::observeEvent(input$compare_input_ready, {
            send_columns()
            send_state()
          })

          list(
            expr = shiny::reactive(
              blockr.core::bbquote(
                blockr.insurance::compare_frames(
                  .(x), .(y),
                  key_cols = .(kc),
                  measure_cols = .(mc),
                  join_type = .(jt),
                  metric = .(mt)
                ),
                list(
                  kc = r_key_cols(),
                  mc = r_measure_cols(),
                  jt = r_join_type(),
                  mt = r_metric()
                )
              )
            ),
            state = list(
              key_cols = r_key_cols,
              measure_cols = r_measure_cols,
              join_type = r_join_type,
              metric = r_metric
            )
          )
        }
      )
    },
    function(id) {
      shiny::tagList(
        blockr.dplyr::blockr_core_js_dep(),
        blockr.dplyr::blockr_blocks_css_dep(),
        blockr.dplyr::blockr_select_dep(),
        compare_block_dep(),
        shiny::div(
          class = "block-container",
          shiny::div(
            id = shiny::NS(id, "compare_input"),
            class = "compare-block-container"
          )
        )
      )
    },
    dat_valid = function(x, y) {
      stopifnot(is.data.frame(x), is.data.frame(y))
    },
    expr_type = "bquoted",
    allow_empty_state = c("key_cols", "measure_cols"),
    external_ctrl = TRUE,
    class = "compare_block",
    ...
  )
}

#' HTML dependency for the compare block's JS + CSS
#' @noRd
compare_block_dep <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "compare-block-js",
      version = utils::packageVersion("blockr.insurance"),
      src = system.file("js", package = "blockr.insurance"),
      script = "compare-block.js"
    ),
    htmltools::htmlDependency(
      name = "compare-block-css",
      version = utils::packageVersion("blockr.insurance"),
      src = system.file("css", package = "blockr.insurance"),
      stylesheet = "compare-block.css"
    )
  )
}

#' Compare two data frames
#'
#' Joins two data frames on key columns and computes a diff metric for each
#' measurement column. When a single metric is selected the output columns
#' keep their original names; division-by-zero produces `NA`.
#'
#' @param x,y Data frames to compare
#' @param key_cols Character vector of columns to join on
#' @param measure_cols Character vector of numeric columns to diff
#' @param join_type `"inner"` or `"full"`
#' @param metric One of `"diff"`, `"abs_diff"`, `"rel_diff"`, `"ratio"`,
#'   `"pct_change"`
#'
#' @return A data frame with key columns and one diff column per measurement.
#'
#' @export
compare_frames <- function(x, y, key_cols, measure_cols,
                           join_type = "inner", metric = "diff") {

  x <- x[, c(key_cols, measure_cols), drop = FALSE]
  y <- y[, c(key_cols, measure_cols), drop = FALSE]

  join_fn <- switch(
    join_type,
    inner = dplyr::inner_join,
    full = dplyr::full_join
  )

  joined <- join_fn(x, y, by = key_cols, suffix = c("__x", "__y"))

  result <- joined[, key_cols, drop = FALSE]

  for (col in measure_cols) {
    xc <- joined[[paste0(col, "__x")]]
    yc <- joined[[paste0(col, "__y")]]

    result[[col]] <- switch(
      metric,
      diff = xc - yc,
      abs_diff = abs(xc - yc),
      rel_diff = ifelse(yc == 0, NA_real_, (xc - yc) / yc * 100),
      ratio = ifelse(yc == 0, NA_real_, xc / yc),
      pct_change = {
        avg <- (xc + yc) / 2
        ifelse(avg == 0, NA_real_, (xc - yc) / avg * 100)
      }
    )
  }

  result
}

#' Heuristic column classification
#'
#' Classifies shared columns of two data frames into key columns (used for
#' joining) and measurement columns (numeric, used for diffing).
#'
#' @param x,y Data frames
#'
#' @return A list with `key_cols` and `measure_cols` character vectors.
#'
#' @noRd
classify_columns <- function(x, y) {
  common <- intersect(colnames(x), colnames(y))

  key_cols <- character()
  measure_cols <- character()

  for (col in common) {
    vals <- x[[col]]
    if (is.character(vals) || is.factor(vals) || is.logical(vals) ||
        inherits(vals, "Date") || inherits(vals, "POSIXct") ||
        (is.integer(vals) && length(unique(vals)) < 20)) {
      key_cols <- c(key_cols, col)
    } else if (is.numeric(vals)) {
      measure_cols <- c(measure_cols, col)
    }
  }

  list(key_cols = key_cols, measure_cols = measure_cols)
}
