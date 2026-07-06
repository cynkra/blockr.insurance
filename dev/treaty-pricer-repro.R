# Reproduction harness for the assistant crash: pre-built iris board
# (Dataset -> Filter -> Chart) matching the rehearsal demo, then prompt
# "add a bar chart of mean petal length by species".
#
#   source("blockr.insurance/dev/treaty-pricer-repro.R")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.io")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.dm")
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.code")
pkgload::load_all("blockr.ai")
pkgload::load_all("blockr.assistant")
pkgload::load_all("blockr.insurance")

options(
  blockr.dock_is_locked = FALSE,
  blockr.lazy_eval      = FALSE,
  blockr.html_table_preview = TRUE,
  blockr.ai_model       = "gpt-5.1",
  blockr.chat_function = function(system_prompt = NULL, params = NULL) {
    ellmer::chat_openai(
      model = "gpt-5.1", system_prompt = system_prompt, echo = "none"
    )
  },
  blockr.assistant_immediate_commit = TRUE
)

# Pre-built iris board: Dataset -> "Pick a Flower" filter (one species)
# -> empty Chart, exactly as the demo builds by hand in slide 5.
board <- new_dock_board(
  blocks = blocks(
    dataset = new_dataset_block("iris"),
    flower  = new_filter_block(
      conditions = list(
        list(type = "values", column = "Species",
             values = list("setosa"), mode = "include")
      ),
      operator = "&"
    ),
    chart   = new_chart_block()
  ),
  links = links(
    l1 = new_link(from = "dataset", to = "flower"),
    l2 = new_link(from = "flower",  to = "chart")
  ),
  extensions = list(
    assistant = new_assistant_extension(),
    dag       = new_dag_extension()
  ),
  layouts = list(
    Build = dock_layout(
      "ext_panel-assistant_extension", "ext_panel-dag_extension",
      sizes = c(1, 1.4),
      name = "Build"
    )
  )
)

serve(
  board,
  plugins = custom_plugins(c(
    ai_ctrl_block(),
    manage_project(),
    generate_flat_code()
  ))
)
