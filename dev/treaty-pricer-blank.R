# Treaty pricer — BLANK board for building a blockr workflow from scratch.
#
# The same application stack as treaty-pricer.R (all block packages + plugins),
# but with an EMPTY board. Use it in the demo to build a workflow live — by hand
# (the "+" button) or by PROMPTING the blockr.assistant. The assistant calls the
# prod LLM (gpt-5.1); OPENAI_API_KEY is picked up from /workspace/.Renviron.
#
# Run from an R session at the workspace root:
#   source("blockr.insurance/dev/treaty-pricer-blank.R")

pkgload::load_all("blockr.core")
pkgload::load_all("blockr.ui")
pkgload::load_all("blockr.dock")
pkgload::load_all("blockr.dplyr")
pkgload::load_all("blockr.input")
pkgload::load_all("blockr.extra")
pkgload::load_all("blockr.io")
pkgload::load_all("blockr.viz")
pkgload::load_all("blockr.dm")     # dm example block: insurance + CDISC ADaM dms
pkgload::load_all("blockr.dag")
pkgload::load_all("blockr.session")
pkgload::load_all("blockr.code")
pkgload::load_all("blockr.ai")
pkgload::load_all("blockr.assistant")
pkgload::load_all("blockr.insurance")

options(
  blockr.dock_is_locked = FALSE,
  blockr.lazy_eval      = FALSE,
  blockr.html_table_preview = TRUE,   # nicer HTML data previews in blocks
  blockr.ai_model       = "gpt-5.1",
  # The assistant's chat client — prod model (ellmer default would be gpt-4.1).
  blockr.chat_function = function(system_prompt = NULL, params = NULL) {
    ellmer::chat_openai(
      model = "gpt-5.1", system_prompt = system_prompt, echo = "none"
    )
  },
  # Command-style: flush each mutation immediately so the model verifies and
  # self-corrects in-turn (vs. staging the whole turn and flushing at the end).
  blockr.assistant_immediate_commit = TRUE
)

# Empty board: nothing but the assistant (chat-to-build) and the DAG (the live
# workflow graph). Add blocks by prompting the assistant or via the "+" button.
board <- new_dock_board(
  blocks = list(),
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
