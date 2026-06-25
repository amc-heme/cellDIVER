# cellDIVER config editor for the demo project.
#
# Served by shiny-server at /demo/config. Each dataset folder holds sibling
# `browser/` and `config/` sub-apps (shiny-server will not serve an app nested
# inside another app, so the dataset folder itself is not an app). This config
# editor edits the config for the same object the /demo/browser app serves.

# Defensive library() attaches — see the note in ../browser/app.R. Removable
# once all cellDIVER calls are confirmed fully namespaced.
library(R.devices)
library(SingleCellExperiment)
library(tools)

# The demo object and its config are staged here by the Dockerfile build step.
parent_object <- "/srv/shiny-server/demo/object.rds"
parent_config <- "/srv/shiny-server/demo/object-config.yaml"

# run_config()'s config_path is a LOAD source: when supplied, the app exposes a
# "Load Config File" link that loads this YAML for editing. (Saving is a browser
# download, not a write-back to this path — see the TODO below.) Gate on
# file.exists() so the editor still starts (blank) if no config is staged.
#
# run_config() takes no host/port/launch_browser arguments; it returns a
# shinyApp object as its last expression and shiny-server runs it.
cellDIVER::run_config(
  object_path = parent_object,
  config_path = if (file.exists(parent_config)) parent_config else NULL
)

# ---------------------------------------------------------------------------
# TODO (future enhancement) — config editor auto-loads the parent project's YAML
#
# Current behavior: passing config_path wires up the "Load Config File" link so
# the user can load the parent project's existing config, but the editor still
# opens from scratch — the user must click that link to populate it. Saving
# always goes through a browser download (run_config never writes back to
# config_path), so server-side round-tripping (load -> edit -> save to the same
# path) is not supported today.
#
# Enhancement: when the parent project already has a config YAML, open the
# editor PRE-POPULATED for editing instead of requiring the manual "Load Config
# File" click. The open question (does config_path load an existing config for
# editing?) is RESOLVED — yes, it loads for editing — but auto-population on
# open, and write-back persistence, both require changes inside run_config()
# itself, so they are out of scope for the Docker work.
# ---------------------------------------------------------------------------
