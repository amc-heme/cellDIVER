# cellDIVER demo browser (single-object mode).
#
# Served by shiny-server at /demo/browser. Opens the bundled `test_dataset` demo
# (Triana et al. downsampled to 250 cells, RNA + AB assays).
#
# Layout: each dataset is a folder under /srv/shiny-server containing a
# `browser/` app (this file) and a `config/` app, plus the dataset's object and
# config. shiny-server will not serve an app nested inside another app, so the
# dataset folder itself is NOT an app — its browser and config editor are
# sibling sub-apps, reachable at /<dataset>/browser and /<dataset>/config.
#
# The object and its config are staged into the PARENT dataset directory by the
# Dockerfile build step (from the installed package's inst/extdata), so they are
# NOT committed to the repo:
#   /srv/shiny-server/demo/object.rds         <- inst/extdata/test_dataset.rds
#   /srv/shiny-server/demo/object-config.yaml <- inst/extdata/test_dataset_config.yaml

# Defensive library() attaches. cellDIVER historically made a few unqualified
# calls (R.devices::suppressGraphics, SingleCellExperiment accessors,
# tools::toTitleCase); these are being fully namespaced on main. Attaching the
# packages here guarantees the app runs even if a stray unqualified call
# remains. These three attaches can be removed once all calls are confirmed
# namespaced. (`tools` is base R; the other two are package dependencies.)
library(R.devices)
library(SingleCellExperiment)
library(tools)

# run_cellDIVER() returns a shinyApp object as its last expression; shiny-server
# owns host/port, so we omit them and only force launch_browser = FALSE (a
# headless server must never try to open a local browser).
cellDIVER::run_cellDIVER(
  object_path = "/srv/shiny-server/demo/object.rds",
  config_path = "/srv/shiny-server/demo/object-config.yaml",
  launch_browser = FALSE
)
