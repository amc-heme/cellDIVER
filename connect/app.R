# cellDIVER demo browser — Posit Connect Cloud entrypoint.
#
# Published to Posit Connect Cloud by the "Posit Connect Cloud" GitHub Actions
# workflow (.github/workflows/posit-connect-cloud.yaml), which runs on manual
# dispatch only. rsconnect bundles THIS directory and records cellDIVER and its
# full dependency tree in the generated manifest; Connect Cloud then installs
# those packages and runs this app.R to serve the app.
#
# The demo object and its config ship inside the installed cellDIVER package
# (inst/extdata), so they are resolved with system.file() rather than committed
# here — this keeps the deployment bundle to just this file. On Connect Cloud,
# cellDIVER is installed from GitHub (see the workflow), so system.file()
# resolves against that install.

# Defensive library() attaches — mirrors docker/shiny-server/demo/browser/app.R.
# cellDIVER historically made a few unqualified calls
# (R.devices::suppressGraphics, SingleCellExperiment accessors,
# tools::toTitleCase). Attaching these packages here guarantees the demo runs
# even if a stray unqualified call remains, and ensures rsconnect records them
# in the deployment manifest so Connect Cloud installs them. (`tools` is base R;
# the other two are package dependencies.)
library(R.devices)
library(SingleCellExperiment)
library(tools)

# Resolve the bundled demo dataset and its config from the installed cellDIVER
# package. mustWork = TRUE fails fast with a clear error if the package was
# installed without its extdata, rather than launching a broken app.
object_path <- system.file(
  "extdata", "test_dataset.rds",
  package = "cellDIVER", mustWork = TRUE
)
config_path <- system.file(
  "extdata", "test_dataset_config.yaml",
  package = "cellDIVER", mustWork = TRUE
)

# run_cellDIVER() returns a shinyApp object as its last expression; Connect Cloud
# owns host/port, so we omit them and only force launch_browser = FALSE (a
# headless server must never try to open a local browser).
cellDIVER::run_cellDIVER(
  object_path = object_path,
  config_path = config_path,
  launch_browser = FALSE
)
