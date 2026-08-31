# cellDIVER demo browser (single-object mode).
#
# Served by shiny-server at /demo/browser. Also reused, unmodified, as the
# Posit Connect Cloud deploy target (.github/workflows/posit-connect-cloud.yaml
# sets appDir to this directory) — see the "own directory" fallback below.
# Opens the bundled `test_dataset` demo (Triana et al. downsampled to 250
# cells, RNA + AB assays).
#
# Layout: each dataset is a folder under /srv/shiny-server containing a
# `browser/` app (this file) and a `config/` app, plus the dataset's object and
# config. shiny-server will not serve an app nested inside another app, so the
# dataset folder itself is NOT an app — its browser and config editor are
# sibling sub-apps, reachable at /<dataset>/browser and /<dataset>/config.
#
# The object and its config are staged from the installed package's
# inst/extdata, so they are NOT committed to the repo. Under shiny-server /
# Docker, they are staged into the PARENT dataset directory by the Dockerfile
# build step:
#   <dataset>/object.rds         <- inst/extdata/test_dataset.rds
#   <dataset>/object-config.yaml <- inst/extdata/test_dataset_config.yaml
# For the bundled demo, <dataset> is /srv/shiny-server/demo. Under Posit
# Connect Cloud, rsconnect bundles only THIS directory (no parent), so the
# workflow instead stages object.rds/object-config.yaml directly alongside
# this file before deploying — see the fallback in "Resolve the dataset
# directory" below.

# Defensive library() attaches. cellDIVER historically made a few unqualified
# calls (R.devices::suppressGraphics, SingleCellExperiment accessors,
# tools::toTitleCase); these are being fully namespaced on main. Attaching the
# packages here guarantees the app runs even if a stray unqualified call
# remains. These three attaches can be removed once all calls are confirmed
# namespaced. (`tools` is base R; the other two are package dependencies.)
library(R.devices)
library(SingleCellExperiment)
library(tools)

# Resolve the dataset directory from this app's OWN location rather than
# hardcoding /srv/shiny-server/demo, so the whole dataset folder can be copied
# or renamed for a real dataset with no edits to this file. shiny-server runs
# each app with the working directory set to that app's own directory (here
# `<dataset>/browser`), so the dataset root is normally its parent. Posit
# Connect Cloud bundles this directory alone (no parent), so we fall back to
# treating THIS directory as the dataset root when object.rds is staged here
# instead — this lets one file, unmodified, serve both deploy targets.
own_dir <- normalizePath(".")
parent_dir <- dirname(own_dir)
dataset_dir <- if (file.exists(file.path(own_dir, "object.rds"))) {
  own_dir
} else {
  parent_dir
}
object_path <- file.path(dataset_dir, "object.rds")
config_path <- file.path(dataset_dir, "object-config.yaml")

# Fail with an actionable message naming the directories we looked in, rather
# than surfacing a deep stack trace from inside the app. A missing object is
# the likeliest mistake when this folder is copied to serve a new dataset.
if (!file.exists(object_path)) {
  stop(
    "No object.rds found in this app's own directory (", own_dir,
    ") or its parent (", parent_dir, "). Each dataset folder must contain ",
    "object.rds and object-config.yaml alongside its browser/ and config/ ",
    "sub-apps, or (for a standalone bundle, e.g. Posit Connect Cloud) ",
    "alongside app.R itself.",
    call. = FALSE
  )
}

# run_cellDIVER() returns a shinyApp object as its last expression; shiny-server
# owns host/port, so we omit them and only force launch_browser = FALSE (a
# headless server must never try to open a local browser).
cellDIVER::run_cellDIVER(
  object_path = object_path,
  config_path = config_path,
  launch_browser = FALSE
)
