# syntax=docker/dockerfile:1
#
# cellDIVER — production Docker image (shiny-server).
#
# Builds a self-contained image that serves the bundled `test_dataset` demo out
# of the box (zero external data). The default image installs everything needed
# for all R-native object types: in-memory .rds Seurat / SingleCellExperiment
# objects AND Seurat v5 objects backed by BPCells. The ONLY optional, off-by-
# default layer is Python/anndata/MuData support (see the commented block below).
#
# ---------------------------------------------------------------------------
# Dependency pinning (see the pinning table in the PR description)
# ---------------------------------------------------------------------------
# Base image: the `4` tag tracks the latest R 4.x patch release and, with it,
#   the Posit Public Package Manager (PPM/RSPM) CRAN snapshot frozen for that
#   release. That snapshot is what protects this build against CRAN archival:
#   if CRAN later removes a package, the snapshot still serves it. Do NOT
#   repoint options(repos) at live cran.r-project.org.
#   Harder line for byte-reproducibility: pin a patch tag (e.g.
#   `rocker/shiny-verse:4.4.2`) or a `@sha256:` digest so the R version + frozen
#   snapshot cannot drift between rebuilds.
FROM rocker/shiny-verse:4

# System libraries: glpk for igraph/Seurat, hdf5 for HDF5Array.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libglpk-dev \
        libhdf5-dev \
    && rm -rf /var/lib/apt/lists/*

# Installer tooling. BiocManager makes Bioconductor repos available so the
# package's Bioc deps (HDF5Array, SingleCellExperiment, SCUBA's deps, ...)
# resolve. Fail loudly (quit status 10) so a broken layer fails the build.
RUN R -e "install.packages(c('BiocManager','remotes')); if (!library(BiocManager, logical.return=TRUE)) quit(status=10)"

# cellDIVER + its full HARD-dependency tree (Depends/Imports/LinkingTo only, so
# test-only Suggests such as reactlog/profvis are skipped — the package calls
# those with a graceful requireNamespace() fallback). The Remotes: field of the
# package (amc-heme/SCUBA, amc-heme/scDE) is pulled automatically by BiocManager.
#
# SOURCE: cellDIVER is installed from the LATEST main branch (the bare repo spec
#   resolves to the default branch). By project decision the image tracks the
#   current package rather than a fixed pin.
#   Do NOT use the v1.0.0 git tag: it predates the scExploreR -> cellDIVER rename
#   and the dependency cleanup, so '@v1.0.0' installs the old "scExploreR"
#   package (wrong name) whose undeclared deps (e.g. pryr) fail a hard-deps-only
#   install. main is clean.
#   Tradeoff: tracking main means rebuilds are NOT byte-reproducible w.r.t. the
#   package. If reproducibility is later needed, pin to a commit or a post-rename
#   release tag, e.g. BiocManager::install('amc-heme/cellDIVER@<sha-or-tag>').
#   The Remotes: entries (SCUBA, scDE) likewise float to their default-branch
#   HEAD (declared without an @ref in DESCRIPTION). To hard-pin them, install at
#   known-good commits BEFORE this line, e.g.:
#     RUN R -e "remotes::install_github(c('amc-heme/SCUBA@<sha>','amc-heme/scDE@<sha>'), upgrade='never')"
#   (with update=FALSE below, the already-installed pinned versions are kept).
# options(timeout=600): the cellDIVER source tarball is ~56 MB and R's default
#   60s download.file timeout is too tight on slower build networks (notably
#   CI), where it caused a partial download and a failed build. 600s is ample.
RUN R -e "options(timeout = 600); BiocManager::install('amc-heme/cellDIVER', update=FALSE, ask=FALSE, dependencies=c('Depends','Imports','LinkingTo')); if (!library(cellDIVER, logical.return=TRUE)) quit(status=10)"

# BPCells (not on CRAN/Bioconductor) for Seurat v5 objects with BPCells assays.
# Default-on: cheap install, no extra system deps, and BPCells-backed objects
# are common enough that the default image should open them out of the box.
# PINNING: r-universe serves the LATEST build, so this floats. For reproducible
#   rebuilds, install a specific version/source ref instead (e.g. a tarball URL
#   or `remotes::install_github('bnprks/BPCells/r@<sha>')`).
RUN R -e "options(timeout = 600); install.packages('BPCells', repos=c('https://bnprks.r-universe.dev', getOption('repos'))); if (!library(BPCells, logical.return=TRUE)) quit(status=10)"

# ---------------------------------------------------------------------------
# OPTIONAL: anndata / MuData / Python support. Uncomment to support .h5ad /
# MuData objects. SCUBA uses a uv-managed Python env (no conda); it py_require()s
# anndata/pandas/numpy/scipy, plus mudata for MuData.
# NOTE: the uv path has been fragile ("minimum uv version" / "python not found").
# If you enable this, prefer pinning a known-good uv and pre-baking the env as
# the runtime `shiny` user so it is not fetched on the first request.
# ---------------------------------------------------------------------------
# USER shiny
# RUN R -e "library(SCUBA); reticulate::py_require(c('anndata','pandas','numpy','scipy','mudata>=0.3.1')); reticulate::import('anndata'); reticulate::import('mudata')"
# USER root

# Shiny-server configuration + served apps. Clear the stock sample apps first.
RUN rm -rf /srv/shiny-server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY docker/shiny-server/ /srv/shiny-server/

# Stage the bundled demo dataset from the INSTALLED package (not the build
# context) into the demo app dir. This keeps the image reproducible from the
# package's own data and avoids re-committing large binaries to the repo.
# Must run after the COPY above so the demo/ dir exists.
RUN R -e "dir.create('/srv/shiny-server/demo', showWarnings=FALSE, recursive=TRUE); \
  if (!file.copy(system.file('extdata','test_dataset.rds', package='cellDIVER', mustWork=TRUE), '/srv/shiny-server/demo/object.rds', overwrite=TRUE)) quit(status=10); \
  if (!file.copy(system.file('extdata','test_dataset_config.yaml', package='cellDIVER', mustWork=TRUE), '/srv/shiny-server/demo/object-config.yaml', overwrite=TRUE)) quit(status=10)"

RUN chown -R shiny:shiny /srv/shiny-server
EXPOSE 3838
