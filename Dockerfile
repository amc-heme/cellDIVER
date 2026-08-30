# syntax=docker/dockerfile:1
#
# cellDIVER — production Docker image (shiny-server). Serves the bundled demo
# dataset out of the box; all R-native object types (in-memory Seurat /
# SingleCellExperiment, and Seurat v5 with BPCells) work by default. anndata /
# MuData support is optional (see the commented block below).
#
# See README.md "Image Notes" for the rationale behind BPCells and Python/anndata.

# Pinned to a specific R patch tag (not the floating "4") for a stable, byte-
# reproducible build. Test the app against a new patch before bumping this.
FROM rocker/shiny-verse:4.6.1

# System libraries: glpk for igraph/Seurat, hdf5 for HDF5Array.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libglpk-dev \
        libhdf5-dev \
    && rm -rf /var/lib/apt/lists/*

# Installer tooling: BiocManager resolves the package's Bioconductor deps.
RUN R -e "install.packages(c('BiocManager','remotes')); if (!library(BiocManager, logical.return=TRUE)) quit(status=10)"

# cellDIVER + its hard-dependency tree, tracking main. Remotes: deps (SCUBA,
# scDE) are pulled automatically. Do NOT substitute the v1.0.0 tag: it predates
# the scExploreR -> cellDIVER rename, so it installs the old scExploreR package
# under the wrong name.
# options(timeout=600): default 60s is too tight for the ~56 MB tarball on slow build networks (seen in CI).
RUN R -e "options(timeout = 600); BiocManager::install('amc-heme/cellDIVER', update=FALSE, ask=FALSE, dependencies=c('Depends','Imports','LinkingTo')); if (!library(cellDIVER, logical.return=TRUE)) quit(status=10)"

# BPCells (not on CRAN/Bioconductor) for Seurat v5 objects with BPCells assays.
RUN R -e "options(timeout = 600); install.packages('BPCells', repos=c('https://bnprks.r-universe.dev', getOption('repos'))); if (!library(BPCells, logical.return=TRUE)) quit(status=10)"

# OPTIONAL: anndata / MuData / Python support. Uncomment to support .h5ad /
# MuData objects (see README.md "Image Notes" before enabling — the uv path
# has been fragile).
# USER shiny
# RUN R -e "library(SCUBA); reticulate::py_require(c('anndata','pandas','numpy','scipy','mudata>=0.3.1')); reticulate::import('anndata'); reticulate::import('mudata')"
# USER root

# Shiny-server configuration + served apps. Clear the stock sample apps first.
RUN rm -rf /srv/shiny-server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY docker/shiny-server/ /srv/shiny-server/

# Stage the bundled demo dataset from the installed package (not the git repo)
# into the demo app dir; must run after the COPY above so the demo/ dir exists.
RUN R -e "dir.create('/srv/shiny-server/demo', showWarnings=FALSE, recursive=TRUE); \
  if (!file.copy(system.file('extdata','test_dataset.rds', package='cellDIVER', mustWork=TRUE), '/srv/shiny-server/demo/object.rds', overwrite=TRUE)) quit(status=10); \
  if (!file.copy(system.file('extdata','test_dataset_config.yaml', package='cellDIVER', mustWork=TRUE), '/srv/shiny-server/demo/object-config.yaml', overwrite=TRUE)) quit(status=10)"

RUN chown -R shiny:shiny /srv/shiny-server
EXPOSE 3838
