# syntax=docker/dockerfile:1
#
# cellDIVER container targets.
#
#   runtime-base : shared Seurat/Bioconductor/BPCells dependency layer reused by
#     both the production image and CI.
#   ci-base      : runtime-base plus browser/test/deploy tooling for GitHub
#     Actions jobs.
#   final stage  : production shiny-server image that installs the checked-out
#     cellDIVER source and serves the bundled demo dataset.
#
# See README.md "Image Notes" for the rationale behind BPCells and
# Python/anndata.

ARG R_VERSION=4.6.1

# Pinned to a specific R patch tag (not the floating "4") for a stable, byte-
# reproducible build. Test the app against a new patch before bumping this.
FROM rocker/shiny-verse:${R_VERSION} AS runtime-base

# System libraries needed across runtime, tests, and Connect deployment:
# glpk for igraph/Seurat, hdf5 for HDF5Array, curl/ssl/xml for package installs,
# and font/image libraries for the wider plotting/reporting tree. Pandoc is
# included here so the CI/deploy target inherits it without another apt layer.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        libglpk-dev \
        libhdf5-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libfontconfig1-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        pandoc \
    && rm -rf /var/lib/apt/lists/*

# Copy only DESCRIPTION so dependency layers stay warm when code changes but the
# package dependency graph does not.
COPY DESCRIPTION /tmp/cellDIVER/DESCRIPTION

# Installer tooling: shiny-verse already carries BiocManager/devtools; verify
# they are present before using them to resolve the dependency graph.
RUN R -e "if (!all(vapply(c('BiocManager', 'devtools'), requireNamespace, logical(1), quietly = TRUE))) quit(status = 10)"

# Install cellDIVER's hard dependency tree once into the shared base image.
RUN R -e "options(timeout = 600); options(repos = BiocManager::repositories()); devtools::install_deps('/tmp/cellDIVER', dependencies = c('Depends', 'Imports', 'LinkingTo'), upgrade = 'never'); description <- read.dcf('/tmp/cellDIVER/DESCRIPTION')[1, , drop = FALSE]; dependency_fields <- intersect(c('Depends', 'Imports', 'LinkingTo'), colnames(description)); packages <- unlist(strsplit(paste(description[1, dependency_fields], collapse = ','), ',')); packages <- trimws(gsub('[[:space:]]+', ' ', packages)); packages <- trimws(gsub('\\s*\\(.*\\)', '', packages)); packages <- setdiff(packages[nzchar(packages)], 'R'); missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing) > 0) stop('Missing runtime dependencies: ', paste(missing, collapse = ', '), call. = FALSE)"

# BPCells (not on CRAN/Bioconductor) for Seurat v5 objects with BPCells assays.
RUN R -e "options(timeout = 600); options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages('BPCells', repos = c('https://bnprks.r-universe.dev', getOption('repos'))); if (!library(BPCells, logical.return = TRUE)) quit(status = 10)"

FROM runtime-base AS ci-base

# Chrome powers shinytest2/chromote browser runs inside CI. The non-root github
# user below matches the hosted runner's uid so mounted workspaces stay writable.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        gnupg \
        wget \
    && wget -q -O - https://dl.google.com/linux/linux_signing_key.pub \
        | gpg --dearmor -o /usr/share/keyrings/google-linux.gpg \
    && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-linux.gpg] http://dl.google.com/linux/chrome/deb/ stable main" \
        > /etc/apt/sources.list.d/google-chrome.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends google-chrome-stable \
    && rm -rf /var/lib/apt/lists/*

# Suggests covers the test stack declared by the package; rsconnect is the extra
# workflow tool used to publish to Connect Cloud.
RUN R -e "options(timeout = 600); options(repos = BiocManager::repositories()); devtools::install_deps('/tmp/cellDIVER', dependencies = 'Suggests', upgrade = 'never'); install.packages('rsconnect', repos = c(CRAN = 'https://cloud.r-project.org')); description <- read.dcf('/tmp/cellDIVER/DESCRIPTION')[1, , drop = FALSE]; packages <- unlist(strsplit(description[1, 'Suggests'], ',')); packages <- trimws(gsub('[[:space:]]+', ' ', packages)); packages <- trimws(gsub('\\s*\\(.*\\)', '', packages)); packages <- c(packages[nzchar(packages)], 'rsconnect'); missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]; if (length(missing) > 0) stop('Missing CI dependencies: ', paste(missing, collapse = ', '), call. = FALSE)"

ENV CHROMOTE_CHROME=/usr/bin/google-chrome \
    HOME=/home/github \
    R_LIBS_USER=/home/github/R/library

RUN groupadd --gid 1001 github \
    && useradd --uid 1001 --gid 1001 --create-home github \
    && mkdir -p /home/github/R/library \
    && chown -R github:github /home/github

USER github
WORKDIR /home/github

FROM runtime-base

# Install the checked-out package source so PR Docker validation exercises the
# proposed code, not GitHub main.
COPY . /tmp/cellDIVER
RUN R CMD INSTALL /tmp/cellDIVER

# Shiny-server configuration + served apps. Clear the stock sample apps first.
RUN rm -rf /srv/shiny-server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY docker/shiny-server/ /srv/shiny-server/

# Stage the bundled demo dataset from the installed package (not the git repo)
# into the demo app dir; must run after the COPY above so the demo/ dir exists.
RUN R -e "dir.create('/srv/shiny-server/demo', showWarnings = FALSE, recursive = TRUE); if (!file.copy(system.file('extdata', 'test_dataset.rds', package = 'cellDIVER', mustWork = TRUE), '/srv/shiny-server/demo/object.rds', overwrite = TRUE)) quit(status = 10); if (!file.copy(system.file('extdata', 'test_dataset_config.yaml', package = 'cellDIVER', mustWork = TRUE), '/srv/shiny-server/demo/object-config.yaml', overwrite = TRUE)) quit(status = 10)"

RUN rm -rf /tmp/cellDIVER \
    && chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
