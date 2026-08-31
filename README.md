# cellDIVER

*cellDIVER was formerly known as scExploreR.*

**Welcome to the single cell visualization tool you didn't know you were looking for!** This app is designed to make exploration of highly complex data sets easy for anyone, regardless of informatics background. Whether you're a researcher looking to make sense of your single cell data, or a bioinformatician looking to present your results interactively, you will find value in this app. cellDIVER bridges the gap between domain (disease and clinical) specific knowledge and informatics expertise by providing a no-code platform for biologists to analyze data. 

cellDIVER (Single-**cell** **D**ata **I**nterface for **V**isualization and **E**xploration in **R**) further facilitates analysis through compatability with most common single cell data formats! Seurat, SingleCellExpreiment, or Anndata objects can be used, and Seurat v5 objects with BP Cells assays are also supported.

Some bioinformatics experience is required to install the app and configure each single-cell dataset, but once set up, the app can be used by anyone.

## Requirements
* [Bioconductor](https://bioconductor.org/install/)
* A pre-processed and finalized single cell object (or objects). Currently supported formats: Seurat, SingleCellExperiment, and Anndata.
* A server to host the app. This can be any computer with at that can be left on and be connected to the internet continuously. RAM requirements vary depending on the object type.

If using anndata objects, [reticulate](https://github.com/rstudio/reticulate) must be installed with the following Python packages:

* Numpy
* Pandas
* Scipy
* Anndata
* Scanpy

<!-- Add page on HDF5 storage, and put a link here -->
  <!-- For Seurat objects, you need at least as much RAM as the size of the object in memory, but the size of the object can be considerably greater than the available RAM for Anndata and SingleCellExperiment objects using HDF5 storage. -->

## Installation and Use

1. Install from Github using [Remotes](https://github.com/r-lib/remotes).
```
remotes::install_github("amc-heme/cellDIVER")
```

If this is your first time setting up cellDIVER, we reccomend you view the [**App Setup Walkthrough**](https://amc-heme.github.io/cellDIVER/articles/dataset_setup_walkthrough.html), which applies the process in steps 3-5 to an example object.

2. Process or obtain a finalized single cell object.

3. Configure an object for the browser by using the configuration app provided with the package. For more information on the config app, see the [**Full Config App Documentation**](https://amc-heme.github.io/cellDIVER/articles/config_documentation.html) or the [**App Setup Walkthrough**](https://amc-heme.github.io/cellDIVER/articles/dataset_setup_walkthrough.html).
```
cellDIVER::run_config(
  object_path = "path_to_your_seurat_object.rds",
  # The config path will be blank the first time you use the config app for an object
  config_path = "previously_loaded_config_file"
  )
```

4. To set up a browser for others to use, create a browser config YAML file (If you are using the browser locally for your own use, skip to step 4). The file will contain a list of datasets with the path to the objects and config files for each, along with browser specific settings. 
<!-- Complete and add -->
<!--See [**browser config setup**]() for more info. -->

5. Run cellDIVER. There are multiple ways to do this: 

<ul>
  <li>
  If setting up an app instance, use the path to your config file.
  
  ```
  cellDIVER::run_cellDIVER(
    browser_config = "./config.yaml"
    )
  ```
  
  </li>
  <li>
  If setting up locally, and if you only have one object, you may instead enter the path to your object and object-specific config file.
  
  ```
  cellDIVER::run_cellDIVER(
    object_path = "./object.rds"
    config_path = "./config.yaml"
    )
  ```
  
  </li>
</ul>

## Docker Installation

cellDIVER ships a self-contained Docker image (shiny-server) that serves a bundled demo dataset out of the box. The quickest way to run it is to pull the pre-built image from the GitHub Container Registry (GHCR):

```
docker pull ghcr.io/amc-heme/celldiver:latest
docker run --rm -p 3838:3838 ghcr.io/amc-heme/celldiver:latest
```

> **Pending image visibility — delete this line once the package is public.** The image publishes automatically on merge to `main`, but a newly-created GHCR package is private by default, so an anonymous `docker pull` returns `denied` / `unauthorized` until an org admin sets `ghcr.io/amc-heme/celldiver` to Public. Until then, [build the image locally](#docker-details).

Open <http://localhost:3838/> for the directory index. The demo data browser is at `/demo/browser` and its config editor at `/demo/config`.

Prefer to build from source, pin versions, or enable anndata/MuData support? See [Docker Details](#docker-details) for the local-build alternative.

## Docker Details

**Build and run locally:**
```
docker build --platform=linux/amd64 -t celldiver .
docker run --rm -p 3838:3838 celldiver
```
(`--platform=linux/amd64` is only needed on Apple-Silicon/ARM hosts.)

**PR approval smoke test (concise):**
1. Build from a clean state: `docker build --platform=linux/amd64 -t celldiver .`
2. Run the container: `docker run --rm -p 3838:3838 celldiver`
3. Confirm these routes load:
   - `/` (directory index)
   - `/demo/` (dataset folder index)
   - `/demo/browser/` (`cellDIVER` app)
   - `/demo/config/` (`Configure Seurat Object` app)
4. Confirm both apps log `Listening on` without startup errors.

**Deploy your own data:** mount a host folder of per-dataset subdirectories at `/srv/shiny-server`:
```
docker run --rm -p 3838:3838 -v /path/to/apps:/srv/shiny-server ghcr.io/amc-heme/celldiver:latest
```
Use [docker/shiny-server/demo](docker/shiny-server/demo) as the per-dataset template. Because shiny-server cannot serve an app nested inside another app, the data browser and config editor are sibling sub-apps (`browser/` and `config/`), not the dataset folder itself. For each dataset:

1. Copy the `demo/` folder and rename it (e.g. `mydata/`).
2. Place your Seurat object in that folder as `object.rds` and its config as `object-config.yaml`, alongside the `browser/` and `config/` sub-apps. These two names are required. (The demo's copies are *not* in the repo — they are baked into the image from the bundled package data — so for your own data you supply them.)
3. No edits to `app.R` are needed: both sub-apps resolve the dataset directory from their own location, so a renamed copy works as-is.
4. Ensure the mounted files are readable by the in-container `shiny` user (UID 999): a bind mount keeps host ownership and shadows the image's build-time `chown`, so run `chmod -R a+rX /path/to/apps` if they are not already world-readable, otherwise the apps will not start.

Then browse to `localhost:3838/mydata/browser`, and generate or edit its config at `localhost:3838/mydata/config`.

Seurat v5 objects with BPCells assays work out of the box. anndata / MuData (`.h5ad`) support requires uncommenting the Python block in the [Dockerfile](Dockerfile) and rebuilding (see below).

**Image Notes:**
* **Architecture: `linux/amd64` only.** The base image (`rocker/shiny-verse`) publishes no `arm64` manifest, so there is no arm64 base to build `FROM` — the image is amd64-only by necessity, not choice. On Apple-Silicon / ARM hosts it runs under emulation (Rosetta / QEMU); if Docker does not select it automatically you may need `--platform=linux/amd64` on `docker run` (as well as on `docker build` when building locally).
* **BPCells**: installed by default (from r-universe, since it isn't on CRAN/Bioconductor) so that Seurat v5 objects with BPCells assays open out of the box — the install is cheap and needs no extra system dependencies. r-universe serves the latest build, so this floats; for a reproducible rebuild, install a specific source ref instead, e.g. `remotes::install_github('bnprks/BPCells/r@<sha>')`.
* **anndata / MuData / Python support is off by default.** Enabling it (the commented block in the Dockerfile) pulls in a uv-managed Python environment via SCUBA, which `py_require()`s anndata/pandas/numpy/scipy (plus mudata for MuData) — this path has been fragile in practice ("minimum uv version" / "python not found" errors). If you enable it, pin a known-good `uv` version and pre-bake the environment as the runtime `shiny` user at build time, rather than letting it fetch on first request.

## Posit Connect Cloud

A GitHub Actions workflow, [`posit-connect-cloud.yaml`](.github/workflows/posit-connect-cloud.yaml), publishes the bundled demo dataset to [Posit Connect Cloud](https://connect.posit.cloud/). It is **manual only** (`workflow_dispatch`): start it from the repository's **Actions** tab. The workflow installs cellDIVER from `main`, then uses [`rsconnect`](https://rstudio.github.io/rsconnect/) to bundle the small demo app in [`connect/app.R`](connect/app.R) and push it to Connect Cloud, which reinstalls cellDIVER and its dependencies to serve the app.

Publishing authenticates non-interactively with three repository secrets (all prefixed `POSIT_`):

* `POSIT_CLIENT_ID` — OAuth client id for a Connect Cloud service account
* `POSIT_CLIENT_SECRET` — the OAuth client secret paired with `POSIT_CLIENT_ID`
* `POSIT_ACCOUNT` — the Connect Cloud account name to publish to

These feed `rsconnect::connectCloudClientCredentials()`; see the Posit docs on [publishing from a console or terminal](https://docs.posit.co/connect-cloud/user/publish/console-or-terminal.html) for how to obtain the client credentials.

## Future Goals

<!-- As stated above, the current version of the app requires manually fitting each new object to its own specific version of the app. Future versions of the app will be able to accept *any* Seurat object, automatically detect (or user specified) metadata values of interest, and build the app to provide exploration of that object. 

<br>
-->

* Additional analyses such as GSEA will be added in the future
* Explicit support for single cell data besides CITE-seq will be added.
