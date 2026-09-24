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

> **Behaviour change.** `run_cellDIVER()` and `run_config()` no longer attach
> their dependencies to your search path. Earlier versions called `library()`
> on around 25 packages as a side effect of launching the app. If you relied on
> that — for example calling `DimPlot()` at the console after closing the app —
> attach the package yourself or qualify the call as `Seurat::DimPlot()`. The
> apps themselves are unaffected. See `NEWS.md`.

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

## Automated testing

The consolidated suite uses testthat (edition 3), `shiny::testServer()` and
shinytest2 with Chrome/Chromium. It adapts the useful coverage from
[`187-add-automated-testing-to-app`](https://github.com/amc-heme/cellDIVER/tree/187-add-automated-testing-to-app)
to the current cellDIVER APIs, rather than merging that older scExploreR branch
over newer application code. The old PhantomJS/shinytest recordings are replaced
by assertions in the normal testthat suite.

Install the package's development dependencies, including Suggests, with
`remotes::install_deps(dependencies = TRUE)`. Install Chrome or Chromium and, if
it is not discovered automatically, set `CHROMOTE_CHROME` to its executable.
Run from the repository root:

```r
Sys.setenv(NOT_CRAN = "true")
devtools::test()
```

All tests are discovered through `tests/testthat.R`; browser scenarios use the
bundled `inst/extdata/test_dataset.rds` and configuration, not external datasets
or a deployed server. Browser tests are skipped on CRAN, but **not** in CI.
Missing browser dependencies or fixtures fail CI instead of silently skipping.
Use `devtools::test(filter = "unit|module")` for a fast non-browser iteration, or
`devtools::test(filter = "browser")` for browser scenarios.

Coverage includes configuration roundtrips, choices/titles, subset and threshold
boundaries, reactive plot controls, DGE filters/statistics, rendered plots and
downloads, marker/pairwise/threshold DGE, and subset/reset behavior. Correlation
tests exercise the existing module in a dedicated fixture because its tab is
currently disabled in the main browser; testing does not re-enable it.

### PR results and merge protection

The **Tests** workflow tests the checked-out PR code on a GitHub-hosted runner
when a PR opens, changes, reopens or becomes ready for review. It also supports
merge queues, pushes to `main` and manual runs. Its stable check name is
**cellDIVER tests**. All test files run without an early failure limit; failures,
errors, skipped tests and empty runs fail the check. The run summary contains
counts; the `test-results` artifact contains JUnit XML and available browser
diagnostics. Dependency installation or browser startup failures also fail the
check.

The separate **PR test results** workflow creates or updates one PR comment with
the outcome and a link to the detailed results, including failed runs and fork
PRs. It uses GitHub job metadata only: it never checks out PR code or executes
artifacts with a write token. Outdated revisions do not overwrite newer reports.
This reporting workflow must be on the default branch before GitHub will run
it; the first PR introducing it still has the normal check and run summary.
Fork runs may require a maintainer's approval under repository Actions settings.

**One-time administrator setup is required to actually block merges.**
A workflow cannot enable branch protection by itself:

1. Run **Tests** once so GitHub discovers **cellDIVER tests**.
2. In **Settings → Rules → Rulesets**, create or edit an active branch ruleset
   targeting `main` (and any other protected target branches).
3. Enable **Require status checks to pass** and add **cellDIVER tests**, choosing
   GitHub Actions as its source. Require branches to be up to date before merging
   (or use the merge queue).
4. Remove bypass permissions for anyone who must be blocked by failing tests.
   Do not require **PR test results**: comments are informational, not the gate.
5. Verify with a deliberately failing test on a temporary PR that the check
   fails and merging is disabled, then fix the test and confirm the new run
   succeeds and updates the comment.

The PR test job uses only a read-only repository token. Do not add deployment
secrets, a cross-repository PAT, or a self-hosted runner to execute untrusted PR
code. If a dependency becomes private, arrange a safe dependency distribution
before enabling the gate; missing dependency access must remain a failure.

### Manual continuous-documentation pilot

The **Continuous documentation pilot** workflow is opt-in and manual-only.
Merge its workflow, support files, and browser capture hooks into `main` first;
then select **Actions → Continuous documentation pilot → Run workflow → main**.
Dispatches from other branches cannot run the pilot. Schedules and path triggers
are deliberately deferred until a maintainer has reviewed a successful pilot.

Administrator setup:

1. Create the `continuous-documentation` environment. Restrict deployment branches
   to **main**, require a trusted reviewer, and prevent self-approval where
   available. This environment protects only the model job.
2. Enable GitHub Copilot access and the organization's approved model policies
   for the account performing the review. Add the environment secret
   `COPILOT_DOCUMENTATION_TOKEN`: a fine-grained personal access token with
   **Copilot Requests** permission only, **no repository permissions**. Do not
   supply an OpenAI key, general-purpose PAT, or deployment credentials.
3. The requested model is exactly `gpt-6-astra`. The pinned Copilot CLI and account
   must explicitly advertise that model and support image reading. Availability
   and the provider's capability identifiers are not assumed: unsupported
   versions, missing entitlement, or an unavailable model fail the run without
   substitution. `CONTINUOUS_DOCUMENTATION_ASTRA_MODEL`, if configured as a
   repository variable, accepts only `gpt-6-astra`; a different capability ID
   requires a reviewed adapter update, not a fallback. The pinned CLI advertises
   this ID, but account availability and vision capability are checked at runtime.
4. In **Settings → Actions → General → Workflow permissions**, allow GitHub
   Actions to create pull requests. Keep default permissions read-only; only the
   separate publish job receives `contents: write` and `pull-requests: write`.
5. Review the test data for suitability before approving the environment. Only
   bundled test fixtures are exercised; screenshots and visible UI text are sent
   through the approved **GitHub Copilot** integration, not a direct model-provider
   API. Never point this workflow at private clinical or production datasets.

Each run freezes the dispatch's exact main SHA once and checks out that same
revision in every job. It builds and installs the package, runs every existing
test (skips, errors, failures, and zero passed expectations fail), runs
`R CMD check`, and builds pkgdown. The check omits duplicate test execution,
manual generation, and vignette checks; the full test suite and pkgdown article
rendering are separate mandatory gates. Warnings and notes remain in the logs
for review. Five assertion-backed screenshots cover configuration, DimPlot,
FeaturePlot, subsetting, and DGE at 1440×1000 with seed 325.

The model receives observations, the existing articles, and actual screenshots.
The pinned CLI's native SDK `models.list` checks account capabilities, and its
documented `--attachment` option supplies all five PNGs with tools disabled.
The adapter rejects unsupported catalogue or response-audit formats.
It can propose only bounded JSON plain-text paragraphs for managed sections in
`vignettes/dataset_setup_walkthrough.Rmd` and
`vignettes/scRNA_Plots_Explained.Rmd`. Trusted code escapes the prose and copies
only captured images; generated R chunks, arbitrary files, remote images, README
changes, and model-written executable documentation are not accepted. Correct
existing prose is preserved, and an empty proposal creates no branch or PR.
Unverified behavior is listed in the draft PR for human review.

A fresh, read-only validation job repeats the full tests, check, and pkgdown
build after applying the proposal. It has no model credentials. Publishing
reconstructs and hash-verifies that exact validated change against the captured
SHA; it never executes proposed code and creates only a **draft**, never an
auto-merged PR. The model job has no repository write token. GitHub normally
does **not** run `pull_request` workflows for PRs created with `GITHUB_TOKEN`;
the explicit validation is mandatory, and a maintainer must arrange the normal
required PR checks before merging. Do not bypass branch protection.

Artifacts retain before/after articles and rendered sites, screenshot evidence,
observations, hashes, model responses, package versions, test reports, check
output, and logs for **14 days**, including available failure diagnostics.
Download evidence needed for long-term review before expiry. The local script
and workflow checks do not establish model availability or replace a successful
credentialed end-to-end Actions run; review the first pilot before broadening
automation.

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

A GitHub Actions workflow, [`posit-connect-cloud.yaml`](.github/workflows/posit-connect-cloud.yaml), publishes the bundled demo dataset to [Posit Connect Cloud](https://connect.posit.cloud/). It is **manual only** (`workflow_dispatch`): start it from the repository's **Actions** tab. The workflow installs cellDIVER from `main`, stages the demo dataset alongside the existing demo browser app ([`docker/shiny-server/demo/browser/app.R`](docker/shiny-server/demo/browser/app.R) — the same app.R shiny-server serves in the Docker image), then uses [`rsconnect`](https://rstudio.github.io/rsconnect/) to bundle that directory and push it to Connect Cloud, which reinstalls cellDIVER and its dependencies to serve the app.

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
