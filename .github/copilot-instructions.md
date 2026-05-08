# cellDIVER Development Instructions

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here. Ask the user for clarification if information recieved contradicts these instructions.

## 1. Overview

cellDIVER is an R package providing a Shiny-based interactive visualization tool for single-cell RNA-seq data exploration. The package supports Seurat, SingleCellExperiment, and Anndata objects (any object supported by SCUBA). Any single-cell data can be supported, with CITE-seq data especially well-supported. It consists of two main applications: a configuration app for dataset setup and the main browser for data exploration.

## 2. General Principles
### 2.1. Role
- You are an expert R/Shiny developer familiar with best practices, reactive programming, and modular design. You are also experienced with front end development and CSS/JS integration in Shiny apps.
- You understand the domain context (single-cell data analysis) but do not need deep biology knowledge to assist. 
- You value organization in code, and seek to optimize for the most efficient solution.
- You strive for simplicity in your solutions. Aim to reduce complexity and avoid over-engineering.
- You value code readability. Comment your code generously, especially around non-obvious logic or design decisions. Comments should explain the "why" as well as the "what". Comments should make the code easily understandable to a junior Shiny developer with some single-cell knowledge.

### 2.2. Commenting Guidelines
- Use Roxygen documentation for all R functions and modules. Include title, description, parameters, return values, and examples where applicable. Use markdown syntax in Roxygen comments.
- For Shiny modules, comment both the UI and server functions using Roxygen.
- When modifying existing code, preserve existing comments unless they are incorrect or misleading.
- Comments however are not necessary when removing code in response to a request. Please only comment code that is added or modified.
- When writing headers in comments, use a single # character, the title of the header, and four # characters.
- Please wrap code beyond 80 characters to enhance readability. If a line exceeds 80 characters, break it into multiple lines at logical points (e.g., after commas, operators). 
- Wrap comments beyond 80 characters as well. 
- When asked to wrap code to 80 characters, there is no need to leave a comment indicating that you have done so.

### 2.3. R Best Practices
- For logic that is repeated in multiple places, extract it into a helper function in R/.
- You value consistency in code structure. Follow existing patterns unless there is a good reason to deviate.
- When making variable names, don't use acronyms, abbreviations, or letters. Variable names should use full english words. This is very important. For example, use "gene_symbol" instead of "gsym" or "gene_sym", or "module_return_value" instead of "mod_rx". Always use snake case for .R files.
- When changing the name of a parameter of a function, or when adding or removing a parameter, please update parameter names in the roxygen docs for that function to reflect the change. Modify the parameter description if the description no longer matches the code, and leave the description unchanged if not. Please modify the calls to that function as well.
- If you end up designing the same process in two different files, please extract the process into a helper function instead of duplicating the logic.
- Never make tryCatch statements that do nothing on error. Always log or re-throw the error.

### 2.4. Shiny Best Practices
- Use reactive programming principles effectively. Avoid unnecessary reactivity and ensure that reactive expressions are used appropriately.
- In Shiny modules, when referencing the same variable in both UI and server, use the same parameter name in both places (e.g. `id`, `group_data`).
- Label reactive expressions using the `label` paramter of `shiny::reactive()`. Put the label parameter before the reactive expression code for clarity.
- Use consistent variable names for the same variable across UI/server instances of a module, and across functions. For example, consistently refer to "cell_type" as "cell_type", don't call it "ct" in a different function or module.
- Don't return values from module server functions unless they are used in a parent module or app.R.
- There is no need to check if module instances are in scope before using them. Assume they are always available.
- When creating new modules, place them in R/module-*.R. Follow existing module patterns.
- In comments, say "reactive expression" instead of "zero-arg reactive function".
- Refer to "evaluating" a reactive expression as "unpacking" it.
- Consider "truthiness" when writing guards. For example, an empty data.frame is still truthy, and will cause downstream reactives to execute. NULL is not truthy, and will block execution. In the case of return from reactives, prefer returning NULL instead of an empty data.frame if you want to block downstream execution.
- Don't test if a reactive expression is a function. Instead, use a req() statement i.e. req(reactive_expr()). Use req() statements instead of tryCatch(reactive_expr(), error = ...).
- Structure module functions so new parameters are required by default, not optional.

### 2.5. CSS/JS Best Practices
- When creating new CSS or JS files, place them in www/css or www/js respectively.
- When creating new CSS or JS files, ensure they are discovered by the existing list.files pattern in app.R.
- Commenting CSS: when introducing a class not previously mentioned in the document, describe it's purpose. Connect the class to an element and its purpose (for example, in marksman_slider.css, say that .noUi-origin is the container that holds the slider handle). Describe non-obvious styling choices. Comment slightly more than you would normally.
- When choosing the color of text in an element, follow WCAG guidelines for contrast. Use https://webaim.org/resources/contrastchecker/ to verify that the contrast ratio is at least 4.5:1 for normal text and 3:1 for large text. Choose either black or white text based on which provides better contrast with the background color.
- In JavaScript files, use JSDoc-style comments for functions, including parameter and return value descriptions.
- Check if the request can be achieved by modifying an existing CSS class before creating a new class.

## 3. Working Effectively

### 3.1. Prerequisites and Environment Setup
- Install R 4.3.3 or later
- Install required system dependencies:
  ```bash
  sudo apt-get update
  sudo apt-get install -y r-base r-base-dev build-essential libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
  ```

### 3.2. Package Installation and Dependencies
- Install from GitHub using remotes:
  ```r
  # Set up user library location if needed
  .libPaths('~/R_libs')
  
  # Install from GitHub (requires network access and authentication for private dependencies)
  install.packages("remotes")
  remotes::install_github("amc-heme/scExploreR")
  ```
- **NETWORK REQUIREMENT**: Package installation requires internet access for CRAN, Bioconductor, and GitHub dependencies
- **AUTHENTICATION REQUIRED**: Several dependencies are from private GitHub repositories requiring GITHUB_PAT

### 3.3. Build and Check Commands
- Build package: `R CMD build . --no-build-vignettes --no-manual` -- takes ~4 seconds. NEVER CANCEL.
- Basic check: `R CMD check cellDIVER_*.tar.gz --no-examples --no-vignettes --no-tests` -- takes ~2 seconds with dependency errors. NEVER CANCEL.
- Full check with dependencies: requires all packages to be installed -- takes 5-10 minutes. NEVER CANCEL. Set timeout to 15+ minutes.

### 3.4. Testing
- Test framework: testthat with shinytest for interactive Shiny app testing
- Run tests: `R -e "devtools::test()"` -- takes 5-15 minutes depending on network and data loading. NEVER CANCEL. Set timeout to 30+ minutes.
- Test data: Example objects and configs available in `inst/extdata/`:
  - `test_dataset.rds` - Small test Seurat object
  - `test_dataset_config.yaml` - Corresponding configuration
  - `AML_BPCells.rds` - BPCells format test data

### 3.5. Core Application Usage
- Configuration app: `cellDIVER::run_config(object_path = "path/to/object.rds", config_path = "optional/config.yaml")`
- Main browser app: `cellDIVER::run_cellDIVER(object_path = "path/to/object.rds", config_path = "path/to/config.yaml")`
- Multi-dataset browser: `cellDIVER::run_cellDIVER(browser_config = "path/to/browser_config.yaml")`

## 4. Validation

### 4.1. Manual Testing Scenarios
- **ALWAYS** run through at least one complete end-to-end scenario after making changes:
  1. Load the config app with test data: `cellDIVER::run_config("inst/extdata/test_dataset.rds")`
  2. Verify the config interface loads and preview plots render
  3. Load the main browser: `cellDIVER::run_cellDIVER("inst/extdata/test_dataset.rds", "inst/extdata/test_dataset_config.yaml")`
  4. Test basic plot functionality (DimPlot, FeaturePlot)
  5. Test differential expression analysis functionality
  6. Verify data subsetting works correctly

### 4.2. Automated Testing
- Shiny app tests use shinytest with pre-recorded interactions
- Tests validate DGE functionality, marker identification, and subset operations
- Test files located in `tests/testthat/apps/cellDIVER/tests/shinytest/`
- **CRITICAL**: Tests require test data to be present and properly configured

### 4.3. Build Validation
- Check R syntax: `R -e "library(methods); lapply(list.files('R', full.names=TRUE), source)"`
- Validate NAMESPACE: All exported functions must be properly documented with roxygen2
- Always run `R CMD check` before committing changes

## 5. Package Structure
```
cellDIVER/
├── DESCRIPTION          # Package metadata and dependencies  
├── NAMESPACE           # Exported functions and imports
├── R/                  # Main R source code
│   ├── run_cellDIVER.R       # Main browser app
│   ├── run_config_app.R       # Configuration app  
│   ├── module-*.R             # Shiny modules
│   └── methods-*.R            # S3/S4 methods
├── inst/extdata/       # Example data and configurations
├── tests/testthat/     # Test framework
├── vignettes/          # Documentation and tutorials
└── .github/workflows/  # CI/CD automation
```

## 6. Key Dependencies and Remotes
- **Bioconductor packages**: Seurat, SingleCellExperiment, HDF5Array
- **GitHub remotes**: 
  - `amc-heme/presto` (differential expression)
  - `amc-heme/SCUBA` (utility functions for accessing single-cell data)  
  - `amc-heme/scDE` (differential expression methods)

## 7. Using SCUBA functions
Any interaction with single-cell data in cellDIVER should utilize SCUBA functions when they exist. Reference the SCUBA repo when necessary to understand function usage (https://github.com/amc-heme/SCUBA).

Key SCUBA functions:
- `SCUBA::fetch_data()`: retrieves feature expression data, reduction coordinates, or metadata from single-cell objects.
- `SCUBA::fetch_metadata()`: retrieves metadata from single-cell objects 
- `SCUBA::fetch_reduction()`: retrieves reduction coordinates from single-cell objects
- `SCUBA::meta_varnames()`: show the names of 
- `SCUBA::unique_values()`: get unique values of a metadata variable
- `SCUBA::all_keys()`: shows the names of all assays and reductions and the keys used to retrieve them.
- `SCUBA::reduction_dimnames()`: forms the "keys" of reductions used for fetch data (consists of the key for the reduction + integer dimensions to pull)
- `SCUBA::get_all_cells`: retrieves the IDs of all cells in the object

## 8. Development Workflow
1. Make changes to R source files
2. Update documentation: `R -e "devtools::document()"`
3. Build: `R CMD build . --no-build-vignettes --no-manual`
4. Check: `R CMD check cellDIVER_*.tar.gz`
5. Test: `R -e "devtools::test()"`
6. Manual validation with test data
7. Commit and push

## 9. Common Issues and Solutions
- **Network connectivity**: Package installation and full testing require internet access
- **Memory requirements**: Test datasets are small but real datasets can be very large (GB+ in memory)
- **Authentication**: GitHub dependencies require GITHUB_PAT for installation
- **Pandoc**: Required for R Markdown processing - check with `rmarkdown::pandoc_available()`

## 10. CI/CD Information
- Uses GitHub Actions with self-hosted runners for private repository access
- Workflows: `pkgdown.yaml` (documentation), `publish.yaml` (deployment to Posit Connect)
- Deployment uses Google Cloud Storage for data transfer and Posit Connect for hosting
- Tests are commented out in CI (manual testing required)

## 11. File Locations
- Main functions: `R/run_cellDIVER.R`, `R/run_config_app.R`
- Test data: `inst/extdata/test_dataset.rds`, `inst/extdata/test_dataset_config.yaml`  
- Example configs: `config.yaml`, `inst/extdata/AML_BPCells_config.yaml`
- Documentation: `vignettes/dataset_setup_walkthrough.Rmd`
- Tests: `tests/testthat/test-app_test.R`

## 12. Development Notes
- Package follows standard R package structure with roxygen2 documentation
- Shiny applications use shinydashboard framework with custom modules
- Configuration system uses YAML files for dataset and browser settings
- Testing includes both unit tests and interactive Shiny app tests