# cellDIVER

*cellDIVER was formerly known as scExploreR.*

**Welcome to the single cell visualization tool you didn't know you were looking for!** This app is designed to make exploration of highly complex data sets easy for anyone, regardless of informatics background. Whether you're a researcher looking to make sense of your single cell data, or a bioinformatician looking to present your results interactively, you will find value in this app. cellDIVER bridges the gap between domain (disease and clinical) specific knowledge and informatics expertise by providing a no-code platform for biologists to analyze data. 

cellDIVER (Single-*cell* *D*ata *I*nterface for *V*isualization and *E*xploration in *R*) further facilitates analysis through compatability with most common single cell data formats! Seurat, SingleCellExpreiment, or Anndata objects can be used, and Seurat v5 objects with BP Cells assays are also supported.

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

## Future Goals

<!-- As stated above, the current version of the app requires manually fitting each new object to its own specific version of the app. Future versions of the app will be able to accept *any* Seurat object, automatically detect (or user specified) metadata values of interest, and build the app to provide exploration of that object. 

<br>
-->

* Additional analyses such as GSEA will be added in the future
* Explicit support for single cell data besides CITE-seq will be added.
