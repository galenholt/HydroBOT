## Getting Started

If starting for the first time you will need to have `renv` installed, at the R command prompt type `install.packages('renv')`.  Worth having a read of [renv doco](https://rstudio.github.io/renv/)  
Next run `renv::restore()` which will use the local `renv.lock` file to build a local R environment for this project.  Note if a `.Rprofile` is not present it will be created. 


### How do I get set up? ###

Most of the work here is in R scripts, with the Rproject providing relative paths to the project directory (repo). *Please* do not setwd()- all paths should be relative to the project dir, unless (*maybe*) they are hardcoded to fixed shared data resources.

The exception to the R code is the Scenario Controller and some initial test data creation, which are written in python/quarto and link closely with the [EWR tool](https://github.com/MDBAuth/EWR_tool).

This is not currently a package, though we are moving that way.

The data in `data/` is test data, and so will be included in the repo, but will hopefully change little. This is not currently tracked during the migration, as it might change more frequently. It can be rebuilt with notebooks in `data_creation/`, which also adds data to `data-raw/`, which is not tracked in the repo.

### Current status
Migrating from a prototype repo, currently developing cleaner structure and environment/data/file management.