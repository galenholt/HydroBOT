## Getting Started

If starting for the first time you will need to have `renv` installed, at the R command prompt type `install.packages('renv')`.  Worth having a read of [renv doco](https://rstudio.github.io/renv/)  
Next run `renv::restore()` which will use the local `renv.lock` file to build a local R environment for this project.  Note if a `.Rprofile` is not present it will be created. 


### How do I get set up? ###

Most of the work here is in R scripts, and expects to be run through the Rproject for now or the paths will break. The path dependencies should go away soon.

The exception to the R code is the Scenario Controller and some initial test data creation, which are written in python/quarto and link closely with the [EWR tool](https://github.com/MDBAuth/EWR_tool).

### Current status
Migrating from a prototype repo, currently developing cleaner structure and environment/data/file management.