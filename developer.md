## Getting Started

Configure git username and email. In bash, `git config --global user.name "User Name"` and `git config --global user.email "user.email@email"`. 
Clone the repo.

### R global setup
The R environment stuff within the project is managed by `renv`, but there are some packages we want to install globally because they make R work in VScode, help development, etc. So, *outside the repo* (or before cloning it), do these things.

To use R in VScode, there are a few things to do, mostly listed at the [VScode docs](https://code.visualstudio.com/docs/languages/r). The essential ones are to install languageserver `install.packages('languageserver')` and then the R extension. I'm experimenting with `radian` and `httpgd`, and think I like them, but they're not necessary. And there are other linters, debuggers, etc beyond what comes with the VScode R extension, but I haven't played with them yet.
If you want to install [radian](github.com/randy3k/radian) as a terminal, it should be global as well. Though it's in python, so will it screw up the python envs? Guess we'll see.

### System setup
On Windows, you'll need Rtools, whichever version matches your local R. Rtools provides access to compilation of C/C++/Fortran. Mac and Unix just have that built-in. But, at least on the Azure boxes, there are a lot of missing libs the R packages depend on. As of this writing, I hit errors with libcurl, libjq, libgeos, libudunits2, libgdal, and libxml2. One option is to try to `renv::restore()` and fix these as they come, but likely easier to just hit them all at once with `sudo apt-get install libcurl4-openssl-dev libjq-dev libgeos-dev libgdal-dev libudunits2-dev libxml2-dev`. Note that if you do need to install after a failure in R and it tells you what it needs, you can use `system("sudo apt-get install packagename")` if you want to stay within R and not switch to bash.

## Project setup

### R environments
If starting for the first time you will need to have `renv` installed. Typically, starting an R session at the command line (`R`) will automatically detect the absence of `renv` and install it. If not, at the R command prompt type `install.packages('renv')`.  Worth having a read of [renv doco](https://rstudio.github.io/renv/).

Next run `renv::restore()` which will use the local `renv.lock` file to build a local R environment for this project.  Note if a `.Rprofile` is not present it will be created. If new packages are added (`renv::install('packagename')`) and want to be kept, add to the lockfile with `renv::snapshot()`.

`xml2` seems to be extra touchy to install on Unix, and errors sometimes with a message about `pkg-config` and sometimes about permissions to move things from the `00LOCK` directory. I am working on a smoother fix for this- currently tried `install.packages('xml2', dependencies = TRUE, INSTALL_opts = c('--no-lock'))` inside R, which worked but took *forever* and seems to have messed up my `activate.R` file.


### How do I get set up? ###

Most of the work here is in R scripts, with the Rproject providing relative paths to the project directory (repo). *Please* do not `setwd()`- all paths should be relative to the project dir, unless (*maybe*) they are hardcoded to fixed shared data resources.

The exception to the R code is the Scenario Controller and some initial test data creation, which are written in python/quarto and link closely with the [EWR tool](https://github.com/MDBAuth/EWR_tool).

This is not currently a package, though we are moving that way.

The data in `data/` is test data, and so will be included in the repo, but will hopefully change little. This is not currently tracked during the migration, as it might change more frequently. It can be rebuilt with notebooks in `data_creation/`, which also adds data to `data-raw/`, which should not be tracked in the repo. The exception (currently) is data that cannot be gathered from elsewhere, but we should move away from this as possible.

### Current status
Migrating from a prototype repo, currently developing cleaner structure and environment/data/file management.