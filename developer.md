## Getting Started

Configure git username and email. In bash, `git config --global user.name "User Name"` and `git config --global user.email "user.email@email"`. 
Clone the repo.

### R global setup
The R environment stuff within the project is managed by `renv`, but there are some packages we want to install globally because they make R work in VScode, help development, etc. So, *outside the repo* (or before cloning it), do these things. 

To use R in VScode, there are a few things to do, mostly listed at the [VScode docs](https://code.visualstudio.com/docs/languages/r). The essential ones are to install `languageserver` (`install.packages('languageserver')`) and then the R extension in VScode. However, I sometimes have to install it inside the repo. I think `.libPaths` doesn't retain some global paths on Azure that it retains elsewhere. VScode will prompt to install if it can't find it.

I'm experimenting with `radian` and `httpgd`, and think I like them, but they're not necessary. And there are other linters, debuggers, etc beyond what comes with the VScode R extension, but I haven't played with them yet.

If you want to install [radian](github.com/randy3k/radian) as a terminal, it should be global as well. The azure boxes need to `pip3 install prompt-toolkit --upgrade` before `pip3 install -U radian` or we get TypeErrors. Hoping this doesn't screw up the python envs? Guess we'll see. It's a bit confusing because when we start a `radian` terminal, it says 'python', not 'R' because radian is written in python. Which is a bit confusing, especially when we're also actually doing python work. In practice, I like some of the things about radian, but have turned it off because it throws weird errors (but runs through them) that aren't real ('unexpected & in }' when neither symbol is in the code). I just don't fully trust it.

Installing `httpgd` globally as well (inside R, `install.packages('httpgd')`).

In settings, I used `which radian` to get the location, and put that (`/anaconda/envs/azureml_py38/bin/radian`) in `Rterm:Linux` in the settings. And I turned on `LSP:debug` and clicked `use httpgd` in the plots settings. I'll try to keep a record here of these setup things I've done so we don't forget and have different vs behaviour.

### System setup
On Windows, you'll need Rtools, whichever version matches your local R. Rtools provides access to compilation of C/C++/Fortran. Mac and Unix just have that built-in. But, at least on the Azure boxes, there are a lot of missing libs the R packages depend on. As of this writing, I hit errors with libcurl, libjq, libgeos, libudunits2, libgdal, and libxml2. One option is to try to `renv::restore()` and fix these as they come, but likely easier to just hit them all at once with `sudo apt-get install libcurl4-openssl-dev libjq-dev libgeos-dev libgdal-dev libudunits2-dev libxml2-dev`. Note that if you do need to install after a failure in R and it tells you what it needs, you can use `system("sudo apt-get install packagename")` if you want to stay within R and not switch to bash.

If the Azure connection crashes, and the Explorer panel all turns yellow, you have to stop and restart the Azure VM. 

## Project setup

### R environments
If starting for the first time you will need to have `renv` installed. Typically, starting an R session at the command line (`R`) will automatically detect the absence of `renv` and install it. If not, at the R command prompt type `install.packages('renv')`.  Worth having a read of [renv doco](https://rstudio.github.io/renv/).

I ended up having to `install.packages('languageserver')` inside the repo on Azure, because it won't talk to the global `.libPaths`. This takes a really long time. I think moving towards a library will start to fix this- then we can use `renv` to manage both used packages and dev packages, while the NAMESPACE and DESCRIPTION will manage the packages actually needed by the code.

The `yaml` package is required for `renv` to parse its lockfile, so `install.packages('yaml')`. On Azure, this has to be done in the repo, since the `.libPaths()` in the repo doesn't have access to the same locations (except base) as the `.libPaths()` outside the repo.

*On Azure* (and maybe other Unix)- `xml2` causes all sorts of issues. Before using `renv::restore()`, in R, run `options(install.opts = "--no-lock")` and then `renv::install('xml2')`. That last may not be necessary (it might be handled by `renv::restore()`). Then proceed as below to install everything else.

Typically, we would next run `renv::restore()` which will use the local `renv.lock` file to build a local R environment for this project. Note if a `.Rprofile` is not present it will be created. If new packages are added (`renv::install('packagename')`) and want to be kept, add to the lockfile with `renv::snapshot()`.

`xml2` seems to be extra touchy to install on Unix, and errors sometimes with a message about `pkg-config` and sometimes about permissions to move things from the `00LOCK` directory. I am working on a smoother fix for this- currently tried `install.packages('xml2', dependencies = TRUE, INSTALL_opts = c('--no-lock'))` inside R, which worked but took *forever* and seems to have messed up my `activate.R` file.


### How do I get set up? ###

Most of the work here is in R scripts, with the Rproject providing relative paths to the project directory (repo). *Please* do not `setwd()`- all paths should be relative to the project dir, unless (*maybe*) they are hardcoded to fixed shared data resources.

The exception to the R code is the Scenario Controller and some initial test data creation, which are written in python/quarto and link closely with the [EWR tool](https://github.com/MDBAuth/EWR_tool).

This is not currently a package, though we are moving that way.

The data in `data/` is test data, and so will be included in the repo, but will hopefully change little. This is not currently tracked during the migration, as it might change more frequently. It can be rebuilt with notebooks in `data_creation/`, which also adds data to `data-raw/`, which should not be tracked in the repo. The exception (currently) is data that cannot be gathered from elsewhere, but we should move away from this as possible.

### Current status
Migrating from a prototype repo, currently developing cleaner structure and environment/data/file management.