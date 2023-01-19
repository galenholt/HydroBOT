## Getting Started

Configure git username and email. In bash, `git config --global user.name "User Name"` and `git config --global user.email "user.email@email"`. 
Clone the repo.

Follow Andrew's instructions for setting up a github ssh key. Mine dies every time, and so before I can push/pull, I have to run `ssh-add ~/.ssh/werp_key`, and then it works. Sometimes VS's source control pane still doesn't talk to github, but the command line seems to always work after that.

### R global setup
The R version that comes on the machines is 4.0, and the usual `apt-get` isn't finding anything newer, but currently (late December 2022) R is at 4.2.2 and that's what I've been developing on. We can use [rig](https://github.com/r-lib/rig) to manage R versions (and this is likely better anyway, because we can swap around). It does seem to run with the old version, but it will be good to be able to choose. At the bash terminal, type `curl -Ls https://github.com/r-lib/rig/releases/download/latest/rig-linux-latest.tar.z | sudo tar xz -C /usr/local` to install rig. Then, `rig list` to see available R versions (likely none- it doesn't store its versions with the system). Then `rig add 4.2.2` (or whatever the current version is). There are ways to use Rstudio with the version in the `renv.lock`, and likely ways to do the same with VS, but for now, easiest is to make sure `renv default` matches the version in `renv.lock`.

The R environment stuff within the project is managed by `renv`, but there are some packages we want to install globally because they make R work in VScode, help development, etc. So, *outside the repo* (or before cloning it), do these things. 

To use R in VScode, there are a few things to do, mostly listed at the [VScode docs](https://code.visualstudio.com/docs/languages/r). The essential ones are to install `languageserver` (`install.packages('languageserver')`) and then the R extension in VScode. However, I sometimes have to install it inside the repo. I think `.libPaths` doesn't retain some global paths on Azure that it retains elsewhere. VScode will prompt to install if it can't find it.

I'm experimenting with `radian` and `httpgd`, and think I like them, but they're not necessary. And there are other linters, debuggers, etc beyond what comes with the VScode R extension, but I haven't played with them yet.

If you want to install [radian](github.com/randy3k/radian) as a terminal, it should be global as well. The azure boxes need to `pip3 install prompt-toolkit --upgrade` before `pip3 install -U radian` or we get TypeErrors. Hoping this doesn't screw up the python envs? Guess we'll see. When we start a `radian` terminal, it says 'python', not 'R' because radian is written in python. Which is a bit confusing, especially when we're also actually doing python work. In practice, I like some of the things about radian, but have turned it off because it throws weird errors (but runs through them) that aren't real ('unexpected & in }' when neither symbol is in the code). I just don't fully trust it. I *think* the issue is that it doesn't handle running code chunks in Quarto correctly- it's effectively copy-pasting them into the terminal and isn't doing it right.

Installing `httpgd` globally as well (inside R, `install.packages('httpgd')`).

In settings, I used `which radian` to get the location, and put that (`/anaconda/envs/azureml_py38/bin/radian`) in `Rterm:Linux` in the settings. And I turned on `LSP:debug` and clicked `use httpgd` in the plots settings. I'll try to keep a record here of these setup things I've done so we don't forget and have different VS behaviour.

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

### Python environments
Use the poetry in `/werptoolkitpy` to build the python environment in `/werptoolkitpy`. See the note in the werptoolkitr readme about what is needed on the python side to actually use the package- the short answer is a python environment with `py-ewr`. I'm working on making that automatic, for now, users will need to set that up manually. Can use WERP_toolkit_demo as a template.

## Notes-edit
VS seems to sometimes struggle to find the library if started in WERP_toolkit. In that case, start a new workspace in WERP_toolkit/werptoolkitr. 

VS opened in WERP_toolkit or werptoolkitr needs to be told the first time we open a workspace where the python interpreter is (in /werptoolkitpy/.venv/Scripts/python.exe) because it's no longer in the top-level.

### Building data and HTML documentation
Some of the data needs to be built, at least right now. And we'll always have the option to do that as the data changes. The easiest way to do it is to run `quarto render -P REBUILD_DATA` from WERP_toolkit to rebuild all `.qmd`, `.rmd`, and `.md` files everywhere and rebuild the data. Leaving off the parameter (just `quarto render`) re-renders the project with the default of FALSE if we just want new HTML but not new data. Individual files can also be rendered, either with the CLI or the Render buttons in VS or Rstudio. *Note*- due to a bug that I can't find, the only way to run `scenario_creation.qmd` (the python version) is locally with the button, after re-enabling the `execute` flag in the YAML header. For some reason it kills the gauge-getter when run from the command line. The R equivalent (`scenario_creation_R_demo`) *does* render from CLI, even though it also uses the same code that fails in the other, and so we still rebuild that data on a full rebuild.


### How do I get set up? ###

Most of the work here is in R files and Quarto notebooks, with the Rproject providing relative paths to the project directory (repo). This should be a self-contained package. *Please* do not `setwd()`- all paths should be relative to the project dir. In *analysis* repos that use this package, that's still a good idea, though *maybe* some limited paths will point to fixed shared data resources.

The exception to the R code is the Scenario Controller and some initial test data creation, which are written in python/quarto and link closely with the [EWR tool](https://github.com/MDBAuth/EWR_tool).

This is now a package (though very rough around the edges).

The data in `data/` is test data, and so will be included in the repo, but will hopefully change little. This is not currently tracked during the migration, as it might change more frequently. It can be rebuilt with notebooks in `data_creation/`, which also adds data to `data-raw/`, which should not be tracked in the repo. The exception (currently) is data that cannot be gathered from elsewhere, but we should move away from this as possible.

### Current status
Migrating from a prototype repo, currently developing cleaner structure and environment/data/file management.