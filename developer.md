## Getting Started

Configure git username and email. In bash, `git config --global user.name "User Name"` and `git config --global user.email "user.email@email"`. 
Clone the repo.

Set up an ssh key to be able to talk to github. Largely following Andrew's instructions:

Basically, setting up the github ssh key follows the [github instructions](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent?platform=linux), and then adding that key to your github account, again [as described by github](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account). This sets up the key in `~/.ssh`. To be safe, I typically cd there with `cd ~/.ssh` to start, though most of the commands put it there no matter where you are. 

A couple small notes:
-   Don't name the file- leave it as the default 'id_TypeOfEncyption'. `ssh-add` auto adds 'id_*' files, but not other names. 
-   Don't include a passphrase (just press enter when it asks for one)- that just yields another layer of signin complexity. 
-   The github docs code sometimes says to use `clip ~/.ssh/id_ed25519.pub` to copy the contents of the file, and sometimes says to use `cat ~/.ssh/id_ed25519.pub` to print it to terminal and then copy (docs seem to be under development). `clip` seems to work on windows with git bash, `cat` works on Linux.

If we stopped here, things would work, but we'd have to use `eval "$(ssh-agent -s)"` and then `ssh-add ~/.ssh/NAME_OF_KEY` every time. So, change the `.bashrc` according to Andrew to auto-run `ssh-add`. I tend to use nano for small edits, so `nano ~/.bashrc` (or `cd` and then `nano .bashrc`), and copy-paste in 

```
[ -z "$SSH_AUTH_SOCK" ] && eval "$(ssh-agent -s)"

env=~/.ssh/agent.env

agent_load_env () { test -f "$env" && . "$env" >| /dev/null ; }

agent_start () {
    (umask 077; ssh-agent >| "$env")
    . "$env" >| /dev/null ; }

agent_load_env

# agent_run_state: 0=agent running w/ key; 1=agent w/o key; 2=agent not running
agent_run_state=$(ssh-add -l >| /dev/null 2>&1; echo $?)

if [ ! "$SSH_AUTH_SOCK" ] || [ $agent_run_state = 2 ]; then
    agent_start
    ssh-add
elif [ "$SSH_AUTH_SOCK" ] && [ $agent_run_state = 1 ]; then
    ssh-add
fi

unset env
```
The original instructions said we needed to add a .config file in the `~/.ssh` dir with the following

```
AddKeysToAgent yes

Host *
IdentityFile  /home/azureuser/.ssh/[your key file name]
```
but it didn't solve the issue with persisting named keys and doesn't seem to be needed if they have the default names. I've dropped it, but may want to resurrect if we have issues.

If the key still isn't persisting, it's likely that `ssh-add` isn't auto-starting it because it has a non-default name. The best solution is to use a default name, but you could also run `ssh-add ~/.ssh/KEY_NAME` every time, and then it works. Sometimes VS's source control pane still doesn't talk to github, but the command line seems to always work after that.

### WINDOWS ssh
Setting up ssh on Windows works basically the same way. Just click the Windows tab at the git instructions, but it's nearly identical if you use git bash. That should be fine for typical repo use, but we have to do more to get ssh to work for *installing* packages from protected repos. To prepare for this, there seem to be an inability to pass anything other than rsa keys, so create one of those (I have one of those and and an ed25519, but I'm pretty sure the rsa is auto-enforced with the `install_git`). The rigmarole for installing is dealt with in the developer.md for WERP_toolkit_demo, since that's where we need to install this from.

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

I ended up having to `install.packages('languageserver')` inside the repo on Azure, because it won't talk to the global `.libPaths`. This takes a really long time.

The `yaml` package is required for `renv` to parse its lockfile, so `install.packages('yaml')`. On Azure, this has to be done in the repo, since the `.libPaths()` in the repo doesn't have access to the same locations (except base) as the `.libPaths()` outside the repo.

*On Azure* (and maybe other Unix)- `xml2` causes all sorts of issues. Before using `renv::restore()`, in R, run `options(install.opts = "--no-lock")` and then `renv::install('xml2')`. That last may not be necessary (it might be handled by `renv::restore()`). Then proceed as below to install everything else.

Typically, we would next run `renv::restore()` which will use the local `renv.lock` file to build a local R environment for this project. Note if a `.Rprofile` is not present it will be created. If new packages are added (`renv::install('packagename')`) and want to be kept, add to the lockfile with `renv::snapshot()`.

`xml2` seems to be extra touchy to install on Unix, and errors sometimes with a message about `pkg-config` and sometimes about permissions to move things from the `00LOCK` directory. I am working on a smoother fix for this- currently tried `install.packages('xml2', dependencies = TRUE, INSTALL_opts = c('--no-lock'))` inside R, which worked but took *forever* and seems to have messed up my `activate.R` file.

### Python environments
Use the poetry in `/werptoolkitpy` to build the python environment in `/werptoolkitpy`. See the note in the werptoolkitr readme about what is needed on the python side to actually use the package- the short answer is a python environment with `py-ewr`. I'm working on making that automatic, for now, users will need to set that up manually. Can use WERP_toolkit_demo as a template.

## Using MDBA systems
To get to the compute machines, open Azure Machine Learning studio. Get there from [the Azure portal](https://portal.azure.com/#home), then Resources (green grid), then MDBA-NPD-ML-AUE, then 'Launch' (button in middle of main panel, NOT on side bar), then Compute (bottom of left column)

Start the computer

Click on VS code- this should start the main VScode on the Remote Desktop, which is connected into the Azure box. There may be a signin needed, and a connection step to do that the first time.

I think it's working if it says Azure ML up at the top of the vs code window (and down in the bottom left)

Now, that opens with CODE[AZURE ML… in the explorer
Then under Users/User.Name is where I work

The terminal down the bottom also opens into CODE, rather than my own directory- be careful when moving files.

`git clone` is unlikely to work here, without re-pointing to the git key every time, as mentioned above `ssh-add ~/.ssh/werp_key`.

Having the top level of the workspace be the repo is much nicer than the Users directory.
	Seems to work to click on the Open Folder link in the middle, and then in the toolbar popup navigage to the repo.

Then I git checkout the branch I want, and off to setting up environments.


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
Migrating functional toolkit to MDBA systems.