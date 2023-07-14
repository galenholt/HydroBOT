## Getting Started


## Git/github

Set up an ssh key to be able to talk to github. Largely following Github and Andrew's instructions (below for Linux and Windows). Use an RSA key, not the default ed25519, and *do not name it*. Overriding defaults is a pain, so just use them.

Basically, setting up the github ssh key follows the [github instructions](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent), and then adding that key to your github account, again [as described by github](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account). This sets up the key in `~/.ssh`. To be safe, I typically cd there with `cd ~/.ssh` to start, though most of the commands put it there no matter where you are. 

A couple small notes:
-   Don't name the file- leave it as the default 'id_TypeOfEncyption'. `ssh-add` auto adds 'id_*' files, but not other names. 
-   Don't include a passphrase (just press enter when it asks for one)- that just yields another layer of signin complexity. 
-   The github docs for copying the public key sometimes swap OSes,  `clip` seems to work on windows with git bash, `cat` works on Linux.

If we stopped here, things would work, but we'd have to use `eval "$(ssh-agent -s)"` and then `ssh-add ~/.ssh/NAME_OF_KEY` every time. So, change the `.bashrc` according to Andrew to auto-run `ssh-add` . I tend to use nano for small edits, so `nano ~/.bashrc` (or `cd` and then `nano .bashrc`), and copy-paste in the below. (and on Windows also add this to `~/.profile`)

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

### Git username
Configure git username and email. In bash, `git config --global user.name "User Name"` and `git config --global user.email "user.email@email"`. See [github instructions](https://docs.github.com/en/get-started/getting-started-with-git/setting-your-username-in-git) and [email](https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-personal-account-on-github/managing-email-preferences/setting-your-commit-email-address). *Use the `noreply` github email or github gets mad when you push*. If you have a Deakin computer, you probably have to set the config `--local` once you've cloned because of the horrible `h/` drive HOME.

After you have the ssh key, and ideally git user name and email, clone the repo.

### Windows issues

If you're on a Deakin computer, HOME is set to the horrible `/h` drive that is locked down and changes. To fix that (I hope), Georgia followed instructions from [stackoverflow](https://superuser.com/questions/1190364/how-to-set-home-directory-in-win10), specifically 

From the Start menu, enter env to display the Environment Variables window. Click environment variables button in bottom right.
Enter a new $HOME variable for my account:
Variable Name: HOME
Value: C:\Users\USERNAME

Confirm by starting a bash and typing `cd ~`.

Setting up ssh on Windows is easiest if you use git bash. That should be fine for typical repo use, but we have to do more to get ssh to work for *installing* packages from protected repos. To prepare for this, there seem to be an inability to pass anything other than rsa keys, so create one of those (I have one of those and and an ed25519, but I'm pretty sure the rsa is auto-enforced with the `install_git`). The rigmarole for actually installing from github is dealt with in the developer.md for WERP_toolkit_demo, since that's where we need to install this from.

Windows should connect with SSH so we're using the same system everywhere. What seems to be working is to go to Settings \--\> Services (really, search for Services), \--\> openSSH authentication \--\> properties \--\> startup type Automatic. Run `ls-remote git@github.com:MDBAuth/WERP_toolkit.git` in command prompt interactively to add github as a known location. Then create a `~/.profile` and `~/.bashrc` with the same bits as in the linux bashrc in the werptoolkitr dev docs, and are given [at the github instructions for auto-launching](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/working-with-ssh-key-passphrases#auto-launching-ssh-agent-on-git-for-windows). Both profile and bashrc seem to be needed. And it's unclear why, since those are for bash, and `install_git` calls cmd. cmd must be calling git bash internally. Then, we also need to install the `git2r` package, or `install_git` will still fail, but we *cannot* pass the `credentials` argument, even though that seems like what we should do.

## R 

### General setup
R versions may not match, especially on Azure, which is typically behind. Currently (late December 2022) R is at 4.2.2 and that's what I've been developing on. 

We can use [rig](https://github.com/r-lib/rig) to manage R versions (and this is likely better anyway, because we can swap around). It does seem to run with the old version, but it will be good to be able to choose. Also see [my notes](https://galenholt.github.io/RpyEnvs/rig.html) for some install notes.

Once you have rig installed, Then, `rig list` to see available R versions (likely none- it doesn't store its versions with the system). Then `rig add 4.2.2` (or whatever the current version is). There are ways to use Rstudio with the version in the `renv.lock`, and likely ways to do the same with VS, but for now, easiest is to make sure `renv default` matches the version in `renv.lock`.

### Azure
The R version that comes on the machines is 4.0, and the usual `apt-get` isn't finding anything newer. To get rig for Linux, at the bash terminal, type `curl -Ls https://github.com/r-lib/rig/releases/download/latest/rig-linux-latest.tar.z | sudo tar xz -C /usr/local` to install rig.  On the Azure boxes, there are a lot of missing libs the R packages depend on. As of this writing, I hit errors with libcurl, libjq, libgeos, libudunits2, libgdal, and libxml2. One option is to try to `renv::restore()` and fix these as they come, but likely easier to just hit them all at once with `sudo apt-get install libcurl4-openssl-dev libjq-dev libgeos-dev libgdal-dev libudunits2-dev libxml2-dev`. Note that if you do need to install after a failure in R and it tells you what it needs, you can use `system("sudo apt-get install packagename")` if you want to stay within R and not switch to bash.

If the Azure connection crashes, and the Explorer panel all turns yellow, you have to stop and restart the Azure VM. 

### Windows
On Windows, you'll need Rtools, whichever version matches your local R. Rtools provides access to compilation of C/C++/Fortran. Mac and Unix just have that built-in. I think there's a commant `rig add rtools4x` to have rig manage the install. Ideally, we'd have rig auto-load the right version, but Rstudio broke that functionality. So need to use `rig default 4.v.v` before opening the project. This can be a pain to switch if we're using different versions across different projects, but does work.



## Project setup

### R environments
If starting for the first time you will need to have `renv` installed. Typically, starting an R session through Rstudio or at the command line (`R`) will automatically detect the absence of `renv` and install it. If not, at the R command prompt type `install.packages('renv')`.  Worth having a read of [renv doco](https://rstudio.github.io/renv/).

I ended up having to `install.packages('languageserver')` inside the repo on Azure, because it won't talk to the global `.libPaths`. This takes a really long time. It's only necessary if we are using VScode (yes on Azure, maybe on Windows).

The `yaml` package is required for `renv` to parse its lockfile, so `install.packages('yaml')`. On Azure, this has to be done in the repo, since the `.libPaths()` in the repo doesn't have access to the same locations (except base) as the `.libPaths()` outside the repo.

*On Azure* (and maybe other Unix)- `xml2` causes all sorts of issues. Before using `renv::restore()`, in R, run `options(install.opts = "--no-lock")` and then `renv::install('xml2')`. That last may not be necessary (it might be handled by `renv::restore()`). Then proceed as below to install everything else.

Typically, we would next run `renv::restore()` which will use the local `renv.lock` file to build a local R environment for this project. Note if a `.Rprofile` is not present it will be created. If new packages are added (`renv::install('packagename')`) and want to be kept, add to the lockfile with `renv::snapshot()`.

`xml2` seems to be extra touchy to install on Unix, and errors sometimes with a message about `pkg-config` and sometimes about permissions to move things from the `00LOCK` directory. I am working on a smoother fix for this- currently tried `install.packages('xml2', dependencies = TRUE, INSTALL_opts = c('--no-lock'))` inside R, which worked but took *forever* and seems to have messed up my `activate.R` file.

### Python environments
Use the poetry in `/werptoolkitpy` to build the python environment in `/werptoolkitpy`. See the note in the werptoolkitr readme about what is needed on the python side to actually use the package- the short answer is a python environment with `py-ewr`. I'm working on making that automatic, for now, users will need to set that up manually. Can use WERP_toolkit_demo as a template.

## VScode
The R environment stuff within the project is managed by `renv`, but there are some packages we want to install globally because they make R work in VScode, help development, etc. So, *outside the repo* (or before cloning it), do these things. 

To use R in VScode, there are a few things to do, mostly listed at the [VScode docs](https://code.visualstudio.com/docs/languages/r). The essential ones are to install `languageserver` (`install.packages('languageserver')`) and then the R extension in VScode. However, I sometimes have to install it inside the repo. I think `.libPaths` doesn't retain some global paths on Azure that it retains elsewhere. VScode will prompt to install if it can't find it.

I'm experimenting with `radian` and `httpgd`, and think I like them, but they're not necessary. And there are other linters, debuggers, etc beyond what comes with the VScode R extension, but I haven't played with them yet.

If you want to install [radian](github.com/randy3k/radian) as a terminal, it should be global as well. The azure boxes need to `pip3 install prompt-toolkit --upgrade` before `pip3 install -U radian` or we get TypeErrors. Hoping this doesn't screw up the python envs? Guess we'll see. When we start a `radian` terminal, it says 'python', not 'R' because radian is written in python. Which is a bit confusing, especially when we're also actually doing python work. In practice, I like some of the things about radian, but have turned it off because it throws weird errors (but runs through them) that aren't real ('unexpected & in }' when neither symbol is in the code). I just don't fully trust it. I *think* the issue is that it doesn't handle running code chunks in Quarto correctly- it's effectively copy-pasting them into the terminal and isn't doing it right.

I finally got radian working, but only if I *uninstall* it from the base python and install it with poetry in each venv. Then we have to set the Rterm path in the R extension settings. On Linux,  `which radian` to get the location, and put that (`/anaconda/envs/azureml_py38/bin/radian`) in `Rterm:Linux` in the settings.
On Windows, `which radian` pointed to the shims, but installing radian *in the repo itself with poetry* and then using `C:\\path\\to\\repo\\.venv\\Scripts\\radian.exe` for the Rterm windows. Also in settings, set the Always Use Active Terminal and Use Bracked Paste settings. *DO NOT USE BRACKETED PASTE IF YOU SWITCH BACK TO VANILLA R TERMINAL*. The active terminal deal is because if we try to auto-start radian from a notebook, it barfs on the python env. But if we start it with `radian` from bash/powershell, then it gets the python env right and we can go. *BUT*, I have dropped it again because it doesn't fully support UTF-8, and there are UTF-8 characters all over the data- e.g. "Macquarie–Castlereagh" has an En-Dash that gets dropped silently and everything breaks. Until that's sorted, it's just not worth it. It's also slower than base R, though the highlighting and code completion is really nice, the introduced bugs are terrible. And for some reason it doesn't work with httpgd. 

Installing `httpgd` globally as well (inside R, `install.packages('httpgd')`).

In settings, I also turned on `LSP:debug` and clicked `use httpgd` in the plots settings. I'll try to keep a record here of these setup things I've done so we don't forget and have different VS behaviour.

VS seems to sometimes struggle to find the library if started in WERP_toolkit. In that case, start a new workspace in WERP_toolkit/werptoolkitr. 

VS opened in WERP_toolkit or werptoolkitr needs to be told the first time we open a workspace where the python interpreter is (in /werptoolkitpy/.venv/Scripts/python.exe) because it's no longer in the top-level.

## Notes on using MDBA systems
To get to the compute machines, open Azure Machine Learning studio. Get there from [the Azure portal](https://portal.azure.com/#home), then Resources (green grid), then MDBA-NPD-ML-AUE, then 'Launch' (button in middle of main panel, NOT on side bar), then Compute (bottom of left column)

Start the computer

Click on VS code- this should start the main VScode on the Remote Desktop, which is connected into the Azure box. There may be a signin needed, and a connection step to do that the first time.

I think it's working if it says Azure ML up at the top of the vs code window (and down in the bottom left)

Now, that opens with CODE[AZURE ML… in the explorer
Then under Users/User.Name is where I work

The terminal down the bottom also opens into CODE sometimes, rather than my own directory- be careful when moving files.

If `git clone` doesn't work, it's likely because you named your ssh key. Either make a default key or `ssh-add ~/.ssh/KEY_NAME`.

Having the top level of the workspace be the repo is much nicer than the Users directory.
	Seems to work to click on the Open Folder link in the middle, and then in the toolbar popup navigage to the repo.

Then I git checkout the branch I want, and off to setting up environments.

## Building data and HTML documentation
Some of the data needs to be built, at least right now. And we'll always have the option to do that as the data changes. The easiest way to do it is to run `quarto render -P REBUILD_DATA` from WERP_toolkit to rebuild all `.qmd`, `.rmd`, and `.md` files everywhere and rebuild the data. Leaving off the parameter (just `quarto render`) re-renders the project with the default of FALSE if we just want new HTML but not new data. Individual files can also be rendered, either with the CLI or the Render buttons in VS or Rstudio. *Note*- due to a bug that I can't find, the only way to run `scenario_creation.qmd` (the python version) is locally with the button, after re-enabling the `execute` flag in the YAML header. For some reason it kills the gauge-getter when run from the command line. The R equivalent (`scenario_creation_R_demo`) *does* render from CLI, even though it also uses the same code that fails in the other, and so we still rebuild that data on a full rebuild.

### Overview

This is a self-contained R package, largely following [Hadley](https://r-pkgs.org/). *Please* try to retain package structure, and do not `setwd()`- all paths should be canonical package directory referencing. In *analysis* repos that use this package, we still should never `setwd()`, though *maybe* some limited paths will point to fixed shared data resources.

The exception to the R code is the Scenario Controller and some initial test data creation, which are written in python/quarto and link closely with the [EWR tool](https://github.com/MDBAuth/EWR_tool). These are in `data-creation`, which is hidden when building the pkg.

The data in `data/` is test data, and so will be included in the repo, but will hopefully change little. It can be rebuilt with notebooks in `data_creation/`, which also adds data to `data-raw/`, which should not be tracked in the repo. The exception (currently) is data that cannot be gathered from elsewhere, but we should move away from this as possible.

### Current status
Migrating functional toolkit to MDBA systems.