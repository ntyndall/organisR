<img align="right" width="125" height="150" src="https://raw.githubusercontent.com/ntyndall/organisR/master/images/sticker.png">


# organisR
This package is intended to help analyse and organise your R packages. It is essentially a set of tools to help clean and tidy code and organise the internal functionality of your code. To get started, install from github by running the following script;
```r
library(devtools)

devtools::install_github(
  repo = "ntyndall/organisR"
)

library(footballstats)
```
or, add the above remote to your `DESCRIPTION` file.

# Functionality
### Check for dead code
Assuming that function calls respective to the package you are working on is written as `{packagename}::{function_name}`, then this call can figure out a call stack and identify functions that are never called. A vector of entry points must be supplied, i.e. this could be a main function call, or a bunch of scripts. To get started with this, change directory to your R project, `cd my-project/`, then from the command line you can run
```shell
Rscript -e "organisR::dead(c('demo/*', 'main', 'main2'))"
```
This will use all the files located in the subdirectory `/demo/`, and also the functions defined in `/R/` (`main` and `main2`).

### Tagging a release for github
This function creates a binary to be uploaded to github for tagging a release. Just follow the 3 steps below,
  - Update `DESCRIPTION` file to required version (must be > than most recent `GIT` version).
  - Run `Rscript -e "organisR::tag()"` inside root directory i.e. `/packagename/`.
  - Update Github with the newest tag that is reported in step 2, and the binary that is produced and saved in `/tagged/`.

### Investigating data sets
Activate an interactive script to look and investigate scripts within your package and check its documentation. Just run the following,
```shell
Rscript -e "organisR::data()"
```
[[(https://github.com/ntyndall/organisR/master/images/data-screen.png)]]
