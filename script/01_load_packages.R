
message("\n...running 01_load_packages.R")

#####################
## DEFINE PACKAGES ##
#####################

## define core packages required to handle other packages installations
core_packages <- c(
  "pacman",
  "remotes",
  "installr",
  "pkgbuild",
  "rio",
  "webshot",
  "matchmaker",
  "stats",
  "epitrix",
  "utils",
  "stringr",
  "epikit",
  "forcats",
  "tidyverse",
  "shiny"
)

## define packages on github that DON'T require rtools
github_packages <- c(
  "cttobin/ggthemr"
)

## define local package paths
# local_packages_path <- dir(
# gsub("(?<=equateur_mve_2020/).*$","3. Analysis/0. Global Scripts/local_packages", getwd(), perl = TRUE),
#   full.names = TRUE
# )

## define local package paths
# local_packages_name <- dir(
#   gsub("(?<=equateur_mve_2020/).*$","3. Analysis/0. Global Scripts/local_packages", getwd(), perl = TRUE),
#   full.names = FALSE
# )

######################
## INSTALL PACKAGES ##
######################

## list installed packages
available_packages <- .packages(all.available = TRUE)

## install core packages
for (pkg in core_packages) {
  if (!pkg %in% available_packages) {
    suppressWarnings(try(utils::install.packages(pkg, repos = "https://cloud.r-project.org")))
  }
}

## install local packages
# for(i in seq_along(local_packages_path)) {
#   if (!local_packages_name[i] %in% available_packages) {
#     suppressMessages(remotes::install_local(local_packages_path[i]))
#   }
# }

## install github packages
for(repo in github_packages) {
  pkg <- strsplit(repo, "/")[[1]][2]
  if(!pkg %in% available_packages) {
    remotes::install_github(repo)
  }
}

## install phantomjs
if(!webshot::is_phantomjs_installed()) webshot::install_phantomjs()


## load packages required across all scripts

pacman::p_load(pacman, here, tidyverse, magrittr, stringi)


#############################
## INSTALL RTOOLS PACKAGES ##
#############################

## this is currently not attempted because installing Rtools failed on too many
## people's computers

## ## define packages that DO require rtools - an installation will be attempted
## ## but might fail if rtools cannot be installed
## github_packages_rtools <- c(
##   "reconhub/linelist"
## )

## ## install rtools using installr if possible
## if(!pkgbuild::has_rtools() & "installr" %in% .packages(all.available = TRUE)) {
##   installr::install.rtools(check = TRUE)
## }

## ## install github_rtools packages if rtools is available
## if(pkgbuild::has_rtools()) {
##   for(repo in github_packages_rtools) {
##     pkg <- strsplit(repo, "/")[[1]][2]
##     if(!pkg %in% available_packages) {
##       remotes::install_github(repo)
##     }
##   }
## } else {
##   message(sprintf("skipping installation of %s as Rtools could not be installed",
##                   paste0(github_packages_rtools, collapse = ",")))
## }

