# --- release_afextremism.R ---
# This script runs af_release_package() for afextremism with initial version

library(afcommon)

new_version <- af_release_package(
  package_path = '.',
  github_repo = 'amirhome61/afextremism',
  version_bump = "minor", # change to "patch" or "major" as needed
  release_message = "New Release of afextremism Package"
)

# Remove local af_* functions to avoid conflicts
to_remove <- ls(envir = .GlobalEnv, pattern = '^af_')
if (length(to_remove) > 0) {
  rm(list = to_remove, envir = .GlobalEnv)
}

# Unload the package if already loaded
if ('afextremism' %in% loadedNamespaces()) {
  unloadNamespace('afextremism')
}

# Install and load the newly released package from GitHub
remotes::install_github(paste0(
  'https://github.com/amirhome61/afextremism@v',
  new_version
))
library('afextremism', character.only = TRUE)

message(sprintf(
  'Successfully released and loaded afextremism version %s',
  new_version
))
