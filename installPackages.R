# installs a list of packages
currentpkgs = installed.packages()[,1] 

# required packages
pkgnames = c(
  "tidyverse",
  "BiocManager",
  "magrittr",
  "argparse",
  "readxl"
)

missingpkgs = setdiff(pkgnames, currentpkgs)

# install package
(R_INSTALL_STAGED = FALSE)
install.packages(missingpkgs, dependencies = T, quiet = T)
currentpkgs = installed.packages()[,1]
missingpkgs = setdiff(pkgnames, currentpkgs)

if (!length(missingpkgs) == 0) {
  noquote(c("The following packages(s) failed to install: ", missingpkgs))
} else {
  noquote("All packages installed successfully")
}

# proprietary installation
BiocManager::install("PrInCE")