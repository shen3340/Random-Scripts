installed <- as.data.frame(installed.packages()) 
write.csv(installed, "installed_previously.csv")

library(installr)
updateR()

baseR <- as.data.frame(installed.packages())
installed_previously <- read.csv("installed_previously.csv")
toInstall <- setdiff(installed_previously, baseR)
final.installlist<- as.vector(unlist(toInstall[1]))
install.packages(final.installlist)

# packages not on CRAN
library(devtools)
devtools::install_github("abresler/nbastatR")
devtools::install_github("josedv82/airball")
devtools::install_github("lbenz730/ncaahoopR")
devtools::install_github("SBGSports/catapultr", dependencies=TRUE, build_vignettes=TRUE)
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)
devtools::install_github("jflancer/bigballR")
devtools::install_github(repo = "JackLich10/gamezoneR")
devtools::install_github("snestler/wncaahoopR")
remotes::install_github("jthomasmock/espnscrapeR")
devtools::install_github("andreweatherman/cbbdata")
devtools::install_github("tmking2002/softballR")
devtools::install_github("andreweatherman/toRvik")

