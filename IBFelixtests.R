require(devtools)
setwd('~/Documents/')
setwd('~/Dropbox/otherRstuff')
getwd()
build('IBFelix', binary = FALSE)
build('Felix', binary = FALSE)
install.packages("~/Documents/IBFelix_1.0.tar.gz", repos = NULL, type = "source")
install.packages("~/Dropbox/otherRstuff/Felix_1.0.tar.gz", repos = NULL, type = "source")
require(Felix)
require(IBFelix)
counter
download_daily

detach('package:IBFelix')
rm(counter)
counter
counter()
search()
searchpaths()
search()
ls(all.names = T)
ls(envir = 0x10934b3a0)
ls('package:IBFelix', all.names = T)
ls('pkg.env', all.names = T)
ls(envir = IBFelix.pkg.env)
IBFelix.pkg.env$count
ls('namespace:IBFelix:pkg.env', all.names = T)

View(pkg.env)
exists('fritz', inherits=TRUE)
exists('fritz', envir = pkg.env)
door.env <- new.env()
door.env2 <- environment()
door.env
ls()

namespace:IBFelix

# https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
# You could set an option, eg

options("IBFelix.pkg.env"=3)
1+getOption("IBFelix.pkg.env")
getOption("IBFelix.pkg.env")
getOption
liss = options()
