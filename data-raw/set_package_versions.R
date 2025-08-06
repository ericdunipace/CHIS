library(readxl)
library(glue)
library(here)

# pkg   <- readxl::read_xlsx(here::here("docs","dac-r-and-stata-packages-list-2.xlsx"),
#                                sheet = 4L,
#                            col_type = c("text","text"))
# names <- pkg$Package
# ver   <- pkg$Version
# 
# lockfile      <- renv::lockfile_read("renv.lock")
# package_names <- names(lockfile$Packages)
# package_ver   <- sapply(lockfile$Packages, function(x) x$Version)
# 
# final_names   <- names[names %in% package_names]
# final_ver     <- ver[names %in% package_names]
# 
# if("askpass" %in% final_names) final_ver[final_names == "askpass"]   <- "1.1"
# if("gridExtra" %in% final_names) final_ver[final_names == "gridExtra"] <- "2.3"
# 
# #### Set package versions ####
# for( i in seq_along(final_names)) {
#   n <- final_names[i]
#   v <- final_ver[i]
#   if ( package_ver[which(package_names == n)] == v ) next
#   tryCatch(renv::install(glue("{n}@{v}"),
#                 lock = TRUE, prompt = FALSE,
#                 dependencies = "strong", type = "binary"),
#            error = function(e) {
#              tryCatch(renv::install(glue("{n}@{v}"),
#               dependencies = "strong", prompt = FALSE,
#               lock = TRUE,
#               type = "binary", dependencies = "strong", 
#               repos = "https://cloud.r-project.org/"),
#               error = function(e) {
#                 renv::install(glue("{n}@{v}"),
#                               dependencies = "strong", prompt = FALSE,
#                               lock = TRUE, dependencies = "strong", )
#               })
#     })
# }


pkg   <- readxl::read_xlsx(here::here("docs","dac-r-and-stata-packages-list-2.xlsx"),
                           sheet = 4L,
                           col_type = c("text","text"))
names <- pkg$Package
ver   <- pkg$Version

lockfile      <- renv::lockfile_read("renv.lock")
package_names <- names(lockfile$Packages)
package_ver   <- sapply(lockfile$Packages, function(x) x$Version)

final_names   <- names[names %in% package_names]
final_ver     <- ver[names %in% package_names]

if("askpass" %in% final_names) final_ver[final_names == "askpass"]   <- "1.1"
if("gridExtra" %in% final_names) final_ver[final_names == "gridExtra"] <- "2.3"

for( i in seq_along(final_names)) {
  n <- final_names[i]
  v <- final_ver[i]
  if ( package_ver[which(package_names == n)] == v ) next
  renv::record(glue("{n}@{v}"))
}

inst_packages <- installed.packages()

package_names <- inst_packages[,"Package"]
package_ver   <- inst_packages[,"Version"]

final_names   <- names[names %in% package_names]
final_ver     <- ver[names %in% package_names]

if("askpass" %in% final_names) final_ver[final_names == "askpass"]   <- "1.1"
if("gridExtra" %in% final_names) final_ver[final_names == "gridExtra"] <- "2.3"

for( i in seq_along(final_names)) {
  n <- final_names[i]
  v <- final_ver[i]
  if ( length(package_ver[which(package_names == n)]) == 1 && package_ver[which(package_names == n)] == v ) next
  renv::record(glue("{n}@{v}"))
}

renv::restore()
