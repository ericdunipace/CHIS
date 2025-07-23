library(readxl)
library(glue)

pkg   <- readxl::read_xlsx(here::here("docs","dac-r-and-stata-packages-list-2.xlsx"),
                               sheet = 4L,
                           col_type = c("text","text"))
names <- pkg$Package
ver   <- pkg$Version

lockfile <- renv::lockfile_read("renv.lock")
package_names <- names(lockfile$Packages)

final_names <- names[names %in% package_names]
final_ver   <- ver[names %in% package_names]

final_ver[final_names == "askpass"]   <- "1.1"
final_ver[final_names == "gridExtra"] <- "2.3"

#### Set package versions ####
for( i in seq_along(final_names)) {
  n <- final_names[i]
  v <- final_ver[i]
  tryCatch(renv::install(glue("{n}@{v}"),
                lock = TRUE, prompt = FALSE,
                dependencies = "strong", type = "binary"),
           error = function(e) {
             tryCatch(renv::install(glue("{n}@{v}"),
              dependencies = "strong", prompt = FALSE,
              lock = TRUE,
              type = "binary",
              repos = "https://cloud.r-project.org/"),
              error = function(e) {
                renv::install(glue("{n}"),
                              dependencies = "strong", prompt = FALSE,
                              lock = TRUE)
              })
    })
}

renv::snapshot()

