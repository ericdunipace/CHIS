# .Rprofile
if (!nzchar(Sys.getenv("RENV_PROFILE"))) {
  Sys.setenv(RENV_PROFILE = if (getRversion() < "4.3") "default" else "local")
}
source("renv/activate.R")
