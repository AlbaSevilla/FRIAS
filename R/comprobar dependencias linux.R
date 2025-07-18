library(pak)


writeLines(pak::pkg_system_requirements("sf", "ubuntu", "20.04"))
writeLines(pak::pkg_system_requirements("CoordinateCleaner", "ubuntu", "20.04"))
writeLines(pak::pkg_system_requirements("spocc", "ubuntu", "20.04"))
writeLines(pak::pkg_system_requirements("tidyverse", "ubuntu", "20.04"))
writeLines(pak::pkg_system_requirements("devtools", "ubuntu", "20.04"))
writeLines(pak::pkg_system_requirements("kableExtra", "ubuntu", "20.04"))
