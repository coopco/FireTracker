library(dplyr)
library(magrittr)

files <- list.files(path="Datasets/ACORN-SATv2/daily_tmax/", full.names = TRUE) %>% lapply(read.csv)

for (i in 1:length(files)) {
    files[[i]]$site.number <- rep(files[[i]]$site.number[1])
    files[[i]]$site.name <- rep(files[[i]]$site.name[1])
    files[[i]] <- files[[i]][-1,]
}

files <- bind_rows(files)

write.csv(files, "Datasets/acorn_merged.csv", row.names = FALSE)

