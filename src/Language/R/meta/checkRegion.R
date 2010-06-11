#!/usr/bin/env r
# encoding: utf-8


library("DBI");
library("RSQLite");

regions <- read.delim("/Users/davidrosenberg/regions167.txt", header=TRUE);
drv <- dbDriver("SQLite");
con <- dbConnect("SQLite");


checkRegion <- function(chromosome, startposition, endposition) {
  probes <- dbGetQuery(con, sprintf("SELECT probeset_id, start, end from Annotations where chromosome=%d and start>%d and end<%d", chromosome, startposition, endposition));
  idx <- (1:length(probes$probeset_id))[substr(probes$probeset_id, 1, 3) == "JAX"]
  starts <- c(startposition, probes$start);
  ends <- c(probes$end, endposition)
  s <- as.numeric(na.omit(starts[c(1, idx+2)]));
  e <- (ends[c(idx, length(idx))])[1:length(s)];
}
