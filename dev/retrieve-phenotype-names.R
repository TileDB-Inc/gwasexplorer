library(tiledb)

uri <- "s3://genomic-datasets/gwas/ukbiobank-gwasdb"

query <- list(
  phenotype = NULL,
  chr = cbind("1", "1"),
  pos = cbind(0, 1e+06)
)

tdb <- tiledb_array(
    uri,
    query_type = "WRITE",
    as.data.frame = TRUE,
    attrs = c("pval"),
    selected_ranges = query
)

system.time(results <- tdb[])

unique(results$phenotype)

which(unique(results$phenotype) == "Number in household")
