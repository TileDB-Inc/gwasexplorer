# Create internal data objects required for the UI

library(GenomeInfoDb)
library(usethis)
library(dplyr)
library(purrr)
library(stringr)
library(googlesheets4)

# Genome Chromosomes ------------------------------------------------------
# Create tables with chromosome lengths for each supported genome version
# names(GenomeInfoDb:::SUPPORTED_UCSC_GENOMES)

genomes <- c(
  grch37 = "hg19"
)

.supported_genomes <- genomes %>%
  map(~ GenomeInfoDb::Seqinfo(genome = .x)) %>%
  map(GenomeInfoDb::keepStandardChromosomes) %>%
  map(~ {
    GenomeInfoDb::seqlevelsStyle(.x) <- "NCBI"
    return(.x)
  }) %>%
  map(GenomeInfoDb::seqlengths)


# UKBioBank Result Files --------------------------------------------------

phenotypes <- c(
  "Sleep duration",
  "Getting up in morning",
  "Nap during day",
  "Snoring"
)

# no need to auth
googlesheets4::gs4_deauth()

manifest <- googlesheets4::read_sheet(
  googlesheets4::as_sheets_id("1kvPoupSzsSFBNSztMzl04xMoSC3Kcx3CrjVf4yBmESU"),
  sheet = "Manifest 201807",
  na = "N/A"
)

colnames(manifest) <- colnames(manifest) %>%
  str_to_lower() %>%
  str_replace_all(" ", "_") %>%
  str_remove("phenotype_")

gwas_manifest <- manifest %>%
  filter(!is.na(code)) %>%
  filter(str_detect(code, "^\\d")) %>%
  filter(md5s != "<pending>") %>%
  mutate(code = unlist(code, use.names = FALSE)) %>%
  filter(sex == "both_sexes") %>%
  # keep one version of results per description
  group_by(description) %>%
  slice(1) %>%
  ungroup()

# randomly sample
set.seed(123)
gwas_manifest <- slice_sample(gwas_manifest, n = 500)

.tbl_phenotypes <- gwas_manifest %>%
  subset(select = c("code", "description", "file"))

# temporary table containing only phenotypes from useR tutorial
tutorial_phenos <- c(
  "Water intake",
  "Milk chocolate intake",
  "Duration of fitness test",
  "Irritability",
  "Ventricular rate",
  "General happiness"
)

.tbl_tutorial_phenotypes <- .tbl_phenotypes %>%
  filter(description %in% tutorial_phenos)


# export ------------------------------------------------------------------

usethis::use_data(
  .supported_genomes,
  .tbl_phenotypes,
  .tbl_tutorial_phenotypes,
  internal = TRUE,
  overwrite = TRUE
)
