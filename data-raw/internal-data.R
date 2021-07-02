# Create internal data objects required for the UI

library(GenomeInfoDb)
library(usethis)
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

manifest <- manifest %>%
  subset(!is.na(code)) %>%
  subset(str_detect(code, "^\\d")) %>%
  subset(md5s != "<pending>") %>%
  transform(code = unlist(code, use.names = FALSE))

.tbl_phenotypes <- manifest %>%
  subset(description %in% phenotypes) %>%
  subset(sex == "both_sexes") %>%
  subset(select = c("code", "description", "file"))


# export ------------------------------------------------------------------

usethis::use_data(
  .supported_genomes,
  .tbl_phenotypes,
  internal = TRUE,
  overwrite = TRUE
)
