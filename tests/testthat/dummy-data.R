# Create dummy data for testing purpose
# Dataset not saved within package to reduce package size

# Set seed for reproducability
set.seed(1234)


##################
# Simple dataset #
##################

# Dimensions
N <- 20
M <- 10

# Create random content
simple_dummy <- cbind(
  data.frame(matrix(sample(1:50, size = N*ceiling(M/2), replace = TRUE), nrow = N, ncol = ceiling(M/2))), # integers
  data.frame(matrix(sample(LETTERS, size = N*floor(M/2), replace = TRUE), nrow = N, ncol = floor(M/2)))   # characters
)
names(simple_dummy) <- paste0("var", 1:M)


########################
# Demographics dataset #
########################

# Number of rows
n <- 100

# Create content
studyid <- rep("1234-5678", n)
domain <- rep("DM", n)
subjid <- as.character(1e3 + 1:n)

random_site_country <- sample(x = 1:5, size = n, replace = TRUE)
siteid <- as.character(floor(runif(5, min = 100, max = 1000)))[random_site_country]
country <- c("BEL", "NLD", "USA", "USA", "JPN")[random_site_country]

usubjid <- paste0(studyid, "-", "1", siteid, subjid)

rficdtc <- as.Date("2020-01-01") + sample(x = 1:100, size = n, replace = TRUE)
rfxstdtc <- rficdtc + sample(x = 1:100, size = n, replace = TRUE)
rfxendtc <- rfxstdtc + sample(x = 300:400, size = n, replace = TRUE)
rfstdtc <- rfxstdtc
rfendtc <- rfxendtc

age <- sample(x = 30:70, size = n, replace = TRUE)
ageu <- rep("YEARS", n)
sex <- sample(x = c("F", "M"), size = n, replace = TRUE)
arm <- sample(x = c("Drug 1", "Drug 2", "Placebo"), size = n, replace = TRUE)
race <- sample(x = c("WHITE","BLACK OR AFRICAN AMERICAN","AMERICAN INDIAN OR ALASKA NATIVE"), size = n, replace = TRUE)

# Gather content in one data frame
dm_dummy <- data.frame(
  STUDYID = studyid,
  DOMAIN = domain,
  USUBJID = usubjid,
  SUBJID = subjid,
  RFSTDTC = rfstdtc,
  RFENDTC = rfendtc,
  RFXSTDTC = rfxstdtc,
  RFXENDTC = rfxendtc,
  RFICDTC = rficdtc,
  SITEID = siteid,
  AGE = age,
  AGEU = ageu,
  SEX = sex,
  ARM = arm,
  COUNTRY = country,
  RACE = race
)

# Set labels
attributes(dm_dummy$STUDYID)$label <- "Study Identifier"
attributes(dm_dummy$DOMAIN)$label <- "Domain Abbreviation"
attributes(dm_dummy$USUBJID)$label <- "Unique Subject Identifier"
attributes(dm_dummy$SUBJID)$label <- "Subject Identifier for the Study"
attributes(dm_dummy$RFSTDTC)$label <- "Subject Reference Start Date/Time"
attributes(dm_dummy$RFENDTC)$label <- "Subject Reference End Date/Time"
attributes(dm_dummy$RFXSTDTC)$label <- "Date/Time of First Study Treatment"
attributes(dm_dummy$RFXENDTC)$label <- "Date/Time of Last Study Treatment"
attributes(dm_dummy$RFICDTC)$label <- "Date/Time of Informed Consent"
attributes(dm_dummy$SITEID)$label <- "Study Site Identifier"
attributes(dm_dummy$AGE)$label <- "Age"
attributes(dm_dummy$AGEU)$label <- "Age Units"
attributes(dm_dummy$SEX)$label <- "Sex"
attributes(dm_dummy$ARM)$label <- "Description of Planned Arm"
attributes(dm_dummy$COUNTRY)$label <- "Country"
attributes(dm_dummy$RACE)$label <- "Race"

# Rewrite data frame as tibble
dm_dummy <- tibble::as_tibble(dm_dummy)


##########################
# Adverse Events dataset #
##########################

# Create content based on DM dataset
num_ae <- sample(x = 0:5, size = n, replace = TRUE)
studyid <- rep(studyid, num_ae)
domain <- rep("AE", sum(num_ae))
usubjid <- rep(usubjid, num_ae)
arm <- rep(arm, num_ae)
aeseq <- 1:sum(num_ae)
aeterm <- sample(x = c("HEADACHE", "BACK PAIN", "FATIGUE", "NAUSEA"), size = sum(num_ae), replace = TRUE)
aesev <- sample(x = c("MILD", "MODERATE", "SEVERE"), size = sum(num_ae), replace = TRUE)
aeser <- rep("N", sum(num_ae))
aeser[aesev == "SEVERE"] <- "Y"
aerel <- sample(x = c("UNLIKELY RELATED", "POSSIBLY RELATED", "RELATED"), size = sum(num_ae), replace = TRUE)
aestdtc <- rep(rfxstdtc, num_ae) + sample(x = 0:200, size = sum(num_ae), replace = TRUE)
aeendtc <- aestdtc + sample(x = 0:200, size = sum(num_ae), replace = TRUE)


# Gather content in one data frame
ae_dummy <- data.frame(
  STUDYID = studyid,
  DOMAIN = domain,
  USUBJID = usubjid,
  ARM = arm,
  AESEQ = aeseq,
  AETERM = aeterm,
  AESEV = aesev,
  AESER = aeser,
  AEREL = aerel,
  AESTDTC = aestdtc,
  AEENDTC = aeendtc
)

# Set labels
attributes(ae_dummy$STUDYID)$label <- "Study Identifier"
attributes(ae_dummy$DOMAIN)$label <- "Domain Abbreviation"
attributes(ae_dummy$USUBJID)$label <- "Unique Subject Identifier"
attributes(ae_dummy$ARM)$label <- "Description of Planned Arm"
attributes(ae_dummy$AESEQ)$label <- "Sequence Number"
attributes(ae_dummy$AETERM)$label <- "Reported Term for the Adverse Event"
attributes(ae_dummy$AESEV)$label <- "Severity/Intensity"
attributes(ae_dummy$AESER)$label <- "Serious Event"
attributes(ae_dummy$AEREL)$label <- "Causality"
attributes(ae_dummy$AESTDTC)$label <- "Start Date/Time of Adverse Event"
attributes(ae_dummy$AEENDTC)$label <- "End Date/Time of Adverse Event"

# Rewrite data frame as tibble
ae_dummy <- tibble::as_tibble(ae_dummy)


