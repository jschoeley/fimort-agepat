#################################################################
# DOWNLOAD AND READ DATA ON US BIRTHS, INFANT- AND FETAL DEATHS #
#################################################################

# (1) Download data on US births, fetal and infant mortality from the web,
# (2) Read the data into R applying variable specifications stored in custom
#     codebook.

# Init --------------------------------------------------------------------

# path to save data
infant_path = "/home/jon/lucile/share/Dropbox/sci/2015-10-fimort-agepat/priv/data/raw/nchs-us_cohort_linked_birth_infant_deaths/"
fetus_path  = "/home/jon/lucile/share/Dropbox/sci/2015-10-fimort-agepat/priv/data/raw/nchs-us_fetal_deaths/"

# path to codebooks
infant_cbook_path = "./code/00-download_and_read_raw_data/codebook-us_cohort_infant_births_deaths_minimal.yaml"
fetus_cbook_path  = "./code/00-download_and_read_raw_data/codebook-us_fetal_deaths_minimal.yaml"

# Functions to read our custom codebook -----------------------------------

# Read Codebook from Path
ReadCodebook <- function (path) {
  cbook <- yaml::yaml.load_file(input = path)
  cbook$files <- OrderVarByStartPosition(cbook)
  return(cbook)
}

# Construct Get Function
GetConstructor <- function (cbook_file, secondary, tertiary) {
  sapply(cbook_file[[secondary]],
         FUN = function (cbook_file) {
           unlist(getElement(cbook_file, name = tertiary))
         })
}

# Get File Names of Files Defined in Codebook
GetFileName <- function(cbook) {unlist(lapply(cbook$files, getElement, name = "filename"))}
# Get URL of Files Defined in Codebook
GetURL <- function(cbook) {unlist(lapply(cbook$files, getElement, name = "url"))}
# Get Variable Names from Codebook File Node
GetVarName <- function (cbook_file) {names(cbook_file$variables)}
# Get Variable Start Position from Codebook File Node
GetVarStart <- function (cbook_file) {GetConstructor(cbook_file, "variables", "start")}
# Get Variable End Position from Codebook File Node
GetVarEnd <- function (cbook_file) {GetConstructor(cbook_file, "variables", "end")}
# Get Variable Type from Codebook File Node
GetVarType <- function (cbook_file) {GetConstructor(cbook_file, "variables", "type")}

# Order the Variable Specifications Within Each File by Column Start Position
OrderVarByStartPosition <- function (cbook) {
  # the variables must be read in order of column start position or otherwise
  # readr gets confused
  cbook$files <-
    lapply(
      cbook$files, function (x)  {
        within(x, {variables = variables[order(GetVarStart(x))]})
      }
    )
}

# Read Fixed Width File Using Codebook Specification
CBookrFWF <- function (path, cbook_file) {
  cat(paste("Reading file", cbook_file$filename, "\n"))
  # depends on readr >= 1.0.0 for the capability to read column subsets
  dat <- readr::read_fwf(file = path,
                         col_positions = readr::fwf_positions(start     = GetVarStart(cbook_file),
                                                              end       = GetVarEnd(cbook_file),
                                                              col_names = GetVarName(cbook_file)),
                         col_types = paste0(TranslateVarType(cbook_file), collapse = "")
  )
  cat(paste("Lines:", nrow(dat), "Variables:", ncol(dat), "\n"))
  return(dat)
}

# Translate Variable Type into String Defining Column Types for read_fwf
TranslateVarType <- function (cbook_file) {
  var_type <- GetVarType(cbook_file)
  translation <- dplyr::recode(var_type,
                               "logical" = "l",
                               "integer" = "i",
                               "double" = "d",
                               "character" = "c",
                               "factor" = "c")
  return(translation)
}

# Apply Variable Specifications from Codebook To Data Frame
ApplyVarSpec <- function (x, cbook_file) {
  vars <- cbook_file$variables
  for (i in 1:ncol(x)) { # for all the variables in the data
    cat(paste(names(vars)[i], x[1:5,i], "\n"))
    # if NAs are available apply NAs
    if (!is.null(vars[[i]]$missing_values)) {
      cat(paste("  Defining NAs as", vars[[i]]$missing_values, "\n"))
      x[,i] <- ifelse(unlist(x[,i]) %in% vars[[i]]$missing_values, NA, unlist(x[,i]))
    }
    # if variable type is integer, convert column to integer
    if (vars[[i]]$type == "integer") {
      cat(paste("  Convert to integer", "\n"))
      x[,i] <- as.integer(unlist(x[,i]))
    }
    # if variable type is double, convert column to double
    if (vars[[i]]$type == "double") {
      cat(paste("  Convert to double", "\n"))
      x[,i] <- as.double(unlist(x[,i]))
    }
    # if variable type is factor and categories are given,
    # convert column to factor and apply factor levels and labels
    if (vars[[i]]$type == "factor" & (!is.null(vars[[i]]$categories))) {
      cat(paste("  Convert to factor", "\n"))
      # extract levels and labels
      level <- names(vars[[i]]$categories)
      label <- unlist(vars[[i]]$categories)
      cat(paste0("    ", level, ": ", label, "\n", collapse = ""))
      # include only levels and labels which don't appear in missing values
      # so that NAs stay NAs
      level_rmna <- level[!(level %in% vars[[i]]$missing_values)]
      label_rmna <- label[!(level %in% vars[[i]]$missing_values)]
      x[,i] <- factor(unlist(x[,i]),
                      levels  = level_rmna,
                      labels  = label_rmna)
      if(vars[[i]]$scale == "ordinal") {
        x[,i] <- as.ordered(unlist(x[,i]))
      }
    }
    # if variable type is date and format is given,
    # convert column to date
    if (vars[[i]]$type == "date" & (!is.null(vars[[i]]$format))) {
      cat(paste("  Convert to date", "\n"))
      x[,i] <- readr::parse_date(as.character(unlist(x[,i])), format = vars[[i]]$format)
    }
  }
  return(x)
}

# Read codebook -----------------------------------------------------------

infant_cbook = ReadCodebook(infant_cbook_path)
fetus_cbook  = ReadCodebook(fetus_cbook_path)

# Download data -----------------------------------------------------------

# download files
DownloadFiles <- function (url, save_path) {
  for (i in url) {
    cat("Download", i)
    download.file(url = i,
                  destfile = paste0(save_path,
                                    rev(strsplit(i, "/")[[1]])[1] # file name part of url
                  ),
                  mode = "wb")
  }
}

# download US cohort linked infant birth / death data
# data
DownloadFiles(GetURL(infant_cbook), infant_path)
# guides
DownloadFiles(paste0("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/DVS/cohortlinked/",
                     "LinkCO", c(89:91, 95:99, paste0(0, 0:9), 10), "Guide", ".pdf"), infant_path)

# Download US fetal deaths data
# data
DownloadFiles(GetURL(fetus_cbook), fetus_path)
# guides
DownloadFiles(paste0("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
                     "Dataset_Documentation/DVS/fetaldeath/", 1989:2010, "FetalUserGuide", ".pdf"), fetus_path)

# Read data into R, apply varspecs and store as RData ---------------------

ReadFromZip <- function (cbook, local_path) {
  # file names of files on fetal data
  file_names = GetFileName(cbook)
  archive_names = sapply(strsplit(GetURL(cbook), "/"), function(x) rev(x)[1])
  # initiate list to hold data
  dat = vector("list", length(file_names)); names(dat) = file_names
  # read data
  for (i in seq_along(file_names)) {
    temp <- CBookrFWF(unz(paste0(local_path, archive_names[i]),
                          file_names[i]),
                      cbook_file = cbook$files[[i]])
    dat[[i]] <- ApplyVarSpec(temp, cbook$files[[i]]); rm(temp)
  }
  return(dat)
}

# read, specify and save data on infant deaths
fdeath <- ReadFromZip(fetus_cbook, fetus_path)
save(fdeath, file = "./priv/data/pre-harmonized/us_fdeath_1989-2010.RData")

# read, specify and save data on births and fetal deaths
ideath <- ReadFromZip(infant_cbook, infant_path)
save(ideath, file = "./priv/data/pre-harmonized/us_ideath_1989-2010.RData")
