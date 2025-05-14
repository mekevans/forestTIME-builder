# read fixed and mixed effects coefficients
# all mixed models using regular schumacher-hall
# species-level fixed models are variable

coef_round <- 12

roundIt <- function(x) {
  
  if (class(x) == 'numeric') round(x, coef_round) else x
  
}

#setwd("~/Documents/Work/R_Data/CRM2/Coefs/")

flist1 <- list.files("Coefs/variable_model_forms/")
flist2 <- list.files("Coefs/nlme")
flist3 <- list.files("Coefs/Caribbean/")

eq_guide <- read.csv("Files/crm2_equation_guide.csv", as.is= TRUE)

readCoefs <- function(file_name) {
  
  f1 <- read.csv(paste0("Coefs/variable_model_forms/", file_name), as.is= TRUE)
  f3 <- read.csv(paste0("Coefs/Caribbean//", file_name), as.is= TRUE)
  
  to_add <- names(f1)[!names(f1) %in% names(f3)]
  
  for (i in to_add) f3[,i] <- NA
  
  f1 <- rbind(f1, f3)
  
  f1[!is.na(f1$b), "equation"] <- 3
  f1[!is.na(f1$b0), "equation"] <- 4
  f1[!is.na(f1$c1), "equation"] <- 5
  f1[!is.na(f1$b2), "equation"] <- 50
  
  f2 <- read.csv(paste0("Coefs/nlme/", file_name), as.is= TRUE)
  
  # for the wdsg model
  # use coefficients from fixeff model
  # ranef model blows up with wdsg
  f2 <- f2[!is.na(f2$SPCD), ]
  f2[!is.na(f2$b), "equation"] <- 3
  
  if (file_name %in% c("rcumib_coefs.csv", "rcumob_coefs.csv")) {
    
    f1$equation <- NULL
    f2$equation <- NULL
    
  }
  
  # too many foliage coefficients
  f2 <- f2[!(f2$SPCD %in% f1$SPCD) | is.na(f2$SPCD),]
  
  to_add <- names(f1)[!names(f1) %in% names(f2)]
  
  for (i in to_add) f2[,i] <- NA
  
  out <- rbind(f1, f2)
  
  out[] <- lapply(out, roundIt)
  
  return(out)
  
}

all_coefs <- lapply(flist1, readCoefs)
names(all_coefs) <- flist1

# name <- names(all_coefs)[1]
writeIt <- function(name) {
  
  fn <- paste0("Coefs/combined/", name)
  
  cat("saving", "\n")
  
  dat <- all_coefs[[name]]
  
  write.csv(dat, fn, row.names= FALSE)
  
  
}

lapply(names(all_coefs), writeIt)

