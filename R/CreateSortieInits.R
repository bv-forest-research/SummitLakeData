

# 2021-12-16
# By: Leah Walker

# A function to clean the Summit lk 1992 - 2019 data

prepSumLakeSORTIE <- function(dbhClSize,MinDBHClass,MaxDBHClass,PlotArea,
                              raw_data,inits_dir) {

  # Create a vector of DBH size classes, by 2 cm increments
  diamClasses <- seq(MinDBHClass,(MaxDBHClass+dbhClSize), by=dbhClSize)

  # Eliminate unneeded columns
  #this selects the rows you want and leaves the file you read in alone
  raw_data <- raw_data %>%
    dplyr::select(Plot, Species, DBH_c_92_live, DBH_c_94_live)



  # Clean species codes
  # I am assuming "l" is for Larch
  raw_data <- raw_data %>%
    dplyr::mutate(Species = replace(Species, Species == "l", "Lw"))



  ### NEEDS FIXING ###
  raw_data <- raw_data %>%
    dplyr::mutate(DBH_c_92_live = ifelse(is.na(DBH_c_92_live), DBH_c_94_live, DBH_c_92_live))


  # Eliminate trees with no DBH measurement for that year
  raw_data <- subset(raw_data, DBH_c_92_live != "NA")



  # Replace old tree tag numbers with new, if applicable
  #raw_data$TreeID <- as.numeric(raw_data$TreeID)
  #raw_data$TreeID_new <- as.numeric(raw_data$TreeID_new)
  #raw_data <- raw_data %>%
  #  dplyr::mutate(TreeID = ifelse(is.na(TreeID_new), TreeID, TreeID_new))
  #raw_data$TreeID_new <- NULL



  # Rename columns to match Parameter Files
  names(raw_data)[names(raw_data) == "Plot"] <- "unit"
  names(raw_data)[names(raw_data) == "Species"] <- "Spp"
  names(raw_data)[names(raw_data) == "DBH_c_92_live"] <- "DBH"
  #str(raw_data) # Verify names are correct



  # Create one unit (plot) label per species
  labels.summit.sp <- merge(unique(raw_data$unit), unique(raw_data$Spp), fill = TRUE)
  names(labels.summit.sp) <- c("unit","Spp")



  # Create SORTIE DBH classes
  # Turn data frame into data table
  raw_data <- as.data.table(raw_data)



  # Create column for and fill with DBH bins
  for(j in 1:length(diamClasses)){
    raw_data[DBH <= diamClasses[j] & DBH > diamClasses[j]-dbhClSize,DBH_bin := diamClasses[j]]
  }



  # Create data table of the diameter classes
  diamDT <- data.table()
  diamDT[,DBH_bin := diamClasses]



  # Create a label for each diameter class of each species of each plot
  labels.summit.spD <- as.data.table(merge(labels.summit.sp, diamDT, fill=TRUE))



  # Merge labels with data, possibly an unnecessary step
  dat.summit.m <- merge(labels.summit.spD, raw_data, all = TRUE)
  dat.summit.m <- subset(dat.summit.m, DBH != "NA")



  # Count number of trees per DBH bin, per species, per unit
  dat.summit.m.s <- dat.summit.m %>%
    dplyr::group_by(unit, Spp, DBH_bin) %>%
    dplyr::mutate(count = n())
  dat.summit.m.s <- as.data.table(dat.summit.m.s)
  # Calculate stems per hectare
  dat.summit.m.s[, SPH := count/ PlotArea]



  # Remove unnecessary columns
  #dat.summit.m.s$TreeID <- NULL
  dat.summit.m.s$DBH <- NULL
  dat.summit.m.s$count <- NULL



  # Merge labels with data set including SPH
  dat.summit.SPH <- merge(labels.summit.spD, dat.summit.m.s, all = T)
  cols <- "SPH"
  # Now that the counts are done, fill in empty DBH bins with zero
  dat.summit.SPH[,(cols) := lapply(.SD,nafill, fill = 0), .SDcols = cols]
  # Eliminate duplicates
  dat.summit.SPH <- unique(dat.summit.SPH)



  # Create new column that names the DBH bins as they need to be for SORTIE
  for(i in 1:nrow(dat.summit.SPH)){
    dat.summit.SPH[i, variable := paste0("Init.Dens_",dat.summit.SPH[i,DBH_bin], ".0"),]
  }




  # Create a data table with all the species headings, but one row of NAs
  DT <- data.table(1)[, `:=`(c("variable", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"), NA)][,V1 := NULL]

  TS92 <- data.table("variable" = "Timesteps",
                     "Sx" = 27)



  # Translate the data from long to short and create a csv for each unit (plot)
  for(ii in unique(dat.summit.SPH$unit)){
    dat.unit <- dat.summit.SPH[unit == ii]
    dat.unit$unit <- NULL
    dat.unit$DBH_bin <- NULL
    dat.unit <- dcast(dat.unit, variable ~ Spp, value.var = "SPH")
    dat.unit <- as.data.table(dat.unit)
    dat.unit[, c("Pl", "At", "Ac") := 0]
    setcolorder(dat.unit, c("variable", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"))
    dat.unit <- rbindlist(list(DT, dat.unit[1:nrow(dat.unit)]), use.names = TRUE, fill = TRUE)
    dat.unit <- rbindlist(list(dat.unit, TS92), use.names = TRUE, fill = TRUE)
    setnames(dat.unit, c("variable", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"),
             c(" ", "Interior_Spruce", "Lodgepole_Pine", "Subalpine_Fir", "Trembling_Aspen",
               "Western_Larch", "Douglas_Fir", "Black_Cottonwood", "Paper_Birch"))

    if(dir.exists(inits_dir)){
      write.csv(dat.unit, paste0(inits_dir,"summit_",ii,".csv"), row.names = FALSE)
    }else{
      dir.create(inits_dir)
      write.csv(dat.unit, paste0(inits_dir,"summit_",ii,".csv"), row.names = FALSE)
    }


  }

}
