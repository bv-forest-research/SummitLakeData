raw_data <- raw_data %>%
  dplyr::mutate(Species = replace(Species, Species == "l", "Lw"))

# Repeat dead DBH's for following years
raw_data <- raw_data %>% dplyr::mutate(DBH_09_dead = ifelse(is.na(DBH_97_dead), DBH_09_dead, DBH_97_dead))
raw_data <- raw_data %>% dplyr::mutate(DBH_19_dead = ifelse(is.na(DBH_09_dead), DBH_19_dead, DBH_09_dead))

raw_data_C <- as.data.table(raw_data)


# rename plot to unit and add treatments
names(raw_data_C)[names(raw_data_C) == "Plot"] <- "unit"

treatment <- data.table(unit = c(3, 6, 9, 10, 16, 17, 20, 4, 8, 11, 12, 15, 18, 5, 7, 13, 14, 19, 24),
                        treatment = c(rep("ctrl", times = 7), rep("med", times = 6), rep("low", times = 6)))

raw_data_C <- merge(raw_data_C, treatment, by = "unit")



# Create a column for tree class, all deadso given a class of 3
raw_data_C[, Class := 3]


# Calculate height per tree
HT92_D <- vector()
for(i in 1:nrow(raw_data_C)){
  HT92_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_92_dead])
}

HT94_D <- vector()
for(i in 1:nrow(raw_data_C)){
  HT94_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_94_dead])
}

HT97_D <- vector()
for(i in 1:nrow(raw_data_C)){
  HT97_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_97_dead])
}

HT09_D <- vector()
for(i in 1:nrow(raw_data_C)){
  HT09_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_09_dead])
}

HT19_D <- vector()
for(i in 1:nrow(raw_data_C)){
  HT19_D[i] <- DiamHgtFN(Species = raw_data_C[i, Species], DBH = raw_data_C[i, DBH_19_dead])
}

raw_data_C[, ':='(HT_92_D = HT92_D)]
raw_data_C[, ':='(HT_94_D = HT94_D)]
raw_data_C[, ':='(HT_97_D = HT97_D)]
raw_data_C[, ':='(HT_09_D = HT09_D)]
raw_data_C[, ':='(HT_19_D = HT19_D)]



# Calculate carbon per tree
C92_D <- vector()
for(ii in 1:nrow(raw_data_C)){
  C92_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_92_dead],
                            HT = raw_data_C[ii, HT_92_D], Tree_class = raw_data_C[ii, Class])
}

C94_D <- vector()
for(ii in 1:nrow(raw_data_C)){
  C94_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_94_dead],
                            HT = raw_data_C[ii, HT_94_D], Tree_class = raw_data_C[ii, Class])
}

C97_D <- vector()
for(ii in 1:nrow(raw_data_C)){
  C97_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_97_dead],
                            HT = raw_data_C[ii, HT_97_D], Tree_class = raw_data_C[ii, Class])
}

C09_D <- vector()
for(ii in 1:nrow(raw_data_C)){
  C09_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_09_dead],
                            HT = raw_data_C[ii, HT_09_D], Tree_class = raw_data_C[ii, Class])
}

C19_D <- vector()
for(ii in 1:nrow(raw_data_C)){
  C19_D[ii] <- TreeCarbonFN(Species = raw_data_C[ii, Species], DBH = raw_data_C[ii, DBH_19_dead],
                            HT = raw_data_C[ii, HT_19_D], Tree_class = raw_data_C[ii, Class])
}


raw_data_C[, ':='(C_92_D = C92_D/1000)]
raw_data_C[, ':='(C_94_D = C94_D/1000)]
raw_data_C[, ':='(C_97_D = C97_D/1000)]
raw_data_C[, ':='(C_09_D = C09_D/1000)]
raw_data_C[, ':='(C_19_D = C19_D/1000)]

# Calculate C per plot
raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_92_D_unit = sum(C_92_D, na.rm = TRUE)*20)
raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_94_D_unit = sum(C_94_D, na.rm = TRUE)*20)
raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_97_D_unit = sum(C_97_D, na.rm = TRUE)*20)
raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_09_D_unit = sum(C_09_D, na.rm = TRUE)*20)
raw_data_C <- raw_data_C %>% group_by(unit) %>% mutate(C_19_D_unit = sum(C_19_D, na.rm = TRUE)*20)



# Clean up columns
raw_data_C <- raw_data_C %>%
  dplyr::select(unit, treatment, C_92_D_unit, C_94_D_unit, C_97_D_unit, C_09_D_unit, C_19_D_unit)


raw_data_C <- unique(raw_data_C)

raw_data_C <- melt(raw_data_C, id.vars = c("unit", "treatment"),
                   measure.vars = c("C_92_D_unit", "C_94_D_unit", "C_97_D_unit", "C_09_D_unit", "C_19_D_unit"))



names(raw_data_C)[names(raw_data_C) == "variable"] <- "timestep"
names(raw_data_C)[names(raw_data_C) == "value"] <- "C_unit"



raw_data_C <- raw_data_C %>%
  dplyr::mutate(timestep = as.character(timestep))

raw_data_C <- raw_data_C %>%
  dplyr::mutate(timestep = replace(timestep, timestep == "C_92_D_unit", 0),
                timestep = replace(timestep, timestep == "C_94_D_unit", 2),
                timestep = replace(timestep, timestep == "C_97_D_unit", 5),
                timestep = replace(timestep, timestep == "C_09_D_unit", 17),
                timestep = replace(timestep, timestep == "C_19_D_unit", 27))

raw_data_C <- raw_data_C %>%
  dplyr::mutate(timestep = as.numeric(timestep))

raw_data_C <- subset(raw_data_C, C_unit != 0.0)

raw_data_C <- unique(raw_data_C)

write.csv(summit.lk.dat_C_D, "./Outputs/csv/SummitLake_C_field_D.csv", row.names = FALSE)




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
