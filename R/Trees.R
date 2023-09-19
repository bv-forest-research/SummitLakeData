

#' Title
#'
#' @param raw_data
#' @param live
#' @param spp
#' @import data.table
#' @return
#' @export
#'
#' @examples
SL_calc_tree_size <- function(raw_data = "./data-raw/SummitLakeData.csv") {

  dat <- data.table::fread(raw_data)

  dat[, treeID := paste(Plot,TreeID,TreeID_new, sep = "_")]

  sl_trees <- dat[,.(Plot,treeID,Species, DBH_c_92_live, DBH_c_94_live, DBH_c_97_live, DBH_09_live, DBH_19_live,
                    DBH_92_dead, DBH_94_dead, DBH_97_dead, DBH_09_dead, DBH_19_dead,
                    COMMENTS_1992,COMMENTS_1994,	COMMENTS_1997,	COMMENTS_2009,COMMENTS_2019_May,
                    COMMENTS_2019_Oct)]
  sl_trees[,COMMENTS_2019 := paste(COMMENTS_2019_May,COMMENTS_2019_Oct)]
  sl_trees[,`:=`(COMMENTS_2019_May = NULL, COMMENTS_2019_Oct = NULL)]
  # Clean species codes - assuming "l" is for Larch
  sl_trees[, Species := ifelse(Species == "l", "Lw",Species)]

  # rename plot to unit and add treatments
  names(sl_trees)[names(sl_trees) == "Plot"] <- "unit"
  sl_trees <- merge(sl_trees, SummitLakeData::Treatments, by = "unit")

  #melt from wide to long - DBH
  datCols <- c("unit","treeID", "Species",grep("DBH", colnames(sl_trees),value = TRUE))
  m <- sl_trees[,..datCols]

  datCols <- grep("DBH", colnames(sl_trees),value = TRUE)
  m[,(datCols):= lapply(.SD, as.numeric), .SDcols = datCols]

  mm <- melt(m, measure.vars = datCols,
                       variable.name = "raw_var",
                       value.name = "DBH")

  #split out DBH by year and live/dead and
  mm[grep("live", raw_var), `:=`(State = "Live", Class = 1)]
  mm[grep("dead", raw_var), `:=`(State = "Dead", Class = 3)]

  mm[grep("92", raw_var), `:=`(Year = 1992)]
  mm[grep("94", raw_var), `:=`(Year = 1994)]
  mm[grep("97", raw_var), `:=`(Year = 1997)]
  mm[grep("09", raw_var), `:=`(Year = 2009)]
  mm[grep("19", raw_var), `:=`(Year = 2019)]

  #melt from wide to long - COMMENTS
  datCols <- c("unit","treeID", "Species",grep("COMMENTS", colnames(sl_trees),value = TRUE))
  cm <- sl_trees[,..datCols]

  datCols <- grep("COMMENTS", colnames(sl_trees),value = TRUE)
  cm[,(datCols):= lapply(.SD, as.character), .SDcols = datCols]

  cmm <- melt(cm, measure.vars = datCols,
             variable.name = "raw_var",
             value.name = "Comments")

  cmm[grep("1992", raw_var), `:=`(Year = 1992)]
  cmm[grep("1994", raw_var), `:=`(Year = 1994)]
  cmm[grep("1997", raw_var), `:=`(Year = 1997)]
  cmm[grep("2009", raw_var), `:=`(Year = 2009)]
  cmm[grep("2019", raw_var), `:=`(Year = 2019)]

  #bring back together

  sl <- merge(mm[,.(unit,treeID,Species,DBH,State,Class,Year)],
              cmm[,.(unit,treeID,Species,Comments,Year)],
              by = c("unit","treeID","Species","Year"), all = TRUE)
  #which are still standing
  sl[grep("Stand", Comments, ignore.case = TRUE), DeadStatus := "StandingDead"]

  #which are missing, dead or down
  sl[grep("Missing", Comments, ignore.case = TRUE), DeadStatus := "DownDead"]
  sl[grep("tag",Comments,ignore.case = TRUE), DeadStatus := NA]
  sl[grep("live",Comments, ignore.case = TRUE), DeadStatus := NA]
  sl[grep("dead",Comments, ignore.case = TRUE), DeadStatus := "Dead"]
  sl[grep("windthrow",Comments, ignore.case = TRUE), DeadStatus := "DownDead"]
  sl[grep("live",Comments, ignore.case = TRUE), DeadStatus := NA]
  sl[grep("ground",Comments, ignore.case = TRUE), DeadStatus := "DownDead"]
  sl[grep("Uprooted",Comments, ignore.case = TRUE), DeadStatus := "DownDead"]

  #sl[,`:=`(State = as.factor(State), DeadStatus = as.factor(DeadStatus))]
  sl[is.na(DeadStatus), DeadStatus := "NotDown"]
  # Dealing with DBH of dead trees is a bit complicated as evidence of
  # whether trees are standing or down is found in comments, but the DBH data
  # would look the same (a DBH for dead early and then blanks.)
  #get last DBH if died over the course of the monitoring
  setkeyv(sl, c("treeID","Year"))

  ids <- unique(sl$treeID)
  MeasYrs <- unique(sl$Year)

  #sl <- sl[!is.na(DBH)]

#is there a DBH for every year after measured the first time until it falls
for(i in 100:length(ids)){
  #what is the first year there is a DBH measurement
  fm <- sl[treeID == ids[i] & !is.na(DBH), min(Year)]

  #what's the last year there's a DBH measurement
  llm <- sl[treeID == ids[i] & !is.na(DBH), max(Year)]

  #did it fall and that's why there's no more measurements?
  if(nrow(sl[treeID == ids[i] & DeadStatus == "DownDead"])>0){
    nr <- sl[treeID == ids[i] & !is.na(DBH) & DeadStatus != "DownDead"]
  }

  #was the last measurement in the last possible year?
  if(llm == max(MeasYrs)){
    nr <- sl[treeID == ids[i] & !is.na(DBH)]
  }else{
    #if not, how many years remaining were surveyed
      yrs_rem <- as.numeric(grep(paste(MeasYrs, collapse ="|"),
                                 seq(max(llm,ldm)+1,max(MeasYrs)), value = TRUE))

      #get last DBH and copy the last status
      misDBH <- data.table(unit = unique(sl[treeID == ids[i]]$unit),
                           treeID = unique(sl[treeID == ids[i]]$treeID),
                           Species = unique(sl[treeID == ids[i]]$Species),
                           Year = yrs_rem,
                           DBH = sl[treeID == ids[i] & Year == llm& !is.na(DBH)]$DBH,
                           State = sl[treeID == ids[i] & Year == llm & !is.na(DBH)]$State,
                           Class = sl[treeID == ids[i] & Year == llm& !is.na(DBH)]$Class,
                           Comments = sl[treeID == ids[i] & Year == llm& !is.na(DBH)]$Comments,
                           DeadStatus = sl[treeID == ids[i] & Year == llm& !is.na(DBH)]$DeadStatus)


      nr <- rbind(sl[treeID == ids[i] & !is.na(DBH)],misDBH)

      #did the tree fall at some point for sure?
      if(nrow(nr[DeadStatus == "DownDead"])>0){
        #take away downed trees
        nr <- nr[DeadStatus != "DownDead"]

      }
      #It's still possible that the last measurement with no other information
      # represents a mortality and fall down, but impossible to say

  }


}




  sl_trees[, Height := treeCalcs::DiamHgtFN(Species = Species, DBH = DBH, BECzone = "SBS"),
               by= seq_len(nrow(sl_trees))]


    # Calculate carbon per tree

    sl_trees[, MgTree := treeCalcs::TreeCarbonFN(Species = Species,
                                                   DBH = DBH,
                                                   HT = Height,
                                                   Tree_class = Class),
               by= seq_len(nrow(raw_data_C))]

    #there was code if null species - but I don't see any nas or nulls




    raw_data <- raw_data %>%
      dplyr::select(Plot, Species, DBH_92_dead, DBH_94_dead, DBH_97_dead, DBH_09_dead, DBH_19_dead)

    # Clean species codes
    # I am assuming "l" is for Larch
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



}
