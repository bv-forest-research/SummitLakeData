

#' Title
#'
#' @param raw_data
#' @import data.table
#' @return
#' @export
#'
#' @examples
clean_trees <- function(raw_data = "./data-raw/SummitLakeData.csv") {

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

  #fix the degrees which had an exit dash
  cmm[unit == 17 & treeID == "17_308_" & raw_var == "COMMENTS_2009", Comments := "live - 45 lean"]
  cmm[unit == 18 & treeID == "18_474_" & raw_var == "COMMENTS_2009", Comments := "live - 45 lean"]
  #bring back together

  sl <- merge(mm[,.(unit,treeID,Species,DBH,State,Class,Year)],
              cmm[,.(unit,treeID,Species,Comments,Year)],
              by = c("unit","treeID","Species","Year"), all = TRUE)

  #which are still standing
  sl[grep("Stand", Comments, ignore.case = TRUE), DeadStatus := "StandingDead"]

  #which are missing, dead or down
  sl[grep("Missing", Comments, ignore.case = TRUE), DeadStatus := "DownDead"]
  #the missing ones are messed up, because they could have fallen or were just missed (still alive)
  sl[grep("tag",Comments,ignore.case = TRUE), DeadStatus := NA]
  sl[grep("live",Comments, ignore.case = TRUE), DeadStatus := NA]
  sl[grep("dead",Comments, ignore.case = TRUE), DeadStatus := "Dead"]
  sl[grep("windthrow",Comments, ignore.case = TRUE), DeadStatus := "DownDead"]
  sl[grep("windthrown",Comments, ignore.case = TRUE), DeadStatus := "DownDead"]
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

  #is there a DBH for every year after measured the first time until it falls?
  #if it dies does it come back to life?
  nra <- c()
  #cwda <- c()
  for(i in 1:length(ids)){
    #what is the first year there is a DBH measurement
    fm <- sl[treeID == ids[i] & !is.na(DBH), min(Year)]

    #what's the last year there's a DBH measurement
    llm <- sl[treeID == ids[i] & !is.na(DBH), max(Year)]

    #check if it was dead and then went live again
      #for those trees that had a live dbh at some point
      if(nrow(sl[treeID == ids[i] & !is.na(DBH) & State == "Live"])>0){
        #what is the year of that live DBH
        ml <- sl[treeID == ids[i] & !is.na(DBH) & State == "Live", max(Year)]
        #the max year that it was alive, everything before has to be live too
        sl[treeID == ids[i] & Year <= ml, DeadStatus := "NotDown"]
      }


    #it it fell, did it stand up again?
    if(nrow(sl[treeID == ids[i] & DeadStatus == "DownDead"])>0){
      mf <- sl[treeID == ids[i] & DeadStatus == "DownDead", max(Year)]
      if(nrow(sl[treeID == ids[i] & DeadStatus == "Dead"])>0){
        #break()
        dy <- sl[treeID == ids[i] & DeadStatus == "Dead", max(Year)]
        if(mf < dy){

          sl[treeID == ids[i] & Year >= mf, DeadStatus := "DownDead"]
        }
      }

      #if(nrow(sl[treeID == ids[i] & is.na(DBH) & DeadStatus == "DownDead" &
       #          State == "Dead" & Year <=2019 & Year >mf])>0){
        #if it's missing dbh because it fell, give the last one

      #}

    }

    #did it fall and that's why there's no more measurements?
    #if(nrow(sl[treeID == ids[i] & DeadStatus == "DownDead"])>0){
     # nr <- sl[treeID == ids[i] & !is.na(DBH) & DeadStatus != "DownDead"]
      #cwd <- sl[treeID == ids[i] & !is.na(DBH) & DeadStatus == "DownDead"]
      #break()
    #}

    #was the last DBH measurement in the last possible year?
    if(llm == max(MeasYrs)){
      nr <- sl[treeID == ids[i] & !is.na(DBH)]
      #cwd <- sl[treeID == ids[i] & !is.na(DBH)]
    }else{
      #break()
      if(nrow(sl[treeID == ids[i] & is.na(DBH)])>0){

        #break()
        ldm <- max(sl[treeID == ids[i] & is.na(DBH), .(Year)])
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
          #if(nrow(nr[DeadStatus == "DownDead"])>0){
           # break()
            #take away downed trees
            #nr <- nr[DeadStatus != "DownDead"]
            #cwd <- nr[DeadStatus == "DownDead"]

          #}
          #It's still possible that the last measurement with no other information
          # represents a mortality and fall down, but impossible to say
      }else{
        #break()
      }
    }

    nra <- rbind(nra, nr)
    #cwda <- rbind(cwda, cwd)

  }

 # nra[, Height := treeCalcs::diam_hgt_summitLake(Species = Species,
  #                                               DBH = DBH,
   #                                              BECzone = "SBS"),
    #  by= seq_len(nrow(nra))]
  #height using same allometry as date creek
  nra[, Height := treeCalcs::height_dbh(Species = Species,
                                                 DBH = DBH,
                                                 BECzone = "SBS"), by= seq_len(nrow(nra))]
  # Calculate carbon per tree
  nra[, Kg_treeC := treeCalcs::calc_tree_c(Species = Species,
                                                   DBH = DBH,
                                                   HT = Height,
                                                   Tree_class = Class),
               by= seq_len(nrow(nra))][, Mg_treeC := Kg_treeC/1000]
  return(nra)

}

#' Number of planted trees per hectare by PSP
#'
#' @param planted_data where is the planted trees data
#' @return
#' @export
#'
#' @examples
#'
#' @details
#' Hybrid spruce (Sx) was filled planted in 14 experimental units to establish Spruce in un- or
#' poorly stocked stands. These included PSPs 3, 15, and 24, but looking at the data, it looks like
#' many plots had planted trees
#'
#' plot area = 0.05 ha
#'
#'
planted_trees <- function(planted_data = "./data-raw/Trees/plantedTrees.csv"){

  plantTrees <- fread(planted_data)

  #remove trees that are outside the plot
  plantTrees <- plantTrees[!grep("Outside|outside", COMMENTS_2021),]

  sph_plant <- plantTrees[, .(SPH = (.N/0.05)), by = .(Plot)]
  return(sph_plant)

}

natural_regen <- function(nat_reg_dat){

}





#' Plot tree summary - by species and size class
#'
#' @param dbh_size_class
#' @param minDBH
#' @param plot_area
#' @param raw_data
#'
#' @return
#' @export
#'
#' @examples
plot_sph_size <- function(dbh_size_class = 2,
                          plot_area = 0.05,
                          raw_data = "./data-raw/Trees/SummitLakeData.csv") {

  sl_dat <- SummitLakeData::clean_tree_data(raw_data = raw_data)
  summary(lm(Height ~ Hgt_allom, data = sl_dat[!is.na(Height)]))

  minDBH <- 8
  maxDBH <- max(sl_dat$DBH, na.rm = TRUE)
  # Create a vector of DBH size classes, by 2 cm increments
  diam_classes <- seq(minDBH,(maxDBH + dbh_size_class),
                      by = dbh_size_class)

  # Replace old tree tag numbers with new, if applicable
  #raw_data$TreeID <- as.numeric(raw_data$TreeID)
  #raw_data$TreeID_new <- as.numeric(raw_data$TreeID_new)
  #raw_data <- raw_data %>%
  #  dplyr::mutate(TreeID = ifelse(is.na(TreeID_new), TreeID, TreeID_new))
  #raw_data$TreeID_new <- NULL

  # Create column for and fill with DBH bins
  for(j in 1:length(diam_classes)){
    sl_dat[DBH <= diam_classes[j] & DBH > diam_classes[j] - dbh_size_class,
           DBH_bin := diam_classes[j]]
  }

  #add all zeros:
  all_poss <- CJ(unique(sl_dat$unit), unique(sl_dat$Species), unique(sl_dat$Year),
                 unique(sl_dat$State), unique(sl_dat$DBH_bin))
  setnames(all_poss,c("V1","V2","V3","V4","V5"),
           c("unit","Species","Year","State","DBH_bin"))

  #merge with sl_dat
  merge(sl_dat, all_poss, by = c("unit","Species","Year","State","DBH_bin"), all = T)


  sl_dat[, .(SPH = .N/plot_area), by = .(unit,Species,Year,State,DBH_bin)]

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



}


