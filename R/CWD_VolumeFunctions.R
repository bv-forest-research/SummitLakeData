#' Title
#'
#' @param CWD_dat
#' @param Horiz_dat
#' @param out_carbon_comp
#'
#' @return
#' @export
#'
#' @examples
CWD_2021_Vol_calc <- function(CWD_dat = "./data-raw/EP1162CWDsurvey2020-2021.csv",
                              Horiz_dat = "./data-raw/EP1162CWDsurveyTransectLines2020-2021.csv",
                              out_carbon_comp = FALSE){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1992
  CWD.2021 <- data.table::fread(CWD_dat, na.strings = "-")
  CWD.2021[, `Accum/Odd-length(cm)`:=NULL]

  CWD.2021[, unit := as.numeric(stringr::str_split_fixed(Plot, "-",
                                                         n=2)[,2])]
  CWD.2021 <- merge(CWD.2021, SummitLakeData::Treatments, by = c("unit"))
  # Square diameter
  CWD.2021[, D2_cosA:= `diam(cm)`^2]

  if(out_carbon_comp == FALSE){
    CWD2021_plot <- CWD.2021[, .(D2cosA = sum(na.omit(D2_cosA))),
                             by =c("Plot", "unit","treatment")]
  }else{
    CWD2021_plot <- CWD.2021[, .(D2cosA = sum(na.omit(D2_cosA))),
                             by =c("Plot","unit", "treatment",
                                   "Species", "DecayClass")]
  }


  # Import horizontal transect csv file then convert individual lines to plot summary
  transect <- data.table::fread(Horiz_dat)
  transect[, unit := as.numeric(stringr::str_split_fixed(Plot, "-",
                                                         n=2)[,2])]
  transect <- merge(transect, SummitLakeData::Treatments, by = c("unit"))
  transectPlot <- transect[, .(HorDist = sum(HorizontalDist)), by =c("unit","treatment")]

  # Merge horizontal distances with all CWD at the plot level
  CWD2021_plot <- merge(CWD2021_plot, transectPlot, by = c("unit", "treatment"))

  # Volume (m3/ha) calculation
  CWD2021_plot[, VolumeHa:= pi^2/(8*HorDist) * D2cosA]
  CWD2021_plot <- CWD2021_plot[!is.na(Species)]

  CWD2021_plot[, `:=`(Year = 2020, Yrs_Post = 2020-1992, Unit = unit,
                      Sp = ifelse(Species =="Unk","U",Species),
                      Decay = DecayClass, VolumeHa = VolumeHa)]
  CWD2021_plot <- CWD2021_plot[,.(Year, Yrs_Post, Unit, Sp, Decay, VolumeHa)]

  return(CWD2021_plot)

}


#' General SBS coarse woody debris
#'
#' @param CWD_dat
#' @param size_thresh default to FALSE, if pass a number, that is used as the cutoff in size class
#' @param out_carbon_comp
#'
#' @return
#' @export
#' @description
#' Bruce Rogers data from the SBS - original file name "Erica-SB Plot Data-CWD-Regen". Assuming
#' 2 x 30m transect lengths
#'
#'
#' @examples
cwd_sub_boreal_vol <- function(CWD_dat,
                               size_thresh = FALSE, out_carbon_comp = FALSE){

  cwd <- fread(CWD_dat)

  # Square diameter
  cwd[, D2_cosA:= `Diam. (cm)`^2, by = seq_len(nrow(cwd))]

  #remove nas for diameter"
  cwd <- cwd[!is.na(`Diam. (cm)`)]


  # assuming BI should be Bl, but it could be Ep?
  cwd[, Species := ifelse(Species == "at", "At",
                          ifelse(Species == "BI", "Bl",
                                 ifelse(Species == "SX", "Sx",
                                        ifelse(Species == "u", "U", Species))))]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  if(out_carbon_comp == FALSE){
    if(size_thresh){
      # Category for small and large logs
      cwd[, sizeGr:= ifelse(`Diam. (cm)` < size_thresh, "small", "large")]
      cwd_plot <- cwd[, .(D2cosA = sum(D2_cosA)),
                      by =c("Plot #", "sizeGr")]
    }else {
      cwd_plot <- cwd[, .(D2cosA = sum(D2_cosA)),
                      by =c("Plot #")]
    }

  }else{
    #if volume will be used for carbon, need to keep species and decay class columns
    if(size_thresh){
      # Category for small and large logs
      cwd[, sizeGr:= ifelse(`Diam. (cm)` < size_thresh, "small", "large")]
      cwd_plot <- cwd[, .(D2cosA = sum(D2_cosA)),
                      by =c("Plot #","Species", "Decay Class", "sizeGr")]

    }else {
      cwd_plot <- cwd[, .(D2cosA = sum(D2_cosA)),
                      by =c("Plot #","Species", "Decay Class")]

    }
  }

  # Volume (m3/ha) calculation (include transect length). I've assumed 2 30m transects
  # but do not have documentation to support that is the total
  cwd_plot[, VolumeHa:= (pi^2/(8*60)) * D2cosA]

  return(cwd_plot)
}


#' Title
#'
#' @param CWD_dat
#' @param size_thresh
#'
#' @return
#' @export
#'
#' @examples
cwd_sub_boreal_diams <- function(CWD_dat, size_thresh = 15){
  #get mean diameters
  #Species groups
  cwd <- fread(CWD_dat)
  #remove nas for diameter"
  cwd <- cwd[!is.na(`Diam. (cm)`)]

  # assuming BI should be Bl, but it could be Ep?
  cwd[, Species := ifelse(Species == "at", "At",
                          ifelse(Species == "BI", "Bl",
                                 ifelse(Species == "SX", "Sx",
                                        ifelse(Species == "u", "U", Species))))]
  cwd[, sizeGr:= ifelse(`Diam. (cm)` < size_thresh, "small", "large")]
  # Species groups -----
  Group1 <- c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3 <- c("At","Ac","Ep")
  cwd[,SpGrp := ifelse(Species %in% Group1, 1,
                       ifelse(Species %in% Group3, 3, 2 ))]
  cwd_mnLogs <- cwd[, .(mean_diam = mean(`Diam. (cm)`)),
                    by = .(`Plot #`, sizeGr, SpGrp, `Decay Class`)]

  return(cwd_mnLogs)

}



#' Title
#'
#' @param CWD_dat
#' @param size_thresh
#' @param out_carbon_comp
#'
#' @return
#' @export
#'
#' @examples
cwd_sub_boreal_props <- function(CWD_dat, size_thresh = 15, out_carbon_comp = TRUE){

  # Import 1992 data and calc volume
  cwd_vol <- cwd_sub_boreal_vol(CWD_dat = CWD_dat,
                                size_thresh = size_thresh,
                                out_carbon_comp = TRUE)

  # Species groups -----
  Group1 <- c("Hw","Ba", "Bl","Sx","Pl","U")
  Group3 <- c("At","Ac","Ep")
  cwd_vol[,SpGrp := ifelse(Species %in% Group1, 1,
                           ifelse(Species %in% Group3, 3, 2 ))]

  #total volume in the plot
  cwd_plot <- cwd_vol[,.(VolumeHa = sum(VolumeHa)),
                      by = .(`Plot #`, SpGrp, sizeGr, `Decay Class`)]
  #check calculations
  #cwd92_Unit[,.(VolumeHa = sum(VolumeHa)), by = .(Unit)]
  cwd_d <- cwd_sub_boreal_diams(CWD_dat, size_thresh = size_thresh)


  cwd_plot <- merge(cwd_plot, cwd_d,
                    by = c("Plot #", "sizeGr", "SpGrp", "Decay Class"),
                    all.x = TRUE)

  # calculate proportion log area
  cwd_plot[, PLA := VolumeHa /(1/3 * pi * (mean_diam/ 2))]

  # Percent area logs by whole unit...using mean diameters for every combination
  cwd_plot[,.(PLA = sum(PLA)), by = .(`Plot #`)]

  sp_decay_size <- CJ(`Plot #` = unique(cwd_plot$`Plot #`),
                      SpGrp = c(1,2,3),
                      sizeGr = c("small","large"),
                      `Decay Class` = c(1,2,3,4,5))

  cwd_all_combos <- merge(cwd_plot,
                          sp_decay_size,
                          by = c("Plot #", "sizeGr", "SpGrp", "Decay Class"),
                          all = TRUE)
  setnafill(cwd_all_combos, cols = c("VolumeHa", "mean_diam", "PLA"), fill = 0)

  cwd_gr_all <- cwd_all_combos[, .(PLA = mean(PLA)),
                               by = c("sizeGr", "SpGrp", "Decay Class")]

  #setnafill(cwd_gr_all, cols = c("VolumeHa", "mean_diam", "PLA"), fill = 0)

  cwd_gr_all[,propLA := PLA/100]

  #to account for recent spruce beetle kill, Bruce recommended modifying large diameter cwd
  #in decay class 1. I will divide it in half
  cwd_gr_all[sizeGr == "large" & SpGrp == 1 & `Decay Class` == 1,
             propLA := propLA/2]

  return(cwd_gr_all)
}




