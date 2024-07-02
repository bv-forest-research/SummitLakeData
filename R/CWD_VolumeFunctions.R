
#' Summit Lakes cwd data - 2020-2021
#'
#' @param dat_loc what is the directory where all data is stored
#'
#' @details we expect the directory to contain 6 files: CWD_1992.csv, CWD_1993.csv, CWD_2011.csv, CWD_2018.csv and CWD_2019.csv
#' @return
#' @export
#'
#' @examples
CWD_vol_calc <- function(dat_loc, incl_sp_decay = FALSE){

  #calculate volume for all years
  #2020 & 2021 considered the same year
  if(incl_sp_decay == FALSE){

    sl_cwd_21 <- CWD_2021_Vol_calc(out_carbon_comp = FALSE)

  }else{

    sl_cwd_21 <- CWD_2021_Vol_calc(out_carbon_comp = TRUE)

  }

  #clean the species columns
  sl_cwd_21[, Sp := ifelse(Species =="Unk","U",Species)]


  return(sl_cwd_21)

}


#' 2020/2021 CWD volume calculation
#'
#' @param CWD_dat
#'
#' @return
#' @export
#' @details Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#'  CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]
#'   Where: 	L = length of total transect (horizontal distance (HD) in m)
#'                                         HD = SD / Square root of [1 + (% slope / 100)2]
#' slope was measured - so need to update this equation
#' D = diameter of each piece of CWD (cm)
#' A = tilt angle from horizontal for each piece (degrees)
#' Because tilt angle was not measured in 1992 and 1993, it was assumed to be zero,
#' so that cos (A) = 1 for all pieces in those years.
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




