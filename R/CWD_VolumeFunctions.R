
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
  sl_cwd_21 <- CWD_2021_Vol_calc(CWD_dat = paste0(dat_loc,"EP1162CWDsurvey2020-2021.csv"),
                                 out_carbon_comp = incl_sp_decay)


  if(incl_sp_decay == FALSE){
    cd_cwd_allyears <- rbind(dc_cwd_92[,.(Year,Yrs_Post = 0,Unit, VolumeHa)],
                             dc_cwd_93[,.(Year,Yrs_Post = 1,Unit = Stand, VolumeHa)],
                             dc_cwd_11[,.(Year = 2011, Yrs_Post = 19,Unit, VolumeHa)],
                             dc_cwd_19[,.(Year = 2019, Yrs_Post = 27,Unit, VolumeHa)])

  }else{
    cd_cwd_allyears <- rbind(dc_cwd_92[,.(Year,Yrs_Post = 0,Unit, Sp, Decay, VolumeHa)],
                             dc_cwd_93[,.(Year,Yrs_Post = 1,Unit = Stand, Sp, Decay, VolumeHa)],
                             dc_cwd_11[,.(Year = 2011, Yrs_Post = 19,Unit, Sp, Decay, VolumeHa)],
                             dc_cwd_19[,.(Year = 2019, Yrs_Post = 27,Unit, Sp, Decay = Decay_2019, VolumeHa)])
  }

  #clean the species columns
  cd_cwd_allyears[, Sp := ifelse(Sp=="u","U",
                                 ifelse(Sp == "", "U",
                                        ifelse(Sp == "ep", "Ep",
                                               ifelse(Sp == "Act","Ac",Sp))))]


  return(cd_cwd_allyears)

}


#' 1992 CWD volume calculation
#'
#' @param CWD_dat
#'
#' @return
#' @export
#' @details Calculate Volume using the British Columbia Ministry of Forests and Range (2009) formula
#'  CWD volume (m3/ha) = pi^2/8L  *  sum[D2/cos (A)]
#'   Where: 	L = length of total transect (horizontal distance (HD) in m)
#'                                         HD = SD / Square root of [1 + (% slope / 100)2]
#' slope was not measured so assume total transect length to be 90m
#' D = diameter of each piece of CWD (cm)
#' A = tilt angle from horizontal for each piece (degrees)
#' Because tilt angle was not measured in 1992 and 1993, it was assumed to be zero,
#' so that cos (A) = 1 for all pieces in those years.
#'
#' @examples
CWD_1992_Vol_calc <- function(CWD_dat, out_carbon_comp = FALSE){
  #-----------------------Prepare data -----------------------------------------#
  # Import 1992
  CWD.1992<- fread(CWD_dat)

  # Square diameter
  CWD.1992[, D2_cosA:= Diam_cm^2]

  # Convert individual piece to plot summary for L (length of total transect horizontal distance)
  if(out_carbon_comp == FALSE){
    CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                              by =c("Year", "Unit", "Block", "Treatment", "Unique_plot")]
  }else{
    #if volume will be used for carbon, need to keep species and decay class columns
    CWD.1992_plot <- CWD.1992[, .(D2cosA = sum(D2_cosA)),
                              by =c("Year", "Unit","Sp", "Decay","Block", "Treatment", "Unique_plot")]
  }

  # Volume (m3/ha) calculation
  CWD.1992_plot[, VolumeHa:= (pi^2/(8*90)) * D2cosA]

  return(CWD.1992_plot)

}




