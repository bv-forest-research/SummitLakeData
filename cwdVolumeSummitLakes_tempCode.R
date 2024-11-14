#' Summit Lakes cwd data - 2020-2021
#'
#' @param dat_loc what is the directory where all data is stored
#' @param incl_sp_decay
#'
#' @details Summit Lake currently has only one year of CWD, so only call 2021, but can add more
#' as re-measurements occur
#'
#' @return
#' @export
#'
#' @examples
CWD_vol_calc <- function(dat_loc, incl_sp_decay = FALSE){

  sl_cwd_21 <- suppressWarnings(CWD_2021_vol_calc(CWD_dat = paste0(dat_loc,
                                                                   "EP1162CWDsurvey2020-2021.csv"),
                                                  line_dat = paste0(dat_loc,
                                                                    "EP1162CWDsurveyTransectLines2020-2021.csv"),
                                                  sp_decay = incl_sp_decay))

  cd_cwd_allyears <- rbind(sl_cwd_21)

  return(cd_cwd_allyears)

}


#


library(data.table)

# load data
cwdDat <- fread("./data-raw/EP1162CWDsurvey2020-2021.csv")
line <- fread("./data-raw/EP1162CWDsurveyTransectLines2020-2021.csv")

# CLEAN/PREPARE DATA
# rename columns
setnames(cwdDat, "diam(cm)", "Diam_cm")
setnames(cwdDat, "Accum/Odd-length(cm)", "OddDiam1")
setnames(cwdDat, "Accum/Odd-length(cm)", "OddDiam2")
# clean the species columns
cwdDat[, Species := ifelse(Species=="Unk","U", Species)]
# change all "-" to NA
cwdDat[cwdDat=="-"] <- NA
# make diam numeric
cwdDat[, Diam_cm:=as.numeric(Diam_cm)][, Tilt:=as.numeric(Tilt)]
# if tilt is NA make it 0
cwdDat[is.na(Tilt), Tilt:=0]

# for the oddly shaped pieces average the two diameters
# if diam=NA and species!=NA then average Odd diameters
cwdDat[, Diam_cm :=ifelse(is.na(Diam_cm) & !is.na(Species),
                          (OddDiam1 + OddDiam2) / 2,
                          Diam_cm)]

# Coarse woody debris volume/ha (line intersect method) by decay class and species
CWD_Vol_calc <- function(cwdDat, lineDat){
  PlotLine <- line[,.(HorizontalDist=sum(HorizontalDist)), by="Plot"]
  # Calculate volume using VanWagner volume equation
  #Convert deg to radians -- if you don't you will get a negative value
  cwdDat[, Tilt.radians:= pi/180*Tilt]

  PlotCWDvol <- cwdDat[, .(D2=sum(Diam_cm^2/cos(Tilt.radians))), by=c("Plot", "DecayClass", "Species")]
  PlotCWDvol <- merge(PlotCWDvol, PlotLine)
  PlotCWDvol[, VolHa:= pi^2/(8*HorizontalDist)*D2]
  PlotCWDvol[,c("D2", "HorizontalDist"):=NULL]
  # remove rows with vol=NA - these are transects with no cwd
  PlotCWDvol <- na.omit(PlotCWDvol)
  return(PlotCWDvol)
}

CWD_Vol_calc(cwdDat,line)
PlotCWDvol

