
#' @param CWD_dat
#' @param line_dat
#' @param sp_decay
#'
#' @return
#' @export
#'
#' @examples
CWD_2021_vol_calc <- function(CWD_dat, line_dat, sp_decay){
  # load data
  cwd <- fread(CWD_dat)
  line <- fread(line_dat)

  # CLEAN/PREPARE DATA
  # rename columns
  setnames(cwd, "diam(cm)", "Diam_cm")
  setnames(cwd, "Accum/Odd-length(cm)", "OddDiam1")
  setnames(cwd, "Accum/Odd-length(cm)", "OddDiam2")
  # clean the species columns
  cwd[, Species := ifelse(Species=="Unk","U", Species)]
  # change all "-" to NA
  cwd[cwd=="-"] <- NA
  # make diam numeric
  cwd[, Diam_cm:=as.numeric(Diam_cm)][, Tilt:=as.numeric(Tilt)]
  # if tilt is NA make it 0
  cwd[is.na(Tilt), Tilt:=0]

  # for the oddly shaped pieces average the two diameters
  # if diam=NA and species!=NA then average Odd diameters
  cwd[, Diam_cm :=ifelse(is.na(Diam_cm) & !is.na(Species),
                         (OddDiam1 + OddDiam2) / 2,
                         Diam_cm)]

  PlotCWDvol <- CWD_vol_int(CWD_dat = cwd,
                            line_dat = line,
                            sp_decay = sp_decay)

  PlotCWDvol[,Year:=2021]

  return(PlotCWDvol)

}
