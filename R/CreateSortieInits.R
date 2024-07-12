
#' create sortie inits from Summit Lake data
#'
#' @param dbh_size_class
#' @param plot_area
#' @param raw_data
#' @param in_dir
#' @param out_dir
#' @author Alana Clason & Leah Walker
#' @return
#' @export
#'
#' @examples
sl_sortie_inits <- function(dbh_size_class = 2,
                              plot_area = 0.05,
                              raw_data = "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv",
                              in_dir,
                              out_dir) {


  #1. tree initial values
  #sbs_trees <- clean_trees(raw_data = "D:/Github/SummitLakeData/data-raw/SummitLakeData.csv")
  summit_sph <- plot_sph_size(dbh_size_class = 2,
                              plot_area = 0.05,
                              raw_data = raw_data)

  #get live trees from 1992
  summit_sph_u <- summit_sph[Year == 1992 & State == "Live"]
  summit_sph_u[, vari := paste0("Init.Dens_", DBH_bin,".0")]


  #prior to 2009, limit was 4cm DBH, so go down to 4cm from summit lake data

  #2. add small trees from Bruce Rogers Sx trial:
  small_trees <- fread(file.path(in_dir, "SBS_nat_regen.csv"))
  # their height and diameter classes don't match SORTIE.
  # diameter cleasses include the lower bound, but not the upper
  small_trees[ , sortie_bins := ifelse(Height_m <= 1.3 & is.na(DBH_cm), "Init.Dens_1",
                                       ifelse(Height_m >= 1.3 & DBH_cm < 2,"Init.Dens_2.0",
                                              ifelse(Height_m >= 1.3 & DBH_cm >= 2 & DBH_cm < 4,
                                                     "Init.Dens_4.0",
                                                     ifelse(Height_m >= 1.3 & DBH_cm >= 4 & DBH_cm < 6,
                                                            "Init.Dens_6.0",
                                                            ifelse(Height_m >= 1.3 & DBH_cm >= 6 & DBH_cm < 7.5,
                                                                   "Init.Dens_8.0",
                                                                   NA)))))]
  small_trees[, Species := ifelse(Species == "SX", "Sx",
                                  Species)]
  #cleaning up some odd ones
  small_trees[DBH_cm == 6.1 & Height_m == 0.81, sortie_bins := "Init.Dens_8.0"]
  small_trees[is.na(DBH_cm) & is.na(sortie_bins)]
  small_trees[DBH_cm == 0.5 & Height_m == 1, sortie_bins := "Init.Dens_1"]
  small_trees[DBH_cm == 8.0 & Height_m == 0.49, sortie_bins := "Init.Dens_10.0"]
  small_trees[DBH_cm == 2.5 & Height_m == 1.17, sortie_bins := "Init.Dens_4.0"]
  small_trees[DBH_cm == 7.4 & is.na(Height_m), sortie_bins := "Init.Dens_8.0"]
  small_trees <- small_trees[!is.na(sortie_bins)]

  sm_trees_u4 <- small_trees[sortie_bins != "Init.Dens_8.0"][sortie_bins != "Init.Dens_6.0"]
  #assuming 5.64m plots
  small_trees_sph <- sm_trees_u4[,.N/0.01, by=.(Plot, Species, sortie_bins)]
  sph_nat <- dcast(small_trees_sph, Plot+sortie_bins ~ Species,
                   value.var = "V1")
  sph_nat[is.na(sph_nat)] <- 0

  sph_nat <- sph_nat[, .(Bl = mean(Bl), Sx = mean(Sx)), by = sortie_bins]
  #sph_nat[is.na()]

  # 3. add planted seedlings
  plantTrees <- fread(file.path(in_dir, "SBS_plantedTrees.csv"))
  #remove trees that are outside the plot
  plantTrees <- plantTrees[!grep("Outside|outside", COMMENTS_2021),]
  # counted in the PSP, so 0.05 ha in size
  sph_plant <- plantTrees[, .(SPH = (.N/0.05)), by = .(Plot)] #all spruce
  setnames(sph_plant, "SPH", "Sx")

  #4. write out the initial plots
  sl_out_dir <- file.path(out_dir, "02_summit_lake","ParameterValues")

  for(ii in 1:length(unique(summit_sph_u$unit))){
    dat.unit <- summit_sph_u[unit == unique(summit_sph_u$unit)[ii]]
    dat.unit$unit <- NULL
    dat.unit$DBH_bin <- NULL
    dat.unit <- dcast(dat.unit, vari ~ Species, value.var = "SPH")
    dat.unit <- as.data.table(dat.unit)
    dat.unit[, c("Pl", "At", "Ac") := 0]

    #random draw from naturals < 4cm
    sph_nat_ii <- sph_nat[,  .(Bl = abs(rnorm(1, mean = Bl, sd = 50)),
                               Sx = abs(rnorm(1, mean = Sx, sd = 50))),
                          by = sortie_bins]
    sph_nat_ii <- sph_nat_ii[sortie_bins == "Init.Dens_1" |
                               sortie_bins == "Init.Dens_2.0" |
                               sortie_bins == "Init.Dens_4.0"]
    setnames(sph_nat_ii, "sortie_bins", "vari")

    #add planted
    sph_plant_ii <- sph_plant[Plot == unique(summit_sph_u$unit)[ii]]
    sph_plant_ii[, Plot := NULL]
    sph_plant_ii[, sortie_bins := "Init.Dens.Seedling.Hgt.Class.1"]
    setnames(sph_plant_ii, "sortie_bins", "vari")

    #join them together
    dat.unit <- rbind(dat.unit, sph_nat_ii, fill = TRUE)
    dat.unit <- rbind(dat.unit, sph_plant_ii, fill = TRUE)

    #fill in any missing inits with 0
    full_list <- data.table(vari = paste0("Init.Dens_", seq(6,90, by = 2),".0"))
    dat.unit <- merge(dat.unit, full_list, by = "vari", all = TRUE)
    dat.unit[is.na(dat.unit)] <- 0

    #dat.unit <- rbindlist(list(dat.unit, TS92), use.names = TRUE, fill = TRUE)
    DT <- data.table(1)[, `:=`(c("vari", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"), NA)][,V1 := NULL]
    dat.unit <- rbindlist(list(DT, dat.unit[1:nrow(dat.unit)]), use.names = TRUE, fill = TRUE)
    setcolorder(dat.unit, c("vari", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"))
    setnames(dat.unit, c("vari", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"),
             c(" ", "Interior_Spruce", "Lodgepole_Pine", "Subalpine_Fir", "Trembling_Aspen",
               "Western_Larch", "Douglas_Fir", "Black_Cottonwood", "Paper_Birch"))

    write.csv(dat.unit, paste0(sl_out_dir,"/","summit_",
                               unique(summit_sph_u$unit)[ii],".csv"), row.names = FALSE)
  }



}
