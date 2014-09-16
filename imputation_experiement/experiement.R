library(raster)
library(rasterVis)

sugarCane = raster("./sugar_cane_earthstat//sugarcane_area1.geotiff")
ndvi = raster("./ndvi/ndvi_sadc_-_2010-03-06.geotiff")
trmm = raster("./trmm/trmm_sadc_-_2010-03-06.geotiff")

par(mfrow = c(1, 3))
plot(sugarCane)
plot(ndvi)
plot(trmm)

levelplot(sugarCane, par.settings=RdBuTheme)
histogram(sugarCane)
vectorplot(sugarCane)
vectorplot(ndvi)
streamplot(sugarCane)

levelplot(sugarCane, layers=1, FUN.margin=median, contour=TRUE)
levelplot(ndvi, layers=2, FUN.margin=median, contour=TRUE)
levelplot(trmm, layers=1, FUN.margin=median, contour=TRUE)

sugarCaneData = matrix(sugarCane[], nc = sugarCane@ncols, byrow = TRUE)

(ndvi@ncolso - 3)/sugarCane@ncols
trmm@ncols

trmm1 = raster("./trmm/trmm_sadc_-_2010-03-06.geotiff")
trmm2 = raster("./trmm/trmm_sadc_-_2010-03-22.geotiff")
stack("./trmm/*.geotiff")


precipitationFiles =
    dir("./trmm/", pattern = "*sadc_\\-_", full.names = TRUE)

for(i in 1:length(precipitationFiles)){
    cat(precipitationFiles[i],
        " NC: ", raster(precipitationFiles[i])@ncols,
        " NR: ", raster(precipitationFiles[i])@nrows, "\n")
}

stackPrecip = stack(precipitationFiles[1:5])
timeID = seq(as.Date("2010-03-06"), as.Date("2010-05-09"), 16)
precip = setZ(stackPrecip, timeID)
names(precip) = paste0("D", 1:5)
levelplot(precip)
levelplot(precip, layers=1, FUN.margin=median, contour=TRUE)
hexbinplot(D1 ~ D2|cut(x, 6), data = precip)
splom(precip)
histogram(precip)
densityplot(precip)
bwplot(precip)
horizonplot(precip, origin = 0)
hovmoller(precip, contour=FALSE, panel = panel.levelplot.raster,
          yscale.components = yscale.raster.subticks,
          interpolate = TRUE, par.settings = RdBuTheme,
          dirXY = xyLayer(precip, x))


ndviFiles =
    dir("./ndvi/", pattern = "*sadc_\\-_", full.names = TRUE)

for(i in 1:length(ndviFiles)){
    print(raster(ndviFiles[i])@ncols)
}

stackNdvi = stack(ndviFiles[1:6])
timeID = seq(as.Date("2010-03-06"), as.Date("2010-05-25"), 16)
ndvi = setZ(stackNdvi, timeID)
names(ndvi) = paste0("D", 1:6)
levelplot(ndvi)
levelplot(ndvi, layers=1, FUN.margin=median, contour=TRUE)
hexbinplot(D1 ~ D2|cut(x, 6), data = ndvi)
splom(ndvi)
histogram(ndvi)
densityplot(ndvi)
bwplot(ndvi)
horizonplot(ndvi, origin = 0)
hovmoller(ndvi, contour=FALSE, panel = panel.levelplot.raster,
          yscale.components = yscale.raster.subticks,
          interpolate = TRUE, par.settings = RdBuTheme,
          dirXY = xyLayer(ndvi, x))


## Pseudo-code for yield
##
## (1) Identify the sugar cane planting region in ndvi using
##     earthstat.
##
## (2) Match the ndvi with the crop calender
##
## (3) Then use the relevant set of ndvi to impute yield. Maybe
##     summary statistic can be used.
##
## (4) Check method of p >> n


## Pseudo-code for area harvested.
##
## (1) Match the dates for earth stat and ndvi data.
##
## (2) Then use the same model to predict earth stat with ndvi.
##
## (3) Regress area harvested with earth stat to impute area harvested.

## The two method are similar, except we take different piece of
## information. For yield, we take the intensity, while for area
## harvested we sum up the area.


test1 = raster("./trmm//trmm_sadc_-_2010-03-06.geotiff")
test2 = raster("./trmm//trmm_sadc_-_2010-05-25.geotiff")
test3 = raster("./ndvi//ndvi_sadc_-_2010-03-06.geotiff")

extent(test1)
extent(test2)
extend(test2, extent(test1))

test2Resample = resample(test2, test1, method = "bilinear")
par(mfrow = c(1, 2))
plot(test2)
plot(test2Resample)

test1Resample = resample(test1, test3, method = "bilinear")
par(mfrow = c(1, 2))
plot(test1)
plot(test1Resample)

test1Resample[is.na(test3[])] = NA
par(mfrow = c(1, 2))
plot(test1Resample)
plot(test3)

test4 = stack(test1Resample, test3)
levelplot(test4)
splom(test4)


sugarCaneArea = raster("./sugar_cane_earthstat//sugarcane_area1.geotiff")
ndvi = raster("./ndvi//ndvi_sadc_-_2010-03-06.geotiff")
sugarCaneResampled = resample(sugarCaneArea, ndvi, method = "bilinear")
sugarCaneResampled[is.na(ndvi[])] = NA
sugarCaneResampled[sugarCaneResampled <=
                       min(sugarCaneArea[sugarCaneArea >0],
                           na.rm = TRUE)] = 0

par(mfrow = c(1, 2))
plot(sugarCaneResampled)
plot(sugarCaneArea)



sugarCaneFinal = stack(sugarCaneResampled, ndvi)
par(mfrow = c(1, 2))
plot(sugarCaneResampled)
plot(ndvi)
## hexbinplot(sugarcane_area1 ~ ndvi_sadc_._2010.03.06, sugarCaneFinal)


sugarCaneNdvi = ndvi
sugarCaneNdvi[sugarCaneResampled[] <
                  quantile(na.omit(sugarCaneResampled[]), 0.9)] = 0
sugarCaneNdvi[is.na(sugarCaneResampled[])] = 0
sugarCaneNdvi[is.na(ndvi[])] = NA

## jpeg("sugarcan_experiement.jpeg", width = 1440, height = 1440,
##      quality = 100)
par(mfrow = c(2, 2))
plot(ndvi, axes = FALSE, box = FALSE, main = "NDVI")
plot(sugarCaneArea, axes = FALSE, box = FALSE,
     main = "Earstat Sugarcane Area")
plot(sugarCaneResampled, axes = FALSE, box = FALSE,
     main = "Sugarcane Interpolated Area")
plot(sugarCaneNdvi, axes = FALSE, box = FALSE,
     main = "NDVI Sugarcane Area")
## graphics.off()



ndviCultivated = function(earthstat, ndvi){
    cultivated = raster(earthstat)
    ndvi = raster(ndvi)
    cultivatedResampled = resample(cultivated, ndvi, method = "bilinear")
    cultivatedResampled[is.na(ndvi[])] = NA
    cultivatedResampled[sugarCaneResampled <=
                           min(cultivated[sugarCaneArea > 0],
                               na.rm = TRUE)] = 0   
    ndviCultivated = ndvi
    ndviCultivated[cultivatedResampled[] <
                      quantile(na.omit(cultivatedResampled[]), 0.90)] = 0
    ndviCultivated[is.na(cultivatedResampled[])] = 0
    ndviCultivated[is.na(ndvi[])] = NA

    areaCultivated = cultivatedResampled[]
    areaCultivated[areaCultivated <
                       quantile(na.omit(areaCultivated), 0.9)] = NA
    totalArea = sum(!is.na(areaCultivated[])) *
        res(cultivatedResampled)[1]^2/1000^2
    
    list(cultivated = cultivated, ndvi = ndvi,
         cultivatedResampled = cultivatedResampled,
         ndviCultivated = ndviCultivated,
         totalArea = totalArea)
}

sugarCaneCultivated =
    ndviCultivated(earthstat =
                       "./sugar_cane_earthstat//sugarcane_area1.geotiff",
                   ndvi = "./ndvi//ndvi_sadc_-_2010-03-06.geotiff")
sugarCaneCultivated$totalArea

lapply(sugarCaneCultivated[1:4],
       FUN = function(x){
           plot(x, axes = FALSE, box = FALSE, main = names(x))
       }
       )


## Need to find a way to estimate the density or clustering.
