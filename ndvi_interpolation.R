########################################################################
## Title: Build teporal resolution for NASA ndvi satelite image
## Date: 2014-08-23
########################################################################

## NOTE (Michael): Next step is to account for the spatial
##                 correlation.

library(raster)

files = dir(path = "NDVI/", pattern = "tif$", full.names = TRUE)
## files = dir(pattern = "tif$", full.names = TRUE)

## Function to extract the date of the file
extractFileDates = function(name){
    ind = regexpr("\\.A", name)[1] + 6
    dates = substr(name, ind, ind + 2)
    as.numeric(dates)
}

## Read the image as raster
readNdviImage = function(file){
    v = raster(file)
    v
}


## Function to interpolate using spline
imageSpline = function(x, y, xout, method = "natural", ...){
    x.max = max(xout)
    if(method == "periodic"){
        x = c(x, x.max)
        y = c(y, y[1])
    }
    x.spline = spline(x = x, y = y, xout = xout, method = method, ...)
    x.spline
}

## Use spline to create the temporal interpolation
image.arr = array(dim = c(1200, 1200, length(files)))
dates = length(files)
for(i in seq_along(files)){
    image.arr[, , i] = matrix(readNdviImage(files[i])[],
                 nrow = 1200, byrow = TRUE)
    dates[i] = extractFileDates(files[i])
}


## Plot the pixel 1 over time with the spline
jpeg(file = "pixel1_timeseries.jpeg", width = 720)
plot(dates, image.arr[1, 1, ], pch = 19,
     main = "Observed images values and interpolation for pixel 1",
     xlab = "Dates", ylab = "NDVI")
lines(imageSpline(x = dates, image.arr[1, 1, ], xout = 1:365))
legend("topleft",
       legend = c("Observed image value",
           "Natural Spline Interpolated image value"),
       lty = c(NA, 1), pch = c(19, NA), bty = "n")
graphics.off()


## Plot the pixel 1 over time with the spline
jpeg(file = "pixel1_timeseries2.jpeg", width = 720)
plot(dates, image.arr[1, 1, ], pch = 19,
     main = "Observed images values and interpolation for pixel 1",
     xlab = "Dates", ylab = "NDVI")
lines(imageSpline(x = dates, image.arr[1, 1, ], xout = 1:365))
lines(imageSpline(x = dates, image.arr[1, 1, ], xout = 1:365,
                  method = "periodic"), col = "red")
legend("topleft",
       legend = c("Observed image value",
           "Natural Spline Interpolated image value",
           "Periodic Spline Interpolated image value"),
       lty = c(NA, 1, 1), pch = c(19, NA, NA),
       col = c("black", "black", "red"), bty = "n")
graphics.off()


## Interpolate the images
interpolated.lst = vector(mode = "list", length = 1200)
system.time(
    {
        for(i in 1:1200){
            interpolated.lst[[i]] = 
                apply(image.arr[i, , ], 1,
                      FUN = function(x){
                          imageSpline(x = dates, y = x, xout = 1:365)$y
                      }
                      )
        }
    }
    )


## Function to get each image
getImage = function(x, num){
    c(sapply(x, FUN = function(x) x[num, ]))
}

## Function to plot the image
vectorToImage = function(vector, dim, ...){
    mat = matrix(vector, nrow = dim[1], ncol = dim[2], byrow = TRUE)
    image(t(mat)[, ncol(mat):1], ...)
}

jpeg("jan_1.jpeg", width = 720, height = 720)
vectorToImage(getImage(interpolated.lst, 1), dim = c(1200, 1200),
              main = "1 Jan (Observed)", useRaster = TRUE, axes = FALSE)
graphics.off()

## Plot selected image
jpeg("jan_interpolated.jpeg", width = 720, height = 720)
par(mfrow = c(2, 2), mar = c(1.1, 1.1, 3.1, 1.1))
vectorToImage(getImage(interpolated.lst, 1), dim = c(1200, 1200),
              main = "1 Jan (Observed)", useRaster = TRUE, axes = FALSE)
vectorToImage(getImage(interpolated.lst, 6), dim = c(1200, 1200),
              main = "6 Jan (Interpolated)",
              useRaster = TRUE, axes = FALSE)
vectorToImage(getImage(interpolated.lst, 11), dim = c(1200, 1200),
              main = "11 Jan (Interpolated)",
              useRaster = TRUE, axes = FALSE)
vectorToImage(getImage(interpolated.lst, 17), dim = c(1200, 1200),
              main = "17 Jan (Observed)", useRaster = TRUE, axes = FALSE)
graphics.off()

