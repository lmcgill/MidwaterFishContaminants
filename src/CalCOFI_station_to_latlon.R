# Convert line-station - from here: https://calcofi.org/downloads/publications/calcofireports/v54/Vol_54_CalCOFIReport.pdf

`.deg2rad` <- function(deg) deg * pi / 180 
`.rad2deg` <- function(rad) rad * 180 / pi 
`.inverse.mercator` <- function(mercatorlat,iterations = 3)
  { 
  approxlat <- mercatorlat 
  iterlatitude <- function(mercatorlat,approxlat) 
    {  
    approxlat <- 2 * (atan(exp(.deg2rad(
      mercatorlat) + 0.00676866 * 
        sin(.deg2rad(approxlat)))) *   
        180 / pi - 45)  
    approxlat 
    } 
  for (i in 1:iterations) 
    approxlat <  iterlatitude(mercatorlat, approxlat) 
  approxlat 
  } 

`.to.mercator` <- function(latitude) 
  { 
  y <- .rad2deg(log(tan(.deg2rad(45 +     
                                   latitude / 2))) - 0.00676866 *     
                  sin(.deg2rad(latitude))) 
  y 
  } 

`station.to.latlon` <- function(x,        
                                roundlines = true) 
{
  if (length(x) == 2 & class(x) != 'matrix')
  {  
    x <- matrix(x, 1, 2) 
  } 
  line <- x[, 1] 
  station <- x[, 2] 
  reflatitude <- 34.15 - 0.2 * (line - 80) *  cos(.deg2rad(30)) 
  latitude <- reflatitude - (station - 60) *  sin(.deg2rad(30)) / 15 
  l1 <- (.to.mercator(latitude) - .to.mercator(  34.15)) * tan(.deg2rad(30)) 
  l2 <- (.to.mercator(reflatitude) - .to.mercator(latitude)) /   
    (cos(.deg2rad(30)) * sin(.deg2rad(30))) 
  longitude <- -1 * (l1 + l2 + 121.15) 
  cbind(lon = longitude, lat = latitude) 
  } 

`latlon.to.station` <- function(x) { 
  if (length(x) == 2 & class(x) != 'matrix'){  
    x <- matrix(x, 1, 2) 
  }
    
  longitude <- x[, 1] 
  latitude <- x[, 2] 
  # assume we're in the western hemispere 
  
  longitude[longitude > 180] <- -1 * (longitude[longitude > 180] - 360) 
  longitude[longitude < 0] <- longitude[  longitude < 0] * -1 
  
  l1 <- (.to.mercator(latitude) - .to.mercator(  34.15)) * tan(.deg2rad(30)) 
  l2 <- longitude - l1 - 121.15 
  mercreflatitude <- l2 * cos(.deg2rad(30)) *  sin(.deg2rad(30)) + .to.mercator(latitude) 
  reflatitude <- .inverse.mercator(  mercreflatitude) 
  line <- 80 - (reflatitude - 34.15) * 5 /  cos(.deg2rad(30)) 
  station <- 60 + (reflatitude - latitude) *  15 / sin(.deg2rad(30)) 
  cbind(line = line, station = station) 
  }
    
    

    