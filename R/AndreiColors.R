# 1.Create the color palettes
#::::::::::::::::::::::::::::::::::::::::::::::
#' Complete list of palettes
#'
#' Use \code{names(at_palettes)} to view list of palette names.
#' Currently:  stalker, nostalghia, solaris, mirror, sacrifice, violin
#'
#' @export
#'
at_palettes <- list(
  stalker = rbind(c("#4D5448", "#838D78", "#67717E", "#BAD5A0", "#9C9DA2"), c(1:5)),
  nostalghia = rbind(c("#191F15", "#DDDCC5", "#768F8E", "#717F76", "#8FACAE"), c(1:5)),
  solaris = rbind(c("#91838B", "#C7BEEE", "#A3B0E1", "#949CCA", "#414241"), c(1:5)),
  mirror = rbind(c("#7E84A1", "#B994A7", "#6D696A", "#848D7B", "#364B39"), c(1:5)),
  sacrifice = rbind(c("#585D5E", "#857E72", "#EAB589", "#D2C3B3", "#EEE2C9"), c(1:5)),
  violin = rbind(c("#2E2725", "#4F738B", "#A3C7D8", "#DDD5D4", "#0D354E"), c(1:5))

)

# The following functions were created based on the PNWcolors package (https://github.com/jakelawlor/PNWColors/blob/master/README.md)
# and the musculusColor package (https://github.com/dawnbarlow/musculusColors/blob/master/README.md)

# 2. Palette builder function
#::::::::::::::::::::::::::::::::::::::::::::::
#' Palette Generator.
#'
#'
#' @param name Name of the color palette. Options are \code{stalker}, \code{nostalghia}, \code{solaris},
#' \code{mirror}, \code{sacrifice}, \code{violin}
#' @param n Number of colors in the palette. Palletes include 5 colors, which can be used discretely,
#' or if more are desired, used as a gradient. If omitted, n = length of palette.
#' @param type Usage of palette as "continuous" or "discrete". Continuous usage interpolates between colors to create
#' a scale of values. If omitted, function assumes continuous if n > length of palette, and discrete if n < length of palette.
#'
#' @return A vector of colors.
#'
#' @examples
#' at_palette("stalker",n=100,type="continuous")
#' at_palette("nostalghia",5)
#' at_palette("solaris",50)
#'
#' @export

at_palette <- function(name, n, type = c("discrete", "continuous")) {

  pal <- at_palettes[[name]]


  if (is.null(pal)){
    stop("Palette not found.")
  }

  if (missing(n)) {
    n <- length(pal[1,])
  }

  if (missing(type)) {
    if(n > length(pal[1,])){type <- "continuous"}
    else{ type <- "discrete"}
  }
  type <- match.arg(type)


  if (type == "discrete" && n > length(pal[1,])) {
    stop("Number of requested colors greater than what discrete palette can offer, \n  use as continuous instead.")
  }


  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal[1,])(n),
                discrete = pal[1,][pal[2,] %in% c(1:n)],
  )
  structure(out, class = "atpalette", name = name)

}

#' @export



# 3. Palette Print Function
#::::::::::::::::::::::::::::::::::::::::
#' @importFrom graphics rect par image text
#' @importFrom stats median
print.atpalette <- function(x, ...) {
  pallength <- length(x)
  Bmpar <- par(mar=c(0.25,0.25,0.25,0.25))
  on.exit(par(Bmpar))

  image(1:pallength, 1,
        as.matrix(1:pallength),
        col = x,
        axes=FALSE)

  text(median(1:pallength), 1,
       labels = paste0(attr(x,"name"),", n=",pallength),
       cex = 3, family = "sans")
}


