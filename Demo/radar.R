webplot = function(data, data.row = NULL, y.cols = NULL, main = "Sustainable Indicators", 
                   col = "red", lty = 1) {
  if (!is.matrix(data) & !is.data.frame(data)) 
    stop("Requires matrix or data.frame")
  if (is.null(y.cols)) 
    y.cols = colnames(data)[sapply(data, is.numeric)]
  if (is.null(data.row)) 
    data.row = 1
  if (is.null(main)) 
    main = rownames(data)[data.row]
  
  data = as.data.frame(data)
  n.y = length(y.cols)
  min.rad = 360/n.y
  polar.vals = (90 + seq(0, 360, length.out = n.y + 1)) * pi/180
  
  # plot
  rstr  = as.character(max(data))
  rstrd = substr(rstr, nchar(rstr), nchar(rstr))
  #   raux = ifelse(rstrd == "0" | rstrd == "2" | rstrd == "4" | rstrd == "6" | rstrd == "8", max(data)+0.2,   max(data)+0.1)
  raux = max(data) + 0.2
  cons  = 0.4
  #   par(mar=c(2,2,2,2))
  plot(0, xlim = c(-cons-0.2, cons+0.2), ylim = c(-cons-0.2, cons+0.2), type = "n", axes = F, xlab = "", ylab = "")
  
  
  #   
  aux = data.frame(0, 0, 0, 0, 0, 0, 0, 0)
  names(aux) = names(data)
  
  #   calculate segments for grid
  segment.step = 0.2
  n.segments   = round(((max(data)+0.2) / segment.step))
  seg.df       = aux
  
  for(i in 1:n.segments) {
    seg.df[i,] = i * segment.step + aux
  }  
  
  #plot segment lines
  for(i in 1:n.segments) {
    r  = seg.df[i, y.cols]
    xs = cons * r * cos(polar.vals)
    ys = cons * r * sin(polar.vals)
    xs = c(xs, xs[1])
    ys = c(ys, ys[1])
    lines(xs, ys, col = "grey70", lwd = 1, lty = 2)
  }
  
  lapply(polar.vals, function(x) lines(c(0, cons * r * cos(x)), c(0, cons * r  * sin(x)),col="grey70"))
  
  lapply(1:n.y, function(x) text((r+0.12) * cons * cos(polar.vals[x]), (r+0.12) * cons * sin(polar.vals[x]), y.cols[x], cex = 1.3))
  #plot data
  r =  data[data.row, y.cols]
  xs = cons * r * cos(polar.vals)
  ys = cons * r * sin(polar.vals)
  xs = c(xs, xs[1])
  ys = c(ys, ys[1])
  lines(xs, ys, col = col, lwd = 1.5, lty = 1)
  
  avg = data.frame(0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4)
  names(avg) = names(data)
  r =  avg[data.row, y.cols]
  xs = cons * r * cos(polar.vals)
  ys = cons * r * sin(polar.vals)
  xs = c(xs, xs[1])
  ys = c(ys, ys[1])
  lines(xs, ys, col = "red", lwd = 2, lty = 2)
  
}