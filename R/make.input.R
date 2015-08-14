make.input <-
function (data, groupcol = 2, altercols = NULL) 
{
  start.data <- data.frame(group = data[, groupcol], data[, altercols],stringsAsFactors = F)
  sizes <- sapply(start.data[,1], function(x) nrow(start.data[which(start.data[,1]==x),]))
  max.size <- max(sizes)
  if (min(sizes) == 1) {
    print('Teams of size 1 present; will be appended to end of data')
    start.data <- start.data[which(sizes > 1),]
  }
  temp.data <- split(start.data, start.data[, 1])
  temp.data <- lapply(temp.data, function(x) lapply(1:(nrow(x) - 1), function(cnt) rbind(do.call(rbind, replicate(cnt, x[(cnt + 1), -1], simplify = F)), do.call(rbind, replicate((nrow(x) - cnt), x[cnt, -1], simplify = F)))))
  temp.data <- lapply(temp.data, function(x) do.call(cbind, x))
  temp.data <- lapply(temp.data, function(x) 
  {
    x <- as.data.frame(x);
    if (nrow(x) < max.size) {
      x[, ((nrow(x) - 1) * length(altercols) + 1):((max.size - 1) * length(altercols))] <- NA;
      x;
    }
    else {
      x
    }
  })
  temp.data <- lapply(temp.data, function(x) {
    names(x) <- sapply(1:(max.size - 1), function(cnt) paste("a", cnt, names(data[, altercols]), sep = ""));
    x;
  })
  new.data <- cbind(data[which(sizes > 1),], do.call(rbind, temp.data), deparse.level = 0)
  singles <- data[which(sizes == 1),]
  if (nrow(singles) > 0)
  {
    singles[,(ncol(singles)+1):ncol(new.data)] <- NA
    names(singles) <- names(new.data)
    rbind(new.data,singles)
  }
  return(new.data)
}