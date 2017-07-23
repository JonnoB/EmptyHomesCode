Shape2Adjmat <- function(Shape, Nodename){
  #Creates an adjacency matrix from a shape file
  #Shape: the shape file
  #Nodename: the attribute of the shape file you want to be the node names. defaults to the first type.
  
  if(is.null(Nodename)){
    Nodename <- names(Shape)[1]
  }
  
  row.names(Shape) <- as.character(Shape[[Nodename]])
  nb <- poly2nb(Shape)
  Shapemat <- nb2mat(nb, style="B", zero.policy = TRUE) #zero policy is for islands
  colnames(Shapemat) <- rownames(Shapemat)
  
  return(Shapemat)
}