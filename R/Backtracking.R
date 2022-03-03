#' La fonction Backtracking
#'
#'
#'@param A : une matrice
#'@param DF : Data frame des indices des cases vide est le nombre de possibilite pour remplir chaque case
#'@param n : La tail du DF
#'@return : Data Frame
#'@author AIT MOULAY ABDERRAHIM
#'@examples
#' Backtracking (A, DF , 33)
#'@export
#'


Backtracking<- function(A,DF,n) {


# Test darret de la fonction
if( n==0) { return(TRUE)}
nbr <<- nbr+1
# Stocker les indices i et j de la premmier case de DF
i <- DF[length(DF[,1])-(n-1),1]
j <- DF[length(DF[,1])-(n-1),2]
r = GitBlock(i,j)

# Bacltracking

  for (k in 1:9){

    # Verifier dans les matrices si la valeur k est deja present
    if( isTRUE( !Tligne[i,k] && !Tcolonne[j,k] && !Tblock[r,k]) ) {

      Tligne[i,k] <<- TRUE
      Tcolonne[j,k]<<- TRUE
      Tblock[r,k]<<- TRUE

      if(Backtracking(A,DF,n-1)==TRUE){
          A[i,j] <<- k
          nbr <<- nbr - 1
          return(TRUE)
          }
      Tligne[i,k] <<- FALSE
      Tcolonne[j,k]<<- FALSE
      Tblock[r,k]<<- FALSE
    }
  }
 return(FALSE)
}









