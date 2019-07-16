ERROR_NOTBLOCK_MSG = "local data does not appear to be amenable to a 1-d block distribution"

check_common_dim = function(d, type)
{
  d_verify = comm_sameval(d)
  if (!identical(d, d_verify))
    pbdMPI::comm.stop(
      paste("not all processes have the same number of", type, "in the local data matrix")
  )
  
  invisible(TRUE)
}

check_noncommon_dim = function(d, rank)
{
  rank_last = pbdMPI::comm.size() - 1L
  if (rank == 0)
  {
    pbdMPI::spmd.send.integer(d, rank.dest=rank_last)
    d_verify = d
  }
  else if (rank == rank_last)
    d_verify = pbdMPI::spmd.recv.integer(integer(1), rank.source=0)
  else
    d_verify = d
  
  d_verify = comm_sameval(d_verify)
  if (!identical(d, d_verify))
    pbdMPI::comm.stop(ERROR_NOTBLOCK_MSG)
}



#' stitch_ddmatrix
#' 
#' Stitch together a distributed matrix from the local pieces. Only works for
#' 1-d block distributions.
#' 
#' @details
#' Assumptions:
#' \itemize{
#'   \item every process owns some of the data
#'   \item the data is laid out in 1-d block format
#'   \itemize{
#'     \item if \code{by == "column"} then all processes should have the same
#'     number of rows, and all but perhaps the last should have the same
#'     number of columns.
#'     \item if \code{by == "row"} then all processes should have the same
#'     number of columns, and all but perhaps the last should have the same
#'     number of rows.
#'   }
#' }
#' 
#' @param x
#' Local data matrix.
#' @param by
#' One of \code{"column"} or \code{"row"}. Are you stitching the data together
#' by columns or rows?
#' @param verify
#' Should some checks be evaluated to (try to) make sure that the distributed
#' matrix makes sense?
#' 
#' @return
#' A ddmatrix.
#' 
#' @export
stitch_ddmatrix = function(x, by="column", verify=TRUE)
{
  by = pbdMPI::comm.match.arg(tolower(by), c("column", "row"))
  
  if (!is.matrix(x))
    x = as.matrix(x)
  
  ldim = dim(x)
  
  rank = pbdMPI::comm.rank()
  bldim = if (rank == 0) ldim else integer(2)
  bldim = pbdMPI::spmd.allreduce.integer(bldim, integer(2))
  
  if (by == "column")
  {
    ICTXT = 1L
    nrows = ldim[1]
    ncols = pbdMPI::spmd.allreduce.integer(ldim[2], integer(1))
    if (isTRUE(verify))
    {
      check_common_dim(nrows, "rows")
      check_noncommon_dim(ncols, rank)
    }
  }
  else if (by == "row")
  {
    ICTXT = 2L
    nrows = pbdMPI::spmd.allreduce.integer(ldim[1], integer(1))
    ncols = ldim[2]
    if (isTRUE(verify))
    {
      check_noncommon_dim(nrows, rank)
      check_common_dim(ncols, "columns")
    }
  }
  
  dim = c(nrows, ncols)
  
  if (isTRUE(verify))
  {
    ldim_verify = pbdBASE::base.numroc(dim, bldim, ICTXT)
    check = identical(ldim, ldim_verify)
    check = comm_check(check)
    if (check)
      pbdMPI::comm.stop(ERROR_NOTBLOCK_MSG)
  }
  
  new("ddmatrix", Data=x, dim=dim, ldim=ldim, bldim=bldim, ICTXT=ICTXT, comm=0L)
}
