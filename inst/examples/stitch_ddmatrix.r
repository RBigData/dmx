# Run with a few ranks, e.g.
#   mpirun -np 4 Rscript stitch_ddmatrix.r
suppressMessages(library(pbdDMAT))
suppressMessages(library(dmx))
init.grid(quiet=TRUE)

m = 4
n = 3
rank = comm.rank()
x = matrix(1:(m*n) * 10^rank, m, n)
if (rank+1 == comm.size())
  x = x[, 1]

dx = stitch_ddmatrix(x)
dx
comm.print(dx@Data, all.rank=TRUE)

dy = reblock(dx, bldim=c(2,2), ICTXT=0)
dy
comm.print(dy@Data, all.rank=TRUE)

finalize()
