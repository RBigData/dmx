comm_check = function(check) as.logical(pbdMPI::spmd.allreduce.integer(as.integer(check), integer(1)))

comm_sameval = function(x) pbdMPI::spmd.allreduce.integer(x, integer(1), op="band")
