comm_check = function(check) as.logical(spmd.allreduce.integer(as.integer(check), integer(1)))

comm_sameval = function(x) spmd.allreduce.integer(x, integer(1), op="band")
