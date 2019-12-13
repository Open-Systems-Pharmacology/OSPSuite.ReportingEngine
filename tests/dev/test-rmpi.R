#getall quantities
#add them up using rmpi

library(Rmpi)

ObjThng <- R6::R6Class(
  "ObjThng",
  public = list(
    par1 = NULL,

    initialize = function(){
      print("new one")
    },

    func1 = function(){
      print("here's function 1")
    }

  )
)

frnd <- ObjThng$new()

#frnd <- ObjThng$new()


if(!(mpi.comm.size() == 0)){
  mpi.close.Rslaves()
}

mpi.spawn.Rslaves(nslaves=2)
# prt <- function(a){
#   return(a)
# }

print("start broadcast")
mpi.bcast.cmd(x<-mpi.bcast.Robj())
x<-c("This is it")
mpi.bcast.Robj(x)
res <-  mpi.remote.exec(x,simplify = TRUE)
print("end broadcast")


#trr <- mpi.remote.exec(cmd = prt , 4,simplify = TRUE , ret= TRUE)
#print(trr)
#mpi.remote.exec(rnorm,4)
#mpi.remote.exec( mpi.comm.rank())
#print(mpi.comm.size())
#print(mpi.comm.rank())
mpi.close.Rslaves()




# library(Rmpi)
# mpi.spawn.Rslaves(nslaves=3)
# mpi.bcast.cmd(x<-mpi.bcast.Robj())
# x<-c("This is a test.")
# #2. Send x to each slave
# mpi.bcast.Robj(x)
# #3. Print x in each slave
# mpi.remote.exec(x)
