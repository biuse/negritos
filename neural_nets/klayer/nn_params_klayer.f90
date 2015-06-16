module param_k
 
type nn_param
     integer::nin
     integer::nout
     integer::nhid1
     integer::nhid2
     integer::nhid3
     integer::nlayer
     integer::ndata
     integer::ntrain
     integer::opt_net ! 0 for classification network 1 for regression network
     integer::opt_alg ! choice of algorithm 0 =gradient 1=genetic 2=simulated
                      !annealing 
     integer::opt_wgt ! choice of algorithm 0 =gradient 1=genetic 2=simulated

     integer::max_iter
     integer::chisq
     integer::rate   ! sted in gradient descent
     logical::ent !.true. for using entropy .false. without

end type

type(nn_param)::User
integer,parameter::itmax=200,Nmax=10000
integer,dimension(:),allocatable::nhid_vec
real,dimension(:),allocatable::x_ini,y_ini,y_net,weights_model1,weights_model2
real,dimension(:,:),allocatable::outputs_train,inputs_train,h
integer::N,nhid1,nin,nout,wdim,new_func,new_func2,maxeval
real::lambda,alpha
end module
