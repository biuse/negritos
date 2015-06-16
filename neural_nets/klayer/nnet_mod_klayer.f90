!
!             In this module network functions 
!             1. network
!             2. Initilization method Nyugen-Widrow
!             3. error functions for different algorithms
!             4. gradient function for gradient conjugate
!           
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
module nnet_mod_klayers
implicit none
contains

subroutine nn(w) 
use param_k

 !------------------------------------------------------------------
  !  compute the neural netowrk ouput for regression and classification
  !
  !     y_net(i)=sum(w(i,j)*tanh(sum(j,k)*x(k)+bias(j))+bias(i)
  !  
  !  w == weights (actualized by the algorithm choosen)
  !  x_ini = global variables, defined in main and called in error_functions
  !  y_net = network ouput. For classification outputs are transformed into
  !          probabilities y_net(i)=e^y_net(i)/sum(e^y_net(i))
  !  h(k) = hidden units
  !----------------------------------------------------------------------

  integer::i,j,k,pos1,pos2
  integer::nhid_i,nhid_l,nlayer
  real::aux,bias
  real::probs
  real,dimension(:),allocatable::w,w2,inp,outp
  
  nlayer=User%nlayer
  if (nlayer==1) then ! this means just inputs and outputs no hidden nodes!
             
             call nnet_last_layer(w,x_ini,nin,y_net)
  else

        do k=1,nlayer
             
             nhid_i=nhid_vec(k)
             nhid_l=nhid_vec(k+1)
             allocate(inp(1:nhid_i),w2(1:nhid_l*(nhid_i+1)))
             if (k.eq.1) then 
                 pos2=0
                 inp=x_ini
             else
                 inp=h(k-1,1:nhid_i)
             endif
                 pos1=pos2+1
                 pos2=pos2+nhid_l*(nhid_i+1)
                 w2=w(pos1:pos2)

             if (k.lt.nlayer) then
                  allocate(outp(1:nhid_l))
                  call nnet_layer(w2,inp,nhid_l,nhid_i,outp)
                  h(k,1:nhid_l)=outp !solo guardamos en h los vlaores en
                                     !hidden nodes
                  deallocate(inp,w2,outp) 
             else
                  call nnet_last_layer(w2,inp,nhid_i,y_net) ! ultima capa
                  deallocate(inp,w2)
             endif
           
         end do

   endif
   if (User%opt_net==0) then ! output transformation for classification case
   probs=0.0
          
           do i=1,nout
              probs=probs+exp(y_net(i))
           enddo
           do i=1,nout
              y_net(i)=exp(y_net(i))/(probs+1.e-10)
           enddo
   endif
  return

end subroutine

subroutine err_func_gen(wgt,fval)
 use param_k
!------------------------------------------------------------------------
!    error asked in various methods. (gradient conjugates, simulated annealing,..). sum over i => 1:nout 
!    and t => 1:ndata of err ((y-y_net)**2 in regression and y*log(y_net)
!    in classification problem.)
!
!    fval=sum(i,t)err
!-------------------------------------------------------------------------
 integer::i
 real::fval,ent
 real,dimension(:),allocatable::wgt

fval=0.0
 do i=1,User%ndata
    x_ini=inputs_train(i,:) 
    y_ini=outputs_train(i,:)
    fval=fval+err_func(wgt)
 enddo
 !print*,'fval',User%ndata,fval
 if (User%ent.eqv..true.) then
          ent=entropy(wgt) 
          fval=fval-alpha*ent
 endif
  fval=fval/User%ndata
 return 
end subroutine
subroutine grad_func_gen(wdim2,wgt,fgrad)
!----------------------------------------------------------------------
!
!    gradient asked in gradient conjugate 
!
!    fdjac(t)=sum(i)grad
!
!================================================================

 use param_k
 !use opt_levmar
 integer::i,wdim2
 real,dimension(:),allocatable::wgt
 real::grad(1:wdim2),grad_ent
 real::fgrad(1:wdim2)
 
 fgrad(1:wdim2)=0.0
 do i=1,User%ndata
    x_ini=inputs_train(i,:) 
    y_ini=outputs_train(i,:)
    call grad_err_func(wgt,grad)
    fgrad(1:wdim2)=fgrad(1:wdim2)+grad(1:wdim2)
 enddo
 if (User%ent.eqv..true.) then

   do i=1,wdim2
          grad_ent=dentropy(wgt(i),weights_model1(i),weights_model2(i))
          fgrad(i)=fgrad(i)-alpha*(grad_ent)
    enddo
 endif
   fgrad=fgrad/User%ndata
 return 
end subroutine

subroutine err_func_levmar(ndata2,wdim2,wgt,fvec,iflag)
!----------------------------------------------------------------------
!
!    error asked in levenberg-marquardt problem. fvec(t) sum over nout 
!    (y-y_net)**2 in regression and y*log(y_net) in classification problem,
!    where t is 1:ndata.
!
!    fvev(t)=sum(i)err
!
!================================================================

 use param_k
 integer::i,ndata2,wdim2,iflag
 real,dimension(:),allocatable::wgt,fvec

 do i=1,ndata2
    x_ini=inputs_train(i,:) 
    y_ini=outputs_train(i,:)
    fvec(i)=err_func(wgt)
 enddo
 return 
end subroutine
subroutine grad_func_levmar(ndata2,wdim2,wgt,fgrad)
!----------------------------------------------------------------------
!
!    gradient asked in levenberg-marquardt problem. 
!
!    fdjac(t)=sum(i)grad
!
!================================================================

 use param_k
 !use opt_levmar
 integer::i,ndata2,wdim2
 real,dimension(:),allocatable::wgt
 real::grad(1:wdim2)
 real::fgrad(1:ndata2,1:wdim2)
 
do i=1,ndata2
    x_ini=inputs_train(i,:) 
    y_ini=outputs_train(i,:)
    call grad_err_func(wgt,grad)
    fgrad(i,:)=grad(:)
 enddo
 return 
end subroutine

real function err_func(W)
  ! --------------------------------------------------------------------- 
  !    sum of errors E=0.5*sum (y(i)-y_net(i))**2 or E=-sum (y(i)*log(y_net(i)))
  !    where i=1:nout. Basic error for gradient conjugate problem, and used 
  !    for other algorithms
  !-----------------------------------------------------------------------

 use param_k 
 integer::i
 real::err_aux,penalty
 real,dimension(:),allocatable::W
 
!incremental training if optimization algorithm is conjugate gradient
 if (User%opt_alg.eq.0) then
   x_ini=inputs_train(new_func,:) 
   y_ini=outputs_train(new_func,:)
 endif
!print*,'leve marq',y_ini,y_net
 
 call nn(W)
 penalty=0.
 do i=1,wdim
    penalty=penalty+0.5*lambda*W(i)**2 !penalize large weights
 enddo
 err_aux=0.
 if (User%opt_net==0) then
        ! Classification optimization function (Cross entropy)
         do i=1,nout
            err_aux=penalty+err_aux-(y_ini(i)*log(y_net(i)+1.e-20))
         enddo
 else 
       ! Regression optimization function (mse error)
      do i=1,nout
           err_aux=penalty+err_aux+0.5*(y_ini(i)-y_net(i))**2 
      enddo
 endif
 err_func=err_aux/nout
 return
end function

 subroutine grad_err_func(X,grad)
 use param_k
!--------------------------------------------------------------
!  gradient computation just for algorithm 0.
!------------------------------------------------------------------
 
  integer::i,j,k,pos1,pos2,nhid_l,nhid_i,nhid_m
  integer::nlayer
  real::aux,signo
  real::grad(1:wdim),w_aux(1:wdim)
  real::grad_aux(1:nout)
  real,dimension(:),allocatable::X,w2,grad_inp,grad_out,inp,med

  call nn(X) ! computing y_net

  grad(:)=0.
  nlayer=User%nlayer
  
  if (nlayer==1) then
     allocate(grad_out(1:wdim))
     call grad_last_layer(x_ini,nin,grad_out)
     grad=grad_out
     deallocate(grad_out)    
  else
      do k=nlayer,1,-1
         nhid_i=nhid_vec(k)
         nhid_l=nhid_vec(k+1)
         allocate(inp(1:nhid_i),grad_out(1:nhid_l*(nhid_i+1)))
         if(k.eq.nlayer) then
            inp=h(k-1,1:nhid_i) ! h just saves hidden nodes 
            call grad_last_layer(inp,nhid_i,grad_out)    
            !posiciones en el array de pesos
            pos2=wdim
            pos1=pos2-(nhid_l*(nhid_i+1))+1
            grad(pos1:pos2)=grad_out
        else
           nhid_m=nhid_l
           nhid_l=nhid_vec(k+2)
           allocate(w2(1:nhid_l*nhid_m),grad_inp(1:nhid_l),med(1:nhid_m))
           w2=X((pos1+nhid_l):pos2) ! pesos capa k+1 sin los bias
           grad_inp=grad(pos1:(pos1+nhid_l-1))! solo gradiente del bias anterior k+1 
           if (k.ne.1) inp=h(k-1,1:nhid_i)
           if (k.eq.1) inp=x_ini
           med=h(k,1:nhid_m)
           call grad_layer(w2,grad_inp,inp,med,nhid_i,nhid_m,nhid_l,grad_out)    
           pos2=pos1-1 
           pos1=pos1-(nhid_m*(nhid_i+1))
           grad(pos1:pos2)=grad_out
           deallocate(grad_inp,w2,med) 
       endif     
       deallocate(inp,grad_out)
     enddo
  endif
  
  w_aux=0.0 
  
  !avoiding large weights. Lambda is a free parameter
  do i=1,wdim
     w_aux(i)=lambda*X(i)
  enddo
  
  do i=1,wdim
     grad(i)=grad(i)+w_aux(i)
  enddo
 return
end subroutine

subroutine nnet_last_layer(w,inp,nhid_i,outp) 
use param_k 
  integer::i,k,nhid_l,nhid_i 
  real::aux,bias 
  real,dimension(:),allocatable::w,inp 
  real,dimension(:),allocatable::outp 
 
  aux=0. 
             do i=1,nout 
                bias=w(i) 
                aux=bias 
                do k=1,nhid_i 
                   aux=aux+w((i-1)*nhid_i+nout+k)*inp(k) 
                end do 
                outp(i)=aux 
             enddo 
   return 
end subroutine 
 
subroutine nnet_layer(W,inp,nhid_l,nhid_i,outp) 
use param_k 
  integer::k,j,nhid_l,nhid_i 
  real::aux,bias 
  real,dimension(:),allocatable::w,inp,outp 
 
 aux=0. 
  
     do k=1,nhid_l 
                 bias=w(k) 
                 aux=bias 
                 do  j=1,nhid_i 
                     aux=aux+w((k-1)*nhid_i+nhid_l+j)*inp(j) 
                 end do 
                 outp(k)=tanh(aux) 
    end do 
 
   return 
end subroutine 

subroutine grad_last_layer(inp,nhid_i,outp) 
use param_k 
  integer::i,k,nhid_l,nhid_i 
  real::signo 
  real,dimension(:),allocatable::outp,inp 
 
  signo=1.
  if (User%opt_alg.eq.0) then
      x_ini=inputs_train(new_func,:) 
      y_ini=outputs_train(new_func,:)
   signo=-1.
  endif
  
  do i=1,nout
       outp(i)=signo*(-y_ini(i)+y_net(i))/nout !derivada error con respecto
         do k=1,nhid_i   
            outp((i-1)*nhid_i+nout+k)=outp(i)*inp(k) !derivada pesos ik
        enddo
  enddo

   return 
end subroutine 
 
subroutine grad_layer(W,grad_inp,inp,med,nhid_i,nhid_m,nhid_l,outp) 
use param_k 
  integer::i,k,j,nhid_l,nhid_i,nhid_m 
  real::aux
  real,dimension(:),allocatable::w,inp,outp,med,grad_inp 
      do i=1,nhid_m
      aux=0.
          do j=1,nhid_l
             aux=aux+grad_inp(j)*W((j-1)*nhid_m+i)
          enddo  
          outp(i)=(1-med(i)**2)*aux 
          do k=1,nhid_i
             outp((i-1)*nhid_i+nhid_m+k)=outp(i)*inp(k) 
          enddo
      enddo
 
   return 
end subroutine 
real function entropy(wgt)
use param_k
integer::i
real,allocatable::wgt(:)
real::S,phi

S=0.0
do i=1,wdim
   phi=comphi(wgt(i),weights_model1(i),weights_model2(i))
   S=S+phi-weights_model1(i)-weights_model2(i)-wgt(i)*log((phi+wgt(i)+1e-20)/(2*weights_model1(i)))
enddo
entropy=S
return
end function

real function dentropy(wgti,wgtm1,wgtm2)
real::wgti,wgtm1,wgtm2
real::S,phi,dphi

S=0.0
   phi=comphi(wgti,wgtm1,wgtm2)
   dphi=dcomphi(wgti,wgtm1,wgtm2)
   S=dphi-log((phi+wgti+1e-20)/(2*wgtm1))-wgti*(dphi+1)/(phi+wgti)
dentropy=S
return
end function

real function comphi(wgti,wgtm1,wgtm2)

real::wgti,wgtm1,wgtm2

comphi=sqrt(wgti**2+4*wgtm1*wgtm2)
return

end function


real function dcomphi(wgti,wgtm1,wgtm2)

real::wgti,wgtm1,wgtm2

dcomphi=wgti/sqrt(wgti**2+4*wgtm1*wgtm2)
return

end function

end module
