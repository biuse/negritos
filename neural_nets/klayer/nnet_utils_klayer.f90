module utils_k
use param_k
use functions
implicit none
contains
! -----------------------------------------------------------------
!
!         In this module I have nnets useful functions
!         1. Initialize weights (Nguyen and Widrow algorithm
!         2. Read files
!         3.
!
!



subroutine NguyenWidrow(wgt)
 use param_k
!-----------------------------------------------------
! initialization proposed by Nguyen and Widrow,
! basically sets the weights range on the weights [-beta,beta]
!
! where     beta=0.7*nhid**(1/nin) 
!
! and bias range is [-0.5,0.5]
!  
! in the examples I have tried it seems to work.
!
!
!----------------------------------------------------
 
 integer::i
 real::beta,norm,bias(1:wdim)
 real,dimension(:),allocatable::wgt
 call init_random_seed() 
! call random_seed()
 call random_number(wgt)
 beta=0.7*(nhid1)**(1/nin)
 bias=beta-2*beta*wgt
 
 call random_number(wgt)
 norm=0.0
 wgt=0.5-wgt
 do i=1,wdim
    norm=norm+wgt(i)**2
 enddo
   norm=sqrt(norm)
 do i=1,wdim
    wgt(i)=beta*wgt(i)/norm
 enddo
 !initialize bias
 if (nhid1.eq.0) then 
     wgt(1:nout)=bias(1:nout)
 else
     wgt(1:nhid1)=bias(1:nhid1)
     wgt(nhid1*(nin+1)+1:nhid1*(nin+1)+nout)=bias(nhid1+1:nhid1+1+nout)
 endif
end subroutine
subroutine readfiles(name_file,ndat,inp,out)
 use functions
 use param_k
 !-------------------------------------------------------
 !  
 !  name_in -- general name given by the user
 !  differnet if classification or regression
 !  
 !  classes in input file from 0 to n-1
 !
 !-----------------------------------------------------
 
 integer::ndat,i,j
 real,dimension(:,:),allocatable::inp,out
 real::class(1:ndat),count(1:nout)
 character(len=200)::name_file
 
 nin=User%nin
 nout=User%nout
 count(:)=0  
 open(12,file=trim(name_file),status='old')
     do i=1,ndat

        if (User%opt_net.eq.0) then 
           read(12,*) class(i),inp(i,1:nin)
           !print*,'HOLAAA',i,class(i),inp(i,1:nin)
           do j=1,nout
              if (class(i).eq.j-1) then ! classes de 0 a N
                 out(i,j)=1
                 count(j)=count(j)+1
              else
                 out(i,j)=0
              endif
           enddo  
        endif   
        if (User%opt_net.eq.1) read(12,*) out(i,1:nout),inp(i,1:nin)
    end do
close(12)
        if (User%opt_net.eq.0) then 
           do i=1,nout
                print*,'cont in class',i,'=',count(i)
           enddo
        endif
end subroutine

subroutine setndata(name_file,ndat)

 character(len=200)::name_file
 integer::ndat

 ndat=0
 open(12,file=trim(name_file),status='old')
      do
         read(12,*,end=100) 
         ndat=ndat+1
        end do
100   close(12)

end subroutine
subroutine tpr(ndata,nt,inputs_tr,out_tr,weights,tpr_train,sum_train)

 use nnet_mod_klayers
 use param_k
 !---------------------------------------------------------
 !
 !  Compute te true positives rate.
 !          Inputs classified properly 
 !    tpr=-------------------------------
 !          Total of inputs in the class
 !
 !----------------------------------------------------------
 integer::i,j,ndata,nt
 integer::cont(1:nt),aciertos(1:nt)
 real,dimension(:),allocatable::weights,tpr_train
 real,dimension(:,:),allocatable::inputs_tr,out_tr
 real::sum_train
 cont=0
 aciertos=0
 

  do i=1,ndata
           x_ini=inputs_tr(i,:)
           y_ini=out_tr(i,:)
 
           call nn(weights)
       do j=1,nt
           if (y_ini(j)==1) then
              cont(j)=cont(j)+1
              if (y_net(j).eq.maxval(y_net)) aciertos(j)=aciertos(j)+1
           endif
       enddo
  enddo
  do j=1,nt
       tpr_train(j)=real(100.*aciertos(j)/cont(j))
  enddo

  sum_train=real(100.*sum(aciertos(:))/sum(cont(:)))

  return
end subroutine
subroutine corr(ndata,nt,out_tr,out_net,weights,r_train)

 use nnet_mod_klayers
 use param_k
 use functions

 integer::ndata,nt,l,m,i,j
 real::r_train
 real,dimension(:),allocatable::weights
 real,dimension(:,:),allocatable::out_net,out_tr
 real::y_train(1:ndata*nt),y_net_train(1:ndata*nt)
 l=0
 m=0 
 do i=1,ndata
         do j=1,nt
             l=l+1
             y_train(l)=out_tr(i,j)
             y_net_train(l)=out_net(i,j)
         enddo

     enddo

          r_train=pears(y_train,y_net_train,ndata*nt)
     return
end subroutine

SUBROUTINE init_random_seed()
            INTEGER :: i, n, clock
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed
          
            CALL RANDOM_SEED(size = n)
            ALLOCATE(seed(n))
          
            CALL SYSTEM_CLOCK(COUNT=clock)
          
            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
            CALL RANDOM_SEED(PUT = seed)
          
            DEALLOCATE(seed)
          END SUBROUTINE
     
end module utils_k
