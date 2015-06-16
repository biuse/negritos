!
!
!
!        NEURAL NETWORK predictions.
!
!        Once the neural netowrk is trained, the weight files are saved
!        and we can use it to estimate the outputs for a given set of
!        new inputs.
!
!        As outputs we have a file with the network computed outputs and the
!        correlation coefficient or true positives rate (depending of type of
!        network)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!111
program nnet_pred
 use param_k
 use nnet_mod_klayers
 use utils_k
 use functions
 integer::i,k
 integer::ndat,nlayer
 integer,allocatable::total_pos(:),total_neg(:),true_positive(:),false_positive(:)
 real::corr_coef,tol
 real,dimension(:),allocatable::weights
 real,dimension(:,:),allocatable::inputs,outputs,outputs_net
 real,allocatable::offin(:),offout(:),scalein(:),scaleout(:)
 character(len=200)::data_file,name_in,weights_name_in
 character(len=1)::fichero
 
 read(*,*) data_file
 read(*,*) weights_name_in
 read(*,*) name_in
 read(*,*) User%nlayer
 read(*,*) User%opt_alg
 nlayer=User%nlayer
 allocate(nhid_vec(1:nlayer+1))
  write(fichero,'(i1)')User%opt_alg
  open(12,file=trim(weights_name_in),status='old') 
       read(12,*)nin,nout,nhid_vec(1:nlayer+1),wdim,User%opt_net    
       allocate(weights(1:wdim))
       do i=1,wdim
           read(12,*)weights(i)
       enddo
  close(12)
  User%nin=nin
  User%nout=nout
  print*,"nin",nin
  print*,"'nout",nout
  print*,"nlayer",nlayer
  print*,"User%opt_net",User%opt_net
  print*,"wdim",wdim
  if (User%opt_net.eq.0) read(*,*) tol

  allocate(scalein(1:nin),offin(1:nin),scaleout(1:nout),offout(1:nout))
  open(12,file=trim(name_in)//'_scales_in.txt',status='old')
       do i=1,nin
           read(12,*)offin(i),scalein(i)
       enddo
  close(12)
  
  open(12,file=trim(name_in)//'_scales_out.txt',status='old')
       do i=1,nout
           read(12,*)offout(i),scaleout(i)
       enddo
  close(12)
  call  setndata(data_file,ndat)
  print*,'ndat',ndat
  print*,'nhid',nhid_vec,wdim,User%opt_net
  !print*,scaleout,offout

  allocate(inputs(1:ndat,1:nin),outputs(1:ndat,1:nout),outputs_net(1:ndat,1:nout))
      call readfiles(data_file,ndat,inputs,outputs)
  allocate(x_ini(1:nin),y_net(1:nout))
  if (User%opt_net.eq.0) then
      allocate(total_pos(1:nout),total_neg(1:nout),true_positive(1:nout),false_positive(1:nout))
      total_pos=0
      total_neg=0
      true_positive=0
      false_positive=0
  endif

  open(14,file=trim(name_in)//'_outputs_prediction_'//fichero//'.out')
  do i=1,ndat
  print*,'HOLA',i,size(inputs),inputs(i,:) 
         x_ini(:)=(inputs(i,:)-offin(:))/scalein(:)
   !      if (i.eq.1) print*,x_ini(1)       
  print*,'OFFIN',offin,'scalein¡',scalein
  print*,'HOLA',i,size(x_ini),x_ini
         call nn(weights)
  print*,'HOLA',i
  !       if (i.eq.1) print*,y_net(1)       
         !print*,'i',y_net
         y_net(:)=y_net(:)*scaleout(:)+offout(:)
         !if (i.eq.1) print*,y_net(1)       
          
         outputs_net(i,:)=y_net(:) 
   !      print*,y_net
   write(14,*)x_ini(:)*scalein(:)+offin(:),y_net 
       if (User%opt_net.eq.0) then
  print*,'HOLA' 

          do k=1,nout
             if (y_net(k).gt.tol)  then
                if (outputs(i,k).gt.0.5) true_positive(k)=true_positive(k)+1
                if (outputs(i,k).lt.0.5) false_positive(k)=false_positive(k)+1
             endif 
             if (outputs(i,k).gt.0.5) total_pos(k)=total_pos(k)+1
             if (outputs(i,k).le.0.5) total_neg(k)=total_neg(k)+1
          enddo        
       endif
  enddo
  close(14)
  deallocate(scalein,scaleout,offin,offout)
       if (User%opt_net.eq.1) then
           
           call corr(ndat,nout,outputs,outputs_net,weights,corr_coef)
          print*,'correlation coefficient',corr_coef
       else
          do k=1,nout
              
          print*,'class',k,'true positive =',true_positive(k),'false positive &
                 &=',false_positive(k),'total pos =',total_pos(k),&
                 & 'total neg =',total_neg(k)
          enddo 
          deallocate(true_positive,false_positive,total_pos,total_neg)
      endif
   deallocate(outputs,outputs_net,inputs)
   print*,'end program'

end program

