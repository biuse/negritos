!
!
!             NEURAL NETWORK, main program
!
!             Inputs:
!             DATA:
!
!             Optimiation algorithm:
!             1. Gradient conjugate (NR) 
!             20/07/2011 Funciona caso lineal, sin neuronas....
!                        segmentation fault si añado 1 neurona  
!              
!             2. Levenberg-Marquardt method
!             3. Simulated annealing
!             4. Powell method
!             5. BFGS method (dfpmin subroutine NR)
!
! MODIFICATIONS:
!
! 23 Feb 2012. Introduction of different hidden layers all with tangh function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program neural_net_k
 
 use param_k
 use nnet_mod_klayers
 use optimizers_mod_k
 use functions
 use opt_levmar_k
 use utils_k 
 !use initial
 implicit none

 integer::i,j,k,m,iter,maxfev
 integer::stopping
 integer::ndata,ntest,epochs,nprint,info,nfev
 integer::nlayer,hidmax
 integer,dimension(:),allocatable::ipvt
 real::r_train,r_test,xnorm,delta,r_test_aux,relative_error_test,relative_error_train
 real::ftol,xtol,gtol,fret,epsfcn,factor,sum_test,sum_train,FP
 real,dimension(:),allocatable::y_train,y_net_train,y_test,y_net_test,tpr_test,tpr_train,diag,qtf,wa1,wa2,wa3,wa4,fvec
 real,dimension(:),allocatable::XI,G
 real,dimension(:),allocatable::xini_save,scalein,offin,scaleout,offout,weights
 real,dimension(:,:),allocatable::fjac,inputs_test,outputs_test,outputs_net_test,outputs_net_train,covar,hessian
 character(len=1)::fichero
 character(len=3)::epoch_num
 character(len=200)::name_in,name_out,inputs_ts,inputs_tr
 
! arg_count=COMMAND_ARGUMENT_COUNT() 
! if (arg_count.ne.7) stop 'Wrong number of arguments'
 ! network parameters ---------------------------------------
 write(*,*)'name of input file'
 read(*,*) name_in
 print*,name_in

 write(*,*)'name of outputs file'
 read(*,*) name_out
 print*,name_out

 write(*,*)'number of active layers (not counting outputs layer)'
 read(*,*) User%nlayer
 nlayer=User%nlayer
 print*,nlayer
 allocate(nhid_vec(1:nlayer+1))
 write(*,*)'nin nhid1 nhid2..., nout'
 read(*,*) nhid_vec(1:nlayer+1)  ! a vector with (ninp,nhid,nhid2,nhid3,...,nout)
 print*,nhid_vec(:)
 write(*,*)'0 for classification 1 for regression network'
 read(*,*) User%opt_net
 if (User%opt_net.lt.0.or.User%opt_net.gt.1) stop 'Wrong network. 0 for classification 1 for regression'

 write(*,*)'Optimization algorithm '
 print*,'1. Levenberg-Marquardt'
 print*,'2. Powell method'
 print*,'3. BFGS'
 read(*,*) User%opt_alg
 if (User%opt_alg.lt.0.or.User%opt_alg.gt.3) stop 'Wrong optimization algorithm chosen'
 print*,User%opt_alg
 write(fichero,'(i1)')User%opt_alg

 write(*,*)'Starting file weight 0 if none 1 if using one that has to&
           & have the same root name as input files'
 read(*,*) User%opt_wgt
 !if (User%opt_wgt.lt.0.or.User%opt_wgt.gt.1) stop 'Wrong weight file option'
 print*,User%opt_wgt

 write(*,*)'Number of evalutaions where we eill check the results'
 read(*,*) maxeval
 print*, maxeval
! 
 User%nin=nhid_vec(1)
 nin=User%nin
 User%nout=nhid_vec(nlayer+1)
 nout=User%nout
 ! allocate matrix of nhid values with dimension nhidmax
 hidmax=0
 if (nlayer.gt.1) then 
    hidmax=maxval(nhid_vec(2:nlayer))
    allocate(h(1:nlayer-1,1:hidmax)) 
 endif

 
   wdim=0
   do i=1,nlayer
      wdim=wdim+nhid_vec(i+1)*(1+nhid_vec(i))
  enddo
  print*,'wdim',wdim
  print*,'hidmax',hidmax
  N=wdim
 
 print*,'nin =', nin
 print*,'nout =', nout
 print*,'nhid =', nhid_vec(2:nlayer)

 ! find ndata and nset and obtain ndata and ntest
 !----------------------------------------------------------
 inputs_ts=trim(name_in)//'_test.txt'
 inputs_tr=trim(name_in)//'_train.txt'
 
 call setndata(inputs_ts,ntest)
 call setndata(inputs_tr,User%ndata)

 ndata=User%ndata
 print*,'ndata =', ndata
 print*,'ntest =', ntest
 print*,'wdim =', wdim
 
 User%ent=.false.
 alpha=100
 ftol=0.000001
 lambda=0.0000
 ! Allocating all variables---------------------------------------

 if (User%opt_net.eq.1) allocate(y_train(1:nout*ndata),y_net_train(1:nout*ndata),y_test(1:ntest*nout),y_net_test(1:ntest*nout))
 
 if (User%opt_alg.eq.1) then
   allocate(fvec(1:ndata),diag(1:wdim),qtf(1:wdim),&
   &wa1(1:wdim),wa2(1:wdim),wa3(1:wdim),wa4(1:ndata),&
   &fjac(1:ndata,1:wdim),ipvt(1:wdim))
 endif
 
 if (User%opt_alg.eq.2) allocate(covar(1:wdim,1:wdim))
 
 allocate(inputs_train(1:ndata,1:nin),outputs_train(1:ndata,1:nout),inputs_test(1:ntest,1:nin),outputs_test(1:ntest,1:nout))
 
 allocate(tpr_train(1:nout),tpr_test(1:nout)) 

 allocate(x_ini(1:nin),xini_save(1:nin),y_ini(1:nout),y_net(1:nout))
 allocate(weights(1:wdim),weights_model2(1:wdim),weights_model1(1:wdim))
 allocate(scalein(1:nin),offin(1:nin),scaleout(1:nout),offout(1:nout)) 
 !--------------------------------------------------------------
 print*,''
 ! reading files test / train
 if (User%opt_net.eq.0) print*,'train-----'
 call readfiles(inputs_tr,ndata,inputs_train,outputs_train)
 if (User%opt_net.eq.0) print*,'test------'
 call readfiles(inputs_ts,ntest,inputs_test,outputs_test)
 print*,'files read'
 print*,'------------------------------------------------------'
 offin=0.
 offout=0.
 scalein=1.
 scaleout=1.
 
! if (VUSer%opt_wgt.eq.0) then ! only useful if weights are taken from neurosys.
! or they are unweighted. In this netowk weights are not unweighted
      do i=1,nin 
         offin(i)=mean(ndata,inputs_train(:,i)) 
         scalein(i)=sigma(ndata,inputs_train(:,i)) 
         inputs_train(:,i)=(inputs_train(:,i)-offin(i))/scalein(i)
         inputs_test(:,i)=(inputs_test(:,i)-offin(i))/scalein(i)
      enddo 
      ! print*,'scalein,offin',scalein(1),offin(1)
      ! print*,'inputs_train',inputs_train(1,:) 
     if (User%opt_net.eq.1) then  
       do i=1,nout 
         offout(i)=mean(ndata,outputs_train(:,i)) 
         scaleout(i)=sigma(ndata,outputs_train(:,i)) 
         outputs_train(:,i)=(outputs_train(:,i)-offout(i))/scaleout(i)
         outputs_test(:,i)=(outputs_test(:,i)-offout(i))/scaleout(i)
      enddo 
    else
         scaleout(:)=1.0
         offout(:)=0.0
     endif

     open(12,file=trim(name_out)//'_scales_in.txt')
     open(14,file=trim(name_out)//'_scales_out.txt')
     do i=1,nin
     write(12,*)offin(i),scalein(i)
     enddo
     close(12)
     do i=1,nout
     write(14,*)offout(i),scaleout(i)
     enddo
     close(14)
 
 if (USer%opt_wgt.eq.0) then
 ! initialization of weights
    if (User%ent.eqv..false.) then 
       call NguyenWidrow(weights)
       !call random_number(weights)
    !open(12,file=trim(name_in)//'_meth4.weights_ini',status='new')
    !write(12,*)weights_ini
    !close(12)
    else
            ! If use entropy, we need the one that allows positive / negative values (see Hobson & Lasenby)
            !model 1 and model 2 have to be positive
            call random_number(weights_model1)
            call random_number(weights_model2)
        do i=1,wdim
            !weights_model1(i)=random_gauss()
            !weights_model2(i)=random_gauss()
            weights(i)=weights_model1(i)-weights_model2(i)
        enddo
        close(12)
    endif
 else
   print*,'read weights from file'
   open(12,file=trim(name_in)//'_weights.ini',status='old')
   do i=1,wdim
      read(12,*)weights(i)
   enddo  
 endif 
 
 new_func2=1 !empezamos desde input 1
 new_func=1 !empezamos desde input 1
 m=0
 stopping=0
 r_test_aux=0.
 iter=1
 
 open(20,file=trim(name_out)//'_corr_test_meth'//fichero//'.txt')
 
 do epochs=1,500
   print*,'' 
   print*,'EPOCHS',epochs!,'weights',weights,inputs_train(1:10,:)
   if (user%ent.eqv..true.)  then
    alpha=alpha*0.8
   print*,'alpha',alpha
   endif
 !choosing algortihm'
   select case (User%opt_alg)
   case (0) 
      print*,'Using conjugate gradients'
      do i=1,ndata
      new_func=new_func+1 
      iter=200
      if (new_func.ge.ndata) new_func=1
      call frprmn(weights,wdim,ftol,iter,Fret)
      enddo
   !================================================================
   case (1) 
      print*,'Using levenberg-Marquardt'
      if (epochs.eq.1) then
         xnorm=0.
         iter=1
         delta=0.
      endif
      xtol=0.001
      gtol=0.01
      factor=1000 
      nprint=0
      maxfev=10000
      epsfcn=10**(-5)
      !lambda=0.1
      call lmdif(ndata,wdim,weights,fvec,ftol,xtol,gtol,maxfev,&
           &epsfcn,diag,1,factor,nprint,info,iter,xnorm,delta,nfev,fjac,ndata,ipvt,qtf,&
           &wa1,wa2,wa3,wa4)
      print*,'iter',iter,'info',info
   !================================================================
    !========================================================
   case(2)
       print*,'Using Powell method'
       call powell(weights,covar,wdim,ftol,iter,Fret)
     print*,'iter',iter
   case(3)
      ! lambda=10
       print*,'Using dfpmin'
       call  dfpmin(weights,wdim,FTOL,XI,Hessian,G,ITER,FRET,FP)    
       print*,'iter',iter
 end select
 
 call random_seed()

 !Checking how it is working using all training and testing data

       
      if (User%opt_net.eq.0) then
         print*,'computing tpr'
         call tpr(ndata,nout,inputs_train,outputs_train,weights,tpr_train,sum_train)
         call tpr(ntest,nout,inputs_test,outputs_test,weights,tpr_test,sum_test)
          do j=1,nout       
             print*,'train class',j,tpr_train(j)
          enddo
          print*,'----------------------------------'
           print*,'TOTAL train',sum_train
          print*,'=================================='
          do j=1,nout       
             print*,'test class',j,tpr_test(j)
          enddo
          print*,'----------------------------------'
             print*,'TOTAL test',sum_test
          print*,'----------------------------------'
          if (sum_test-r_test_aux.le.0.001) stopping=stopping+1
          r_test_aux=sum_test
          print*,'stopping',stopping
          write(20,*)sum_train,sum_test
          if (stopping.eq.20) exit 
      else
          allocate(outputs_net_train(1:ndata,1:nout))
          relative_error_train=0.0
          do i=1,ndata
             x_ini=inputs_train(i,:)
             y_ini=outputs_train(i,:)
             call nn(weights)
     !        y_net(:)=y_net(:)*scaleout(:)+offout(:)
             outputs_net_train(i,:)=y_net(:)
             do j=1,nout
             if (y_ini(j).ne.0) relative_error_train=relative_error_train+abs((y_net(j)-y_ini(j))/y_ini(j))
             enddo
          enddo
          call corr(ndata,nout,outputs_net_train,outputs_train,weights,r_train)           
          deallocate(outputs_net_train)         

          relative_error_test=0.0
          allocate(outputs_net_test(1:ndata,1:nout))
          do i=1,ntest
             x_ini=inputs_test(i,:)
             y_ini=outputs_test(i,:)
             call nn(weights)
             !y_net(:)=y_net(:)*scaleout(:)+offout(:)
             outputs_net_test(i,:)=y_net(:)
             do j=1,nout
             if (y_ini(j).ne.0) relative_error_test=relative_error_test+abs((y_net(j)-y_ini(j))/y_ini(j))
             enddo
          enddo
          call corr(ntest,nout,outputs_net_test,outputs_test,weights,r_test)              
          deallocate(outputs_net_test)                   
          
          if (r_train-r_test_aux.le.0.000001) stopping=stopping+1
          r_test_aux=r_train
          print*,'--------------------------------------'
          print*,'Network performance'
          print*,'--------------------------------------'
          print*,'train correlation',r_train
          print*,'test correlation',r_test
          print*,'-------------------------------------'
          print*,'train relative error',relative_error_train/ndata
          print*,'test correlation',relative_error_test/ntest
          write(20,*)r_train,r_test,relative_error_train/ndata,relative_error_test/ntest
          if (stopping.eq.20) exit 
         
      endif
 !------------------------------------------------------------------
 ! Saving results
      print*,''
      print*,'saving results'        
      open(12,file=trim(name_out)//'_out_test_meth'//fichero//'.txt')
      do i=ndata+1,ndata+ntest
         x_ini=inputs_test(i-ndata,:)
         y_ini=outputs_test(i-ndata,:) 
         do k=1,nin
            xini_save(k)=x_ini(k)*scalein(k)+offin(k)
         enddo  
        call nn(weights)
         do k=1,nout
            y_ini(k)=y_ini(k)*scaleout(k)+offout(k) 
            y_net(k)=y_net(k)*scaleout(k)+offout(k) 
         enddo
         !if (i.lt.10+ndata) print*,y_net(1),weights(1)! computing network ouput
        write(12,*)y_ini,y_net
      enddo
      close(12)
      
      print*,''
      print*,'saving weights'        
      if (epochs.lt.10) write(epoch_num,'(i1)')epochs
      if (epochs.ge.10) write(epoch_num,'(i2)')epochs
      if (epochs.ge.100) write(epoch_num,'(i3)')epochs
    ! open(12,file=trim(name_out)//'_meth'//trim(fichero)//'_weights_com.out')
      open(12,file=trim(name_out)//'_meth'//trim(fichero)//'_weights_com'//'_'//trim(epoch_num)//'.out')
          write(12,*)nin,nout,nhid_vec,wdim,User%opt_net
          do i=1,wdim
              write(12,*)weights(i)
          enddo
      close(12)  
          print*,''
          print*,'Go to next iteration'
          print*,'-------------------------------------'
     
 enddo !close epochs
 close(20)
deallocate(inputs_train,outputs_train,inputs_test,outputs_test)
 print*,'end of program' 
end program neural_net_k

   

