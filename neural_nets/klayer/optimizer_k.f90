
!        Optimizer modules
!        Gradient conjugate Polak-RiBiere method (NR)
!        Powell method (NR)
!        16 september 2011. Modification, so at each upgrade of the weights 
!        we change the error and grad function with new train pair
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module optimizers_mod_k
 use nnet_mod_klayers
 use param_k
 implicit none
 contains
 subroutine dfpmin(P,N,FTOL,XI,Hessian,G,ITER,FRET,FP)
 ! BFGS method from NR
 ! 
 ! Function and its derivative needed, Hessian inerted matrix is estimated in a
 ! iterative procedure.
 !
 ! P=initial point, as output best point that minimizes the function.

 ! brent_log = if .true. linmin uses brent subroutine if false uses dbrent
 !
 ! G, XI, and HESSIAN are internal vairables that are saved at each iterations,
 ! this is way we leave them as intput/output
 !
 ! FRET :: function value at minimum point 
 !---------------------------------------------------------------
 integer::i,j,N
 integer::its,iter
 real, parameter::eps=1.e-10
 real,dimension(:),allocatable::P,XI
 real,dimension(:,:),allocatable::HESSIAN
 real,dimension(:),allocatable::G,DG,HDG
 real::FP,FRET,FAC,FAE,FAD,FTOL
 Logical::brent_log
 brent_log=.true.
 allocate(DG(1:N),HDG(1:N))
 if (iter.eq.1) then
 allocate(Hessian(1:N,1:N),XI(1:N),G(1:N))
    call err_func_gen(P,FP)
    call grad_func_gen(N,P,G)
     do i=1,N
        do j=1,N
           Hessian(i,j)=0.
        enddo
           Hessian(i,i)=1.
           XI(i)=-G(i)
     enddo
    iter=0
 endif
 do its=1,itmax
    iter=iter+1
    if (mod(iter,maxeval).eq.0) return
    !print*,'P after linmin',P(1),XI(1),FRET,FP
    call linmin(P,XI,N,FRET,brent_log) 
    !print*,'P after linmin',P(1),XI(1),FRET,FP
 !   print*,'first if',2.*abs(FRET-FP),'<=',FTOL*(ABS(FRET)+ABS(FP)+EPS)
    IF(2.*ABS(FRET-FP).LE.FTOL*(ABS(FRET)+ABS(FP)+EPS))RETURN  
  !  print*,'hola'
        FP=FRET 
        DO 13 I=1,N 
          DG(I)=G(I) 
13      CONTINUE 
        call err_func_gen(P,FRET) 
        CALL grad_func_gen(N,P,G) 
        DO 14 I=1,N 
          DG(I)=G(I)-DG(I) 
14      CONTINUE 
        DO 16 I=1,N 
          HDG(I)=0. 
          DO 15 J=1,N 
            HDG(I)=HDG(I)+HESSIAN(I,J)*DG(J) 
15        CONTINUE 
16      CONTINUE 
        FAC=0. 
        FAE=0. 
        DO 17 I=1,N 
          FAC=FAC+DG(I)*XI(I) 
          FAE=FAE+DG(I)*HDG(I) 
17      CONTINUE 
        FAC=1./FAC 
        FAD=1./FAE 
        DO 18 I=1,N 
          DG(I)=FAC*XI(I)-FAD*HDG(I) 
18      CONTINUE 
        DO 21 I=1,N 
          DO 19 J=1,N 
            HESSIAN(I,J)=HESSIAN(I,J)+FAC*XI(I)*XI(J)-FAD*HDG(I)*HDG(J)+FAE*DG(I)*DG(J) 
19        CONTINUE 
21      CONTINUE 
        DO 23 I=1,N 
          XI(I)=0. 
          DO 22 J=1,N 
            XI(I)=XI(I)-HESSIAN(I,J)*G(J) 
22        CONTINUE 
23      CONTINUE 
 enddo 
 deallocate(hessian,XI,G,DG,HDG)
 print*,'too many iterations in DFPMIN'
 return
 
 end subroutine dfpmin 
 subroutine powell(P,XI,N,FTOL,ITER,FRET)
!-----------------------------------------------------------------
!  powell method code from NR. 
!  In this case we do not need to introduce derivateives. Tipically goes to a
!  local minimum, but in some cases might be useful. This method finds conjugate
!  directions.
!  
!   P = inital point, and as output best minimum point. Dimension N
!   
!   XI= matrix with inital directions. Usually set as a diagonal covariance
!   matrix XI(i,i)=1 XI(i,j)=0. Output current direction.  Dimension NxN
!     
!   FTOL  determines the failure ti decrease by more than this amount on one
!   iteration
!
!   ITER number of iterations taken
!
!   FRET function returned at best point P
!
!   FUNC, we need to define give the function that we want to minimize
!
!
!----------------------------------------------------------------+
    implicit none
    integer::i,j,ibig,N,iter
    real,dimension(:),allocatable::P,PTT
    real,dimension(:,:),allocatable::XI
    real::PT(N),XIT(N),FRET,FP,FPTT,DEL
    real::FTOL,T
    logical::brent_log
    brent_log=.true.
    !FRET=err_func(P)
    if ( iter.eq.1 ) then
    ! inicialilizo
        do i=1,N
             do j=1,N
                if (i.eq.j)XI(i,j)=1.
                if (i.ne.j)XI(i,j)=0.
             enddo
          enddo
    call err_func_gen(P,fret)
    iter=0
    endif
    PT(:)=P(:)
    
    do 
    iter=iter+1 
    if (mod(iter,maxeval).eq.0) return    
    FP=FRET
    IBIG=0
    DEL=0.
    do i=1,N
       do j=1,N
          XIT(j)=XI(j,i)
       enddo  
       FPTT=FRET
    call linmin(P,XIT,N,FRET,brent_log)
       if (abs(FPTT-FRET).gt.DEL) then
          DEL=abs(FPTT-FRET)
          IBIG=i
       endif
    enddo
   ! print*,2.*ABS(FP-FRET),FTOl*(ABS(FP)+ABS(FRET))
    if (2.*ABS(FP-FRET).le.FTOl*(ABS(FP)+ABS(FRET))) return
    if (iter.eq.itmax) then 
    print*,'Powell excedding maximum iterations' 
    return 
    endif
    allocate(PTT(1:N))
    do j=1,N
       PTT(j)=2.*P(j)-PT(j)
       XIT(j)=P(j)-PT(j)
       PT(j)=P(j)
    enddo
    call err_func_gen(PTT,FPTT)
     deallocate(PTT)
     if (FPTT.lt.FP) then
       T=2.*(FP-2.*FRET+FPTT)*(FP-FRET-DEL)**2-DEL*(FP-FPTT)**2
       if (T.lt.0.) then 
          call linmin(P,XIT,N,FRET,brent_log)
          do j=1,N
              XI(j,ibig)=XIT(j)
           enddo
       endif
     endif
    enddo
end subroutine
subroutine frprmn(P,N,FTOL,ITER,FRET)
    !----------------------------------------------------
    ! frprmn is exact code from NR. with modification of new_func++ as the
    ! counter to change the error function (new network input/output) at 
    ! each weight uptade. 
    ! 
    ! iterative process P_{k}=-g_{k}+beta(k)*P_{k-1}
    !
    !                   delta(gT_{k-1})g_{k}
    ! where     beta(k)=--------------------     
    !                    gT_{k-1}g_{k-1}
    !
    !  gT=tranpost of g
    !
    ! P == input/output variable. Represent points in the solutions space,
    ! in NN case are the weights. P is allocated and initialized in main. As
    ! output gives the best solution at minimu FUNC.
    !
    ! To run this code you need to give the function that you want to minimize
    ! and the gradient. That is given in err_func (function) and grad_err_func
    ! (subroutine). 
    !
    ! N == dimension of P
    !
    ! FTOL tolerance useful to decide when the function is minimum
    ! ITER number of iterations allowed
    ! FRET output value of the function when iter or ftol is reached. 
    !
    ! brent_log indicates to lmin if use brent or dbrent
    !----------------------------------------------------
    implicit none
    integer::its,j,N,iter
    real,parameter::EPS=1.e-20
    real::G(N),H(N),XI(N)
    real::FP,FP0,GG,DGG,GAM,FRET,FTOL
    real,dimension(:),allocatable::P
    logical::brent_log
    brent_log=.false.
 !   print*,'main func',P
    FP=err_func(P)
    FP0=FP
    !print*,'error',FP
    call grad_err_func(P,XI)
    !print*,'main func XI',XI
    do j=1,N
      G(j)=-XI(J)
      H(j)=G(j)
      XI(j)=H(j)
    enddo
    do its=1,itmax
      ITER=its
      call linmin(P,XI,N,FRET,brent_log)
   
      if(2.*abs(FRET-FP).le.FTOL*(abs(FRET)+ABS(FP)+EPS)) return
 !===============modificación======================
 ! deberíamos aqui calcular esto para el siguiente input/output training
 ! puedo poner un contador aqui global, y entonces en err_func y grad_err_func
 ! salta el siguiente input
 !=================================================
     new_func=new_func+1 
     !print*,'new_func',new_func,'x_ini',x_ini
     FP=err_func(P)
     !print*,'FP main',FP
      call grad_err_func(P,XI)
     !print*,'XI main',XI
      GG=0.
      DGG=0.
      do j=1,N
          GG=GG+G(j)**2
          DGG=DGG+XI(J)**2
          DGG=DGG+(XI(j)+G(j))*XI(j)
      enddo
      !print*,'GG',GG
      if(GG.eq.0.)return
      GAM=DGG/GG
      do j=1,N
          G(j)=-XI(j)
          H(j)=G(j)+GAM*H(j)
          XI(j)=H(j)
      enddo
    ! print*,'main func XI enddo',XI
   enddo
   stop 'frpr maximum iterations excedeed'
   return 
end subroutine    

subroutine linmin(P,XI,N,FRET,brent_log)
   
   integer::j,N,Ncom
   integer,parameter::NMAX=10000
   real::Pcom(Nmax),XIcom(Nmax)
   real,parameter::tol=1.e-2 
   real::XI(N)
   common /f1com/ Ncom,Pcom,XIcom
   real::AX,XX,FRET
   real::BX,FA,FX,FB,xmin
   real,dimension(:),allocatable::P
   logical::brent_log

   Ncom=N
   do j=1,N
      Pcom(j)=P(j)
      XIcom(j)=XI(j)
   enddo
    !  print*,'Pcom',P
    !  print*,'XIcom',XI

   AX=0.
   XX=1.
   call mnbrak(AX,XX,BX,FA,FX,FB,f1dim)
  if (brent_log.eqv..false.)   FRET=dbrent(AX,XX,BX,f1dim,df1dim,tol,xmin)
  if (brent_log.eqv..true.)    FRET=brent(AX,XX,BX,f1dim,tol,xmin)
   do j=1,N
      XI(j)=xmin*XI(j)
      P(j)=P(j)+XI(j)
   enddo
   !print*,'XI after brent',XI(1),P(1),FRET
   !print*,'linmin after dbrent and mbrak',P,XI
   return
end subroutine

real function f1dim(X)
   use nnet_mod_klayers
   integer::i,Ncom
   real::Pcom(Nmax),XIcom(Nmax)
   common /F1com/ Ncom,Pcom,XIcom
   real::X
   real,dimension(:),allocatable::XT
   allocate(XT(1:wdim))
   do i=1,Ncom
      XT(i)=Pcom(i)+X*XIcom(i)
   enddo
   !print*,'Pcom',Pcom(1),X,XIcom(1)
   !f1dim=err_func(XT)
   if (User%opt_alg.eq.0) f1dim=err_func(XT)
   if (User%opt_alg.eq.2.or.User%opt_alg.eq.3) call err_func_gen(XT,f1dim)
   !print*,'f1dim',f1dim
   deallocate(XT)
   return
end function f1dim

real function df1dim(X)

   integer::j,Ncom
   real::Pcom(Nmax),XIcom(Nmax)
   COMMON /f1com/ Ncom,Pcom,XIcom
   real::DF(Ncom),X
   real,dimension(:),allocatable::XT
   allocate(XT(1:wdim))
   do j=1,Ncom
      XT(j)=Pcom(j)+X*XIcom(j)
   enddo
   call grad_err_func(XT,DF)
   df1dim=0.
   do j=1,Ncom
      df1dim=df1dim+DF(j)*XIcom(j)
   enddo
  deallocate(XT)
  return
end function df1dim

real function brent(AX,BX,CX,F,TOL,XMIN)
    integer::iter
    integer,parameter::Cgold=0.3819660,itmaxb=2000
    real,parameter::zeps=1.0e-10
    real::A,B,V,W,X,XM,D,R,Q,P
    real::F,FX,FU,FV,FW,AX,BX,CX
    real::U,E,ETEMP
    real::TOL,TOL1,TOL2,Xmin
    A=min(AX,CX)
    B=max(AX,CX)
    V=BX
    W=V
    X=V
    E=0.
    FX=F(X)
    FV=FX
    FW=FX  
    D=0.
    do iter=1,itmaxb
       XM=0.5*(A+B)
       TOL1=TOL*ABS(X)+zeps
       TOL2=2.*TOL1
       if (abs(x-xm).le.(tol2-0.5*(B-A))) then
          xmin=x
          brent=fx
          return
       endif
       if (abs(E).gt.tol1)then
           R=(X-W)*(FX-FV)
           Q=(X-V)*(FX-FW)
           P=(X-V)*Q-(X-W)*R
           Q=2.*(Q-R)
           if (Q.gt.0.) P=-P
           Q=abs(Q)
           ETEMP=E
           E=D
           if (ABS(P).ge.ABS(0.5*Q*ETEMP).or.P.le.Q*(A-X).or.&
               & P.ge.Q*(B-X)) then
              if (X.ge.XM)then
                 E=A-X
              else
                 E=B-X
              endif
              D=CGOLD*E
           else
               D=P/Q
               U=X+D
               if (U-A.lt.tol2.or.B-U.lt.Tol2) D=sign(TOL1,Xm-X)
           endif
        else
              if (X.ge.XM)then
                 E=A-X
              else
                 E=B-X
              endif
              D=CGOLD*E
        endif
        if (ABS(D).ge.TOL1) then
           U=X+D
        else
           U=X+sign(TOL1,D)
        endif
        FU=F(U)
        if(FU.le.FX) then
           if (U.ge.X)then
              A=X
           else
              B=X
           endif
           V=W
           FV=FW
           W=X
           FW=FX
           X=U
           FX=FU
        else
           if (U.lt.X)then
              A=U
           else
              B=U
           endif
           if (FU.le.FW.or.W.eq.X) then
              V=W
              FV=FW
              W=U
              FW=FU
           elseif (FU.le.FV.or.V.eq.X.or.V.eq.W) then
              V=U
              FV=FU
           endif
        endif
      enddo
      print*,'brent exceed maximum iterations'
      xmin=x
      brent=fx
end function    

real function dbrent(AX,BX,CX,F,DF,TOL,Xmin)
   
    integer::i
    real,parameter::zeps=1.0e-10
    integer,parameter::itmaxb=10000
    real::A,B,V,W,X,XM,D,D1,D2,DU,DX,DV
    real::F,DF,FX,FU,FV,FW,DW,AX,BX,CX
    real::U,U1,U2,OLDE,E
    real::TOL,TOL1,TOL2,Xmin
    logical::Ok1,Ok2
   
    A=min(AX,CX)
    B=max(AX,CX)
    V=BX
    W=V
    X=V
    E=0.
    D=0.
    !print*,'dbernt X',X
    FX=F(X)
    !print*,'dbernt FX',FX
    FV=FX
    FW=FX
    DX=DF(X)
    !print*,'dbernt DFDX',DX
    DV=DX
    DW=DX
    do i=1,itmaxb
        XM=0.5*(A+B)
        TOL1=TOL*ABS(X)+zeps
        TOL2=2.*TOL1
        if (abs(x-xm).le.(tol2-0.5*(B-A))) goto 3
        if (abs(E).gt.tol1)then
            D1=2*(B-A)
            D2=D1
            if(DW.ne.DX) D1=(W-X)*DX/(DX-DW)
            if(DV.ne.DX) D2=(V-X)*DX/(DX-DV)
            U1=X+D1      
            U2=X+D2
            Ok1=((A-U1)*(U1-B).gt.0.).and.(DX*D1.le.0.)
            Ok2=((A-U2)*(U2-B).gt.0.).and.(DX*D2.le.0.)
            OLDE=E
            E=D
        if(.not.(Ok1.or.Ok2))then
              goto 1
            else if (Ok1.and.Ok2) then
              if (abs(D1).lt.abs(D2))then
                D=D1
              else
                D=D2
              endif 
            else if (Ok1) then
              D=D1
            else
              D=D2
            endif
            if (abs(D).gt.abs(0.5*OLDE))goto 1
            U=X+D
            if((U-A).lt.TOL2.or.(B-U).lt.TOL2) D=sign(TOL1,XM-X)
            goto 2
        endif
    1  if(DX.ge.0.) then
            E=A-X
        else
            E=B-X
        endif
        D=0.5*E
    2  if (abs(D).ge.TOL1) then
            U=X+D
            FU=F(U)
        else
            U=X+sign(TOL1,D)
            FU=F(U)
            if (FU.gt.FX) goto 3 
        endif
        DU=DF(U)
        if(FU.le.FX)then
            if(U.ge.X) then
               A=X
            else
               B=X
            endif
            V=W
            FV=FW
            DV=DW
            W=X
            FW=FX
            DW=DX
            X=U
            FX=FU
            DX=DU
        else
            if(U.lt.X) then
               A=U
            else
               B=U
            endif
            if(FU.le.FW.or.W.eq.X)then
               V=W
               FV=FW
               DV=DW
               W=U
               FW=FU
               DW=DU
            elseif (FU.le.FV.or.V.eq.X.or.V.eq.W) then
               V=U
               FV=FU
               DV=DU
            endif
        endif
     enddo
     stop 'dbrent excedeed maximum interations'
  !   print*, 'dbrent excedded maximum iterations, we start again' 
  !   Xmin=123.12 !TEST!!!!
  !   dbrent=123.12 !TEST!!!
 3   Xmin=X
     dbrent=FX
     return
end function

subroutine mnbrak(AX,BX,CX,FA,FB,FC,f1dim)
     
     real,parameter::GOLD=1.618034,Glimit=100.,etiny=1.e-20
     real::AX,BX,CX,FA,FB,FC,FU
     real:: DUM,R,Q,U,Ulim
     real,external::f1dim
     FA=f1dim(AX)
     FB=f1dim(BX)
     if (FB.gt.FA) then
        DUM=AX
        AX=BX
        BX=DUM
        DUM=FB
        FB=FA
        FA=DUM
     endif
     CX=BX+GOLD*(BX-AX)
     FC=f1dim(CX)
    
     do while (FB.gt.FC) 
        !print*,'FB,FC',FB,FC
        R=(BX-AX)*(FB-FC)   
        Q=(BX-CX)*(FB-FA)
        !print*,'R',R,Q
        !print*,'BX',BX,2.*sign(max(abs(Q-R),etiny),Q-R)
        U=BX-((BX-CX)*Q-(BX-AX)*R)/(2.*sign(max(abs(Q-R),etiny),Q-R))
        !print*,'U',U
        Ulim=BX+Glimit*(CX-BX)
        if((BX-U)*(U-CX).gt.0.)then
           FU=f1dim(U)
           if(FU.lt.FC)then
              AX=BX
              FA=FB
              BX=U
              FB=FU
             ! print*,'elseif mnbrak FU.lt.FC'
              return
           elseif(FU.gt.FB)then
              CX=U
              FC=FU
             ! print*,'elseif mnbrak FU.gt.FB'
              return
           endif
           U=CX+GOLD*(CX-BX)
           FU=f1dim(U)
        elseif ((CX-U)*(U-Ulim).gt.0.)then
           FU=f1dim(U)
           if(FU.lt.FC)then
               BX=CX
               CX=U
               U=CX+GOLD*(CX-BX)
               FB=FC
               FC=FU
               FU=f1dim(U)
           endif
        elseif((U-Ulim)*(Ulim-CX).ge.0.)then
           U=Ulim
           FU=f1dim(U)
        else
           U=CX+GOLD*(CX-BX)
           FU=f1dim(U)
        endif
        AX=BX
        BX=CX
        CX=U
        FA=FB
        FB=FC
        FC=FU
    enddo
    return

end subroutine

end module
