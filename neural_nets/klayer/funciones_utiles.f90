!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!                  MODULO con funciones útiles
!                  sigma,mean,kurtosis,skewness, 
!                  chisquare
!                  Pearson's correlation coefficient                 
!
!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module functions

implicit none 

contains

real function random_gauss()

integer::i,iset
real::X,X2,V1,V2,R,fact,Gset,gasdev

 
iset=0

if (iset.eq.0)then

1 call random_number(X)
  call random_number(X2)
  V1=2*X-1
  V2=2*X2-1

R=V1**2+V2**2

if (R.ge.1..or.R.eq.0.) goto 1 

fact=sqrt(-2*log(R)/R)
Gset=V1*fact
gasdev=V2*fact


iset=1

else

gasdev=gset
iset=0
endif

random_gauss=gasdev
return

end function random_gauss


!====================================================SIGMA
real function sigma(n, list)
    
    integer :: n,i
    real :: list(n),sig,mu

    sig = 0.0
    mu = mean(n,list)
    
    do i = 1, n
       sig = sig + (list(i)-mu)**2
    end do

    sig = sig/float(n)
    sig = sqrt(sig)

    sigma = sig
    
    return

  end function sigma
!======================================================================= MEAN
  real function mean(n, list)
    
    integer :: n,i
    real :: list(n),mu

    mu = 0.0
    
    do i = 1, n
       mu = mu + list(i)
    end do

    mu = mu/float(n)

    mean = mu
    
    return

  end function mean
!========================================================================CHI2

real function skewness(n, list)

    integer :: n,i
    real :: list(n),sig,mu,s

    s = 0.0
    mu = mean(n,list)
    sig = sigma(n,list)

    do i = 1, n
       s = s + (list(i)-mu)**3
    end do

    s = s/float(n)/sig**3
    skewness = s

    return

  end function skewness
!=======================================================
  real function kurtosis(n, list)

    integer :: n,i
    real :: list(n),sig,mu,k

    k = 0.0
    mu = mean(n,list)
    sig = sigma(n,list)

    do i = 1, n
       k = k + (list(i)-mu)**4
    end do

    k = k/float(n)/sig**4-3.0
    kurtosis = k

    return


  end function kurtosis
!=================================================
  real function chi(dim, Cinv,V)
    
    integer :: dim
    real,dimension(dim,dim) :: Cinv
    real,dimension(1,dim) :: V
    real,dimension(1,1) :: chi_aux
    real,dimension(dim,1)::aux_chi
   


      aux_chi=matmul(Cinv,transpose(V))
      chi_aux=matmul(V,aux_chi)  
     
      chi=chi_aux(1,1)
         
    return

  end function chi

!==================================================
  real function pears(X,Y,N2)
     
     integer :: i,N2
     real::X(N2),Y(N2),AX,AY,XT,SXX,SYY,SXY,YT,R

     AX=0.
     AY=0.
     do i=1,N2
        AX=AX+X(i)
        AY=AY+Y(i)
     enddo
     AX=AX/N2
     AY=AY/N2
     SXX=0.
     SYY=0.
     SXY=0.
     do i=1,N2
        XT=X(i)-AX
        YT=Y(i)-AY
        SXX=SXX+XT**2
        SYY=SYY+YT**2
        SXY=SXY+XT*YT
     enddo
     R=SXY/sqrt(SXX*SYY)
     pears=R
     return
  end function
    
  
end module
