module opt_levmar_k
use nnet_mod_klayers
use param_k
implicit none
contains
! MODIFICATION FROM ORIGINAL: Instead of calling fdjac2, we compute the gradient in nnet_mod_mod. grad_func_levmar. MUCH FASTER
! 
! MODIFICATION: Print at maxeval.
!
!  MINPACK code: this module contains lmdif and all the subroutines that this
! function call. Every subroutine is well explained before starts.

      real function dpmpar(i)
      integer i
!    **********

!    Function dpmpar

!    This function provides real machine parameters
!    when the appropriate set of data statements is activated (by
!    removing the !from column 1) and all other data statements are
!    rendered inactive. Most of the parameter values were obtained
!    from the corresponding Bell Laboratories Port Library function.

!    The function statement is

!      real function dpmpar(i)

!    where

!      i is an integer input variable set to 1, 2, or 3 which
!        selects the desired machine parameter. If the machine has
!        t base b digits and its smallest and largest exponents are
!        emin and emax, respectively, then these parameters are

!        dpmpar(1) = b**(1 - t), the machine precision,

!        dpmpar(2) = b**(emin - 1), the smallest magnitude,

!        dpmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude.

!    Argonne National Laboratory. MINPACK Project. November 1996.
!    Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More'

!    **********
      integer mcheps(4)
      integer minmag(4)
      integer maxmag(4)
      real dmach(3)
      equivalence (dmach(1),mcheps(1))
      equivalence (dmach(2),minmag(1))
      equivalence (dmach(3),maxmag(1))

!    Machine constants for the IBM 360/370 series,
!    the Amdahl 470/V6, the ICL 2900, the Itel AS/6,
!    the Xerox Sigma 5/7/9 and the Sel systems 85/86.

!    data mcheps(1),mcheps(2) / z34100000, z00000000 /
!    data minmag(1),minmag(2) / z00100000, z00000000 /
!    data maxmag(1),maxmag(2) / z7fffffff, zffffffff /

!    Machine constants for the Honeywell 600/6000 series.

!    data mcheps(1),mcheps(2) / o606400000000, o000000000000 /
!    data minmag(1),minmag(2) / o402400000000, o000000000000 /
!    data maxmag(1),maxmag(2) / o376777777777, o777777777777 /

!    Machine constants for the CDC 6000/7000 series.

!    data mcheps(1) / 15614000000000000000b /
!    data mcheps(2) / 15010000000000000000b /

!    data minmag(1) / 00604000000000000000b /
!    data minmag(2) / 00000000000000000000b /

!    data maxmag(1) / 37767777777777777777b /
!    data maxmag(2) / 37167777777777777777b /

!    Machine constants for the PDP-10 (KA processor).

!    data mcheps(1),mcheps(2) / "114400000000, "000000000000 /
!    data minmag(1),minmag(2) / "033400000000, "000000000000 /
!    data maxmag(1),maxmag(2) / "377777777777, "344777777777 /

!    Machine constants for the PDP-10 (KI processor).

!    data mcheps(1),mcheps(2) / "104400000000, "000000000000 /
!    data minmag(1),minmag(2) / "000400000000, "000000000000 /
!    data maxmag(1),maxmag(2) / "377777777777, "377777777777 /

!    Machine constants for the PDP-11. 

!    data mcheps(1),mcheps(2) /   9472,      0 /
!    data mcheps(3),mcheps(4) /      0,      0 /

!    data minmag(1),minmag(2) /    128,      0 /
!    data minmag(3),minmag(4) /      0,      0 /

!    data maxmag(1),maxmag(2) /  32767,     -1 /
!    data maxmag(3),maxmag(4) /     -1,     -1 /

!    Machine constants for the Burroughs 6700/7700 systems.

!    data mcheps(1) / o1451000000000000 /
!    data mcheps(2) / o0000000000000000 /

!    data minmag(1) / o1771000000000000 /
!    data minmag(2) / o7770000000000000 /

!    data maxmag(1) / o0777777777777777 /
!    data maxmag(2) / o7777777777777777 /

!    Machine constants for the Burroughs 5700 system.

!    data mcheps(1) / o1451000000000000 /
!    data mcheps(2) / o0000000000000000 /

!    data minmag(1) / o1771000000000000 /
!    data minmag(2) / o0000000000000000 /

!    data maxmag(1) / o0777777777777777 /
!    data maxmag(2) / o0007777777777777 /

!    Machine constants for the Burroughs 1700 system.

!    data mcheps(1) / zcc6800000 /
!    data mcheps(2) / z000000000 /

!    data minmag(1) / zc00800000 /
!    data minmag(2) / z000000000 /

!    data maxmag(1) / zdffffffff /
!    data maxmag(2) / zfffffffff /

!    Machine constants for the Univa!1100 series.

!    data mcheps(1),mcheps(2) / o170640000000, o000000000000 /
!    data minmag(1),minmag(2) / o000040000000, o000000000000 /
!    data maxmag(1),maxmag(2) / o377777777777, o777777777777 /

!    Machine constants for the Data General Eclipse S/200.

!    Note - it may be appropriate to include the following card -
!    stati!dmach(3)

!    data minmag/20k,3*0/,maxmag/77777k,3*177777k/
!    data mcheps/32020k,3*0/

!    Machine constants for the Harris 220.

!    data mcheps(1),mcheps(2) / '20000000, '00000334 /
!    data minmag(1),minmag(2) / '20000000, '00000201 /
!    data maxmag(1),maxmag(2) / '37777777, '37777577 /

!    Machine constants for the Cray-1.

!    data mcheps(1) / 0376424000000000000000b /
!    data mcheps(2) / 0000000000000000000000b /

!    data minmag(1) / 0200034000000000000000b /
!    data minmag(2) / 0000000000000000000000b /

!    data maxmag(1) / 0577777777777777777777b /
!    data maxmag(2) / 0000007777777777777776b /

!    Machine constants for the Prime 400.

!    data mcheps(1),mcheps(2) / :10000000000, :00000000123 /
!    data minmag(1),minmag(2) / :10000000000, :00000100000 /
!    data maxmag(1),maxmag(2) / :17777777777, :37777677776 /

!    Machine constants for the VAX-11.

!    data mcheps(1),mcheps(2) /   9472,  0 /
!    data minmag(1),minmag(2) /    128,  0 /
!    data maxmag(1),maxmag(2) / -32769, -1 /

!    Machine constants for IEEE machines.

      data dmach(1) /2.22044604926d-8/
      data dmach(2) /2.22507385852d-8/
      data dmach(3) /1.79769313485d+8/

      dpmpar = dmach(i)
      return
      end function

!    Last card of function dpmpar.

      real function enorm(n2,x)
      integer n2
      real x(n2)
!    **********

!    function enorm

!    given an n-vector x, this function calculates the
!    euclidean norm of x.

!    the euclidean norm is computed by accumulating the sum of
!    squares in three different sums. the sums of squares for the
!    small and large components are scaled so that no overflows
!    occur. non-destructive underflows are permitted. underflows
!    and overflows do not occur in the computation of the unscaled
!    sum of squares for the intermediate components.
!    the definitions of small, intermediate and large components
!    depend on two constants, rdwarf and rgiant. the main
!    restrictions on these constants are that rdwarf**2 not
!    underflow and rgiant**2 not overflow. the constants
!    given here are suitable for every known computer.

!    the function statement is

!      real function enorm(n,x)

!    where

!      n is a positive integer input variable.

!      x is an input array of length n.

!    subprograms called

!      fortran-supplied ... abs,dsqrt

!    argonne national laboratory. minpack project. march 1980.
!    burton s. garbow, kenneth e. hillstrom, jorge j. more

!    **********
      integer i
      real agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs,x1max,x3max,zero
      data one,zero,rdwarf,rgiant /1.0d0,0.0d0,3.834d-20,1.304d19/
      s1 = zero
      s2 = zero
      s3 = zero
      x1max = zero
      x3max = zero
      floatn = n2
      agiant = rgiant/floatn
      do 90 i = 1, n2
         xabs = abs(x(i))
         if (xabs .gt. rdwarf .and. xabs .lt. agiant) go to 70
            if (xabs .le. rdwarf) go to 30

!             sum for large components.

               if (xabs .le. x1max) go to 10
                  s1 = one + s1*(x1max/xabs)**2
                  x1max = xabs
                  go to 20
   10          continue
                  s1 = s1 + (xabs/x1max)**2
   20          continue
               go to 60
   30       continue

!             sum for small components.

               if (xabs .le. x3max) go to 40
                  s3 = one + s3*(x3max/xabs)**2
                  x3max = xabs
                  go to 50
   40          continue
                  if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2
   50          continue
   60       continue
            go to 80
   70    continue

!          sum for intermediate components.

            s2 = s2 + xabs**2
   80    continue
   90    continue

!    calculation of norm.

      if (s1 .eq. zero) go to 100
         enorm = x1max*sqrt(s1+(s2/x1max)/x1max)
         go to 130
  100 continue
         if (s2 .eq. zero) go to 110
            if (s2 .ge. x3max) enorm = sqrt(s2*(one+(x3max/s2)*(x3max*s3)))
            if (s2 .lt. x3max) enorm = sqrt(x3max*((s2/x3max)+(x3max*s3)))
            go to 120
  110    continue
            enorm = x3max*sqrt(s3)
  120    continue
  130 continue
      return

!    last card of function enorm.

      end function
  
      subroutine fdjac2(m,n2,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
      integer m,n2,ldfjac,iflag
      real epsfcn
      real,dimension(:),allocatable:: x,fvec,wa
      real::fjac(1:m,1:n2)
!    **********

!    subroutine fdjac2

!    this subroutine computes a forward-difference approximation
!    to the m by n jacobian matrix associated with a specified
!    problem of m functions in n variables.

!    the subroutine statement is

!      subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)

!    where

!      fcn is the name of the user-supplied subroutine which
!        calculates the functions. fcn must be declared
!        in an external statement in the user calling
!        program, and should be written as follows.

!        subroutine fcn(m,n,x,fvec,iflag)
!        integer m,n,iflag
!        real x(wdim),fvec(m)
!        ----------
!        calculate the functions at x and
!        return this vector in fvec.
!        ----------
!        return
!        end

!        the value of iflag should not be changed by fcn unless
!        the user wants to terminate execution of fdjac2.
!        in this case set iflag to a negative integer.

!      m is a positive integer input variable set to the number
!        of functions.

!      n is a positive integer input variable set to the number
!        of variables. n must not exceed m.

!      x is an input array of length n.

!      fve!is an input array of length m which must contain the
!        functions evaluated at x.

!      fja!is an output m by n array which contains the
!        approximation to the jacobian matrix evaluated at x.

!      ldfja!is a positive integer input variable not less than m
!        which specifies the leading dimension of the array fjac.

!      iflag is an integer variable which can be used to terminate
!        the execution of fdjac2. see description of fcn.

!      epsfcn is an input variable used in determining a suitable
!        step length for the forward-difference approximation. this
!        approximation assumes that the relative errors in the
!        functions are of the order of epsfcn. if epsfcn is less
!        than the machine precision, it is assumed that the relative
!        errors in the functions are of the order of the machine
!        precision.

!      wa is a work array of length m.

!    subprograms called

!      user-supplied ...... fcn

!      minpack-supplied ... dpmpar

!      fortran-supplied ... abs,amax1,dsqrt

!    argonne national laboratory. minpack project. march 1980.
!    burton s. garbow, kenneth e. hillstrom, jorge j. more

!    **********
      integer i,j
      real eps,epsmch,h,temp,zero
      !real dpmpar
      data zero /0.0d0/

!    epsmch is the machine precision.

      epsmch = dpmpar(1)

      eps = sqrt(amax1(epsfcn,epsmch))
      !print*,'estamos en jacobian',eps
       do 20 j = 1, n2
         temp = x(j)
         h = 1000*eps*abs(temp)
         if (h .eq. zero) h = eps
         x(j) = temp + h
         !print*,'h',h
         call err_func_levmar(m,n2,x,wa,iflag)
         if (iflag .lt. 0) go to 30
         x(j) = temp
         do 10 i = 1, m
            fjac(i,j) = (wa(i) - fvec(i))/h
   10       continue
   20    continue
   30 continue
      return

!    last card of subroutine fdjac2.

      end subroutine
      subroutine lmdif(m,n2,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,&
       &diag,mode,factor,nprint,info,iter,xnorm,delta,nfev,fjac,ldfjac,&
                     & ipvt,qtf,wa1,wa2,wa3,wa4)
      use nnet_mod_klayers

      integer m,n2,maxfev,mode,nprint,info,nfev,ldfjac
      integer,dimension(:),allocatable::ipvt
      real ftol,xtol,gtol,epsfcn,factor
      real,dimension(:),allocatable:: diag,qtf,wa1,wa2,wa3,wa4
      real::fjac(1:m,1:n2)
      real, dimension(:),allocatable::x,fvec
!    **********

!    subroutine lmdif

!    the purpose of lmdif is to minimize the sum of the squares of
!    m nonlinear functions in n variables by a modification of
!    the levenberg-marquardt algorithm. the user must provide a
!    subroutine which calculates the functions. the jacobian is
!    then calculated by a forward-difference approximation.

!    the subroutine statement is

!      subroutine lmdif(fcn,m,n,x,fvec,ftol,xtol,gtol,maxfev,epsfcn,
!                       diag,mode,factor,nprint,info,nfev,fjac,
!                       ldfjac,ipvt,qtf,wa1,wa2,wa3,wa4)

!    where

!      fcn is the name of the user-supplied subroutine which
!        calculates the functions. fcn must be declared
!        in an external statement in the user calling
!        program, and should be written as follows.

!        subroutine fcn(m,n,x,fvec,iflag)
!        integer m,n,iflag
!        real x(n),fvec(m)
!        ----------
!        calculate the functions at x and
!        return this vector in fvec.
!        ----------
!        return
!        end

!        the value of iflag should not be changed by fcn unless
!        the user wants to terminate execution of lmdif.
!        in this case set iflag to a negative integer.

!      m is a positive integer input variable set to the number
!        of functions.

!      n is a positive integer input variable set to the number
!        of variables. n must not exceed m.

!      x is an array of length n. on input x must contain
!        an initial estimate of the solution vector. on output x
!        contains the final estimate of the solution vector.

!      fve!is an output array of length m which contains
!        the functions evaluated at the output x.

!      ftol is a nonnegative input variable. termination
!        occurs when both the actual and predicted relative
!        reductions in the sum of squares are at most ftol.
!        therefore, ftol measures the relative error desired
!        in the sum of squares.

!      xtol is a nonnegative input variable. termination
!        occurs when the relative error between two consecutive
!        iterates is at most xtol. therefore, xtol measures the
!        relative error desired in the approximate solution.

!      gtol is a nonnegative input variable. termination
!        occurs when the cosine of the angle between fve!and
!        any column of the jacobian is at most gtol in absolute
!        value. therefore, gtol measures the orthogonality
!        desired between the function vector and the columns
!        of the jacobian.

!      maxfev is a positive integer input variable. termination
!        occurs when the number of calls to fcn is at least
!        maxfev by the end of an iteration.

!      epsfcn is an input variable used in determining a suitable
!        step length for the forward-difference approximation. this
!        approximation assumes that the relative errors in the
!        functions are of the order of epsfcn. if epsfcn is less
!        than the machine precision, it is assumed that the relative
!        errors in the functions are of the order of the machine
!        precision.

!      diag is an array of length n. if mode = 1 (see
!        below), diag is internally set. if mode = 2, diag
!        must contain positive entries that serve as
!        multiplicative scale factors for the variables.

!      mode is an integer input variable. if mode = 1, the
!        variables will be scaled internally. if mode = 2,
!        the scaling is specified by the input diag. other
!        values of mode are equivalent to mode = 1.

!      factor is a positive input variable used in determining the
!        initial step bound. this bound is set to the product of
!        factor and the euclidean norm of diag*x if nonzero, or else
!        to factor itself. in most cases factor should lie in the
!        interval (.1,100.). 100. is a generally recommended value.

!      nprint is an integer input variable that enables controlled
!        printing of iterates if it is positive. in this case,
!        fcn is called with iflag = 0 at the beginning of the first
!        iteration and every nprint iterations thereafter and
!        immediately prior to return, with x and fve!available
!        for printing. if nprint is not positive, no special calls
!        of fcn with iflag = 0 are made.

!      info is an integer output variable. if the user has
!        terminated execution, info is set to the (negative)
!        value of iflag. see description of fcn. otherwise,
!        info is set as follows.

!        info = 0  improper input parameters.

!        info = 1  both actual and predicted relative reductions
!                  in the sum of squares are at most ftol.

!        info = 2  relative error between two consecutive iterates
!                  is at most xtol.

!        info = 3  conditions for info = 1 and info = 2 both hold.

!        info = 4  the cosine of the angle between fve!and any
!                  column of the jacobian is at most gtol in
!                  absolute value.

!        info = 5  number of calls to fcn has reached or
!                  exceeded maxfev.

!        info = 6  ftol is too small. no further reduction in
!                  the sum of squares is possible.

!        info = 7  xtol is too small. no further improvement in
!                  the approximate solution x is possible.

!        info = 8  gtol is too small. fve!is orthogonal to the
!                  columns of the jacobian to machine precision.
!
!        iter      we count from outside 1 for epoch 1 and then counts
!
!        xnorm     avoiding reinitilizing
!
!        delta          "         "
!
!        
     
!      nfev is an integer output variable set to the number of
!        calls to fcn.

!      fja!is an output m by n array. the upper n by n submatrix
!        of fja!contains an upper triangular matrix r with
!        diagonal elements of nonincreasing magnitude such that

!               t     t           t
!              p *(ja!*jac)*p = r *r,

!        where p is a permutation matrix and ja!is the final
!        calculated jacobian. column j of p is column ipvt(j)
!        (see below) of the identity matrix. the lower trapezoidal
!        part of fja!contains information generated during
!        the computation of r.

!      ldfja!is a positive integer input variable not less than m
!        which specifies the leading dimension of the array fjac.

!      ipvt is an integer output array of length n. ipvt
!        defines a permutation matrix p such that jac*p = q*r,
!        where ja!is the final calculated jacobian, q is
!        orthogonal (not stored), and r is upper triangular
!        with diagonal elements of nonincreasing magnitude.
!        column j of p is column ipvt(j) of the identity matrix.

!      qtf is an output array of length n which contains
!        the first n elements of the vector (q transpose)*fvec.

!      wa1, wa2, and wa3 are work arrays of length n.

!      wa4 is a work array of length m.

!    subprograms called

!      user-supplied ...... fcn

!      minpack-supplied ... dpmpar,enorm,fdjac2,lmpar,qrfac

!      fortran-supplied ... abs,amax1,amin1,dsqrt,mod

!    argonne national laboratory. minpack project. march 1980.
!    burton s. garbow, kenneth e. hillstrom, jorge j. more

!    **********
      integer i,iflag,iter,j,l
      real actred,delta,dirder,epsmch,fnorm,fnorm1,gnorm,&
                     & one,par,pnorm,prered,p1,p5,p25,p75,p0001,ratio,&
                     & temp,temp1,temp2,xnorm,zero,suma
   !   real dpmpar,enorm
      data one,p1,p5,p25,p75,p0001,zero/1.0d0,1.0d-1,5.0d-1,2.5d-1,7.5d-1,1.0d-4,0.0d0/
!    epsmch is the machine precision.

      epsmch = dpmpar(1)
      !xnorm=0. !se lo doy fuera
      info = 0
      iflag = 0
      nfev = 0
      !temp=0 
!    check the input parameters for errors.

      if (n2 .le. 0 .or. m .lt. n2 .or. ldfjac.lt. m&
     &    .or. ftol .lt. zero .or. xtol .lt. zero .or. gtol .lt. zero&
     &    .or. maxfev .le. 0 .or. factor .le. zero) go to 300
     !print*,'levmar',size(x)
      if (mode .ne. 2) go to 20
      do 10 j = 1, n2
         if (diag(j) .le. zero) go to 300
   10    continue
   20 continue

!    evaluate the function at the starting point
!    and calculate its norm.

      iflag = 1
      call err_func_levmar(m,n2,x,fvec,iflag)
      !print*,'fvec',fvec(m-1)
      nfev = 1
      if (iflag .lt. 0) go to 300
      fnorm = enorm(m,fvec)
     ! print*,'fnorm',fnorm
      !print*,'fvec',fvec(1:10),fnorm
!    initialize levenberg-marquardt parameter and iteration counter.

      par = zero
      !iter = 1 !se lo doy fuera

!    beginning of the outer loop.

   30 continue

    ! print*,'levmar 30'
!       ca/lculate the jacobian matrix.
         iflag = 2
         call grad_func_levmar(m,n2,x,fjac)
        
       !  call fdjac2(m,n2,x,fvec,fjac,ldfjac,iflag,epsfcn,wa4)
       !  print*,'fjac',fjac(1,:),'weighrts',iflag,m,n2
         
         wa4=0.0
         nfev = nfev + n2
         if (iflag .lt. 0) go to 300
!       if requested, call fcn to enable printing of iterates.

         if (nprint .le. 0) go to 40
         iflag = 0
         if (mod(iter-1,nprint) .eq. 0) then 
         call err_func_levmar(m,n2,x,fvec,iflag)
         endif
         if (iflag .lt. 0) go to 300
   40    continue

!       compute the qr factorization of the jacobian.
         call qrfac(m,n2,fjac,ldfjac,.true.,ipvt,n2,wa1,wa2,wa3)
!       on the first iteration and if mode is 1, scale according
!       to the norms of the columns of the initial jacobian.
         if (iter .ne. 1) go to 80 !aqui importante porque al volver a empezar
!hago esto otra vez
         if (mode .eq. 2) go to 60
         do 50 j = 1, n2
            diag(j) = wa2(j)
            if (wa2(j) .eq. zero) diag(j) = one
   50       continue
   60    continue
!        print*,'hola 60'
!       on the first iteration, calculate the norm of the scaled x
!       and initialize the step bound delta.
         do 70 j = 1, n2
            wa3(j) = diag(j)*x(j)
   70       continue
         xnorm = enorm(n2,wa3)
         delta = factor*xnorm
         if (delta .eq. zero) delta = factor
   80    continue

!       form (q transpose)*fve!and store the first n components in
!       qtf.
         do 90 i = 1, m
            wa4(i) = fvec(i)
   90       continue
         do 130 j = 1, n2
            if (fjac(j,j) .eq. zero) go to 120
            suma = zero
            do 100 i = j, m
               suma = suma + fjac(i,j)*wa4(i)
  100          continue
            temp = -suma/fjac(j,j)
            do 110 i = j, m
               wa4(i) = wa4(i) + fjac(i,j)*temp
  110          continue
  120       continue
            fjac(j,j) = wa1(j)
            qtf(j) = wa4(j)
  130       continue

!       compute the norm of the scaled gradient.
         gnorm = zero
         if (fnorm .eq. zero) go to 170
         do 160 j = 1, n2
            l = ipvt(j)
            if (wa2(l) .eq. zero) go to 150
            suma = zero
            do 140 i = 1, j
               suma = suma + fjac(i,j)*(qtf(i)/fnorm)
  140          continue
            gnorm = amax1(gnorm,abs(suma/wa2(l)))
  150       continue
  160       continue
  170    continue

!       test for convergence of the gradient norm.

         if (gnorm .le. gtol) info = 4
         if (info .ne. 0) go to 300

!       rescale if necessary.

         if (mode .eq. 2) go to 190
         do 180 j = 1, n2
            diag(j) = amax1(diag(j),wa2(j))
  180       continue
  190    continue

!       beginning of the inner loop.

  200   continue
!          determine the levenberg-marquardt parameter.

            !print*,'pnorm',pnorm,wa3(1),wa1(1),diag(1)
            call lmpar(n2,fjac,ldfjac,ipvt,diag,qtf,delta,par,wa1,wa2,&
                     &wa3,wa4)
            
!          store the direction p and x + p. calculate the norm of p.

            do 210 j = 1, n2
               wa1(j) = -wa1(j)
               wa2(j) = x(j) + wa1(j)
               wa3(j) = diag(j)*wa1(j)
  210          continue
            pnorm = enorm(n2,wa3)
!          on the first iteration, adjust the initial step bound.

            if (iter .eq. 1) delta = amin1(delta,pnorm)

!          evaluate the function at x + p and calculate its norm.

            iflag = 1
            call err_func_levmar(m,n2,wa2,wa4,iflag)
            nfev = nfev + 1
            if (iflag .lt. 0) go to 300
            fnorm1 = enorm(m,wa4)
!          compute the scaled actual reduction.
            actred = -one 
            if (p1*fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2
!          compute the scaled predicted reduction and
!          the scaled directional derivative.
           do 230 j = 1, n2
               wa3(j) = zero
               l = ipvt(j)
               temp = wa1(l)
               do 220 i = 1, j
                  wa3(i) = wa3(i) + fjac(i,j)*temp
  220             continue
  230          continue
            temp1 = enorm(n2,wa3)/fnorm
            temp2 = (sqrt(par)*pnorm)/fnorm
            prered = temp1**2 + temp2**2/p5
            dirder = -(temp1**2 + temp2**2)

!          compute the ratio of the actual to the predicted
!          reduction.

            ratio = zero
            !print*,'actred,prered',actred,prered
            if (prered .ne. zero) ratio = actred/prered

!          update the step bound.

            if (ratio .gt. p25) go to 240
               if (actred .ge. zero) temp = p5
               if (actred .lt. zero) temp = p5*dirder/(dirder + p5*actred)
               if (p1*fnorm1 .ge. fnorm .or. temp .lt. p1) temp = p1
               delta = temp*amin1(delta,pnorm/p1)
               !print*,'delta 240',delta
               par = par/temp
               go to 260
  240       continue
               if (par .ne. zero .and. ratio .lt. p75) go to 250
               delta = pnorm/p5
               !print*,'delta 250',delta
               par = p5*par
  250          continue
  260       continue

!          test for successful iteration.

           !print*,'ratio',ratio,p0001
           if (ratio .ge. p0001) then 

!          successful iteration. update x, fvec, and their norms.
          !      print*,'update weights'
                do j = 1, n2
                   x(j) = wa2(j)
                   wa2(j) = diag(j)*x(j)
                enddo
                do i = 1, m
                   fvec(i) = wa4(i)
                enddo 
                xnorm = enorm(n2,wa2)
                fnorm = fnorm1
                iter = iter + 1
!                print*,'iter',iter
                if (mod(iter,maxeval).eq.0) return

           endif
!          tests for convergence.
            if (abs(actred) .le. ftol .and. prered .le. ftol&
              & .and. p5*ratio .le. one) info = 1
             !   print*,'delta,xtol*xnorm',delta,xtol,xnorm
            if (delta .le. xtol*xnorm) info = 2
            if (abs(actred) .le. ftol .and. prered .le. ftol&
              &.and. p5*ratio .le. one .and. info .eq. 2) info = 3
            if (info .ne. 0) go to 300

!          tests for termination and stringent tolerances.

            if (nfev .ge. maxfev) info = 5
            if (abs(actred) .le. epsmch .and. prered .le. epsmch&
               &.and. p5*ratio .le. one) info = 6
            if (delta .le. epsmch*xnorm) info = 7
            if (gnorm .le. epsmch) info = 8
            if (info .ne. 0) go to 300

!          end of the inner loop. repeat if iteration unsuccessful.
          !  print*,'ratio',ratio
            if (ratio .lt. p0001) go to 200

!       end of the outer loop.

         go to 30
  300 continue

!    termination, either normal or user imposed.

      if (iflag .lt. 0) info = iflag
      iflag = 0
      if (nprint .gt. 0) call err_func_levmar(m,n2,x,fvec,iflag)
      return

!    last card of subroutine lmdif.

      end subroutine
      subroutine lmpar(n2,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,wa1,wa2)
      integer n2,ldr
      integer,dimension(:),allocatable:: ipvt
      real delta,par
      real::r(1:ldr,1:n2)      
      real,dimension(:),allocatable::diag,x,wa1,wa2,qtb,sdiag

!    **********

!    subroutine lmpar

!    given an m by n matrix a, an n by n nonsingular diagonal
!    matrix d, an m-vector b, and a positive number delta,
!    the problem is to determine a value for the parameter
!    par s/qrfauch that if x solves the system

!          a*x = b ,     sqrt(par)*d*x = 0 ,

!    in the least squares sense, and dxnorm is the euclidean
!    norm of d*x, then either par is zero and

!          (dxnorm-delta) .le. 0.1*delta ,

!    or par is positive and

!          abs(dxnorm-delta) .le. 0.1*delta .

!    this subroutine completes the solution of the problem
!    if it is provided with the necessary information from the
!    qr factorization, with column pivoting, of a. that is, if
!    a*p = q*r, where p is a permutation matrix, q has orthogonal
!    columns, and r is an upper triangular matrix with diagonal
!    elements of nonincreasing magnitude, then lmpar expects
!    the full upper triangle of r, the permutation matrix p,
!    and the first n components of (q transpose)*b. on output
!    lmpar also provides an upper triangular matrix s such that

!           t   t                   t
!          p *(a *a + par*d*d)*p = s *s .

!    s is employed within lmpar and may be of separate interest.

!    only a few iterations are generally needed for convergence
!    of the algorithm. if, however, the limit of 10 iterations
!    is reached, then the output par will contain the best
!    value obtained so far.

!    the subroutine statement is

!      subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,
!                       wa1,wa2)

!    where

!      n is a positive integer input variable set to the order of r.

!      r is an n by n array. on input the full upper triangle
!        must contain the full upper triangle of the matrix r.
!        on output the full upper triangle is unaltered, and the
!        strict lower triangle contains the strict upper triangle
!        (transposed) of the upper triangular matrix s.

!      ldr is a positive integer input variable not less than n
!        which specifies the leading dimension of the array r.

!      ipvt is an integer input array of length n which defines the
!        permutation matrix p such that a*p = q*r. column j of p
!        is column ipvt(j) of the identity matrix.

!      diag is an input array of length n which must contain the
!        diagonal elements of the matrix d.

!      qtb is an input array of length n which must contain the first
!        n elements of the vector (q transpose)*b.

!      delta is a positive input variable which specifies an upper
!        bound on the euclidean norm of d*x.

!      par is a nonnegative variable. on input par contains an
!        initial estimate of the levenberg-marquardt parameter.
!        on output par contains the final estimate.

!      x is an output array of length n which contains the least
!        squares solution of the system a*x = b, sqrt(par)*d*x = 0,
!        for the output par.

!      sdiag is an output array of length n which contains the
!        diagonal elements of the upper triangular matrix s.

!      wa1 and wa2 are work arrays of length n.

!    subprograms called

!      minpack-supplied ... dpmpar,enorm,qrsolv

!      fortran-supplied ... dabs,amax1,amin1,dsqrt

!    argonne national laboratory. minpack project. march 1980.
!    burton s. garbow, kenneth e. hillstrom, jorge j. more

!    **********
      integer i,iter,j,jm1,jp1,k,l,nsing
      real dxnorm,dwarf,fp,gnorm,parc,parl,paru,p1,p001,suma,temp,zero
     !real dpmpar, enorm
      data p1,p001,zero /1.0d-1,1.0d-3,0.0d0/

!    dwarf is the smallest positive magnitude.

      dwarf = dpmpar(2)

!    compute and store in x the gauss-newton direction. if the
!    jacobian is rank-deficient, obtain a least squares solution.

      nsing = n2
      do 10 j = 1, n2
         wa1(j) = qtb(j)
         if (r(j,j) .eq. zero .and. nsing .eq. n2) nsing = j - 1
         if (nsing .lt. n2) wa1(j) = zero
   10    continue
      if (nsing .lt. 1) go to 50
      do 40 k = 1, nsing
         j = nsing - k + 1
         wa1(j) = wa1(j)/r(j,j)
         temp = wa1(j)
         jm1 = j - 1
         if (jm1 .lt. 1) go to 30
         do 20 i = 1, jm1
            wa1(i) = wa1(i) - r(i,j)*temp
   20       continue
   30    continue
   40    continue
   50 continue
      do 60 j = 1, n2
         l = ipvt(j)
         x(l) = wa1(j)
   60    continue

!    initialize the iteration counter.
!    evaluate the function at the origin, and test
!    for acceptance of the gauss-newton direction.

      iter = 0
      do 70 j = 1, n2
         wa2(j) = diag(j)*x(j)
   70    continue
      dxnorm = enorm(n2,wa2)
      fp = dxnorm - delta
      if (fp .le. p1*delta) go to 220

!    if the jacobian is not rank deficient, the newton
!    step provides a lower bound, parl, for the zero of
!    the function. otherwise set this bound to zero.

      parl = zero
      if (nsing .lt. n2) go to 120
      do 80 j = 1, n2
         l = ipvt(j)
         wa1(j) = diag(l)*(wa2(l)/dxnorm)
   80    continue
      do 110 j = 1, n2
         suma = zero
         jm1 = j - 1
         if (jm1 .lt. 1) go to 100
         do 90 i = 1, jm1
            suma = suma + r(i,j)*wa1(i)
   90       continue
  100    continue
         wa1(j) = (wa1(j) - suma)/r(j,j)
  110    continue
      temp = enorm(n2,wa1)
      parl = ((fp/delta)/temp)/temp
  120 continue

!    calculate an upper bound, paru, for the zero of the function.

      do 140 j = 1, n2
         suma = zero
         do 130 i = 1, j
            suma = suma + r(i,j)*qtb(i)
  130       continue
         l = ipvt(j)
         wa1(j) = suma/diag(l)
  140    continue
      gnorm = enorm(n2,wa1)
      paru = gnorm/delta
      if (paru .eq. zero) paru = dwarf/amin1(delta,p1)

!    if the input par lies outside of the interval (parl,paru),
!    set par to the closer endpoint.

      par = amax1(par,parl)
      par = amin1(par,paru)
      if (par .eq. zero) par = gnorm/dxnorm

!    beginning of an iteration.

  150 continue
         iter = iter + 1

!       evaluate the function at the current value of par.

         if (par .eq. zero) par = amax1(dwarf,p001*paru)
         temp = sqrt(par)
         do 160 j = 1, n2
            wa1(j) = temp*diag(j)
  160       continue
         call qrsolv(n2,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2)
         do 170 j = 1, n2
            wa2(j) = diag(j)*x(j)
  170       continue
         dxnorm = enorm(n2,wa2)
         temp = fp
         fp = dxnorm - delta

!       if the function is small enough, accept the current value
!       of par. also test for the exceptional cases where parl
!       is zero or the number of iterations has reached 10.

         if (abs(fp) .le. p1*delta.or. parl .eq. zero .and. fp .le. temp&
                 & .and. temp .lt. zero .or. iter .eq. 20) go to 220

!       compute the newton correction.

         do 180 j = 1, n2
            l = ipvt(j)
            wa1(j) = diag(l)*(wa2(l)/dxnorm)
  180       continue
         do 210 j = 1, n2
            wa1(j) = wa1(j)/sdiag(j)
            temp = wa1(j)
            jp1 = j + 1
            if (n2 .lt. jp1) go to 200
            do 190 i = jp1, n2
               wa1(i) = wa1(i) - r(i,j)*temp
  190          continue
  200       continue
  210       continue
         temp = enorm(n2,wa1)
         parc = ((fp/delta)/temp)/temp

!       depending on the sign of the function, update parl or paru.

         if (fp .gt. zero) parl = amax1(parl,par)
         if (fp .lt. zero) paru = amin1(paru,par)

!       compute an improved estimate for par.

         par = amax1(parl,par+parc)

!       end of an iteration.

         go to 150
  220 continue

!    termination.

      if (iter .eq. 0) par = zero
      return

!    last card of subroutine lmpar.

      end subroutine
      subroutine qrfac(m,n2,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
      integer m,n2,lda,lipvt
      integer,dimension(:),allocatable:: ipvt
      logical pivot
      real:: a(1:m,1:n)
      real,dimension(:),allocatable::rdiag,acnorm,wa
!    **********

!    subroutine qrfac

!    this subroutine uses householder transformations with column
!    pivoting (optional) to compute a qr factorization of the
!    m by n matrix a. that is, qrfa!determines an orthogonal
!    matrix q, a permutation matrix p, and an upper trapezoidal
!    matrix r with diagonal elements of nonincreasing magnitude,
!    such that a*p = q*r. the householder transformation for
!    column k, k = 1,2,...,min(m,n), is of the form

!                          t
!          i - (1/u(k))*u*u

!    where u has zeros in the first k-1 positions. the form of
!    this transformation and the method of pivoting first
!    appeared in the corresponding linpack subroutine.

!    the subroutine statement is

!      subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)

!    where

!      m is a positive integer input variable set to the number
!        of rows of a.

!      n is a positive integer input variable set to the number
!        of columns of a.

!      a is an m by n array. on input a contains the matrix for
!        which the qr factorization is to be computed. on output
!        the strict upper trapezoidal part of a contains the strict
!        upper trapezoidal part of r, and the lower trapezoidal
!        part of a contains a factored form of q (the non-trivial
!        elements of the u vectors described above).

!      lda is a positive integer input variable not less than m
!        which specifies the leading dimension of the array a.

!      pivot is a logical input variable. if pivot is set true,
!        then column pivoting is enforced. if pivot is set false,
!        then no column pivoting is done.

!      ipvt is an integer output array of length lipvt. ipvt
!        defines the permutation matrix p such that a*p = q*r.
!        column j of p is column ipvt(j) of the identity matrix.
!        if pivot is false, ipvt is not referenced.

!      lipvt is a positive integer input variable. if pivot is false,
!        then lipvt may be as small as 1. if pivot is true, then
!        lipvt must be at least n.

!      rdiag is an output array of length n which contains the
!        diagonal elements of r.

!      acnorm is an output array of length n which contains the
!        norms of the corresponding columns of the input matrix a.
!        if this information is not needed, then acnorm can coincide
!        with rdiag.

!      wa is a work array of length n. if pivot is false, then wa
!        can coincide with rdiag.

!    subprograms called

!      minpack-supplied ... dpmpar,enorm

!      fortran-supplied ... amax1,dsqrt,min0

!    argonne national laboratory. minpack project. march 1980.
!    burton s. garbow, kenneth e. hillstrom, jorge j. more

!    **********
      integer i,j,jp1,k,kmax,minmn
      real ajnorm,epsmch,one,p05,suma,temp,zero
      !real dpmpar,enorm
      data one,p05,zero /1.0d0,5.0d-2,0.0d0/

!    epsmch is the machine precision.

      epsmch =0.05 

!    compute the initial column norms and initialize several arrays.
      do 10 j = 1, n2
         acnorm(j) = enorm(m,a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
   10    continue

!    reduce a to r with householder transformations.

      minmn = min0(m,n2)
      do 110 j = 1, minmn
         if (.not.pivot) go to 40

!       bring the column of largest norm into the pivot position.

         kmax = j
         do 20 k = j, n2
            if (rdiag(k) .gt. rdiag(kmax)) kmax = k
   20       continue
         if (kmax .eq. j) go to 40
         do 30 i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
   30       continue
         rdiag(kmax) = rdiag(j)
         wa(kmax) = wa(j)
         k = ipvt(j)
         ipvt(j) = ipvt(kmax)
         ipvt(kmax) = k
   40    continue

!       compute the householder transformation to reduce the
!       j-th column of a to a multiple of the j-th unit vector.

         ajnorm = enorm(m-j+1,a(j,j))
         if (ajnorm .eq. zero) go to 100
         if (a(j,j) .lt. zero) ajnorm = -ajnorm
         do 50 i = j, m
            a(i,j) = a(i,j)/ajnorm
   50       continue
             a(j,j) = a(j,j) + one

!       apply the transformation to the remaining columns
!       and update the norms.
            !print*,'k',j

         jp1 = j + 1
         if (n2 .lt. jp1) go to 100
         do 90 k = jp1, n2
            suma = zero
           
            do 60 i = j, m
               suma = suma + a(i,j)*a(i,k)
   60          continue
            temp = suma/a(j,j)
           !  a(:,k)=a(:,k)-temp*a(:,j)            
             do 70 i = j, m
               a(i,k) = a(i,k) - temp*a(i,j)
   70          continue
            if (.not.pivot .or. rdiag(k) .eq. zero) go to 80
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k)*sqrt(amax1(zero,one-temp**2))
            if (p05*(rdiag(k)/wa(k))**2 .gt. epsmch) go to 80
            rdiag(k) = enorm(m-j,a(jp1,k))
            wa(k) = rdiag(k)
   80       continue
   90       continue
  100    continue
         rdiag(j) = -ajnorm
  110    continue
      return

!    last card of subroutine qrfac.

      end subroutine
      subroutine qrsolv(n2,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
      integer n2,ldr
      integer,dimension(:),allocatable:: ipvt
      real:: r(1:ldr,1:n2)
      real,dimension(:),allocatable::diag,qtb,x,sdiag,wa
!    **********

!    subroutine qrsolv

!    given an m by n matrix a, an n by n diagonal matrix d,
!    and an m-vector b, the problem is to determine an x which
!    solves the system

!          a*x = b ,     d*x = 0 ,

!    in the least squares sense.

!    this subroutine completes the solution of the problem
!    if it is provided with the necessary information from the
!    qr factorization, with column pivoting, of a. that is, if
!    a*p = q*r, where p is a permutation matrix, q has orthogonal
!    columns, and r is an upper triangular matrix with diagonal
!    elements of nonincreasing magnitude, then qrsolv expects
!    the full upper triangle of r, the permutation matrix p,
!    and the first n components of (q transpose)*b. the system
!    a*x = b, d*x = 0, is then equivalent to

!                 t       t
!          r*z = q *b ,  p *d*p*z = 0 ,

!    where x = p*z. if this system does not have full rank,
!    then a least squares solution is obtained. on output qrsolv
!    also provides an upper triangular matrix s such that

!           t   t               t
!          p *(a *a + d*d)*p = s *s .

!    s is computed within qrsolv and may be of separate interest.

!    the subroutine statement is

!      subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)

!    where

!      n is a positive integer input variable set to the order of r.

!      r is an n by n array. on input the full upper triangle
!        must contain the full upper triangle of the matrix r.
!        on output the full upper triangle is unaltered, and the
!        strict lower triangle contains the strict upper triangle
!        (transposed) of the upper triangular matrix s.

!      ldr is a positive integer input variable not less than n
!        which specifies the leading dimension of the array r.

!      ipvt is an integer input array of length n which defines the
!        permutation matrix p such that a*p = q*r. column j of p
!        is column ipvt(j) of the identity matrix.

!      diag is an input array of length n which must contain the
!        diagonal elements of the matrix d.

!      qtb is an input array of length n which must contain the first
!        n elements of the vector (q transpose)*b.

!      x is an output array of length n which contains the least
!        squares solution of the system a*x = b, d*x = 0.

!      sdiag is an output array of length n which contains the
!        diagonal elements of the upper triangular matrix s.


!      wa is a work array of length n.

!    subprograms called

!      fortran-supplied ... dabs,dsqrt

!    argonne national laboratory. minpack project. march 1980.
!    burton s. garbow, kenneth e. hillstrom, jorge j. more

!    **********
      integer i,j,jp1,k,kp1,l,nsing
      real cos,cotan,p5,p25,qtbpj,sin,suma,tan,temp,zero
      data p5,p25,zero /5.0d-1,2.5d-1,0.0d0/

!    copy r and (q transpose)*b to preserve input and initialize s.
!    in particular, save the diagonal elements of r in x.

      do 20 j = 1, n2
         do 10 i = j, n2
            r(i,j) = r(j,i)
   10       continue
         x(j) = r(j,j)
         wa(j) = qtb(j)
   20    continue

!    eliminate the diagonal matrix d using a givens rotation.

      do 100 j = 1, n2

!       prepare the row of d to be eliminated, locating the
!       diagonal element using p from the qr factorization.

         l = ipvt(j)
         if (diag(l) .eq. zero) go to 90
         do 30 k = j, n2
            sdiag(k) = zero
   30       continue
         sdiag(j) = diag(l)

!       the transformations to eliminate the row of d
!       modify only a single element of (q transpose)*b
!       beyond the first n, which is initially zero.

         qtbpj = zero
         do 80 k = j, n2

!          determine a givens rotation which eliminates the
!          appropriate element in the current row of d.

            if (sdiag(k) .eq. zero) go to 70
            if (abs(r(k,k)) .ge. abs(sdiag(k))) go to 40
               cotan = r(k,k)/sdiag(k)
               sin = p5/sqrt(p25+p25*cotan**2)
               cos = sin*cotan
               go to 50
   40       continue
               tan = sdiag(k)/r(k,k)
               cos = p5/sqrt(p25+p25*tan**2)
               sin = cos*tan
   50       continue

!          compute the modified diagonal element of r and
!          the modified element of ((q transpose)*b,0).

            r(k,k) = cos*r(k,k) + sin*sdiag(k)
            temp = cos*wa(k) + sin*qtbpj
            qtbpj = -sin*wa(k) + cos*qtbpj
            wa(k) = temp

!          accumulate the tranformation in the row of s.

            kp1 = k + 1
            if (n2 .lt. kp1) go to 70
            do 60 i = kp1, n2
               temp = cos*r(i,k) + sin*sdiag(i)
               sdiag(i) = -sin*r(i,k) + cos*sdiag(i)
               r(i,k) = temp
   60          continue
   70       continue
   80       continue
   90    continue

!       store the diagonal element of s and restore
!       the corresponding diagonal element of r.

         sdiag(j) = r(j,j)
         r(j,j) = x(j)
  100    continue

!    solve the triangular system for z. if the system is
!    singular, then obtain a least squares solution.

      nsing = n2
      do 110 j = 1, n2
         if (sdiag(j) .eq. zero .and. nsing .eq. n2) nsing = j - 1
         if (nsing .lt. n2) wa(j) = zero
  110    continue
      if (nsing .lt. 1) go to 150
      do 140 k = 1, nsing
         j = nsing - k + 1
         suma = zero
         jp1 = j + 1
         if (nsing .lt. jp1) go to 130
         do 120 i = jp1, nsing
            suma = suma + r(i,j)*wa(i)
  120       continue
  130    continue
         wa(j) = (wa(j) - suma)/sdiag(j)
  140    continue
  150 continue

!    permute the components of z back to components of x.

      do 160 j = 1, n2
         l = ipvt(j)
         x(l) = wa(j)
  160    continue
      return

!    last card of subroutine qrsolv.

      end subroutine
end module
