program SPECPLOT
    use iso_fortran_env, wp => real64
    use spectramod
    implicit none

      type(spectrum) :: spec

      integer :: i,j,k,l,args
      integer :: io,b
      integer :: RUNTYPE
      character(len=:),allocatable :: arg
      character(len=:),allocatable :: fname
      character(len=:),allocatable :: fname2
      character(len=:),allocatable :: dum
      character(len=:),allocatable :: oname
      character(len=:),allocatable :: comname
      character(len=1024) :: cmd
      character(len=126) :: atmp
      logical :: ex
    
      real(wp) :: noisecut
      real(wp) :: wid
      real(wp) :: xmin,xmax,dx
      real(wp) :: dummy
      integer :: npoints
      logical :: matchdx
      logical :: norm
      logical  :: smooth
      integer  :: nsmooth

      logical :: verbose

!====================================================================================!
!-- some Defaults
     oname = 'specplot.dat'

     autodetect = .false.  

     wid = 30.0_wp     !-- line width (FWHM)
     noisecut = 0.0d0  !-- noise cut-off
     !smooth =.false.   !-- apply savitzky-golay polynominal?
     !nsmooth = 10      !-- how often apply Sav-Gol polynominal (if applied)

     matchdx=.false.
     dx=1.0d0
     xmin = 100.0_wp   !-- lower plot boundary
     xmax = 4000.0_wp  !-- upper plot boundary
     npoints = 0
     !npoints = nint((xmax-xmin)/dx) !-- number of points in new plot

     norm = .false.
     verbose =.true.
 
!===================================================================================!

     write(6,'(/,1x,a)')'Command line input:'
     call get_command(cmd)
     write(6,'(1x,a,a,/)')'> ',trim(cmd)

      args = iargc()
      if(args.lt.1)then
         error stop 'no input arguments! exit'
      endif
      arg=repeat(' ',len_trim(cmd)) !allocation


      do i=1,args
         call getarg(i,arg)
           if(arg(1:2)=='--')then
               arg = arg(2:)
           endif

           help : select case( arg )
           case( '-h','-H','-help' )
              write(*,*) 'there is no help!'
              stop
           case default
              continue
           end select help

      !--------------------------------------------------- 
      ! The first argument is a spectrum
         if(i==1)then
           fname=trim(arg)
           inquire(file=fname,exist=ex)
           dum='File '//fname//' does not exist!'
           if(.not.ex) then
              write(0,*) dum
              error stop
           endif
         endif
     
         ARGPARSER : select case( arg )
           case( '-range' )
             if(i+1 .le. args)then
                call getarg(i+1,atmp) 
                read(atmp,*,iostat=io) dummy
                if(io==0)xmin=dummy
             endif    
             if(i+2 .le. args)then
                call getarg(i+2,atmp)
                read(atmp,*,iostat=io) dummy
                if(io==0)xmax=dummy
             endif
             if(xmax < xmin)then
                 dummy = xmax
                 xmax = xmin
                 xmin = xmax
             endif
             npoints = nint(xmax-xmin) !-- number of points in new plot
           case( '-xmin' )
             if(i+1 .le. args)then
                call getarg(i+1,atmp)
                read(atmp,*,iostat=io) dummy
                if(io==0)xmin=dummy
             endif
           case( '-xmax' )
             if(i+1 .le. args)then
                call getarg(i+1,atmp)
                read(atmp,*,iostat=io) dummy
                if(io==0)xmax=dummy
             endif
           case( '-norm' )
             norm = .true.
           case( '-o','-O' )  
             if(i+1 .le. args)then
                call getarg(i+1,atmp)
                oname=trim(atmp)
             endif  
           case( '-lw','-width' ) 
             if(i+1 .le. args)then
                call getarg(i+1,atmp)
                read(atmp,*,iostat=io) dummy
                if(io==0)wid=dummy
             endif
           case( '-dx','-DX' )
             if(i+1 .le. args)then
                call getarg(i+1,atmp)
                read(atmp,*,iostat=io) dummy
                if(io==0)dx = dummy
             endif
           case( '-short' )
             verbose = .false.  
           case default
              continue 
           end select ARGPARSER
      enddo


      if(npoints==0)then
          npoints=nint((xmax-xmin)/dx)
      endif

!==================================================!
! Read the spectrum    

      call spec%read(fname)

      !-- if the spectrum is a plain list of datapoints
      !   try to determine if it is experimental or just
      !   the normal mode frequencies, e.g. from a calculation
      if(spec%spectype == type_plain)then
        call spec%checkplain()
      endif

      if(spec%spectype == type_expl )then
          !--apply new xmin and xmax
          call spec%rerange(xmin,xmax)
          if(matchdx)then
          !-- get points at dx by interpolation
          call spec%interpol_newdx(dx)
          else
          !-- or adjust the spectrum to get exactly npoints
          call spec%interpol_npoints(npoints)
          endif
          if(norm)then
            call spec%numnorm(dummy,'defualt')
            spec%ints = spec%ints * dummy
          endif
          call spec%plot(oname)
      else if(spec%spectype == type_theo)then
          spec%xmi=xmin
          spec%xma=xmax
          !-- apply Lorentzian functions
          ! call spec%expand(npoints,xmin,xmax,dx,wid)
          if(norm)then
            !-- numerical normalization  
            ! call spec%numnorm(dummy,'default')
            ! spec%ints = spec%ints * dummy

            !--- analytical normalization
            call spec%ananorm(dummy,'default',wid)
          endif              
          !call spec%plot(oname)
          call spec%plot2(oname,xmin,xmax,npoints,wid)
      endif

      !-- write a summary
      write(*,'(1x,a,1x,a)') 'File:',spec%filename
      write(*,'(1x,a,1x,i0)') 'number of points:',npoints
      write(*,'(1x,a,f12.2,1x,a,f12.2)')'xmin',xmin,'xmax',xmax
      write(*,'(1x,a,f12.2)') 'dx',dx
      write(*,'(1x,a,1x,l)') 'normalization?',norm
      write(*,*)
      write(*,'(1x,a,1x,a)') 'file written:',oname


end program SPECPLOT

subroutine rdarg(i,arg)
   integer,intent(in) :: i
   character(len=:),allocatable,intent(out) :: arg
   integer :: l,err
   call get_command_argument(i,length=l,status=err)
   allocate( character(len=l) :: arg, stat=err )
   call get_command_argument(i,arg,status=err)
end subroutine rdarg

