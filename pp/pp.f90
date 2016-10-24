! strip missing values from cosp output and re-write  to files containing 3hr slices
PROGRAM pp

 USE mo_util 

 IMPLICIT NONE

! CHARACTER(LEN=8), PARAMETER :: ifile="ifile.nc" 
 CHARACTER(LEN=512) :: ifile 
 CHARACTER(LEN=512) :: opath
 CHARACTER(LEN=14), PARAMETER :: ofstr="cf3hr" 
 INTEGER :: icid, ndims, recdim, ngatts, yyid, npts0, npts, iyyid, nvars
 INTEGER, ALLOCATABLE, DIMENSION(:) :: idimsize, odimsize, dimid
 INTEGER, ALLOCATABLE, DIMENSION(:,:) :: vdimid
 INTEGER, ALLOCATABLE, DIMENSION(:) :: ovarid, vtype, nvdims
 CHARACTER(len=256), ALLOCATABLE, DIMENSION(:) :: dimname, vname
 CHARACTER(len=256) :: name, ofile
 INTEGER, ALLOCATABLE, DIMENSION(:) :: year0, month0, day0, hour0
 INTEGER, ALLOCATABLE, DIMENSION(:) :: year, month, day, hour, indarr
 INTEGER, ALLOCATABLE, DIMENSION(:) :: indarr_sl
 INTEGER, PARAMETER :: imissing = -999 
 INTEGER, PARAMETER :: maxdim = 3

 INTEGER, PARAMETER :: per_day=8
 INTEGER, PARAMETER, DIMENSION(per_day) :: hours = (/0,3,6,9,12,15,18,21/)
 CHARACTER(len=4) :: ystr
 CHARACTER(len=2) :: mstr, dstr, hstr
  ! maximum number of 3-hr slices in input file plus 1
 INTEGER, PARAMETER :: maxslice = 260
 INTEGER, DIMENSION(  maxslice ) :: istart, icount

 INTEGER :: dimidpt, varid, i, ihid, iyid, imid, idid, ihstart,ihend, isl
 INTEGER :: ocid, is, p_dim_id, dum1 , dum2, slen_out, slen_in, dimidlev, nlev
 INTEGER :: ip, ii,px
 INTEGER, DIMENSION(1) :: start_in_1d, count_in_1d
 INTEGER, DIMENSION(2) :: start_in_2d, count_in_2d

 INTEGER, ALLOCATABLE, DIMENSION(:) :: v1di_in, v1di_out, ind_out
 REAL, ALLOCATABLE, DIMENSION(:) :: v1dr_in, v1dr_out
 REAL, ALLOCATABLE, DIMENSION(:,:) :: v2dr_in,  v2dr_out

 LOGICAL :: copied

 NAMELIST /ppctl/                &
               ifile,            &
               opath            

  WRITE(6,*) "This is offl pp"

! open input file
  READ(5,ppctl)
  print *, TRIM(ifile)
  print *, TRIM(opath)


  CALL check(NF_OPEN(TRIM(ifile), NF_NOWRITE, icid))
 
  CALL check(NF_INQ(icid, ndims, nvars, ngatts, recdim))

  ALLOCATE (dimname (ndims) )
  ALLOCATE (idimsize (ndims) )
  ALLOCATE (odimsize (ndims) )
  ALLOCATE (dimid (ndims) )

  ALLOCATE (vname (nvars))  
  ALLOCATE (vtype (nvars))  
  ALLOCATE (nvdims (nvars))
  ALLOCATE( ovarid(nvars))

  icount=0
  istart=0
  
  DO i=1, ndims
    CALL check(NF_INQ_DIM(icid,i,dimname(i),idimsize(i)))
  END DO

  CALL check(NF_INQ_DIMID( icid, "point", dimidpt) )
  CALL check(NF_INQ_DIMLEN( icid, dimidpt, npts0))

  CALL check(NF_INQ_DIMID( icid, "level", dimidlev) )
  CALL check(NF_INQ_DIMLEN( icid, dimidlev, nlev))

  CALL check(NF_INQ_VARID( icid, "year", iyid))
  ALLOCATE(year0(npts0))
  CALL check(NF_GET_VAR_INT( icid, iyid, year0))

  CALL check(NF_INQ_VARID( icid, "month", imid))
  ALLOCATE(month0(npts0))
  CALL check(NF_GET_VAR_INT( icid, imid, month0))

  CALL check(NF_INQ_VARID( icid, "day", idid))
  ALLOCATE(day0(npts0))
  CALL check(NF_GET_VAR_INT( icid, idid, day0))

  CALL check(NF_INQ_VARID( icid, "hour", ihid))
  ALLOCATE(hour0(npts0))
  CALL check(NF_GET_VAR_INT( icid, ihid, hour0))


   ALLOCATE(vdimid(nvars,maxdim))
   DO i=1,nvars
     CALL check(NF_INQ_VARNDIMS (icid, i , nvdims(i)))
     CALL check(NF_INQ_VARNAME (icid, i, vname(i)))
     CALL check(NF_INQ_VARTYPE (icid, i, vtype(i) ))  
     CALL check(NF_INQ_VARDIMID (icid, i, vdimid(i,1:nvdims(i))))
   END DO 


 ! get rid of missing values
    npts=0
   DO i=1, npts0
    IF ( hour0(i) .NE. imissing ) THEN 
      npts=npts+1
    END IF
   END DO

   ALLOCATE ( year(npts) )
   ALLOCATE ( month(npts) )
   ALLOCATE ( day(npts) )
   ALLOCATE ( hour(npts) )
   ALLOCATE ( indarr(npts) )
   ALLOCATE ( indarr_sl(npts) )

     npts=0
   DO i=1,npts0
     IF ( hour0(i) .NE. imissing ) THEN     
        npts=npts+1
        hour(npts)  = hour0(i)
        year(npts)  = year0(i)
        month(npts) = month0(i)
        day(npts)   = day0(i)
        indarr(npts)=i
     END IF
   END DO

! set bounds for 3-hr slices (ihstart, ihend)

   ihstart=per_day
  DO i=1,per_day-1
   !!write(6,*) hour(1), hours(i)
   IF ( hour(1) .GE. hours(i) .AND. hour(1) .LT. hours(i+1) ) THEN
     ihstart=i
   END IF
  END DO

   ihend=MOD(ihstart, per_day)+1
   isl=1   
   istart(isl)=1

  DO i=1, npts
   IF ( hour(i) .EQ. hours(ihend) ) THEN
    isl=isl+1
    istart(isl)=i
     IF (isl .GE. maxslice ) THEN
       WRITE(0,*) "ERROR: increase maxslice"
       STOP
     END IF
    ihstart=ihend
    ihend=MOD(ihstart, per_day)+1
   END IF
   icount(isl) = icount(isl)+1
   !write(6,*) i, hour(i), hours(ihstart), hours(ihend), " -- ",  isl,  istart(isl), icount(isl)
  END DO
  
   istart(isl+1)=npts
   odimsize=idimsize

    write(6,*) SUM(icount(1:isl)), npts

! slice loop ----
 slice_loop: DO is=1, isl
 write(6,*) "== slice ", is, " of ",isl
  ! open output file
     WRITE(ystr,'(I4.4)') year(istart(is))
     WRITE(mstr,'(I2.2)') month(istart(is))
     WRITE(dstr,'(I2.2)') day(istart(is))
     WRITE(hstr,'(I2.2)') hour(istart(is))
     WRITE(ofile,*) TRIM(opath)//"/"//TRIM(ofstr)//"_"//ystr//"-"//mstr//"-"//dstr//"_"//hstr//":00:00"
    ofile=ADJUSTL(ofile) 


    CALL check(NF_CREATE(TRIM(ofile), NF_CLOBBER, ocid))

  !  number of points in new file
    slen_out=icount(is)
    odimsize(dimidpt)=slen_out

    IF (is .EQ. isl ) THEN
      px=1
    ELSE
      px=0
    END IF   
 
    slen_in=indarr(istart(is+1))-indarr(istart(is))+ px
    !write(6,*) "slen_in, slen_out ", slen_in, slen_out 

    ALLOCATE (v1di_out( slen_out ))
    ALLOCATE (v1dr_out( slen_out ))
    ALLOCATE (v2dr_out( nlev, slen_out ))
    ALLOCATE (ind_out( slen_out ))

    start_in_1d(1) = indarr(istart(is))
    count_in_1d(1) = slen_in
    start_in_2d(1) = 1
    start_in_2d(2) = indarr(istart(is))
    count_in_2d(1) = nlev
    count_in_2d(2) = slen_in
 
    ALLOCATE (v1di_in( slen_in ))
    ALLOCATE (v1dr_in( slen_in ))
    ALLOCATE (v2dr_in( nlev, slen_in ))

     DO i=1, ndims
      CALL check(NF_DEF_DIM(ocid, TRIM(dimname(i)),odimsize(i),dimid(i)))
     END DO

     DO i=1,nvars
       CALL check(NF_DEF_VAR (ocid, TRIM(vname(i)) , vtype(i), nvdims(i),  vdimid(i,1:nvdims(i)), ovarid(i) ) )
     END DO 

     CALL check( NF_ENDDEF(ocid) )

     ! get indices of non-missing based on missing values in hour 
     CALL check(NF_GET_VARA_INT(icid, ihid, start_in_1d, count_in_1d, v1di_in))
         ii=0
     DO ip=1,slen_in
       IF ( v1di_in(ip) .NE. imissing ) THEN
          ii=ii+1
          ind_out(ii)=ip
       END IF
     END DO

     var_loop_put: DO i=1,nvars 
        copied=.FALSE.
       IF ( vtype(i) .EQ. NF_INT .AND.  nvdims(i) .EQ. 1 ) THEN
          CALL check(NF_GET_VARA_INT(icid, i, start_in_1d, count_in_1d, v1di_in))  
         DO ii=1,slen_out
            v1di_out(ii)=v1di_in(ind_out(ii))
         END DO
           CALL check(NF_PUT_VAR_INT(ocid, ovarid(i), v1di_out))
          copied=.TRUE.
       END IF 
       IF ( vtype(i) .EQ. NF_REAL .AND.  nvdims(i) .EQ. 1 ) THEN
          CALL check(NF_GET_VARA_REAL(icid, i, start_in_1d, count_in_1d, v1dr_in))  
         DO ii=1,slen_out
            v1dr_out(ii)=v1dr_in(ind_out(ii))
         END DO
           CALL check(NF_PUT_VAR_REAL(ocid, ovarid(i), v1dr_out))
          copied=.TRUE.
       END IF 
       IF ( vtype(i) .EQ. NF_REAL .AND.  nvdims(i) .EQ. 2 ) THEN
           CALL check(NF_GET_VARA_REAL(icid, i, start_in_2d, count_in_2d, v2dr_in))
         DO ii=1,slen_out
            v2dr_out(:,ii)=v2dr_in(:,ind_out(ii))
         END DO
          CALL check(NF_PUT_VAR_REAL(ocid, ovarid(i), v2dr_out))
          copied=.TRUE.
       END IF 

       IF ( .NOT. copied ) THEN
        WRITE(6,*) "WARNING: variable not copied: ", vname(i)
       END IF 

     END DO var_loop_put

    DEALLOCATE(v1di_in, v1dr_in, v2dr_in)
    DEALLOCATE(v1di_out, v1dr_out, v2dr_out)
    DEALLOCATE(ind_out)

   !close output file
    CALL check( NF_CLOSE(ocid))

 END DO slice_loop


  DEALLOCATE(ovarid)
  DEALLOCATE(vname, vtype)  
  DEALLOCATE( vdimid)

 !close input file 
   CALL check( NF_CLOSE(icid))


END PROGRAM pp
