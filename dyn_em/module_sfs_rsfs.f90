






























































MODULE module_sfs_rsfs

CONTAINS



SUBROUTINE rec_0( urec, vrec, wrec, rcoef,             &
                  u, v, w, recl,                       &
                  ids, ide, jds, jde, kds, kde,        &
                  ims, ime, jms, jme, kms, kme,        &
                  ips, ipe, jps, jpe, kps, kpe,        &
                  its, ite, jts, jte, kts, kte         )






  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: urec        & 
   , vrec        & 
   , wrec          


  REAL, DIMENSION(0:10), INTENT( OUT ) &
  :: rcoef         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u           & 
   , v           & 
   , w             


  INTEGER,                                               INTENT( IN  ) &
  :: recl          

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k



  rcoef = 0.0

  SELECT CASE (recl)

    CASE (0)

      rcoef(0)=1.0D0

    CASE (1)

      rcoef(0)=2.0D0
      rcoef(1)=-1.0D0

    CASE (2)

      rcoef(0)=3.0D0
      rcoef(1)=-3.0D0
      rcoef(2)=1.0D0

    CASE (3)

      rcoef(0)=4.0D0
      rcoef(1)=-6.0D0
      rcoef(2)=4.0D0
      rcoef(3)=-1.0D0

    CASE (4)

      rcoef(0)=5.0D0
      rcoef(1)=-10.0D0
      rcoef(2)=10.0D0
      rcoef(3)=-5.0D0
      rcoef(4)=1.0D0

    CASE (5)

      rcoef(0)=6.0D0
      rcoef(1)=-15.0D0
      rcoef(2)=20.0D0
      rcoef(3)=-15.0D0
      rcoef(4)=6.0D0
      rcoef(5)=-1.0D0

    CASE DEFAULT

      WRITE(*,*)'Error-improperly specified reconstruction level in'
      WRITE(*,*)'dyn_em/module_reconstruct.F/subroutine rec_0'
      STOP

  END SELECT

  DO j = jts, jte   
  DO k = kts, kte
  DO i = its, ite
    urec(i,k,j) = rcoef(0)*u(i,k,j)
    vrec(i,k,j) = rcoef(0)*v(i,k,j)
    wrec(i,k,j) = rcoef(0)*w(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec_0



SUBROUTINE rec_0_tq ( trec, qrec, rcoef,               &
                      t, q,  recl,                     &
                      ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      ips, ipe, jps, jpe, kps, kpe,    &
                      its, ite, jts, jte, kts, kte     )






  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: trec        & 
   , qrec          

  REAL, DIMENSION(0:10), INTENT( OUT ) &
  :: rcoef         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: t           & 
   , q             

  INTEGER,                                               INTENT( IN  ) &
  :: recl          

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k



  rcoef = 0.0

  SELECT CASE (recl)

    CASE (0)

      rcoef(0)=1.0D0

    CASE (1)

      rcoef(0)=2.0D0
      rcoef(1)=-1.0D0

    CASE (2)

      rcoef(0)=3.0D0
      rcoef(1)=-3.0D0
      rcoef(2)=1.0D0

    CASE (3)

      rcoef(0)=4.0D0
      rcoef(1)=-6.0D0
      rcoef(2)=4.0D0
      rcoef(3)=-1.0D0

    CASE (4)

      rcoef(0)=5.0D0
      rcoef(1)=-10.0D0
      rcoef(2)=10.0D0
      rcoef(3)=-5.0D0
      rcoef(4)=1.0D0

    CASE (5)

      rcoef(0)=6.0D0
      rcoef(1)=-15.0D0
      rcoef(2)=20.0D0
      rcoef(3)=-15.0D0
      rcoef(4)=6.0D0
      rcoef(5)=-1.0D0

    CASE DEFAULT

      WRITE(*,*)'Error-improperly specified reconstruction level in'
      WRITE(*,*)'dyn_em/module_reconstruct.F/subroutine rec_0'
      STOP

  END SELECT

  DO j = jts, jte   
  DO k = kts, kte
  DO i = its, ite
    trec(i,k,j) = rcoef(0)*t(i,k,j)
    qrec(i,k,j) = rcoef(0)*q(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec_0_tq



SUBROUTINE rec( urec, vrec, wrec,             &
                ufilt, vfilt, wfilt, coef,    &
                ids, ide, jds, jde, kds, kde, &
                ims, ime, jms, jme, kms, kme, &
                ips, ipe, jps, jpe, kps, kpe, &
                its, ite, jts, jte, kts, kte  )






  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT(INOUT) &
  :: urec        & 
   , vrec        & 
   , wrec          

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT( IN  ) &
  :: ufilt       & 
   , vfilt       & 
   , wfilt         

  REAL,                                                INTENT( IN  ) &
  :: coef          

  INTEGER,                                             INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k



  DO j = jts, jte   
  DO k = kts, kte
  DO i = its, ite
    urec(i,k,j) = urec(i,k,j) + coef*ufilt(i,k,j)
    vrec(i,k,j) = vrec(i,k,j) + coef*vfilt(i,k,j)
    wrec(i,k,j) = wrec(i,k,j) + coef*wfilt(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec



SUBROUTINE rec_tq ( trec, qrec,                               &
                    tfilt, qfilt, coef,                       &
                    ids, ide, jds, jde, kds, kde,             &
                    ims, ime, jms, jme, kms, kme,             &
                    ips, ipe, jps, jpe, kps, kpe,             &
                    its, ite, jts, jte, kts, kte              )






  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT(INOUT) &
  :: trec        &  
   , qrec           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT( IN  ) &
  :: tfilt       & 
   , qfilt         

  REAL,                                                INTENT( IN  ) &
  :: coef          

  INTEGER,                                             INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k



  DO j = jts, jte   
  DO k = kts, kte
  DO i = its, ite
    trec(i,k,j) = trec(i,k,j) + coef*tfilt(i,k,j)
    qrec(i,k,j) = qrec(i,k,j) + coef*qfilt(i,k,j)
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE rec_tq



SUBROUTINE uvw_s( us, vs, ws,                   &
                  u, v,                         &
                  ht, rdx, rdy,                 &
                  cf1, cf2, cf3,                &
                  rsfsvsbc,                     &
                  ids, ide, jds, jde, kds, kde, &
                  ims, ime, jms, jme, kms, kme, &
                  ips, ipe, jps, jpe, kps, kpe, &
                  its, ite, jts, jte, kts, kte  )

















  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: us          & 
   , vs          & 
   , ws            

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: u           & 
   , v             


  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: ht            

  INTEGER,                                               INTENT( IN  ) &
  :: rsfsvsbc

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  REAL,                                                  INTENT( IN  ) &
  :: rdx         & 
    ,rdy         & 
    ,cf1         & 
    ,cf2         & 
    ,cf3           
 


  INTEGER:: i, j



  IF ( rsfsvsbc .EQ. 0 ) THEN 

    DO j=jts-1, jte+1 
    DO i=its-1, ite+1
      us(i,j) = cf1*u(i,1,j)+cf2*u(i,2,j)+cf3*u(i,3,j) 
      vs(i,j) = cf1*v(i,1,j)+cf2*v(i,2,j)+cf3*v(i,3,j)
    ENDDO
    ENDDO

    DO j=jts, jte 
    DO i=its, ite
      ws(i,j)=                         &
                0.5*rdy*(              &
                (ht(i,j+1)-ht(i,j  ))  &
                *(vs(i,j+1))           &
                +(ht(i,j  )-ht(i,j-1)) &
                *(vs(i,j  ))  )        &
                +.5*rdx*(              &
                (ht(i+1,j)-ht(i,j  ))  &
                *(us(i+1,j))           &
                +(ht(i,j  )-ht(i-1,j)) &
                *(us(i,j))  )   
    ENDDO
    ENDDO

  ELSEIF ( rsfsvsbc .EQ. 1 ) THEN 

  DO j=jts-1, jte+1
  DO i=its-1, ite+1
    us(i,j) = u(i,1,j)
    vs(i,j) = v(i,1,j)
  ENDDO
  ENDDO

  DO j=jts, jte
  DO i=its, ite
    ws(i,j) =                        &
              0.5*rdy*(              &
              (ht(i,j+1)-ht(i,j  ))  &
              *(vs(i,j+1))           &
              +(ht(i,j  )-ht(i,j-1)) &
              *(vs(i,j  ))  )        &
              +.5*rdx*(              &
              (ht(i+1,j)-ht(i,j  ))  &
              *(us(i+1,j))           &
              +(ht(i,j  )-ht(i-1,j)) &
              *(us(i,j))  )
  ENDDO
  ENDDO

  ELSEIF ( rsfsvsbc .EQ. 2 ) THEN 

    DO j=jts, jte
    DO i=its, ite
      us(i,j) = 0.0
      vs(i,j) = 0.0
      ws(i,j) = 0.0  
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE uvw_s



SUBROUTINE tq_s(  ts, qs,                       &
                  t, q,                         &
                  ht, rdx, rdy,                 &
                  cf1, cf2, cf3,                &
                  rsfsvsbc,                     &
                  ids, ide, jds, jde, kds, kde, &
                  ims, ime, jms, jme, kms, kme, &
                  ips, ipe, jps, jpe, kps, kpe, &
                  its, ite, jts, jte, kts, kte  )

















  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: ts          & 
   , qs            

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: t           & 
   , q             

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: ht            

  INTEGER,                                               INTENT( IN  ) &
  :: rsfsvsbc

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte

  REAL,                                                  INTENT( IN  ) &
  :: rdx         & 
    ,rdy         & 
    ,cf1         & 
    ,cf2         & 
    ,cf3           
 


  INTEGER:: i, j



  IF ( rsfsvsbc .EQ. 0 ) THEN 

    DO j=jts-1, jte+1 
    DO i=its-1, ite+1
      ts(i,j) = cf1*t(i,1,j)+cf2*t(i,2,j)+cf3*t(i,3,j) 
      qs(i,j) = cf1*q(i,1,j)+cf2*q(i,2,j)+cf3*q(i,3,j)
    ENDDO
    ENDDO

  ELSEIF ( rsfsvsbc .EQ. 1 ) THEN 

  DO j=jts-1, jte+1
  DO i=its-1, ite+1
    ts(i,j) = t(i,1,j)
    qs(i,j) = q(i,1,j)
  ENDDO
  ENDDO

  ELSEIF ( rsfsvsbc .EQ. 2 ) THEN 

    DO j=jts, jte
    DO i=its, ite
      ts(i,j) = 0.0
      qs(i,j) = 0.0  
    ENDDO
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE tq_s



SUBROUTINE update_w1( w, ws,                        &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      ips, ipe, jps, jpe, kps, kpe, &
                      its, ite, jts, jte, kts, kte  )






  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT(INOUT) &
  :: w             

  REAL, DIMENSION( ims:ime, jms:jme ),                 INTENT( IN  ) &
  :: ws            

  INTEGER,                                             INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j



  DO j=jts, jte
  DO i=its, ite
    w(i,kts,j) = ws(i,j)
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE update_w1



SUBROUTINE proj_s( usc, vsc,           &
                   uusc, vvsc, wws,    &
                   usd, vsd, uvsd,     &
                   wse, uwse,          &
                   wsf, vwsf,          &
                   us, vs, ws,         &
                   ids, ide, jds, jde, &
                   ims, ime, jms, jme, &
                   ips, ipe, jps, jpe, &
                   its, ite, jts, jte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: us          & 
   , vs          & 
   , ws          

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: usc         & 
   , vsc         & 
   , uusc        & 
   , vvsc        & 
   , wws         & 
   , usd         & 
   , vsd         & 
   , uvsd        & 
   , wse         & 
   , uwse        & 
   , wsf         & 
   , vwsf          

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, &
     ims, ime, jms, jme, &
     ips, ipe, jps, jpe, &
     its, ite, jts, jte



  INTEGER:: i, j





  DO j=jts, jte
  DO i=its, ite
    usc(i,j) = 0.5*( us(i+1,j) + us(i,j) )
    vsc(i,j) = 0.5*( vs(i,j+1) + vs(i,j) )
    uusc(i,j) = usc(i,j)*usc(i,j)
    vvsc(i,j) = vsc(i,j)*vsc(i,j)
    usd(i,j) = 0.5*( us(i,j-1) + us(i,j) )
    vsd(i,j) = 0.5*( vs(i-1,j) + vs(i,j) )
    uvsd(i,j) = usd(i,j)*vsd(i,j)
    wse(i,j) = 0.5*( ws(i-1,j) + ws(i,j) )
    uwse(i,j) =us(i,j)*wse(i,j)
    wsf(i,j) = 0.5*( ws(i,j-1) + ws(i,j) )
    vwsf(i,j) =vs(i,j)*wsf(i,j)
    wws(i,j) = ws(i,j)*ws(i,j)
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_s



SUBROUTINE proj_s_tq ( tsu, tsv,           &
                       utsu, vtsv, wtsw,   &
                       qsu, qsv,           &
                       uqsu, vqsv, wqsw,   &
                       us, vs, ws, ts, qs, &
                       ids, ide, jds, jde, &
                       ims, ime, jms, jme, &
                       ips, ipe, jps, jpe, &
                       its, ite, jts, jte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: us          & 
   , vs          & 
   , ws          & 
   , ts          & 
   , qs            

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( OUT ) &
  :: tsu         & 
   , tsv         & 
   , utsu        & 
   , vtsv        & 
   , wtsw        & 
   , qsu         & 
   , qsv         & 
   , uqsu        & 
   , vqsv        & 
   , wqsw          


  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, &
     ims, ime, jms, jme, &
     ips, ipe, jps, jpe, &
     its, ite, jts, jte



  INTEGER:: i, j





  DO j=jts, jte
  DO i=its, ite

    tsu(i,j) = 0.5*( ts(i-1,j) + ts(i,j) )     
    tsv(i,j) = 0.5*( ts(i,j-1) + ts(i,j) )                     
    utsu(i,j) = us(i,j)*tsu(i,j)       
    vtsv(i,j) = vs(i,j)*tsv(i,j)         
    wtsw(i,j) = ws(i,j)*ts(i,j)      

    qsu(i,j) = 0.5*( qs(i-1,j) + qs(i,j) )     
    qsv(i,j) = 0.5*( qs(i,j-1) + qs(i,j) )                     
    uqsu(i,j) = us(i,j)*qsu(i,j)       
    vqsv(i,j) = vs(i,j)*qsv(i,j)         
    wqsw(i,j) = ws(i,j)*qs(i,j)      

  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_s_tq



SUBROUTINE proj_c( var1c, var2c, var3c,           &
                   var4c, var5c, var6c,           &
                   var1, var2, var3, variable,    &
                   ids, ide, jds, jde, kds, kde,  &
                   ims, ime, jms, jme, kms, kme,  &
                   ips, ipe, jps, jpe, kps, kpe,  &
                   its, ite, jts, jte, kts, kte   )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: var1c       & 
   , var2c       & 
   , var3c       & 
   , var4c       & 
   , var5c       & 
   , var6c         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & 
   , var2        & 
   , var3          

  CHARACTER, INTENT( IN ) :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k, kend



  SELECT CASE (variable)

    CASE ('u') 

  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite

    var1c(i,k,j) = 0.5*( var1(i+1,k,j) + var1(i,k,j) )
    var2c(i,k,j) = 0.5*( var2(i,k,j+1) + var2(i,k,j) )
    var3c(i,k,j) = 0.5*( var3(i,k+1,j) + var3(i,k,j) )
    var4c(i,k,j) = var1c(i,k,j)*var1c(i,k,j)
    var5c(i,k,j) = var2c(i,k,j)*var2c(i,k,j)
    var6c(i,k,j) = var3c(i,k,j)*var3c(i,k,j)

  ENDDO
  ENDDO
  ENDDO

    CASE ('d') 
   
      kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite

        var1c(i,k,j)=0.125*(   var1(i+1,k,j)   + var1(i,k,j)   &   
                            + var1(i+1,k,j+1) + var1(i,k,j+1) )

        var2c(i,k,j)=0.125*(   var2(i+1,k,j)   + var2(i,k,j)   &  
                            + var2(i+1,k+1,j) + var2(i,k+1,j) )

        var3c(i,k,j)=0.125*(   var3(i,k,j+1) + var3(i,k,j)     & 
                            + var3(i,k+1,j+1) + var3(i,k+1,j) )

      ENDDO
      ENDDO
      ENDDO

    CASE ('c') 
   
      kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite

        var1c(i,k,j)=0.5*var1(i,k,j)

        var2c(i,k,j)=0.5*var2(i,k,j) 

        var3c(i,k,j)=0.5*var3(i,k,j)
 
      ENDDO
      ENDDO
      ENDDO

    CASE ('h') 
   
      kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
        var1c(i,k,j)=0.25*(   var1(i+1,k,j)   + var1(i,k,j)   &   
                            + var1(i+1,k,j+1) + var1(i,k,j+1) )

        var2c(i,k,j)=0.25*(   var2(i+1,k,j)   + var2(i,k,j)   &  
                            + var2(i+1,k+1,j) + var2(i,k+1,j) )

        var3c(i,k,j)=0.25*(   var3(i,k,j+1) + var3(i,k,j)     & 
                            + var3(i,k+1,j+1) + var3(i,k+1,j) )
      ENDDO
      ENDDO
      ENDDO


    CASE DEFAULT
 
  END SELECT     

  RETURN

END SUBROUTINE proj_c



SUBROUTINE proj_d( var1d, var2d, var3d,          &
                   var1, var2,                   &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe, &
                   its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: var1d       & 
   , var3d       & 
   , var2d         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & 
   , var2          

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k, kend



  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
    var1d(i,k,j) = 0.5*( var1(i,k,j-1) + var1(i,k,j) )
    var2d(i,k,j) = 0.5*( var2(i-1,k,j) + var2(i,k,j) )
    var3d(i,k,j) =var1d(i,k,j)*var2d(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_d



SUBROUTINE proj_e( var1e, var2e, var3e,          &
                   var1, var2, fnm, fnp,         &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe, &
                   its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: var1e       & 
   , var2e       & 
   , var3e         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & 
   , var2          

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & 
   , fnp

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k, kend



  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    var1e(i,k,j) = fnm(k)*var1(i,k,j) + fnp(k)*var1(i,k-1,j)
    var2e(i,k,j) = 0.5*( var2(i-1,k,j) + var2(i,k,j) )
    var3e(i,k,j) = var1e(i,k,j)*var2e(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_e



SUBROUTINE proj_f( var1f, var2f, var3f,          &
                   var1, var2, fnm, fnp,         &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe, &
                   its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: var1f       & 
   , var2f       & 
   , var3f         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & 
   , var2          

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & 
   , fnp

 INTEGER,                                               INTENT( IN  ) &
 :: ids, ide, jds, jde, kds, kde, &
    ims, ime, jms, jme, kms, kme, &
    ips, ipe, jps, jpe, kps, kpe, &
    its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k, kend



  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    var1f(i,k,j) = fnm(k)*var1(i,k,j) + fnp(k)*var1(i,k-1,j)
    var2f(i,k,j) = 0.5*( var2(i,k,j-1) + var2(i,k,j) )
    var3f(i,k,j) = var1f(i,k,j)*var2f(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE proj_f



SUBROUTINE proj_tuvw( tatu, tatv, tatw,             &
                     utatu, vtatv, wtatw,          &
                     urec, vrec, wrec, trec,       &
                     fnm, fnp,                     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: tatu        & 
   , tatv        & 
   , tatw        & 
   , utatu       & 
   , vtatv       & 
   , wtatw         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: urec        & 
   , vrec        & 
   , wrec        & 
   , trec          

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & 
   , fnp

 INTEGER,                                               INTENT( IN  ) &
 :: ids, ide, jds, jde, kds, kde, &
    ims, ime, jms, jme, kms, kme, &
    ips, ipe, jps, jpe, kps, kpe, &
    its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k, kend



  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
    tatu(i,k,j) = 0.5*( trec(i-1,k,j) + trec(i,k,j) )
    tatv(i,k,j) = 0.5*( trec(i,k,j-1) + trec(i,k,j) )
    utatu(i,k,j) = urec(i,k,j)*tatu(i,k,j)
    vtatv(i,k,j) = vrec(i,k,j)*tatv(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    tatw(i,k,j) = fnm(k)*trec(i,k,j) + fnp(k)*trec(i,k-1,j)
    wtatw(i,k,j) = wrec(i,k,j)* tatw(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO i = its, ite
    tatw(i,kts,j) = 0.0
    wtatw(i,kts,j) = 0.0
  ENDDO
  ENDDO


  RETURN

END SUBROUTINE proj_tuvw

SUBROUTINE proj_quvw( tatu, tatv, tatw,             &
                     utatu, vtatv, wtatw,          &
                     urec, vrec, wrec, trec,       &
                     fnm, fnp,                     &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     ips, ipe, jps, jpe, kps, kpe, &
                     its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT(INOUT) &
  :: tatu        & 
   , tatv        & 
   , tatw        & 
   , utatu       & 
   , vtatv       & 
   , wtatw         

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: urec        & 
   , vrec        & 
   , wrec        & 
   , trec          

  REAL, DIMENSION( kms:kme ),                            INTENT( IN  ) &
  :: fnm         & 
   , fnp

 INTEGER,                                               INTENT( IN  ) &
 :: ids, ide, jds, jde, kds, kde, &
    ims, ime, jms, jme, kms, kme, &
    ips, ipe, jps, jpe, kps, kpe, &
    its, ite, jts, jte, kts, kte



  INTEGER:: i, j, k, kend



  kend = MIN( kte, kde )-1

  DO j = jts, jte
  DO k = kts, kend
  DO i = its, ite
    tatu(i,k,j) = 0.5*( trec(i-1,k,j) + trec(i,k,j) )
    tatv(i,k,j) = 0.5*( trec(i,k,j-1) + trec(i,k,j) )
    utatu(i,k,j) = urec(i,k,j)*tatu(i,k,j)
    vtatv(i,k,j) = vrec(i,k,j)*tatv(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO k = kts+1, kend
  DO i = its, ite
    tatw(i,k,j) = fnm(k)*trec(i,k,j) + fnp(k)*trec(i,k-1,j)
    wtatw(i,k,j) = wrec(i,k,j)* tatw(i,k,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = jts, jte
  DO i = its, ite
    tatw(i,kts,j) = 0.0
    wtatw(i,kts,j) = 0.0
  ENDDO
  ENDDO


  RETURN

END SUBROUTINE proj_quvw



SUBROUTINE rsfs_filter( varfilt, var, vars, variable, ifilt, &
                        ids, ide, jds, jde, kds, kde,        &
                        ims, ime, jms, jme, kms, kme,        &
                        ips, ipe, jps, jpe, kps, kpe,        &
                        its, ite, jts, jte, kts, kte         )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: varfilt       

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var           

  REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( IN  ) &
  :: vars          

  CHARACTER(LEN=2),                                      INTENT( IN  ) &
  :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ifilt

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  REAL, DIMENSION( its-1:ite+1, kts-1:kte+1, jts-1:jte+1 ) &
  :: fx         &  
   , fxy        &  
   , vart          

  REAL :: a, b, c, du

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend





  IF ( ifilt .EQ. 1 ) THEN      
    a = 0.25
    b = 0.5
    c = 0.25
  ELSEIF ( ifilt .EQ. 2 ) THEN  
    a = 1.0/6.0
    b = 4.0/6.0
    c = 1.0/6.0
  ELSEIF ( ifilt .EQ. 3 ) THEN 
    a = 0.5                    
    b = 0.0                    
    c = 0.5
  ELSE
    WRITE(*,*) 'Invalid filter type, ifilt=', ifilt
    STOP  
  ENDIF

  SELECT CASE (variable)


    CASE ('uu') 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('vv') 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) =  vart(i,kend-1,j)                  


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('ww') 

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                


          vart(i,kbgn,j) = vars(i,j)                          

      END DO
      END DO



    CASE ('cw') 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                 


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('cc') 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      ENDDO
      ENDDO


    CASE ('hc') 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1

        vart(i,kend,j) = 2.0*vart(i,kend-1,j)-vart(i,kend-2,j)     


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)          
      ENDDO
      ENDDO


    CASE ('dd') 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   


        vart(i,kbgn,j) = 2.0*vars(i,j) - vart(i,kbgn+1,j)   
      END DO
      END DO


    CASE ('ee') 

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('ew') 

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('ff') 

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = vart(i,kend-1,j)                   


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('fw') 

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                


        vart(i,kbgn,j) = vars(i,j)                          
      END DO
      END DO



    CASE ('sh') 

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1

        vart(i,kend,j) =  vart(i,kend-1,j)                    

        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  
      ENDDO
      ENDDO


    CASE ('sv') 

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                  


        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  
      ENDDO
      ENDDO


    CASE ('tc') 

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                  


        vart(i,kbgn,j) = vart(i,kbgn+1,j)                  
      ENDDO
      ENDDO


    CASE ('td') 

      kbgn = kts-1
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = - vart(i,kend-1,j)                  


        vart(i,kbgn,j) = vart(i,kbgn+1,j)                    
      ENDDO
      ENDDO


    CASE ('te') 

      kbgn = kts
      kend = MIN( kte, kde )

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                 

        vart(i,kbgn,j) = vart(i,kbgn+1,j) 
      ENDDO
      ENDDO


    CASE ('tf') 

      kbgn = kts
      kend = MIN( kte, kde )  

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO   
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1
        vart(i,kend,j) = 0.0                                 

        vart(i,kbgn,j) = vart(i,kbgn+1,j) 
      ENDDO
      ENDDO


    CASE ('kc') 

      kbgn = kts-1    
      kend = MIN( kte, kde ) 

      DO j = jts-1, jte+1
      DO k = kbgn+1, kend-1
      DO i = its-1, ite+1
        vart(i,k,j) = var(i,k,j)
      END DO
      END DO
      END DO

      DO j = jts-1, jte+1
      DO i = its-1, ite+1

        vart(i,kend,j) =   vart(i,kend-1,j)                  
        vart(i,kbgn,j) =  vart(i,kbgn+1,j)                 
      ENDDO
      ENDDO



    CASE DEFAULT
 
      WRITE(*,*)'Error-improperly specified variable in'
      WRITE(*,*)'dyn_em/module_reconstruct.F/subroutine drm_filter'
      STOP

  END SELECT




  DO j = jts-1, jte+1
  DO k = kbgn, kend
  DO i = its, ite
    fx(i,k,j) = a*vart(i+1,k,j) + b*vart(i,k,j) + c*vart(i-1,k,j)
  ENDDO
  ENDDO
  ENDDO


  DO j = jts, jte
  DO k = kbgn, kend
  DO i = its, ite
    fxy(i,k,j) = a*fx(i,k,j+1) + b*fx(i,k,j) + c*fx(i,k,j-1)
  END DO
  END DO
  ENDDO


  DO j = jts, jte
  DO k = kbgn+1, kend-1
  DO i = its, ite
    varfilt(i,k,j) = a*fxy(i,k+1,j) + b*fxy(i,k,j) + c*fxy(i,k-1,j)
  ENDDO
  ENDDO
  ENDDO

  RETURN

END SUBROUTINE rsfs_filter



SUBROUTINE rmij( tau, var1, var2, var3, variable, &
                 ids, ide, jds, jde, kds, kde,    &
                 ims, ime, jms, jme, kms, kme,    &
                 ips, ipe, jps, jpe, kps, kpe,    &
                 its, ite, jts, jte, kts, kte     )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( OUT ) &
  :: tau           

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN  ) &
  :: var1        & 
   , var2        & 
   , var3          

  CHARACTER,                                             INTENT( IN  ) &
  :: variable

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



   REAL, DIMENSION( kms:kme ) &
  :: fac    

  INTEGER:: i, j, k, ibgn, iend, jbgn, jend, kbgn, kend



  kbgn = kts
  IF ( (variable .EQ. 'e') .OR. (variable .EQ. 'f') .OR. (variable .EQ. 'w') ) kbgn = kts+1

  DO j = jts, jte  
  DO i = its, ite
    tau(i,kte,j) = 0.0
  ENDDO
  ENDDO



      DO j = jts, jte
      DO k =  kbgn, kte-1
      DO i = its, ite
        tau(i,k,j) = var3(i,k,j) - var1(i,k,j)*var2(i,k,j) 
      ENDDO
      ENDDO
      ENDDO

  IF ( variable .EQ. 'h' ) THEN

  kbgn = kts+1 

  DO j = jts, jte  
  DO i = its, ite
    tau(i,kte,j) = 0.0
    tau(i,kte-1,j) = 0.0
  ENDDO
  ENDDO



      DO j = jts, jte
      DO k =  kbgn, kte-2
      DO i = its, ite
        tau(i,k,j) = var3(i,k,j) - var1(i,k,j)*var2(i,k,j) 
      ENDDO
      ENDDO
      ENDDO

ENDIF

  RETURN

END SUBROUTINE rmij



SUBROUTINE zero_rsfs( var11, var22, var33,          &
                      var12, var13, var23,          &
                      ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      ips, ipe, jps, jpe, kps, kpe, &
                      its, ite, jts, jte, kts, kte  )





  IMPLICIT NONE

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),        INTENT( INOUT ) &
  :: var11    & 
   , var22    & 
   , var33    & 
   , var12    & 
   , var13    & 
   , var23      

  INTEGER,                                               INTENT( IN  ) &
  :: ids, ide, jds, jde, kds, kde, &
     ims, ime, jms, jme, kms, kme, &
     ips, ipe, jps, jpe, kps, kpe, &
     its, ite, jts, jte, kts, kte



  INTEGER :: i, j, k



  DO j = jts-1, jte+1
  DO k = kts, kte
  DO i = its-1, ite+1
    var11(i,k,j) = 0.0
    var22(i,k,j) = 0.0
    var33(i,k,j) = 0.0
    var12(i,k,j) = 0.0
    var13(i,k,j) = 0.0
    var23(i,k,j) = 0.0
  END DO
  END DO
  END DO

  RETURN

END SUBROUTINE zero_rsfs



 END MODULE module_sfs_rsfs
