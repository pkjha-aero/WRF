























MODULE module_meso_tend_micro_force

  USE MODULE_DM

CONTAINS








SUBROUTINE initialize_meso_forcing( m_tend_u_adv,  m_tend_v_adv,      &
                                    m_tend_u_pgf,  m_tend_v_pgf,      & 
                                    m_tend_th, m_tend_z,              & 
                                    m_valu_hfx, m_valu_tsk, m_tend_t, &
                                    n_meso_t_levs, n_meso_z_levs      ) 
                              

   IMPLICIT NONE

   REAL , DIMENSION( 1:n_meso_t_levs, 1:n_meso_z_levs ), INTENT(INOUT) :: m_tend_u_adv, &
                                                                          m_tend_v_adv, &
                                                                          m_tend_u_pgf, &
                                                                          m_tend_v_pgf, &
                                                                          m_tend_th,    &
                                                                          m_tend_z

   REAL , DIMENSION( 1:n_meso_t_levs ),                  INTENT(INOUT) :: m_valu_hfx, &
                                                                          m_valu_tsk, &  
                                                                          m_tend_t


   INTEGER,                                              INTENT( IN  ) :: n_meso_t_levs, &   
                                                                          n_meso_z_levs




   INTEGER :: i, j, k, n, i_start,i_end,j_start,j_end
   INTEGER :: nz_meso, nt_meso
                                












OPEN(19, FILE='june2018_meso_tends_for_micro_forcing_5ptsmooth_time0.dat',FORM='FORMATTED',STATUS='OLD')

    READ (19,*) nt_meso
    READ (19,*) nz_meso


     print*,'nt_meso,nz_meso ',nt_meso,nz_meso

  IF ( ( nt_meso .NE. n_meso_t_levs ) .OR. ( nz_meso .NE. n_meso_z_levs ) ) THEN

     print*,'Something wrong with the meso forcing data'
     print*,'t levels specified in namelist ',n_meso_t_levs
     print*,'t levels in file ',nt_meso
     print*,'z levels specified in namelist ',n_meso_z_levs
     print*,'z levels in file ',nz_meso

  ENDIF 

  DO n=1,n_meso_t_levs

    READ (19,*) m_tend_t(n)
    READ (19,*) m_valu_hfx(n),m_valu_tsk(n)

       print*,n,m_tend_t(n)

    DO k=1,n_meso_z_levs

       READ (19,*) m_tend_z(n,k),m_tend_u_adv(n,k),m_tend_v_adv(n,k),m_tend_u_pgf(n,k),m_tend_v_pgf(n,k),m_tend_th(n,k)



   ENDDO

ENDDO

CLOSE (19)



   RETURN

END SUBROUTINE initialize_meso_forcing


SUBROUTINE apply_meso_tends ( ru_tendf, rv_tendf, rt_tendf,     &
                              u_base, v_base, z_base,           &
                              m_tend_u_adv,  m_tend_v_adv,      &
                              m_tend_u_pgf,  m_tend_v_pgf,      &
                              m_tend_th, m_tend_z, m_tend_t,    & 
                              meso_force,                       &
                              n_meso_t_levs, n_meso_z_levs,     &
                              rdz, mu, xtime, dt,               &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              its, ite, jts, jte, kts, kte      )

   IMPLICIT NONE

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme),      INTENT(INOUT) :: ru_tendf, &
                                                                       rv_tendf, &
                                                                       rt_tendf  

   REAL , DIMENSION( kms:kme),                        INTENT(INOUT) :: u_base, &
                                                                       v_base

   REAL , DIMENSION( kms:kme),                           INTENT(IN) :: z_base

   REAL , DIMENSION( 1:n_meso_t_levs, 1:n_meso_z_levs ), INTENT(IN) :: m_tend_u_adv, &
                                                                       m_tend_v_adv, &
                                                                       m_tend_u_pgf, &
                                                                       m_tend_v_pgf, &
                                                                       m_tend_th,    &
                                                                       m_tend_z

   REAL , DIMENSION( 1:n_meso_t_levs ),                  INTENT(IN) :: m_tend_t

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme),         INTENT(IN) :: rdz
                                                                      

   REAL , DIMENSION( ims:ime , jms:jme ),                INTENT(IN) ::  mu

   REAL ,                                                INTENT(IN) :: xtime
   REAL ,                                                INTENT(IN) :: dt

   INTEGER,                                              INTENT(IN) :: meso_force,    &  
                                                                       n_meso_t_levs, &  
                                                                       n_meso_z_levs

   INTEGER , INTENT( IN  ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte



   REAL , DIMENSION( n_meso_z_levs ) :: m_tend_z_int

   INTEGER :: i, j, k, k_meso, n, t, i_start, i_end, j_start, j_end
   REAL :: fac_t, fac_z, zm_u, zm_v
 
   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1) :: zm
                                       
   REAL :: mtenduadv, mtendvadv, mtendupgf, mtendvpgf, mtendth               
                             








   IF ( ( xtime .GE. m_tend_t(1) ) .AND. ( xtime .LE. m_tend_t(n_meso_t_levs) ) ) THEN

      DO j = jts-1, jte+1
      DO i = its-1, ite+1

         zm(i,kts,j) = 1.0/rdz(i,kts,j)

         DO k = kts+1,MIN(kte,kde-1) 

            zm(i,k,j) =  zm(i,k-1,j) + 1.0/rdz(i,k,j)

         END DO

      END DO
      END DO



      DO n = 1, n_meso_t_levs-1
         IF ( ( xtime .GE. m_tend_t(n) ) .AND. ( xtime .LT. m_tend_t(n+1) ) ) t = n
      END DO



      fac_t = (xtime - m_tend_t(t))/(m_tend_t(t+1)-m_tend_t(t))



      DO k = 1, n_meso_z_levs
         m_tend_z_int(k) = (1.0-fac_t)*m_tend_z(t,k) + fac_t*m_tend_z(t+1,k)
      END DO









      DO j = jts, jte
      DO i = its, ite

         DO k = kts,MIN(kte,kde-1)  



            IF ( zm(i,k,j) .LE. m_tend_z_int(1) ) THEN 

               mtendth = (1.0-fac_t)*m_tend_th(t,1) + fac_t*m_tend_th(t+1,1)











            ELSE IF ( zm(i,k,j) .GE. m_tend_z_int(n_meso_z_levs) ) THEN

               mtendth = (1.0-fac_t)*m_tend_th(t,n_meso_z_levs) + fac_t*m_tend_th(t+1,n_meso_z_levs)







                     
            ELSE



               DO k_meso = 1, n_meso_z_levs-1

                  IF ( ( zm(i,k,j) .GE. m_tend_z_int(k_meso) ) .AND. ( zm(i,k,j) .LT. m_tend_z_int(k_meso+1) ) ) THEN 

                     fac_z = (zm(i,k,j) - m_tend_z_int(k_meso))/(m_tend_z_int(k_meso+1) - m_tend_z_int(k_meso) )

                     mtendth = (1.0-fac_z)*( (1.0-fac_t)*m_tend_th(t,k_meso) + fac_t*m_tend_th(t+1,k_meso) ) &
                                   + fac_z*( (1.0-fac_t)*m_tend_th(t,k_meso+1) + fac_t*m_tend_th(t+1,k_meso+1) )









                  ENDIF

               END DO

            ENDIF

            rt_tendf(i,k,j)=rt_tendf(i,k,j)+mu(i,j)*mtendth

         END DO



         DO k = kts,MIN(kte,kde-1)  

            zm_u = 0.5*( zm(i-1,k,j) + zm(i,k,j) )

            IF ( zm_u .LE. m_tend_z_int(1) ) THEN 

               mtenduadv = (1.0-fac_t)*m_tend_u_adv(t,1) + fac_t*m_tend_u_adv(t+1,1)

            ELSE IF ( zm_u .GE. m_tend_z_int(n_meso_z_levs) ) THEN 

               mtenduadv = (1.0-fac_t)*m_tend_u_adv(t,n_meso_z_levs) + fac_t*m_tend_u_adv(t+1,n_meso_z_levs)

            ELSE

               DO k_meso = 1, n_meso_z_levs-1

                  IF ( ( zm_u .GE. m_tend_z_int(k_meso) ) .AND. ( zm_u .LT. m_tend_z_int(k_meso+1) ) ) THEN

                     fac_z = (zm_u - m_tend_z_int(k_meso))/(m_tend_z_int(k_meso+1) - m_tend_z_int(k_meso) )

                     mtenduadv = (1.0-fac_z)*( (1.0-fac_t)*m_tend_u_adv(t,k_meso) + fac_t*m_tend_u_adv(t+1,k_meso) ) &
                                     + fac_z*( (1.0-fac_t)*m_tend_u_adv(t,k_meso+1) + fac_t*m_tend_u_adv(t+1,k_meso+1) )

                  ENDIF

               END DO

            ENDIF

            ru_tendf(i,k,j)=ru_tendf(i,k,j)+0.5*(mu(i,j)+mu(i-1,j))*mtenduadv   
        
         END DO



         DO k = kts,MIN(kte,kde-1)  

            zm_v = 0.5*( zm(i,k,j-1) + zm(i,k,j) )

            IF ( zm_v .LE. m_tend_z_int(1) ) THEN 

               mtendvadv = (1.0-fac_t)*m_tend_v_adv(t,1) + fac_t*m_tend_v_adv(t+1,1)

            ELSE IF ( zm_v .GE. m_tend_z_int(n_meso_z_levs) ) THEN 

               mtendvadv = (1.0-fac_t)*m_tend_v_adv(t,n_meso_z_levs) + fac_t*m_tend_v_adv(t+1,n_meso_z_levs)

            ELSE

               DO k_meso = 1, n_meso_z_levs-1

                  IF ( ( zm_v .GE. m_tend_z_int(k_meso) ) .AND. ( zm_v .LT. m_tend_z_int(k_meso+1) ) ) THEN

                     fac_z = (zm_v - m_tend_z_int(k_meso))/(m_tend_z_int(k_meso+1) - m_tend_z_int(k_meso) )

                     mtendvadv = (1.0-fac_z)*( (1.0-fac_t)*m_tend_v_adv(t,k_meso) + fac_t*m_tend_v_adv(t+1,k_meso) ) &
                                     + fac_z*( (1.0-fac_t)*m_tend_v_adv(t,k_meso+1) + fac_t*m_tend_v_adv(t+1,k_meso+1) )

                  ENDIF

               END DO

            ENDIF

            rv_tendf(i,k,j)=rv_tendf(i,k,j)+0.5*(mu(i,j)+mu(i,j-1))*mtendvadv

         END DO

      END DO
      END DO



      DO k = kts,MIN(kte,kde-1)  

            IF ( z_base(k) .LE. m_tend_z_int(1) ) THEN 

               mtendupgf = (1.0-fac_t)*m_tend_u_pgf(t,1) + fac_t*m_tend_u_pgf(t+1,1)
               mtendvpgf = (1.0-fac_t)*m_tend_v_pgf(t,1) + fac_t*m_tend_v_pgf(t+1,1)

            ELSE IF ( z_base(k) .GE. m_tend_z_int(n_meso_z_levs) ) THEN 

               mtendupgf = (1.0-fac_t)*m_tend_u_pgf(t,n_meso_z_levs) + fac_t*m_tend_u_pgf(t+1,n_meso_z_levs)
               mtendvpgf = (1.0-fac_t)*m_tend_v_pgf(t,n_meso_z_levs) + fac_t*m_tend_v_pgf(t+1,n_meso_z_levs)

            ELSE

               DO k_meso = 1, n_meso_z_levs-1

                  IF ( ( z_base(k) .GE. m_tend_z_int(k_meso) ) .AND. ( z_base(k) .LT. m_tend_z_int(k_meso+1) ) ) THEN

                     fac_z = (z_base(k) - m_tend_z_int(k_meso))/(m_tend_z_int(k_meso+1) - m_tend_z_int(k_meso) )

                     mtendupgf = (1.0-fac_z)*( (1.0-fac_t)*m_tend_u_pgf(t,k_meso) + fac_t*m_tend_u_pgf(t+1,k_meso) ) &
                                     + fac_z*( (1.0-fac_t)*m_tend_u_pgf(t,k_meso+1) + fac_t*m_tend_u_pgf(t+1,k_meso+1) )

                     mtendvpgf = (1.0-fac_z)*( (1.0-fac_t)*m_tend_v_pgf(t,k_meso) + fac_t*m_tend_v_pgf(t+1,k_meso) ) &
                                     + fac_z*( (1.0-fac_t)*m_tend_v_pgf(t,k_meso+1) + fac_t*m_tend_v_pgf(t+1,k_meso+1) )

                  ENDIF

               END DO

            ENDIF

            u_base(k) =  mtendupgf
            v_base(k) =  mtendvpgf







      END DO

   ENDIF

   RETURN

END SUBROUTINE apply_meso_tends


SUBROUTINE update_meso_sfc_vals ( meso_hfx, meso_tsk,               &
                                  m_valu_hfx, m_valu_tsk, m_tend_t, & 
                                  meso_force, n_meso_t_levs,        & 
                                  xtime,                            & 
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  its, ite, jts, jte, kts, kte      )

   IMPLICIT NONE

   REAL ,                                             INTENT(INOUT) :: meso_hfx, &
                                                                       meso_tsk

   REAL , DIMENSION( 1:n_meso_t_levs ),                  INTENT(IN) :: m_valu_hfx,  &
                                                                       m_valu_tsk,  &
                                                                       m_tend_t

   REAL ,                                                INTENT(IN) :: xtime

   INTEGER,                                              INTENT(IN) :: meso_force,   &  
                                                                       n_meso_t_levs

   INTEGER , INTENT( IN  ) :: ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte



   INTEGER :: n, t
   REAL :: fac_t              
                             






   IF ( ( xtime .GE. m_tend_t(1) ) .AND. ( xtime .LE. m_tend_t(n_meso_t_levs) ) ) THEN



       DO n = 1, n_meso_t_levs-1

          IF ( ( xtime .GE. m_tend_t(n) ) .AND. ( xtime .LT. m_tend_t(n+1) ) ) t = n

       END DO



       fac_t = (xtime - m_tend_t(t))/(m_tend_t(t+1)-m_tend_t(t))





       IF ( meso_force .EQ. 1 ) THEN

          meso_hfx = (1.0-fac_t)*m_valu_hfx(t) + fac_t*m_valu_hfx(t+1)

       ELSE

          meso_tsk = (1.0-fac_t)*m_valu_tsk(t) + fac_t*m_valu_tsk(t+1)

       ENDIF

   ENDIF

   RETURN

END SUBROUTINE update_meso_sfc_vals



END MODULE module_meso_tend_micro_force
