





















SUBROUTINE init_domain_constants_em ( parent , nest )
   USE module_domain, ONLY : domain
   IMPLICIT NONE
   TYPE(domain)  :: parent , nest

   INTEGER iswater, islake, isice, isurban, isoilwater, map_proj, julyr, julday
   REAL    truelat1 , truelat2 , gmt , moad_cen_lat , stand_lon, pole_lat, pole_lon
   CHARACTER (LEN=256) :: char_junk



   nest%p_top   = parent%p_top
   nest%save_topo_from_real   = parent%save_topo_from_real
   nest%cfn     = parent%cfn
   nest%cfn1    = parent%cfn1
   nest%rdx     = 1./nest%dx
   nest%rdy     = 1./nest%dy

   nest%dtseps  = parent%dtseps  
   nest%resm    = parent%resm    
   nest%zetatop = parent%zetatop 
   nest%cf1     = parent%cf1
   nest%cf2     = parent%cf2
   nest%cf3     = parent%cf3
   nest%gmt     = parent%gmt
   nest%julyr   = parent%julyr
   nest%julday  = parent%julday
   nest%iswater = parent%iswater
   nest%isice   = parent%isice
   nest%isurban = parent%isurban
   nest%islake  = parent%islake
   nest%isoilwater = parent%isoilwater
   nest%mminlu  = trim(parent%mminlu)
   nest%tiso    = parent%tiso
   nest%tlp     = parent%tlp
   nest%p00     = parent%p00
   nest%t00     = parent%t00
   nest%tlp_strat= parent%tlp_strat
   nest%p_strat = parent%p_strat

   nest%traj_k = parent%traj_k
   nest%traj_long = parent%traj_long
   nest%traj_lat = parent%traj_lat
   nest%this_is_an_ideal_run = parent%this_is_an_ideal_run
   nest%lake_depth_flag = parent%lake_depth_flag

   CALL nl_get_mminlu ( 1, char_junk )
   CALL nl_get_iswater( 1, iswater )
   CALL nl_get_islake ( 1, islake )
   CALL nl_get_isice  ( 1, isice )
   CALL nl_get_isurban( 1, isurban )
   CALL nl_get_isoilwater(1, isoilwater )
   CALL nl_get_truelat1 ( 1 , truelat1 )
   CALL nl_get_truelat2 ( 1 , truelat2 )
   CALL nl_get_moad_cen_lat ( 1 , moad_cen_lat )
   CALL nl_get_stand_lon ( 1 , stand_lon )
   CALL nl_get_pole_lat ( 1 , pole_lat )
   CALL nl_get_pole_lon ( 1 , pole_lon )
   CALL nl_get_map_proj ( 1 , map_proj )
   CALL nl_get_gmt ( 1 , gmt)
   CALL nl_get_julyr ( 1 , julyr)
   CALL nl_get_julday ( 1 , julday)
   IF ( nest%id .NE. 1 ) THEN
     CALL nl_set_gmt (nest%id, gmt)
     CALL nl_set_julyr (nest%id, julyr)
     CALL nl_set_julday (nest%id, julday)
     CALL nl_set_iswater ( nest%id, iswater )
     CALL nl_set_islake  ( nest%id, islake )
     CALL nl_set_isice   ( nest%id, isice )
     CALL nl_set_isurban ( nest%id, isurban )
     CALL nl_set_isoilwater ( nest%id, isoilwater )
     CALL nl_set_mminlu ( nest%id, char_junk )
     CALL nl_set_truelat1 ( nest%id , truelat1 )
     CALL nl_set_truelat2 ( nest%id , truelat2 )
     CALL nl_set_moad_cen_lat ( nest%id , moad_cen_lat )
     CALL nl_set_stand_lon ( nest%id , stand_lon )
     CALL nl_set_pole_lat ( nest%id , pole_lat )
     CALL nl_set_pole_lon ( nest%id , pole_lon )
     CALL nl_set_map_proj ( nest%id , map_proj )
   END IF
   nest%gmt     = gmt
   nest%julday  = julday
   nest%julyr   = julyr
   nest%iswater = iswater
   nest%islake  = islake
   nest%isice   = isice
   nest%isoilwater = isoilwater
   nest%mminlu  = trim(char_junk)
   nest%truelat1= truelat1
   nest%truelat2= truelat2
   nest%moad_cen_lat= moad_cen_lat
   nest%stand_lon= stand_lon
   nest%pole_lat= pole_lat
   nest%pole_lon= pole_lon
   nest%map_proj= map_proj

   nest%step_number  = parent%step_number



   nest%fnm    = parent%fnm
   nest%fnp    = parent%fnp
   nest%rdnw   = parent%rdnw
   nest%rdn    = parent%rdn
   nest%dnw    = parent%dnw
   nest%dn     = parent%dn
   nest%znu    = parent%znu
   nest%znw    = parent%znw
   nest%t_base = parent%t_base
   nest%u_base    = parent%u_base
   nest%v_base    = parent%v_base
   nest%qv_base   = parent%qv_base
   nest%z_base    = parent%z_base
   nest%dzs       = parent%dzs
   nest%zs        = parent%zs

END SUBROUTINE init_domain_constants_em




SUBROUTINE init_domain_vert_nesting ( parent, nest)


   USE module_domain
   IMPLICIT NONE
   TYPE(domain), POINTER                      :: parent, nest
					 
   
   REAL, DIMENSION(parent%e_vert)             :: znw_c

   INTERFACE

      SUBROUTINE vert_cor_vertical_nesting_integer(nest,znw_c,k_dim_c)
         USE module_domain
         TYPE(domain), POINTER :: nest
         integer , intent(in) :: k_dim_c
         real , dimension(k_dim_c), INTENT(IN) :: znw_c
      END SUBROUTINE vert_cor_vertical_nesting_integer

      SUBROUTINE vert_cor_vertical_nesting_arbitrary(nest,znw_c,kde_c)
          USE module_domain
          TYPE(domain), POINTER :: nest
          INTEGER, INTENT(IN   ) :: kde_c
          REAL, DIMENSION(kde_c), INTENT(IN   ) :: znw_c
      END SUBROUTINE vert_cor_vertical_nesting_arbitrary
      
   END INTERFACE

   
   
   
   znw_c = nest%znw(1:parent%e_vert)
   
   
   
   if (nest%vert_refine_method .EQ. 1) then  
      CALL vert_cor_vertical_nesting_integer(nest,znw_c,parent%e_vert)
   elseif (nest%vert_refine_method .EQ. 2) then
      CALL vert_cor_vertical_nesting_arbitrary(nest,znw_c,parent%e_vert)
   endif

END SUBROUTINE init_domain_vert_nesting






      SUBROUTINE vert_cor_vertical_nesting_integer(nest,znw_c,k_dim_c)
         USE module_domain
   IMPLICIT NONE
         TYPE(domain), POINTER :: nest
         integer , intent(in) :: k_dim_c
         real , dimension(k_dim_c), INTENT(IN) :: znw_c

       integer :: kde_c , kde_n ,n_refine,ii,kkk,k
       real :: dznw_m,cof1,cof2








        kde_c = k_dim_c
        kde_n = nest%e_vert

        n_refine = (kde_n-1)/(kde_c-1)
	
         kkk = 0
         do k = 1 , kde_c-1
         dznw_m = znw_c(k+1) - znw_c(k)
         do ii = 1,n_refine
         kkk = kkk + 1
         nest%znw(kkk) = znw_c(k) + float(ii-1)/float(n_refine)*dznw_m
         enddo
         enddo
         nest%znw(kde_n) = znw_c(kde_c)
         nest%znw(1) = znw_c(1)

      DO k=1, kde_n-1
         nest%dnw(k) = nest%znw(k+1) - nest%znw(k)
         nest%rdnw(k) = 1./nest%dnw(k)
         nest%znu(k) = 0.5*(nest%znw(k+1)+nest%znw(k))
      END DO

      DO k=2, kde_n-1
         nest%dn(k) = 0.5*(nest%dnw(k)+nest%dnw(k-1))
         nest%rdn(k) = 1./nest%dn(k)
         nest%fnp(k) = .5* nest%dnw(k  )/nest%dn(k)
         nest%fnm(k) = .5* nest%dnw(k-1)/nest%dn(k)
      END DO

  cof1 = (2.*nest%dn(2)+nest%dn(3))/(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(2)
  cof2 =     nest%dn(2)        /(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(3)

      nest%cf1  = nest%fnp(2) + cof1
      nest%cf2  = nest%fnm(2) - cof1 - cof2
      nest%cf3  = cof2

      nest%cfn  = (.5*nest%dnw(kde_n-1)+nest%dn(kde_n-1))/nest%dn(kde_n-1)
      nest%cfn1 = -.5*nest%dnw(kde_n-1)/nest%dn(kde_n-1)
      
      

      END SUBROUTINE vert_cor_vertical_nesting_integer



SUBROUTINE vert_cor_vertical_nesting_arbitrary(nest,znw_c,kde_c)
    USE module_domain
    IMPLICIT NONE
    TYPE(domain), POINTER :: nest
    INTEGER, INTENT(IN   ) :: kde_c
    REAL, DIMENSION(kde_c), INTENT(IN   ) :: znw_c
    INTEGER :: k, kde_n, ks, id
    REAL :: cof1, cof2

    kde_n = nest%e_vert

    
    IF (nest%id .NE. 1) THEN
      id = 1
      ks = 1
      DO WHILE (nest%id .GT. id)
        id = id+1
        ks = ks+model_config_rec%e_vert(id-1)
      ENDDO
    ENDIF
    DO k=1,kde_n
      nest%znw(k) = model_config_rec%eta_levels(ks+k-1)
      write(*,'(A,I3,A,F5.3)') "DJW[nest_init_utils]: nest%znw(",k,") = ",nest%znw(k)
    ENDDO
    
    
    IF (nest%znw(1) .NE. 1.0) THEN
      CALL wrf_error_fatal3("<stdin>",270,&
"error with specified eta_levels, first level is not 1.0")
    ENDIF
    IF (nest%znw(kde_n) .NE. 0.0) THEN
      CALL wrf_error_fatal3("<stdin>",274,&
"error with specified eta_levels, last level is not 0.0")
    ENDIF
    DO k=2,kde_n
      IF (nest%znw(k) .GT. nest%znw(k-1)) THEN
        CALL wrf_error_fatal3("<stdin>",279,&
"eta_levels are not uniformly decreasing from 1.0 to 0.0")
      ENDIF
    ENDDO
    

    DO k=1,kde_n-1
      nest%dnw(k) = nest%znw(k+1)-nest%znw(k)
      nest%rdnw(k) = 1./nest%dnw(k)
      nest%znu(k) = 0.5*(nest%znw(k+1)+nest%znw(k))
    ENDDO
    nest%znu(kde_n) = 0.0

    DO k=2,kde_n-1
      nest%dn(k) = 0.5*(nest%dnw(k)+nest%dnw(k-1))
      nest%rdn(k) = 1./nest%dn(k)
      nest%fnp(k) = .5* nest%dnw(k  )/nest%dn(k)
      nest%fnm(k) = .5* nest%dnw(k-1)/nest%dn(k)
    ENDDO

    cof1 = (2.*nest%dn(2)+nest%dn(3))/(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(2)
    cof2 =     nest%dn(2)            /(nest%dn(2)+nest%dn(3))*nest%dnw(1)/nest%dn(3)

    nest%cf1  = nest%fnp(2) + cof1
    nest%cf2  = nest%fnm(2) - cof1 - cof2
    nest%cf3  = cof2
    nest%cfn  = (.5*nest%dnw(kde_n-1)+nest%dn(kde_n-1))/nest%dn(kde_n-1)
    nest%cfn1 = -.5*nest%dnw(kde_n-1)/nest%dn(kde_n-1)

END SUBROUTINE vert_cor_vertical_nesting_arbitrary



SUBROUTINE blend_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   USE module_configure
   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)    :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: ter_input

   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) :: ter_temp
   INTEGER :: i , j , k , spec_bdy_width
   REAL    :: r_blend_zones
   INTEGER blend_cell, blend_width

   
   
   
   

   CALL nl_get_spec_bdy_width ( 1, spec_bdy_width)
   CALL nl_get_blend_width ( 1, blend_width)

   
   

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_temp(i,k,j) = ter_input(i,k,j)
         END DO
      END DO
   END DO

   
   
   
   

   r_blend_zones = 1./(blend_width+1)
   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            DO blend_cell = blend_width,1,-1
               IF   ( ( i .EQ.       spec_bdy_width + blend_cell ) .OR.  ( j .EQ.       spec_bdy_width + blend_cell ) .OR. &
                      ( i .EQ. ide - spec_bdy_width - blend_cell ) .OR.  ( j .EQ. jde - spec_bdy_width - blend_cell ) ) THEN
                  ter_temp(i,k,j) = ( (blend_cell)*ter_input(i,k,j) + (blend_width+1-blend_cell)*ter_interpolated(i,k,j) ) &
                                    * r_blend_zones
               END IF
            ENDDO
            IF      ( ( i .LE.       spec_bdy_width     ) .OR.  ( j .LE.       spec_bdy_width     ) .OR. &
                      ( i .GE. ide - spec_bdy_width     ) .OR.  ( j .GE. jde - spec_bdy_width     ) ) THEN
               ter_temp(i,k,j) =      ter_interpolated(i,k,j)
            END IF
         END DO
      END DO
   END DO

   
   

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_input(i,k,j) = ter_temp(i,k,j)
         END DO
      END DO
   END DO

END SUBROUTINE blend_terrain

SUBROUTINE copy_3d_field ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT) :: ter_interpolated
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)  :: ter_input

   INTEGER :: i , j , k

   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe
         DO i = ips , MIN(ipe, ide-1)
            ter_interpolated(i,k,j) = ter_input(i,k,j)
         END DO
      END DO
   END DO

END SUBROUTINE copy_3d_field

SUBROUTINE adjust_tempqv ( mub, save_mub, znw, p_top, &
                           th, pp, qv,  &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   
   
   USE module_model_constants

   
   
   
   
   
   IMPLICIT NONE

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)    :: mub, save_mub
   REAL , DIMENSION(kms:kme) , INTENT(IN)    :: znw
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: th, pp, qv

   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) :: p_old, p_new, rh
   REAL :: es,dth,tc,e,dth1
   INTEGER :: i , j , k

   real p_top




   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe-1
         DO i = ips , MIN(ipe, ide-1)
            p_old(i,k,j) = 0.5*(znw(k+1)+znw(k))*save_mub(i,j) + p_top + pp(i,k,j)
            tc = (th(i,k,j)+300.)*(p_old(i,k,j)/1.e5)**(2./7.) - 273.15
            es = 610.78*exp(17.0809*tc/(234.175+tc))
            e = qv(i,k,j)*p_old(i,k,j)/(0.622+qv(i,k,j))
            rh(i,k,j) = e/es
         END DO
      END DO
   END DO


   DO j = jps , MIN(jpe, jde-1)
      DO k = kps , kpe-1
         DO i = ips , MIN(ipe, ide-1)
            p_new(i,k,j) = 0.5*(znw(k+1)+znw(k))*mub(i,j) + p_top + pp(i,k,j)

            dth1 = -191.86e-3*(th(i,k,j)+300.)/(p_new(i,k,j)+p_old(i,k,j))*(p_new(i,k,j)-p_old(i,k,j))
            dth = -191.86e-3*(th(i,k,j)+0.5*dth1+300.)/(p_new(i,k,j)+p_old(i,k,j))*(p_new(i,k,j)-p_old(i,k,j))
            th(i,k,j) = th(i,k,j)+dth
            tc = (th(i,k,j)+300.)*(p_new(i,k,j)/1.e5)**(2./7.) - 273.15
            es = 610.78*exp(17.0809*tc/(234.175+tc))
            e = rh(i,k,j)*es
            qv(i,k,j) = 0.622*e/(p_new(i,k,j)-e)
         END DO
      END DO
   END DO


END SUBROUTINE adjust_tempqv

SUBROUTINE input_terrain_rsmas ( grid ,                        &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )

   USE module_domain, ONLY : domain
   IMPLICIT NONE
   TYPE ( domain ) :: grid

   INTEGER , INTENT(IN)                       :: ids , ide , jds , jde , kds , kde , &
                                                 ims , ime , jms , jme , kms , kme , &
                                                 ips , ipe , jps , jpe , kps , kpe

   LOGICAL, EXTERNAL ::  wrf_dm_on_monitor

   INTEGER :: i , j , k , myproc
   INTEGER, DIMENSION(256) :: ipath  
   CHARACTER*256 :: message, message2
   CHARACTER*256 :: rsmas_data_path


   REAL , DIMENSION(ids:ide,jds:jde) :: ht_g, xlat_g, xlon_g

   CALL wrf_get_myproc ( myproc )


   CALL nl_get_rsmas_data_path(1,rsmas_data_path)
   do i = 1, LEN(TRIM(rsmas_data_path))
      ipath(i) = ICHAR(rsmas_data_path(i:i))
   enddo


   CALL wrf_patch_to_global_real ( grid%xlat , xlat_g , grid%domdesc, ' ' , 'xy' ,       &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   )
   CALL wrf_patch_to_global_real ( grid%xlong , xlon_g , grid%domdesc, ' ' , 'xy' ,       &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   )

   IF ( wrf_dm_on_monitor() ) THEN
     CALL get_terrain ( grid%dx/1000., xlat_g(ids:ide,jds:jde), xlon_g(ids:ide,jds:jde), ht_g(ids:ide,jds:jde), &
                        ide-ids+1,jde-jds+1,ide-ids+1,jde-jds+1, ipath, LEN(TRIM(rsmas_data_path)) )
     WHERE ( ht_g(ids:ide,jds:jde) < -1000. ) ht_g(ids:ide,jds:jde) = 0.
   ENDIF

   CALL wrf_global_to_patch_real ( ht_g , grid%ht , grid%domdesc, ' ' , 'xy' ,         &
                                   ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                   ims, ime   , jms , jme   , 1 , 1 , &
                                   ips, ipe   , jps , jpe   , 1 , 1   )


END SUBROUTINE input_terrain_rsmas

SUBROUTINE update_after_feedback_em ( grid  &







,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs,dfi_moist_bxe, &
dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs,scalar_bxe,scalar_bys, &
scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe,dfi_scalar_bys, &
dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,ozmixm,aerosolc_1,aerosolc_2,fdda3d,fdda2d, &
advh_t,advz_t,nba_mij,nba_rij,lasd_uvw_bar_hat,lasd_s_bar_hat,lasd_sij,lasd_lij,lasd_qij,lasd_gij,lasd_nij,lasd_tc,lasd_lagr, &
rsfs_rec,rsfs_prec,rsfs_filt,rsfs_rmij,tfu_drm,uc_drm,tfuc_drm,tgrmij_drm,tfrmij_drm,sijc_drm,tfsij_drm,tfsijc_drm,hij_drm, &
hijc_drm,evc_drm,chem,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys,tracer_btye &


                 )






   USE module_domain, ONLY : domain, get_ijk_from_grid
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask
   USE module_comm_dm, ONLY : HALO_EM_FEEDBACK_sub
   USE module_bc


   USE module_state_description

   IMPLICIT NONE

   

   TYPE(domain) , TARGET         :: grid

   






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_uvw_bar_hat)           :: lasd_uvw_bar_hat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_s_bar_hat)           :: lasd_s_bar_hat
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_sij)           :: lasd_sij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_lij)           :: lasd_lij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_qij)           :: lasd_qij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_gij)           :: lasd_gij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_nij)           :: lasd_nij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_tc)           :: lasd_tc
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_lasd_lagr)           :: lasd_lagr
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_rsfs_rec)           :: rsfs_rec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_rsfs_prec)           :: rsfs_prec
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_rsfs_filt)           :: rsfs_filt
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_rsfs_rmij)           :: rsfs_rmij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tfu_drm)           :: tfu_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_uc_drm)           :: uc_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tfuc_drm)           :: tfuc_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tgrmij_drm)           :: tgrmij_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tfrmij_drm)           :: tfrmij_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_sijc_drm)           :: sijc_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tfsij_drm)           :: tfsij_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tfsijc_drm)           :: tfsijc_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_hij_drm)           :: hij_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_hijc_drm)           :: hijc_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_evc_drm)           :: evc_drm
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye


   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe

  CALL wrf_debug( 500, "entering update_after_feedback_em" )


  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )

  CALL wrf_debug( 500, "before HALO_EM_FEEDBACK.inc in update_after_feedback_em" )






CALL HALO_EM_FEEDBACK_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde,       &
  ims, ime, jms, jme, kms, kme,       &
  ips, ipe, jps, jpe, kps, kpe )

  CALL wrf_debug( 500, "leaving update_after_feedback_em" )

END SUBROUTINE update_after_feedback_em

