
MODULE module_wind_param_driver

CONTAINS

!------------------------------------------------------------------

SUBROUTINE wind_param_driver( grid, config_flags,           &
                              ru_tendf, rv_tendf, rw_tendf, &
                              u_2, v_2, w_2, rdzw,          &
                              muu, muv, mut,                &
                              dx, dy,                       &
                              ht, alt,                      & !NM  
                              wp_acc_u, wp_acc_v, wp_acc_w, &
                              trbloc_ugx, trbloc_mlx,       & !NM	 
                              trbloc_vgy, trbloc_mly,       & !NM	
                              trbloc_ulx, trbloc_vly,       & !NM
                              upwnd_ugx, upwnd_vgy,         & !NM
                              upwnd_mgx, upwnd_mgy,         & !NM
                              trblsp_xs, trblsp_xe,         & !NM
                              trblsp_ys, trblsp_ye,         & !NM
                              trblsp_zs, trblsp_ze,         & !NM
                              itimestep, dt,                & !NM
                              x_turbine, y_turbine,         & !NM
                              hub_height, rotor_diameter,   & !NM
                              blade_length, theta_turbine,  & !NM
                              cutinspd, cutoutspd,          & !NM -new
                              xlat, xlon,                   & !NM -new u lat
                              yaw,                          & !NM
                              rst_cust,                     & !NM	
                              ywert, v0t,                   & !NM
                              u_x, vw_x, v_y, uw_y,         & !NM
                              dau, dav, daw, dru, drv, drw, & !NM
                              ywacc_trb,                    & !NM
                              yetmct,                       & !NM
                              x_s_turbA, x_e_turbA,         & !NM
                              y_s_turbA, y_e_turbA,         & !NM
                              thrust, torque,               &
                              rotrate, power,               &
                              ! out1, out2, out3,           & !NM
                              n_turbines,                   &
                              n_timeseries,                 & !NM
                              ids,ide,jds,jde,kds,kde,      & 
                              ims,ime,jms,jme,kms,kme,      &
                              ips,ipe,jps,jpe,kps,kpe,      &
                              its,ite,jts,jte,kts,kte       )

!------------------------------------------------------------------

! Driver layer modules
  USE module_domain
  USE module_configure
  USE module_tiles
  USE module_dm
  USE module_machine
  USE module_state_description
! Model layer modules
  USE module_bc

!! *** add new modules of schemes here

  USE module_gen_act_disk

  IMPLICIT NONE

! Input data.

  TYPE(domain) , TARGET          :: grid

  TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT( INOUT ) &
  ::  ru_tendf,  & ! u tendency * mass           (kg s-4)                                                              
      rv_tendf,  & ! v                                                            
      rw_tendf     ! w

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT( IN  ) &
  ::  u_2,       & ! u-velocity                  (m s-1)                                                           
      v_2,       & ! v                                                              
      w_2,       & ! w                                                           
      rdzw         ! inverse vertical gid spacing at w-nodes (m-1)

  REAL, DIMENSION( ims:ime, jms:jme) ,         INTENT( IN  ) &
  ::  muu,       & ! column pressure at u nodes  (kg m-1 s-2)                                                                  
      muv,       & !                    v                                                                
      mut          !                    w 

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( INOUT ) &
  :: wp_acc_u    & ! acceleration for u          (m s-2)
   , wp_acc_v    & !                  v          (m s-2)
   , wp_acc_w      !                  w          (m s-2)

  INTEGER,                                        INTENT( IN  ) &
  :: n_turbines   &  !NM number of turbines from namelist
   , n_timeseries    ! number of time series points for time average

  INTEGER, DIMENSION( n_turbines ),  INTENT( INOUT ) & !NM 
  :: trbloc_ugx   &     !u-staggered global index of turbine x coord
   , trbloc_mlx   &     !mass-center local index for turbine x coord
   , trbloc_vgy   &     !v-staggered global index for turbine y coord
   , trbloc_mly   &     !mass-center local index for turbine y coord
   , trbloc_ulx   &     !u-staggered local index for turbine x coord
   , trbloc_vly   &     !v-staggered local index for turbine y coord
   , trblsp_xs   &      !starting local index for turbine span on x-cord 
   , trblsp_xe   &      !end local index for turbine span on x-cord
   , trblsp_ys   &      !start local index for turbine span y coord
   , trblsp_ye   &      !end local index for turbine span y coord
   , trblsp_zs   &      !start local index for turbine span z coord
   , trblsp_ze   &      !end local index for turbine span z coord
   , upwnd_ugx   &      !u-stagg global index of upwind turb x cor
   , upwnd_vgy   &      !v-stagg global index for upwnd turb y cor
   , upwnd_mgx   &      !mass-center global index for upwnd turb x
   , upwnd_mgy          !mass-center global index for upwnd turb y
    
  INTEGER,                                     INTENT( IN ) & !NM 
  :: itimestep                                 

  REAL, DIMENSION( n_turbines ),             INTENT( INOUT ) &    !NM global 
  :: x_turbine  &  !x turbine location in meters
   , y_turbine  &  !y turbine location in meters
   , hub_height &  !hub height of turbine in meters
   , rotor_diameter &  !rotor diameter of a turbine
   , blade_length   &  !blade length of turbine
   , theta_turbine  &  !angle between turbine and x axis
   , ywacc_trb &    !yaw error accumulation
   , cutinspd &     !cut in speed
   , cutoutspd      !cut out speed

  REAL, DIMENSION( n_turbines ), INTENT(INOUT) & !NM 
  :: x_s_turbA  &  !NM start for turbine are calc in x dir in meters
   , x_e_turbA  &
   , y_s_turbA  &
   , y_e_turbA       

  INTEGER, DIMENSION( n_turbines ), INTENT(INOUT)  & !NM
  :: yetmct !NM yaw error time counter 

  LOGICAL, DIMENSION( n_turbines ), INTENT(INOUT)  &  !NM 
  :: yaw    !NM tells us if turbine yawing 1 or not 0

  LOGICAL, INTENT(INOUT)  & !NM
  :: rst_cust !NM keeps track of restart

  REAL, DIMENSION( n_timeseries, n_turbines), INTENT(INOUT)  & !NM
  :: ywert  & !NM keeps track of yaw errors in time series
   , v0t      !NM wind speed time serries

  REAL, DIMENSION( ims:ime ),                  INTENT( INOUT ) & !NM so can get memory location of turbine!its:ite ) &  !what does t mean (means tiles) use for local variables
  :: u_x         & !x coordinate of u-nodes (m) x-coordinates of u nodes (staggered) or in meters (distance), only one to !probably have all points used since staggered rest will be ite-1 or its+1 !(missing data then????)
   , vw_x          !NM                v,w nodes (m)

  REAL, DIMENSION( jms:jme ),                  INTENT( INOUT ) & !NM so can get memory location of turbine !jts:jte ) &
  :: v_y         & !y coordinate of v-nodes (m) !y-coord for u,v,w (distance)
   , uw_y          !NM             u,w nodes (m)

  REAL, DIMENSION( ims:ime, n_turbines, jms:jme ), INTENT( INOUT ) &  !NM if we only need to change once why use INOUT ??????????????
  :: dau       & !normal distance from u-nodes to the rotor plane (m) !This is perpendicular distance from the u-nodes to the axial or n plane
   , dav       &!                    v                           (m)
   , daw        !                    w                          

  REAL, DIMENSION( ims:ime, n_turbines, jms:jme ), INTENT( INOUT ) &  !perpendicular distance from the u-nodes to the rotor plane
  :: dru       & !normal distance from u-nodes to the rotor plane (m) !this is per column since it does not change with height
   , drv       & !                    v                           (m) 
   , drw         !                    w

!JMTN
!  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( OUT ) & !NM
!  :: out1        & ! storage variable
!   , out2        &
!   , out3

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),           INTENT( OUT ) & !NM
  :: thrust      & !                             (N)
   , torque      & !                             (N m)
   , rotrate     & !                             (rad s-1)
   , power         !                             (W) 

  REAL,                                           INTENT( IN  ) &
  :: dx          & !grid spacing in x-direction  (m)
   , dy          & !                y
   , dt            !      temporal resolution (s)

  REAL, DIMENSION( ims:ime, jms:jme ),           INTENT( IN  ) & !NM 
  :: ht         !NM elevation/ grid height cell center (m)

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT( IN  ) & !NM
  :: alt        !NM, inverse air density (m3/kg) 

  REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN ) & !NM 
  :: xlat       & !NM latitude of u value
   , xlon         !NM longitude of u value


  INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde,               &
                         ims,ime,jms,jme,kms,kme,               &
                         ips,ipe,jps,jpe,kps,kpe,               &
                         its,ite,jts,jte,kts,kte

!local variables

  INTEGER :: ij, i, j, k

  IF ( config_flags%wp_opt .EQ. 2 ) then

     CALL gen_act_disk ( ru_tendf, rv_tendf, rw_tendf,     &  
                         config_flags%wp_acc_amp,          & 
                         n_turbines,                       &
                         n_timeseries,                     & !NM
                         wp_acc_u, wp_acc_v, wp_acc_w,     &
                         trbloc_ugx, trbloc_mlx,           & !NM	 
                         trbloc_vgy, trbloc_mly,           & !NM	
                         trbloc_ulx, trbloc_vly,           & !NM
                         upwnd_ugx, upwnd_vgy,             & !JMTN
                         upwnd_mgx, upwnd_mgx,             & !JMTN
                         trblsp_xs, trblsp_xe,             & !NM
                         trblsp_ys, trblsp_ye,             & !NM
                         trblsp_zs, trblsp_ze,             & !NM
                         x_turbine, y_turbine,             & !NM
                         hub_height, rotor_diameter,       & !NM
                         blade_length, theta_turbine,      & !NM
                         cutinspd, cutoutspd,              & !JMTN
                         xlat, xlon,                       & !JMTN
                         itimestep, dt,                    & !NM
                         yaw,                              & !NM
                         rst_cust,                         & !NM
                         ywert, v0t,                       & !NM
                         u_x, vw_x, v_y, uw_y,             & !NM
                         dau, dav, daw, dru, drv, drw,     & !NM
                         ywacc_trb,                        & !NM
                         yetmct,                           & !NM
                         x_s_turbA, x_e_turbA,             & !NM
                         y_s_turbA, y_e_turbA,             & !NM
                         thrust, torque,                   &
                         rotrate, power,                   &
                         ! out1, out2, out3,               & !NM
                         u_2, v_2, w_2,                    & 
                         muu, muv, mut,                    & 
                         rdzw, dx, dy,                     &
                         ht, alt,                          & !NM
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         ips, ipe, jps, jpe, kps, kpe,     &
                         its, ite, jts, jte, kts, kte      )

  ENDIF

RETURN

END SUBROUTINE wind_param_driver


END MODULE module_wind_param_driver
