!======================================================================
! Rad_ukmo Interface
! Author: Nian Guokui
! E-mail: nian@lasg.iap.ac.cn
! Date  : 2014.09.15 
!
! Rad_rrtmg Interface
! Author: Nian Guokui
! E-mail: nian@lasg.iap.ac.cn
! Date  : 2017.08.01
!======================================================================

!#define snow_radiation

module raduk_mod

    use aer_rad_props_mod,only: nswbands, nlwbands
    use fms_mod,          only: file_exist, close_file, open_namelist_file, check_nml_error
    use physconst_mod,    only: gravit, pi, cpair, rair
    use time_manager_mod, only: time_type
    use diag_manager_mod, only: send_data
    use time_cal_mod,     only: get_calendar

    use rrtmg_sw_init,    only: rrtmg_sw_ini
    use rrtmg_lw_init,    only: rrtmg_lw_ini
    implicit none

    integer, save :: id_rsds, id_rsus, id_rsns, id_rlds, id_rlus, id_rlns, &
                     id_rsdt, id_rsut, id_rsnt, id_rldt, id_rlut, id_rlnt
    integer, save :: id_rds,  id_rus,  id_rdt,  id_rut
    integer, save :: id_rsdscs, id_rsuscs, id_rsnscs, id_rldscs, id_rluscs, id_rlnscs, &
                     id_rsdtcs, id_rsutcs, id_rsntcs, id_rldtcs, id_rlutcs, id_rlntcs
    integer, save :: id_rdscs,  id_ruscs,  id_rdtcs,  id_rutcs
    integer, save :: id_qrl, id_qrs, id_swcf, id_lwcf, id_ozone
    integer, save :: id_aodvis, id_aoddust, id_aoddust1, id_aoddust2, id_aoddust3
    integer, save :: id_cldliq, id_cldice, id_clw, id_cli, id_clwvi, id_clivi
    integer, save :: id_ttauda
    integer, save :: id_so4, id_soa, id_cb1, id_cb2, id_dst01, id_dst02,      &
                     id_dst03, id_dst04, id_oc1, id_oc2, id_sslt01, id_sslt02,&
                     id_sslt03, id_sslt04, id_volc
    integer, save :: id_tau_1, id_tau_2, id_tau_3, id_tau_4, id_tau_5, id_tau_6, id_tau_7, &
                     id_tau_8, id_tau_9, id_tau_10, id_tau_11, id_tau_12, id_tau_13, id_tau_14
    integer, save :: id_tau_w_1, id_tau_w_2, id_tau_w_3, id_tau_w_4, id_tau_w_5, id_tau_w_6, id_tau_w_7, &
                     id_tau_w_8, id_tau_w_9, id_tau_w_10, id_tau_w_11, id_tau_w_12, id_tau_w_13, id_tau_w_14
    integer, save :: id_tau_w_g_1, id_tau_w_g_2, id_tau_w_g_3, id_tau_w_g_4, id_tau_w_g_5, id_tau_w_g_6, id_tau_w_g_7, &
                     id_tau_w_g_8, id_tau_w_g_9, id_tau_w_g_10, id_tau_w_g_11, id_tau_w_g_12, id_tau_w_g_13, id_tau_w_g_14
    integer, save :: id_tau_w_f_1, id_tau_w_f_2, id_tau_w_f_3, id_tau_w_f_4, id_tau_w_f_5, id_tau_w_f_6, id_tau_w_f_7, &
                     id_tau_w_f_8, id_tau_w_f_9, id_tau_w_f_10, id_tau_w_f_11, id_tau_w_f_12, id_tau_w_f_13, id_tau_w_f_14
    integer, save :: id_odap_aer_1, id_odap_aer_2, id_odap_aer_3, id_odap_aer_4, id_odap_aer_5, id_odap_aer_6, id_odap_aer_7, &
                     id_odap_aer_8, id_odap_aer_9, id_odap_aer_10, id_odap_aer_11, id_odap_aer_12, id_odap_aer_13, id_odap_aer_14, &
                     id_odap_aer_15, id_odap_aer_16 
    integer, save :: id_cld_tau_1, id_cld_tau_2, id_cld_tau_3, id_cld_tau_4, id_cld_tau_5, id_cld_tau_6, id_cld_tau_7, &
                     id_cld_tau_8, id_cld_tau_9, id_cld_tau_10, id_cld_tau_11, id_cld_tau_12, id_cld_tau_13, id_cld_tau_14
    integer, save :: id_cld_tau_w_1, id_cld_tau_w_2, id_cld_tau_w_3, id_cld_tau_w_4, id_cld_tau_w_5, id_cld_tau_w_6, id_cld_tau_w_7, &
                     id_cld_tau_w_8, id_cld_tau_w_9, id_cld_tau_w_10, id_cld_tau_w_11, id_cld_tau_w_12, id_cld_tau_w_13, id_cld_tau_w_14
    integer, save :: id_cld_tau_w_g_1, id_cld_tau_w_g_2, id_cld_tau_w_g_3, id_cld_tau_w_g_4, id_cld_tau_w_g_5, id_cld_tau_w_g_6, id_cld_tau_w_g_7, &
                     id_cld_tau_w_g_8, id_cld_tau_w_g_9, id_cld_tau_w_g_10, id_cld_tau_w_g_11, id_cld_tau_w_g_12, id_cld_tau_w_g_13, id_cld_tau_w_g_14
    integer, save :: id_cld_tau_w_f_1, id_cld_tau_w_f_2, id_cld_tau_w_f_3, id_cld_tau_w_f_4, id_cld_tau_w_f_5, id_cld_tau_w_f_6, id_cld_tau_w_f_7, &
                     id_cld_tau_w_f_8, id_cld_tau_w_f_9, id_cld_tau_w_f_10, id_cld_tau_w_f_11, id_cld_tau_w_f_12, id_cld_tau_w_f_13, id_cld_tau_w_f_14
    integer, save :: id_cld_lw_abs_1, id_cld_lw_abs_2, id_cld_lw_abs_3, id_cld_lw_abs_4, id_cld_lw_abs_5, id_cld_lw_abs_6, id_cld_lw_abs_7, &
                     id_cld_lw_abs_8, id_cld_lw_abs_9, id_cld_lw_abs_10, id_cld_lw_abs_11, id_cld_lw_abs_12, id_cld_lw_abs_13, id_cld_lw_abs_14, &
                     id_cld_lw_abs_15, id_cld_lw_abs_16 

    integer, save :: id_swuflx_sf, id_swdflx_sf, id_swuflxc_sf, id_swdflxc_sf
    integer, save :: id_swuflx_lv, id_swdflx_lv, id_swuflxc_lv, id_swdflxc_lv, id_swhr, id_swhrc
    integer, save :: id_lwuflx_sf, id_lwdflx_sf, id_lwuflxc_sf, id_lwdflxc_sf
    integer, save :: id_lwuflx_lv, id_lwdflx_lv, id_lwuflxc_lv, id_lwdflxc_lv, id_lwhr, id_lwhrc

    integer, save :: id_net_flx_top, id_net_flx_srf

    integer, save :: rad_interval      ! Radiation interval related to physical processes

    integer, save :: inflag  = 0       ! Flag for cloud optical properties
    integer, save :: iceflag = 0       ! Flag for ice particle specification
    integer, save :: liqflag = 0       ! Flag for liquid droplet specification

    integer, save :: id_ccosz, id_ttaud

    integer, public :: nxsw

    public :: next_radiation

!-----------------------------------------------------------------------
! Select parameterization of cloud ice and liquid optical depths
! Use CAM shortwave cloud optical properties directly

!   inflag  = 0
!   iceflag = 0
!   liqflag = 0

! Use E&C param for ice to mimic CAM3 for now

!   inflag  = 2 
!   iceflag = 1
!   liqflag = 1

! Use merged Fu and E&C params for ice 

!   inflag  = 2 
!   iceflag = 3
!   liqflag = 1

    public :: raduk, rad_ukmo, rad_rrtmg, raduk_init

    character(len = 8) :: rad_scheme = "RRTMG"

contains

!=======================================================================

subroutine next_radiation(na)

    implicit none

    integer, intent(in), optional :: na

    integer :: nstep

    logical :: dosw

!-----------------------------------------------------------------------
! Next Radiation time step setting

    dosw  = .false.
    if (present(na)) then
        nstep = na
    else
        nstep = 1
    end if
    nxsw  = 0
    do while (.not. dosw)
        nstep = nstep + 1
        nxsw  = nxsw  + 1
        if (rad_interval .eq. 0) then
            dosw = .true.
        else if (mod(nstep - 1, rad_interval) .eq. 0) then
            dosw = .true.
        end if
    end do

end subroutine next_radiation

!=======================================================================

subroutine raduk(na, ng, nq, nwat, is, ie, js, je, npz, ak, bk, ts, pt, &
                 q, ua, va, pdt, zmd, grid, pmd, pit, delp, Time, ozone,&
                 aero_lev, aerosols, tau, tau_w, tau_w_g, tau_w_f,      &
                 odap_aer, cloudc, maskout, net_flx, net_sw, ttauda,    &
                 direct_vis, direct_nir, diffuse_vis, diffuse_nir,      &
                 asdir, aldir, asdif, aldif, awdij, t_dt, radheat,      &
                 do_APE, qliqc, qicec, cfrac, effr, cld_sw_067,         &
!nian            cld_lw_105, no_cpl)
                 cld_lw_105, lwhr, no_cpl)

    use fms_mod,             only: error_mesg, FATAL
    use ozcmip_mod,          only: ozcmipset, ozcmip_init
    use prescribed_aerosols, only: a3z, naer

    implicit none 

!----------------------------------------------------------------------
! Dummy arguments

    integer, intent(in)    :: na                                            ! Current time step
    integer, intent(in)    :: ng                                            ! Number of ghost zones required
    integer, intent(in)    :: nq                                            ! Total number of tracers
    integer, intent(in)    :: is, ie, js, je                                ! Longitude and Latitude index 
    integer, intent(in)    :: npz                                           ! Number of vertical levels 
    integer, intent(in)    :: nwat                                          ! Number of water species

    integer, intent(in)    :: maskout(is:ie, js:je)                         ! Land sea ice mask(1: land, 0: sea, -1: sea ice)

    real,    intent(in)    :: delp(is-ng:ie+ng, js-ng:je+ng, npz)           ! Pressure thichness (Pa)

	real,    intent(in)    :: pdt                                           ! Time step

	real,    intent(in)    :: ak(1:npz+1), bk(1:npz+1)                      ! Sigma-p coordinate parameters 

	real,    intent(in)    :: pt(is-ng:ie+ng, js-ng:je+ng, npz)             ! Temperature (K)
	real,    intent(in)    :: q (is-ng:ie+ng, js-ng:je+ng, npz, nq)         ! Specific humidity and constituents

	real,    intent(in)    :: ua(is-ng:ie+ng, js-ng:je+ng, npz)             ! A grid zonal wind (m/s)
	real,    intent(in)    :: va(is-ng:ie+ng, js-ng:je+ng, npz)             ! A grid meridional wind (m/s)

	real,    intent(inout) :: ozone(is:ie, js:je, npz)                      ! CMIP5 Ozone (kg/kg)
	real,    intent(inout) :: aero_lev(a3z)                                 ! CMIP5 Aerosol Level (Pa)
	real,    intent(inout) :: aerosols(is:ie, js:je, a3z, 12, naer)         ! CMIP5 Aerosol (kg/kg)

    real,    intent(inout) :: tau     (is:ie, js:je, npz, nswbands)         ! Aerosol extinction optical depth
    real,    intent(inout) :: tau_w   (is:ie, js:je, npz, nswbands)         ! Aerosol single scattering albedo
    real,    intent(inout) :: tau_w_g (is:ie, js:je, npz, nswbands)         ! Aerosol assymetry parameter
    real,    intent(inout) :: tau_w_f (is:ie, js:je, npz, nswbands)         ! Aerosol forward scattered fraction
    real,    intent(inout) :: odap_aer(is:ie, js:je, npz, nlwbands)         ! Absorption optical depth, per layer (1)

	real,    intent(in)    :: zmd(is:ie, js:je, npz)                        ! Height at layer midpoints above the surface (m)

	real,    intent(in)    :: pmd(is:ie, js:je, npz)                        ! Pressure at layer midpoints (pa)
	real,    intent(in)    :: pit(is:ie, js:je, npz+1)                      ! Pressure at interface points (pa)

	real,    intent(in)    :: ts(is:ie, js:je)                              ! Skin temperature

	real,    intent(in)    :: grid(is-ng:ie+ng, js-ng:je+ng, 1:2)           ! Grid latlon information

	real,    intent(in)    :: cloudc(is:ie, js:je, npz)                     ! Total cloud cover fraction(2-D)

    real,    intent(in)    :: qliqc(is:ie, js:je, npz)                      ! Cloud liquid water content (g/m**3)
    real,    intent(in)    :: qicec(is:ie, js:je, npz)                      ! Cloud ice water content (g/m**3)
    real,    intent(in)    :: cfrac(is:ie, js:je, npz)                      ! Cloud fraction (1)

    real,    intent(in)    :: effr(is:ie, js:je, npz, 2)                    ! Liquid and ice effective drop size (micron)

    type(time_type), intent(in) :: Time                                     ! Current model time

	real,    intent(inout) :: t_dt(is:ie, js:je, npz)                       ! T tendencies

	real,    intent(inout) :: radheat(is:ie, js:je, npz)                    ! Radiation Heating Ratio (K/s)

	real,    intent(out) :: ttauda(is:ie, js:je)                            ! Fraction of day (or of timestep) that sun is above horizon

	real,    intent(inout) :: net_flx(is:ie, js:je)                         ! Net fluxes from the radiation
	real,    intent(inout) :: net_sw (is:ie, js:je)                         ! Net surface SW fluxes from the radiation
	real,    intent(inout) :: awdij  (is:ie, js:je)                         ! Diagnostic downwards longwave flux at the surface

	real,    intent(inout) :: direct_vis (is:ie, js:je)                     ! Visible downward short wave beam at ground
	real,    intent(inout) :: direct_nir (is:ie, js:je)                     ! Near infrared downward short wave beam at ground
	real,    intent(inout) :: diffuse_vis(is:ie, js:je)                     ! Visible downward short wave diffuse at ground
	real,    intent(inout) :: diffuse_nir(is:ie, js:je)                     ! Near infrared downward short wave diffuse at ground

	real,    intent(inout) :: asdir(is:ie, js:je)                           ! Surface albedo for direct radiation   0.2-0.7 micrometers
	real,    intent(inout) :: aldir(is:ie, js:je)                           ! Surface albedo for direct radiation   0.7-5.0 micrometers
	real,    intent(inout) :: asdif(is:ie, js:je)                           ! Surface albedo for diffuse radiation   0.2-0.7 micrometers
	real,    intent(inout) :: aldif(is:ie, js:je)                           ! Surface albedo for diffuse radiation   0.7-5.0 micrometers

	logical, intent(in)    :: do_APE                                        ! Whether do Aqua Planet Experiment
    logical, intent(in)    :: no_cpl                                        ! No Coupling

!-----------------------------------------------------------------------
! For COSP

	real,    intent(out) :: cld_sw_067(is:ie, js:je, npz)                   ! mean 0.67 micron optical depth of cloud
	real,    intent(out) :: cld_lw_105(is:ie, js:je, npz)                   ! 10.5 micron longwave emissivity of stratiform cloud

!----------------------------------------------------------------------
! Outpute variables
!nian
    
    real,    intent(out) :: lwhr(is:ie, js:je, npz)                         !Total sky longwave radiative heating rate (K/d) 


!----------------------------------------------------------------------
! Local variables

    integer :: i, j, k, l

	real, dimension(is:ie, js:je) :: ccosz                                  ! Solar zenith angle
	real :: ddmsqi                                                          ! Correct the distance of between the sun and earth

    integer :: nar                                                          ! Time interval of the calculation of radiation
    integer :: unit, io, ierr                                               ! Open and read namelist file
    integer :: date(6), ncsec, days

    real :: calday

	logical :: do_rad                                                       ! Whether do radiation
	logical :: used

    integer :: nstep

    logical :: dosw

!-----------------------------------------------------------------------
! Radiation time step setting

    nar = rad_interval

    if (mod(nar, 2) .ne. 0 ) then
        call error_mesg ('subroutine raduk', 'nar is an odd, but it needs an even!', FATAL)
    end if

    if (nar .eq. 0) then
        do_rad = .true.
    else if (mod(na - 1, nar) .eq. 0) then
        do_rad = .true.
    else
        do_rad = .false.
    end if

    call next_radiation(na)

!-----------------------------------------------------------------------
! Initial radiation, get ccosz, ttauda, ddmsqi and call radiation main program

    if (do_rad) then

        if (na .eq. 1) then
            call ozcmip_init
            if (trim(rad_scheme) .eq. "UKMO") then
                call inirad0(ng, is, ie, js, je, npz, pdt, grid, ak, bk)
            else if (trim(rad_scheme) .eq. "RRTMG") then
                call rrtmg_sw_ini(cpair)
                call rrtmg_lw_ini(cpair)
            else
                call error_mesg ('raduk', 'There is no scheme named: '//trim(rad_scheme), FATAL)
            end if
        end if

        call ozcmipset(ng, is, ie, js, je, npz, ak, bk, pit, grid, ozone, Time, pdt, do_APE)

        call cdcosz(nar, pdt, ng, is, ie, js, je, grid, Time, ccosz, ttauda, ddmsqi, do_APE)

        if (id_ccosz > 0) used = send_data(id_ccosz, ccosz (is:ie, js:je), Time)
        if (id_ttaud > 0) used = send_data(id_ttaud, ttauda(is:ie, js:je), Time)

        if (trim(rad_scheme) .eq. "UKMO") then

            call rad_ukmo(ng, nq, nwat, is, ie, js, je, npz, ts, pt, q,      &
                          ua, va, zmd, grid, pmd, pit, ozone, ccosz, ttauda, &
                          ddmsqi, cloudc, maskout, net_flx, net_sw, Time,    &
                          radheat, direct_vis, direct_nir, diffuse_vis,      &
                          diffuse_nir, asdir, aldir, asdif, aldif, awdij,    &
                          qliqc, qicec, cfrac, effr, ak, bk, no_cpl)

        else if (trim(rad_scheme) .eq. "RRTMG") then

            if (nwat .eq. 6) then
            call rad_rrtmg(is, ie, js, je, ng, nq, npz, pt, q, delp, pmd,    &
                           zmd, pit, maskout, ts, pdt, asdir, asdif, aldir,  &
                           aldif, ccosz*ttauda, ddmsqi, ozone, cfrac, effr,  &
                           tau, tau_w, tau_w_g, tau_w_f, odap_aer, awdij,    &
                           radheat, direct_vis, direct_nir, diffuse_vis,     &
                           diffuse_nir, net_flx, net_sw, cld_sw_067,         &
!nian                      cld_lw_105, Time, no_cpl)
                           cld_lw_105, Time, lwhr, no_cpl)
   
            else
            call error_mesg ('subroutine raduk', 'To use RRTMG, nwat should be 6!', FATAL)
            end if

        else
            call error_mesg ('raduk', 'There is no scheme named: '//trim(rad_scheme), FATAL)

        end if

    end if  ! if rad end

    t_dt = radheat

    if (id_ozone > 0) used = send_data(id_ozone, ozone(is:ie, js:je, :), Time)

    call get_calendar(Time, date, ncsec, days, calday)

    if (id_so4    > 0) used = send_data(id_so4,    aerosols(is:ie, js:je, :, date(2), 1 ), Time)
    if (id_soa    > 0) used = send_data(id_soa,    aerosols(is:ie, js:je, :, date(2), 1 ), Time)
    if (id_cb1    > 0) used = send_data(id_cb1,    aerosols(is:ie, js:je, :, date(2), 11), Time)
    if (id_cb2    > 0) used = send_data(id_cb2,    aerosols(is:ie, js:je, :, date(2), 13), Time)
    if (id_oc1    > 0) used = send_data(id_oc1,    aerosols(is:ie, js:je, :, date(2), 10), Time)
    if (id_oc2    > 0) used = send_data(id_oc2,    aerosols(is:ie, js:je, :, date(2), 12), Time)
    if (id_volc   > 0) used = send_data(id_volc,   aerosols(is:ie, js:je, :, date(2), 14), Time)
    if (id_dst01  > 0) used = send_data(id_dst01,  aerosols(is:ie, js:je, :, date(2), 6 ), Time)
    if (id_dst02  > 0) used = send_data(id_dst02,  aerosols(is:ie, js:je, :, date(2), 7 ), Time)
    if (id_dst03  > 0) used = send_data(id_dst03,  aerosols(is:ie, js:je, :, date(2), 8 ), Time)
    if (id_dst04  > 0) used = send_data(id_dst04,  aerosols(is:ie, js:je, :, date(2), 9 ), Time)
    if (id_sslt01 > 0) used = send_data(id_sslt01, aerosols(is:ie, js:je, :, date(2), 2 ), Time)
    if (id_sslt02 > 0) used = send_data(id_sslt02, aerosols(is:ie, js:je, :, date(2), 3 ), Time)
    if (id_sslt03 > 0) used = send_data(id_sslt03, aerosols(is:ie, js:je, :, date(2), 4 ), Time)
    if (id_sslt04 > 0) used = send_data(id_sslt04, aerosols(is:ie, js:je, :, date(2), 5 ), Time)

end subroutine raduk 

!=======================================================================

subroutine rad_ukmo(ng, nq, nwat, is, ie, js, je, npz, ts, pt, q,      &
                    ua, va, zmd, grid, pmd, pit, ozone, ccosz, ttauda, &
                    ddmsqi, cloudc, maskout, net_flx, net_sw, Time,    &
                    radheat, direct_vis, direct_nir, diffuse_vis,      &
                    diffuse_nir, asdir, aldir, asdif, aldif, awdij,    &
                    qliqc, qicec, cfrac, effr, ak, bk, no_cpl)

	use diag_manager_mod, only: send_data
	use physconst_mod,    only: gravit, pi

	implicit none 

!----------------------------------------------------------------------
! Dummy arguments

	integer, intent(in)    :: ng                                              ! Number of ghost zones required
	integer, intent(in)    :: nq                                              ! Total number of tracers
	integer, intent(in)    :: is, ie, js, je                                  ! Longitude and Latitude index 
	integer, intent(in)    :: npz                                             ! Number of vertical levels 
    integer, intent(in)    :: nwat                                            ! Number of water species

	integer, intent(in)    :: maskout(is:ie, js:je)                           ! Land sea ice mask(1: land, 0: sea, -1: sea ice)

	real,    intent(in)    :: pt  (is-ng:ie+ng, js-ng:je+ng, npz)             ! Temperature (K)
	real,    intent(in)    :: q   (is-ng:ie+ng, js-ng:je+ng, npz, nq)         ! Specific humidity and constituents

	real,    intent(in)    :: ua  (is-ng:ie+ng, js-ng:je+ng, npz)             ! A grid zonal wind (m/s)
	real,    intent(in)    :: va  (is-ng:ie+ng, js-ng:je+ng, npz)             ! A grid meridional wind (m/s)

	real,    intent(in)    :: ozone(is:ie, js:je, npz)                        ! CMIP5 Ozone (kg/kg)

	real,    intent(in)    :: zmd(is:ie, js:je, npz)                          ! Height at layer midpoints above the surface (m)

	real,    intent(in)    :: pmd(is:ie, js:je, npz)                          ! Pressure at layer midpoints (pa)
	real,    intent(in)    :: pit(is:ie, js:je, npz+1)                        ! Pressure at interface points (pa)

	real,    intent(in)    :: ts (is:ie, js:je)                               ! Skin temperature

	real,    intent(in)    :: ak(1:npz+1), bk(1:npz+1)                        ! Sigma-p coordinate parameters 

	real,    intent(in)    :: ccosz (is:ie, js:je)                            ! Solar zenith angle
	real,    intent(in)    :: ttauda(is:ie, js:je)                            ! Fraction of day (or of timestep) that sun is above horizon
	real,    intent(in)    :: ddmsqi                                          ! Correct the distance of between the sun and earth

	real,    intent(in)    :: grid(is-ng:ie+ng, js-ng:je+ng, 1:2)             ! Grid latlon information

	real,    intent(in)    :: cloudc(is:ie, js:je, npz)                       ! Total cloud cover fraction(2-D)

    real,    intent(in)    :: qliqc(is:ie, js:je, npz)                        ! Cloud liquid water content (g/m**3)
    real,    intent(in)    :: qicec(is:ie, js:je, npz)                        ! Cloud ice water content (g/m**3)
    real,    intent(in)    :: cfrac(is:ie, js:je, npz)                        ! Cloud fraction (1)

    real,    intent(in)    :: effr(is:ie, js:je, npz, 2)                      ! Liquid and ice effective drop size (micron)

	type(time_type), intent(in) :: Time                                       ! Current model time

	real,    intent(inout) :: radheat(is:ie, js:je, npz)                      ! Radiation Heating Ratio (K/s)

	real,    intent(inout) :: net_flx    (is:ie, js:je)                       ! Net fluxes from the radiation
	real,    intent(inout) :: net_sw     (is:ie, js:je)                       ! Net surface SW fluxes from the radiation
	real,    intent(inout) :: awdij      (is:ie, js:je)                       ! Surface downwelling longwave radiation (W/m^2)

	real,    intent(inout) :: direct_vis (is:ie, js:je)                       ! Visible downward short wave beam at ground
	real,    intent(inout) :: direct_nir (is:ie, js:je)                       ! Near infrared downward short wave beam at ground
	real,    intent(inout) :: diffuse_vis(is:ie, js:je)                       ! Visible downward short wave diffuse at ground
	real,    intent(inout) :: diffuse_nir(is:ie, js:je)                       ! Near infrared downward short wave diffuse at ground

	real,    intent(inout) :: asdir(is:ie, js:je)                             ! Surface albedo for direct radiation   0.2-0.7 micrometers
	real,    intent(inout) :: aldir(is:ie, js:je)                             ! Surface albedo for direct radiation   0.7-5.0 micrometers
	real,    intent(inout) :: asdif(is:ie, js:je)                             ! Surface albedo for diffuse radiation   0.2-0.7 micrometers
	real,    intent(inout) :: aldif(is:ie, js:je)                             ! Surface albedo for diffuse radiation   0.7-5.0 micrometers

    logical, intent(in)    :: no_cpl                                          ! No Coupling

!----------------------------------------------------------------------
! Local variables

	integer :: i,  j,  k

	real :: along                                                          ! Longitude in degrees of each grid point 
	real :: tsg

	real, dimension(npz)   :: qcg, qig                                     ! Cloud liquid/ice water mixing rate
	real, dimension(npz)   :: geopot                                       ! Geopotential
	real, dimension(npz)   :: tg                                           ! Temperature
	real, dimension(npz)   :: qg                                           ! Constituent mixing ratio field
	real, dimension(npz)   :: ug                                           ! Zonal wind
	real, dimension(npz)   :: vg                                           ! Meridional wind
	real, dimension(npz)   :: hypmx                                        ! Pressures at model levels
	real, dimension(npz+1) :: hypix                                        ! Pressure at model interfaces
	real, dimension(npz)   :: qo3                                          ! Mass mixing ratio (g/g) of O3 at model data lvls.

	integer, dimension(is:ie, js:je) :: msklnd                             ! Land mask
	integer, dimension(is:ie, js:je) :: msksea                             ! Sea mask
	integer, dimension(is:ie, js:je) :: msksic                             ! Ice mask

	real, dimension(is:ie, js:je, 2) :: grid0                              ! Grid in degree format
	real, dimension(is:ie, js:je, 2) :: grid1                              ! Grid in radian format

!----------------------------------------------------------------------
! Diagnostic arguments

	real, dimension(npz) :: lwheat                                  ! Net LW heating rates
	real, dimension(npz) :: swheat                                  ! Net SW heating rates

	real, dimension(is:ie, js:je) :: clwvi                                 ! Vertically integerated cloud liquid water path
	real, dimension(is:ie, js:je) :: clivi                                 ! Vertically integerated cloud ice water path

	real, dimension(is:ie, js:je) :: aero_vistau                           ! Aerosol optical depth/thickness at visible band (AOD)
	real, dimension(is:ie, js:je) :: aero_irtau                            ! Aerosol optical depth/thickness at infra band (AOD)

	real, dimension(is:ie, js:je) :: aoddust                               ! Total dust optical depth
	real, dimension(is:ie, js:je) :: aoddust1                              ! Total dust optical depth at ultra band
	real, dimension(is:ie, js:je) :: aoddust2                              ! Total dust optical depth at visible band
	real, dimension(is:ie, js:je) :: aoddust3                              ! Total dust optical depth at infra band

	real, dimension(is:ie, js:je) :: netnj                                 ! Net radiative flux into surface (W/m^2)
	real, dimension(is:ie, js:je) :: netij                                 ! Net radiative flux into TOA (W/m^2)

	real, dimension(is:ie, js:je) :: dwdnj                                 ! Surface downwelling longwave radiation (W/m^2)
                                                                           ! When "cldfor" is true, which consider both cloudy and clear sky, dwdnj = awdij.

	real, dimension(is:ie, js:je) :: cwdij                                 ! Surface incident shortwave radiation (W/m^2) 
	real, dimension(is:ie, js:je) :: cwuij                                 ! Surface reflected shortwave radiation (W/m^2) 
!	real, dimension(is:ie, js:je) :: awdij                                 ! Surface downwelling longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: awuij                                 ! Surface upwelling longwave radiation (W/m^2)

	real, dimension(is:ie, js:je) :: srtij                                 ! TOA incident shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: sreij                                 ! TOA reflected shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: awtij                                 ! TOA incoming Longwave Radiation (W/m^2)
	real, dimension(is:ie, js:je) :: aweij                                 ! TOA outgoing Longwave Radiation (W/m^2)

	real, dimension(is:ie, js:je) :: sswdn                                 ! Surface incident clear-sky shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: sswup                                 ! Surface reflected clear-sky shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: slwdn                                 ! Surface downwelling clear-sky longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: slwup                                 ! Surface upwelling clear-sky longwave radiation (W/m^2)

	real, dimension(is:ie, js:je) :: toaswd                                ! TOA incident clear-sky shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: toaswu                                ! TOA reflected clear-sky shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: toalwd                                ! TOA downwelling clear-sky longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: toalwu                                ! TOA upwelling clear-sky longwave radiation (W/m^2)

	real, dimension(is:ie, js:je) :: swcf                                  ! Shortwave Cloud Forcing
	real, dimension(is:ie, js:je) :: lwcf                                  ! Longwave Cloud Forcing

	real, dimension(is:ie, js:je, npz) :: clw                              ! Cloud liquid water path [g/m^2]
	real, dimension(is:ie, js:je, npz) :: cli                              ! Cloud ice water path [g/m^2]

	real, dimension(is:ie, js:je, npz) :: qrl                              ! Net LW heating rates
	real, dimension(is:ie, js:je, npz) :: qrs                              ! Net SW heating rates

	real, dimension(is:ie, js:je, npz) :: cldliq                           ! Cloud liquid water mixing rate
	real, dimension(is:ie, js:je, npz) :: cldice                           ! Cloud ice water mixing rate

	real, dimension(is:ie, js:je, npz, 2) :: eff                           ! Liquid and ice effective drop size (micron)

	real, dimension(is:ie, js:je) :: net_flx_top                           ! Net fluxes from the radiation at model top
	real, dimension(is:ie, js:je) :: net_flx_srf                           ! Net fluxes from the radiation at model bottom

!-----------------------------------------------------------------------
! Standard name of radiation variables

	real, dimension(is:ie, js:je) :: rsds                                  ! Surface downwelling shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rsus                                  ! Surface upwelling shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rsns                                  ! Surface net shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rlds                                  ! Surface downwelling longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rlus                                  ! Surface upwelling longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rlns                                  ! Surface net longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rds                                   ! Surface downwelling radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rus                                   ! Surface upwelling radiation (W/m^2)

	real, dimension(is:ie, js:je) :: rsdt                                  ! TOA downwelling shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rsut                                  ! TOA upwelling shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rsnt                                  ! TOA net shortwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rldt                                  ! TOA downwelling longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rlut                                  ! TOA upwelling longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rlnt                                  ! TOA net longwave radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rdt                                   ! TOA downwelling radiation (W/m^2)
	real, dimension(is:ie, js:je) :: rut                                   ! TOA upwelling radiation (W/m^2)

	real, dimension(is:ie, js:je) :: rsdscs                                ! Clear-sky surface downwelling shortwave ra	diation (W/m^2)
	real, dimension(is:ie, js:je) :: rsuscs                                ! Clear-sky surface upwelling shortwave radi	on (W/m^2)
	real, dimension(is:ie, js:je) :: rsnscs                                ! Clear-sky surface net shortwave radiation 	(W/m^2)
	real, dimension(is:ie, js:je) :: rldscs                                ! Clear-sky surface downwelling longwave rad	(W/m^2)
	real, dimension(is:ie, js:je) :: rluscs                                ! Clear-sky surface upwelling longwave radia	(W/m^2)
	real, dimension(is:ie, js:je) :: rlnscs                                ! Clear-sky surface net longwave radiation (	W/m^2)
	real, dimension(is:ie, js:je) :: rdscs                                 ! Clear-sky surface downwelling radiation (W	/m^2)
	real, dimension(is:ie, js:je) :: ruscs                                 ! Clear-sky surface upwelling radiation (W/m	^2)

	real, dimension(is:ie, js:je) :: rsdtcs                                ! Clear-sky TOA downwelling shortwave radiat	ion (W/m^2)
	real, dimension(is:ie, js:je) :: rsutcs                                ! Clear-sky TOA upwelling shortwave radiatio	n (W/m^2)
	real, dimension(is:ie, js:je) :: rsntcs                                ! Clear-sky TOA net shortwave radiation (W/m	^2)
	real, dimension(is:ie, js:je) :: rldtcs                                ! Clear-sky TOA downwelling longwave radiati	on (W/m^2)
	real, dimension(is:ie, js:je) :: rlutcs                                ! Clear-sky TOA upwelling longwave radiation	 (W/m^2)
	real, dimension(is:ie, js:je) :: rlntcs                                ! Clear-sky TOA net longwave radiation (W/m^	2)
	real, dimension(is:ie, js:je) :: rdtcs                                 ! Clear-sky TOA downwelling radiation (W/m^2	)
	real, dimension(is:ie, js:je) :: rutcs                                 ! Clear-sky TOA upwelling radiation (W/m^2) 

	logical :: used

!-----------------------------------------------------------------------
! Heat forcing

	real :: deg2rad, rad2deg
	real :: sigx, sigy, sigz
	real :: xc, yc, zc, lon1, lon2, lat1, lat2, lev1, lev2
	real :: lev, lat, lon
	real :: amp, heat_max
	real :: wgt(is:ie, js:je, 1:npz)

	deg2rad = pi / 180.0
	rad2deg = 180.0 / pi

	grid0 = grid(is:ie, js:je, 1:2) / pi * 180.0
	grid1 = grid(is:ie, js:je, 1:2)

	awtij  = 0.0

	do j = js, je
	do i = is, ie

		geopot = zmd(i, j, npz:1:-1) * gravit

		tsg = ts(i, j) 
		tg  = pt(i, j, npz:1:-1) 
		qg  = q (i, j, npz:1:-1, 1) 
		ug  = ua(i, j, npz:1:-1) 
		vg  = va(i, j, npz:1:-1) 

		hypmx  = pmd(i, j, npz:1:-1)
		hypix  = pit(i, j, npz+1:1:-1)

		select case (maskout(i,j))
			case (1)
				msklnd(i,j) = 1
				msksea(i,j) = 0
				msksic(i,j) = 0
			case (0)
				msklnd(i,j) = 0
				msksea(i,j) = 1
				msksic(i,j) = 0
			case (-1)
				msklnd(i,j) = 0
				msksea(i,j) = 0
				msksic(i,j) = 1
		end select

		cldliq(i, j, :) = 0.0
		cldice(i, j, :) = 0.0

		along = grid0(i, j, 1)

		qo3 = ozone(i, j, :)

        if (no_cpl) then
		    call albocean0(1, 1, asdir(i,j), asdif(i,j), aldir(i,j), aldif(i,j), msklnd(i,j), msksic(i,j), ccosz(i,j))
        end if

        if (nwat .eq. 3) then

		call raddrv0(1, 1, 1, npz, i, j, nwat, along, msklnd(i,j), msksea(i,j), msksic(i,j), tsg, tg, qg, geopot, ug, vg,       &
                     cldliq(i,j,:), cldice(i,j,:), cloudc(i,j,:), eff(i,j,:,:), clw(i,j,:), cli(i,j,:), clwvi(i,j), clivi(i,j), &  ! different
                     aweij(i,j), aero_vistau(i,j), aero_irtau(i,j), srtij(i,j), sreij(i,j), awuij(i,j), awdij(i,j), lwheat,     &
                     swheat, cwdij(i,j), dwdnj(i,j), netnj(i,j), cwuij(i,j), direct_vis(i,j), direct_nir(i,j), diffuse_vis(i,j),&
                     diffuse_nir(i,j), asdir(i,j), asdif(i,j), aldir(i,j), aldif(i,j), ccosz(i,j), ttauda(i,j), ddmsqi, hypix,  &
                     hypmx, 1, 1, npz, npz+1, qo3, grid1(i,j,:), sswdn(i,j), sswup(i,j), slwdn(i,j), toaswu(i,j), toaswd(i,j),  &
                     toalwu(i,j), cloudc(i,j,:))

        else if (nwat .eq. 6) then

		call raddrv0(1, 1, 1, npz, i, j, nwat, along, msklnd(i,j), msksea(i,j), msksic(i,j), tsg, tg, qg, geopot, ug, vg,       &
                     qliqc(i,j,:), qicec(i,j,:), cfrac(i,j,:), effr(i,j,:,:), clw(i,j,:), cli(i,j,:), clwvi(i,j), clivi(i,j),   &  ! different
                     aweij(i,j), aero_vistau(i,j), aero_irtau(i,j), srtij(i,j), sreij(i,j), awuij(i,j), awdij(i,j), lwheat,     &
                     swheat, cwdij(i,j), dwdnj(i,j), netnj(i,j), cwuij(i,j), direct_vis(i,j), direct_nir(i,j), diffuse_vis(i,j),&
                     diffuse_nir(i,j), asdir(i,j), asdif(i,j), aldir(i,j), aldif(i,j), ccosz(i,j), ttauda(i,j), ddmsqi, hypix,  &
                     hypmx, 1, 1, npz, npz+1, qo3, grid1(i,j,:), sswdn(i,j), sswup(i,j), slwdn(i,j), toaswu(i,j), toaswd(i,j),  &
                     toalwu(i,j), cloudc(i,j,:))

        end if

		radheat(i, j, :) = lwheat + swheat
		net_flx(i, j) = (srtij(i, j) - sreij(i, j)) + (awtij(i, j) - aweij(i, j)) &
                      + (awuij(i, j) - awdij(i, j)) + (cwuij(i, j) - cwdij(i, j))

		net_flx_top(i, j) = (srtij(i, j) - sreij(i, j)) + (awtij(i, j) - aweij(i, j))
		net_flx_srf(i, j) = (awuij(i, j) - awdij(i, j)) + (cwuij(i, j) - cwdij(i, j))

		net_sw(i, j) = cwdij(i, j) - cwuij(i, j)

		qrl(i, j, :) = lwheat
		qrs(i, j, :) = swheat

	end do
	end do

!-----------------------------------------------------------------------
! Heat Forcing

!#define Heat_forcing
#ifdef Heat_forcing

	sigx = 1.20
	sigy = 0.14
	sigz = 0.40

	xc = 180.0    ! E
	yc = 45.0     ! N
	zc = 1000.0   ! Pa

	lon1 = 0.0      ! E
	lon2 = 360.0    ! E
	lat1 = 25.0     ! N
	lat2 = 65.0     ! N
	lev1 = 100.0    ! Pa
	lev2 = 10000.0  ! Pa

	amp = 0.35

	heat_max = maxval(qrl+qrs)

	do k = 1, npz
		do j = js, je
			do i = is, ie

				lev = (ak(k) + ak(k + 1)) / 2 + (bk(k) + bk(k + 1)) / 2 * 1.0E5
				lon = grid(i,j,1) * rad2deg
				lat = grid(i,j,2) * rad2deg

				if (lev .gt. lev1 .and. lev .lt. lev2 .and. lat .gt. lat1 .and. lat .lt. lat2 .and. lon .gt. lon1 .and. lon .lt. lon2) then
					wgt(i, j, k) = amp * 1.0 / (2 * pi * sigy * sigz) *                         &
                                   & exp(- 1.0 / 2.0 * (((lat - yc) * deg2rad / sigy) ** 2 +    &
                                   & ((log10(lev) - log10(zc)) / sigz) ** 2)) * sin(2 * lon * deg2rad)
				else
					wgt(i, j, k) = 0.0
				end if

			end do
		end do
	end do

!	qrl = qrl + qrl * wgt
!	qrs = qrs + qrs * wgt
	qrs = qrs + 2.0/86400 * wgt

	radheat = qrs + qrl

#endif

!-----------------------------------------------------------------------

	slwup  = awuij
	toalwd = awtij
	toaswd = srtij

	netij = srtij + awtij - sreij - aweij

!-----------------------------------------------------------------------
! From non-standard name to standard name

	rsds = cwdij
	rsus = cwuij
	rsns = rsds - rsus
	rlds = awdij
	rlus = awuij
	rlns = rlus - rlds
	rds  = rsds + rlds
	rus  = rsus + rlus

	rsdt = srtij
	rsut = sreij
	rsnt = rsdt - rsut
	rldt = awtij
	rlut = aweij
	rlnt = rlut - rldt
	rdt  = rsdt + rldt
	rut  = rsut + rlut

	rsdscs = sswdn
	rsuscs = sswup
	rsnscs = rsdscs - rsuscs
	rldscs = slwdn
	rluscs = slwup
	rlnscs = rluscs - rldscs
	rdscs  = rsdscs + rldscs
	ruscs  = rsuscs + rluscs

	rsdtcs = toaswd
	rsutcs = toaswu
	rsntcs = rsdtcs - rsutcs
	rldtcs = toalwd
	rlutcs = toalwu
	rlntcs = rlutcs - rldtcs
	rdtcs  = rsdtcs + rldtcs
	rutcs  = rsutcs + rlutcs

	swcf = rsnt - rsntcs
	lwcf = rlntcs - rlnt

!-----------------------------------------------------------------------
! Aerosol optical dept

    aoddust1 = 0.0
    aoddust2 = aero_vistau
    aoddust3 = aero_irtau
    aoddust  = aoddust1 + aoddust2 + aoddust3

!-----------------------------------------------------------------------
! Standard output for AMIP II

	if(id_rsds   > 0) used = send_data(id_rsds,   rsds  (is:ie, js:je), Time)
	if(id_rsus   > 0) used = send_data(id_rsus,   rsus  (is:ie, js:je), Time)
	if(id_rsns   > 0) used = send_data(id_rsns,   rsns  (is:ie, js:je), Time)
	if(id_rlds   > 0) used = send_data(id_rlds,   rlds  (is:ie, js:je), Time)
	if(id_rlus   > 0) used = send_data(id_rlus,   rlus  (is:ie, js:je), Time)
	if(id_rlns   > 0) used = send_data(id_rlns,   rlns  (is:ie, js:je), Time)
	if(id_rds    > 0) used = send_data(id_rds,    rds   (is:ie, js:je), Time)
	if(id_rus    > 0) used = send_data(id_rus,    rus   (is:ie, js:je), Time)

	if(id_rsdt   > 0) used = send_data(id_rsdt,   rsdt  (is:ie, js:je), Time)
	if(id_rsut   > 0) used = send_data(id_rsut,   rsut  (is:ie, js:je), Time)
	if(id_rsnt   > 0) used = send_data(id_rsnt,   rsnt  (is:ie, js:je), Time)
	if(id_rldt   > 0) used = send_data(id_rldt,   rldt  (is:ie, js:je), Time)
	if(id_rlut   > 0) used = send_data(id_rlut,   rlut  (is:ie, js:je), Time)
	if(id_rlnt   > 0) used = send_data(id_rlnt,   rlnt  (is:ie, js:je), Time)
	if(id_rdt    > 0) used = send_data(id_rdt,    rdt   (is:ie, js:je), Time)
	if(id_rut    > 0) used = send_data(id_rut,    rut   (is:ie, js:je), Time)

	if(id_rsdscs > 0) used = send_data(id_rsdscs, rsdscs(is:ie, js:je), Time)
	if(id_rsuscs > 0) used = send_data(id_rsuscs, rsuscs(is:ie, js:je), Time)
	if(id_rsnscs > 0) used = send_data(id_rsnscs, rsnscs(is:ie, js:je), Time)
	if(id_rldscs > 0) used = send_data(id_rldscs, rldscs(is:ie, js:je), Time)
	if(id_rluscs > 0) used = send_data(id_rluscs, rluscs(is:ie, js:je), Time)
	if(id_rlnscs > 0) used = send_data(id_rlnscs, rlnscs(is:ie, js:je), Time)
	if(id_rdscs  > 0) used = send_data(id_rdscs,  rdscs (is:ie, js:je), Time)
	if(id_ruscs  > 0) used = send_data(id_ruscs,  ruscs (is:ie, js:je), Time)

	if(id_rsdtcs > 0) used = send_data(id_rsdtcs, rsdtcs(is:ie, js:je), Time)
	if(id_rsutcs > 0) used = send_data(id_rsutcs, rsutcs(is:ie, js:je), Time)
	if(id_rsntcs > 0) used = send_data(id_rsntcs, rsntcs(is:ie, js:je), Time)
	if(id_rldtcs > 0) used = send_data(id_rldtcs, rldtcs(is:ie, js:je), Time)
	if(id_rlutcs > 0) used = send_data(id_rlutcs, rlutcs(is:ie, js:je), Time)
	if(id_rlntcs > 0) used = send_data(id_rlntcs, rlntcs(is:ie, js:je), Time)
	if(id_rdtcs  > 0) used = send_data(id_rdtcs,  rdtcs (is:ie, js:je), Time)
	if(id_rutcs  > 0) used = send_data(id_rutcs,  rutcs (is:ie, js:je), Time)

	if(id_qrl    > 0) used = send_data(id_qrl,    qrl   (is:ie, js:je, :), Time)
	if(id_qrs    > 0) used = send_data(id_qrs,    qrs   (is:ie, js:je, :), Time)

	if(id_swcf   > 0) used = send_data(id_swcf,   swcf  (is:ie, js:je), Time)
	if(id_lwcf   > 0) used = send_data(id_lwcf,   lwcf  (is:ie, js:je), Time)

	if(id_aodvis   > 0) used = send_data(id_aodvis,   aero_vistau(is:ie, js:je), Time)
	if(id_aoddust  > 0) used = send_data(id_aoddust,  aoddust    (is:ie, js:je), Time)
	if(id_aoddust1 > 0) used = send_data(id_aoddust1, aoddust1   (is:ie, js:je), Time)
	if(id_aoddust2 > 0) used = send_data(id_aoddust2, aoddust2   (is:ie, js:je), Time)
	if(id_aoddust3 > 0) used = send_data(id_aoddust3, aoddust3   (is:ie, js:je), Time)

	if(id_ttauda > 0) used = send_data(id_ttauda, ttauda(is:ie, js:je), Time)

    if (nwat .eq. 3) then

	    if(id_clw    > 0) used = send_data(id_clw,    clw   (is:ie, js:je, :), Time)
	    if(id_cli    > 0) used = send_data(id_cli,    cli   (is:ie, js:je, :), Time)

	    if(id_clwvi  > 0) used = send_data(id_clwvi,  clwvi (is:ie, js:je), Time)
	    if(id_clivi  > 0) used = send_data(id_clivi,  clivi (is:ie, js:je), Time)

	    if(id_cldliq > 0) used = send_data(id_cldliq, cldliq(is:ie, js:je, :), Time)
	    if(id_cldice > 0) used = send_data(id_cldice, cldice(is:ie, js:je, :), Time)

    end if

    if (id_net_flx_top > 0) used = send_data(id_net_flx_top, net_flx_top(is:ie, js:je), Time)
    if (id_net_flx_srf > 0) used = send_data(id_net_flx_srf, net_flx_srf(is:ie, js:je), Time)

end subroutine rad_ukmo

!=======================================================================

subroutine rad_rrtmg(is, ie, js, je, ng, nq, npz, pt, q, delp, pmd, zmd,&
                     pit, maskout, ts, pdt, asdir, asdif, aldir,        &
                     aldif, ccosz, ddmsqi, ozone, cfrac, effr, tau,     &
                     tau_w, tau_w_g, tau_w_f, odap_aer, awdij, radheat, &
                     direct_vis, direct_nir, diffuse_vis, diffuse_nir,  &
                     net_flx, net_sw, cld_sw_067, cld_lw_105, Time,     &
                     lwhr_rrtmg,                                        &
                     no_cpl)

    use aer_rad_props_mod,   only: nswbands, nlwbands, aer_rad_props_readnl, &
                                   aer_rad_props_sw, aer_rad_props_lw,       &
                                   aer_rad_props_end
    use cld_rad_props,       only: get_ice_props_sw, get_liq_props_sw,       &
                                   get_snow_props_sw, get_ice_props_lw,      &
                                   get_liq_props_lw, get_snow_props_lw,      &
                                   cld_rad_props_init, cld_rad_props_end

    use rrtmg_sw_rad,        only: rrtmg_sw
    use rrtmg_lw_rad,        only: rrtmg_lw
    use parrrsw,             only: nbndsw, ngptsw, naerec
    use parrrtm,             only: nbndlw, ngptlw
    use mcica_subcol_gen_sw, only: mcica_subcol_sw
    use mcica_subcol_gen_lw, only: mcica_subcol_lw

    use ghg_surfvals,        only: co2vmr, n2ovmr, ch4vmr, f11vmr, f12vmr
    use ramp_scon,           only: scon

    implicit none

!----------------------------------------------------------------------
! Dummy arguments

    integer, intent(in)    :: ng                                            ! Number of ghost zones required
    integer, intent(in)    :: nq                                            ! Total number of tracers
    integer, intent(in)    :: is, ie, js, je                                ! Longitude and Latitude index 
    integer, intent(in)    :: npz                                           ! Number of vertical levels 

    integer, intent(in)    :: maskout(is:ie, js:je)                         ! Land sea ice mask(1: land, 0: sea, -1: sea ice)

    real,    intent(in)    :: delp(is-ng:ie+ng, js-ng:je+ng, npz)           ! Pressure thichness (Pa)

	real,    intent(in)    :: pdt                                           ! Time step

	real,    intent(in)    :: pt(is-ng:ie+ng, js-ng:je+ng, npz)             ! Temperature (K)
	real,    intent(in)    :: q (is-ng:ie+ng, js-ng:je+ng, npz, nq)         ! Specific humidity and constituents

	real,    intent(in)    :: ozone(is:ie, js:je, npz)                      ! CMIP5 Ozone (kg/kg)

    real,    intent(inout) :: tau     (is:ie, js:je, npz, nswbands)         ! Aerosol extinction optical depth
    real,    intent(inout) :: tau_w   (is:ie, js:je, npz, nswbands)         ! Aerosol single scattering albedo
    real,    intent(inout) :: tau_w_g (is:ie, js:je, npz, nswbands)         ! Aerosol assymetry parameter
    real,    intent(inout) :: tau_w_f (is:ie, js:je, npz, nswbands)         ! Aerosol forward scattered fraction
    real,    intent(inout) :: odap_aer(is:ie, js:je, npz, nlwbands)         ! Absorption optical depth, per layer (1)

	real,    intent(in)    :: zmd(is:ie, js:je, npz)                        ! Height at layer midpoints above the surface (m)

	real,    intent(in)    :: pmd(is:ie, js:je, npz)                        ! Pressure at layer midpoints (pa)
	real,    intent(in)    :: pit(is:ie, js:je, npz+1)                      ! Pressure at interface points (pa)

	real,    intent(in)    :: ts(is:ie, js:je)                              ! Skin temperature

    real,    intent(in)    :: cfrac(is:ie, js:je, npz)                      ! Cloud fraction (1)

    real,    intent(in)    :: effr(is:ie, js:je, npz, 2)                    ! Liquid and ice effective drop size (micron)

	real,    intent(in)    :: ccosz(is:ie, js:je)                           ! Solar zenith angle
	real,    intent(in)    :: ddmsqi                                        ! Correct the distance of between the sun and earth

    type(time_type), intent(in) :: Time                                     ! Current model time

	real,    intent(inout) :: radheat(is:ie, js:je, npz)                    ! Radiation Heating Ratio (K/s)

	real,    intent(inout) :: net_flx(is:ie, js:je)                         ! Net fluxes from the radiation
	real,    intent(inout) :: net_sw (is:ie, js:je)                         ! Net surface SW fluxes from the radiation
	real,    intent(inout) :: awdij  (is:ie, js:je)                         ! Diagnostic downwards longwave flux at the surface

	real,    intent(inout) :: direct_vis (is:ie, js:je)                     ! Visible downward short wave beam at ground
	real,    intent(inout) :: direct_nir (is:ie, js:je)                     ! Near infrared downward short wave beam at ground
	real,    intent(inout) :: diffuse_vis(is:ie, js:je)                     ! Visible downward short wave diffuse at ground
	real,    intent(inout) :: diffuse_nir(is:ie, js:je)                     ! Near infrared downward short wave diffuse at ground

	real,    intent(inout) :: asdir(is:ie, js:je)                           ! Surface albedo for direct radiation   0.2-0.7 micrometers
	real,    intent(inout) :: aldir(is:ie, js:je)                           ! Surface albedo for direct radiation   0.7-5.0 micrometers
	real,    intent(inout) :: asdif(is:ie, js:je)                           ! Surface albedo for diffuse radiation   0.2-0.7 micrometers
	real,    intent(inout) :: aldif(is:ie, js:je)                           ! Surface albedo for diffuse radiation   0.7-5.0 micrometers

    logical, intent(in)    :: no_cpl                                        ! No Coupling

!-----------------------------------------------------------------------
! For COSP

	real,    intent(out) :: cld_sw_067(is:ie, js:je, npz)                   ! mean 0.67 micron optical depth of cloud
	real,    intent(out) :: cld_lw_105(is:ie, js:je, npz)                   ! 10.5 micron longwave emissivity of stratiform cloud

    integer, parameter :: rrtmg_lw_cloudsim_band = 6                        ! rrtmg band for 10.5 micron
    integer, parameter :: rrtmg_sw_cloudsim_band = 9                        ! rrtmg band for 0.67 micron

!-----------------------------------------------------------------------
! Local variables

    integer :: i, j, k, l

!-----------------------------------------------------------------------
! For RRTMG Cloud Properties
 
    real :: mucon                                      ! Convective size distribution shape parameter
    real :: dcon_land                                  ! Convective size distribution effective radius (meters) on the land 
    real :: dcon_sea                                   ! Convective size distribution effective radius (meters) on the sea 
    real :: lamcon                                     ! Convective size distribution slope parameter (meters -1)
    real :: deicon                                     ! Convective ice effective diameter (meters)
    real :: rhosn                                      ! bulk density snow
    real :: rhoic                                      ! bulk density ice
  
    real, dimension(is:ie, js:je, npz) :: dei          ! Ice effective diameter (meters) (AG: microns?)
    real, dimension(is:ie, js:je, npz) :: des          ! Snow effective diameter (meters) (AG: microns?)
    real, dimension(is:ie, js:je, npz) :: pgam         ! Size distribution shape parameter for radiation
    real, dimension(is:ie, js:je, npz) :: lamc         ! Size distribution shape parameter for radiation
    real, dimension(is:ie, js:je, npz) :: iclwpth      ! In-cloud ice water path for radiation
    real, dimension(is:ie, js:je, npz) :: iciwpth      ! In-cloud liquid water path for radiation
    real, dimension(is:ie, js:je, npz) :: icswpth      ! In-cloud snow water path for radiation
  
    real, dimension(nswbands, is:ie, js:je, npz) :: ice_tau, ice_tau_w, ice_tau_w_g, ice_tau_w_f
    real, dimension(nswbands, is:ie, js:je, npz) :: snw_tau, snw_tau_w, snw_tau_w_g, snw_tau_w_f
    real, dimension(nswbands, is:ie, js:je, npz) :: liq_tau, liq_tau_w, liq_tau_w_g, liq_tau_w_f
    real, dimension(nswbands, is:ie, js:je, npz) :: cld_tau, cld_tau_w, cld_tau_w_g, cld_tau_w_f
    real, dimension(nlwbands, is:ie, js:je, npz) :: ice_lw_abs, liq_lw_abs, cld_lw_abs, snw_lw_abs
  
!-----------------------------------------------------------------------
! Temperal variables

    real :: qq      (is:ie,js:je,npz,nq) 
    real :: tit     (is:ie,js:je,npz+1)

!-----------------------------------------------------------------------

    integer :: iplon = 0
    integer :: icld = 2    ! Cloud overlap method
                           !  0: Clear only
                           !  1: Random
                           !  2: Maximum/random
                           !  3: Maximum  
    integer :: irng = 0    ! Flag for random number generator
                           !  0: Kissvec
                           !  1: Mersenne Twister
    integer :: idrv = 0    ! Flag for calculation of dFdT, the change in upward flux as a function of surface temperature [0=off, 1=on]
                           !  0: Normal forward calculation
                           !  1: Normal forward calculation with duflx_dt and duflxc_dt output
    integer :: permuteseed ! If the cloud generator is called multiple times, 
                           !  permute the seed between each call.
                           !  between calls for LW and SW, recommended
                           !  permuteseed differesby 'ngpt'

!-----------------------------------------------------------------------
! Cloud

    real :: cldfrac(is:ie,npz)                                         ! Layer cloud fraction
                                                                       !    Dimensions:(ncol,nlay)

    real :: ciwp(is:ie,npz)                                            ! In-cloud ice water path
                                                                       !    Dimensions:(ncol,nlay)
    real :: clwp(is:ie,npz)                                            ! In-cloud liquid water path
                                                                       !    Dimensions:(ncol,nlay)

    real :: rei(is:ie,npz)                                             ! Cloud ice particle size
                                                                       !    Dimensions:(ncol,nlay)
    real :: rel(is:ie,npz)                                             ! Cloud liquid particle size
                                                                       !    Dimensions:(ncol,nlay)

!-----------------------------------------------------------------------

    real :: play(is:ie,npz)                                            ! Layer pressures(hPa, mb)
                                                                       !    Dimensions:(ncol,nlay)
    real :: plev(is:ie,npz+1)                                          ! Interface pressures(hPa, mb)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: tlay(is:ie,npz)                                            ! Layer temperatures(K)
                                                                       !    Dimensions:(ncol,nlay)
    real :: tlev(is:ie,npz+1)                                          ! Interface temperatures (K)
                                                                       !    Dimensions:(ncol,nlay+1)

    real :: tsfc(is:ie)                                                ! Surface temperature(K)
                                                                       !    Dimensions:(ncol)

!-----------------------------------------------------------------------
! Green house gases

    real :: o2mmr = 0.23143
    real :: h2ovmr(is:ie,npz)                                          ! H2O volume mixing ratio
                                                                       !    Dimensions:(ncol,nlay)
    real :: o3vmr (is:ie,npz)                                          ! O3 volume mixing ratio
                                                                       !    Dimensions:(ncol,nlay)
    real :: o2vmr (is:ie,npz)                                          ! Oxygen volume mixing ratio
                                                                       !    Dimensions:(ncol,nlay)

    real :: co2vmr2(is:ie,npz)                                         ! CO2 volume mixing ratio
                                                                       !    Dimensions:(ncol,nlay)
    real :: ch4vmr2(is:ie,npz)                                         ! Methane volume mixing ratio
                                                                       !    Dimensions:(ncol,nlay)
    real :: n2ovmr2(is:ie,npz)                                         ! Nitrous oxide volume mixing ratio
                                                                       !    Dimensions:(ncol,nlay)

    real :: cfc11vmr(is:ie,npz)                                        ! CFC11 volume mixing ratio
    real :: cfc12vmr(is:ie,npz)                                        ! CFC12 volume mixing ratio
    real :: cfc22vmr(is:ie,npz)                                        ! CFC22 volume mixing ratio
    real :: ccl4vmr (is:ie,npz)                                        ! CCL4 volume mixing ratio

    real :: emis(is:ie,nbndlw)                                         ! Surface emissivity
                                                                       !    Dimensions:(ncol,nbndlw)

!-----------------------------------------------------------------------
! Solar radiation and albedo

    integer :: dyofyr                                                  ! Day of the year (used to get Earth/Sun
                                                                       !  distance if adjflx not provided)

    real :: asdir2(is:ie)                                              ! UV/vis surface albedo direct rad
                                                                       !    Dimensions:(ncol)
    real :: aldir2(is:ie)                                              ! Near-IR surface albedo direct rad
                                                                       !    Dimensions:(ncol)
    real :: asdif2(is:ie)                                              ! UV/vis surface albedo: diffuse rad
                                                                       !    Dimensions:(ncol)
    real :: aldif2(is:ie)                                              ! Near-IR surface albedo: diffuse rad
                                                                       !    Dimensions:(ncol)

    real :: coszen(is:ie)                                              ! Cosine of solar zenith angle
                                                                       !    Dimensions:(ncol)

    real :: adjes                                                      ! Flux adjustment for Earth/Sun distance
    real :: scon2                                                      ! Solar constant(W/m2)

!-----------------------------------------------------------------------

    real :: dirdnuv(is:ie,npz+2)                                       ! Visible downward short wave beam at ground
    real :: difdnuv(is:ie,npz+2)                                       ! Visible downward short wave diffuse at ground
    real :: dirdnir(is:ie,npz+2)                                       ! Near infrared downward short wave beam at ground
    real :: difdnir(is:ie,npz+2)                                       ! Near infrared downward short wave diffuse at ground

!-----------------------------------------------------------------------
! Land, sea, ice mask

    integer, dimension(is:ie, js:je) :: msklnd                         ! Land mask
    integer, dimension(is:ie, js:je) :: msksea                         ! Sea mask
    integer, dimension(is:ie, js:je) :: msksic                         ! Ice mask

!-----------------------------------------------------------------------
! SW

    real :: tauc_sw(nswbands,is:ie,npz)                                ! In-cloud optical depth
                                                                       !    Dimensions:(nbndsw,ncol,nlay)
    real :: ssac_sw(nswbands,is:ie,npz)                                ! In-cloud single scattering albedo (non-delta scaled)
                                                                       !    Dimensions:(nbndsw,ncol,nlay)
    real :: asmc_sw(nswbands,is:ie,npz)                                ! In-cloud asymmetry parameter (non-delta scaled)
                                                                       !    Dimensions:(nbndsw,ncol,nlay)
    real :: fsfc_sw(nswbands,is:ie,npz)                                ! In-cloud forward scattering fraction (non-delta scaled )
                                                                       !    Dimensions:(nbndsw,ncol,nlay)

    real :: tauaer_sw(is:ie,npz,nswbands)                              ! Aerosol optical depth (iaer=10 only)
                                                                       !    Dimensions:(ncol,nlay,nbndsw)
                                                                       ! (non-delta scaled)      
    real :: ssaaer_sw(is:ie,npz,nswbands)                              ! Aerosol single scattering albedo (iaer=10 only)
                                                                       !    Dimensions:(ncol,nlay,nbndsw)
                                                                       ! (non-delta scaled)      
    real :: asmaer_sw(is:ie,npz,nswbands)                              ! Aerosol asymmetry parameter (iaer=10 only)
                                                                       !    Dimensions:(ncol,nlay,nbndsw)
                                                                       ! (non-delta scaled)      
    real :: ecaer_sw(is:ie,npz,naerec)                                 ! Aerosol optical depth at 0.55 micron (iaer=6 only)
                                                                       !    Dimensions:(ncol,nlay,naerec)
                                                                       ! (non-delta scaled)  

    real :: cldfmcl_sw(ngptsw,is:ie,npz)                               ! Cloud fraction
                                                                       !    Dimensions:(ngptsw,ncol,nlay)

    real :: ciwpmcl_sw(ngptsw,is:ie,npz)                               ! In-cloud ice water path (g/m2)
                                                                       !    Dimensions:(ngptsw,ncol,nlay)
    real :: clwpmcl_sw(ngptsw,is:ie,npz)                               ! In-cloud liquid water path (g/m2)
                                                                       !    Dimensions:(ngptsw,ncol,nlay)

    real :: taucmcl_sw(ngptsw,is:ie,npz)                               ! In-cloud optical depth
                                                                       !    Dimensions:(ngptsw,ncol,nlay)
    real :: ssacmcl_sw(ngptsw,is:ie,npz)                               ! In-cloud single scattering albedo
                                                                       !    Dimensions:(ngptsw,ncol,nlay)
    real :: asmcmcl_sw(ngptsw,is:ie,npz)                               ! In-cloud asymmetry parameter
                                                                       !    Dimensions:(ngptsw,ncol,nlay)
    real :: fsfcmcl_sw(ngptsw,is:ie,npz)                               ! In-cloud forward scattering fraction
                                                                       !    Dimensions:(ngptsw,ncol,nlay)

    real :: reicmcl_sw(is:ie,npz)                                      ! Cloud ice effective radius (microns)
                                                                       !    Dimensions:(ncol,nlay)
    real :: relqmcl_sw(is:ie,npz)                                      ! Cloud water drop effective radius (microns)
                                                                       !    Dimensions:(ncol,nlay)

! LW

    real :: tauc_lw(nlwbands,is:ie,npz)                                ! In-cloud optical depth
                                                                       !    Dimensions:(nbndlw,ncol,nlay)

    real :: tauaer_lw(is:ie,npz,nlwbands)                              ! Aerosol optical depth
                                                                       !    Dimensions: (ncol,nlay,nbndlw)

    real :: cldfmcl_lw(ngptlw,is:ie,npz)                               ! Cloud fraction[mcica]
                                                                       !    Dimensions:(ngptlw,ncol,nlay)

    real :: ciwpmcl_lw(ngptlw,is:ie,npz)                               ! In-cloud ice water path [mcica]
                                                                       !    Dimensions:(ngptlw,ncol,nlay)
    real :: clwpmcl_lw(ngptlw,is:ie,npz)                               ! In-cloud liquid water path [mcica]
                                                                       !    Dimensions:(ngptlw,ncol,nlay)

    real :: taucmcl_lw(ngptlw,is:ie,npz)                               ! In-cloud optical depth [mcica]
                                                                       !    Dimensions:(ngptlw,ncol,nlay)

    real :: relqmcl_lw(is:ie,npz)                                      ! Liquid particle size (microns)
                                                                       !    Dimensions:(ncol,nlay)
    real :: reicmcl_lw(is:ie,npz)                                      ! Ice partcle size (microns)
                                                                       !    Dimensions:(ncol,nlay)

!-----------------------------------------------------------------------
! RRTMG output

    real :: swuflx (is:ie,npz+1)                                       ! Total sky shortwave upward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: swdflx (is:ie,npz+1)                                       ! Total sky shortwave downward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: swhr   (is:ie,npz)                                         ! Total sky shortwave radiative heating rate (K/d)
                                                                       !    Dimensions:(ncol,nlay)
    real :: swuflxc(is:ie,npz+1)                                       ! Clear sky shortwave upward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: swdflxc(is:ie,npz+1)                                       ! Clear sky shortwave downward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: swhrc  (is:ie,npz)                                         ! Clear sky shortwave radiative heating rate (K/d)
                                                                       !    Dimensions:(ncol,nlay)

    real :: lwuflx (is:ie,npz+1)                                       ! Total sky longwave upward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: lwdflx (is:ie,npz+1)                                       ! Total sky longwave downward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: lwhr   (is:ie,npz)                                         ! Total sky longwave radiative heating rate (K/d)
                                                                       !    Dimensions:(ncol,nlay)
    real :: lwuflxc(is:ie,npz+1)                                       ! Clear sky longwave upward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: lwdflxc(is:ie,npz+1)                                       ! Clear sky longwavedownward flux (W/m2)
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: lwhrc  (is:ie,npz)                                         ! Clear sky longwaveradiative heating rate (K/d)
                                                                       !    Dimensions:(ncol,nlay)
 
!-----------------------------------------------------------------------
! Optional output

    real :: duflx_dt (is:ie,npz+1)                                     ! Change in upwardlongwave flux (w/m2/K)
                                                                       ! with respect tosurface temperature
                                                                       !    Dimensions:(ncol,nlay+1)
    real :: duflxc_dt(is:ie,npz+1)                                     ! Change in clear skyupward longwave flux (w/m2/K)
                                                                       ! with respect tosurface temperature
                                                                       !    Dimensions:(ncol,nlay+1)

!-----------------------------------------------------------------------
! Output RRTMG 3D fluxes

    real :: swuflx_rrtmg    (is:ie,js:je,npz+1)                        ! Total sky shortwave upward flux(W/m2)
    real :: swdflx_rrtmg    (is:ie,js:je,npz+1)                        ! Total sky shortwave downward flux(W/m2)
    real :: swuflxc_rrtmg   (is:ie,js:je,npz+1)                        ! Clear sky shortwave upward flux(W/m2)
    real :: swdflxc_rrtmg   (is:ie,js:je,npz+1)                        ! Clear sky shortwave downward flux(W/m2)
    real :: swhr_rrtmg      (is:ie,js:je,npz)                          ! Total sky shortwave heat rate(K/s)
    real :: swhrc_rrtmg     (is:ie,js:je,npz)                          ! Clear sky shortwave heat rate(K/s)

    real :: lwuflx_rrtmg    (is:ie,js:je,npz+1)                        ! Total sky longwave upward flux(W/m2)
    real :: lwdflx_rrtmg    (is:ie,js:je,npz+1)                        ! Total sky longwave downward flux(W/m2)
    real :: lwuflxc_rrtmg   (is:ie,js:je,npz+1)                        ! Clear sky longwave upward flux(W/m2)
    real :: lwdflxc_rrtmg   (is:ie,js:je,npz+1)                        ! Clear sky longwave downward flux(W/m2)
    real :: lwhr_rrtmg      (is:ie,js:je,npz)                          ! Total sky longwave heat rate(K/s)
    real :: lwhrc_rrtmg     (is:ie,js:je,npz)                          ! Clear sky longwave heat rate(K/s)

    real :: swuflx_rrtmg_sf (is:ie,js:je)                              ! Total sky shortwave upward flux(W/m2), surface
    real :: swdflx_rrtmg_sf (is:ie,js:je)                              ! Total sky shortwave downward flux(W/m2), surface
    real :: swuflxc_rrtmg_sf(is:ie,js:je)                              ! Clear sky shortwave upward flux(W/m2), surface
    real :: swdflxc_rrtmg_sf(is:ie,js:je)                              ! Clear sky shortwave downward flux(W/m2), surface

    real :: lwuflx_rrtmg_sf (is:ie,js:je)                              ! Total sky shortwave upward flux(W/m2), surface
    real :: lwdflx_rrtmg_sf (is:ie,js:je)                              ! Total sky shortwave downward flux(W/m2), surface
    real :: lwuflxc_rrtmg_sf(is:ie,js:je)                              ! Clear sky shortwave upward flux(W/m2), surface
    real :: lwdflxc_rrtmg_sf(is:ie,js:je)                              ! Clear sky shortwave downward flux(W/m2), surface

    real :: swuflx_rrtmg_lv (is:ie,js:je,npz)                          ! Total sky shortwave upward flux(W/m2), model half level
    real :: swdflx_rrtmg_lv (is:ie,js:je,npz)                          ! Total sky shortwave downward flux(W/m2), model half level
    real :: swuflxc_rrtmg_lv(is:ie,js:je,npz)                          ! Clear sky shortwave upward flux(W/m2), model half level
    real :: swdflxc_rrtmg_lv(is:ie,js:je,npz)                          ! Clear sky shortwave downward flux(W/m2), model half level

    real :: lwuflx_rrtmg_lv (is:ie,js:je,npz)                          ! Total sky shortwave upward flux(W/m2), model half level
    real :: lwdflx_rrtmg_lv (is:ie,js:je,npz)                          ! Total sky shortwave downward flux(W/m2), model half level
    real :: lwuflxc_rrtmg_lv(is:ie,js:je,npz)                          ! Clear sky shortwave upward flux(W/m2), model half level
    real :: lwdflxc_rrtmg_lv(is:ie,js:je,npz)                          ! Clear sky shortwave downward flux(W/m2), model half level

!-----------------------------------------------------------------------
! Standard name of radiation variables
 
    real, dimension(is:ie, js:je) :: rsds                              ! Surface downwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsus                              ! Surface upwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsns                              ! Surface net shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rlds                              ! Surface downwelling longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rlus                              ! Surface upwelling longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rlns                              ! Surface net longwave radiation ( W/m^2)
    real, dimension(is:ie, js:je) :: rds                               ! Surface downwelling radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rus                               ! Surface upwelling radiation (W/m^2)

    real, dimension(is:ie, js:je) :: rsdt                              ! TOA downwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsut                              ! TOA upwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsnt                              ! TOA net shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rldt                              ! TOA downwelling longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rlut                              ! TOA upwelling longwave radiation(W/m^2)
    real, dimension(is:ie, js:je) :: rlnt                              ! TOA net longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rdt                               ! TOA downwelling radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rut                               ! TOA upwelling radiation (W/m^2)
 
    real, dimension(is:ie, js:je) :: rsdscs                            ! Clear-sky surface downwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsuscs                            ! Clear-sky surface upwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsnscs                            ! Clear-sky surface net shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rldscs                            ! Clear-sky surface downwelling longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rluscs                            ! Clear-sky surface upwelling longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rlnscs                            ! Clear-sky surface net longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rdscs                             ! Clear-sky surface downwelling radiation (W/m^2)
    real, dimension(is:ie, js:je) :: ruscs                             ! Clear-sky surface upwelling radiation (W/m^2)

    real, dimension(is:ie, js:je) :: rsdtcs                            ! Clear-sky TOA downwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsutcs                            ! Clear-sky TOA upwelling shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rsntcs                            ! Clear-sky TOA net shortwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rldtcs                            ! Clear-sky TOA downwelling longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rlutcs                            ! Clear-sky TOA upwelling longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rlntcs                            ! Clear-sky TOA net longwave radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rdtcs                             ! Clear-sky TOA downwelling radiation (W/m^2)
    real, dimension(is:ie, js:je) :: rutcs                             ! Clear-sky TOA upwelling radiation (W/m^2) 

    real, dimension(is:ie, js:je) :: swcf                              ! Shortwave Cloud Forcing
    real, dimension(is:ie, js:je) :: lwcf                              ! Longwave Cloud Forcing

	real, dimension(is:ie, js:je) :: net_flx_top                       ! Net fluxes from the radiation at model top
	real, dimension(is:ie, js:je) :: net_flx_srf                       ! Net fluxes from the radiation at model bottom

!-----------------------------------------------------------------------
! Time

    integer :: date(6), ncsec, days

    real :: calday

	logical :: used

!-----------------------------------------------------------------------
! Aerosol properties input

    call get_calendar(Time, date, ncsec, days, calday)

    if (days .eq. 1 .and. ncsec .eq. nint(pdt)) then

        call aer_rad_props_readnl
        call aer_rad_props_sw(is, ie, js, je, ng, nq, npz, pt, q, pmd, delp, zmd, tau, tau_w, tau_w_g, tau_w_f)
        call aer_rad_props_lw(is, ie, js, je, ng, nq, npz, pt, q, pmd, delp, zmd, odap_aer)
        call aer_rad_props_end

        do k = 1, npz
            do j = js, je
                do i = is, ie
                    do l = 1, nswbands
                        if (tau_w(i,j,k,l) .gt. 1.e-80) then
                            tau_w_g(i,j,k,l) = tau_w_g(i,j,k,l) / tau_w(i,j,k,l)
                            tau_w_f(i,j,k,l) = tau_w_f(i,j,k,l) / tau_w(i,j,k,l)
                        else
                            tau_w_g(i,j,k,l) = 0.0
                            tau_w_f(i,j,k,l) = 0.0
                        end if
                        if (tau(i,j,k,l) .gt. 0.0) then
                            tau_w(i,j,k,l) = tau_w(i,j,k,l) / tau(i,j,k,l)
                        else 
                            tau(i,j,k,l)     = 0.0
                            tau_w(i,j,k,l)   = 1.0
                            tau_w_g(i,j,k,l) = 0.0
                            tau_w_f(i,j,k,l) = 0.0
                        end if
                    end do
                end do
            end do
        end do

    end if

!-----------------------------------------------------------------------
! Cloud properties input
! This should be in-cloud variables, but so far, not ready for in-cloud

    if (inflag .eq. 0) then
  
! Assign default size distribution parameters for no-stratiform clouds(convection only)
! Also put into physics buffer for possible separate use by radiation
  
! Nian 15.01.05

        dcon_land = 10.e-6
        dcon_sea  = 12.e-6
        mucon     = 5.3
        deicon    = 50.
  
        pgam = mucon
        dei  = deicon

    do i = is, ie
       do j = js, je
          select case (maskout(i,j))
           case (1)
            lamc(i,j,:) = (mucon + 1.) / dcon_land
           case (0)
            lamc(i,j,:) = (mucon + 1.) / dcon_sea
           case (-1)
            lamc(i,j,:) = (mucon + 1.) / dcon_sea
          end select
       end do
    end do

#ifdef snow_radiation
        des = 0.0
        rhosn = 100.0  ! bulk density snow
        rhoic = 500.0  ! bulk density ice
        do i = is, ie
            do j = js, je
                do k = 1, npz
                    if (q(i, j, k, 5) .gt. 1.e-7 .and. q(i, j, k, 11) .gt. 0.0) then
                        des(i, j, k) = ((6 * q(i, j, k, 5) * pmd(i, j, k) / rair / pt(i, j, k)) / (q(i, j, k, 11) * rhosn * pi)) ** (1.0 / 3.0)
                    end if
                end do
            end do
        end do
#endif

        iclwpth = q(is:ie, js:je, :, 2) * delp(is:ie, js:je, :) / gravit
        iciwpth = q(is:ie, js:je, :, 4) * delp(is:ie, js:je, :) / gravit
#ifdef snow_radiation
        icswpth = q(is:ie, js:je, :, 5) * delp(is:ie, js:je, :) / gravit
#endif
  
        call cld_rad_props_init
  
        call get_liq_props_sw(is, ie, js, je, npz, lamc, pgam, iclwpth, liq_tau, liq_tau_w, liq_tau_w_g, liq_tau_w_f)
        call get_ice_props_sw(is, ie, js, je, npz, dei, iciwpth, ice_tau, ice_tau_w, ice_tau_w_g, ice_tau_w_f)
#ifdef snow_radiation
        call get_ice_props_sw(is, ie, js, je, npz, des, icswpth, snw_tau, snw_tau_w, snw_tau_w_g, snw_tau_w_f)
#endif

#ifdef snow_radiation
        cld_tau     = liq_tau     + ice_tau     + snw_tau     * (rhosn / rhoic)
        cld_tau_w   = liq_tau_w   + ice_tau_w   + snw_tau_w   * (rhosn / rhoic)
        cld_tau_w_g = liq_tau_w_g + ice_tau_w_g + snw_tau_w_g * (rhosn / rhoic)
        cld_tau_w_f = liq_tau_w_f + ice_tau_w_f + snw_tau_w_f * (rhosn / rhoic)
#else
        cld_tau     = liq_tau     + ice_tau
        cld_tau_w   = liq_tau_w   + ice_tau_w
        cld_tau_w_g = liq_tau_w_g + ice_tau_w_g
        cld_tau_w_f = liq_tau_w_f + ice_tau_w_f
#endif
  
        call get_liq_props_lw(is, ie, js, je, npz, lamc, pgam, iclwpth, liq_lw_abs)
        call get_ice_props_lw(is, ie, js, je, npz, dei, iciwpth, ice_lw_abs)
#ifdef snow_radiation
        call get_ice_props_lw(is, ie, js, je, npz, des, icswpth, snw_lw_abs)
#endif

#ifdef snow_radiation
        cld_lw_abs = liq_lw_abs + ice_lw_abs + snw_lw_abs
#else
        cld_lw_abs = liq_lw_abs + ice_lw_abs
#endif
  
        call cld_rad_props_end
  
        do k = 1, npz
            do j = js, je
                do i = is, ie
                    if (cfrac(i,j,k) .le. 0.0) then
                        cld_tau    (:,i,j,k) = 0.0
                        cld_tau_w  (:,i,j,k) = 1.0
                        cld_tau_w_g(:,i,j,k) = 0.0
                        cld_tau_w_f(:,i,j,k) = 0.0
                        cld_lw_abs (:,i,j,k) = 0.0
                    end if
                    do l = 1, nswbands
                        if (cld_tau_w(l,i,j,k) .gt. 0.0) then
                            cld_tau_w_g(l,i,j,k) = cld_tau_w_g(l,i,j,k) / max(cld_tau_w(l,i,j,k), 1.e-80)
                            cld_tau_w_f(l,i,j,k) = cld_tau_w_f(l,i,j,k) / max(cld_tau_w(l,i,j,k), 1.e-80)
                        else
                            cld_tau_w_g(l,i,j,k) = 0.0
                            cld_tau_w_f(l,i,j,k) = 0.0
                        end if
                        if (cld_tau(l,i,j,k) .gt. 0.0) then
                            cld_tau_w  (l,i,j,k) = max(cld_tau_w(l,i,j,k), 1.e-80)/ max(cld_tau(l,i,j,k), 1.e-80)
                        else
                            cld_tau    (l,i,j,k) = 0.0
                            cld_tau_w  (l,i,j,k) = 1.0
                            cld_tau_w_g(l,i,j,k) = 0.0
                            cld_tau_w_f(l,i,j,k) = 0.0
                        end if
                    end do
                end do
            end do
        end do

    else

        cld_tau     = 0.0
        cld_tau_w   = 1.0
        cld_tau_w_g = 0.0
        cld_tau_w_f = 0.0
        cld_lw_abs  = 0.0

    end if

!----------------------------------------------------------------------
! For COSP

    cld_sw_067 = cld_tau(rrtmg_sw_cloudsim_band,:,:,:)
    cld_lw_105 = 1.0 - exp(-cld_lw_abs(rrtmg_lw_cloudsim_band,:,:,:))

!-----------------------------------------------------------------------
! Calcluate albedo on ocean and sea ice for rrtmg

    if (no_cpl) then
        do i = is, ie
            do j= js, je
     
                select case (maskout(i,j))
                    case (1)
                       msklnd(i,j) = 1
                       msksea(i,j) = 0
                       msksic(i,j) = 0
                    case (0)
                       msklnd(i,j) = 0
                       msksea(i,j) = 1
                       msksic(i,j) = 0
                    case (-1)
                       msklnd(i,j) = 0
                       msksea(i,j) = 0
                       msksic(i,j) = 1
                end select
     
                call albocean0(1, 1, asdir(i,j), asdif(i,j), aldir(i,j), aldif(i,j), msklnd(i,j), msksic(i,j), ccosz(i,j))
    
            end do
        end do
    end if

!-----------------------------------------------------------------------
! Calculated mid layer temperature

    tit(:,:,1) = pt(is:ie,js:je,1)
    do k = 2, npz
        tit(:,:,k) = 0.5 * (pt(is:ie,js:je,k) + pt(is:ie,js:je,k-1))
    end do
    tit(:,:,npz+1) = ts(is:ie,js:je)

!-----------------------------------------------------------------------
! Fix negative species for intial run

    qq = q(is:ie,js:je,:,:)

    do j = js, je
        do i = is, ie
            do k = 1, npz
                qq(i,j,k,1) = max(qq(i,j,k,1), 1e-7)
                qq(i,j,k,2) = max(qq(i,j,k,2), 1e-15)
                qq(i,j,k,4) = max(qq(i,j,k,4), 1e-15)
            end do
        end do
    end do

!-----------------------------------------------------------------------
! Main loop

    do j = js, je

        plev = pit(is:ie,j,npz+1:1:-1) / 100.  ! hPa
        play = pmd(is:ie,j,npz  :1:-1) / 100.  ! hPa          
        tlev = tit(is:ie,j,npz+1:1:-1) 
        tlay = pt (is:ie,j,npz  :1:-1)

        tsfc = ts(is:ie,j)

!-----------------------------------------------------------------------
! Solar radiation and albedo

        asdir2 = asdir(is:ie,j)
        asdif2 = asdif(is:ie,j)
        aldir2 = aldir(is:ie,j)
        aldif2 = aldif(is:ie,j)

        coszen = ccosz(is:ie,j)

        adjes  = ddmsqi
        dyofyr = days
        scon2  = scon * 1e-3     

!-----------------------------------------------------------------------
! Green house gases

        h2ovmr   = qq   (is:ie,j,npz:1:-1,1)   ! mmr, kg/kg
        o3vmr    = ozone(is:ie,j,npz:1:-1)     ! mmr, kg/kg
        o2vmr    = o2mmr                       ! mmr, kg/kg

        co2vmr2  = co2vmr
        ch4vmr2  = ch4vmr
        n2ovmr2  = n2ovmr 

        cfc11vmr = f11vmr
        cfc12vmr = f12vmr
        cfc22vmr = 0.
        ccl4vmr  = 0.
        emis     = 1.

!-----------------------------------------------------------------------
! Cloud, aerosol and their properties

        cldfrac = cfrac(is:ie,j,npz:1:-1)  

        clwp = qq(is:ie,j,npz:1:-1,2) * delp(is:ie,j,npz:1:-1) / gravit * 1000.    ! g/m2
        ciwp = qq(is:ie,j,npz:1:-1,4) * delp(is:ie,j,npz:1:-1) / gravit * 1000.    ! g/m2

        rel = effr(is:ie,j,npz:1:-1,1)     ! This variable is needed when inflag = 2
        rei = effr(is:ie,j,npz:1:-1,2)     ! This variable is needed when inflag = 2

        do k = 1, npz
            do i = is, ie
                if (liqflag .eq. 1) then
                    rel(i,k) = min(max(2.5,rel(i,k)),60.)
                end if
                if (iceflag .eq. 1) then
                    rei(i,k) = min(max(13.,rei(i,k)),130.)
                else if (iceflag .eq. 2) then
                    rei(i,k) = min(max(5.,rei(i,k)),131.)
                else if (iceflag .eq. 3) then
                    rei(i,k) = min(max(5.,rei(i,k)),140.)
                end if
            end do
        end do
    
        tauc_sw = cld_tau    (:,is:ie,j,npz:1:-1)
        ssac_sw = cld_tau_w  (:,is:ie,j,npz:1:-1)
        asmc_sw = cld_tau_w_g(:,is:ie,j,npz:1:-1)
        fsfc_sw = cld_tau_w_f(:,is:ie,j,npz:1:-1)
        tauc_lw = cld_lw_abs (:,is:ie,j,npz:1:-1)

        tauaer_sw = tau     (is:ie,j,npz:1:-1,:)
        ssaaer_sw = tau_w   (is:ie,j,npz:1:-1,:)
        asmaer_sw = tau_w_g (is:ie,j,npz:1:-1,:)
        tauaer_lw = odap_aer(is:ie,j,npz:1:-1,:)

        ecaer_sw = 0.
    
!-----------------------------------------------------------------------
! RRTMG SW RADIATION TRANSFER

        permuteseed = 1

        call mcica_subcol_sw(iplon, ie-is+1, npz, icld, permuteseed,   &
                             irng, play, cldfrac, ciwp, clwp, rei, rel,&
                             tauc_sw, ssac_sw, asmc_sw, fsfc_sw,       &
                             cldfmcl_sw, ciwpmcl_sw, clwpmcl_sw,       &
                             reicmcl_sw, relqmcl_sw, taucmcl_sw,       &
                             ssacmcl_sw, asmcmcl_sw, fsfcmcl_sw)

        call rrtmg_sw(ie-is+1, npz, icld, play, plev, tlay, tlev, tsfc,&
                      h2ovmr, o3vmr, co2vmr2, ch4vmr2, n2ovmr2, o2vmr, &
                      asdir2, asdif2, aldir2, aldif2, coszen, adjes,   &
                      dyofyr, scon2, inflag, iceflag, liqflag,         &
                      cldfmcl_sw, taucmcl_sw, ssacmcl_sw, asmcmcl_sw,  &
                      fsfcmcl_sw, ciwpmcl_sw, clwpmcl_sw, reicmcl_sw,  &
                      relqmcl_sw, tauaer_sw, ssaaer_sw, asmaer_sw,     &
                      ecaer_sw, swuflx, swdflx, swhr, swuflxc, swdflxc,&
                      swhrc, dirdnuv, difdnuv, dirdnir, difdnir)

        swuflx_rrtmg (:,j,:) = swuflx (:,npz+1:1:-1) 
        swdflx_rrtmg (:,j,:) = swdflx (:,npz+1:1:-1)
        swhr_rrtmg   (:,j,:) = swhr   (:,npz  :1:-1) / 86400.
        swuflxc_rrtmg(:,j,:) = swuflxc(:,npz+1:1:-1)
        swdflxc_rrtmg(:,j,:) = swdflxc(:,npz+1:1:-1)
        swhrc_rrtmg  (:,j,:) = swhrc  (:,npz  :1:-1) / 86400.

        direct_vis (:,j) = dirdnuv(:,1)
        direct_nir (:,j) = dirdnir(:,1)
        diffuse_vis(:,j) = difdnuv(:,1)
        diffuse_nir(:,j) = difdnir(:,1)

!-----------------------------------------------------------------------
! RRTMG LW RADIATION TRANSFER

        permuteseed = 150

        call mcica_subcol_lw(iplon, ie-is+1, npz, icld, permuteseed,    &
                             irng, play, cldfrac, ciwp, clwp, rei, rel, &
                             tauc_lw, cldfmcl_lw, ciwpmcl_lw,           &
                             clwpmcl_lw, reicmcl_lw, relqmcl_lw,        &
                             taucmcl_lw)

        call rrtmg_lw(ie-is+1, npz, icld, idrv, play, plev, tlay, tlev, &
                      tsfc, h2ovmr, o3vmr, co2vmr2, ch4vmr2, n2ovmr2,   &
                      o2vmr, cfc11vmr, cfc12vmr, cfc22vmr, ccl4vmr,     &
                      emis, inflag, iceflag, liqflag, cldfmcl_lw,       &
                      taucmcl_lw, ciwpmcl_lw, clwpmcl_lw, reicmcl_lw,   &
                      relqmcl_lw, tauaer_lw, lwuflx, lwdflx, lwhr,      &
                      lwuflxc, lwdflxc, lwhrc, duflx_dt, duflxc_dt)

        lwuflx_rrtmg (:,j,:) = lwuflx (:,npz+1:1:-1)
        lwdflx_rrtmg (:,j,:) = lwdflx (:,npz+1:1:-1)
        lwhr_rrtmg   (:,j,:) = lwhr   (:,npz  :1:-1) / 86400.
        lwuflxc_rrtmg(:,j,:) = lwuflxc(:,npz+1:1:-1)
        lwdflxc_rrtmg(:,j,:) = lwdflxc(:,npz+1:1:-1)
        lwhrc_rrtmg  (:,j,:) = lwhrc  (:,npz  :1:-1) / 86400.

    end do
!-----------------------------------------------------------------------
! Radiation heating rate

    radheat = swhr_rrtmg + lwhr_rrtmg

!-----------------------------------------------------------------------
! Output surface and TOA fluxes

    awdij = lwdflx_rrtmg(:,:,npz+1)

    rsds = swdflx_rrtmg(:,:,npz+1)
    rsus = swuflx_rrtmg(:,:,npz+1)
    rlds = lwdflx_rrtmg(:,:,npz+1)
    rlus = lwuflx_rrtmg(:,:,npz+1)
    rsns = rsds - rsus
    rlns = rlus - rlds
    rds  = rsds + rlds
    rus  = rsus + rlus

    rsdt = swdflx_rrtmg(:,:,1)
    rsut = swuflx_rrtmg(:,:,1)
    rldt = lwdflx_rrtmg(:,:,1)
    rlut = lwuflx_rrtmg(:,:,1)
    rsnt = rsdt - rsut
    rlnt = rlut - rldt
    rdt  = rsdt + rldt
    rut  = rsut + rlut

    rsdscs = swdflxc_rrtmg(:,:,npz+1) 
    rsuscs = swuflxc_rrtmg(:,:,npz+1) 
    rldscs = lwdflxc_rrtmg(:,:,npz+1)
    rluscs = lwuflxc_rrtmg(:,:,npz+1)
    rsnscs = rsdscs - rsuscs
    rlnscs = rluscs - rldscs
    rdscs  = rsdscs + rldscs
    ruscs  = rsuscs + rluscs

    rsdtcs = swdflxc_rrtmg(:,:,1)
    rsutcs = swuflxc_rrtmg(:,:,1)
    rldtcs = lwdflxc_rrtmg(:,:,1)
    rlutcs = lwuflxc_rrtmg(:,:,1)
    rsntcs = rsdtcs - rsutcs
    rlntcs = rlutcs - rldtcs
    rdtcs  = rsdtcs + rldtcs
    rutcs  = rsutcs + rlutnian
    swcf = rsnt   - rsntcs
    lwcf = rlntcs - rlnt  

    net_flx = (rsdt - rsut) + (rldt - rlut) &
            + (rsus - rsds) + (rlus - rlds)

    net_sw = rsds - rsus

    net_flx_top = (rsdt - rsut) + (rldt - rlut)
    net_flx_srf = (rsus - rsds) + (rlus - rlds)

!-----------------------------------------------------------------------
! Interpolation from half level to full level
 
    swuflx_rrtmg_sf = swuflx_rrtmg(:,:,npz+1)
    swdflx_rrtmg_sf = swdflx_rrtmg(:,:,npz+1)
    lwuflx_rrtmg_sf = lwuflx_rrtmg(:,:,npz+1)
    lwdflx_rrtmg_sf = lwdflx_rrtmg(:,:,npz+1)

    swuflxc_rrtmg_sf = swuflxc_rrtmg(:,:,npz+1)
    swdflxc_rrtmg_sf = swdflxc_rrtmg(:,:,npz+1)
    lwuflxc_rrtmg_sf = lwuflxc_rrtmg(:,:,npz+1)
    lwdflxc_rrtmg_sf = lwdflxc_rrtmg(:,:,npz+1)

    swuflx_rrtmg_lv = swuflx_rrtmg(:,:,1:npz)
    swdflx_rrtmg_lv = swdflx_rrtmg(:,:,1:npz)
    lwuflx_rrtmg_lv = lwuflx_rrtmg(:,:,1:npz)
    lwdflx_rrtmg_lv = lwdflx_rrtmg(:,:,1:npz)

    swuflxc_rrtmg_lv = swuflxc_rrtmg(:,:,1:npz)
    swdflxc_rrtmg_lv = swdflxc_rrtmg(:,:,1:npz)
    lwuflxc_rrtmg_lv = lwuflxc_rrtmg(:,:,1:npz)
    lwdflxc_rrtmg_lv = lwdflxc_rrtmg(:,:,1:npz)

!-----------------------------------------------------------------------
! Diagnostic output

    if (id_swuflx_sf  > 0) used = send_data(id_swuflx_sf,  swuflx_rrtmg_sf (is:ie, js:je   ), Time)
    if (id_swdflx_sf  > 0) used = send_data(id_swdflx_sf,  swdflx_rrtmg_sf (is:ie, js:je   ), Time)
    if (id_swuflxc_sf > 0) used = send_data(id_swuflxc_sf, swuflxc_rrtmg_sf(is:ie, js:je   ), Time)
    if (id_swdflxc_sf > 0) used = send_data(id_swdflxc_sf, swdflxc_rrtmg_sf(is:ie, js:je   ), Time)
    if (id_swuflx_lv  > 0) used = send_data(id_swuflx_lv,  swuflx_rrtmg_lv (is:ie, js:je, :), Time)
    if (id_swdflx_lv  > 0) used = send_data(id_swdflx_lv,  swdflx_rrtmg_lv (is:ie, js:je, :), Time)
    if (id_swuflxc_lv > 0) used = send_data(id_swuflxc_lv, swuflxc_rrtmg_lv(is:ie, js:je, :), Time)
    if (id_swdflxc_lv > 0) used = send_data(id_swdflxc_lv, swdflxc_rrtmg_lv(is:ie, js:je, :), Time)
    if (id_swhr       > 0) used = send_data(id_swhr,       swhr_rrtmg      (is:ie, js:je, :), Time)
    if (id_swhrc      > 0) used = send_data(id_swhrc,      swhrc_rrtmg     (is:ie, js:je, :), Time)

    if (id_lwuflx_sf  > 0) used = send_data(id_lwuflx_sf,  lwuflx_rrtmg_sf (is:ie, js:je   ), Time)
    if (id_lwdflx_sf  > 0) used = send_data(id_lwdflx_sf,  lwdflx_rrtmg_sf (is:ie, js:je   ), Time)
    if (id_lwuflxc_sf > 0) used = send_data(id_lwuflxc_sf, lwuflxc_rrtmg_sf(is:ie, js:je   ), Time)
    if (id_lwdflxc_sf > 0) used = send_data(id_lwdflxc_sf, lwdflxc_rrtmg_sf(is:ie, js:je   ), Time)
    if (id_lwuflx_lv  > 0) used = send_data(id_lwuflx_lv,  lwuflx_rrtmg_lv (is:ie, js:je, :), Time)
    if (id_lwdflx_lv  > 0) used = send_data(id_lwdflx_lv,  lwdflx_rrtmg_lv (is:ie, js:je, :), Time)
    if (id_lwuflxc_lv > 0) used = send_data(id_lwuflxc_lv, lwuflxc_rrtmg_lv(is:ie, js:je, :), Time)
    if (id_lwdflxc_lv > 0) used = send_data(id_lwdflxc_lv, lwdflxc_rrtmg_lv(is:ie, js:je, :), Time)
    if (id_lwhr       > 0) used = send_data(id_lwhr,       lwhr_rrtmg      (is:ie, js:je, :), Time)
    if (id_lwhrc      > 0) used = send_data(id_lwhrc,      lwhrc_rrtmg     (is:ie, js:je, :), Time)

    if (id_rsds   > 0) used = send_data(id_rsds,   rsds  (is:ie, js:je), Time)
    if (id_rsus   > 0) used = send_data(id_rsus,   rsus  (is:ie, js:je), Time)
    if (id_rsns   > 0) used = send_data(id_rsns,   rsns  (is:ie, js:je), Time)
    if (id_rlds   > 0) used = send_data(id_rlds,   rlds  (is:ie, js:je), Time)
    if (id_rlus   > 0) used = send_data(id_rlus,   rlus  (is:ie, js:je), Time)
    if (id_rlns   > 0) used = send_data(id_rlns,   rlns  (is:ie, js:je), Time)
    if (id_rds    > 0) used = send_data(id_rds,    rds   (is:ie, js:je), Time)
    if (id_rus    > 0) used = send_data(id_rus,    rus   (is:ie, js:je), Time)
 
    if (id_rsdt   > 0) used = send_data(id_rsdt,   rsdt  (is:ie, js:je), Time)
    if (id_rsut   > 0) used = send_data(id_rsut,   rsut  (is:ie, js:je), Time)
    if (id_rsnt   > 0) used = send_data(id_rsnt,   rsnt  (is:ie, js:je), Time)
    if (id_rldt   > 0) used = send_data(id_rldt,   rldt  (is:ie, js:je), Time)
    if (id_rlut   > 0) used = send_data(id_rlut,   rlut  (is:ie, js:je), Time)
    if (id_rlnt   > 0) used = send_data(id_rlnt,   rlnt  (is:ie, js:je), Time)
    if (id_rdt    > 0) used = send_data(id_rdt,    rdt   (is:ie, js:je), Time)
    if (id_rut    > 0) used = send_data(id_rut,    rut   (is:ie, js:je), Time)

    if (id_rsdscs > 0) used = send_data(id_rsdscs, rsdscs(is:ie, js:je), Time)
    if (id_rsuscs > 0) used = send_data(id_rsuscs, rsuscs(is:ie, js:je), Time)
    if (id_rsnscs > 0) used = send_data(id_rsnscs, rsnscs(is:ie, js:je), Time)
    if (id_rldscs > 0) used = send_data(id_rldscs, rldscs(is:ie, js:je), Time)
    if (id_rluscs > 0) used = send_data(id_rluscs, rluscs(is:ie, js:je), Time)
    if (id_rlnscs > 0) used = send_data(id_rlnscs, rlnscs(is:ie, js:je), Time)
    if (id_rdscs  > 0) used = send_data(id_rdscs,  rdscs (is:ie, js:je), Time)
    if (id_ruscs  > 0) used = send_data(id_ruscs,  ruscs (is:ie, js:je), Time)
 
    if (id_rsdtcs > 0) used = send_data(id_rsdtcs, rsdtcs(is:ie, js:je), Time)
    if (id_rsutcs > 0) used = send_data(id_rsutcs, rsutcs(is:ie, js:je), Time)
    if (id_rsntcs > 0) used = send_data(id_rsntcs, rsntcs(is:ie, js:je), Time)
    if (id_rldtcs > 0) used = send_data(id_rldtcs, rldtcs(is:ie, js:je), Time)
    if (id_rlutcs > 0) used = send_data(id_rlutcs, rlutcs(is:ie, js:je), Time)
    if (id_rlntcs > 0) used = send_data(id_rlntcs, rlntcs(is:ie, js:je), Time)
    if (id_rdtcs  > 0) used = send_data(id_rdtcs,  rdtcs (is:ie, js:je), Time)
    if (id_rutcs  > 0) used = send_data(id_rutcs,  rutcs (is:ie, js:je), Time)

    if (id_qrl > 0) used = send_data(id_qrl, lwhr_rrtmg(is:ie, js:je, :), Time)
    if (id_qrs > 0) used = send_data(id_qrs, swhr_rrtmg(is:ie, js:je, :), Time)

    if (id_swcf > 0) used = send_data(id_swcf, swcf(is:ie, js:je), Time)
    if (id_lwcf > 0) used = send_data(id_lwcf, lwcf(is:ie, js:je), Time)

    if (id_tau_1  > 0) used = send_data(id_tau_1,  tau(is:ie, js:je, :, 1 ), Time)
    if (id_tau_2  > 0) used = send_data(id_tau_2,  tau(is:ie, js:je, :, 2 ), Time)
    if (id_tau_3  > 0) used = send_data(id_tau_3,  tau(is:ie, js:je, :, 3 ), Time)
    if (id_tau_4  > 0) used = send_data(id_tau_4,  tau(is:ie, js:je, :, 4 ), Time)
    if (id_tau_5  > 0) used = send_data(id_tau_5,  tau(is:ie, js:je, :, 5 ), Time)
    if (id_tau_6  > 0) used = send_data(id_tau_6,  tau(is:ie, js:je, :, 6 ), Time)
    if (id_tau_7  > 0) used = send_data(id_tau_7,  tau(is:ie, js:je, :, 7 ), Time)
    if (id_tau_8  > 0) used = send_data(id_tau_8,  tau(is:ie, js:je, :, 8 ), Time)
    if (id_tau_9  > 0) used = send_data(id_tau_9,  tau(is:ie, js:je, :, 9 ), Time)
    if (id_tau_10 > 0) used = send_data(id_tau_10, tau(is:ie, js:je, :, 10), Time)
    if (id_tau_11 > 0) used = send_data(id_tau_11, tau(is:ie, js:je, :, 11), Time)
    if (id_tau_12 > 0) used = send_data(id_tau_12, tau(is:ie, js:je, :, 12), Time)
    if (id_tau_13 > 0) used = send_data(id_tau_13, tau(is:ie, js:je, :, 13), Time)
    if (id_tau_14 > 0) used = send_data(id_tau_14, tau(is:ie, js:je, :, 14), Time)

    if (id_tau_w_1  > 0) used = send_data(id_tau_w_1,  tau_w(is:ie, js:je, :, 1 ), Time)
    if (id_tau_w_2  > 0) used = send_data(id_tau_w_2,  tau_w(is:ie, js:je, :, 2 ), Time)
    if (id_tau_w_3  > 0) used = send_data(id_tau_w_3,  tau_w(is:ie, js:je, :, 3 ), Time)
    if (id_tau_w_4  > 0) used = send_data(id_tau_w_4,  tau_w(is:ie, js:je, :, 4 ), Time)
    if (id_tau_w_5  > 0) used = send_data(id_tau_w_5,  tau_w(is:ie, js:je, :, 5 ), Time)
    if (id_tau_w_6  > 0) used = send_data(id_tau_w_6,  tau_w(is:ie, js:je, :, 6 ), Time)
    if (id_tau_w_7  > 0) used = send_data(id_tau_w_7,  tau_w(is:ie, js:je, :, 7 ), Time)
    if (id_tau_w_8  > 0) used = send_data(id_tau_w_8,  tau_w(is:ie, js:je, :, 8 ), Time)
    if (id_tau_w_9  > 0) used = send_data(id_tau_w_9,  tau_w(is:ie, js:je, :, 9 ), Time)
    if (id_tau_w_10 > 0) used = send_data(id_tau_w_10, tau_w(is:ie, js:je, :, 10), Time)
    if (id_tau_w_11 > 0) used = send_data(id_tau_w_11, tau_w(is:ie, js:je, :, 11), Time)
    if (id_tau_w_12 > 0) used = send_data(id_tau_w_12, tau_w(is:ie, js:je, :, 12), Time)
    if (id_tau_w_13 > 0) used = send_data(id_tau_w_13, tau_w(is:ie, js:je, :, 13), Time)
    if (id_tau_w_14 > 0) used = send_data(id_tau_w_14, tau_w(is:ie, js:je, :, 14), Time)

    if (id_tau_w_g_1  > 0) used = send_data(id_tau_w_g_1,  tau_w_g(is:ie, js:je, :, 1 ), Time)
    if (id_tau_w_g_2  > 0) used = send_data(id_tau_w_g_2,  tau_w_g(is:ie, js:je, :, 2 ), Time)
    if (id_tau_w_g_3  > 0) used = send_data(id_tau_w_g_3,  tau_w_g(is:ie, js:je, :, 3 ), Time)
    if (id_tau_w_g_4  > 0) used = send_data(id_tau_w_g_4,  tau_w_g(is:ie, js:je, :, 4 ), Time)
    if (id_tau_w_g_5  > 0) used = send_data(id_tau_w_g_5,  tau_w_g(is:ie, js:je, :, 5 ), Time)
    if (id_tau_w_g_6  > 0) used = send_data(id_tau_w_g_6,  tau_w_g(is:ie, js:je, :, 6 ), Time)
    if (id_tau_w_g_7  > 0) used = send_data(id_tau_w_g_7,  tau_w_g(is:ie, js:je, :, 7 ), Time)
    if (id_tau_w_g_8  > 0) used = send_data(id_tau_w_g_8,  tau_w_g(is:ie, js:je, :, 8 ), Time)
    if (id_tau_w_g_9  > 0) used = send_data(id_tau_w_g_9,  tau_w_g(is:ie, js:je, :, 9 ), Time)
    if (id_tau_w_g_10 > 0) used = send_data(id_tau_w_g_10, tau_w_g(is:ie, js:je, :, 10), Time)
    if (id_tau_w_g_11 > 0) used = send_data(id_tau_w_g_11, tau_w_g(is:ie, js:je, :, 11), Time)
    if (id_tau_w_g_12 > 0) used = send_data(id_tau_w_g_12, tau_w_g(is:ie, js:je, :, 12), Time)
    if (id_tau_w_g_13 > 0) used = send_data(id_tau_w_g_13, tau_w_g(is:ie, js:je, :, 13), Time)
    if (id_tau_w_g_14 > 0) used = send_data(id_tau_w_g_14, tau_w_g(is:ie, js:je, :, 14), Time)

    if (id_tau_w_f_1  > 0) used = send_data(id_tau_w_f_1,  tau_w_f(is:ie, js:je, :, 1 ), Time)
    if (id_tau_w_f_2  > 0) used = send_data(id_tau_w_f_2,  tau_w_f(is:ie, js:je, :, 2 ), Time)
    if (id_tau_w_f_3  > 0) used = send_data(id_tau_w_f_3,  tau_w_f(is:ie, js:je, :, 3 ), Time)
    if (id_tau_w_f_4  > 0) used = send_data(id_tau_w_f_4,  tau_w_f(is:ie, js:je, :, 4 ), Time)
    if (id_tau_w_f_5  > 0) used = send_data(id_tau_w_f_5,  tau_w_f(is:ie, js:je, :, 5 ), Time)
    if (id_tau_w_f_6  > 0) used = send_data(id_tau_w_f_6,  tau_w_f(is:ie, js:je, :, 6 ), Time)
    if (id_tau_w_f_7  > 0) used = send_data(id_tau_w_f_7,  tau_w_f(is:ie, js:je, :, 7 ), Time)
    if (id_tau_w_f_8  > 0) used = send_data(id_tau_w_f_8,  tau_w_f(is:ie, js:je, :, 8 ), Time)
    if (id_tau_w_f_9  > 0) used = send_data(id_tau_w_f_9,  tau_w_f(is:ie, js:je, :, 9 ), Time)
    if (id_tau_w_f_10 > 0) used = send_data(id_tau_w_f_10, tau_w_f(is:ie, js:je, :, 10), Time)
    if (id_tau_w_f_11 > 0) used = send_data(id_tau_w_f_11, tau_w_f(is:ie, js:je, :, 11), Time)
    if (id_tau_w_f_12 > 0) used = send_data(id_tau_w_f_12, tau_w_f(is:ie, js:je, :, 12), Time)
    if (id_tau_w_f_13 > 0) used = send_data(id_tau_w_f_13, tau_w_f(is:ie, js:je, :, 13), Time)
    if (id_tau_w_f_14 > 0) used = send_data(id_tau_w_f_14, tau_w_f(is:ie, js:je, :, 14), Time)

    if (id_odap_aer_1  > 0) used = send_data(id_odap_aer_1,  odap_aer(is:ie, js:je, :, 1 ), Time)
    if (id_odap_aer_2  > 0) used = send_data(id_odap_aer_2,  odap_aer(is:ie, js:je, :, 2 ), Time)
    if (id_odap_aer_3  > 0) used = send_data(id_odap_aer_3,  odap_aer(is:ie, js:je, :, 3 ), Time)
    if (id_odap_aer_4  > 0) used = send_data(id_odap_aer_4,  odap_aer(is:ie, js:je, :, 4 ), Time)
    if (id_odap_aer_5  > 0) used = send_data(id_odap_aer_5,  odap_aer(is:ie, js:je, :, 5 ), Time)
    if (id_odap_aer_6  > 0) used = send_data(id_odap_aer_6,  odap_aer(is:ie, js:je, :, 6 ), Time)
    if (id_odap_aer_7  > 0) used = send_data(id_odap_aer_7,  odap_aer(is:ie, js:je, :, 7 ), Time)
    if (id_odap_aer_8  > 0) used = send_data(id_odap_aer_8,  odap_aer(is:ie, js:je, :, 8 ), Time)
    if (id_odap_aer_9  > 0) used = send_data(id_odap_aer_9,  odap_aer(is:ie, js:je, :, 9 ), Time)
    if (id_odap_aer_10 > 0) used = send_data(id_odap_aer_10, odap_aer(is:ie, js:je, :, 10), Time)
    if (id_odap_aer_11 > 0) used = send_data(id_odap_aer_11, odap_aer(is:ie, js:je, :, 11), Time)
    if (id_odap_aer_12 > 0) used = send_data(id_odap_aer_12, odap_aer(is:ie, js:je, :, 12), Time)
    if (id_odap_aer_13 > 0) used = send_data(id_odap_aer_13, odap_aer(is:ie, js:je, :, 13), Time)
    if (id_odap_aer_14 > 0) used = send_data(id_odap_aer_14, odap_aer(is:ie, js:je, :, 14), Time)
    if (id_odap_aer_15 > 0) used = send_data(id_odap_aer_15, odap_aer(is:ie, js:je, :, 15), Time)
    if (id_odap_aer_16 > 0) used = send_data(id_odap_aer_16, odap_aer(is:ie, js:je, :, 16), Time)

    if (id_cld_tau_1  > 0) used = send_data(id_cld_tau_1,  cld_tau(1 , is:ie, js:je, :), Time)
    if (id_cld_tau_2  > 0) used = send_data(id_cld_tau_2,  cld_tau(2 , is:ie, js:je, :), Time)
    if (id_cld_tau_3  > 0) used = send_data(id_cld_tau_3,  cld_tau(3 , is:ie, js:je, :), Time)
    if (id_cld_tau_4  > 0) used = send_data(id_cld_tau_4,  cld_tau(4 , is:ie, js:je, :), Time)
    if (id_cld_tau_5  > 0) used = send_data(id_cld_tau_5,  cld_tau(5 , is:ie, js:je, :), Time)
    if (id_cld_tau_6  > 0) used = send_data(id_cld_tau_6,  cld_tau(6 , is:ie, js:je, :), Time)
    if (id_cld_tau_7  > 0) used = send_data(id_cld_tau_7,  cld_tau(7 , is:ie, js:je, :), Time)
    if (id_cld_tau_8  > 0) used = send_data(id_cld_tau_8,  cld_tau(8 , is:ie, js:je, :), Time)
    if (id_cld_tau_9  > 0) used = send_data(id_cld_tau_9,  cld_tau(9 , is:ie, js:je, :), Time)
    if (id_cld_tau_10 > 0) used = send_data(id_cld_tau_10, cld_tau(10, is:ie, js:je, :), Time)
    if (id_cld_tau_11 > 0) used = send_data(id_cld_tau_11, cld_tau(11, is:ie, js:je, :), Time)
    if (id_cld_tau_12 > 0) used = send_data(id_cld_tau_12, cld_tau(12, is:ie, js:je, :), Time)
    if (id_cld_tau_13 > 0) used = send_data(id_cld_tau_13, cld_tau(13, is:ie, js:je, :), Time)
    if (id_cld_tau_14 > 0) used = send_data(id_cld_tau_14, cld_tau(14, is:ie, js:je, :), Time)

    if (id_cld_tau_w_1  > 0) used = send_data(id_cld_tau_w_1,  cld_tau_w(1 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_2  > 0) used = send_data(id_cld_tau_w_2,  cld_tau_w(2 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_3  > 0) used = send_data(id_cld_tau_w_3,  cld_tau_w(3 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_4  > 0) used = send_data(id_cld_tau_w_4,  cld_tau_w(4 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_5  > 0) used = send_data(id_cld_tau_w_5,  cld_tau_w(5 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_6  > 0) used = send_data(id_cld_tau_w_6,  cld_tau_w(6 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_7  > 0) used = send_data(id_cld_tau_w_7,  cld_tau_w(7 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_8  > 0) used = send_data(id_cld_tau_w_8,  cld_tau_w(8 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_9  > 0) used = send_data(id_cld_tau_w_9,  cld_tau_w(9 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_10 > 0) used = send_data(id_cld_tau_w_10, cld_tau_w(10, is:ie, js:je, :), Time)
    if (id_cld_tau_w_11 > 0) used = send_data(id_cld_tau_w_11, cld_tau_w(11, is:ie, js:je, :), Time)
    if (id_cld_tau_w_12 > 0) used = send_data(id_cld_tau_w_12, cld_tau_w(12, is:ie, js:je, :), Time)
    if (id_cld_tau_w_13 > 0) used = send_data(id_cld_tau_w_13, cld_tau_w(13, is:ie, js:je, :), Time)
    if (id_cld_tau_w_14 > 0) used = send_data(id_cld_tau_w_14, cld_tau_w(14, is:ie, js:je, :), Time)

    if (id_cld_tau_w_g_1  > 0) used = send_data(id_cld_tau_w_g_1,  cld_tau_w_g(1 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_2  > 0) used = send_data(id_cld_tau_w_g_2,  cld_tau_w_g(2 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_3  > 0) used = send_data(id_cld_tau_w_g_3,  cld_tau_w_g(3 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_4  > 0) used = send_data(id_cld_tau_w_g_4,  cld_tau_w_g(4 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_5  > 0) used = send_data(id_cld_tau_w_g_5,  cld_tau_w_g(5 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_6  > 0) used = send_data(id_cld_tau_w_g_6,  cld_tau_w_g(6 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_7  > 0) used = send_data(id_cld_tau_w_g_7,  cld_tau_w_g(7 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_8  > 0) used = send_data(id_cld_tau_w_g_8,  cld_tau_w_g(8 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_9  > 0) used = send_data(id_cld_tau_w_g_9,  cld_tau_w_g(9 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_10 > 0) used = send_data(id_cld_tau_w_g_10, cld_tau_w_g(10, is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_11 > 0) used = send_data(id_cld_tau_w_g_11, cld_tau_w_g(11, is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_12 > 0) used = send_data(id_cld_tau_w_g_12, cld_tau_w_g(12, is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_13 > 0) used = send_data(id_cld_tau_w_g_13, cld_tau_w_g(13, is:ie, js:je, :), Time)
    if (id_cld_tau_w_g_14 > 0) used = send_data(id_cld_tau_w_g_14, cld_tau_w_g(14, is:ie, js:je, :), Time)

    if (id_cld_tau_w_f_1  > 0) used = send_data(id_cld_tau_w_f_1,  cld_tau_w_f(1 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_2  > 0) used = send_data(id_cld_tau_w_f_2,  cld_tau_w_f(2 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_3  > 0) used = send_data(id_cld_tau_w_f_3,  cld_tau_w_f(3 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_4  > 0) used = send_data(id_cld_tau_w_f_4,  cld_tau_w_f(4 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_5  > 0) used = send_data(id_cld_tau_w_f_5,  cld_tau_w_f(5 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_6  > 0) used = send_data(id_cld_tau_w_f_6,  cld_tau_w_f(6 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_7  > 0) used = send_data(id_cld_tau_w_f_7,  cld_tau_w_f(7 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_8  > 0) used = send_data(id_cld_tau_w_f_8,  cld_tau_w_f(8 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_9  > 0) used = send_data(id_cld_tau_w_f_9,  cld_tau_w_f(9 , is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_10 > 0) used = send_data(id_cld_tau_w_f_10, cld_tau_w_f(10, is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_11 > 0) used = send_data(id_cld_tau_w_f_11, cld_tau_w_f(11, is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_12 > 0) used = send_data(id_cld_tau_w_f_12, cld_tau_w_f(12, is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_13 > 0) used = send_data(id_cld_tau_w_f_13, cld_tau_w_f(13, is:ie, js:je, :), Time)
    if (id_cld_tau_w_f_14 > 0) used = send_data(id_cld_tau_w_f_14, cld_tau_w_f(14, is:ie, js:je, :), Time)

    if (id_cld_lw_abs_1  > 0) used = send_data(id_cld_lw_abs_1,  cld_lw_abs(1 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_2  > 0) used = send_data(id_cld_lw_abs_2,  cld_lw_abs(2 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_3  > 0) used = send_data(id_cld_lw_abs_3,  cld_lw_abs(3 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_4  > 0) used = send_data(id_cld_lw_abs_4,  cld_lw_abs(4 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_5  > 0) used = send_data(id_cld_lw_abs_5,  cld_lw_abs(5 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_6  > 0) used = send_data(id_cld_lw_abs_6,  cld_lw_abs(6 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_7  > 0) used = send_data(id_cld_lw_abs_7,  cld_lw_abs(7 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_8  > 0) used = send_data(id_cld_lw_abs_8,  cld_lw_abs(8 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_9  > 0) used = send_data(id_cld_lw_abs_9,  cld_lw_abs(9 , is:ie, js:je, :), Time)
    if (id_cld_lw_abs_10 > 0) used = send_data(id_cld_lw_abs_10, cld_lw_abs(10, is:ie, js:je, :), Time)
    if (id_cld_lw_abs_11 > 0) used = send_data(id_cld_lw_abs_11, cld_lw_abs(11, is:ie, js:je, :), Time)
    if (id_cld_lw_abs_12 > 0) used = send_data(id_cld_lw_abs_12, cld_lw_abs(12, is:ie, js:je, :), Time)
    if (id_cld_lw_abs_13 > 0) used = send_data(id_cld_lw_abs_13, cld_lw_abs(13, is:ie, js:je, :), Time)
    if (id_cld_lw_abs_14 > 0) used = send_data(id_cld_lw_abs_14, cld_lw_abs(14, is:ie, js:je, :), Time)
    if (id_cld_lw_abs_15 > 0) used = send_data(id_cld_lw_abs_15, cld_lw_abs(15, is:ie, js:je, :), Time)
    if (id_cld_lw_abs_16 > 0) used = send_data(id_cld_lw_abs_16, cld_lw_abs(16, is:ie, js:je, :), Time)

    if (id_net_flx_top > 0) used = send_data(id_net_flx_top, net_flx_top(is:ie, js:je), Time)
    if (id_net_flx_srf > 0) used = send_data(id_net_flx_srf, net_flx_srf(is:ie, js:je), Time)

end subroutine rad_rrtmg

!=======================================================================

subroutine raduk_init(axes, time)

    use diag_manager_mod, only: register_diag_field
    use time_manager_mod, only: time_type

    implicit none

!-----------------------------------------------------------------------
! Dummy variables

    integer, intent(in) :: axes(4)

    type(time_type), intent(in) :: time

!-----------------------------------------------------------------------
! Local variables

    real :: missing_value = -1.e10

    character(len = 80) :: mod_name

    integer :: unit, io, ierr                                               ! Open and read namelist file

!-----------------------------------------------------------------------
! Read namelist

	namelist /radiation_nml/ rad_interval, rad_scheme, inflag, liqflag, iceflag

	if ( file_exist('input.nml') ) then
		unit = open_namelist_file()
		ierr = 1
		do while (ierr /= 0)
			read (unit, nml = radiation_nml, iostat = io, end = 10)
			ierr = check_nml_error (io, 'radiation_nml')
		enddo
	10  call close_file (unit)
	endif

!-----------------------------------------------------------------------
! Output register

    mod_name = 'raduk'

    id_rsds   = register_diag_field(trim(mod_name), 'rsds',   axes(1:2), time, 'Surface Incident Shortwave Radiation (positive downward)',             'W/m^2', missing_value=missing_value)
    id_rsus   = register_diag_field(trim(mod_name), 'rsus',   axes(1:2), time, 'Surface Reflected Shortwave Radiation (positive upward)',              'W/m^2', missing_value=missing_value)
    id_rsns   = register_diag_field(trim(mod_name), 'rsns',   axes(1:2), time, 'Surface Net Shortwave Radiation (positive downward)',                  'W/m^2', missing_value=missing_value)
    id_rlds   = register_diag_field(trim(mod_name), 'rlds',   axes(1:2), time, 'Surface Downwelling Longwave Radiation (positive downward)',           'W/m^2', missing_value=missing_value)
    id_rlus   = register_diag_field(trim(mod_name), 'rlus',   axes(1:2), time, 'Surface Upwelling Longwave Radiation (positive upward)',               'W/m^2', missing_value=missing_value)
    id_rlns   = register_diag_field(trim(mod_name), 'rlns',   axes(1:2), time, 'Surface Net Longwave Radiation (positive upward)',                     'W/m^2', missing_value=missing_value)
    id_rds    = register_diag_field(trim(mod_name), 'rds',    axes(1:2), time, 'Surface Downwelling Radiation (positive downward)',                    'W/m^2', missing_value=missing_value)
    id_rus    = register_diag_field(trim(mod_name), 'rus',    axes(1:2), time, 'Surface Upwelling Radiation (positive upward)',                        'W/m^2', missing_value=missing_value)
    id_rsdt   = register_diag_field(trim(mod_name), 'rsdt',   axes(1:2), time, 'TOA Incident Shortwave Radiation (positive downward)',                 'W/m^2', missing_value=missing_value)
    id_rsut   = register_diag_field(trim(mod_name), 'rsut',   axes(1:2), time, 'TOA Reflected Shortwave Radiation (positive upward)',                  'W/m^2', missing_value=missing_value)
    id_rsnt   = register_diag_field(trim(mod_name), 'rsnt',   axes(1:2), time, 'TOA Net Shortwave Radiation (positive downward)',                      'W/m^2', missing_value=missing_value)
    id_rldt   = register_diag_field(trim(mod_name), 'rldt',   axes(1:2), time, 'TOA Incoming Longwave Radiation (positive downward)',                  'W/m^2', missing_value=missing_value)
    id_rlut   = register_diag_field(trim(mod_name), 'rlut',   axes(1:2), time, 'TOA Outgoing Longwave Radiation (positive upward)',                    'W/m^2', missing_value=missing_value)
    id_rlnt   = register_diag_field(trim(mod_name), 'rlnt',   axes(1:2), time, 'TOA Net Longwave Radiation (positive upward)',                         'W/m^2', missing_value=missing_value)
    id_rdt    = register_diag_field(trim(mod_name), 'rdt',    axes(1:2), time, 'TOA Downwelling Radiation (positive downward)',                        'W/m^2', missing_value=missing_value)
    id_rut    = register_diag_field(trim(mod_name), 'rut',    axes(1:2), time, 'TOA Upwelling Radiation (positive upward)',                            'W/m^2', missing_value=missing_value)
    id_rsdscs = register_diag_field(trim(mod_name), 'rsdscs', axes(1:2), time, 'Surface Incident Clear-sky Shortwave Radiation (positive downward)',   'W/m^2', missing_value=missing_value)
    id_rsuscs = register_diag_field(trim(mod_name), 'rsuscs', axes(1:2), time, 'Surface Reflected Clear-sky Shortwave Radiation (positive upward)',    'W/m^2', missing_value=missing_value)
    id_rsnscs = register_diag_field(trim(mod_name), 'rsnscs', axes(1:2), time, 'Surface Net Clear-sky Shortwave Radiation (positive downward)',        'W/m^2', missing_value=missing_value)
    id_rldscs = register_diag_field(trim(mod_name), 'rldscs', axes(1:2), time, 'Surface Downwelling Clear-sky Longwave Radiation (positive downward)', 'W/m^2', missing_value=missing_value)
    id_rluscs = register_diag_field(trim(mod_name), 'rluscs', axes(1:2), time, 'Surface Upwelling Clear-sky Longwave Radiation (positive upward)',     'W/m^2', missing_value=missing_value)
    id_rlnscs = register_diag_field(trim(mod_name), 'rlnscs', axes(1:2), time, 'Surface Net Clear-sky Longwave Radiation (positive upward)',           'W/m^2', missing_value=missing_value)
    id_rdscs  = register_diag_field(trim(mod_name), 'rdscs',  axes(1:2), time, 'Surface Downwelling Clear-sky Radiation (positive downward)',          'W/m^2', missing_value=missing_value)
    id_ruscs  = register_diag_field(trim(mod_name), 'ruscs',  axes(1:2), time, 'Surface Upwelling Clear-sky Radiation (positive upward)',              'W/m^2', missing_value=missing_value)
    id_rsdtcs = register_diag_field(trim(mod_name), 'rsdtcs', axes(1:2), time, 'TOA Incident Clear-sky Shortwave Radiation (positive downward)',       'W/m^2', missing_value=missing_value)
    id_rsutcs = register_diag_field(trim(mod_name), 'rsutcs', axes(1:2), time, 'TOA Reflected Clear-sky Shortwave Radiation (positive upward)',        'W/m^2', missing_value=missing_value)
    id_rsntcs = register_diag_field(trim(mod_name), 'rsntcs', axes(1:2), time, 'TOA Net Clear-sky Shortwave Radiation (positive downward)',            'W/m^2', missing_value=missing_value)
    id_rldtcs = register_diag_field(trim(mod_name), 'rldtcs', axes(1:2), time, 'TOA Downwelling Clear-sky Longwave Radiation (positive downward)',     'W/m^2', missing_value=missing_value)
    id_rlutcs = register_diag_field(trim(mod_name), 'rlutcs', axes(1:2), time, 'TOA Upwelling Clear-sky Longwave Radiation (positive upward)',         'W/m^2', missing_value=missing_value)
    id_rlntcs = register_diag_field(trim(mod_name), 'rlntcs', axes(1:2), time, 'TOA Net Clear-sky Longwave Radiation (positive upward)',               'W/m^2', missing_value=missing_value)
    id_rdtcs  = register_diag_field(trim(mod_name), 'rdtcs',  axes(1:2), time, 'TOA Downwelling Clear-sky Radiation (positive downward)',              'W/m^2', missing_value=missing_value)
    id_rutcs  = register_diag_field(trim(mod_name), 'rutcs',  axes(1:2), time, 'TOA Upwelling Clear-sky Radiation (positive upward)',                  'W/m^2', missing_value=missing_value)

    id_qrl    = register_diag_field(trim(mod_name), 'qrl',    axes(1:3), time, 'Longwave Radiation Heating Rate',  'K/s',   missing_value=missing_value)
    id_qrs    = register_diag_field(trim(mod_name), 'qrs',    axes(1:3), time, 'Shortwave Radiation Heating Rate', 'K/s',   missing_value=missing_value)
    id_swcf   = register_diag_field(trim(mod_name), 'swcf',   axes(1:2), time, 'Shortwave Cloud Forcing',          'w/m^2', missing_value=missing_value)
    id_lwcf   = register_diag_field(trim(mod_name), 'lwcf',   axes(1:2), time, 'Longwave Cloud Forcing',           'w/m^2', missing_value=missing_value)
    id_ozone  = register_diag_field(trim(mod_name), 'ozone',  axes(1:3), time, 'Ozone',                            'kg/kg', missing_value=missing_value)

    id_aodvis   = register_diag_field(trim(mod_name), 'aodvis',   axes(1:2), time, 'Aerosol Optical Depth (550 nm)',           '1', missing_value=missing_value)
    id_aoddust  = register_diag_field(trim(mod_name), 'aoddust',  axes(1:2), time, 'Total dust optical depth',                 '1', missing_value=missing_value)
    id_aoddust1 = register_diag_field(trim(mod_name), 'aoddust1', axes(1:2), time, 'Total dust optical depth at ultra band',   '1', missing_value=missing_value)
    id_aoddust2 = register_diag_field(trim(mod_name), 'aoddust2', axes(1:2), time, 'Total dust optical depth at visible band', '1', missing_value=missing_value)
    id_aoddust3 = register_diag_field(trim(mod_name), 'aoddust3', axes(1:2), time, 'Total dust optical depth at infra band',   '1', missing_value=missing_value)

    id_clw    = register_diag_field(trim(mod_name), 'clw',    axes(1:3), time, 'Cloud Water (liquid and solid phase)',                       'g/m^2', missing_value=missing_value)
    id_cli    = register_diag_field(trim(mod_name), 'cli',    axes(1:3), time, 'Cloud Ice',                                                  'g/m^2', missing_value=missing_value)
    id_clwvi  = register_diag_field(trim(mod_name), 'clwvi',  axes(1:2), time, 'Vertically Integrated Cloud Water (liquid and solid phase)', 'g/m^2', missing_value=missing_value)
    id_clivi  = register_diag_field(trim(mod_name), 'clivi',  axes(1:2), time, 'Vertically Integrated Cloud Ice',                            'g/m^2', missing_value=missing_value)
    id_cldliq = register_diag_field(trim(mod_name), 'cldliq', axes(1:3), time, 'Cloud Liquid Water Mixing Rate',                             'kg/kg', missing_value=missing_value)
    id_cldice = register_diag_field(trim(mod_name), 'cldice', axes(1:3), time, 'Cloud Ice Water Mixing Rate',                                'kg/kg', missing_value=missing_value)

    id_ttauda = register_diag_field(trim(mod_name), 'ttauda', axes(1:2), time, 'Fraction of Day', '1', missing_value=missing_value)

    id_so4    = register_diag_field(trim(mod_name), 'so4',    axes(1:3), time, 'SO4 Mixing Ratio',    'kg/kg', missing_value=missing_value)
    id_soa    = register_diag_field(trim(mod_name), 'soa',    axes(1:3), time, 'SOA Mixing Ratio',    'kg/kg', missing_value=missing_value)
    id_cb1    = register_diag_field(trim(mod_name), 'cb1',    axes(1:3), time, 'CB1 Mixing Ratio',    'kg/kg', missing_value=missing_value)
    id_cb2    = register_diag_field(trim(mod_name), 'cb2',    axes(1:3), time, 'CB2 Mixing Ratio',    'kg/kg', missing_value=missing_value)
    id_oc1    = register_diag_field(trim(mod_name), 'oc1',    axes(1:3), time, 'OC1 Mixing Ratio',    'kg/kg', missing_value=missing_value)
    id_oc2    = register_diag_field(trim(mod_name), 'oc2',    axes(1:3), time, 'OC2 Mixing Ratio',    'kg/kg', missing_value=missing_value)
    id_volc   = register_diag_field(trim(mod_name), 'volc',   axes(1:3), time, 'VOLC Mixing Ratio',   'kg/kg', missing_value=missing_value)
    id_dst01  = register_diag_field(trim(mod_name), 'dst01',  axes(1:3), time, 'DST01 Mixing Ratio',  'kg/kg', missing_value=missing_value)
    id_dst02  = register_diag_field(trim(mod_name), 'dst02',  axes(1:3), time, 'DST02 Mixing Ratio',  'kg/kg', missing_value=missing_value)
    id_dst03  = register_diag_field(trim(mod_name), 'dst03',  axes(1:3), time, 'DST03 Mixing Ratio',  'kg/kg', missing_value=missing_value)
    id_dst04  = register_diag_field(trim(mod_name), 'dst04',  axes(1:3), time, 'DST04 Mixing Ratio',  'kg/kg', missing_value=missing_value)
    id_sslt01 = register_diag_field(trim(mod_name), 'sslt01', axes(1:3), time, 'SSLT01 Mixing Ratio', 'kg/kg', missing_value=missing_value)
    id_sslt02 = register_diag_field(trim(mod_name), 'sslt02', axes(1:3), time, 'SSLT02 Mixing Ratio', 'kg/kg', missing_value=missing_value)
    id_sslt03 = register_diag_field(trim(mod_name), 'sslt03', axes(1:3), time, 'SSLT03 Mixing Ratio', 'kg/kg', missing_value=missing_value)
    id_sslt04 = register_diag_field(trim(mod_name), 'sslt04', axes(1:3), time, 'SSLT04 Mixing Ratio', 'kg/kg', missing_value=missing_value)

    id_tau_1  = register_diag_field(trim(mod_name), 'tau_1',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_2  = register_diag_field(trim(mod_name), 'tau_2',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_3  = register_diag_field(trim(mod_name), 'tau_3',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_4  = register_diag_field(trim(mod_name), 'tau_4',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_5  = register_diag_field(trim(mod_name), 'tau_5',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_6  = register_diag_field(trim(mod_name), 'tau_6',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_7  = register_diag_field(trim(mod_name), 'tau_7',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_8  = register_diag_field(trim(mod_name), 'tau_8',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_9  = register_diag_field(trim(mod_name), 'tau_9',  axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_10 = register_diag_field(trim(mod_name), 'tau_10', axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_11 = register_diag_field(trim(mod_name), 'tau_11', axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_12 = register_diag_field(trim(mod_name), 'tau_12', axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_13 = register_diag_field(trim(mod_name), 'tau_13', axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)
    id_tau_14 = register_diag_field(trim(mod_name), 'tau_14', axes(1:3), time, 'Aerosol Extinction Optical Depth', '1', missing_value=missing_value)

    id_tau_w_1  = register_diag_field(trim(mod_name), 'tau_w_1',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_2  = register_diag_field(trim(mod_name), 'tau_w_2',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_3  = register_diag_field(trim(mod_name), 'tau_w_3',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_4  = register_diag_field(trim(mod_name), 'tau_w_4',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_5  = register_diag_field(trim(mod_name), 'tau_w_5',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_6  = register_diag_field(trim(mod_name), 'tau_w_6',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_7  = register_diag_field(trim(mod_name), 'tau_w_7',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_8  = register_diag_field(trim(mod_name), 'tau_w_8',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_9  = register_diag_field(trim(mod_name), 'tau_w_9',  axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_10 = register_diag_field(trim(mod_name), 'tau_w_10', axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_11 = register_diag_field(trim(mod_name), 'tau_w_11', axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_12 = register_diag_field(trim(mod_name), 'tau_w_12', axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_13 = register_diag_field(trim(mod_name), 'tau_w_13', axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)
    id_tau_w_14 = register_diag_field(trim(mod_name), 'tau_w_14', axes(1:3), time, 'Aerosol Single Scattering Albedo', '1', missing_value=missing_value)

    id_tau_w_g_1  = register_diag_field(trim(mod_name), 'tau_w_g_1',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_2  = register_diag_field(trim(mod_name), 'tau_w_g_2',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_3  = register_diag_field(trim(mod_name), 'tau_w_g_3',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_4  = register_diag_field(trim(mod_name), 'tau_w_g_4',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_5  = register_diag_field(trim(mod_name), 'tau_w_g_5',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_6  = register_diag_field(trim(mod_name), 'tau_w_g_6',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_7  = register_diag_field(trim(mod_name), 'tau_w_g_7',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_8  = register_diag_field(trim(mod_name), 'tau_w_g_8',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_9  = register_diag_field(trim(mod_name), 'tau_w_g_9',  axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_10 = register_diag_field(trim(mod_name), 'tau_w_g_10', axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_11 = register_diag_field(trim(mod_name), 'tau_w_g_11', axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_12 = register_diag_field(trim(mod_name), 'tau_w_g_12', axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_13 = register_diag_field(trim(mod_name), 'tau_w_g_13', axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)
    id_tau_w_g_14 = register_diag_field(trim(mod_name), 'tau_w_g_14', axes(1:3), time, 'Aerosol Assymetry Parameter', '1', missing_value=missing_value)

    id_tau_w_f_1  = register_diag_field(trim(mod_name), 'tau_w_f_1',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_2  = register_diag_field(trim(mod_name), 'tau_w_f_2',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_3  = register_diag_field(trim(mod_name), 'tau_w_f_3',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_4  = register_diag_field(trim(mod_name), 'tau_w_f_4',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_5  = register_diag_field(trim(mod_name), 'tau_w_f_5',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_6  = register_diag_field(trim(mod_name), 'tau_w_f_6',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_7  = register_diag_field(trim(mod_name), 'tau_w_f_7',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_8  = register_diag_field(trim(mod_name), 'tau_w_f_8',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_9  = register_diag_field(trim(mod_name), 'tau_w_f_9',  axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_10 = register_diag_field(trim(mod_name), 'tau_w_f_10', axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_11 = register_diag_field(trim(mod_name), 'tau_w_f_11', axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_12 = register_diag_field(trim(mod_name), 'tau_w_f_12', axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_13 = register_diag_field(trim(mod_name), 'tau_w_f_13', axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)
    id_tau_w_f_14 = register_diag_field(trim(mod_name), 'tau_w_f_14', axes(1:3), time, 'Aerosol Forward Scattered Fraction', '1', missing_value=missing_value)

    id_odap_aer_1  = register_diag_field(trim(mod_name), 'odap_aer_1',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_2  = register_diag_field(trim(mod_name), 'odap_aer_2',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_3  = register_diag_field(trim(mod_name), 'odap_aer_3',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_4  = register_diag_field(trim(mod_name), 'odap_aer_4',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_5  = register_diag_field(trim(mod_name), 'odap_aer_5',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_6  = register_diag_field(trim(mod_name), 'odap_aer_6',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_7  = register_diag_field(trim(mod_name), 'odap_aer_7',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_8  = register_diag_field(trim(mod_name), 'odap_aer_8',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_9  = register_diag_field(trim(mod_name), 'odap_aer_9',  axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_10 = register_diag_field(trim(mod_name), 'odap_aer_10', axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_11 = register_diag_field(trim(mod_name), 'odap_aer_11', axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_12 = register_diag_field(trim(mod_name), 'odap_aer_12', axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_13 = register_diag_field(trim(mod_name), 'odap_aer_13', axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_14 = register_diag_field(trim(mod_name), 'odap_aer_14', axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_15 = register_diag_field(trim(mod_name), 'odap_aer_15', axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_odap_aer_16 = register_diag_field(trim(mod_name), 'odap_aer_16', axes(1:3), time, 'Aerosol Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)

    id_cld_tau_1  = register_diag_field(trim(mod_name), 'cld_tau_1',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_2  = register_diag_field(trim(mod_name), 'cld_tau_2',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_3  = register_diag_field(trim(mod_name), 'cld_tau_3',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_4  = register_diag_field(trim(mod_name), 'cld_tau_4',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_5  = register_diag_field(trim(mod_name), 'cld_tau_5',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_6  = register_diag_field(trim(mod_name), 'cld_tau_6',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_7  = register_diag_field(trim(mod_name), 'cld_tau_7',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_8  = register_diag_field(trim(mod_name), 'cld_tau_8',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_9  = register_diag_field(trim(mod_name), 'cld_tau_9',  axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_10 = register_diag_field(trim(mod_name), 'cld_tau_10', axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_11 = register_diag_field(trim(mod_name), 'cld_tau_11', axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_12 = register_diag_field(trim(mod_name), 'cld_tau_12', axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_13 = register_diag_field(trim(mod_name), 'cld_tau_13', axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)
    id_cld_tau_14 = register_diag_field(trim(mod_name), 'cld_tau_14', axes(1:3), time, 'Cloud Extinction Optical Depth', '1', missing_value=missing_value)

    id_cld_tau_w_1  = register_diag_field(trim(mod_name), 'cld_tau_w_1',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_2  = register_diag_field(trim(mod_name), 'cld_tau_w_2',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_3  = register_diag_field(trim(mod_name), 'cld_tau_w_3',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_4  = register_diag_field(trim(mod_name), 'cld_tau_w_4',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_5  = register_diag_field(trim(mod_name), 'cld_tau_w_5',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_6  = register_diag_field(trim(mod_name), 'cld_tau_w_6',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_7  = register_diag_field(trim(mod_name), 'cld_tau_w_7',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_8  = register_diag_field(trim(mod_name), 'cld_tau_w_8',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_9  = register_diag_field(trim(mod_name), 'cld_tau_w_9',  axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_10 = register_diag_field(trim(mod_name), 'cld_tau_w_10', axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_11 = register_diag_field(trim(mod_name), 'cld_tau_w_11', axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_12 = register_diag_field(trim(mod_name), 'cld_tau_w_12', axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_13 = register_diag_field(trim(mod_name), 'cld_tau_w_13', axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)
    id_cld_tau_w_14 = register_diag_field(trim(mod_name), 'cld_tau_w_14', axes(1:3), time, 'Cloud Single Scattering Albedo', '1', missing_value=missing_value)

    id_cld_tau_w_g_1  = register_diag_field(trim(mod_name), 'cld_tau_w_g_1',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_2  = register_diag_field(trim(mod_name), 'cld_tau_w_g_2',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_3  = register_diag_field(trim(mod_name), 'cld_tau_w_g_3',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_4  = register_diag_field(trim(mod_name), 'cld_tau_w_g_4',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_5  = register_diag_field(trim(mod_name), 'cld_tau_w_g_5',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_6  = register_diag_field(trim(mod_name), 'cld_tau_w_g_6',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_7  = register_diag_field(trim(mod_name), 'cld_tau_w_g_7',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_8  = register_diag_field(trim(mod_name), 'cld_tau_w_g_8',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_9  = register_diag_field(trim(mod_name), 'cld_tau_w_g_9',  axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_10 = register_diag_field(trim(mod_name), 'cld_tau_w_g_10', axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_11 = register_diag_field(trim(mod_name), 'cld_tau_w_g_11', axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_12 = register_diag_field(trim(mod_name), 'cld_tau_w_g_12', axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_13 = register_diag_field(trim(mod_name), 'cld_tau_w_g_13', axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)
    id_cld_tau_w_g_14 = register_diag_field(trim(mod_name), 'cld_tau_w_g_14', axes(1:3), time, 'Cloud Assymetry Parameter', '1', missing_value=missing_value)

    id_cld_tau_w_f_1  = register_diag_field(trim(mod_name), 'cld_tau_w_f_1',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_2  = register_diag_field(trim(mod_name), 'cld_tau_w_f_2',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_3  = register_diag_field(trim(mod_name), 'cld_tau_w_f_3',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_4  = register_diag_field(trim(mod_name), 'cld_tau_w_f_4',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_5  = register_diag_field(trim(mod_name), 'cld_tau_w_f_5',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_6  = register_diag_field(trim(mod_name), 'cld_tau_w_f_6',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_7  = register_diag_field(trim(mod_name), 'cld_tau_w_f_7',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_8  = register_diag_field(trim(mod_name), 'cld_tau_w_f_8',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_9  = register_diag_field(trim(mod_name), 'cld_tau_w_f_9',  axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_10 = register_diag_field(trim(mod_name), 'cld_tau_w_f_10', axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_11 = register_diag_field(trim(mod_name), 'cld_tau_w_f_11', axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_12 = register_diag_field(trim(mod_name), 'cld_tau_w_f_12', axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_13 = register_diag_field(trim(mod_name), 'cld_tau_w_f_13', axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)
    id_cld_tau_w_f_14 = register_diag_field(trim(mod_name), 'cld_tau_w_f_14', axes(1:3), time, 'Cloud Forward Scattered Fraction', '1', missing_value=missing_value)

    id_cld_lw_abs_1  = register_diag_field(trim(mod_name), 'cld_lw_abs_1',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_2  = register_diag_field(trim(mod_name), 'cld_lw_abs_2',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_3  = register_diag_field(trim(mod_name), 'cld_lw_abs_3',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_4  = register_diag_field(trim(mod_name), 'cld_lw_abs_4',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_5  = register_diag_field(trim(mod_name), 'cld_lw_abs_5',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_6  = register_diag_field(trim(mod_name), 'cld_lw_abs_6',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_7  = register_diag_field(trim(mod_name), 'cld_lw_abs_7',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_8  = register_diag_field(trim(mod_name), 'cld_lw_abs_8',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_9  = register_diag_field(trim(mod_name), 'cld_lw_abs_9',  axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_10 = register_diag_field(trim(mod_name), 'cld_lw_abs_10', axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_11 = register_diag_field(trim(mod_name), 'cld_lw_abs_11', axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_12 = register_diag_field(trim(mod_name), 'cld_lw_abs_12', axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_13 = register_diag_field(trim(mod_name), 'cld_lw_abs_13', axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_14 = register_diag_field(trim(mod_name), 'cld_lw_abs_14', axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_15 = register_diag_field(trim(mod_name), 'cld_lw_abs_15', axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)
    id_cld_lw_abs_16 = register_diag_field(trim(mod_name), 'cld_lw_abs_16', axes(1:3), time, 'Cloud Absorption Optical Depth, Per Layer', '1', missing_value=missing_value)

    id_swuflx_sf  = register_diag_field(trim(mod_name), 'swuflx_sf',  axes(1:2), time, 'Total Sky Shortwave Upward Flux, Surface',            'W/m2', missing_value=missing_value)
    id_swdflx_sf  = register_diag_field(trim(mod_name), 'swdflx_sf',  axes(1:2), time, 'Total Sky Shortwave Downward Flux, Surface',          'W/m2', missing_value=missing_value)
    id_swuflxc_sf = register_diag_field(trim(mod_name), 'swuflxc_sf', axes(1:2), time, 'Clear Sky Shortwave Upward Flux, Surface',            'W/m2', missing_value=missing_value)
    id_swdflxc_sf = register_diag_field(trim(mod_name), 'swdflxc_sf', axes(1:2), time, 'Clear Sky Shortwave Downward Flux, Surface',          'W/m2', missing_value=missing_value)
    id_swuflx_lv  = register_diag_field(trim(mod_name), 'swuflx_lv',  axes(1:3), time, 'Total Sky Shortwave Upward Flux, Model Half Level',   'W/m2', missing_value=missing_value)
    id_swdflx_lv  = register_diag_field(trim(mod_name), 'swdflx_lv',  axes(1:3), time, 'Total Sky Shortwave Downward Flux, Model Half Level', 'W/m2', missing_value=missing_value)
    id_swuflxc_lv = register_diag_field(trim(mod_name), 'swuflxc_lv', axes(1:3), time, 'Clear Sky Shortwave Upward Flux, Model Half Level',   'W/m2', missing_value=missing_value)
    id_swdflxc_lv = register_diag_field(trim(mod_name), 'swdflxc_lv', axes(1:3), time, 'Clear Sky Shortwave Downward Flux, Model Half Level', 'W/m2', missing_value=missing_value)
    id_swhr       = register_diag_field(trim(mod_name), 'swhr',       axes(1:3), time, 'Total Sky Shortwave Heat Rate',                       'K/s',  missing_value=missing_value)
    id_swhrc      = register_diag_field(trim(mod_name), 'swhrc',      axes(1:3), time, 'Clear Sky Shortwave Heat Rate',                       'K/s',  missing_value=missing_value)

    id_lwuflx_sf  = register_diag_field(trim(mod_name), 'lwuflx_sf',  axes(1:2), time, 'Total Sky Longwave Upward Flux, Surface',             'W/m2', missing_value=missing_value)
    id_lwdflx_sf  = register_diag_field(trim(mod_name), 'lwdflx_sf',  axes(1:2), time, 'Total Sky Longwave Downward Flux, Surface',           'W/m2', missing_value=missing_value)
    id_lwuflxc_sf = register_diag_field(trim(mod_name), 'lwuflxc_sf', axes(1:2), time, 'Clear Sky Longwave Upward Flux, Surface',             'W/m2', missing_value=missing_value)
    id_lwdflxc_sf = register_diag_field(trim(mod_name), 'lwdflxc_sf', axes(1:2), time, 'Clear Sky Longwave Downward Flux, Surface',           'W/m2', missing_value=missing_value)
    id_lwuflx_lv  = register_diag_field(trim(mod_name), 'lwuflx_lv',  axes(1:3), time, 'Total Sky Longwave Upward Flux, Model Half Level',    'W/m2', missing_value=missing_value)
    id_lwdflx_lv  = register_diag_field(trim(mod_name), 'lwdflx_lv',  axes(1:3), time, 'Total Sky Longwave Downward Flux, Model Half Level',  'W/m2', missing_value=missing_value)
    id_lwuflxc_lv = register_diag_field(trim(mod_name), 'lwuflxc_lv', axes(1:3), time, 'Clear Sky Longwave Upward Flux, Model Half Level',    'W/m2', missing_value=missing_value)
    id_lwdflxc_lv = register_diag_field(trim(mod_name), 'lwdflxc_lv', axes(1:3), time, 'Clear Sky Longwave Downward Flux, Model Half Level',  'W/m2', missing_value=missing_value)
    id_lwhr       = register_diag_field(trim(mod_name), 'lwhr',       axes(1:3), time, 'Total Sky Longwave Heat Rate',                        'K/s',  missing_value=missing_value)
    id_lwhrc      = register_diag_field(trim(mod_name), 'lwhrc',      axes(1:3), time, 'Clear Sky Longwave Heat Rate',                        'K/s',  missing_value=missing_value)

    id_ccosz = register_diag_field(trim(mod_name), 'ccosz', axes(1:2), time, 'Solar Zenith Angle',                                            '1', missing_value=missing_value)
    id_ttaud = register_diag_field(trim(mod_name), 'ttaud', axes(1:2), time, 'Fraction of Day (or of Timestep) that Sun is above Horizontal', '1', missing_value=missing_value)

    mod_name = 'check_energy'

    id_net_flx_top = register_diag_field(trim(mod_name), 'net_flx_top', axes(1:2), time, 'Net Fluxes From The Radiation At Model Top',    'W/m^2', missing_value=missing_value)
    id_net_flx_srf = register_diag_field(trim(mod_name), 'net_flx_srf', axes(1:2), time, 'Net Fluxes From The Radiation At Model Bottom', 'W/m^2', missing_value=missing_value)

end subroutine raduk_init

end module raduk_mod
