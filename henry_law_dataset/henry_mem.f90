!*****************************************************************************
!                   Time-stamp: <2015-02-11 19:14:48 sander>
!      Author: Rolf Sander, Max-Planck Institute Mainz, Germany 1996-2014
!*****************************************************************************

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! You should have received a copy of the GNU General Public License
! along with this program; if not, get it from:
! http://www.gnu.org/copyleft/gpl.html

!*****************************************************************************

MODULE henry_mem

  IMPLICIT NONE
  SAVE

  LOGICAL, PARAMETER :: L_MAKEINDEX = .FALSE. ! see xhenry

  INTEGER, PARAMETER :: STRLEN       =    30
  INTEGER, PARAMETER :: STRLEN_VLONG =    80
  INTEGER, PARAMETER :: STRLEN_ULONG =   256
  INTEGER, PARAMETER :: STRLEN_HUGE  = 10000

  ! I/O units
  ! reserved for reading input:   10
  INTEGER, PARAMETER :: IO_CONV = 21 ! tex
  INTEGER, PARAMETER :: IO_DATA = 22 ! tex
  INTEGER, PARAMETER :: IO_DEFS = 23 ! tex
  INTEGER, PARAMETER :: IO_NOTE = 24 ! tex
  INTEGER, PARAMETER :: IO_WARN = 30
  INTEGER, PARAMETER :: IO_ODBH = 31
  INTEGER, PARAMETER :: IO_ODBS = 32
  INTEGER, PARAMETER :: IO_DEBG = 33
  INTEGER, PARAMETER :: IO_SPCA = 34
  INTEGER, PARAMETER :: IO_SPCB = 35
  INTEGER, PARAMETER :: IO_SPCC = 36
  INTEGER, PARAMETER :: IO_PICS = 37
  INTEGER, PARAMETER :: IO_GNUP = 38 ! gnuplot
  INTEGER, PARAMETER :: IO_NML  = 41 ! namelist
  ! f90 output files:
  INTEGER, PARAMETER :: IO_SOLUB  = 60 ! reserve 61...60+N_SOLUB
  INTEGER, PARAMETER :: IO_VOLAT  = 70 ! reserve 71...70+N_VOLAT

  ! molar masses [kg/mol]
  ! elements
  REAL, PARAMETER :: MH  =   1.00794E-3
  REAL, PARAMETER :: MC  =  12.011E-3
  REAL, PARAMETER :: MN  =  14.0067E-3
  REAL, PARAMETER :: MO  =  15.9994E-3
  REAL, PARAMETER :: MF  =  18.998E-3
  REAL, PARAMETER :: MNa =  22.98977E-3
  REAL, PARAMETER :: MMg =  24.305E-3
  REAL, PARAMETER :: MS  =  32.06E-3
  REAL, PARAMETER :: MCl =  35.453E-3
  REAL, PARAMETER :: MK  =  39.0983E-3
  REAL, PARAMETER :: MCa =  40.08E-3
  REAL, PARAMETER :: MBr =  79.904E-3
  REAL, PARAMETER :: MI  = 126.9045E-3
  ! compounds
  REAL, PARAMETER :: MH2O = MH*2.+MO

  ! define some constants
  REAL, PARAMETER :: rhoH2O = 997.        ! density at T0 (CRC handbook) [kg/m3]
  REAL, PARAMETER :: cH2O   = rhoH2O/MH2O ! conc of H2O [mol/m3]
  REAL, PARAMETER :: p0     = 101325.     ! standard pressure [Pa]
  REAL, PARAMETER :: T0     = 298.15      ! standard temperature [K]
  REAL, PARAMETER :: Tstp   = 273.15      ! STP temperature [K]
  REAL, PARAMETER :: Rgas    = 8.3144621  ! gas constant [J/(mol*K)]

  ! define some non-SI conversion factors
  REAL, PARAMETER :: percent = 0.01
  REAL, PARAMETER :: kilo = 1E3
  REAL, PARAMETER :: atm = 101325.   ! [Pa]
  REAL, PARAMETER :: mmHg = atm/760. ! [Pa] (mmHg=Torr)
  REAL, PARAMETER :: bar = 1E5       ! [Pa]
  REAL, PARAMETER :: dm3 = 1E-3      ! [m3]
  REAL, PARAMETER :: cal = 4.184     ! [J]
  REAL, PARAMETER :: kcal = kilo*cal ! [J]
  REAL, PARAMETER :: CtoK = 273.15

  ! conversion factors for H (solubilities)
  REAL, PARAMETER :: alpha_TO_HcpSI   = 1./(Rgas*Tstp) ! 4.403e-4
  REAL, PARAMETER :: S_TO_HcpSI        = 1E-3*rhoH2O/(Rgas*Tstp) ! for Kuenen in [cm3/g]
  REAL, PARAMETER :: Hb2p_TO_Hc2pSI    = rhoH2O**2/atm
  REAL, PARAMETER :: HbpSI_TO_HcpSI    = rhoH2O
  REAL, PARAMETER :: Hbp_TO_HcpSI      = rhoH2O/atm
  REAL, PARAMETER :: Hc2p_TO_Hc2pSI    = 1./(atm*dm3*dm3)
  !                  Hcc_TO_HcpSI      = see function definition
  REAL, PARAMETER :: Hcc_TO_HcpSI_atT0 = 1./(Rgas*T0)
  REAL, PARAMETER :: Hcp_TO_HcpSI      = 1./(atm*dm3)
  REAL, PARAMETER :: Hxp_TO_HcpSI      = cH2O/atm

  ! products of H (solubilities) and KH (volatilities)
  REAL, PARAMETER :: KHpcSI_TIMES_HcpSI    = 1.
  REAL, PARAMETER :: KHpc_TIMES_HcpSI      = 1./atm
  REAL, PARAMETER :: KHpx_TIMES_HcpSI      = cH2O/atm ! for KHpx in [atm]
  !                  KHcc_TO_HcpSI         = see function definition
  REAL, PARAMETER :: KHcc_TIMES_HcpSI_atT0 = 1./(Rgas*T0)

  INTEGER, PARAMETER :: N_SOLUB=7, N_VOLAT=4
  ! conversion factors:
  REAL, PARAMETER, DIMENSION(N_SOLUB) :: conv_solub = (/ &
    1.,                       &  ! HcpSI  to    HcpSI
    Hcp_TO_HcpSI,             &  ! Hcp    to    HcpSI
    Hcc_TO_HcpSI_atT0,        &  ! Hcc    to    HcpSI
    HbpSI_TO_HcpSI,           &  ! HbpSI  to    HcpSI
    Hbp_TO_HcpSI,             &  ! Hbp    to    HcpSI
    Hxp_TO_HcpSI,             &  ! Hxp    to    HcpSI
    alpha_TO_HcpSI           /) ! alpha  to    HcpSI
  REAL, PARAMETER, DIMENSION(N_VOLAT) :: conv_volat = (/ &
    KHpx_TIMES_HcpSI,         &  ! KHpx   times HcpSI
    KHpcSI_TIMES_HcpSI,       &  ! KHpcSI times HcpSI
    KHpcSI_TIMES_HcpSI/atm,   &  ! KHpc   times HcpSI
    KHcc_TIMES_HcpSI_atT0     /) ! KHcc   times HcpSI
  ! offset for mindHR:
  REAL, PARAMETER, DIMENSION(N_SOLUB) :: mindHR_offset = (/ &
    0., &  ! HcpSI
    0., &  ! Hcp
    T0, &  ! Hcc
    0., &  ! HbpSI
    0., &  ! Hbp
    0., &  ! Hxp
    0.  /) ! alpha
  REAL, PARAMETER, DIMENSION(N_VOLAT) :: mindHR_inv_offset = (/ &
    0., &  ! KHpx
    0., &  ! KHpcSI
    0., &  ! KHpc
    T0  /) ! KHcc
  ! symbols (in TeX syntax):
  CHARACTER(LEN=STRLEN), DIMENSION(N_SOLUB) :: texsymbol_solub = (/ &
    "H^{cp}", &  ! HcpSI
    "H^{cp}", &  ! Hcp
    "H^{cc}", &  ! Hcc
    "H^{bp}", &  ! HbpSI
    "H^{bp}", &  ! Hbp
    "H^{xp}", &  ! Hxp
    "\alpha"  /) ! alpha
  CHARACTER(LEN=STRLEN), DIMENSION(N_VOLAT) :: texsymbol_volat = (/ &
    "\KHpx", &  ! KHpx
    "\KHpc", &  ! KHpcSI
    "\KHpc", &  ! KHpc
    "\KHcc"  /) ! KHcc
  ! symbols (in plain text):
  CHARACTER(LEN=STRLEN), DIMENSION(N_SOLUB) :: txtsymbol_solub = (/ &
    "HcpSI ", &
    "Hcp   ", &
    "Hcc   ", &
    "HbpSI ", &
    "Hbp   ", &
    "Hxp   ", &
    "alpha "  /)
  CHARACTER(LEN=STRLEN), DIMENSION(N_VOLAT) :: txtsymbol_volat = (/ &
    "KHpx  ", &
    "KHpcSI", &
    "KHpc  ", &
    "KHcc  "  /)
  ! units shown as a fraction (in TeX syntax):
  CHARACTER(LEN=STRLEN), DIMENSION(N_SOLUB) :: unit_fr = (/ &
    "\DS\frac{mol}{m^3~Pa}", &  ! HcpSI
    "\DS\frac{M}{atm}     ", &  ! Hcp
    "                     ", &  ! Hcc
    "\DS\frac{mol}{kg~Pa} ", &  ! HbpSI
    "\DS\frac{mol}{kg~atm}", &  ! Hbp
    "\DS\frac{1}{atm}     ", &  ! Hxp
    "                     "  /) ! alpha
  CHARACTER(LEN=STRLEN), DIMENSION(N_VOLAT) :: unit_fr_inv = (/ &
    "atm                   ", &  ! KHpx
    "\DS\frac{m^3~Pa}{mol} ", &  ! KHpcSI
    "\DS\frac{m^3~atm}{mol}", &  ! KHpc
    "                      "  /) ! KHcc
  ! units shown as one line (in TeX syntax):
  CHARACTER(LEN=STRLEN), DIMENSION(N_SOLUB) :: unit_ol = (/ &
    "mol/(m^3~Pa)", &  ! HcpSI
    "M/atm       ", &  ! Hcp
    "1           ", &  ! Hcc
    "mol/(kg~Pa) ", &  ! HbpSI
    "mol/(kg~atm)", &  ! Hbp
    "1/atm       ", &  ! Hxp
    "1           "  /) ! alpha
  CHARACTER(LEN=STRLEN), DIMENSION(N_VOLAT) :: unit_ol_inv = (/ &
    "atm         ", &  ! KHpx
    "m^3~Pa/mol  ", &  ! KHpcSI
    "m^3~atm/mol ", &  ! KHpc
    "1           "  /) ! KHcc
  CHARACTER :: lessthan, morethan

  ! global variables:
  REAL                            :: conv_fact ! conversion factor for output
  INTEGER                         :: ndata ! number of data points
  REAL, ALLOCATABLE, DIMENSION(:) :: temp, Harray ! array of size ndata
  REAL                            :: Hominus ! Henry constant at T0
  REAL                            :: mindHR ! - delta H / R
  REAL                            :: r2 ! correlation coefficient
  CHARACTER(STRLEN_VLONG)         :: chem ! name of the chemical species
  CHARACTER(STRLEN_VLONG)         :: casrn ! CAS registry number
  CHARACTER(STRLEN_VLONG)         :: ref ! reference number in bib file
  CHARACTER(STRLEN_VLONG)         :: xref ! cross reference for type X
  CHARACTER                       :: type ! how value was obtained
  CHARACTER(STRLEN_ULONG)         :: seenote = "" ! for \seenote{}
  CHARACTER(STRLEN_HUGE)          :: AllLabels = "%" ! all \seenote{} labels
  CHARACTER(STRLEN_HUGE)          :: AllRefs = "%" ! all references
  INTEGER                         :: n_refs ! number of references
  LOGICAL                         :: warnings = .FALSE.
  ! global namelist variables:
  LOGICAL                         :: l_showpics     = .FALSE. ! show pictures?
  LOGICAL                         :: l_showbiblabel = .FALSE.
  LOGICAL                         :: l_debug        = .FALSE.

  CHARACTER(STRLEN_VLONG)         :: H_symbol
  CHARACTER(STRLEN_VLONG)         :: H_unit_ol ! unit in one line, e.g.: M/atm
  CHARACTER(STRLEN_VLONG)         :: H_unit_fr ! fraction, e.g.: \frac{M}{atm}
  CHARACTER(STRLEN_VLONG)         :: Hprime_unit_ol_TeX

  TYPE HENRY_DATUM
    CHARACTER(STRLEN_VLONG) :: chem    = "" ! name of the chemical species
    CHARACTER(STRLEN_VLONG) :: casrn   = "" ! CAS registry number
    REAL                    :: Hominus = 0. ! Henry constant at T0
    REAL                    :: mindHR  = 0. ! - delta H / R
    CHARACTER(STRLEN_VLONG) :: ref     = "" ! reference number in bib file
    CHARACTER               :: type    = "" ! how H was obtained
    CHARACTER(STRLEN_ULONG) :: seenote = "" ! for \seenote{}
    CHARACTER               :: limit   = "" ! "<", ">" or "i"=infinity
  END TYPE HENRY_DATUM
  INTEGER, PARAMETER :: MAXDATA = 20000
  TYPE(HENRY_DATUM), DIMENSION(MAXDATA) :: henry_data
  INTEGER :: idx_h ! current index in henry_data array

  TYPE HENRY_NOTE
    CHARACTER(STRLEN_VLONG) :: label
    INTEGER :: start
    INTEGER :: END
  END TYPE HENRY_NOTE
  INTEGER, PARAMETER :: MAXNOTES = 1000
  TYPE(HENRY_NOTE), DIMENSION(MAXNOTES) :: henry_notes
  INTEGER :: idx_n ! current index in henry_notes array

  INTEGER, PARAMETER :: MAXNOTECHARS = 100000
  CHARACTER(MAXNOTECHARS) :: allnotes
  INTEGER :: idx_allnotes ! current index in allnotes

  ! dummy for undefined data:
  REAL, PARAMETER :: DUMMY = -999.

  INTEGER, PARAMETER :: MAXSPECIES = 6000
  CHARACTER(STRLEN), DIMENSION(MAXSPECIES) :: allcasrn
  CHARACTER(STRLEN), DIMENSION(MAXSPECIES) :: allinchikey
  INTEGER :: idx_s ! current index in allcasrn array

END MODULE Henry_mem

!*****************************************************************************
!                                  end of file
!*****************************************************************************
