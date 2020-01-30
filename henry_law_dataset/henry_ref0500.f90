!*****************************************************************************
!                   Time-stamp: <2015-04-22 14:05:27 sander>
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

MODULE henry_ref0500

  USE henry_mem
  USE henry_util
  IMPLICIT NONE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ref_thiswork
    IMPLICIT NONE
    REAL :: H_Cl2, H_Br2, K_aq, K_gas

    ref = "thiswork"
    chem = "bromine chloride" ; casrn = "13863-41-7"
    type = "T"
    H_Cl2 = H_from_dG((6.94-0.)*1e3)   * rhoH2O / 1E5 ! ref489 p. 2-47
    H_Br2 = H_from_dG((3.93-3.11)*1e3) * rhoH2O / 1E5 ! ref489 p. 2-50
    K_aq    = 200. ! ref318
    K_gas   = H_from_dG((-2.*0.98-3.11)*1e3)
    Hominus     = SQRT(H_Br2*H_Cl2*K_aq/K_gas)
    ! PRINT *, "H_Cl2 = ", H_Cl2,          " mol/(m3*Pa)"
    ! PRINT *, "H_Br2 = ", H_Br2,          " mol/(m3*Pa)"
    ! PRINT *, "K_aq    = ", K_aq
    ! PRINT *, "K_gas   = ", K_gas
    ! PRINT *, "HcpSI  = ", Hominus,              " mol/(m3*Pa)"
    ! PRINT *, "Hcp    = ", Hominus / Hcp_TO_HcpSI, " M/atm"
    CALL MakeNote("BrClRS", &
      "Calculated using data from \citet{489} and the aqueous-phase "// &
      "equilibrium \chem{Cl_2} + \chem{Br_2} \EQ\  2 \chem{BrCl} from "// &
      "\citet{318}.")
    CALL Output(Hominus)

  END SUBROUTINE ref_thiswork

  !---------------------------------------------------------------------------

  SUBROUTINE ref0019 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "19"

    chem = "methyl nitrate" ; casrn = "598-58-3" ! CH3ONO2
    type = "C"
    CALL MakeNoteOtherTemp("295")
    CALL Output(2.6*Hcp_TO_HcpSI)

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0" ! CH3COOONO2 PAN
    type = "C"
    CALL MakeNoteOtherTemp("295")
    CALL Output(3.6*Hcp_TO_HcpSI)

  END SUBROUTINE ref0019

  !---------------------------------------------------------------------------

  SUBROUTINE ref0031 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "31"

    chem = "carbon dioxide" ; casrn = "124-38-9"
    type = "C"
    CALL Output(3.4E-2*Hcp_TO_HcpSI, 2420.)

    chem = "methanoic acid" ; casrn = "64-18-6"
    type = "C"
    CALL Output(3.7E3*Hcp_TO_HcpSI, 5700.)

    chem = "methylperoxy radical" ; casrn = "2143-58-0"
    type = "E"
    CALL MakeNote("31CH3OO", &
      TRIM(citet())//" assume $\H(\chem{CH_3OO})$ = $\H(\chem{HO_2})$.")
    CALL Output(DUMMY)

    chem = "nitric acid" ; casrn = "7697-37-2"
    type = "R"
    CALL MakeNote("31HNO3", &
      TRIM(citet())//" assume the temperature dependence to be the same as "// &
      "for $a(\chem{H^+})a(\chem{NO_3^-})/p(\chem{HNO_3})$ in \citet{449}.")
    CALL Output(2.1E5*Hcp_TO_HcpSI, 8700.)

    chem = "hydroxyl radical" ; casrn = "3352-57-6"
    type = "C"
    CALL Output(2.5E1*Hcp_TO_HcpSI)

    chem = "hydroxyl radical" ; casrn = "3352-57-6"
    type = "C"
    CALL Output(2.0E2*Hcp_TO_HcpSI)

    chem = "hydroxyl radical" ; casrn = "3352-57-6"
    type = "C"
    CALL Output(9.0E3*Hcp_TO_HcpSI)

    chem = "methanal" ; casrn = "50-00-0" ! HCHO formaldehyde
    type = "W"
    CALL MakeNote("481HCHO")
    CALL Output(DUMMY)

  END SUBROUTINE ref0031

  !---------------------------------------------------------------------------

  SUBROUTINE ref0034 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "34"

    chem = "hydrogen peroxide" ; casrn = "7722-84-1" ! H2O2
    type = "W"
    CALL MakeNote("34H2O2", &
      TRIM(citet())//" cite an incorrect value from \citet{258}, see "// &
      "erratum by \citet{311}.")
    CALL Output(DUMMY)

    chem = "nitric acid" ; casrn = "7697-37-2" ! HNO3
    type = "?"
    CALL MakeNote("34HNO3", &
      TRIM(citet())//" refer to \citet{209} "// &
      "as the source but it is probably from \citet{449}.")
    CALL Output(2.1E5*Hcp_TO_HcpSI)

    chem = "nitrogen dioxide" ; casrn = "10102-44-0" ! NO2
    type = "?"
    CALL MakeNote("34NO2wrongref", &
      TRIM(citet())//" refer to \citet{209} "// &
      "as the source but the quoted value cannot be found there.")
    CALL Output(1.00E-2*Hcp_TO_HcpSI)

    chem = "hydrogen chloride" ; casrn = "7647-01-0" ! HCl
    type = "W"
    CALL MakeNote("34HClwrongref", &
      TRIM(citet())//" refer to \citet{167} "// &
      "as the source but the quoted value cannot be found there.")
    CALL Output(7.27E2*Hcp_TO_HcpSI, 2020.)

    ! HCHO probably results from incorrect conversion of data in ref481,
    !   using a factor of 1013 instead of 760.
    chem = "methanal" ; casrn = "50-00-0" ! HCHO formaldehyde
    type = "W"
    CALL MakeNote("481HCHO")
    CALL Output(DUMMY)

    chem = "carbon dioxide" ; casrn = "124-38-9"
    type = "C"
    CALL Output(3.4E-2*Hcp_TO_HcpSI, 2420.)

    chem = "methanoic acid" ; casrn = "64-18-6"
    type = "C"
    CALL Output(3.5E3*Hcp_TO_HcpSI, 5740.)

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0"
    type = "C"
    CALL Output(2.9*Hcp_TO_HcpSI, 5910.)

    chem = "sulfur dioxide" ; casrn = "7446-09-5"
    type = "C"
    CALL Output(1.23*Hcp_TO_HcpSI, 3120.)

  END SUBROUTINE ref0034

  !---------------------------------------------------------------------------

  SUBROUTINE ref0043 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "43"
    type = "T"
    chem = "hydrogen peroxide" ; casrn = "7722-84-1" ! H2O2
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,5.,10.,15.,20.,25./) + CtoK
    Harray = (/6.07E5,3.82E5,2.45E5,1.65E5,1.05E5,7.1E4/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0043

  !---------------------------------------------------------------------------

  SUBROUTINE ref0046 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "46"

    ! not used:
    ! O3 is from ref140
    ! CO2 is from ref140
    ! SO2 is from ref140

    chem = "hydroxyl radical" ; casrn = "3352-57-6"
    type = "C"
    CALL MakeNote("46OH", &
      TRIM(citet())//" assumed the temperature dependence to be the same "// &
      "as for water.")
    CALL Output(2.5E1*Hcp_TO_HcpSI, 10.5*kcal/Rgas)

    chem = "hydroperoxy radical" ; casrn = "3170-83-0"
    type = "E"
    CALL MakeNote("46HO2", &
      "The value of $\H^{\ominus}$ was taken from \citet{209}.")
    CALL Output(DUMMY, 13.2*kcal/Rgas)

    chem = "methylperoxy radical" ; casrn = "2143-58-0"
    type = "E"
    CALL MakeNote("46CH3OO", &
      TRIM(citet())//" assumes $\H(\chem{CH_3OO}) = "// &
      "\H(\chem{CH_3OOH}) \times \H(\chem{HO_2}) / "// &
      "\H(\chem{H_2O_2})$.")
    CALL Output(6.0*Hcp_TO_HcpSI, 11.2*kcal/Rgas)

    chem = "methanoic acid" ; casrn = "64-18-6"
    type = "T"
    CALL MakeNote("46HCOOH", &
      "Calculated using thermodynamic data from \citet{499}.")
    CALL Output(3.7E3*Hcp_TO_HcpSI, 11.4*kcal/Rgas)

    chem = "dinitrogen pentoxide" ; casrn = "10102-03-1"
    type = "E"
    CALL MakeNote("infty")
    CALL Output(DUMMY, limit="i")

    chem = "nitrogen trioxide" ; casrn = "12033-49-7" ! NO3 nitrate radical
    type = "E"
    CALL MakeNote("46NO3", &
      TRIM(citet())//" assume that \chem{NO_3} has the same Henry's law "// &
      "constant as \chem{HNO_3}.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0046

  !---------------------------------------------------------------------------

  SUBROUTINE ref0059 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "59"

    ! chem = "nitrogen dioxide" ; casrn = "10102-44-0"
    ! CALL SettypeX("3120")
    ! CALL Output(2.4E-2*Hcp_TO_HcpSI)

    chem = "nitrogen dioxide" ; casrn = "10102-44-0"
    type = "M"
    CALL MakeNoteOtherTemp("295")
    CALL Output(7.0E-3*Hcp_TO_HcpSI)

  END SUBROUTINE ref0059

  !---------------------------------------------------------------------------

  SUBROUTINE ref0067 ! KHpx [bar]
    IMPLICIT NONE

    ref = "67"

    chem = "methanal" ; casrn = "50-00-0"
    type = "C"
    CALL MakeNote("HCHOdiol")
    CALL Output(ch2o/(3.85E-3*bar))

  END SUBROUTINE ref0067

  !---------------------------------------------------------------------------

  SUBROUTINE ref0074 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "74"
    type = "T"
    chem = "nitrous acid" ; casrn = "7782-77-6" ! HNO2

    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/25.,20.,15.,10.,5.,0./)
    temp = temp + CtoK
    Harray = (/48.6,61.7,83.2,111.,151.,206./)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0074

  !---------------------------------------------------------------------------

  SUBROUTINE ref0076 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "76"

    chem = "ethanoic acid" ; casrn = "64-19-7"
    type = "T"
    mindHR = 12.8 * kcal / Rgas
    CALL Output(8.8E3*Hcp_TO_HcpSI, mindHR)

    chem = "pernitric acid" ; casrn = "26404-66-0"
    type = "C"
    CALL Output(2.0E4*Hcp_TO_HcpSI, 0.)

  END SUBROUTINE ref0076

  !---------------------------------------------------------------------------

  SUBROUTINE ref0082 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "82"

    chem = "hydrogen chloride" ; casrn = "7647-01-0"
    type = "C"
    CALL Output(2.0E1*Hcp_TO_HcpSI)

  END SUBROUTINE ref0082

  !---------------------------------------------------------------------------

  SUBROUTINE ref0087 ! Hcp [M/atm]
    IMPLICIT NONE

    ! estimates for OH and HNO3 not used

    ref = "87"

    chem = "hydroperoxy radical" ; casrn = "3170-83-0"
    type = "T"
    CALL Output(9.0E3*Hcp_TO_HcpSI)

    chem = "hydrogen peroxide" ; casrn = "7722-84-1"
    type = "T"
    CALL Output(9.7E4*Hcp_TO_HcpSI, 6600.)

    chem = "ozone" ; casrn = "10028-15-6"
    type = "T"
    CALL Output(1.15E-2*Hcp_TO_HcpSI, 2560.)

    chem = "nitric acid" ; casrn = "7697-37-2"
    type = "T"
    CALL Output(2.6E6*Hcp_TO_HcpSI, 8700.)

    chem = "methanal" ; casrn = "50-00-0"
    type = "T"
    CALL MakeNote("HCHOdiol")
    CALL Output(7.0E3*Hcp_TO_HcpSI, 6425.)

    chem = "ammonia" ; casrn = "7664-41-7"
    type = "T"
    CALL Output(5.8E1*Hcp_TO_HcpSI, 4085.)

    chem = "sulfur dioxide" ; casrn = "7446-09-5"
    type = "T"
    CALL Output(1.23*Hcp_TO_HcpSI, 3120.)

    chem = "carbon dioxide" ; casrn = "124-38-9"
    type = "T"
    CALL Output(3.11E-2*Hcp_TO_HcpSI, 2423.)

    chem = "nitrous acid" ; casrn = "7782-77-6"
    type = "T"
    CALL Output(4.9E1*Hcp_TO_HcpSI, 4781.)

    chem = "nitrogen dioxide" ; casrn = "10102-44-0"
    type = "T"
    CALL Output(1.2E-2*Hcp_TO_HcpSI, 2500.)

  END SUBROUTINE ref0087

  !---------------------------------------------------------------------------

  SUBROUTINE ref0088 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "88"
    type = "T"
    chem = "nitrogen trioxide" ; casrn = "12033-49-7" ! NO3
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/288.,298./)
    Harray = (/15.,12./)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0088

  !---------------------------------------------------------------------------

  SUBROUTINE ref0103 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "103"
    type = "?"

    ! same T-dep as ref0523
    CALL CalcH("carbon dioxide",    "124-38-9",   3.390e-2, -4.846) ! CO2
    CALL CalcH("ammonia",           "7664-41-7",  5.844e1,  -8.17)  ! NH3
    CALL CalcH("sulfur dioxide",    "7446-09-5",  1.245,    -6.247) ! SO2
    CALL CalcH("nitric acid",       "7697-37-2",  3.46e5,   -17.46) ! HNO3
    CALL CalcH("hydrogen peroxide", "7722-84-1",  7.1e4,    -14.5)  ! H2O2
    CALL CalcH("ozone",             "10028-15-6", 9.4e-3,   -5.04)  ! O3

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, H_, mindHr_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: H_
      REAL, INTENT(IN)             :: mindHr_
      REAL, PARAMETER :: CONV = kcal/Rgas
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = H_ * Hcp_TO_HcpSI
      CALL MakeNote("whichref")
      CALL Output(Hominus, -mindHR_*CONV)

    END SUBROUTINE CalcH

  END SUBROUTINE ref0103

  !---------------------------------------------------------------------------

  SUBROUTINE ref0140 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "140"
    chem = "sulfuric acid" ; casrn = "7664-93-9" ! H2SO4
    type = "T"
    CALL Output(1.27E15*Hcp_TO_HcpSI, 166.78E3/Rgas)

  END SUBROUTINE ref0140

  !---------------------------------------------------------------------------

  SUBROUTINE ref0160 ! only DUMMY
    IMPLICIT NONE

    ref = "160"

    chem = "methanoic acid" ; casrn = "64-18-6"
    type = "T"
    CALL MakeNote(TRIM(ref), &
      "The value of $\H^{\ominus}$ was taken from \citet{508}.")
    CALL Output(DUMMY, 5736.)

    chem = "ethanoic acid" ; casrn = "64-19-7"
    type = "T"
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY, 6391.)

  END SUBROUTINE ref0160

  !---------------------------------------------------------------------------

  SUBROUTINE ref0167 ! Hcc [1]
    IMPLICIT NONE

    ref = "167"
    chem = "hydrogen chloride" ; casrn = "7647-01-0"
    type = "T"
    Hominus = 10.**(-1.524+878.6/T0) * Hcc_TO_HcpSI_atT0
    mindHR = 878.6 * LOG(10.) + T0 ! see ref958, eqn (34) why T0 is added
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0167

  !---------------------------------------------------------------------------

  SUBROUTINE ref0171 ! Hc2p [M2/atm]
    IMPLICIT NONE

    ref = "171"
    chem = "hydrogen bromide" ; casrn = "10035-10-6"
    type = "?"
    Hominus = 7.19E8 * Hc2p_TO_Hc2pSI ! = Hprime, converted to [mol2 m-6 Pa-1]
    mindHR = 6077
    CALL MakeNote("171HBr", &
      TRIM(citet())//" give a value of $\Hprime$~= $"// &
      TRIM(Hprime_string(Hominus))// &
      "\times\exp\left("//TRIM(mindHR_string(mindHR))//"~\unit{K} "// &
      "\left(\DS\frac{1}{T}-\frac{1}{T^{\ominus}}\right)\right)$ "// &
      TRIM(Hprime_unit_ol_TeX)//". They refer to \citet{46} and "// &
      "\citet{87} as the source but this value cannot be found there.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0171

  !---------------------------------------------------------------------------

  SUBROUTINE ref0183 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "183"
    type = "M"
    chem = "hypochlorous acid" ; casrn = "7790-92-3" ! HOCl
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/215.,263./)
    Harray = (/4.E3,1.E3/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("183HOCl", &
      "This value was extrapolated from data at $T$ = 215~\unit{K} and "// &
      "$T$ = 263~\unit{K}.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0183

  !---------------------------------------------------------------------------

  SUBROUTINE ref0189 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "189"
    type = "T"

    chem = "hydroxyl radical" ; casrn = "3352-57-6"
    CALL Output(32.*Hcp_TO_HcpSI)

    chem = "chlorine atom" ; casrn = "22537-15-1"
    ! exp(-0.9 kcal / Rgas T0) = 0.21 M/atm
    CALL MakeNote("189ClBr", &
      "Calculated from the free energy of solution by \citet{761}.")
    CALL Output(0.2*Hcp_TO_HcpSI)

    chem = "bromine atom" ; casrn = "10097-32-2"
    ! exp( 0.1 kcal / Rgas T0) = 1.18 M/atm
    CALL MakeNote("189ClBr")
    CALL Output(1.2*Hcp_TO_HcpSI)

    chem = "iodine atom" ; casrn = "14362-44-8"
    ! exp(-1.5 kcal / Rgas T0) = 0.079 M/atm
    CALL MakeNote("189I", &
      "Calculated from the free energy of solution by \citet{762}.")
    CALL Output(0.08*Hcp_TO_HcpSI)

  END SUBROUTINE ref0189

  !---------------------------------------------------------------------------

  SUBROUTINE ref0190 ! R ln(Hxp) with Hxp in [1/atm] and R in [cal/(mol*K)]

    IMPLICIT NONE
    REAL    :: A, B, C, D
    INTEGER :: I

    ref = "190"
    ndata = 60
    OPEN (10,FILE="input/ref0190.dat",STATUS="OLD")
    DO I = 1, ndata
      READ (10,*) chem, casrn
      READ (10,*) A
      READ (10,*) B
      READ (10,*) C
      READ (10,*) D
      Hominus = EXP((A+B/T0+C*LOG(T0)+D*T0)*cal/Rgas)*Hxp_TO_HcpSI
      mindHR = (B-C*T0-D*T0*T0)*cal/Rgas ! d(lnH)/d(1/T) = -delta H/R
      IF ((TRIM(casrn)=="74-99-7").OR.(TRIM(casrn)=="10102-43-9")) THEN
        ! propyne and NO are incorrect:
        CALL MakeNote(TRIM(ref), &
          "The fitting parameters $A$, $B$, $C$, and $D$ in Table I of "// &
          TRIM(citet())//" do not reproduce the data in their Table III.")
        type = "W"
        CALL Output(DUMMY)
      ELSE
        type = "L"
        CALL Output(Hominus, mindHR)
      ENDIF
    ENDDO
    CLOSE(10)

    ! propyne:
    ! (/ 273.15, 278.15, 283.15, 288.15, 293.15, 298.15, 303.15, 308.15, 313.15,
    ! 318.15, 323.15, 328.15, 333.15, 338.15, 343.15, 348.15, 353.16, 358.15,
    ! 368.15 /)
    ! (/ 30.22, 23.18, 18.77, 15.85, 13.83, 12.33, 11.17, 10.17, 9.286, 8.436,
    ! 7.580, 6.705, 5.804, 4.917, 4.036, 3.220, 2.487, 1.842, 1.315 /)
    ! (/ 3.770, 2.943, 2.424, 2.082, 1.846, 1.671, 1.537, 1.421, 1.315, 1.212,
    ! 1.103, 0.9885, 0.8665, 0.7429, 0.6171, 0.4979, 0.3889, 0.2910, 0.2100 /)
    ! NO:
    ! (/ 273.15, 278.15, 283.15, 288.15, 293.15, 298.15, 303.15, 308.15, 313.15,
    ! 318.15, 323.15, 328.15, 333.15, 338.15, 343.15, 348.15, 353.15, 358.15 /)
    ! (/ 0.5904, 0.5195, 0.4624, 0.4163, 0.3786, 0.3477, 0.3222, 0.3012, 0.2838,
    ! 0.2695, 0.2577, 0.2481, 0.2404, 0.2343, 0.2297, 0.2263, 0.2242, 0.2231 /)
    ! (/ 0.07345, 0.06581, 0.05963, 0.05459, 0.05046, 0.04708, 0.04430, 0.04202,
    ! 0.04017, 0.03867, 0.03748, 0.03655, 0.03586, 0.03539, 0.03510, 0.03499,
    ! 0.03505, 0.03526 /)

  END SUBROUTINE ref0190

  !---------------------------------------------------------------------------

  SUBROUTINE ref0201
    IMPLICIT NONE

    ref = "201"
    type = "M"
    chem = "sulfuric acid" ; casrn = "7664-93-9"
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" give partial pressures of \chem{H_2SO_4} over "// &
      "concentrated solutions at high temperatures. Extrapolating this to "// &
      "dilute solutions can only be considered an order-of-magnitude "// &
      "approximation for $\H$.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0201

  !---------------------------------------------------------------------------

  SUBROUTINE ref0203 ! special definition
    IMPLICIT NONE

    ref = "203"

    chem = "hypoiodous acid" ; casrn = "14332-21-9"
    type = "E"
    CALL MakeNote("203HOI", &
      TRIM(citet())//" assume that $\Hsymbol(\chem{HOI})$ is between "// &
      TRIM(H_TeX(convert(1E-3)))//" and "// &
      TRIM(H_TeX(convert(1E-6)))//".")
    CALL Output(DUMMY)

    chem = "molecular iodine" ; casrn = "7553-56-2"
    type = "C"
    CALL MakeNote("203I2", &
      TRIM(citet())//" quote a paper as the source that gives only the "// &
      "solubility but not the Henry's law constant.")
    CALL Output(convert(0.04))

CONTAINS

  REAL FUNCTION convert(H)
    REAL, INTENT(IN) :: H
    ! comparing the NO2 and HNO3 data to the cited ref449, it looks like
    ! the conversion from the strange unit "cm^3(liq)/cm^3(air)" is:
    ! HcpSI [mol/m3/Pa] = 1 / ( H * R * 273.15 )
    convert = 1./(H*Rgas*273.15)
  END FUNCTION convert

  END SUBROUTINE ref0203

  !---------------------------------------------------------------------------

  SUBROUTINE ref0205 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "205"

    chem = "hydroperoxy radical" ; casrn = "3170-83-0"
    type = "T" ! see p. 798 of ref205
    CALL Output(9.0E3*Hcp_TO_HcpSI)

  END SUBROUTINE ref0205

  !---------------------------------------------------------------------------

  SUBROUTINE ref0209 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "209"

    chem = "hydroperoxy radical" ; casrn = "3170-83-0"
    type = "T"
    Hominus = 1.2E3*Hcp_TO_HcpSI
    CALL MakeNote("209HO2", &
      "In the abstract, "//TRIM(citet())//" gives a range of "// &
      TRIM(H_range(1E3*Hcp_TO_HcpSI,3E3*Hcp_TO_HcpSI))//". The mean value "// &
      "of this range ("//TRIM(H_TeX(2E3*Hcp_TO_HcpSI))//") "// &
      "has been used by \citet{31}, \citet{34}, and \citet{46}.")
    CALL Output(Hominus)

  END SUBROUTINE ref0209

  !---------------------------------------------------------------------------

  ! ref0258 is not used, see correction ref0311 instead

  !---------------------------------------------------------------------------

  SUBROUTINE ref0260 ! Hb2p [mol^2/(kg^2*atm)]
    IMPLICIT NONE

    ref = "260"
    type = "T"

    chem = "hydrogen fluoride" ; casrn = "7664-39-3" ! HF
    CALL MakeNote(TRIM(ref), &
      "The value is incorrect. See erratum \citep{530}.")
    CALL Output(DUMMY)

    chem = "hydrogen chloride" ; casrn = "7647-01-0" ! HCl
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

    chem = "hydrogen bromide" ; casrn = "10035-10-6" ! HBr
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

    chem = "hydrogen iodide" ; casrn = "10034-85-2" ! HI
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

    chem = "nitric acid" ; casrn = "7697-37-2" ! HNO3
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

    chem = "methanesulfonic acid" ; casrn = "75-75-2" ! CH3SO3H MSA
    Hominus = 6.5E13*Hb2p_TO_Hc2pSI
    CALL MakeNote("260MSA", &
      "$\Hprime$~= "//TRIM(Hprime_string(Hominus))// &
      "~"//TRIM(Hprime_unit_ol_TeX))
    CALL Output(DUMMY)

  END SUBROUTINE ref0260

  !---------------------------------------------------------------------------

  SUBROUTINE ref0271 ! only DUMMY
    IMPLICIT NONE

    ref = "271"
    type = "E"

    chem = "bromine nitrate" ; casrn = "40423-14-1"
    CALL MakeNote("infty")
    CALL Output(DUMMY, limit="i")

    chem = "chlorine nitrate" ; casrn = "14545-72-3"
    CALL MakeNote("infty")
    CALL Output(DUMMY, limit="i")

    chem = "dinitrogen pentoxide" ; casrn = "10102-03-1"
    CALL MakeNote("infty")
    CALL Output(DUMMY, limit="i")

    chem = "sulfur trioxide" ; casrn = "7446-11-9"
    CALL MakeNote("infty")
    CALL Output(DUMMY, limit="i")

  END SUBROUTINE ref0271

  !---------------------------------------------------------------------------

  SUBROUTINE ref0276 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "276"

    chem = "hypobromous acid" ; casrn = "13517-11-8"
    type = "T"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/240.,298./)
    Harray = (/48.,1.8/) * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref), &
      "The value is from Table 1 of the paper. However, {\em J.\ Geophys.\ "// &
      "Res.} forgot to print the tables and I received them directly "// &
      "from the author.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0276

  !---------------------------------------------------------------------------

  SUBROUTINE ref0287 ! KHpx [atm]
    IMPLICIT NONE

    ref = "287"
    type = "M"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/20.,40./)
    temp = temp + CtoK

    chem = "hypochlorous acid" ; casrn = "7790-92-3" ! HOCl
    Harray = (/0.069,0.2/)
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("287HOCl", "Value at pH = 6.5.")
    CALL Output(Hominus, mindHR, r2)

    chem = "chloramide" ; casrn = "10599-90-3" ! NH2Cl
    Harray = (/0.45,1.28/)
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "dichloroamine" ; casrn = "3400-09-7" ! NHCl2
    Harray = (/1.52,3.76/)
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "nitrogen trichloride" ; casrn = "10025-85-1" ! NCl3
    Harray = (/435,1067/)
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "ammonia" ; casrn = "7664-41-7" ! NH3
    Hominus = KHpx_TIMES_HcpSI / 0.71
    CALL Output(Hominus)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0287

  !---------------------------------------------------------------------------

  SUBROUTINE ref0288 ! KHpx [atm]
    IMPLICIT NONE

    ref = "288"
    type = "M"

    chem = "hypochlorous acid" ; casrn = "7790-92-3" ! HOCl
    Hominus = KHpx_TIMES_HcpSI / 0.06
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hominus)

    chem = "hypobromous acid" ; casrn = "13517-11-8" ! HOBr
    Hominus = KHpx_TIMES_HcpSI / 0.0293
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hominus, limit=">")

  END SUBROUTINE ref0288

  !---------------------------------------------------------------------------

  SUBROUTINE ref0311 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "311"
    type = "M"

    chem = "hydrogen peroxide" ; casrn = "7722-84-1"
    CALL MakeNote(TRIM(ref), &
      "This value is a correction of the solubility published by "// &
      "\citet{258}.")
    mindHR = 6338.
    Hominus = EXP(mindHR/T0- 9.74) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

    chem = "methyl hydroperoxide" ; casrn = "3031-73-0"
    CALL MakeNote(TRIM(ref))
    mindHR = 5322.
    Hominus = EXP(mindHR/T0-12.14) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

    chem = "ethanoic peroxyacid" ; casrn = "79-21-0"
    CALL MakeNote(TRIM(ref))
    mindHR = 5896.
    Hominus = EXP(mindHR/T0-13.28) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0311

  !---------------------------------------------------------------------------

  SUBROUTINE ref0315 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "315"
    chem = "hypochlorous acid" ; casrn = "7790-92-3"
    type = "L"
    CALL Output(EXP(6.4946)*Hbp_TO_HcpSI, 5862.)

  END SUBROUTINE ref0315

  !---------------------------------------------------------------------------

  SUBROUTINE ref0379 ! H expressed as alpha, l, and A
    IMPLICIT NONE

    ref = "379"
    type = "?"

    chem = "ethyne" ; casrn = "74-86-2" ! C2H2 acetylene
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ 1.73, 1.68, 1.63, 1.58, 1.53, 1.49, 1.45, 1.41, 1.37, 1.34, 1.31, &
      1.27, 1.24, 1.21, 1.18, 1.15, 1.13, 1.10, 1.08, 1.05, 1.03, 1.01, 0.99, &
      0.97, 0.95, 0.93, 0.91, 0.89, 0.87, 0.85, 0.84/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref), &
      "Only the tabulated data between $T$~= 273~\unit{K} and $T$~= "// &
      "303~\unit{K} from "//TRIM(citet())//" were used to derive $\H$ and "// &
      "its temperature dependence. Above $T$~= 303~\unit{K}, the tabulated "// &
      "data could not be parameterized by Eq.~(\ref{eq:Tdep}) very "// &
      "well. The partial pressure of water vapor (needed to convert some "// &
      "Henry's law constants) was calculated using the formula given by "// &
      "\citet{255}. The quantities $A$ and $\alpha$ from "// &
      TRIM(citet())//" were assumed to be identical.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "ammonia" ; casrn = "7664-41-7" ! NH3
    ndata = 12
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,4.,8.,10.,12.,13.,15.,16.,20.,24.,28.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/1130.,1047.,947.,870.,857.,837.,770.,775.,680.,639.,586.,530./)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "molecular bromine" ; casrn = "7726-95-6" ! Br2
    ndata = 16
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26.,28.,30./)
    temp = temp + CtoK
    Harray = (/60.5,54.1,48.3,43.3,38.9,35.1,31.5,28.4,25.7,23.4,21.3,19.4,17.7,&
      & 16.3,15.0,13.8/)
    !q=(42.9,38.3,34.2,30.6,27.5,24.8,22.2,20.0,18,16.4,14.9,13.5, &
    !12.3,11.3,10.3,9.5)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "carbon dioxide" ; casrn = "124-38-9" ! CO2
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    Harray = (/1.713,1.646,1.584,1.527,1.473,1.424,1.377,1.331,1.282,1.237,1.194&
      &,1.154,1.117,1.083,1.050,1.019,.985,.956,.928,.902,.878,.854,.829&
      &,.804,.781,.759,.738,.718,.699,.682,.665/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "carbon monoxide" ; casrn = "630-08-0" ! CO
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    Harray = (/0.03537,0.03455,0.03375,0.03297,0.03222,0.03149,0.03078,0.03009&
      &,0.02942,0.02878,0.02816,0.02757,0.02701,0.02646,0.02593,0.02543&
      &,0.02494,0.02448,0.02402,0.02360,0.02319,0.02281,0.02244,0.02208&
      &,0.02174,0.02142,0.02110,0.02080,0.02051,0.02024,0.01998/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "molecular chlorine" ; casrn = "7782-50-5" ! Cl2
    ndata = 21
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/10.,11.,12.,13.,14.,15.,16.,17.,18.,19.,20.,21.,22.,23.,24.,25.&
      &,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    ! using l between 10 and 30 C:
    Harray = (/3.148,3.047,2.95,2.856,2.767,2.68,2.597,2.517,2.44,2.368,2.299&
      &,2.238,2.18,2.123,2.07,2.019,1.97,1.923,1.88,1.839,1.799/)
    Harray = LtoH(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "ethane" ; casrn = "74-84-0" ! C2H6
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ &
      0.09874, 0.09476, 0.09093, 0.08725, 0.08372, 0.08033, 0.07709, 0.07400, &
      0.07106, 0.06826, 0.06561, 0.06328, 0.06106, 0.05894, 0.05694, 0.05504, &
      0.05326, 0.05159, 0.05003, 0.04858, 0.04724, 0.04589, 0.04459, 0.04335, &
      0.04217, 0.04104, 0.03997, 0.03895, 0.03799, 0.03709, 0.03624 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "ethene" ; casrn = "74-85-1" ! C2H4 ethylene
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ &
      0.226, 0.219, 0.211, 0.204, 0.197, 0.191, 0.184, 0.178, 0.173, 0.167, &
      0.162, 0.157, 0.152, 0.148, 0.143, 0.139, 0.136, 0.132, 0.129, 0.125, &
      0.122, 0.119, 0.116, 0.114, 0.111, 0.108, 0.106, 0.104, 0.102, 0.100, &
      0.098 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "hydrogen" ; casrn = "1333-74-0" ! H2
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    Harray = (/0.02148,0.02126,0.02105,0.02084,0.02064,0.02044,0.02025,0.02007&
      &,0.01989,0.01972,0.01955,0.01940,0.01925,0.01911,0.01897,0.01883&
      &,0.01869,0.01856,0.01844,0.01831,0.01819,0.01805,0.01792,0.01779&
      &,0.01766,0.01754,0.01742,0.01731,0.01720,0.01709,0.01699/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "hydrogen sulfide" ; casrn = "7783-06-4" ! H2S
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    Harray = (/4.67,4.522,4.379,4.241,4.107,3.977,3.852,3.732,3.616,3.505,3.399&
      &,3.300,3.206,3.115,3.028,2.945,2.865,2.789,2.717,2.647,2.582,2.517&
      &,2.456,2.396,2.338,2.282,2.229,2.177,2.128,2.081,2.037/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "methane" ; casrn = "74-82-8" ! CH4
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    Harray = (/0.05563,0.05401,0.05244,0.05093,0.04946,0.04805,0.04669,0.04539&
      &,0.04413,0.04292,0.04177,0.04072,0.03970,0.03872,0.03779,0.03690&
      &,0.03606,0.03525,0.03448,0.03376,0.03308,0.03243,0.03180,0.03119&
      &,0.03061,0.03006,0.02952,0.02901,0.02852,0.02806,0.02762/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "nitrogen monoxide" ; casrn = "10102-43-9" ! NO
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    Harray = (/0.07381,0.07184,0.06993,0.06809,0.06632,0.06461,0.06298,0.06140&
      &,0.05990,0.05846,0.05709,0.05587,0.05470,0.05357,0.05250,0.05147&
      &,0.05049,0.04956,0.04868,0.04785,0.04706,0.04625,0.04545,0.04469&
      &,0.04395,0.04323,0.04254,0.04188,0.04124,0.04063,0.04004/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "nitrogen" ; casrn = "7727-37-9" ! N2
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ &
      0.02354, 0.02297, 0.02241, 0.02187, 0.02135, 0.02086, 0.02037, 0.01990, &
      0.01945, 0.01902, 0.01861, 0.01823, 0.01786, 0.01750, 0.01717, 0.01685, &
      0.01654, 0.01625, 0.01597, 0.01570, 0.01545, 0.01522, 0.01498, 0.01475, &
      0.01454, 0.01434, 0.01413, 0.01394, 0.01376, 0.01358, 0.01342 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "oxygen" ; casrn = "7782-44-7" ! O2
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    Harray = (/0.04889,0.04758,0.04633,0.04512,0.04397,0.04287,0.04180,0.04080&
      &,0.03983,0.03891,0.03802,0.03718,0.03637,0.03559,0.03486,0.03415&
      &,0.03348,0.03283,0.03220,0.03161,0.03102,0.03044,0.02988,0.02934&
      &,0.02881,0.02831,0.02783,0.02736,0.02691,0.02649,0.02608/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "sulfur dioxide" ; casrn = "7446-09-5" ! SO2
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,17.,18.&
      &,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./)
    temp = temp + CtoK
    ! using l between 0 and 30 C:
    Harray = (/79.789,77.210,74.691,72.230,69.828,67.485,65.2,62.973,60.805,58.697&
      &,56.647,54.655,52.723,50.849,49.033,47.276,45.578,43.939,42.36&
      &,40.838,39.374,37.97,36.617,35.302,34.026,32.786,31.584,30.422&
      &,29.314,28.210,27.161/)
    Harray = LtoH(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "argon" ; casrn = "7440-37-1" ! Ar
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,20.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ 0.0528, 0.0413, 0.0337, 0.0288 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "helium" ; casrn = "7440-59-7" ! He
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,20.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ 0.0098, 0.00911, 0.0086, 0.00839 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "hydrogen bromide" ; casrn = "10035-10-6" ! HBr
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,25./)
    temp = temp + CtoK
    Harray = (/612.,582.,533./)
    Harray = LtoH(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "hydrogen chloride" ; casrn = "7647-01-0" ! HCl
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,20.,30./)
    temp = temp + CtoK
    Harray = (/512.,475.,442.,412./)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "krypton" ; casrn = "7439-90-9" ! Kr
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,20.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ 0.1105, 0.0810, 0.0626, 0.0511 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "neon" ; casrn = "7440-01-9" ! Ne
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/9.,20.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ 0.0117, 0.0106, 0.0100 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "dinitrogen monoxide" ; casrn = "10024-97-2" ! N2O
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/10.,20./)
    temp = temp + CtoK
    ! A value treated exactly like alpha
    Harray = (/0.88,0.63/)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "radon" ; casrn = "10043-92-2" ! Rn
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,20.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ 0.510, 0.326, 0.222, 0.162 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "xenon" ; casrn = "7440-63-3" ! Xe
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,20.,30./)
    temp = temp + CtoK
    ! using alpha between 0 and 30 C:
    Harray = (/ 0.242, 0.174, 0.123, 0.098 /)
    Harray = Harray * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0379

  !---------------------------------------------------------------------------

  SUBROUTINE ref0389 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "389"

    chem = "dimethylsulfone" ; casrn = "67-71-0"
    type = "E"
    CALL Output(5E4*Hcp_TO_HcpSI, limit=">")

    ! for DMSO, see ref0824

  END SUBROUTINE ref0389

  !---------------------------------------------------------------------------

  ! ref0399 HBr not used, it only refers to wagman tables

  !---------------------------------------------------------------------------

  SUBROUTINE ref0443 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "443"
    chem = "methanal" ; casrn = "50-00-0" ! HCHO formaldehyde
    type = "C"
    CALL MakeNote("481HCHO")
    CALL Output(DUMMY)

  END SUBROUTINE ref0443

  !---------------------------------------------------------------------------

  SUBROUTINE ref0446 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "446"
    chem = "hypobromous acid" ; casrn = "13517-11-8"
    type = "E"
    CALL Output(92.6*Hcp_TO_HcpSI)

  END SUBROUTINE ref0446

  !---------------------------------------------------------------------------

  SUBROUTINE ref0449 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "449"

    chem = "nitric acid" ; casrn = "7697-37-2"
    type = "T"
    CALL Output(2.1E5*Hcp_TO_HcpSI)

    chem = "nitrous acid" ; casrn = "7782-77-6"
    type = "L"
    CALL Output(49.*Hcp_TO_HcpSI, 9.5*kcal/Rgas)

    chem = "nitrogen monoxide" ; casrn = "10102-43-9"
    type = "L"
    CALL Output(1.93E-3*Hcp_TO_HcpSI, 2.94*kcal/Rgas)

    chem = "nitrogen dioxide" ; casrn = "10102-44-0"
    type = "L"
    CALL Output(1.2E-2*Hcp_TO_HcpSI)

    chem = "dinitrogen tetroxide" ; casrn = "10544-72-6"
    type = "L"
    CALL Output(1.4*Hcp_TO_HcpSI)

    chem = "dinitrogen trioxide" ; casrn = "10544-73-7"
    type = "L"
    CALL Output(6.0E-1*Hcp_TO_HcpSI)

  END SUBROUTINE ref0449

  !---------------------------------------------------------------------------

  SUBROUTINE ref0477 ! Hxp [1/atm] and KHpc [bar*m3/mol]
    IMPLICIT NONE

    ! data from p. 6-3 ff.
    ref = "477"
    type = "X"

    chem = "hydrogen" ; casrn = "1333-74-0" ! H2
    CALL SettypeX("young81a")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/1.51E-5,1.455E-5,1.411E-5,1.377E-5,1.35E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "D2" ; casrn = "7782-39-0" ! D2
    CALL SettypeX("young81a")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/283.15,288.15,293.15,298.15,303.15/)
    Harray = (/1.675E-5,1.595E-5,1.512E-5,1.46E-5,1.395E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "helium" ; casrn = "7440-59-7" ! He
    CALL SettypeX("clever79a")
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15/)
    Harray = (/7.123E-6,7.044E-6,6.997E-6,6.978E-6/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("477He", &
      "The value at $T$~= 308.15~\unit{K} doesn't fit and is not used "// &
      "for the linear regression.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "neon" ; casrn = "7440-01-9" ! Ne
    CALL SettypeX("clever79a")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/8.702E-6,8.395E-6,8.152E-6,7.966E-6,7.829E-6/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "argon" ; casrn = "7440-37-1" ! Ar
    CALL SettypeX("clever80")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/3.025E-5,2.748E-5,2.519E-5,2.328E-5,2.169E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "krypton" ; casrn = "7439-90-9" ! Kr
    CALL SettypeX("clever79b")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/5.696E-5,5.041E-5,4.512E-5,4.079E-5,3.725E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "xenon" ; casrn = "7440-63-3" ! Xe
    CALL SettypeX("clever79b")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/10.519E-5,9.051E-5,7.89E-5,6.961E-5,6.212E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "radon" ; casrn = "10043-92-2" ! Rn
    type = "?"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/2.299E-4,1.945E-4,1.671E-4,1.457E-4,1.288E-4/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("477Rn", &
      "Though no reference was given, the value is probably "// &
      "from \citet{clever79b}.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "oxygen" ; casrn = "7782-44-7" ! O2
    CALL SettypeX("battino81")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/2.756E-5,2.501E-5,2.293E-5,2.122E-5,1.982E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "ozone" ; casrn = "10028-15-6" ! O3
    CALL SettypeX("battino81")
    Hominus = 1.885E-6 * Hxp_TO_HcpSI
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hominus)

    chem = "nitrogen" ; casrn = "7727-37-9" ! N2
    CALL SettypeX("battino82")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/1.386E-5,1.274E-5,1.183E-5,1.108E-5,1.047E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "dinitrogen monoxide" ; casrn = "10024-97-2" ! N2O
    CALL SettypeX("young81b")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/5.948E-4,5.068E-4,4.367E-4,3.805E-4,3.348E-4/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("477ABC", &
      "The parametrization given by "//TRIM(citet())//" with parameters "// &
      "$A$, $B$, and $C$ doesn't fit the data in the same paper for this "// &
      "substance. Therefore the parametrization of the solubility "// &
      "data was recalculated.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "nitrogen monoxide" ; casrn = "10102-43-9" ! NO
    CALL SettypeX("young81b")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/4.163E-5,3.786E-5,3.477E-5,3.222E-5,3.012E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "carbon monoxide" ; casrn = "630-08-0" ! CO
    CALL SettypeX("cargill90")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/2.095E-5,1.918E-5,1.774E-5,1.657E-5,1.562E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "selenium hydride" ; casrn = "7783-07-5" ! H2Se
    CALL SettypeX("fogg88")
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,298.15,308.15/)
    Harray = (/1.8E-3,1.49E-3,1.24E-3/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "hydrogen sulfide" ; casrn = "7783-06-4" ! H2S
    CALL SettypeX("fogg88")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/2.335E-3,2.075E-3,1.85E-3,1.66E-3,1.51E-3/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "sulfur dioxide" ; casrn = "7446-09-5" ! SO2
    CALL SettypeX("young83")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/3.45E-2,2.9E-2,2.46E-2,2.1E-2,1.8E-2/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "molecular chlorine" ; casrn = "7782-50-5" ! Cl2
    CALL SettypeX("young83")
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/283.15,293.15,303.15,313.15/)
    Harray = (/2.48E-3,1.88E-3,1.5E-3,1.23E-3/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "dichlorine monoxide" ; casrn = "7791-21-1" ! Cl2O
    CALL SettypeX("young83")
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/273.15,276.61,283.15,293.15/)
    Harray = (/5.25E-1,4.54E-1,4.273E-1,3.353E-1/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "chlorine dioxide" ; casrn = "10049-04-4" ! ClO2
    CALL SettypeX("young83")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/2.67E-2,2.2E-2,1.823E-2,1.513E-2,1.259E-2/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("477ABC")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "methane" ; casrn = "74-82-8" ! CH4
    CALL SettypeX("clever87")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/3.122E-5,2.806E-5,2.552E-5,2.346E-5,2.18E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("477ABC")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "ethane" ; casrn = "74-84-0" ! C2H6
    CALL SettypeX("hayduk82")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/4.556E-5,3.907E-5,3.401E-5,3.002E-5,2.686E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "propane" ; casrn = "74-98-6" ! C3H8
    CALL SettypeX("hayduk86")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/3.813E-5,3.2E-5,2.732E-5,2.37E-5,2.088E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "butane" ; casrn = "106-97-8" ! C4H10
    CALL SettypeX("hayduk86")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/3.274E-5,2.687E-5,2.244E-5,1.906E-5,1.645E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "2-methylpropane" ; casrn = "75-28-5" ! HC(CH3)3
    CALL SettypeX("hayduk86")
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp=(/288.15,293.15,298.15,303.15,308.15/)
    Harray = (/2.333E-5,1.947E-5,1.659E-5,1.443E-5,1.278E-5/)
    Harray = Harray * Hxp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    ! data from p. 16-24 ff.
    type = "V"

    CALL CalcH("methanal",                "50-00-0",  3.3E-7)
    CALL CalcH("bromomethane",            "74-83-9",  6.3E-3)
    CALL CalcH("chloromethane",           "74-87-3",  2.4E-2)
    CALL CalcH("hexachloroethane",        "67-72-1",  2.8E-3)
    CALL CalcH("1,1-dichloroethene",      "75-35-4",  3.0E-2)
    CALL CalcH("chloroethene",            "75-01-4",  1.1E-2)  ! vinyl chloride
    CALL CalcH("ethylene oxide",          "75-21-8",  1.2E-4)  ! C2H4O
    CALL CalcH("2-propenenitrile",        "107-13-1", 1.1E-4)  ! acrylonitrile
    CALL CalcH("propenal",                "107-02-8", 4.4E-6)  ! acrolein
    CALL CalcH("propenoic acid",          "79-10-7",  3.2E-7)  ! acrylic acid
    CALL CalcH("acrylamide",              "79-06-1",  3.2E-10)
    CALL CalcH("2-propen-1-ol",           "107-18-6", 4.9E-6)  ! allyl alcohol
    CALL CalcH("1,2-epoxypropane",        "75-56-9",  8.5E-5)  ! propyleneoxide
    CALL CalcH("{cis}-butenedioic acid",  "110-16-7", 7.0E-14) ! HOOC(CH)2COOH
    CALL CalcH("1,3-butadiene",           "106-99-0", 2.57)
    CALL CalcH("vinyl acetate",           "108-05-4", 4.9E-4)
    CALL CalcH("bis(2-chloroethyl)ether", "111-44-4", 2.9E-4)
    CALL CalcH("methyl methacrylate",     "80-62-6",  3.3E-4)
    CALL CalcH("hexachlorobenzene",       "118-74-1", 1.3E-3)
    CALL CalcH("1,2,4-trichlorobenzene",  "120-82-1", 1.4E-3)
    CALL CalcH("1,3,5-trichlorobenzene",  "108-70-3", 1.0E-3)
    CALL CalcH("2,4,6-trichlorophenol",   "88-06-2",  6.2E-8)
    CALL CalcH("o-chloronitrobenzene",    "88-73-3",  3.6E-5)
    CALL CalcH("p-chloronitrobenzene",    "100-00-5", 3.6E-5)
    CALL CalcH("o-dichlorobenzene",       "95-50-1",  1.2E-3)
    CALL CalcH("m-dichlorobenzene",       "541-73-1", 1.8E-3)
    CALL CalcH("p-dichlorobenzene",       "106-46-7", 1.5E-3)
    CALL CalcH("chlorobenzene",           "108-90-7", 3.5E-3)
    CALL CalcH("o-chlorophenol",          "95-57-8",  5.7E-7)
    CALL CalcH("m-chlorophenol",          "108-43-0", 5.7E-7)
    CALL CalcH("p-chlorophenol",          "106-48-9", 5.7E-7)
    CALL CalcH("nitrobenzene",            "98-95-3",  2.4E-5)
    CALL CalcH("o-nitrophenol",           "88-75-5",  3.5E-6)
    CALL CalcH("m-nitrophenol",           "554-84-7", 1.0E-5)
    CALL CalcH("p-nitrophenol",           "100-02-7", 3.3E-8)
    CALL CalcH("o-chloroaniline",         "95-51-2",  7.5E-6)
    CALL CalcH("p-chloroaniline",         "106-47-8", 1.1E-5)
    CALL CalcH("phenol",                  "108-95-2", 4.0E-7)
    CALL CalcH("aniline",                 "62-53-3",  1.4E-1)
    CALL CalcH("adipic acid",             "124-04-9", 9.5E-7)
    CALL CalcH("benzoic acid",            "65-85-0",  7.1E-8)
    CALL CalcH("chloromethylbenzene",     "100-44-7", 3.5E-4)
    CALL CalcH("o-nitrotoluene",          "88-72-2",  5.6E-5)
    CALL CalcH("p-nitrotoluene",          "99-99-0",  5.0E-5)
    CALL CalcH("o-cresol",                "95-48-7",  1.6E-6)
    CALL CalcH("m-cresol",                "108-39-4", 8.8E-7)
    CALL CalcH("p-cresol",                "106-44-5", 9.7E-7)
    CALL CalcH("phthalic anhydride",      "85-44-9",  6.2E-9)
    CALL CalcH("styrene",                 "100-42-5", 2.8E-3)
    CALL CalcH("ethylbenzene",            "100-41-4", 8.5E-3)
    CALL CalcH("2,4-xylenol",             "105-67-9", 6.4E-7)
    CALL CalcH("naphthalene",             "91-20-3",  4.9E-4)
    CALL CalcH("dimethyl phthalate",      "131-11-3", 1.1E-7)
    CALL CalcH("benzidine",               "92-87-5",  3.9E-11)
    CALL CalcH("diethyl phthalate",       "84-66-2",  4.8E-7)
    CALL CalcH("dibutyl phthalate",       "84-74-2",  4.6E-7)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: KHpc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (KHpc*bar)
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0477

  !---------------------------------------------------------------------------

  SUBROUTINE ref0479 ! special definition
    IMPLICIT NONE
    INTEGER :: i

    ref = "479"

    ! some data from this reference were not used:
    ! tetradecane        no recommended value; H looks too small
    ! hexadecane         no recommended value; H looks too small
    ! octadecane         no recommended value; H looks too small
    ! eicosane           no recommended value; H looks too small
    ! hexacosane         no recommended value; H looks too small
    ! 1,2-benzanthracene no recommended value; H looks strange
    ! 3,4-benzopyrene    no recommended value; H looks strange
    ! vinylchloride      no recommended value
    ! trichloropropane   no recommended value; which isomer is this?

    ndata = 165
    OPEN (10,FILE="input/ref0479.dat",STATUS="OLD")
    DO i = 1, ndata
      READ (10,*) Hominus, chem, casrn, type
      Hominus =  1./(1000.*Hominus)
      IF (TRIM(casrn)=="74-83-9") CALL MakeNoteOtherTemp("293")
      IF ((TRIM(casrn)=="108-36-1").OR.(TRIM(casrn)=="106-37-6")) &
        CALL MakeNoteOtherTemp("308")
      IF (TRIM(type)=="W") THEN
        CALL MakeNote("479wrong", &
          "According to \citet{1337}, the value is incorrect.")
        CALL Output(DUMMY)
      ELSE
        CALL Output(Hominus)
      ENDIF
    ENDDO
    CLOSE(10)

  END SUBROUTINE ref0479

  !---------------------------------------------------------------------------

  SUBROUTINE ref0480 ! KHpc [atm/M]
    IMPLICIT NONE

    ref = "480"

    chem = "ozone" ; casrn = "10028-15-6"
    type = "M"
    mindHR = 2297.
    Hominus = Hcp_TO_HcpSI / EXP(12.19 - mindHR/T0)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0480

  !---------------------------------------------------------------------------

  SUBROUTINE ref0481 ! special definition
    IMPLICIT NONE

    ref = "481"
    type = "M"
    chem = "methanal" ; casrn = "50-00-0" ! HCHO
    CALL MakeNote("481HCHO")
    CALL Output(DUMMY)

  END SUBROUTINE ref0481

  !---------------------------------------------------------------------------

  SUBROUTINE ref0482 ! Hcc [1]
    IMPLICIT NONE

    ref = "482"
    chem = "ammonia" ; casrn = "7664-41-7"
    type = "M"
    Hominus = 10.**(-1.69+1477.7/T0) * Hcc_TO_HcpSI_atT0
    mindHR = 1477.7 * LOG(10.) + T0 ! see ref958, eqn (34) why T0 is added
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0482

  !---------------------------------------------------------------------------

  SUBROUTINE ref0483 ! Hcc [1]
    IMPLICIT NONE

    ref = "483"
    type = "M"

    ! alcohols:
    CALL CalcH("methanol",            "67-56-1",  25., 5.5)
    CALL CalcH("ethanol",             "64-17-5",  33., 4.7)
    CALL CalcH("1-propanol",          "71-23-8",  30., 3.3)
    CALL CalcH("2-propanol",          "67-63-0",  28., 3.1)
    CALL CalcH("1-butanol",           "71-36-3",  26., 3.1)
    CALL CalcH("2-butanol",           "78-92-2",  23., 2.7) ! sec-BuOH
    CALL CalcH("2-methyl-2-propanol", "75-65-0",  20., 1.7) ! tert-BuOH
    chem = "2-methyl-1-propanol" ; casrn = "78-83-1"         ! iso-BuOH
    CALL Output(1000.*Hcc_TO_HcpSI(2.5, 298.))
    ! ketones:
    CALL CalcH("propanone",           "67-64-1",  2.5, 0.63)
    CALL CalcH("2-butanone",          "78-93-3",  2.3, 0.43)
    chem = "2,3-butanedione" ; casrn = "431-03-8"
    CALL Output(1000.*Hcc_TO_HcpSI(1.4, 298.))
    ! aldehydes:
    CALL CalcH("ethanal",             "75-07-0",  1.7, 0.31)
    CALL CalcH("propenal",            "107-02-8", 0.8, 0.18)
    ! nitriles:
    CALL CalcH("ethane nitrile",      "75-05-8",  3.7, 1.2)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcc273, Hcc298)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: Hcc273, Hcc298
      REAL :: H273, H298

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      H273 = 1000. * Hcc_TO_HcpSI(Hcc273, 273.)
      H298 = 1000. * Hcc_TO_HcpSI(Hcc298, 298.)
      ndata = 2
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = (/273.,  298./)
      Harray   = (/H273, H298/)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)

    END SUBROUTINE CalcH

  END SUBROUTINE ref0483

  !---------------------------------------------------------------------------

  SUBROUTINE ref0484 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "484"
    type = "M"

    chem = "trichloroethanal" ; casrn = "75-87-6" ! CCl3CHO
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/15.,25.,35.,45./)
    temp = temp + CtoK
    Harray = (/4.99E5,3.44E5,2.45E5,1.56E5/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("RCHOdiol")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "ethanedial" ; casrn = "107-22-2" ! OHCCHO
    Hominus = 3.E5 * Hcp_TO_HcpSI
    CALL MakeNote("RCHOdiol")
    CALL Output(Hominus, limit=">")

    chem = "propanonal" ; casrn = "78-98-8" ! CH3COCHO
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/15.,25.,35.,45./)
    temp = temp + CtoK
    Harray = (/8.47E3,3.71E3,1.29E3,7.79E2/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("RCHOdiol")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "methanal" ; casrn = "50-00-0" ! HCHO
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/15.,25.,35.,45./)
    temp = temp + CtoK
    Harray = (/7.31E3,2.97E3,1.5E3,6.73E2/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("HCHOdiol")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "benzaldehyde" ; casrn = "100-52-7" ! C6H5CHO
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/15.,25.,35.,45./)
    temp = temp + CtoK
    Harray = (/67.3,37.4,21.8,12.8/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("RCHOdiol")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "2-hydroxyethanal" ; casrn = "141-46-8" ! HOCH2CHO
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/25.,45./)
    temp = temp + CtoK
    Harray = (/4.14E4,1.56E4/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("RCHOdiol")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "ethanal" ; casrn = "75-07-0" ! CH3CHO
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/5.,10.,25.,35./)
    temp = temp + CtoK
    Harray = (/55.4,39.3,11.4,6.56/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("RCHOdiol")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0484

  !---------------------------------------------------------------------------

  SUBROUTINE ref0485 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "485"
    type = "M"
    chem = "methanal" ; casrn = "50-00-0" ! HCHO
    CALL MakeNote("485HCHO", &
      TRIM(citet())//" found that the Henry's law constant for \chem{HCHO} "// &
      "is not a true constant but increases with increasing "// &
      "concentration. They recommend the expression "// &
      "$$ [\chem{HCHO}] =10^{(4538/T-11.34)} \times "// &
      "p(\chem{HCHO})^{(252.2/T+0.2088)}$$ "// &
      "with [\chem{HCHO}]~= aqueous-phase concentration in [M], "// &
      "p(\chem{HCHO})~= partial pressure in [atm], and $T$~= temperature "// &
      "in [K]. Note that this expression "// &
      "does not converge asymptotically to a constant value at infinite "// &
      "dilution.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0485

  !---------------------------------------------------------------------------

  SUBROUTINE ref0486 ! special definition
    IMPLICIT NONE

    ref = "486"
    type = "M"
    chem = "methanal" ; casrn = "50-00-0" ! HCHO
    CALL MakeNote("481HCHO")
    CALL Output(DUMMY)

  END SUBROUTINE ref0486

  !---------------------------------------------------------------------------

  SUBROUTINE ref0487 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "487"

    chem = "ammonia" ; casrn = "7664-41-7" ! NH3
    type = "M"
    mindHR = 4092.
    Hominus=EXP(mindHR/T0-9.7) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

    chem = "ammonia" ; casrn = "7664-41-7" ! NH3
    type = "T"
    mindHR = 4166.
    Hominus=EXP(mindHR/T0-9.94) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0487

  !---------------------------------------------------------------------------

  SUBROUTINE ref0488 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "488"
    type = "M"
    chem = "hydrogen peroxide" ; casrn = "7722-84-1" ! H2O2
    mindHR = 7.92E3
    Hominus=EXP(mindHR/T0-15.44) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0488

  !---------------------------------------------------------------------------

  SUBROUTINE ref0489 ! Hbp [mol/(kg*1E5Pa)]
    IMPLICIT NONE

    ref = "489"
    type = "T"

    ! H      = exp((dGao-dGg)/(Rgas*T0))
    ! mindHR = -(Delta_H(ao)-Delta_H(g)) / Rgas
    ! factor "rhoH2O/1E5" converts Hbp to HcpSI in [mol/(m3*Pa)]

    chem   = "molecular chlorine" ; casrn = "7782-50-5"           ! Cl2
    Hominus    = H_from_dG((6.94-0.)*1e3) * rhoH2O / 1E5            ! p. 2-47
    mindHR = -(-23.4-0.)*1e3/Rgas
    CALL Output(Hominus, mindHR)

    chem   = "hydrogen chloride" ; casrn = "7647-01-0"            ! HCl
    Hominus    = H_from_dG((-131.228+95.299)*1e3) * rhoH2O**2/1E5   ! p. 2-47
    mindHR = -(-167.159+92.307)*1e3/Rgas
    CALL MakeNote("489HCl", "$\Hprime$~= $"//TRIM(Hprime_string(Hominus))// &
      "\times\exp\left("//TRIM(mindHR_string(mindHR))//"~\unit{K} "// &
      "\left(\DS\frac{1}{T}-\frac{1}{T^{\ominus}}\right)\right)$~"//TRIM(Hprime_unit_ol_TeX))
    CALL Output(DUMMY)

    chem   = "hypochlorous acid" ; casrn = "7790-92-3"            ! HOCl
    Hominus    = H_from_dG((-79.9+66.1)*1e3) * rhoH2O / 1E5         ! p. 2-48
    mindHR = -(-120.9+78.7)*1e3/Rgas
    CALL Output(Hominus, mindHR)

    chem   = "molecular bromine" ; casrn = "7726-95-6"            ! Br2
    Hominus    = H_from_dG((3.93-3.11)*1e3) * rhoH2O / 1E5          ! p. 2-50
    mindHR = -(-2.59-30.907)*1e3/Rgas
    CALL Output(Hominus, mindHR)

    chem   = "hydrogen bromide" ; casrn = "10035-10-6"            ! HBr
    Hominus    = H_from_dG((-103.96+53.45)*1e3) * rhoH2O**2/1E5     ! p. 2-50
    mindHR = -(-121.55+36.4)*1e3/Rgas
    CALL MakeNote("489HBr", "$\Hprime$~= $"//TRIM(Hprime_string(Hominus))// &
      "\times\exp\left("//TRIM(mindHR_string(mindHR))//"~\unit{K} "// &
      "\left(\DS\frac{1}{T}-\frac{1}{T^{\ominus}}\right)\right)$~"//TRIM(Hprime_unit_ol_TeX))
    CALL Output(DUMMY)

    chem   = "molecular iodine" ; casrn = "7553-56-2"             ! I2
    Hominus    = H_from_dG((16.4-19.327)*1e3) * rhoH2O / 1E5        ! p. 2-52
    mindHR = -(22.6-62.438)*1e3/Rgas
    CALL Output(Hominus, mindHR)

    chem = "hydrogen iodide" ; casrn = "10034-85-2"               ! HI
    Hominus    = H_from_dG((-51.57-1.7)*1e3) * rhoH2O**2/1E5        ! p. 2-52
    mindHR = -(-55.19-26.48)*1e3/Rgas
    CALL MakeNote("489HI", "$\Hprime$~= $"//TRIM(Hprime_string(Hominus))// &
      "\times\exp\left("//TRIM(mindHR_string(mindHR))//"~\unit{K} "// &
      "\left(\DS\frac{1}{T}-\frac{1}{T^{\ominus}}\right)\right)$~"//TRIM(Hprime_unit_ol_TeX))
    CALL Output(DUMMY)

    chem   = "iodine chloride" ; casrn = "7790-99-0"              ! ICl
    Hominus    = H_from_dG((-17.1+5.46)*1e3) * rhoH2O / 1E5         ! p. 2-54
    CALL Output(Hominus)

    chem   = "iodine bromide" ; casrn = "7789-33-5"               ! IBr
    Hominus    = H_from_dG((-4.2-3.69)*1e3) * rhoH2O / 1E5          ! p. 2-54
    CALL Output(Hominus)

  END SUBROUTINE ref0489

  !---------------------------------------------------------------------------

  SUBROUTINE ref0490 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "490"

    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    type = "T"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 5.49E-3)
    CALL Output(Hominus)

    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 5.55E-3)
    CALL Output(Hominus)

    chem = "methylbenzene" ; casrn = "108-88-3" ! C6H5CH3
    type = "T"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 6.66E-3)
    CALL Output(Hominus)

    chem = "methylbenzene" ; casrn = "108-88-3" ! C6H5CH3
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 6.64E-3)
    CALL Output(Hominus)

    chem = "ethylbenzene" ; casrn = "100-41-4" ! C6H5C2H5
    type = "T"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 8.73E-3)
    CALL Output(Hominus)

    chem = "ethylbenzene" ; casrn = "100-41-4" ! C6H5C2H5
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 8.43E-3)
    CALL Output(Hominus)

    chem = "chlorobenzene" ; casrn = "108-90-7" ! C6H5Cl
    type = "T"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 3.71E-3)
    CALL Output(Hominus)

    chem = "chlorobenzene" ; casrn = "108-90-7" ! C6H5Cl
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 3.77E-3)
    CALL Output(Hominus)

    chem = "naphthalene" ; casrn = "91-20-3" ! C{10}H8
    type = "T"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 4.68E-4)
    CALL Output(Hominus)

    chem = "naphthalene" ; casrn = "91-20-3" ! C{10}H8
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 4.83E-4)
    CALL Output(Hominus)

    chem = "biphenyl" ; casrn = "92-52-4" ! (C6H5)2
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 4.08E-4)
    CALL Output(Hominus)

    chem = "acenaphthene" ; casrn = "83-32-9" ! C{12}H{10}
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 1.46E-4)
    CALL Output(Hominus)

    chem = "phenanthrene" ; casrn = "85-01-8" ! C{14}H{10}
    type = "M"
    Hominus = KHpcSI_TIMES_HcpSI / (atm * 3.93E-5)
    CALL Output(Hominus)

  END SUBROUTINE ref0490

  !---------------------------------------------------------------------------

  SUBROUTINE ref0491 ! KHpx [GPa]
    IMPLICIT NONE

    ref = "491"
    type = "M"

    ! O2 from Tab. I:
    chem = "oxygen" ; casrn = "7782-44-7" ! O2
    ndata = 14
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 275.45, 288.15, 298.15, 298.15, 298.15, 298.15, 298.16, &
      303.14, 318.14, 318.16, 318.16, 318.17, 318.17, 328.14 /)
    Harray = cH2O / (1E9 * (/ 2.7199, 3.6697, 4.4058, 4.3980, 4.4068, 4.3955, &
      4.4070, 4.7521, 5.6985, 5.6908, 5.6895, 5.6979, 5.6956, 6.1995 /) )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    ! CH4 and C2H6 from Tab. IV:
    CALL CalcH("methane", "74-82-8", &
      9.881539, 9.864392E3, -2.150246E6, 8.810241E7, 0.) ! CH4
    CALL CalcH("ethane",  "74-84-0", &
      -2.056101E2, 2.624901E5, -1.128303E8, 2.159875E10, -1.569604E12) ! C2H6

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A0, A1, A2, A3, A4)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A0, A1, A2, A3, A4
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = cH2O / (EXP(A0+A1/T0+A2/T0**2+A3/T0**3+A4/T0**4))
      mindHR = - (A1 + 2.*A2/T0 + 3.*A3/T0**2 + 4.*A4/T0**3)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0491

  !---------------------------------------------------------------------------

  SUBROUTINE ref0492 ! KHcc [1]
    IMPLICIT NONE

    ref = "492"

    chem = "carbon dioxide" ; casrn = "124-38-9" ! CO2
    CALL SettypeX("perry73")
    Hominus = KHcc_TIMES_HcpSI_atT0/1.2
    CALL Output(Hominus)

    chem = "ozone" ; casrn = "10028-15-6" ! O3
    CALL SettypeX("perry73")
    Hominus = KHcc_TIMES_HcpSI_atT0/3.36
    CALL Output(Hominus)

    ! vapor pressure probably from Abel and solubility probably from McKay
    chem = "nitric acid" ; casrn = "7697-37-2" ! HNO3
    type = "V"
    Hominus = KHcc_TIMES_HcpSI_atT0/0.46E-6
    CALL Output(Hominus)

  END SUBROUTINE ref0492

  !---------------------------------------------------------------------------

  SUBROUTINE ref0493 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "493"
    type = "M"

    chem = "methanoic acid" ; casrn = "64-18-6"
    CALL Output(5.53E3*Hbp_TO_HcpSI)

    chem = "ethanoic acid" ; casrn = "64-19-7"
    CALL Output(5.502E3*Hbp_TO_HcpSI)

    chem = "propanoic acid" ; casrn = "79-09-4"
    CALL Output(5.713E3*Hbp_TO_HcpSI)

    chem = "butanoic acid" ; casrn = "107-92-6"
    CALL Output(4.727E3*Hbp_TO_HcpSI)

    chem = "pentanoic acid" ; casrn = "109-52-4"
    mindHR = 6582.96
    CALL Output(EXP(-14.3371 + mindHR/T0)*Hbp_TO_HcpSI, mindHR)

    chem = "hexanoic acid" ; casrn = "142-62-1"
    mindHR = 6303.73
    CALL Output(EXP(-13.9424 + mindHR/T0)*Hbp_TO_HcpSI, mindHR)

    chem = "2-methyl propanoic acid" ; casrn = "79-31-2"
    CALL Output(1.127E3*Hbp_TO_HcpSI)

    chem = "3-methyl butanoic acid" ; casrn = "503-74-2"
    CALL Output(1.195E3*Hbp_TO_HcpSI)

    chem = "2,2-dimethyl propanoic acid" ; casrn = "75-98-9" ! neovaleric acid
    CALL Output(3.53E2*Hbp_TO_HcpSI)

    chem = "2-oxopropanoic acid" ; casrn = "127-17-3"
    mindHR = 5087.92
    CALL Output(EXP(-4.41706 + mindHR/T0)*Hbp_TO_HcpSI, mindHR)

  END SUBROUTINE ref0493

  !---------------------------------------------------------------------------

  SUBROUTINE ref0495 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "495"

    chem = "propanone" ; casrn = "67-64-1"
    type = "M"
    CALL Output(32.*Hcp_TO_HcpSI, 48E3/Rgas)

    chem = "1-phenylethanone" ; casrn = "98-86-2"
    type = "M"
    CALL Output(110.*Hcp_TO_HcpSI, 50E3/Rgas)

    chem = "chloro-2-propanone" ; casrn = "78-95-5"
    type = "M"
    CALL Output(59.*Hcp_TO_HcpSI, 45E3/Rgas)

    chem = "2,3-butanedione" ; casrn = "431-03-8"
    type = "M"
    CALL Output(74.*Hcp_TO_HcpSI, 47E3/Rgas)

    chem = "1,1,1-trifluoro-2-propanone" ; casrn = "421-50-1"
    type = "M"
    CALL Output(138.*Hcp_TO_HcpSI, 74E3/Rgas)

    ! from p. 1477:

    chem = "2-butanone" ; casrn = "78-93-3" ! C2H5COCH3 methyl ethyl ketone; MEK
    type = "?"
    CALL Output(31.*Hcp_TO_HcpSI)

    chem = "4-methyl-2-pentanone" ; casrn = "108-10-1" ! MIBK
    type = "?"
    CALL Output(30.*Hcp_TO_HcpSI)

    chem = "3-buten-2-one" ; casrn = "78-94-4" ! MVK
    type = "?"
    CALL Output(44.*Hcp_TO_HcpSI)

  END SUBROUTINE ref0495

  !---------------------------------------------------------------------------

  SUBROUTINE ref0496 ! Hcc [1]
    IMPLICIT NONE

    ref = "496"
    type = "M"

    chem = "propanone" ; casrn = "67-64-1"
    CALL Output(682.*Hcc_TO_HcpSI_atT0)

    chem = "methanol" ; casrn = "67-56-1"
    CALL Output(5580.*Hcc_TO_HcpSI_atT0)

    chem = "ethanol" ; casrn = "64-17-5"
    CALL Output(5320.*Hcc_TO_HcpSI_atT0)

    chem = "1-propanol" ; casrn = "71-23-8"
    CALL Output(4000.*Hcc_TO_HcpSI_atT0)

    chem = "1-butanol" ; casrn = "71-36-3"
    CALL Output(3390.*Hcc_TO_HcpSI_atT0)

  END SUBROUTINE ref0496

  !---------------------------------------------------------------------------

  SUBROUTINE ref0498 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "498"

    type = "C"

    chem = "CHF2Cl" ; casrn = "75-45-6" ! R22
    CALL IncorrectCitation("1014","HCFC-22")
    CALL Output(DUMMY)
    ! see ref1014 for other HCFC data

    chem = "carbonyl fluoride" ; casrn = "353-50-4" ! COF2
    CALL SettypeX("george93")
    CALL Output(20.*Hcp_TO_HcpSI)

    chem = "carbonic chloride fluoride" ; casrn = "353-49-1" ! COFCl
    CALL SettypeX("george93")
    CALL Output(10.*Hcp_TO_HcpSI)

    type = "E"

    chem = "chlorodifluoronitrooxymethane" ; casrn = "70490-95-8" ! CClF2OONO2
    CALL MakeNote("498XPAN", &
      TRIM(citet())//" assume $\H(\chem{CClF_2OONO_2})$ = "// &
      "$\H(\chem{PAN})$.")
    mindHR = 5910.
    Hominus = 2.9*Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

    chem = "formyl fluoride" ; casrn = "1493-02-3" ! FCHO
    CALL Output(3.*Hcp_TO_HcpSI)

  END SUBROUTINE ref0498

  !---------------------------------------------------------------------------

END MODULE Henry_ref0500

!*****************************************************************************
!                                  end of file
!*****************************************************************************
