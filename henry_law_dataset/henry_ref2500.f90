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

MODULE henry_ref2500

  USE henry_mem
  USE henry_util
  IMPLICIT NONE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ref2013 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2013"
    type = "M"

    ! from Table C2-1:
    CALL CalcH("R50",     "74-82-8",  137.66,   -6938.2,  -18.616)
    CALL CalcH("R40",     "74-87-3",  174.34,   -9923.2,  -24.030)
    CALL CalcH("R32",     "75-10-5",  140.32,   -8046.9,  -19.122)
    CALL CalcH("R22",     "75-45-6",  200.18,  -11267.0,  -27.611)
    CALL CalcH("R11",     "75-69-4",  211.71,  -12405.5,  -28.809)
    CALL CalcH("R12",     "75-71-8",  124.26,   -7710.8,  -15.950)
    CALL CalcH("R13",     "75-72-9",   16.040,  -2168.4)
    CALL CalcH("R14",     "75-73-0",  348.83,  -16760.1,  -49.562)
    CALL CalcH("R170",    "74-84-0",   99.633,  -5994.4,  -12.567)
    CALL CalcH("R160",    "75-00-3",  192.42,  -11153.1,  -26.492)
    CALL CalcH("R152a",   "75-37-6",  172.15,   -9778.0,  -23.631)
    CALL CalcH("R142b",   "75-68-3",  188.95,  -10856.3,  -25.727)
    CALL CalcH("R133a",   "75-88-7",  205.06,  -11848.4,  -28.154)
    CALL CalcH("R134a",  "811-97-2",  204.11,  -11302.1,  -28.142)
    CALL CalcH("R124",  "2837-89-0",  188.82,  -11032.5,  -25.552)
    CALL CalcH("R125",   "354-33-6",  156.22,   -9145.9,  -20.743)
    CALL CalcH("R113",    "76-13-1",   29.510,  -6538.2)
    CALL CalcH("R114",    "76-14-2",  101.51,   -6487.3,  -12.466)
    CALL CalcH("R115",    "76-15-3",  134.83,   -7327.1,  -17.632)
    CALL CalcH("R1150",   "74-85-1",  164.07,   -8636.6,  -22.474)
    CALL CalcH("R1122",  "359-10-4",  186.57,  -10884.0,  -25.321)
    CALL CalcH("R290",    "74-98-6",  275.25,  -14165.6,  -38.524)
    CALL CalcH("R227",   "431-89-0",   54.131,  -4851.1,   -5.188)
    CALL CalcH("R1270",  "115-07-1",  233.74,  -12490.1,  -32.457)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, C0, Cm1, Cln)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: C0, Cm1 ! Cm1 = C_{-1}
      REAL, OPTIONAL,   INTENT(IN) :: Cln
      REAL :: HiSV ! Henry constant in [MPa]

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (PRESENT(Cln)) THEN
        ! T-dep with 3 parameters:
        ! ln(H) = C0 + Cm1/T + Cln*ln(T)
        HiSV = EXP(C0 + Cm1/T0 + Cln*LOG(T0))
        ! analytical derivative:
        ! d ln(H) / d (1/T) = Cm1 - Cln*T
        mindHR = Cln*T0 - Cm1
      ELSE
        ! T-dep with 2 parameters C0, Cm1
        HiSV = EXP(C0 + Cm1/T0)
        mindHR = -Cm1
      ENDIF
      CALL Output(cH2O/(HiSV*1.E6), mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2013

  !---------------------------------------------------------------------------

  SUBROUTINE ref2014 ! Hcc [1]
    IMPLICIT NONE

    ref = "2014"
    type = "M"

    chem = "N-acetylpyrrolidine" ; casrn = "4030-18-6"
    CALL Output(Hcc_TO_HcpSI_atT0*1.54E7)

    chem = "N-butylacetamide" ; casrn = "1119-49-9"
    CALL Output(Hcc_TO_HcpSI_atT0*6.7E6)

  END SUBROUTINE ref2014

  !---------------------------------------------------------------------------

  SUBROUTINE ref2016 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2016"
    type = "T"

    CALL CalcH("dimethylamine",                             "124-40-3",  0.0,   -4.5) ! (CH3)2NH
    CALL CalcH("diethylamine",                              "109-89-7",  0.2,   -7.1) ! (C2H5)2NH
    CALL CalcH("dimethyl ether",                            "115-10-6",  2.4,   -1.5) ! CH3OCH3
    CALL CalcH("ethyl methyl ether",                        "540-67-0",  2.2        ) ! C2H5OCH3
    CALL CalcH("diethyl ether",                              "60-29-7",  2.7,   -3.0) ! C2H5OC2H5
    CALL CalcH("methyl {tert}-butyl ether",                "1634-04-4",  2.1        ) ! CH3OC(CH3)3
    CALL CalcH("dimethyl sulfide",                           "75-18-3",  2.7,    0.8) ! CH3SCH3 DMS
    CALL CalcH("ethyl methyl sulfide",                      "624-89-5",  2.8        ) ! C3H8S
    CALL CalcH("dimethylsulfoxide",                          "67-68-5", -5.8,   -9.0) ! CH3SOCH3 DMSO
    CALL CalcH("propanone",                                  "67-64-1",  0.5,   -1.5) ! CH3COCH3 acetone
    CALL CalcH("2-butanone",                                 "78-93-3",  0.6,   -2.7) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("3-methyl-2-butanone",                       "563-80-4",  1.1,   -3.1) ! C5H{10}O isopropyl methyl ketone
    CALL CalcH("1-cyclopropyl-ethanone",                    "765-43-5", -0.3,   -3.5) ! C5H8O cyclopropyl methyl ketone
    CALL CalcH("cyclohexyl methyl ketone",                  "823-76-7",  0.4,   -6.1) ! C6H{11}COCH3
    CALL CalcH("3,3-dimethyl-2-butanone",                    "75-97-8",  1.2,   -3.7) ! C6H{12}O tert-butyl methyl ketone
    CALL CalcH("benzaldehyde",                              "100-52-7",  0.3,   -2.5) ! C6H5CHO
    CALL CalcH("1-phenylethanone",                           "98-86-2", -0.3,   -4.4) ! C6H5COCH3 acetophenone
    CALL CalcH("4-methoxyphenyl methyl ketone",             "100-06-1", -0.1        ) ! C9H{10}O2
    CALL CalcH("3-pentanone",                                "96-22-0",  0.9,   -3.6) ! C2H5COC2H5
    CALL CalcH("2,4-dimethyl-3-pentanone",                  "565-80-0", -0.3,   -4.4) ! C7H{14}O diisopropyl ketone
    CALL CalcH("dicyclopropylmethanone",                   "1121-37-5", -1.0,   -6.2) ! C7H{10}O dicyclopropyl ketone
    CALL CalcH("2,2,4,4-tetramethyl-3-pentanone",           "815-24-7",  1.9        ) ! C9H{18}O di-t-butyl ketone
    CALL CalcH("benzophenone",                              "119-61-9", -1.4,  -10.4) ! C{13}H{10}O diphenyl ketone
    CALL CalcH("methyl ethanoate",                           "79-20-9",  1.0,   -1.2) ! CH3COOCH3 methyl acetate
    CALL CalcH("methyl propanoate",                         "554-12-1",  1.4,   -2.4) ! C2H5COOCH3 methyl propionate
    CALL CalcH("2-methyl-propanoic acid methyl ester",      "547-63-7",  1.7,   -3.1) ! C5H{10}O2 methyl isobutyrate
    CALL CalcH("cyclopropanecarboxylic acid methyl ester", "2868-37-3",  0.2,   -3.8) ! C5H8O2
    CALL CalcH("cyclohexanecarboxylic acid methyl ester",  "4630-82-4",  1.0,   -6.0) ! C6H{11}COOCH3
    CALL CalcH("2,2-dimethyl-propanoic acid methyl ester",  "598-98-1",  1.9,   -3.6) ! C6H{12}O2 methyl pivalate
    CALL CalcH("methyl benzoate",                            "93-58-3",  0.0,   -4.2) ! C6H5COOCH3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, relDeltaG, relDeltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: relDeltaG ! [kcal/mol]
      REAL, OPTIONAL,   INTENT(IN) :: relDeltaH ! [kcal/(mol*K)]
      REAL :: DeltaG, DeltaH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! Listed values are relative to NH3 (see footnotes in Table 3).
      DeltaG = 1E3 * cal * (relDeltaG - 2.409) ! [J/mol]
      Hominus    = Hcp_TO_HcpSI * EXP(-DeltaG / (Rgas*T0))
      CALL MakeNote(TRIM(ref), &
        "Calculated under the assumption that $\Delta G$ and $\Delta H$ "// &
        "are based on [\unit{mol/l}] and [\unit{atm}] as the standard states.")
      IF (PRESENT(relDeltaH)) THEN
        DeltaH = 1E3 * cal * (relDeltaH - 8.243) ! [J/mol]
        mindHR = -DeltaH / Rgas
        CALL Output(Hominus, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref2016

  !---------------------------------------------------------------------------

  SUBROUTINE ref2105 ! special definition
    IMPLICIT NONE

    ref = "2105"
    type = "L"

    CALL CalcH("helium",              "7440-59-7",  -3.52839, 7.12983,   4.47770) ! He
    CALL CalcH("neon",                "7440-01-9",  -3.18301, 5.31448,   5.43774) ! Ne
    CALL CalcH("argon",               "7440-37-1",  -8.40954, 4.29587,  10.52779) ! Ar
    CALL CalcH("krypton",             "7439-90-9",  -8.97358, 3.61508,  11.29963) ! Kr
    CALL CalcH("xenon",               "7440-63-3", -14.21635, 4.00041,  15.60999) ! Xe
    CALL CalcH("hydrogen",            "1333-74-0",  -4.73284, 6.08954,   6.06066) ! H2
    CALL CalcH("nitrogen",            "7727-37-9",  -9.67578, 4.72162,  11.70585) ! N2
    CALL CalcH("oxygen",              "7782-44-7",  -9.44833, 4.43822,  11.42005) ! O2
    CALL CalcH("carbon monoxide",      "630-08-0", -10.52862, 5.13259,  12.01421) ! CO
    CALL CalcH("carbon dioxide",       "124-38-9",  -8.55445, 4.01195,   9.52345) ! CO2
    CALL CalcH("hydrogen sulfide",    "7783-06-4",  -4.51499, 5.23538,   4.42126) ! H2S
    CALL CalcH("methane",               "74-82-8", -10.44708, 4.66491,  12.12986) ! CH4
    CALL CalcH("ethane",                "74-84-0", -19.67563, 4.51222,  20.62567) ! C2H6
    CALL CalcH("sulfur hexafluoride", "2551-62-4", -16.56118, 2.15289,  20.35440) ! SF6

  CONTAINS

    REAL FUNCTION H_T (A, B, C, temp)
      IMPLICIT NONE
      REAL,             INTENT(IN) :: A, B, C, temp
      REAL, PARAMETER :: Tc = 647.096  ! T_crit [K]
      REAL, PARAMETER :: pc = 22.064E6 ! p_crit [Pa]
      REAL, PARAMETER :: a1=-7.85951783, a2=1.84408259,  a3=-11.7866497, &
                         a4=22.6807411,  a5=-15.9618719, a6=1.80122502
      REAL :: theta, tau, p1star
      theta = temp/Tc
      tau = 1. - theta
      p1star = EXP((Tc/temp)*( &
        a1*tau+a2*tau**1.5+a3*tau**3+a4*tau**3.5+a5*tau**4+a6*tau**7.5 &
        )) * pc
      H_T = cH2O / (EXP(A/theta+B*tau**0.355/theta+C*theta**(-0.41)*EXP(tau)) &
        * p1star)
    END FUNCTION H_T

    REAL FUNCTION derivative(A, B, C, delta)
      IMPLICIT NONE
      REAL, INTENT(IN) :: A, B, C
      REAL, INTENT(IN) :: delta
      ! calculate d ln(H) / d (1/T) at T0 numerically:
      derivative = &
        (LOG(H_T(A, B, C, T0+delta)) - LOG(H_T(A, B, C, T0-delta))) / &
        ((1./(T0+delta)) - (1./(T0-delta)))
    END FUNCTION derivative

    SUBROUTINE CalcH (chem_, casrn_, A, B, C)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, C
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = H_T(A, B, C, T0)
      ! print numerical solutions to find a suitable delta:
      ! PRINT *, derivative(A, B, C, 10.)        ,10.
      ! PRINT *, derivative(A, B, C, 1.)         ,1.
      ! PRINT *, derivative(A, B, C, 0.1)        ,0.1
      ! PRINT *, derivative(A, B, C, 0.01)       ,0.01
      ! PRINT *, derivative(A, B, C, 0.001)      ,0.001
      ! PRINT *, derivative(A, B, C, 0.0001)     ,0.0001
      ! PRINT *, derivative(A, B, C, 0.00001)    ,0.00001
      ! PRINT *, derivative(A, B, C, 0.000001)   ,0.000001
      ! PRINT *, derivative(A, B, C, 0.0000001)  ,0.0000001
      ! PRINT *
      mindHR = derivative(A, B, C, 0.0001)
      CALL MakeNote(TRIM(ref), &
        "Vapor pressure data for water from \citet{2555} "// &
        "was needed to calculate $\H$.")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2105

  !---------------------------------------------------------------------------

  SUBROUTINE ref2121 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2121"
    type = "M"

    CALL CalcH("chlorodifluoromethane",               "75-45-6", &
      (/ 6., 12., 22., 30., 40. /), (/ 0.0142, 0.0298, 0.0406, 0.0586, 0.0907 /) ) ! CHF2Cl R22
    CALL CalcH("1-chloro-1,1-difluoroethane",         "75-68-3", &
      (/ 6., 12., 22., 30., 40. /), (/ 0.0318, 0.0409, 0.0588, 0.0798, 0.101 /) ) ! CH3CF2Cl R142b
    CALL CalcH("2,2-dichloro-1,1,1-trifluoroethane", "306-83-2", &
      (/ 6., 12., 22., 30., 40. /), (/ 0.0131, 0.0193, 0.0256, 0.0364, 0.0508 /) ) ! C2HF3Cl2 R123
    CALL CalcH("1,1,1,2-tetrafluoroethane",          "811-97-2", &
      (/ 6., 12., 22., 30.      /), (/ 0.0272, 0.0357, 0.0500, 0.0546 /) ) ! C2H2F4 R134a

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp+CtoK, 1./(atm*H), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2121

  !---------------------------------------------------------------------------

  SUBROUTINE ref2122 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2122"
    type = "M"
    ndata = 9
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0., 5., 10., 15., 20., 25., 30., 35., 40. /) + CtoK

    CALL CalcH("1,8-dichlorooctane", "2162-99-4", (/ 0.666, 1.18,  2.00, 3.26, 5.12, 7.75, 11.4, 16.2, 22.3 /) )
    CALL CalcH("1,8-dibromooctane",  "4549-32-0", (/ 0.406, 0.692, 1.14, 1.81, 2.79, 4.19, 6.12, 8.73, 12.2 /) )

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHMPa)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHMPa  ! KH [MPa]
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = cH2O / (KHMPa * 1.E6)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2122

  !---------------------------------------------------------------------------

  SUBROUTINE ref2123 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "2123"
    type = "V"

    CALL CalcH("nitrobenzene",     "98-95-3", 64.5260, -59.7840, -38.4032) ! C6H5NO2
    CALL CalcH("aminobenzene",     "62-53-3", 48.9325, -46.4403, -24.6450) ! C6H7N aniline
    CALL CalcH("cyclohexanamine", "108-91-8", 66.7211, -63.4764, -37.2838) ! C6H{13}N cyclohexylamine

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, C
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! T-dep with 3 parameters:
      ! ln(H) = A + B/tau + C ln(tau) with tau = T/T0
      Hominus    = cH2O / (EXP(A+B) * 1.E3)
      ! analytical derivative:
      ! d ln(H) / d (1/T) = Cm1 - Cln*T
      mindHR = - ( B*T0 - C*T0 )
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2123

  !---------------------------------------------------------------------------

  SUBROUTINE ref2124 ! KHpc [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2124"

    ! Table 1:
    CALL CalcH("alpha-pinene",         "80-56-8", 13560., 36.9E3)
    CALL CalcH("beta-pinene",         "127-91-3",  6880., 37.0E3)
    CALL CalcH("alpha-phellandrene",   "99-83-2",  5566., 37.0E3)
    CALL CalcH("beta-phellandrene",   "555-10-2",  5567., 42.3E3)
    CALL CalcH("alpha-terpinene",      "99-86-5",  3491., 39.8E3)
    CALL CalcH("gamma-terpinene",      "99-85-4",  2615., 39.7E3)
    CALL CalcH("terpinolene",         "586-62-9",  2658., 44.3E3)
    CALL CalcH("R-(+)-limonene",     "5989-27-5",  2851., 37.8E3)
    CALL CalcH("S-(-)-limonene",     "5989-54-8",  2876., 36.9E3)
    CALL CalcH("alpha-terpineol",      "98-55-5",  0.226, 18.0E3)
    ! Table 2:
    CALL CalcH("linalool",             "78-70-6", 2.078  )
    CALL CalcH("alpha-terpineol",      "98-55-5", 0.244  )
    CALL CalcH("camphor",              "76-22-2", 0.900  )
    CALL CalcH("1,4-cineole",         "470-67-7", 13.48  )
    CALL CalcH("1,8-cineole",         "470-82-6", 13.27  )
    CALL CalcH("alpha-pinene oxide", "1686-14-2", 42.2   )
    CALL CalcH("isoprene",             "78-79-5", 7789.  )
    CALL CalcH("myrcene",             "123-35-3", 6300.  )
    CALL CalcH("beta-ocimene",      "13877-91-3", 2507.  )
    CALL CalcH("m-cymene",            "535-77-3", 1105.  )
    CALL CalcH("p-cymene",             "99-87-6", 935.   )
    CALL CalcH("camphene",             "79-92-5", 3238.  )
    CALL CalcH("3-carene",          "13466-78-9", 13650. )
    CALL CalcH("alpha-pinene",         "80-56-8", 13590. )
    CALL CalcH("beta-pinene",         "127-91-3", 6826.  )
    CALL CalcH("sabinene",           "3387-41-5", 6451.  )
    CALL CalcH("limonene",            "138-86-3", 2850.  )
    CALL CalcH("p-menthane",           "99-82-1", 179900.)
    CALL CalcH("alpha-phellandrene",   "99-83-2", 5496.  )
    CALL CalcH("beta-phellandrene",   "555-10-2", 5666.  )
    CALL CalcH("alpha-terpinene",      "99-86-5", 3593.  )
    CALL CalcH("gamma-terpinene",      "99-85-4", 2601.  )
    CALL CalcH("terpinolene",         "586-62-9", 2682.  )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hpc, deltaHvol)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Hpc
      REAL, OPTIONAL,   INTENT(IN) :: deltaHvol
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (PRESENT(deltaHvol)) THEN
        ! data from Table 1:
        type = "M"
        Hominus = KHpcSI_TIMES_HcpSI / Hpc
        mindHR = deltaHvol / Rgas
        CALL Output(Hominus, mindHR)
      ELSE
        ! data from Table 2:
        type = "V"
        Hominus = KHpcSI_TIMES_HcpSI / Hpc
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2124

  !---------------------------------------------------------------------------

  SUBROUTINE ref2125 ! KHcc [1]
    IMPLICIT NONE

    ref = "2125"
    type = "V"

    CALL CalcH("myrcene",         "123-35-3", -0.25, 21.1E3)
    CALL CalcH("citronellal",     "106-23-0", -1.79, 34.9E3)
    CALL CalcH("limonene",        "138-86-3", -0.20, 22.6E3)
    CALL CalcH("pulegone",         "89-82-7", -2.84, 41.8E3)
    CALL CalcH("terpineol",        "98-55-5", -3.17, 37.7E3)
    CALL CalcH("anethole",        "104-46-1", -2.39, 49.1E3)
    CALL CalcH("cinnamaldehyde",  "104-55-2", -3.54, 49.5E3)
    CALL CalcH("carvacrol",       "499-75-2", -3.77, 74.6E3)
    CALL CalcH("thymol",           "89-83-8", -3.87, 75.1E3)
    CALL CalcH("eugenol",          "97-53-0", -4.25, 78.5E3)
    CALL CalcH("limonene oxide",  "470-82-6", -1.83, 35.5E3)
    CALL CalcH("cineole",         "470-67-7", -2.54, 30.9E3)
    CALL CalcH("thujone",        "1125-12-8", -2.41, 36.4E3)
    CALL CalcH("camphor",          "76-22-2", -3.13, 37.4E3)
    CALL CalcH("norcamphor",      "497-38-1", -3.03, 40.0E3)
    CALL CalcH("norborneol",     "1632-68-4", -3.74, 39.4E3)
    CALL CalcH("pinene oxide",   "1686-14-2", -2.13, 34.1E3)
    CALL CalcH("adamantane",      "281-23-2", -0.30, 25.7E3)
    CALL CalcH("adamantanone",    "700-58-3", -3.54, 46.0E3)
    CALL CalcH("1-adamantanol",   "768-95-6", -4.17, 41.9E3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKAW, DeltaHAW)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logKAW, DeltaHAW
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / (10.**logKAW)
      mindHR = DeltaHAW/Rgas + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2125

  !---------------------------------------------------------------------------

  SUBROUTINE ref2165 ! KHpc [Pa*L/mol]
    IMPLICIT NONE

    ref = "2165"

    CALL CalcH("benzene",                         "71-43-2", "C",  5.75)
    CALL CalcH("methylbenzene",                  "108-88-3", "C",  5.83)
    CALL CalcH("hydroxybenzene",                 "108-95-2", "C",  1.60)
    CALL CalcH("chlorobenzene",                  "108-90-7", "C",  5.57)
    CALL CalcH("1,2-dichlorobenzene",             "95-50-1", "C",  5.28)
    CALL CalcH("1,4-dichlorobenzene",            "106-46-7", "C",  5.39)
    CALL CalcH("bromobenzene",                   "108-86-1", "C",  5.40)
    CALL CalcH("1,2-dibromobenzene",             "583-53-9", "V",  5.02)
    CALL CalcH("1,4-dibromobenzene",             "106-37-6", "V",  5.37)
    CALL CalcH("aminobenzene",                    "62-53-3", "V",  2.22)
    CALL CalcH("methoxybenzene",                 "100-66-3", "C",  4.16)
    CALL CalcH("benzaldehyde",                   "100-52-7", "C",  3.43)
    CALL CalcH("1-hydroxy-2-methylbenzene",       "95-48-7", "C",  2.08)
    CALL CalcH("1-hydroxy-3-methylbenzene",      "108-39-4", "V",  1.93)
    CALL CalcH("1-hydroxy-4-methylbenzene",      "106-44-5", "C",  1.90)
    CALL CalcH("nitrobenzene",                    "98-95-3", "C",  3.39)
    CALL CalcH("benzenethiol",                   "108-98-5", "C",  4.52)
    CALL CalcH("1,4-dimethylbenzene",            "106-42-3", "C",  5.88)
    CALL CalcH("1,4-dihydroxybenzene",           "123-31-9", "V", -1.50)
    CALL CalcH("benzenenitrile",                 "100-47-0", "V",  3.41)
    CALL CalcH("1,2,3-trimethoxybenzene",        "634-36-6", "V",  2.44)
    CALL CalcH("fluorobenzene",                  "462-06-6", "V",  5.80)
    CALL CalcH("1-chloro-2-methylbenzene",        "95-49-8", "C",  5.55)
    CALL CalcH("1-chloro-3-methylbenzene",       "108-41-8", "V",  6.21)
    CALL CalcH("iodobenzene",                    "591-50-4", "V",  5.10)
    CALL CalcH("1,2-dihydroxybenzene",           "120-80-9", "V",  0.80)
    CALL CalcH("1,3-dihydroxybenzene",           "108-46-3", "V", -0.70)
    CALL CalcH("1,2-benzenediamine",              "95-54-5", "V",  1.12)
    CALL CalcH("1,3-benzenediamine",             "108-45-2", "V", -1.13)
    CALL CalcH("2-methylbenzonitrile",           "529-19-1", "V",  3.12)
    CALL CalcH("2,6-dichlorobenzenenitrile",    "1194-65-6", "V",  2.86)
    CALL CalcH("2-nitrotoluene",                  "88-72-2", "V",  3.73)
    CALL CalcH("1-methyl-2,4-dinitrobenzene",    "121-14-2", "V",  1.00)
    CALL CalcH("1-methyl-2,4,6-trinitrobenzene", "118-96-7", "V",  0.27)
    CALL CalcH("1-chloro-3-nitrobenzene",        "121-73-3", "V",  3.97)
    CALL CalcH("3-bromonitrobenzene",            "585-79-5", "V",  2.27)
    CALL CalcH("1-phenylethanone",                "98-86-2", "C",  3.03)
    CALL CalcH("2-methylaniline",                 "95-53-4", "V",  2.39)
    CALL CalcH("(methylamino)-benzene",          "100-61-8", "V",  3.06)
    CALL CalcH("2,4-dimethylbenzenamine",         "95-68-1", "V",  3.85)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, logH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: logH     ! from Table 1
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI * 1000. / (10.**logH)
      IF (type_=="C") CALL MakeNote("whichref")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2165

  !---------------------------------------------------------------------------

  SUBROUTINE ref2169 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2169"
    type = "?"

    chem = "mercury dibromide" ; casrn = "7789-47-1" ! HgBr2
    CALL MakeNote("2169wrongrefHgBr2", &
      TRIM(citet())//" refer to \citet{2255} "// &
      "as the source but this value cannot be found there.")
    CALL Output(2.75E6*Hcp_TO_HcpSI)

  END SUBROUTINE ref2169

  !---------------------------------------------------------------------------

  ! ref2171 is not used because it is just a collection of citations
  ! with no proper references, a mix of units, and a very strange value
  ! for HgO.

  SUBROUTINE ref2171 ! KHpcSI [Pa*m3/mol] and KHcc [1]
    IMPLICIT NONE

    ref = "2171"
    type = "?"

    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))

    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    CALL MakeNoteOtherTemp("293")
    CALL MakeNote("whichref")
    CALL Output(KHpcSI_TIMES_HcpSI/729.)

    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    temp = (/ 5., 25. /) + CtoK
    Harray   = KHcc_TO_HcpSI ( (/ 0.18, 0.32 /), temp )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("whichref")
    CALL Output(Hominus, mindHR, r2)

    chem = "mercury dichloride" ; casrn = "7487-94-7" ! HgCl2
    CALL MakeNoteOtherTemp("293")
    CALL MakeNote("whichref")
    CALL Output(KHpcSI_TIMES_HcpSI/3.69E-5)

    chem = "mercury(II) oxide" ; casrn = "21908-53-2" ! HgO
    CALL MakeNote("whichref")
    CALL Output(KHpcSI_TIMES_HcpSI/3.76E-11)

    chem = "chloromethylmercury" ; casrn = "115-09-3" ! CH3HgCl
    CALL MakeNoteOtherTemp("288")
    CALL MakeNote("whichref")
    CALL Output(KHcc_TO_HcpSI ( 1.6E-5, 288. ))

    chem = "dimethylmercury" ; casrn = "593-74-8" ! (CH3)2Hg
    CALL MakeNote("whichref")
    CALL Output(KHpcSI_TIMES_HcpSI/646.)

    chem = "dimethylmercury" ; casrn = "593-74-8" ! (CH3)2Hg
    temp = (/ 0., 25. /) + CtoK
    Harray   = KHcc_TO_HcpSI ( (/ 0.15, 0.31 /), temp )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("whichref")
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2171

  !---------------------------------------------------------------------------

  SUBROUTINE ref2183 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "2183"
    type = "M"

    chem = "hydroxybenzene" ; casrn = "108-95-2" ! C6H5OH phenol
    CALL Output(2820.*Hbp_TO_HcpSI,2720.)

    chem = "2-nitrophenol" ; casrn = "88-75-5" ! HOC6H4(NO2)
    CALL Output(147.*Hbp_TO_HcpSI,5720.)

    chem = "3-nitrophenol" ; casrn = "554-84-7" ! HOC6H4(NO2)
    CALL MakeNoteOtherTemp("308")
    CALL Output(1.6E4*Hbp_TO_HcpSI)

    chem = "4-nitrophenol" ; casrn = "100-02-7" ! HOC6H4(NO2)
    CALL MakeNoteOtherTemp("308")
    CALL Output(2.1E4*Hbp_TO_HcpSI)

  END SUBROUTINE ref2183

  !---------------------------------------------------------------------------

  SUBROUTINE ref2203 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "2203"
    type = "M"
    chem = "sulfuryl fluoride" ; casrn = "2699-79-8"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0., 23.3 /) + CtoK
    Harray = (/0.529, 0.215 /) * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2203

  !---------------------------------------------------------------------------

  SUBROUTINE ref2212 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2212"
    type = "?"
    chem = "oxoethanoic acid" ; casrn = "298-12-4" ! OHCCOOH glyoxylic acid
    CALL MakeNote("2212wrongref", &
      TRIM(citet())//" refers to \citet{588} "// &
      "as the source but the quoted value cannot be found there.")
    CALL Output(DUMMY)

  END SUBROUTINE ref2212

  !---------------------------------------------------------------------------

  SUBROUTINE ref2216 ! KHpc [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2216"
    type = "M"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 5., 10., 20., 30., 35. /) + CtoK

    CALL CalcH("alpha-HCH", "319-84-6", (/ 0.095,  0.15,   0.39,  0.85,  1.30  /) ) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("beta-HCH",  "319-85-7", (/ 0.0054, 0.0092, 0.022, 0.053, 0.088 /) ) ! C6H6Cl6 $\beta$-lindane
    CALL CalcH("gamma-HCH",  "58-89-9", (/ 0.040,  0.061,  0.14,  0.33,  0.59  /) ) ! C6H6Cl6 $\gamma$-lindane

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = KHpcSI_TIMES_HcpSI / H
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2216

  !---------------------------------------------------------------------------

  SUBROUTINE ref2218 ! KHpx [mmHg]
    IMPLICIT NONE

    ref = "2218"

    ! Table 1:
    CALL CalcH("methanol",            "67-56-1", 187.)
    CALL CalcH("ethanol",             "64-17-5", 220.)
    CALL CalcH("1-propanol",          "71-23-8", 283.)
    CALL CalcH("2-propanol",          "67-63-0", 335.)
    CALL CalcH("1-butanol",           "71-36-3", 358.)
    CALL CalcH("2-butanol",           "78-92-2", 424.)
    CALL CalcH("2-methyl-1-propanol", "78-83-1", 516.)
    CALL CalcH("2-methyl-2-propanol", "75-65-0", 547.)
    CALL CalcH("1-pentanol",          "71-41-0", 461.)
    CALL CalcH("1-hexanol",          "111-27-3", 600.)

    CALL CalcH("1-butanamine",       "109-73-9", 800.)
    CALL CalcH("1-pentanamine",      "110-58-7", 1030.)
    CALL CalcH("1-hexanamine",       "111-26-2", 1310.)
    CALL CalcH("1-heptanamine",      "111-68-2", 1700.)
    CALL CalcH("1-octanamine",       "111-86-4", 2170.)

    CALL CalcH("pentane",            "109-66-0", 5.33E7)
    CALL CalcH("hexane",             "110-54-3", 7.62E7)
    CALL CalcH("heptane",            "142-82-5", 8.63E7)
    CALL CalcH("octane",             "111-65-9", 1.35E8)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpx)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpx

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      CALL Output(cH2O / (mmHg * KHpx))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2218

  !---------------------------------------------------------------------------

  SUBROUTINE ref2219 ! Hcc [1]
    IMPLICIT NONE

    ref = "2219"

    CALL CalcH("benzene",         "71-43-2",  -7342.9,  24.3, -0.009)
    CALL CalcH("methylbenzene",  "108-88-3", -11705.1,  49.3, -0.044)
    CALL CalcH("ethylbenzene",   "100-41-4", -21362.9, 110.1, -0.138)
    CALL CalcH("propylbenzene",  "103-65-1", -17393.7,  79.3, -0.079)
    CALL CalcH("butylbenzene",   "104-51-8", -26582.0, 134.0, -0.160)
    CALL CalcH("pentylbenzene",  "538-68-1", -27621.6, 131.6, -0.144)
    CALL CalcH("hexylbenzene",  "1077-16-3", -28680.6, 128.2, -0.127)
    CALL CalcH("heptylbenzene", "1078-71-3", -26489.8, 101.5, -0.069)
    CALL CalcH("octylbenzene",  "2189-60-8", -32478.7, 129.6, -0.102)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, C

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! ln(H) = L ln(T) + A/T^2 + B/T + C + DT
      ! substitute x:= 1/T
      ! ln(H) = L ln(1/x) + A x^2 + B x + C + D/x
      ! analytical derivative:
      ! d ln(H) / d (1/T) = d ln(H) / dx = -L/x + 2 Ax + B - D/x^2
      !                                    = -LT + 2 A/T + B - DT^2
      ! same formula as in ref1498, see also ref1498.gnu

      Hominus = EXP((A + B*T0 + C*T0**2)*cal/(-Rgas*T0)) / (Rgas*T0)
      mindHR = T0 -A*cal/Rgas + C*T0**2*cal/Rgas
      type = "V"
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2219

  !---------------------------------------------------------------------------

  SUBROUTINE ref2220 ! only temperature dependence
    IMPLICIT NONE

    ref = "2220"
    ! see also ref1497, Tab. 14

    ! Table IV:
    CALL CalcH("2-pentanone",                                 "107-87-9", -48.7) ! n-PrCOMe
    CALL CalcH("3-methyl-2-butanone",                         "563-80-4", -47.4) ! i-PrCOMe
    CALL CalcH("1-cyclopropyl-ethanone",                      "765-43-5", -49.3) ! c-PrCOMe
    CALL CalcH("2-hexanone",                                  "591-78-6", -51.8) ! n-BuCOMe
    CALL CalcH("3,3-dimethyl-2-butanone",                      "75-97-8", -49.9) ! t-BuCOMe
    CALL CalcH("methyl butanoate",                            "623-42-7", -48.1) ! n-PrCOOMe
    CALL CalcH("2-methyl-propanoic acid methyl ester",        "547-63-7", -47.5) ! i-PrCOOMe
    CALL CalcH("methyl pentanoate",                           "624-24-8", -51.6) ! n-BuCOOMe
    CALL CalcH("propanoic acid, 2,2-dimethyl-, methyl ester", "598-98-1", -49.6) ! t-BuCOOMe
    CALL CalcH("2-butanone",                                   "78-93-3", -45.7) ! EtCOMe
    CALL CalcH("3-pentanone",                                  "96-22-0", -49.5) ! EtCOEt
    CALL CalcH("2,4-dimethyl-3-pentanone",                    "565-80-0", -53.0) ! i-PrCO-i-Pr
    CALL CalcH("dicyclopropylmethanone",                     "1121-37-5", -60.4) ! c-PrCO-c-Pr

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, DeltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: DeltaH ! [kJ/mol]

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "T"
      CALL MakeNote("DeltaHsolv")
      CALL Output(DUMMY, -1E3*DeltaH/Rgas)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2220

  !---------------------------------------------------------------------------

  SUBROUTINE ref2222 ! KHpc [mmHg/M]
    IMPLICIT NONE

    ref = "2222"

    ! Table 3:
    CALL CalcH("fluoromethane", "593-53-3",  59.096, 17.781, 3265.7)
    CALL CalcH("chloromethane",  "74-87-3",  71.005, 21.656, 4043.9)
    CALL CalcH("bromomethane",   "74-83-9",  73.022, 22.261, 4254.8)
    CALL CalcH("iodomethane",    "74-88-4", 133.252, 42.967, 6955.2)
    CALL CalcH("methanol",       "67-56-1",  55.791, 16.163, 4552.6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a, b, c)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: a, b, c
      REAL :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      KHpc    = EXP(LOG(10.)*a-b*LOG(T0)-c*LOG(10.)/T0)
      mindHR = b*T0 - c*LOG(10.)
      CALL Output(1./(dm3*mmHg*KHpc), -mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2222

  !---------------------------------------------------------------------------

  SUBROUTINE ref2224 ! only temperature dependence
    IMPLICIT NONE

    ref = "2224"

    ! Table 2:
    CALL CalcH("benzene",        "71-43-2", -31.77) ! C6H6
    CALL CalcH("benzene-d6",   "1076-43-3", -31.75) ! C6D6
    CALL CalcH("methylbenzene", "108-88-3", -36.26) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",  "100-41-4", -40.24) ! C6H5C2H5
    CALL CalcH("propylbenzene", "103-65-1", -43.9)  ! C6H5C3H7
    CALL CalcH("cyclohexane",   "110-82-7", -33.2)  ! C6H{12}
    CALL CalcH("pentane",       "109-66-0", -24.7)  ! C5H{12}
    CALL CalcH("hexane",        "110-54-3", -31.6)  ! C6H{14}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, DeltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: DeltaH ! [kJ/mol]

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "T"
      CALL MakeNote("DeltaHsolv")
      CALL Output(DUMMY, -1E3*DeltaH/Rgas)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2224

  !---------------------------------------------------------------------------

  SUBROUTINE ref2225 ! KHpx [atm]
    IMPLICIT NONE

    ref = "2225"

    chem = "tetramethylstannane" ; casrn = "594-27-4" ; type = "M" ! tetramethyltin
    Hominus = KHpx_TIMES_HcpSI/EXP(6.5E3*cal/(Rgas*T0))
    mindHR = 7.6E3*cal / Rgas
    CALL Output(Hominus, mindHR)

    chem = "3,3-diethylpentane" ; casrn = "1067-20-5" ; type = "R" ! C9H{20}
    CALL Output(DUMMY, 9.8E3*cal / Rgas)

    chem = "tetraethylstannane" ; casrn = "597-64-8" ; type = "?" ! tetraethyltin
    CALL MakeNote(TRIM(ref), &
      "\citet{190} and \citet{2226} are quoted as the source. However, "// &
      "the data cannot be found there.")
    Hominus = KHpx_TIMES_HcpSI/EXP(6.8E3*cal/(Rgas*T0))
    mindHR = 12.1E3*cal / Rgas
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2225

  !---------------------------------------------------------------------------

  SUBROUTINE ref2226 ! KHpx [atm]
    IMPLICIT NONE

    ref = "2226"

    ! Table VII:
    CALL CalcH("2-methylpropane",       "75-28-5", "?", 6.60) ! HC(CH3)3 isobutane
    CALL CalcH("cyclopentane",         "287-92-3", "?", 5.48) ! C5H{10}
    CALL CalcH("cyclohexane",          "110-82-7", "?", 5.50) ! C6H{12}
    CALL CalcH("dimethylpropane",      "463-82-1", "?", 6.78) ! C(CH3)4 neopentane
    CALL CalcH("tetramethylstannane",  "594-27-4", "?", 6.36) ! tetramethyltin
    CALL CalcH("3,3-diethylpentane",  "1067-20-5", "?", 6.50) ! C9H{20}
    CALL CalcH("tetraethylstannane",   "597-64-8", "?", 6.39) ! tetraethyltin
    CALL CalcH("tetraethyllead",        "78-00-2", "?", 6.32) ! C8H{20}Pb

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, DeltaGs)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: DeltaGs ! [kcal/mol]

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      CALL Output(KHpx_TIMES_HcpSI/EXP(DeltaGs*1E3*cal/(Rgas*T0)))

    END SUBROUTINE CalcH

  END SUBROUTINE ref2226

  !---------------------------------------------------------------------------

  SUBROUTINE ref2229 ! KHcc [1]
    IMPLICIT NONE

    ref = "2229"
    type = "M"

    ! add a fourth, optional parameter if value is an upper limit of KHcc
    CALL CalcH("thiobencarb",             "28249-77-6", 8.3e-5)
    CALL CalcH("trichloronitromethane",      "76-06-2", 8.4e-2)
    CALL CalcH("chlorothalonil",           "1897-45-6", 8.0e-6)
    CALL CalcH("chlornitrofen",            "1836-77-7", 5.0e-5, 1)
    CALL CalcH("pentachloronitrobenzene",    "82-68-8", 1.5e-4)
    CALL CalcH("trichlorfon",                "52-68-6", 5.0e-7, 1)
    CALL CalcH("dichlorvos",                 "62-73-7", 5.0e-3)
    CALL CalcH("tetrachlorophthalide",    "27355-22-2", 2.2e-5)
    CALL CalcH("DCIP",                      "108-60-1", 9.6e-4)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc, upperlimit)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc
      INTEGER,          INTENT(IN), OPTIONAL :: upperlimit

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      IF (PRESENT(upperlimit)) THEN
        ! upper limit of KHcc is lower limit of Hcp:
        CALL Output(Hominus, limit=">")
      ELSE
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2229

  !---------------------------------------------------------------------------

  SUBROUTINE ref2230 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "2230"
    type = "M"

    chem = "methanoic acid" ; casrn = "64-18-6" ! HCOOH formic acid
    CALL Output(5530.*Hbp_TO_HcpSI,5634.)

    chem = "ethanoic acid" ; casrn = "64-19-7" ! CH3COOH acetic acid
    CALL Output(5500.*Hbp_TO_HcpSI,8322.)

    chem = "propanoic acid" ; casrn = "79-09-4" ! C2H5COOH propionic acid
    CALL Output(5640.*Hbp_TO_HcpSI)

    chem = "butanoic acid" ; casrn = "107-92-6" ! C3H7COOH butyric acid
    CALL Output(4550.*Hbp_TO_HcpSI)

    chem = "2-methylpropanoic acid" ; casrn = "79-31-2" ! (CH3)2CHCOOH
    CALL Output(1130.*Hbp_TO_HcpSI)

    chem = "pentanoic acid" ; casrn = "109-52-4" ! C4H9COOH
    CALL Output(2120.*Hbp_TO_HcpSI,6879.)

    chem = "3-methylbutanoic acid" ; casrn = "503-74-2" ! (CH3)2CHCH2COOH
    CALL Output(1200.*Hbp_TO_HcpSI)

    chem = "2,2-dimethylpropanoic acid" ; casrn = "75-98-9" ! (CH3)3CCOOH pivalic acid
    CALL Output(360.*Hbp_TO_HcpSI)

    chem = "hexanoic acid" ; casrn = "142-62-1" ! C5H{11}COOH caproic acid
    CALL Output(1320.*Hbp_TO_HcpSI,5908.)

    chem = "2-oxopropanoic acid" ; casrn = "127-17-3" ! CH3COCOOH pyruvic acid
    CALL Output(310000.*Hbp_TO_HcpSI,5224.)

  END SUBROUTINE ref2230

  !---------------------------------------------------------------------------

  SUBROUTINE ref2231 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "2231"
    type = "V"

    chem = "pentanoic acid" ; casrn = "109-52-4" ! C4H9COOH
    CALL Output(1640.*Hbp_TO_HcpSI)

    chem = "hexanoic acid" ; casrn = "142-62-1" ! C5H{11}COOH caproic acid
    CALL Output(1160.*Hbp_TO_HcpSI)

    chem = "heptanoic acid" ; casrn = "111-14-8" ! C7H{14}O2
    CALL Output(980.*Hbp_TO_HcpSI)

    chem = "octanoic acid" ; casrn = "124-07-2" ! C8H{16}O2 caprylic acid
    CALL Output(770.*Hbp_TO_HcpSI)

    chem = "nonanoic acid" ; casrn = "112-05-0"
    CALL Output(383.*Hbp_TO_HcpSI)

    chem = "ethanedioic acid" ; casrn = "144-62-7" ! HOOCCOOH oxalic acid
    CALL Output(3.2e6*Hbp_TO_HcpSI,7285.)

    chem = "glycine" ; casrn = "56-40-6" ! C2H5NO2
    CALL Output(1.2e13*Hbp_TO_HcpSI,15947.)

    chem = "alanine" ; casrn = "302-72-7" ! C3H7NO2
    CALL Output(3.6e12*Hbp_TO_HcpSI,16463.)

  END SUBROUTINE ref2231

  !---------------------------------------------------------------------------

  SUBROUTINE ref2232 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2232"

    CALL CalcH("pyridine",                   "110-86-1", "M", 1.2E-5)
    CALL CalcH("2,6-dimethylpyridine",       "108-48-5", "M", 1.5E-5)
    CALL CalcH("2,4-dimethylpyridine",       "108-47-4", "M", 1.0E-5)
    CALL CalcH("pyrrole",                    "109-97-7", "M", 1.8E-5)
    CALL CalcH("propane nitrile",            "107-12-0", "M", 5.3E-5)
    CALL CalcH("butane nitrile",             "109-74-0", "M", 6.9E-5)
    CALL CalcH("1-hydroxy-2,6-diMe-benzene", "576-26-1", "M", 0.76E-5)
    CALL CalcH("butanone",                    "78-93-3", "M", 7.0E-5)
    CALL CalcH("2-pentanone",                "107-87-9", "M", 11.E-5)
    CALL CalcH("cyclopentanone",             "120-92-3", "M", 1.2E-5)
    CALL CalcH("cyclohexanone",              "108-94-1", "M", 1.2E-5)

    ! same citation as cited by ref0636:
    !CALL CalcH("butanone",                    "78-93-3", "C", 4.3E-5)
    !CALL CalcH("2-pentanone",                "107-87-9", "C", 5.8E-5)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = 1. / (atm*KHpc)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2232

  !---------------------------------------------------------------------------

  SUBROUTINE ref2233 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2233"

    CALL CalcH("benzene",              "71-43-2", "?", 4.39E-3)
    CALL CalcH("methylbenzene",       "108-88-3", "?", 5.18E-3)
    CALL CalcH("1,2-dichloropropane",  "78-87-5", "?", 2.07E-3)
    CALL CalcH("1,2-dibromoethane",   "106-93-4", "?", 6.29E-4)
    CALL CalcH("chlorobenzene",       "108-90-7", "?", 2.61E-3)
    CALL CalcH("tetrachloromethane",   "56-23-5", "?", 2.27E-2)
    CALL CalcH("2-pentanone",         "107-87-9", "?", 3.16E-5)
    CALL CalcH("2-heptanone",         "110-43-0", "?", 9.00E-5)
    CALL CalcH("1-pentanol",           "71-41-0", "?", 1.03E-5)
    CALL CalcH("2-methyl-1-propanol",  "78-83-1", "?", 1.03E-5)
    CALL CalcH("1-butanol",            "71-36-3", "?", 5.57E-6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = 1. / (atm*KHpc)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2233

  !---------------------------------------------------------------------------

  SUBROUTINE ref2234 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2234"

    CALL CalcH("propanone",             "67-64-1", "V", 4.05E-5)
    CALL CalcH("butanone",              "78-93-3", "V", 6.11E-5)
    CALL CalcH("2-pentanone",          "107-87-9", "V", 3.78E-5)
    CALL CalcH("3-pentanone",           "96-22-0", "V", 3.57E-5)
    CALL CalcH("4-methyl-2-pentanone", "108-10-1", "V", 6.86E-5)
    CALL CalcH("2-heptanone",          "110-43-0", "V", 5.64E-5)
    CALL CalcH("2-octanone",           "111-13-7", "V", 18.1E-5)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = 1. / (atm*KHpc)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2234

  !---------------------------------------------------------------------------

  SUBROUTINE ref2235 ! KHcc [1]
    IMPLICIT NONE

    ref = "2235"
    type = "C"

    ! column 1:
    CALL CalcH("propenal",                          "107-02-8", 2.8E-03) ! CH2CHCHO acrolein
    CALL CalcH("octachloro-4,7-methanohydroindane",  "57-74-9", 3.9E-03) ! C{10}H6Cl8 chlordane
    CALL CalcH("DDE",                                "72-55-9", 9.0E-04) ! C{14}H8Cl4 $p$,$p$'-dde
    CALL CalcH("dieldrin",                           "60-57-1", 3.0E-04) ! C{12}H8OCl6
    CALL CalcH("endrin",                             "72-20-8", 1.7E-05) ! C{12}H8Cl6O
    CALL CalcH("heptachlorepoxide",                "1024-57-3", 3.2E-05) ! C{10}H5Cl7O
    CALL CalcH("$\beta$-lindane",                   "319-85-7", 6.0E-04) ! C6H6Cl6 $\beta$-lindane
    ! aroclor is a mixture, not a pure substance:
    !CALL CalcH("aroclor1016",                     "12674-11-2", 8.0E-01) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1232",                     "11141-16-5", 2.1E+00) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1248",                     "12672-29-6", 1.1E-01) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1260",                     "11096-82-5", 2.9E-01) ! C{12}HxCl{(10-x)}
    CALL CalcH("chloromethane",                      "74-87-3", 1.6E+01) ! CH3Cl methyl chloride
    CALL CalcH("trichloromethane",                   "67-66-3", 1.2E-01) ! CHCl3 chloroform
    CALL CalcH("chloroethane",                       "75-00-3", 6.1E-01) ! C2H5Cl
    CALL CalcH("1,2-dichloroethane",                "107-06-2", 3.8E-02) ! CH2ClCH2Cl
    CALL CalcH("1,1,2-trichloroethane",              "79-00-5", 3.1E-01) ! CHCl2CH2Cl
    CALL CalcH("hexachloroethane",                   "67-72-1", 4.1E-01) ! C2Cl6
    CALL CalcH("1,1-dichloroethene",                 "75-35-4", 1.8E-01) ! CH2CCl2
    CALL CalcH("trichloroethene",                    "79-01-6", 3.8E-01) ! C2HCl3 trichloroethylene
    CALL CalcH("1,2-dichloropropane",                "78-87-5", 1.2E-01) ! C3H6Cl2
    CALL CalcH("hexachlorobutadiene",                "87-68-3", 4.3E-01) ! CCl2CClCClCCl2
    CALL CalcH("bromoethane",                        "74-96-4", 4.4E+00) ! C2H5Br
    CALL CalcH("dichlorodifluoromethane",            "75-71-8", 6.3E+01) ! CF2Cl2 R12
    CALL CalcH("bis(chloromethyl)ether",            "542-88-1", 8.6E-02) ! C2H4Cl2O
    CALL CalcH("bis-(2-chloroisopropyl)-ether",     "108-60-1", 4.7E-02) ! C6H{12}Cl2O DCIP
    CALL CalcH("1-chloro-4-phenoxy-benzene",       "7005-72-3", 1.0E-02) ! C{12}H9ClO 4-chlorophenyl phenyl ether
    CALL CalcH("bis-(2-chloroethoxy)-methane",      "111-91-1", 1.1E-05) ! C5H{10}Cl2O2
    CALL CalcH("benzene",                            "71-43-2", 2.3E-01) ! C6H6
    CALL CalcH("1,2-dichlorobenzene",                "95-50-1", 1.5E-01) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",               "106-46-7", 9.9E-02) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("hexachlorobenzene",                 "118-74-1", 7.0E-02) ! C6Cl6
    CALL CalcH("nitrobenzene",                       "98-95-3", 5.4E-04) ! C6H5NO2
    CALL CalcH("1-methyl-2,4-dinitrobenzene",       "121-14-2", 1.3E-02) ! C7H6N2O4 2,4-dinitrotoluene
    CALL CalcH("hydroxybenzene",                    "108-95-2", 1.9E-05) ! C6H5OH phenol
    CALL CalcH("2,4-dichlorophenol",                "120-83-2", 2.3E-04) ! C6H4Cl2O
    CALL CalcH("hydroxypentachlorobenzene",          "87-86-5", 1.2E-04) ! C6HCl5O pentachlorophenol
    CALL CalcH("4-nitrophenol",                     "100-02-7", 2.6E-04) ! HOC6H4(NO2)
    CALL CalcH("1-hydroxy-2,4-dimethylbenzene",     "105-67-9", 7.5E-04) ! C8H{10}O 2,4-xylenol; 2,4-dimethylphenol
    CALL CalcH("dimethyl phthalate",                "131-11-3", 8.0E-06) ! C{10}H{10}O4
    CALL CalcH("dibutyl phthalate",                  "84-74-2", 1.2E-05) ! C{16}H{22}O4
    CALL CalcH("di-(2-ethylhexyl)-phthalate",       "117-81-7", 1.2E-05) ! C{24}H{38}O4
    CALL CalcH("acenaphthene",                       "83-32-9", 1.0E-02) ! C{12}H{10}
    CALL CalcH("2,3-benzindene",                     "86-73-7", 4.8E-03) ! C{13}H{10} fluorene
    CALL CalcH("anthracene",                        "120-12-7", 1.1E-02) ! C{14}H{10}
    CALL CalcH("phenanthrene",                       "85-01-8", 1.6E-03) ! C{14}H{10}
    CALL CalcH("chrysene",                          "218-01-9", 8.8E-02) !  C{18}H{12}
    CALL CalcH("2-propenenitrile",                  "107-13-1", 3.8E-03) ! C3H3N acrylonitrile
    ! column 2:
    CALL CalcH("aldrin",                            "309-00-2", 4.9E-04) ! C{12}H8Cl6
    CALL CalcH("DDD",                                "72-54-8", 0.5E+00) ! C{14}H{10}Cl4 $p$,$p$'-ddd
    CALL CalcH("DDT",                                "50-29-3", 2.0E-03) ! C{14}H9Cl5 1,1,1-trichloro-2,2-bis-(4-chlorophenyl)-ethane
    CALL CalcH("heptachlor",                         "76-44-8", 6.2E-02) ! C{10}H5Cl7
    ! toxaphene is a mixture, not a pure substance:
    ! CALL CalcH("toxaphene",                        "8001-35-2", 2.1E-01) ! C{10}H{10}Cl8
    ! aroclor is a mixture, not a pure substance:
    !CALL CalcH("aroclor1221",                     "11104-28-2", 1.3E-02) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1242",                     "53469-21-9", 5.5E-02) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1254",                     "11097-69-1", 1.1E-01) ! C{12}HxCl{(10-x)}
    CALL CalcH("2-chloronaphthalene",                "91-58-7", 1.3E-02) ! C{10}H7Cl
    CALL CalcH("dichloromethane",                    "75-09-2", 8.5E-02) ! CH2Cl2 methylene chloride
    CALL CalcH("tetrachloromethane",                 "56-23-5", 9.0E-01) ! CCl4 carbontetrachloride
    CALL CalcH("1,1-dichloroethane",                 "75-34-3", 1.7E-01) ! CHCl2CH3
    CALL CalcH("1,1,1-trichloroethane",              "71-55-6", 1.3E+00) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1,2,2-tetrachloroethane",          "79-34-5", 1.6E-02) ! CHCl2CHCl2
    CALL CalcH("chloroethene",                       "75-01-4", 6.2E-01) ! CH2CHCl vinyl chloride
    CALL CalcH("(E)-1,2-dichloroethene",            "156-60-5", 2.7E-01) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("tetrachloroethene",                 "127-18-4", 6.4E-01) ! C2Cl4 tetrachloroethylene
    CALL CalcH("1,3-dichloropropene",               "542-75-6", 5.0E-02) ! C3H4Cl2
    CALL CalcH("hexachlorocyclopentadiene",          "77-47-4", 1.5E+00) ! C5Cl6
    CALL CalcH("tribromomethane",                    "75-25-2", 2.4E-02) ! CHBr3 bromoform
    CALL CalcH("trichlorofluoromethane",             "75-69-4", 2.4E+00) ! CFCl3 R11
    CALL CalcH("1,5-dichloro-3-oxapentane",         "111-44-4", 4.7E-05) ! C4H8Cl2O bis-(2-chloroethyl)-ether
    CALL CalcH("(2-chloroethoxy)-ethene",           "110-75-8", 1.0E-02) ! C4H7ClO 2-chloroethylvinylether
    CALL CalcH("chlorobenzene",                     "108-90-7", 1.5E-01) ! C6H5Cl
    CALL CalcH("1,3-dichlorobenzene",               "541-73-1", 1.1E-01) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,2,4-trichlorobenzene",            "120-82-1", 9.6E-01) ! C6H3Cl3
    CALL CalcH("ethylbenzene",                      "100-41-4", 2.6E-01) ! C6H5C2H5
    CALL CalcH("methylbenzene",                     "108-88-3", 2.8E-01) ! C6H5CH3 toluene
    CALL CalcH("2-methyl-1,3-dinitrobenzene",       "606-20-2", 1.3E-02) ! C7H6N2O4 2,6-dinitrotoluene
    CALL CalcH("2-hydroxychlorobenzene",             "95-57-8", 1.9E-04) ! C6H5ClO $o$-chlorophenol
    CALL CalcH("2,4,6-trichlorophenol",              "88-06-2", 1.7E-04) ! C6H3Cl3O
    CALL CalcH("2-nitrophenol",                      "88-75-5", 3.2E-04) ! HOC6H4(NO2)
    CALL CalcH("2,4-dinitrophenol",                  "51-28-5", 2.7E-08) ! C6H4N2O5
    CALL CalcH("1-chloro-2-methyl-4-hydroxybenzene", "59-50-7", 1.0E-04) ! C7H7ClO 4-chloro-3-methylphenol
    CALL CalcH("diethyl phthalate",                  "84-66-2", 1.9E-03) ! C{12}H{14}O4
    CALL CalcH("dioctyl phthalate",                 "117-84-0", 1.2E-05) ! C{24}H{38}O4
    CALL CalcH("butyl benzyl phthalate",             "85-68-7", 4.2E-05) ! C{19}H{20}O4
    CALL CalcH("acenaphthylene",                    "208-96-8", 4.8E-03) ! C{12}H8
    CALL CalcH("naphthalene",                        "91-20-3", 2.0E-02) ! C{10}H8
    CALL CalcH("fluoranthene",                      "206-44-0", 4.0E-04) ! C{16}H{10}
    CALL CalcH("benz[a]anthracene",                  "56-55-3", 4.1E-05) ! C{18}H{12}
    CALL CalcH("pyrene",                            "129-00-0", 2.8E-01) ! C{16}H{10}
    CALL CalcH("benzo[a]pyrene",                     "50-32-8", 4.9E-01) ! C{20}H{12}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2235

  !---------------------------------------------------------------------------

  SUBROUTINE ref2236 ! KHpc [mmHg/M]
    IMPLICIT NONE

    ref = "2236"

    !CALL CalcH("naphthalene", "91-20-3", "C", 365.) ! from ref490
    CALL CalcH("anthracene",                  "120-12-7", "C",  75.)
    CALL CalcH("1-methyl-2,4-dinitrobenzene", "121-14-2", "V",   0.12)
    CALL CalcH("1,3-dinitrobenzene",           "99-65-0", "V",   0.19)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      CALL Output(1./(dm3*mmHg*KHpc))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2236

  !---------------------------------------------------------------------------

  ! ref2237 preprint from 1999 not supposed to be cited

  !---------------------------------------------------------------------------

  SUBROUTINE ref2240 ! Hcp [M/atm]
    IMPLICIT NONE
    ref = "2240"
    type = "M"

    CALL CalcH("trichlorofluoromethane",  "75-69-4", -134.1536, 203.2156, 56.2320) ! CFCl3 R11
    CALL CalcH("dichlorodifluoromethane", "75-71-8", -122.3246, 182.5306, 50.5898) ! CF2Cl2 R12

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a1, a2, a3)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: a1, a2, a3
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = EXP(a1+a2/(T0/100.)+a3*LOG(T0/100.)) * Hcp_TO_HcpSI
      mindHR = 100.*a2 - a3 * T0
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2240

  !---------------------------------------------------------------------------

  SUBROUTINE ref2241 ! KHcc [1]
    IMPLICIT NONE

    ref = "2241"
    chem = "S-ethyl dipropylthiocarbamate" ; casrn = "759-94-4" ! C9H{19}NOS

    type = "M"
    Hominus = KHcc_TIMES_HcpSI_atT0 / 0.0107
    mindHR = 37.12E3 / Rgas + T0 ! see ref958, eqn (34) why T0 is added
    CALL Output(Hominus, mindHR)

    type = "V"
    Hominus = KHcc_TIMES_HcpSI_atT0 / 0.000950
    CALL Output(Hominus)

  END SUBROUTINE ref2241

  !---------------------------------------------------------------------------

  SUBROUTINE ref2243 ! KHcc [1]
    IMPLICIT NONE

    ref = "2243"

    CALL CalcH("tribromomethane",         "75-25-2", 4.729, 1905.) ! CHBr3 bromoform
    CALL CalcH("hexachloroethane",        "67-72-1", 6.982, 2320.) ! C2Cl6
    CALL CalcH("trichloromethane",        "67-66-3", 4.990, 1729.) ! CHCl3 chloroform
    CALL CalcH("trichloroethene",         "79-01-6", 6.026, 1909.) ! C2HCl3 trichloroethylene
    CALL CalcH("1,1,1-trichloroethane",   "71-55-6", 5.327, 1636.) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("tetrachloroethene",      "127-18-4", 5.920, 1802.) ! C2Cl4 tetrachloroethylene
    CALL CalcH("tetrachloromethane",      "56-23-5", 5.853, 1718.) ! CCl4 carbontetrachloride
    CALL CalcH("dichlorodifluoromethane", "75-71-8", 5.811, 1399.) ! CF2Cl2 R12

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      Hominus = KHcc_TIMES_HcpSI_atT0 / (10.**(A-B/T0))
      mindHR = B * LOG(10.) + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus,mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2243

  !---------------------------------------------------------------------------

  SUBROUTINE ref2269 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2269"

    ! EPICS are not used here because they were already published in ref0601:
    ! CALL CalcH("tetrachloroethene",      "127-18-4", 13.12,    5119.) ! C2Cl4 tetrachloroethylene
    ! CALL CalcH("1,1,1-trichloroethane",   "71-55-6", 10.21,    4262.) ! CH3CCl3 methylchloroform; MCF
    ! CALL CalcH("trichloroethene",         "79-01-6", 11.94,    4929.) ! C2HCl3 trichloroethylene
    ! CALL CalcH("trichloromethane",        "67-66-3",  8.553,   4180.) ! CHCl3 chloroform
    ! CALL CalcH("dichloromethane",         "75-09-2",  8.200,   4191.) ! CH2Cl2 methylene chloride

    ! batch air stripping:
    CALL CalcH("tetrachloroethene",      "127-18-4", 11.32,    4622.) ! C2Cl4 tetrachloroethylene
    CALL CalcH("1,1,1-trichloroethane",   "71-55-6",  9.975,   4186.) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("trichloroethene",         "79-01-6",  9.703,   4308.) ! C2HCl3 trichloroethylene
    CALL CalcH("trichloromethane",        "67-66-3",  8.956,   4322.) ! CHCl3 chloroform
    CALL CalcH("dichloromethane",         "75-09-2",  9.035,   4472.) ! CH2Cl2 methylene chloride
    CALL CalcH("1,2-dichlorobenzene",     "95-50-1", 15.96,    6665.) ! C6H4Cl2 $o$-dichlorobenzene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      mindHR = B
      Hominus    = 1./(atm*EXP(A - mindHR/T0))
      CALL Output(Hominus,mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2269

  !---------------------------------------------------------------------------

  SUBROUTINE ref2270 ! KHcc [1]
    IMPLICIT NONE

    ref = "2270"

    CALL CalcH("benzene",                 "71-43-2", 11.931, 3974.) ! C6H6
    CALL CalcH("tetrachloromethane",      "56-23-5", 18.071, 5282.) ! CCl4 carbontetrachloride
    CALL CalcH("chlorobenzene",          "108-90-7", 12.858, 4370.) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",     "95-50-1", 13.755, 4847.) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,2-dichloroethane",     "107-06-2", 11.383, 4218.) ! CH2ClCH2Cl
    CALL CalcH("1,1-dichloroethene",      "75-35-4", 14.347, 4230.) ! CH2CCl2
    CALL CalcH("(Z)-1,2-dichloroethene", "156-59-2", 11.092, 3853.) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("(E)-1,2-dichloroethene", "156-60-5", 14.525, 4551.) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("1,2-dichloropropane",     "78-87-5",  9.705, 3494.) ! C3H6Cl2
    CALL CalcH("ethylbenzene",           "100-41-4", 16.333, 5168.) ! C6H5C2H5
    CALL CalcH("ethenylbenzene",         "100-42-5", 12.960, 4455.) ! C8H8 styrene
    CALL CalcH("tetrachloroethene",      "127-18-4", 16.352, 5003.) ! C2Cl4 tetrachloroethylene
    CALL CalcH("methylbenzene",          "108-88-3", 14.529, 4696.) ! C6H5CH3 toluene
    CALL CalcH("1,1,1-trichloroethane",   "71-55-6", 16.229, 4910.) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("trichloroethene",         "79-01-6", 15.604, 4935.) ! C2HCl3 trichloroethylene
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6", 16.036, 5266.) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",    "108-38-3", 17.979, 5722.) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",    "106-42-3", 15.805, 5037.) ! C6H4(CH3)2 $p$-xylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      Hominus = KHcc_TIMES_HcpSI_atT0 / (EXP(A-B/T0))
      mindHR = B + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus,mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2270

  !---------------------------------------------------------------------------

  SUBROUTINE ref2271 ! KHcc [1]
    IMPLICIT NONE

    ref = "2271"

    CALL CalcH("bromodichloromethane",    "75-27-4", 14.262, 4894.)
    CALL CalcH("benzene",                 "71-43-2", 15.670, 5007.)
    CALL CalcH("chlorobenzene",          "108-90-7", 11.250, 3945.)
    CALL CalcH("(Z)-1,2-dichloroethene", "156-59-2", 10.934, 3761.)
    CALL CalcH("trichloromethane",        "67-66-3", 16.499, 5310.)
    CALL CalcH("dibromochloromethane",   "124-48-1", 12.714, 4701.)
    CALL CalcH("1,1-dichloroethane",      "75-34-3", 14.270, 4599.)
    CALL CalcH("ethylbenzene",           "100-41-4", 16.143, 5180.)
    CALL CalcH("1,1,1-trichloroethane",   "71-55-6", 22.868, 6694.)
    CALL CalcH("(E)-1,2-dichloroethene", "156-60-5", 16.483, 5079.)
    CALL CalcH("methylbenzene",          "108-88-3", 14.014, 4574.)
    CALL CalcH("trichloroethene",         "79-01-6", 14.451, 4489.)
    CALL CalcH("tetrachloroethene",      "127-18-4", 16.007, 4875.)
    CALL CalcH("1,1-dichloroethene",      "75-35-4", 22.085, 6269.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      Hominus = KHcc_TIMES_HcpSI_atT0 / (EXP(A-B/T0))
      mindHR = B + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus,mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2271

  !---------------------------------------------------------------------------

  SUBROUTINE ref2279 ! KHpx [bar]
    IMPLICIT NONE
    REAL, PARAMETER :: A = 55.7339
    REAL, PARAMETER :: B = -95.4036
    REAL, PARAMETER :: C = -16.0477

    ref = "2279"
    type = "L"
    chem = "mercury" ; casrn = "7439-97-6" ! Hg

    ! ln(H) = A + B/(T/100) + C*ln(T/100)
    Hominus    = cH2O / (EXP(A + B/(T0/100.) + C*LOG(T0/100.)) * bar)
    ! analytical derivative: (see also gnuplot/ref2279.gnu)
    ! mindHR = 100*B - C*T0
    mindHR = - (100.*B - C*T0)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2279

  !---------------------------------------------------------------------------

  SUBROUTINE ref2280 ! KHpx [atm]
    IMPLICIT NONE
    REAL :: Tdep

    ref = "2280"
    type = "M"
    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    Tdep = 1078.
    Hominus = KHpx_TIMES_HcpSI / (10.**(6.250-Tdep/T0))
    mindHR = LOG(10.) * Tdep
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2280

  !---------------------------------------------------------------------------

  SUBROUTINE ref2285 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2285"

    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    type = "C"
    CALL Output(1.2E-1*Hcp_TO_HcpSI)

    chem = "mercury(II) oxide" ; casrn = "21908-53-2" ! HgO
    type = "?"
    CALL MakeNote("2285wrongrefHgO", &
      TRIM(citet())//" refer to \citet{2286} "// &
      "as the source but a different value is listed there.")
    CALL Output(3.2E6*Hcp_TO_HcpSI)

    chem = "mercury dichloride" ; casrn = "7487-94-7" ! HgCl2
    type = "C"
    CALL Output(2.4E7*Hcp_TO_HcpSI)

    chem = "hydroxymethylmercury" ; casrn = "1184-57-2" ! Hg(CH3)OH
    type = "?"
    CALL MakeNote("2285wrongrefHgCH3OH", &
      TRIM(citet())//" refer to \citet{2286} "// &
      "as the source but this value cannot be found there.")
    CALL Output(1.5E5*Hcp_TO_HcpSI)

  END SUBROUTINE ref2285

  !---------------------------------------------------------------------------

  SUBROUTINE ref2286 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2286"
    type = "?"

    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" give the invalid unit "// &
      "``\unit{mol\,L^{-1}\,ppm^{-1}}''. Here, it is assumed that "// &
      "``ppm'' is used as a synonym for ``$10^{-6}$ \unit{atm}''.")
    CALL Output(0.1361E-6*1E6*Hcp_TO_HcpSI)

    chem = "mercury(II) oxide" ; casrn = "21908-53-2" ! HgO
    CALL MakeNote(TRIM(ref))
    CALL Output(0.1408E1*1E6*Hcp_TO_HcpSI)

  END SUBROUTINE ref2286

  !---------------------------------------------------------------------------

  SUBROUTINE ref2288 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2288"
    type = "M"

    ndata = 9
    ALLOCATE(temp(ndata), Harray(ndata))
    chem = "chloromethylmercury" ; casrn = "115-09-3" ! CH3HgCl
    temp = (/ 25., 25., 25., 16., 16., 15., 15., 15., 15. /) + CtoK
    Harray = (/ 2.21E3, 2.40E3, 1.95E3, 2.87E3, 2.87E3, 2.58E3, 2.94E3, &
      2.24E3, 2.73E3 /) * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref), &
      "The value from their experiment 7 at 10~\unit{\degree C} is not "// &
      "used in the determination of the temperature dependence because "// &
      "of very different ionic strengths and concentrations for that "// &
      "experiment.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    ! using dimensionless data in Tab. 2 yields the same results:
    ! ndata = 10
    ! ALLOCATE(temp(ndata), Harray(ndata))
    ! chem = "chloromethylmercury" ; casrn = "115-09-3" ! CH3HgCl
    ! temp = (/ 25., 25., 25., 16., 16., 15., 15., 15., 15., 10. /) + CtoK
    ! Harray = (/ 1.85E-5, 1.70E-5, 2.10E-5, 1.47E-5, 1.47E-5, 1.64E-5, &
    !   1.44E-5, 1.89E-5, 1.55E-5, 0.94E-5 /)
    ! Harray = KHcc_TO_HcpSI(Harray,temp)
    ! CALL HTdep(temp, Harray, Hominus, mindHR)
    ! CALL Output(Hominus, mindHR, r2)
    ! DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2288

  !---------------------------------------------------------------------------

  SUBROUTINE ref2289 ! KHcc [1]
    IMPLICIT NONE

    ref = "2289"
    type = "M"

    chem = "dimethylmercury" ; casrn = "593-74-8" ! (CH3)2Hg
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0., 25. /) + CtoK
    Harray = (/ 0.15, 0.31 /)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "chloromethylmercury" ; casrn = "115-09-3" ! CH3HgCl
    Hominus = KHcc_TO_HcpSI(2.8E-5, 295.)
    CALL MakeNoteOtherTemp("295")
    CALL Output(Hominus)

  END SUBROUTINE ref2289

  !---------------------------------------------------------------------------

  SUBROUTINE ref2290 ! KHcc [1]
    IMPLICIT NONE

    ref = "2290"
    type = "C"

    ! data for CH3HgCl is just a citation of ref2288:
    ! ndata = 3
    ! ALLOCATE(temp(ndata), Harray(ndata))
    ! temp = (/ 25., 15., 10. /) + CtoK
    ! chem = "chloromethylmercury" ; casrn = "115-09-3" ! CH3HgCl
    ! Harray = (/ 1.9E-5, 1.6E-5, 0.9E-5 /)
    ! Harray = KHcc_TO_HcpSI(Harray,temp)
    ! CALL HTdep(temp, Harray, Hominus, mindHR)
    ! CALL Output(Hominus, mindHR, r2)
    ! DEALLOCATE(temp, Harray)

    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 25., 10. /) + CtoK

    chem = "mercury dihydroxide" ; casrn = "_CAS-84" ! Hg(OH)2
    Harray = (/ 3.2E-6, 1.6E-6 /)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "mercury dichloride" ; casrn = "7487-94-7" ! HgCl2
    Harray = (/ 2.9E-8, 1.2E-8 /)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2290

  !---------------------------------------------------------------------------

  SUBROUTINE ref2291 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "2291"
    type = "M"
    chem = "hydroxybenzene" ; casrn = "108-95-2" ! C6H5OH phenol
    mindHR = 6120.
    Hominus    = cH2O / ( 2.69E9*EXP(-mindHR/T0) * 1.E3)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2291

  !---------------------------------------------------------------------------

  SUBROUTINE ref2292 ! KHcc [1]
    IMPLICIT NONE

    ref = "2292"
    chem = "2,4,6-trichlorophenol" ; casrn = "88-06-2" ! C6H3Cl3O
    type = "M"
    Hominus = KHcc_TO_HcpSI( 2.1E-4 / (1.+10**(4.-6.06)), 293.)
    CALL MakeNote("2292pH4", "Value at pH = 4.")
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hominus)

  END SUBROUTINE ref2292

  !---------------------------------------------------------------------------

  SUBROUTINE ref2301 ! Hxp [1/atm] and Hcc [1] = Ostwald coefficient
    IMPLICIT NONE
    REAL :: A0, A1, A2
    ref = "2301"
    type = "L"
    chem = "oxygen" ; casrn = "7782-44-7" ! O2

    ! ln x = -64.21517 + 83.91236 / T + 23.24323 ln(T)
    A0 = -64.21517
    A1 = 83.91236
    A2 = 23.24323
    Hominus = EXP(A0+A1/(T0/100.)+A2*LOG(T0/100.)) * Hxp_TO_HcpSI
    mindHR = 100.*A1 - A2 * T0
    CALL Output(Hominus, mindHR)

    ! activate the following lines to check that data for ln(L) are
    ! consistent with data for ln(x):
    ! ln L = -58.51653 + 84.5991  / T + 24.41285 ln(T)
    ! A0 = -58.51653
    ! A1 = 84.5991
    ! A2 = 24.41285
    ! Hominus = Hcc_TO_HcpSI(EXP(A0+A1/(T0/100.)+A2*LOG(T0/100.)),T0)
    ! mindHR = 100.*A1 - A2 * T0 + T0 ! see ref958, eqn (34) why T0 is added
    ! CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2301

  !---------------------------------------------------------------------------

  SUBROUTINE ref2302 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2302"
    type = "V"

    CALL CalcH("biphenyl",   "92-52-4", 53.5)
    CALL CalcH("PCB-1",    "2051-60-7", 70.)
    CALL CalcH("PCB-2",    "2051-61-8", 76.)
    CALL CalcH("PCB-3",    "2051-62-9", 43.)
    CALL CalcH("PCB-4",   "13029-08-8", 59.)
    CALL CalcH("PCB-6",   "25569-80-6", 40.)
    CALL CalcH("PCB-7",   "33284-50-3", 45.)
    CALL CalcH("PCB-8",   "34883-43-7", 90.)
    CALL CalcH("PCB-9",   "34883-39-1", 20.)
    CALL CalcH("PCB-15",   "2050-68-2", 17.)
    CALL CalcH("PCB-16",  "38444-78-9", 80.)
    CALL CalcH("PCB-18",  "37680-65-2", 92.)
    CALL CalcH("PCB-20",  "38444-84-7", 82.)
    CALL CalcH("PCB-29",  "15862-07-4", 24.)
    CALL CalcH("PCB-30",  "35693-92-6", 50.)
    CALL CalcH("PCB-31",  "16606-02-3", 55.)
    CALL CalcH("PCB-33",  "38444-86-9", 44.)
    CALL CalcH("PCB-37",  "38444-90-5", 84.)
    CALL CalcH("PCB-40",  "38444-93-8", 22.)
    CALL CalcH("PCB-44",  "41464-39-5", 50.)
    CALL CalcH("PCB-47",   "2437-79-8", 17.)
    CALL CalcH("PCB-49",  "41464-40-8", 20.)
    CALL CalcH("PCB-50",  "62796-65-0", 77.)
    CALL CalcH("PCB-52",  "35693-99-3", 48.)
    CALL CalcH("PCB-53",  "41464-41-9", 30.)
    CALL CalcH("PCB-66",  "32598-10-0", 84.)
    CALL CalcH("PCB-70",  "32598-11-1", 20.)
    CALL CalcH("PCB-77",  "32598-13-3", 1.7)
    CALL CalcH("PCB-82",  "52663-62-4", 20.)
    CALL CalcH("PCB-86",  "55312-69-1", 151.)
    CALL CalcH("PCB-87",  "38380-02-8", 25.)
    CALL CalcH("PCB-101", "37680-73-2", 35.)
    CALL CalcH("PCB-128", "38380-07-3", 12.)
    CALL CalcH("PCB-138", "35065-28-2", 82.)
    CALL CalcH("PCB-141", "52712-04-6", 40.)
    CALL CalcH("PCB-144", "68194-14-9", 60.)
    CALL CalcH("PCB-149", "38380-04-0", 30.)
    CALL CalcH("PCB-151", "52663-63-5", 30.)
    CALL CalcH("PCB-153", "35065-27-1", 43.)
    CALL CalcH("PCB-155", "33979-03-2", 818.)
    CALL CalcH("PCB-156", "38380-08-4", 88.)
    CALL CalcH("PCB-157", "69782-90-7", 60.)
    CALL CalcH("PCB-158", "74472-42-7", 65.)
    CALL CalcH("PCB-171", "52663-71-5", 5.4)
    CALL CalcH("PCB-202",  "2136-99-4", 37.)
    CALL CalcH("PCB-209",  "2051-24-3", 21.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2302

  !---------------------------------------------------------------------------

  SUBROUTINE ref2321 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2321"

    chem = "ethanedial" ; casrn = "107-22-2" ! OHCCHO glyoxal
    type = "M"
    CALL MakeNote("RCHOdiol")
    CALL MakeNote(TRIM(ref), &
      "Solubility in sulfate aerosol.")

    CALL Output(2.6E7*Hcp_TO_HcpSI)

  END SUBROUTINE ref2321

  !---------------------------------------------------------------------------

  ! not used because a newer version of CRC is available: ref3034

  ! SUBROUTINE ref2323 ! KHpc [kPa*m3/mol]
  !   IMPLICIT NONE
  !   REAL          :: KHpc
  !   INTEGER       :: i, refnumber, temp
  !   CHARACTER(STRLEN_VLONG) :: formula, trivial
  !   LOGICAL :: l_output

  !   ref = "2323"
  !   ndata = 212
  !   OPEN (10,FILE="input/ref2323.dat",STATUS="OLD")
  !   DO i = 1, ndata
  !     READ (10,*) chem, formula, casrn, KHpc, refnumber, temp, trivial
  !     ! only show output for debugging or if original paper is not yet included:
  !     l_output = .TRUE.
  !     SELECT CASE(refnumber)
  !     CASE (5)  ; CALL SettypeX("479")         ; l_output = .FALSE.
  !     CASE (7)  ; CALL SettypeX("2302")        ; l_output = .FALSE.
  !     CASE (11) ; CALL SettypeX("633")         ; l_output = .FALSE.
  !     CASE (12) ; CALL SettypeX("634")       ! ; l_output = .FALSE.
  !     CASE (13) ; CALL SettypeX("635")         ; l_output = .FALSE.
  !     CASE (15) ; CALL SettypeX("howard89")
  !     CASE (22) ; CALL SettypeX("2338")        ; l_output = .FALSE.
  !     CASE (28) ; CALL SettypeX("2339")        ; l_output = .FALSE.
  !     CASE (31) ; CALL SettypeX("2340")        ; l_output = .FALSE.
  !     CASE DEFAULT
  !       CALL PrintWarning("refnumber error in ref2323")
  !     END SELECT
  !     IF (l_output) THEN
  !       SELECT CASE(temp)
  !       CASE(0)  ; CALL MakeNoteOtherTemp("273")
  !       CASE(17) ; CALL MakeNoteOtherTemp("290")
  !       CASE(19) ; CALL MakeNoteOtherTemp("292")
  !       CASE(20) ; CALL MakeNoteOtherTemp("293")
  !       CASE(24) ; CALL MakeNoteOtherTemp("297")
  !       CASE(26) ; CALL MakeNoteOtherTemp("299")
  !       CASE(30) ; CALL MakeNoteOtherTemp("303")
  !       CASE(35) ; CALL MakeNoteOtherTemp("308")
  !       CASE(50) ; CALL MakeNoteOtherTemp("323")
  !       CASE(75) ; CALL MakeNoteOtherTemp("348")
  !       CASE DEFAULT
  !         IF (temp/=25) CALL PrintWarning("temp error in ref2323")
  !       END SELECT
  !       Hominus = KHpcSI_TIMES_HcpSI/(KHpc*1.E3)
  !       CALL Output(Hominus)
  !     ELSE
  !       seenote = "" ! reset so that next CALL Output won't use it
  !       type    = "" ! reset to invalid value that will be overwritten
  !     ENDIF
  !   ENDDO
  !   CLOSE(10)

  !   chem = "1-chloro-2-methylpropene" ; casrn = "513-37-1"
  !   type = "?"
  !   CALL MakeNote("CRCwrong1", &
  !     TRIM(citet())//" refer to \citet{479} but that "// &
  !     "article lists this value for 1-chloro-2-methylpropane "// &
  !     "(the saturated compound), not for 1-chloro-2-methylpropene.")
  !   CALL Output(DUMMY)

  !   chem = "trans-1,2-dimethylcyclohexane" ; casrn = "6876-23-9"
  !   type = "?"
  !   CALL MakeNote("CRCwrong2", &
  !     TRIM(citet())//" refer to \citet{479} but that "// &
  !     "article lists this value for 1,4-dimethylcyclohexane, "// &
  !     "not for 1,2-dimethylcyclohexane.")
  !   CALL Output(DUMMY)

  ! END SUBROUTINE ref2323

  !---------------------------------------------------------------------------

  SUBROUTINE ref2338 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2338"

    CALL CalcH("benzene",                 "71-43-2", 557.       )
    CALL CalcH("methylbenzene",          "108-88-3", 660.       )
    CALL CalcH("ethylbenzene",           "100-41-4", 843.       )
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6", 551.       )
    CALL CalcH("1,3-dimethylbenzene",    "108-38-3", 730.       )
    CALL CalcH("1,4-dimethylbenzene",    "106-42-3", 690.       )
    CALL CalcH("1,2,3-trimethylbenzene", "526-73-8", 343.       )
    CALL CalcH("1,2,4-trimethylbenzene",  "95-63-6", 569.       )
    CALL CalcH("1,3,5-trimethylbenzene", "108-67-8", 781.       )
    CALL CalcH("propylbenzene",          "103-65-1", 1040.      )
    CALL CalcH("cumene",                  "98-82-8", 1466.      )
    CALL CalcH("butylbenzene",           "104-51-8", 1332.      )
    CALL CalcH("styrene",                "100-42-5", DUMMY) ! not in Tab. 4, 286
    CALL CalcH("naphthalene",             "91-20-3", 43.        )
    CALL CalcH("1-methylnaphthalene",     "90-12-0", 44.90      )
    CALL CalcH("2-methylnaphthalene",     "91-57-6", DUMMY) ! 51, not in Tab.5
    CALL CalcH("biphenyl",                "92-52-4", 28.0       )
    CALL CalcH("acenaphthene",            "83-32-9", 12.2       )
    CALL CalcH("acenaphthylene",         "208-96-8", DUMMY) ! 8.4, not in Tab.5
    CALL CalcH("fluorene",                "86-73-7", 7.87       )
    CALL CalcH("anthracene",             "120-12-7", 4.0        )
    CALL CalcH("phenanthrene",            "85-01-8", 3.2        )
    CALL CalcH("pyrene",                 "129-00-0", 0.92       )
    CALL CalcH("fluoranthene",           "206-44-0", 1.0        )
    CALL CalcH("chrysene",               "218-01-9", 0.065      )
    CALL CalcH("benzo[a]fluorene",       "238-84-6", DUMMY) ! 0.058, not in Tab.5
    CALL CalcH("benz[a]anthracene",       "56-55-3", DUMMY) ! 0.0465 incorrect in Tab. 4, 0.058
    CALL CalcH("benzo[a]pyrene",          "50-32-8", DUMMY) ! not in Tab. 4, 0.0465
    CALL CalcH("benzo[e]pyrene",         "192-97-2", DUMMY) ! not in Tab. 4, 0.0467

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (ABS(H-DUMMY) > TINY(0.)) THEN
        type = "V"
        CALL Output(KHpcSI_TIMES_HcpSI/H)
      ELSE
        type = "W"
        CALL MakeNote(TRIM(ref), &
          "Because of discrepancies between the values shown in Tables 4 and 5 of " &
          //TRIM(citet())//", the data are not used here.")
        CALL Output(DUMMY)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2338

  !---------------------------------------------------------------------------

  SUBROUTINE ref2339 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2339"

    CALL CalcH("naphthalene",                 "91-20-3", M=44.6, V=43.01) ! C{10}H8
    CALL CalcH("1-methylnaphthalene",         "90-12-0", M=24.3, V=44.90) ! C{10}H7CH3
    CALL CalcH("2-methylnaphthalene",         "91-57-6",         V=51.20) ! C{10}H7CH3
    CALL CalcH("1,5-dimethylnaphthalene",    "571-61-9", M=36.3)          ! C{12}H{12}
    CALL CalcH("biphenyl",                    "92-52-4", M=31.2, V=28.64) ! (C6H5)2
    CALL CalcH("acenaphthylene",             "208-96-8",         V=8.40)  ! C{12}H8
    CALL CalcH("acenaphthene",                "83-32-9", M=16.2, V=12.17) ! C{12}H{10}
    CALL CalcH("2,3-benzindene",              "86-73-7", M=9.75, V=7.87)  ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",                "85-01-8", M=3.61, V=3.24)  ! C{14}H{10}
    CALL CalcH("anthracene",                 "120-12-7", M=7.66, V=3.96)  ! C{14}H{10}
    CALL CalcH("benzo[$jk$]fluorene",        "206-44-0",         V=0.957) ! C{16}H{10} fluoranthene
    CALL CalcH("pyrene",                     "129-00-0", M=1.21, V=0.920) ! C{16}H{10}
    CALL CalcH("benzo[$k$]fluoranthene",     "207-08-9",         V=0.016) ! C{20}H{12}
    CALL CalcH("benzo[$a$]pyrene",            "50-32-8",         V=0.046) ! C{20}H{12} benz[$a$]pyrene
    CALL CalcH("benzo[$ghi$]perylene",       "191-24-2",         V=0.075) ! C{22}H{12}
    CALL CalcH("benzene",                     "71-43-2",         V=557.)  ! C6H6
    CALL CalcH("chlorobenzene",              "108-90-7", M=315., V=367.)  ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",         "95-50-1", M=195., V=244.2) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",        "541-73-1",         V=376.1) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",        "106-46-7", M=244., V=159.8) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,2,3-trichlorobenzene",      "87-61-6", M=127., V=242.0) ! C6H3Cl3
    CALL CalcH("1,2,4-trichlorobenzene",     "120-82-1",         V=276.7) ! C6H3Cl3
    CALL CalcH("1,3,5-trichlorobenzene",     "108-70-3",         V=1096.) ! C6H3Cl3
    CALL CalcH("1,2,3,4-tetrachlorobenzene", "634-66-2",         V=143.9) ! C6H2Cl4
    CALL CalcH("1,2,3,5-tetrachlorobenzene", "634-90-2", M=160., V=587.7) ! C6H2Cl4
    CALL CalcH("1,2,4,5-tetrachlorobenzene",  "95-94-3",         V=122.4) ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",         "608-93-5",         V=84.72) ! C6HCl5
    CALL CalcH("hexachlorobenzene",          "118-74-1",         V=131.0) ! C6Cl6
    CALL CalcH("1-chloronaphthalene",         "90-13-1", M=36.3)          ! C{10}H7Cl
    CALL CalcH("2-chloronaphthalene",         "91-58-7", M=33.5)          ! C{10}H7Cl
    CALL CalcH("bromobenzene",               "108-86-1", M=250., V=211.)  ! C6H5Br
    CALL CalcH("2-methyl-1-propanol",         "78-83-1", M=2.73, V=1.37)  ! C4H{10}O isobutanol
    CALL CalcH("1-heptanol",                 "111-70-6", M=5.62, V=1.603) ! C7H{16}O
    CALL CalcH("2-pentanone",                "107-87-9", M=8.47, V=6.83)  ! C3H7COCH3
    CALL CalcH("2-heptanone",                "110-43-0", M=17.1, V=13.3)  ! C7H{14}O
    CALL CalcH("1-phenylethanone",            "98-86-2", M=1.08, V=0.982) ! C6H5COCH3 acetophenone

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, V, M)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, OPTIONAL,   INTENT(IN) :: V, M
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      IF (PRESENT(V)) THEN
        type = "V"
        CALL Output(KHpcSI_TIMES_HcpSI/V)
      ENDIF

      IF (PRESENT(M)) THEN
        type = "M"
        CALL Output(KHpcSI_TIMES_HcpSI/M)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref2339

  !---------------------------------------------------------------------------

  SUBROUTINE ref2340 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2340"
    type = "M"

    CALL CalcH("PCB-1",    "2051-60-7", 20.43, 42.7E3)
    CALL CalcH("PCB-8",   "34883-43-7", 24.89, 44.1E3)
    CALL CalcH("PCB-18",  "37680-65-2", 25.35, 34.9E3)
    CALL CalcH("PCB-28",   "7012-37-5", 38.14, 32.5E3)
    CALL CalcH("PCB-29",  "15862-07-4", 37.89, 34.9E3)
    CALL CalcH("PCB-44",  "41464-39-5", 28.05, 25.8E3)
    CALL CalcH("PCB-50",  "62796-65-0", 64.3,  23.8E3)
    CALL CalcH("PCB-52",  "35693-99-3", 31.07, 30.5E3)
    CALL CalcH("PCB-66",  "32598-10-0", 36.97, 29.0E3)
    CALL CalcH("PCB-77",  "32598-13-3", 16.20, 39.8E3)
    CALL CalcH("PCB-87",  "38380-02-8", 37.71, 32.5E3)
    CALL CalcH("PCB-101", "37680-73-2", 42.07, 29.7E3)
    CALL CalcH("PCB-104", "56558-16-8", 66.0,  14.5E3)
    CALL CalcH("PCB-105", "32598-14-4", 33.6,  75.6E3)
    CALL CalcH("PCB-118", "31508-00-6", 36.2,  49.8E3)
    CALL CalcH("PCB-126", "57465-28-8", 21.02, 98.5E3)
    CALL CalcH("PCB-128", "38380-07-3", 35.4,  118.0E3)
    CALL CalcH("PCB-138", "35065-28-2", 44.6,  87.1E3)
    CALL CalcH("PCB-153", "35065-27-1", 52.8,  66.1E3)
    CALL CalcH("PCB-154", "60145-22-4", 76.7,  46.2E3)
    CALL CalcH("PCB-170", "35065-30-6", 20.84, 164.0E3)
    CALL CalcH("PCB-180", "35065-29-3", 37.0,  143.6E3)
    CALL CalcH("PCB-187", "52663-68-0", 62.2,  96.3E3)
    CALL CalcH("PCB-188", "74487-85-7", 113.1, 62.0E3)
    CALL CalcH("PCB-195", "52663-78-2", 14.13, 167E3)
    CALL CalcH("PCB-201", "40186-71-8", 95.8,  144.5E3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Harray, DeltaHh)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Harray
      REAL,             INTENT(IN) :: DeltaHh
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      mindHR = DeltaHh / Rgas
      Hominus = KHpcSI_TIMES_HcpSI/Harray
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2340

  !---------------------------------------------------------------------------

  SUBROUTINE ref2360 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2360"
    type = "?"
    chem = "nitryl chloride" ; casrn = "13444-90-1" ! ClNO2
    CALL Output(4E-2*Hcp_TO_HcpSI)

  END SUBROUTINE ref2360

  !---------------------------------------------------------------------------

  SUBROUTINE ref2371 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2371"
    type = "M"

    chem = "ethyl ethanoate" ; casrn = "141-78-6" ! CH3COOC2H5 ethyl acetate
    CALL Output(5.2*Hcp_TO_HcpSI)

    chem = "ethyl butanoate" ; casrn = "105-54-4" ! C3H7COOC2H5 ethyl butyrate
    CALL Output(2.4*Hcp_TO_HcpSI)

    chem = "ethyl hexanoate" ; casrn = "123-66-0" ! C5H{11}COOC2H5
    CALL Output(1.4*Hcp_TO_HcpSI)

    chem = "ethyl octanoate" ; casrn = "106-32-1" ! C7H{15}COOC2H5
    CALL Output(1.1*Hcp_TO_HcpSI)

    chem = "ethyl decanoate" ; casrn = "110-38-3" ! C9H{19}COOC2H5
    CALL Output(1.4*Hcp_TO_HcpSI)

    chem = "methyl butanoate" ; casrn = "623-42-7" ! C3H7COOCH3 methyl butyrate
    CALL Output(3.7*Hcp_TO_HcpSI)

    chem = "methyl hexanoate" ; casrn = "106-70-7" ! C5H{11}COOCH3
    CALL Output(1.9*Hcp_TO_HcpSI)

    chem = "methyl octanoate" ; casrn = "111-11-5" ! C6H{13}COOCH3
    CALL Output(1.0*Hcp_TO_HcpSI)

    chem = "methyl decanoate" ; casrn = "110-42-9" ! C{11}H{22}O{2} methyl caprate
    CALL Output(1.1*Hcp_TO_HcpSI)

  END SUBROUTINE ref2371

  !---------------------------------------------------------------------------

  SUBROUTINE ref2376 ! special definition (DeltaG and DeltaH for KHpx [atm])
    IMPLICIT NONE

    ref = "2376"
    type = "T"

    !CALL CalcH("tetrahydrofuran",                "109-99-9",  0.80, -11.30) ! from ref0723
    !CALL CalcH("tetrahydropyran",                "142-68-7",  1.15, -11.68) ! from ref0723
    !CALL CalcH("piperidine",                     "110-89-4", -0.83, -15.63) ! from ref0722
    !CALL CalcH("N-methyl-piperidine",            "626-67-5",  0.38, -15.72) ! from ref0722
    !CALL CalcH("1,3-dioxolane",                  "646-06-0",  0.18,  -9.54) ! from ref0723
    !CALL CalcH("1,4-dioxane",                    "123-91-1", -0.78, -11.47) ! from ref0723
    CALL CalcH("1-oxa-4-azacyclohexane",          "110-91-8", -2.9,  -16.60)
    CALL CalcH("4-methyl-1-oxa-4-azacyclohexane", "109-02-4", -2.06, -16.41)
    CALL CalcH("1,4-diazacyclohexane",            "110-85-0", -3.1,  -21.6 )
    CALL CalcH("N-methylpiperazine",              "109-01-3", -3.5,  -21.61)
    CALL CalcH("N,N'-dimethylpiperazine",         "106-58-1", -3.3,  -21.68)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, dH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpx_TIMES_HcpSI / EXP(dG*kcal/(Rgas*T0))
      mindHR = -dH * kcal/Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2376

  !---------------------------------------------------------------------------

  SUBROUTINE ref2377 ! special definition (DeltaG and DeltaH for KHpx [atm])
    IMPLICIT NONE

    ref = "2377"
    type = "T"

    CALL CalcH("2-butanol",      "78-92-2", -0.30, -14.99) ! C4H{10}O {sec}-butanol
    CALL CalcH("3-pentanol",    "584-02-1", -0.08, -15.77) ! C5H{12}O
    CALL CalcH("3-hexanol",     "623-37-0",  0.20, -16.63) ! C6H{14}O
    CALL CalcH("4-heptanol",    "589-55-9",  0.27, -18.00) ! C7H{15}OH
    CALL CalcH("cyclopentanol",  "96-41-3", -1.22, -15.98) ! C5H9OH
    CALL CalcH("cyclohexanol",  "108-93-0", -1.20, -16.85) ! C6H{11}OH
    CALL CalcH("cycloheptanol", "502-41-0", -1.21, -17.82) ! C7H{13}OH

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, dH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpx_TIMES_HcpSI / EXP(dG*kcal/(Rgas*T0))
      mindHR = -dH * kcal/Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2377

  !---------------------------------------------------------------------------

  SUBROUTINE ref2378 ! special definition (DeltaG and DeltaH for KHpx [atm])

    IMPLICIT NONE

    ref = "2378"

    ! conversion factors from page 7143:
    ! ln(10) 8.314 298.15 / 4186. => 1.36351932821
    ! log10(760 8.314 298.15 1000. / 101325) => 4.26934181372

    chem = "2,6-bis-(1,1-dimethylethyl)-pyridine" ; casrn = "585-48-8" ! DTBP
    type = "V"
    CALL MakeNote("2378DTBP", &
      "This value is calculated from the solubility of 9.4\E{-3} "// &
      "\unit{mol/L} and the vapor pressure of 0.255 \unit{mmHg}, as shown "// &
      "on pages 7142-7143 of "//TRIM(citet())//". It is inconsistent with "// &
      "the entry in Table IV of that paper.")
    CALL Output(9.4E-3/(dm3*0.255*mmHg))

    ! Some values are apparently copied from ref0719. These are not used here.
    ! Entries in Table 2 marked with footnote c ("this report") are type "M" here.

    CALL CalcH("2,6-bis-(1,1-dimethylethyl)-pyridine", "585-48-8", "M",    3.87, -1.7  )
    CALL CalcH("4-(1,1-dimethylethyl)-pyridine",      "3978-81-2", "?",   -0.19, -1.9  )
    !CALL CalcH("2,6-dimethylpyridine",                "108-48-5", "719", -0.33, -2.88 )
    !CALL CalcH("2,4-dimethylpyridine",                "108-47-4", "719", -0.59, -2.58 )
    CALL CalcH("4-methoxypyridine",                    "620-08-6", "?",       dH=-2.20 )
    !CALL CalcH("2,5-dimethylpyridine",                "589-93-5", "719", -0.44, -2.61 )
    !CALL CalcH("3,5-dimethylpyridine",                "591-22-0", "719", -0.57, -2.53 )
    !CALL CalcH("4-methylpyridine",                    "108-89-4", "719", -0.66, -1.34 )
    CALL CalcH("4-methylpyridine",                     "108-89-4", "?",   -0.57        )
    !CALL CalcH("2-methylpyridine",                    "109-06-8", "719", -0.36, -1.24 )
    !CALL CalcH("3-methylpyridine",                    "108-99-6", "719", -0.50, -1.12 )
    !CALL CalcH("pyridine",                            "110-86-1", "719", -0.42,  0.00 )
    CALL CalcH("pyridine",                             "110-86-1", "M",   -0.41        )
    CALL CalcH("3-chloropyridine",                     "626-60-8", "M",    0.26,  0.88 )
    CALL CalcH("2-chloropyridine",                     "109-09-1", "M",   -0.18,  0.11 )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL, OPTIONAL,   INTENT(IN) :: dG
      REAL, OPTIONAL,   INTENT(IN) :: dH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it

      CALL MakeNote("2378like0917","Calculated using $\Delta "// &
      "G_s^{g\rightarrow \chem{H_2O}}$ and $\Delta H_s^{g\rightarrow "// &
      "\chem{H_2O}}$ from Table IV of "//TRIM(citet())//". Since some of "// &
      "the values in this table are taken directly from \citet{719}, it "// &
      "is assumed that the thermodynamic properties are defined in the "// &
      "same way. Since $\Delta H_s^{g\rightarrow \chem{H_2O}}$ is defined "// &
      "relative to pyridine, a value of -11.93 \unit{kcal/mol} from "// &
      "\citet{2386} was added.")

      IF (PRESENT(dG)) THEN
        Hominus = KHpx_TIMES_HcpSI / EXP(dG*kcal/(Rgas*T0))
      ELSE
        Hominus = DUMMY
      ENDIF
      IF (PRESENT(dH)) THEN
        ! subtract dH(pyridine)=11.93 kcal/mol from ref2386:
        mindHR = - kcal * (dH-11.93) / Rgas
        CALL Output(Hominus, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref2378

  !---------------------------------------------------------------------------

  SUBROUTINE ref2379 ! KHcc [1]
    IMPLICIT NONE

    ref = "2379"
    type = "M"

    CALL CalcH("ethanamide", "60-35-5", 7.6E-8) ! C2H5NO acetamide

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2379

  !---------------------------------------------------------------------------

  SUBROUTINE ref2381 ! special definition (DeltaG and DeltaH for KHpx [atm])
    IMPLICIT NONE

    ref = "2381"
    type = "T"

    CALL CalcH("ethylenediamine",       "107-15-3", -3.32, -18.36) ! H2NCH2CH2NH2
    CALL CalcH("2-methoxyethylamine",   "109-85-3", -2.27, -15.18)
    CALL CalcH("3-methoxypropylamine", "5332-73-0", -2.65, -17.28)
    CALL CalcH("1,2-dimethoxyethane",   "110-71-4", -0.56, -14.18)
    CALL CalcH("2-methoxyethanol",      "109-86-4", -2.49, -14.45) ! C3H8O2 methyl cellosolve
    CALL CalcH("2-ethoxyethanol",       "110-80-5", -2.33, -15.86) ! C4H{10}O2
    CALL CalcH("2-propoxyethanol",     "2807-30-9", -2.14, -16.72)
    CALL CalcH("2-butoxyethanol",       "111-76-2", -1.99, -17.60)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, dH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpx_TIMES_HcpSI / EXP(dG*kcal/(Rgas*T0))
      mindHR = -dH * kcal/Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2381

  !---------------------------------------------------------------------------

  SUBROUTINE ref2387 ! KHcc [1]
    IMPLICIT NONE

    ref = "2387"
    type = "M"

    CALL CalcH("propanone",         "67-64-1", 0.0013) ! CH3COCH3 acetone
    CALL CalcH("ethyl ethanoate",  "141-78-6", 0.0071) ! CH3COOC2H5 ethyl acetate
    CALL CalcH("isovaleraldehyde", "590-86-3", 0.020 )
    CALL CalcH("2-pentanone",      "107-87-9", 0.0044)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, kw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: kw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / kw
      CALL MakeNoteOtherTemp("301") ! 28 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2387

  !---------------------------------------------------------------------------

  SUBROUTINE ref2388 ! KHcc [1]
    IMPLICIT NONE

    ref = "2388"

    ! Tab. IV:
    CALL CalcH("propanal",               "123-38-6", "V", 2.6E-3 )
    CALL CalcH("butanal",                "123-72-8", "V", 6.0E-3 )
    CALL CalcH("pentanal",               "110-62-3", "V", 6.3E-3 )
    CALL CalcH("hexanal",                 "66-25-1", "V", 11.4E-3)
    CALL CalcH("heptanal",               "111-71-7", "V", 7.5E-3 )
    CALL CalcH("octanal",                "124-13-0", "V", 14E-3  )
    CALL CalcH("nonanal",                "124-19-6", "V", 31E-3  )
    ! Tab. V:
    CALL CalcH("pentane",                "109-66-0", "V", 52.     )
    CALL CalcH("1-pentene",              "109-67-1", "V", 22.     )
    CALL CalcH("1-pentyne",              "627-19-0", "V", 1.6     )
    CALL CalcH("1-chloropentane",        "543-59-9", "V", 0.93    )
    CALL CalcH("1-pentanethiol",         "110-66-7", "V", 0.55    )
    CALL CalcH("methyl butyl ether",     "628-28-4", "V", 0.091   )
    CALL CalcH("methyl butanoate",       "623-42-7", "V", 0.011   )
    CALL CalcH("1-nitropentane",         "628-05-7", "V", 0.0085  )
    !CALL CalcH("pentanal",               "110-62-3", "V", 0.0063  ) ! also in Tab. IV
    CALL CalcH("pentane nitrile",        "110-59-8", "V", 0.0026  )
    CALL CalcH("2-pentanone",            "107-87-9", "V", 0.0013  )
    CALL CalcH("1-pentanamine",          "110-58-7", "M", 0.0013  ) ! experimental
    CALL CalcH("1-pentanol",              "71-41-0", "V", 0.00052 )
    CALL CalcH("pentanoic acid",         "109-52-4", "V", 0.000031)
    ! Tab. VI:
    CALL CalcH("1-butanamine",           "109-73-9", "V", 8.9E-4)
    CALL CalcH("cyclohexanamine",        "108-91-8", "V", 4.3E-4)
    CALL CalcH("pyrrolidine",            "123-75-1", "V", 9.6E-5)
    CALL CalcH("piperidine",             "110-89-4", "V", 2.0E-4)
    CALL CalcH("3-pyrroline",            "109-96-6", "V", 8.2E-5)
    CALL CalcH("musk xylol",              "81-15-2", "V", 2.4E-2)
    CALL CalcH("1,4-dioxane",            "123-91-1", "V", 3.7E-4)
    ! Tab. VII, calculated:
    CALL CalcH("isobutyl isobutyrate",    "97-85-8", "V", 0.056    )
    CALL CalcH("1-butanol",               "71-36-3", "V", 0.00035  )
    CALL CalcH("3-methylbutanoic acid",  "503-74-2", "V", 0.000055 )
    CALL CalcH("trimethylamine",          "75-50-3", "V", 0.0041   )
    CALL CalcH("2-methylpropanal",        "78-84-2", "V", 0.0060   )
    CALL CalcH("pentadecalactone",       "106-02-5", "V", 0.0053   )
    CALL CalcH("Carvone",               "6485-40-1", "V", 0.00073  )
    CALL CalcH("1,8-cineole",            "470-82-6", "V", 0.0052   )
    ! Tab. VII, experimental:
    CALL CalcH("isobutyl isobutyrate",    "97-85-8", "M", 0.040     )
    CALL CalcH("1-butanol",               "71-36-3", "M", 0.00036   )
    CALL CalcH("pyridine",               "110-86-1", "M", 0.00057   )
    CALL CalcH("3-methylbutanoic acid",  "503-74-2", "M", 0.000034  )
    CALL CalcH("1-pyrroline",           "5724-81-2", "M", 0.00026   )
    CALL CalcH("trimethylamine",          "75-50-3", "M", 0.0053    )
    CALL CalcH("2-methylpropanal",        "78-84-2", "M", 0.0080    )
    CALL CalcH("androstenone",         "18339-16-7", "M", 0.012     )
    CALL CalcH("pentadecalactone",       "106-02-5", "M", 0.10      )
    CALL CalcH("Carvone",               "6485-40-1", "M", 0.00083   )
    CALL CalcH("1,8-cineole",            "470-82-6", "M", 0.0035    )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, Kaw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: Kaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / Kaw
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2388

  !---------------------------------------------------------------------------

  SUBROUTINE ref2389 ! KHcc [1]
    IMPLICIT NONE

    ref = "2389"

  CALL CalcH("2,3,4-trithiapentane",               "3658-80-8", 1.9E-2)
  CALL CalcH("2,3-butanedione",                     "431-03-8", 1.1E-3)
  CALL CalcH("2,3-diethyl-5-methylpyrazine",      "18138-04-0", 5.0E-4)
  CALL CalcH("1-octen-3-ol",                       "3391-86-4", 3.1E-3)
  CALL CalcH("2-methylbutanoic acid, ethyl ester", "7452-79-1", 1.5E-2)
  CALL CalcH("(E)-2-nonenal",                     "18829-56-6", 7.0E-3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Kaw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Kaw

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      Hominus = KHcc_TIMES_HcpSI_atT0 / Kaw
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2389

  !---------------------------------------------------------------------------

  SUBROUTINE ref2401 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2401"
    type = "M"

    chem = "ethanedial" ; casrn = "107-22-2" ! OHCCHO glyoxal
    CALL MakeNote("RCHOdiol")
    CALL Output(4.19E5*Hcp_TO_HcpSI, 62.2E3/Rgas)

    chem = "hydroxyethanoic acid" ; casrn = "79-14-1" ! HOCH2COOH glycolic acid
    CALL Output(2.83E4*Hcp_TO_HcpSI, 33.5E3/Rgas)

    chem = "oxoethanoic acid" ; casrn = "298-12-4" ! OHCCOOH glyoxylic acid
    CALL Output(1.09E4*Hcp_TO_HcpSI, 40.0E3/Rgas)

  END SUBROUTINE ref2401

  !---------------------------------------------------------------------------

  SUBROUTINE ref2406 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2406"
    type = "M"
    chem = "trifluoroethanoic acid" ; casrn = "76-05-1" ! CF3COOH
    Hominus = 5780. * Hcp_TO_HcpSI
    mindHR = 4120.
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2406

  !---------------------------------------------------------------------------

  SUBROUTINE ref2431 ! KHcc [1]
    IMPLICIT NONE

    ref = "2431"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 5., 25. /) + CtoK

    CALL CalcH("ethyl {tert}-butyl ether",  "637-92-3", "?", (/ 0.019,    0.11     /) ) ! C2H5OC(CH3)3 ETBE
    CALL CalcH("2-methoxy-2-methylbutane",  "994-05-8", "?", (/ 0.014,    0.081    /) ) ! C6H{14}O {tert}-amyl methyl ether; TAME
    CALL CalcH("diisopropyl ether",         "108-20-3", "V", (/ 0.030,    0.13     /) ) ! C3H7OC3H7 DIPE
    CALL CalcH("2-methyl-2-propanol",       "75-65-0",  "C", (/ 0.000113, 0.000503 /) ) ! C4H{10}O {tert}-butanol

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, HRT)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      CHARACTER,          INTENT(IN) :: type_
      REAL, DIMENSION(:), INTENT(IN) :: HRT

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it
      Harray = KHcc_TO_HcpSI(HRT,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2431

  !---------------------------------------------------------------------------

  SUBROUTINE ref2432 ! Hcp [M/bar]
    IMPLICIT NONE

    ref = "2432"
    type = "L"
    chem = "ozone" ; casrn = "10028-15-6" ! O3
    mindHR = 2363.
    Hominus = EXP(-12.44 + mindHR/T0) / (bar*dm3)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2432

  !---------------------------------------------------------------------------

  SUBROUTINE ref2433 ! Hxp [1/atm]

    IMPLICIT NONE
    REAL    :: A, B, C

    chem = "nitrogen" ; casrn = "7727-37-9" ! N2
    ref = "2433"
    type = "L"
    A = -67.38765
    B = 86.32129
    C = 24.79808
    Hominus = EXP( A + B/(T0/100.) + C * LOG(T0/100.) ) * Hxp_TO_HcpSI
    mindHR = 100.*B - C*T0
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2433

  !---------------------------------------------------------------------------

  SUBROUTINE ref2434 ! KHpx [Pa]

    IMPLICIT NONE

    chem = "nitrogen" ; casrn = "7727-37-9" ! N2
    ref = "2434"
    type = "M"

    ndata = 17
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 278.124, 278.146, 283.154, 288.153, 293.149, 298.142, 298.148, &
      298.158, 298.158, 298.163, 303.136, 308.141, 308.142, 313.154, 318.151, &
      323.147, 323.150 /)
    Harray = cH2O * 1E-9 / (/ 5.93802, 5.94498, 6.63892, 7.31392, 7.98287, 8.60124, &
      8.60862, 8.61330, 8.60942, 8.60790, 9.18382, 9.73210, 9.72912, 10.22765, &
      10.67186, 11.04854, 11.05092 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2434

  !---------------------------------------------------------------------------

  SUBROUTINE ref2435 ! KHpx [Pa]

    IMPLICIT NONE

    chem = "argon" ; casrn = "7440-37-1" ! Ar
    ref = "2435"
    type = "M"

    ndata = 22
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 275.099, 276.139, 277.137, 277.140, 277.644, 278.163, 279.149, &
      280.227, 281.146, 281.150, 283.157, 288.158, 288.165, 293.142, 298.138, &
      298.152, 298.156, 303.157, 303.158, 308.159, 313.145, 313.150 /)
    Harray = cH2O * 1E-9 / (/ 2.46741, 2.52799, 2.59429, 2.59547, 2.63035, 2.67011, &
      2.74163, 2.80726, 2.86923, 2.87057, 2.99898, 3.34220, 3.34090, 3.67146, &
      3.99911, 4.00114, 4.00404, 4.31977, 4.31718, 4.62764, 4.91250, 4.90693 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2435

  !---------------------------------------------------------------------------

  SUBROUTINE ref2436 ! KHpx [atm]
    IMPLICIT NONE

    ref = "2436"
    type = "M"

    CALL CalcH("helium",  "7440-59-7", -1.542063,  6.586451, 4.827122) ! He
    CALL CalcH("neon",    "7440-01-9", -1.819817,  7.218989, 4.612540) ! Ne
    CALL CalcH("argon",   "7440-37-1", -2.479705,  8.545065, 3.718433) ! Ar
    CALL CalcH("krypton", "7439-90-9", -2.779465,  9.212515, 3.095855) ! Kr
    CALL CalcH("xenon",   "7440-63-3", -3.211715, 10.405394, 1.984034) ! Xe

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a2Tc1m2, a1Tc1m1, a0)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: a2Tc1m2, a1Tc1m1, a0
      REAL, PARAMETER :: Tc1 = 647.
      REAL :: a2, a1

      a2 = a2Tc1m2 * Tc1**2
      a1 = a1Tc1m1 * Tc1
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpx_TIMES_HcpSI / EXP(a0 + a1/T0 + a2/T0**2)
      mindHR = - (a1 + 2.*a2/T0)
      CALL Output(Hominus, mindHR)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2436

  !---------------------------------------------------------------------------

  SUBROUTINE ref2437 ! KHpx [atm]
    IMPLICIT NONE
    REAL :: a0, a1, a2

    ref = "2437"
    type = "M"
    chem = "oxygen" ; casrn = "7782-44-7" ! O2
    a0 = 3.71814
    a1 = 5596.17
    a2 = -1049668.
    Hominus = KHpx_TIMES_HcpSI / EXP(a0 + a1/T0 + a2/T0**2)
    mindHR = - (a1 + 2.*a2/T0)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2437

  !---------------------------------------------------------------------------

  SUBROUTINE ref2438 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2438"
    type = "M"

    chem = "alachlor" ; casrn = "15972-60-8" ! C{14}H{20}ClNO2
    CALL Output(14E3*Hcp_TO_HcpSI, 9200.)

    chem = "dimethyl-2,2-dichlorovinyl phosphate" ; casrn = "62-73-7" ! dichlorvos
    CALL Output(4E3*Hcp_TO_HcpSI, 11100.)

  END SUBROUTINE ref2438

  !---------------------------------------------------------------------------

  SUBROUTINE ref2439 ! KHcc [1]
    IMPLICIT NONE

    ref = "2439"
    type = "V"

    CALL CalcH("butane",                         "106-97-8", 5.7E+4) ! C4H{10}
    CALL CalcH("hexane",                         "110-54-3", 1.0E+5) ! C6H{14}
    CALL CalcH("octane",                         "111-65-9", 1.8E+5) ! C8H{18}
    CALL CalcH("decane",                         "124-18-5", 2.7E+5) ! C{10}H{22}
    CALL CalcH("cyclopentane",                   "287-92-3", 1.0E+4) ! C5H{10}
    CALL CalcH("cyclohexane",                    "110-82-7", 9.1E+3) ! C6H{12}
    CALL CalcH("1-hexene",                       "592-41-6", 2.3E+4) ! C6H{12}
    CALL CalcH("1-octene",                       "111-66-0", 5.3E+4) ! C8H{16}
    CALL CalcH("cyclopentene",                   "142-29-0", 3.5E+3) ! C5H8
    CALL CalcH("cyclohexene",                    "110-83-8", 2.5E+3) ! C6H{10}
    CALL CalcH("1,3-butadiene",                  "106-99-0", 1.1E+4) ! C4H6
    CALL CalcH("1,5-hexadiene",                  "592-42-7", 8.1E+3) ! C6H{10}
    CALL CalcH("ethyne",                          "74-86-2", 1.4E+3) ! C2H2 acetylene
    CALL CalcH("1-butyne",                       "107-00-6", 1.9E+3) ! C2H5CCH ethylacetylene
    CALL CalcH("benzene",                         "71-43-2", 3.0E+2) ! C6H6
    CALL CalcH("methylbenzene",                  "108-88-3", 4.1E+2) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",                   "100-41-4", 4.7E+2) ! C6H5C2H5
    CALL CalcH("1,4-dimethylbenzene",            "106-42-3", 3.7E+2) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("(2-propyl)-benzene",              "98-82-8", 8.0E+2) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("naphthalene",                     "91-20-3", 6.1E+1) ! C{10}H8
    CALL CalcH("acenaphthene",                    "83-32-9", 4.5E+1) ! C{12}H{10}
    CALL CalcH("2,3-benzindene",                  "86-73-7", 3.6E+1) ! C{13}H{10} fluorene
    CALL CalcH("anthracene",                     "120-12-7", 1.8E+1) ! C{14}H{10}
    CALL CalcH("phenanthrene",                    "85-01-8", 1.7E+1) ! C{14}H{10}
    CALL CalcH("pyrene",                         "129-00-0", 1.5E+1) ! C{16}H{10}
    CALL CalcH("dichloromethane",                 "75-09-2", 1.9E+2) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",                "67-66-3", 2.1E+2) ! CHCl3 chloroform
    CALL CalcH("tetrachloromethane",              "56-23-5", 1.6E+3) ! CCl4 carbontetrachloride
    CALL CalcH("chloroethane",                    "75-00-3", 9.8E+2) ! C2H5Cl
    CALL CalcH("hexachloroethane",                "67-72-1", 3.6E+1) ! C2Cl6
    CALL CalcH("chloroethene",                    "75-01-4", 5.0E+3) ! CH2CHCl vinyl chloride
    CALL CalcH("(E)-1,2-dichloroethene",         "156-60-5", 3.6E+2) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",                 "79-01-6", 4.8E+2) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",              "127-18-4", 1.5E+3) ! C2Cl4 tetrachloroethylene
    CALL CalcH("chlorobenzene",                  "108-90-7", 2.2E+2) ! C6H5Cl
    CALL CalcH("bromobenzene",                   "108-86-1", 1.1E+2) ! C6H5Br
    CALL CalcH("1,2-dichlorobenzene",             "95-50-1", 9.1E+1) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("2-chloronaphthalene",             "91-58-7", 3.7E+1) ! C{10}H7Cl
    CALL CalcH("2-chlorobiphenyl",              "2051-60-7", 2.0E+2) ! C{12}H9Cl PCB-1
    CALL CalcH("2,2',4,4'-tetrachlorobiphenyl", "2437-79-8", 2.7E+2) ! C{12}H6Cl4 PCB-47
    CALL CalcH("methanol",                        "67-56-1", 2.9E-1) ! CH3OH
    CALL CalcH("ethanol",                         "64-17-5", 4.3E-1) ! C2H5OH
    CALL CalcH("1-butanol",                       "71-36-3", 6.6E-1) ! C4H9OH
    CALL CalcH("1-hexanol",                      "111-27-3", 7.2E-1) ! C6H{13}OH
    CALL CalcH("1,2-ethanediol",                 "107-21-1", 1.1E-4) ! HO(CH2)2OH ethylene glycol
    CALL CalcH("1,2,3-propanetriol",              "56-81-5", 1.1E-7) ! C3H8O3 glycerol
    CALL CalcH("hydroxybenzene",                 "108-95-2", 1.6E-1) ! C6H5OH phenol
    CALL CalcH("1-hydroxy-2,4-dimethylbenzene",  "105-67-9", 1.0E+0) ! C8H{10}O 2,4-xylenol; 2,4-dimethylphenol
    CALL CalcH("methanal",                        "50-00-0", 2.4E-1) ! HCHO formaldehyde
    CALL CalcH("ethanal",                         "75-07-0", 4.6E+0) ! CH3CHO acetaldehyde
    CALL CalcH("propenal",                       "107-02-8", 7.8E+0) ! CH2CHCHO acrolein
    CALL CalcH("butanal",                        "123-72-8", 5.4E+0) ! C3H7CHO butyraldehyde
    CALL CalcH("methanoic acid",                  "64-18-6", 3.6E-2) ! HCOOH formic acid
    CALL CalcH("ethanoic acid",                   "64-19-7", 6.0E-2) ! CH3COOH acetic acid
    CALL CalcH("butanoic acid",                  "107-92-6", 5.8E-2) ! C3H7COOH butyric acid
    CALL CalcH("hexanoic acid",                  "142-62-1", 2.7E-2) ! C5H{11}COOH caproic acid
    CALL CalcH("ethyl ethanoate",                "141-78-6", 1.5E+0) ! CH3COOC2H5 ethyl acetate
    CALL CalcH("butyl ethanoate",                "123-86-4", 2.0E+1) ! CH3COOC4H9 butyl acetate
    CALL CalcH("dimethyl phthalate",             "131-11-3", 1.1E-2) ! C{10}H{10}O4
    CALL CalcH("dibutyl phthalate",               "84-74-2", 2.1E-3) ! C{16}H{22}O4
    CALL CalcH("propanone",                       "67-64-1", 2.1E+0) ! CH3COCH3 acetone
    CALL CalcH("butanone",                        "78-93-3", 2.1E+0) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("4-methyl-2-pentanone",           "108-10-1", 7.6E+0) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("isophorone",                      "78-59-1", 3.2E-1) ! C9H{14}O isophorone
    CALL CalcH("diethyl ether",                   "60-29-7", 6.3E+1) ! C2H5OC2H5
    CALL CalcH("methyl {tert}-butyl ether",     "1634-04-4", 2.7E+1) ! CH3OC(CH3)3 MTBE
    CALL CalcH("dipropyl ether",                 "111-43-3", 9.5E+1) ! C3H7OC3H7
    CALL CalcH("oxirane",                         "75-21-8", 1.1E+1) ! C2H4O ethylene oxide
    CALL CalcH("1,4-dioxane",                    "123-91-1", 2.9E-1) ! C4H8O2 dioxane
    CALL CalcH("ethanamine",                      "75-04-7", 1.8E+0) ! C2H5NH2 ethylamine
    CALL CalcH("1-butanamine",                   "109-73-9", 2.5E+0) ! C4H9NH2 1-butylamine
    CALL CalcH("aminobenzene",                    "62-53-3", 1.0E-1) ! C6H7N aniline
    CALL CalcH("caprolactam",                    "105-60-2", 2.8E-4)
    CALL CalcH("ethane nitrile",                  "75-05-8", 1.1E+0) ! CH3CN acetonitrile
    CALL CalcH("2-propenenitrile",               "107-13-1", 5.6E+0) ! C3H3N acrylonitrile
    CALL CalcH("nitroethane",                     "79-24-3", 2.9E+0) ! C2H5NO2
    CALL CalcH("nitrobenzene",                    "98-95-3", 1.2E+0) ! C6H5NO2
    CALL CalcH("carbon disulfide",                "75-15-0", 6.8E+2) ! CS2
    CALL CalcH("ethanethiol",                     "75-08-1", 1.6E+2) ! C2H5SH ethyl mercaptan
    CALL CalcH("1-butanethiol",                  "109-79-5", 4.0E+2) ! C4H9SH butyl mercaptan

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpx_TIMES_HcpSI / KHcc
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2439

  !---------------------------------------------------------------------------

  SUBROUTINE ref2440 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2440"

    CALL CalcH("bromochlorodifluoromethane", "353-59-3", 1.6542E-01)
    CALL CalcH("bromotrifluoromethane",       "75-63-8", 4.8031E-01)
    CALL CalcH("chlorotrifluoromethane",      "75-72-9", 1.1233E+00)
    CALL CalcH("cyanogen chloride",          "506-77-4", 1.9412E-03)
    CALL CalcH("dichlorodifluoromethane",     "75-71-8", 3.9001E-01)
    CALL CalcH("phosgene",                    "75-44-5", 1.3949E-02)
    CALL CalcH("trichlorofluoromethane",      "75-69-4", 1.2165E-01)
    CALL CalcH("carbon tetrachloride",        "56-23-5", 2.9338E-02)
    CALL CalcH("carbon tetrafluoride",        "75-73-0", 5.3236E+00)
    CALL CalcH("tribromomethane",             "75-25-2", 5.8601E-04)
    CALL CalcH("chlorodifluoromethane",       "75-45-6", 3.0148E-02)
    CALL CalcH("dichlorofluoromethane",       "75-43-4", 5.2167E-03)
    CALL CalcH("chloroform",                  "67-66-3", 4.1011E-03)
    CALL CalcH("trifluoromethane",            "75-46-7", 7.5246E-02)
    CALL CalcH("hydrogen cyanide",            "74-90-8", 8.8973E-05)
    CALL CalcH("bromochloromethane",          "74-97-5", 1.5967E-03)
    CALL CalcH("dibromomethane",              "74-95-3", 9.2995E-04)
    CALL CalcH("chlorofluoromethane",        "593-70-4", 6.2516E-03)
    CALL CalcH("dichloromethane",             "75-09-2", 2.4567E-03)
    CALL CalcH("difluoromethane",             "75-10-5", 1.1443E-02)
    CALL CalcH("diiodomethane",               "75-11-6", 3.4133E-04)
    CALL CalcH("formic acid",                 "64-18-6", 7.6622E-07)
    CALL CalcH("methyl bromide",              "74-83-9", 6.7780E-03)
    CALL CalcH("methyl chloride",             "74-87-3", 8.2512E-03)
    CALL CalcH("methyl fluoride",            "593-53-3", 1.3972E-02)
    CALL CalcH("methyl iodide",               "74-88-4", 2.8332E-03)
    CALL CalcH("nitromethane",                "75-52-5", 2.8632E-04)
    CALL CalcH("methane",                     "74-82-8", 6.3640E-01)
    CALL CalcH("methanol",                    "67-56-1", 5.1932E-06)
    CALL CalcH("methyl mercaptan",            "74-93-1", 1.9396E-03)
    CALL CalcH("carbon monoxide",            "630-08-0", 1.1405E+00)
    CALL CalcH("carbonyl sulfide",           "463-58-1", 5.0523E-02)
    CALL CalcH("carbon dioxide",             "124-38-9", 2.1820E-02)
    CALL CalcH("carbon disulfide",            "75-15-0", 1.0471E-01)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, henry)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: henry
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "?"
      CALL Output(1./(atm*henry))

    END SUBROUTINE CalcH

  END SUBROUTINE ref2440

  !---------------------------------------------------------------------------

  SUBROUTINE ref2441 ! Hcc [1] = Ostwald coefficient
    IMPLICIT NONE

    ref = "2441"

    CALL CalcH("trichloromethane",                                      "67-66-3", "L", 4.0    ) ! CHCl3 chloroform
    CALL CalcH("cyclopropane",                                          "75-19-4", "L", 0.21   ) ! C3H6
    CALL CalcH("diethyl ether",                                         "60-29-7", "L", 13.    ) ! C2H5OC2H5
    CALL CalcH("divinyl ether",                                        "109-93-3", "L", 1.4    )
    CALL CalcH("2-chloro-1,1,2-trifluoroethyl difluoromethyl ether", "13838-16-9", "C", 0.78   ) ! C3H2ClF5O enflurane
    CALL CalcH("chloroethane",                                          "75-00-3", "L", 1.2    ) ! C2H5Cl
    CALL CalcH("ethene",                                                "74-85-1", "L", 0.090  ) ! C2H4 ethylene
    CALL CalcH("(2,2,2-trifluoroethoxy)-ethene",                       "406-90-6", "L", 0.85   ) ! CF3CH2OCHCH2 fluoroxene
    CALL CalcH("1-chloro-2,2,2-trifluoroethyl difluoromethyl ether", "26675-46-7", "L", 0.62   ) ! C3H2ClF5O forane; isoflurane
    CALL CalcH("1-bromo-1-chloro-2,2,2-trifluoroethane",               "151-67-7", "L", 0.80   ) ! C2HBrClF3 halothane
    CALL CalcH("krypton",                                             "7439-90-9", "L", 0.051  ) ! Kr
    CALL CalcH("2,2-dichloro-1,1-difluoro-1-methoxyethane",             "76-38-0", "L", 4.5    ) ! C3H4Cl2F2O methoxyflurane
    CALL CalcH("nitrogen",                                            "7727-37-9", "L", 0.0140 ) ! N2
    CALL CalcH("dinitrogen monoxide",                                "10024-97-2", "L", 0.47   ) ! N2O nitrous oxide; laughing gas
    ! the value for teflurane is taken from ref2442:
    !CALL CalcH("1-bromo-1,2,2,2-tetrafluoroethane",                   "124-72-1", "L", 0.32   ) ! C2HBrF4 teflurane
    CALL CalcH("trichloroethene",                                       "79-01-6", "L", 1.7    ) ! C2HCl3 trichloroethylene
    CALL CalcH("xenon",                                               "7440-63-3", "L", 0.085  ) ! Xe

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, ostwald)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: ostwald
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(ostwald, 37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2441

  !---------------------------------------------------------------------------

  SUBROUTINE ref2442 ! Hcc [1] = Ostwald coefficient
    IMPLICIT NONE

    ref = "2442"
    type = "M"
    chem = "1-bromo-1,2,2,2-tetrafluoroethane" ; casrn = "124-72-1" ! C2HBrF4 teflurane
    Hominus = Hcc_TO_HcpSI(0.32, 37.+CtoK)
    CALL MakeNoteOtherTemp("310") ! 37 C
    CALL Output(Hominus)

  END SUBROUTINE ref2442

  !---------------------------------------------------------------------------
  
  ! only gamma_infinity but no vapor pressure to calculate H
  !
  ! SUBROUTINE ref2443
  !   IMPLICIT NONE
  !
  !   ref = "2443"
  !   type = "M"
  !
  !   CALL CalcH("N,N-Dimethylmethanamide", "68-12-2", &
  !     (/ 289.42, 298.0, 307.8, 317.9, 328.3, 338.0 /), &
  !     (/ 0.58, 0.65, 0.81, 0.95, 1.11, 1.30 /)) ! C3H7NO N,N-dimethylformamide
  !   CALL CalcH("dimethylsulfoxide",       "67-68-5", &
  !     (/ 298.8, 308.4, 318.0, 328.4, 337.9 /), &
  !     (/ 0.09, 0.10, 0.12, 0.135, 0.17 /)) ! CH3SOCH3 DMSO
  !   CALL CalcH("N-methyl-2-pyrrolidone", "872-50-4", &
  !     (/ 298.8, 308.4, 318.0, 328.4, 337.9 /), &
  !     (/ 0.37, 0.60, 0.86, 1.13, 1.39 /))
  !
  ! CONTAINS
  !
  !   SUBROUTINE CalcH (chem_, casrn_, temp, gamma_inf)
  !     IMPLICIT NONE
  !     CHARACTER(LEN=*),   INTENT(IN) :: chem_
  !     CHARACTER(LEN=*),   INTENT(IN) :: casrn_
  !     REAL, DIMENSION(:), INTENT(IN) :: temp, gamma_inf
  !     chem  = chem_  ! make value global, so Output will find it
  !     casrn = casrn_ ! make value global, so Output will find it
  !     ndata = SIZE(temp)
  !     ALLOCATE(Harray(ndata))
  !     Harray = ???
  !     CALL HTdep(temp, Harray, Hominus, mindHR)
  !     DEALLOCATE(Harray)
  !     CALL Output(Hominus, mindHR, r2)
  !   END SUBROUTINE CalcH
  ! END SUBROUTINE ref2443
  
  !---------------------------------------------------------------------------
  
  SUBROUTINE ref2444 ! Hcc [1]
    IMPLICIT NONE

    ref = "2444"
    type = "M"

    CALL CalcH("isoflurane",     "26675-46-7", 0.626)
    CALL CalcH("enflurane",      "13838-16-9", 0.754)
    CALL CalcH("halothane",        "151-67-7", 0.859)
    CALL CalcH("methoxyflurane",    "76-38-0", 4.33)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, part_coeff)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: part_coeff
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(part_coeff, 37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2444

  !---------------------------------------------------------------------------

  SUBROUTINE ref2445 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "2445"
    type = "M"


    chem = "2,2-dichloro-1,1-difluoro-1-methoxyethane" ; casrn = "76-38-0" ! C3H4Cl2F2O methoxyflurane
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 8., 20., 30., 37. /) + CtoK
    Harray   = (/ 11.67, 8.08, 5.48, 3.80 /) * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "1-bromo-1-chloro-2,2,2-trifluoroethane" ; casrn = "151-67-7" ! C2HBrClF3 halothane
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 4., 10., 20., 25., 30., 37. /) + CtoK
    Harray   = (/ 4.28, 2.92, 1.60, 1.20, 0.92, 0.63 /) * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "1-chloro-2,2,2-trifluoroethyl difluoromethyl ether" ; casrn = "26675-46-7" ! C3H2ClF5O forane; isoflurane
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 25., 37. /) + CtoK
    Harray   = (/ 1.08, 0.54 /) * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "(2,2,2-trifluoroethoxy)-ethene" ; casrn = "406-90-6" ! CF3CH2OCHCH2 fluoroxene
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 25., 37. /) + CtoK
    Harray   = (/ 1.24, 0.71 /) * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2445

  !---------------------------------------------------------------------------

  SUBROUTINE ref2446 ! Hcc [1]
    IMPLICIT NONE

    ref = "2446"
    type = "M"
    chem = "(2,2,2-trifluoroethoxy)-ethene" ; casrn = "406-90-6" ! CF3CH2OCHCH2 fluoroxene
    Hominus = Hcc_TO_HcpSI(0.84, 37.+CtoK)
    CALL MakeNoteOtherTemp("310") ! 37 C
    CALL Output(Hominus)

  END SUBROUTINE ref2446

  !---------------------------------------------------------------------------

  SUBROUTINE ref2447 ! Hcc [1] = Ostwald coefficient and Hxp [1/atm]
    IMPLICIT NONE

    ref = "2447"

    CALL CalcH("octafluorocyclobutane",  "115-25-3", "L", & ! c-C4F8
      (/ 278.15, 283.15, 288.15, 293.15, 298.15, 303.15, 308.15, 313.15, 318.15 /), &
      (/ 0.728, 0.588, 0.443, 0.363, 0.301, 0.266, 0.238, 0.218, 0.204 /), &
      (/ 5.748, 4.328, 3.376, 2.721, 2.226, 1.937, 1.703, 1.537, 1.420 /) * 1E-6 )

    CALL CalcH("trifluoro(trifluoromethyl)-oxirane", "428-59-1", "C", &
      (/ 283.2, 293.2, 303.2, 313.2, 323.2, 333.2 /), &
      (/ 0.0317, 0.0259, 0.0195, 0.0131, 0.0131, 0.0067 /), &
      (/ 0.246, 0.194, 0.142, 0.112, 0.090, 0.045 /) * 1E-4 )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, temp, ostwald, x)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL, DIMENSION(:), INTENT(IN) :: temp, ostwald, x
      REAL :: H_from_ostwald, mindHR_from_ostwald
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      CALL HTdep(temp, Hcc_TO_HcpSI(ostwald, temp), H_from_ostwald, mindHR_from_ostwald, "fromostwald")
      CALL HTdep(temp, x * Hxp_TO_HcpSI, Hominus, mindHR)
      ! compare values:
      !print *, "from x: ", Hominus, mindHR
      !print *, "from L: ", H_from_ostwald, mindHR_from_ostwald
      !print *, "%diff = ", 100.*(Hominus-H_from_ostwald)/Hominus, 100.*(mindHR-mindHR_from_ostwald)/mindHR
      IF (casrn_=="115-25-3") THEN
        CALL MakeNote(TRIM(ref)//"-"//casrn_, &
          "In their Table 13, "//TRIM(citet())//" list Ostwald coefficients "// &
          "that are probably incorrect by a factor of 100. Therefore, these "// &
          "values are not used. Instead, $\H$ is calculated using the mol "// &
          "fraction $x_1$ from the same table.")
      ENDIF
      IF (casrn_=="428-59-1") THEN
        CALL MakeNote(TRIM(ref)//"-"//casrn_, &
          "The Ostwald coefficient given by "//TRIM(citet())//" at "// &
          "313.2~\unit{K} is probably incorrect. Therefore, the Ostwald "// &
          "coefficients are not used. Instead, $\H$ is calculated "// &
          "using the mol fraction $x_1$ from the same table.")
      ENDIF
      CALL Output(Hominus, mindHR, r2)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2447

  !---------------------------------------------------------------------------

  SUBROUTINE ref2448 ! Hcc [1]
    IMPLICIT NONE

    ref = "2448"
    type = "M"

    ! Tab. 1:
    CALL CalcH("dichloromethane",             "75-09-2",   14.  ) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",            "67-66-3",    7.  ) ! CHCl3 chloroform
    CALL CalcH("tetrachloromethane",          "56-23-5",    0.8 ) ! CCl4 carbontetrachloride
    CALL CalcH("trichloroethene",             "79-01-6",    2.4 ) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",          "127-18-4",    1.9 ) ! C2Cl4 tetrachloroethylene
    CALL CalcH("benzene",                     "71-43-2",    5.  ) ! C6H6
    CALL CalcH("chlorobenzene",              "108-90-7",    7.  ) ! C6H5Cl
    CALL CalcH("1,3,5-trichlorobenzene",     "108-70-3",   87.  ) ! C6H3Cl3
    CALL CalcH("1,2,3,4-tetrachlorobenzene", "634-66-2",  140.  ) ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",         "608-93-5",  500.  ) ! C6HCl5
    CALL CalcH("hexachlorobenzene",          "118-74-1", 6300.  ) ! C6Cl6
    CALL CalcH("nitrobenzene",                "98-95-3",  350.  ) ! C6H5NO2
    CALL CalcH("2-chloronitrobenzene",        "88-73-3",  550.  ) ! C6H4ClNO2 $o$-chloronitrobenzene
    CALL CalcH("4-chloronitrobenzene",       "100-00-5",  450.  ) ! C6H4ClNO2 $p$-chloronitrobenzene
    CALL CalcH("hydroxypentachlorobenzene",   "87-86-5",  1E6   ) ! C6HCl5O pentachlorophenol
    ! Tab. 3:
    CALL CalcH("pentyl ethanoate",           "628-63-7",   83.  ) ! CH3COOC5H{11} amyl acetate
    CALL CalcH("4-methyl-2-pentanone",       "108-10-1",  160.  ) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("butanone",                    "78-93-3",  330.  ) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("propanone",                   "67-64-1",  350.  ) ! CH3COCH3 acetone
    CALL CalcH("2,4-pentanedione",           "123-54-6", 4200.  ) ! C5H8O2 acetylacetone

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, part_coeff)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: part_coeff
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(part_coeff, 22.+CtoK)
      CALL MakeNoteOtherTemp("295") ! 22 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2448

  !---------------------------------------------------------------------------

  SUBROUTINE ref2449 ! Hcc [1]
    IMPLICIT NONE

    ref = "2449"
    type = "M"

    CALL CalcH("butanone",      "78-93-3",  437.) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("ethanol",       "64-17-5", 4580.) ! C2H5OH
    CALL CalcH("1,4-dioxane",  "123-91-1", 5096.) ! C4H8O2 dioxane
    CALL CalcH("nitromethane",  "75-52-5",  886.) ! CH3NO2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, part_coeff)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: part_coeff
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(part_coeff, 22.+CtoK)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2449

  !---------------------------------------------------------------------------

  SUBROUTINE ref2450 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2450"
    type = "M"

    CALL CalcH("dichloromethane",           "75-09-2", 10.8) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",          "67-66-3", 17.1) ! CHCl3 chloroform
    CALL CalcH("1,1-dichloroethane",        "75-34-3", 25.6) ! CHCl2CH3
    CALL CalcH("1,2-dichloroethane",       "107-06-2", 5.24) ! CH2ClCH2Cl
    CALL CalcH("1,1,1-trichloroethane",     "71-55-6", 70.0) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1,2-trichloroethane",     "79-00-5", 3.67) ! CHCl2CH2Cl
    CALL CalcH("1,1,2,2-tetrachloroethane", "79-34-5", 1.87) ! CHCl2CHCl2
    CALL CalcH("1,2-dichloropropane",       "78-87-5", 12.1) ! C3H6Cl2
    CALL CalcH("(Z)-1,2-dichloroethene",   "156-59-2", 17.5) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("(E)-1,2-dichloroethene",   "156-60-5", 42.3) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",           "79-01-6", 41.1) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",        "127-18-4", 65.5) ! C2Cl4 tetrachloroethylene
    CALL CalcH("chlorobenzene",            "108-90-7", 15.7) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",       "95-50-1", 7.65) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",      "541-73-1", 11.7) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",      "106-46-7", 10.3) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("(chloromethyl)-benzene",   "100-44-7", 1.96) ! C6H5CH2Cl benzylchloride
    CALL CalcH("dibromomethane",            "74-95-3", 3.65) ! CH2Br2
    CALL CalcH("tribromomethane",           "75-25-2", 2.38) ! CHBr3 bromoform
    CALL CalcH("1,2-dibromoethane",        "106-93-4", 2.87) ! C2H4Br2 ethylene dibromide
    CALL CalcH("bromobenzene",             "108-86-1", 9.12) ! C6H5Br

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, H12)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL,             INTENT(IN) :: H12
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("293")
      CALL Output(1E-6 * cH2O / H12)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2450

  !---------------------------------------------------------------------------

  SUBROUTINE ref2451 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "2451"
    type = "M"

    ! from Table 3:
    CALL CalcH3("hydroxybenzene",         "108-95-2", & ! phenol
      (/ 313.24, 323.15, 333.15, 343.18, 353.12, 363.14 /), (/  1.9,   5.0,  11.2,  19.7,  28.1, 55.5 /) )
    CALL CalcH3("2-hydroxychlorobenzene",  "95-57-8", & ! o-chlorophenol
      (/ 323.15, 333.39, 343.09, 353.08, 363.09         /), (/ 66.3, 114.8, 191.1, 300.4, 453.9       /) )
    CALL CalcH3("3-hydroxychlorobenzene", "108-43-0", & ! m-chlorophenol
      (/ 323.18, 333.52, 343.17, 353.22, 363.22         /), (/  8.5,  16.0,  28.1,  46.5,  76.9       /) )
    CALL CalcH3("4-hydroxychlorobenzene", "106-48-9", & ! p-chlorophenol
      (/ 323.12, 333.52, 343.21, 353.23, 363.15         /), (/  0.5,   2.7,   6.1,  12.9,  24.7       /) )
    CALL CalcH3("2,4-dichlorophenol",     "120-83-2", &
      (/ 323.10, 333.48, 343.16, 353.18, 363.21         /), (/ 48.2,  94.0, 170.3, 296.6,  486.9      /) )

    ! data from Table 4 is not used because the fit functions can not be
    ! extrapolated very well to 298 K, see ref2451.gnu
    ! CALL CalcH4("hydroxybenzene",         "108-95-2", 670.117,   -39274.5,   -94.6679 ) ! C6H5OH phenol
    ! CALL CalcH4("2-hydroxychlorobenzene",  "95-57-8", 161.250,   -12658.0,   -20.4027 ) ! C6H5ClO $o$-chlorophenol
    ! CALL CalcH4("3-hydroxychlorobenzene", "108-43-0", 22.0921,   -6444.46,   0.       ) ! C6H5ClO $m$-chlorophenol
    ! CALL CalcH4("4-hydroxychlorobenzene", "106-48-9", 2017.07,   -110385.,   -290.078 ) ! C6H5ClO $p$-chlorophenol
    ! CALL CalcH4("2,4-dichlorophenol",     "120-83-2", 24.9070,   -6791.07,   0.       ) ! C6H4Cl2O

  CONTAINS

    SUBROUTINE CalcH3 (chem_, casrn_, temp, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, H

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, cH2O/(H*1.E3), Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH3

    SUBROUTINE CalcH4 (chem_, casrn_, A1, A2, A3)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A1, A2, A3
      REAL :: H ! Henry constant in [kPa]

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! T-dep with 3 parameters:
      ! ln(H) = A1 + A2/T + A3*ln(T)
      H = EXP(A1 + A2/T0 + A3*LOG(T0))
      ! analytical derivative:
      ! d ln(H) / d (1/T) = A2 - A3*T
      mindHR = A3*T0 - A2
      CALL Output(cH2O/(H*1.E3), mindHR)
    END SUBROUTINE CalcH4

  END SUBROUTINE ref2451

  !---------------------------------------------------------------------------

  SUBROUTINE ref2452 ! KHcc [1]
    IMPLICIT NONE

    ref = "2452"
    type = "V"

    CALL CalcH("diazinon",              "333-41-5", 60.  ) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("parathion",              "56-38-2", 9.5  ) ! C{10}H{14}NO5PS
    CALL CalcH("chlorpyrifos",         "2921-88-2", 500. ) ! C9H{11}Cl3NO3PS
    CALL CalcH("methidathion",          "950-37-8", 0.07 )
    CALL CalcH("malathion",             "121-75-5", 2.4  ) ! C{10}H{19}O6PS2
    CALL CalcH("methylparathion",       "298-00-0", 4.4  ) ! C{8}H{10}NO5PS
    CALL CalcH("paraoxon",              "311-45-5", 0.25 )
    CALL CalcH("DEF",                    "78-48-8", 320. )
    CALL CalcH("atrazine",             "1912-24-9", 0.2  ) ! C8H{14}ClN5
    CALL CalcH("simazine",              "122-34-9", 0.025) ! C7H{12}ClN5
    CALL CalcH("penoxaline",          "40487-42-1", 1500.)
    CALL CalcH("alachlor",            "15972-60-8", 1.3  ) ! C{14}H{20}ClNO2
    CALL CalcH("metolachlor",         "51218-45-2", 0.37 ) ! C{15}H{22}ClNO2
    CALL CalcH("tri-n-butylphosphate",  "126-73-8", 26.  )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 *1E6 / KHcc
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2452

  !---------------------------------------------------------------------------

  SUBROUTINE ref2453 ! KHpx [1E7 Pa]
    IMPLICIT NONE

    ref = "2453"
    type = "M"

    ! from Table 2:
    CALL CalcH("benzene",                 "71-43-2", (/ 5., 15., 25., 35., 45. /), (/ 1.25, 2.15, 3.38, 5.03, 7.34 /) ) ! C6H6
    CALL CalcH("methylbenzene",          "108-88-3", (/ 15., 25., 35., 45. /),     (/ 2.20, 3.69, 5.89, 8.73 /)       ) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",           "100-41-4", (/ 15., 25., 35., 45. /),     (/ 2.55, 4.43, 7.44, 11.2 /)       ) ! C6H5C2H5
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6", (/ 15., 25., 35., 45. /),     (/ 1.66, 2.92, 4.69, 7.35 /)       ) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",    "108-38-3", (/ 15., 25., 35., 45. /),     (/ 2.25, 4.06, 6.83, 10.4 /)       ) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",    "106-42-3", (/ 15., 25., 35., 45. /),     (/ 2.39, 4.24, 7.03, 11.4 /)       ) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("propylbenzene",          "103-65-1", (/ 15., 25., 35., 45. /),     (/ 3.30, 5.90, 10.1, 15.3 /)       ) ! C6H5C3H7
    CALL CalcH("(2-propyl)-benzene",      "98-82-8", (/ 15., 25., 35., 45. /),     (/ 3.50, 6.47, 10.6, 16.6 /)       ) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("1,2,3-trimethylbenzene", "526-73-8", (/ 15., 25., 35., 45. /),     (/ 1.34, 2.45, 3.92, 5.88 /)       ) ! C6H3(CH3)3
    CALL CalcH("1,2,4-trimethylbenzene",  "95-63-6", (/ 15., 25., 35., 45. /),     (/ 1.93, 3.44, 5.79, 9.24 /)       ) ! C6H3(CH3)3
    CALL CalcH("1,3,5-trimethylbenzene", "108-67-8", (/ 15., 25., 35., 45. /),     (/ 2.84, 4.93, 8.14, 13.3 /)       ) ! C6H3(CH3)3 mesitylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp+CtoK, cH2O/(H*1.E7), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2453

  !---------------------------------------------------------------------------

  SUBROUTINE ref2454 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2454"

    CALL CalcH("fenitrothion",    "122-14-5", 6.6E-7, 9.3E-7) ! C9H{12}NO5PS
    CALL CalcH("methylparathion", "298-00-0", 6.0E-8, 1.0E-7) ! C{8}H{10}NO5PS

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, est_H, expt_H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: est_H, expt_H

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      type = "V"
      CALL Output(1./(atm*est_H))

      type = "M"
      CALL Output(1./(atm*expt_H))

    END SUBROUTINE CalcH

  END SUBROUTINE ref2454

  !---------------------------------------------------------------------------

  SUBROUTINE ref2455 ! KHpx [mmHg/ppm]
    IMPLICIT NONE

    ref = "2455"

    ! example conversion to M/atm for methylparathion:
    ! (57e-6 * 55.5) / (11.2e-6 / 760) => 214666.071428
    ! (solub * cH2O) / (press  / mmHg)

    ! Table 1:
    CALL CalcH("mevinphos",       "7786-34-7", 1.76E-9)
    CALL CalcH("trifluralin",     "1582-09-8", 1.08E-4) ! C{13}H{16}F3N3O4
    CALL CalcH("diazinon",         "333-41-5", 4.05E-6) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("methylparathion",  "298-00-0", 1.96E-7) ! C{8}H{10}NO5PS
    CALL CalcH("malathion",        "121-75-5", 5.68E-8) ! C{10}H{19}O6PS2
    CALL CalcH("parathion",         "56-38-2", 2.52E-7) ! C{10}H{14}NO5PS

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpx)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpx
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "V"
      CALL MakeNoteOtherTemp("295")
      CALL Output(cH2O / (1E6 * mmHg * KHpx))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2455

  !---------------------------------------------------------------------------

  SUBROUTINE ref2456 ! KHcc [1]
    IMPLICIT NONE

    ref = "2456"

    CALL CalcH("octamethylcyclotetrasiloxane", "556-67-2", 3.4,  "M")
    ! the next value can also be calculated and converted to M/atm with:
    ! 2.5e-7 / (0.681 / 760) => 2.79e-4
    CALL CalcH("octamethylcyclotetrasiloxane", "556-67-2", 148., "V")

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc, type_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      CHARACTER(LEN=*), INTENT(IN) :: type_
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      CALL MakeNoteOtherTemp("293")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2456

  !---------------------------------------------------------------------------

  SUBROUTINE ref2457 ! Hcc [1]
    IMPLICIT NONE

    ref = "2457"
    type = "M"

    CALL CalcH("methanol",                  "67-56-1",  3.744) ! CH3OH
    CALL CalcH("ethanol",                   "64-17-5",  3.662) ! C2H5OH
    CALL CalcH("1-propanol",                "71-23-8",  3.539) ! C3H7OH
    CALL CalcH("1-butanol",                 "71-36-3",  3.451) ! C4H9OH
    CALL CalcH("1-pentanol",                "71-41-0",  3.317) ! C5H{11}OH amylalcohol
    CALL CalcH("1-hexanol",                "111-27-3",  3.200) ! C6H{13}OH
    CALL CalcH("ethane nitrile",            "75-05-8",  3.060) ! CH3CN acetonitrile
    CALL CalcH("propane nitrile",          "107-12-0",  2.788) ! C2H5CN propionitrile
    CALL CalcH("butane nitrile",           "109-74-0",  2.680) ! C3H7CN butyronitrile
    CALL CalcH("2-methylpropane nitrile",   "78-82-0",  2.368) ! C4H7N isobutyronitrile
    CALL CalcH("valeronitrile",            "110-59-8",  2.547)
    CALL CalcH("diisopropyl ether",        "108-20-3",  1.015) ! C3H7OC3H7
    CALL CalcH("dipropyl ether",           "111-43-3",  0.872) ! C3H7OC3H7
    CALL CalcH("dibutyl ether",            "142-96-1",  0.735) ! C4H9OC4H9
    CALL CalcH("methoxybenzene",           "100-66-3",  1.901) ! C6H5OCH3 anisole
    CALL CalcH("ethoxybenzene",            "103-73-1",  1.631) ! C8H{10}O phenetole
    CALL CalcH("octanal",                  "124-13-0",  1.715) ! C7H{15}CHO
    CALL CalcH("2-nonanone",               "821-55-6",  2.012) ! C7H{15}COCH3
    CALL CalcH("3-nitrotoluene",            "99-08-1",  2.840) ! C6H4(NO2)CH3
    CALL CalcH("1,1,2,2-tetrachloroethane", "79-34-5",  1.851) ! CHCl2CHCl2
    CALL CalcH("tetrachloromethane",        "56-23-5", -0.237) ! CCl4 carbontetrachloride
    CALL CalcH("1,2-dichlorobenzene",       "95-50-1",  1.071) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("(chloromethyl)-benzene",   "100-44-7",  1.460) ! C6H5CH2Cl benzylchloride
    CALL CalcH("fluorobenzene",            "462-06-6",  0.436) ! C6H5F
    CALL CalcH("chlorobenzene",            "108-90-7",  0.796) ! C6H5Cl
    CALL CalcH("bromobenzene",             "108-86-1",  1.040) ! C6H5Br
    CALL CalcH("iodobenzene",              "591-50-4",  1.275) ! C6H5I
    CALL CalcH("benzene",                   "71-43-2",  0.630) ! C6H6
    CALL CalcH("methylbenzene",            "108-88-3",  0.595) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",             "100-41-4",  0.507) ! C6H5C2H5
    CALL CalcH("propylbenzene",            "103-65-1",  0.361) ! C6H5C3H7
    CALL CalcH("butylbenzene",             "104-51-8",  0.244) ! C6H5C4H9
    CALL CalcH("1,2-dimethylbenzene",       "95-47-6",  0.681) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",      "108-38-3",  0.502) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",      "106-42-3",  0.495) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("(2-propyl)-benzene",        "98-82-8",  0.354) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("1,3,5-trimethylbenzene",   "108-67-8",  0.508) ! C6H3(CH3)3 mesitylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logKw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcc_TO_HcpSI_atT0*10.**(logKw))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2457

  !---------------------------------------------------------------------------

  SUBROUTINE ref2458 ! Hcc [1]
    IMPLICIT NONE

    ref = "2458"
    type = "M"

    ! deltaH/R and a from Tab. 2 could not be reproduced. Instead, data
    ! from Tab. 1 is used here:
    CALL CalcH("naphthalene",           "91-20-3", &
      (/ 21.9, 25.3, 16.6, 8.4, 10.7, 18.5, 30.3, 23.1 /) + CtoK, &
      (/ 67., 53., 90., 146., 124., 80., 39., 60. /)) ! C{10}H8
    CALL CalcH("1-methylnaphthalene",   "90-12-0", &
      (/ 25.3, 16.6, 8.4, 30.3, 23.1 /) + CtoK, &
      (/ 71., 110., 166., 51., 73. /)) ! C{10}H7CH3
    CALL CalcH("1-ethylnaphthalene",  "1127-76-0", &
      (/ 23.1, 21.9, 13.2, 8.4, 25.3, 27.5, 30.3 /) + CtoK, &
      (/ 60., 67., 99., 134., 53., 48., 42. /)) ! C{10}H7C2H5

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, HRT)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, HRT

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(temp)
      ALLOCATE(Harray(ndata))
      Harray = Hcc_TO_HcpSI(HRT,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2458

  !---------------------------------------------------------------------------

  SUBROUTINE ref2459 ! KHcc [1]
    IMPLICIT NONE

    ref = "2459"

    ! experimental:
    CALL CalcH("naphthalene",        "91-20-3", "M", 2.26E-2) ! C{10}H8
    CALL CalcH("anthracene",        "120-12-7", "M", 2.66E-3) ! C{14}H{10}
    CALL CalcH("phenanthrene",       "85-01-8", "M", 2.24E-3) ! C{14}H{10}
    CALL CalcH("pyrene",            "129-00-0", "M", 7.64E-4) ! C{16}H{10}
    CALL CalcH("benz[a]anthracene",  "56-55-3", "M", 3.28E-4) ! C{18}H{12}

    ! from ref2458:
    !CALL CalcH("naphthalene",       "91-20-3", "V", 1.9E-2) ! C{10}H8
    ! vapor pressure / solubility:
    CALL CalcH("anthracene",        "120-12-7", "V", 12E-3)  ! C{14}H{10}
    CALL CalcH("phenanthrene",       "85-01-8", "V", 2.0E-3) ! C{14}H{10}
    CALL CalcH("pyrene",            "129-00-0", "V", 4.3E-4) ! C{16}H{10}
    CALL CalcH("benz[a]anthracene",  "56-55-3", "V", 1.0E-4) ! C{18}H{12}
    CALL CalcH("benzo[a]pyrene",     "50-32-8", "V", 2.1E-5) ! C{20}H{12}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0/H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2459

  !---------------------------------------------------------------------------

  SUBROUTINE ref2460 ! Hcc [1]
    IMPLICIT NONE

    ref = "2460"
    type = "M"

    CALL CalcH("$\alpha$-pinene",   "80-56-8", 0.12) ! C{10}H{16}
    CALL CalcH("$\beta$-pinene",   "127-91-3", 0.12) ! C{10}H{16}
    CALL CalcH("3-carene",       "13466-78-9", 0.41)
    CALL CalcH("limonene",         "138-86-3", 1.8 )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, part_coeff)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: part_coeff
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(part_coeff, 37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2460

  !---------------------------------------------------------------------------

  SUBROUTINE ref2462 ! KHpx [Pa]
    IMPLICIT NONE

    ref = "2462"
    type = "M"

    CALL CalcH("limonene",                  "138-86-3", 7.88E7)
    CALL CalcH("$\alpha$-pinene",            "80-56-8", 7.94E8) ! C{10}H{16}
    CALL CalcH("myrcene",                   "123-35-3", 6.33E7)
    CALL CalcH("1S-endo-(-)-borneol",       "464-45-9", 1.24E5)
    CALL CalcH("endo-(+)-fenchyl alcohol", "2217-02-9", 1.55E5)
    CALL CalcH("(-)-alpha-pinene oxide",   "1686-14-2", 2.38E6)
    CALL CalcH("(+)-limonene oxide",        "470-82-6", 9.96E5)
    CALL CalcH("beta-ionone",             "14901-07-6", 4.54E4)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, gamma_p0)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL,             INTENT(IN) :: gamma_p0
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      CALL Output(cH2O / gamma_p0)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2462

  !---------------------------------------------------------------------------

  SUBROUTINE ref2463 ! KHcc [1]
    IMPLICIT NONE

    ref = "2463"

    CALL CalcH("benzene",               "71-43-2", (/ 10., 15., 25., 35. /), (/ 0.122, 0.163, 0.258, 0.385 /))
    CALL CalcH("methylbenzene",        "108-88-3", (/ 11., 15., 25., 35. /), (/ 0.159, 0.192, 0.273, 0.469 /))
    CALL CalcH("ethylbenzene",         "100-41-4", (/ 25. /),                (/ 0.361 /))
    CALL CalcH("1,2-dimethylbenzene",   "95-47-6", (/ 25. /),                (/ 0.295 /))
    CALL CalcH("trichloromethane",      "67-66-3", (/ 15., 25. /),           (/ 0.109, 0.182 /))
    CALL CalcH("1,1,1-trichloroethane", "71-55-6", (/ 25. /),                (/ 0.836 /))
    CALL CalcH("trichloroethene",       "79-01-6", (/ 25. /),                (/ 0.473 /))

    ! Data from Tab. 14 is not used because the fit function is very unusual and some produce
    ! decreasing H at low T. See ref2463.gnu for plots.
    !CALL CalcH14("benzene",               "71-43-2", 0.0763,  0.00211, 0.000162) ! C6H6
    !CALL CalcH14("methylbenzene",        "108-88-3", 0.115,  -0.00474, 0.000466) ! C6H5CH3 toluene
    !CALL CalcH14("ethylbenzene",         "100-41-4", 0.0500,  0.00487, 0.000250) ! C6H5C2H5
    !CALL CalcH14("1,2-dimethylbenzene",   "95-47-6", 0.0353,  0.00444, 0.000131) ! C6H4(CH3)2 $o$-xylene
    !CALL CalcH14("1,3-dimethylbenzene",  "108-38-3", 0.0683,  0.00292, 0.000255) ! C6H4(CH3)2 $m$-xylene
    !CALL CalcH14("1,4-dimethylbenzene",  "106-42-3", 0.146,  -0.00349, 0.000392) ! C6H4(CH3)2 $p$-xylene
    !CALL CalcH14("trichloromethane",      "67-66-3", 0.0394,  0.00486, 0.      ) ! CHCl3 chloroform
    !CALL CalcH14("1,1,1-trichloroethane", "71-55-6", 0.204,   0.0182,  0.000173) ! CH3CCl3 methylchloroform; MCF
    !CALL CalcH14("trichloroethene",       "79-01-6", 0.151,  -0.00597, 0.000680) ! C2HCl3 trichloroethylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, part_coeff)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_
      REAL, DIMENSION(:), INTENT(IN) :: part_coeff
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = "M"
      ndata = SIZE(part_coeff)
      IF (ndata>1) THEN
        ALLOCATE(Harray(ndata))
        Harray = KHcc_TO_HcpSI(part_coeff,temp_+CtoK)
        CALL HTdep(temp_+CtoK, Harray, Hominus, mindHR)
        DEALLOCATE(Harray)
        CALL Output(Hominus, mindHR, r2)
      ELSE
        CALL Output(KHcc_TO_HcpSI(part_coeff(1),temp_(1)+CtoK))
      ENDIF
    END SUBROUTINE CalcH

    SUBROUTINE CalcH14 (chem_, casrn_, a, b, c)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: a, b, c
      REAL  :: aa, bb, cc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = "L"
      aa = a - CtoK*b + CtoK**2*c
      bb = b - 2.*CtoK*c
      cc = c
      Hominus = KHcc_TIMES_HcpSI_atT0 / (aa + bb*T0 + cc*T0**2)
      mindHR = (bb*T0**2 + 2.*cc*T0**3) / (aa + bb*T0 + cc*T0**2) + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH14

  END SUBROUTINE ref2463

  !---------------------------------------------------------------------------

  SUBROUTINE ref2464 ! Hcp [M/Pa]
    IMPLICIT NONE

    ref = "2464"
    type = "V"

    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 6., 23.5 /) + CtoK

    CALL CalcH("$\alpha$-pinene",                   "80-56-8", (/ 16.7E-6  / 59.7  , 18.3E-6  / 544. /) ) ! C{10}H{16}
    CALL CalcH("limonene",                         "138-86-3", (/ 31.8E-6  / 18.4  , 41.0E-6  / 202. /) )
    CALL CalcH("gamma-terpinene",                   "99-85-4", (/ 44.7E-6  / 13.4  , 63.7E-6  / 103. /) )
    CALL CalcH("terpinolene",                      "586-62-9", (/ 56.7E-6  / 5.97  , 69.6E-6  / 99.0 /) )
    CALL CalcH("arbanol",                         "7070-15-7", (/ 1523.E-6 / 0.582 , 2911.E-6 / 2.66 /) )
    CALL CalcH("alpha-terpineol",                   "98-55-5", (/ 2202.E-6 / 0.862 , 4600.E-6 / 5.69 /) )
    CALL CalcH("3,7-dimethyl-1,6-octadien-3-ol",    "78-70-6", (/ 3570.E-6 / 0.751 , 5530.E-6 / 21.2 /) ) ! linalool
    CALL CalcH("plinol",                         "72402-00-7", (/ 5281.E-6 / 0.266 , 9610.E-6 / 18.0 /) )

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, ratio)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: ratio
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Harray = 1E3 * ratio ! convert M to mol/m3
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2464

  !---------------------------------------------------------------------------

  SUBROUTINE ref2465 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2465"
    type = "M"
    chem = "dieldrin" ; casrn = "60-57-1" ! C{12}H8OCl6
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./(atm*2.9E-5))

  END SUBROUTINE ref2465

  !---------------------------------------------------------------------------

  SUBROUTINE ref2467 ! Hcc [1]
    IMPLICIT NONE

    ref = "2467"
    type = "M"

    CALL CalcH("fluoroxene",     "406-90-6", 0.83)
    CALL CalcH("halothane",      "151-67-7", 0.82)
    CALL CalcH("methoxyflurane",  "76-38-0", 4.65)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, part_coeff)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: part_coeff
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(part_coeff, 37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2467

  !---------------------------------------------------------------------------

  SUBROUTINE ref2468 ! KHcc [1]
    IMPLICIT NONE

    ref = "2468"
    type = "M"

    CALL CalcH("BDE-28",   "41318-75-6", 17.8, -7125.)
    CALL CalcH("BDE-47",    "5436-43-1", 15.9, -7029.)
    CALL CalcH("BDE-100", "189084-64-8", 12.7, -6515.)
    CALL CalcH("BDE-99",   "60348-60-9", 20.3, -8513.)
    CALL CalcH("BDE-154", "207122-15-4", 12.0, -6501.)
    CALL CalcH("BDE-153",  "68631-49-2", 16.0, -7475.)
    CALL CalcH("BDE-209",   "1163-19-5", 14.8, -7601.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / EXP(A+B/T0)
      mindHR = -B + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2468

  !---------------------------------------------------------------------------

  SUBROUTINE ref2469 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2469"
    type = "V"

    CALL CalcH("dbp-br3cl3a", "400766-93-0", 0.14  )
    CALL CalcH("dbp-br3cl3b", "666856-68-4", 0.030 )
    CALL CalcH("dbp-br4cl2",  "253798-64-0", 0.036 )
    CALL CalcH("dbp-br5cl",   "400767-00-2", 0.0068)
    CALL CalcH("dbp-br6",     "253798-63-9", 0.0020)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H25)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H25
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/H25)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2469

  !---------------------------------------------------------------------------

  SUBROUTINE ref2471 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2471"
    type = "V"

    CALL CalcH("octane",                 "111-65-9", 3.21)    ! C8H{18}
    CALL CalcH("2,2,4-trimethylpentane", "540-84-1", 3.04)    ! C8H{18} isooctane
    CALL CalcH("methylbenzene",          "108-88-3", 6.68E-3) ! C6H5CH3 toluene
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6", 5.27E-3) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("cumene",                  "98-82-8", 1.46E-2) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("naphthalene",             "91-20-3", 1.18E-3) ! C{10}H8
    CALL CalcH("biphenyl",                "92-52-4", 1.55E-3) ! (C6H5)2
    CALL CalcH("DDT",                     "50-29-3", 3.89E-5) ! C{14}H9Cl5 DDT
    ! assuming that "lindane" refers to gamma-lindane:
    CALL CalcH("lindane",                 "58-89-9", 4.93E-7) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("dieldrin",                "60-57-1", 2.00E-7) ! C{12}H8OCl6
    CALL CalcH("aldrin",                 "309-00-2", 1.44E-5) ! C{12}H8Cl6
    ! aroclor is a mixture, not a pure substance:
    !CALL CalcH("aroclor1242",          "53469-21-9", 5.73E-4) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1248",          "12672-29-6", 3.51E-3) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1254",          "11097-69-1", 2.76E-3) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1260",          "11096-82-5", 7.13E-3) ! C{12}HxCl{(10-x)}
    CALL CalcH("mercury",               "7439-97-6", 1.14E-2) ! Hg

    ! for benzene, values at 2 temperatures are available:
    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10., 25. /) + CtoK
    Harray = 1. / ( atm * (/ 2.67E-3, 5.5E-3 /) )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*KHpc)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2471

  !---------------------------------------------------------------------------

  SUBROUTINE ref2472 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2472"
    type = "M"

    CALL CalcH("PCB-3",    "2051-62-9", 5.84,  56E3)
    CALL CalcH("PCB-4",   "13029-08-8", 8.02,  50E3)
    CALL CalcH("PCB-5",   "16605-91-7", 8.97,  48E3)
    CALL CalcH("PCB-6",   "25569-80-6", 9.14,  47E3)
    CALL CalcH("PCB-7",   "33284-50-3", 11.5,  43E3)
    CALL CalcH("PCB-13",   "2974-90-5", 7.44,  51E3)
    CALL CalcH("PCB-16",  "38444-78-9", 9.36,  47E3)
    CALL CalcH("PCB-17",  "37680-66-3", 13.9,  39E3)
    CALL CalcH("PCB-19",  "38444-73-4", 13.9,  39E3)
    CALL CalcH("PCB-22",  "38444-85-8", 13.2,  40E3)
    CALL CalcH("PCB-24",  "55702-45-9", 14.1,  39E3)
    CALL CalcH("PCB-25",  "55712-37-3", 14.0,  39E3)
    CALL CalcH("PCB-26",  "38444-81-4", 12.6,  41E3)
    CALL CalcH("PCB-31",  "16606-02-3", 12.9,  41E3)
    CALL CalcH("PCB-33",  "38444-86-9", 11.9,  42E3)
    CALL CalcH("PCB-40",  "38444-93-8", 15.4,  30E3)
    CALL CalcH("PCB-42",  "36559-22-5", 21.4,  26E3)
    CALL CalcH("PCB-45",  "70362-45-7", 25.1,  24E3)
    CALL CalcH("PCB-46",  "41464-47-5", 18.8,  28E3)
    CALL CalcH("PCB-48",  "70362-47-9", 22.8,  25E3)
    CALL CalcH("PCB-49",  "41464-40-8", 22.8,  25E3)
    CALL CalcH("PCB-56",  "41464-43-1", 13.9,  32E3)
    CALL CalcH("PCB-63",  "74472-34-7", 24.5,  25E3)
    CALL CalcH("PCB-64",  "52663-58-8", 24.7,  24E3)
    CALL CalcH("PCB-70",  "32598-11-1", 17.2,  29E3)
    CALL CalcH("PCB-74",  "32690-93-0", 23.1,  25E3)
    CALL CalcH("PCB-81",  "70362-50-4", 12.7,  33E3)
    CALL CalcH("PCB-82",  "52663-62-4", 16.3,  42E3)
    CALL CalcH("PCB-83",  "60145-20-2", 23.6,  30E3)
    CALL CalcH("PCB-85",  "65510-45-4", 26.4,  26E3)
    CALL CalcH("PCB-89",  "73575-57-2", 30.6,  21E3)
    CALL CalcH("PCB-91",  "68194-05-8", 42.2,  10E3)
    CALL CalcH("PCB-92",  "52663-61-3", 28.4,  24E3)
    CALL CalcH("PCB-95",  "38379-99-6", 30.8,  21E3)
    CALL CalcH("PCB-97",  "41464-51-1", 23.5,  30E3)
    CALL CalcH("PCB-99",  "38380-01-7", 35.4,  16E3)
    CALL CalcH("PCB-107", "70424-68-9", 16.1,  18E3)
    CALL CalcH("PCB-110", "38380-03-9", 18.5,  43E3)
    CALL CalcH("PCB-119", "56558-17-9", 31.8,  38E3)
    CALL CalcH("PCB-132", "38380-05-1", 16.6,  20E3)
    CALL CalcH("PCB-134", "52704-70-8", 25.6,  61E3)
    CALL CalcH("PCB-135", "52744-13-5", 26.8,  46E3)
    CALL CalcH("PCB-136", "38411-22-2", 45.4,  45E3)
    CALL CalcH("PCB-137", "35694-06-5", 13.0,  27E3)
    CALL CalcH("PCB-141", "52712-04-6", 12.7,  70E3)
    CALL CalcH("PCB-146", "51908-16-8", 17.8,  59E3)
    CALL CalcH("PCB-149", "38380-04-0", 25.9,  46E3)
    CALL CalcH("PCB-151", "52663-63-5", 33.4,  37E3)
    CALL CalcH("PCB-156", "38380-08-4", 3.72, 112E3)
    CALL CalcH("PCB-157", "69782-90-7", 2.27, 129E3)
    CALL CalcH("PCB-158", "74472-42-7", 9.64,  80E3)
    CALL CalcH("PCB-163", "74472-44-9", 9.37,  81E3)
    CALL CalcH("PCB-167", "52663-72-6", 4.47, 106E3)
    CALL CalcH("PCB-169", "32774-16-6", 0.85, 162E3)
    CALL CalcH("PCB-174", "38411-25-5", 4.91, 113E3)
    CALL CalcH("PCB-177", "52663-70-4", 5.12, 112E3)
    CALL CalcH("PCB-178", "52663-67-9", 11.2,  90E3)
    CALL CalcH("PCB-182", "60145-23-5", 8.71,  97E3)
    CALL CalcH("PCB-183", "52663-69-1", 8.02, 100E3)
    CALL CalcH("PCB-193", "69782-91-8", 1.93, 140E3)
    CALL CalcH("PCB-194", "35694-08-7", 0.34, 169E3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH_11, DeltaHh)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KH_11 ! at 11 C
      REAL,             INTENT(IN) :: DeltaHh
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      mindHR = DeltaHh / Rgas
      ! change from KH at 11 C to T0=298 K:
      Hominus = KHpcSI_TIMES_HcpSI/KH_11 * EXP(mindHR*(1./T0-1./(11.+CtoK)))
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2472

  !---------------------------------------------------------------------------

  SUBROUTINE ref2475 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2475"
    type = "Q"

    ! Tab. III:
    CALL CalcH("PCB-1",    "2051-60-7", 3.526)
    CALL CalcH("PCB-2",    "2051-61-8", 3.544)
    CALL CalcH("PCB-3",    "2051-62-9", 3.562)
    CALL CalcH("PCB-4",   "13029-08-8", 3.483)
    CALL CalcH("PCB-5",   "16605-91-7", 3.622)
    CALL CalcH("PCB-6",   "25569-80-6", 3.486)
    CALL CalcH("PCB-7",   "33284-50-3", 3.424)
    CALL CalcH("PCB-8",   "34883-43-7", 3.518)
    CALL CalcH("PCB-9",   "34883-39-1", 3.490)
    CALL CalcH("PCB-10",  "33146-45-1", 3.373)
    CALL CalcH("PCB-11",   "2050-67-1", 3.537)
    CALL CalcH("PCB-12",   "2974-92-7", 3.631)
    CALL CalcH("PCB-13",   "2974-90-5", 3.595)
    CALL CalcH("PCB-14",  "34883-41-5", 3.376)
    CALL CalcH("PCB-15",   "2050-68-2", 3.649)
    CALL CalcH("PCB-16",  "38444-78-9", 3.600)
    CALL CalcH("PCB-17",  "37680-66-3", 3.428)
    CALL CalcH("PCB-18",  "37680-65-2", 3.495)
    CALL CalcH("PCB-19",  "38444-73-4", 3.355)
    CALL CalcH("PCB-20",  "38444-84-7", 3.663)
    CALL CalcH("PCB-21",  "55702-46-0", 3.644)
    CALL CalcH("PCB-22",  "38444-85-8", 3.719)
    CALL CalcH("PCB-23",  "55720-44-0", 3.497)
    CALL CalcH("PCB-24",  "55702-45-9", 3.507)
    CALL CalcH("PCB-25",  "55712-37-3", 3.500)
    CALL CalcH("PCB-26",  "38444-81-4", 3.526)
    CALL CalcH("PCB-27",  "38444-76-7", 3.393)
    CALL CalcH("PCB-28",   "7012-37-5", 3.544)
    CALL CalcH("PCB-29",  "15862-07-4", 3.529)
    CALL CalcH("PCB-30",  "35693-92-6", 3.242)
    CALL CalcH("PCB-31",  "16606-02-3", 3.562)
    CALL CalcH("PCB-32",  "38444-77-8", 3.407)
    CALL CalcH("PCB-33",  "38444-86-9", 3.620)
    CALL CalcH("PCB-34",  "37680-68-5", 3.375)
    CALL CalcH("PCB-35",  "37680-69-6", 3.745)
    CALL CalcH("PCB-36",  "38444-87-0", 3.473)
    CALL CalcH("PCB-37",  "38444-90-5", 3.818)
    CALL CalcH("PCB-38",  "53555-66-1", 3.634)
    CALL CalcH("PCB-39",  "38444-88-1", 3.524)
    CALL CalcH("PCB-40",  "38444-93-8", 3.738)
    CALL CalcH("PCB-41",  "52663-59-9", 3.612)
    CALL CalcH("PCB-42",  "36559-22-5", 3.592)
    CALL CalcH("PCB-43",  "70362-46-8", 3.475)
    CALL CalcH("PCB-44",  "41464-39-5", 3.638)
    CALL CalcH("PCB-45",  "70362-45-7", 3.450)
    CALL CalcH("PCB-46",  "41464-47-5", 3.470)
    CALL CalcH("PCB-47",   "2437-79-8", 3.434)
    CALL CalcH("PCB-48",  "70362-47-9", 3.519)
    CALL CalcH("PCB-49",  "41464-40-8", 3.452)
    CALL CalcH("PCB-50",  "62796-65-0", 3.215)
    CALL CalcH("PCB-51",  "68194-04-7", 3.292)
    CALL CalcH("PCB-52",  "35693-99-3", 3.496)
    CALL CalcH("PCB-53",  "41464-41-9", 3.366)
    CALL CalcH("PCB-54",  "15968-05-5", 3.242)
    CALL CalcH("PCB-55",  "74338-24-2", 3.739)
    CALL CalcH("PCB-56",  "41464-43-1", 3.820)
    CALL CalcH("PCB-57",  "70424-67-8", 3.568)
    CALL CalcH("PCB-58",  "41464-49-7", 3.602)
    CALL CalcH("PCB-59",  "74472-33-6", 3.517)
    CALL CalcH("PCB-60",  "33025-41-1", 3.816)
    CALL CalcH("PCB-61",  "33284-53-6", 3.623)
    CALL CalcH("PCB-62",  "54230-22-7", 3.432)
    CALL CalcH("PCB-63",  "74472-34-7", 3.615)
    CALL CalcH("PCB-64",  "52663-58-8", 3.565)
    CALL CalcH("PCB-65",  "33284-54-7", 3.473)
    CALL CalcH("PCB-66",  "32598-10-0", 3.693)
    CALL CalcH("PCB-67",  "73575-53-8", 3.631)
    CALL CalcH("PCB-68",  "73575-52-7", 3.424)
    CALL CalcH("PCB-69",  "60233-24-1", 3.296)
    CALL CalcH("PCB-70",  "32598-11-1", 3.694)
    CALL CalcH("PCB-71",  "41464-46-4", 3.503)
    CALL CalcH("PCB-72",  "41464-42-0", 3.441)
    CALL CalcH("PCB-73",  "74338-23-1", 3.284)
    CALL CalcH("PCB-74",  "32690-93-0", 3.668)
    CALL CalcH("PCB-75",  "32598-12-2", 3.333)
    CALL CalcH("PCB-76",  "70362-48-0", 3.622)
    CALL CalcH("PCB-77",  "32598-13-3", 3.989)
    CALL CalcH("PCB-78",  "70362-49-1", 3.787)
    CALL CalcH("PCB-79",  "41464-48-6", 3.705)
    CALL CalcH("PCB-80",  "33284-52-5", 3.426)
    CALL CalcH("PCB-81",  "70362-50-4", 3.844)
    CALL CalcH("PCB-82",  "52663-62-4", 3.835)
    CALL CalcH("PCB-83",  "60145-20-2", 3.674)
    CALL CalcH("PCB-84",  "52663-60-2", 3.600)
    CALL CalcH("PCB-85",  "65510-45-4", 3.716)
    CALL CalcH("PCB-86",  "55312-69-1", 3.623)
    CALL CalcH("PCB-87",  "38380-02-8", 3.736)
    CALL CalcH("PCB-88",  "55215-17-3", 3.415)
    CALL CalcH("PCB-89",  "73575-57-2", 3.526)
    CALL CalcH("PCB-90",  "68194-07-0", 3.531)
    CALL CalcH("PCB-91",  "68194-05-8", 3.461)
    CALL CalcH("PCB-92",  "52663-61-3", 3.585)
    CALL CalcH("PCB-93",  "73575-56-1", 3.468)
    CALL CalcH("PCB-94",  "73575-55-0", 3.407)
    CALL CalcH("PCB-95",  "38379-99-6", 3.523)
    CALL CalcH("PCB-96",  "73575-54-9", 3.387)
    CALL CalcH("PCB-97",  "41464-51-1", 3.745)
    CALL CalcH("PCB-98",  "60233-25-2", 3.407)
    CALL CalcH("PCB-99",  "38380-01-7", 3.603)
    CALL CalcH("PCB-100", "39485-83-1", 3.250)
    CALL CalcH("PCB-101", "37680-73-2", 3.610)
    CALL CalcH("PCB-102", "68194-06-9", 3.431)
    CALL CalcH("PCB-103", "60145-21-3", 3.298)
    CALL CalcH("PCB-104", "56558-16-8", 3.130)
    CALL CalcH("PCB-105", "32598-14-4", 4.003)
    CALL CalcH("PCB-106", "70424-69-0", 3.783)
    CALL CalcH("PCB-107", "70424-68-9", 3.798)
    CALL CalcH("PCB-108", "70362-41-3", 3.755)
    CALL CalcH("PCB-109", "74472-35-8", 3.550)
    CALL CalcH("PCB-110", "38380-03-9", 3.707)
    CALL CalcH("PCB-111", "39635-32-0", 3.574)
    CALL CalcH("PCB-112", "74472-36-9", 3.574)
    CALL CalcH("PCB-113", "68194-10-5", 3.487)
    CALL CalcH("PCB-114", "74472-37-0", 3.845)
    CALL CalcH("PCB-115", "74472-38-1", 3.610)
    CALL CalcH("PCB-116", "18259-05-7", 3.529)
    CALL CalcH("PCB-117", "68194-11-6", 3.618)
    CALL CalcH("PCB-118", "31508-00-6", 3.901)
    CALL CalcH("PCB-119", "56558-17-9", 3.508)
    CALL CalcH("PCB-120", "68194-12-7", 3.610)
    CALL CalcH("PCB-121", "56558-18-0", 3.253)
    CALL CalcH("PCB-122", "76842-07-4", 3.901)
    CALL CalcH("PCB-123", "65510-44-3", 3.759)
    CALL CalcH("PCB-124", "70424-70-3", 3.768)
    CALL CalcH("PCB-125", "74472-39-2", 3.541)
    CALL CalcH("PCB-126", "57465-28-8", 4.087)
    CALL CalcH("PCB-127", "39635-33-1", 3.807)
    CALL CalcH("PCB-128", "38380-07-3", 3.984)
    CALL CalcH("PCB-129", "55215-18-4", 3.854)
    CALL CalcH("PCB-130", "52663-66-8", 3.817)
    CALL CalcH("PCB-131", "61798-70-7", 3.616)
    CALL CalcH("PCB-132", "38380-05-1", 3.693)
    CALL CalcH("PCB-133", "35694-04-3", 3.691)
    CALL CalcH("PCB-134", "52704-70-8", 3.639)
    CALL CalcH("PCB-135", "52744-13-5", 3.571)
    CALL CalcH("PCB-136", "38411-22-2", 3.492)
    CALL CalcH("PCB-137", "35694-06-5", 3.731)
    CALL CalcH("PCB-138", "35065-28-2", 3.886)
    CALL CalcH("PCB-139", "56030-56-9", 3.483)
    CALL CalcH("PCB-140", "59291-64-4", 3.512)
    CALL CalcH("PCB-141", "52712-04-6", 3.760)
    CALL CalcH("PCB-142", "41411-61-4", 3.502)
    CALL CalcH("PCB-143", "68194-15-0", 3.531)
    CALL CalcH("PCB-144", "68194-14-9", 3.529)
    CALL CalcH("PCB-145", "74472-40-5", 3.328)
    CALL CalcH("PCB-146", "51908-16-8", 3.727)
    CALL CalcH("PCB-147", "68194-13-8", 3.501)
    CALL CalcH("PCB-148", "74472-41-6", 3.367)
    CALL CalcH("PCB-149", "38380-04-0", 3.625)
    CALL CalcH("PCB-150", "68194-08-1", 3.296)
    CALL CalcH("PCB-151", "52663-63-5", 3.548)
    CALL CalcH("PCB-152", "68194-09-2", 3.369)
    CALL CalcH("PCB-153", "35065-27-1", 3.783)
    CALL CalcH("PCB-154", "60145-22-4", 3.418)
    CALL CalcH("PCB-155", "33979-03-2", 3.075)
    CALL CalcH("PCB-156", "38380-08-4", 4.053)
    CALL CalcH("PCB-157", "69782-90-7", 4.073)
    CALL CalcH("PCB-158", "74472-42-7", 3.782)
    CALL CalcH("PCB-159", "39635-35-3", 3.808)
    CALL CalcH("PCB-160", "41411-62-5", 3.670)
    CALL CalcH("PCB-161", "74472-43-8", 3.545)
    CALL CalcH("PCB-162", "39635-34-2", 3.881)
    CALL CalcH("PCB-163", "74472-44-9", 3.781)
    CALL CalcH("PCB-164", "74472-45-0", 3.754)
    CALL CalcH("PCB-165", "74472-46-1", 3.560)
    CALL CalcH("PCB-166", "41411-63-6", 3.735)
    CALL CalcH("PCB-167", "52663-72-6", 3.959)
    CALL CalcH("PCB-168", "59291-65-5", 3.559)
    CALL CalcH("PCB-169", "32774-16-6", 4.186)
    CALL CalcH("PCB-170", "35065-30-6", 4.059)
    CALL CalcH("PCB-171", "52663-71-5", 3.763)
    CALL CalcH("PCB-172", "52663-74-8", 3.924)
    CALL CalcH("PCB-173", "68194-16-1", 3.739)
    CALL CalcH("PCB-174", "38411-25-5", 3.772)
    CALL CalcH("PCB-175", "40186-70-7", 3.651)
    CALL CalcH("PCB-176", "52663-65-7", 3.527)
    CALL CalcH("PCB-177", "52663-70-4", 3.787)
    CALL CalcH("PCB-178", "52663-67-9", 3.671)
    CALL CalcH("PCB-179", "52663-64-6", 3.560)
    CALL CalcH("PCB-180", "35065-29-3", 3.969)
    CALL CalcH("PCB-181", "74472-47-2", 3.638)
    CALL CalcH("PCB-182", "60145-23-5", 3.590)
    CALL CalcH("PCB-183", "52663-69-1", 3.696)
    CALL CalcH("PCB-184", "74472-48-3", 3.339)
    CALL CalcH("PCB-185", "52712-05-7", 3.669)
    CALL CalcH("PCB-186", "74472-49-4", 3.434)
    CALL CalcH("PCB-187", "52663-68-0", 3.693)
    CALL CalcH("PCB-188", "74487-85-7", 3.353)
    CALL CalcH("PCB-189", "39635-31-9", 4.177)
    CALL CalcH("PCB-190", "41411-64-7", 3.950)
    CALL CalcH("PCB-191", "74472-50-7", 3.876)
    CALL CalcH("PCB-192", "74472-51-8", 3.718)
    CALL CalcH("PCB-193", "69782-91-8", 3.872)
    CALL CalcH("PCB-194", "35694-08-7", 4.174)
    CALL CalcH("PCB-195", "52663-78-2", 3.926)
    CALL CalcH("PCB-196", "42740-50-1", 3.884)
    CALL CalcH("PCB-197", "33091-17-7", 3.596)
    CALL CalcH("PCB-198", "68194-17-2", 3.812)
    CALL CalcH("PCB-199", "52663-75-9", 3.644)
    CALL CalcH("PCB-200", "52663-73-7", 3.619)
    CALL CalcH("PCB-201", "40186-71-8", 3.884)
    CALL CalcH("PCB-202",  "2136-99-4", 3.651)
    CALL CalcH("PCB-203", "52663-76-0", 3.853)
    CALL CalcH("PCB-204", "74472-52-9", 3.463)
    CALL CalcH("PCB-205", "74472-53-0", 4.059)
    CALL CalcH("PCB-206", "40186-72-9", 4.059)
    CALL CalcH("PCB-207", "52663-79-3", 3.772)
    CALL CalcH("PCB-208", "52663-77-1", 3.777)
    CALL CalcH("PCB-209",  "2051-24-3", 3.948)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, minlogHLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: minlogHLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(1./(atm*(10.**(-minlogHLC))))

    END SUBROUTINE CalcH

  END SUBROUTINE ref2475

  !---------------------------------------------------------------------------

  SUBROUTINE ref2477 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2477"
    type = "M"

    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 4.1, 11.0, 18.0, 25.0, 31.0 /) + CtoK

    CALL CalcH("2-methylnaphthalene",   "91-57-6", (/ 13.0, 21.0, 33.2, 51.3, 73.3 /) ) ! C{10}H7CH3
    CALL CalcH("1-methylnaphthalene",   "90-12-0", (/ 10.1, 17.4, 29.2, 47.8, 71.7 /) ) ! C{10}H7CH3
    CALL CalcH("acenaphthylene",       "208-96-8", (/ 2.38, 4.27, 7.46, 12.7, 19.6 /) ) ! C{12}H8
    CALL CalcH("acenaphthene",          "83-32-9", (/ 3.52, 6.29, 10.9, 18.5, 28.6 /) ) ! C{12}H{10}
    CALL CalcH("2,3-benzindene",        "86-73-7", (/ 2.05, 3.54, 5.96, 9.81, 14.8 /) ) ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",          "85-01-8", (/ 0.94, 1.60, 2.65, 4.29, 6.38 /) ) ! C{14}H{10}
    CALL CalcH("anthracene",           "120-12-7", (/ 1.25, 2.12, 3.50, 5.64, 8.36 /) ) ! C{14}H{10}
    CALL CalcH("1-methylphenanthrene", "832-69-9", (/ 1.58, 2.36, 3.47, 5.00, 6.77 /) ) ! C{15}H{12}
    CALL CalcH("benzo[jk]fluorene",    "206-44-0", (/ 0.56, 0.87, 1.32, 1.96, 2.72 /) ) ! C{16}H{10} fluoranthene
    CALL CalcH("pyrene",               "129-00-0", (/ 0.43, 0.69, 1.10, 1.71, 2.45 /) ) ! C{16}H{10}
    CALL CalcH("benzo[a]fluorene",     "238-84-6", (/ 0.88, 1.30, 1.89, 2.70, 3.62 /) ) ! C{17}H{12}
    CALL CalcH("benz[a]anthracene",     "56-55-3", (/ 0.15, 0.31, 0.63, 1.22, 2.11 /) ) ! C{18}H{12}
    CALL CalcH("chrysene",             "218-01-9", (/ 0.02, 0.07, 0.19, 0.53, 1.20 /) ) ! C{18}H{12}

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: H
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Harray = KHpcSI_TIMES_HcpSI/H
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2477

  !---------------------------------------------------------------------------

  SUBROUTINE ref2478 ! KHcc [1]
    IMPLICIT NONE

    ref = "2478"
    type = "V"

    ! p. 196
    CALL CalcH("2-methylbutane",                    "78-78-4", 5.62E+1) ! C5H{12} isopentane
    CALL CalcH("1-pentene",                        "109-67-1", 1.63E+1) ! C5H{10}
    CALL CalcH("pentane",                          "109-66-0", 4.88E+1) ! C5H{12}
    CALL CalcH("2-pentene",                        "109-68-2", 9.20E+0) ! C5H{10}
    CALL CalcH("cyclopentane",                     "287-92-3", 7.50E+0) ! C5H{10}
    CALL CalcH("2,2-dimethylbutane",                "75-83-2", 6.99E+1) ! C6H{14}
    CALL CalcH("4-methyl-1-pentene",               "691-37-2", 2.55E+1) ! C6H{12}
    CALL CalcH("2,3-dimethylbutane",                "79-29-8", 5.70E+1) ! C6H{14}
    CALL CalcH("2-methylpentane",                  "107-83-5", 7.11E+1) ! C6H{14} isohexane
    CALL CalcH("2-methyl-1-pentene",               "763-29-1", 1.13E+1) ! C6H{12}
    CALL CalcH("3-methylpentane",                   "96-14-0", 6.87E+1) ! C6H{14}
    CALL CalcH("1-hexene",                         "592-41-6", 1.68E+1) ! C6H{12}
    CALL CalcH("hexane",                           "110-54-3", 5.71E+1) ! C6H{14}
    CALL CalcH("methylcyclopentane",                "96-37-7", 1.49E+1) ! C5H9CH3
    CALL CalcH("2,2-dimethylpentane",              "590-35-2", 1.29E+2) ! C7H{16}
    ! p. 197
    CALL CalcH("benzene",                           "71-43-2", 2.24E-1) ! C6H6
    CALL CalcH("2,4-dimethylpentane",              "108-08-7", 1.20E+2) ! C7H{16}
    CALL CalcH("cyclohexane",                      "110-82-7", 7.50E+0) ! C6H{12}
    CALL CalcH("3,3-dimethylpentane",              "562-49-2", 7.49E+1) ! C7H{16}
    CALL CalcH("2,3-dimethylpentane",              "565-59-3", 7.07E+1) ! C7H{16}
    CALL CalcH("2-methylhexane",                   "591-76-4", 1.40E+2) ! C7H{16} isoheptane
    CALL CalcH("3-methylhexane",                   "589-34-4", 1.26E+2) ! C7H{16}
    CALL CalcH("{trans}-2-heptene",              "14686-13-6", 1.70E+1) ! C7H{14}
    CALL CalcH("heptane",                          "142-82-5", 8.06E+1) ! C7H{16}
    CALL CalcH("methylcyclohexane",                "108-87-2", 1.53E+1) ! C6H{11}CH3
    CALL CalcH("1,1,3-trimethylcyclopentane",     "4516-69-2", 6.43E+1) ! C5H7(CH3)3
    CALL CalcH("methylbenzene",                    "108-88-3", 2.44E-1) ! C6H5CH3 toluene
    CALL CalcH("2,3,4-trimethylpentane",           "565-75-3", 7.22E+1) ! C8H{18}
    CALL CalcH("3-methylheptane",                  "589-81-1", 1.51E+2) ! C8H{18}
    CALL CalcH("{trans}-1,4-dimethylcyclohexane", "2207-04-7", 3.56E+1) ! C6H{10}(CH3)2
    CALL CalcH("1-octene",                         "111-66-0", 3.89E+1) ! C8H{16}
    CALL CalcH("octane",                           "111-65-9", 1.27E+2) ! C8H{18}
    CALL CalcH("2,2,4-trimethylpentane",           "540-84-1", 1.32E+2) ! C8H{18} isooctane
    ! p. 198
    CALL CalcH("{cis}-1,2-dimethylcyclohexane",   "2207-01-4", 1.46E+1) ! C6H{10}(CH3)2
    CALL CalcH("propylcyclopentane",              "2040-96-2", 3.64E+1) ! C5H9C3H7
    CALL CalcH("ethylbenzene",                     "100-41-4", 4.04E-1) ! C6H5C2H5
    CALL CalcH("1,4-dimethylbenzene",              "106-42-3", 2.27E-1) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("1,3-dimethylbenzene",              "108-38-3", 2.94E-1) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("4-methyloctane",                  "2216-34-4", 4.06E+2) ! C9H{20}
    CALL CalcH("1,2-dimethylbenzene",               "95-47-6", 1.76E-1) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("nonane",                           "111-84-2", 2.42E+2) ! C9H{20}
    CALL CalcH("(2-propyl)-benzene",                "98-82-8", 6.14E-1) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("propylbenzene",                    "103-65-1", 4.17E-1) ! C6H5C3H7
    CALL CalcH("1-ethyl-4-methylbenzene",          "622-96-8", 2.01E-1) ! C6H4CH3C2H5 $p$-ethyltoluene
    CALL CalcH("1-ethyl-2-methylbenzene",          "611-14-3", 2.15E-1) ! C6H4CH3C2H5 $o$-ethyltoluene
    CALL CalcH("1,3,5-trimethylbenzene",           "108-67-8", 3.30E-1) ! C6H3(CH3)3 mesitylene
    CALL CalcH("(1,1-dimethylethyl)-benzene",       "98-06-6", 5.25E-1) ! C6H5C4H9 {tert}-butylbenzene
    CALL CalcH("1,2,4-trimethylbenzene",            "95-63-6", 2.53E-1) ! C6H3(CH3)3
    CALL CalcH("(2-methylpropyl)-benzene",         "538-93-2", 1.33E+0) ! C6H5C4H9 isobutylbenzene
    ! p. 199
    CALL CalcH("(1-methylpropyl)-benzene",         "135-98-8", 7.42E-1) ! C6H5C4H9 {sec}-butylbenzene
    CALL CalcH("pentylcyclopentane",              "3741-00-2", 7.48E+1) ! C5H9C5H{11}
    CALL CalcH("decane",                           "124-18-5", 1.74E+2) ! C{10}H{22}
    CALL CalcH("1,2,3-trimethylbenzene",           "526-73-8", 1.50E-1) ! C6H3(CH3)3
    CALL CalcH("p-cymene",                          "99-87-6", 3.24E-1) ! C{10}H{14} $p$-cymene
    CALL CalcH("butylbenzene",                     "104-51-8", 5.38E-1) ! C6H5C4H9
    CALL CalcH("undecane",                        "1120-21-4", 7.49E+1) ! C{11}H{24}
    CALL CalcH("1,2,4,5-tetramethylbenzene",        "95-93-2", 1.03E+0) ! C6H2(CH3)4
    CALL CalcH("pentylbenzene",                    "538-68-1", 2.49E-1) ! C6H5C5H{11}
    CALL CalcH("dodecane",                         "112-40-3", 3.17E+2) ! C{12}H{26}
    CALL CalcH("naphthalene",                       "91-20-3", 5.62E-2) ! C{10}H8
    CALL CalcH("2-methylnaphthalene",               "91-57-6", 2.03E-2) ! C{10}H7CH3
    CALL CalcH("1-methylnaphthalene",               "90-12-0", 1.60E-2) ! C{10}H7CH3
    CALL CalcH("tetradecane",                      "629-59-4", 1.56E+2) ! C{14}H{30}
    CALL CalcH("biphenyl",                          "92-52-4", 3.48E-2) ! (C6H5)2
    CALL CalcH("2-ethylnaphthalene",               "939-27-5", 2.55E-2) ! C{10}H7C2H5
    CALL CalcH("hexylbenzene",                    "1077-16-3", 7.98E-1) ! C6H5C6H{13}
    CALL CalcH("1-ethylnaphthalene",              "1127-76-0", 1.48E-2) ! C{10}H7C2H5
    CALL CalcH("acenaphthene",                      "83-32-9", 4.24E-3) ! C{12}H{10}
    CALL CalcH("2,6-dimethylnaphthalene",          "581-42-0", 6.53E-3) ! C{12}H{12}
    ! p. 200
    CALL CalcH("2,3-dimethylnaphthalene",          "581-40-8", 6.26E-3) ! C{12}H{12}
    CALL CalcH("hexadecane",                       "544-76-3", 1.57E+2) ! C{16}H{34}
    CALL CalcH("2,3-benzindene",                    "86-73-7", 3.56E-3) ! C{13}H{10} fluorene
    CALL CalcH("octadecane",                       "593-45-3", 2.51E+2) ! C{18}H{38}
    CALL CalcH("phenanthrene",                      "85-01-8", 1.45E-3) ! C{14}H{10}
    CALL CalcH("anthracene",                       "120-12-7", 6.63E-4) ! C{14}H{10}
    CALL CalcH("1,4,5-trimethylnaphthalene",      "2131-41-1", 9.48E-3)
    CALL CalcH("pyrene",                           "129-00-0", 3.74E-4) ! C{16}H{10}
    CALL CalcH("benzo[jk]fluorene",                "206-44-0", 3.51E-4) ! C{16}H{10} fluoranthene
    ! p. 201
    CALL CalcH("9-methylanthracene",               "779-02-2", 4.28E-2)
    CALL CalcH("chrysene",                         "218-01-9", 1.80E-4) ! C{18}H{12}
    CALL CalcH("benz[a]anthracene",                 "56-55-3", 1.68E-4) ! C{18}H{12}
    CALL CalcH("eicosane",                         "112-95-8", 8.00E+1) ! C{20}H{42}
    CALL CalcH("dibenz[ah]anthracene",              "53-70-3", 3.07E-6) ! C{22}H{14}
    CALL CalcH("benzo[ghi]perylene",               "191-24-2", 5.86E-6) ! C{22}H{12}
    CALL CalcH("hexacosane",                       "630-01-3", 8.13E+0) ! C{26}H{54}
    CALL CalcH("benzo[a]pyrene",                    "50-32-8", 2.27E-5) ! C{20}H{12} benz[a]pyrene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2478

  !---------------------------------------------------------------------------

  SUBROUTINE ref2479 ! KHcc [1]
    IMPLICIT NONE

    ref = "2479"
    type = "M"

    ! Table: 7:
    CALL CalcH("dichloromethane",         "75-09-2", 0.077) ! CH2Cl2 methylene chloride
    CALL CalcH("tetrachloromethane",      "56-23-5", 0.936) ! CCl4 carbontetrachloride
    CALL CalcH("1,1,1-trichloroethane",   "71-55-6", 0.645) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1-dichloroethene",      "75-35-4", 3.089) ! CH2CCl2
    CALL CalcH("(Z)-1,2-dichloroethene", "156-59-2", 0.181) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("(E)-1,2-dichloroethene", "156-60-5", 0.375) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",         "79-01-6", 0.430) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",      "127-18-4", 0.535) ! C2Cl4 tetrachloroethylene
    CALL CalcH("benzene",                 "71-43-2", 0.306) ! C6H6
    CALL CalcH("methylbenzene",          "108-88-3", 0.244) ! C6H5CH3 toluene
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6", 0.175) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,2,4-trimethylbenzene",  "95-63-6", 0.195) ! C6H3(CH3)3
    CALL CalcH("chlorobenzene",          "108-90-7", 0.131) ! C6H5Cl
    CALL CalcH("1,4-dichlorobenzene",    "106-46-7", 0.078) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("naphthalene",             "91-20-3", 0.015) ! C{10}H8

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      CALL MakeNoteOtherTemp("293")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2479

  !---------------------------------------------------------------------------

  SUBROUTINE ref2481 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2481"
    type = "Q"

    CALL CalcH("phenanthrene",            "85-01-8", -4.420) ! C{14}H{10}
    CALL CalcH("acenaphthene",            "83-32-9", -3.904) ! C{12}H{10}
    CALL CalcH("biphenyl",                "92-52-4", -3.466) ! (C6H5)2
    CALL CalcH("naphthalene",             "91-20-3", -3.538) ! C{10}H8
    CALL CalcH("1,1,2-trichloroethane",   "79-00-5", -2.887) ! CHCl2CH2Cl
    CALL CalcH("trichloromethane",        "67-66-3", -2.375) ! CHCl3 chloroform
    CALL CalcH("1,4-dichlorobenzene",    "106-46-7", -2.338) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("chlorobenzene",          "108-90-7", -2.269) ! C6H5Cl
    CALL CalcH("benzene",                 "71-43-2", -2.264) ! C6H6
    CALL CalcH("methylbenzene",          "108-88-3", -2.089) ! C6H5CH3 toluene
    CALL CalcH("1,2,4-trimethylbenzene",  "95-63-6", -1.922) ! C6H3(CH3)3
    CALL CalcH("1,1,1-trichloroethane",   "71-55-6", -2.223) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("ethylbenzene",           "100-41-4", -2.116) ! C6H5C2H5
    CALL CalcH("tetrachloromethane",      "56-23-5", -1.622) ! CCl4 carbontetrachloride

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, calcd)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: calcd
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*10.**calcd)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2481

  !---------------------------------------------------------------------------

  SUBROUTINE ref2482 ! KHcc [1]
    IMPLICIT NONE

    ref = "2482"
    type = "Q"

    ! Tab. III:
    CALL CalcH("ethane",                            "74-84-0",  1.31,  1.27,  0.04)
    CALL CalcH("propane",                           "74-98-6",  1.46,  1.41,  0.05)
    CALL CalcH("n-butane",                         "106-97-8",  1.58,  1.51,  0.07)
    CALL CalcH("2-methylpropane",                   "75-28-5",  1.68,  1.60,  0.08)
    CALL CalcH("n-pentane",                        "109-66-0",  1.71,  1.61,  0.10)
    CALL CalcH("2,2-dimethylpropane",              "463-82-1",  1.95,  1.81,  0.14)
    CALL CalcH("n-hexane",                         "110-54-3",  1.87,  1.71,  0.16)
    CALL CalcH("2-methylpentane",                  "107-83-5",  1.85,  1.78,  0.07)
    CALL CalcH("3-methylpentane",                   "96-14-0",  1.84,  1.76,  0.08)
    CALL CalcH("2,2-dimethylbutane",                "75-83-2",  1.90,  1.88,  0.02)
    CALL CalcH("heptane",                          "142-82-5",  1.92,  1.81,  0.11)
    CALL CalcH("2,4-dimethylpentane",              "108-08-7",  2.11,  1.95,  0.16)
    CALL CalcH("octane",                           "111-65-9",  2.12,  1.91,  0.21)
    CALL CalcH("2,2,4-trimethylpentane",           "540-84-1",  2.09,  2.15, -0.06)
    CALL CalcH("ethylene",                          "74-85-1",  0.94,  0.89,  0.06)
    CALL CalcH("propylene",                        "115-07-1",  0.93,  0.99, -0.06)
    CALL CalcH("1-butene",                         "106-98-9",  1.01,  1.08, -0.07)
    CALL CalcH("2-methylpropene",                  "115-11-7",  0.94,  1.16, -0.22)
    CALL CalcH("1-pentene",                        "109-67-1",  1.22,  1.18,  0.04)
    CALL CalcH("trans-2-pentene",                  "646-04-8",  0.98,  1.18, -0.20)
    CALL CalcH("2-methyl-2-butene",                "513-35-9",  0.98,  1.25, -0.27)
    CALL CalcH("3-methyl-1-butene",                "563-45-1",  1.34,  1.24,  0.10)
    CALL CalcH("1-hexene",                         "592-41-6",  1.25,  1.28, -0.03)
    CALL CalcH("4-methyl-1-pentene",               "691-37-2",  1.40,  1.35,  0.05)
    CALL CalcH("1-octene",                         "111-66-0",  1.59,  1.48,  0.11)
    CALL CalcH("1,3-butadiene",                    "106-99-0",  0.41,  0.64, -0.23)
    CALL CalcH("1,4-pentadiene",                   "591-93-5",  0.69,  0.74, -0.05)
    CALL CalcH("2-methyl-1,3-butadiene",            "78-79-5",  0.50,  0.78, -0.28)
    CALL CalcH("1,5-hexadiene",                    "592-42-7",  0.74,  0.84, -0.10)
    CALL CalcH("2,3-dimethyl-1,3-butadiene",       "513-81-5",  0.29,  0.93, -0.64)
    CALL CalcH("acetylene",                         "74-86-2", -0.01, -0.16,  0.15)
    CALL CalcH("propyne",                           "74-99-7", -0.35, -0.04, -0.31)
    CALL CalcH("1-butyne",                         "107-00-6", -0.12,  0.04, -0.16)
    CALL CalcH("1-pentyne",                        "627-19-0",  0.01,  0.14, -0.13)
    CALL CalcH("1-hexyne",                         "693-02-7",  0.21,  0.24, -0.03)
    CALL CalcH("1-heptyne",                        "628-71-7",  0.44,  0.34,  0.10)
    CALL CalcH("1-octyne",                         "629-05-0",  0.52,  0.44,  0.08)
    CALL CalcH("1-nonyne",                        "3452-09-3",  0.77,  0.54,  0.23)
    CALL CalcH("cyclopentane",                     "287-92-3",  0.88,  0.85,  0.03)
    CALL CalcH("cyclohexane",                      "110-82-7",  0.90,  0.95, -0.05)
    CALL CalcH("methylcyclopentane",                "96-37-7",  1.17,  1.01,  0.17)
    CALL CalcH("methylcyclohexane",                "108-87-2",  1.25,  1.11,  0.15)
    CALL CalcH("1,2-dimethylcyclohexane",          "583-57-3",  1.16,  1.25, -0.09)
    CALL CalcH("cyclopentene",                     "142-29-0",  0.41,  0.40,  0.01)
    CALL CalcH("cyclohexene",                      "110-83-8",  0.27,  0.50, -0.23)
    ! mix of isomers? not used:
    !CALL CalcH("methylcyclohexene",               "1335-86-0",  0.49,  0.65, -0.16)
    CALL CalcH("benzene",                           "71-43-2", -0.65, -0.73,  0.08)
    CALL CalcH("toluene",                          "108-88-3", -0.56, -0.59,  0.03)
    CALL CalcH("ethylbenzene",                     "100-41-4", -0.45, -0.52,  0.07)
    CALL CalcH("o-xylene",                          "95-47-6", -0.66, -0.45, -0.22)
    CALL CalcH("m-xylene",                         "108-38-3", -0.59, -0.45, -0.15)
    CALL CalcH("p-xylene",                         "106-42-3", -0.59, -0.45, -0.15)
    CALL CalcH("propylbenzene",                    "103-65-1", -0.39, -0.42,  0.03)
    CALL CalcH("1,2,4-trimethylbenzene",            "95-63-6", -0.63, -0.30, -0.33)
    CALL CalcH("2-propylbenzene",                   "98-82-8", -0.22, -0.36,  0.14)
    CALL CalcH("butylbenzene",                     "104-51-8", -0.29, -0.32,  0.03)
    CALL CalcH("2-butylbenzene",                   "135-98-8", -0.33, -0.25, -0.09)
    CALL CalcH("tert-butylbenzene",                 "98-06-6", -0.32, -0.17, -0.15)
    CALL CalcH("tert-amylbenzene",                "2049-95-8", -0.13, -0.10, -0.03)
    CALL CalcH("chloromethane",                     "74-87-3", -0.39,  0.02, -0.41)
    CALL CalcH("dichloromethane",                   "75-09-2", -1.03, -0.74, -0.29)
    CALL CalcH("trichloromethane",                  "67-66-3", -0.75, -0.98,  0.23)
    CALL CalcH("tetrachloromethane",                "56-23-5",  0.07,  0.06,  0.01)
    CALL CalcH("bromomethane",                      "74-83-9", -0.58, -0.88,  0.30)
    CALL CalcH("dibromomethane",                    "74-95-3", -1.44, -1.37, -0.07)
    CALL CalcH("tribromomethane",                   "75-25-2", -1.56, -1.77,  0.21)
    CALL CalcH("iodomethane",                       "74-88-4", -0.65, -0.95,  0.30)
    CALL CalcH("fluoromethane",                    "593-53-3", -0.16,  0.32, -0.48)
    CALL CalcH("trifluoromethane",                  "75-46-7",  0.59,  0.53,  0.06)
    CALL CalcH("tetrafluoromethane",                "75-73-0",  2.32,  1.87,  0.45)
    CALL CalcH("chlorofluoromethane",              "593-70-4", -0.57, -0.18, -0.39)
    CALL CalcH("chlorodifluoromethane",             "75-45-6",  0.08,  0.00,  0.08)
    CALL CalcH("chlorotrifluoromethane",            "75-72-9",  1.85,  1.46,  0.40)
    CALL CalcH("dichlorodifluoromethane",           "75-71-8",  1.24,  0.93,  0.31)
    CALL CalcH("bromotrifluoromethane",             "75-63-8",  1.31,  1.18,  0.13)
    CALL CalcH("chloroethane",                      "75-00-3", -0.46, -0.29, -0.17)
    CALL CalcH("bromoethane",                       "74-96-4", -0.51, -0.61,  0.10)
    CALL CalcH("iodoethane",                        "75-03-6", -0.53, -0.47, -0.06)
    CALL CalcH("1,1-dichloroethane",                "75-34-3", -0.62, -0.54, -0.08)
    CALL CalcH("1,2-dichloroethane",               "107-06-2", -1.27, -0.64, -0.63)
    CALL CalcH("1,2-dibromoethane",                "106-93-4", -1.54, -1.27, -0.27)
    CALL CalcH("1-chloro-2-bromoethane",           "107-04-0", -1.43, -0.96, -0.47)
    CALL CalcH("1,1,1-trichloroethane",             "71-55-6", -0.18, -0.76,  0.58)
    CALL CalcH("1,1,2-trichloroethane",             "79-00-5", -1.43, -0.91, -0.53)
    CALL CalcH("1,1,2,2-tetrachloroethane",         "79-34-5", -1.73, -1.18, -0.55)
    CALL CalcH("pentachloroethane",                 "76-01-7", -1.00, -1.41,  0.41)
    CALL CalcH("hexachloroethane",                  "67-72-1", -1.03, -0.40, -0.63)
    CALL CalcH("1,1-difluoroethane",                "75-37-6",  0.08,  0.47, -0.39)
    CALL CalcH("2-chloro,1,1,1-trifluoroethane",    "75-88-7",  0.04,  0.02,  0.02)
    CALL CalcH("1-chloropropane",                  "540-54-5", -0.26, -0.19, -0.07)
    CALL CalcH("2-chloropropane",                   "75-29-6", -0.18, -0.10, -0.09)
    CALL CalcH("1-bromopropane",                   "106-94-5", -0.41, -0.51,  0.10)
    CALL CalcH("2-bromopropane",                    "75-26-3", -0.35, -0.36,  0.01)
    CALL CalcH("1-iodopropane",                    "107-08-4", -0.43, -0.37, -0.06)
    CALL CalcH("2-iodopropane",                     "75-30-9", -0.34, -0.13, -0.21)
    CALL CalcH("1,2-dichloropropane",               "78-87-5", -0.92, -0.47, -0.45)
    CALL CalcH("1,3-dichloropropane",              "142-28-9", -1.39, -0.54, -0.85)
    CALL CalcH("1,2-dibromopropane",                "78-75-1", -1.42, -1.04, -0.38)
    CALL CalcH("1,3-dibromopropane",               "109-64-8", -1.44, -1.17, -0.27)
    CALL CalcH("1-chlorobutane",                   "109-69-3", -0.10, -0.09, -0.01)
    CALL CalcH("1-bromobutane",                    "109-65-9", -0.30, -0.41,  0.11)
    CALL CalcH("1-bromo-2-methylbutane",         "10422-35-2", -0.02, -0.34,  0.32)
    CALL CalcH("1-iodobutane",                     "542-69-8", -0.19, -0.27,  0.08)
    CALL CalcH("1,1-dichlorobutane",               "541-33-3", -0.51, -0.36, -0.15)
    CALL CalcH("1-chloropentane",                  "543-59-9", -0.05,  0.01, -0.06)
    CALL CalcH("2-chloropentane",                  "625-29-6",  0.05,  0.09, -0.04)
    CALL CalcH("3-chloropentane",                  "616-20-6",  0.03,  0.07, -0.04)
    CALL CalcH("1-bromo-3-methylpentane",        "51116-73-5",  0.15, -0.16,  0.31)
    CALL CalcH("chloroethylene",                    "75-01-4",  0.36, -0.70,  1.06)
    CALL CalcH("1,2-dichloroethylene",             "540-59-0", -0.56, -1.05,  0.49)
    CALL CalcH("trichloroethylene",                 "79-01-6", -0.32, -1.32,  1.00)
    CALL CalcH("tetrachloroethylene",              "127-18-4", -0.30, -0.34,  0.04)
    CALL CalcH("3-chloropropene",                  "107-05-1", -0.42, -0.63,  0.21)
    CALL CalcH("chlorobenzene",                    "108-90-7", -0.74, -1.02,  0.28)
    CALL CalcH("bromobenzene",                     "108-86-1", -1.07, -1.26,  0.19)
    CALL CalcH("1,2-dichlorobenzene",               "95-50-1", -1.00, -1.32,  0.32)
    CALL CalcH("1,3-dichlorobenzene",              "541-73-1", -0.72, -1.32,  0.60)
    CALL CalcH("1,4-dichlorobenzene",              "106-46-7", -1.10, -1.32,  0.22)
    CALL CalcH("1,4-dibromobenzene",               "106-37-6", -1.69, -1.78,  0.09)
    CALL CalcH("p-bromotoluene",                   "106-38-7", -1.02, -1.11,  0.09)
    CALL CalcH("1-bromo-2-ethylbenzene",          "1973-22-4", -0.87, -1.05,  0.18)
    CALL CalcH("o-bromocumene",                   "7073-94-1", -0.62, -0.89,  0.27)
    CALL CalcH("acetic acid",                       "64-19-7", -4.91, -4.99,  0.08)
    CALL CalcH("propionic acid",                    "79-09-4", -4.74, -4.92,  0.18)
    CALL CalcH("butyric acid",                     "107-92-6", -4.66, -4.82,  0.16)
    CALL CalcH("methyl formate",                   "107-31-3", -2.04, -2.20,  0.16)
    CALL CalcH("ethyl formate",                    "109-94-4", -1.94, -2.15,  0.21)
    CALL CalcH("methyl acetate",                    "79-20-9", -2.43, -1.98, -0.46)
    CALL CalcH("propyl formate",                   "110-74-7", -1.82, -2.04,  0.22)
    CALL CalcH("isopropyl formate",                "625-55-8", -1.48, -1.99,  0.51)
    CALL CalcH("ethyl acetate",                    "141-78-6", -2.26, -2.01, -0.25)
    CALL CalcH("methyl propionate",                "554-12-1", -2.18, -2.00, -0.18)
    CALL CalcH("isobutyl formate",                 "542-55-2", -1.63, -1.88,  0.25)
    CALL CalcH("propyl acetate",                   "109-60-4", -2.09, -1.91, -0.18)
    CALL CalcH("isopropyl acetate",                "108-21-4", -1.94, -1.86, -0.08)
    CALL CalcH("ethyl propionate",                 "105-37-3", -2.05, -1.94, -0.11)
    CALL CalcH("methyl butyrate",                  "623-42-7", -2.08, -1.90, -0.18)
    CALL CalcH("butyl acetate",                    "123-86-4", -1.87, -1.81, -0.06)
    CALL CalcH("isobutyl acetate",                 "110-19-0", -1.73, -1.74,  0.01)
    CALL CalcH("propyl propionate",                "106-36-5", -1.80, -1.84,  0.04)
    CALL CalcH("isopropyl propionate",             "637-78-5", -1.63, -1.79,  0.16)
    CALL CalcH("ethyl butyrate",                   "105-54-4", -1.83, -1.84,  0.01)
    CALL CalcH("methyl pentanoate",                "624-24-8", -1.86, -1.80, -0.06)
    CALL CalcH("amyl acetate",                     "628-63-7", -1.80, -1.71, -0.09)
    CALL CalcH("propyl butyrate",                  "105-66-8", -1.67, -1.74,  0.07)
    CALL CalcH("ethyl pentanoate",                 "539-82-2", -1.85, -1.74, -0.11)
    CALL CalcH("methyl hexanoate",                 "106-70-7", -1.82, -1.70, -0.12)
    CALL CalcH("hexyl acetate",                    "142-92-7", -1.66, -1.74,  0.08)
    CALL CalcH("amyl propionate",                  "624-54-4", -1.46, -1.64,  0.18)
    CALL CalcH("isoamyl formate",                  "110-45-2", -1.56, -1.78,  0.22)
    CALL CalcH("isoamyl acetate",                  "123-92-2", -1.62, -1.64,  0.02)
    CALL CalcH("methyl octanoate",                 "111-11-5", -1.50, -1.44, -0.06)
    CALL CalcH("ethyl heptanoate",                 "106-30-9", -1.69, -1.71,  0.02)
    CALL CalcH("methyl benzoate",                   "93-58-3", -3.14, -2.83, -0.31)
    CALL CalcH("methanol",                          "67-56-1", -3.72, -3.65, -0.07)
    CALL CalcH("ethanol",                           "64-17-5", -3.59, -3.59,  0.00)
    CALL CalcH("1-propanol",                        "71-23-8", -3.56, -3.49, -0.07)
    CALL CalcH("2-propanol",                        "67-63-0", -3.46, -3.43, -0.03)
    CALL CalcH("allyl alcohol",                    "107-18-6", -3.69, -3.92,  0.23)
    CALL CalcH("1-butanol",                         "71-36-3", -3.46, -3.39, -0.07)
    CALL CalcH("2-butanol",                         "78-92-2", -3.38, -3.35, -0.03)
    CALL CalcH("tert-butyl alcohol",                "75-65-0", -3.31, -3.24, -0.07)
    CALL CalcH("2-methyl-1-propanol",               "78-83-1", -3.31, -3.32,  0.01)
    CALL CalcH("1-pentanol",                        "71-41-0", -3.29, -3.29,  0.00)
    CALL CalcH("2-pentanol",                      "6032-29-7", -3.22, -3.25,  0.03)
    CALL CalcH("2-methyl-1-butanol",               "137-32-6", -3.24, -3.23, -0.01)
    CALL CalcH("2-methyl-2-butanol",                "75-85-4", -3.25, -3.17, -0.08)
    CALL CalcH("1-hexanol",                        "111-27-3", -3.20, -3.19, -0.01)
    CALL CalcH("3-hexanol",                        "623-37-0", -2.70, -3.17,  0.47)
    CALL CalcH("2,3-dimethylbutanol",            "19550-30-2", -2.87, -3.07,  0.20)
    CALL CalcH("2-methyl-3-pentanol",              "565-67-3", -2.85, -3.11,  0.26)
    CALL CalcH("4-methyl-2-pentanol",              "108-11-2", -2.74, -3.08,  0.34)
    CALL CalcH("2-methyl-2-pentanol",              "590-36-3", -2.88, -3.07,  0.19)
    CALL CalcH("1-heptanol",                       "111-70-6", -3.12, -3.09, -0.03)
    CALL CalcH("1-octanol",                        "111-87-5", -3.01, -2.99, -0.02)
    CALL CalcH("phenol",                           "108-95-2", -4.79, -4.39, -0.41)
    CALL CalcH("4-bromophenol",                    "106-41-2", -5.21, -4.91, -0.30)
    CALL CalcH("4-tert-butylphenol",                "98-54-4", -4.34, -3.82, -0.52)
    CALL CalcH("2-cresol",                          "95-48-7", -4.30, -4.25, -0.05)
    CALL CalcH("4-cresol",                         "106-44-5", -4.49, -4.24, -0.25)
    ! Tab. V:
    CALL CalcH("2,2,2-trifluoroethanol",            "75-89-8", -3.15, -3.09, -0.06)
    CALL CalcH("1,1,1-trifluoro-2-propanol",       "374-01-6", -3.05, -3.11,  0.06)
    CALL CalcH("2,2,3,3-tetrafluoropropanol",       "76-37-9", -3.59, -2.96, -0.63)
    CALL CalcH("2,2,3,3,3-pentafluoropropane",    "1814-88-6", -3.04, -2.87, -0.17)
    CALL CalcH("hexafluoro-2-propanol",            "920-66-1", -2.76, -2.75, -0.01)
    CALL CalcH("cyclohexanol",                     "108-93-0", -3.63, -3.92,  0.29)
    CALL CalcH("naphthalene",                       "91-20-3", -1.77, -1.90,  0.13)
    CALL CalcH("acenaphthene",                      "83-32-9", -2.49, -2.42, -0.07)
    CALL CalcH("anthracene",                       "120-12-7", -3.14, -1.35, -1.79)
    CALL CalcH("phenanthrene",                      "85-01-8", -2.98, -3.08,  0.10)
    CALL CalcH("fluorene",                          "86-73-7", -2.37, -2.69,  0.32)
    CALL CalcH("pyrene",                           "129-00-0", -3.33, -3.13, -0.20)
    CALL CalcH("biphenyl",                          "92-52-4", -1.77, -1.30, -0.47)
    CALL CalcH("1-methylnaphthalene",               "90-12-0", -1.96, -1.75, -0.21)
    CALL CalcH("1,5-dimethylnaphthalene",          "571-61-9", -1.82, -2.43,  0.61)
    CALL CalcH("1-chloronaphthalene",               "90-13-1", -1.84, -2.21,  0.37)
    CALL CalcH("2-chloronaphthalene",               "91-58-7", -1.88, -2.21,  0.33)
    CALL CalcH("1,2,3-trichlorobenzene",            "87-61-6", -1.30, -1.65,  0.35)
    CALL CalcH("1,2,3,5-tetrachlorobenzene",       "634-90-2", -1.19, -1.78,  0.59)
    CALL CalcH("hexachlorobenzene",                "118-74-1", -1.27, -1.33,  0.06)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, obsd, fitted, residue)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: obsd, fitted, residue
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! check if residue is correct (+- rounding error):
      IF (ABS(obsd-fitted-residue)>0.011) THEN
        CALL PrintWarning("ref2482: incorrect residue for "//TRIM(casrn_))
        PRINT *, obsd, fitted, residue, ABS(obsd-fitted-residue)
      ENDIF
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**fitted))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2482

  !---------------------------------------------------------------------------

  SUBROUTINE ref2483 ! KHcc [1]
    IMPLICIT NONE

    ref = "2483"
    type = "M"

    CALL CalcH("ethanol",              "64-17-5") ! C2H5OH
    CALL CalcH("2-propanol",           "67-63-0") ! C3H7OH isopropanol
    CALL CalcH("2-methyl-1-propanol",  "78-83-1") ! C4H{10}O isobutanol
    CALL CalcH("butanone",             "78-93-3") ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("methylbenzene",       "108-88-3") ! C6H5CH3 toluene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      CALL MakeNote("cdep")
      CALL Output(DUMMY)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2483

  !---------------------------------------------------------------------------

  SUBROUTINE ref2484 ! Hcc [1] = Ostwald coefficient and Hxp [1/atm]
    IMPLICIT NONE

    ref = "2484"
    type = "M"

    CALL CalcH("tetrafluoromethane",      "75-73-0", & ! CF4
      (/ 5., 10., 15., 20., 25., 30. /) + CtoK, &
      (/ 8.42, 7.23, 6.23, 5.52, 5.04, 4.62 /) * 1E-3, &
      (/ 6.68, 5.62, 4.77, 4.16, 3.74, 3.37 /) * 1E-6 )
    CALL CalcH("hexafluoroethane",        "76-16-4", & ! C2F6
      (/ 5., 10., 15., 20., 25., 30. /) + CtoK, &
      (/ 2.69, 2.22, 1.88, 1.51, 1.38, 1.29 /)  * 1E-3, &
      (/ 2.15, 1.73, 1.45, 1.15, 1.03, 0.942 /) * 1E-6 )
    CALL CalcH("octafluoropropane",       "76-19-7", & ! C3F8
      (/ 5., 10., 15. /) + CtoK, &
      (/ 1.63, 0.846, 0.717 /) * 1E-3, &
      (/ 1.32, 0.674, 0.560 /) * 1E-6 )
    CALL CalcH("octafluorocyclobutane",  "115-25-3", & ! c-C4F8
      (/ 5., 10., 15., 20., 25., 30. /) + CtoK, &
      (/ 7.00, 5.35, 4.14, 3.38, 2.89, 2.52 /) * 1E-3, &
      (/ 5.73, 4.29, 3.21, 2.62, 2.20, 1.88 /) * 1E-6 )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, ostwald, x)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, ostwald, x
      REAL :: H_from_x, mindHR_from_x
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, Hcc_TO_HcpSI(ostwald, temp), Hominus, mindHR)
      CALL HTdep(temp, x * Hxp_TO_HcpSI, H_from_x, mindHR_from_x, "fromx")
      ! compare values:
      !print *, "from x: ", H_from_x, mindHR_from_x
      !print *, "from L: ", Hominus, mindHR
      !print *, "%diff = ", 100.*(Hominus-H_from_x)/Hominus, 100.*(mindHR-mindHR_from_x)/mindHR
      CALL Output(Hominus, mindHR, r2)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2484

  !---------------------------------------------------------------------------

  SUBROUTINE ref2485 ! Hcc [1] = Ostwald coefficient
    IMPLICIT NONE

    ref = "2485"
    type = "M"

    CALL CalcH("dichlorodifluoromethane", "75-71-8", &
      (/ 288.19, 298.14, 308.12, 318.25 /), 1E-2 * (/ 9.828, 7.110, 5.613, 4.397 /)     )
    CALL CalcH("chlorotrifluoromethane",  "75-72-9", &
      (/ 288.22, 298.06, 308.02, 318.02 /), 1E-2 * (/ 2.812, 2.253, 1.871, 1.683 /)     )
    CALL CalcH("tetrafluoromethane",      "75-73-0", &
      (/ 288.19, 298.17, 308.15, 318.20 /), 1E-2 * (/ 0.6399, 0.5083, 0.4592, 0.4391 /) )
    CALL CalcH("octafluorocyclobutane",  "115-25-3", &
      (/ 288.22, 298.16, 308.13, 318.23 /), 1E-2 * (/ 0.4673, 0.3119, 0.2355, 0.1984 /) )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, ostwald)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, ostwald
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, Hcc_TO_HcpSI(ostwald, temp), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2485

  !---------------------------------------------------------------------------

  SUBROUTINE ref2486 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "2486"
    type = "M"

    CALL CalcH("trichlorofluoromethane",  "75-69-4", 0.2491)   ! CFCl3 R11
    CALL CalcH("dichlorodifluoromethane", "75-71-8", 0.06572)  ! CF2Cl2 R12
    CALL CalcH("chlorotrifluoromethane",  "75-72-9", 0.01771)  ! CF3Cl R13
    CALL CalcH("tetrafluoromethane",      "75-73-0", 0.004751) ! CF4
    CALL CalcH("hexafluoroethane",        "76-16-4", 0.001206) ! C2F6
    CALL CalcH("octafluorocyclobutane",  "115-25-3", 0.002689) ! c-C4F8
    CALL CalcH("sulfur hexafluoride",   "2551-62-4", 0.005440) ! SF6
    CALL CalcH("argon",                 "7440-37-1", (0.03122+0.03105)/2.) ! Ar

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, alpha)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: alpha
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(alpha*alpha_TO_HcpSI)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2486

  !---------------------------------------------------------------------------

  SUBROUTINE ref2487 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2487"
    type = "V"

    CALL CalcH("camphor",               "76-22-2", 1.22  )
    CALL CalcH("camphene",              "79-92-5", 1600. )
    CALL CalcH("3-carene",           "13466-78-9", 13640.)
    CALL CalcH("1,8-cineole",          "470-82-6", 13.6  )
    CALL CalcH("p-cymene",              "99-87-6", 1100. )
    CALL CalcH("limonene",             "138-86-3", 2850. )
    CALL CalcH("linalool",              "78-70-6", 2.09  )
    CALL CalcH("myrcene",              "123-35-3", 6300. )
    CALL CalcH("cis-beta-ocimene",    "3338-55-4", 2470. )
    CALL CalcH("trans-beta-ocimene",  "3779-61-1", 3330. )
    CALL CalcH("alpha-phellandrene",    "99-83-2", 6950. )
    CALL CalcH("beta-phellandrene",    "555-10-2", 5670. )
    CALL CalcH("alpha-pinene",          "80-56-8", 13600.)
    CALL CalcH("beta-pinene",          "127-91-3", 6830. )
    CALL CalcH("sabinene",            "3387-41-5", 6450. )
    CALL CalcH("alpha-terpinene",       "99-86-5", 1960. )
    CALL CalcH("gamma-terpinene",       "99-85-4", 3590. )
    CALL CalcH("alpha-terpineol",       "98-55-5", 0.239 )
    CALL CalcH("alpha-terpinolene",    "586-62-9", 2600. )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2487

  !---------------------------------------------------------------------------

  SUBROUTINE ref2488 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2488"
    type = "M"

    CALL CalcH("hydroxybenzene",            "108-95-2",  647., 7684.) ! C6H5OH phenol
    CALL CalcH("1-hydroxy-2-methylbenzene",  "95-48-7",  424., 8544.) ! HOC6H4CH3 2-cresol; $o$-cresol
    CALL CalcH("1-hydroxy-3-methylbenzene", "108-39-4",  798., 9028.) ! HOC6H4CH3 3-cresol; $m$-cresol
    CALL CalcH("1-hydroxy-4-methylbenzene", "106-44-5", 1025., 9258.) ! HOC6H4CH3 4-cresol; $p$-cresol

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC, mindHR_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      REAL,             INTENT(IN) :: mindHR_
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI * HLC, mindHR_)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2488

  !---------------------------------------------------------------------------

  SUBROUTINE ref2489 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2489"
    type = "M"

    CALL CalcH("anthracene",               "120-12-7", &
      (/ 282.09,284.72,286.54,289.03,291.74,295.69,297.76,300.25,301.35,302.27,303.68,308.02,313.06,318.05,323.07 /) , &
      (/ 1.44,1.76,2.09,2.41,2.97,3.81,4.30,5.16,5.56,5.88,6.27,7.73,10.61,13.98,18.92 /) ) ! C{14}H{10}
    CALL CalcH("pyrene",                   "129-00-0", &
      (/ 281.69,283.54,286.65,287.61,289.85,291.20,294.68,298.70,300.51,302.81,305.43 /) , &
      (/ 0.35,0.38,0.52,0.55,0.64,0.71,0.90,1.23,1.37,1.62,1.96 /) ) ! C{16}H{10}
    CALL CalcH("9,10-dihydrophenanthrene", "776-35-2", &
      (/ 277.96,279.15,283.82,288.05,293.14,298.16 /) , &
      (/ 1.41,1.58,2.47,3.58,5.36,9.00 /) ) ! C{14}H{12}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(H)
      ALLOCATE(Harray(ndata))
      Harray = KHpcSI_TIMES_HcpSI/H
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2489

  !---------------------------------------------------------------------------

  SUBROUTINE ref2491 ! KHpb [mmHg/(mmol/kg)]
    IMPLICIT NONE

    ref = "2491"
    type = "M"

    CALL CalcH("benzene",             "71-43-2", 4.9)  ! C6H6
    CALL CalcH("1,2-dichloroethane", "107-06-2", 1.04) ! CH2ClCH2Cl
    CALL CalcH("3-pentanone",         "96-22-0", 77.1) ! C2H5COC2H5

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, p_c)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: p_c
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus =  rhoH2O / (1E3 * p_c * mmHg)
      CALL MakeNoteOtherTemp("303")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2491

  !---------------------------------------------------------------------------

  SUBROUTINE ref2492 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2492"
    type = "M"

    CALL CalcH("trans-chlordane",    "5103-74-2", 29., -1524.) ! C{10}H6Cl8 $\beta$-chlordane; $\gamma$-chlordane
    CALL CalcH("cis-chlordane",      "5103-71-9", 27., -1786.) ! C{10}H6Cl8 $\alpha$-chlordane
    CALL CalcH("trans-nonachlor",   "39765-80-5", 32., -2068.) ! C{10}H5Cl9
    CALL CalcH("hexachlorobenzene",   "118-74-1", 33., -3013.) ! C6Cl6
    CALL CalcH("p,p'-DDE",             "72-55-9", 35., -2043.) ! C{14}H8Cl4

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC, m)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC, m
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/HLC
      mindHR = -m * LOG(10.)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2492

  !---------------------------------------------------------------------------

  SUBROUTINE ref2493 ! Hcc [1]
    IMPLICIT NONE

    ref = "2493"
    type = "M"

    CALL CalcH("dichloromethane",         "75-09-2",  9.33) ! CH2Cl2 methylene chloride
    CALL CalcH("diisopropyl ether",      "108-20-3", 10.99) ! C3H7OC3H7
    CALL CalcH("1,2-dichloroethane",     "107-06-2", 20.23) ! CH2ClCH2Cl
    CALL CalcH("bromoethane",             "74-96-4",  3.23) ! C2H5Br
    CALL CalcH("trichloromethane",        "67-66-3",  5.85) ! CHCl3 chloroform
    CALL CalcH("2-chloropropane",         "75-29-6",  1.35) ! C3H7Cl
    CALL CalcH("1-chloropropane",        "540-54-5",  1.71) ! C3H7Cl
    CALL CalcH("2-bromopropane",          "75-26-3",  2.08) ! C3H7Br
    CALL CalcH("iodoethane",              "75-03-6",  3.45) ! C2H5I
    CALL CalcH("dipropyl ether",         "111-43-3",  7.11) ! C3H7OC3H7
    CALL CalcH("benzene",                 "71-43-2",  4.36) ! C6H6
    CALL CalcH("1-bromopropane",         "106-94-5",  2.61) ! C3H7Br
    CALL CalcH("1,1,1-trichloroethane",   "71-55-6",  1.41) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1-chlorobutane",         "109-69-3",  1.32) ! C4H9Cl
    CALL CalcH("2-bromobutane",           "78-76-2",  1.91)
    CALL CalcH("1-iodopropane",          "107-08-4",  2.79) ! C3H7I
    CALL CalcH("trichloroethene",         "79-01-6",  2.49) ! C2HCl3 trichloroethylene
    CALL CalcH("methylbenzene",          "108-88-3",  3.93) ! C6H5CH3 toluene
    CALL CalcH("1-bromobutane",          "109-65-9",  2.04) ! C4H9Br
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6",  5.11) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1-chloropentane",        "543-59-9",  1.03) ! C5H{11}Cl
    CALL CalcH("ethylbenzene",           "100-41-4",  3.28) ! C6H5C2H5
    CALL CalcH("1,3-dimethylbenzene",    "108-38-3",  3.73) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",    "106-42-3",  3.56) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("tetrachloroethene",      "127-18-4",  1.56) ! C2Cl4 tetrachloroethylene
    CALL CalcH("dibutyl ether",          "142-96-1",  3.24) ! C4H9OC4H9
    CALL CalcH("(2-propyl)-benzene",      "98-82-8",  2.20) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("1,3,5-trimethylbenzene", "108-67-8",  3.52) ! C6H3(CH3)3 mesitylene
    CALL CalcH("propylbenzene",          "103-65-1",  2.25) ! C6H5C3H7
    CALL CalcH("1-chlorohexane",         "544-10-5",  0.77) ! C6H{13}Cl
    CALL CalcH("butylbenzene",           "104-51-8",  1.65) ! C6H5C4H9

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: Hcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI_atT0 * Hcc
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2493

  !---------------------------------------------------------------------------

  SUBROUTINE ref2494 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2494"
    type = "V"

    CALL CalcH("hexabromobenzene",                              "87-82-1", 0.14  )
    CALL CalcH("4,4'-dibromodiphenyl ether",                  "2050-47-7", 21.   ) ! BDE-15
    CALL CalcH("2,4,4'-tribromodiphenyl ether",              "41318-75-6", 5.1   ) ! BDE-28
    CALL CalcH("2,2',4,4'-tetrabromodiphenyl ether",          "5436-43-1", 1.5   ) ! BDE-47
    CALL CalcH("2,3',4,4'-tetrabromodiphenyl ether",        "189084-61-5", 0.50  ) ! BDE-66
    CALL CalcH("3,3',4,4'-tetrabromodiphenyl ether",         "93703-48-1", 1.2   ) ! BDE-77
    CALL CalcH("2,2',3,4,4'-pentabromodiphenyl ether",      "182346-21-0", 0.11  ) ! BDE-85
    CALL CalcH("2,2',4,4',5-pentabromodiphenyl ether",       "60348-60-9", 0.23  ) ! BDE-99
    CALL CalcH("2,2',4,4',6-pentabromodiphenyl ether",      "189084-64-8", 0.069 ) ! BDE-100
    CALL CalcH("2,2',4,4',5,5'-hexabromodiphenyl ether",     "68631-49-2", 0.067 ) ! BDE-153
    CALL CalcH("2,2',4,4',5,6'-hexabromodiphenyl ether",    "207122-15-4", 0.24  ) ! BDE-154
    CALL CalcH("2,2',3,4,4',5',6-heptabromodiphenyl ether", "207122-16-5", 0.0074) ! BDE-183

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H25)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H25
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/H25)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2494

  !---------------------------------------------------------------------------

  SUBROUTINE ref2496 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2496"
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 5., 15., 25., 35. /) + CtoK

    ! PCBs from Table 6:
    CALL CalcH("2-chlorobiphenyl",                       "2051-60-7", (/ 9.0,  19.,  33.,  58.  /) ) ! PCB-1
    CALL CalcH("4-chlorobiphenyl",                       "2051-62-9", (/ 5.7,  13.,  28.,  60.  /) ) ! PCB-3
    CALL CalcH("2,4'-dichlorobiphenyl",                 "34883-43-7", (/ 14.,  20.,  39.,  88.  /) ) ! PCB-8
    CALL CalcH("4,4'-dichlorobiphenyl",                  "2050-68-2", (/ 10.,  13.,  20.,  70.  /) ) ! PCB-15
    CALL CalcH("2,4,4'-trichlorobiphenyl",               "7012-37-5", (/ 39.,  32.,  44.,  88.  /) ) ! PCB-28
    CALL CalcH("2,2',4,4'-tetrachlorobiphenyl",          "2437-79-8", (/ 20.,  15.,  4.7,  2.8  /) ) ! PCB-47
    CALL CalcH("3,3',4,4'-tetrachlorobiphenyl",         "32598-13-3", (/ 0.74, 24.,  32.,  96.  /) ) ! PCB-77
    CALL CalcH("2,2',4,4',5-pentachlorobiphenyl",       "38380-01-7", (/ 5.7,  117., 231., 123. /) ) ! PCB-99
    CALL CalcH("2,3',4,4',5-pentachlorobiphenyl",       "31508-00-6", (/ 0.60, 48.,  88.,  104. /) ) ! PCB-118
    ! PBDEs from Table 7:
    CALL CalcH("4-bromodiphenyl ether",                   "101-55-3", (/ 4.7,  8.1,  20.,  29.  /) ) ! PBDE-3
    CALL CalcH("4,4'-dibromodiphenyl ether",             "2050-47-7", (/ 4.6,  8.5,  12.,  24.  /) ) ! PBDE-15
    CALL CalcH("2,4,4'-tribromodiphenyl ether",         "41318-75-6", (/ 0.18, 9.6,  9.5,  16.  /) ) ! PBDE-28
    CALL CalcH("2,2',4,4'-tetrabromodiphenyl ether",     "5436-43-1", (/ 9.6,  2.1,  6.4,  8.8  /) ) ! PBDE-47
    CALL CalcH("2,2',4,4',5-pentabromodiphenyl ether",  "60348-60-9", (/ 48.,  2.7,  1.6,  4.7  /) ) ! PBDE-99
    CALL CalcH("2,2',4,4',6-pentabromodiphenyl ether", "189084-64-8", (/ 16.,  1.2,  3.0,  13.  /) ) ! PBDE-100
    CALL CalcH("2,3',4,4',5-Pentabromodiphenyl ether", "446254-77-9", (/ 1.1,  0.14, 1.6,  2.5  /) ) ! PBDE-118

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, henry)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: henry
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = 1. / henry
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2496

  !---------------------------------------------------------------------------

  SUBROUTINE ref2497 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2497"
    type = "M"

    ! Table 1 (and Table 5):
    CALL CalcH("PFBHA-methanal",                      "_CAS-60",  61.3, 59.9E3)
    CALL CalcH("PFBHA-ethanal",                       "_CAS-61",  53.0, 44.7E3)
    CALL CalcH("PFBHA-propanone",                     "_CAS-62",  90.3, 31.7E3)
    CALL CalcH("PFBHA-butanone",                      "_CAS-63", 215.,  50.0E3)
    CALL CalcH("PFBHA-2-pentanone",                   "_CAS-64", 268.,  18.2E3)
    CALL CalcH("PFBHA-hexanal",                       "_CAS-65", 173.         )
    CALL CalcH("PFBHA-octanal",                       "_CAS-66", 126.         )
    CALL CalcH("PFBHA-decanal",                       "_CAS-67",  41.         )
    CALL CalcH("PFBHA-propenal",                      "_CAS-68", 105.2, 44.6E3)
    CALL CalcH("PFBHA-(E)-2-butenal",                 "_CAS-69", 147.,  28.5E3)
    CALL CalcH("PFBHA-benzaldehyde",                  "_CAS-70", 199.,  17.0E3)
    CALL CalcH("PFBHA-4-methyl-benzaldehyde",         "_CAS-71", 151.         )
    CALL CalcH("PFBHA-9-fluorenone",                  "_CAS-72",  91.         )
    CALL CalcH("PFBHA-ethanedial",                    "_CAS-73",  62.         )
    CALL CalcH("PFBHA-1-hydroxypropanone",            "_CAS-74",  37.         )
    CALL CalcH("PFBHA-3-hydroxy-3-methyl-2-butanone", "_CAS-75",  81.         )
    ! Table 2:
    CALL CalcH("methylbenzene",                 "108-88-3", 495.0) ! C6H5CH3 toluene
    CALL CalcH("naphthalene",                    "91-20-3",  42.2) ! C{10}H8
    CALL CalcH("biphenyl",                       "92-52-4",  29.6) ! (C6H5)2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Harray, DeltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Harray
      REAL, OPTIONAL,   INTENT(IN) :: DeltaH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (PRESENT(DeltaH)) THEN
        mindhr = DeltaH / Rgas
        CALL Output(KHpcSI_TIMES_HcpSI/Harray, mindHR)
      ELSE
        CALL Output(KHpcSI_TIMES_HcpSI/Harray)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2497

  !---------------------------------------------------------------------------

END MODULE Henry_ref2500

!*****************************************************************************
!                                  end of file
!*****************************************************************************

