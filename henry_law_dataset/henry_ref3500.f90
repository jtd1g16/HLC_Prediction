!*****************************************************************************
!                   Time-stamp: <2015-04-10 16:23:18 sander>
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

MODULE henry_ref3500

  USE henry_mem
  USE henry_util
  IMPLICIT NONE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ref3003 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3003"
    type = "M"

    chem = "ammonia" ; casrn = "7664-41-7" ! NH3
    mindHR = 3214.4
    Hominus = EXP(-7.44+mindHR/T0)*Hcp_TO_HcpSI
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref3003

  !---------------------------------------------------------------------------

  SUBROUTINE ref3005 ! KHpx [kPa]
    IMPLICIT NONE
    REAL :: KH_kPa
    chem = "ozone" ; casrn = "10028-15-6" ! O3
    ref = "3005"
    type = "M"
    mindHR = 1364.5
    KH_kPa = 4.67E7*EXP(-mindHR/T0)
    Hominus = cH2O/(1E3*KH_kPa)
    CALL Output(Hominus,mindHR)
  END SUBROUTINE ref3005

  !---------------------------------------------------------------------------

  SUBROUTINE ref3006 ! KHpx [kPa], KHcc [1]
    IMPLICIT NONE

    ref = "3006"
    type = "V"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 298.15, 308.15, 318.15, 328.15, 338.15 /)
    CALL CalcH_Tdep( "fluoranthene",   "206-44-0", &
      (/ 29.9,   68.3,  193.,  425.,  911. /), (/ 21.7,   48.0,   131.,  280.,  583. /))
    CALL CalcH_Tdep( "1,2,3,4-dibenzantheracene DBA(a,c)",   "215-58-7", &
      (/ 2.61,   7.17,  23.9,  45.7,  71.7 /), (/ 1.89,   5.04,   16.3,  30.1,  45.9 /))
    CALL CalcH_Tdep( "1,2,5,6-dibenzantheracene DBA(a,h)",   "53-70-3", &
      (/ 0.297,  1.22,  3.37,  12.2,  32.3 /), (/ 0.216,  0.858,  2.30,  8.06,  20.7 /))
    DEALLOCATE(temp, Harray)

CONTAINS

    SUBROUTINE CalcH_Tdep (chem_, casrn_, KHpx, Kaw)

      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHpx, Kaw
      REAL :: Hominus_from_KHpx, mindHR_from_KHpx
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      Harray = KHcc_TO_HcpSI(1E-5*Kaw,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)

      ! compare to Kaw values from the same table:
      Harray = cH2O/(1E3*KHpx)
      CALL HTdep(temp, Harray, Hominus_from_KHpx, mindHR_from_KHpx)
      CALL consistency_check(Hominus_from_KHpx, Hominus, &
        "Hominus Values in Table 11")
      CALL consistency_check(mindHR_from_KHpx, mindHR, &
        "mindHR Values in Table 11")

    END SUBROUTINE CalcH_Tdep

  END SUBROUTINE ref3006

  !---------------------------------------------------------------------------

  SUBROUTINE ref3007 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3007"
    type = "M"

    chem = "methylbenzene" ; casrn = "108-88-3" ! C6H5CH3 toluene
    CALL Output(0.157*Hcp_TO_HcpSI)

    chem = "2-butanamine" ; casrn = "13952-84-6" ! C4H{11}N
    CALL MakeNote(TRIM(ref), "In their Fig.~5b, "//TRIM(citet())// &
      " apply an unspecified factor to the Henry's law constant, " // &
      "and it is not clear if the temperature dependence shown " // &
      "there is correct (Y.~Liu, pers.\ comm.\ 2014).")
    CALL Output(40.7*Hcp_TO_HcpSI, 7657.)

    chem = "cineole" ; casrn = "470-82-6" ! 1,8-cineole
    CALL Output(6.0*Hcp_TO_HcpSI)

  END SUBROUTINE ref3007

  !---------------------------------------------------------------------------

  SUBROUTINE ref3008 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3008"
    type = "M"
    ndata = 5
    ALLOCATE(temp(ndata))
    temp = (/ 298., 293., 288., 283., 278. /)
    CALL CalcH("isoprene",      "78-79-5", (/ 0.036, 0.044, 0.056, 0.079, 0.100 /)) ! C_5H_8
    CALL CalcH("limonene",     "138-86-3", (/ 0.048, 0.066, 0.083, 0.112, 0.147 /)) ! C_{10}H_{16}
    CALL CalcH("alpha-pinene",  "80-56-8", (/ 0.029, 0.034, 0.036, 0.042, 0.045 /)) ! C_{10}H_{16}
    CALL CalcH("linalool",      "78-70-6", (/ 21.20, 25.97, 33.63, 43.22, 62.08 /)) ! C_{10}H_{18}O
    DEALLOCATE(temp)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Harray)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: Harray
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, Harray*Hcp_TO_HcpSI, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3008

  !---------------------------------------------------------------------------

  SUBROUTINE ref3009 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "3009"
    type = "M"
    ! Tab. 2:
    CALL CalcH("1,2,4,5-tetrachlorobenzene",  "95-94-3", 57.) ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",         "608-93-5", 33.) ! C6HCl5
    CALL CalcH("hexachlorobenzene",          "118-74-1", 30.) ! C6Cl6

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

  END SUBROUTINE ref3009

  !---------------------------------------------------------------------------

  SUBROUTINE ref3010 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "3010"
    type = "M"
    chem = "ethanedial" ; casrn = "107-22-2" ! OHCCHO glyoxal

    CALL MakeNote(TRIM(ref), &
      "Effective value suitable for the conditions of a case study in " // &
      "Mexico City.")
    CALL Output(5E7 * rhoH2O / atm)

  END SUBROUTINE ref3010

  !---------------------------------------------------------------------------

  SUBROUTINE ref3013 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "3013"

    ! Tab. 2:
    type = "M"
    CALL CalcH_Tdep("4-2FTOH",  "2043-47-2", &
      (/ 309.6, 314.5, 319.6, 323., 330., 334.2 /), &
      (/ 283., 314., 399., 483., 631., 806. /), &
      (/ 2.45, 2.50, 2.60, 2.68, 2.80, 2.91 /))
    CALL CalcH_Tdep("6-2FTOH",   "647-42-7", &
      (/ 309.6, 314.5, 319.6, 323., 330., 334.2 /), &
      (/ 7259., 9413., 10894., 11467., 12456., 14309. /), &
      (/ 3.86, 3.97, 4.04, 4.06, 4.10, 4.16 /))
    CALL CalcH_Tdep("8-2FTOH",   "678-39-7", &
      (/ 309.6, 314.5, 319.6, 323. /), &
      (/ 7593., 8550., 10257., 11520. /), &
      (/ 3.88, 3.93, 4.01, 4.06 /))
    CALL CalcH_Tdep("10-2FTOH",  "865-86-1", &
      (/ 309.6, 314.5, 319.6, 323., 330., 334.2 /), &
      (/ 9987., 12943., 15119., 16193., 18245., 19589. /), &
      (/ 4.00, 4.11, 4.18, 4.21, 4.26, 4.29 /))

    ! Tab. 4:
    type = "V"
    CALL CalcH("4-2FTOH",  "2043-47-2", 1.77)
    CALL CalcH("6-2FTOH",   "647-42-7", 2.54)
    CALL CalcH("8-2FTOH",   "678-39-7", 4.10)
    CALL CalcH("10-2FTOH",  "865-86-1", 4.01)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/KH)
    END SUBROUTINE CalcH

    SUBROUTINE CalcH_Tdep (chem_, casrn_, temp, KH, logKH)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, KH, logKH
      REAL :: Hominus_from_logKH, mindHR_from_logKH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, KHpcSI_TIMES_HcpSI/KH, Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL Output(Hominus, mindHR, r2)
      ! compare to log(KH) values from the same Table:
      CALL HTdep(temp, KHpcSI_TIMES_HcpSI/(10.**logKH), Hominus_from_logKH, mindHR_from_logKH)
      CALL consistency_check(Hominus_from_logKH, Hominus, &
        "Hominus Values in Table 2")
      CALL consistency_check(mindHR_from_logKH, mindHR, &
        "mindHR Values in Table 2")
    END SUBROUTINE CalcH_Tdep

  END SUBROUTINE ref3013

  !---------------------------------------------------------------------------

  SUBROUTINE ref3014 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "3014"
    type = "M"
    ! Tab. 2:
    CALL CalcH("naphthalene",                 "91-20-3", 16.7    ) ! C{10}H8
    CALL CalcH("acenaphthene",                "83-32-9", 3.84    ) ! C{12}H{10}
    CALL CalcH("2,3-benzindene",              "86-73-7", 3.12    ) ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",                "85-01-8", 5.61    ) ! C{14}H{10}
    CALL CalcH("anthracene",                 "120-12-7", 6.33    ) ! C{14}H{10}
    CALL CalcH("benzo[$jk$]fluorene",        "206-44-0", 2.98    ) ! C{16}H{10} fluoranthene
    CALL CalcH("pyrene",                     "129-00-0", 2.44    ) ! C{16}H{10}
    CALL CalcH("benz[$a$]anthracene",         "56-55-3", 0.600   ) ! C{18}H{12}
    CALL CalcH("chrysene",                   "218-01-9", 0.478   ) ! C{18}H{12}
    CALL CalcH("benzo[$k$]fluoranthene",     "207-08-9", 0.0958  ) ! C{20}H{12}
    CALL CalcH("benzo[$a$]pyrene",            "50-32-8", 0.161   ) ! C{20}H{12} benz[$a$]pyrene
    CALL CalcH("1,2,3-trichlorobenzene",      "87-61-6", 125.    ) ! C6H3Cl3
    CALL CalcH("1,2,4-trichlorobenzene",     "120-82-1", 171.    ) ! C6H3Cl3
    CALL CalcH("1,2,4,5-tetrachlorobenzene",  "95-94-3", 152.    ) ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",         "608-93-5", 179.    ) ! C6HCl5
    CALL CalcH("hexachlorobenzene",          "118-74-1", 132.    ) ! C6Cl6
    CALL CalcH("dibutyl phthalate",           "84-74-2", 0.108   ) ! C{16}H{22}O4
    CALL CalcH("butyl benzyl phthalate",      "85-68-7", 0.00969 ) ! C{19}H{20}O4
    CALL CalcH("musk xylene",                 "81-15-2", 3.11    ) ! C{12}H{15}N3O6 musk xylol
    CALL CalcH("musk ketone",                 "81-14-1", 0.336   ) ! C{14}H{18}N2O5

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

  END SUBROUTINE ref3014

  !---------------------------------------------------------------------------

  SUBROUTINE ref3015 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "3015"
    type = "M"
    chem = "silicic acid" ; casrn = "10193-36-9" ! Si(OH)4
    ! Tab. 2:
    ndata = 23
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 273.15, 298.15, 323.15, 348.15, 373.15, 398.15, 423.15, 448.15, 473.15, &
      498.15, 523.15, 548.15, 573.15, 598.15, 623.15, 628.15, 633.15, 638.15, &
      643.15, 644.15, 645.15, 646.15, 647.096 /)
    Harray = 1E-6 * cH2O / EXP( (/ -31.185, -26.803, -23.104, -19.970, -17.294, -14.991, -12.993, &
      -11.247, -9.709, -8.342, -7.117, -6.006, -4.979, -3.999, -2.991, &
      -2.767, -2.523, -2.245, -1.896, -1.803, -1.689, -1.532, -1.054 /) )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref), "Extrapolated from data at elevated temperatures.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref3015

  !---------------------------------------------------------------------------

  SUBROUTINE ref3022 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3022"
    type = "V"

    CALL CalcH("oxalic acid",    "144-62-7", 6.2E8        )
    CALL CalcH("malonic acid",   "141-82-2", 3.9E10,  92. )
    CALL CalcH("malonic acid",   "141-82-2", 9.4E9,  113. )
    CALL CalcH("succinic acid",  "110-15-6", 4.2E9,   94. )
    CALL CalcH("succinic acid",  "110-15-6", 2.0E9,   97. )
    CALL CalcH("glutaric acid",  "110-94-1", 5.2E9,   97. )
    CALL CalcH("glutaric acid",  "110-94-1", 2.4E9,  109. )
    CALL CalcH("adipic acid",    "124-04-9", 6.7E9,  105. )
    CALL CalcH("pimelic acid",   "111-16-0", 8.2E9,  121. )
    CALL CalcH("suberic acid",   "505-48-6", 7.8E9,  120. )
    CALL CalcH("azelaic acid",   "123-99-9", 9.0E9,  140. )
    CALL CalcH("sebacic acid",   "111-20-6", 7.7E9        )
    CALL CalcH("malic acid",    "6915-15-7", 2.7E10       )
    CALL CalcH("tartaric acid",   "87-69-4", &
      range=TRIM(H_range(7E16*Hcp_TO_HcpSI,93E16*Hcp_TO_HcpSI)))
    CALL CalcH("citric acid",     "77-92-9", &
      range=TRIM(H_range(2E16*Hcp_TO_HcpSI,60E16*Hcp_TO_HcpSI)))

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, minDHg, range)
      IMPLICIT NONE
      CHARACTER(LEN=*),             INTENT(IN) :: chem_
      CHARACTER(LEN=*),             INTENT(IN) :: casrn_
      REAL,             OPTIONAL,   INTENT(IN) :: H
      REAL,             OPTIONAL,   INTENT(IN) :: minDHg ! minus Delta Hg
      CHARACTER(LEN=*), OPTIONAL,   INTENT(IN) :: range
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (PRESENT(range)) THEN
        CALL MakeNote(TRIM(ref)//"-"//casrn_//"-range", TRIM(citet())// &
          " recommend $\Hsymbol$ for "//chem_//" in the range of "//range//".")
      ENDIF
      IF (PRESENT(H)) THEN
        Hominus = Hcp_TO_HcpSI * H
        IF (PRESENT(minDHg)) THEN
          CALL Output(Hominus, 1E3*minDHg/Rgas)
        ELSE
          CALL Output(Hominus)
        ENDIF
      ELSE
        CALL Output(DUMMY)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref3022

  !---------------------------------------------------------------------------

  SUBROUTINE ref3024 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "3024"
    type = "V"
    ! Tab. 4:
    CALL CalcH("BDE-47",            "5436-43-1", 1.08    ) ! C{12}H6Br4O
    CALL CalcH("BDE-99",           "60348-60-9", 4.74e-1 ) ! C{12}H5Br5O
    CALL CalcH("BDE-153",          "68631-49-2", 1.63e-1 ) ! C{12}H4Br6O
    CALL CalcH("hexabromobenzene",    "87-82-1", 2.43    ) ! C6Br6
    CALL CalcH("BTBPE",            "37853-59-1", 5.56e-2 ) ! C{14}H8Br6O2

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

  END SUBROUTINE ref3024

  !---------------------------------------------------------------------------

  SUBROUTINE ref3025 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3025"
    type = "Q"

    CALL CalcH("ethyl butanoate", "105-54-4", 0.00041) ! C3H7COOC2H5 ethyl butyrate
    CALL CalcH("ethyl hexanoate", "123-66-0", 0.00072) ! C5H{11}COOC2H5
    CALL CalcH("ethyl octanoate", "106-32-1", 0.00127) ! C7H{15}COOC2H5
    CALL CalcH("cumene",           "98-82-8", 0.01050) ! C6H5C3H7 isopropylbenzene
    CALL CalcH("1-octanol",       "111-87-5", 0.00003) ! C8H{18}O
    CALL CalcH("linalool",         "78-70-6", 0.00004) ! C{10}H{18}O

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*H)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3025

  !---------------------------------------------------------------------------

  SUBROUTINE ref3026 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "3026"
    type = "M"

    CALL CalcH("hydrogen", "1333-74-0", (/ 0.50, 0.58, 0.62, 10.07, &
      9.85, 10.00, 20.00, 20.03, 20.00, 30.01, 30.00, 29.98, 30.00, 30.00, &
      30.00, 39.96, 39.89, 39.88, 40.00, 40.00, 40.00, 50.02, 49.85, 49.98, &
      50.10, 50.03, 49.90/), &
      (/ 0.02141, 0.02134, 0.02132, 0.01956, 0.01961, 0.01948, 0.01828, &
      0.01815, 0.01815, 0.01692, 0.01696, 0.01671, 0.01712, 0.01714, &
      0.01706, 0.01649, 0.01639, 0.01636, 0.01644, 0.01645, 0.01648, &
      0.01603, 0.01609, 0.01609, 0.01616, 0.01607, 0.01606 /) ) ! H2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, alpha)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, alpha
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(alpha)
      ALLOCATE(Harray(ndata))
      Harray = alpha * alpha_TO_HcpSI
      CALL HTdep(temp_+CtoK, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3026

  !---------------------------------------------------------------------------

  SUBROUTINE ref3027 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ! The absorption coefficient beta is not defined in this paper but
    ! it is assumed that it has the same definition as in ref3026.

    ref = "3027"
    type = "M"

    CALL CalcH("nitrogen", "7727-37-9", (/ 0.09, 0.12, 0.10, 0.07, 0.05, &
      0.07, 10.02, 9.97, 10.00, 10.00, 10.05, 20.04, 20.02, 19.99, 19.93, &
      20.05, 20.05, 20.00, 20.00, 30.00, 29.95, 29.93, 30.01, 30.00, 29.93, &
      39.94, 39.92, 40.04, 40.01, 40.05, 40.05, 50.00, 50.03, 49.88, 50.07, &
      49.97, 50.05 /), &
      ! high-temperature data not used: 60.08, 60.05, 60.00, 60.01,
      ! 59.96, 70.05, 70.01, 69.90, 80.03, 80.07, 79.88, 79.95, 80.04
      (/ 0.02340, 0.02342, 0.02342, 0.02345, 0.02345, 0.02347, 0.01854, &
      0.01852, 0.01860, 0.01860, 0.01856, 0.01540, 0.01545, 0.01542, &
      0.01536, 0.01546, 0.01544, 0.01521, 0.01542, 0.01345, 0.01341, &
      0.01341, 0.01341, 0.01334, 0.01338, 0.01180, 0.01182, 0.01182, &
      0.01185, 0.01184, 0.01182, 0.01084, 0.01088, 0.01076, 0.01089, &
      0.01087, 0.01096 /) ) ! N2
      ! high-temperature data not used: 0.01026, 0.01027, 0.01021,
      ! 0.01018, 0.01016, 0.00977, 0.00983, 0.00968, 0.00968, 0.00944,
      ! 0.00953, 0.00970, 0.00949
    CALL CalcH("oxygen", "7782-44-7", (/ 20.40, 20.40, 20.35, 30.02, &
      30.00, 29.93, 40.00, 40.02, 40.00, 49.97, 50.00, 49.98 /),  &
      ! high-temperature data not used: 60.01, 60.04, 59.95, 69.95,
      ! 70.02, 70.03, 80.05, 79.86, 80.00
      (/ 0.03080, 0.03081, 0.03087, 0.02612, 0.02598, 0.02617, 0.02310, &
      0.02303, 0.02305, 0.02090, 0.02090, 0.02089 /)) ! O2
      ! high-temperature data not used: 0.01949, 0.01951, 0.01939,
      ! 0.01831, 0.01829, 0.01838, 0.01764, 0.01748, 0.01772

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, alpha)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, alpha
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(alpha)
      ALLOCATE(Harray(ndata))
      Harray = alpha * alpha_TO_HcpSI
      CALL HTdep(temp_+CtoK, Harray, Hominus, mindHR)
      CALL MakeNote(TRIM(ref), &
        TRIM(citet())//" also contains high-temperature data. However, only " // &
        "data up to 330\,K were used here to calculate the temperature dependence.")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3027

  !---------------------------------------------------------------------------

  SUBROUTINE ref3032 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3032"
    type = "M"

    CALL CalcH("benzene",               "71-43-2", 0.17) ! C6H6
    CALL CalcH("methylbenzene",        "108-88-3", 0.15) ! C6H5CH3 toluene
    CALL CalcH("1,4-dimethylbenzene",  "106-42-3", 0.13) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("1,3-dimethylbenzene",  "108-38-3", 0.13) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,2-dimethylbenzene",   "95-47-6", 0.19) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("ethenylbenzene",       "100-42-5", 0.27) ! C8H8 styrene
    CALL CalcH("propanal",             "123-38-6", 9.26) ! C2H5CHO propionaldehyde
    CALL CalcH("butanal",              "123-72-8", 6.19) ! C3H7CHO butyraldehyde
    CALL CalcH("3-methylbutanal",      "590-86-3", 2.14) ! C5H{10}O isovaleraldehyde
    CALL CalcH("pentanal",             "110-62-3", 3.98) ! C4H{9}CHO valeraldehyde
    CALL CalcH("butanone",              "78-93-3", 10.5) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("4-methyl-2-pentanone", "108-10-1", 3.93) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("butyl ethanoate",      "123-86-4", 2.41) ! CH3COOC4H9 butyl acetate
    CALL CalcH("2-methyl-1-propanol",   "78-83-1", 22.2) ! C4H{10}O isobutanol

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcp_TO_HcpSI * HLC
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3032

  !---------------------------------------------------------------------------

  SUBROUTINE ref3033 ! KHcc [1]
    IMPLICIT NONE

    ref = "3033"
    type = "M"

    ! Table 1:
    CALL CalcH("phosphorus trihydride", "7803-51-2", &
      (/ 276.15, 283.15, 296.15, 301.15 /), &
      (/ 3.186,  4.271,  6.415,  7.334  /) ) ! PH3 phosphine

    ! Alternatively, Eqn. (10) could be used which describes the
    ! temperature dependence of the Henry's law constant for sea water:
    ! KHcc = a + b * T with a = -42.548 and b = 0.166.
    ! The conversion to Hcp is:
    ! Hcp = 1 / (KHcc * R * T)
    !     = 1 / ((a + b * T) * R * T)
    ! The analytical derivative is:
    ! d(ln Hcp) / d(1/T) = T * (1 + b / (b + a / T))
    ! At T0 = 298.15, this yields 2861 K.

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, k)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, k
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, KHcc_TO_HcpSI(k,temp), Hominus, mindHR)
      CALL MakeNote(TRIM(ref)//"seawater", "Solubility in natural sea " // &
        "water. Measurements at different salinities were also " // &
        "performed, but only at a fixed temperature of 296.15 K.")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3033

  !---------------------------------------------------------------------------

  SUBROUTINE ref3034 ! KHpc [kPa*m3/mol]
    IMPLICIT NONE

    ref = "3034"

    CALL CalcH("perylene",                                  "198-55-0", 25, 0.000003,  12)
    CALL CalcH("naphthacene",                                "92-24-0", 25, 0.000004,  12)
    CALL CalcH("triphenylene",                              "217-59-4", 25, 0.00001,   12)
    CALL CalcH("benzo[a]pyrene",                             "50-32-8", 25, 0.0000465, 22)
    CALL CalcH("benzo[e]pyrene",                            "192-97-2", 17, 0.0000467, 22)
    CALL CalcH("chrysene",                                  "218-01-9", 25, 0.000065,  22)
    CALL CalcH("benzo[ghi]perylene",                        "191-24-2", 25, 0.000075,  12)
    CALL CalcH("benz[a]anthracene",                          "56-55-3", 25, 0.00058,   22)
    CALL CalcH("pyrene",                                    "129-00-0", 25, 0.00092,   22)
    CALL CalcH("fluoranthene",                              "206-44-0", 25, 0.00096,   22)
    CALL CalcH("diphenylmethane",                           "101-81-5", 25, 0.001,     12)
    CALL CalcH("acetophenone",                               "98-86-2", 20, 0.00108,   28)
    CALL CalcH("acetophenone",                               "98-86-2", 50, 0.00108,   28)
    CALL CalcH("acetophenone",                               "98-86-2", 80, 0.00108,   28)
    CALL CalcH("2-methyl-1-propanol",                        "78-83-1", 25, 0.00273,   28)
    CALL CalcH("epichlorohydrin",                           "106-89-8", 20, 0.003,     13) ! other CAS: 13403-37-7
    CALL CalcH("bis(2-chloroethyl) ether",                  "111-44-4", 20, 0.003,     13)
    CALL CalcH("phenanthrene",                               "85-01-8", 25, 0.00324,   22)
    CALL CalcH("anthracene",                                "120-12-7", 25, 0.00396,   22)
    CALL CalcH("2,2',3,3',4,4',6-heptachlorobiphenyl",    "52663-71-5", 25, 0.0054,    7)
    CALL CalcH("1-heptanol",                                "111-70-6", 25, 0.00562,   28)
    CALL CalcH("9h-fluorene",                                "86-73-7", 25, 0.00787,   22)
    CALL CalcH("2-pentanone",                               "107-87-9", 25, 0.00847,   28)
    CALL CalcH("methyloxirane",                              "75-56-9", 20, 0.0087,    13) ! other CAS: 16033-71-9
    CALL CalcH("dibenzofuran",                              "132-64-9", 25, 0.011,     12)
    CALL CalcH("acenaphthylene",                            "208-96-8", 20, 0.012,     28)
    CALL CalcH("acenaphthene",                               "83-32-9", 25, 0.01217,   22)
    CALL CalcH("1,2-diphenylethane",                        "103-29-7", 25, 0.017,     12)
    CALL CalcH("2-heptanone",                               "110-43-0", 25, 0.0171,    28)
    CALL CalcH("2-heptanone",                               "110-43-0", 90, 0.0171,    28)
    CALL CalcH("2,5-dichlorobiphenyl",                    "34883-39-1", 25, 0.0201,    7)
    CALL CalcH("decachlorobiphenyl",                       "2051-24-3", 25, 0.0208,    7)
    CALL CalcH("anisole",                                   "100-66-3", 20, 0.025,     13)
    CALL CalcH("anisole",                                   "100-66-3", 40, 0.025,     13)
    CALL CalcH("anisole",                                   "100-66-3", 81, 0.025,     13)
    CALL CalcH("1,1,2,2-tetrachloroethane",                  "79-34-5", 25, 0.026,     13)
    CALL CalcH("diphenyl ether",                            "101-84-8", 25, 0.027,     13)
    CALL CalcH("biphenyl",                                   "92-52-4", 25, 0.0280,    22)
    CALL CalcH("diiodomethane",                              "75-11-6", 30, 0.032,     13)
    CALL CalcH("2-chloronaphthalene",                        "91-58-7", 25, 0.0335,    28)
    CALL CalcH("2,2',3,3',4,4'-hexachlorobiphenyl",       "38380-07-3", 25, 0.0354,    31)
    CALL CalcH("1,5-dimethylnaphthalene",                   "571-61-9", 25, 0.036,     28)
    CALL CalcH("1-chloronaphthalene",                        "90-13-1", 25, 0.0363,    28)
    CALL CalcH("2,4,5-trichlorobiphenyl",                 "15862-07-4", 25, 0.0379,    31)
    CALL CalcH("1,2,3-trichloropropane",                     "96-18-4", 25, 0.038,     13)
    CALL CalcH("2,2',3,3',5,5',6,6'-octachlorobiphenyl",   "2136-99-4", 25, 0.0381,    7)
    CALL CalcH("1-ethylnaphthalene",                       "1127-76-0", 25, 0.039,     12)
    CALL CalcH("trans-stilbene",                            "103-30-0", 25, 0.040,     12)
    CALL CalcH("2,2',4,5,5'-pentachlorobiphenyl",         "37680-73-2", 25, 0.0421,    31)
    CALL CalcH("naphthalene",                                "91-20-3", 25, 0.043,     22)
    CALL CalcH("1-methylnaphthalene",                        "90-12-0", 25, 0.045,     22)
    CALL CalcH("tribromomethane",                            "75-25-2", 25, 0.047,     13)
    CALL CalcH("2,4,6-trichlorobiphenyl",                 "35693-92-6", 25, 0.0495,    7)
    CALL CalcH("2-methylnaphthalene",                        "91-57-6", 25, 0.051,     12)
    CALL CalcH("1,2-dibromoethane",                         "106-93-4", 50, 0.066,     13)
    CALL CalcH("2-chlorobiphenyl",                         "2051-60-7", 25, 0.0701,    7)
    CALL CalcH("2-ethylnaphthalene",                        "939-27-5", 25, 0.078,     12)
    CALL CalcH("iodobenzene",                               "591-50-4", 25, 0.078,     11)
    CALL CalcH("pentachlorobenzene",                        "608-93-5", 25, 0.085,     11)
    CALL CalcH("dibromomethane",                             "74-95-3", 20, 0.086,     13)
    CALL CalcH("diethyl ether",                              "60-29-7", 0,  0.088,     13)
    CALL CalcH("diethyl ether",                              "60-29-7", 25, 0.088,     13)
    CALL CalcH("diethyl ether",                              "60-29-7", 38, 0.088,     13)
    CALL CalcH("diethyl ether",                              "60-29-7", 82, 0.088,     13)
    CALL CalcH("1,1,2-trichloroethane",                      "79-00-5", 25, 0.092,     13)
    CALL CalcH("1-chloro-2-methylpropene",                  "513-37-1", 25, 0.12,      5)
    CALL CalcH("1,2,4,5-tetrachlorobenzene",                 "95-94-3", 25, 0.122,     11)
    CALL CalcH("hexachlorobenzene",                         "118-74-1", 25, 0.131,     11)
    CALL CalcH("1,2-dichloroethane",                        "107-06-2", 25, 0.14,      13)
    CALL CalcH("1,2,3,4-tetrachlorobenzene",                "634-66-2", 25, 0.144,     11)
    CALL CalcH("trans-1,3-dichloropropene",               "10061-02-6", 20, 0.18,      5)
    CALL CalcH("bromochloromethane",                         "74-97-5", 25, 0.18,      13)
    CALL CalcH("cis-1,3-dichloropropene",                 "10061-01-5", 20, 0.24,      5)
    CALL CalcH("1,1,1,2-tetrachloroethane",                 "630-20-6", 25, 0.24,      13)
    CALL CalcH("1,2,3-trichlorobenzene",                     "87-61-6", 25, 0.242,     11)
    CALL CalcH("p-dichlorobenzene",                         "106-46-7", 25, 0.244,     28)
    CALL CalcH("bromobenzene",                              "108-86-1", 25, 0.250,     28)
    CALL CalcH("pentachloroethane",                          "76-01-7", 25, 0.25,      13)
    CALL CalcH("dipropyl ether",                            "111-43-3", 0,  0.26,      13)
    CALL CalcH("dipropyl ether",                            "111-43-3", 25, 0.26,      13)
    CALL CalcH("diisopropyl ether",                         "108-20-3", 20, 0.26,      13)
    CALL CalcH("1,2,4-trichlorobenzene",                    "120-82-1", 25, 0.277,     11)
    CALL CalcH("styrene",                                   "100-42-5", 25, 0.286,     22)
    CALL CalcH("1,2-dichloropropane, (+-)-",                 "78-87-5", 25, 0.29,      13) ! other CAS: 26198-63-0
    CALL CalcH("dichloromethane",                            "75-09-2", 25, 0.30,      13)
    CALL CalcH("styrene",                                   "100-42-5", 50, 0.30,      13)
    CALL CalcH("1,2,3-trimethylbenzene",                    "526-73-8", 25, 0.343,     22)
    CALL CalcH("2,3-dichloropropene",                        "78-88-6", 25, 0.36,      5)
    CALL CalcH("m-dichlorobenzene",                         "541-73-1", 25, 0.376,     11)
    CALL CalcH("trichloromethane",                           "67-66-3", 25, 0.43,      13)
    CALL CalcH("trichloromethane",                           "67-66-3", 59, 0.43,      13)
    CALL CalcH("cis-1,2-dichloroethene",                    "156-59-2", 25, 0.46,      13)
    CALL CalcH("1,3,5-cycloheptatriene",                    "544-25-2", 25, 0.47,      13)
    CALL CalcH("dibutyl ether",                             "142-96-1", 0,  0.48,      13)
    CALL CalcH("dibutyl ether",                             "142-96-1", 20, 0.48,      13)
    CALL CalcH("4-ethyltoluene",                            "622-96-8", 25, 0.500,     13)
    CALL CalcH("iodoethane",                                 "75-03-6", 25, 0.52,      13)
    CALL CalcH("2-ethyltoluene",                            "611-14-3", 25, 0.529,     13)
    CALL CalcH("furan",                                     "110-00-9", 25, 0.54,      13)
    CALL CalcH("iodomethane",                                "74-88-4", 20, 0.54,      13)
    CALL CalcH("o-xylene",                                   "95-47-6", 25, 0.551,     22)
    CALL CalcH("1,2,4-trimethylbenzene",                     "95-63-6", 25, 0.569,     22)
    CALL CalcH("1,2,3,5-tetrachlorobenzene",                "634-90-2", 25, 0.59,      11)
    CALL CalcH("1,1-dichloroethane",                         "75-34-3", 25, 0.63,      13)
    CALL CalcH("bromomethane",                               "74-83-9", 20, 0.63,      13)
    CALL CalcH("toluene",                                   "108-88-3", 25, 0.660,     22)
    CALL CalcH("2-methyltetrahydrofuran",                    "96-47-9", 19, 0.67,      13)
    CALL CalcH("p-xylene",                                  "106-42-3", 25, 0.690,     22)
    CALL CalcH("fluorobenzene",                             "462-06-6", 19, 0.70,      11)
    CALL CalcH("fluorobenzene",                             "462-06-6", 80, 0.70,      11)
    CALL CalcH("m-xylene",                                  "108-38-3", 25, 0.730,     22)
    CALL CalcH("1,3,5-trimethylbenzene",                    "108-67-8", 25, 0.781,     22)
    CALL CalcH("1-isopropyl-4-methylbenzene",                "99-87-6", 25, 0.80,      5)
    CALL CalcH("2,2',4,4',6,6'-hexachlorobiphenyl",       "33979-03-2", 25, 0.818,     7)
    CALL CalcH("ethylbenzene",                              "100-41-4", 25, 0.843,     22)
    CALL CalcH("hexachloroethane",                           "67-72-1", 25, 0.85,      13)
    CALL CalcH("1-iodopropane",                             "107-08-4", 20, 0.93,      13)
    CALL CalcH("trans-1,2-dichloroethene",                  "156-60-5", 25, 0.96,      13)
    CALL CalcH("chloromethane",                              "74-87-3", 25, 0.98,      13)
    CALL CalcH("chloroethane",                               "75-00-3", 25, 1.02,      13)
    CALL CalcH("1,4-cyclohexadiene",                        "628-41-1", 25, 1.03,      13)
    CALL CalcH("trichloroethene",                            "79-01-6", 25, 1.03,      13)
    CALL CalcH("propylbenzene",                             "103-65-1", 25, 1.041,     22)
    CALL CalcH("1,3,5-trichlorobenzene",                    "108-70-3", 25, 1.1,       11)
    CALL CalcH("3-chloropropene",                           "107-05-1", 25, 1.10,      5)
    CALL CalcH("propyne",                                    "74-99-7", 25, 1.11,      5)
    CALL CalcH("1-bromobutane",                             "109-65-9", 25, 1.2,       13)
    CALL CalcH("bromoethane",                                "74-96-4", 25, 1.23,      13)
    CALL CalcH("2-bromopropane",                             "75-26-3", 20, 1.27,      13)
    CALL CalcH("tert-butylbenzene",                          "98-06-6", 25, 1.28,      11)
    CALL CalcH("butylbenzene",                              "104-51-8", 25, 1.33,      22)
    CALL CalcH("1-chloropropane",                           "540-54-5", 25, 1.41,      13)
    CALL CalcH("isopropylbenzene",                           "98-82-8", 25, 1.466,     22)
    CALL CalcH("1-chlorobutane",                            "109-69-3", 25, 1.54,      13)
    CALL CalcH("pentylbenzene",                             "538-68-1", 25, 1.69,      11)
    CALL CalcH("tetrachloroethene",                         "127-18-4", 20, 1.73,      13)
    CALL CalcH("1,1,1-trichloroethane",                      "71-55-6", 25, 1.76,      13)
    CALL CalcH("1-iodobutane",                              "542-69-8", 17, 1.87,      13)
    CALL CalcH("sec-butylbenzene, (+-)-",                   "135-98-8", 25, 1.89,      11) ! other CAS: 36383-15-0
    CALL CalcH("1-butyne",                                  "107-00-6", 25, 1.91,      5)
    CALL CalcH("1-chloropentane",                           "543-59-9", 25, 2.37,      13)
    CALL CalcH("1-pentyne",                                 "627-19-0", 25, 2.5,       5)
    CALL CalcH("1,2,4,5-tetramethylbenzene",                 "95-93-2", 25, 2.55,      11)
    CALL CalcH("1,1-dichloroethene",                         "75-35-4", 25, 2.62,      13)
    CALL CalcH("chloroethene",                               "75-01-4", 25, 2.68,      13)
    CALL CalcH("tetrachloromethane",                         "56-23-5", 25, 2.99,      13)
    CALL CalcH("tetrachloromethane",                         "56-23-5", 75, 2.99,      13)
    CALL CalcH("chlorodifluoromethane",                      "75-45-6", 25, 3.0,       13)
    CALL CalcH("trans-decahydronaphthalene",                "493-02-7", 25, 3.,        13) ! ref 13 says: 3.644
    CALL CalcH("isobutylbenzene",                           "538-93-2", 25, 3.32,      11)
    CALL CalcH("1-bromopropane",                            "106-94-5", 25, 3.8,       13)
    CALL CalcH("1-hexyne",                                  "693-02-7", 25, 4.14,      13)
    CALL CalcH("1-heptyne",                                 "628-71-7", 25, 4.47,      13)
    CALL CalcH("cyclohexene",                               "110-83-8", 25, 4.57,      13)
    CALL CalcH("cycloheptene",                              "628-92-2", 25, 4.9,       13)
    CALL CalcH("cyclopentene",                              "142-29-0", 25, 6.56,      13)
    CALL CalcH("chlorotrifluoromethane",                     "75-72-9", 25, 6.9,       13)
    CALL CalcH("2-methyl-1,3-butadiene",                     "78-79-5", 25, 7.78,      5)
    CALL CalcH("1-octyne",                                  "629-05-0", 25, 7.87,      13)
    CALL CalcH("cycloheptane",                              "291-64-5", 25, 9.59,      13)
    CALL CalcH("trichlorofluoromethane",                     "75-69-4", 20, 10.2,      13)
    CALL CalcH("cyclooctane",                               "292-64-8", 25, 10.7,      13)
    CALL CalcH("1,4-pentadiene",                            "591-93-5", 25, 12.,       5)
    CALL CalcH("aniline",                                    "62-53-3", 25, 14.,       15)
    CALL CalcH("cyclopentane",                              "287-92-3", 25, 19.1,      13)
    CALL CalcH("cyclohexane",                               "110-82-7", 25, 19.4,      13)
    CALL CalcH("1,3-butadiene",                             "106-99-0", 25, 20.7,      13)
    CALL CalcH("propene",                                   "115-07-1", 25, 21.3,      5)
    CALL CalcH("isobutene",                                 "115-11-7", 25, 21.6,      13)
    CALL CalcH("ethylene",                                   "74-85-1", 25, 21.7,      5)
    CALL CalcH("cis-2-pentene",                             "627-20-3", 25, 22.8,      5)
    CALL CalcH("1-butene",                                  "106-98-9", 25, 25.6,      13)
    CALL CalcH("2-methyl-1-pentene",                        "763-29-1", 25, 28.1,      5)
    CALL CalcH("1,1,2-trichloro-1,2,2-trifluoroethane",      "76-13-1", 25, 32.,       13)
    CALL CalcH("cis-1,2-dimethylcyclohexane",              "2207-01-4", 25, 36.,       5)
    CALL CalcH("methylcyclopentane",                         "96-37-7", 25, 36.7,      5)
    CALL CalcH("1-heptene",                                 "592-76-7", 25, 40.3,      13)
    CALL CalcH("1-pentene",                                 "109-67-1", 25, 40.3,      5)
    CALL CalcH("dichlorodifluoromethane",                    "75-71-8", 20, 41.,       13)
    CALL CalcH("1-hexene",                                  "592-41-6", 25, 41.8,      5)
    CALL CalcH("trans-2-heptene",                         "14686-13-6", 25, 42.2,      13)
    CALL CalcH("methylcyclohexane",                         "108-87-2", 26, 43.3,      13)
    CALL CalcH("ethane",                                     "74-84-0", 25, 50.6,      5)
    CALL CalcH("3-methyl-1-butene",                         "563-45-1", 25, 54.7,      5)
    CALL CalcH("4-methyl-1-pentene",                        "691-37-2", 25, 63.2,      5)
    CALL CalcH("methane",                                    "74-82-8", 25, 67.4,      5)
    CALL CalcH("propane",                                    "74-98-6", 25, 71.6,      5)
    CALL CalcH("trans-1,2-dimethylcyclohexane",            "6876-23-9", 30, 88.2,      5)
    CALL CalcH("propylcyclopentane",                       "2040-96-2", 25, 90.2,      5)
    CALL CalcH("butane",                                    "106-97-8", 25, 95.9,      5)
    CALL CalcH("1-octene",                                  "111-66-0", 25, 96.3,      13)
    CALL CalcH("1,1,3-trimethylcyclohexane",               "3073-66-3", 25, 105.,      13)
    CALL CalcH("isobutane",                                  "75-28-5", 25, 120.,      5)
    CALL CalcH("1,2-dichloro-1,1,2,2-tetrafluoroethane",     "76-14-2", 25, 127.,      13)
    CALL CalcH("pentane",                                   "109-66-0", 25, 128.,      13)
    CALL CalcH("2,3-dimethylbutane",                         "79-29-8", 25, 144.,      13)
    CALL CalcH("1,1,3-trimethylcyclopentane",              "4516-69-2", 25, 159.,      5)
    CALL CalcH("3-methylpentane",                            "96-14-0", 25, 170.,      13)
    CALL CalcH("2,3-dimethylpentane",                       "565-59-3", 25, 175.,      5)
    CALL CalcH("2-methylpentane",                           "107-83-5", 25, 176.,      13)
    CALL CalcH("hexane",                                    "110-54-3", 75, 183.,      13)
    CALL CalcH("pentylcyclopentane",                       "3741-00-2", 25, 185.,      5)
    CALL CalcH("3,3-dimethylpentane",                       "562-49-2", 25, 186.,      5)
    CALL CalcH("2,2-dimethylbutane",                         "75-83-2", 25, 199.,      13)
    CALL CalcH("2,3,4-trimethylpentane",                    "565-75-3", 25, 206.,      13)
    CALL CalcH("heptane",                                   "142-82-5", 50, 209.,      13)
    CALL CalcH("neopentane",                                "463-82-1", 25, 220.,      13)
    CALL CalcH("2,2,5-trimethylhexane",                    "3522-94-9", 25, 246.,      13)
    CALL CalcH("3-methylhexane",                            "589-34-4", 25, 249.,      13) ! other CAS: 78918-91-9 for (R)-enantiomer
    CALL CalcH("chloropentafluoroethane",                    "76-15-3", 25, 260.,      13)
    CALL CalcH("2,2,4-trimethylpentane",                    "540-84-1", 25, 307.,      13)
    CALL CalcH("octane",                                    "111-65-9", 25, 311.,      13)
    CALL CalcH("2,2-dimethylpentane",                       "590-35-2", 25, 318.,      5)
    CALL CalcH("2,4-dimethylpentane",                       "108-08-7", 25, 323.,      13)
    CALL CalcH("nonane",                                    "111-84-2", 25, 333.,      13)
    CALL CalcH("2-methylhexane",                            "591-76-4", 25, 346.,      5)
    CALL CalcH("3-methylheptane",                           "589-81-1", 25, 376.,      5)
    CALL CalcH("isopentane",                                 "78-78-4", 25, 479.,      13)
    CALL CalcH("decane",                                    "124-18-5", 0,  479.,      13)
    CALL CalcH("dodecane",                                  "112-40-3", 25, 750.,      5)
    CALL CalcH("4-methyloctane",                           "2216-34-4", 25, 1000.,     5)

    chem = "1-chloro-2-methylpropene" ; casrn = "513-37-1"
    type = "W"
    CALL MakeNote("CRCwrong1", &
      TRIM(citet())//" refer to \citet{479} but that "// &
      "article lists this value for 1-chloro-2-methylpropane "// &
      "(the saturated compound), not for 1-chloro-2-methylpropene.")
    CALL Output(DUMMY)

    chem = "trans-1,2-dimethylcyclohexane" ; casrn = "6876-23-9"
    type = "W"
    CALL MakeNote("CRCwrong2", &
      TRIM(citet())//" refer to \citet{479} but that "// &
      "article lists this value for 1,4-dimethylcyclohexane, "// &
      "not for 1,2-dimethylcyclohexane.")
    CALL Output(DUMMY)

    chem = "1-bromobutane" ; casrn = "109-65-9" ! C4H9Br
    type = "?"
    CALL MakeNote("CRCwrong3", &
      TRIM(citet())//" refer to \citet{635} "// &
      "as the source but this value cannot be found there.")
    CALL Output(KHpcSI_TIMES_HcpSI/(1.2*1.E3))

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, KHpc, refnumber)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      INTEGER,          INTENT(IN) :: temp
      REAL,             INTENT(IN) :: KHpc
      INTEGER,          INTENT(IN) :: refnumber
      LOGICAL :: l_output
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! only show output for debugging or if original paper is not yet included:
      l_output = .TRUE.
      SELECT CASE(refnumber)
      CASE ( 5)  ; CALL SettypeX("479")      ; l_output = .FALSE.
      CASE ( 7)  ; CALL SettypeX("2302")     ; l_output = .FALSE.
      CASE (11)  ; CALL SettypeX("633")      ; l_output = .FALSE.
      CASE (12)  ; CALL SettypeX("634")      ! l_output = .FALSE.
      CASE (13)  ; CALL SettypeX("635")      ; l_output = .FALSE.
      CASE (15)  ; CALL SettypeX("howard89") ! l_output = .FALSE.
      CASE (22)  ; CALL SettypeX("2338")     ; l_output = .FALSE.
      CASE (28)  ; CALL SettypeX("2339")     ; l_output = .FALSE.
      CASE (31)  ; CALL SettypeX("2340")     ; l_output = .FALSE.
      CASE DEFAULT
        print *, "refnumber error in ref3034"
      END SELECT
      IF (l_output) THEN
        SELECT CASE(temp)
        CASE(0)  ; CALL MakeNoteOtherTemp("273")
        CASE(17) ; CALL MakeNoteOtherTemp("290")
        CASE(19) ; CALL MakeNoteOtherTemp("292")
        CASE(20) ; CALL MakeNoteOtherTemp("293")
        CASE(26) ; CALL MakeNoteOtherTemp("299")
        CASE(30) ; CALL MakeNoteOtherTemp("303")
        CASE(38) ; CALL MakeNoteOtherTemp("311")
        CASE(40) ; CALL MakeNoteOtherTemp("313")
        CASE(50) ; CALL MakeNoteOtherTemp("323")
        CASE(59) ; CALL MakeNoteOtherTemp("332")
        CASE(75) ; CALL MakeNoteOtherTemp("348")
        CASE(80) ; CALL MakeNoteOtherTemp("353")
        CASE(81) ; CALL MakeNoteOtherTemp("354")
        CASE(82) ; CALL MakeNoteOtherTemp("355")
        CASE(90) ; CALL MakeNoteOtherTemp("363")
        CASE DEFAULT
          IF (temp/=25) print *, "temp error in ref3034"
        END SELECT
        Hominus = KHpcSI_TIMES_HcpSI/(KHpc*1.E3)
        CALL Output(Hominus)
      ELSE
        seenote = "" ! reset so that next CALL Output won't use it
        type    = "" ! reset to invalid value that will be overwritten
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref3034

  !---------------------------------------------------------------------------

  SUBROUTINE ref3064 ! special definition

    IMPLICIT NONE
    REAL :: factor

    ref = "3064"
    type = "M"
    ! here, "H" in [m3/mol] as given in ref3064
    ! H = Hcp * p * M(H2O) / (ca * rho(H2O))
    ! with ca = molar conc of air at 80 C
    !         = p/(RT) = 101325 / (8.314*353) = 34.5 mol/m3
    ! => Hcp = H * rho(H2O) / ( R * T *  M(H2O) )
    factor = rhoH2O / ( Rgas * (80.+CtoK) * MH2O )

    chem = "methanol" ; casrn = "67-56-1" ! CH3OH
    CALL MakeNoteOtherTemp("353")
    CALL Output(0.0019*factor)

    chem = "sulfur dioxide" ; casrn = "7446-09-5" ! SO2
    CALL MakeNoteOtherTemp("353")
    CALL Output(0.021*factor)

  END SUBROUTINE ref3064

  !---------------------------------------------------------------------------

  SUBROUTINE ref3065 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "3065"
    type = "L"

    ! Tab. 6:
    CALL CalcH("methanol",   "67-56-1", 35.2636, -31.9283, -13.6130, -0.0177)
    CALL CalcH("ethanol",    "64-17-5", 48.4419, -50.3880, -34.4862,  5.3681)
    CALL CalcH("1-propanol", "71-23-8", 59.5372, -67.7465, -56.3580, 11.8908)
    CALL CalcH("1-butanol",  "71-36-3", 69.1201, -82.5385, -74.7421, 17.2409)
    CALL CalcH("1-pentanol", "71-41-0", 78.7049, -99.5059, -97.8025, 24.8176)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: A, B, C, D
      REAL :: KH_kPa
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      ! The temperature dependence is:
      ! ln(KH) = A + B/tau + C*ln(tau) + D*tau with tau = T/T0 and T0 = 298.15 K
      ! At T=T0, this reduces to:
      KH_kPa = EXP(A + B + D)
      Hominus    = cH2O/(1E3*KH_kPa)
      ! The analytical derivative is:
      ! dln(KH)/d(1/T) = B*T0 - C*T - D*T^2/T0
      ! At T=T0, this reduces to:
      mindHR = -(B-C-D)*T0 ! the minus sign comes from KHpx->Hcp
      ! see also gnuplot/ref3065.gnu

      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3065

  !---------------------------------------------------------------------------

  SUBROUTINE ref3066 ! KHpx [mmHg]
    IMPLICIT NONE

    ref = "3066"
    type = "M"
    chem = "1,3-butadiene" ; casrn = "106-99-0" ! C4H6
    CALL Output(cH2O/(2.96E6*mmHg))

  END SUBROUTINE ref3066

  !---------------------------------------------------------------------------

  SUBROUTINE ref3067 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3067"
    type = "V"

    CALL CalcH("1,2-ethanediol",         "107-21-1", 6.6E5,  -72.9  )
    CALL CalcH("1,2-propanediol",         "57-55-6", 2.7E5,  -78.8  )
    CALL CalcH("1,3-propanediol",        "504-63-2", 1.6E6,  -79.1  )
    CALL CalcH("1,2-butanediol",         "584-03-2", 2.1E5,  -82.1  )
    CALL CalcH("1,3-butanediol",         "107-88-0", 7.1E5,  -84.5  )
    CALL CalcH("1,4-butanediol",         "110-63-4", 3.5E6,  -89.6  )
    CALL CalcH("2,3-butanediol",         "513-85-9", 1.1E5,  -82.2  )
    CALL CalcH("1,2-pentanediol",       "5343-92-0", 1.4E5          )
    CALL CalcH("1,4-pentanediol",        "626-95-9", 2.3E6          )
    CALL CalcH("1,5-pentanediol",        "111-29-5", 7.1E6,  -103.5 )
    CALL CalcH("2,4-pentanediol",        "625-69-4", 3.9E5          )
    CALL CalcH("1,2-hexanediol",        "6920-22-5", 1.7E5          )
    CALL CalcH("2,5-hexanediol",        "2935-44-6", 1.4E6          )
    CALL CalcH("1,7-heptanediol",        "629-30-1", &
      range=TRIM(H_range(4.6E6*Hcp_TO_HcpSI,8.4E6*Hcp_TO_HcpSI)))
    CALL CalcH("1,9-nonanediol",        "3937-56-2", &
      range=TRIM(H_range(2.4E6*Hcp_TO_HcpSI,4.0E6*Hcp_TO_HcpSI)))
    CALL CalcH("1,10-decanediol",        "112-47-0", &
      range=TRIM(H_range(2.5E6*Hcp_TO_HcpSI,3.0E6*Hcp_TO_HcpSI)))
    CALL CalcH("glycerol",                "56-81-5", 4.8E8,  -92.6  )
    CALL CalcH("erythritol",             "149-32-6", 1.1E12, -133.  )
    CALL CalcH("pentaerythritol",        "115-77-5", 7.4E12, -133.  )
    CALL CalcH("xylitol",                 "87-99-0", 4.0E13, -140.  )
    CALL CalcH("adonitol",               "488-81-3", 4.7E13, -147.  )
    CALL CalcH("arabinitol",            "2152-56-9", 6.8E13, -147.  )
    CALL CalcH("sorbitol",                "50-70-4", 6.7E16, -181.  )
    CALL CalcH("mannitol",                "69-65-8", 1.8E17, -184.  )
    CALL CalcH("dulcitol",               "608-66-2", 9.1E16, -181.  )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, DHg, range)
      IMPLICIT NONE
      CHARACTER(LEN=*),             INTENT(IN) :: chem_
      CHARACTER(LEN=*),             INTENT(IN) :: casrn_
      REAL,             OPTIONAL,   INTENT(IN) :: H
      REAL,             OPTIONAL,   INTENT(IN) :: DHg ! Delta Hg
      CHARACTER(LEN=*), OPTIONAL,   INTENT(IN) :: range
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (PRESENT(range)) THEN
        CALL MakeNote(TRIM(ref)//"-"//casrn_//"-range", TRIM(citet())// &
          " recommend $\Hsymbol$ for "//chem_//" in the range of "//range//".")
      ENDIF
      IF (PRESENT(H)) THEN
        Hominus = Hcp_TO_HcpSI * H
        IF (PRESENT(DHg)) THEN
          CALL Output(Hominus, -1E3*DHg/Rgas)
        ELSE
          CALL Output(Hominus)
        ENDIF
      ELSE
        CALL Output(DUMMY)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref3067

  !---------------------------------------------------------------------------

  SUBROUTINE ref3070 ! KHcc [1]
    IMPLICIT NONE

    ref = "3070"
    type = "C"

    ! Table 2:
    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    CALL MakeNoteOtherTemp("293")
    CALL Output(KHcc_TO_HcpSI(0.29,20.+CtoK))

    CALL CalcH("dimethylmercury",      "593-74-8", (/ 0., 25. /),       (/ 0.15, 0.31 /))             ! C2H6Hg
    CALL CalcH("chloromethylmercury",  "115-09-3", (/ 10., 15., 25. /), (/ 0.9E-5, 1.6E-5, 1.9E-5 /)) ! CH3HgCl
    CALL CalcH("mercury dihydroxide",   "_CAS-84", (/ 10., 25. /),      (/ 1.6E-6, 3.2E-6 /))         ! Hg(OH)2
    CALL CalcH("mercury dichloride",  "7487-94-7", (/ 10., 25. /),      (/ 1.2E-8, 2.9E-8 /))         ! HgCl2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp+CtoK, KHcc_TO_HcpSI(KHcc,temp+CtoK), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3070

  !---------------------------------------------------------------------------

  SUBROUTINE ref3071 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3071"
    type = "C" ! from US EPA 1992

    ! Table 1:
    CALL CalcH("hydroxybenzene",                "108-95-2", 1.30E-06     ) ! Phenol
    CALL CalcH("1-hydroxy-2,4-dimethylbenzene", "105-67-9", 1.70E-05     ) ! 2,4-Dimethylphenol
    CALL CalcH("naphthalene",                    "91-20-3", 4.83E-04     ) ! Naphthalene
    CALL CalcH("acenaphthene",                   "83-32-9", 2.41E-04     ) ! Acenaphthene
    CALL CalcH("acenaphthylene",                "208-96-8", 1.14E-04     ) ! Acenaphthylene
    CALL CalcH("anthracene",                    "120-12-7", 8.60E-05     ) ! Anthracene
    CALL CalcH("2,3-benzindene",                 "86-73-7", 1.17E-04     ) ! Fluorene
    CALL CalcH("phenanthrene",                   "85-01-8", 3.93E-05, 21.) ! Phenanthrene
    CALL CalcH("benz[$a$]anthracene",            "56-55-3", 1.16E-06, 24.) ! Benzo(a)anthracene
    CALL CalcH("chrysene",                      "218-01-9", 1.05E-06     ) ! Chrysene
    CALL CalcH("benzo[$jk$]fluorene",           "206-44-0", 6.50E-06     ) ! Fluoranthene
    CALL CalcH("pyrene",                        "129-00-0", 5.10E-06, 26.) ! Pyrene
    CALL CalcH("benzo[$a$]pyrene",               "50-32-8", 4.90E-07     ) ! Benzo(a)pyrene
    CALL CalcH("benzo[$b$]fluoranthene",        "205-99-2", 1.19E-05     ) ! Benzo(b)fluoranthene
    CALL CalcH("benzo[$k$]fluoranthene",        "207-08-9", 3.94E-05     ) ! Benzo(k)fluoranthene
    CALL CalcH("dibenz[$a,h$]anthracene",        "53-70-3", 7.30E-08     ) ! Dibenzo(a,h)anthracene
    CALL CalcH("benzo[$ghi$]perylene",          "191-24-2", 5.34E-08     ) ! Benzo(g,h,i)perylene
    CALL CalcH("indeno[1,2,3-$cd$]pyrene",      "193-39-5", 6.95E-08     ) ! Indeno(1,2,3-cd)pyrene
    CALL CalcH("benzene",                        "71-43-2", 5.55E-03, 20.) ! Benzene
    CALL CalcH("methylbenzene",                 "108-88-3", 5.92E-03, 20.) ! Toluene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, othertemp)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      REAL,   OPTIONAL, INTENT(IN) :: othertemp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*H)
      IF (PRESENT(othertemp)) THEN
        CALL MakeNoteOtherTemp(TRIM(str(NINT(othertemp+CtoK))))
      ENDIF
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3071

  !---------------------------------------------------------------------------

  ! ref3072 no data (data are in ref1500)

  !---------------------------------------------------------------------------

  SUBROUTINE ref3073 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3073"
    type = "C"

    ! Table 1:
    CALL CalcH("triclopyr",                           "55335-06-3", 8.2E-10 )
    CALL CalcH("dicamba",                              "1918-00-9", 4.4E-10 )
    CALL CalcH("bensulfuron methyl",                  "83055-99-6", 1.4E-16 )
    CALL CalcH("sulfometuron methyl",                 "74222-97-2", 1.2E-18 )
    CALL CalcH("chlorsulfuron",                       "64902-72-3", 6.7E-11 )
    CALL CalcH("(2,4-dichlorophenoxy)-ethanoic acid",    "94-75-7", 1.8E-12 )
    CALL CalcH("mecoprop",                             "7085-19-0", 1.1E-9  )
    CALL CalcH("molinate",                             "2212-67-1", 1.3E-6  )
    CALL CalcH("thiobencarb",                         "28249-77-6", 2.7E-7  )
    CALL CalcH("imidacloprid",                       "138261-41-3", 2.0E-15 )
    CALL CalcH("oxadiazon",                           "19666-30-9", 7.1E-8  )
    CALL CalcH("chlorpyrifos",                         "2921-88-2", 7.3E-6  )
    CALL CalcH("chlorothalonil",                       "1897-45-6", 2.2E-7  )
    CALL CalcH("carbaryl",                               "63-25-2", 2.8E-9  )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*H)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3073

  !---------------------------------------------------------------------------

  ! ref3074 data are only in Fig. 1 but no values are listed

  !---------------------------------------------------------------------------

  SUBROUTINE ref3075 ! KHcc [1]
    IMPLICIT NONE

    ref = "3075"
    type = "M"

    ! Table I:
    CALL CalcH("propanal", "123-38-6", 54E-4) ! C2H5CHO propionaldehyde
    CALL CalcH("butanal",  "123-72-8", 63E-4) ! C3H7CHO butyraldehyde
    CALL CalcH("pentanal", "110-62-3", 69E-4) ! C4H{9}CHO valeraldehyde
    CALL CalcH("hexanal",   "66-25-1", 69E-4) ! C5H{11}CHO
    CALL CalcH("heptanal", "111-71-7", 67E-4) ! C6H{13}CHO
    CALL CalcH("octanal",  "124-13-0", 54E-4) ! C7H{15}CHO
    CALL CalcH("nonanal",  "124-19-6", 57E-4) ! C8H{17}CHO
    CALL CalcH("decanal",  "112-31-2", 24E-4) ! C9H{19}CHO

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0/KHcc)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3075

  !---------------------------------------------------------------------------

  SUBROUTINE ref3076 ! KHcc [1]
    IMPLICIT NONE

    ref = "3076"
    type = "?"

    ! Table 1:
    CALL CalcH("alachlor",    "15972-60-8", 1.3E-6     ) ! C{14}H{20}ClNO2
    CALL CalcH("metolachlor", "51218-45-2", 3.7E-7, 20.) ! C{15}H{22}ClNO2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc, othertemp)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      REAL,   OPTIONAL, INTENT(IN) :: othertemp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (PRESENT(othertemp)) THEN
        CALL MakeNoteOtherTemp(TRIM(str(NINT(othertemp+CtoK))))
        CALL Output(KHcc_TO_HcpSI(KHcc,othertemp+CtoK))
      ELSE
        CALL Output(KHcc_TIMES_HcpSI_atT0/KHcc)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref3076

  !---------------------------------------------------------------------------

  SUBROUTINE ref3077 ! KHcc [1]
    IMPLICIT NONE

    ref = "3077"
    type = "M"

    ! Table 1:
    CALL CalcH("tetraethyllead", "78-00-2", &
      (/ 0.0,        24.7,       26.6,       31.5,       31.5,       37.5 /), &
      (/ 0.39/0.081, 3.60/0.155, 4.10/0.116, 4.60/0.095, 6.40/0.102, 5.01/0.078 /) ) ! C8H{20}Pb

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp+CtoK, KHcc_TO_HcpSI(KHcc,temp+CtoK), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3077

  !---------------------------------------------------------------------------

  SUBROUTINE ref3078 ! KHcc [1]
    IMPLICIT NONE

    ref = "3078"
    type = "V"

    chem = "sulfur hexafluoride" ; casrn = "2551-62-4" ! SF6
    CALL Output(KHcc_TIMES_HcpSI_atT0/185.)

  END SUBROUTINE ref3078

  !---------------------------------------------------------------------------

  SUBROUTINE ref3079 ! Hcc [1]
    IMPLICIT NONE

    ref = "3079"
    type = "M"

    ! Table 2:
    CALL CalcH("1-methoxy-2-propanol",    "107-98-2", 12280.) ! C4H{10}O2
    CALL CalcH("2-methoxyethanol",        "109-86-4", 35869.) ! C3H8O2 methyl cellosolve
    CALL CalcH("2-ethoxyethanol",         "110-80-5", 23069.) ! C4H{10}O2
    CALL CalcH("2-isopropoxyethanol",     "109-59-1", 12349.) ! C5H{12}O2
    CALL CalcH("2-ethoxyethyl ethanoate", "111-15-9",  3822.) ! C6H{12}O3
    CALL CalcH("3-oxa-1-heptanol",        "111-76-2",  7051.) ! C6H{14}O2 2-butoxyethanol; butyl cellosolve

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Hcc
      REAL :: temp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      temp = 37.+CtoK
      CALL MakeNoteOtherTemp(TRIM(str(NINT(temp))))
      CALL Output(Hcc_TO_HcpSI(Hcc,temp))
    END SUBROUTINE CalcH

  END SUBROUTINE ref3079

  !---------------------------------------------------------------------------

  SUBROUTINE ref3080 ! special definition (aq. solution in % and p in mmHg)
    IMPLICIT NONE

    ref = "3080"
    type = "M"

    ! Table III:
    CALL CalcH("N-nitrosodimethylamine",        "62-75-9", 1E4*0.67/74.0818, &
      (/ 0., 10., 20., 30., 40. /), &
      (/ 18., 29., 74., 163., 327. /), &
      (/ 79., 120., 300., 640., 1240. /) )
    CALL CalcH("N-ethyl-N-nitroso-ethanamine",  "55-18-5", 1E4*0.78/102.1350, &
      (/ 0., 10., 20., 30., 40. /), &
      (/ 16., 31., 66., 140., 310. /), &
      (/ 95., 180., 370., 760., 1600. /) )
    CALL CalcH("N-nitrosopyrrolidine",         "930-55-2", 1E4*0.54/100.1191, &
      (/ 20., 30., 40. /), &
      (/ 1.5, 5.1, 9.6 /), &
      (/ 4.2, 12., 24. /) )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, caq, temp, part_press, mgm3)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: caq ! aq conc in [mol/m3] = 1E4 * aq% / M
      REAL, DIMENSION(:), INTENT(IN) :: temp ! degree C
      REAL, DIMENSION(:), INTENT(IN) :: part_press ! 1E-3 mmHg
      REAL, DIMENSION(:), INTENT(IN) :: mgm3 ! mg/m3
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp+CtoK, caq / (1E-3*part_press*mmHg), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      ! The calculation could also be done using the gas-phase conc in mg/m3. The results
      ! should be the same (not tested here).

!!$(1/760) 0.074 / (6700. / 74.0818) => 1.07660117831e-6
!!$print *, (part_press/760.)/caq
!!$print *, caq/(part_press/760.)/101325.

    END SUBROUTINE CalcH

  END SUBROUTINE ref3080

  !---------------------------------------------------------------------------

  SUBROUTINE ref3081 ! KHcc [1]
    IMPLICIT NONE

    ref = "3081"
    !                                                       V      temp  VEL   HS
    CALL CalcH("benzene",                        "71-43-2", 0.22,  301., 0.19, 0.240)
    CALL CalcH("octamethylcyclotetrasiloxane",  "556-67-2", 259.,  301., 24.,  23.  ) ! D4
    CALL CalcH("decamethylcyclopentasiloxane",  "541-02-6", 185.,  299., 12.,  13.  ) ! D5
    CALL CalcH("dodecamethylcyclohexasiloxane", "540-97-6", 104.,  299., 5.9,  2.7  ) ! D6
    CALL CalcH("hexamethyldisiloxane",          "107-46-0", 397.,  300., 2.4,  1.3  ) ! L2
    CALL CalcH("octamethyltrisiloxane",         "107-51-7", 1465., 300., 121., 147. ) ! L3
    CALL CalcH("decamethyltetrasiloxane",       "141-62-8", 943.,  300., 697.       ) ! L4

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, V, temp, VEL, HS)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: V, temp, VEL
      REAL, OPTIONAL,   INTENT(IN) :: HS
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      ! Table 1:
      type = "V"
      CALL Output(KHcc_TIMES_HcpSI_atT0/V)
      ! VEL from Table 2:
      type = "M"
      CALL MakeNoteOtherTemp(TRIM(str(NINT(temp))))
      CALL MakeNote(TRIM(ref)//"VEL", &
        "Value obtained by applying a modified batch air-stripping method, " &
        //"otherwise called the vapor entry loop (VEL) method, see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHcc_TO_HcpSI(VEL,temp))
      ! HS from Table 2:
      IF (PRESENT(HS)) THEN
        type = "M"
        seenote = ""
        CALL MakeNoteOtherTemp(TRIM(str(NINT(temp))))
        CALL MakeNote(TRIM(ref)//"HS", &
          "Value obtained by applying the static head space (HS) method, see " &
          //TRIM(citet())//" for details.")
        CALL Output(KHcc_TO_HcpSI(HS,temp))
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref3081

  !---------------------------------------------------------------------------

  SUBROUTINE ref3082 ! KHcc [1]
    IMPLICIT NONE

    ref = "3082"
    type = "M"

    ! Table 1:                                                  -1 = no data
    CALL CalcH("dimethylnitrosamine",                "62-75-9", 7.8,  7.1              ) ! DMN
    CALL CalcH("diethylnitrosamine",                 "55-18-5", 24.,  30.              ) ! DEN
    CALL CalcH("dipropylnitrosamine",               "621-64-7", 19.,  25.              ) ! DPN
    CALL CalcH("dibutylnitrosamine",                "924-16-3", 64.,  44.              ) ! DBN
    CALL CalcH("diisopropylnitrosamine",            "601-77-4", -1.,  32.              ) ! DIN
    CALL CalcH("methylbutylnitrosamine",           "7068-83-9", 17.,  30.              ) ! MBN
    CALL CalcH("methylpentylnitrosamine",         "13256-07-0", -1.,  19.              ) ! MAN
    CALL CalcH("ethylbutylnitrosamine",            "4549-44-4", -1.,  39.              ) ! EBN
    CALL CalcH("methylbenzylnitrosamine",           "937-40-6", -1.,  49.              ) ! MBZ
    CALL CalcH("nitrosoazetidine",                "15216-10-1", 0.2,  -1., upperlimit=1) ! NAZ
    CALL CalcH("nitrosopyrrolidine",                "930-55-2", -1.,  0.2              ) ! NPY
    CALL CalcH("nitrosopiperidine",                 "100-75-4", 2.5,  4.4              ) ! NPP
    CALL CalcH("nitrosohexamethyleneimine",         "932-83-2", -1.,  0.9              ) ! NHI
    CALL CalcH("nitrosomorpholine",                  "59-89-2", -1.,  0.1              ) ! NM
    CALL CalcH("2,6-dimethylnitrosomorpholine",    "1456-28-6", -1.,  1.1              ) ! DMNM
    CALL CalcH("dinitrosopiperazine",               "140-79-4", 0.2,  -1., upperlimit=1) ! DNP
    CALL CalcH("2,6-dimethyldinitrosopiperazine", "55380-34-2", 0.2,  -1., upperlimit=1) ! DMDNP
    CALL CalcH("methylnitrosoacetamide",           "7417-67-6", 450., -1.              ) ! MNA
    CALL CalcH("methylnitrosourea",                 "684-93-5", 0.2,  -1., upperlimit=1) ! MNU
    CALL CalcH("methylnitrosourethan",              "615-53-2", 100., -1.              ) ! MNUT
    CALL CalcH("ethylnitrosocyanamide",           "38434-77-4", 150., -1.              ) ! ENC

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, K1_m1, K1_m2, upperlimit)
      IMPLICIT NONE
      CHARACTER(LEN=*),  INTENT(IN) :: chem_
      CHARACTER(LEN=*),  INTENT(IN) :: casrn_
      REAL,              INTENT(IN) :: K1_m1, K1_m2 ! K1 method 1, method 2
      INTEGER, OPTIONAL, INTENT(IN) :: upperlimit
      REAL :: K1, Hominus
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      K1 = (K1_m1 + K1_m2) / 2. ! average of both methods
      IF (K1_m2<0.) K1 = K1_m1 ! only method 1 available
      IF (K1_m1<0.) K1 = K1_m2 ! only method 2 available
      Hominus = KHcc_TO_HcpSI(1E-5*K1,37.+CtoK)
      CALL MakeNoteOtherTemp(TRIM(str(NINT(37.+CtoK))))
      IF (PRESENT(upperlimit)) THEN
        ! upper limit of KHcc is lower limit of Hcp:
        CALL Output(Hominus, limit=">")
      ELSE
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref3082

  !---------------------------------------------------------------------------

  SUBROUTINE ref3083 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3083"
    type = "C"

    ! Table 1:
    CALL CalcH("acenaphthene",                 "83-32-9", 0.241  ) ! C{12}H{10}
    CALL CalcH("benzene",                      "71-43-2", 5.55   ) ! C6H6
    CALL CalcH("tetrachloromethane",           "56-23-5", 30.2   ) ! CCl4 carbontetrachloride
    CALL CalcH("chlorobenzene",               "108-90-7", 3.93   ) ! C6H5Cl
    CALL CalcH("1,2,4-trichlorobenzene",      "120-82-1", 1.42   ) ! C6H3Cl3
    CALL CalcH("hexachlorobenzene",           "118-74-1", 1.70   ) ! C6Cl6
    CALL CalcH("1,2-dichloroethane",          "107-06-2", 1.10   ) ! CH2ClCH2Cl
    CALL CalcH("1,1,1-trichloroethane",        "71-55-6", 4.92   ) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("hexachloroethane",             "67-72-1", 9.85   ) ! C2Cl6
    CALL CalcH("1,1-dichloroethane",           "75-34-3", 5.45   ) ! CHCl2CH3
    CALL CalcH("trichloromethane",             "67-66-3", 3.39   ) ! CHCl3 chloroform
    CALL CalcH("1,2-dichlorobenzene",          "95-50-1", 1.94   ) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",         "541-73-1", 2.63   ) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",         "106-46-7", 2.72   ) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,1-dichloroethene",           "75-35-4", 15.0   ) ! CH2CCl2
    CALL CalcH("($E$)-1,2-dichloroethene",    "156-60-5", 5.32   ) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("1,2-dichloropropane",          "78-87-5", 2.82   ) ! C3H6Cl2
    CALL CalcH("1,3-dichloropropene",         "542-75-6", 3.55   ) ! C3H4Cl2
    CALL CalcH("ethylbenzene",                "100-41-4", 6.44   ) ! C6H5C2H5
    CALL CalcH("dichloromethane",              "75-09-2", 3.19   ) ! CH2Cl2 methylene chloride
    CALL CalcH("tribromomethane",              "75-25-2", 0.532  ) ! CHBr3 bromoform
    CALL CalcH("bromodichloromethane",         "75-27-4", 2.12   ) ! CHCl2Br
    CALL CalcH("trichlorofluoromethane",       "75-69-4", 58.3   ) ! CFCl3 R11
    CALL CalcH("dibromochloromethane",        "124-48-1", 0.783  ) ! CHClBr2
    CALL CalcH("hexachlorobutadiene",          "87-68-3", 10.3   ) ! CCl2CClCClCCl2
    CALL CalcH("hexachlorocyclopentadiene",    "77-47-4", 16.4   ) ! C5Cl6
    CALL CalcH("nitrobenzene",                 "98-95-3", 0.024  ) ! C6H5NO2
    CALL CalcH("2-methyl-4,6-dinitrophenol",  "534-52-1", 0.0014 ) ! 4,6-dinitro-$o$-cresol
    CALL CalcH("hydroxybenzene",              "108-95-2", 0.0013 ) ! C6H5OH phenol
    CALL CalcH("acenaphthylene",              "208-96-8", 0.114  ) ! C{12}H8
    CALL CalcH("2,3-benzindene",               "86-73-7", 0.117  ) ! C{13}H{10} fluorene
    CALL CalcH("tetrachloroethene",           "127-18-4", 28.7   ) ! C2Cl4 tetrachloroethylene
    CALL CalcH("methylbenzene",               "108-88-3", 5.93   ) ! C6H5CH3 toluene
    CALL CalcH("trichloroethene",              "79-01-6", 11.7   ) ! C2HCl3 trichloroethylene
    CALL CalcH("aldrin",                      "309-00-2", 0.496  ) ! C{12}H8Cl6
    CALL CalcH("dieldrin",                     "60-57-1", 0.058  ) ! C{12}H8OCl6
    CALL CalcH("chlordane",                    "57-74-9", 0.048  ) ! C{10}H6Cl8
    CALL CalcH("heptachlor",                   "76-44-8", 1.48   ) ! C{10}H5Cl7
    CALL CalcH("heptachlorepoxide",          "1024-57-3", 0.032  ) ! C{10}H5Cl7O
    ! arochlor 1254 is a mixture (not used)               8.37
    ! toxaphene     is a mixture (not used)               4.89

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1E3 / (atm*H)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3083

  !---------------------------------------------------------------------------

  SUBROUTINE ref3084 ! KHcc [1]
    IMPLICIT NONE

    ref = "3084"
    type = "M"

    ! Table 1:
    CALL CalcH("tripropylphosphate", "513-08-6", 2.8E-5) ! C6H{15}O4P
    CALL CalcH("triethylphosphate",   "78-40-0", 1.5E-6) ! C6H{15}O4P
    CALL CalcH("trimethylphosphate", "512-56-1", 3.0E-7) ! C3H9O4P

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp(TRIM(str(NINT(20.+CtoK))))
      CALL Output(KHcc_TO_HcpSI(KHcc,20.+CtoK))
    END SUBROUTINE CalcH

  END SUBROUTINE ref3084

  !---------------------------------------------------------------------------

  SUBROUTINE ref3085 ! KHpb [atm*kg/mol]
    IMPLICIT NONE

    ref = "3085"
    type = "L"

    ! Tab. 4:
    CALL CalcH("ammonia",          "7664-41-7",  5.6024, -0.020262, 2.5647, -5441.6) ! NH3
    CALL CalcH("carbon dioxide",    "124-38-9", 13.7750, -0.024950, 1.8090, -3955.5) ! CO2
    CALL CalcH("hydrogen sulfide", "7783-06-4",  8.3325, -0.019254, 1.7928, -3137.4) ! H2S
    CALL CalcH("sulfur dioxide",   "7446-09-5",  7.2068, -0.018880, 2.1423, -4158.5) ! SO2
    CALL CalcH("hydrogen cyanide",   "74-90-8",  9.5850, -0.03147,  3.1704, -6302.0) ! HCN

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: A, B, C, D
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! ln(KHpb) = A + B*T + C*ln(T) + D/T
      Hominus = EXP(A + B*T0 + C*LOG(T0) + D/T0)
      Hominus = (1./Hominus)*Hbp_TO_HcpSI
      ! analytical derivative: (see also gnuplot/ref3085.gnu)
      mindHR = B*T0*T0 + C*T0 - D ! d(lnH)/d(1/T) = -delta H/R
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3085

  !---------------------------------------------------------------------------

  SUBROUTINE ref3086 ! Hcc [1]
    IMPLICIT NONE

    ref = "3086"
    type = "M"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 2., 11., 20. /) + CtoK
    ! distribution ratio from fig. 1, digitized with "engauge":
    CALL CalcH("trans-1,3-dichloropropene", "10061-02-6", (/ 92.81, 47.55, 27.68 /)) ! C3H4Cl2
    CALL CalcH("cis-1,3-dichloropropene",   "10061-01-5", (/ 58.89, 30.07, 17.27 /)) ! C3H4Cl2
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcc)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: Hcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = Hcc_TO_HcpSI(Hcc,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3086

  !---------------------------------------------------------------------------

  SUBROUTINE ref3087 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "3087"
    type = "?"

    CALL CalcH("aldoxycarb",                    "1646-88-4", 2.67E-4                     )
    CALL CalcH("anilazine",                      "101-05-3", 2.82E-5                     )
    CALL CalcH("atraton",                       "1610-17-9", 4.46E-4                     )
    CALL CalcH("aziprotryne",                   "4658-28-0", 1.09E-3                     )
    CALL CalcH("barban",                         "101-27-9", 1.17E-3                     )
    CALL CalcH("chlorthion",                     "500-28-7", 4.09E-3                     )
    CALL CalcH("benodanil",                    "15310-01-7", 1.6E-4,  20., KHpc_limit=1  )
    CALL CalcH("bromofenoxim",                 "13181-17-4", 7.68E-6, 20.                )
    CALL CalcH("bromophos",                     "2104-96-3", 8.89,    20.                )
    CALL CalcH("bromuron",                      "3408-97-7", 5E-5                        )
    CALL CalcH("buminafos",                    "51249-05-9", 2E-1,    20.                )
    CALL CalcH("butenachlor",                  "87310-56-3", 1E-2                        )
    CALL CalcH("buthidazole",                  "55511-98-3", 2.09E-7                     )
    CALL CalcH("butocarboxim",                 "34681-10-2", 5.76E-5                     )
    CALL CalcH("butroxydim",                  "138164-12-2", 5.79E-5                     )
    CALL CalcH("buturon",                       "3766-60-7", 7.89E-5                     )
    CALL CalcH("carbon tetrachloride",            "56-23-5", 8.24E3                      )
    CALL CalcH("CGA 80000",                    "67932-85-8", 2.29E-7                     )
    CALL CalcH("chinomethionat",                "2439-01-2", 6.09E-3                     )
    CALL CalcH("chlobenthiazone",              "63755-05-5", 7.47E-1                     )
    CALL CalcH("chloraniformethan",            "20856-57-9", 2E-3,    20., KHpc_limit=1  )
    CALL CalcH("chlorbromuron",                "13360-45-7", 4E-4                        )
    CALL CalcH("chlorbufam",                    "1967-16-4", 8.70E-4                     )
    CALL CalcH("chlordimeform",                 "6164-98-3", 3.78E-2, 20.                )
    CALL CalcH("chlorobenzilate",                "510-15-6", 3.90E-3                     )
    CALL CalcH("chloromethiuron",              "28217-97-2", 5.03E-6                     )
    CALL CalcH("chloropropylate",               "5836-10-2", 9.95E-3                     )
    CALL CalcH("chloroxuron",                   "1982-47-4", 1.88E-5                     )
    CALL CalcH("chlorphonium chloride",          "115-78-6", 3.52E-8, 20.                )
    CALL CalcH("chlorphoxim",                  "14816-20-7", 1.96E-1,      KHpc_limit=1  )
    CALL CalcH("chlorthiamid",                  "1918-13-4", 2.82E-5                     )
    CALL CalcH("chlozolinate",                 "84332-86-5", 2.29E-3                     )
    CALL CalcH("cloethocarb",                  "51487-69-5", 2.00E-6                     )
    CALL CalcH("clofencet",                   "129025-54-3", 5.7E-9,       KHpc_limit=1  )
    CALL CalcH("crotoxyphos",                   "7700-17-6", 5.97E-4                     )
    CALL CalcH("cyometrinil",                  "78370-21-5", 9.07E-5                     )
    CALL CalcH("defenuron",                     "1007-36-9", 1.18E-6, 20.                )
    CALL CalcH("demeton-O",                      "298-03-3", 1.64E-1, 20.                )
    CALL CalcH("demeton-S-methylsulphon",      "17040-19-6", 7.34E-8,      KHpc_limit=-1 )
    CALL CalcH("desmetryn",                     "1014-69-3", 4.8E-5                      )
    CALL CalcH("di-allate",                     "2303-16-4", 3.86E-1                     )
    CALL CalcH("1,2-dibromo-3-chloropropane",     "96-12-8", 2.53E1                      )
    CALL CalcH("1,2-dichloropropane",             "78-87-5", 1.17E3                      )
    CALL CalcH("diclobutrazol",                "75736-33-3", 1.25E-4                     )
    CALL CalcH("dieldrin",                        "60-57-1", 8.19E-1                     )
    CALL CalcH("difenoxuron",                  "14214-32-5", 1.78E-8                     )
    CALL CalcH("dimethipin",                   "55290-64-7", 2.33E-6                     )
    CALL CalcH("dimethirimol",                  "5221-53-4", 2.55E-4,      KHpc_limit=1  )
    CALL CalcH("dinoseb",                         "88-85-7", 6.0E-4,  20.                )
    CALL CalcH("dinoterb",                      "1420-07-1", 1.07                        )
    CALL CalcH("diofenolan",                   "63837-33-2", 6.74E-3                     )
    CALL CalcH("dioxabenzofos",                 "3811-49-2", 2.11                        )
    CALL CalcH("dioxacarb",                     "6988-21-2", 1.49E-6                     )
    CALL CalcH("dipropetryn",                   "4147-51-7", 1.55E-3                     )
    CALL CalcH("etacelasil",                   "37894-46-5", 3.42E-4                     )
    CALL CalcH("etaconazole",                  "60207-93-4", 1.27E-4                     )
    CALL CalcH("ethidimuron",                  "30043-49-3", 7.05E-9                     )
    CALL CalcH("ethylene dichloride",            "107-06-2", 2.39E2                      )
    CALL CalcH("fenchlorazole-ethyl",         "103112-35-2", 3.71E-4, 20.                )
    CALL CalcH("fenpiclonil",                  "74738-17-3", 5.4E-4                      )
    CALL CalcH("flamprop-methyl",              "52756-25-9", 4.51E-4                     )
    CALL CalcH("fluacrypyrim",                "229977-93-9", 3.33E-3, 20.                )
    CALL CalcH("fluazolate",                  "174514-07-9", 7.89E-2                     )
    CALL CalcH("fluazuron",                    "86811-58-7", 3.04E-6,      KHpc_limit=1  )
    CALL CalcH("flubenzimine",                 "37893-02-0", 2.60E-1,      KHpc_limit=1  )
    CALL CalcH("flucycloxuron",                "94050-52-9", 2.6E-2                      )
    CALL CalcH("fluorodifen",                  "15457-05-3", 1.53E-3                     )
    CALL CalcH("fluothiuron",                  "33439-45-1", 7.34E-5,      KHpc_limit=1  )
    CALL CalcH("flurazole",                    "72850-64-7", 2.51E-2                     )
    CALL CalcH("fosthietan",                   "21548-32-3", 4.15E-6                     )
    CALL CalcH("furalaxyl",                    "57646-30-7", 9.3E-5                      )
    CALL CalcH("furmecyclox",                  "60568-05-0", 7.04E-3                     )
    CALL CalcH("glyphosate-trimesium",         "81591-81-3", 2E-9,         KHpc_limit=1  )
    CALL CalcH("halacrinate",                  "34462-96-9", 4.17E-3                     )
    CALL CalcH("heptachlor",                      "76-44-8", 3.53E2                      )
    CALL CalcH("heptenophos",                  "23560-59-0", 2.33E-4                     )
    CALL CalcH("isazofos",                     "42509-80-8", 1.39E-2                     )
    CALL CalcH("isofenphos",                   "25311-71-1", 4.2E-3,  20.                )
    CALL CalcH("isomethiozin",                 "57052-04-7", 1.26E-3                     )
    CALL CalcH("jodfenphos",                   "18181-70-9", 2.19E-2,      KHpc_limit=1  )
    CALL CalcH("metamifop",                   "256412-89-2", 6.35E-2, 20.                )
    CALL CalcH("methacrifos",                  "62610-77-9", 9.61E-2                     )
    CALL CalcH("methazole",                    "20354-26-1", 2.32E-2                     )
    CALL CalcH("methoprotryne",                  "841-06-5", 3.22E-5                     )
    CALL CalcH("monalide",                      "7287-36-7", 2.52E-3                     )
    CALL CalcH("monuron",                        "150-68-5", 5.79E-5                     )
    CALL CalcH("myclozolin",                   "54864-61-8", 2.68E-3                     )
    CALL CalcH("naphthalene",                     "91-20-3", 2.78E1                      )
    CALL CalcH("naptalam",                       "132-66-1", 1.94E2,       KHpc_limit=1  )
    CALL CalcH("nuarimol",                     "63284-71-9", 6.73E-8                     )
    CALL CalcH("pebulate",                      "1114-71-2", 20.,     20., KHpc_limit=1  )
    CALL CalcH("phenisopham",                  "57375-63-0", 7.59E-5                     )
    CALL CalcH("phosdiphen",                   "36519-00-3", 3.92E1                      )
    CALL CalcH("profluralin",                  "26399-36-0", 2.92E1                      )
    CALL CalcH("promecarb",                     "2631-37-0", 3.19E-3                     )
    CALL CalcH("propaphos",                     "7292-16-2", 2.92E-4                     )
    CALL CalcH("prothiocarb hydrochloride",    "19622-19-6", 4E-10                       )
    CALL CalcH("proximpham",                    "2828-42-4", 2.58E-4, 20.                )
    CALL CalcH("secbumeton",                   "26259-45-0", 3.64E-4                     )
    CALL CalcH("ethiozin",                     "64529-56-2", 5.04E-3                     )
    CALL CalcH("sulprofos",                    "35400-43-2", 8.74E-2, 20.                )
    CALL CalcH("2,4,5-T",                         "93-76-5", 1.19E-6                     )
    CALL CalcH("tebutam",                      "35256-85-0", 1.5E-2                      )
    CALL CalcH("tetrachlorvinphos",            "22248-79-9", 1.86E-4                     )
    CALL CalcH("tetrasul",                      "2227-13-6", 1.08                        )
    CALL CalcH("thiazafluron",                 "25366-23-8", 3.09E-5                     )
    CALL CalcH("thicyofen",                   "116170-30-0", 1.02E-3,      KHpc_limit=1  )
    CALL CalcH("thidiazimin",                 "123249-43-4", 2.82E-9                     )
    CALL CalcH("thionazin",                      "297-97-2", 8.71E-2                     )
    CALL CalcH("plifenat",                     "21757-82-4", 9.42E-5                     )
    CALL CalcH("trichloronat",                   "327-98-0", 1.33E-2                     )
    CALL CalcH("tridiphane",                   "58138-08-2", 5.16                        )
    CALL CalcH("trifenmorph",                   "1420-06-0", 3.13E-1                     )
    CALL CalcH("XMC",                           "2655-14-3", 2.33E-3                     )
    CALL CalcH("zarilamid",                    "84527-51-5", 6.72E-6                     )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Henry, othertemp, KHpc_limit)
      IMPLICIT NONE
      CHARACTER(LEN=*),  INTENT(IN) :: chem_
      CHARACTER(LEN=*),  INTENT(IN) :: casrn_
      REAL,              INTENT(IN) :: Henry
      REAL,    OPTIONAL, INTENT(IN) :: othertemp
      INTEGER, OPTIONAL, INTENT(IN) :: KHpc_limit
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      IF (PRESENT(KHpc_limit)) THEN
        IF (KHpc_limit>0) THEN
          ! upper limit of KHpc is lower limit of Hcp:
          CALL Output(Hominus, limit=">")
        ELSE
          ! lower limit of KHpc is upper limit of Hcp:
          CALL Output(Hominus, limit="<")
        ENDIF
      ELSE
        IF (PRESENT(othertemp)) THEN
          CALL MakeNoteOtherTemp(TRIM(str(NINT(othertemp+CtoK))))
        ENDIF
        CALL Output(KHpcSI_TIMES_HcpSI/Henry)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref3087

  !---------------------------------------------------------------------------

  SUBROUTINE ref3088 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3088"

    ! type = "C"
    CALL CalcH("acenaphthene",         "83-32-9", 1.55E-4, "C") ! D. Mackay, W.Y. Shiu, A. Bobra, J. Billington, E. Chau, A. Yeun, C. Ng, F. Szeto, Volatilization of Organic Pollutants from Water, USEPA-600/53-82-019, NTIS PB 82-230 939 (1982)
    CALL CalcH("bromobenzene",        "108-86-1", 2.08E-3, "C") ! D. Mackay, W.Y. Shiu, A. Bobra, J. Billington, E. Chau, A. Yeun, C. Ng, F. Szeto, Volatilization of Organic Pollutants from Water, USEPA-600/53-82-019, NTIS PB 82-230 939 (1982)
    CALL CalcH("chlorpyrifos",       "2921-88-2", 2.93E-6, "C") ! Rice CP, Chernyak SM; Organohalogen Compd. 24, 439-44 (1995)
    CALL CalcH("ethylene dibromide",  "106-93-4", 6.50E-4, "C") ! Rathbun RE; US Geol. Surv. Prof. Pap. 1589: 1-151 (1998)
    CALL CalcH("fluometuron",        "2164-17-2", 2.6E-9,  "C") ! Ahrens WH; Herbicide Handbook of the Weed Science Society of America. 7th ed. Champaign, IL: Weed Sci Soc Amer, p. 136 (1994)
    CALL CalcH("fluorene",             "86-73-7", 0.0001,  "C") ! D. Mackay, W.Y. Shiu, A. Bobra, J. Billington, E. Chau, A. Yeun, C. Ng, F. Szeto, Volatilization of Organic Pollutants from Water, USEPA-600/53-82-019, NTIS PB 82-230 939 (1982)
    CALL CalcH("trifluralin",        "1582-09-8", 1.03E-4, "C") ! Rice CP, Chernyak SM; Organohalogen Compd. 24, 439-44 (1995)
    ! type = "Q", "EPI-Suite"
    CALL CalcH("(L)-ephedrine",                       "299-42-3", 8.7E-11,  "Q", "EPI-Suite")
    CALL CalcH("1,2-dimethyl-3-nitrobenzene",          "83-41-0", 5.11E-5,  "Q", "EPI-Suite")
    CALL CalcH("1,3-diisopropylcarbodiimide",         "693-13-0", 9.92E-4,  "Q", "EPI-Suite")
    CALL CalcH("1,3-dimethylamylamine",               "105-41-9", 4.2E-5,   "Q", "EPI-Suite")
    CALL CalcH("1-fluoro-2,4-dinitrobenzene",          "70-34-8", 9.8E-8,   "Q", "EPI-Suite")
    CALL CalcH("1-nitrobenzo(a)pyrene",             "70021-99-7", 3.2E-9,   "Q", "EPI-Suite")
    CALL CalcH("2,2',6,6'-tetrachlorobisphenol A",     "79-95-8", 2.8E-12,  "Q", "EPI-Suite")
    CALL CalcH("2,2-dimethyltrimethylene acrylate",  "2223-82-7", 3.6E-7,   "Q", "EPI-Suite")
    CALL CalcH("2,3-dinitrotoluene",                  "602-01-7", 9.3E-8,   "Q", "EPI-Suite")
    CALL CalcH("3,3'-dimethylbenzidine",              "119-93-7", 6.3E-11,  "Q", "EPI-Suite")
    CALL CalcH("3,4,3',4'-tetrachloroazobenzene",   "14047-09-7", 4.4E-6,   "Q", "EPI-Suite")
    CALL CalcH("3,4-dinitrotoluene",                  "610-39-9", 9.3E-8,   "Q", "EPI-Suite")
    CALL CalcH("3,5-dinitrotoluene",                  "618-85-9", 9.3E-8,   "Q", "EPI-Suite")
    CALL CalcH("3-nitrobenzo(a)pyrene",             "70021-98-6", 3.2E-9,   "Q", "EPI-Suite")
    CALL CalcH("4-(1-methyl-1-phenylethyl)-phenol",   "599-64-4", 8.8E-8,   "Q", "EPI-Suite")
    CALL CalcH("4-bromoaniline",                      "106-40-1", 9.1E-7,   "Q", "EPI-Suite")
    CALL CalcH("4-fluoroaniline",                     "371-40-4", 6.1E-6,   "Q", "EPI-Suite")
    CALL CalcH("6-nitrobenzo(a)pyrene",             "63041-90-7", 3.2E-9,   "Q", "EPI-Suite")
    CALL CalcH("9,10-phenanthrenedione",               "84-11-7", 2.7E-9,   "Q", "EPI-Suite")
    CALL CalcH("acetaldehyde oxime",                  "107-29-9", 5.9E-6,   "Q", "EPI-Suite")
    CALL CalcH("alpha-amyl cinnamaldehyde",           "122-40-7", 7.8E-6,   "Q", "EPI-Suite")
    CALL CalcH("alpha-bromo-o-chlorotoluene",         "611-17-6", 5.1E-4,   "Q", "EPI-Suite")
    CALL CalcH("avobenzone",                        "70356-09-1", 2E-10,    "Q", "EPI-Suite")
    CALL CalcH("benz(c)acridine",                     "225-51-4", 2.7E-8,   "Q", "EPI-Suite")
    CALL CalcH("beta-nitrostyrene",                   "102-96-5", 3.48E-6,  "Q", "EPI-Suite")
    CALL CalcH("bisphenol AF",                       "1478-61-1", 5.7E-10,  "Q", "EPI-Suite")
    CALL CalcH("bisphenol B",                          "77-40-7", 1.2E-11,  "Q", "EPI-Suite")
    CALL CalcH("bisphenol C",                          "79-97-0", 1.1E-11,  "Q", "EPI-Suite")
    CALL CalcH("bisphenol F",                         "620-92-8", 5.2E-12,  "Q", "EPI-Suite")
    CALL CalcH("bisphenol S",                          "80-09-1", 2.7E-15,  "Q", "EPI-Suite")
    CALL CalcH("bromoacetonitrile",                   "590-17-0", 3.52E-6,  "Q", "EPI-Suite")
    CALL CalcH("C.I. Acid Green 3",                  "4680-78-8", 4.9E-29,  "Q", "EPI-Suite")
    CALL CalcH("chloroacetonitrile",                  "107-14-2", 1.08E-5,  "Q", "EPI-Suite")
    CALL CalcH("cinoxate",                            "104-28-9", 5.1E-9,   "Q", "EPI-Suite")
    CALL CalcH("D&C Green No. 5",                    "4403-90-1", 3.2E-29,  "Q", "EPI-Suite")
    CALL CalcH("D&C Yellow No. 8",                    "518-47-8", 2.8E-16,  "Q", "EPI-Suite")
    CALL CalcH("deoxynivalenol",                    "51481-10-8", 2.0E-14,  "Q", "EPI-Suite")
    CALL CalcH("diacetoxyscirpenol",                 "2270-40-8", 9.8E-17,  "Q", "EPI-Suite")
    CALL CalcH("difenacoum",                        "56073-07-5", 1.4E-12,  "Q", "EPI-Suite")
    CALL CalcH("dilauryl thiodipropionate",           "123-28-4", 4E-6,     "Q", "EPI-Suite")
    CALL CalcH("dinitrotoluene",                    "25321-14-6", 9.3E-8,   "Q", "EPI-Suite")
    CALL CalcH("diosmetin",                           "520-34-3", 3.0E-18,  "Q", "EPI-Suite")
    CALL CalcH("distearyl thiodipropionate",          "693-36-7", 1.2E-4,   "Q", "EPI-Suite")
    CALL CalcH("dulcin",                              "150-69-6", 1.58E-11, "Q", "EPI-Suite")
    CALL CalcH("FD&C Green No. 2",                   "5141-20-8", 1.4E-36,  "Q", "EPI-Suite")
    CALL CalcH("glutamine",                            "56-85-9", 3.0E-16,  "Q", "EPI-Suite")
    CALL CalcH("isopropyl palmitate",                 "142-91-6", 4.66E-2,  "Q", "EPI-Suite")
    CALL CalcH("maslinic acid",                      "4373-41-5", 3.5E-11,  "Q", "EPI-Suite")
    CALL CalcH("melatonin",                            "73-31-4", 2.6E-14,  "Q", "EPI-Suite")
    CALL CalcH("N-nitrosodiethanolamine",            "1116-54-7", 4.9E-12,  "Q", "EPI-Suite")
    CALL CalcH("N-nitrosomethylethylamine",         "10595-95-6", 1.44E-6,  "Q", "EPI-Suite")
    CALL CalcH("N-nitrosonornicotine",              "16543-55-8", 1.69E-10, "Q", "EPI-Suite")
    CALL CalcH("nornicotine",                         "494-97-3", 1.37E-9,  "Q", "EPI-Suite")
    CALL CalcH("octinoxate",                         "5466-77-3", 8.5E-6,   "Q", "EPI-Suite")
    CALL CalcH("p-cresyl diphenyl phosphate",          "78-31-9", 1.0E-7,   "Q", "EPI-Suite")
    CALL CalcH("Padimate O",                        "21245-02-3", 4E-6,     "Q", "EPI-Suite")
    CALL CalcH("pyrimethamine",                        "58-14-0", 1.08E-10, "Q", "EPI-Suite")
    CALL CalcH("raspberry ketone",                   "5471-51-2", 5.5E-10,  "Q", "EPI-Suite")
    CALL CalcH("resveratrol",                         "501-36-0", 1.4E-16,  "Q", "EPI-Suite")
    CALL CalcH("rosmarinic acid",                     "537-15-5", 2.7E-27,  "Q", "EPI-Suite")
    CALL CalcH("shikonin",                            "517-89-5", 8.3E-15,  "Q", "EPI-Suite")
    CALL CalcH("sulfanilic acid",                     "121-57-3", 8.9E-13,  "Q", "EPI-Suite")
    CALL CalcH("taurine",                             "107-35-7", 1.7E-12,  "Q", "EPI-Suite")
    CALL CalcH("taurocholic acid",                     "81-24-3", 5.3E-21,  "Q", "EPI-Suite")
    CALL CalcH("terephthalic acid",                   "100-21-0", 3.9E-13,  "Q", "EPI-Suite")
    CALL CalcH("theanine",                           "3081-61-6", 8.8E-16,  "Q", "EPI-Suite")
    CALL CalcH("tri-m-cresyl phosphate",              "563-04-2", 1.E-6,    "Q", "EPI-Suite")
    CALL CalcH("tri-o-cresyl phosphate",               "78-30-8", 1.9E-6,   "Q", "EPI-Suite")
    CALL CalcH("tri-p-cresyl phosphate",               "78-32-0", 5.4E-8,   "Q", "EPI-Suite")
    CALL CalcH("tribromobisphenol A",                "6386-73-8", 9.0E-8,   "Q", "EPI-Suite")
    CALL CalcH("triisopropanolamine",                 "122-20-3", 9.8E-12,  "Q", "EPI-Suite")
    CALL CalcH("trimethylolpropane triacrylate",    "15625-89-5", 6E-10,    "Q", "EPI-Suite")
    CALL CalcH("triolein",                            "122-32-7", 9.6E-4,   "Q", "EPI-Suite")
    ! type = "Q", "642"
    CALL CalcH("2-anthracenamine",                    "613-13-8", 3.0E-7,   "Q", "642")
    CALL CalcH("1-amino-2-propanol",                   "78-96-6", 2.34E-10, "Q", "642")
    CALL CalcH("1-propoxy-2-propanol",               "1569-01-3", 3.46E-8,  "Q", "642")
    CALL CalcH("2,2'-bipyridine",                     "366-18-7", 5.4E-10,  "Q", "642")
    CALL CalcH("2,4-dinitroaniline",                   "97-02-9", 1.51E-10, "Q", "642")
    CALL CalcH("2-aminofluorene",                     "153-78-6", 3.62E-8,  "Q", "642")
    CALL CalcH("2-chlorophenylthiourea",             "5344-82-1", 1E-7,     "Q", "642", upperlimit=1)
    CALL CalcH("2-chloro-4-nitrotoluene",             "121-86-8", 4.06E-5,  "Q", "642")
    CALL CalcH("2-nitro-9H-fluorene",                 "607-57-8", 2.87E-7,  "Q", "642")
    CALL CalcH("4-hydroxy-4-methyl-2-pentanone",      "123-42-2", 4.24E-9,  "Q", "642")
    CALL CalcH("5-chloro-o-toluidine",                 "95-79-4", 1.56E-6,  "Q", "642")
    CALL CalcH("5-nitro-o-anisidine",                  "99-59-2", 1.3E-8,   "Q", "642")
    CALL CalcH("erucamide",                           "112-84-5", 2.844E-6, "Q", "642")
    CALL CalcH("heptachlorodibenzo-p-dioxin",       "35822-46-9", 2.18E-5,  "Q", "642")
    CALL CalcH("m-chlorobenzoic acid",                "535-80-8", 3.88E-8,  "Q", "642")
    CALL CalcH("p-nitrobiphenyl",                      "92-93-3", 3.54E-6,  "Q", "642")
    CALL CalcH("toluene-3,5-diamine",                 "108-71-4", 7.43E-10, "Q", "642")
    ! type = "Q", "1145"
    CALL CalcH("(d)-propoxyphene",                                         "469-62-5", 2.3E-9,    "Q", "1145")
    CALL CalcH("1,1,1,2,3,3,3-heptafluoropropane",                         "431-89-0", 16.,       "Q", "1145")
    CALL CalcH("1,1,1,2-tetrafluoroethane",                                "811-97-2", 1.53,      "Q", "1145")
    CALL CalcH("1,1,1,3,3,3-hexafluoro-2-propanone",                       "684-16-2", 3.07E-3,   "Q", "1145")
    CALL CalcH("1,1,1-tribromo-2-methyl-2-propanol",                        "76-08-4", 9.6E-9,    "Q", "1145")
    CALL CalcH("1,1,1-trichloro-2,2,2-trifluoroethane",                    "354-58-5", 0.27,      "Q", "1145")
    CALL CalcH("1,1,1-trichloroacetone",                                   "918-00-3", 2.2E-6,    "Q", "1145")
    CALL CalcH("1,1,1-trifluoro-2-chloroethane",                            "75-88-7", 0.27,      "Q", "1145")
    CALL CalcH("1,1,1-tris(hydroxymethyl)ethane",                           "77-85-0", 1.1E-8,    "Q", "1145")
    CALL CalcH("1,1,2,2-tetrachloro-1-fluoroethane",                       "354-14-3", 0.003,     "Q", "1145")
    CALL CalcH("1,1-dibromoethane",                                        "557-91-5", 1.3E-3,    "Q", "1145")
    CALL CalcH("1,1-dichloro-1,2,2-trifluoroethane",                       "812-04-4", 9.6E-2,    "Q", "1145")
    CALL CalcH("1,1-dichloro-1-nitroethane",                               "594-72-9", 0.00128,   "Q", "1145")
    CALL CalcH("1,1-dichloro-2,2,3,3,3-pentafluoropropane",                "422-56-0", 0.11,      "Q", "1145")
    CALL CalcH("1,1-dichloroacetone",                                      "513-88-2", 6.1E-6,    "Q", "1145")
    CALL CalcH("1,1-dimethoxyethane",                                      "534-15-6", 6.7E-5,    "Q", "1145")
    CALL CalcH("1,1-diphenylhydrazine",                                    "530-50-7", 4.1E-8,    "Q", "1145")
    CALL CalcH("1,2,3,6,7,8-hexachlorodibenzo-p-dioxin",                 "57653-85-7", 1.9E-6,    "Q", "1145")
    CALL CalcH("1,2,3,6-tetrahydrophthalimide",                             "85-40-5", 3.0E-8,    "Q", "1145")
    CALL CalcH("1,2,3,7,8,9-hexachlorodibenzo-p-dioxin",                 "19408-74-3", 1.9E-6,    "Q", "1145")
    CALL CalcH("1,2,3-benzotriazin-4-(1H)-one",                             "90-16-4", 3.2E-10,   "Q", "1145")
    CALL CalcH("1,2,3-trichloro-1-propene",                                 "96-19-5", 1.8E-2,    "Q", "1145")
    CALL CalcH("1,2,4-tribromo-3,5,6-trichlorobenzene",                  "13075-01-9", 2.39E-4,   "Q", "1145")
    CALL CalcH("1,2-benzenedicarbonitrile",                                 "91-15-6", 5.0E-7,    "Q", "1145")
    CALL CalcH("1,2-bis(2,4,6-tribromophenoxy)ethane",                   "37853-59-1", 4.3E-7,    "Q", "1145")
    CALL CalcH("1,2-butadiene",                                            "590-19-2", 0.097,     "Q", "1145")
    CALL CalcH("1,2-dibromo-1,1-dichloroethane",                            "75-81-0", 1.6E-4,    "Q", "1145")
    CALL CalcH("1,2-dibromo-4-(1,2-dibromoethyl)cyclohexane",             "3322-93-8", 5.7E-8,    "Q", "1145")
    CALL CalcH("1,2-dichloro-1,1,2-trifluoroethane",                       "354-23-4", 9.5E-2,    "Q", "1145")
    CALL CalcH("1,2-diethylhydrazine",                                    "1615-80-1", 1.2E-7,    "Q", "1145")
    CALL CalcH("1,2-difluoroethane",                                       "624-72-6", 0.39,      "Q", "1145")
    CALL CalcH("1,2-dimethoxyethane",                                      "110-71-4", 1.1E-6,    "Q", "1145")
    CALL CalcH("1,2-ethanedithiol",                                        "540-63-6", 1.2E-4,    "Q", "1145")
    CALL CalcH("1,2-naphthoquinone",                                       "524-42-5", 4.2E-9,    "Q", "1145")
    CALL CalcH("1,2-propanediol dinitrate",                               "6423-43-4", 9.4E-7,    "Q", "1145")
    CALL CalcH("1,2-propylene glycol diacetate",                           "623-84-7", 1.4E-7,    "Q", "1145")
    CALL CalcH("1,25-dihydroxycholecalciferol",                          "32222-06-3", 3.1E-7,    "Q", "1145")
    CALL CalcH("1,2:3,4-diepoxybutane",                                   "1464-53-5", 3.54E-8,   "Q", "1145")
    CALL CalcH("1,3-butylene oxide",                                      "2167-39-7", 8.4E-5,    "Q", "1145")
    CALL CalcH("1,3-dichloro-1,1,2,2,3-pentafluoropropane",                "507-55-1", 2.76,      "Q", "1145")
    CALL CalcH("1,3-dichloro-2-butene",                                    "926-57-8", 3.8E-2,    "Q", "1145")
    CALL CalcH("1,3-dichloro-2-propanol",                                   "96-23-1", 6E-7,      "Q", "1145")
    CALL CalcH("1,3-dichloro-5,5-dimethylhydantoin",                       "118-52-5", 1.0E-6,    "Q", "1145")
    CALL CalcH("1,3-dimethylol-5,5-dimethylhydantoin",                    "6440-58-0", 7.1E-12,   "Q", "1145")
    CALL CalcH("1,3-pentadiene",                                           "504-60-9", 0.069,     "Q", "1145")
    CALL CalcH("1,4-benzenediamine",                                       "106-50-3", 6.7E-10,   "Q", "1145")
    CALL CalcH("1,4-dinitrobenzene",                                       "100-25-4", 8.4E-8,    "Q", "1145")
    CALL CalcH("1,4-hexadiene",                                            "592-45-0", 0.117,     "Q", "1145")
    CALL CalcH("1,4-naphthoquinone",                                       "130-15-4", 1.97E-9,   "Q", "1145")
    CALL CalcH("1,5,9-cyclododecatriene",                                 "4904-61-4", 0.03,      "Q", "1145")
    CALL CalcH("1,5-naphthalenediamine",                                  "2243-62-1", 6.6E-11,   "Q", "1145")
    CALL CalcH("1,6-dimethylnaphthalene",                                  "575-43-9", 4.2E-4,    "Q", "1145")
    CALL CalcH("1,6-dinitropyrene",                                      "42397-64-8", 1.3E-10,   "Q", "1145")
    CALL CalcH("1,6-hexanediol",                                           "629-11-8", 2.2E-10,   "Q", "1145")
    CALL CalcH("1,8-dihydroxyanthraquinone",                               "117-10-2", 5.4E-11,   "Q", "1145")
    CALL CalcH("1,8-dinitropyrene",                                      "42397-65-9", 1.3E-10,   "Q", "1145")
    CALL CalcH("1-acetyl-2-thiourea",                                      "591-08-2", 2.6E-11,   "Q", "1145")
    CALL CalcH("1-amino-2,4-dibromoanthraquinone",                          "81-49-2", 1.8E-13,   "Q", "1145")
    CALL CalcH("1-amino-2-methylanthraquinone",                             "82-28-0", 1.2E-12,   "Q", "1145")
    CALL CalcH("1-amino-2-thiourea",                                        "79-19-6", 6.6E-10,   "Q", "1145")
    CALL CalcH("1-aziridineethanol",                                      "1072-52-2", 7.8E-10,   "Q", "1145")
    CALL CalcH("1-bromo-3-chloro-5,5-dimethylhydantoin",                 "16079-88-2", 8.1E-7,    "Q", "1145")
    CALL CalcH("1-bromo-3-chloropropane",                                  "109-70-6", 2.5E-4,    "Q", "1145")
    CALL CalcH("1-chloro-1,1,2,2-tetrafluoroethane",                       "354-25-6", 0.54,      "Q", "1145")
    CALL CalcH("1-chloro-1-propene",                                       "590-21-6", 0.055,     "Q", "1145")
    CALL CalcH("1-chloro-2-propanol",                                      "127-00-4", 1.7E-6,    "Q", "1145")
    CALL CalcH("1-chloro-4-(trifluoromethyl)benzene",                       "98-56-6", 3.5E-2,    "Q", "1145")
    CALL CalcH("1-chloro-4-chloromethylbenzene",                           "104-83-6", 3.4E-4,    "Q", "1145")
    CALL CalcH("1-chloroanthraquinone",                                     "82-44-0", 2.36E-9,   "Q", "1145")
    CALL CalcH("1-chlorooctane",                                           "111-85-3", 6.0E-2,    "Q", "1145")
    CALL CalcH("1-docosanol",                                              "661-19-8", 1.6E-3,    "Q", "1145")
    CALL CalcH("1-dodecanethiol",                                          "112-55-0", 5.9E-2,    "Q", "1145")
    CALL CalcH("1-dodecene",                                               "112-41-4", 4.25,      "Q", "1145")
    CALL CalcH("1-eicosanol",                                              "629-96-9", 2.1E-5,    "Q", "1145")
    CALL CalcH("1-methoxy-4-methylbenzene",                                "104-93-8", 4.7E-3,    "Q", "1145")
    CALL CalcH("1-methylcyclopropene",                                    "3100-04-7", 4E-2,      "Q", "1145")
    CALL CalcH("1-methylpyrene",                                          "2381-21-7", 3.2E-6,    "Q", "1145")
    CALL CalcH("1-nitropyrene",                                           "5522-43-0", 2.5E-8,    "Q", "1145")
    CALL CalcH("1-octanethiol",                                            "111-88-6", 2.3E-2,    "Q", "1145")
    CALL CalcH("1-phenyl-2-thiourea",                                      "103-85-5", 1.0E-10,   "Q", "1145")
    CALL CalcH("1-piperidinyl, 4-hydroxy-2,2,6,6-tetramethyl-",           "2226-96-2", 3.0E-15,   "Q", "1145")
    CALL CalcH("1-tetracosanol",                                           "506-51-4", 2.9E-3,    "Q", "1145")
    CALL CalcH("1-tetradecene",                                           "1120-36-1", 8.48,      "Q", "1145")
    CALL CalcH("1-tridecanol",                                             "112-70-9", 1.3E-4,    "Q", "1145")
    CALL CalcH("1-tridecene",                                             "2437-56-1", 2.61,      "Q", "1145")
    CALL CalcH("1-undecanol",                                              "112-42-5", 7.23E-5,   "Q", "1145")
    CALL CalcH("1-undecene",                                               "821-95-4", 1.48,      "Q", "1145")
    CALL CalcH("10-methylbenz(a)anthracene",                              "2381-15-9", 1.9E-6,    "Q", "1145")
    CALL CalcH("12-methylbenz(a)anthracene",                              "2422-79-9", 1.9E-6,    "Q", "1145")
    CALL CalcH("17-methyltestosterone",                                     "58-18-4", 4.7E-9,    "Q", "1145")
    CALL CalcH("1H-1,2,4-triazole",                                        "288-88-0", 1.5E-6,    "Q", "1145")
    CALL CalcH("2(5H)-furanone",                                           "497-23-4", 9.7E-6,    "Q", "1145")
    CALL CalcH("2,2',4,4',6,6'-hexanitrodiphenylamine",                    "131-73-7", 2.3E-17,   "Q", "1145")
    CALL CalcH("2,2'-dibenzothiazyl disulfide",                            "120-78-5", 2.34E-13,  "Q", "1145")
    CALL CalcH("2,2,4-trimethyl-1,3-pentanediol diisobutyrate",           "6846-50-0", 1.1E-5,    "Q", "1145")
    CALL CalcH("2,2,4-trimethyl-1,3-pentanediol",                          "144-19-4", 7.16E-7,   "Q", "1145")
    CALL CalcH("2,2,4-trimethyl-2-pentene",                                "107-40-4", 0.881,     "Q", "1145")
    CALL CalcH("2,2-bis(bromomethyl)-1,3-propanediol",                    "3296-90-0", 4.1E-9,    "Q", "1145")
    CALL CalcH("2,3,4,5-tetrachloronitrobenzene",                          "879-39-0", 2.3E-5,    "Q", "1145")
    CALL CalcH("2,3,4,5-tetrachlorophenol",                               "4901-51-3", 3.5E-7,    "Q", "1145")
    CALL CalcH("2,3,4,6-tetrabromophenol",                               "14400-94-3", 1.4E-8,    "Q", "1145")
    CALL CalcH("2,3,5,6-tetrachloro-4-nitroanisole",                      "2438-88-2", 1.9E-5,    "Q", "1145")
    CALL CalcH("2,3,5,6-tetrachlorophenol",                                "935-95-5", 3.5E-7,    "Q", "1145")
    CALL CalcH("2,3,5,6-tetrachloropyridine",                             "2402-79-1", 8.5E-3,    "Q", "1145")
    CALL CalcH("2,3,6-trichlorophenol",                                    "933-75-5", 2.3E-7,    "Q", "1145")
    CALL CalcH("2,3,6-trichlorophenylacetic acid",                          "85-34-7", 1.8E-8,    "Q", "1145")
    CALL CalcH("2,3,7,8-tetrachlorodibenzofuran",                        "51207-31-9", 1.54E-5,   "Q", "1145")
    CALL CalcH("2,3-diaminotoluene",                                      "2687-25-4", 9.5E-10,   "Q", "1145")
    CALL CalcH("2,3-dibromopropanol",                                       "96-13-9", 6.3E-8,    "Q", "1145")
    CALL CalcH("2,3-dichloroaniline",                                      "608-27-5", 1.6E-6,    "Q", "1145")
    CALL CalcH("2,3-dichloronitrobenzene",                                "3209-22-1", 1.2E-5,    "Q", "1145")
    CALL CalcH("2,3-dichloropropanol",                                     "616-23-9", 3E-9,      "Q", "1145")
    CALL CalcH("2,3-xylidine",                                              "87-59-2", 2.5E-6,    "Q", "1145")
    CALL CalcH("2,4,4-trimethyl-1-pentene",                                "107-39-1", 0.746,     "Q", "1145")
    CALL CalcH("2,4,5-t butoxyethanol ester",                             "2545-59-7", 7.95E-8,   "Q", "1145")
    CALL CalcH("2,4,6-tribromoanisole",                                    "607-99-8", 3.15E-4,   "Q", "1145")
    CALL CalcH("2,4,6-tribromophenol",                                     "118-79-6", 4.8E-8,    "Q", "1145")
    CALL CalcH("2,4,6-trimethylaniline",                                    "88-05-1", 2.7E-6,    "Q", "1145")
    CALL CalcH("2,4,6-trinitrobenzoic acid",                               "129-66-8", 2.62E-14,  "Q", "1145")
    CALL CalcH("2,4-D dimethylamine",                                     "2008-39-1", 1.4E-16,   "Q", "1145")
    CALL CalcH("2,4-D isooctyl ester",                                   "25168-26-7", 5.7E-5,    "Q", "1145")
    CALL CalcH("2,4-diamino-6-phenyl-1,3,5-triazine",                       "91-76-9", 4.1E-11,   "Q", "1145")
    CALL CalcH("2,4-diaminoanisole",                                       "615-05-4", 7.2E-10,   "Q", "1145")
    CALL CalcH("2,4-dibromophenol",                                        "615-58-7", 8.9E-8,    "Q", "1145")
    CALL CalcH("2,4-dichloroaniline",                                      "554-00-7", 1.6E-6,    "Q", "1145")
    CALL CalcH("2,4-dichloronitrobenzene",                                 "611-06-3", 3.2E-5,    "Q", "1145")
    CALL CalcH("2,4-dichlorophenoxyethyl sulfate",                         "136-78-7", 2.6E-11,   "Q", "1145")
    CALL CalcH("2,4-dichlorotoluene",                                       "95-73-8", 4.2E-3,    "Q", "1145")
    CALL CalcH("2,4-dinitro-6-cyclohexylphenol",                           "131-89-5", 5.5E-8,    "Q", "1145")
    CALL CalcH("2,4-dinonylphenol",                                        "137-99-5", 6.4E-5,    "Q", "1145")
    CALL CalcH("2,4-xylidine",                                              "95-68-1", 2.5E-6,    "Q", "1145")
    CALL CalcH("2,5-dichloroaniline",                                       "95-82-9", 1.6E-6,    "Q", "1145")
    CALL CalcH("2,5-dihydrothiophene 1,1-dioxide",                          "77-79-2", 4.3E-6,    "Q", "1145")
    CALL CalcH("2,5-xylidine",                                              "95-78-3", 2.5E-6,    "Q", "1145")
    CALL CalcH("2,6-di-tert-butyl-p-benzoquinone",                         "719-22-2", 1.6E-8,    "Q", "1145")
    CALL CalcH("2,6-di-tert-butylphenol",                                  "128-39-2", 3.15E-6,   "Q", "1145")
    CALL CalcH("2,6-diaminotoluene",                                       "823-40-5", 7.4E-10,   "Q", "1145")
    CALL CalcH("2,6-dibromophenol",                                        "608-33-3", 8.9E-8,    "Q", "1145")
    CALL CalcH("2,6-dichlorobenzamide",                                   "2008-58-4", 1.2E-9,    "Q", "1145")
    CALL CalcH("2,6-dichlorotoluene",                                      "118-69-4", 4.2E-3,    "Q", "1145")
    CALL CalcH("2,6-dinitro-4-chloroaniline",                             "5388-62-5", 1.3E-7,    "Q", "1145")
    CALL CalcH("2,6-dinitro-4-octylphenol",                               "4097-33-0", 6E-10,     "Q", "1145")
    CALL CalcH("2,7-dimethylpyrene",                                     "15679-24-0", 3.38E-6,   "Q", "1145")
    CALL CalcH("2-(1-methylpropyl)phenol",                                  "89-72-5", 2.1E-6,    "Q", "1145")
    CALL CalcH("2-(ethylthio)ethanol",                                     "110-77-0", 5.1E-8,    "Q", "1145")
    CALL CalcH("2-acetylaminofluorene",                                     "53-96-3", 1.9E-10,   "Q", "1145")
    CALL CalcH("2-amino-1-methyl-6-phenylimidazo(4,5-b)pyridine",       "105650-23-5", 2.8E-13,   "Q", "1145")
    CALL CalcH("2-amino-3,4-dimethylimidazo(4,5-f)quinoxaline",          "77094-11-2", 3.9E-13,   "Q", "1145")
    CALL CalcH("2-amino-3,8-dimethylimidazo(4,5-f)quinoxaline",          "77500-04-0", 1.6E-13,   "Q", "1145")
    CALL CalcH("2-amino-4-nitrophenol",                                     "99-57-0", 2.2E-12,   "Q", "1145")
    CALL CalcH("2-amino-5-nitrophenol",                                    "121-88-0", 7.8E-13,   "Q", "1145")
    CALL CalcH("2-amino-5-nitrothiazole",                                  "121-66-4", 5.3E-12,   "Q", "1145")
    CALL CalcH("2-amino-9H-pyrido(2,3-b)indole",                         "26148-68-5", 4.0E-14,   "Q", "1145")
    CALL CalcH("2-aminobenzamide",                                          "88-68-6", 7.8E-13,   "Q", "1145")
    CALL CalcH("2-aminobenzothiazole",                                     "136-95-8", 1.3E-10,   "Q", "1145")
    CALL CalcH("2-aminophenol",                                             "95-55-6", 2E-10,     "Q", "1145")
    CALL CalcH("2-aminopyridine",                                          "504-29-0", 2.5E-9,    "Q", "1145")
    CALL CalcH("2-biphenylamine",                                           "90-41-5", 1.5E-7,    "Q", "1145")
    CALL CalcH("2-bromophenol",                                             "95-56-7", 2.2E-7,    "Q", "1145")
    CALL CalcH("2-bromotoluene",                                            "95-46-5", 2.4E-3,    "Q", "1145")
    CALL CalcH("2-chloro-1,3,5-trinitrobenzene",                            "88-88-0", 2.5E-10,   "Q", "1145")
    CALL CalcH("2-chloro-1,3-butadiene",                                   "126-99-8", 0.056,     "Q", "1145")
    CALL CalcH("2-chloro-1-propanol",                                       "78-89-7", 1.7E-6,    "Q", "1145")
    CALL CalcH("2-chloro-1-propene",                                       "557-98-2", 7.0E-2,    "Q", "1145")
    CALL CalcH("2-chloro-5-nitroaniline",                                 "6283-25-6", 5.6E-9,    "Q", "1145")
    CALL CalcH("2-chloroacetophenone",                                     "532-27-4", 3.5E-6,    "Q", "1145")
    CALL CalcH("2-chlorobenzalmalononitrile",                             "2698-41-1", 1.0E-8,    "Q", "1145")
    CALL CalcH("2-chloroethanol",                                          "107-07-3", 1.04E-7,   "Q", "1145")
    CALL CalcH("2-chloroethyl ethyl sulfide",                              "693-07-2", 4.9E-4,    "Q", "1145")
    CALL CalcH("2-chloropropanoic acid",                                   "598-78-7", 2.6E-7,    "Q", "1145")
    CALL CalcH("2-ethylaniline",                                           "578-54-1", 3.6E-6,    "Q", "1145")
    CALL CalcH("2-ethylhexyl acetate",                                     "103-09-3", 1.5E-3,    "Q", "1145")
    CALL CalcH("2-ethylhexyl butyl phthalate",                              "85-69-8", 2.1E-6,    "Q", "1145")
    CALL CalcH("2-hydroxy-4-methoxybenzophenone",                          "131-57-7", 1.5E-8,    "Q", "1145")
    CALL CalcH("2-hydroxyethyl methacrylate",                              "868-77-9", 4.6E-9,    "Q", "1145")
    CALL CalcH("2-hydroxymethyl-2-nitro-1,3-propanediol",                  "126-11-4", 4.8E-12,   "Q", "1145")
    CALL CalcH("2-hydroxypropanenitrile",                                   "78-97-7", 9.8E-6,    "Q", "1145")
    CALL CalcH("2-hydroxypropyl acrylate",                                 "999-61-1", 6.0E-9,    "Q", "1145")
    CALL CalcH("2-isopropylnaphthalene",                                  "2027-17-0", 8.5E-4,    "Q", "1145")
    CALL CalcH("2-mercaptobenzothiazole",                                  "149-30-4", 3.6E-8,    "Q", "1145")
    CALL CalcH("2-methoxy-1,4-benzenediamine",                            "5307-02-8", 4.0E-11,   "Q", "1145")
    CALL CalcH("2-methoxy-1-propanol",                                    "1589-47-5", 1.8E-8,    "Q", "1145")
    CALL CalcH("2-methyl-1,3-propanediol",                                "2163-42-0", 2.3E-7,    "Q", "1145")
    CALL CalcH("2-methyl-1,4-benzenediamine",                               "95-70-5", 7.4E-10,   "Q", "1145")
    CALL CalcH("2-methyl-1,4-benzoquinone",                                "553-97-9", 1.9E-9,    "Q", "1145")
    CALL CalcH("2-methyl-2,4-pentanediol",                                 "107-41-5", 4E-7,      "Q", "1145")
    CALL CalcH("2-methyl-2-butanol",                                        "75-85-4", 1.38E-5,   "Q", "1145")
    CALL CalcH("2-methyl-5-vinylpyridine",                                 "140-76-1", 4.4E-6,    "Q", "1145")
    CALL CalcH("2-methylbenzaldehyde",                                     "529-20-4", 3.0E-5,    "Q", "1145")
    CALL CalcH("2-methylimidazole",                                        "693-98-1", 4.4E-6,    "Q", "1145")
    CALL CalcH("2-methylpentaldehyde",                                     "123-15-9", 3.7E-4,    "Q", "1145")
    CALL CalcH("2-methylpentanedinitrile",                                "4553-62-2", 2.97E-8,   "Q", "1145")
    CALL CalcH("2-methylpropanenitrile",                                    "78-82-0", 5.39E-5,   "Q", "1145")
    CALL CalcH("2-methylpyrene",                                          "3442-78-2", 3.2E-6,    "Q", "1145")
    CALL CalcH("2-naphthol",                                               "135-19-3", 4.6E-8,    "Q", "1145")
    CALL CalcH("2-nitro-p-phenylenediamine",                              "5307-14-2", 5.8E-11,   "Q", "1145")
    CALL CalcH("2-phenylethyl propionate",                                 "122-70-3", 2.5E-5,    "Q", "1145")
    CALL CalcH("2-phenylethylamine",                                        "64-04-0", 8.1E-7,    "Q", "1145")
    CALL CalcH("2-phenylisopropanol",                                      "617-94-7", 3.8E-7,    "Q", "1145")
    CALL CalcH("2-pyridine ethanol",                                       "103-74-2", 1.5E-10,   "Q", "1145")
    CALL CalcH("2-pyridinecarbonitrile",                                   "100-70-9", 6.89E-8,   "Q", "1145")
    CALL CalcH("2-tert-butyl-4-methylphenol",                             "2409-55-4", 1.5E-6,    "Q", "1145")
    CALL CalcH("2-tert-butylphenol",                                        "88-18-6", 1.4E-6,    "Q", "1145")
    CALL CalcH("2-thiocresol",                                             "137-06-4", 3.6E-4,    "Q", "1145")
    CALL CalcH("2-vinylpyridine",                                          "100-69-6", 3.6E-6,    "Q", "1145")
    CALL CalcH("3,3',4',5-tetrachlorosalicylanilide",                     "1154-59-2", 4.8E-11,   "Q", "1145")
    CALL CalcH("3,3',5,5'-tetramethylbenzidine",                         "54827-17-7", 7.7E-11,   "Q", "1145")
    CALL CalcH("3,3'-dichlorobenzidine",                                    "91-94-1", 2.8E-11,   "Q", "1145")
    CALL CalcH("3,3'-dimethoxybenzidine",                                  "119-90-4", 4.7E-11,   "Q", "1145")
    CALL CalcH("3,3,3-trifluoro-1-propene",                                "677-21-4", 0.76,      "Q", "1145")
    CALL CalcH("3,4,4-trimethyl-2-pentene",                                "598-96-9", 0.881,     "Q", "1145")
    CALL CalcH("3,4,5-trichlorophenol",                                    "609-19-8", 2.3E-7,    "Q", "1145")
    CALL CalcH("3,4-dichlorobenzotrifluoride",                             "328-84-7", 2.6E-2,    "Q", "1145")
    CALL CalcH("3,4-dichlorophenol",                                        "95-77-2", 4.77E-7,   "Q", "1145")
    CALL CalcH("3,5,5-trimethylhexanal",                                  "5435-64-3", 4.9E-4,    "Q", "1145")
    CALL CalcH("3,5-dichloroaniline",                                      "626-43-7", 1.58E-6,   "Q", "1145")
    CALL CalcH("3,5-xylidine",                                             "108-69-0", 2.5E-6,    "Q", "1145")
    CALL CalcH("3,5-xylyl methylcarbamate",                               "2655-14-3", 3.91E-8,   "Q", "1145")
    CALL CalcH("3,7-dinitrofluoranthene",                               "105735-71-5", 2.0E-10,   "Q", "1145")
    CALL CalcH("3-(methylamino)-(DL)-alanine",                           "16676-91-8", 3.4E-13,   "Q", "1145")
    CALL CalcH("3-aminophenol",                                            "591-27-5", 2.7E-10,   "Q", "1145")
    CALL CalcH("3-bromophenol",                                            "591-20-8", 2.2E-7,    "Q", "1145")
    CALL CalcH("3-chloro-1,2-dihydroxypropane",                             "96-24-2", 6.1E-8,    "Q", "1145")
    CALL CalcH("3-chloro-2-methylphenylamine",                              "87-60-5", 1.56E-6,   "Q", "1145")
    CALL CalcH("3-chloro-4-(dichloromethyl)-2-(5H)-furanone",           "122551-89-7", 6.7E-6,    "Q", "1145")
    CALL CalcH("3-chloro-4-(dichloromethyl)-5-hydroxy-2(5H)-furanone",   "77439-76-0", 2.5E-10,   "Q", "1145")
    CALL CalcH("3-chloro-p-toluidine",                                      "95-74-9", 2.0E-6,    "Q", "1145")
    CALL CalcH("3-ethylphenol",                                            "620-17-7", 1.1E-6,    "Q", "1145")
    CALL CalcH("3-hydroxy-2-naphthalenecarboxylic acid",                    "92-70-6", 1.4E-9,    "Q", "1145")
    CALL CalcH("3-isopropylphenyl methyl carbamate",                        "64-00-6", 6.3E-8,    "Q", "1145")
    CALL CalcH("3-methyl-6-chlorophenol",                                  "615-74-7", 4.6E-7,    "Q", "1145")
    CALL CalcH("3-methylbenzaldehyde",                                     "620-23-5", 3.0E-5,    "Q", "1145")
    CALL CalcH("3-pyridinecarbonitrile",                                   "100-54-9", 2.74E-7,   "Q", "1145")
    CALL CalcH("3-quinuclidinol",                                         "1619-34-7", 8.1E-10,   "Q", "1145")
    CALL CalcH("3-thiocresol",                                             "108-40-7", 3.6E-4,    "Q", "1145")
    CALL CalcH("3-trifluoromethyl-4-nitrophenol",                           "88-30-2", 1.9E-8,    "Q", "1145")
    CALL CalcH("4,4'-dichloroazobenzene",                                 "1602-00-2", 8.1E-6,    "Q", "1145")
    CALL CalcH("4,4'-dichlorophenyl sulfone",                               "80-07-9", 1.4E-7,    "Q", "1145")
    CALL CalcH("4,4'-methylenebis(2-chloroaniline)",                       "101-14-4", 1.1E-11,   "Q", "1145")
    CALL CalcH("4,4'-methylenebis(N-methylaniline)",                      "1807-55-2", 2.9E-10,   "Q", "1145")
    CALL CalcH("4,6-dinitro-2-aminophenol",                                 "96-91-3", 9.8E-12,   "Q", "1145")
    CALL CalcH("4-(1,1,3,3-tetramethylbutyl)phenol",                       "140-66-9", 6.9E-6,    "Q", "1145")
    CALL CalcH("4-(2,4-dichlorophenoxy)butyric acid",                       "94-82-6", 2.29E-9,   "Q", "1145")
    CALL CalcH("4-(iso-propylamino)diphenylamine",                         "101-72-4", 1.4E-9,    "Q", "1145")
    CALL CalcH("4-(N-nitrosomethylamino)-1-(3-pyridyl)-1-butanone",      "64091-91-4", 8.3E-14,   "Q", "1145")
    CALL CalcH("4-acetylaniline",                                           "99-92-3", 4.4E-9,    "Q", "1145")
    CALL CalcH("4-amino-2-nitrophenol",                                    "119-34-6", 2.2E-12,   "Q", "1145")
    CALL CalcH("4-aminoazobenzene",                                         "60-09-3", 8.70E-11,  "Q", "1145")
    CALL CalcH("4-aminopropiophenone",                                      "70-69-9", 4.6E-9,    "Q", "1145")
    CALL CalcH("4-biphenylamine",                                           "92-67-1", 1.5E-7,    "Q", "1145")
    CALL CalcH("4-bromophenyl phenyl ether",                               "101-55-3", 1.2E-4,    "Q", "1145")
    CALL CalcH("4-chloro-2-nitroaniline",                                   "89-63-4", 1.2E-7,    "Q", "1145")
    CALL CalcH("4-chloro-3-nitroaniline",                                  "635-22-3", 5.6E-9,    "Q", "1145")
    CALL CalcH("4-chloro-3-nitrobenzenesulfonamide",                        "97-09-6", 1.2E-9,    "Q", "1145")
    CALL CalcH("4-chloro-o-toluidine",                                      "95-69-2", 2E-6,      "Q", "1145")
    CALL CalcH("4-chlorobenzophenone",                                     "134-85-0", 1.4E-6,    "Q", "1145")
    CALL CalcH("4-hydroxybiphenyl",                                         "92-69-3", 5.2E-8,    "Q", "1145")
    CALL CalcH("4-hydroxyphenylsulfonic acid",                              "98-67-9", 2.62E-13,  "Q", "1145")
    CALL CalcH("4-methoxyphenol",                                          "150-76-5", 5.3E-7,    "Q", "1145")
    CALL CalcH("4-methylcyclohexanemethanol",                            "34885-03-5", 6.4E-6,    "Q", "1145")
    CALL CalcH("4-methylimidazole",                                        "822-36-6", 4.1E-6,    "Q", "1145")
    CALL CalcH("4-methylquinoline",                                        "491-35-0", 7.6E-7,    "Q", "1145")
    CALL CalcH("4-nitro-1,2-diaminobenzene",                                "99-56-9", 7.4E-12,   "Q", "1145")
    CALL CalcH("4-nitro-1,3-benzenediamine",                              "5131-58-8", 5.8E-11,   "Q", "1145")
    CALL CalcH("4-nitrobenzoic acid",                                       "62-23-7", 3.8E-10,   "Q", "1145")
    CALL CalcH("4-nitropyrene",                                          "57835-92-4", 2.5E-08,   "Q", "1145")
    CALL CalcH("4-tert-butyltoluene",                                       "98-51-1", 0.01535,   "Q", "1145")
    CALL CalcH("4-thiocresol",                                             "106-45-6", 3.6E-4,    "Q", "1145")
    CALL CalcH("4-vinylpyridine",                                          "100-43-6", 3.2E-6,    "Q", "1145")
    CALL CalcH("5,5-dimethylhydantoin",                                     "77-71-4", 2.8E-9,    "Q", "1145")
    CALL CalcH("5-ethylidene-2-norbornene",                              "16219-75-3", 0.13,      "Q", "1145")
    CALL CalcH("5-hydroxymethyl-2-furfuraldehyde (HMF)",                    "67-47-0", 5.41E-10,  "Q", "1145")
    CALL CalcH("5-methylchrysene",                                        "3697-24-3", 1.9E-6,    "Q", "1145")
    CALL CalcH("5-nitro-o-toluidine",                                       "99-55-8", 8.3E-9,    "Q", "1145")
    CALL CalcH("5-nitroacenaphthene",                                      "602-87-9", 1.1E-6,    "Q", "1145")
    CALL CalcH("5-nitrobenzimidazole",                                      "94-52-0", 3.7E-7,    "Q", "1145")
    CALL CalcH("6-bromobenzo(a)pyrene",                                  "21248-00-0", 8.48E-8,   "Q", "1145")
    CALL CalcH("6-methyl-3-heptanone",                                     "624-42-0", 2.7E-4,    "Q", "1145")
    CALL CalcH("6-nitrochrysene",                                         "7496-02-8", 1.5E-8,    "Q", "1145")
    CALL CalcH("7,12-dimethylbenz(a)anthracene",                            "57-97-6", 2.0E-6,    "Q", "1145")
    CALL CalcH("7-methylbenz(a)anthracene",                               "2541-69-7", 1.9E-6,    "Q", "1145")
    CALL CalcH("7H-dibenzo(c,g)carbazole",                                 "194-59-2", 2.45E-9,   "Q", "1145")
    CALL CalcH("8-methoxypsoralen",                                        "298-81-7", 4.0E-8,    "Q", "1145")
    CALL CalcH("9,10-dimethylanthracene",                                  "781-43-1", 2.9E-5,    "Q", "1145")
    CALL CalcH("acesulfame",                                             "33665-90-6", 9.6E-9,    "Q", "1145")
    CALL CalcH("acetamide",                                                 "60-35-5", 1.1E-8,    "Q", "1145")
    CALL CalcH("acetaminophen",                                            "103-90-2", 6.4E-13,   "Q", "1145")
    CALL CalcH("acetamiprid",                                           "135410-20-7", 6.9E-8,    "Q", "1145")
    CALL CalcH("acetanilide",                                              "103-84-4", 6.2E-9,    "Q", "1145")
    CALL CalcH("acetin",                                                 "26446-35-5", 4.1E-10,   "Q", "1145")
    CALL CalcH("acetoacetanilide",                                         "102-01-2", 4.2E-12,   "Q", "1145")
    CALL CalcH("acetoin",                                                  "513-86-0", 1E-5,      "Q", "1145")
    CALL CalcH("acetoxon",                                                "2425-25-4", 7.6E-10,   "Q", "1145")
    CALL CalcH("acetyl ketene",                                            "674-82-8", 6.07E-4,   "Q", "1145")
    CALL CalcH("acetyl tributyl citrate",                                   "77-90-7", 3.8E-10,   "Q", "1145")
    CALL CalcH("propoxycarbazone",                                      "145026-81-9", 1.4E-17,   "Q", "1145")
    CALL CalcH("sodium dodecylbenzenesulfonate",                         "25155-30-0", 6.27E-8,   "Q", "1145")
    CALL CalcH("acridine",                                                 "260-94-6", 4.0E-7,    "Q", "1145")
    CALL CalcH("acyclovir",                                              "59277-89-3", 3.2E-22,   "Q", "1145")
    CALL CalcH("adamsite",                                                 "578-94-9", 3.3E-8,    "Q", "1145")
    CALL CalcH("aflatoxin B1",                                            "1162-65-8", 1.4E-13,   "Q", "1145")
    CALL CalcH("aflatoxin B2",                                            "7220-81-7", 3.0E-15,   "Q", "1145")
    CALL CalcH("aflatoxin G1",                                            "1165-39-5", 5.0E-13,   "Q", "1145")
    CALL CalcH("aflatoxin G2",                                            "7241-98-7", 1.1E-14,   "Q", "1145")
    CALL CalcH("albendazole",                                            "54965-21-8", 7.6E-14,   "Q", "1145")
    CALL CalcH("albuterol",                                              "18559-94-9", 6.4E-16,   "Q", "1145")
    CALL CalcH("alfuzosin",                                              "81403-80-7", 9.47E-20,  "Q", "1145")
    CALL CalcH("allantoin",                                                 "97-59-6", 3.4E-18,   "Q", "1145")
    CALL CalcH("allopurinol",                                              "315-30-0", 2.0E-14,   "Q", "1145")
    CALL CalcH("allyl bromide",                                            "106-95-6", 1.1E-2,    "Q", "1145")
    CALL CalcH("allyl methacrylate",                                        "96-05-9", 4.1E-4,    "Q", "1145")
    CALL CalcH("allyl propyl disulfide",                                  "2179-59-1", 2.8E-3,    "Q", "1145")
    CALL CalcH("allyl sulfide",                                            "592-88-1", 1.3E-3,    "Q", "1145")
    CALL CalcH("alpha,beta-thujone",                                     "76231-76-0", 1.6E-5,    "Q", "1145")
    CALL CalcH("alpha-methylbenzyl alcohol",                                "98-85-1", 2.9E-7,    "Q", "1145")
    CALL CalcH("alpha-methylstyrene dimer",                               "6144-04-3", 9.2E-4,    "Q", "1145")
    CALL CalcH("alpha-naphthylthiourea",                                    "86-88-4", 8.5E-9,    "Q", "1145")
    CALL CalcH("aminocarb",                                               "2032-59-9", 5.636E-10, "Q", "1145")
    CALL CalcH("aminoethyl ethanolamine",                                  "111-41-1", 1.10E-13,  "Q", "1145")
    CALL CalcH("aminopyralid",                                          "150114-71-9", 1.7E-12,   "Q", "1145")
    CALL CalcH("amitryptyline",                                            "549-18-8", 7E-8,      "Q", "1145")
    CALL CalcH("anagrelide",                                             "68475-42-3", 2.7E-13,   "Q", "1145")
    CALL CalcH("anatoxin A",                                             "64285-06-9", 6.6E-9,    "Q", "1145")
    CALL CalcH("androstenedione",                                           "63-05-8", 3.7E-8,    "Q", "1145")
    CALL CalcH("anethole",                                                 "104-46-1", 7.18E-5,   "Q", "1145")
    CALL CalcH("anisole",                                                  "100-66-3", 4.35E-3,   "Q", "1145")
    CALL CalcH("annatto",                                                 "1393-63-1", 6.4E-17,   "Q", "1145")
    CALL CalcH("anthranilic acid",                                         "118-92-3", 3.8E-11,   "Q", "1145")
    CALL CalcH("anthrone",                                                  "90-44-8", 7.9E-7,    "Q", "1145")
    CALL CalcH("aramite",                                                  "140-57-8", 1.9E-7,    "Q", "1145")
    CALL CalcH("arbutin",                                                  "497-76-7", 1.2E-19,   "Q", "1145")
    CALL CalcH("aripiprazole",                                          "129722-12-9", 1E-17,     "Q", "1145")
    CALL CalcH("ascorbyl palmitate",                                       "137-66-6", 1.4E-7,    "Q", "1145")
    CALL CalcH("aspartame",                                              "22839-47-0", 2.5E-18,   "Q", "1145")
    CALL CalcH("atazanavir",                                            "198904-31-3", 3.6E-32,   "Q", "1145")
    CALL CalcH("atractylenolide III",                                    "73030-71-4", 9.5E-9,    "Q", "1145")
    CALL CalcH("auramine hydrochloride",                                  "2465-27-2", 2.8E-16,   "Q", "1145")
    CALL CalcH("bantrol",                                                 "1689-83-4", 5.5E-10,   "Q", "1145")
    CALL CalcH("bendamustine",                                           "16506-27-7", 3.9E-13,   "Q", "1145")
    CALL CalcH("benzanthrone",                                              "82-05-3", 6.61E-8,   "Q", "1145")
    CALL CalcH("benzenesulfonic acid",                                      "98-11-3", 2.52E-9,   "Q", "1145")
    CALL CalcH("benzidine",                                                 "92-87-5", 5.2E-11,   "Q", "1145")
    CALL CalcH("benzimidazole",                                             "51-17-2", 3.7E-7,    "Q", "1145")
    CALL CalcH("benzo(c)chrysene",                                         "194-69-4", 1.23E-7,   "Q", "1145")
    CALL CalcH("benzo(g)chrysene",                                         "196-78-1", 1.23E-7,   "Q", "1145")
    CALL CalcH("benzo(j)fluoranthene",                                     "205-82-3", 2.0E-7,    "Q", "1145")
    CALL CalcH("benzocaine",                                                "94-09-7", 1.6E-8,    "Q", "1145")
    CALL CalcH("benzofuran",                                               "271-89-6", 5.3E-4,    "Q", "1145")
    CALL CalcH("benzophenone",                                             "119-61-9", 1.9E-6,    "Q", "1145")
    CALL CalcH("benzothiazole",                                             "95-16-9", 3.7E-7,    "Q", "1145")
    CALL CalcH("benzotrichloride",                                          "98-07-7", 2.6E-4,    "Q", "1145")
    CALL CalcH("benzoyl peroxide",                                          "94-36-0", 3.5E-6,    "Q", "1145")
    CALL CalcH("benzphetamine",                                            "156-08-1", 4.2E-7,    "Q", "1145")
    CALL CalcH("benztropine",                                               "86-13-5", 2.2E-9,    "Q", "1145")
    CALL CalcH("benzyl bromide",                                           "100-39-0", 6.9E-3,    "Q", "1145")
    CALL CalcH("benzyl cinnamate",                                         "103-41-3", 3.3E-7,    "Q", "1145")
    CALL CalcH("benzyl mercaptan",                                         "100-53-8", 2.1E-4,    "Q", "1145")
    CALL CalcH("benzyl sulfide",                                           "538-74-9", 5.1E-6,    "Q", "1145")
    CALL CalcH("benzyldimethylstearylammonium chloride",                   "122-19-0", 4.2E-11,   "Q", "1145")
    CALL CalcH("benzylsulfonic acid",                                      "100-87-8", 1.0E-9,    "Q", "1145")
    CALL CalcH("beta,beta'-iminodipropionitrile",                          "111-94-4", 5.0E-12,   "Q", "1145")
    CALL CalcH("beta-bromostyrene",                                        "103-64-0", 5.5E-4,    "Q", "1145")
    CALL CalcH("beta-methyl styrene",                                      "637-50-3", 2.7E-3,    "Q", "1145")
    CALL CalcH("beta-pinene",                                              "127-91-3", 0.16,      "Q", "1145")
    CALL CalcH("bis(1,1-dimethylethyl)peroxide",                           "110-05-4", 0.012,     "Q", "1145")
    CALL CalcH("bis(2,4-dichlorophenyl)ether",                           "28076-73-5", 3.5E-5,    "Q", "1145")
    CALL CalcH("bis(2-(2-butoxyethoxy)ethyl) adipate",                     "141-17-3", 3.1E-13,   "Q", "1145")
    CALL CalcH("bis(2-butoxyethyl) phthalate",                             "117-83-9", 2.0E-12,   "Q", "1145")
    CALL CalcH("bis(2-ethylhexyl) hydrogen phosphite",                    "3658-48-8", 1.7E-4,    "Q", "1145")
    CALL CalcH("bis(2-ethylhexyl) phosphate",                              "298-07-7", 4.1E-8,    "Q", "1145")
    CALL CalcH("bis(2-ethylhexyl) sebacate",                               "122-62-3", 8.5E-5,    "Q", "1145")
    CALL CalcH("bis(2-ethylhexyl) sodium sulfosuccinate",                  "577-11-7", 5E-12,     "Q", "1145")
    CALL CalcH("bis(2-ethylhexyl) terephthalate",                         "6422-86-2", 1.0E-5,    "Q", "1145")
    CALL CalcH("bis(2-hydroxy-3-tert-butyl-5-methylphenyl)methane",        "119-47-1", 7.9E-12,   "Q", "1145")
    CALL CalcH("bis(2-methoxyethyl) phthalate",                            "117-82-8", 2.8E-13,   "Q", "1145")
    CALL CalcH("bis(4-aminophenyl) ether",                                 "101-80-4", 1.5E-11,   "Q", "1145")
    CALL CalcH("bis(4-aminophenyl) sulfide",                               "139-65-1", 3.9E-12,   "Q", "1145")
    CALL CalcH("bis(dimethylthiocarbamoyl) sulfide",                        "97-74-5", 1.7E-5,    "Q", "1145")
    CALL CalcH("bis(p-dimethylamino)phenylmethane",                        "101-61-1", 1.2E-7,    "Q", "1145")
    CALL CalcH("bis(trichloromethyl) sulfone",                            "3064-70-8", 1.2E-8,    "Q", "1145")
    CALL CalcH("bisacodyl",                                                "603-50-9", 7.3E-12,   "Q", "1145")
    CALL CalcH("bisphenol A diglycidyl ether",                            "1675-54-3", 4.4E-11,   "Q", "1145")
    CALL CalcH("bromethalin",                                            "63333-35-7", 4.0E-9,    "Q", "1145")
    CALL CalcH("bromoacetone",                                             "598-31-2", 5.7E-6,    "Q", "1145")
    CALL CalcH("bromochloroacetonitrile",                                "83463-62-1", 1.2E-6,    "Q", "1145")
    CALL CalcH("bromochlorodifluoromethane",                               "353-59-3", 9.4E-2,    "Q", "1145")
    CALL CalcH("bromocresol purple",                                       "115-40-2", 1.0E-18,   "Q", "1145")
    CALL CalcH("bromophos-ethyl",                                         "4824-78-6", 1.6E-5,    "Q", "1145")
    CALL CalcH("bromotrichloromethane",                                     "75-62-7", 3.7E-4,    "Q", "1145")
    CALL CalcH("brucine",                                                  "357-57-3", 2.1E-16,   "Q", "1145")
    CALL CalcH("butalbital",                                                "77-26-9", 6.3E-13,   "Q", "1145")
    CALL CalcH("butonate",                                                 "126-22-7", 3.0E-10,   "Q", "1145")
    CALL CalcH("butyl cyclohexyl phthalate",                                "84-64-0", 9.5E-7,    "Q", "1145")
    CALL CalcH("butyl glycolyl butyl phthalate",                            "85-70-1", 2.1E-8,    "Q", "1145")
    CALL CalcH("butylated hydroxyanisole",                               "25013-16-5", 1.17E-6,   "Q", "1145")
    CALL CalcH("butylene chlorohydrin",                                   "1320-66-7", 2.2E-6,    "Q", "1145")
    CALL CalcH("butylparaben",                                              "94-26-8", 8.5E-9,    "Q", "1145")
    CALL CalcH("C.I. Food Yellow 10",                                       "85-84-7", 5.1E-10,   "Q", "1145")
    CALL CalcH("C.I. Pigment Orange 5",                                   "3468-63-1", 9.1E-15,   "Q", "1145")
    CALL CalcH("C.I. Pigment Red 3",                                      "2425-85-6", 1.2E-12,   "Q", "1145")
    CALL CalcH("C.I. Pigment Red 4",                                      "2814-77-9", 9.3E-13,   "Q", "1145")
    CALL CalcH("caffeic acid",                                             "331-39-5", 1.4E-16,   "Q", "1145")
    CALL CalcH("calusterone",                                            "17021-26-0", 6.2E-9,    "Q", "1145")
    CALL CalcH("caprolactone",                                             "502-44-3", 1.809E-4,  "Q", "1145")
    CALL CalcH("capsaicin",                                                "404-86-4", 1.0E-13,   "Q", "1145")
    CALL CalcH("capsanthin",                                               "465-42-9", 2.9E-8,    "Q", "1145")
    CALL CalcH("carbadox",                                                "6804-07-5", 4.5E-23,   "Q", "1145")
    CALL CalcH("carbamazepine",                                            "298-46-4", 1.1E-10,   "Q", "1145")
    CALL CalcH("carbazole",                                                 "86-74-8", 8.65E-8,   "Q", "1145")
    CALL CalcH("carisoprodol",                                              "78-44-4", 7.14E-10,  "Q", "1145")
    CALL CalcH("carmustine",                                               "154-93-8", 4.8E-11,   "Q", "1145")
    CALL CalcH("carvone",                                                   "99-49-0", 7.7E-5,    "Q", "1145")
    CALL CalcH("celecoxib",                                             "169590-42-5", 7.8E-13,   "Q", "1145")
    CALL CalcH("cerivastatin",                                          "145599-86-6", 5.8E-19,   "Q", "1145")
    CALL CalcH("chlorambucil",                                             "305-03-3", 2.7E-10,   "Q", "1145")
    CALL CalcH("chloramphenicol",                                           "56-75-7", 2.3E-18,   "Q", "1145")
    CALL CalcH("chlorantraniliprole",                                   "500008-45-7", 1.4E-21,   "Q", "1145")
    CALL CalcH("chlordene",                                               "3734-48-3", 5.0E-4,    "Q", "1145")
    CALL CalcH("chlorendic acid",                                          "115-28-6", 3.0E-14,   "Q", "1145")
    CALL CalcH("chlorethoxyfos",                                         "54593-83-8", 4.2E-6,    "Q", "1145")
    CALL CalcH("chlorfenapyr",                                          "122453-73-0", 5.7E-9,    "Q", "1145")
    CALL CalcH("chlorfenson",                                               "80-33-1", 1.6E-7,    "Q", "1145")
    CALL CalcH("chlorhexidine",                                             "55-56-1", 1.1E-30,   "Q", "1145")
    CALL CalcH("chlormadinone acetate",                                    "302-22-7", 5.6E-10,   "Q", "1145")
    CALL CalcH("chloroacetaldehyde",                                       "107-20-0", 2.4E-5,    "Q", "1145")
    CALL CalcH("chloroacetamide",                                           "79-07-2", 3.94E-9,   "Q", "1145")
    CALL CalcH("chloroacetic anhydride",                                   "541-88-8", 4.42E-6,   "Q", "1145")
    CALL CalcH("chloroacetyl chloride",                                     "79-04-9", 2.3E-4,    "Q", "1145")
    CALL CalcH("chloroethyl chloroformate",                                "627-11-2", 1.1E-3,    "Q", "1145")
    CALL CalcH("chlorotrifluoroethylene",                                   "79-38-9", 0.31,      "Q", "1145")
    CALL CalcH("chloroxylenol",                                             "88-04-0", 5.1E-7,    "Q", "1145")
    CALL CalcH("chlorthiophos",                                          "21923-23-9", 1.2E-6,    "Q", "1145")
    CALL CalcH("cholecalciferol",                                           "67-97-0", 2.3E-4,    "Q", "1145")
    CALL CalcH("cholesterol",                                               "57-88-5", 1.7E-4,    "Q", "1145")
    CALL CalcH("cimetidine",                                             "51481-61-9", 9.5E-16,   "Q", "1145")
    CALL CalcH("cinacalcet",                                            "226256-56-0", 2.2E-7,    "Q", "1145")
    CALL CalcH("cinerin I",                                              "25402-06-6", 9.6E-7,    "Q", "1145")
    CALL CalcH("cinerin II",                                               "121-20-0", 9.2E-10,   "Q", "1145")
    CALL CalcH("cis-2-pentene",                                            "627-20-3", 0.22,      "Q", "1145")
    CALL CalcH("citral",                                                  "5392-40-5", 4.35E-5,   "Q", "1145")
    CALL CalcH("citronellal",                                              "106-23-0", 2.62E-4,   "Q", "1145")
    CALL CalcH("citrus red 2",                                            "6358-53-8", 5.2E-13,   "Q", "1145")
    CALL CalcH("clethodim",                                              "99129-21-2", 1.2E-11,   "Q", "1145")
    CALL CalcH("clindamycin",                                            "18323-44-9", 2.9E-22,   "Q", "1145")
    CALL CalcH("clobetasol",                                             "25122-41-2", 1.6E-10,   "Q", "1145")
    CALL CalcH("clonazepam",                                              "1622-61-3", 7.0E-13,   "Q", "1145")
    CALL CalcH("clonidine",                                               "4205-90-7", 1.5E-11,   "Q", "1145")
    CALL CalcH("clopidogrel",                                           "113665-84-2", 2.2E-9,    "Q", "1145")
    CALL CalcH("clopidol",                                                "2971-90-6", 1.0E-9,    "Q", "1145")
    CALL CalcH("colchicine",                                                "64-86-8", 1.8E-17,   "Q", "1145")
    CALL CalcH("cotinine",                                                 "486-56-6", 3.3E-12,   "Q", "1145")
    CALL CalcH("cresyl glycidyl ether",                                  "26447-14-3", 7.6E-7,    "Q", "1145")
    CALL CalcH("crimidine",                                                "535-89-7", 3.8E-8,    "Q", "1145")
    CALL CalcH("curcumin",                                                 "458-37-7", 7.0E-22,   "Q", "1145")
    CALL CalcH("cyanoguanidine",                                           "461-58-5", 2.3E-10,   "Q", "1145")
    CALL CalcH("cyanuric acid",                                            "108-80-5", 8.7E-15,   "Q", "1145")
    CALL CalcH("cyanuric chloride",                                        "108-77-0", 4.9E-7,    "Q", "1145")
    CALL CalcH("cyclobutane",                                              "287-23-0", 0.14,      "Q", "1145")
    CALL CalcH("cyclododecane",                                            "294-62-2", 1.54,      "Q", "1145")
    CALL CalcH("cycloheptane",                                             "291-64-5", 0.4,       "Q", "1145")
    CALL CalcH("cycloheximide",                                             "66-81-9", 3.5E-15,   "Q", "1145")
    CALL CalcH("cyclohexyl acetate",                                       "622-45-7", 1.2E-4,    "Q", "1145")
    CALL CalcH("cyclohexylisocyanate",                                    "3173-53-3", 1.7E-3,    "Q", "1145")
    CALL CalcH("cyclopentane",                                             "287-92-3", 0.19,      "Q", "1145")
    CALL CalcH("cyclopentanol",                                             "96-41-3", 2.6E-6,    "Q", "1145")
    CALL CalcH("cyclophosphamide",                                          "50-18-0", 1.4E-11,   "Q", "1145")
    CALL CalcH("cyclotetramethylenetetranitramine",                       "2691-41-0", 8.7E-10,   "Q", "1145")
    CALL CalcH("cycluron",                                                "2163-69-1", 1.2E-8,    "Q", "1145")
    CALL CalcH("cyhalothrin",                                            "68085-85-8", 1.4E-5,    "Q", "1145")
    CALL CalcH("cyprazine",                                              "22936-86-3", 2.6E-9,    "Q", "1145")
    CALL CalcH("cypromid",                                                "2759-71-9", 2.6E-9,    "Q", "1145")
    CALL CalcH("cysteamine",                                                "60-23-1", 3.6E-7,    "Q", "1145")
    CALL CalcH("D&C Black No. 1",                                         "1064-48-8", 1.2E-31,   "Q", "1145")
    CALL CalcH("D&C Green No. 6",                                          "128-80-3", 1.5E-16,   "Q", "1145")
    CALL CalcH("D&C Yellow No. 10",                                       "8004-92-0", 2.9E-20,   "Q", "1145")
    CALL CalcH("D&C Yellow No. 11",                                       "8003-22-3", 6.1E-14,   "Q", "1145")
    CALL CalcH("D-mannitol",                                                "69-65-8", 7.3E-13,   "Q", "1145")
    CALL CalcH("D-sorbitol",                                                "50-70-4", 7.3E-13,   "Q", "1145")
    CALL CalcH("D-xylose",                                                  "58-86-6", 1.2E-9,    "Q", "1145")
    CALL CalcH("dalfopristin",                                          "112362-50-2", 4.5E-30,   "Q", "1145")
    CALL CalcH("decabromobiphenyl ether",                                 "1163-19-5", 1.2E-8,    "Q", "1145")
    CALL CalcH("decabromobiphenyl",                                      "13654-09-6", 4.2E-8,    "Q", "1145")
    CALL CalcH("dechlorane plus",                                        "13560-89-9", 7.4E-6,    "Q", "1145")
    CALL CalcH("DEET",                                                     "134-62-3", 2.1E-8,    "Q", "1145")
    CALL CalcH("delta 9-tetrahydrocannabinol",                            "1972-08-3", 2.4E-7,    "Q", "1145")
    CALL CalcH("desethyl atrazine",                                       "6190-65-4", 1.5E-9,    "Q", "1145")
    CALL CalcH("desflurane",                                             "57041-67-5", 7.1E-2,    "Q", "1145")
    CALL CalcH("desomorphine",                                             "427-00-9", 4.1E-12,   "Q", "1145")
    CALL CalcH("desoxypipradrol",                                          "519-74-4", 1.5E-7,    "Q", "1145")
    CALL CalcH("dessin",                                                   "973-21-7", 1.6E-8,    "Q", "1145")
    CALL CalcH("dexamethasone",                                             "50-02-2", 7.15E-8,   "Q", "1145")
    CALL CalcH("dexrazoxane",                                            "24584-09-6", 2.1E-19,   "Q", "1145")
    CALL CalcH("di-2-ethylhexyl azelate",                                  "103-24-2", 1.2E-4,    "Q", "1145")
    CALL CalcH("di-n-dodecyl phosphate",                                  "7057-92-3", 4.0E-7,    "Q", "1145")
    CALL CalcH("diallyl phthalate",                                        "131-17-9", 3.9E-7,    "Q", "1145")
    CALL CalcH("diamyl phthalate",                                         "131-18-0", 8.9E-7,    "Q", "1145")
    CALL CalcH("diazepam",                                                 "439-14-5", 3.6E-9,    "Q", "1145")
    CALL CalcH("dibenz(a,h)acridine",                                      "226-36-8", 1.9E-9,    "Q", "1145")
    CALL CalcH("dibenz(a,h)anthracene",                                     "53-70-3", 7.3E-8,    "Q", "1145")
    CALL CalcH("dibenz(a,j)acridine",                                      "224-42-0", 1.9E-9,    "Q", "1145")
    CALL CalcH("dibenz(b,f)(1,4)oxazepine",                                "257-07-8", 4.1E-3,    "Q", "1145")
    CALL CalcH("dibenzo(a,e)fluoranthene",                                "5385-75-1", 1.4E-8,    "Q", "1145")
    CALL CalcH("dibenzo(a,e)pyrene",                                       "192-65-4", 1.41E-8,   "Q", "1145")
    CALL CalcH("dibenzo(a,h)pyrene",                                       "189-64-0", 1.4E-8,    "Q", "1145")
    CALL CalcH("dibenzo(a,i)pyrene",                                       "189-55-9", 1.4E-8,    "Q", "1145")
    CALL CalcH("dibenzo(b,k)chrysene",                                     "217-54-9", 8.48E-9,   "Q", "1145")
    CALL CalcH("dibenzyl ether",                                           "103-50-4", 8.3E-8,    "Q", "1145")
    CALL CalcH("dibromoacetonitrile",                                     "3252-43-5", 4.1E-7,    "Q", "1145")
    CALL CalcH("dibromodifluoromethane",                                    "75-61-6", 0.03,      "Q", "1145")
    CALL CalcH("dibutyl azelate",                                         "2917-73-9", 1.2E-5,    "Q", "1145")
    CALL CalcH("dibutyl hydrogen phosphite",                              "1809-19-4", 1.8E-5,    "Q", "1145")
    CALL CalcH("dibutyl phenyl phosphate",                                "2528-36-1", 5.04E-7,   "Q", "1145")
    CALL CalcH("dibutyl phosphate",                                        "107-66-4", 4.3E-9,    "Q", "1145")
    CALL CalcH("dichlofluanid",                                           "1085-98-9", 6.7E-7,    "Q", "1145")
    CALL CalcH("dichlone",                                                 "117-80-6", 1.02E-9,   "Q", "1145")
    CALL CalcH("dichlorfop-methyl",                                      "51338-27-3", 3.8E-8,    "Q", "1145")
    CALL CalcH("dichloroacetaldehyde",                                      "79-02-7", 8.42E-6,   "Q", "1145")
    CALL CalcH("dichloroacetonitrile",                                    "3018-12-0", 3.8E-6,    "Q", "1145")
    CALL CalcH("dichloroacetylene",                                       "7572-29-4", 2.0E-2,    "Q", "1145")
    CALL CalcH("dicumarol",                                                 "66-76-2", 1.4E-13,   "Q", "1145")
    CALL CalcH("dicumyl peroxide",                                          "80-43-3", 4.42E-5,   "Q", "1145")
    CALL CalcH("dicyclohexyl disulfide",                                  "2550-40-5", 4.0E-3,    "Q", "1145")
    CALL CalcH("dicyclohexylamine",                                        "101-83-7", 5.5E-5,    "Q", "1145")
    CALL CalcH("dicyclopentadiene",                                         "77-73-6", 6.25E-2,   "Q", "1145")
    CALL CalcH("didecyl phthalate",                                         "84-77-5", 2.8E-5,    "Q", "1145")
    CALL CalcH("diethyl fumarate",                                         "623-91-6", 2.4E-8,    "Q", "1145")
    CALL CalcH("diethyl hydrogen phosphite",                               "762-04-9", 5.8E-6,    "Q", "1145")
    CALL CalcH("diethylarsine",                                            "692-42-2", 4.5E-1,    "Q", "1145")
    CALL CalcH("diethyldimethyl lead",                                    "1762-27-2", 0.47,      "Q", "1145")
    CALL CalcH("diethylene glycol bis(methacrylate)",                     "2358-84-1", 7.9E-10,   "Q", "1145")
    CALL CalcH("diethylene glycol diacrylate",                            "4074-88-8", 9.5E-10,   "Q", "1145")
    CALL CalcH("diethylene glycol dibenzoate",                             "120-55-8", 3E-12,     "Q", "1145")
    CALL CalcH("diethylene glycol monomethyl ether",                       "111-77-3", 1.6E-11,   "Q", "1145")
    CALL CalcH("diethylene glycol",                                        "111-46-6", 2.0E-9,    "Q", "1145")
    CALL CalcH("diethylenetriamine",                                       "111-40-0", 1E-14,     "Q", "1145")
    CALL CalcH("diethylpropion",                                            "90-84-6", 2.3E-7,    "Q", "1145")
    CALL CalcH("diethylstilbestrol",                                        "56-53-1", 5.8E-8,    "Q", "1145")
    CALL CalcH("digitoxin",                                                 "71-63-6", 1.3E-25,   "Q", "1145")
    CALL CalcH("diglycidyl ether",                                        "2238-07-5", 1.3E-8,    "Q", "1145")
    CALL CalcH("digoxigenin",                                             "1672-46-4", 2.3E-11,   "Q", "1145")
    CALL CalcH("digoxin",                                                "20830-75-5", 4.7E-27,   "Q", "1145")
    CALL CalcH("diheptyl phthalate",                                      "3648-21-3", 3.5E-6,    "Q", "1145")
    CALL CalcH("dihydropinene",                                            "473-55-2", 0.35,      "Q", "1145")
    CALL CalcH("dihydrosafrole",                                            "94-58-6", 1.2E-5,    "Q", "1145")
    CALL CalcH("dihydrotachysterol",                                        "67-96-9", 3.7E-4,    "Q", "1145")
    CALL CalcH("dihydrotanshinone I",                                    "87205-99-0", 1.3E-10,   "Q", "1145")
    CALL CalcH("diiodomethyl p-tolyl sulfone",                           "20018-09-1", 7.7E-9,    "Q", "1145")
    CALL CalcH("diisononyl hexahydrophthalate",                         "166412-78-8", 7.1E-5,    "Q", "1145")
    CALL CalcH("diisooctyl adipate",                                      "1330-86-5", 5.2E-5,    "Q", "1145")
    CALL CalcH("diisopropyl fluorophosphate",                               "55-91-4", 3.2E-6,    "Q", "1145")
    CALL CalcH("diisopropylbenzene",                                     "25321-09-9", 0.0204,    "Q", "1145")
    CALL CalcH("diisopropylbiphenyl",                                    "36876-13-8", 2.2E-3,    "Q", "1145")
    CALL CalcH("dikegulac acid",                                         "18467-77-1", 1.90E-16,  "Q", "1145")
    CALL CalcH("dimethomorph",                                          "110488-70-5", 1.0E-15,   "Q", "1145")
    CALL CalcH("dimethoxane",                                              "828-00-2", 1.24E-7,   "Q", "1145")
    CALL CalcH("dimethrin",                                                 "70-38-2", 7.6E-5,    "Q", "1145")
    CALL CalcH("dimethyl adipate",                                         "627-93-0", 9.77E-7,   "Q", "1145")
    CALL CalcH("dimethyl carbonate",                                       "616-38-6", 6.2E-4,    "Q", "1145")
    CALL CalcH("dimethyl dicarbonate",                                    "4525-33-1", 4.5E-4,    "Q", "1145")
    CALL CalcH("dimethyl fumarate",                                        "624-49-7", 7.11E-7,   "Q", "1145")
    CALL CalcH("dimethyl isophthalate",                                   "1459-93-4", 6.1E-8,    "Q", "1145")
    CALL CalcH("dimethyl methylphosphonate",                               "756-79-6", 1.3E-6,    "Q", "1145")
    CALL CalcH("dimethyl succinate",                                       "106-65-0", 6.4E-8,    "Q", "1145")
    CALL CalcH("dimethyldioctadecylammonium chloride",                     "107-64-2", 6.4E-8,    "Q", "1145")
    CALL CalcH("dimetilan",                                                "644-64-4", 4.1E-11,   "Q", "1145")
    CALL CalcH("dinonyl phthalate",                                         "84-76-4", 1.4E-5,    "Q", "1145")
    CALL CalcH("diphenyl carbonate",                                       "102-09-0", 8.5E-5,    "Q", "1145")
    CALL CalcH("diphenyl octyl phosphate",                                 "115-88-8", 2.5E-7,    "Q", "1145")
    CALL CalcH("diquat dibromide",                                          "85-00-7", 1.4E-13,   "Q", "1145")
    CALL CalcH("disulfiram",                                                "97-77-8", 8.3E-5,    "Q", "1145")
    CALL CalcH("ditridecyl phthalate",                                     "119-06-2", 2.2E-4,    "Q", "1145")
    CALL CalcH("diundecyl phthalate",                                     "3648-20-2", 5.6E-5,    "Q", "1145")
    CALL CalcH("dodecyl sulfate",                                          "151-41-7", 1.8E-7,    "Q", "1145")
    CALL CalcH("dodecylamine",                                             "124-22-1", 2.7E-4,    "Q", "1145")
    CALL CalcH("dodecylbenzene",                                           "123-01-3", 0.13,      "Q", "1145")
    CALL CalcH("donepezil",                                             "120014-06-4", 1.2E-12,   "Q", "1145")
    CALL CalcH("dromostanolone",                                            "58-19-5", 8.5E-9,    "Q", "1145")
    CALL CalcH("droperidol",                                               "548-73-2", 2.7E-17,   "Q", "1145")
    CALL CalcH("efavirenz",                                             "154598-52-4", 7.0E-9,    "Q", "1145")
    CALL CalcH("emtricitabine",                                         "143491-57-0", 1.1E-17,   "Q", "1145")
    CALL CalcH("endothion",                                               "2778-04-3", 6.5E-12,   "Q", "1145")
    CALL CalcH("entecavir",                                             "142217-69-4", 1.6E-21,   "Q", "1145")
    CALL CalcH("epibromohydrin",                                          "3132-64-7", 2.4E-6,    "Q", "1145")
    CALL CalcH("epinephrine",                                               "51-43-4", 7.1E-19,   "Q", "1145")
    CALL CalcH("ergosterol",                                                "57-87-4", 1.6E-4,    "Q", "1145")
    CALL CalcH("erythorbic acid",                                           "89-65-6", 4.07E-8,   "Q", "1145")
    CALL CalcH("erythritol",                                               "149-32-6", 3.1E-10,   "Q", "1145")
    CALL CalcH("esomeprazole",                                          "119141-88-7", 3.0E-19,   "Q", "1145")
    CALL CalcH("estradiol",                                                 "50-28-2", 3.6E-11,   "Q", "1145")
    CALL CalcH("estriol",                                                   "50-27-1", 1.3E-12,   "Q", "1145")
    CALL CalcH("estrone",                                                   "53-16-7", 3.8E-10,   "Q", "1145")
    CALL CalcH("ethinylestradiol",                                          "57-63-6", 7.9E-12,   "Q", "1145")
    CALL CalcH("ethoate-methyl",                                           "116-01-8", 2.8E-11,   "Q", "1145")
    CALL CalcH("ethyl 2-bromoacetate",                                     "105-36-2", 2.70E-5,   "Q", "1145")
    CALL CalcH("ethyl anthranilate",                                        "87-25-2", 1.6E-8,    "Q", "1145")
    CALL CalcH("ethyl chloroacetate",                                      "105-39-5", 8.20E-5,   "Q", "1145")
    CALL CalcH("ethyl chloroformate",                                      "541-41-3", 3.1E-3,    "Q", "1145")
    CALL CalcH("ethyl methanesulfonate",                                    "62-50-0", 5.4E-6,    "Q", "1145")
    CALL CalcH("ethyl methyl ether",                                       "540-67-0", 6.7E-4,    "Q", "1145")
    CALL CalcH("ethyl N-phenylcarbamate",                                  "101-99-5", 2.9E-8,    "Q", "1145")
    CALL CalcH("ethyl nitrate",                                            "625-58-1", 3E-4,      "Q", "1145")
    CALL CalcH("ethyl nitrite",                                            "109-95-5", 8.7E-5,    "Q", "1145")
    CALL CalcH("ethyl thiocyanate",                                        "542-90-5", 5.8E-5,    "Q", "1145")
    CALL CalcH("ethyldiethanolamine",                                      "139-87-7", 1.1E-10,   "Q", "1145")
    CALL CalcH("ethylene bis(tetrabromophthalimide)",                    "32588-76-4", 3.6E-21,   "Q", "1145")
    CALL CalcH("ethylene carbonate",                                        "96-49-1", 2.74E-4,   "Q", "1145")
    CALL CalcH("ethylene fluorohydrin",                                    "371-62-0", 7.1E-6,    "Q", "1145")
    CALL CalcH("ethylene glycol monopropyl ether",                        "2807-30-9", 1.5E-8,    "Q", "1145")
    CALL CalcH("ethylene glycol monovinyl ether",                          "764-48-7", 2.5E-7,    "Q", "1145")
    CALL CalcH("ethylene sulfide",                                         "420-12-2", 3.5E-4,    "Q", "1145")
    CALL CalcH("ethylene thiourea",                                         "96-45-7", 3.4E-7,    "Q", "1145")
    CALL CalcH("ethylestrenol",                                            "965-90-2", 2.3E-5,    "Q", "1145")
    CALL CalcH("ethylparaben",                                             "120-47-8", 4.8E-9,    "Q", "1145")
    CALL CalcH("ethyltrimethyl lead",                                     "1762-26-1", 0.35,      "Q", "1145")
    CALL CalcH("etoposide",                                              "33419-42-0", 1.7E-30,   "Q", "1145")
    CALL CalcH("ezetimibe",                                             "163222-33-1", 4.4E-18,   "Q", "1145")
    CALL CalcH("famciclovir",                                           "104227-87-4", 9.4E-14,   "Q", "1145")
    CALL CalcH("farnesol",                                                "4602-84-0", 2.5E-4,    "Q", "1145")
    CALL CalcH("FD&C Yellow No. 4",                                        "131-79-3", 5.6E-10,   "Q", "1145")
    CALL CalcH("fenfluramine",                                             "458-24-2", 2.7E-5,    "Q", "1145")
    CALL CalcH("fenofibrate",                                            "49562-28-9", 4.5E-9,    "Q", "1145")
    CALL CalcH("fensulfothion",                                            "115-90-2", 1.4E-10,   "Q", "1145")
    CALL CalcH("fentanyl",                                                 "437-38-7", 9.2E-12,   "Q", "1145")
    CALL CalcH("fenuron",                                                  "101-42-8", 9.71E-10,  "Q", "1145")
    CALL CalcH("flazasulfuron",                                         "104040-78-0", 6.1E-12,   "Q", "1145")
    CALL CalcH("flocoumafen",                                            "90035-08-8", 7.3E-13,   "Q", "1145")
    CALL CalcH("fluconazole",                                            "86386-73-4", 1.0E-13,   "Q", "1145")
    CALL CalcH("flufenoxuron",                                          "101463-69-8", 2.6E-12,   "Q", "1145")
    CALL CalcH("flumequine",                                             "42835-25-6", 2.7E-13,   "Q", "1145")
    CALL CalcH("flunitrazepam",                                           "1622-62-4", 2.3E-11,   "Q", "1145")
    CALL CalcH("fluorenone",                                               "486-25-9", 6.8E-7,    "Q", "1145")
    CALL CalcH("fluorescein",                                             "2321-07-5", 8.9E-17,   "Q", "1145")
    CALL CalcH("fluoroacetamide",                                          "640-19-7", 2.23E-8,   "Q", "1145")
    CALL CalcH("fluorouracil",                                              "51-21-8", 1.7E-10,   "Q", "1145")
    CALL CalcH("fluoxymesterone",                                           "76-43-7", 6.1E-10,   "Q", "1145")
    CALL CalcH("fluticasone",                                            "90566-53-3", 2.3E-9,    "Q", "1145")
    CALL CalcH("fluvalinate",                                            "69409-94-5", 1.5E-8,    "Q", "1145")
    CALL CalcH("formetanate hydrochloride",                              "23422-53-9", 2.3E-19,   "Q", "1145")
    CALL CalcH("formoterol",                                             "73573-87-2", 9.0E-23,   "Q", "1145")
    CALL CalcH("furazolidone",                                              "67-45-8", 3.3E-11,   "Q", "1145")
    CALL CalcH("furosemide",                                                "54-31-9", 3.9E-16,   "Q", "1145")
    CALL CalcH("fusarenon X",                                            "23255-69-8", 4.8E-17,   "Q", "1145")
    CALL CalcH("gallic acid",                                              "149-91-7", 8.5E-20,   "Q", "1145")
    CALL CalcH("ganciclovir",                                            "82410-32-0", 1.5E-23,   "Q", "1145")
    CALL CalcH("geraniol",                                                 "106-24-1", 5.9E-5,    "Q", "1145")
    CALL CalcH("geranyl acetate",                                          "105-87-3", 2.4E-3,    "Q", "1145")
    CALL CalcH("gibberellic acid",                                          "77-06-5", 1.6E-15,   "Q", "1145")
    CALL CalcH("glucosamine",                                             "3416-24-8", 7.7E-16,   "Q", "1145")
    CALL CalcH("glutaraldehyde",                                           "111-30-8", 2.4E-8,    "Q", "1145")
    CALL CalcH("glycidaldehyde",                                           "765-34-4", 5.1E-7,    "Q", "1145")
    CALL CalcH("glycidamide",                                             "5694-00-8", 1.29E-10,  "Q", "1145")
    CALL CalcH("glycidol",                                                 "556-52-5", 5.8E-9,    "Q", "1145")
    CALL CalcH("glycidyl methacrylate",                                    "106-91-2", 3.1E-7,    "Q", "1145")
    CALL CalcH("glycolonitrile",                                           "107-16-4", 7.4E-6,    "Q", "1145")
    CALL CalcH("glyoxylic acid",                                           "298-12-4", 3E-9,      "Q", "1145")
    CALL CalcH("gossypol",                                                 "303-45-7", 2.3E-28,   "Q", "1145")
    CALL CalcH("guaifenesin",                                               "93-14-1", 4.4E-11,   "Q", "1145")
    CALL CalcH("halofenozide",                                          "112226-61-6", 3.6E-11,   "Q", "1145")
    CALL CalcH("haloperidol",                                               "52-86-8", 2.3E-14,   "Q", "1145")
    CALL CalcH("heptabromodiphenyl ether",                               "68928-80-3", 1.9E-7,    "Q", "1145")
    CALL CalcH("hexabromobenzene",                                          "87-82-1", 2.8E-5,    "Q", "1145")
    CALL CalcH("hexabromodiphenyl ether",                                "36483-60-0", 4.7E-7,    "Q", "1145")
    CALL CalcH("hexachloronaphthalene",                                   "1335-87-1", 8.7E-5,    "Q", "1145")
    CALL CalcH("hexachlorophene",                                           "70-30-4", 5.5E-13,   "Q", "1145")
    CALL CalcH("hexachloropropene",                                       "1888-71-7", 1.6E-3,    "Q", "1145")
    CALL CalcH("hexaethyl tetraphosphate",                                 "757-58-4", 3.3E-17,   "Q", "1145")
    CALL CalcH("hexafluoropropene",                                        "116-15-4", 5.34,      "Q", "1145")
    CALL CalcH("hexahydrophthalic anhydride",                               "85-42-7", 2.1E-5,    "Q", "1145")
    CALL CalcH("hexamethyldisilazane",                                     "999-97-3", 8.7E-5,    "Q", "1145")
    CALL CalcH("hexamethylene diamine",                                    "124-09-4", 3.2E-9,    "Q", "1145")
    CALL CalcH("hexylresorcinol",                                          "136-77-6", 2.6E-10,   "Q", "1145")
    CALL CalcH("hydrocortisone",                                            "50-23-7", 5.8E-8,    "Q", "1145")
    CALL CalcH("hydroquinone dimethyl ether",                              "150-78-7", 3.5E-3,    "Q", "1145")
    CALL CalcH("hydroxylamine",                                           "7803-49-8", 6.9E-9,    "Q", "1145")
    CALL CalcH("hydroxypropyl acrylate",                                 "25584-83-2", 1.7E-9,    "Q", "1145")
    CALL CalcH("ibogaine",                                                  "83-74-9", 1.2E-11,   "Q", "1145")
    CALL CalcH("ifosfamide",                                              "3778-73-2", 1.4E-11,   "Q", "1145")
    CALL CalcH("imazamox",                                              "114311-32-9", 9.15E-19,  "Q", "1145")
    CALL CalcH("imazethapyr",                                            "81335-77-5", 1.0E-16,   "Q", "1145")
    CALL CalcH("imiquimod",                                              "99011-02-6", 8.3E-13,   "Q", "1145")
    CALL CalcH("indalone",                                                 "532-34-3", 4.7E-8,    "Q", "1145")
    CALL CalcH("indene",                                                    "95-13-6", 1.59E-3,   "Q", "1145")
    CALL CalcH("iodoacetic acid",                                           "64-69-7", 4.1E-8,    "Q", "1145")
    CALL CalcH("iodoform",                                                  "75-47-8", 3.1,       "Q", "1145")
    CALL CalcH("iopamidol",                                              "60166-93-0", 1.1E-25,   "Q", "1145")
    CALL CalcH("isobenzan",                                                "297-78-9", 5.9E-8,    "Q", "1145")
    CALL CalcH("isobornyl thiocyanoacetate",                               "115-31-1", 2.60E-7,   "Q", "1145")
    CALL CalcH("isobutyl stearate",                                        "646-13-9", 0.038,     "Q", "1145")
    CALL CalcH("isodecyl alcohol",                                       "25339-17-7", 5.5E-5,    "Q", "1145")
    CALL CalcH("isodecyl diphenyl phosphate",                            "29761-21-5", 4.36E-7,   "Q", "1145")
    CALL CalcH("isodecyl octyl phthalate",                                "1330-96-7", 2.1E-5,    "Q", "1145")
    CALL CalcH("isodrin",                                                  "465-73-6", 3.9E-4,    "Q", "1145")
    CALL CalcH("isolan",                                                   "119-38-0", 2.0E-9,    "Q", "1145")
    CALL CalcH("isophthalic acid",                                         "121-91-5", 2.2E-12,   "Q", "1145")
    CALL CalcH("isopropenyl acetate",                                      "108-22-5", 1.8E-3,    "Q", "1145")
    CALL CalcH("isopropyl carbamate",                                     "1746-77-6", 7.0E-8,    "Q", "1145")
    CALL CalcH("isopropyl chlorocarbonate",                                "108-23-6", 4.15E-3,   "Q", "1145")
    CALL CalcH("isopropyl mercaptan",                                       "75-33-2", 4.6E-3,    "Q", "1145")
    CALL CalcH("isopropyl methanesulfonate",                               "926-06-7", 7.1E-6,    "Q", "1145")
    CALL CalcH("isopropyl myristate",                                      "110-27-0", 2.34E-2,   "Q", "1145")
    CALL CalcH("isopropylphenyl diphenyl phosphate",                     "28108-99-8", 7.744E-8,  "Q", "1145")
    CALL CalcH("isosafrole",                                               "120-58-1", 0.036,     "Q", "1145")
    CALL CalcH("isoxathion",                                             "18854-01-8", 6.1E-8,    "Q", "1145")
    CALL CalcH("kadethrin",                                              "58769-20-3", 8.3E-10,   "Q", "1145")
    CALL CalcH("kinetin",                                                  "525-79-1", 1.2E-14,   "Q", "1145")
    CALL CalcH("kojic acid",                                               "501-30-4", 2.4E-7,    "Q", "1145")
    CALL CalcH("L-menthol",                                               "2216-51-5", 1.5E-5,    "Q", "1145")
    CALL CalcH("lactitol",                                                 "585-86-4", 8.4E-22,   "Q", "1145")
    CALL CalcH("lauramine oxide",                                         "1643-20-5", 6.6E-11,   "Q", "1145")
    CALL CalcH("leflunomide",                                            "75706-12-6", 1.23E-10,  "Q", "1145")
    CALL CalcH("leucomalachite green",                                     "129-73-7", 9.8E-9,    "Q", "1145")
    CALL CalcH("levonorgestrel",                                           "797-63-7", 7.7E-10,   "Q", "1145")
    CALL CalcH("linalyl acetate",                                          "115-95-7", 1.7E-3,    "Q", "1145")
    CALL CalcH("lincomycin",                                               "154-21-2", 3.0E-23,   "Q", "1145")
    CALL CalcH("lomustine",                                              "13010-47-4", 1.8E-10,   "Q", "1145")
    CALL CalcH("lopinavir",                                             "192725-17-0", 4.3E-28,   "Q", "1145")
    CALL CalcH("LSD",                                                       "50-37-3", 1.5E-16,   "Q", "1145")
    CALL CalcH("m-anisidine",                                              "536-90-3", 1.1E-7,    "Q", "1145")
    CALL CalcH("m-chlorostyrene",                                         "2039-85-2", 2.1E-3,    "Q", "1145")
    CALL CalcH("m-cresidine",                                              "102-50-1", 1.2E-7,    "Q", "1145")
    CALL CalcH("m-xylyl bromide",                                          "620-13-3", 7.6E-4,    "Q", "1145")
    CALL CalcH("malachite green",                                          "569-64-2", 1.9E-14,   "Q", "1145")
    CALL CalcH("malaoxon",                                                "1634-78-2", 1.8E-12,   "Q", "1145")
    CALL CalcH("malic acid",                                              "6915-15-7", 8.4E-13,   "Q", "1145")
    CALL CalcH("malononitrile",                                            "109-77-3", 1.27E-8,   "Q", "1145")
    CALL CalcH("maltitol",                                                 "585-88-6", 4.2E-21,   "Q", "1145")
    CALL CalcH("mebendazole",                                            "31431-39-7", 5.4E-16,   "Q", "1145")
    CALL CalcH("mecarbam",                                                "2595-54-2", 9.0E-10,   "Q", "1145")
    CALL CalcH("medroxyprogesterone",                                      "520-85-4", 1.3E-8,    "Q", "1145")
    CALL CalcH("mefluidide",                                             "53780-34-0", 1.3E-11,   "Q", "1145")
    CALL CalcH("melphalan",                                                "148-82-3", 4.2E-13,   "Q", "1145")
    CALL CalcH("menadione",                                                 "58-27-5", 3.1E-9,    "Q", "1145")
    CALL CalcH("menthone",                                                  "89-80-5", 1.6E-4,    "Q", "1145")
    CALL CalcH("menthyl acetate",                                        "16409-45-3", 8.3E-4,    "Q", "1145")
    CALL CalcH("meprobamate",                                               "57-53-4", 1.8E-10,   "Q", "1145")
    CALL CalcH("mercaptoacetic acid",                                       "68-11-1", 1.9E-8,    "Q", "1145")
    CALL CalcH("merphos",                                                  "150-50-5", 2.27E-5,   "Q", "1145")
    CALL CalcH("mesalamine",                                                "89-57-6", 5.0E-12,   "Q", "1145")
    CALL CalcH("mestranol",                                                 "72-33-3", 4.5E-9,    "Q", "1145")
    CALL CalcH("metaxalone",                                              "1665-48-1", 2.7E-10,   "Q", "1145")
    CALL CalcH("metformin",                                               "1115-70-4", 7.6E-16,   "Q", "1145")
    CALL CalcH("methamidophos",                                          "10265-92-6", 8.7E-10,   "Q", "1145")
    CALL CalcH("methandrostenolone",                                        "72-63-9", 2.2E-9,    "Q", "1145")
    CALL CalcH("methanesulfonyl chloride",                                 "124-63-0", 4.4E-5,    "Q", "1145")
    CALL CalcH("methocarbamol",                                            "532-03-6", 6.5E-16,   "Q", "1145")
    CALL CalcH("methoxyfenozide",                                       "161050-58-4", 3.8E-12,   "Q", "1145")
    CALL CalcH("methyl carbamate",                                         "598-55-0", 4.0E-8,    "Q", "1145")
    CALL CalcH("methyl decanoate",                                         "110-42-9", 0.0017,    "Q", "1145")
    CALL CalcH("methyl dodecanoate",                                       "111-82-0", 0.003,     "Q", "1145")
    CALL CalcH("methyl jasmonate",                                        "1211-29-6", 1.4E-8,    "Q", "1145")
    CALL CalcH("methyl lactate",                                           "547-64-8", 8.51E-9,   "Q", "1145")
    CALL CalcH("methyl methanesulfonate",                                   "66-27-3", 4.0E-6,    "Q", "1145")
    CALL CalcH("methyl myristate",                                         "124-10-7", 0.0052,    "Q", "1145")
    CALL CalcH("methyl nitrite",                                           "624-91-9", 6.6E-5,    "Q", "1145")
    CALL CalcH("methyl oleate",                                            "112-62-9", 0.014,     "Q", "1145")
    CALL CalcH("methyl palmitate",                                         "112-39-0", 0.009,     "Q", "1145")
    CALL CalcH("methyl stearate",                                          "112-61-8", 0.016,     "Q", "1145")
    CALL CalcH("methyl thiocyanate",                                       "556-64-9", 4.4E-5,    "Q", "1145")
    CALL CalcH("methyl thioglycolate",                                    "2365-48-2", 6.22E-6,   "Q", "1145")
    CALL CalcH("methyl trithion",                                          "953-17-3", 1.0E-7,    "Q", "1145")
    CALL CalcH("methylbiphenyl",                                           "643-58-3", 4.5E-4,    "Q", "1145")
    CALL CalcH("methyldichlorosilane",                                      "75-54-7", 1.3E-2,    "Q", "1145")
    CALL CalcH("methylmercuric dicyanamide",                               "502-39-6", 1.4E-10,   "Q", "1145")
    CALL CalcH("methylneodecanamide",                                   "105726-67-8", 2.4E-7,    "Q", "1145")
    CALL CalcH("methylparaben",                                             "99-76-3", 2.2E-9,    "Q", "1145")
    CALL CalcH("methylphosphonyl difluoride",                              "676-99-3", 2.2E-5,    "Q", "1145")
    CALL CalcH("methylprednisolone",                                        "83-43-2", 3.6E-8,    "Q", "1145")
    CALL CalcH("methyltriethyl lead",                                     "1762-28-3", 0.62,      "Q", "1145")
    CALL CalcH("metoprolol",                                             "37350-58-6", 2.1E-11,   "Q", "1145")
    CALL CalcH("metronidazole",                                            "443-48-1", 1.7E-11,   "Q", "1145")
    CALL CalcH("mevinphos",                                               "7786-34-7", 3.9E-9,    "Q", "1145")
    CALL CalcH("MGK 264",                                                  "113-48-4", 2.8E-7,    "Q", "1145")
    CALL CalcH("mifepristone",                                           "84371-65-3", 5.0E-5,    "Q", "1145")
    CALL CalcH("milk thistle extract",                                   "84604-20-6", 1.6E-23,   "Q", "1145")
    CALL CalcH("mobam",                                                   "1079-33-0", 1.7E-9,    "Q", "1145")
    CALL CalcH("monensin",                                               "17090-79-8", 2E-24,     "Q", "1145")
    CALL CalcH("musk ambrette",                                             "83-66-9", 7.05E-7,   "Q", "1145")
    CALL CalcH("musk ketone",                                               "81-14-1", 1.9E-9,    "Q", "1145")
    CALL CalcH("musk xylene",                                               "81-15-2", 7.7E-9,    "Q", "1145")
    CALL CalcH("mycophenolate mofetil",                                 "128794-94-5", 5.5E-15,   "Q", "1145")
    CALL CalcH("myristicin",                                               "607-91-0", 5.4E-7,    "Q", "1145")
    CALL CalcH("N,N'-diphenyl-p-phenylenediamine",                          "74-31-7", 2.1E-10,   "Q", "1145")
    CALL CalcH("N,N'-diphenylguanidine",                                   "102-06-7", 7.1E-12,   "Q", "1145")
    CALL CalcH("N,N-di(2-hydroxyethyl)lauramide",                          "120-40-1", 2.16E-12,  "Q", "1145")
    CALL CalcH("N,N-diethyl-p-phenylenediamine",                            "93-05-0", 5.3E-8,    "Q", "1145")
    CALL CalcH("N,N-diethylthiourea",                                      "105-55-5", 6.85E-8,   "Q", "1145")
    CALL CalcH("N,N-dimethyl-1-dodecanamine",                              "112-18-5", 4.9E-3,    "Q", "1145")
    CALL CalcH("N,N-dimethyl-p-benzenediamine",                             "99-98-9", 3.0E-8,    "Q", "1145")
    CALL CalcH("N,N-dimethyltryptamine",                                    "61-50-7", 6.4E-10,   "Q", "1145")
    CALL CalcH("N-(2-aminoethyl)piperazine",                               "140-31-8", 6.6E-13,   "Q", "1145")
    CALL CalcH("N-(cyclohexylthio)phthalimide",                          "17796-82-6", 6.4E-8,    "Q", "1145")
    CALL CalcH("n-butyl carbamate",                                        "592-35-8", 9.3E-8,    "Q", "1145")
    CALL CalcH("n-butyl stearate",                                         "123-95-5", 0.038,     "Q", "1145")
    CALL CalcH("N-ethyl-3-methylaniline",                                  "102-27-2", 6.1E-6,    "Q", "1145")
    CALL CalcH("N-ethyl-N-nitrosourethane",                                "614-95-9", 1.9E-8,    "Q", "1145")
    CALL CalcH("N-ethylaniline",                                           "103-69-5", 1.6E-5,    "Q", "1145")
    CALL CalcH("N-ethylmorpholine",                                        "100-74-3", 2.47E-8,   "Q", "1145")
    CALL CalcH("N-ethylthiourea",                                          "625-53-6", 2.35E-8,   "Q", "1145")
    CALL CalcH("N-isopropylacrylamide",                                   "2210-25-5", 2.3E-8,    "Q", "1145")
    CALL CalcH("N-isopropylaniline",                                       "768-52-5", 7.4E-6,    "Q", "1145")
    CALL CalcH("N-methyl-4-aminophenol",                                   "150-75-4", 4.4E-10,   "Q", "1145")
    CALL CalcH("N-methyl-N'-nitro-N-nitrosoguanidine",                      "70-25-7", 1.2E-12,   "Q", "1145")
    CALL CalcH("N-methyl-N-nitrosobenzeneamine",                           "614-00-6", 4.83E-6,   "Q", "1145")
    CALL CalcH("N-nitroso-n-butylurea",                                    "869-01-2", 2.32E-10,  "Q", "1145")
    CALL CalcH("N-nitrosodiphenylamine",                                    "86-30-6", 1.21E-6,   "Q", "1145")
    CALL CalcH("N-nitrosomethylvinylamine",                               "4549-40-0", 3.6E-6,    "Q", "1145")
    CALL CalcH("n-pentadecylbenzene",                                     "2131-18-2", 0.79,      "Q", "1145")
    CALL CalcH("N-phenylformamide",                                        "103-70-8", 8.45E-9,   "Q", "1145")
    CALL CalcH("n-propyl methacrylate",                                   "2210-28-8", 5.6E-4,    "Q", "1145")
    CALL CalcH("nadolol",                                                "42200-33-9", 1.4E-14,   "Q", "1145")
    CALL CalcH("nalmefene",                                              "55096-26-9", 1.8E-16,   "Q", "1145")
    CALL CalcH("naltrexone",                                             "16590-41-3", 4.2E-19,   "Q", "1145")
    CALL CalcH("nandrolone",                                               "434-22-0", 2.7E-9,    "Q", "1145")
    CALL CalcH("naphthalic anhydride",                                      "81-84-5", 6.2E-7,    "Q", "1145")
    CALL CalcH("neotame",                                               "165450-17-9", 2.3E-9,    "Q", "1145")
    CALL CalcH("nicotinamide",                                              "98-92-0", 2.9E-12,   "Q", "1145")
    CALL CalcH("nicotine",                                                  "54-11-5", 3.0E-9,    "Q", "1145")
    CALL CalcH("nifedipine",                                             "21829-25-4", 7.3E-14,   "Q", "1145")
    CALL CalcH("nifurthiazole",                                           "3570-75-0", 7.6E-18,   "Q", "1145")
    CALL CalcH("nithiazide",                                               "139-94-6", 1.6E-15,   "Q", "1145")
    CALL CalcH("nivalenol",                                              "23282-20-4", 7.3E-16,   "Q", "1145")
    CALL CalcH("nonachlor",                                               "3734-49-4", 2.5E-5,    "Q", "1145")
    CALL CalcH("nonoxynol-9",                                            "26571-11-9", 5.6E-22,   "Q", "1145")
    CALL CalcH("norbormide",                                               "991-42-4", 2.7E-23,   "Q", "1145")
    CALL CalcH("norepinephrine",                                            "51-41-2", 3.2E-19,   "Q", "1145")
    CALL CalcH("norethindrone",                                             "68-22-4", 5.8E-10,   "Q", "1145")
    CALL CalcH("norethynodrel",                                             "68-23-5", 1.3E-9,    "Q", "1145")
    CALL CalcH("norgestrel",                                              "6533-00-2", 7.7E-10,   "Q", "1145")
    CALL CalcH("notoginsenoside R1",                                     "80418-24-2", 1.5E-31,   "Q", "1145")
    CALL CalcH("nystatin",                                                "1400-61-9", 2.0E-10,   "Q", "1145")
    CALL CalcH("o-anisidine",                                               "90-04-0", 9.3E-7,    "Q", "1145")
    CALL CalcH("o-chlorostyrene",                                         "2039-87-4", 2.1E-3,    "Q", "1145")
    CALL CalcH("o-toluenesulfonamide",                                      "88-19-7", 4.7E-7,    "Q", "1145")
    CALL CalcH("o-xylyl bromide",                                           "89-92-9", 7.6E-4,    "Q", "1145")
    CALL CalcH("ochratoxin C",                                            "4865-85-4", 1.3E-14,   "Q", "1145")
    CALL CalcH("octachlorostyrene",                                      "29082-74-4", 2.3E-4,    "Q", "1145")
    CALL CalcH("octadecylamine",                                           "124-30-1", 9.4E-4,    "Q", "1145")
    CALL CalcH("octyl decyl phthalate",                                    "119-07-3", 2.1E-5,    "Q", "1145")
    CALL CalcH("oil pink",                                                "1229-55-6", 1.1E-10,   "Q", "1145")
    CALL CalcH("olanzapine",                                            "132539-06-1", 7.5E-15,   "Q", "1145")
    CALL CalcH("oleyl alcohol",                                            "143-28-2", 4.6E-4,    "Q", "1145")
    CALL CalcH("omethoate",                                               "1113-02-6", 4.6E-14,   "Q", "1145")
    CALL CalcH("oseltamivir",                                           "196618-13-0", 2.9E-16,   "Q", "1145")
    CALL CalcH("oxandrolone",                                               "53-39-4", 2.3E-8,    "Q", "1145")
    CALL CalcH("oxazepam",                                                 "604-75-1", 5.5E-10,   "Q", "1145")
    CALL CalcH("oxcarbazepine",                                          "28721-07-5", 6.9E-13,   "Q", "1145")
    CALL CalcH("oxychlordane",                                           "27304-13-8", 8.6E-8,    "Q", "1145")
    CALL CalcH("oxydemeton-methyl",                                        "301-12-2", 1.6E-13,   "Q", "1145")
    CALL CalcH("oxymatrine",                                             "16837-52-8", 1.0E-18,   "Q", "1145")
    CALL CalcH("oxymetholone",                                             "434-07-1", 1.5E-9,    "Q", "1145")
    CALL CalcH("oxymorphone",                                               "76-41-5", 4.1E-19,   "Q", "1145")
    CALL CalcH("p,p'-oxybis(benzenesulfonyl hydrazide)",                    "80-51-3", 1.26E-17,  "Q", "1145")
    CALL CalcH("p-aminodiphenylamine",                                     "101-54-2", 3.7E-10,   "Q", "1145")
    CALL CalcH("p-anisoyl chloride",                                       "100-07-2", 7.8E-6,    "Q", "1145")
    CALL CalcH("p-chloroacetanilide",                                      "539-03-7", 4.6E-6,    "Q", "1145")
    CALL CalcH("p-chlorostyrene",                                         "1073-67-2", 2.1E-3,    "Q", "1145")
    CALL CalcH("p-cresidine",                                              "120-71-8", 1.24E-7,   "Q", "1145")
    CALL CalcH("p-terphenyl",                                               "92-94-4", 3.4E-5,    "Q", "1145")
    CALL CalcH("p-toluenesulfonamide",                                      "70-55-3", 4.7E-7,    "Q", "1145")
    CALL CalcH("p-toluenesulfonic acid",                                   "104-15-4", 2.78E-9,   "Q", "1145")
    CALL CalcH("p-xylyl bromide",                                          "104-81-4", 7.6E-4,    "Q", "1145")
    CALL CalcH("paliperidone",                                          "144598-75-4", 7.9E-21,   "Q", "1145")
    CALL CalcH("pantoprazole",                                          "102625-70-7", 5.84E-20,  "Q", "1145")
    CALL CalcH("papaverine",                                                "58-74-2", 7.5E-13,   "Q", "1145")
    CALL CalcH("paraoxon",                                                 "311-45-5", 6.4E-10,   "Q", "1145")
    CALL CalcH("paricalcitol",                                          "131918-61-1", 3.8E-7,    "Q", "1145")
    CALL CalcH("patulin",                                                  "149-29-1", 1.1E-10,   "Q", "1145")
    CALL CalcH("penciclovir",                                            "39809-25-1", 9.5E-32,   "Q", "1145")
    CALL CalcH("pentabromophenol",                                         "608-71-9", 5.6E-9,    "Q", "1145")
    CALL CalcH("pentachloroaniline",                                       "527-20-8", 4.25E-7,   "Q", "1145")
    CALL CalcH("pentachloroanisole",                                      "1825-21-4", 1.94E-3,   "Q", "1145")
    CALL CalcH("pentachloronaphthalene",                                  "1321-64-8", 1.2E-4,    "Q", "1145")
    CALL CalcH("pentachloropyridine",                                     "2176-62-7", 6.3E-3,    "Q", "1145")
    CALL CalcH("pentachlorothiophenol",                                    "133-49-3", 1.5E-4,    "Q", "1145")
    CALL CalcH("pentaerythritol",                                          "115-77-5", 4.1E-10,   "Q", "1145")
    CALL CalcH("pentafluoroethane",                                        "354-33-6", 4.9E-2,    "Q", "1145")
    CALL CalcH("pentobarbital",                                             "76-74-4", 8.4E-13,   "Q", "1145")
    CALL CalcH("perflexane",                                               "355-42-0", 1.84E4,    "Q", "1145")
    CALL CalcH("perfluorobutane",                                          "355-25-9", 667.,      "Q", "1145")
    CALL CalcH("perfluorobutyl ethylene",                                "19430-93-4", 110.,      "Q", "1145")
    CALL CalcH("perfluoroisobutylene",                                     "382-21-8", 34.,       "Q", "1145")
    CALL CalcH("perfluorooctanesulfonamide",                               "754-91-6", 1.8,       "Q", "1145")
    CALL CalcH("perfluorooctylsulfonyl fluoride",                          "307-35-7", 68.,       "Q", "1145")
    CALL CalcH("perfluorotributylamine",                                   "311-89-7", 5.5E4,     "Q", "1145")
    CALL CalcH("peroxybenzoic acid, t-butyl ester",                        "614-45-9", 2.1E-4,    "Q", "1145")
    CALL CalcH("peroxyformic acid",                                        "107-32-4", 1.9E-6,    "Q", "1145")
    CALL CalcH("perthane",                                                  "72-56-0", 1.7E-4,    "Q", "1145")
    CALL CalcH("phenazopyridine",                                           "94-78-0", 3.30E-15,  "Q", "1145")
    CALL CalcH("phencyclidine",                                             "77-10-1", 5.5E-6,    "Q", "1145")
    CALL CalcH("phendimetrazine",                                          "634-03-7", 2.7E-8,    "Q", "1145")
    CALL CalcH("phenelzine",                                                "51-71-8", 3.4E-9,    "Q", "1145")
    CALL CalcH("phenobarbital",                                             "50-06-6", 1.7E-14,   "Q", "1145")
    CALL CalcH("phenolphthalein",                                           "77-09-8", 9.0E-16,   "Q", "1145")
    CALL CalcH("phenothiazine",                                             "92-84-2", 2.8E-8,    "Q", "1145")
    CALL CalcH("phentermine",                                              "122-09-8", 1.4E-6,    "Q", "1145")
    CALL CalcH("phenyl acetate",                                           "122-79-2", 6.5E-5,    "Q", "1145")
    CALL CalcH("phenylbutazone",                                            "50-33-9", 6.56E-9,   "Q", "1145")
    CALL CalcH("phenyldichloroarsine",                                     "696-28-6", 3.0E-5,    "Q", "1145")
    CALL CalcH("phenytoin",                                                 "57-41-0", 1.02E-11,  "Q", "1145")
    CALL CalcH("phosacetim",                                              "4104-14-7", 4.8E-9,    "Q", "1145")
    CALL CalcH("phosalone",                                               "2310-17-0", 3.9E-7,    "Q", "1145")
    CALL CalcH("phosgene oxime",                                          "1794-86-1", 5.5E-7,    "Q", "1145")
    CALL CalcH("phosphamidon",                                           "13171-21-6", 1.5E-12,   "Q", "1145")
    CALL CalcH("phthalamide",                                               "88-96-0", 1.4E-12,   "Q", "1145")
    CALL CalcH("phthalimide",                                               "85-41-6", 1E-8,      "Q", "1145")
    CALL CalcH("picaridin",                                             "119515-38-7", 3.0E-11,   "Q", "1145")
    CALL CalcH("pinacolyl alcohol",                                        "464-07-3", 1.76E-5,   "Q", "1145")
    CALL CalcH("pindone",                                                   "83-26-1", 9.3E-12,   "Q", "1145")
    CALL CalcH("pioglitazone",                                          "111025-46-8", 1.7E-12,   "Q", "1145")
    CALL CalcH("piperalin",                                               "3478-94-2", 2.3E-8,    "Q", "1145")
    CALL CalcH("piperonyl butoxide",                                        "51-03-6", 8.9E-11,   "Q", "1145")
    CALL CalcH("PR-toxin",                                               "56299-00-4", 6.3E-14,   "Q", "1145")
    CALL CalcH("prednisolone",                                              "50-24-8", 2.7E-8,    "Q", "1145")
    CALL CalcH("prednisone",                                                "53-03-2", 2.8E-10,   "Q", "1145")
    CALL CalcH("primisulfuron-methyl",                                   "86209-51-0", 1.4E-12,   "Q", "1145")
    CALL CalcH("progesterone",                                              "57-83-0", 6.5E-8,    "Q", "1145")
    CALL CalcH("propofol",                                                "2078-54-8", 2.1E-6,    "Q", "1145")
    CALL CalcH("propyl thiouracil",                                         "51-52-5", 1.1E-9,    "Q", "1145")
    CALL CalcH("propylparaben",                                             "94-13-3", 6.4E-9,    "Q", "1145")
    CALL CalcH("pseudohypericin",                                        "55954-61-5", 1.8E-29,   "Q", "1145")
    CALL CalcH("pulegone",                                                  "89-82-7", 5.9E-5,    "Q", "1145")
    CALL CalcH("punicalagin",                                            "65995-63-3", 1.8E-16,   "Q", "1145")
    CALL CalcH("pyrethrum",                                               "8003-34-7", 6.6E-7,    "Q", "1145")
    CALL CalcH("pyrimethanil",                                           "53112-28-0", 2.5E-6,    "Q", "1145")
    CALL CalcH("pyriminil",                                              "53558-25-1", 1.84E-16,  "Q", "1145")
    CALL CalcH("pyriproxyfen",                                           "95737-68-1", 6.3E-10,   "Q", "1145")
    CALL CalcH("pyromellitic dianhydride",                                  "89-32-7", 7.5E-9,    "Q", "1145")
    CALL CalcH("quinoxyfen",                                            "124495-18-7", 9.6E-9,    "Q", "1145")
    CALL CalcH("quinupristin",                                          "120138-50-3", 2.0E-28,   "Q", "1145")
    CALL CalcH("rabeprazole",                                           "117976-89-3", 1.2E-17,   "Q", "1145")
    CALL CalcH("raltegravir",                                           "518048-05-0", 9.1E-23,   "Q", "1145")
    CALL CalcH("reserpine",                                                 "50-55-5", 5.4E-23,   "Q", "1145")
    CALL CalcH("ricinoleic acid, methyl ester",                            "141-24-2", 1.47E-7,   "Q", "1145")
    CALL CalcH("risperidone",                                           "106266-06-2", 2.2E-16,   "Q", "1145")
    CALL CalcH("rosiglitazone",                                         "122320-73-4", 1.7E-14,   "Q", "1145")
    CALL CalcH("rosuvastatin",                                          "287714-41-4", 3.4E-20,   "Q", "1145")
    CALL CalcH("rotenone",                                                  "83-79-4", 1.12E-13,  "Q", "1145")
    CALL CalcH("ruelene",                                                  "299-86-5", 2.5E-9,    "Q", "1145")
    CALL CalcH("saccharin",                                                 "81-07-2", 1.2E-9,    "Q", "1145")
    CALL CalcH("safrole",                                                   "94-59-7", 9.1E-6,    "Q", "1145")
    CALL CalcH("salicylamide",                                              "65-45-2", 2.9E-10,   "Q", "1145")
    CALL CalcH("schradan",                                                 "152-16-9", 6.3E-17,   "Q", "1145")
    CALL CalcH("sec-amyl acetate",                                         "626-38-0", 7.9E-4,    "Q", "1145")
    CALL CalcH("sec-butyl bromide",                                         "78-76-2", 1.6E-2,    "Q", "1145")
    CALL CalcH("selenium methionine",                                     "1464-42-2", 3.4E-11,   "Q", "1145")
    CALL CalcH("semustine",                                              "13909-09-6", 2.5E-10,   "Q", "1145")
    CALL CalcH("sethoxydim",                                             "74051-80-2", 2.2E-11,   "Q", "1145")
    CALL CalcH("sevoflurane",                                            "28523-86-6", 1.9E-1,    "Q", "1145")
    CALL CalcH("shikimic acid",                                            "138-59-0", 2.7E-14,   "Q", "1145")
    CALL CalcH("sildenafil",                                            "139755-83-2", 7.2E-21,   "Q", "1145")
    CALL CalcH("simvastatin",                                            "79902-63-9", 2.8E-10,   "Q", "1145")
    CALL CalcH("sorbic acid",                                              "110-44-1", 5.0E-8,    "Q", "1145")
    CALL CalcH("spironolactone",                                            "52-01-7", 1.1E-10,   "Q", "1145")
    CALL CalcH("stanozolol",                                             "10418-03-8", 1.1E-8,    "Q", "1145")
    CALL CalcH("stavudine",                                               "3056-17-5", 2.3E-15,   "Q", "1145")
    CALL CalcH("stigmasterol",                                              "83-48-7", 2.6E-4,    "Q", "1145")
    CALL CalcH("strychnine",                                                "57-24-9", 6.0E-14,   "Q", "1145")
    CALL CalcH("sucralose",                                              "56038-13-2", 4.0E-19,   "Q", "1145")
    CALL CalcH("sufentanil",                                             "56030-54-7", 4.11E-15,  "Q", "1145")
    CALL CalcH("sulfamethazine",                                            "57-68-1", 3.1E-13,   "Q", "1145")
    CALL CalcH("sulfamethizole",                                           "144-82-1", 2.6E-14,   "Q", "1145")
    CALL CalcH("sulfamethoxazole",                                         "723-46-6", 6.4E-13,   "Q", "1145")
    CALL CalcH("sulfanilamide",                                             "63-74-1", 1.5E-10,   "Q", "1145")
    CALL CalcH("sulfathiazole",                                             "72-14-0", 5.8E-14,   "Q", "1145")
    CALL CalcH("sulfisoxazole",                                            "127-69-5", 1.6E-12,   "Q", "1145")
    CALL CalcH("sulfluramid",                                             "4151-50-2", 5.4,       "Q", "1145")
    CALL CalcH("sulfolane",                                                "126-33-0", 4.8E-6,    "Q", "1145")
    CALL CalcH("sulfometuron methyl",                                    "74222-97-2", 5.2E-14,   "Q", "1145")
    CALL CalcH("sulphenone",                                                "80-00-2", 1.9E-7,    "Q", "1145")
    CALL CalcH("sumatriptan",                                           "103628-46-2", 4.5E-14,   "Q", "1145")
    CALL CalcH("swep",                                                    "1918-18-9", 1.20E-8,   "Q", "1145")
    CALL CalcH("T-2 toxin",                                              "21259-20-1", 5.5E-18,   "Q", "1145")
    CALL CalcH("t-butyl mercaptan",                                         "75-66-1", 6.1E-3,    "Q", "1145")
    CALL CalcH("t-octyl mercaptan",                                        "141-59-3", 1.9E-2,    "Q", "1145")
    CALL CalcH("tadalafil",                                             "171596-29-5", 5.0E-18,   "Q", "1145")
    CALL CalcH("tamsulosin",                                            "106133-20-4", 4.9E-15,   "Q", "1145")
    CALL CalcH("tanshinone II",                                            "568-72-9", 5.0E-9,    "Q", "1145")
    CALL CalcH("tecnazene",                                                "117-18-0", 2.3E-5,    "Q", "1145")
    CALL CalcH("telapravir",                                            "402957-28-2", 7.8E-31,   "Q", "1145")
    CALL CalcH("temephos",                                                "3383-96-8", 2.0E-9,    "Q", "1145")
    CALL CalcH("tert-butyl acetate",                                       "540-88-5", 4.1E-4,    "Q", "1145")
    CALL CalcH("tert-butyl hydroperoxide",                                  "75-91-2", 1.6E-5,    "Q", "1145")
    CALL CalcH("testolactone",                                             "968-93-4", 6.1E-8,    "Q", "1145")
    CALL CalcH("testosterone",                                              "58-22-0", 3.5E-9,    "Q", "1145")
    CALL CalcH("tetra-n-butyl tin",                                       "1461-25-2", 6.,        "Q", "1145")
    CALL CalcH("tetrabromodiphenyl ether",                               "40088-47-9", 8.5E-6,    "Q", "1145")
    CALL CalcH("tetradecylbenzene",                                       "1459-10-5", 0.237,     "Q", "1145")
    CALL CalcH("tetradecylbenzyldimethylammonium chloride",                "139-08-2", 1.3E-11,   "Q", "1145")
    CALL CalcH("tetraethyl silicate",                                       "78-10-4", 2.0E-5,    "Q", "1145")
    CALL CalcH("tetraethyl tin",                                           "597-64-8", 0.63,      "Q", "1145")
    CALL CalcH("tetraethylene glycol",                                     "112-60-7", 5.5E-19,   "Q", "1145")
    CALL CalcH("tetraethylenepentamine",                                   "112-57-2", 3E-20,     "Q", "1145")
    CALL CalcH("tetrahydrofurfuryl alcohol",                                "97-99-4", 4.1E-9,    "Q", "1145")
    CALL CalcH("tetrakis(hydroxymethyl)phosphonium sulfate",             "55566-30-8", 1.7E-23,   "Q", "1145")
    CALL CalcH("tetralin",                                                 "119-64-2", 1.7E-3,    "Q", "1145")
    CALL CalcH("tetramethyl silicate",                                     "681-84-5", 6.4E-6,    "Q", "1145")
    CALL CalcH("tetramethylammonium chloride",                              "75-57-0", 4.2E-12,   "Q", "1145")
    CALL CalcH("tetramethylammonium hydroxide",                             "75-59-2", 4.36E-16,  "Q", "1145")
    CALL CalcH("tetramethylsuccinonitrile",                               "3333-52-6", 5.2E-8,    "Q", "1145")
    CALL CalcH("tetramethylthiourea",                                     "2782-91-4", 1.16E-8,   "Q", "1145")
    CALL CalcH("tetryl",                                                   "479-45-8", 2.7E-9,    "Q", "1145")
    CALL CalcH("theobromine",                                               "83-67-0", 1.6E-11,   "Q", "1145")
    CALL CalcH("theophylline",                                              "58-55-9", 1.8E-14,   "Q", "1145")
    CALL CalcH("thioacetamide",                                             "62-55-5", 6.4E-6,    "Q", "1145")
    CALL CalcH("thiocyanic acid, (2-benzothiazolylthio) methyl ester",   "21564-17-0", 6.5E-12,   "Q", "1145")
    CALL CalcH("thiodiglycol",                                             "111-48-8", 1.9E-9,    "Q", "1145")
    CALL CalcH("thiodiglycolic acid",                                      "123-93-3", 4.4E-14,   "Q", "1145")
    CALL CalcH("thiophanate-ethyl",                                      "23564-06-9", 5.2E-13,   "Q", "1145")
    CALL CalcH("thioquinox",                                                "93-75-4", 7.8E-8,    "Q", "1145")
    CALL CalcH("timolol",                                                "26839-75-8", 4.3E-17,   "Q", "1145")
    CALL CalcH("tinidazole",                                             "19387-91-8", 5.2E-11,   "Q", "1145")
    CALL CalcH("tirofiban",                                             "144494-65-5", 7.4E-15,   "Q", "1145")
    CALL CalcH("toluene-2,4-diamine",                                       "95-80-7", 9.5E-10,   "Q", "1145")
    CALL CalcH("topramezone",                                           "210631-68-8", 9.4E-18,   "Q", "1145")
    CALL CalcH("trans-1,3-pentadiene",                                    "2004-70-8", 0.12,      "Q", "1145")
    CALL CalcH("trans-2-pentene",                                          "646-04-8", 0.32,      "Q", "1145")
    CALL CalcH("tri(2-butoxyethyl) phosphate",                              "78-51-3", 1.2E-11,   "Q", "1145")
    CALL CalcH("tri-(2-chloroisopropyl)phosphate",                       "13674-84-5", 6.0E-8,    "Q", "1145")
    CALL CalcH("tri-n-octylamine",                                        "1116-76-3", 1.4E-2,    "Q", "1145")
    CALL CalcH("triallyl phosphate",                                      "1623-19-4", 5.6E-7,    "Q", "1145")
    CALL CalcH("triaziquone",                                               "68-76-8", 9.3E-16,   "Q", "1145")
    CALL CalcH("tributyrin",                                                "60-01-5", 9.6E-9,    "Q", "1145")
    CALL CalcH("tricaprylin",                                              "538-23-8", 2.56E-8,   "Q", "1145")
    CALL CalcH("trichloroacetonitrile",                                    "545-06-2", 1.3E-6,    "Q", "1145")
    CALL CalcH("trichloroacetyl chloride",                                  "76-02-8", 2.9E-5,    "Q", "1145")
    CALL CalcH("trichloroisocyanuric acid",                                 "87-90-1", 6.2E-11,   "Q", "1145")
    CALL CalcH("trichloronaphthalene",                                    "1321-65-9", 3.1E-4,    "Q", "1145")
    CALL CalcH("triclocarban",                                             "101-20-2", 4.5E-11,   "Q", "1145")
    CALL CalcH("triclofos",                                                "306-52-5", 1.41E-13,  "Q", "1145")
    CALL CalcH("triclosan",                                               "3380-34-5", 2.1E-8,    "Q", "1145")
    CALL CalcH("tridecylbenzene",                                          "123-02-4", 0.178,     "Q", "1145")
    CALL CalcH("triethylene glycol bis(2-ethylbutyrate)",                   "95-08-9", 1E-11,     "Q", "1145")
    CALL CalcH("triethylene glycol dimethacrylate",                        "109-16-0", 1.7E-12,   "Q", "1145")
    CALL CalcH("triethylene glycol monobutyl ether",                       "143-22-6", 9.5E-14,   "Q", "1145")
    CALL CalcH("triethylene glycol monoethyl ether",                       "112-50-5", 4.8E-14,   "Q", "1145")
    CALL CalcH("triethylene glycol monomethyl ether",                      "112-35-6", 3.5E-14,   "Q", "1145")
    CALL CalcH("triethylene glycol",                                       "112-27-6", 3.2E-11,   "Q", "1145")
    CALL CalcH("triethylene glycol, diacetate",                            "111-21-7", 2.7E-13,   "Q", "1145")
    CALL CalcH("trifenmorph",                                             "1420-06-0", 1.3E-10,   "Q", "1145")
    CALL CalcH("trifluoroethene",                                          "359-11-5", 0.43,      "Q", "1145")
    CALL CalcH("trifluridine",                                              "70-00-8", 9.5E-17,   "Q", "1145")
    CALL CalcH("trimethoate",                                             "2275-18-5", 6.6E-11,   "Q", "1145")
    CALL CalcH("trimethoprim",                                             "738-70-5", 2.4E-14,   "Q", "1145")
    CALL CalcH("trimethoxysilylpropyl methacrylate",                      "2530-85-0", 3.0E-7,    "Q", "1145")
    CALL CalcH("trimethyl phosphite",                                      "121-45-9", 1.1E-5,    "Q", "1145")
    CALL CalcH("trimethylbenzylammonium chloride",                          "56-93-9", 3.4E-14,   "Q", "1145")
    CALL CalcH("trimethylhexadecylammonium chloride",                      "112-02-7", 2.9E-10,   "Q", "1145")
    CALL CalcH("tripropylene glycol",                                    "24800-44-0", 3.3E-15,   "Q", "1145")
    CALL CalcH("tris(1,3-dichloro-2-propyl) phosphate",                  "13674-87-8", 2.61E-9,   "Q", "1145")
    CALL CalcH("tris(2,3-dichloropropyl) phosphate",                        "78-43-3", 2.6E-9,    "Q", "1145")
    CALL CalcH("tris(2,4-xylenyl)phosphate",                              "3862-12-2", 7.2E-8,    "Q", "1145")
    CALL CalcH("tris(2,5-xylenyl)phosphate",                             "19074-59-0", 7.2E-8,    "Q", "1145")
    CALL CalcH("tris(2,6-xylenyl)phosphate",                               "121-06-2", 7.2E-8,    "Q", "1145")
    CALL CalcH("tris(2-ethylhexyl) trimellitate",                         "3319-31-1", 4.4E-7,    "Q", "1145")
    CALL CalcH("tris(3,4-xylenyl)phosphate",                              "3862-11-1", 7.2E-8,    "Q", "1145")
    CALL CalcH("tris(3,5-xylenyl)phosphate",                             "25653-16-1", 7.2E-8,    "Q", "1145")
    CALL CalcH("tris(4-iisopropylphenyl)phosphate",                      "26967-76-0", 2.9E-7,    "Q", "1145")
    CALL CalcH("tristearin",                                               "555-43-1", 1.4E-3,    "Q", "1145")
    CALL CalcH("triticonazole",                                         "131983-72-7", 1.5E-10,   "Q", "1145")
    CALL CalcH("tylosin",                                                 "1401-69-0", 5.8E-38,   "Q", "1145")
    CALL CalcH("undecylbenzene",                                          "6742-54-7", 0.10,      "Q", "1145")
    CALL CalcH("uracil mustard",                                            "66-75-1", 3.9E-13,   "Q", "1145")
    CALL CalcH("urea nitrate",                                             "124-47-0", 1.7E-17,   "Q", "1145")
    CALL CalcH("valdecoxib",                                            "181695-72-7", 2.2E-11,   "Q", "1145")
    CALL CalcH("valsartan",                                             "137862-53-4", 3.1E-18,   "Q", "1145")
    CALL CalcH("vamidothion",                                             "2275-23-2", 8.6E-16,   "Q", "1145")
    CALL CalcH("vardenafil",                                            "224785-90-4", 1.9E-21,   "Q", "1145")
    CALL CalcH("vernurafenib",                                          "918504-65-1", 1.2E-17,   "Q", "1145")
    CALL CalcH("vinyl bromide",                                            "593-60-2", 1.4E-2,    "Q", "1145")
    CALL CalcH("vinyl fluoride",                                            "75-02-5", 0.12,      "Q", "1145")
    CALL CalcH("vinyl sulfone",                                             "77-77-0", 4.9E-5,    "Q", "1145")
    CALL CalcH("vinyl sulfoxide",                                         "1115-15-7", 3.9E-7,    "Q", "1145")
    CALL CalcH("vismodegib",                                            "879085-55-9", 1.6E-17,   "Q", "1145")
    CALL CalcH("xylidine",                                                "1300-73-8", 2.5E-6,    "Q", "1145")
    CALL CalcH("xylitol",                                                   "87-99-0", 1.5E-11,   "Q", "1145")
    CALL CalcH("zoxamide",                                              "156052-68-5", 2.0E-9,    "Q", "1145")
    ! type = "V"
    CALL CalcH("(2-bromoethyl)benzene",                                                   "103-63-9", 1.52E-3,  "V")
    CALL CalcH("1,1'-azo-bis(formamide)",                                                 "123-77-3", 8.2E-13,  "V")
    CALL CalcH("1,1'-methylenebisbenzene",                                                "101-81-5", 1.3E-4,   "V")
    CALL CalcH("1,1,1,2-tetrachloro-2,2-difluoroethane",                                   "76-11-9", 1.6E-1,   "V")
    CALL CalcH("1,1,2,2-tetrabromoethane",                                                 "79-27-6", 1.3E-5,   "V")
    CALL CalcH("1,1,2,2-tetrachloro-1,2-difluoroethane",                                   "76-12-0", 0.11,     "V")
    CALL CalcH("1,1,2-trichloro-1,2,2-trifluoroethane",                                    "76-13-1", 5.26E-1,  "V")
    CALL CalcH("1,1-dichloro-1,2,2,2-tetrafluoroethane",                                  "374-07-2", 1.2,      "V")
    CALL CalcH("1,1-dichloro-1-fluoroethane",                                            "1717-00-6", 0.0220,   "V")
    CALL CalcH("1,1-dichloropropane",                                                      "78-99-9", 3.8E-3,   "V")
    CALL CalcH("1,1-difluoroethene",                                                       "75-38-7", 0.4,      "V")
    CALL CalcH("1,1-dimethylhydrazine",                                                    "57-14-7", 1.3E-5,   "V")
    CALL CalcH("1,2,3,4-tetrachloronaphthalene",                                        "20020-02-4", 0.000238, "V")
    CALL CalcH("1,2,3-benzotriazole",                                                      "95-14-7", 3.2E-7,   "V")
    CALL CalcH("1,2,5,6,9,10-hexabromocyclododecane",                                    "3194-55-6", 4.6E-5,   "V")
    CALL CalcH("1,2-bis(2-chloroethoxy)ethane",                                           "112-26-5", 7.8E-7,   "V")
    CALL CalcH("1,2-dibromo-2,4-dicyanobutane",                                         "35691-65-7", 8.3E-9,   "V")
    CALL CalcH("1,2-dibromo-3-chloropropane",                                              "96-12-8", 1.5E-4,   "V")
    CALL CalcH("1,2-dibromoethylene",                                                     "540-49-8", 8.5E-4,   "V")
    CALL CalcH("1,2-dibromopropane",                                                       "78-75-1", 1.46E-3,  "V")
    CALL CalcH("1,2-dibromotetrafluoroethane",                                            "124-73-2", 37.,      "V")
    CALL CalcH("1,2-dichloro-1,1-difluoroethane",                                        "1649-08-7", 0.0710,   "V")
    CALL CalcH("1,2-dichloropropene",                                                     "563-54-2", 4.91E-3,  "V")
    CALL CalcH("1,2-diethylbenzene",                                                      "135-01-3", 2.6E-3,   "V")
    CALL CalcH("1,2-dimethylhydrazine",                                                   "540-73-8", 5.5E-6,   "V")
    CALL CalcH("1,2-dinitrobenzene",                                                      "528-29-0", 5.3E-8,   "V")
    CALL CalcH("1,2-diphenylhydrazine",                                                   "122-66-7", 4.8E-7,   "V")
    CALL CalcH("1,2-epoxybutane",                                                         "106-88-7", 1.8E-4,   "V")
    CALL CalcH("1,2-propylene oxide",                                                      "75-56-9", 6.96E-5,  "V")
    CALL CalcH("1,3,5-trinitrobenzene",                                                    "99-35-4", 6.5E-9,   "V")
    CALL CalcH("1,3-benzenediamine",                                                      "108-45-2", 1.3E-9,   "V")
    CALL CalcH("1,3-butadiene",                                                           "106-99-0", 0.074,    "V")
    CALL CalcH("1,3-cyclopentadiene",                                                     "542-92-7", 2.1E-2,   "V")
    CALL CalcH("1,3-diethylbenzene",                                                      "141-93-5", 0.00883,  "V")
    CALL CalcH("1,3-epoxypropane",                                                        "503-30-0", 2.5E-5,   "V")
    CALL CalcH("1,4-benzoquinone",                                                        "106-51-4", 4.79E-4,  "V")
    CALL CalcH("1,4-butanediol",                                                          "110-63-4", 1.3E-9,   "V")
    CALL CalcH("1,4-butynediol",                                                          "110-65-6", 1.7E-11,  "V")
    CALL CalcH("1,4-cyclohexanedimethanol",                                               "105-08-8", 6.4E-11,  "V")
    CALL CalcH("1,4-dichloro-2-butene",                                                   "764-41-0", 5.8E-4,   "V")
    CALL CalcH("1,4-dichloro-cis-2-butene",                                              "1476-11-5", 1.2E-3,   "V")
    CALL CalcH("1,4-dichloro-trans-2-butene",                                             "110-57-6", 6.6E-4,   "V")
    CALL CalcH("1,4-diethylbenzene",                                                      "105-05-5", 7.3E-3,   "V")
    CALL CalcH("1,4-dithiane",                                                            "505-29-3", 4.2E-5,   "V")
    CALL CalcH("1-bromobutane",                                                           "109-65-9", 8.7E-3,   "V")
    CALL CalcH("1-bromopropane",                                                          "106-94-5", 7.3E-3,   "V")
    CALL CalcH("1-buten-3-yne",                                                           "689-97-4", 0.029,    "V")
    CALL CalcH("1-chloro-2,4-dinitrobenzene",                                              "97-00-7", 2.45E-6,  "V")
    CALL CalcH("1-chloro-2-methyl-1-propene",                                             "513-37-1", 1.9E-2,   "V")
    CALL CalcH("1-chloropropane",                                                         "540-54-5", 0.013,    "V")
    CALL CalcH("1-decene",                                                                "872-05-9", 2.68,     "V")
    CALL CalcH("1-heptene",                                                               "592-76-7", 0.421,    "V")
    CALL CalcH("1-hexadecanol",                                                           "124-29-8", 4.6E-5,   "V")
    CALL CalcH("1-hexene",                                                                "592-41-6", 0.41,     "V")
    CALL CalcH("1-hydroxyanthraquinone",                                                  "129-43-1", 7.25E-9,  "V")
    CALL CalcH("1-naphthol",                                                               "90-15-3", 6.0E-8,   "V")
    CALL CalcH("1-naphthylamine",                                                         "134-32-7", 4.6E-7,   "V")
    CALL CalcH("1-nitroguanidine",                                                        "556-88-7", 4.45E-16, "V")
    CALL CalcH("1-nonanol",                                                               "143-08-8", 3.08E-5,  "V")
    CALL CalcH("1-octadecanol",                                                           "112-92-5", 8.41E-4,  "V")
    CALL CalcH("1-octene",                                                                "111-66-0", 0.627,    "V")
    CALL CalcH("1-pentene",                                                               "109-67-1", 0.40,     "V")
    CALL CalcH("1-propyne",                                                                "74-99-7", 1.1E-2,   "V")
    CALL CalcH("1-tetradecanol",                                                          "112-72-1", 1.6E-4,   "V")
    CALL CalcH("2,2',4,4'5,5'-hexabromobiphenyl",                                       "59080-40-9", 4.3E-6,   "V")
    CALL CalcH("2,2',6,6'-tetrabromobisphenol A",                                          "79-94-7", 4.1E-8,   "V")
    CALL CalcH("2,2-dibromo-3-nitrilopropionamide",                                     "10222-01-2", 1.9E-8,   "V")
    CALL CalcH("2,2-dimethylbutane",                                                       "75-83-2", 1.7,      "V")
    CALL CalcH("2,2-dimethylpropane",                                                     "463-82-1", 3.7,      "V")
    CALL CalcH("2,3,4,6-tetrachlorophenol",                                                "58-90-2", 1.3E-6,   "V")
    CALL CalcH("2,3,4,7,8-pentachlorodibenzofuran",                                     "57117-31-4", 5.0E-6,   "V")
    CALL CalcH("2,3,5-trimethacarb",                                                     "2655-15-4", 2.2E-7,   "V")
    CALL CalcH("2,3,6-trimethylphenol",                                                  "2416-94-6", 3.9E-6,   "V")
    CALL CalcH("2,3,7,8-tetrachlorodibenzo-p-dioxin",                                    "1746-01-6", 5.0E-5,   "V")
    CALL CalcH("2,3-butanediol",                                                          "513-85-9", 2.9E-8,   "V")
    CALL CalcH("2,3-dichlorophenol",                                                      "576-24-9", 3.46E-6,  "V")
    CALL CalcH("2,3-dimethylbutane",                                                       "79-29-8", 1.2,      "V")
    CALL CalcH("2,3-dimethylphenol",                                                      "526-75-0", 3.1E-6,   "V")
    CALL CalcH("2,4,5-trichlorophenol",                                                    "95-95-4", 1.6E-6,   "V")
    CALL CalcH("2,4,6-trichloroaniline",                                                  "634-93-5", 1.34E-6,  "V")
    CALL CalcH("2,4,6-trichlorophenol",                                                    "88-06-2", 2.6E-6,   "V")
    CALL CalcH("2,4,6-trimethylphenol",                                                   "527-60-6", 3.1E-6,   "V")
    CALL CalcH("2,4,6-trimethylpyridine",                                                 "108-75-8", 8.8E-6,   "V")
    CALL CalcH("2,4,6-trinitrotoluene",                                                   "118-96-7", 2.1E-8,   "V")
    CALL CalcH("2,4-D butoxyethyl ester",                                                "1929-73-3", 1.6E-7,   "V")
    CALL CalcH("2,4-D butyl ester",                                                        "94-80-4", 4.9E-7,   "V")
    CALL CalcH("2,4-D isopropyl ester",                                                    "94-11-1", 2.2E-6,   "V")
    CALL CalcH("2,4-dichlorophenol",                                                      "120-83-2", 3.5E-6,   "V")
    CALL CalcH("2,5-dichlorophenol",                                                      "583-78-8", 6.03E-6,  "V")
    CALL CalcH("2,5-dimethylphenol",                                                       "95-87-4", 7.1E-6,   "V")
    CALL CalcH("2,5-dinitrotoluene",                                                      "619-15-8", 5.5E-7,   "V")
    CALL CalcH("2,6-dichloro-4-nitroaniline",                                              "99-30-9", 8.4E-8,   "V")
    CALL CalcH("2,6-dichlorophenol",                                                       "87-65-0", 2.67E-6,  "V")
    CALL CalcH("2,6-diethylaniline",                                                      "579-66-8", 1.1E-6,   "V")
    CALL CalcH("2,6-dinitrotoluene",                                                      "606-20-2", 6.7E-7,   "V")
    CALL CalcH("2,6-xylidine",                                                             "87-62-7", 2.5E-6,   "V")
    CALL CalcH("2-amino-1,3,4-triazole",                                                   "61-82-5", 2.2E-13,  "V")
    CALL CalcH("2-aminoanthraquinone",                                                    "117-79-3", 9.2E-11,  "V")
    CALL CalcH("2-butanone oxime",                                                         "96-29-7", 1.22E-6,  "V")
    CALL CalcH("2-chloroaniline",                                                          "95-51-2", 5.4E-6,   "V")
    CALL CalcH("2-chloroethyl vinyl ether",                                               "110-75-8", 8.8E-3,   "V")
    CALL CalcH("2-chloropropane",                                                          "75-29-6", 1.8E-2,   "V")
    CALL CalcH("2-ethylhexaldehyde",                                                      "123-05-7", 8.4E-4,   "V")
    CALL CalcH("2-ethylhexanoic acid",                                                    "149-57-5", 2.8E-6,   "V")
    CALL CalcH("2-ethylhexanol",                                                          "104-76-7", 2.6E-5,   "V")
    CALL CalcH("2-ethylhexyl acrylate",                                                   "103-11-7", 0.000432, "V")
    CALL CalcH("2-ethylphenol",                                                            "90-00-6", 4.6E-6,   "V")
    CALL CalcH("2-hexanone",                                                              "591-78-6", 9.3E-5,   "V")
    CALL CalcH("2-hydroxyethyl acrylate",                                                 "818-61-1", 8E-9,     "V")
    CALL CalcH("2-mercaptoehtanol",                                                        "60-24-2", 1.8E-7,   "V")
    CALL CalcH("2-methyl-1-butene",                                                       "563-46-2", 0.43,     "V")
    CALL CalcH("2-methyl-1-pentanol",                                                     "105-30-6", 4.3E-5,   "V")
    CALL CalcH("2-methyl-5-ethylpyridine",                                                "104-90-5", 1.9E-5,   "V")
    CALL CalcH("2-methylaminoethanol",                                                    "109-83-1", 1.1E-7,   "V")
    CALL CalcH("2-methylpentane",                                                         "107-83-5", 1.7,      "V")
    CALL CalcH("2-nitroanisole",                                                           "91-23-6", 4.3E-7,   "V")
    CALL CalcH("2-nitropropane",                                                           "79-46-9", 1.19E-4,  "V")
    CALL CalcH("2-octanol",                                                               "123-96-6", 3.70E-5,  "V")
    CALL CalcH("2-phenoxyethanol",                                                        "122-99-6", 4.9E-8,   "V")
    CALL CalcH("2-phenylethanol",                                                          "60-12-8", 1.5E-7,   "V")
    CALL CalcH("2-pyrrolidone",                                                           "616-45-5", 1.06E-9,  "V")
    CALL CalcH("3,3-dimethyl-2-butanone",                                                  "75-97-8", 2.2E-4,   "V")
    CALL CalcH("3,4-dichloro-1-butene",                                                   "760-23-6", 8.6E-3,   "V")
    CALL CalcH("3,4-dichloroaniline",                                                      "95-76-1", 1.46E-5,  "V")
    CALL CalcH("3,4-dimethylphenol",                                                       "95-65-8", 1.2E-6,   "V")
    CALL CalcH("3,5-dichlorophenol",                                                      "591-35-5", 2.4E-7,   "V")
    CALL CalcH("3,5-dimethylphenol",                                                      "108-68-9", 1.3E-6,   "V")
    CALL CalcH("3-bromotoluene",                                                          "591-17-3", 6.7E-3,   "V")
    CALL CalcH("3-chloro-2-methyl-1-propene",                                             "563-47-3", 8.7E-3,   "V")
    CALL CalcH("3-heptanone",                                                             "106-35-4", 9.1E-5,   "V")
    CALL CalcH("3-iodo-2-propynyl butylcarbamate",                                      "55406-53-6", 1.2E-7,   "V")
    CALL CalcH("3-methyl-1-butene",                                                       "563-45-1", 0.54,     "V")
    CALL CalcH("3-methyl-4-chlorophenol",                                                  "59-50-7", 2.4E-6,   "V")
    CALL CalcH("3-methylbutanal",                                                         "590-86-3", 4.0E-4,   "V")
    CALL CalcH("3-methylcholanthrene",                                                     "56-49-5", 5.2E-6,   "V")
    CALL CalcH("3-methylindole",                                                           "83-34-1", 2.1E-6,   "V")
    CALL CalcH("3-methylpentane",                                                          "96-14-0", 1.7,      "V")
    CALL CalcH("3-octanone",                                                              "106-68-3", 1.3E-4,   "V")
    CALL CalcH("4,4'-diaminodiphenylmethane",                                             "101-77-9", 5.3E-11,  "V")
    CALL CalcH("4-(1,1-dimethylpropyl)phenol",                                             "80-46-6", 2.0E-6,   "V")
    CALL CalcH("4-aminobenzoic acid",                                                     "150-13-0", 1.5E-10,  "V")
    CALL CalcH("4-aminophenol",                                                           "123-30-8", 3.6E-10,  "V")
    CALL CalcH("4-aminopyridine",                                                         "504-24-5", 2.3E-10,  "V")
    CALL CalcH("4-chloroaniline",                                                         "106-47-8", 3.1E-6,   "V")
    CALL CalcH("4-chlorophenol",                                                          "106-48-9", 6.3E-7,   "V")
    CALL CalcH("4-chlorotoluene",                                                         "106-43-4", 4.4E-3,   "V")
    CALL CalcH("4-hydroxyazobenzene",                                                    "1689-82-3", 6.7E-10,  "V")
    CALL CalcH("4-hydroxybenzoic acid",                                                    "99-96-7", 7.2E-12,  "V")
    CALL CalcH("4-methoxy-4-methyl-2-pentanone",                                          "107-70-0", 1.93E-6,  "V")
    CALL CalcH("4-methyl-2-pentyl acetate",                                               "108-84-9", 5.8E-4,   "V")
    CALL CalcH("4-methylbenzaldehyde",                                                    "104-87-0", 1.7E-5,   "V")
    CALL CalcH("4-methylbenzyl alcohol",                                                  "589-18-4", 1.1E-6,   "V")
    CALL CalcH("4-vinylcyclohexene",                                                      "100-40-3", 4.5E-2,   "V")
    CALL CalcH("4-vinyltoluene",                                                          "622-97-9", 3.2E-3,   "V")
    CALL CalcH("5-methyl-2-hexanone",                                                     "110-12-3", 1.6E-4,   "V")
    CALL CalcH("8-hydroxyquinoline",                                                      "148-24-3", 5.7E-7,   "V")
    CALL CalcH("abamectin",                                                             "71751-41-2", 1.4E-9,   "V")
    CALL CalcH("acenaphthylene",                                                          "208-96-8", 1.13E-5,  "V")
    CALL CalcH("acephate",                                                              "30560-19-1", 5E-13,    "V")
    CALL CalcH("acequinocyl",                                                           "57960-19-7", 9.7E-7,   "V")
    CALL CalcH("acetal",                                                                  "105-57-7", 9.75E-5,  "V")
    CALL CalcH("acetochlor",                                                            "34256-82-1", 2.7E-10,  "V")
    CALL CalcH("acetone cyanohydrin",                                                      "75-86-5", 1.23E-7,  "V")
    CALL CalcH("acetyl acetone",                                                          "123-54-6", 2.3E-6,   "V")
    CALL CalcH("acetylene",                                                                "74-86-2", 2.2E-2,   "V")
    CALL CalcH("acibenzolar-S-methyl",                                                 "135158-54-2", 1.2E-7,   "V")
    CALL CalcH("acrylamide",                                                               "79-06-1", 1.8E-9,   "V")
    CALL CalcH("adipic acid",                                                             "124-04-9", 4.7E-12,  "V")
    CALL CalcH("adiponitrile",                                                            "111-69-3", 1.21E-9,  "V")
    CALL CalcH("aldicarb",                                                                "116-06-3", 1.5E-9,   "V")
    CALL CalcH("allyl acetate",                                                           "591-87-7", 1.3E-4,   "V")
    CALL CalcH("allyl chloride",                                                          "107-05-1", 1.10E-2,  "V")
    CALL CalcH("allyl isothiocyanate",                                                     "57-06-7", 2.4E-4,   "V")
    CALL CalcH("allylamine",                                                              "107-11-9", 1.82E-5,  "V")
    CALL CalcH("alpha-methylstyrene",                                                      "98-83-9", 2.6E-3,   "V")
    CALL CalcH("alpha-pinene",                                                             "80-56-8", 0.29,     "V")
    CALL CalcH("alphacypermethrin",                                                     "67375-30-8", 9.5E-6,   "V")
    CALL CalcH("ametryne",                                                                "834-12-8", 2.4E-9,   "V")
    CALL CalcH("anthraquinone",                                                            "84-65-1", 2.35E-8,  "V")
    CALL CalcH("auramine",                                                                "492-80-8", 8.05E-8,  "V")
    CALL CalcH("azadirachtin",                                                          "11141-17-6", 2.8E-25,  "V")
    CALL CalcH("azinphos-ethyl",                                                         "2642-71-9", 9.9E-8,   "V")
    CALL CalcH("azinphos-methyl",                                                          "86-50-0", 2.9E-9,   "V")
    CALL CalcH("azobenzene",                                                              "103-33-3", 1.4E-5,   "V")
    CALL CalcH("azocyclotin",                                                           "41083-11-8", 2.15E-12, "V")
    CALL CalcH("azoxystrobin",                                                         "131860-33-8", 7.3E-14,  "V")
    CALL CalcH("barban",                                                                  "101-27-9", 1.2E-8,   "V")
    CALL CalcH("bendiocarb",                                                            "22781-23-3", 3.9E-8,   "V")
    CALL CalcH("benfluralin",                                                            "1861-40-1", 2.9E-4,   "V")
    CALL CalcH("bensulide",                                                               "741-58-2", 9.1E-9,   "V")
    CALL CalcH("bentazon",                                                              "25057-89-0", 2.2E-9,   "V")
    CALL CalcH("benzo(e)pyrene",                                                          "192-97-2", 3.0E-7,   "V")
    CALL CalcH("benzonitrile",                                                            "100-47-0", 5.21E-5,  "V")
    CALL CalcH("benzotrifluoride",                                                         "98-08-8", 0.017,    "V")
    CALL CalcH("benzyl acetate",                                                          "140-11-4", 1.1E-5,   "V")
    CALL CalcH("benzyl chloride",                                                         "100-44-7", 4.1E-4,   "V")
    CALL CalcH("benzyl cyanide",                                                          "140-29-4", 1.4E-4,   "V")
    CALL CalcH("benzyladenine",                                                          "1214-39-7", 8.84E-14, "V")
    CALL CalcH("betanal",                                                               "13684-63-4", 8.41E-13, "V")
    CALL CalcH("bifenox",                                                               "42576-02-3", 2.7E-6,   "V")
    CALL CalcH("bifenthrin",                                                            "82657-04-3", 1.0E-6,   "V")
    CALL CalcH("bis(2-chloro-1-methylethyl)ether",                                        "108-60-1", 7.4E-5,   "V")
    CALL CalcH("bis(2-chloroethoxy)methane",                                              "111-91-1", 3.9E-6,   "V")
    CALL CalcH("bis(2-chloroethyl)ether",                                                 "111-44-4", 2.9E-5,   "V")
    CALL CalcH("bis(2-ethylhexyl)phthalate",                                              "117-81-7", 2.7E-7,   "V")
    CALL CalcH("bis(tributyltin)oxide",                                                    "56-35-9", 1.3E-7,   "V")
    CALL CalcH("bisphenol A",                                                              "80-05-7", 4.0E-11,  "V")
    CALL CalcH("boric acid",                                                            "10043-35-3", 2.6E-12,  "V")
    CALL CalcH("bromacil",                                                                "314-40-9", 1.3E-10,  "V")
    CALL CalcH("bromadiolone",                                                          "28772-56-7", 8.9E-12,  "V")
    CALL CalcH("bromomuconazole",                                                      "116255-48-2", 8.2E-11,  "V")
    CALL CalcH("bromophos",                                                              "2104-96-3", 9.5E-5,   "V")
    CALL CalcH("bromopropylate",                                                        "18181-80-1", 4.6E-7,   "V")
    CALL CalcH("bromoxynil octanoate",                                                   "1689-99-2", 3.2E-5,   "V")
    CALL CalcH("bronopol",                                                                 "52-51-7", 1.3E-11,  "V")
    CALL CalcH("buprofezin",                                                            "69327-76-0", 4.2E-6,   "V")
    CALL CalcH("butacarb",                                                               "2655-19-8", 4.57E-8,  "V")
    CALL CalcH("butocarboxim",                                                          "34681-10-2", 5.70E-10, "V")
    CALL CalcH("butoxycarboxim",                                                        "34681-23-7", 2.8E-12,  "V")
    CALL CalcH("butralin",                                                              "33629-47-9", 4.9E-6,   "V")
    CALL CalcH("butyl benzyl phthalate",                                                   "85-68-7", 1.3E-6,   "V")
    CALL CalcH("butyl vinyl ether",                                                       "111-34-2", 2.2E-3,   "V")
    CALL CalcH("butylate",                                                               "2008-41-5", 8.5E-5,   "V")
    CALL CalcH("butyrolactone",                                                            "96-48-0", 5.3E-8,   "V")
    CALL CalcH("cadusafos",                                                             "95465-99-9", 1.3E-6,   "V")
    CALL CalcH("caffeine",                                                                 "58-08-2", 1.1E-11,  "V")
    CALL CalcH("camphene",                                                                 "79-92-5", 0.098,    "V")
    CALL CalcH("camphor",                                                                  "76-22-2", 8.1E-5,   "V")
    CALL CalcH("caprolactam",                                                             "105-60-2", 5.4E-11,  "V")
    CALL CalcH("captan",                                                                  "133-06-2", 7.0E-9,   "V")
    CALL CalcH("carbendazim",                                                           "10605-21-7", 2.1E-11,  "V")
    CALL CalcH("carbofuran",                                                             "1563-66-2", 4.5E-10,  "V")
    CALL CalcH("carbonyl sulfide",                                                        "463-58-1", 0.61,     "V")
    CALL CalcH("carbophenothion",                                                         "786-19-6", 2.0E-7,   "V")
    CALL CalcH("carboxin",                                                               "5234-68-4", 3.2E-10,  "V")
    CALL CalcH("carfentrazone-ethyl",                                                  "128639-02-1", 3.0E-9,   "V")
    CALL CalcH("catechol",                                                                "120-80-9", 1.2E-9,   "V")
    CALL CalcH("chloral hydrate",                                                         "302-17-0", 4.1E-9,   "V")
    CALL CalcH("chloranil",                                                               "118-75-2", 6.6E-9,   "V")
    CALL CalcH("chlorbromuron",                                                         "13360-45-7", 4.4E-9,   "V")
    CALL CalcH("chlorbufam",                                                             "1967-16-4", 8.6E-9,   "V")
    CALL CalcH("chlordimeform",                                                          "6164-98-3", 3.45E-7,  "V")
    CALL CalcH("chlorimuron-ethyl",                                                     "90982-32-4", 1.8E-15,  "V")
    CALL CalcH("chlormephos",                                                           "24934-91-6", 2.9E-4,   "V")
    CALL CalcH("chlormethazole",                                                        "20354-26-1", 2.29E-7,  "V")
    CALL CalcH("chlorobenzilate",                                                         "510-15-6", 7.2E-8,   "V")
    CALL CalcH("chlorobromomethane",                                                       "74-97-5", 0.00146,  "V")
    CALL CalcH("chlorofenvinphos",                                                        "470-90-6", 2.9E-8,   "V")
    CALL CalcH("chloroneb",                                                              "2675-77-6", 1.0E-4,   "V")
    CALL CalcH("chloropentafluoroethane",                                                  "76-15-3", 5.6,      "V")
    CALL CalcH("chloropropylate",                                                        "5836-10-2", 8.03E-9,  "V")
    CALL CalcH("chloroxuron",                                                            "1982-47-4", 4.03E-10, "V")
    CALL CalcH("chlorpropham",                                                            "101-21-3", 5.7E-7,   "V")
    CALL CalcH("chlorpyrifos-methyl",                                                    "5598-13-0", 2.4E-6,   "V")
    CALL CalcH("chlorthion",                                                              "500-28-7", 4.0E-8,   "V")
    CALL CalcH("chlortoluron",                                                          "15545-48-9", 1.4E-10,  "V")
    CALL CalcH("chrysene",                                                                "218-01-9", 9.9E-7,   "V")
    CALL CalcH("cinnamaldehyde",                                                          "104-55-2", 3.5E-6,   "V")
    CALL CalcH("ciodrin",                                                                "7700-17-6", 5.8E-9,   "V")
    CALL CalcH("clodinafop-propargyl",                                                 "105512-06-9", 2.8E-9,   "V")
    CALL CalcH("clofencet",                                                            "129025-54-3", 5.24E-14, "V", upperlimit=1)
    CALL CalcH("clonitralide",                                                           "1420-04-8", 3.8E-10,  "V", upperlimit=1)
    CALL CalcH("cocaine",                                                                  "50-36-2", 4.2E-11,  "V")
    CALL CalcH("coumaphos",                                                                "56-72-4", 1.1E-7,   "V")
    CALL CalcH("coumarin",                                                                 "91-64-5", 9.9E-8,   "V")
    CALL CalcH("coumatetralyl",                                                          "5836-29-3", 5.8E-14,  "V")
    CALL CalcH("cumene hydroperoxide",                                                     "80-15-9", 4.7E-8,   "V")
    CALL CalcH("cyanamide",                                                               "420-04-2", 2.66E-10, "V")
    CALL CalcH("cyanogen",                                                                "460-19-5", 5.4E-3,   "V")
    CALL CalcH("cyanophos",                                                              "2636-26-2", 5.5E-6,   "V")
    CALL CalcH("cyazofamid",                                                           "120116-88-3", 3.9E-7,   "V")
    CALL CalcH("cycloate",                                                               "1134-23-2", 5.2E-6,   "V")
    CALL CalcH("cyclohexanone",                                                           "108-94-1", 9.00E-6,  "V")
    CALL CalcH("cyclonite",                                                               "121-82-4", 2.0E-11,  "V")
    CALL CalcH("cyclopropane",                                                             "75-19-4", 0.79,     "V")
    CALL CalcH("cyclosarin",                                                              "329-99-7", 2.8E-6,   "V")
    CALL CalcH("cyfluthrin",                                                            "68359-37-5", 2.9E-8,   "V")
    CALL CalcH("cymoxanil",                                                             "57966-95-7", 3.3E-10,  "V")
    CALL CalcH("cypermethrin",                                                          "52315-07-8", 2.4E-7,   "V")
    CALL CalcH("cyproconazole",                                                         "94361-06-5", 7.1E-10,  "V")
    CALL CalcH("cyprodinil",                                                           "121552-61-2", 8.4E-8,   "V")
    CALL CalcH("cyromazine",                                                            "66215-27-8", 5.65E-14, "V")
    CALL CalcH("d-limonene",                                                             "5989-27-5", 0.025,    "V")
    CALL CalcH("decahydronaphthalene",                                                     "91-17-8", 0.47,     "V")
    CALL CalcH("decylbenzene",                                                            "104-72-3", 0.076,    "V")
    CALL CalcH("dehydroacetic acid",                                                      "520-45-6", 3.4E-7,   "V")
    CALL CalcH("delta-hexachlorocyclohexane",                                             "319-86-8", 4.3E-7,   "V")
    CALL CalcH("deltamethrin",                                                          "52918-63-5", 5.0E-6,   "V")
    CALL CalcH("demeton-S-methyl",                                                        "919-86-8", 2.7E-8,   "V")
    CALL CalcH("desmetryne",                                                             "1014-69-3", 4.8E-10,  "V")
    CALL CalcH("di-iso-amyl ether",                                                       "544-01-4", 1.5E-3,   "V")
    CALL CalcH("di-n-octyl phthalate",                                                    "117-84-0", 2.6E-6,   "V")
    CALL CalcH("dialifor",                                                              "10311-84-9", 1.8E-7,   "V")
    CALL CalcH("diallate",                                                               "2303-16-4", 3.8E-6,   "V")
    CALL CalcH("diallylamine",                                                            "124-02-7", 3.0E-5,   "V")
    CALL CalcH("dibenzo-p-dioxin",                                                        "262-12-4", 1.1E-4,   "V")
    CALL CalcH("dibenzofuran",                                                            "132-64-9", 2.1E-4,   "V")
    CALL CalcH("dibenzothiophene",                                                        "132-65-0", 3.4E-5,   "V")
    CALL CalcH("dibutyl ketone",                                                          "502-56-7", 2.8E-4,   "V")
    CALL CalcH("dibutyl sebacate",                                                        "109-43-3", 4.8E-8,   "V")
    CALL CalcH("dicamba",                                                                "1918-00-9", 4.38E-10, "V")
    CALL CalcH("dicapthon",                                                              "2463-84-5", 9.59E-8,  "V")
    CALL CalcH("dichlobenil",                                                            "1194-65-6", 1.0E-5,   "V")
    CALL CalcH("dichlofenthion",                                                           "97-17-6", 9.5E-4,   "V")
    CALL CalcH("dichlorofluoromethane",                                                    "75-43-4", 1.08E-02, "V")
    CALL CalcH("dichlorophene",                                                            "97-23-4", 1.2E-12,  "V")
    CALL CalcH("dichlorvos",                                                               "62-73-7", 5.7E-7,   "V")
    CALL CalcH("dicofol",                                                                 "115-32-2", 2.4E-7,   "V")
    CALL CalcH("dicyclohexyl phthalate",                                                   "84-61-7", 1.0E-7,   "V")
    CALL CalcH("diethanolamine",                                                          "111-42-2", 3.9E-11,  "V")
    CALL CalcH("diethyl ((diethanolamino)methyl) phosphonate",                           "2781-11-5", 1.6E-7,   "V")
    CALL CalcH("diethyl adipate",                                                         "141-28-6", 3.6E-6,   "V")
    CALL CalcH("diethyl carbonate",                                                       "105-58-8", 8.9E-5,   "V")
    CALL CalcH("diethyl phthalate",                                                        "84-66-2", 6.1E-7,   "V")
    CALL CalcH("diethylene glycol diethyl ether",                                         "112-36-7", 1.1E-7,   "V")
    CALL CalcH("diethylene glycol dimethyl ether",                                        "111-96-6", 5.2E-7,   "V")
    CALL CalcH("diethylene glycol dinitrate",                                             "693-21-0", 3.9E-7,   "V")
    CALL CalcH("diethylene glycol hexyl ether",                                           "112-59-4", 1.7E-8,   "V")
    CALL CalcH("diethylene glycol monobutyl ether acetate",                               "124-17-4", 3.5E-7,   "V")
    CALL CalcH("diethylene glycol monoethyl ether acetate",                               "112-15-2", 2.3E-8,   "V")
    CALL CalcH("diethylene glycol monoethyl ether",                                       "111-90-0", 2.2E-8,   "V")
    CALL CalcH("difethialone",                                                         "104653-34-1", 1.0E-6,   "V")
    CALL CalcH("diflubenzuron",                                                         "35367-38-5", 4.6E-9,   "V")
    CALL CalcH("difolatan",                                                              "2425-06-1", 2.7E-9,   "V")
    CALL CalcH("dihexyl phthalate",                                                        "84-75-3", 2.6E-5,   "V")
    CALL CalcH("dihydroxyacetone",                                                         "96-26-4", 5.5E-12,  "V")
    CALL CalcH("diisobutyl ketone",                                                       "108-83-8", 1.2E-4,   "V")
    CALL CalcH("diisobutyl phthalate",                                                     "84-69-5", 2.8E-6,   "V")
    CALL CalcH("diisodecyl phthalate",                                                  "26761-40-0", 1.1E-6,   "V")
    CALL CalcH("diisononyl phthalate",                                                  "28553-12-0", 1.5E-6,   "V")
    CALL CalcH("diisooctyl phthalate",                                                  "27554-26-3", 3.1E-5,   "V")
    CALL CalcH("diisopropyl methylphosphonate",                                          "1445-75-6", 4.4E-5,   "V")
    CALL CalcH("diisopropylnaphthalene",                                                "38640-62-9", 0.00127,  "V")
    CALL CalcH("dimefox",                                                                 "115-26-4", 2.2E-8,   "V")
    CALL CalcH("dimethoate",                                                               "60-51-5", 2.4E-10,  "V")
    CALL CalcH("dimethyl ether",                                                          "115-10-6", 5.9E-3,   "V")
    CALL CalcH("dimethyl hexahydroterephthalate",                                          "94-60-0", 9.7E-8,   "V")
    CALL CalcH("dimethyl phthalate",                                                      "131-11-3", 2.0E-7,   "V")
    CALL CalcH("dimethyl terephthalate",                                                  "120-61-6", 1.3E-4,   "V")
    CALL CalcH("dimethyl tetrachloroterephthalate",                                      "1861-32-1", 2.18E-6,  "V")
    CALL CalcH("dinitramine",                                                           "29091-05-2", 1.39E-6,  "V")
    CALL CalcH("dinocap",                                                               "39300-45-3", 4.79E-9,  "V")
    CALL CalcH("dinotefuran",                                                          "165252-70-0", 6.4E-14,  "V")
    CALL CalcH("diphenamid",                                                              "957-51-7", 2.42E-11, "V")
    CALL CalcH("diphenyl ether",                                                          "101-84-8", 0.00028,  "V")
    CALL CalcH("dipropyl ketone",                                                         "123-19-3", 2.4E-4,   "V")
    CALL CalcH("dipropylene glycol",                                                    "25265-71-8", 5.6E-9,   "V")
    CALL CalcH("disperse blue 1",                                                        "2475-45-8", 2.1E-7,   "V")
    CALL CalcH("disulfoton",                                                              "298-04-4", 2.2E-6,   "V")
    CALL CalcH("dithianon",                                                              "3347-22-6", 5.7E-11,  "V")
    CALL CalcH("diuron",                                                                  "330-54-1", 5.0E-10,  "V")
    CALL CalcH("dodecane",                                                                "112-40-3", 8.2,      "V")
    CALL CalcH("dyphonate",                                                               "944-22-9", 7.0E-6,   "V")
    CALL CalcH("dyrene",                                                                  "101-05-3", 2.8E-10,  "V")
    CALL CalcH("edifenphos",                                                            "17109-49-8", 7.6E-10,  "V")
    CALL CalcH("emamectin benzoate",                                                   "119791-41-2", 1.7E-9,   "V")
    CALL CalcH("endosulfan sulfate",                                                     "1031-07-8", 1.2E-11,  "V")
    CALL CalcH("endrin aldehyde",                                                        "7421-93-4", 4.2E-6,   "V")
    CALL CalcH("enflurane",                                                             "13838-16-9", 7.5E-3,   "V")
    CALL CalcH("epichlorohydrin",                                                         "106-89-8", 3.0E-5,   "V")
    CALL CalcH("EPN",                                                                    "2104-64-5", 4.44E-7,  "V")
    CALL CalcH("eptam",                                                                   "759-94-4", 1.6E-5,   "V")
    CALL CalcH("esfenvalerate",                                                         "66230-04-4", 4.1E-7,   "V")
    CALL CalcH("ethalfluralin",                                                         "55283-68-6", 1.3E-4,   "V")
    CALL CalcH("Ethane",                                                                   "74-84-0", 0.5,      "V")
    CALL CalcH("ethephon",                                                              "16672-87-0", 1.43E-13, "V")
    CALL CalcH("ethiofencarb",                                                          "29973-13-5", 1.2E-9,   "V")
    CALL CalcH("ethion",                                                                  "563-12-2", 3.8E-7,   "V")
    CALL CalcH("ethofumesate",                                                          "26225-79-6", 3.7E-8,   "V")
    CALL CalcH("ethoprop",                                                              "13194-48-4", 1.62E-7,  "V")
    CALL CalcH("ethyl acrylate",                                                          "140-88-5", 3.4E-4,   "V")
    CALL CalcH("ethyl butyrate",                                                          "105-54-4", 3.99E-4,  "V")
    CALL CalcH("ethyl carbamate",                                                          "51-79-6", 6.4E-8,   "V")
    CALL CalcH("ethyl cyanoacetate",                                                      "105-56-6", 2.9E-7,   "V")
    CALL CalcH("ethyl lactate",                                                            "97-64-3", 5.8E-7,   "V")
    CALL CalcH("ethyl methacrylate",                                                       "97-63-2", 5.7E-4,   "V")
    CALL CalcH("ethyl propionate",                                                        "105-37-3", 2.51E-4,  "V")
    CALL CalcH("ethyl vanillin",                                                          "121-32-4", 8.1E-10,  "V")
    CALL CalcH("ethylbis(2-chloroethyl)amine",                                            "538-07-8", 3.36E-4,  "V")
    CALL CalcH("ethylene cyanohydrin",                                                    "109-78-4", 4.3E-10,  "V")
    CALL CalcH("ethylene glycol diacetate",                                               "111-55-7", 8.4E-8,   "V")
    CALL CalcH("ethylene glycol dibutyl ether",                                           "112-48-1", 1E-5,     "V")
    CALL CalcH("ethylene glycol diethyl ether",                                           "629-14-1", 6.3E-5,   "V")
    CALL CalcH("ethylene glycol",                                                         "107-21-1", 2.1E-6,   "V")
    CALL CalcH("ethyleneimine",                                                           "151-56-4", 1.2E-5,   "V")
    CALL CalcH("etoxazole",                                                            "153233-91-1", 1.0E-7,   "V")
    CALL CalcH("etridiazole",                                                            "2593-15-9", 3.0E-5,   "V")
    CALL CalcH("etrimfos",                                                              "38260-54-7", 6.2E-7,   "V")
    CALL CalcH("eugenol",                                                                  "97-53-0", 1.92E-6,  "V")
    CALL CalcH("famoxadone",                                                           "131807-57-3", 4.6E-8,   "V")
    CALL CalcH("fenamiphos",                                                            "22224-92-6", 9.0E-9,   "V")
    CALL CalcH("fenazaquin",                                                           "120928-09-8", 1.0E-7,   "V")
    CALL CalcH("fenbutatin oxide",                                                      "13356-08-6", 2.0E-9,   "V")
    CALL CalcH("fenoxycarb",                                                            "79127-80-3", 4.3E-13,  "V")
    CALL CalcH("fenpropathrin",                                                         "39515-41-8", 1.8E-4,   "V")
    CALL CalcH("fenthion",                                                                 "55-38-9", 1.46E-6,  "V")
    CALL CalcH("fenvalerate",                                                           "51630-58-1", 3.4E-8,   "V")
    CALL CalcH("fipronil",                                                             "120068-37-3", 8.4E-10,  "V")
    CALL CalcH("flonicamid",                                                           "158062-67-0", 4.1E-13,  "V")
    CALL CalcH("florasulam",                                                           "145701-23-1", 5.7E-12,  "V")
    CALL CalcH("fluazifop-butyl",                                                       "69806-50-4", 2.1E-7,   "V")
    CALL CalcH("fluazinam",                                                             "79622-59-6", 2.5E-4,   "V")
    CALL CalcH("flubendiamide",                                                        "272451-65-7", 2.2E-4,   "V")
    CALL CalcH("fluchloralin",                                                          "33245-39-5", 1.5E-5,   "V")
    CALL CalcH("flucythrinate",                                                         "70124-77-5", 8.6E-8,   "V")
    CALL CalcH("flufenacet",                                                           "142459-58-3", 5.8E-9,   "V")
    CALL CalcH("flumioxazin",                                                          "103361-09-7", 6.28E-7,  "V")
    CALL CalcH("fluopicolide",                                                         "239110-15-7", 1.1E-9,   "V")
    CALL CalcH("fluoxastrobin",                                                        "361377-29-9", 1.1E-12,  "V")
    CALL CalcH("fluridone",                                                             "59756-60-4", 3.5E-9,   "V")
    CALL CalcH("fluroxypyr",                                                            "69377-81-7", 1.72E-11, "V")
    CALL CalcH("fluthiacet-methyl",                                                    "117337-19-6", 2.1E-9,   "V")
    CALL CalcH("folpet",                                                                  "133-07-3", 7.7E-8,   "V")
    CALL CalcH("foramsulfuron",                                                        "173159-57-4", 5.7E-17,  "V")
    CALL CalcH("formamide",                                                                "75-12-7", 1.4E-9,   "V")
    CALL CalcH("formothion",                                                             "2540-82-1", 1.1E-10,  "V")
    CALL CalcH("fosamine-ammonium",                                                     "25954-13-6", 5E-13,    "V")
    CALL CalcH("fosetyl-aluminum",                                                      "39148-24-8", 3.2E-15,  "V")
    CALL CalcH("fosthietan",                                                            "21548-32-3", 4.11E-11, "V")
    CALL CalcH("furan",                                                                   "110-00-9", 5.4E-3,   "V")
    CALL CalcH("furathiocarb",                                                          "65907-30-4", 1.3E-9,   "V")
    CALL CalcH("furfural",                                                                 "98-01-1", 3.8E-6,   "V")
    CALL CalcH("furfuryl alcohol",                                                         "98-00-0", 7.9E-8,   "V")
    CALL CalcH("galaxolide",                                                             "1222-05-5", 1.3E-4,   "V")
    CALL CalcH("gossyplure",                                                            "50933-33-0", 1.5E-4,   "V")
    CALL CalcH("heptenophos",                                                           "23560-59-0", 1.69E-7,  "V")
    CALL CalcH("hexabromobiphenyl",                                                     "36355-01-8", 4.3E-6,   "V")
    CALL CalcH("hexabromocyclododecane",                                                 "3194-55-6", 6.1E-4,   "V")
    CALL CalcH("hexaflumuron",                                                          "86479-06-3", 1.0E-5,   "V")
    CALL CalcH("hexazinone",                                                            "51235-04-2", 2.26E-12, "V")
    CALL CalcH("hexythiazox",                                                           "78587-05-0", 2.37E-8,  "V")
    CALL CalcH("hydramethylnon",                                                        "67485-29-4", 2.2E-6,   "V")
    CALL CalcH("hydrazine",                                                               "302-01-2", 6.1E-7,   "V")
    CALL CalcH("hydroquinone",                                                            "123-31-9", 3.8E-11,  "V")
    CALL CalcH("ibuprofen",                                                             "15687-27-1", 1.5E-7,   "V")
    CALL CalcH("imazalil",                                                              "35554-44-0", 2.6E-9,   "V")
    CALL CalcH("imazamethabenz-methyl",                                                 "81405-85-8", 3.86E-12, "V")
    CALL CalcH("indole",                                                                  "120-72-9", 5.3E-7,   "V")
    CALL CalcH("indoxacarb",                                                           "173584-44-6", 6.6E-10,  "V")
    CALL CalcH("iodobenzene",                                                             "591-50-4", 8.4E-4,   "V")
    CALL CalcH("iodofenphos",                                                           "18181-70-9", 4.5E-6,   "V")
    CALL CalcH("isazofos",                                                              "42509-80-8", 5.21E-7,  "V")
    CALL CalcH("isobutane",                                                                "75-28-5", 1.19,     "V")
    CALL CalcH("isobutyl acrylate",                                                       "106-63-8", 6.0E-4,   "V")
    CALL CalcH("isobutyl methacrylate",                                                    "97-86-9", 5.2E-4,   "V")
    CALL CalcH("isobutyraldehyde",                                                         "78-84-2", 1.8E-4,   "V")
    CALL CalcH("isoeugenol",                                                               "97-54-1", 3.6E-6,   "V")
    CALL CalcH("isooctyl alcohol",                                                       "1653-40-3", 9.2E-5,   "V")
    CALL CalcH("isopentane",                                                               "78-78-4", 1.4,      "V")
    CALL CalcH("isophorone",                                                               "78-59-1", 6.6E-6,   "V")
    CALL CalcH("isoprene",                                                                 "78-79-5", 0.077,    "V")
    CALL CalcH("isopropyl bromide",                                                        "75-26-3", 0.011,    "V")
    CALL CalcH("isopropyl ether",                                                         "108-20-3", 2.3E-3,   "V")
    CALL CalcH("isopropyl phenylcarbmate",                                                "122-42-9", 1.8E-7,   "V")
    CALL CalcH("isosystox",                                                               "126-75-0", 4.9E-8,   "V")
    CALL CalcH("kepone",                                                                  "143-50-0", 5.4E-8,   "V")
    CALL CalcH("kresoxim-methyl",                                                      "143390-89-0", 3.6E-9,   "V")
    CALL CalcH("lactic acid",                                                              "50-21-5", 8.1E-8,   "V")
    CALL CalcH("lactofen",                                                              "77501-63-4", 4.2E-7,   "V")
    CALL CalcH("leptophos",                                                             "21609-90-5", 2.65E-6,  "V")
    CALL CalcH("lewisite",                                                                "541-25-3", 3.2E-4,   "V")
    CALL CalcH("limonene",                                                                "138-86-3", 0.032,    "V")
    CALL CalcH("linoleic acid",                                                            "60-33-3", 2.0E-7,   "V")
    CALL CalcH("m-cymene",                                                                "535-77-3", 7.15E-3,  "V")
    CALL CalcH("m-terphenyl",                                                              "92-06-8", 3.5E-6,   "V")
    CALL CalcH("mandipropamid",                                                        "374726-62-2", 9.2E-10,  "V")
    CALL CalcH("melamine",                                                                "108-78-1", 1.8E-14,  "V")
    CALL CalcH("menazon",                                                                  "78-57-9", 1.5E-9,   "V")
    CALL CalcH("mesityl oxide",                                                           "141-79-7", 3.67E-5,  "V")
    CALL CalcH("mesosulfuron-methyl",                                                  "208465-21-8", 1.1E-16,  "V")
    CALL CalcH("metalaxyl",                                                             "57837-19-1", 3.0E-9,   "V")
    CALL CalcH("metaldehyde",                                                             "108-62-3", 5.2E-5,   "V")
    CALL CalcH("methacrolein",                                                             "78-85-3", 1.9E-4,   "V")
    CALL CalcH("methane",                                                                  "74-82-8", 0.66,     "V")
    CALL CalcH("methapyrilene",                                                            "91-80-5", 2.74E-7,  "V")
    CALL CalcH("methenamine",                                                             "100-97-0", 1.6E-9,   "V")
    CALL CalcH("methidathion",                                                            "950-37-8", 7.2E-9,   "V")
    CALL CalcH("methomyl",                                                              "16752-77-5", 1.9E-11,  "V")
    CALL CalcH("methoprene",                                                            "40596-69-8", 6.9E-6,   "V")
    CALL CalcH("methoprotryne",                                                           "841-06-5", 3.18E-10, "V")
    CALL CalcH("methyl acetoacetate",                                                     "105-45-3", 2.7E-7,   "V")
    CALL CalcH("methyl acrylate",                                                          "96-33-3", 2.0E-4,   "V")
    CALL CalcH("methyl anthranilate",                                                     "134-20-3", 1.9E-6,   "V")
    CALL CalcH("methyl benzoate",                                                          "93-58-3", 3.24E-5,  "V")
    CALL CalcH("methyl cellosolve acetate",                                               "110-49-6", 1.1E-6,   "V")
    CALL CalcH("methyl chloroacetate",                                                     "96-34-4", 2.4E-4,   "V")
    CALL CalcH("methyl isobutyl ketone",                                                  "108-10-1", 1.4E-4,   "V")
    CALL CalcH("methyl isopropyl ketone",                                                 "563-80-4", 1.13E-4,  "V")
    CALL CalcH("methyl isothiocyanate",                                                   "556-61-6", 4.5E-5,   "V")
    CALL CalcH("methyl mercaptan",                                                         "74-93-1", 0.0031,   "V")
    CALL CalcH("methyl methacrylate",                                                      "80-62-6", 3.2E-4,   "V")
    CALL CalcH("methyl salicylate",                                                       "119-36-8", 9.3E-7,   "V")
    CALL CalcH("methylacrylonitrile",                                                     "126-98-7", 2.47E-4,  "V")
    CALL CalcH("methylal",                                                                "109-87-5", 1.63E-4,  "V")
    CALL CalcH("methylcyclopentane",                                                       "96-37-7", 0.369,    "V")
    CALL CalcH("methyleugenol",                                                            "93-15-2", 5.6E-6,   "V")
    CALL CalcH("methylhydrazine",                                                          "60-34-4", 3E-6,     "V")
    CALL CalcH("metobromuron",                                                           "3060-89-7", 3.1E-9,   "V")
    CALL CalcH("metofluthrin",                                                         "240494-70-6", 9.5E-6,   "V")
    CALL CalcH("metolcarb",                                                              "1129-41-5", 8.4E-10,  "V")
    CALL CalcH("metribuzin",                                                            "21087-64-9", 1.2E-10,  "V")
    CALL CalcH("metsulfuron methyl",                                                    "74223-64-6", 1.32E-16, "V")
    CALL CalcH("mipafox",                                                                 "371-86-8", 3.0E-9,   "V")
    CALL CalcH("mitotane",                                                                 "53-19-0", 8.2E-6,   "V")
    CALL CalcH("monocrotophos",                                                          "6923-22-4", 6.5E-13,  "V")
    CALL CalcH("monolinuron",                                                            "1746-81-2", 4.6E-8,   "V")
    CALL CalcH("monuron",                                                                 "150-68-5", 5.72E-10, "V")
    CALL CalcH("morpholine",                                                              "110-91-8", 1.2E-6,   "V")
    CALL CalcH("myclobutanil",                                                          "88671-89-0", 4.3E-9,   "V")
    CALL CalcH("myrcene",                                                                 "123-35-3", 0.0916,   "V")
    CALL CalcH("N,N-diethylaniline",                                                       "91-66-7", 1.9E-4,   "V")
    CALL CalcH("N,N-dimethyl-p-(phenylazo)aniline",                                        "60-11-7", 7.1E-9,   "V")
    CALL CalcH("N,N-dimethylaniline",                                                     "121-69-7", 5.7E-5,   "V")
    CALL CalcH("N,N-diphenylamine",                                                       "122-39-4", 2.7E-6,   "V")
    CALL CalcH("N-(2-ethyl(3-methyl-4-nitrosophenyl)amino)ethyl)-methanesulfonamide",   "56046-62-9", 1E-10,    "V")
    CALL CalcH("n-amyl mercaptan",                                                        "110-66-7", 1.2E-2,   "V")
    CALL CalcH("n-butane",                                                                "106-97-8", 0.95,     "V")
    CALL CalcH("n-butyl acrylate",                                                        "141-32-2", 4.6E-4,   "V")
    CALL CalcH("n-butyl glycidyl ether",                                                 "2426-08-6", 2.5E-5,   "V")
    CALL CalcH("n-butyl lactate",                                                         "138-22-7", 2.0E-6,   "V")
    CALL CalcH("n-butyl methacrylate",                                                     "97-88-1", 5.0E-4,   "V")
    CALL CalcH("n-butylbenzene",                                                          "104-51-8", 0.016,    "V")
    CALL CalcH("n-decane",                                                                "124-18-5", 5.15,     "V")
    CALL CalcH("n-heptane",                                                               "142-82-5", 1.8,      "V")
    CALL CalcH("n-hexane",                                                                "110-54-3", 1.80,     "V")
    CALL CalcH("N-methylacetamide",                                                        "79-16-3", 4.23E-8,  "V")
    CALL CalcH("N-methylaniline",                                                         "100-61-8", 1.14E-5,  "V")
    CALL CalcH("N-methyldiethanolamine",                                                  "105-59-9", 3.1E-11,  "V")
    CALL CalcH("N-methylformamide",                                                       "123-39-7", 2.0E-8,   "V")
    CALL CalcH("N-nitroso-N-methylurethane",                                              "615-53-2", 5.5E-6,   "V")
    CALL CalcH("n-nonane",                                                                "111-84-2", 3.4,      "V")
    CALL CalcH("n-octane",                                                                "111-65-9", 3.2,      "V")
    CALL CalcH("n-pentadecane",                                                           "629-62-9", 13.,      "V")
    CALL CalcH("N-phenyl-1-naphthylamine",                                                 "90-30-2", 1.4E-7,   "V")
    CALL CalcH("n-tetradecane",                                                           "629-59-4", 9.2,      "V")
    CALL CalcH("n-undecane",                                                             "1120-21-4", 1.9,      "V")
    CALL CalcH("naled",                                                                   "300-76-5", 6.5E-5,   "V")
    CALL CalcH("napropamide",                                                           "15299-99-7", 8.41E-10, "V")
    CALL CalcH("neopentyl alcohol",                                                        "75-84-3", 5.3E-5,   "V")
    CALL CalcH("niclosamide",                                                              "50-65-7", 6.5E-10,  "V")
    CALL CalcH("nitralin",                                                               "4726-14-1", 7E-9,     "V")
    CALL CalcH("nitrofen",                                                               "1836-75-5", 3.0E-6,   "V")
    CALL CalcH("nitroglycerin",                                                            "55-63-0", 4.3E-8,   "V")
    CALL CalcH("norflurazon",                                                           "27314-13-2", 3.4E-10,  "V")
    CALL CalcH("o-benzyl-p-chlorophenol",                                                 "120-32-1", 2.7E-9,   "V")
    CALL CalcH("o-cymene",                                                                "527-84-4", 0.011,    "V")
    CALL CalcH("o-phenylenediamine",                                                       "95-54-5", 7.2E-9,   "V")
    CALL CalcH("o-phenylphenol",                                                           "90-43-7", 1.05E-6,  "V")
    CALL CalcH("o-terphenyl",                                                              "84-15-1", 6.1E-5,   "V")
    CALL CalcH("octabromobiphenyl",                                                     "27858-07-7", 2.4E-9,   "V")
    CALL CalcH("octabromodiphenyl ether",                                               "32536-52-0", 2.7E-7,   "V")
    CALL CalcH("octachlorodibenzo-p-dioxin",                                             "3268-87-9", 6.7E-6,   "V")
    CALL CalcH("octachloronaphthalene",                                                  "2234-13-1", 0.000731, "V")
    CALL CalcH("octafluoropropane",                                                        "76-19-7", 33.,      "V")
    CALL CalcH("orthosulfamuron",                                                      "213464-77-8", 7.4E-10,  "V")
    CALL CalcH("oryzalin",                                                              "19044-88-3", 1.9E-9,   "V")
    CALL CalcH("oxadiazon",                                                             "19666-30-9", 7E-8,     "V")
    CALL CalcH("oxamyl",                                                                "23135-22-0", 2.37E-10, "V")
    CALL CalcH("oxycarboxin",                                                            "5259-88-1", 1.1E-11,  "V")
    CALL CalcH("oxyfluorfen",                                                           "42874-03-3", 8.2E-7,   "V")
    CALL CalcH("oxythioquinox",                                                          "2439-01-2", 6.2E-8,   "V")
    CALL CalcH("p-cymene",                                                                 "99-87-6", 0.011,    "V")
    CALL CalcH("p-dibromobenzene",                                                        "106-37-6", 8.93E-4,  "V")
    CALL CalcH("p-ethylphenol",                                                           "123-07-9", 1.2E-6,   "V")
    CALL CalcH("p-nonylphenol",                                                           "104-40-5", 3.4E-5,   "V")
    CALL CalcH("paraldehyde",                                                             "123-63-7", 4E-5,     "V")
    CALL CalcH("paraquat",                                                               "4685-14-7", 4.09E-14, "V", upperlimit=1)
    CALL CalcH("pebulate",                                                               "1114-71-2", 2.4E-4,   "V")
    CALL CalcH("penoxsulam",                                                           "219714-96-2", 1.1E-18,  "V")
    CALL CalcH("pentachlorodibenzo-p-dioxin",                                           "36088-22-9", 2.2E-6,   "V")
    CALL CalcH("pentachloroethane",                                                        "76-01-7", 1.9E-3,   "V")
    CALL CalcH("pentachloronitrobenzene",                                                  "82-68-8", 4.4E-5,   "V")
    CALL CalcH("pentaerythritol tetranitrate",                                             "78-11-5", 1.3E-9,   "V")
    CALL CalcH("permethrin",                                                            "52645-53-1", 2.4E-6,   "V")
    CALL CalcH("phenacetin",                                                               "62-44-2", 2.1E-10,  "V")
    CALL CalcH("phenetole",                                                               "103-73-1", 4.4E-4,   "V")
    CALL CalcH("phenthoate",                                                             "2597-03-7", 5.48E-9,  "V")
    CALL CalcH("phenyl ethyl ketone",                                                      "93-55-0", 1.3E-4,   "V")
    CALL CalcH("phenyl glycidyl ether",                                                   "122-60-1", 8.2E-7,   "V")
    CALL CalcH("phenylhydrazine",                                                         "100-63-0", 2.9E-8,   "V")
    CALL CalcH("phorate",                                                                 "298-02-2", 4.73E-6,  "V")
    CALL CalcH("phosmet",                                                                 "732-11-6", 8.4E-9,   "V")
    CALL CalcH("phthalic acid",                                                            "88-99-3", 2E-11,    "V")
    CALL CalcH("pinoxaden",                                                            "243973-20-8", 9.1E-12,  "V")
    CALL CalcH("piperonal",                                                               "120-57-0", 5.6E-7,   "V")
    CALL CalcH("pirimicarb",                                                            "23103-98-2", 8.4E-10,  "V")
    CALL CalcH("pirimiphos-ethyl",                                                      "23505-41-1", 5.5E-5,   "V")
    CALL CalcH("pirimiphos-methyl",                                                     "29232-93-7", 6.0E-7,   "V")
    CALL CalcH("prallethrin",                                                           "23031-36-9", 1.6E-6,   "V")
    CALL CalcH("profenofos",                                                            "41198-08-7", 2.2E-8,   "V")
    CALL CalcH("profluralin",                                                           "26399-36-0", 2.9E-4,   "V")
    CALL CalcH("promecarb",                                                              "2631-37-0", 9.0E-8,   "V")
    CALL CalcH("prometon",                                                               "1610-18-0", 9.1E-10,  "V")
    CALL CalcH("prometryne",                                                             "7287-19-6", 1.2E-8,   "V")
    CALL CalcH("pronamide",                                                             "23950-58-5", 1.9E-6,   "V")
    CALL CalcH("propachlor",                                                             "1918-16-7", 3.6E-7,   "V")
    CALL CalcH("propane",                                                                  "74-98-6", 7.07E-1,  "V")
    CALL CalcH("propanil",                                                                "709-98-8", 1.7E-9,   "V")
    CALL CalcH("propaphos",                                                              "7292-16-2", 2.9E-9,   "V")
    CALL CalcH("propargite",                                                             "2312-35-8", 6.4E-7,   "V")
    CALL CalcH("propargyl alcohol",                                                       "107-19-7", 1.1E-6,   "V")
    CALL CalcH("propazine",                                                               "139-40-2", 4.6E-9,   "V")
    CALL CalcH("propetamphos",                                                          "31218-83-4", 4.8E-8,   "V")
    CALL CalcH("propiconazole",                                                         "60207-90-1", 9.1E-10,  "V")
    CALL CalcH("propoxur",                                                                "114-26-1", 3.35E-9,  "V")
    CALL CalcH("propyl carbamate",                                                        "627-12-3", 9.8E-8,   "V")
    CALL CalcH("propylene carbonate",                                                     "108-32-7", 3.4E-8,   "V")
    CALL CalcH("propylene glycol mono-t-butyl ether",                                   "57018-52-7", 4.73E-6,  "V")
    CALL CalcH("propylene glycol phenyl ether",                                           "770-35-4", 2.9E-8,   "V")
    CALL CalcH("propylene glycol",                                                         "57-55-6", 1.3E-8,   "V")
    CALL CalcH("prothioconazole",                                                      "178928-70-6", 4.5E-10,  "V")
    CALL CalcH("pymetrozine",                                                          "123312-89-0", 3.0E-11,  "V")
    CALL CalcH("pyrasulfotole",                                                        "365400-11-9", 1.4E-14,  "V")
    CALL CalcH("pyrazon",                                                                "1698-60-8", 3.33E-10, "V")
    CALL CalcH("pyrethrin I",                                                             "121-21-1", 4.4E-5,   "V")
    CALL CalcH("pyrethrin II",                                                            "121-29-9", 2.2E-8,   "V")
    CALL CalcH("pyridaben",                                                             "96489-71-3", 4.7E-5,   "V")
    CALL CalcH("pyridalyl",                                                            "179101-81-6", 2.0E-12,  "V")
    CALL CalcH("pyrogallic acid",                                                          "87-66-1", 1.57E-10, "V")
    CALL CalcH("quinoline",                                                                "91-22-5", 1.7E-6,   "V")
    CALL CalcH("quizalofop-ethyl",                                                      "76578-14-8", 1.1E-8,   "V")
    CALL CalcH("randox",                                                                   "93-71-0", 1.07E-7,  "V")
    CALL CalcH("resmethrin",                                                            "10453-86-8", 1.3E-7,   "V")
    CALL CalcH("resorcinol",                                                              "108-46-3", 9.9E-11,  "V")
    CALL CalcH("rimsulfuron",                                                          "122931-48-0", 6.4E-10,  "V")
    CALL CalcH("sarin",                                                                   "107-44-8", 5.7E-7,   "V")
    CALL CalcH("sec-butyl acetate",                                                       "105-46-4", 4.2E-4,   "V")
    CALL CalcH("sec-butyl mercaptan",                                                     "513-53-1", 7.3E-3,   "V")
    CALL CalcH("sec-butylbenzene",                                                        "135-98-8", 0.018,    "V")
    CALL CalcH("siduron",                                                                "1982-49-6", 6.8E-11,  "V")
    CALL CalcH("simazine",                                                                "122-34-9", 9.4E-10,  "V")
    CALL CalcH("soman",                                                                    "96-64-0", 4.7E-6,   "V")
    CALL CalcH("spirodiclofen",                                                        "148477-71-8", 5.7E-8,   "V")
    CALL CalcH("spiromesifen",                                                         "283594-90-1", 5.6E-4,   "V")
    CALL CalcH("styrene-7,8-oxide",                                                        "96-09-3", 1.6E-5,   "V")
    CALL CalcH("succinic acid",                                                           "110-15-6", 3.6E-13,  "V")
    CALL CalcH("succinonitrile",                                                          "110-61-2", 6.5E-9,   "V")
    CALL CalcH("sulfallate",                                                               "95-06-7", 6.5E-6,   "V")
    CALL CalcH("sulfentrazone",                                                        "122836-35-5", 6.37E-13, "V")
    CALL CalcH("sulfosulfuron",                                                        "141776-32-1", 2.3E-11,  "V")
    CALL CalcH("sulfotep",                                                               "3689-24-5", 4.4E-6,   "V")
    CALL CalcH("sulprofos",                                                             "35400-43-2", 8.6E-7,   "V")
    CALL CalcH("systox",                                                                 "8065-48-3", 1.8E-7,   "V")
    CALL CalcH("tabun",                                                                    "77-81-6", 1.5E-7,   "V")
    CALL CalcH("tebuconazole",                                                         "107534-96-3", 1.4E-10,  "V")
    CALL CalcH("tebufenozide",                                                         "112410-23-8", 1.3E-8,   "V")
    CALL CalcH("tebufenpyrad",                                                         "119168-77-3", 1.2E-8,   "V", upperlimit=1)
    CALL CalcH("tebupirimfos",                                                          "96182-53-5", 2.8E-6,   "V")
    CALL CalcH("tefluthrin",                                                            "79538-32-2", 1.6E-3,   "V")
    CALL CalcH("tembotrione",                                                          "335104-84-2", 1.7E-15,  "V")
    CALL CalcH("terbacil",                                                               "5902-51-2", 1.9E-10,  "V")
    CALL CalcH("terbufos",                                                              "13071-79-9", 0.000024, "V")
    CALL CalcH("terbuthylazine",                                                         "5915-41-3", 2.3E-8,   "V")
    CALL CalcH("terbutryne",                                                              "886-50-0", 2.1E-8,   "V")
    CALL CalcH("terpinolene",                                                             "586-62-9", 0.014,    "V")
    CALL CalcH("tert-butyl bromide",                                                      "507-19-7", 0.041,    "V")
    CALL CalcH("tert-butylbenzene",                                                        "98-06-6", 1.32E-2,  "V")
    CALL CalcH("tert-butylphenyl diphenyl phosphate",                                   "56803-37-3", 2.2E-7,   "V")
    CALL CalcH("tetrabromomethane",                                                       "558-13-4", 4.9E-4,   "V")
    CALL CalcH("tetrachlorvinphos",                                                     "22248-79-9", 1.8E-9,   "V")
    CALL CalcH("tetraconazole",                                                        "112281-77-3", 4.2E-9,   "V")
    CALL CalcH("tetradifon",                                                              "116-29-0", 1.44E-9,  "V")
    CALL CalcH("tetraethyl pyrophosphate",                                                "107-49-3", 2.2E-10,  "V")
    CALL CalcH("tetrafluoroethylene",                                                     "116-14-3", 0.63,     "V")
    CALL CalcH("tetramethrin",                                                           "7696-12-0", 1.7E-6,   "V")
    CALL CalcH("tetramethyl lead",                                                         "75-74-1", 0.61,     "V")
    CALL CalcH("tetramethylsilane",                                                        "75-76-3", 4.25,     "V")
    CALL CalcH("tetranitromethane",                                                       "509-14-8", 0.0024,   "V")
    CALL CalcH("thiabendazole",                                                           "148-79-8", 2.1E-11,  "V")
    CALL CalcH("thiacloprid",                                                          "111988-49-9", 1.1E-14,  "V")
    CALL CalcH("thiamethoxam",                                                         "153719-23-4", 4.6E-15,  "V")
    CALL CalcH("thiazopyr",                                                            "117718-60-2", 4.7E-7,   "V")
    CALL CalcH("thidiazuron",                                                           "51707-55-2", 3.3E-13,  "V")
    CALL CalcH("thifensulfuron methyl",                                                 "79277-27-3", 2.92E-14, "V")
    CALL CalcH("thiobencarb",                                                           "28249-77-6", 2.7E-7,   "V")
    CALL CalcH("thiodicarb",                                                            "59669-26-0", 9E-7,     "V")
    CALL CalcH("thiometon",                                                               "640-15-3", 2.8E-5,   "V")
    CALL CalcH("thiophanate methyl",                                                    "23564-05-8", 1.2E-9,   "V")
    CALL CalcH("thiophene",                                                               "110-02-1", 2.9E-3,   "V")
    CALL CalcH("thiophenol",                                                              "108-98-5", 3.4E-4,   "V")
    CALL CalcH("thiourea",                                                                 "62-56-6", 2.0E-9,   "V")
    CALL CalcH("tolylfluanid",                                                            "731-27-1", 7.6E-7,   "V")
    CALL CalcH("tonalide",                                                              "21145-77-7", 1.4E-4,   "V")
    CALL CalcH("tralkoxydim",                                                           "87820-88-0", 2.41E-10, "V")
    CALL CalcH("tralomethrin",                                                          "66841-25-6", 3.9E-10,  "V")
    CALL CalcH("trans-1,2-diphenylethylene",                                              "103-30-0", 7.2E-4,   "V")
    CALL CalcH("trans-crotonic acid",                                                    "3724-65-0", 2.4E-7,   "V")
    CALL CalcH("triacetin",                                                               "102-76-1", 1.2E-8,   "V")
    CALL CalcH("triadimefon",                                                           "43121-43-3", 8.10E-11, "V")
    CALL CalcH("triadimenol",                                                           "55219-65-3", 1.3E-12,  "V")
    CALL CalcH("triallate",                                                              "2303-17-5", 1.2E-5,   "V")
    CALL CalcH("triallylamine",                                                           "102-70-5", 2.63E-4,  "V")
    CALL CalcH("triazophos",                                                            "24017-47-8", 3.1E-8,   "V")
    CALL CalcH("tributyl phosphate",                                                      "126-73-8", 1.4E-6,   "V")
    CALL CalcH("trichlorfon",                                                              "52-68-6", 1.7E-11,  "V")
    CALL CalcH("trichloronate",                                                           "327-98-0", 1.1E-5,   "V")
    CALL CalcH("triclopyr",                                                             "55335-06-3", 9.7E-10,  "V")
    CALL CalcH("tricresyl phosphate",                                                    "1330-78-5", 8.1E-7,   "V")
    CALL CalcH("triethanolamine",                                                         "102-71-6", 7.1E-13,  "V")
    CALL CalcH("triforine",                                                             "26644-46-2", 3.8E-9,   "V")
    CALL CalcH("trioctyl phosphate",                                                       "78-42-2", 7.9E-8,   "V")
    CALL CalcH("triphenyl phosphate",                                                     "115-86-6", 3.3E-6,   "V")
    CALL CalcH("tripropylamine",                                                          "102-69-2", 3.8E-4,   "V")
    CALL CalcH("tris(2,3-dibromo-1-propyl) phosphate",                                    "126-72-7", 2.6E-5,   "V")
    CALL CalcH("tris(2-chloroethyl)amine",                                                "555-77-1", 1.85E-5,  "V")
    CALL CalcH("tris(2-chloroethyl)phosphate",                                            "115-96-8", 3.3E-6,   "V")
    CALL CalcH("urea",                                                                     "57-13-6", 1.74E-12, "V")
    CALL CalcH("vanillin",                                                                "121-33-5", 2.1E-9,   "V")
    CALL CalcH("vernolate",                                                              "1929-77-7", 3.1E-5,   "V")
    CALL CalcH("vinclozolin",                                                           "50471-44-8", 1.7E-8,   "V")
    CALL CalcH("vinyl acetate",                                                           "108-05-4", 5.1E-4,   "V")
    CALL CalcH("vinyl methyl ether",                                                      "107-25-5", 6.7E-3,   "V")
    CALL CalcH("VX",                                                                    "50782-69-9", 1.08E-8,  "V")
    CALL CalcH("warfarin",                                                                 "81-81-2", 2.7E-10,  "V")
    CALL CalcH("ziram",                                                                   "137-30-4", 6.2E-10,  "V")

    ! type = "X"
    CALL CalcH("peracetic acid",                                "79-21-0", 2.14E-6,  "X", "258")
    CALL CalcH("2-chloronaphthalene",                           "91-58-7", 0.00032,  "X", "479")
    CALL CalcH("isobutyl alcohol",                              "78-83-1", 9.78E-6,  "X", "483")
    CALL CalcH("methyl ethyl ketone",                           "78-93-3", 5.69E-5,  "X", "483")
    CALL CalcH("n-propanol",                                    "71-23-8", 7.41E-6,  "X", "483")
    CALL CalcH("sec-butyl alcohol",                             "78-92-2", 9.06E-6,  "X", "483")
    CALL CalcH("benzaldehyde",                                 "100-52-7", 2.67E-5,  "X", "484")
    CALL CalcH("chloral",                                       "75-87-6", 2.91E-9,  "X", "484")
    CALL CalcH("formaldehyde",                                  "50-00-0", 3.4E-7,   "X", "484")
    CALL CalcH("glyoxal",                                      "107-22-2", 3.33E-9,  "X", "484")
    CALL CalcH("methyl glyoxal",                                "78-98-8", 2.70E-7,  "X", "484")
    CALL CalcH("benzene",                                       "71-43-2", 5.56E-3,  "X", "490")
    CALL CalcH("toluene",                                      "108-88-3", 6.64E-3,  "X", "490")
    CALL CalcH("n-pentanoic acid",                             "109-52-4", 4.72E-7,  "X", "493")
    CALL CalcH("1-chloro-2-propanone",                          "78-95-5", 1.65E-5,  "X", "495")
    CALL CalcH("acetophenone",                                  "98-86-2", 1.04E-5,  "X", "495")
    CALL CalcH("diacetyl",                                     "431-03-8", 1.33E-5,  "X", "495")
    CALL CalcH("2-chloro-1,1,1,2-tetrafluoroethane",          "2837-89-0", 0.714,    "X", "498")
    CALL CalcH("isobutyl acetate",                             "110-19-0", 4.54E-4,  "X", "510")
    CALL CalcH("isopropyl acetate",                            "108-21-4", 2.78E-4,  "X", "510")
    CALL CalcH("n-hexyl acetate",                              "142-92-7", 5.3E-4,   "X", "510")
    CALL CalcH("1-butyl mercaptan",                            "109-79-5", 4.54E-3,  "X", "522")
    CALL CalcH("diethyl sulfide",                              "352-93-2", 1.68E-3,  "X", "522")
    CALL CalcH("propyl mercaptan",                             "107-03-9", 4.08E-3,  "X", "522")
    CALL CalcH("1,1,1-trichloroethane",                         "71-55-6", 0.0172,   "X", "532")
    CALL CalcH("1,1-dichloroethane",                            "75-34-3", 5.62E-3,  "X", "532")
    CALL CalcH("1,1-dichloroethylene",                          "75-35-4", 2.61E-2,  "X", "532")
    CALL CalcH("1,2-dichloroethylene",                         "540-59-0", 4.08E-3,  "X", "532")
    CALL CalcH("chloroform",                                    "67-66-3", 3.67E-3,  "X", "532")
    CALL CalcH("cis-1,2-dichloroethylene",                     "156-59-2", 4.08E-3,  "X", "532")
    CALL CalcH("ethyl chloride",                                "75-00-3", 1.11E-2,  "X", "532")
    CALL CalcH("methyl chloride",                               "74-87-3", 8.82E-3,  "X", "532")
    CALL CalcH("tetrachloroethylene",                          "127-18-4", 0.0177,   "X", "532")
    CALL CalcH("trans-1,2-dichloroethylene",                   "156-60-5", 9.38E-3,  "X", "532")
    CALL CalcH("vinyl chloride",                                "75-01-4", 2.78E-2,  "X", "532")
    CALL CalcH("methyl iodide",                                 "74-88-4", 0.00526,  "X", "608")
    CALL CalcH("(E)-crotonaldehyde",                           "123-73-9", 1.94E-5,  "X", "642")
    CALL CalcH("1,1-difluoroethane",                            "75-37-6", 0.02,     "X", "642")
    CALL CalcH("1,3-butanediol",                               "107-88-0", 2.30E-7,  "X", "642")
    CALL CalcH("1,3-dibromopropane",                           "109-64-8", 8.88E-4,  "X", "642")
    CALL CalcH("1-bromo-2-chloroethane",                       "107-04-0", 9.1E-4,   "X", "642")
    CALL CalcH("1-bromo-3-methylbutane",                       "107-82-4", 3.46E-2,  "X", "642")
    CALL CalcH("1-nitropropane",                               "108-03-2", 8.70E-5,  "X", "642")
    CALL CalcH("2-methyl-1-butanol",                           "137-32-6", 1.41E-5,  "X", "642")
    CALL CalcH("4-bromotoluene",                               "106-38-7", 2.33E-3,  "X", "642")
    CALL CalcH("4-methyl-2-pentanol",                          "108-11-2", 4.45E-5,  "X", "642")
    CALL CalcH("allyl alcohol",                                "107-18-6", 4.99E-6,  "X", "642")
    CALL CalcH("amylene",                                      "513-35-9", 0.110,    "X", "642")
    CALL CalcH("bromotrifluoromethane",                         "75-63-8", 0.499,    "X", "642")
    CALL CalcH("cyclohexene",                                  "110-83-8", 4.55E-2,  "X", "642")
    CALL CalcH("dibutyl ether",                                "142-96-1", 6.0E-3,   "X", "642")
    CALL CalcH("ethylenediamine",                              "107-15-3", 1.73E-9,  "X", "642")
    CALL CalcH("glycerin",                                      "56-81-5", 1.73E-8,  "X", "642")
    CALL CalcH("isoamyl acetate",                              "123-92-2", 5.9E-4,   "X", "642")
    CALL CalcH("methyl formate",                               "107-31-3", 2.2E-4,   "X", "642")
    CALL CalcH("methyl t-butyl ether",                        "1634-04-4", 5.87E-4,  "X", "642")
    CALL CalcH("n-pentane",                                    "109-66-0", 1.25,     "X", "642")
    CALL CalcH("trifluoromethane",                              "75-46-7", 9.52E-2,  "X", "642")
    CALL CalcH("1-heptanol",                                   "111-70-6", 1.88E-5,  "X", "712")
    CALL CalcH("isopentanol",                                  "123-51-3", 1.41E-5,  "X", "712")
    CALL CalcH("n-pentyl alcohol",                              "71-41-0", 1.3E-5,   "X", "712")
    CALL CalcH("butanenitrile",                                "109-74-0", 5.23E-5,  "X", "713")
    CALL CalcH("ethyl acetate",                                "141-78-6", 1.34E-4,  "X", "713")
    CALL CalcH("ethylene glycol",                              "107-21-1", 6.00E-8,  "X", "713")
    CALL CalcH("n-butanoic acid",                              "107-92-6", 5.35E-7,  "X", "713")
    CALL CalcH("propionitrile",                                "107-12-0", 3.70E-5,  "X", "713")
    CALL CalcH("3-nitrophenol",                                "554-84-7", 2E-9,     "X", "715")
    CALL CalcH("acetaldehyde",                                  "75-07-0", 6.67E-5,  "X", "715")
    CALL CalcH("acetonitrile",                                  "75-05-8", 3.45E-5,  "X", "715")
    CALL CalcH("acrolein",                                     "107-02-8", 1.22E-4,  "X", "715")
    CALL CalcH("dimethyl sulfide",                              "75-18-3", 1.61E-3,  "X", "715")
    CALL CalcH("ethanol",                                       "64-17-5", 5E-6,     "X", "715")
    CALL CalcH("formic acid",                                   "64-18-6", 1.67E-7,  "X", "715")
    CALL CalcH("hydrogen cyanide",                              "74-90-8", 1.33E-4,  "X", "715")
    CALL CalcH("methanol",                                      "67-56-1", 4.55E-6,  "X", "715")
    CALL CalcH("nitroethane",                                   "79-24-3", 4.76E-5,  "X", "715")
    CALL CalcH("nitromethane",                                  "75-52-5", 2.86E-5,  "X", "715")
    CALL CalcH("o-cresol",                                      "95-48-7", 1.2E-6,   "X", "715")
    CALL CalcH("oxalic acid",                                  "144-62-7", 1.4E-10,  "X", "715")
    CALL CalcH("p-cresol",                                     "106-44-5", 1E-6,     "X", "715")
    CALL CalcH("peroxyacetyl nitrate",                        "2278-22-0", 2.78E-4,  "X", "715")
    CALL CalcH("phenol",                                       "108-95-2", 3.33E-7,  "X", "715")
    CALL CalcH("trans-crotonaldehyde",                         "123-73-9", 1.96E-5,  "X", "715")
    CALL CalcH("2,6-lutidine",                                 "108-48-5", 1.04E-5,  "X", "719")
    CALL CalcH("2-methylpyridine",                             "109-06-8", 9.96E-6,  "X", "719")
    CALL CalcH("3-methylpyridine",                             "108-99-6", 7.73E-6,  "X", "719")
    CALL CalcH("4-methylpyridine",                             "108-89-4", 6.00E-6,  "X", "719")
    CALL CalcH("trimethylamine",                                "75-50-3", 1.0E-4,   "X", "721")
    CALL CalcH("hexamethyleneimine",                           "111-49-9", 6.1E-6,   "X", "722")
    CALL CalcH("piperidine",                                   "110-89-4", 4.45E-6,  "X", "722")
    CALL CalcH("pyrrolidine",                                  "123-75-1", 2.39E-6,  "X", "722")
    CALL CalcH("1,3-dioxolane",                                "646-06-0", 2.45E-5,  "X", "723")
    CALL CalcH("tetrahydrofuran",                              "109-99-9", 7.05E-5,  "X", "723")
    CALL CalcH("4-bromophenol",                                "106-41-2", 1.51E-7,  "X", "724")
    CALL CalcH("dimethyl disulfide",                           "624-92-0", 1.21E-3,  "X", "728")
    CALL CalcH("ethyl mercaptan",                               "75-08-1", 4.53E-3,  "X", "728")
    CALL CalcH("1,2-dichlorobenzene",                           "95-50-1", 1.5E-3,   "X", "747")
    CALL CalcH("1,3-dichlorobenzene",                          "541-73-1", 2.8E-3,   "X", "747")
    CALL CalcH("1,4-dichlorobenzene",                          "106-46-7", 2.7E-3,   "X", "747")
    CALL CalcH("n-butyl acetate",                              "123-86-4", 2.81E-4,  "X", "747")
    CALL CalcH("n-propyl acetate",                             "109-60-4", 2.18E-4,  "X", "747")
    CALL CalcH("1-hexanol",                                    "111-27-3", 1.71E-5,  "X", "754")
    CALL CalcH("1-octanol",                                    "111-87-5", 2.45E-5,  "X", "754")
    CALL CalcH("2-octanone",                                   "111-13-7", 1.88E-4,  "X", "754")
    CALL CalcH("2-undecanone",                                 "112-12-9", 6.36E-5,  "X", "754")
    CALL CalcH("butyraldehyde",                                "123-72-8", 1.15E-4,  "X", "754")
    CALL CalcH("heptanal",                                     "111-71-7", 2.7E-4,   "X", "754")
    CALL CalcH("hexaldehyde",                                   "66-25-1", 2.13E-4,  "X", "754")
    CALL CalcH("methyl n-butyrate",                            "623-42-7", 2.05E-4,  "X", "754")
    CALL CalcH("methyl n-octanoate",                           "111-11-5", 7.83E-4,  "X", "754")
    CALL CalcH("methyl propionate",                            "554-12-1", 1.74E-4,  "X", "754")
    CALL CalcH("n-butyl alcohol",                               "71-36-3", 8.8E-6,   "X", "754")
    CALL CalcH("nonanal",                                      "124-19-6", 7.34E-4,  "X", "754")
    CALL CalcH("octylaldehyde",                                "124-13-0", 5.14E-4,  "X", "754")
    CALL CalcH("pentanal",                                     "110-62-3", 1.47E-4,  "X", "754")
    CALL CalcH("propionaldehyde",                              "123-38-6", 7.34E-5,  "X", "754")
    CALL CalcH("2,4-hexadienal",                               "142-83-6", 9.78E-6,  "X", "755")
    CALL CalcH("1-butene",                                     "106-98-9", 0.245,    "X", "789")
    CALL CalcH("1-nonene",                                     "124-11-8", 0.7941,   "X", "789")
    CALL CalcH("ethyl bromide",                                 "74-96-4", 7.41E-3,  "X", "789")
    CALL CalcH("n-tridecane",                                  "629-50-5", 2.3,      "X", "789")
    CALL CalcH("2-butene",                                     "107-01-7", 0.224,    "X", "791")
    CALL CalcH("cis-2-butene",                                 "590-18-1", 0.231,    "X", "791")
    CALL CalcH("ethylene",                                      "74-85-1", 0.228,    "X", "791")
    CALL CalcH("isobutylene",                                  "115-11-7", 0.218,    "X", "791")
    CALL CalcH("propylene",                                    "115-07-1", 1.96E-1,  "X", "791")
    CALL CalcH("trans-2-butene",                               "624-64-6", 0.224,    "X", "791")
    CALL CalcH("mirex",                                       "2385-85-5", 8.1E-4,   "X", "852")
    CALL CalcH("2,4-D",                                         "94-75-7", 8.6E-6,   "X", "893")
    CALL CalcH("endosulfan",                                   "115-29-7", 6.5E-5,   "X", "893")
    CALL CalcH("alachlor",                                   "15972-60-8", 8.32E-9,  "X", "900")
    CALL CalcH("diazinon",                                     "333-41-5", 1.17E-7,  "X", "900")
    CALL CalcH("trichloroacetic acid",                          "76-03-9", 1.35E-8,  "X", "992")
    CALL CalcH("n-propyl nitrate",                             "627-13-4", 1.27E-3,  "X", "1024")
    CALL CalcH("phosgene",                                      "75-44-5", 1.7E-2,   "X", "1125")
    CALL CalcH("molinate",                                    "2212-67-1", 4.1E-6,   "X", "1142")
    CALL CalcH("o-methoxyphenol",                               "90-05-1", 1.2E-6,   "X", "1142")
    CALL CalcH("malathion",                                    "121-75-5", 4.9E-9,   "X", "1144")
    CALL CalcH("pendimethalin",                              "40487-42-1", 8.56E-7,  "X", "1144")
    CALL CalcH("tribufos",                                      "78-48-8", 2.9E-7,   "X", "1144")
    CALL CalcH("benzo(a)pyrene",                                "50-32-8", 4.57E-7,  "X", "1146")
    CALL CalcH("benzo(b)fluoranthene",                         "205-99-2", 5.0E-7,   "X", "1146")
    CALL CalcH("benzo(ghi)perylene",                           "191-24-2", 2.66E-7,  "X", "1146")
    CALL CalcH("benzo(k)fluoranthene",                         "207-08-9", 5.84E-7,  "X", "1146")
    CALL CalcH("hexachlorobenzene",                            "118-74-1", 5.8E-4,   "X", "1146")
    CALL CalcH("indeno(1,2,3-cd)pyrene",                       "193-39-5", 3.48E-7,  "X", "1146")
    CALL CalcH("pentachlorobenzene",                           "608-93-5", 7.03E-4,  "X", "1146")
    CALL CalcH("1,1,2,2-tetrachloroethane",                     "79-34-5", 3.67E-4,  "X", "1150")
    CALL CalcH("1,1,2-trichloroethane",                         "79-00-5", 8.24E-4,  "X", "1150")
    CALL CalcH("1,2,3-trichloropropane",                        "96-18-4", 3.43E-4,  "X", "1150")
    CALL CalcH("1,2-dichloroethane",                           "107-06-2", 1.18E-3,  "X", "1150")
    CALL CalcH("1,3-dichloropropane",                          "142-28-9", 9.76E-4,  "X", "1150")
    CALL CalcH("1-chloropentane",                              "543-59-9", 2.38E-2,  "X", "1150")
    CALL CalcH("2-chlorotoluene",                               "95-49-8", 3.57E-3,  "X", "1150")
    CALL CalcH("carbon tetrachloride",                          "56-23-5", 2.76E-2,  "X", "1150")
    CALL CalcH("dichloromethane",                               "75-09-2", 3.25E-3,  "X", "1150")
    CALL CalcH("n-butyl chloride",                             "109-69-3", 0.0167,   "X", "1150")
    CALL CalcH("trichloroethylene",                             "79-01-6", 9.85E-3,  "X", "1150")
    CALL CalcH("alpha-hexachlorocyclohexane",                  "319-84-6", 6.7E-6,   "X", "1152")
    CALL CalcH("4-methyl-2,6-dinitrophenol",                   "609-93-8", 5.3E-8,   "X", "1155")
    CALL CalcH("4-nitrophenol",                                "100-02-7", 1.28E-8,  "X", "1155")
    CALL CalcH("ronnel",                                       "299-84-3", 3.2E-5,   "X", "1157")
    CALL CalcH("1,4-dichloro-2-nitrobenzene",                   "89-61-2", 1.5E-5,   "X", "1195")
    CALL CalcH("1-chloro-2-nitrobenzene",                       "88-73-3", 9.3E-6,   "X", "1195")
    CALL CalcH("1-chloro-3-nitrobenzene",                      "121-73-3", 1.4E-5,   "X", "1195")
    CALL CalcH("1-chloro-4-nitrobenzene",                      "100-00-5", 4.9E-6,   "X", "1195")
    CALL CalcH("1-decanol",                                    "112-30-1", 3.2E-5,   "X", "1195")
    CALL CalcH("1-dodecanol",                                  "112-53-8", 2.22E-5,  "X", "1195")
    CALL CalcH("1-methylnaphthalene",                           "90-12-0", 0.000514, "X", "1195")
    CALL CalcH("1-nitronaphthalene",                            "86-57-7", 1.76E-6,  "X", "1195")
    CALL CalcH("2,4-dinitrotoluene",                           "121-14-2", 5.40E-8,  "X", "1195")
    CALL CalcH("2-aminotoluene",                                "95-53-4", 1.98E-6,  "X", "1195")
    CALL CalcH("2-chloro-4-nitroaniline",                      "121-87-9", 9.5E-9,   "X", "1195")
    CALL CalcH("2-methylnaphthalene",                           "91-57-6", 0.000518, "X", "1195")
    CALL CalcH("2-nitroaniline",                                "88-74-4", 5.9E-8,   "X", "1195")
    CALL CalcH("2-nitrotoluene",                                "88-72-2", 1.25E-5,  "X", "1195")
    CALL CalcH("3,4-dichloronitrobenzene",                      "99-54-7", 8.07E-6,  "X", "1195")
    CALL CalcH("3-aminotoluene",                               "108-44-1", 1.66E-6,  "X", "1195")
    CALL CalcH("3-chloroaniline",                              "108-42-9", 1.0E-6,   "X", "1195")
    CALL CalcH("3-nitrotoluene",                                "99-08-1", 9.30E-6,  "X", "1195")
    CALL CalcH("4-nitroaniline",                               "100-01-6", 1.26E-9,  "X", "1195")
    CALL CalcH("4-nitrotoluene",                                "99-99-0", 5.63E-6,  "X", "1195")
    CALL CalcH("aldrin",                                       "309-00-2", 4.4E-5,   "X", "1195")
    CALL CalcH("beta-hexachlorocyclohexane",                   "319-85-7", 4.4E-7,   "X", "1195")
    CALL CalcH("cyclohexanol",                                 "108-93-0", 4.40E-6,  "X", "1195")
    CALL CalcH("DDD",                                           "72-54-8", 6.6E-6,   "X", "1195")
    CALL CalcH("DDE",                                           "72-55-9", 4.16E-5,  "X", "1195")
    CALL CalcH("DDT",                                           "50-29-3", 8.32E-6,  "X", "1195")
    CALL CalcH("dieldrin",                                      "60-57-1", 1E-5,     "X", "1195")
    CALL CalcH("endrin",                                        "72-20-8", 6.4E-6,   "X", "1195")
    CALL CalcH("heptachlor",                                    "76-44-8", 2.94E-4,  "X", "1195")
    CALL CalcH("linalool",                                      "78-70-6", 2.15E-5,  "X", "1195")
    CALL CalcH("lindane",                                       "58-89-9", 5.14E-6,  "X", "1195")
    CALL CalcH("m-cresol",                                     "108-39-4", 8.6E-7,   "X", "1195")
    CALL CalcH("methoxychlor",                                  "72-43-5", 2.03E-7,  "X", "1195")
    CALL CalcH("p-anisidine",                                  "104-94-9", 6.60E-8,  "X", "1195")
    CALL CalcH("t-butyl alcohol",                               "75-65-0", 9.05E-6,  "X", "1195")
    CALL CalcH("3-bromo-1-propyne",                            "106-96-7", 1.13E-3,  "X", "1330")
    CALL CalcH("methyl bromide",                                "74-83-9", 7.34E-3,  "X", "1330")
    CALL CalcH("2-nitrophenol",                                 "88-75-5", 1.63E-5,  "X", "1513")
    CALL CalcH("anthracene",                                   "120-12-7", 4.88E-5,  "X", "1597")
    CALL CalcH("1-chloro-1,1-difluoroethane",                   "75-68-3", 0.0588,   "X", "2121")
    CALL CalcH("2,2-dichloro-1,1,1-trifluoroethane",           "306-83-2", 0.0256,   "X", "2121")
    CALL CalcH("chlorodifluoromethane",                         "75-45-6", 4.06E-2,  "X", "2121")
    CALL CalcH("alpha-terpineol",                               "98-55-5", 2.3E-6,   "X", "2124")
    CALL CalcH("chloropicrin",                                  "76-06-2", 2.05E-3,  "X", "2229")
    CALL CalcH("2,2-dimethylpropanoic acid",                    "75-98-9", 2.78E-6,  "X", "2230")
    CALL CalcH("isovaleric acid",                              "503-74-2", 8.33E-7,  "X", "2230")
    CALL CalcH("2,6-dimethylphenol",                           "576-26-1", 6.7E-6,   "X", "2232")
    CALL CalcH("cyclopentanone",                               "120-92-3", 1.0E-5,   "X", "2232")
    CALL CalcH("pyridine",                                     "110-86-1", 1.1E-5,   "X", "2232")
    CALL CalcH("pyrrole",                                      "109-97-7", 1.8E-5,   "X", "2232")
    CALL CalcH("dichlorodifluoromethane",                       "75-71-8", 0.343,    "X", "2240")
    CALL CalcH("trichlorofluoromethane",                        "75-69-4", 9.70E-2,  "X", "2240")
    CALL CalcH("bromoform",                                     "75-25-2", 5.35E-4,  "X", "2243")
    CALL CalcH("hexachloroethane",                              "67-72-1", 3.89E-3,  "X", "2243")
    CALL CalcH("1,2,3,4-tetrachlorobenzene",                   "634-66-2", 6.9E-4,   "X", "2339")
    CALL CalcH("1,2,3,5-tetrachlorobenzene",                   "634-90-2", 1.6E-3,   "X", "2339")
    CALL CalcH("1,2,3-trichlorobenzene",                        "87-61-6", 1.25E-3,  "X", "2339")
    CALL CalcH("1,2,4,5-tetrachlorobenzene",                    "95-94-3", 1.0E-3,   "X", "2339")
    CALL CalcH("1,3,5-trichlorobenzene",                       "108-70-3", 1.89E-3,  "X", "2339")
    CALL CalcH("1-chloronaphthalene",                           "90-13-1", 3.55E-4,  "X", "2339")
    CALL CalcH("2-pentanone",                                  "107-87-9", 8.36E-5,  "X", "2339")
    CALL CalcH("biphenyl",                                      "92-52-4", 3.08E-4,  "X", "2339")
    CALL CalcH("chlorobenzene",                                "108-90-7", 3.11E-3,  "X", "2339")
    CALL CalcH("fluoranthene",                                 "206-44-0", 9.45E-6,  "X", "2339")
    CALL CalcH("naphthalene",                                   "91-20-3", 4.4E-4,   "X", "2339")
    CALL CalcH("pyrene",                                       "129-00-0", 1.19E-5,  "X", "2339")
    CALL CalcH("1,4-dioxane",                                  "123-91-1", 4.8E-6,   "X", "2449")
    CALL CalcH("1,2,3-trimethylbenzene",                       "526-73-8", 4.36E-3,  "X", "2453")
    CALL CalcH("1,2,4-trimethylbenzene",                        "95-63-6", 6.16E-3,  "X", "2453")
    CALL CalcH("1,3,5-trimethylbenzene",                       "108-67-8", 8.77E-3,  "X", "2453")
    CALL CalcH("2-xylene",                                      "95-47-6", 5.18E-3,  "X", "2453")
    CALL CalcH("3-xylene",                                     "108-38-3", 7.18E-3,  "X", "2453")
    CALL CalcH("ethylbenzene",                                 "100-41-4", 7.88E-3,  "X", "2453")
    CALL CalcH("isopropylbenzene",                              "98-82-8", 1.15E-2,  "X", "2453")
    CALL CalcH("n-propylbenzene",                              "103-65-1", 1.05E-2,  "X", "2453")
    CALL CalcH("fenitrothion",                                 "122-14-5", 9.3E-7,   "X", "2454")
    CALL CalcH("methyl parathion",                             "298-00-0", 1.0E-7,   "X", "2454")
    CALL CalcH("octamethylcyclotetrasiloxane",                 "556-67-2", 0.117,    "X", "2456")
    CALL CalcH("phenanthrene",                                  "85-01-8", 4.23E-5,  "X", "2477")
    CALL CalcH("chlorotrifluoromethane",                        "75-72-9", 1.38,     "X", "2486")
    CALL CalcH("perfluoroethane",                               "76-16-4", 20.3,     "X", "2486")
    CALL CalcH("tetrafluoromethane",                            "75-73-0", 5.15,     "X", "2486")
    CALL CalcH("2,4,5-trimethylaniline",                       "137-17-7", 2.48E-6,  "X", "2525")
    CALL CalcH("3,4-xylidine",                                  "95-64-7", 1.85E-5,  "X", "2525")
    CALL CalcH("4-aminotoluene",                               "106-49-0", 2.02E-6,  "X", "2525")
    CALL CalcH("aniline",                                       "62-53-3", 2.02E-6,  "X", "2525")
    CALL CalcH("hexachlorocyclopentadiene",                     "77-47-4", 0.027,    "X", "2529")
    CALL CalcH("acetone",                                       "67-64-1", 3.97E-5,  "X", "2531")
    CALL CalcH("dimethyl sulfoxide",                            "67-68-5", 1.5E-9,   "X", "2531")
    CALL CalcH("isopropanol",                                   "67-63-0", 8.10E-6,  "X", "2531")
    CALL CalcH("N,N-dimethylacetamide",                        "127-19-5", 1.31E-8,  "X", "2531")
    CALL CalcH("N,N-dimethylformamide",                         "68-12-2", 7.39E-8,  "X", "2531")
    CALL CalcH("n-amyl acetate",                               "628-63-7", 3.88E-4,  "X", "2531")
    CALL CalcH("dibutyl phthalate",                             "84-74-2", 1.81E-6,  "X", "2533")
    CALL CalcH("ethylene oxide",                                "75-21-8", 1.48E-4,  "X", "2534")
    CALL CalcH("butachlor",                                  "23184-66-9", 5.10E-8,  "X", "2538")
    CALL CalcH("carbon disulfide",                              "75-15-0", 1.44E-2,  "X", "2631")
    CALL CalcH("ethyl tert-butyl ether",                       "637-92-3", 1.64E-3,  "X", "2646")
    CALL CalcH("tert-amyl methyl ether",                       "994-05-8", 1.32E-3,  "X", "2646")
    CALL CalcH("1-methyl-2-pyrrolidone",                       "872-50-4", 3.20E-9,  "X", "2837")
    CALL CalcH("diethylene glycol mono-n-butyl ether",         "112-34-5", 7.2E-9,   "X", "2837")
    CALL CalcH("ethylene glycol monobutyl ether acetate",      "112-07-2", 5.46E-6,  "X", "2837")
    CALL CalcH("1,2,4-trichlorobenzene",                       "120-82-1", 1.42E-3,  "X", "2900")
    CALL CalcH("1,2-dichloropropane",                           "78-87-5", 2.82E-3,  "X", "2900")
    CALL CalcH("1,3-dichloropropene",                          "542-75-6", 3.55E-3,  "X", "2900")
    CALL CalcH("4,6-dinitro-o-cresol",                         "534-52-1", 1.4E-6,   "X", "2900")
    CALL CalcH("bromodichloromethane",                          "75-27-4", 2.12E-3,  "X", "2900")
    CALL CalcH("chlordane",                                     "57-74-9", 4.86E-5,  "X", "2900")
    CALL CalcH("chlorodibromomethane",                         "124-48-1", 7.83E-4,  "X", "2900")
    CALL CalcH("hexachloro-1,3-butadiene",                      "87-68-3", 1.03E-2,  "X", "2900")
    CALL CalcH("nitrobenzene",                                  "98-95-3", 2.4E-5,   "X", "2900")
    CALL CalcH("perylene",                                     "198-55-0", 4.34E-6,  "X", "2966")
    CALL CalcH("mercuric bichloride",                         "7487-94-7", 7.09E-10, "X", "3070")
    CALL CalcH("mercuric hydroxide",                            "_CAS-84", 7.82E-8,  "X", "3070")
    CALL CalcH("2-bromo-2-chloro-1,1,1-trifluoroethane",       "151-67-7", 0.0203,   "?", "3072")
    CALL CalcH("2-chlorophenol",                                "95-57-8", 1.12E-5,  "?", "3072")
    CALL CalcH("2-naphthylamine",                               "91-59-8", 8.10E-8,  "?", "3072")
    CALL CalcH("3-chlorophenol",                               "108-43-0", 3.45E-7,  "?", "3072")
    CALL CalcH("3-nitroaniline",                                "99-09-2", 7.9E-9,   "?", "3072")
    CALL CalcH("benzamide",                                     "55-21-0", 2.45E-10, "?", "3072")
    CALL CalcH("benzyl alcohol",                               "100-51-6", 3.37E-7,  "?", "3072")
    CALL CalcH("isoflurane",                                 "26675-46-7", 2.87E-2,  "?", "3072")
    CALL CalcH("methoxyflurane",                                "76-38-0", 3.7E-3,   "?", "3072")
    CALL CalcH("carbaryl",                                      "63-25-2", 2.8E-9,   "X", "3073")
    CALL CalcH("acrylonitrile",                                "107-13-1", 1.38E-4,  "X", "3074")
    CALL CalcH("cyclohexane",                                  "110-82-7", 0.15,     "X", "3074")
    CALL CalcH("diethyl ether",                                 "60-29-7", 1.23E-3,  "X", "3074")
    CALL CalcH("dipropyl ether",                               "111-43-3", 2.2E-3,   "X", "3074")
    CALL CalcH("ethyl formate",                                "109-94-4", 3.85E-4,  "X", "3074")
    CALL CalcH("styrene",                                      "100-42-5", 2.75E-3,  "X", "3074")
    CALL CalcH("decaldehyde",                                  "112-31-2", 5.87E-5,  "X", "3075")
    CALL CalcH("metolachlor",                                "51218-45-2", 9.0E-9,   "X", "3076")
    CALL CalcH("tetraethyl lead",                               "78-00-2", 0.568,    "X", "3077")
    CALL CalcH("sulfur hexafluoride",                         "2551-62-4", 4.25,     "X", "3078")
    CALL CalcH("1-methoxy-2-hydroxypropane",                   "107-98-2", 9.2E-7,   "X", "3079")
    CALL CalcH("2-methoxyethanol",                             "109-86-4", 3.30E-7,  "X", "3079")
    CALL CalcH("ethylene glycol mono-n-butyl ether",           "111-76-2", 1.6E-6,   "X", "3079")
    CALL CalcH("ethylene glycol monoethyl ether acetate",      "111-15-9", 3.2E-6,   "X", "3079")
    CALL CalcH("ethylene glycol monoethyl ether",              "110-80-5", 4.7E-7,   "X", "3079")
    CALL CalcH("N-nitrosodimethylamine",                        "62-75-9", 1.08E-6,  "X", "3080")
    CALL CalcH("decamethylcyclopentasiloxane",                 "541-02-6", 0.306,    "X", "3081")
    CALL CalcH("dodecamethylcyclohexasiloxane",                "540-97-6", 0.10,     "X", "3081")
    CALL CalcH("hexamethyldisiloxane",                         "107-46-0", 0.0453,   "X", "3081")
    CALL CalcH("N,N-dibutylnitrosoamine",                      "924-16-3", 1.32E-5,  "X", "3082")
    CALL CalcH("N-nitrosodi-n-propylamine",                    "621-64-7", 5.38E-6,  "X", "3082")
    CALL CalcH("N-nitrosodiethylamine",                         "55-18-5", 3.63E-6,  "X", "3082")
    CALL CalcH("N-nitrosomorpholine",                           "59-89-2", 2.45E-8,  "X", "3082")
    CALL CalcH("N-nitrosopiperidine",                          "100-75-4", 8.44E-7,  "X", "3082")
    CALL CalcH("heptachlor epoxide",                          "1024-57-3", 3.2E-5,   "X", "3083")
    CALL CalcH("triethyl phosphate",                            "78-40-0", 3.60E-8,  "X", "3084")
    CALL CalcH("trimethyl phosphate",                          "512-56-1", 7.2E-9,   "X", "3084")
    CALL CalcH("hydrogen sulfide",                            "7783-06-4", 0.0098,   "X", "3085")
    CALL CalcH("cis-1,3-dichloropropene",                    "10061-01-5", 2.71E-3,  "X", "3086")
    CALL CalcH("trans-1,3-dichloropropene",                  "10061-02-6", 8.7E-4,   "X", "3086")
    CALL CalcH("brodifacoum",                                "56073-10-0", 2.2E-8,   "X", "3089")
    CALL CalcH("4-chloro-2-methylphenol",                     "1570-64-5", 1.1E-6,   "X", "3090")
    CALL CalcH("2,3-dichloro-1-propene",                        "78-88-6", 2.82E-3,  "X", "albanese87")
    CALL CalcH("bis(2-ethylhexyl) adipate",                    "103-23-1", 4.34E-7,  "X", "felder86")
    CALL CalcH("di-n-octyl adipate",                           "123-79-5", 4.34E-7,  "X", "felder86")
    CALL CalcH("4-xylene",                                     "106-42-3", 6.90E-3,  "X", "foster94")
    CALL CalcH("2,4-D, 2-ethylhexyl ester",                   "1928-43-4", 1.8E-5,   "X", "macbean2012")
    CALL CalcH("amicarbazone",                              "129909-90-6", 6.7E-13,  "X", "macbean2012")
    CALL CalcH("amisulbrom",                                "348635-87-0", 2.1E-7,   "X", "macbean2012")
    CALL CalcH("amitraz",                                    "33089-61-1", 9.87E-6,  "X", "macbean2012")
    CALL CalcH("benthiavalicarb isopropyl",                 "177406-68-7", 8.6E-8,   "X", "macbean2012")
    CALL CalcH("bifenazate",                                "149877-41-8", 9.86E-9,  "X", "macbean2012")
    CALL CalcH("boscalid",                                  "188425-85-6", 5.11E-10, "X", "macbean2012")
    CALL CalcH("chlorothalonil",                              "1897-45-6", 2.5E-7,   "X", "macbean2012")
    CALL CalcH("clothianidin",                              "210880-92-5", 2.9E-16,  "X", "macbean2012")
    CALL CalcH("cyclanilide",                               "113136-77-9", 7.31E-10, "X", "macbean2012")
    CALL CalcH("cyhalofop-butyl",                           "122008-85-9", 9.4E-9,   "X", "macbean2012")
    CALL CalcH("dazomet",                                      "533-74-4", 2.66E-10, "X", "macbean2012")
    CALL CalcH("dimethazone",                                "81777-89-1", 4.13E-8,  "X", "macbean2012")
    CALL CalcH("dimethenamid-P",                            "163515-14-8", 4.73E-9,  "X", "macbean2012")
    CALL CalcH("fenhexamid",                                "126833-17-8", 4.9E-11,  "X", "macbean2012")
    CALL CalcH("fenpyroximate",                             "134098-61-6", 1.3E-6,   "X", "macbean2012")
    CALL CalcH("forchlorfenuron",                            "68157-60-8", 2.86E-12, "X", "macbean2012")
    CALL CalcH("fosthiazate",                                "98886-44-3", 1.32E-7,  "X", "macbean2012")
    CALL CalcH("glufosinate-ammonium",                       "77182-82-2", 4.4E-14,  "X", "macbean2012")
    CALL CalcH("isofenphos",                                 "25311-71-1", 4.15E-8,  "X", "macbean2012")
    CALL CalcH("isoxaben",                                   "82558-50-7", 1.27E-9,  "X", "macbean2012")
    CALL CalcH("isoxaflutole",                              "141112-29-0", 1.85E-10, "X", "macbean2012")
    CALL CalcH("linuron",                                      "330-55-2", 1.97E-9,  "X", "macbean2012")
    CALL CalcH("methiocarb",                                  "2032-65-7", 1.18E-9,  "X", "macbean2012")
    CALL CalcH("parathion",                                     "56-38-2", 2.98E-7,  "X", "macbean2012")
    CALL CalcH("phenothrin",                                 "26002-80-2", 6.8E-6,   "X", "macbean2012")
    CALL CalcH("pyraclostrobin",                            "175013-18-0", 5.2E-11,  "X", "macbean2012")
    CALL CalcH("pyraflufen-ethyl",                          "129630-19-9", 7.9E-10,  "X", "macbean2012")
    CALL CalcH("thiram",                                       "137-26-8", 3.26E-7,  "X", "macbean2012")
    CALL CalcH("tribenuron methyl",                         "101200-48-0", 1.02E-13, "X", "macbean2012")
    CALL CalcH("trifloxystrobin",                           "141517-21-7", 2.27E-8,  "X", "macbean2012")
    CALL CalcH("zineb",                                      "12122-67-7",  2.7E-9,  "X", "macbean2012", upperlimit=1)
    CALL CalcH("bis(2-chloroethyl)sulfide",                  "69020-37-7", 2.45E-5,  "X", "3121")
    CALL CalcH("decylamine",                                  "2016-57-1", 6.69E-5,  "X", "yaws2001")

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, type_, xref_, upperlimit)
      IMPLICIT NONE
      CHARACTER(LEN=*),           INTENT(IN) :: chem_
      CHARACTER(LEN=*),           INTENT(IN) :: casrn_
      REAL,                       INTENT(IN) :: H
      CHARACTER,                  INTENT(IN) :: type_
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: xref_
      INTEGER,          OPTIONAL, INTENT(IN) :: upperlimit
      LOGICAL :: l_output
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      l_output = .TRUE.
      IF (type_ == "Q") THEN
        IF (xref_ == "EPI-Suite") THEN
          CALL MakeNote(TRIM(ref)//"-EPI-Suite", &
            "Calculated using the EPI Suite method "// &
          "(\url{http://www.epa.gov/oppt/exposure/pubs/episuitedl.htm}).")
        ENDIF
        IF (xref_ == "642") THEN
          CALL MakeNote(TRIM(ref)//"-642", &
            "Calculated based on the method by \citet{642}.")
        ENDIF
        IF (xref_ == "1145") THEN
          CALL MakeNote(TRIM(ref)//"-1145", &
            "Calculated based on the method by \citet{1145}.")
        ENDIF
      ENDIF
      IF (type_ == "?") THEN
        IF (xref_ == "3072") THEN
          CALL MakeNote(TRIM(ref)//"-3072", &
            TRIM(citet())//" refer to \citet{3072} as the source but this "// &
            "value cannot be found there. Maybe the value is taken from "// &
            "\citet{1500}.")
        ENDIF
      ENDIF
      IF (type_ == "X") THEN
        CALL SettypeX(xref_)
        ! only use value here if it is in a paper that I don't have:
        IF (.NOT.unread_bib(xref_)) THEN
          l_output = .FALSE.
        ENDIF
      ENDIF
      Hominus = 1. / (atm*H)
      IF (l_output) THEN
        IF (PRESENT(upperlimit)) THEN
          ! upper limit of KHcc is lower limit of Hcp:
          CALL Output(Hominus, limit=">")
        ELSE
          CALL Output(Hominus)
        ENDIF
      ELSE
        seenote = "" ! reset so that next CALL Output won't use it
        type    = "" ! reset to invalid value that will be overwritten
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref3088

  !---------------------------------------------------------------------------

  SUBROUTINE ref3089 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "3089"
    type = "?"
    ! Section 5.2.2:
    chem = "brodifacoum" ; casrn = "56073-10-0" ! C{31}H{23}BrO3
    CALL Output(KHpcSI_TIMES_HcpSI/2.18E-3)

  END SUBROUTINE ref3089

  !---------------------------------------------------------------------------

  SUBROUTINE ref3090 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3090"
    type = "V"

    ! Table 1:
    CALL CalcH("molinate",                "2212-67-1", 9.6E-7) ! C9H{17}NOS
    CALL CalcH("thiobencarb",            "28249-77-6", 1.7E-7, othertemp=20.) ! C{12}H{16}ClNOS
    CALL CalcH("MCPA",                      "94-74-6", 1.0E-9) ! C9H9ClO3 MCPA
    ! MCPA DMA salt not used here
    CALL CalcH("4-chloro-2-methylphenol", "1570-64-5", 1.1E-6) ! C7H7ClO
    CALL CalcH("methylparathion",          "298-00-0", 1.0E-7) ! C{8}H{10}NO5PS
    CALL CalcH("methyl paraoxon",          "950-35-6", 9E-10, upperlimit=1)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, othertemp, upperlimit)
      IMPLICIT NONE
      CHARACTER(LEN=*),  INTENT(IN) :: chem_
      CHARACTER(LEN=*),  INTENT(IN) :: casrn_
      REAL,              INTENT(IN) :: H
      REAL,    OPTIONAL, INTENT(IN) :: othertemp
      INTEGER, OPTIONAL, INTENT(IN) :: upperlimit
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*H)
      IF (PRESENT(othertemp)) THEN
        CALL MakeNoteOtherTemp(TRIM(str(NINT(othertemp+CtoK))))
      ENDIF
      IF (PRESENT(upperlimit)) THEN
        ! upper limit of KHpc is lower limit of Hcp:
        CALL Output(Hominus, limit=">")
      ELSE
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref3090

  !---------------------------------------------------------------------------

  SUBROUTINE ref3091 ! KHpx [atm], KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3091"
    type = "?"

    !                                                    temp  KHpx atm    KHpc atm/(mol/m3)
    CALL CalcH("propanone",                   "67-64-1", 25.0, 3.0733E+00, 5.5320E-05) ! CH3COCH3 acetone
    CALL CalcH("butanone",                    "78-93-3", 25.0, 3.6867E+00, 6.6361E-05) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("2-pentanone",                "107-87-9", 25.0, 4.0882E+00, 7.3588E-05) ! C3H7COCH3
    CALL CalcH("3-pentanone",                 "96-22-0", 25.0, 4.6350E+00, 8.3429E-05) ! C2H5COC2H5
    CALL CalcH("3-methyl-2-butanone",        "563-80-4", 25.0, 5.1422E+00, 9.2561E-05) ! C5H{10}O isopropyl methyl ketone
    CALL CalcH("2-hexanone",                 "591-78-6", 25.0, 4.7018E+00, 8.4633E-05) ! C6H{12}O
    CALL CalcH("3-hexanone",                 "589-38-8", 25.0, 6.8494E+00, 1.2329E-04) ! C6H{12}O
    CALL CalcH("3-methyl-2-pentanone",       "565-61-7", 25.0, 5.6881E+00, 1.0239E-04) ! C6H{12}O
    CALL CalcH("4-methyl-2-pentanone",       "108-10-1", 25.0, 7.6132E+00, 1.3704E-04) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("2-methyl-3-pentanone",       "565-69-5", 25.0, 8.6037E+00, 1.5487E-04) ! C6H{12}O
    CALL CalcH("3,3-dimethyl-2-butanone",     "75-97-8", 25.0, 8.5208E+00, 1.5337E-04) ! C6H{12}O {tert}-butyl methyl ketone
    CALL CalcH("2-heptanone",                "110-43-0", 25.0, 7.3354E+00, 1.3204E-04) ! C7H{14}O
    CALL CalcH("3-heptanone",                "106-35-4", 25.0, 2.2425E+01, 4.0366E-04) ! C7H{14}O
    CALL CalcH("4-heptanone",                "123-19-3", 30.0, 2.3771E+01, 4.2787E-04) ! C7H{14}O
    CALL CalcH("3-methyl-2-hexanone",       "2550-21-2", 25.0, 1.7178E+01, 3.0920E-04) ! C7H{14}O
    CALL CalcH("4-methyl-2-hexanone",        "105-42-0", 25.0, 1.6571E+01, 2.9828E-04) ! C7H{14}O
    CALL CalcH("5-methyl-2-hexanone",        "110-12-3", 25.0, 2.0417E+01, 3.6751E-04) ! C7H{14}O
    CALL CalcH("2-methyl-3-hexanone",       "7379-12-6", 25.0, 1.3356E+01, 2.4041E-04) ! C7H{14}O
    CALL CalcH("4-methyl-3-hexanone",      "17042-16-9", 25.0, 1.4878E+01, 2.6780E-04) ! C7H{14}O
    CALL CalcH("5-methyl-3-hexanone",        "623-56-3", 25.0, 1.4878E+01, 2.6780E-04) ! C7H{14}O
    CALL CalcH("3-ethyl-2-pentanone",       "6137-03-7", 25.0, 1.5986E+01, 2.8775E-04) ! C7H{14}O
    CALL CalcH("3,3-dimethyl-2-pentanone", "20669-04-9", 25.0, 1.2250E+01, 2.2050E-04) ! C7H{14}O diisopropyl ketone
    CALL CalcH("3,4-dimethyl-2-pentanone",   "565-78-6", 25.0, 1.2884E+01, 2.3191E-04) ! C7H{14}O diisopropyl ketone
    CALL CalcH("4,4-dimethyl-2-pentanone",   "590-50-1", 25.0, 1.0004E+01, 1.8007E-04) ! C7H{14}O diisopropyl ketone
    CALL CalcH("2,2-dimethyl-3-pentanone",   "564-04-5", 25.0, 1.0004E+01, 1.8007E-04) ! C7H{14}O diisopropyl ketone
    CALL CalcH("2,4-dimethyl-3-pentanone",   "565-80-0", 25.0, 1.9526E+01, 3.5148E-04) ! C7H{14}O diisopropyl ketone
    CALL CalcH("2-octanone",                 "111-13-7", 20.0, 3.7524E+00, 6.7543E-05) ! C6H{13}COCH3
    CALL CalcH("2-nonanone",                 "821-55-6", 25.0, 1.8918E+01, 3.4052E-04) ! C7H{15}COCH3
    CALL CalcH("5-nonanone",                 "502-56-7", 30.0, 1.6267E+01, 2.9281E-04) ! C9H{18}O dibutyl ketone
    CALL CalcH("2,6-dimethyl-4-heptanone",   "108-83-8", 23.5, 5.9533E+00, 1.0716E-04) ! C9H{18}O
    CALL CalcH("2-decanone",                 "693-54-9", 25.0, 3.8620E+01, 6.9516E-04) ! C8H{17}COCH3
    CALL CalcH("2-undecanone",               "112-12-9", 25.0, 9.4288E+01, 1.6972E-03) ! C9H{19}COCH3
    CALL CalcH("2-dodecanone",              "6175-49-1", 25.0, 2.6681E+02, 4.8026E-03) ! C9H{19}COCH3
    CALL CalcH("2-tridecanone",              "593-08-8", 25.0, 8.2379E+02, 1.4828E-02) ! C9H{19}COCH3
    CALL CalcH("2-tetradecanone",           "2345-27-9", 25.0, 2.5692E+03, 4.6246E-02) ! C9H{19}COCH3
    CALL CalcH("2-pentadecanone",           "2345-28-0", 25.0, 1.0066E+04, 1.8118E-01) ! C9H{19}COCH3
    CALL CalcH("2-hexadecanone",           "18787-63-8", 25.0, 3.1989E+04, 5.7580E-01) ! C9H{19}COCH3
    CALL CalcH("2-heptadecanone",           "2922-51-2", 25.0, 1.4011E+05, 2.5220E+00) ! C9H{19}COCH3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, KHpx, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: temp, KHpx, KHpc
      INTEGER :: temp_int ! T as an integer [K]

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*KHpc)
      CALL consistency_check(Hominus, KHpx_TIMES_HcpSI/KHpx, &
        "Different types of Henry's law constants") ! add ",.TRUE." for verbose mode
      temp_int = NINT(temp+CtoK)
      IF (temp_int /= 298) CALL MakeNoteOtherTemp(TRIM(str(temp_int)))
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3091

  !---------------------------------------------------------------------------

  SUBROUTINE ref3093 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "3093"
    type = "M"

    ! Table 8:
    CALL CalcH("methyl ethanoate",  "79-20-9", 46.2372, -39.7112, -23.3518,  683.) ! CH3COOCH3 methyl acetate
    CALL CalcH("ethyl ethanoate",  "141-78-6", 59.1021, -52.3026, -33.9931,  897.) ! CH3COOC2H5 ethyl acetate
    CALL CalcH("propyl ethanoate", "109-60-4", 66.3149, -59.2033, -39.3865, 1225.) ! CH3COOC3H7 propyl acetate
    CALL CalcH("butyl ethanoate",  "123-86-4", 77.1587, -69.7898, -48.5241, 1585.) ! CH3COOC4H9 butyl acetate
    CALL CalcH("ethyl propanoate", "105-37-3", 67.7415, -60.5378, -40.6026, 1345.) ! C2H5COOC2H5 ethyl propionate
    CALL CalcH("ethyl butanoate",  "105-54-4", 75.9298, -68.3771, -47.0474, 1905.) ! C3H7COOC2H5 ethyl butyrate

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, KH)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: A, B, C, KH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      ! The temperature dependence is:
      ! ln(KH) = A + B/tau + C*ln(tau) with tau = T/T0 and T0 = 298.15 K
      ! At T=T0, this reduces to exp(A+B)
      ! The analytical derivative is:
      ! dln(KH)/d(1/T) = B*T0 - C*T - D*T^2/T0
      ! At T=T0, this reduces to:
      mindHR = -(B-C)*T0 ! the minus sign comes from KHpx->Hcp
      CALL Output(cH2O/(1E3*KH), mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3093

  !---------------------------------------------------------------------------

  SUBROUTINE ref3094 ! KHcc [1]
    IMPLICIT NONE

    ref = "3094"

    ! Table 4:
    ! L2:
    CALL CalcH("hexamethyldisiloxane",          "107-46-0", 2.49,  25., "M"        ) ! L2
    CALL CalcH("hexamethyldisiloxane",          "107-46-0", 1.98,  25., "X", "3095") ! L2
    CALL CalcH("hexamethyldisiloxane",          "107-46-0", 2.60,  25., "V"        ) ! L2
    CALL CalcH("hexamethyldisiloxane",          "107-46-0", 0.27,  28., "X", "3081") ! L2
    ! L3:
    CALL CalcH("octamethyltrisiloxane",         "107-51-7", 3.07,  25., "M"        ) ! L3
    CALL CalcH("octamethyltrisiloxane",         "107-51-7", 2.52,  25., "X", "3095") ! L3
    CALL CalcH("octamethyltrisiloxane",         "107-51-7", 3.16,  25., "V"        ) ! L3
    CALL CalcH("octamethyltrisiloxane",         "107-51-7", 2.13,  28., "X", "3081") ! L3
    ! L4:
    CALL CalcH("decamethyltetrasiloxane",       "141-62-8", 3.45,  25., "M"        ) ! L4
    CALL CalcH("decamethyltetrasiloxane",       "141-62-8", 3.12,  25., "X", "3095") ! L4
    CALL CalcH("decamethyltetrasiloxane",       "141-62-8", 3.04,  25., "V"        ) ! L4
    CALL CalcH("decamethyltetrasiloxane",       "141-62-8", 2.84,  28., "X", "3081") ! L4
    ! D4:
    CALL CalcH("octamethylcyclotetrasiloxane",  "556-67-2", 2.74,  25., "M"        ) ! D4
    CALL CalcH("octamethylcyclotetrasiloxane",  "556-67-2", 2.69,  22., "X", "3096") ! D4
    CALL CalcH("octamethylcyclotetrasiloxane",  "556-67-2", 2.69,  25., "X", "3095") ! D4
    CALL CalcH("octamethylcyclotetrasiloxane",  "556-67-2", 2.43,  25., "V"        ) ! D4
    CALL CalcH("octamethylcyclotetrasiloxane",  "556-67-2", 0.53,  20., "X", "2456") ! D4
    CALL CalcH("octamethylcyclotetrasiloxane",  "556-67-2", 1.37,  28., "X", "3081") ! D4
    ! D5:
    CALL CalcH("decamethylcyclopentasiloxane",  "541-02-6", 3.16,  25., "M"        ) ! D5
    CALL CalcH("decamethylcyclopentasiloxane",  "541-02-6", 3.13,  25., "X", "3096") ! D5
    CALL CalcH("decamethylcyclopentasiloxane",  "541-02-6", 2.43,  23., "X", "3095") ! D5
    CALL CalcH("decamethylcyclopentasiloxane",  "541-02-6", 2.25,  25., "V"        ) ! D5
    CALL CalcH("decamethylcyclopentasiloxane",  "541-02-6", 0.74,  23., "X", "1573") ! D5
    CALL CalcH("decamethylcyclopentasiloxane",  "541-02-6", 1.1,   28., "X", "3081") ! D5
    ! TMS:
    CALL CalcH("trimethylsilanol",             "1066-40-6", -2.24, 25., "M"        ) ! TMS
    CALL CalcH("trimethylsilanol",             "1066-40-6", -2.74, 25., "X", "3095") ! TMS

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKAW, temp, type_, xref_)
      IMPLICIT NONE
      CHARACTER(LEN=*),           INTENT(IN) :: chem_
      CHARACTER(LEN=*),           INTENT(IN) :: casrn_
      REAL,                       INTENT(IN) :: logKAW
      REAL,                       INTENT(IN) :: temp
      CHARACTER,                  INTENT(IN) :: type_
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: xref_
      INTEGER :: temp_int ! T as an integer [K]
      LOGICAL :: l_output
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      l_output = .TRUE.
      IF (type_ == "X") THEN
        ! only use value here if it is in a paper that I don't have:
        IF (unread_bib(xref_)) THEN
          CALL SettypeX(xref_)
        ELSE
          l_output = .FALSE.
        ENDIF
      ENDIF
      IF (l_output) THEN
        temp_int = NINT(temp+CtoK)
        IF (temp_int /= 298) CALL MakeNoteOtherTemp(TRIM(str(temp_int)))
        CALL Output(KHcc_TO_HcpSI(10.**logKAW,temp+CtoK))
      ELSE
        seenote = "" ! reset so that next CALL Output won't use it
        type    = "" ! reset to invalid value that will be overwritten
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref3094

  !---------------------------------------------------------------------------

  SUBROUTINE ref3095 ! KHcc [1]
    IMPLICIT NONE

    ref = "3095"
    type = "V"

    ! Table 6a:
    CALL CalcH("hexamethyldisiloxane",                "107-46-0", 95.     ) ! L2
    CALL CalcH("octamethyltrisiloxane",               "107-51-7", 332.    ) ! L3
    CALL CalcH("decamethyltetrasiloxane",             "141-62-8", 1310.   ) ! L4
    CALL CalcH("dodecamethylpentasiloxane",           "141-63-9", 4663.   ) ! L5
    CALL CalcH("tetradecamethylhexasiloxane",         "107-52-8", 15075.  ) ! L6
    CALL CalcH("hexadecamethylheptasiloxane",         "541-01-5", 52790.  ) ! L7
    CALL CalcH("octadecamethyloctasiloxane",          "556-69-4", 121809. ) ! L8
    ! Table 6b:
    CALL CalcH("hexamethylcyclotrisiloxane",          "541-05-9", 72.     ) ! D3
    CALL CalcH("octamethylcyclotetrasiloxane",        "556-67-2", 487.    ) ! D4
    CALL CalcH("decamethylcyclopentasiloxane",        "541-02-6", 268.    ) ! D5
    ! Table 6c:
    CALL CalcH("trimethylsilanol",                   "1066-40-6", 0.00182 ) ! MOH, TMS
    CALL CalcH("pentamethyldisiloxanol",            "56428-93-4", 0.550   ) ! MDOH
    CALL CalcH("dimethylsilanediol",                 "1066-42-8", 0.0014  ) ! D(OH)2
    CALL CalcH("tetramethyldisiloxane-1,3-diol",     "1118-15-6", 0.0023  ) ! HOD2OH
    CALL CalcH("hexamethyltrisiloxane-1,5-diol",     "3663-50-1", 0.120   ) ! HOD3OH
    CALL CalcH("octamethyltetrasiloxane-1,7-diol",   "3081-07-0", 0.150   ) ! HOD4OH
    CALL CalcH("pentamethylcyclotrisiloxanol",     "106916-50-1", 0.380   ) ! D2TOH
    CALL CalcH("heptamethylcyclotetrasiloxanol",     "5290-02-8", 1.780   ) ! D3TOH
    CALL CalcH("nonamethylcyclopentasiloxanol",      "5290-04-0", 5.730   ) ! D4TOH

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0/KHcc)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3095

  !---------------------------------------------------------------------------

  SUBROUTINE ref3096 ! KHcc [1]
    IMPLICIT NONE

    ref = "3096"
    type = "M"

    ! Table 2:
    CALL CalcH("octamethylcyclotetrasiloxane",   "556-67-2",  2.69, 21.7) ! D4
    CALL CalcH("decamethylcyclopentasiloxane",   "541-02-6",  3.13, 24.6) ! D5
    CALL CalcH("dodecamethylcyclohexasiloxane",  "540-97-6",  3.01, 23.6) ! D6
    CALL CalcH("dimethylsilanediol",            "1066-42-8", -6.84, 20.1) ! C2H8O2Si

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKAW, temp)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logKAW
      REAL,             INTENT(IN) :: temp
      INTEGER :: temp_int ! T as an integer [K]
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      temp_int = NINT(temp+CtoK)
      IF (temp_int /= 298) CALL MakeNoteOtherTemp(TRIM(str(temp_int)))
      CALL Output(KHcc_TO_HcpSI(10.**logKAW,temp+CtoK))
    END SUBROUTINE CalcH

  END SUBROUTINE ref3096

  !---------------------------------------------------------------------------

  SUBROUTINE ref3097 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3097"
    type = "Q"

    chem = "IEPOX" ; casrn = "_CAS-86"
    CALL MakeNote_range(1.9E7*Hcp_TO_HcpSI,9.6E8*Hcp_TO_HcpSI)
    CALL Output(DUMMY)

  END SUBROUTINE ref3097

  !---------------------------------------------------------------------------

  SUBROUTINE ref3098 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3098"
    type = "Q"

    chem = "TOL_EPOX" ; casrn = "_CAS-85"
    CALL Output(2.5e5*Hcp_TO_HcpSI)

  END SUBROUTINE ref3098

  !---------------------------------------------------------------------------

  SUBROUTINE ref3100 ! special definition (aq. mol fraction and p in psia)
    IMPLICIT NONE
    REAL :: H70, H100, H160, H220

    ref = "3100"
    type = "M"
    H70 =  avg_H( &
      (/ 17.4, 20.9, 25.4, 29.4, 30.9, 39.9, 41.4, 48.4, 55.9, &
      60.9, 63.9, 71.4, 76.4 /), &
      (/ 12.24, 20.74, 24.88, 26.82, 30.12, 36.41, 39.09, 43.52, 48.84, &
      54.01, 55.89, 63.95, 66.83 /))
    H100 =  avg_H( &
      (/ 23.9, 41.9, 46.9, 53.9, 59.9, 70.4, 80.4, 102.8, 116.9 /), &
      (/ 15.42, 26.69, 32.25, 34.29, 39.84, 45.04, 49.94, 60.04, 67.72 /))
    H160 = avg_H( &
      (/ 17.9, 28.4, 43.4, 53.4, 62.4, 63.4, 82.4, 100.8, 169.5 /), &
      (/ 6.16, 9.49, 13.91, 17.83, 21.18, 21.84, 29.00, 33.78, 51.15 /))
    H220 = avg_H( &
      (/ 20.9, 53.9, 74.4, 94.9, 112.8, 132.3, 151.8, 170.8 /), &
      (/ 1.40, 10.08, 15.06, 20.05, 25.39, 28.51, 33.85, 37.12 /))
    chem = "propyne" ; casrn = "74-99-7" ! CH3CCH
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    ! convert Fahrenheit to K:
    temp = ((/ 70., 100., 160., 220. /) - 32.) * 5./9. + CtoK
    Harray = (/ H70, H100, H160, H220 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  CONTAINS

    REAL FUNCTION avg_H(psia, molfrac)
      REAL, DIMENSION(:), INTENT(IN) :: psia, molfrac
      REAL, DIMENSION(SIZE(psia)) :: c, p
      REAL, PARAMETER :: psia2Pa = 9.80665 * 0.45359237 / (0.0254**2) ! 6894.
      c = 1E-4 * molfrac * cH2O
      p = psia * psia2Pa
      avg_H = SUM(c/p)/SIZE(psia)
      ! to check results, see gnuplot/ref3100.gnu
    END FUNCTION avg_H
  END SUBROUTINE ref3100

  !---------------------------------------------------------------------------

  SUBROUTINE ref3110 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "3110"
    type = "V"
    ! Tab. 1:
    CALL CalcH("chlorobenzene",              "108-90-7", 0.367E3 ) ! CB
    CALL CalcH("1,2-dichlorobenzene",         "95-50-1", 0.245E3 ) ! 1,2-CB
    CALL CalcH("1,3-dichlorobenzene",        "541-73-1", 0.370E3 ) ! 1,3-CB
    CALL CalcH("1,4-dichlorobenzene",        "106-46-7", 0.171E3 ) ! 1,4-CB
    CALL CalcH("1,2,3-trichlorobenzene",      "87-61-6", 0.239E3 ) ! 1,2,3-CB
    CALL CalcH("1,2,4-trichlorobenzene",     "120-82-1", 0.277E3 ) ! 1,2,4-CB
    CALL CalcH("1,3,5-trichlorobenzene",     "108-70-3", 1.10E3  ) ! 1,3,5-CB
    CALL CalcH("1,2,3,4-tetrachlorobenzene", "634-66-2", 0.144E3 ) ! 1,2,3,4-CB
    CALL CalcH("1,2,3,5-tetrachlorobenzene", "634-90-2", 0.590E3 ) ! 1,2,3,5-CB
    CALL CalcH("1,2,4,5-tetrachlorobenzene",  "95-94-3", 0.122E3 ) ! 1,2,4,5-CB
    CALL CalcH("pentachlorobenzene",         "608-93-5", 0.085E3 ) ! penta-CB
    CALL CalcH("hexachlorobenzene",          "118-74-1", 0.139E3 ) ! hexa-CB

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

  END SUBROUTINE ref3110

  !---------------------------------------------------------------------------

  SUBROUTINE ref3111 ! KHcc [1]
    IMPLICIT NONE

    ref = "3111"
    type = "M"

    CALL CalcH("pentanal", "110-62-3", 5.94E-3, 23.)
    CALL CalcH("propanal", "123-38-6", 3.21E-3, 23.)

    ! Tab. S3 (supplement):
    chem = "methanal" ; casrn = "50-00-0" ! HCHO formaldehyde
    ALLOCATE(temp(3), Harray(3))
    temp = (/ 23., 40., 55. /) + CtoK
    Harray = (/ 1.06E-5, 2.42E-5, 6.40E-5 /)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc, temp)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc, temp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp(TRIM(str(NINT(temp+CtoK))))
      CALL Output(KHcc_TO_HcpSI(KHcc,temp+CtoK))
    END SUBROUTINE CalcH

  END SUBROUTINE ref3111

  !---------------------------------------------------------------------------

  SUBROUTINE ref3112 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "3112"
    type = "M"
    ! Table 2:
    CALL CalcH("propanone",    "67-64-1", (/ 454., 686., 1016., 1489., 2086., 2910. /)     )
    CALL CalcH("butanone",     "78-93-3", (/ 721., 1167., 1775., 2670., 3733., 4960. /)    )
    CALL CalcH("2-pentanone", "107-87-9", (/ 1149., 1819., 2826., 4300., 6290., 8690. /)   )
    CALL CalcH("2-hexanone",  "591-78-6", (/ 1497., 2429., 3822., 5740., 9370., 11900. /)  )
    CALL CalcH("2-heptanone", "110-43-0", (/ 2220., 3630., 5680., 9510., 15830., 21730. /) )
    ! Table 3:
    CALL CalcH("ethanol",      "64-17-5", (/ 68., 144., 251., 382., 613., 950. /)          )
    CALL CalcH("1-propanol",   "71-23-8", (/ 126., 220., 382., 626., 1070., 2030. /)       )
    CALL CalcH("1-butanol",    "71-36-3", (/ 184., 310., 624., 1000., 1730., 2680. /)      )
    CALL CalcH("1-pentanol",   "71-41-0", (/ 160., 369., 683., 1180., 2000., 3410. /)      )
    CALL CalcH("1-hexanol",   "111-27-3", (/ 360., 620., 1137., 1840., 3140., 4510. /)     )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpx)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHpx
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep( (/ 313., 323., 333., 343., 353., 363. /), &
        cH2O/(1E3*KHpx), Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL MakeNote("alsosalt")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3112

  !---------------------------------------------------------------------------

  ! ref3113 no original measurements, only references to other papers

  !---------------------------------------------------------------------------

  ! ref3114 no original measurements, only references to other papers

  !---------------------------------------------------------------------------

  SUBROUTINE ref3115 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "3115"
    type = "M"
    ! Table 19, p. 104 (page 122 in pdf):
    CALL CalcH("dimethyl sulfide",    "75-18-3", &
      (/ 17610., 24400., 31000., 38510. /) ) ! CH3SCH3 DMS
    CALL CalcH("dimethyl disulfide", "624-92-0", &
      (/ 13880., 19830., 25500., 34100. /) ) ! CH3SSCH3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpx)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHpx
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep( (/ 313., 323., 333., 343. /), &
        cH2O/(1E3*KHpx), Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL MakeNote("alsosalt")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3115

  !---------------------------------------------------------------------------

  SUBROUTINE ref3116 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "3116"
    type = "M"

    ! Table 2:
    CALL CalcH("methyl t-butyl ether", "1634-04-4", (/ 10040., 15620.,  23860.,  34000. /)) ! CH3OC(CH3)3 MTBE
    CALL CalcH("ethyl t-butyl ether",   "637-92-3", (/ 23150., 41700.,  59500.,  78700. /)) ! C2H5OC(CH3)3 ETBE
    CALL CalcH("methylbenzene",         "108-88-3", (/ 58810., 74400.,  90200., 107000. /)) ! C6H5CH3 toluene
    CALL CalcH("1,2-dimethylbenzene",    "95-47-6", (/ 47760., 64800.,  84800.,  95800. /)) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("ethylbenzene",          "100-41-4", (/ 72140., 92600., 124400., 149200. /)) ! C6H5C2H5

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep( (/ 313., 323., 333., 343. /), &
        cH2O/(1E3*H), Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL MakeNote("alsosalt")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3116

  !---------------------------------------------------------------------------

  SUBROUTINE ref3117 ! KHpx [1E2 kPa]
    IMPLICIT NONE

    ref = "3117"
    type = "M"
    chem = "2-methylpropane" ; casrn = "75-28-5" ! HC(CH3)3 isobutane
    ndata = 11
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 293.30, 292.75, 290.85, 288.65, 283.15, 283.15, 279.35, &
      277.85, 276.95, 276.05, 274.95 /)
    Harray = cH2O * 1E-5 / (/ 3580., 3910., 3460., 2910., &
      2230., 2310., 1480., 1350., 1320., 1270., 1350. /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref3117

  !---------------------------------------------------------------------------

  SUBROUTINE ref3118 ! KHcc [1]
    IMPLICIT NONE

    ref = "3118"
    type = "M"

    ! Table 1:
    CALL CalcH("2,4,6-trichloroanisole",          "87-40-1", (/ 9.00, 9.76  /)) ! 2,4,6-triCl
    CALL CalcH("2,4,6-tribromoanisole",          "607-99-8", (/ 1.70, 7.64  /)) ! 2,4,6-triBr
    CALL CalcH("2,3,6-trichloroanisole",       "50375-10-5", (/ 3.30, 9.14  /)) ! 2,3,6-triCl
    CALL CalcH("2,3,6-tribromoanisole",        "95970-19-7", (/ 7.10, 13.08 /)) ! 2,3,6-triBr
    CALL CalcH("6-bromo-2,4-dichloroanisole",  "60633-26-3", (/ 4.48, 8.96  /)) ! 2,4-diCl-6-Br
    CALL CalcH("4-bromo-2,6-dichloroanisole",  "19240-91-6", (/ 2.80, 8.62  /)) ! 2,6-diCl-4-Br
    CALL CalcH("2,6-dibromo-4-chloroanisole", "174913-44-1", (/ 1.65, 7.92  /)) ! 2,6-diBr-4-Cl
    CALL CalcH("6-bromo-2,5-dichloroanisole", "174913-14-5", (/ 4.80, 9.29  /)) ! 2,5-diCl-6-Br
    CALL CalcH("3-bromo-2,6-dichloroanisole",     "_CAS-88", (/ 3.40, 6.06  /)) ! 2,6-diCl-3-Br
    CALL CalcH("2,6-dibromo-3-chloroanisole",     "_CAS-89", (/ 5.40, 6.05  /)) ! 2,6-diBr-3-Cl

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KH
      REAL, DIMENSION(SIZE(KH)) :: temp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      temp = (/ 22., 45. /) + CtoK
      CALL HTdep(temp, KHcc_TO_HcpSI(KH/100.,temp), Hominus, mindHR)
      IF ((casrn_=="95970-19-7").OR.(casrn_=="_CAS-88").OR.(casrn_=="_CAS-89")) THEN
        CALL MakeNote(TRIM(ref), TRIM(citet())// &
          " also cite a Henry's law constant from \citet{1524} even " // &
          "though this species is not mentioned there. " // &
          "There might be a mix up of the different haloanisoles.")
      ENDIF
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3118

  !---------------------------------------------------------------------------

  SUBROUTINE ref3119 ! KHcc [1]
    IMPLICIT NONE

    ref = "3119"
    type = "M"
    chem = "carbon oxide sulfide" ; casrn = "463-58-1" ! OCS carbonyl sulfide
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10., 15., 20., 25. /)
    temp = temp+CtoK
    Harray = (/ 1.5, 1.9, 2.2, 2.7 /) ! KHcc
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref3119

  !---------------------------------------------------------------------------

  SUBROUTINE ref3120 ! Hcc [1]
    IMPLICIT NONE

    ref = "3120"
    type = "M"

    ! Table 2:
    CALL CalcH("nitrogen monoxide",    "10102-43-9", 0.0542 , 288.) ! NO
    CALL CalcH("nitrogen dioxide",     "10102-44-0", 0.556  , 288.) ! NO2
    CALL CalcH("dinitrogen tetroxide", "10544-72-6", 48.2   , 288.) ! N2O4

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcc, temp)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Hcc, temp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp(TRIM(str(NINT(temp))))
      CALL Output(Hcc_TO_HcpSI(Hcc,temp))
    END SUBROUTINE CalcH

  END SUBROUTINE ref3120

  !---------------------------------------------------------------------------

  SUBROUTINE ref3121 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "3121"
    type = "?"

    CALL CalcH("sarin",         "107-44-8", 5.34E-7 ) ! C4H{10}FO2P
    CALL CalcH("tabun",          "77-81-6", 1.52E-7 ) ! C5H{11}N2O2P
    CALL CalcH("Agent VX",    "50782-69-9", 8.19E-9 ) ! C{11}H{26}NO2PS
    CALL CalcH("soman",          "96-64-0", 4.56E-6 ) ! C7H{16}FO2P
    CALL CalcH("mustard gas", "69020-37-7", 2.4E-5  ) ! (ClCH2CH2)2S

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(1./(atm*H))

    END SUBROUTINE CalcH

  END SUBROUTINE ref3121

  !---------------------------------------------------------------------------

  SUBROUTINE ref3122 ! Hcc [1]
    IMPLICIT NONE
    REAL :: average

    ref = "3122"
    chem = "sulfur dioxide" ; casrn = "7446-09-5" ! SO2
    type = "V"
    ! Table I:
    average = 1E3 * (0.023 + 0.028 + 0.026 + 0.028) / 4.
    CALL Output(average * Hcc_TO_HcpSI_atT0, mindHR)

  END SUBROUTINE ref3122

  !---------------------------------------------------------------------------

  SUBROUTINE ref3123 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3123"
    type = "M"
    chem = "dinitrogen tetroxide" ; casrn = "10544-72-6" ! N2O4
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 20., 30. /) + CtoK
    Harray = (/ 1.39, 1.23 /)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref3123

  !---------------------------------------------------------------------------

  SUBROUTINE ref3124 ! KHpx [GPa]
    IMPLICIT NONE

    ref = "3124"
    type = "M"
    ! eqn (6) on page 161:
    ! ln H = 285.8038 + 0.017687 T - 13546.17/T - 42.85973 ln T
    CALL CalcH("butane", "106-97-8", 285.8038, 0.017687, -13546.17, -42.85973)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: A, B, C, D
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! ln(KHpx) = A + B*T + C/T + D*ln(T)
      Hominus = cH2O / (1E9*EXP(A + B*T0 + C/T0 + D*LOG(T0)))
      ! analytical derivative: (see also gnuplot/ref3124.gnu)
      mindHR = B*T0*T0 - C + D*T0 ! d(lnH)/d(1/T) = -delta H/R
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3124

  !---------------------------------------------------------------------------

  ! ref3125 only high-pressure data

  !---------------------------------------------------------------------------

  SUBROUTINE ref3126 ! KHpx [MPa]
    IMPLICIT NONE

    ! Table 2:
    ref = "3126"
    type = "M"
    chem = "methane" ; casrn = "74-82-8" ! CH4
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 1.20, 10.22, 12.46 /) + CtoK
    Harray = cH2O * 1E-6 / (/ 2275.80, 3041.82, 3214.17 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref3126

  !---------------------------------------------------------------------------

  ! ref3127 only high-pressure data

  !---------------------------------------------------------------------------

  ! ref3128 only high-pressure data

  !---------------------------------------------------------------------------

  SUBROUTINE ref3129 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "3129"
    type = "M"
    ! eqn (20) on page 218:
    ! ln(H) = 552.64799 + 0.078453 T - 21334.4 / T - 85.89736 ln(T)
    CALL CalcH("propane", "74-98-6", 552.64799, 0.078453, -21334.4, -85.89736)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: A, B, C, D
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! ln(KHpx) = A + B*T + C/T + D*ln(T)
      Hominus = cH2O / (1E3*EXP(A + B*T0 + C/T0 + D*LOG(T0)))
      ! analytical derivative: (see also ref3124)
      mindHR = B*T0*T0 - C + D*T0 ! d(lnH)/d(1/T) = -delta H/R
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3129

  !---------------------------------------------------------------------------

  ! ref3130 only high-pressure data

  !---------------------------------------------------------------------------

  ! ref3131 only high-pressure data

  !---------------------------------------------------------------------------

  SUBROUTINE ref3132 ! KHcc [1]
    IMPLICIT NONE

    ref = "3132"
    type = "M"
    ! Tab. 1 for pure water:
    chem = "methanol" ; casrn = "67-56-1" ! CH3OH
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 40., 50., 60., 65. /) + CtoK
    Harray = (/ 4.76, 7.69, 13.2, 16.1 /) * 1E-4
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("highTextrapol")
    CALL MakeNote("alsosalt")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref3132

  !---------------------------------------------------------------------------

  ! ref3133 only infinite dilution activity coefficients but no Henry's
  ! law constants

  !---------------------------------------------------------------------------

  SUBROUTINE ref3134 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "3134"
    type = "M"
    chem = "ethanedial" ; casrn = "107-22-2" ! OHCCHO glyoxal
    CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-range", &
      TRIM(citet())//" found average effective Henry's law constants"// &
      " for CHOCHO in the range "// &
      TRIM(H_range(1.6e8*Hcp_TO_HcpSI,6e8*Hcp_TO_HcpSI))// &
      " for solutions containing ammonium sulfate and/or fulvic acid. A"// &
      " salting-in effect by fulvic acid was observed even in the absence"// &
      " of sulfate.")
    CALL Output(DUMMY)

  END SUBROUTINE ref3134

  !---------------------------------------------------------------------------

  SUBROUTINE ref3135 ! KHpc [kPa*m3/mol]
    IMPLICIT NONE

    ref = "3135"
    type = "C" ! from SRC Phys Prop Database

    ! Table 1:
    CALL CalcH("benzene",                 "71-43-2", 0.557) ! C6H6 
    CALL CalcH("methylbenzene",          "108-88-3", 0.673) ! C6H5CH3 toluene
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6", 0.525) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",    "108-38-3", 0.728) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",    "106-42-3", 0.699) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("ethylbenzene",           "100-41-4", 0.798) ! C6H5C2H5 
    CALL CalcH("hexanal",                 "66-25-1", 0.021) ! C5H{11}CHO 
    CALL CalcH("heptanal",               "111-71-7", 0.027) ! C6H{13}CHO 
    CALL CalcH("octanal",                "124-13-0", 0.052) ! C7H{15}CHO 
    CALL CalcH("nonanal",                "124-19-6", 0.074) ! C8H{17}CHO 
    CALL CalcH("decanal",                "112-31-2", 0.182) ! C9H{19}CHO 
    CALL CalcH("fluorobenzene",          "462-06-6", 0.638) ! C6H5F 
    CALL CalcH("1-chloro-2-bromoethane", "107-04-0", 0.092) ! C2H4BrCl 

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/(KHpc*1.E3)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref3135

  !---------------------------------------------------------------------------
END MODULE Henry_ref3500

!*****************************************************************************
!                                  end of file
!*****************************************************************************
