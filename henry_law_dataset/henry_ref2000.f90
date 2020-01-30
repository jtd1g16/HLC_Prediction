!*****************************************************************************
!                   Time-stamp: <2015-04-22 14:08:02 sander>
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

MODULE henry_ref2000

  USE henry_mem
  USE henry_util
  IMPLICIT NONE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ref1501 ! Hcc [1]
    IMPLICIT NONE

    ref = "1501"

    ! Table 1:
    type = "V"
    CALL CalcH("ethenylbenzene",              "100-42-5", 0.91) ! C8H8 styrene
    CALL CalcH("alpha-methyl styrene",         "98-83-9", 0.91)
    CALL CalcH("1-isopropyl-4-methylbenzene",  "99-87-6", 0.50) ! C6H4CH3C3H7
    CALL CalcH("1,2,3-trimethylbenzene",      "526-73-8", 0.89) ! C6H3(CH3)3
    CALL CalcH("1,2,4-trimethylbenzene",       "95-63-6", 0.63) ! C6H3(CH3)3
    CALL CalcH("1,3,5-trimethylbenzene",      "108-67-8", 0.66) ! C6H3(CH3)3 mesitylene
    CALL CalcH("naphthalene",                  "91-20-3", 1.76) ! C{10}H8
    CALL CalcH("1-methylnaphthalene",          "90-12-0", 1.79)
    CALL CalcH("biphenyl",                     "92-52-4", 1.95) ! (C6H5)2
    CALL CalcH("indane",                      "496-11-7", 1.07)
    CALL CalcH("1,2,3-trichlorobenzene",       "87-61-6", 0.91) ! C6H3Cl3
    CALL CalcH("1,3,5-trichlorobenzene",      "108-70-3", 0.57) ! C6H3Cl3
    CALL CalcH("(trifluoromethyl)-benzene",    "98-08-8", 0.18)
    ! Table 2:
    type = "R"
    CALL CalcH("2-ethoxy-ethanol",            "110-80-5", 4.91)
    CALL CalcH("4-methylacetophenone",        "122-00-9", 3.45)
    CALL CalcH("benzenenitrile",              "100-47-0", 3.09) ! C6H5CN benzonitrile
    CALL CalcH("2-methylaniline",              "95-53-4", 4.06)
    CALL CalcH("4-methylaniline",             "106-49-0", 4.09) ! C7H9N $p$-toluidine
    CALL CalcH("1-amino-2-chlorobenzene",      "95-51-2", 3.60) ! C6H6ClN $o$-chloroaniline
    CALL CalcH("$m$-chloroaniline",           "108-42-9", 4.27)
    CALL CalcH("1-amino-4-chlorobenzene",     "106-47-8", 4.33) ! C6H6ClN $p$-chloroaniline
    CALL CalcH("2-methoxyaniline",             "90-04-0", 4.49)
    CALL CalcH("3-methoxyaniline",            "536-90-3", 5.35)
    CALL CalcH("4-methoxyaniline",            "104-94-9", 5.49) ! C7H9NO
    CALL CalcH("2-nitrobenzenamine",           "88-74-4", 5.41) ! C6H6N2O2 2-nitroaniline
    CALL CalcH("4-chloro-3-methylphenol",      "59-50-7", 4.98)
    CALL CalcH("2-iodophenol",                "533-58-4", 4.55)
    ! Table 3:
    type = "R"
    CALL CalcH("triethylphosphate",            "78-40-0", 5.53)
    CALL CalcH("4-methyl-benzaldehyde",       "104-87-0", 3.13)
    CALL CalcH("methyl benzoate",              "93-58-3", 2.88) ! C6H5COOCH3
    CALL CalcH("ethyl benzoate",               "93-89-0", 2.67)
    !CALL CalcH("benzenenitrile", "100-47-0", 3.09) ! same data in Tab. 2
    CALL CalcH("2,6-dimethylaniline",          "87-62-7", 3.82)
    CALL CalcH("3-nitrobenzenamine",           "99-09-2", 6.49) ! C6H6N2O2 3-nitroaniline
    CALL CalcH("4-nitrobenzenamine",          "100-01-6", 7.54) ! C6H6N2O2 4-nitroaniline
    CALL CalcH("1-naphthylamine",             "134-32-7", 5.34) ! C{10}H9N
    CALL CalcH("2-naphthylamine",              "91-59-8", 5.48)
    CALL CalcH("N-methylaniline",             "100-61-8", 3.44)
    CALL CalcH("benzamide",                    "55-21-0", 8.07)
    CALL CalcH("2,3-dimethylphenol",          "526-75-0", 4.52) ! C8H{10}O 2,3-xylenol
    CALL CalcH("2,4-dimethylphenol",          "105-67-9", 4.41) ! C8H{10}O 2,4-xylenol
    CALL CalcH("2,5-dimethylphenol",           "95-87-4", 4.34) ! C8H{10}O 2,5-xylenol
    CALL CalcH("2,6-dimethylphenol",          "576-26-1", 3.86) ! C8H{10}O 2,6-xylenol
    CALL CalcH("3,4-dimethylphenol",           "95-65-8", 4.77) ! C8H{10}O 3,4-xylenol
    CALL CalcH("3,5-dimethylphenol",          "108-68-9", 4.60) ! C8H{10}O 3,5-xylenol
    CALL CalcH("3-ethylphenol",               "620-17-7", 4.59) ! C8H{10}O 3-ethylphenol
    CALL CalcH("4-ethylphenol",               "123-07-9", 4.50)
    CALL CalcH("4-propylphenol",              "645-56-7", 4.33)
    CALL CalcH("2-hydroxyfluorobenzene",      "367-12-4", 3.88) ! C6H5ClO $o$-fluorophenol
    CALL CalcH("4-hydroxyfluorobenzene",      "371-41-5", 4.54) ! C6H5ClO $p$-fluorophenol
    CALL CalcH("2-hydroxychlorobenzene",       "95-57-8", 3.34) ! C6H5ClO $o$-chlorophenol
    CALL CalcH("3-hydroxychlorobenzene",      "108-43-0", 4.85) ! C6H5ClO $m$-chlorophenol
    CALL CalcH("4-hydroxychlorobenzene",      "106-48-9", 5.16) ! C6H5ClO $p$-chlorophenol
    CALL CalcH("4-bromophenol",               "106-41-2", 5.22) ! HOC6H4Br
    CALL CalcH("2-methoxyphenol",              "90-05-1", 4.09)
    CALL CalcH("3-methoxyphenol",             "150-19-6", 5.62)
    CALL CalcH("2-nitrophenol",                "88-75-5", 3.36) ! HOC6H4(NO2)
    CALL CalcH("1-naphthol",                   "90-15-3", 5.63)
    CALL CalcH("2-naphthol",                  "135-19-3", 5.95)
    CALL CalcH("benzyl alcohol",              "100-51-6", 4.86)
    CALL CalcH("2-phenylethanol",              "60-12-8", 4.98) ! C8H{10}O
    CALL CalcH("3-phenylpropanol",            "122-97-4", 5.08)
    CALL CalcH("3-cyanopyridine",             "100-54-9", 4.95)
    CALL CalcH("4-cyanopyridine",             "100-48-1", 4.42)
    CALL CalcH("3-formylpyridine",            "500-22-1", 5.21)
    CALL CalcH("4-formylpyridine",            "872-85-5", 5.14)
    CALL CalcH("3-acetylpyridine",            "350-03-8", 6.06)
    CALL CalcH("4-acetylpyridine",           "1122-54-9", 5.59)
    CALL CalcH("quinoline",                    "91-22-5", 4.20)
    CALL CalcH("N-methylpiperidine",          "626-67-5", 2.77)

    ! Table 4:
    type = "R"
    CALL CalcH("N,N-dimethylformamide",        "68-12-2", 5.73)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logLW)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logLW
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(10.**logLW * Hcc_TO_HcpSI_atT0)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1501

  !---------------------------------------------------------------------------

  SUBROUTINE ref1502 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1502"
    type = "M"
    chem = "nitrogen dioxide" ; casrn = "10102-44-0" ! NO2
    CALL Output(1.4E-2*Hcp_TO_HcpSI)

  END SUBROUTINE ref1502

  !---------------------------------------------------------------------------

  SUBROUTINE ref1513 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1513"
    type = "M"

    CALL CalcH("hydroxybenzene",            "108-95-2", 5850., 11.6)
    CALL CalcH("1-hydroxy-2-methylbenzene",  "95-48-7", 6680., 15.4)
    CALL CalcH("2-nitrophenol",              "88-75-5", 6270., 16.6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      mindHR = A
      Hominus = Hcp_TO_HcpSI * EXP(A/T0-B)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1513

  !---------------------------------------------------------------------------

  SUBROUTINE ref1514 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1514"
    type = "M"

    CALL CalcH("2-nitrophenol", "88-75-5", 6290., 16.6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      mindHR = A
      Hominus = Hcp_TO_HcpSI * EXP(A/T0-B)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1514

  !---------------------------------------------------------------------------

  SUBROUTINE ref1516 ! Hbp [mol/(kg*1E3Pa)]
    IMPLICIT NONE
    ref = "1516"
    type = "M"
    chem = "ethanol" ; casrn = "64-17-5" ! C2H5OH
    Hominus    = (1./0.877) * rhoH2O / 1E3
    CALL Output(Hominus)
  END SUBROUTINE ref1516

  !---------------------------------------------------------------------------

  SUBROUTINE ref1524 ! KHcc [1]
    IMPLICIT NONE
    ref = "1524"
    type = "M"

    ! Table 5:
    CALL CalcH("1-chloro-2-methoxybenzene",                       "766-51-8",  0.4 ) ! 2-chloroanisole
    CALL CalcH("1-chloro-3-methoxybenzene",                      "2845-89-8",  0.9 ) ! 3-chloroanisole
    CALL CalcH("1-chloro-4-methoxybenzene",                       "623-12-1",  0.7 ) ! 4-chloroanisole
    CALL CalcH("1,2-dichloro-3-methoxybenzene",                  "1984-59-4",  1.8 ) ! 2,3-dichloroanisole
    CALL CalcH("1,5-dichloro-2-methoxybenzene",                   "553-82-2",  3.3 ) ! 2,4-dichloroanisole
    CALL CalcH("1,4-dichloro-2-methoxybenzene",                  "1984-58-3",  1.9 ) ! 2,5-dichloroanisole
    CALL CalcH("1,3-dichloro-2-methoxybenzene",                  "1984-65-2",  4.6 ) ! 2,6-dichloroanisole
    CALL CalcH("1,2-dichloro-4-methoxybenzene",                 "36404-30-5",  4.4 ) ! 3,4-dichloroanisole
    CALL CalcH("1,3-dichloro-5-methoxybenzene",                 "33719-74-3", 17.6 ) ! 3,5-dichloroanisole
    CALL CalcH("1,2,3-trichloro-4-methoxybenzene",              "54135-80-7",  3.0 ) ! 2,3,4-trichloroanisole
    CALL CalcH("1,2,5-trichloro-3-methoxybenzene",              "54135-81-8",  5.3 ) ! 2,3,5-trichloroanisole
    CALL CalcH("1,2,4-trichloro-3-methoxybenzene",              "50375-10-5",  4.1 ) ! 2,3,6-trichloroanisole
    CALL CalcH("1,2,4-trichloro-5-methoxybenzene",               "6130-75-2",  3.8 ) ! 2,4,5-trichloroanisole
    CALL CalcH("1,3,5-trichloro-2-methoxybenzene",                 "87-40-1",  8.8 ) ! 2,4,6-trichloroanisole
    CALL CalcH("1,2,3-trichloro-5-methoxybenzene",              "54135-82-9",  9.2 ) ! 3,4,5-trichloroanisole
    CALL CalcH("1,2,3,4-tetrachloro-5-methoxybenzene",            "938-86-3",  6.2 ) ! 2,3,4,5-tetrachloroanisole
    CALL CalcH("1,2,3,5-tetrachloro-4-methoxybenzene",            "938-22-7", 13.0 ) ! 2,3,4,6-tetrachloroanisole
    CALL CalcH("1,2,4,5-tetrachloro-3-methoxybenzene",           "6936-40-9", 12.8 ) ! 2,3,5,6-tetrachloroanisole
    CALL CalcH("pentachloromethoxybenzene",                      "1825-21-4", 18.8 ) ! pentachloroanisole
    CALL CalcH("1-bromo-2-methoxybenzene",                        "578-57-4",  1.4 ) ! 2-bromoanisole
    CALL CalcH("1-bromo-3-methoxybenzene",                       "2398-37-0",  5.6 ) ! 3-bromoanisole
    CALL CalcH("1-bromo-4-methoxybenzene",                        "104-92-7",  3.8 ) ! 4-bromoanisole
    CALL CalcH("1,5-dibromo-2-methoxybenzene",                  "21702-84-1",  0.5 ) ! 2,4-dibromoanisole
    CALL CalcH("1,3-dibromo-2-methoxybenzene",                  "38603-09-7",  1.1 ) ! 2,6-dibromoanisole
    CALL CalcH("1,3,5-tribromo-2-methoxybenzene",                 "607-99-8",  3.0 ) ! 2,4,6-tribromoanisole
    CALL CalcH("pentabromomethoxybenzene",                       "1825-26-9",  0.04) ! pentabromoanisole
    CALL CalcH("2-bromo-4-chloro-1-methoxybenzene",             "60633-25-2",  2.2 ) ! 2-bromo-4-chloroanisole
    CALL CalcH("2-bromo-6-chloro-1-methoxybenzene",            "174913-10-1",  2.96) ! 2-bromo-6-chloroanisole
    CALL CalcH("4-bromo-2-chloro-1-methoxybenzene",             "50638-47-6",  3.18) ! 4-bromo-2-chloroanisole
    CALL CalcH("2-bromo-3,5-dichloro-1-methoxybenzene",            "_CAS-14",  3.64) ! 2-bromo-3,5-dichloroanisole
    CALL CalcH("2-bromo-4,6-dichloro-1-methoxybenzene",         "60633-26-3",  3.34) ! 2-bromo-4,6-dichloroanisole
    CALL CalcH("4-bromo-2,3-dichloro-1-methoxybenzene",        "109803-52-3",  3.70) ! 4-bromo-2,3-dichloroanisole
    CALL CalcH("4-bromo-2,6-dichloro-1-methoxybenzene",         "19240-91-6",  3.36) ! 4-bromo-2,6-dichloroanisole
    CALL CalcH("4-bromo-3,5-dichloro-1-methoxybenzene",            "_CAS-18",  3.64) ! 4-bromo-3,5-dichloroanisole
    CALL CalcH("5-bromo-2,4-dichloro-1-methoxybenzene",            "_CAS-19",  3.70) ! 5-bromo-2,4-dichloroanisole
    CALL CalcH("6-bromo-2,3-dichloro-1-methoxybenzene",            "_CAS-20",  3.70) ! 6-bromo-2,3-dichloroanisole
    CALL CalcH("2-bromo-3,4,5-trichloro-1-methoxybenzene",         "_CAS-21",  4.12) ! 2-bromo-3,4,5-trichloroanisole
    CALL CalcH("3-bromo-2,4,6-trichloro-1-methoxybenzene",     "174913-28-1",  3.85) ! 3-bromo-2,4,6-trichloroanisole
    CALL CalcH("3-bromo-2,5,6-trichloro-1-methoxybenzene",      "78647-93-5",  3.86) ! 3-bromo-2,5,6-trichloroanisole
    CALL CalcH("4-bromo-2,3,6-trichloro-1-methoxybenzene",      "78647-87-7",  3.86) ! 4-bromo-2,3,6-trichloroanisole
    CALL CalcH("6-bromo-2,3,4-trichloro-1-methoxybenzene",         "_CAS-25",  3.84) ! 6-bromo-2,3,4-trichloroanisole
    CALL CalcH("4-bromo-2,3,5,6-tetrachloro-1-methoxybenzene", "174913-33-8",  4.39) ! 4-bromo-2,3,5,6-tetrachloroanisole
    CALL CalcH("2,6-dibromo-4-chloro-1-methoxybenzene",        "174913-44-1",  3.59) ! 2,6-dibromo-4-chloroanisole
    CALL CalcH("2,4-dibromo-3,5-dichloro-1-methoxybenzene",    "174913-52-1",  4.42) ! 2,4-dibromo-3,5-dichloroanisole
    CALL CalcH("2,4-dibromo-5,6-dichloro-1-methoxybenzene",        "_CAS-29",  4.13) ! 2,4-dibromo-5,6-dichloroanisole
    CALL CalcH("2,3-dibromo-5,6-dichloro-1-methoxybenzene",        "_CAS-30",  4.42) ! 2,3-dibromo-5,6-dichloroanisole
    CALL CalcH("2,6-dibromo-3,4,5-trichloro-1-methoxybenzene",     "_CAS-31",  4.68) ! 2,6-dibromo-3,4,5-trichloroanisole
    CALL CalcH("2,4,6-tribromo-3-chloro-1-methoxybenzene",     "174913-78-1",  4.42) ! 2,4,6-tribromo-3-chloroanisole

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNote(TRIM(ref), &
        "When comparing $H$ in table 4 with $K_{\rm gw}$ in table 5 of "// &
        TRIM(citet())//", it can be seen that the values refer to "// &
        "$K_{\rm gw}\times 100$ and not $K_{\rm gw}/100$.")
      CALL Output(KHcc_TIMES_HcpSI_atT0/(1E-2*KHcc))
    END SUBROUTINE CalcH

  END SUBROUTINE ref1524

  !---------------------------------------------------------------------------

  SUBROUTINE ref1525 ! KHcc [1]
    IMPLICIT NONE

    ref = "1525"

    CALL CalcH("bromomethane",                     "74-83-9",  3.468,  1221., 2.01E-01, "L"            ) ! CH3Br methyl bromide
    CALL CalcH("chloromethane",                    "74-87-3",  3.889,  1292., 3.05E-01, "L"            ) ! CH3Cl methyl chloride
    CALL CalcH("fluoromethane",                   "593-53-3",  2.298,   738., 6.04E-01, "2222"         ) ! CH3F
    CALL CalcH("iodomethane",                      "74-88-4",  4.059,  1416., 1.70E-01, "L"            ) ! CH3I methyl iodide
    CALL CalcH("dichloromethane",                  "75-09-2",  4.561,  1644., 9.04E-02, "L"            ) ! CH2Cl2 methylene chloride
    CALL CalcH("tribromomethane",                  "75-25-2",  5.476,  2120., 1.75E-02, "L"            ) ! CHBr3 bromoform
    CALL CalcH("dibromochloromethane",            "124-48-1",  6.296,  2273., 3.50E-02, "L"            ) ! CHClBr2
    CALL CalcH("bromodichloromethane",             "75-27-4",  6.143,  2130., 7.60E-02, "L"            ) ! CHCl2Br
    CALL CalcH("trichloromethane",                 "67-66-3",  5.343,  1830., 1.26E-01, "L"            ) ! CHCl3 chloroform
    CALL CalcH("dichlorodifluoromethane",          "75-71-8",  5.749,  1380., 1.10E+01, "L"            ) ! CF2Cl2 R12
    CALL CalcH("trichlorofluoromethane",           "75-69-4",  5.023,  1324., 3.21E+00, "L"            ) ! CFCl3 R11
    CALL CalcH("tetrachloromethane",               "56-23-5",  5.736,  1689., 9.49E-01, "L"            ) ! CCl4 carbontetrachloride
    CALL CalcH("chloroethane",                     "75-00-3",  3.406,  1110., 4.18E-01, "L"            ) ! C2H5Cl
    CALL CalcH("1,2-dibromoethane",               "106-93-4",  3.661,  1556., 2.25E-02, "1156"         ) ! C2H4Br2 ethylene dibromide
    CALL CalcH("1,1-dichloroethane",               "75-34-3",  4.416,  1498., 2.05E-01, "L"            ) ! CHCl2CH3
    CALL CalcH("1,2-dichloroethane",              "107-06-2",  4.434,  1705., 4.19E-02, "L"            ) ! CH2ClCH2Cl
    CALL CalcH("1,1,1-trichloroethane",            "71-55-6",  5.163,  1588., 5.62E-01, "L"            ) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1,2-trichloroethane",            "79-00-5",  5.219,  1989., 2.74E-02, "L"            ) ! CHCl2CH2Cl
    CALL CalcH("1,1,1,2-tetrachloroethane",       "630-20-6",  2.429,  1255., 1.40E-02, "L"            ) ! CCl3CH2Cl
    CALL CalcH("1,1,2-trichlorotrifluoroethane",   "76-13-1",  5.375,  1281., 1.01E+01, "1156"         ) ! C2F3Cl3 R113
    CALL CalcH("hexachloroethane",                 "67-72-1",  6.982,  2320., 1.17E-01, "2243"         ) ! C2Cl6
    CALL CalcH("1,2-dichloropropane",              "78-87-5",  4.878,  1730., 9.51E-02, "L"            ) ! C3H6Cl2
    CALL CalcH("1,3-dichloropropane",             "142-28-9",  3.888,  1577., 3.20E-02, "1150"         ) ! C3H6Cl2
    CALL CalcH("1,2,3-trichloropropane",           "96-18-4",  3.073,  1496., 9.33E-03, "L"            ) ! C3H5Cl3
    CALL CalcH("1-chlorobutane",                  "109-69-3",  4.488,  1388., 5.68E-01, "1150"         ) ! C4H9Cl
    CALL CalcH("2-chlorobutane",                   "78-86-4",  6.129,  1829., 7.74E-01, "1150"         ) ! C4H9Cl
    CALL CalcH("1,4-dichlorobutane",              "110-56-5",  2.438,  1234., 1.70E-02, "1150"         ) ! C4H8Cl2
    CALL CalcH("1-chloropentane",                 "543-59-9",  6.455,  1928., 7.53E-01, "1150"         ) ! C5H{11}Cl
    !CALL CalcH("2-methylpentane",                "107-83-5",  2.470,   288., 3.06E-01, "1156"         ) ! poor correlation
    CALL CalcH("hexane",                          "110-54-3", 12.150,  3143., 2.68E+01, "1156"         ) ! C6H{14}
    CALL CalcH("1-chlorohexane",                  "544-10-5",  6.073,  1812., 7.79E-01, "1150"         ) ! C6H{13}Cl
    !CALL CalcH("2-methylhexane",                 "591-76-4", -4.274, -1669., 2.62E+01, "903"          ) ! poor correlation
    CALL CalcH("heptane",                         "142-82-5",  6.532,  1491., 2.79E+01, "903"          ) ! C7H{16}
    !CALL CalcH("octane",                         "111-65-9",  12.08,  3263., 8.96E+00, "903"          ) ! poor correlation
    !CALL CalcH("nonane",                         "111-84-2",  1.104,   -39., 1.73E+01, "1156"         ) ! poor correlation
    ! ----------------------------------------------------------------------------------------------------------
    CALL CalcH("chloroethene",                     "75-01-4",  4.119,  1223., 8.91E-01, "L"            ) ! CH2CHCl vinyl chloride
    CALL CalcH("1,1-dichloroethene",               "75-35-4",  5.397,  1586., 9.75E-01, "L"            ) ! CH2CCl2
    CALL CalcH("(Z)-1,2-dichloroethene",          "156-59-2",  4.464,  1559., 1.40E-01, "L"            ) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("(E)-1,2-dichloroethene",          "156-60-5",  5.247,  1669., 3.59E-01, "L"            ) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",                  "79-01-6",  5.874,  1871., 3.14E-01, "L"            ) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",               "127-18-4",  6.394,  1955., 5.33E-01, "L"            ) ! C2Cl4 tetrachloroethylene
    CALL CalcH("cyclopentane",                    "287-92-3",  5.162,  1302., 5.25E+00, "903"          ) ! C5H{10}
    CALL CalcH("cyclohexane",                     "110-82-7",  5.154,  1279., 6.18E+00, "1156"         ) ! C6H{12}
    CALL CalcH("methylcyclohexane",               "108-87-2", 13.507,  3836., 2.63E+00, "903"          ) ! C6H{11}CH3
    CALL CalcH("benzene",                          "71-43-2",  5.053,  1693., 1.91E-01, "L"            ) ! C6H6
    CALL CalcH("bromobenzene",                    "108-86-1",  6.375,  2233., 5.72E-02, "903"          ) ! C6H5Br
    CALL CalcH("chlorobenzene",                   "108-90-7",  4.225,  1507., 1.22E-01, "L"            ) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",              "95-50-1",  7.045,  2436., 5.47E-02, "L"            ) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",             "541-73-1",  2.436,   986., 1.18E-01, "1156"         ) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",             "106-46-7",  2.649,  1054., 1.13E-01, "1156"         ) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,2,4-trichlorobenzene",          "120-82-1",  4.381,  1622., 7.04E-02, "1156"         ) ! C6H3Cl3
    CALL CalcH("1,2,3,4-tetrachlorobenzene",      "634-66-2",  5.014,  1945., 2.40E-02, "1146"         ) ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",              "608-93-5",  5.607,  2132., 2.16E-02, "1146"         ) ! C6HCl5
    CALL CalcH("hexachlorobenzene",               "118-74-1",  6.266,  2377., 1.44E-02, "1146"         ) ! C6Cl6
    CALL CalcH("methylbenzene",                   "108-88-3",  5.271,  1745., 2.09E-01, "L"            ) ! C6H5CH3 toluene
    CALL CalcH("1-chloro-2-methylbenzene",         "95-49-8",  3.890,  1409., 1.21E-01, "1150"         ) ! C7H7Cl $o$-chlorotoluene
    CALL CalcH("1,3-dimethylbenzene",             "108-38-3",  5.204,  1713., 2.30E-01, "L"            ) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,2-dimethylbenzene",              "95-47-6",  5.064,  1719., 1.60E-01, "L"            ) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,4-dimethylbenzene",             "106-42-3",  4.900,  1615., 2.48E-01, "L"            ) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("ethylbenzene",                    "100-41-4",  6.541,  2100., 2.39E-01, "L"            ) ! C6H5C2H5
    CALL CalcH("ethenylbenzene",                  "100-42-5",  5.628,  1935., 1.07E-01, "2270"         ) ! C8H8 styrene
    CALL CalcH("1,2,4-trimethylbenzene",           "95-63-6",  5.125,  1697., 2.17E-01, "903"          ) ! C6H3(CH3)3
    CALL CalcH("1,3,5-trimethylbenzene",          "108-67-8",  4.329,  1448., 2.45E-01, "1156"         ) ! C6H3(CH3)3 mesitylene
    CALL CalcH("propylbenzene",                   "103-65-1",  4.587,  1471., 3.70E-01, "1156"         ) ! C6H5C3H7
    CALL CalcH("(2-propyl)-benzene",               "98-82-8",  3.774,  1265., 3.02E-01, "L"            ) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("decahydronaphthalene",             "91-17-8",  6.331,  1664., 4.51E+00, "1156"         ) ! C{10}H{18} decalin
    CALL CalcH("1,2,3,4-tetrahydronaphthalene",   "119-64-2",  6.322,  2215., 5.86E-02, "1156"         ) ! C{10}H{12} tetralin
    CALL CalcH("naphthalene",                      "91-20-3",  6.058,  2332., 1.27E-02, "1597"         ) ! C{10}H8
    CALL CalcH("2-methylnaphthalene",              "91-57-6",  2.245,   399., 7.64E+00, "903"          ) ! C{10}H7CH3
    CALL CalcH("phenanthrene",                     "85-01-8",  2.417,  1530., 1.58E-03, "1597"         ) ! C{14}H{10}
    ! ----------------------------------------------------------------------------------------------------------
    CALL CalcH("anthracene",                      "120-12-7",  2.065,  1404., 1.88E-03, "1597"         ) ! C{14}H{10}
    CALL CalcH("fluoranthene",                    "206-44-0",  6.175,  2868., 2.47E-04, "1146"         ) ! C{16}H{10}
    CALL CalcH("benzo[b]fluoranthene",            "205-99-2",  2.955,  2245., 1.99E-05, "1146"         ) ! C{20}H{12}
    CALL CalcH("benzo[k]fluoranthene",            "207-08-9",  3.498,  2421., 1.73E-05, "1146"         ) ! C{20}H{12}
    CALL CalcH("benzo[a]pyrene",                   "50-32-8",  1.732,  1927., 1.44E-05, "1146"         ) ! C{20}H{12}
    CALL CalcH("benzo[ghi]perylene",              "191-24-2", -0.651,  1258., 1.14E-05, "1146"         ) ! C{22}H{12}
    CALL CalcH("indeno[1,2,3-cd]pyrene",          "193-39-5",  0.033,  1455., 1.17E-05, "1146"         ) ! C{22}H{12}
    CALL CalcH("2,5-dichlorobiphenyl",          "34883-39-1",  6.055,  2331., 1.27E-02, "1146"         ) ! C{12}H8Cl2 PCB-9
    CALL CalcH("2,4,4'-trichlorobiphenyl",       "7012-37-5",  6.324,  2467., 8.09E-03, "1146"         ) ! C{12}H7Cl3 PCB-28
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl", "35693-99-3",  6.472,  2530., 6.96E-03, "1146"         ) ! C{12}H6Cl4 PCB-52
    ! aroclor is a mixture, not a pure substance:
    !CALL CalcH("aroclor1242",                  "53469-21-9", 12.869,  4339., 1.16E-02, "853"          ) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1254",                  "11097-69-1", 11.880,  4099., 7.90E-03, "853"          ) ! C{12}HxCl{(10-x)}
    !CALL CalcH("aroclor1260",                  "11096-82-5", 11.848,  4104., 7.07E-03, "853"          ) ! C{12}HxCl{(10-x)}
    CALL CalcH("alpha-lindane",                   "319-84-6",  5.485,  2682., 2.17E-04, "1152"         ) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("gamma-lindane",                    "58-89-9",  3.715,  2254., 1.06E-04, "1152"         ) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("S-ethyl dipropylthiocarbamate",   "759-94-4",  4.535,  1939., 8.33E-03, "2241"         ) ! C9H{19}NOS
    CALL CalcH("molinate",                       "2212-67-1",  6.527,  3024., 1.63E-04, "1142"         ) ! C9H{17}NOS
    CALL CalcH("chlorpyrifos",                   "2921-88-2",  0.173,  1187., 1.33E-04, "893"          ) ! C9H{11}Cl3NO3PS
    CALL CalcH("$\alpha$-endosulfan",             "959-98-8",  0.446,   876., 2.87E-03, "rice97"       ) ! C9H6Cl6O3S
    CALL CalcH("mirex",                          "2385-85-5", 13.899,  4585., 1.81E-02, "852"          ) ! C{10}Cl{12} dodecachloropentacyclodecane
    CALL CalcH("trifluralin",                    "1582-09-8",  2.870,  1546., 3.95E-03, "893"          ) ! C{13}H{16}F3N3O4
    CALL CalcH("1-hydroxy-2-methoxybenzene",       "90-05-1",  6.198,  3144., 2.97E-05, "1142"         ) ! C7H8O2 guaicol; 2-methoxyphenol
    !CALL CalcH("1-hydroxy-2,4-dimethylbenzene",  "105-67-9", -5.912, -1563., 2.64E-01, "1156"         ) ! poor correlation
    CALL CalcH("4-methyl-2-methoxyphenol",         "93-51-6",  6.040,  3066., 3.82E-05, "1142"         ) ! C8H{10}O2
    CALL CalcH("1,3-dimethoxy-2-hydroxybenzene",   "91-10-1",  4.197,  2768., 5.68E-06, "1142"         ) ! C8H{10}O3 2,6-dimethoxyphenol
    CALL CalcH("methanol",                         "67-56-1",  3.444,  2142., 1.37E-04, "483"          ) ! CH3OH
    CALL CalcH("ethanol",                          "64-17-5",  5.576,  2757., 1.48E-04, "483"          ) ! C2H5OH
    ! ----------------------------------------------------------------------------------------------------------
    CALL CalcH("1-propanol",                       "71-23-8",  6.955,  3123., 2.01E-04, "483"          ) ! C3H7OH
    CALL CalcH("2-propanol",                       "67-63-0",  6.952,  3114., 2.14E-04, "483"          ) ! C3H7OH isopropanol
    !CALL CalcH("2-methoxyethanol",               "109-86-4", -1.443,  -507., 1.93E+00, "1156"         ) ! poor correlation
    CALL CalcH("1-butanol",                        "71-36-3",  6.600,  3009., 2.17E-04, "483"          ) ! C4H9OH
    CALL CalcH("2-butanol",                        "78-92-2",  6.734,  3031., 2.48E-04, "483"          ) ! C4H{10}O {sec}-butanol
    CALL CalcH("2-methyl-2-propanol",              "75-65-0",  8.467,  3488., 3.72E-04, "483"          ) ! C4H{10}O {tert}-butanol
    CALL CalcH("methyl hydroperoxide",           "3031-73-0",  3.398,  2170., 1.00E-04, "L"            ) ! CH3OOH methylperoxide
    CALL CalcH("hydroxymethyl hydroperoxide",   "15932-89-5",  6.456,  4193., 1.42E-08, "L"            ) ! HOCH2OOH HMHP; HMP
    CALL CalcH("bis-(hydroxymethyl)-peroxide",  "17088-73-2",  4.750,  3516., 5.71E-08, "800"          ) ! HOCH2OOCH2OH BHMP
    CALL CalcH("ethyl hydroperoxide",            "3031-74-1",  4.398,  2479., 8.71E-05, "516"          ) ! C2H5OOH ethylperoxide
    CALL CalcH("ethanoic peroxyacid",              "79-21-0",  3.484,  2309., 4.07E-05, "L"            ) ! CH3COOOH peroxyacetic acid
    CALL CalcH("methanal",                         "50-00-0",  4.621,  2840., 8.61E-06, "L"            ) ! HCHO formaldehyde
    CALL CalcH("ethanal",                          "75-07-0",  5.324,  2340., 2.21E-03, "L"            ) ! CH3CHO acetaldehyde
    CALL CalcH("2-hydroxyethanal",                "141-46-8",  0.203,  1850., 7.80E-07, "85"           ) ! HOCH2CHO hydroxyacetaldehyde
    CALL CalcH("trichloroethanal",                 "75-87-6", -2.323,  1368., 1.02E-07, "85"           ) ! CCl3CHO trichloroacetaldehyde; chloral
    CALL CalcH("propanal",                        "123-38-6",  5.324,  2337., 2.25E-03, "630"          ) ! C2H5CHO propionaldehyde
    CALL CalcH("propenal",                        "107-02-8",  4.823,  2110., 4.21E-03, "483"          ) ! CH2CHCHO acrolein
    CALL CalcH("propanonal",                       "78-98-8",  5.541,  3121., 7.84E-06, "85"           ) ! CH3COCHO methylglyoxal; pyruvaldehyde
    CALL CalcH("butanal",                         "123-72-8",  6.244,  2571., 2.98E-03, "630"          ) ! C3H7CHO butyraldehyde
    CALL CalcH("2-methylpropenal",                 "78-85-3",  5.281,  2177., 7.15E-03, "985"          ) ! C4H6O methacrolein
    CALL CalcH("pentanal",                        "110-62-3",  6.594,  2623., 4.44E-03, "630"          ) ! C4H{9}CHO valeraldehyde
    CALL CalcH("hexanal",                          "66-25-1",  6.934,  2689., 5.78E-03, "630"          ) ! C5H{11}CHO
    CALL CalcH("heptanal",                        "111-71-7",  8.574,  3122., 8.41E-03, "630"          ) ! C6H{13}CHO
    CALL CalcH("benzaldehyde",                    "100-52-7",  4.665,  2276., 7.97E-04, "L"            ) ! C6H5CHO
    CALL CalcH("octanal",                         "124-13-0",  8.574,  3084., 1.13E-02, "630"          ) ! C7H{15}CHO
    CALL CalcH("nonanal",                         "124-19-6",  7.984,  2799., 2.73E-02, "630"          ) ! C8H{17}CHO
    CALL CalcH("decanal",                         "112-31-2", 10.974,  3610., 4.57E-02, "630"          ) ! C9H{19}CHO
    CALL CalcH("propanone",                        "67-64-1",  3.742,  1965., 1.10E-03, "L"            ) ! CH3COCH3 acetone
    CALL CalcH("chloro-2-propanone",               "78-95-5",  4.275,  2223., 4.92E-04, "495"          ) ! CH2ClCOCH3 chloroacetone
    CALL CalcH("1,1,1-trifluoro-2-propanone",     "421-50-1",  8.803,  3670., 1.92E-04, "495"          ) ! CF3COCH3
    CALL CalcH("butanone",                         "78-93-3",  4.764,  2213., 1.62E-03, "L"            ) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("3-buten-2-one",                    "78-94-4",  8.221,  3257., 1.29E-03, "985"          ) ! C4H6O methyl vinyl ketone; MVK
    CALL CalcH("2,3-butanedione",                 "431-03-8",  4.520,  2323., 3.94E-04, "495"          ) ! CH3COCOCH3 biacetyl; dimethylglycol
    !CALL CalcH("4-methyl-2-pentanone",           "108-10-1", -1.924,   -57., 1.87E-02, "1156"         ) ! poor correlation
    CALL CalcH("1-phenylethanone",                 "98-86-2",  7.307,  3202., 2.42E-04, "L"            ) ! C6H5COCH3 acetophenone
    ! ----------------------------------------------------------------------------------------------------------
    CALL CalcH("methanoic acid",                   "64-18-6",  2.914,  2425., 4.38E-06, "L"            ) ! HCOOH formic acid
    CALL CalcH("ethanoic acid",                    "64-19-7",  3.650,  2596., 6.30E-06, "L"            ) ! CH3COOH acetic acid
    CALL CalcH("bromoethanoic acid",               "79-08-3",  6.490,  3895., 1.59E-07, "896"          ) ! CH2BrCOOH bromoacetic acid
    CALL CalcH("chloroethanoic acid",              "79-11-8",  7.343,  4104., 2.20E-07, "896"          ) ! CH2ClCOOH chloroacetic acid
    CALL CalcH("dibromoethanoic acid",            "631-64-1",  5.847,  3754., 1.10E-07, "896"          ) ! CHBr2COOH dibromoacetic acid
    CALL CalcH("dichloroethanoic acid",            "79-43-6",  4.776,  3352., 2.20E-07, "896"          ) ! CHCl2COOH dichloroacetic acid
    CALL CalcH("difluoroethanoic acid",           "381-73-7",  3.707,  2856., 9.24E-07, "896"          ) ! CHF2COOH difluoroacetic acid
    CALL CalcH("tribromoethanoic acid",            "75-96-7",  5.848,  3791., 8.27E-08, "896"          ) ! CBr3COOH tribromoacetic acid
    CALL CalcH("trichloroethanoic acid",           "76-03-9",  5.931,  3634., 3.42E-07, "896"          ) ! CCl3COOH trichloroacetic acid
    CALL CalcH("trifluoroethanoic acid",           "76-05-1",  7.828,  3926., 2.72E-06, "896"          ) ! CF3COOH trifluoroacetic acid
    CALL CalcH("chlorodifluoroethanoic acid",      "76-04-0",  8.721,  4323., 9.39E-07, "896"          ) ! CF2ClCOOH chlorodifluoroacetic acid
    CALL CalcH("2-oxopropanoic acid",             "127-17-3",  0.345,  2153., 1.00E-07, "L"            ) ! CH3COCOOH pyruvic acid
    CALL CalcH("pentanoic acid",                  "109-52-4",  4.861,  2865., 1.23E-05, "L"            ) ! C4H9COOH
    CALL CalcH("hexanoic acid",                   "142-62-1",  3.955,  2520., 2.28E-05, "L"            ) ! C5H{11}COOH caproic acid
    CALL CalcH("methyl ethanoate",                 "79-20-9",  4.590,  2048., 4.02E-03, "628"          ) ! CH3COOCH3 methyl acetate
    CALL CalcH("ethyl ethanoate",                 "141-78-6",  5.095,  2163., 5.22E-03, "628"          ) ! CH3COOC2H5 ethyl acetate
    CALL CalcH("propyl ethanoate",                "109-60-4",  5.519,  2257., 6.62E-03, "628"          ) ! CH3COOC3H7 propyl acetate
    CALL CalcH("butyl ethanoate",                 "123-86-4",  6.400,  2486., 8.29E-03, "628"          ) ! CH3COOC4H9 butyl acetate
    CALL CalcH("pentyl ethanoate",                "628-63-7",  7.167,  2685., 1.02E-02, "628"          ) ! CH3COOC5H{11} amyl acetate
    CALL CalcH("diethyl ether",                    "60-29-7",  5.953,  2158., 3.90E-02, "2901"         ) ! C2H5OC2H5
    CALL CalcH("methyl tert-butyl ether",        "1634-04-4",  9.070,  3178., 1.69E-02, "599"          ) ! CH3OC(CH3)3
    CALL CalcH("methyl nitrate",                  "598-58-3",  6.296,  2381., 1.50E-02, "541"          ) ! CH3ONO2
    CALL CalcH("ethyl nitrate",                   "625-58-1",  7.489,  2707., 1.80E-02, "541"          ) ! C2H5ONO2
    CALL CalcH("peroxyacetyl nitrate",           "2278-22-0",  7.236,  2704., 1.03E-02, "502"          ) ! CH3COOONO2 PAN
    CALL CalcH("1-propyl nitrate",                "627-13-4",  6.421,  2316., 3.32E-02, "L"            ) ! C3H7ONO2
    CALL CalcH("2-propyl nitrate",               "1712-64-7",  6.210,  2213., 4.58E-02, "L"            ) ! C3H7ONO2 isopropyl nitrate
    CALL CalcH("2-methyl-1-nitropropane",         "543-29-3",  7.648,  2649., 4.09E-02, "541"          ) ! C4H9ONO2 isobutyl nitrate
    CALL CalcH("1-butyl nitrate",                 "928-45-0",  6.979,  2480., 3.30E-02, "L"            ) ! C4H9ONO2
    CALL CalcH("2-butyl nitrate",                 "924-52-7",  7.275,  2526., 4.55E-02, "L"            ) ! C4H9ONO2
    CALL CalcH("1-pentyl nitrate",               "1002-16-0",  7.472,  2591., 4.32E-02, "1024"         ) ! C5H{11}ONO2 amyl nitrate
    CALL CalcH("2-pentyl nitrate",              "21981-48-6",  7.890,  2638., 7.85E-02, "L"            ) ! C5H{11}ONO2
    CALL CalcH("3-pentyl nitrate",              "82944-59-0",  6.251,  2154., 8.02E-02, "1024"         ) ! C5H{13}ONO2
    ! ----------------------------------------------------------------------------------------------------------
    CALL CalcH("3-methyl-1-butanol nitrate",      "543-87-3",  7.038,  2424., 5.89E-02, "1024"         ) ! C5H{11}ONO2 isoamyl nitrate
    CALL CalcH("1-hexyl nitrate",               "20633-11-8",  7.993,  2764., 3.67E-02, "1024"         ) ! C6H{13}ONO2
    CALL CalcH("2-nitrooxyethanol",             "16051-48-2",  6.195,  3629., 6.52E-07, "548"          ) ! HOC2H4ONO2
    CALL CalcH("1-nitrooxy-2-propanol",         "20266-65-3",  8.655,  4199., 2.15E-06, "548"          ) ! C3H7O4N
    CALL CalcH("2-nitrooxy-1-propanol",         "20266-74-4",  7.323,  3687., 5.58E-06, "548"          ) ! C3H7O4N
    CALL CalcH("2-nitrooxy-3-butanol",         "147794-10-3",  8.023,  4000., 2.38E-06, "548"          ) ! C4H9O4N
    CALL CalcH("1-nitrooxy-2-butanol",         "147794-11-4",  7.788,  3859., 4.20E-06, "548"          ) ! C4H9O4N
    CALL CalcH("2-nitrooxy-1-butanol",         "147794-12-5",  8.357,  4032., 4.03E-06, "548"          ) ! C4H9O4N
    CALL CalcH("nitromethane",                     "75-52-5",  2.416,  1595., 9.41E-04, "1996"         ) ! CH3NO2
    CALL CalcH("nitroethane",                      "79-24-3",  3.233,  1779., 1.46E-03, "1996"         ) ! C2H5NO2
    CALL CalcH("1-nitropropane",                  "108-03-2",  3.851,  1898., 2.39E-03, "1996"         ) ! C3H7NO2
    CALL CalcH("2-nitropropane",                   "79-46-9",  3.763,  1814., 3.75E-03, "1996"         ) ! CH3CH(NO2)CH3
    CALL CalcH("ethane nitrile",                   "75-05-8",  2.353,  1627., 6.35E-04, "L"            ) ! CH3CN acetonitrile
    CALL CalcH("pyridine",                        "110-86-1", -1.508,   128., 1.14E-02, "719"          ) ! C5H5N
    CALL CalcH("2-methylpyridine",                "109-06-8", -0.700,   354., 1.24E-02, "719"          ) ! C5H4NCH3 2-picoline; $\alpha$-picoline
    CALL CalcH("3-methylpyridine",                "108-99-6", -0.826,   348., 9.72E-03, "719"          ) ! C5H4NCH3 3-picoline; $\beta$-picoline
    CALL CalcH("4-methylpyridine",                "108-89-4", -0.675,   428., 7.33E-03, "719"          ) ! C5H4NCH3
    CALL CalcH("2-ethylpyridine",                 "100-71-0",  0.031,   506., 2.01E-02, "719"          ) ! C5H4NC2H5
    CALL CalcH("3-ethylpyridine",                 "536-78-7", -0.577,   385., 1.29E-02, "719"          ) ! C5H4NC2H5
    CALL CalcH("4-ethylpyridine",                 "536-75-4", -0.904,   316., 1.04E-02, "719"          ) ! C5H4NC2H5
    CALL CalcH("2,3-dimethylpyridine",            "583-61-9",  0.039,   617., 8.62E-03, "719"          ) ! C5H3N(CH3)2
    CALL CalcH("2,4-dimethylpyridine",            "108-47-4",  0.187,   669., 8.02E-03, "719"          ) ! C5H3N(CH3)2
    CALL CalcH("2,5-dimethylpyridine",            "589-93-5",  0.285,   667., 1.03E-02, "719"          ) ! C5H3N(CH3)2
    CALL CalcH("2,6-dimethylpyridine",            "108-48-5",  0.707,   767., 1.23E-02, "719"          ) ! C5H3N(CH3)2
    CALL CalcH("3,4-dimethylpyridine",            "583-58-4", -0.440,   560., 4.44E-03, "719"          ) ! C5H3N(CH3)2
    CALL CalcH("3,5-dimethylpyridine",            "591-22-0", -0.235,   539., 8.46E-03, "719"          ) ! C5H3N(CH3)2
    CALL CalcH("carbon disulfide",                 "75-15-0",  3.485,  1077., 6.47E-01, "586"          ) ! CS2
    CALL CalcH("carbon oxide sulfide",            "463-58-1",  2.917,   792., 1.64E+00, "586"          ) ! OCS carbonyl sulfide
    CALL CalcH("dimethyl sulfide",                 "75-18-3",  3.556,  1394., 6.35E-02, "L"            ) ! CH3SCH3 DMS
    CALL CalcH("dimethyl disulfide",              "624-92-0",  4.828,  1854., 3.18E-02, "522"          ) ! CH3SSCH3
    CALL CalcH("diethyl sulfide",                 "352-93-2",  5.300,  1927., 5.34E-02, "522"          ) ! C2H5SC2H5
    CALL CalcH("diethyl disulfide",               "110-81-6",  5.071,  1865., 5.12E-02, "522"          ) ! C2H5SSC2H5
    CALL CalcH("dipropyl sulfide",                "111-47-7",  5.678,  1955., 1.02E-01, "522"          ) ! C3H7SC3H7
    CALL CalcH("di-(2-propyl)-sulfide",           "625-80-9",  6.038,  2060., 1.03E-01, "522"          ) ! (C3H7)2S diisopropyl sulfide
    ! ----------------------------------------------------------------------------------------------------------
    CALL CalcH("thiophene",                       "110-02-1",  4.542,  1662., 7.46E-02, "522"          ) ! C4H4S
    CALL CalcH("2-methylthiophene",               "554-14-3",  5.320,  1887., 7.66E-02, "522"          ) ! CH3C4H3S
    CALL CalcH("methanethiol",                     "74-93-1",  3.249,  1219., 1.23E-01, "L"            ) ! CH3SH methyl mercaptan
    CALL CalcH("ethanethiol",                      "75-08-1",  4.147,  1486., 1.20E-01, "522"          ) ! C2H5SH ethyl mercaptan
    CALL CalcH("1-propanethiol",                  "107-03-9",  4.428,  1552., 1.36E-01, "522"          ) ! C3H7SH propyl mercaptan
    CALL CalcH("1-butanethiol",                   "109-79-5",  4.823,  1656., 1.49E-01, "522"          ) ! C4H9SH butyl mercaptan

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, Hcc, type_or_xref)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, Hcc
      CHARACTER(LEN=*), INTENT(IN) :: type_or_xref
      REAL :: Hcc_calc
      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it

      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")

      ! check that the values of B and DeltaH don't diverge by more than 1%:
      Hcc_calc = 10.**(A-B/293.15)
      IF (ABS(Hcc-Hcc_calc)/Hcc>0.01) THEN
        !PRINT *, TRIM(chem), Hcc_calc, Hcc, ABS(Hcc-Hcc_calc)/Hcc
        CALL PrintWarning("Internal inconsistency > 1%")
      ENDIF

      Hominus = KHcc_TIMES_HcpSI_atT0 / (10.**(A-B/T0))
      mindHR = B * LOG(10.) + T0 ! see ref958, eqn (34) why T0 is added

      IF (type_or_xref=="L") THEN
        type = "L"
        CALL Output(Hominus, mindHR)
      ELSE
        ! only use value here if it is in a paper that I don't have:
        IF (unread_bib(type_or_xref)) THEN
          CALL SettypeX(type_or_xref)
          CALL Output(Hominus, mindHR)
        ELSE
          IF (type_or_xref=="719") THEN
            type = "W"
            CALL MakeNote("1525ref719", &
              "Due to an apparently incorrect definition of the Henry's "// &
              "law constant by \citet{719}, "//TRIM(citet())//" quote "// &
              "incorrect values from that paper.")
            CALL Output(DUMMY)
          ELSE
            ! no output required, see original papers
          ENDIF
        ENDIF
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref1525

  !---------------------------------------------------------------------------

  SUBROUTINE ref1536 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1536"
    type = "E"
    chem = "perchloric acid" ; casrn = "7601-90-3" ! HClO4
    CALL MakeNoteOtherTemp("200")
    CALL Output(1E6*Hcp_TO_HcpSI)

  END SUBROUTINE ref1536

  !---------------------------------------------------------------------------

  SUBROUTINE ref1571 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1571"
    type = "M"

    chem = "propanone" ; casrn = "67-64-1" ! CH3COCH3 acetone
    CALL Output(26. * Hcp_TO_HcpSI, 6400.)

    chem = "butanone" ; casrn = "78-93-3" ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL Output(2.7 * Hcp_TO_HcpSI, 12000.)

    chem = "2,3-butanedione" ; casrn = "431-03-8" ! CH3COCOCH3 biacetyl; dimethylglycol
    CALL Output(57. * Hcp_TO_HcpSI, 6700.)

    chem = "2-methylpropanal" ; casrn = "78-84-2" ! C4H8O isobutyraldehyde
    CALL Output(0.60 * Hcp_TO_HcpSI, 4500.)

  END SUBROUTINE ref1571

  !---------------------------------------------------------------------------

  SUBROUTINE ref1573 ! KHcc [1]
    IMPLICIT NONE

    ref = "1573"
    type = "M"

    CALL CalcH("dichloromethane",               "75-09-2", 0.105) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",              "67-66-3", 0.166) ! CHCl3 chloroform
    CALL CalcH("trichloroethene",               "79-01-6", 0.406) ! C2HCl3 trichloroethylene
    CALL CalcH("methylbenzene",                "108-88-3", 0.263) ! C6H5CH3 toluene
    CALL CalcH("hexamethyldisiloxane",         "107-46-0", 530.)  ! C6H{18}OSi2, HMDS
    CALL CalcH("decamethylcyclopentasiloxane", "541-02-6", 5.46)  ! C{10}H{30}O5Si5, D5

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("296")
      CALL Output(KHcc_TO_HcpSI(H,23.+CtoK))
    END SUBROUTINE CalcH

  END SUBROUTINE ref1573

  !---------------------------------------------------------------------------

  SUBROUTINE ref1574 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "1574"
    type = "M"
    chem = "methanol" ; casrn = "67-56-1" ! CH3OH
    mindHR = 5878.
    Hominus = cH2O/(1E3*EXP(-mindHR/T0+22.766))
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref1574

  !---------------------------------------------------------------------------

  SUBROUTINE ref1597 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1597"
    type = "M"

    CALL CalcH("benzene",       "71-43-2", 550.,   32.2) ! C6H6
    CALL CalcH("naphthalene",   "91-20-3",  42.6,  44.6) ! C{10}H8
    CALL CalcH("phenanthrene",  "85-01-8",   4.68, 29.3) ! C{14}H{10}
    CALL CalcH("anthracene",   "120-12-7",   4.94, 26.9) ! C{14}H{10}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, dHvol)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H     ! from Table 1
      REAL,             INTENT(IN) :: dHvol ! from Table 3

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI / H
      mindhr = 1E3*dHvol/Rgas + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)

    END SUBROUTINE CalcH

  END SUBROUTINE ref1597


  !---------------------------------------------------------------------------

  SUBROUTINE ref1658 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1658"

    chem = "pentanedioic acid" ; casrn = "110-94-1" ! HOOC(CH2)3COOH glutaric acid
    type = "M"
    CALL Output(1.9E9*Hcp_TO_HcpSI)

  END SUBROUTINE ref1658

  !---------------------------------------------------------------------------

  SUBROUTINE ref1721 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1721"
    type = "M"

    chem = "ethanoic acid" ; casrn = "64-19-7" ! CH3COOH acetic acid
    CALL Output(1.44E3*Hcp_TO_HcpSI)

    chem = "propanoic acid" ; casrn = "79-09-4" ! C2H5COOH propionic acid
    CALL Output(1.49E3*Hcp_TO_HcpSI)

    chem = "butanoic acid" ; casrn = "107-92-6" ! C3H7COOH butyric acid
    CALL Output(9.86E2*Hcp_TO_HcpSI)

    chem = "2-methylpropanoic acid" ; casrn = "79-31-2" ! (CH3)2CHCOOH isobutyric acid
    CALL Output(9.69E2*Hcp_TO_HcpSI)

    chem = "pentanoic acid" ; casrn = "109-52-4" ! C4H9COOH valeric acid
    CALL Output(1.22E3*Hcp_TO_HcpSI)

    chem = "3-methylbutanoic acid" ; casrn = "503-74-2" ! (CH3)2CHCH2COOH isovaleric acid
    CALL Output(1.14E3*Hcp_TO_HcpSI)

    chem = "hexanoic acid" ; casrn = "142-62-1" ! C5H{11}COOH caproic acid
    CALL Output(7.62E2*Hcp_TO_HcpSI)

  END SUBROUTINE ref1721

  !---------------------------------------------------------------------------

  SUBROUTINE ref1773 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1773"
    type = "M"

    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    chem = "dimethyl sulfide" ; casrn = "75-18-3" ! CH3SCH3 DMS
    temp = (/ 274.4, 283.4, 291., 303.4, 313.4 /)
    Harray = (/ 2.16, 1.47, 0.72, 0.57, 0.33 /) * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1773

  !---------------------------------------------------------------------------

  SUBROUTINE ref1775 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "1775"
    type = "M"

    ndata = 27
    ALLOCATE(temp(ndata), Harray(ndata))
    chem = "1,1,2-trichlorotrifluoroethane" ; casrn = "76-13-1" ! C2F3Cl3 R113
    temp = (/ 0.04, 0.04, 0.04, 0.71, 0.71, 9.55, 9.55, 9.55, 9.55, &
      9.55, 19.00, 19.00, 19.00, 19.00, 19.00, 28.49, 28.49, 29.47, 29.47, &
      29.47, 29.47, 29.47, 33.95, 39.59, 39.59, 39.59, 39.59 /) + CtoK
    Harray = (/ 1.2678, 1.2636, 1.2613, 1.2004, 1.2062, 0.6701, 0.6764, &
      0.6700, 0.6699, 0.6692, 0.3940, 0.3953, 0.3935, 0.3941, 0.3945, 0.2585, &
      0.2560, 0.2468, 0.2476, 0.2452, 0.2471, 0.2463, 0.2076, 0.1701, 0.1713, &
      0.1715, 0.1697 /) * Hbp_TO_HcpSI / 100.
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1775

  !---------------------------------------------------------------------------

  SUBROUTINE ref1776 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "1776"
    type = "M"

    ndata = 25
    ALLOCATE(temp(ndata), Harray(ndata))
    chem = "sulfur hexafluoride" ; casrn = "2551-62-4" ! SF6
    temp = (/ 0.18, 0.25, 0.28, 0.38, 10.27, 10.32, 10.34, 10.36, 10.40, &
      20.01, 20.01, 20.06, 20.17, 30.82, 31.27, 31.38, 31.48, 32.16, 39.09, &
      39.11, 39.11, 39.38, 39.52, 39.63, 39.72 /) + CtoK
    Harray = (/ 0.61935, 0.61231, 0.60759, 0.61575, 0.39058, 0.39351, &
      0.39093, 0.38588, 0.39451, 0.27614, 0.26945, 0.27100, 0.26939, 0.19841, &
      0.19504, 0.19581, 0.19350, 0.19238, 0.16287, 0.15630, 0.16229, 0.15812, &
      0.15960, 0.15881, 0.15687 /) * Hcp_TO_HcpSI / 1000.
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1776

  !---------------------------------------------------------------------------

  SUBROUTINE ref1779 ! Hcp [M/atm]
    IMPLICIT NONE
    REAL, PARAMETER :: A1 = -58.0931
    REAL, PARAMETER :: A2 = 90.5069
    REAL, PARAMETER :: A3 = 22.2940

    ref = "1779"
    type = "L"
    chem = "carbon dioxide" ; casrn = "124-38-9" ! CO2
    ! T-dep with 3 parameters:
    ! ln(H) = A1 + A2*(100/T) + A3*ln(T/100)
    Hominus = EXP(A1 + A2*(100./T0) + A3 * LOG(T0/100.)) * Hcp_TO_HcpSI
    ! analytical derivative:
    ! d ln(H) / d (1/T) = 100*A2 - A3*T
    mindHR = 100.*A2 - A3*T0
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref1779

  !---------------------------------------------------------------------------

  SUBROUTINE ref1831 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1831"
    type = "L"

    chem = "methanol" ; casrn = "67-56-1" ! CH3OH
    CALL Output(2.16E2 * Hcp_TO_HcpSI, 5312.4)

    chem = "ethanol" ; casrn = "64-17-5" ! C2H5OH
    CALL Output(1.94E2 * Hcp_TO_HcpSI, 6274.0)

  END SUBROUTINE ref1831

  !---------------------------------------------------------------------------

  SUBROUTINE ref1855 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1855"
    type = "L"

    CALL CalcH("chloromethane",              "74-87-3", 0.109,  27.12E3)
    CALL CalcH("dichloromethane",            "75-09-2", 0.393,  30.48E3)
    CALL CalcH("trichloromethane",           "67-66-3", 0.261,  35.43E3)
    CALL CalcH("tetrachloromethane",         "56-23-5", 0.0361, 35.62E3)
    CALL CalcH("chloroethane",               "75-00-3", 0.0842, 23.31E3)
    CALL CalcH("1,1-dichloroethane",         "75-34-3", 0.177,  34.27E3)
    CALL CalcH("1,2-dichloroethane",        "107-06-2", 0.906,  36.08E3)
    CALL CalcH("1,1,1-trichloroethane",      "71-55-6", 0.0613, 30.74E3)
    CALL CalcH("1,1,2-trichloroethane",      "79-00-5", 1.141,  33.75E3)
    CALL CalcH("1,1,1,2-tetrachloroethane", "630-20-6", 0.430,  38.46E3)
    CALL CalcH("1,1,2,2-tetrachloroethane",  "79-34-5", 2.384,  39.78E3)
    ! hexachloroethane is only cited from Munz&Roberts
    CALL CalcH("chloroethene",               "75-01-4", 0.039,  26.14E3)
    CALL CalcH("1,1-dichloroethene",         "75-35-4", 0.0374, 28.59E3)
    CALL CalcH("cis-1,2-dichloroethene",    "156-59-2", 0.259,  30.77E3)
    CALL CalcH("trans-1,2-dichloroethene",  "156-60-5", 0.103,  29.51E3)
    CALL CalcH("trichloroethene",            "79-01-6", 0.114,  35.81E3)
    CALL CalcH("tetrachloroethene",         "127-18-4", 0.0630, 37.38E3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcp, DeltaHsol)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: Hcp, DeltaHsol
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = Hcp * Hcp_TO_HcpSI
      mindHR = DeltaHsol / Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1855

  !---------------------------------------------------------------------------

  SUBROUTINE ref1856 ! KHcc [1]
    IMPLICIT NONE

    ref = "1856"
    type = "M"

    CALL CalcH("benzene",        "71-43-2") ! C6H6
    CALL CalcH("methylbenzene", "108-88-3") ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",  "100-41-4") ! C6H5C2H5
    CALL CalcH("2-propanol",     "67-63-0") ! C3H7OH isopropanol
    CALL CalcH("butanone",       "78-93-3") ! C2H5COCH3 methyl ethyl ketone; MEK

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

  END SUBROUTINE ref1856

  !---------------------------------------------------------------------------

  SUBROUTINE ref1907 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "1907"

    CALL CalcH("hydroxybenzene",            "108-95-2",  3.36, 6032.)
    CALL CalcH("1-hydroxy-2-methylbenzene",  "95-48-7",  9.8,  5827.)
    CALL CalcH("1-hydroxy-3-methylbenzene", "108-39-4",  4.7,  5994.)
    CALL CalcH("1-hydroxy-4-methylbenzene", "106-44-5",  4.4,  6138.)
    CALL CalcH("2,3-xylenol",               "526-75-0",  5.3,  6839.)
    CALL CalcH("2,4-xylenol",               "105-67-9", 11.2,  6084.)
    CALL CalcH("2,5-xylenol",                "95-87-4",  7.4,  6783.)
    CALL CalcH("2,6-xylenol",               "576-26-1", 24.4,  6179.)
    CALL CalcH("3,4-xylenol",                "95-65-8",  2.33, 7082.)
    CALL CalcH("3,5-xylenol",               "108-68-9",  3.41, 6908.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpx_, mindHR_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL,             INTENT(IN) :: KHpx_, mindHR_
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      CALL Output(cH2O/(1E3*KHpx_), mindHR_)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1907

  !---------------------------------------------------------------------------

  SUBROUTINE ref1910 ! special definition (DeltaG and DeltaH for KHcc [1])

    IMPLICIT NONE

    ref = "1910"

    ! next block only for comparison with ref2378:
    !CALL CalcH("4-(1,1-dimethylethyl)-pyridine",      "3978-81-2", "C", -18.68, 57.86)
    !CALL CalcH("2,6-bis-(1,1-dimethylethyl)-pyridine", "585-48-8", "C",  -1.70, 57.03)
    !CALL CalcH("2-chloropyridine",                     "109-09-1", "C", -18.39, 49.45)
    !CALL CalcH("3-chloropyridine",                     "626-60-8", "C", -16.80, 45.23)
    !CALL CalcH("2-methylpyridine",     "109-06-8", "C", mindH=55.10)
    !CALL CalcH("3-methylpyridine",     "108-99-6", "C", mindH=54.60)
    !CALL CalcH("4-methylpyridine",     "108-89-4", "C", mindH=55.52)
    !CALL CalcH("2,4-dimethylpyridine", "108-47-4", "C", mindH=60.71)
    !CALL CalcH("2,5-dimethylpyridine", "589-93-5", "C", mindH=60.84)
    !CALL CalcH("2,6-dimethylpyridine", "108-48-5", "C", mindH=61.97)
    !CALL CalcH("3,5-dimethylpyridine", "591-22-0", "C", mindH=60.50)

    ! next 3 entries only for testing and debugging:
    !CALL CalcH("methane",       "74-82-8",  "V",   8.37, 13.79)
    !CALL CalcH("methylbenzene", "108-88-3", "V",  -3.71, 36.26)
    !CALL CalcH("ethanoic acid", "64-19-7",  "V", -28.05, 52.8)

    ! type "V":
    CALL CalcH("2-methylbutane",            "78-78-4", "V",   9.97        ) ! C5H{12} isopentane
    CALL CalcH("hexane",                   "110-54-3", "V",  10.40        ) ! C6H{14}
    CALL CalcH("2,2,5-trimethylhexane",   "3522-94-9", "V",  11.39        ) ! C9H{20}
    CALL CalcH("cycloheptane",             "291-64-5", "V",   3.33        ) ! C7H{14}
    CALL CalcH("cyclooctane",              "292-64-8", "V",   3.58        ) ! C8H{16}
    CALL CalcH("2-methyl-1-pentene",       "763-29-1", "V",   6.15        ) ! C6H{12}
    CALL CalcH("1,3,5-cycloheptatriene",   "544-25-2", "V",  -4.14        ) ! C7H8
    CALL CalcH("biphenyl",                  "92-52-4", "V", -11.06        ) ! (C6H5)2
    CALL CalcH("diphenylmethane",          "101-81-5", "V", -11.78        ) ! C{13}H{12}
    CALL CalcH("2,3-benzindene",            "86-73-7", "V", -14.41        ) ! C{13}H{10} fluorene
    CALL CalcH("naphthalene",               "91-20-3", "V", -10.01        ) ! C{10}H8
    CALL CalcH("1-methylnaphthalene",       "90-12-0", "V",  -9.91        ) ! C{10}H7CH3
    CALL CalcH("1-ethylnaphthalene",      "1127-76-0", "V", -10.02        ) ! C{10}H7C2H5
    CALL CalcH("1,3-dimethylnaphthalene",  "575-41-7", "V", -10.35        ) ! C{12}H{12}
    CALL CalcH("1,4-dimethylnaphthalene",  "571-58-4", "V", -11.79        ) ! C{12}H{12}
    CALL CalcH("2,3-dimethylnaphthalene",  "581-40-8", "V", -11.64        ) ! C{12}H{12}
    CALL CalcH("2,6-dimethylnaphthalene",  "581-42-0", "V", -11.00        ) ! C{12}H{12}
    CALL CalcH("acenaphthene",              "83-32-9", "V", -13.17        ) ! C{12}H{10}
    CALL CalcH("anthracene",               "120-12-7", "V", -17.70        ) ! C{14}H{10}
    CALL CalcH("phenanthrene",              "85-01-8", "V", -16.53        ) ! C{14}H{10}
    CALL CalcH("pyrene",                   "129-00-0", "V", -18.68        ) ! C{16}H{10}
    CALL CalcH("3-methyl-2-butanone",      "563-80-4", "V", -13.56        ) ! C5H{10}O isopropyl methyl ketone
    CALL CalcH("2-hexanone",               "591-78-6", "V", -13.76        ) ! C6H{12}O
    CALL CalcH("4-methyl-2-pentanone",     "108-10-1", "V", -12.81        ) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("4-heptanone",              "123-19-3", "V", -12.24        ) ! C7H{14}O
    CALL CalcH("2,4-dimethyl-3-pentanone", "565-80-0", "V", -11.46        ) ! C7H{14}O diisopropyl ketone
    CALL CalcH("5-nonanone",               "502-56-7", "V", -11.18        ) ! C9H{18}O dibutyl ketone
    CALL CalcH("2-adamantanone",           "700-58-3", "V", -18.66        )
    CALL CalcH("chlorobenzene",            "108-90-7", "V",  -4.69        ) ! C6H5Cl
    ! type "C":
    CALL CalcH("propanone",                 "67-64-1", "C", -16.12        ) ! CH3COCH3 acetone
    CALL CalcH("butanone",                  "78-93-3", "C", -15.22        ) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("3-pentanone",               "96-22-0", "C", -14.28        ) ! C2H5COC2H5

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, dG, mindH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL, OPTIONAL,   INTENT(IN) :: dG
      REAL, OPTIONAL,   INTENT(IN) :: mindH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it

      IF (PRESENT(dG)) THEN
        Hominus = KHcc_TIMES_HcpSI_atT0 / EXP(1E3*dG/(Rgas*T0))
      ELSE
        Hominus = DUMMY
      ENDIF
      IF (PRESENT(mindH)) THEN
        mindHR = 1E3*mindH / Rgas
        CALL Output(Hominus, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref1910

  !---------------------------------------------------------------------------

  SUBROUTINE ref1911 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "1911"
    type = "M"
    chem = "carbon monoxide" ; casrn = "630-08-0" ! CO
    ndata = 7
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0.88, 6.10, 10.04, 15.25, 19.86, 25.23, 30.05 /) + CtoK
    Harray = (/ 0.02756, 0.02445, 0.02270, 0.02073, 0.01921, 0.01779, 0.01683 /) &
      * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("1911seawater", &
      "Solubility in sea water at 20.99 \% chlorinity.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1911

  !---------------------------------------------------------------------------

  SUBROUTINE ref1912 ! KHcc [1]
    IMPLICIT NONE

    ref = "1912"

    ! Wood:
    CALL CalcH("tetrachloromethane",         "56-23-5", 1.052  )
    CALL CalcH("trichloromethane",           "67-66-3", 0.167  )
    CALL CalcH("dichloromethane",            "75-09-2", 0.0965 )
    CALL CalcH("1,2-dichloroethane",        "107-06-2", 0.0468 )
    ! Ehrenfeld:
    CALL CalcH("tetrachloromethane",         "56-23-5", 1.91   )
    CALL CalcH("trichloromethane",           "67-66-3", 0.118  )
    CALL CalcH("dichloromethane",            "75-09-2", 0.117  )
    CALL CalcH("1,2-dichloroethane",        "107-06-2", 0.0450 )
    CALL CalcH("butanone",                   "78-93-3", 0.00098) ! MEK
    CALL CalcH("1,5-dichloro-3-oxapentane", "111-44-4", 0.0011 ) ! bis-(2-chloroethyl)-ether

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc_
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "C"
      CALL Output(KHcc_TIMES_HcpSI_atT0/KHcc_)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1912

  !---------------------------------------------------------------------------

  SUBROUTINE ref1913 ! Hcc [1]
    IMPLICIT NONE

    ref = "1913"
    type = "M"
    chem = "ethanoic acid" ; casrn = "64-19-7"
    CALL MakeNote("cdep")
    CALL Output(DUMMY)

  END SUBROUTINE ref1913

  !---------------------------------------------------------------------------

  SUBROUTINE ref1914 ! Hcc [1]
    IMPLICIT NONE

    ref = "1914"
    type = "M"
    chem = "hydrogen fluoride" ; casrn = "7664-39-3" ! HF
    ! the value at the lowest concentration (0.05 M) is used here:
    CALL Output(Hcc_TO_HcpSI_atT0 * 3.23E5)

  END SUBROUTINE ref1914

  !---------------------------------------------------------------------------

  SUBROUTINE ref1915 ! Hcc [1]
    IMPLICIT NONE

    ref = "1915"
    type = "M"
    chem = "hydrogen cyanide" ; casrn = "74-90-8" ! HCN hydrocyanic acid
    ! the value at the lowest concentration (3.4 M) is used here:
    CALL Output(Hcc_TO_HcpSI_atT0 * 291.8)

  END SUBROUTINE ref1915

  !---------------------------------------------------------------------------

  SUBROUTINE ref1916 ! KHcc [1]
    IMPLICIT NONE

    ref = "1916"

    ! Tab. II, partition coefficient, calcd:
    CALL CalcH("dichloromethane",       "75-09-2", "V", 0.10) ! CH2Cl2
    CALL CalcH("trichloromethane",      "67-66-3", "V", 0.16) ! CHCl3 chloroform
    CALL CalcH("1,1,1-trichloroethane", "71-55-6", "V", 0.68) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("trichloroethene",       "79-01-6", "V", 0.48) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",    "127-18-4", "V", 0.41) ! C2Cl4 tetrachloroethylene
    ! Tab. II, partition coefficient, found:
    CALL CalcH("dichloromethane",       "75-09-2", "C", 0.11) ! CH2Cl2
    CALL CalcH("trichloromethane",      "67-66-3", "C", 0.13) ! CHCl3 chloroform
    CALL CalcH("tetrachloroethene",    "127-18-4", "C", 0.50) ! C2Cl4 tetrachloroethylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, KHcc_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: KHcc_
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0/KHcc_)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1916

  !---------------------------------------------------------------------------

  SUBROUTINE ref1917 ! only temperature dependence
    IMPLICIT NONE

    ref = "1917"

    CALL CalcH("methane",                                       "74-82-8",  626.,   848.,  12.0,  16.2)
    CALL CalcH("ethane",                                        "74-84-0",  936.,   998.,  17.9,  19.1)
    CALL CalcH("propane",                                       "74-98-6", 1067.,  1148.,  20.4,  22.0)
    CALL CalcH("n-butane",                                     "106-97-8", 1293.,  1298.,  24.8,  24.8)
    CALL CalcH("n-pentane",                                    "109-66-0", 1715.,  1448.,  32.8,  27.7)
    CALL CalcH("n-hexane",                                     "110-54-3", 1667.,  1598.,  31.9,  30.6)
    CALL CalcH("n-heptane",                                    "142-82-5", 2008.,  1748.,  38.4,  33.5)
    CALL CalcH("n-octane",                                     "111-65-9", 2197.,  1898.,  42.1,  36.3)
    CALL CalcH("n-nonane",                                     "111-84-2", 1642.,  2048.,  31.4,  39.2)
    CALL CalcH("isobutane",                                     "75-28-5", 1133.,  1298.,  21.7,  24.8)
    CALL CalcH("2-methylpentane",                              "107-83-5", 1594.,  1598.,  30.5,  30.6)
    CALL CalcH("3-methylpentane",                               "96-14-0", 1922.,  1598.,  36.8,  30.6)
    CALL CalcH("2,3-dimethylbutane",                            "79-29-8", 1690.,  1598.,  32.4,  30.6)
    CALL CalcH("2,5-dimethylhexane",                           "592-13-2", 1897.,  1898.,  36.3,  36.3)
    CALL CalcH("2,3,4-trimethylpentane",                       "565-75-3", 2013.,  1898.,  38.5,  36.3)
    CALL CalcH("2,2-dimethylpropane",                          "463-82-1", 1222.,  1448.,  23.4,  27.7)
    CALL CalcH("3,3-dimethylpentane",                          "562-49-2", 1152.,  1748.,  22.1,  33.5)
    CALL CalcH("2,2-dimethylhexane",                           "590-73-8", 2107.,  1898.,  40.3,  36.3)
    CALL CalcH("2,2,4-trimethylpentane",                       "540-84-1", 1618.,  1898.,  31.0,  36.3)
    CALL CalcH("cyclopropane",                                  "75-19-4",  805.,   972.,  15.4,  18.6)
    CALL CalcH("cyclopentane",                                 "287-92-3", 1733.,  1272.,  33.2,  24.4)
    CALL CalcH("cyclohexane",                                  "110-82-7", 1565.,  1422.,  30.0,  27.2)
    CALL CalcH("methylcyclohexane",                            "108-87-2", 1228.,  1572.,  23.5,  30.1)
    CALL CalcH("cyclooctane",                                  "292-64-8", 2035.,  1722.,  39.0,  33.0)
    CALL CalcH("ethylcyclohexane",                            "1678-91-7", 1921.,  1722.,  36.8,  33.0)
    CALL CalcH("cis-1,2-dimethylcyclohexane",                 "2207-01-4", 2000.,  1722.,  38.3,  33.0)
    CALL CalcH("trans-1,2-dimethylcyclohexane",               "6876-23-9", 1888.,  1722.,  36.1,  33.0)
    CALL CalcH("decalin",                                       "91-17-8", 1658.,  1846.,  31.7,  35.3)
    CALL CalcH("ethylene",                                      "74-85-1",  715.,  1057.,  13.7,  20.2)
    CALL CalcH("propene",                                      "115-07-1", 1530.,  1207.,  29.3,  23.1)
    CALL CalcH("1-hexene",                                     "592-41-6", 1587.,  1657.,  30.4,  31.7)
    CALL CalcH("isobutene",                                    "115-11-7", 1188.,  1357.,  22.7,  26.0)
    CALL CalcH("cyclopentene",                                 "142-29-0",  823.,  1331.,  15.8,  25.5)
    CALL CalcH("cyclohexene",                                  "110-83-8", 1428.,  1481.,  27.3,  28.4)
    CALL CalcH("cyclooctene",                                  "931-88-4", 1800.,  1781.,  34.5,  34.1)
    CALL CalcH("1,3-butadiene",                                "106-99-0", 1640.,  1416.,  31.4,  27.1)
    CALL CalcH("acetylene",                                     "74-86-2",  658.,   646.,  12.6,  12.4)
    CALL CalcH("propyne",                                       "74-99-7",  931.,   796.,  17.8,  15.2)
    CALL CalcH("1-butyne",                                     "107-00-6",  704.,   946.,  13.5,  18.1)
    CALL CalcH("1-buten-3-yne",                                "689-97-4",  795.,  1005.,  15.2,  19.2)
    CALL CalcH("benzene",                                       "71-43-2", 1470.,  1590.,  28.1,  30.4)
    CALL CalcH("toluene",                                      "108-88-3", 1690.,  1740.,  32.4,  33.3)
    CALL CalcH("ethylbenzene",                                 "100-41-4", 2057.,  1890.,  39.4,  36.2)
    CALL CalcH("o-xylene",                                      "95-47-6", 1637.,  1659.,  31.3,  31.8)
    CALL CalcH("m-xylene",                                     "108-38-3", 2014.,  1890.,  38.6,  36.2)
    CALL CalcH("p-xylene",                                     "106-42-3", 1816.,  1890.,  34.8,  36.2)
    CALL CalcH("n-propylbenzene",                              "103-65-1", 1900.,  2040.,  36.4,  39.1)
    CALL CalcH("1-methyl-2-ethylbenzene",                      "611-14-3", 1254.,  1809.,  24.0,  34.6)
    CALL CalcH("1,2,3-trimethylbenzene",                       "526-73-8", 1780.,  1578.,  34.1,  30.2)
    CALL CalcH("1,2,4-trimethylbenzene",                        "95-63-6", 1914.,  1809.,  36.6,  34.6)
    CALL CalcH("1,3,5-trimethylbenzene",                       "108-67-8", 1800.,  2040.,  34.5,  39.1)
    CALL CalcH("butylbenzene",                                 "104-51-8", 2011.,  2190.,  38.5,  41.9)
    CALL CalcH("o-diethylbenzene",                             "135-01-3", 2099.,  1959.,  40.2,  37.5)
    CALL CalcH("m-diethylbenzene",                             "141-93-5", 2174.,  2190.,  41.6,  41.9)
    CALL CalcH("p-diethylbenzene",                             "105-05-5", 2426.,  2190.,  46.4,  41.9)
    CALL CalcH("cumene",                                        "98-82-8", 1760.,  2040.,  33.7,  39.1)
    CALL CalcH("p-isopropyltoluen",                             "99-87-6", 1808.,  2190.,  34.6,  41.9)
    CALL CalcH("1,2,3,4-tetrahydronaphthalene",                "119-64-2", 2180.,  2014.,  41.7,  38.6)
    CALL CalcH("styrene",                                      "100-42-5", 1484.,  1949.,  28.4,  37.3)
    CALL CalcH("biphenyl",                                      "92-52-4", 2466.,  2099.,  47.2,  40.2)
    CALL CalcH("9H-fluorene",                                   "86-73-7", 2229.,  2073.,  42.7,  39.7)
    CALL CalcH("9,10-dihydrophenanthrene",                     "776-35-2", 3113.,  2223.,  59.6,  42.6)
    CALL CalcH("naphthalene",                                   "91-20-3", 2236.,  2126.,  42.8,  40.7)
    CALL CalcH("1-methylnaphthalene",                           "90-12-0", 2353.,  2276.,  45.0,  43.6)
    CALL CalcH("2-methylnaphthalene",                           "91-57-6", 2344.,  2276.,  44.9,  43.6)
    CALL CalcH("acenaphthene",                                  "83-32-9", 2722.,  2250.,  52.1,  43.1)
    CALL CalcH("acenaphthylene",                               "208-96-8", 2744.,  2309.,  52.5,  44.2)
    CALL CalcH("benzo(a)fluorene",                             "238-84-6", 1798.,  2609.,  34.4,  49.9)
    CALL CalcH("fluoranthene",                                 "206-44-0", 2023.,  2076.,  38.7,  39.7)
    CALL CalcH("anthracene",                                   "120-12-7", 2075.,  2662.,  39.7,  51.0)
    CALL CalcH("benzo[k]fluoranthene",                         "207-08-9", 2404.,  2612.,  46.0,  50.0)
    CALL CalcH("phenanthrene",                                  "85-01-8", 2175.,  1966.,  41.6,  37.6)
    CALL CalcH("1-methylphenanthrene",                         "832-69-9", 1854.,  2116.,  35.5,  40.5)
    CALL CalcH("benzo[b]fluoranthene",                         "205-99-2", 2226.,  1916.,  42.6,  36.7)
    CALL CalcH("benz(a)anthracene",                             "56-55-3", 3475.,  2502.,  66.5,  47.9)
    CALL CalcH("pyrene",                                       "129-00-0", 2248.,  2146.,  43.0,  41.1)
    CALL CalcH("indeno-[1,2,3-cd]-pyrene",                     "193-39-5", 1446.,  2096.,  27.7,  40.1)
    CALL CalcH("benzo(a)pyrene",                                "50-32-8", 1924.,  1986.,  36.8,  38.0)
    CALL CalcH("benzo[ghi]perylene",                           "191-24-2", 1284.,  1470.,  24.6,  28.1)
    CALL CalcH("fluoromethane",                                "593-53-3",  840.,   845.,  16.1,  16.2)
    CALL CalcH("difluoromethane",                               "75-10-5",  899.,   842.,  17.2,  16.1)
    CALL CalcH("1,1-difluoroethane",                            "75-37-6", 1079.,   992.,  20.7,  19.0)
    CALL CalcH("trifluoromethane",                              "75-46-7", 1179.,   839.,  22.6,  16.1)
    CALL CalcH("tetrafluoromethane",                            "75-73-0",  704.,   836.,  13.5,  16.0)
    ! name and casrn don't fit together:
    !CALL CalcH("1,1,1,2-tetrafluoroethane",                    "359-35-3", 1158.,   986.,  22.2,  18.9)
    CALL CalcH("pentafluoroethane",                            "354-33-6", 1123.,   983.,  21.5,  18.8)
    CALL CalcH("hexafluoroethane",                              "76-16-4", 1117.,   980.,  21.4,  18.8)
    CALL CalcH("heptafluoropropane",                           "431-89-0", 1294.,  1127.,  24.8,  21.6)
    CALL CalcH("chloromethane",                                 "74-87-3", 1053.,  1007.,  20.2,  19.3)
    CALL CalcH("chloroethane",                                  "75-00-3", 1150.,  1157.,  22.0,  22.1)
    CALL CalcH("1-chloropropane",                              "540-54-5", 1409.,  1307.,  27.0,  25.0)
    CALL CalcH("1-chlorobutane",                               "109-69-3", 1475.,  1457.,  28.2,  27.9)
    CALL CalcH("2-chlorobutane",                                "78-86-4", 1806.,  1457.,  34.6,  27.9)
    CALL CalcH("1-chloropentane",                              "543-59-9", 1781.,  1607.,  34.1,  30.8)
    CALL CalcH("1-chlorohexane",                               "544-10-5", 1802.,  1757.,  34.5,  33.6)
    CALL CalcH("dichloromethane",                               "75-09-2", 1583.,  1166.,  30.3,  22.3)
    CALL CalcH("1,2-dichloroethane",                           "107-06-2", 1455.,  1316.,  27.9,  25.2)
    CALL CalcH("1,1-dichloroethane",                            "75-34-3", 1581.,  1316.,  30.3,  25.2)
    CALL CalcH("1,3-dichloropropane",                          "142-28-9", 1550.,  1466.,  29.7,  28.1)
    CALL CalcH("1,2-dichloropropane",                           "78-87-5", 1623.,  1466.,  31.1,  28.1)
    CALL CalcH("2,2-dichloropropane",                          "594-20-7", 1581.,  1466.,  30.3,  28.1)
    CALL CalcH("1,4-dichlorobutane",                           "110-56-5", 1472.,  1616.,  28.2,  30.9)
    CALL CalcH("1,5-dichloropentane",                          "628-76-2", 1630.,  1766.,  31.2,  33.8)
    CALL CalcH("chloroform",                                    "67-66-3", 1750.,  1325.,  33.5,  25.4)
    CALL CalcH("1,1,2-trichloroethane",                         "79-00-5", 1696.,  1475.,  32.5,  28.2)
    CALL CalcH("1,1,1-trichloroethane",                         "71-55-6", 1499.,  1475.,  28.7,  28.2)
    CALL CalcH("1,2,3-trichloropropane",                        "96-18-4", 1648.,  1625.,  31.5,  31.1)
    CALL CalcH("carbon tetrachloride",                          "56-23-5", 1724.,  1484.,  33.0,  28.4)
    CALL CalcH("1,1,1,2-tetrachloroethane",                    "630-20-6", 1889.,  1634.,  36.2,  31.3)
    CALL CalcH("1,1,2,2-tetrachloroethane",                     "79-34-5", 1818.,  1634.,  34.8,  31.3)
    CALL CalcH("bromomethane",                                  "74-83-9", 1242.,  1330.,  23.8,  25.5)
    CALL CalcH("bromoethane",                                   "74-96-4", 1539.,  1480.,  29.5,  28.3)
    CALL CalcH("dibromomethane",                                "74-95-3", 1724.,  1812.,  33.0,  34.7)
    CALL CalcH("ethylene dibromide",                           "106-93-4", 1684.,  1962.,  32.2,  37.6)
    CALL CalcH("bromoform",                                     "75-25-2", 2058.,  2294.,  39.4,  43.9)
    CALL CalcH("methyl iodide",                                 "74-88-4", 1471.,  1534.,  28.2,  29.4)
    CALL CalcH("iodoethane",                                    "75-03-6", 1656.,  1684.,  31.7,  32.2)
    CALL CalcH("1-iodopropane",                                "107-08-4", 1844.,  1834.,  35.3,  35.1)
    CALL CalcH("2-iodopropane",                                 "75-30-9", 1914.,  1834.,  36.6,  35.1)
    CALL CalcH("chlorofluoromethane",                          "593-70-4",  949.,  1004.,  18.2,  19.2)
    CALL CalcH("chlorodifluoromethane",                         "75-45-6", 1193.,  1001.,  22.8,  19.2)
    CALL CalcH("1-chloro-1,2-difluoroethane",                  "338-64-7", 1263.,  1151.,  24.2,  22.0)
    CALL CalcH("chlorotrifluoromethane",                        "75-72-9",  731.,   998.,  14.0,  19.1)
    CALL CalcH("1-chloro-1,1,2-trifluoroethane",               "421-04-5", 1398.,  1148.,  26.8,  22.0)
    CALL CalcH("1-chloro-1,2,2,2-tetrafluoroethane",          "2837-89-0", 1353.,  1145.,  25.9,  21.9)
    CALL CalcH("chloropentafluoroethane",                       "76-15-3",  732.,  1142.,  14.0,  21.9)
    CALL CalcH("1,1-dichloro-1-fluoroethane",                 "1717-00-6", 1479.,  1313.,  28.3,  25.1)
    CALL CalcH("dichlorodifluoromethane",                       "75-71-8", 1356.,  1160.,  26.0,  22.2)
    CALL CalcH("1,2-dichlorotetrafluoroethane",                 "76-14-2", 1054.,  1304.,  20.2,  25.0)
    CALL CalcH("trichlorofluoromethane",                        "75-69-4", 1527.,  1322.,  29.2,  25.3)
    CALL CalcH("1,1,2-trichlorotrifluoroethane",                "76-13-1", 1502.,  1466.,  28.8,  28.1)
    CALL CalcH("halothane",                                    "151-67-7", 2028.,  1630.,  38.8,  31.2)
    CALL CalcH("bromodichloromethane",                          "75-27-4", 1508.,  1648.,  28.9,  31.5)
    CALL CalcH("dibromochloromethane",                         "124-48-1", 1872.,  1971.,  35.8,  37.7)
    CALL CalcH("perfluorocyclobutane",                         "115-25-3", 1504.,  1842.,  28.8,  35.3)
    CALL CalcH("chlorocyclohexane",                            "542-18-7", 1244.,  1674.,  23.8,  32.0)
    CALL CalcH("alpha-hexachlorocyclohexane",                  "319-84-6", 2938.,  2934.,  56.2,  56.2)
    CALL CalcH("beta-hexachlorocyclohexane",                   "319-85-7", 3274.,  2934.,  62.7,  56.2)
    CALL CalcH("gamma-hexachlorocyclohexane",                   "58-89-9", 2568.,  2934.,  49.2,  56.2)
    CALL CalcH("mirex",                                       "2385-85-5", 4598.,  4342.,  88.0,  83.1)
    CALL CalcH("tetrafluoroethylene",                          "116-14-3",  789.,   901.,  15.1,  17.2)
    CALL CalcH("perfluoropropene",                             "116-15-4",  907.,  1081.,  17.4,  20.7)
    CALL CalcH("cis-1-chloro-2-butene",                       "4628-21-1", 1090.,  1516.,  20.9,  29.0)
    CALL CalcH("trans-1-chloro-2-butene",                     "4894-61-5", 1176.,  1516.,  22.5,  29.0)
    CALL CalcH("cis-1,2-dichloroethene",                       "156-59-2", 1681.,  1303.,  32.2,  24.9)
    CALL CalcH("trans-1,2-dichloroethene",                     "156-60-5", 1746.,  1303.,  33.4,  24.9)
    CALL CalcH("1,1-dichloroethene",                            "75-35-4", 1488.,  1303.,  28.5,  24.9)
    CALL CalcH("trichloroethene",                               "79-01-6", 1680.,  1426.,  32.2,  27.3)
    CALL CalcH("tetrachloroethene",                            "127-18-4", 2071.,  1549.,  39.6,  29.7)
    CALL CalcH("1-chloro-2,2-difluoroethene",                  "359-10-4", 1298.,  1102.,  24.8,  21.1)
    CALL CalcH("hexachlorobutadiene",                           "87-68-3", 1404.,  2154.,  26.9,  41.2)
    CALL CalcH("3-bromoprop-1-yne",                            "106-96-7", 1711.,  1278.,  32.8,  24.5)
    CALL CalcH("fluorobenzene",                                "462-06-6", 1528.,  1459.,  29.3,  27.9)
    CALL CalcH("chlorobenzene",                                "108-90-7", 1598.,  1621.,  30.6,  31.0)
    CALL CalcH("o-chlorotoluene",                               "95-49-8", 2001.,  1771.,  38.3,  33.9)
    CALL CalcH("m-chlorotoluene",                              "108-41-8", 1935.,  1771.,  37.0,  33.9)
    CALL CalcH("p-chlorotoluene",                              "106-43-4", 1739.,  1771.,  33.3,  33.9)
    CALL CalcH("1,2-dichlorobenzene",                           "95-50-1", 1946.,  1797.,  37.3,  34.4)
    CALL CalcH("1,3-dichlorobenzene",                          "541-73-1", 1843.,  1652.,  35.3,  31.6)
    CALL CalcH("1,4-dichlorobenzene",                          "106-46-7", 1483.,  1652.,  28.4,  31.6)
    CALL CalcH("2,4-dichlorotoluene",                           "95-73-8", 2251.,  1802.,  43.1,  34.5)
    CALL CalcH("1,2,3-trichlorobenzene",                        "87-61-6", 1704.,  1973.,  32.6,  37.8)
    CALL CalcH("1,2,4-trichlorobenzene",                       "120-82-1", 1276.,  1828.,  24.4,  35.0)
    CALL CalcH("1,3,5-trichlorobenzene",                       "108-70-3", 1788.,  1683.,  34.2,  32.2)
    CALL CalcH("1,2,3,4-tetrachlorobenzene",                   "634-66-2", 1830.,  2149.,  35.0,  41.1)
    CALL CalcH("pentachlorobenzene",                           "608-93-5", 2084.,  2325.,  39.9,  44.5)
    CALL CalcH("hexachlorobenzene",                            "118-74-1", 2989.,  2646.,  57.2,  50.7)
    CALL CalcH("bromobenzene",                                 "108-86-1", 1752.,  1944.,  33.5,  37.2)
    CALL CalcH("1,4-dibromobenzene",                           "106-37-6", 2882.,  2298.,  55.2,  44.0)
    CALL CalcH("2-chlorobiphenyl",                            "2051-60-7", 2237.,  1872.,  42.8,  35.8)
    CALL CalcH("2,3-dichlorobiphenyl",                       "16605-91-7", 2382.,  2048.,  45.6,  39.2)
    CALL CalcH("2,4-dichlorobiphenyl",                       "33284-50-3", 2248.,  1903.,  43.0,  36.4)
    CALL CalcH("2,4'-dichlorobiphenyl",                      "34883-43-7", 2307.,  1903.,  44.2,  36.4)
    CALL CalcH("2,5-dichlorobiphenyl",                       "34883-39-1", 2380.,  1903.,  45.6,  36.4)
    CALL CalcH("2,2',3-trichlorobiphenyl",                   "38444-78-9", 1909.,  1821.,  36.5,  34.9)
    CALL CalcH("2,2',5-trichlorobiphenyl",                   "37680-65-2", 1826.,  1676.,  35.0,  32.1)
    CALL CalcH("2,2',6-trichlorobiphenyl",                   "38444-73-4", 1230.,  1418.,  23.5,  27.1)
    CALL CalcH("2,3,6-trichlorobiphenyl",                    "55702-45-9", 1079.,  1821.,  20.7,  34.9)
    CALL CalcH("2,3',4-trichlorobiphenyl",                   "55712-37-3", 2346.,  1934.,  44.9,  37.0)
    CALL CalcH("2,4,4'-trichlorobiphenyl",                    "7012-37-5", 1970.,  1934.,  37.7,  37.0)
    CALL CalcH("2,4,5-trichlorobiphenyl",                    "15862-07-4", 1826.,  2079.,  35.0,  39.8)
    CALL CalcH("2,2',3,5'-tetrachlorobiphenyl",              "41464-39-5", 1350.,  1852.,  25.8,  35.5)
    CALL CalcH("2,2',4,6-tetrachlorobiphenyl",               "62796-65-0", 1228.,  1449.,  23.5,  27.7)
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl",              "35693-99-3", 1996.,  1707.,  38.2,  32.7)
    CALL CalcH("2,3',4,4'-tetrachlorobiphenyl",              "32598-10-0", 1517.,  2110.,  29.0,  40.4)
    CALL CalcH("3,3',4,4'-tetrachlorobiphenyl",              "32598-13-3", 2318.,  2513.,  44.4,  48.1)
    CALL CalcH("2,2',3,4,5'-pentachlorobiphenyl",            "38380-02-8", 1701.,  2028.,  32.6,  38.8)
    CALL CalcH("2,2',4,5,5'-pentachlorobiphenyl",            "37680-73-2", 1556.,  1883.,  29.8,  36.0)
    CALL CalcH("2,2',4,6,6'-pentachlorobiphenyl",            "56558-16-8",  756.,  1222.,  14.5,  23.4)
    CALL CalcH("2,3,3',4',6-pentachlorobiphenyl",            "38380-03-9", 1733.,  2028.,  33.2,  38.8)
    CALL CalcH("2,3',4,4',5-pentachlorobiphenyl",            "31508-00-6", 2605.,  2286.,  49.9,  43.8)
    CALL CalcH("2,2',4,4',5,6-hexachlorobiphenyl",           "41411-63-6", 2391.,  1656.,  45.8,  31.7)
    CALL CalcH("2,3,3',4,4',5'-hexachlorobiphenyl",          "69782-90-7", 2073.,  2607.,  39.7,  49.9)
    CALL CalcH("2,2',3,3',4,4',5,5'-octachlorobiphenyl",     "35694-08-7", 2746.,  2701.,  52.6,  51.7)
    CALL CalcH("2,2',3,3',5,5',6,6'-octachlorobiphenyl",      "2136-99-4", 2050.,  1895.,  39.2,  36.3)
    CALL CalcH("decachlorobiphenyl",                          "2051-24-3", 3060.,  2537.,  58.6,  48.6)
    CALL CalcH("methanol",                                      "67-56-1", 2028.,  2560.,  38.8,  49.0)
    CALL CalcH("ethanol",                                       "64-17-5", 2643.,  2710.,  50.6,  51.9)
    CALL CalcH("1-propanol",                                    "71-23-8", 3128.,  2860.,  59.9,  54.8)
    CALL CalcH("1-butanol",                                     "71-36-3", 2871.,  3010.,  55.0,  57.6)
    CALL CalcH("1-pentanol",                                    "71-41-0", 3232.,  3160.,  61.9,  60.5)
    CALL CalcH("1-hexanol",                                    "111-27-3", 3523.,  3310.,  67.4,  63.4)
    CALL CalcH("1-heptanol",                                   "111-70-6", 3957.,  3460.,  75.8,  66.2)
    CALL CalcH("1-octanol",                                    "111-87-5", 3196.,  3610.,  61.2,  69.1)
    CALL CalcH("isobutyl alcohol",                              "78-83-1", 3383.,  3010.,  64.8,  57.6)
    CALL CalcH("isopentanol",                                  "123-51-3", 3439.,  3160.,  65.8,  60.5)
    CALL CalcH("2-methyl-1-butanol",                           "137-32-6", 2816.,  3160.,  53.9,  60.5)
    CALL CalcH("2,2-dimethyl-1-propanol",                       "75-84-3", 3293.,  3160.,  63.0,  60.5)
    CALL CalcH("isopropanol",                                   "67-63-0", 2466.,  2860.,  47.2,  54.8)
    CALL CalcH("2-butanol",                                     "78-92-2", 2954.,  3010.,  56.6,  57.6)
    CALL CalcH("2-pentanol",                                  "6032-29-7", 3304.,  3160.,  63.3,  60.5)
    CALL CalcH("3-pentanol",                                   "584-02-1", 3112.,  3160.,  59.6,  60.5)
    CALL CalcH("3-methyl-2-butanol",                           "598-75-4", 3131.,  3160.,  59.9,  60.5)
    CALL CalcH("4-methyl-2-pentanol",                          "108-11-2", 3649.,  3310.,  69.9,  63.4)
    CALL CalcH("cyclopentanol",                                 "96-41-3", 3056.,  2984.,  58.5,  57.1)
    CALL CalcH("cyclohexanol",                                 "108-93-0", 3107.,  3134.,  59.5,  60.0)
    CALL CalcH("t-butanol",                                     "75-65-0", 3489.,  3010.,  66.8,  57.6)
    CALL CalcH("2-methyl-2-butanol",                            "75-85-4", 3011.,  3160.,  57.6,  60.5)
    CALL CalcH("phenol",                                       "108-95-2", 2232.,  2542.,  42.7,  48.7)
    CALL CalcH("o-cresol",                                      "95-48-7", 3385.,  2692.,  64.8,  51.5)
    CALL CalcH("m-cresol",                                     "108-39-4", 2694.,  2692.,  51.6,  51.5)
    CALL CalcH("p-cresol",                                     "106-44-5", 2497.,  2692.,  47.8,  51.5)
    CALL CalcH("2-naphthol",                                   "135-19-3", 3000.,  3078.,  57.4,  58.9)
    CALL CalcH("1,2-benzenediol",                              "120-80-9", 3076.,  3494.,  58.9,  66.9)
    CALL CalcH("1,4-benzenediol",                              "123-31-9", 3217.,  3494.,  61.6,  66.9)
    CALL CalcH("diethyl ether",                                 "60-29-7", 2364.,  2152.,  45.3,  41.2)
    CALL CalcH("di(n-propyl) ether",                           "111-43-3", 3036.,  2452.,  58.1,  46.9)
    CALL CalcH("ethyl butyl ether",                            "628-81-9", 2048.,  2452.,  39.2,  46.9)
    CALL CalcH("di-n-butyl ether",                             "142-96-1", 2917.,  2752.,  55.8,  52.7)
    CALL CalcH("diisopropyl ether",                            "108-20-3", 3004.,  2749.,  57.5,  52.6)
    CALL CalcH("methyl t-butyl ether",                        "1634-04-4", 2484.,  2599.,  47.6,  49.8)
    CALL CalcH("methyl t-amyl ether",                          "994-05-8", 2883.,  2749.,  55.2,  52.6)
    CALL CalcH("methoxybenzene",                               "100-66-3", 1719.,  1834.,  32.9,  35.1)
    CALL CalcH("1,2-dimethoxybenzene",                          "91-16-7",  915.,  2078.,  17.5,  39.8)
    CALL CalcH("tetrahydrofuran",                              "109-99-9", 1258.,  1624.,  24.1,  31.1)
    CALL CalcH("2-methyltetrahydrofuran",                       "96-47-9", 2223.,  1774.,  42.6,  34.0)
    CALL CalcH("3-methyltetrahydropyran",                    "26093-63-0", 2180.,  1924.,  41.7,  36.8)
    CALL CalcH("2H-pyran, 3,4-dihydro-",                       "110-87-2", 1455.,  1375.,  27.9,  26.3)
    CALL CalcH("1,4-dioxane",                                  "123-91-1", 2527.,  2126.,  48.4,  40.7)
    CALL CalcH("methylhydroperoxide",                         "3031-73-0", 2148.,  2569.,  41.1,  49.2)
    CALL CalcH("ethylhydroperoxide",                          "3031-74-1", 2478.,  2719.,  47.4,  52.1)
    CALL CalcH("acetaldehyde",                                  "75-07-0", 2411.,  2121.,  46.2,  40.6)
    CALL CalcH("propionaldehyde",                              "123-38-6", 2060.,  2271.,  39.4,  43.5)
    CALL CalcH("butyraldehyde",                                "123-72-8", 2648.,  2421.,  50.7,  46.3)
    CALL CalcH("1-pentanal",                                   "110-62-3", 2242.,  2571.,  42.9,  49.2)
    CALL CalcH("hexanal",                                       "66-25-1", 2886.,  2721.,  55.2,  52.1)
    CALL CalcH("heptanal",                                     "111-71-7", 2958.,  2871.,  56.6,  55.0)
    CALL CalcH("octanal",                                      "124-13-0", 2550.,  3021.,  48.8,  57.8)
    CALL CalcH("decanal",                                      "112-31-2", 3542.,  3321.,  67.8,  63.6)
    CALL CalcH("undecanal",                                    "112-44-7", 3464.,  3471.,  66.3,  66.4)
    CALL CalcH("isobutyraldehyde",                              "78-84-2", 2089.,  2026.,  40.0,  38.8)
    CALL CalcH("2-methylvaleraldehyde",                        "123-15-9", 2151.,  2326.,  41.2,  44.5)
    CALL CalcH("acrolein",                                     "107-02-8", 1503.,  1872.,  28.8,  35.8)
    CALL CalcH("trans-crotonaldehyde",                         "123-73-9", 1732.,  2022.,  33.2,  38.7)
    CALL CalcH("alpha-methylacrolein",                          "78-85-3", 1944.,  1627.,  37.2,  31.1)
    CALL CalcH("glutaral",                                     "111-30-8", 3813.,  3694.,  73.0,  70.7)
    CALL CalcH("benzaldehyde",                                 "100-52-7", 2200.,  2368.,  42.1,  45.3)
    CALL CalcH("acetone",                                       "67-64-1", 2072.,  2271.,  39.7,  43.5)
    CALL CalcH("2-butanone",                                    "78-93-3", 2188.,  2421.,  41.9,  46.3)
    CALL CalcH("2-pentanone",                                  "107-87-9", 2712.,  2571.,  51.9,  49.2)
    CALL CalcH("diethyl ketone",                                "96-22-0", 2812.,  2571.,  53.8,  49.2)
    CALL CalcH("methyl butyl ketone",                          "591-78-6", 2556.,  2721.,  48.9,  52.1)
    CALL CalcH("3-hexanone",                                   "589-38-8", 2403.,  2721.,  46.0,  52.1)
    CALL CalcH("2-heptanone",                                  "110-43-0", 2867.,  2871.,  54.9,  55.0)
    CALL CalcH("3-heptanone",                                  "106-35-4", 2465.,  2871.,  47.2,  55.0)
    CALL CalcH("4-heptanone",                                  "123-19-3", 3261.,  2871.,  62.4,  55.0)
    CALL CalcH("2-octanone",                                   "111-13-7", 3048.,  3021.,  58.3,  57.8)
    CALL CalcH("2-nonanone",                                   "821-55-6", 3409.,  3171.,  65.3,  60.7)
    CALL CalcH("dibutyl ketone",                               "502-56-7", 3283.,  3171.,  62.8,  60.7)
    CALL CalcH("methyl isopropyl ketone",                      "563-80-4", 3011.,  2176.,  57.6,  41.7)
    CALL CalcH("methyl isobutyl ketone",                       "108-10-1", 2332.,  2721.,  44.6,  52.1)
    CALL CalcH("5-methyl-2-hexanone",                          "110-12-3", 3153.,  2871.,  60.4,  55.0)
    CALL CalcH("diisopropyl ketone",                           "565-80-0", 1990.,  2476.,  38.1,  47.4)
    CALL CalcH("2,6-dimethyl-4-heptanone",                     "108-83-8", 2273.,  3171.,  43.5,  60.7)
    CALL CalcH("3,3-dimethyl-2-butanone",                       "75-97-8", 2197.,  2326.,  42.1,  44.5)
    CALL CalcH("cyclopentanone",                               "120-92-3", 2314.,  2395.,  44.3,  45.8)
    CALL CalcH("cyclohexanone",                                "108-94-1", 2602.,  2545.,  49.8,  48.7)
    CALL CalcH("2-methylcyclohexanone",                        "583-60-8", 1873.,  2300.,  35.9,  44.0)
    CALL CalcH("4-methylcyclohexanone",                        "589-92-4", 2499.,  2695.,  47.8,  51.6)
    CALL CalcH("methyl vinyl ketone",                           "78-94-4", 3269.,  2480.,  62.6,  47.5)
    CALL CalcH("isophorone",                                    "78-59-1", 3088.,  3054.,  59.1,  58.5)
    CALL CalcH("2,3-butanedione",                              "431-03-8", 2492.,  2699.,  47.7,  51.7)
    CALL CalcH("acetylacetone",                                "123-54-6", 1788.,  3026.,  34.2,  57.9)
    CALL CalcH("acetophenone",                                  "98-86-2", 2785.,  2518.,  53.3,  48.2)
    CALL CalcH("propiophenone",                                 "93-55-0", 3236.,  2668.,  61.9,  51.1)
    CALL CalcH("methyl glyoxal",                                "78-98-8", 3150.,  2549.,  60.3,  48.8)
    CALL CalcH("formic acid",                                   "64-18-6", 2680.,  2376.,  51.3,  45.5)
    CALL CalcH("acetic acid",                                   "64-19-7", 2558.,  2526.,  49.0,  48.4)
    CALL CalcH("n-valeric acid",                               "109-52-4", 2856.,  2976.,  54.7,  57.0)
    CALL CalcH("n-hexanoic acid",                              "142-62-1", 3018.,  3126.,  57.8,  59.8)
    CALL CalcH("n-heptanoic acid",                             "111-14-8", 3295.,  3276.,  63.1,  62.7)
    CALL CalcH("caprylic acid",                                "124-07-2", 3527.,  3426.,  67.5,  65.6)
    CALL CalcH("benzoic acid",                                  "65-85-0", 2578.,  2773.,  49.4,  53.1)
    CALL CalcH("p-toluicacid",                                  "99-94-5", 3118.,  2923.,  59.7,  56.0)
    CALL CalcH("peroxyacetic acid",                             "79-21-0", 2179.,  2535.,  41.7,  48.5)
    CALL CalcH("methyl formate",                               "107-31-3", 1708.,  1668.,  32.7,  31.9)
    CALL CalcH("methyl acetate",                                "79-20-9", 1991.,  1818.,  38.1,  34.8)
    CALL CalcH("ethyl acetate",                                "141-78-6", 2133.,  1968.,  40.8,  37.7)
    CALL CalcH("n-butyl acetate",                              "123-86-4", 2189.,  2268.,  41.9,  43.4)
    CALL CalcH("isobutyl acetate",                             "110-19-0", 1878.,  2268.,  36.0,  43.4)
    CALL CalcH("2-methylpropyl propanoate",                    "540-42-1", 3047.,  2418.,  58.3,  46.3)
    CALL CalcH("cyclohexyl butyrate",                         "1551-44-6", 2310.,  2692.,  44.2,  51.5)
    CALL CalcH("dimethyl succinate",                           "106-65-0", 2915.,  2938.,  55.8,  56.2)
    CALL CalcH("malonic acid diethylester",                    "105-53-3", 2656.,  2420.,  50.8,  46.3)
    CALL CalcH("methyl benzoate",                               "93-58-3", 1393.,  2065.,  26.7,  39.5)
    CALL CalcH("diethyl phthalate",                             "84-66-2", 5261.,  5157., 100.7,  98.7)
    CALL CalcH("dibutyl phthalate",                             "84-74-2", 5647.,  5757., 108.1, 110.2)
    CALL CalcH("hydroxymethyl hydroperoxide",                "15932-89-5", 4233.,  3613.,  81.0,  69.2)
    CALL CalcH("bis(hydroxymethyl)peroxide",                 "17088-73-2", 3542.,  3949.,  67.8,  75.6)
    CALL CalcH("2-methoxyphenol",                               "90-05-1", 3270.,  2786.,  62.6,  53.3)
    CALL CalcH("4-methyl-2-methoxyphenol",                      "93-51-6", 3297.,  2936.,  63.1,  56.2)
    CALL CalcH("2,6-dimethoxyphenol",                           "91-10-1", 3180.,  3030.,  60.9,  58.0)
    CALL CalcH("glycolaldehyde",                               "141-46-8", 1877.,  3165.,  35.9,  60.6)
    CALL CalcH("furfural",                                      "98-01-1", 2413.,  2514.,  46.2,  48.1)
    CALL CalcH("pyruvic acid",                                 "127-17-3", 2166.,  2286.,  41.5,  43.8)
    CALL CalcH("2,2,2-trifluoroethanol",                        "75-89-8", 2291.,  2701.,  43.9,  51.7)
    CALL CalcH("2,2,3,3-tetrafluoropropanol",                   "76-37-9", 2726.,  2848.,  52.2,  54.5)
    CALL CalcH("pentafluoro-1-propanol",                       "422-05-9", 2478.,  2845.,  47.4,  54.5)
    CALL CalcH("1,1,1-trifluoro-2-propanol",                   "374-01-6", 2606.,  2851.,  49.9,  54.6)
    CALL CalcH("1,1,1,3,3,3-hexafluoropropan-2-ol",            "920-66-1", 2774.,  2842.,  53.1,  54.4)
    CALL CalcH("2-chlorophenol",                                "95-57-8", 2310.,  2573.,  44.2,  49.3)
    CALL CalcH("3-chlorophenol",                               "108-43-0", 2528.,  2573.,  48.4,  49.3)
    CALL CalcH("4-chlorophenol",                               "106-48-9", 2653.,  2573.,  50.8,  49.3)
    CALL CalcH("2,4-dichlorophenol",                           "120-83-2", 3088.,  2604.,  59.1,  49.9)
    CALL CalcH("2,4,6-trichlorophenol",                         "88-06-2", 2681.,  2635.,  51.3,  50.4)
    CALL CalcH("pentachlorophenol",                             "87-86-5", 3078.,  3277.,  58.9,  62.7)
    CALL CalcH("2,2'-dichloroethylether",                      "111-44-4", 2485.,  2470.,  47.6,  47.3)
    CALL CalcH("isofluorane",                                "26675-46-7", 1844.,  1775.,  35.3,  34.0)
    CALL CalcH("methoxyfluorane",                               "76-38-0", 1590.,  1943.,  30.4,  37.2)
    CALL CalcH("1-chlorodibenzo-p-dioxin",                   "39227-53-7", 2700.,  2939.,  51.7,  56.3)
    CALL CalcH("hexachlorodibenzo-p-dioxin",                 "39227-28-6", 3945.,  3674.,  75.5,  70.3)
    CALL CalcH("octachlorodibenzo-p-dioxin",                  "3268-87-9", 3984.,  4026.,  76.3,  77.1)
    CALL CalcH("trichloroacetaldehyde",                         "75-87-6", 1390.,   594.,  26.6,  11.4)
    CALL CalcH("chloroacetone",                                 "78-95-5", 2251.,  1762.,  43.1,  33.7)
    CALL CalcH("difluoroacetic acid",                          "381-73-7", 2858.,  3209.,  54.7,  61.4)
    CALL CalcH("trifluoroacetic acid",                          "76-05-1", 3943.,  3206.,  75.5,  61.4)
    CALL CalcH("chloroacetic acid",                             "79-11-8", 3970.,  3374.,  76.0,  64.6)
    CALL CalcH("dichloroacetic acid",                           "79-43-6", 3339.,  3533.,  63.9,  67.6)
    CALL CalcH("trichloroacetic acid",                          "76-03-9", 3626.,  3692.,  69.4,  70.7)
    CALL CalcH("bromoacetic acid",                              "79-08-3", 3888.,  3697.,  74.4,  70.8)
    CALL CalcH("dibromoacetic acid",                           "631-64-1", 3772.,  4179.,  72.2,  80.0)
    CALL CalcH("methyl trifluoroacetate",                      "431-47-0", 2373.,  2498.,  45.4,  47.8)
    CALL CalcH("trifluoroethyl acetate",                       "406-95-1", 2262.,  2648.,  43.3,  50.7)
    CALL CalcH("methylamine",                                   "74-89-5", 1269.,  2042.,  24.3,  39.1)
    CALL CalcH("octylamine",                                   "111-86-4", 2732.,  3092.,  52.3,  59.2)
    CALL CalcH("2-ethylhexylamine",                            "104-75-6", 3098.,  3092.,  59.3,  59.2)
    CALL CalcH("methylbutylamine",                             "110-68-9", 2031.,  2730.,  38.9,  52.3)
    CALL CalcH("dipropylamine",                                "142-84-7", 3405.,  2880.,  65.2,  55.1)
    CALL CalcH("dibutylamine",                                 "111-92-2", 3096.,  3180.,  59.3,  60.9)
    CALL CalcH("diisopropylamine",                             "108-18-9", 3586.,  2880.,  68.6,  55.1)
    CALL CalcH("diisobutylamine",                              "110-96-3", 3055.,  3180.,  58.5,  60.9)
    CALL CalcH("di-sec-butylamine",                            "626-23-3", 2891.,  3180.,  55.3,  60.9)
    CALL CalcH("n-ethyl-cyclohexylamine",                     "5459-93-8", 2702.,  3004.,  51.7,  57.5)
    CALL CalcH("diallylamine",                                 "124-02-7", 3363.,  2998.,  64.4,  57.4)
    CALL CalcH("triethylamine",                                "121-44-8", 3788.,  2769.,  72.5,  53.0)
    CALL CalcH("tributylamine",                                "102-82-9", 3140.,  3669.,  60.1,  70.2)
    CALL CalcH("N-methylpiperidine",                           "626-67-5", 2718.,  2593.,  52.0,  49.6)
    CALL CalcH("N-ethylpiperidine",                            "766-09-6", 2724.,  2743.,  52.1,  52.5)
    CALL CalcH("N,N-dimethylcyclohexylamine",                   "98-94-2", 3577.,  2893.,  68.5,  55.4)
    CALL CalcH("aniline",                                       "62-53-3", 2953.,  2578.,  56.5,  49.4)
    CALL CalcH("o-ethylaniline",                               "578-54-1", 3117.,  2996.,  59.7,  57.4)
    CALL CalcH("p-ethylaniline",                               "589-16-2", 3393.,  2878.,  65.0,  55.1)
    CALL CalcH("2,4-dimethylaniline",                           "95-68-1", 3064.,  2996.,  58.7,  57.4)
    CALL CalcH("2,5-dimethylaniline",                           "95-78-3", 3212.,  2996.,  61.5,  57.4)
    CALL CalcH("2,6-dimethylaniline",                           "87-62-7", 3159.,  3114.,  60.5,  59.6)
    CALL CalcH("2-isopropylaniline",                           "643-28-7", 2644.,  3146.,  50.6,  60.2)
    CALL CalcH("N-ethylaniline",                               "103-69-5", 3152.,  2966.,  60.3,  56.8)
    CALL CalcH("N,N-dimethylaniline",                          "121-69-7", 2591.,  2855.,  49.6,  54.7)
    CALL CalcH("N,N-dimethylbenzylamine",                      "103-83-3", 3207.,  3211.,  61.4,  61.5)
    CALL CalcH("N,N-diethylaniline",                            "91-66-7", 2385.,  3155.,  45.7,  60.4)
    CALL CalcH("acetonitrile",                                  "75-05-8", 1735.,  1692.,  33.2,  32.4)
    CALL CalcH("butyronitrile",                                "109-74-0", 1890.,  1992.,  36.2,  38.1)
    CALL CalcH("isobutyronitrile",                              "78-82-0", 2083.,  1992.,  39.9,  38.1)
    CALL CalcH("acrylonitrile",                                "107-13-1", 1448.,  1443.,  27.7,  27.6)
    CALL CalcH("methacrylonitrile",                            "126-98-7", 1847.,  1593.,  35.4,  30.5)
    CALL CalcH("benzonitrile",                                 "100-47-0", 2652.,  2434.,  50.8,  46.6)
    CALL CalcH("phenylacetonitrile",                           "140-29-4", 2072.,  2584.,  39.7,  49.5)
    CALL CalcH("pyridine",                                     "110-86-1", 2197.,  2495.,  42.1,  47.8)
    CALL CalcH("2-methylpyridine",                             "109-06-8", 2627.,  2645.,  50.3,  50.6)
    CALL CalcH("3-methylpyridine",                             "108-99-6", 2627.,  2645.,  50.3,  50.6)
    CALL CalcH("4-methylpyridine",                             "108-89-4", 2708.,  2645.,  51.8,  50.6)
    CALL CalcH("2-ethylpyridine",                              "100-71-0", 3289.,  2795.,  63.0,  53.5)
    CALL CalcH("3-ethylpyridine",                              "536-78-7", 2566.,  2795.,  49.1,  53.5)
    CALL CalcH("4-ethylpyridine",                              "536-75-4", 2621.,  2795.,  50.2,  53.5)
    CALL CalcH("2,3-dimethylpyridine",                         "583-61-9", 2411.,  2564.,  46.2,  49.1)
    CALL CalcH("2,4-dimethylpyridine",                         "108-47-4", 2670.,  2795.,  51.1,  53.5)
    CALL CalcH("2,5-dimethylpyridine",                         "589-93-5", 2867.,  2795.,  54.9,  53.5)
    CALL CalcH("2,6-dimethylpyridine",                         "108-48-5", 2734.,  2795.,  52.3,  53.5)
    CALL CalcH("3,4-dimethylpyridine",                         "583-58-4", 2636.,  2564.,  50.5,  49.1)
    CALL CalcH("3,5-dimethylpyridine",                         "591-22-0", 2680.,  2795.,  51.3,  53.5)
    CALL CalcH("2,4,6-trimethylpyridine",                      "108-75-8", 3586.,  2945.,  68.6,  56.4)
    CALL CalcH("quinoline",                                     "91-22-5", 3039.,  3031.,  58.2,  58.0)
    CALL CalcH("N-isobutylmorpholine",                       "10315-98-7", 2480.,  3395.,  47.5,  65.0)
    CALL CalcH("chloroacetonitrile",                           "107-14-2", 2222.,  1851.,  42.5,  35.4)
    CALL CalcH("2,6-dichlorobenzonitrile",                    "1194-65-6", 2242.,  2496.,  42.9,  47.8)
    CALL CalcH("2-chloropyridine",                             "109-09-1", 2720.,  2526.,  52.1,  48.4)
    CALL CalcH("acrylamide",                                    "79-06-1", 3306.,  3524.,  63.3,  67.5)
    CALL CalcH("alachlor",                                   "15972-60-8", 3894.,  4820.,  74.5,  92.3)
    CALL CalcH("metolachlor",                                "51218-45-2", 4279.,  4970.,  81.9,  95.1)
    CALL CalcH("nitromethane",                                  "75-52-5", 1412.,  1484.,  27.0,  28.4)
    CALL CalcH("nitroethane",                                   "79-24-3", 1699.,  1634.,  32.5,  31.3)
    CALL CalcH("1-nitropropane",                               "108-03-2", 1795.,  1784.,  34.4,  34.2)
    CALL CalcH("2-nitropropane",                                "79-46-9", 1780.,  1784.,  34.1,  34.2)
    CALL CalcH("nitrobenzene",                                  "98-95-3", 2286.,  1866.,  43.8,  35.7)
    CALL CalcH("2-nitrotoluene",                                "88-72-2", 2424.,  2016.,  46.4,  38.6)
    CALL CalcH("3-nitrotoluene",                                "99-08-1", 2009.,  2016.,  38.5,  38.6)
    CALL CalcH("4-nitrotoluene",                                "99-99-0", 1542.,  2016.,  29.5,  38.6)
    CALL CalcH("2,4,6-trinitrotoluene",                        "118-96-7", 2668.,  2568.,  51.1,  49.2)
    CALL CalcH("methyl nitrate",                               "598-58-3", 1937.,  1986.,  37.1,  38.0)
    CALL CalcH("1-propyl nitrate",                             "627-13-4", 1855.,  2286.,  35.5,  43.8)
    CALL CalcH("1-butyl nitrate",                              "928-45-0", 2385.,  2436.,  45.7,  46.6)
    CALL CalcH("1-pentyl nitrate",                            "1002-16-0", 2591.,  2586.,  49.6,  49.5)
    CALL CalcH("1-hexyl nitrate",                            "20633-11-8", 2764.,  2736.,  52.9,  52.4)
    CALL CalcH("2-propyl nitrate",                            "1712-64-7", 1718.,  1856.,  32.9,  35.5)
    CALL CalcH("2-butyl nitrate",                              "924-52-7", 2224.,  2006.,  42.6,  38.4)
    CALL CalcH("2-pentyl nitrate",                           "21981-48-6", 2074.,  2156.,  39.7,  41.3)
    CALL CalcH("3-pentyl nitrate",                           "82944-59-0", 2154.,  2156.,  41.2,  41.3)
    CALL CalcH("1-butanol, 3-methyl-, nitrate",                "543-87-3", 2424.,  2586.,  46.4,  49.5)
    CALL CalcH("2-nitrophenol",                                 "88-75-5", 2603.,  1772.,  49.8,  33.9)
    CALL CalcH("3-methyl-2-nitrophenol",                      "4920-77-8", 1681.,  1922.,  32.2,  36.8)
    CALL CalcH("4-methyl-2-nitrophenol",                       "119-33-5", 2832.,  1922.,  54.2,  36.8)
    CALL CalcH("5-methyl-2-nitrophenol",                       "700-38-9", 2301.,  1922.,  44.0,  36.8)
    CALL CalcH("2-methyl-6-nitrophenol",                     "13073-29-5", 2124.,  1922.,  40.7,  36.8)
    CALL CalcH("4-(sec-butyl)-2-nitrophenol",                 "3555-18-8", 1743.,  2372.,  33.4,  45.4)
    CALL CalcH("2,4-dinitrophenol",                             "51-28-5", 1301.,  2048.,  24.9,  39.2)
    CALL CalcH("4,6-dinitro-o-cresol",                         "534-52-1", 1681.,  2198.,  32.2,  42.1)
    CALL CalcH("2,6-dinitro-p-cresol",                         "609-93-8", 1327.,  1152.,  25.4,  22.1)
    CALL CalcH("dinoseb",                                       "88-85-7", 2982.,  2648.,  57.1,  50.7)
    CALL CalcH("2-nitrooxyethanol",                          "16051-48-2", 3634.,  3848.,  69.6,  73.7)
    CALL CalcH("4-methoxy-2-nitrophenol",                     "1568-70-3", 2743.,  2016.,  52.5,  38.6)
    CALL CalcH("peroxyacetylnitrate",                         "2278-22-0", 2614.,  1961.,  50.0,  37.5)
    CALL CalcH("o-chloronitrobenzene",                          "88-73-3", 2460.,  1897.,  47.1,  36.3)
    CALL CalcH("p-chloronitrobenzene",                         "100-00-5", 1621.,  1897.,  31.0,  36.3)
    CALL CalcH("trifluralin",                                 "1582-09-8",  794.,  2056.,  15.2,  39.4)
    CALL CalcH("5-fluoro-2-nitrophenol",                       "446-36-6", 2566.,  1641.,  49.1,  31.4)
    CALL CalcH("methanethiol",                                  "74-93-1", 1346.,  1293.,  25.8,  24.8)
    CALL CalcH("ethanethiol",                                   "75-08-1", 1479.,  1443.,  28.3,  27.6)
    CALL CalcH("n-propanethiol",                               "107-03-9", 1540.,  1593.,  29.5,  30.5)
    CALL CalcH("n-butanethiol",                                "109-79-5", 1706.,  1743.,  32.7,  33.4)
    CALL CalcH("dimethylsulfide",                               "75-18-3", 1397.,  1218.,  26.7,  23.3)
    CALL CalcH("dipropyl_sulfide",                             "111-47-7", 1825.,  1818.,  34.9,  34.8)
    CALL CalcH("diisopropyl sulfide",                          "625-80-9", 1712.,  1818.,  32.8,  34.8)
    CALL CalcH("dimethyldisulfide",                            "624-92-0",  560.,   593.,  10.7,  11.4)
    CALL CalcH("thiophene",                                    "110-02-1",  679.,  1102.,  13.0,  21.1)
    CALL CalcH("carbonyl sulfide",                             "463-58-1", 1307.,  1120.,  25.0,  21.4)
    CALL CalcH("S-ethyl dipropylthiocarbamate",                "759-94-4", 1935.,  1948.,  37.0,  37.3)
    CALL CalcH("dimethyl sulfoxide",                            "67-68-5", 1632.,  1218.,  31.2,  23.3)
    CALL CalcH("alpha-endosulfan",                             "959-98-8",  891.,  1277.,  17.1,  24.4)
    CALL CalcH("carbon dioxide",                               "124-38-9",  933.,  1120.,  17.9,  21.4)
    CALL CalcH("nitrogen oxide",                             "10102-43-9",  552.,   534.,  10.6,  10.2)
    CALL CalcH("nitrous oxide",                              "10024-97-2", 1034.,  1418.,  19.8,  27.1)
    CALL CalcH("sulfur hexafluoride",                         "2551-62-4", 1083.,  1252.,  20.7,  24.0)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, B_exp, B_calc, DeltaH_exp, DeltaH_calc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: B_exp, B_calc, DeltaH_exp, DeltaH_calc
      REAL :: from_B, from_DeltaH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! check that the values of B and DeltaH don't diverge by more than 1%:
      ! see ref958, eqn (34) why T0 is added to mindHR
      ! exp:
      type = "?"
      from_B      = B_exp*LOG(10.)
      from_DeltaH = 1e3*DeltaH_exp/Rgas
      IF (ABS(from_DeltaH-from_B)/from_B>0.01) &
        PRINT *, "ref1917: ", from_B, from_DeltaH
      CALL Output(DUMMY, B_exp*LOG(10.)+T0)
      ! calc:
      type = "Q"
      from_B      = B_calc*LOG(10.)
      from_DeltaH = 1e3*DeltaH_calc/Rgas
      IF (ABS(from_DeltaH-from_B)/from_B>0.01) &
        PRINT *, "ref1917: ", from_B, from_DeltaH
      CALL Output(DUMMY, B_calc*LOG(10.)+T0)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1917

  !---------------------------------------------------------------------------

  SUBROUTINE ref1922 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1922"
    type = "M"

    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.18*Hcp_TO_HcpSI)

    chem = "methylbenzene" ; casrn = "108-88-3" ! C6H5CH3 toluene
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.14*Hcp_TO_HcpSI)

    chem = "propylbenzene" ; casrn = "103-65-1" ! C6H5C3H7
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.15*Hcp_TO_HcpSI)

    chem = "1,3,5-trimethylbenzene" ; casrn = "108-67-8" ! C6H3(CH3)3 mesitylene
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.20*Hcp_TO_HcpSI)

    chem = "1,3-dimethylbenzene" ; casrn = "108-38-3" ! C6H4(CH3)2 $m$-xylene
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.136*Hcp_TO_HcpSI)

    chem = "$\alpha$-pinene" ; casrn = "80-56-8" ! C{10}H{16}
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.059*Hcp_TO_HcpSI)

    chem = "$\beta$-pinene" ; casrn = "127-91-3" ! C{10}H{16}
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.050*Hcp_TO_HcpSI)

    chem = "1-methyl-4-(1-methylethyl)-1,3-cyclohexadiene" ; casrn = "99-86-5" ! C{10}H{16} $\alpha$-terpinene
    CALL MakeNoteOtherTemp("295")
    CALL Output(0.046*Hcp_TO_HcpSI)

    chem = "2-methyl-1,3-butadiene" ; casrn = "78-79-5" ! C5H8 isoprene
    CALL MakeNoteOtherTemp("291")
    CALL Output(0.029*Hcp_TO_HcpSI)

    chem = "{trans}-2-hexenal" ; casrn = "6728-26-3" ! C3H7CHCHCHO
    CALL Output(14.5*Hcp_TO_HcpSI)

    chem = "hexanal" ; casrn = "66-25-1" ! C5H{11}CHO
    CALL Output(3.2*Hcp_TO_HcpSI)

    chem = "(E)-3-hexenyl ethanoate" ; casrn = "3681-82-1" ! C8H{14}O2
    CALL Output(3.3*Hcp_TO_HcpSI)

    chem = "(Z)-3-hexenyl ethanoate" ; casrn = "3681-71-8" ! C8H{14}O2
    CALL Output(3.1*Hcp_TO_HcpSI)

    chem = "hexyl ethanoate" ; casrn = "142-92-7" ! CH3COOC6H{13} hexyl acetate
    CALL Output(1.5*Hcp_TO_HcpSI)

    chem = "2-methylpropanal" ; casrn = "78-84-2" ! C4H8O
    CALL Output(3.3*Hcp_TO_HcpSI)

    chem = "2-butanone" ; casrn = "78-93-3" ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL Output(11.2*Hcp_TO_HcpSI)

    chem = "1-ethyl-3-hydroxybenzene" ; casrn = "620-17-7" ! C8H{10}O 3-ethylphenol
    CALL Output(500.*Hcp_TO_HcpSI)

    chem = "2-isobutyl-3-methoxypyrazine" ; casrn = "24683-00-9" ! C4N2H3(C4H9)OCH3
    CALL Output(17.*Hcp_TO_HcpSI)

  END SUBROUTINE ref1922

  !---------------------------------------------------------------------------

  SUBROUTINE ref1923 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1923"

    CALL CalcH("3-methylbutanal",                     "590-86-3", "M", 2.6)
    CALL CalcH("2-methylbutanal",                      "96-17-3", "M", 2.3)
    CALL CalcH("2-methylpropanal",                     "78-84-2", "M", 3.4)
    CALL CalcH("dimethyl sulfide",                     "75-18-3", "M", 0.5) ! CH3SCH3 DMS
    CALL CalcH("dimethyl disulfide",                  "624-92-0", "M", 0.6) ! CH3SSCH3
    CALL CalcH("2-methylbutanoic acid, ethyl ester", "7452-79-1", "M", 0.9)

    ! citation of other refs:
    !CALL CalcH("3-methylbutanal",                     "590-86-3", "C", 2.02 ) ! ref2387
    !CALL CalcH("2-methylbutanal",                      "96-17-3", "C", 2.51 ) ! ref1922
    !CALL CalcH("2-methylpropanal",                     "78-84-2", "C", 5.11 ) ! ref2388
    !CALL CalcH("dimethyl sulfide",                     "75-18-3", "C", 0.615) ! ref0728
    !CALL CalcH("dimethyl disulfide",                  "624-92-0", "C", 0.840) ! ref0728
    !CALL CalcH("2-methylbutanoic acid, ethyl ester", "7452-79-1", "C", 2.68 ) ! ref2389

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: HLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it
      Hominus = Hcp_TO_HcpSI * HLC
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1923

  !---------------------------------------------------------------------------

  SUBROUTINE ref1925 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "1925"
    type = "V"

    ndata = 8
    ALLOCATE(temp(ndata), Harray(ndata))
    chem = "ethanedioic acid" ; casrn = "144-62-7" ! HOOCCOOH oxalic acid
    temp = (/ -5., 0., 5., 10., 15., 20., 25., 30./) + CtoK
    Harray = (/ 2.91E10, 1.46E10, 7.55E9, 4.03E9, 2.21E9, 1.25E9, &
      7.24E8, 4.29E8 /) * Hbp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1925

  !---------------------------------------------------------------------------

  SUBROUTINE ref1926 ! KHcc [1]
    IMPLICIT NONE

    ref = "1926"
    type = "M"

    CALL CalcH("hexachlorobenzene",                            "118-74-1",   0.054)   ! C6Cl6
    ! aroclor is a mixture, not a pure substance:
    !CALL CalcH("aroclor1242",                                  "53469-21-9", 0.032)   ! C{12}HxCl{(10-x)}
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethene", "72-55-9",    0.050)   ! C{14}H8Cl4 DDE
    CALL CalcH("trans-chlordane",                              "5103-74-2",  0.055)   ! C{10}H6Cl8 $\beta$-chlordane; $\gamma$-chlordane
    CALL CalcH("cis-chlordane",                                "5103-71-9",  0.036)   ! C{10}H6Cl8 $\alpha$-chlordane
    CALL CalcH("$\alpha$-1,2,3,4,5,6-hexachlorocyclohexane",   "319-84-6",   0.00096) ! C6H6Cl6 $\alpha$-lindane
    ! according to the footnote in the table of the paper, the
    ! next two numbers are not equilibrium values:
    !CALL CalcH("dieldrin",                                     "60-57-1",    0.0044)  ! C{12}H8OCl6
    !CALL CalcH("dibutyl phthalate",                            "84-74-2",    0.011)   ! C{16}H{22}O4
    CALL CalcH("2,4'-dichlorobiphenyl",                        "34883-43-7", 0.039)   ! C{12}H8Cl2 PCB-8
    CALL CalcH("2,2',5-trichlorobiphenyl",                     "37680-65-2", 0.041)   ! C{12}H7Cl3 PCB-18 (2,5,2')
    CALL CalcH("2,2',3-trichlorobiphenyl",                     "38444-78-9", 0.033)   ! C{12}H7Cl3 PCB-16 (2,3,2')
    CALL CalcH("2,4',5-trichlorobiphenyl",                     "16606-02-3", 0.038)   ! C{12}H7Cl3 PCB-31 (2,5,4')
    CALL CalcH("2,3,3'-trichlorobiphenyl",                     "38444-84-7", 0.033)   ! C{12}H7Cl3 PCB-20
    CALL CalcH("2,2',4,6-tetrachlorobiphenyl",                 "62796-65-0", 0.031)   ! C{12}H6Cl4 PCB-50 (2,4,2',6')
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl",                "35693-99-3", 0.038)   ! C{12}H6Cl4 PCB-52 (2,5,2',5')
    CALL CalcH("2,2',3,5'-tetrachlorobiphenyl",                "41464-39-5", 0.032)   ! C{12}H6Cl4 PCB-44 (2,3,2',5')
    CALL CalcH("3,4,4'-trichlorobiphenyl",                     "38444-90-5", 0.034)   ! C{12}H7Cl3 PCB-37
    CALL CalcH("2,3,4,4'-tetrachlorobiphenyl",                 "33025-41-1", 0.034)   ! C{12}H6Cl4 PCB-60

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HRT)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HRT
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNote(TRIM(ref), &
        "As explained by \citet{1927}, the measurements were "// &
        "performed at 296~\unit{K}.")
      CALL Output(KHcc_TO_HcpSI(HRT,23.+CtoK))
    END SUBROUTINE CalcH

  END SUBROUTINE ref1926

  !---------------------------------------------------------------------------

  SUBROUTINE ref1928 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1928"
    type = "M"

    ! Tab. 1:
    CALL CalcH("methyl {tert}-butyl ether", "1634-04-4", 1.73) ! CH3OC(CH3)3 MTBE
    CALL CalcH("diisopropyl ether",         "108-20-3",  0.439) ! C3H7OC3H7 DIPE
    CALL CalcH("ethyl {tert}-butyl ether",  "637-92-3",  0.421) ! C2H5OC(CH3)3 ETBE
    CALL CalcH("2-methoxy-2-methylbutane",  "994-05-8",  0.529) ! C6H{14}O {tert}-amyl methyl ether; TAME
    CALL CalcH("1-ethoxy-butane",           "628-81-9",  0.646) ! C6H{14}O ethyl butyl ether EBE

    ! Tab. 2:
    CALL CalcH("benzene",                   "71-43-2",   0.1825) ! C6H6
    CALL CalcH("methylbenzene",             "108-88-3",  0.1573) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",              "100-41-4",  0.1611) ! C6H5C2H5

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("296") ! as explained in ref1927
      CALL Output(Hcp_TO_HcpSI * H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1928

  !---------------------------------------------------------------------------

  SUBROUTINE ref1929 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "1929"
    type = "M"

    CALL CalcH("1-propanethiol",   "107-03-9", &
      (/ 293.2, 303.1, 333.1 /),        (/ 23.9, 46.8, 93.9 /))      ! C3H7SH
    CALL CalcH("1-butanethiol",    "109-79-5", &
      (/ 292.8, 312.8, 332.8 /),        (/ 29.3, 70.1, 125.7 /))     ! C4H9SH
    CALL CalcH("dimethyl sulfide",  "75-18-3", &
      (/ 292.6, 302.7, 312.5, 332.8 /), (/ 8.4, 13.0, 18.8, 37.1 /)) ! DMS

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, temp_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, H
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      ndata = SIZE(H)
      ALLOCATE(temp(ndata), Harray(ndata))
      Harray = KHpx_TIMES_HcpSI / (H*1.E6/atm)
      CALL HTdep(temp_, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1929

  !---------------------------------------------------------------------------

  SUBROUTINE ref1930 ! KHcc [1]
    IMPLICIT NONE

    ref = "1930"
    type = "M"

    CALL CalcH("hydroxybenzene",     "108-95-2",  2.20E-5)
    CALL CalcH("2,3-dimethylphenol", "526-75-0",  4.42E-5)
    CALL CalcH("2,4-dimethylphenol", "105-67-9",  6.23E-5)
    CALL CalcH("2,4-dichlorophenol", "120-83-2", 11.97E-5)
    CALL CalcH("2-chlorophenol",      "95-57-8", 26.70E-5)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("293")
      CALL Output(KHcc_TO_HcpSI(HLC,20.+CtoK))
    END SUBROUTINE CalcH

  END SUBROUTINE ref1930

  !---------------------------------------------------------------------------

  SUBROUTINE ref1931 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1931"

    ! RIVM:
    CALL CalcH("desmetryn",  "1014-69-3",  "C", 2.0E-8   ) ! C8H{15}N5S
    CALL CalcH("prometryn",  "7287-19-6",  "C", 3.4E-7   ) ! C{10}H{19}N5S
    CALL CalcH("terbutryn",  "886-50-0",   "C", 8.6E-7   ) ! C{10}H{19}N5S
    CALL CalcH("atrazine",   "1912-24-9",  "C", 1.2E-7   ) ! C8H{14}ClN5
    CALL CalcH("cyanazine",  "21725-46-2", "C", 1.2E-10  ) ! C9H{13}ClN6
    CALL CalcH("simazine",   "122-34-9",   "C", 1.6E-8   ) ! C7H{12}ClN5
    CALL CalcH("metamitron", "41394-05-2", "C", 4.6E-7   ) ! C{10}H{10}N4O
    ! PHYSPROP:
    CALL CalcH("ametryn",    "834-12-8",   "C", 2.43E-4  ) ! C9H{17}N5S
    CALL CalcH("prometryn",  "7287-19-6",  "C", 1.32E-3  ) ! C{10}H{19}N5S
    CALL CalcH("terbutryn",  "886-50-0",   "C", 1.15E-3  ) ! C{10}H{19}N5S
    CALL CalcH("atrazine",   "1912-24-9",  "C", 2.30E-4  ) ! C8H{14}ClN5
    CALL CalcH("cyanazine",  "21725-46-2", "C", 2.57E-7  ) ! C9H{13}ClN6
    CALL CalcH("simazine",   "122-34-9",   "C", 9.42E-5  ) ! C7H{12}ClN5
    ! SM2:
    CALL CalcH("ametryn",    "834-12-8",   "Q", 1.12E-8  ) ! C9H{17}N5S
    CALL CalcH("desmetryn",  "1014-69-3",  "Q", 7.24E-10 ) ! C8H{15}N5S
    CALL CalcH("prometryn",  "7287-19-6",  "Q", 1.98E-7  ) ! C{10}H{19}N5S
    CALL CalcH("terbutryn",  "886-50-0",   "Q", 1.98E-7  ) ! C{10}H{19}N5S
    CALL CalcH("atrazine",   "1912-24-9",  "Q", 3.63E-5  ) ! C8H{14}ClN5
    CALL CalcH("cyanazine",  "21725-46-2", "Q", 2.24E-7  ) ! C9H{13}ClN6
    CALL CalcH("simazine",   "122-34-9",   "Q", 1.83E-6  ) ! C7H{12}ClN5
    CALL CalcH("metamitron", "41394-05-2", "Q", 3.56E-8  ) ! C{10}H{10}N4O
    ! SM3:
    CALL CalcH("ametryn",    "834-12-8",   "Q", 9.24E-8  ) ! C9H{17}N5S
    CALL CalcH("desmetryn",  "1014-69-3",  "Q", 2.54E-8  ) ! C8H{15}N5S
    CALL CalcH("prometryn",  "7287-19-6",  "Q", 7.38E-7  ) ! C{10}H{19}N5S
    CALL CalcH("terbutryn",  "886-50-0",   "Q", 7.06E-7  ) ! C{10}H{19}N5S
    CALL CalcH("atrazine",   "1912-24-9",  "Q", 2.51E-6  ) ! C8H{14}ClN5
    CALL CalcH("cyanazine",  "21725-46-2", "Q", 9.67E-10 ) ! C9H{13}ClN6
    CALL CalcH("simazine",   "122-34-9",   "Q", 2.50E-7  ) ! C7H{12}ClN5
    CALL CalcH("metamitron", "41394-05-2", "Q", 6.24E-8  ) ! C{10}H{10}N4O

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
      CALL Output(KHpcSI_TIMES_HcpSI/H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1931

  !---------------------------------------------------------------------------

  SUBROUTINE ref1932 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1932"

    chem = "1,4-dibromobenzene" ; casrn = "106-37-6" ! C6H4Br2
    type = "M"
    CALL Output(KHpcSI_TIMES_HcpSI/106.)
    type = "C"
    CALL Output(KHpcSI_TIMES_HcpSI/90.5)

    chem = "1,2,4-tribromobenzene" ; casrn = "615-54-3"
    type = "M"
    CALL Output(KHpcSI_TIMES_HcpSI/31.9)
    type = "C"
    CALL Output(KHpcSI_TIMES_HcpSI/34.6)

    chem = "1,2,4,5-tetrabromobenzene" ; casrn = "636-28-2"
    type = "M"
    CALL Output(KHpcSI_TIMES_HcpSI/376.)

    chem = "hexabromobenzene" ; casrn = "87-82-1"
    type = "M"
    CALL Output(KHpcSI_TIMES_HcpSI/10.8)

  END SUBROUTINE ref1932

  !---------------------------------------------------------------------------

  ! ref1934 MeOH in pulping spent liquors

  !---------------------------------------------------------------------------

  SUBROUTINE ref1935 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1935"
    type = "Q"

    CALL CalcH("chlorobenzene",              "108-90-7", 352.) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",         "95-50-1", 140.) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",        "541-73-1", 220.) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",        "106-46-7", 241.) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,2,3-trichlorobenzene",      "87-61-6",  90.) ! C6H3Cl3
    CALL CalcH("1,2,4-trichlorobenzene",     "120-82-1", 150.) ! C6H3Cl3
    CALL CalcH("1,3,5-trichlorobenzene",     "108-70-3", 218.) ! C6H3Cl3
    CALL CalcH("1,2,3,5-tetrachlorobenzene", "634-90-2", 141.) ! C6H2Cl4
    CALL CalcH("1,2,4,5-tetrachlorobenzene",  "95-94-3", 148.) ! C6H2Cl4
    CALL CalcH("1,2,3,4-tetrachlorobenzene", "634-66-2",  91.) ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",         "608-93-5", 127.) ! C6HCl5
    CALL CalcH("hexachlorobenzene",          "118-74-1", 153.) ! C6Cl6

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

  END SUBROUTINE ref1935

  !---------------------------------------------------------------------------

  SUBROUTINE ref1936 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1936"
    type = "M"
    chem = "dimethyl sulfide" ; casrn = "75-18-3" ! CH3SCH3 DMS
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 288.15, 293.15, 298.15, 303.15, 308.15 /)
    Harray = KHpcSI_TIMES_HcpSI / (/ 117.7, 146.2, 182.1, 224.3, 273.2 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1936

  !---------------------------------------------------------------------------

  SUBROUTINE ref1937 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "1937"
    type = "M"

    CALL CalcH("ethanol",          "64-17-5", (/ 29.7                                                           /) )
    CALL CalcH("propanol",         "71-23-8", (/ 37.2, 37.1, 37.3, 37.1, 37.2                                   /) )
    CALL CalcH("acetone",          "67-64-1", (/ 233., 234., 236., 236., 237., 235., 237.                       /) )
    CALL CalcH("2-butanone",       "78-93-3", (/ 322., 330.                                                     /) )
    CALL CalcH("2-pentanone",     "107-87-9", (/ 696., 597.                                                     /) )
    CALL CalcH("2-hexanone",      "591-78-6", (/ 538., 454.                                                     /) )
    CALL CalcH("diacetyl",        "431-03-8", (/ 100.4, 101., 98.5, 98.3, 97.9, 98.1                            /) )
    CALL CalcH("methyl sulfide",   "75-18-3", (/ 11260., 10912., 11140., 11096., 10952., 11475., 11810., 11868. /) )
    CALL CalcH("cis-4-heptenal", "6728-31-0", (/ 625.4, 624.4, 640.3                                            /) )
    CALL CalcH("acetoin",         "513-86-0", (/ 97.6, 99.3, 98.1, 96.0, 96.8                                   /) )
    CALL CalcH("acetaldehyde",     "75-07-0", (/ 488., 492., 496., 495., 501.                                   /) )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = SUM(H) / SIZE(H) ! mean value
      Hominus = KHpx_TIMES_HcpSI / (Hominus*1.E3/atm)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1937

  !---------------------------------------------------------------------------

  SUBROUTINE ref1940 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1940"
    type = "M"

    chem = "dimethyl malonate" ; casrn = "108-59-8"
    ndata = 12
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 283.1,285.1,286.0,287.1,288.1,289.1,290.1,291.1, &
      292.6,293.1,295.1,298.1 /)
    Harray = (/ 2.6,2.1,1.7,1.9,1.7,1.2,0.8,0.7,0.9,0.9,0.5,0.4 /) * 1E4*Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "dimethyl succinate" ; casrn = "106-65-0"
    ndata = 11
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 283.15,284.15,286.05,287.15,288.15,289.15,290.15, &
      292.15,292.65,295.15,298.15 /)
    Harray = (/ 1.2,1.1,1.2,0.9,1.0,0.8,0.6,0.5,0.5,0.4,0.3 /) * 1E4*Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1940

  !---------------------------------------------------------------------------

  SUBROUTINE ref1942 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "1942"
    type = "M"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 278., 283., 288., 293., 298. /)

    CALL CalcH("4-(3',5'-dimethyl-3'-heptyl)-phenol(+)",      "_CAS-33", (/ 2151., 1474., 850., 593., 242.   /)) ! C{15}H{24}O
    CALL CalcH("4-(3',5'-dimethyl-3'-heptyl)-phenol(-)",      "_CAS-34", (/ 2399., 1652., 966., 667., 279.   /)) ! C{15}H{24}O
    CALL CalcH("4-(1,1,3,3-tetramethylbutyl)-phenol",        "140-66-9", (/ 2008., 1277., 566., 574., 195.   /)) ! C{14}H{22}O $p$-{tert}-octylphenol
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",  "58-89-9", (/ 2906., 1629., 1176., 1166., 533. /)) ! C6H6Cl6 $\gamma$-lindane

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: HLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = HLC * Hcp_TO_HcpSI
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1942

  !---------------------------------------------------------------------------

  SUBROUTINE ref1943 ! miscellaneous definitions
    IMPLICIT NONE

    ref = "1943"

    ! Most Henry's law constants in this book are citations of other
    ! publications. Only derived work is listed here (or type "X" if I
    ! don't have the original paper)
    !----------------------
    ! Table 9.16:
    ! Hcp [M/bar]
    type = "V"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))

    chem = "bromine (molecular)" ; casrn = "7726-95-6" ! Br2
    temp = (/ 273.15, 293.15, 323.15 /)
    Harray = (/ 3.02, 1., 0.29 /) / (bar*dm3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "iodine (molecular)" ; casrn = "7553-56-2" ! I2
    temp = (/ 293.15, 298.15, 323.15 /)
    Harray = (/ 3.78, 2.59, 1.03 /) / (bar*dm3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)
    !----------------------
    ! for O3, see ref2432
    !----------------------
    ! Chapter 11:
    ! HbpSI [molkg-1Pa-1]
    !----------------------
    ! Often, ln(x) is shown first and then converted to
    ! ln(H/molkg-1Pa-1). The conversion factor from atm to molkg-1Pa-1 is:
    ! conv = atm*MH20 = 101325 * 18.01528E-3 = 1825.398246
    !----------------------
    ! A correct conversion from ln(x) to ln(H) is given on p. 268 for N2O:
    ! -60.7467 - 21.2531 ln(100) - ln(conv) = -166.1303
    !----------------------
    ! example for conversion from T'=T/100 to T:
    ! exponent := 74.7627 / T' - 65.1756 + 20.1398 ln(T')            => -18.09
    ! exponent := 7476.27 / T  - 65.1756 + 20.1398 (ln(T) - ln(100)) => -18.09
    !----------------------
    CALL CalcH("Ar",              "7440-37-1", "clever80",   "A+B/T'+Cln(T')",    -65.1756,         74.7627,    20.1398)   ! p.258
    CALL CalcH("ClO2",           "10049-04-4", "young83",    "A+B/T'+Cln(T')",    0.4070,           0.4791,     -11.0593)  ! p.259
    CALL CalcH("CO",               "630-08-0", "cargill90",  "A+B/T+Cln(T)+DT",   &
      -435.165523,      15259.9953, 67.8429542, -0.0704595356)                                                              ! p.260
    CALL CalcH("CO2",              "124-38-9", "scharlin96", "A+B/T+C/T^2+D/T^3", &
      -2.9644,          -1.2817e4,  3.7688e6,   -2.997e8)                                                                   ! p.261
    CALL CalcH("H2",              "1333-74-0", "young81a",   "A+B/T+Cln(T)",      -133.4487,        5.52845e3,  16.8893)   ! p.263
    CALL CalcH("H2O2",            "7722-84-1", "L",          "A+B/T",             -20.449,          6091.5)                ! p.264
    CALL CalcH("He",              "7440-59-7", "clever79a",  "A+B/T+Cln(T)",      -113.4867,        4.25962e3,  14.0094)   ! p.265
    CALL CalcH("Kr",              "7439-90-9", "clever79b",  "A+B/T'+Cln(T')",    -74.5023,         91.0166,    24.2207)   ! p.266
    CALL CalcH("N2O",            "10024-97-2", "young81b",   "A+B/T+Cln(T)",      -166.1303,        8.88280e3,  21.2531)   ! p.268
    CALL CalcH("NO",             "10102-43-9", "young81b",   "A+B/T'+Cln(T')",    LOG(100.)-74.923, 82.342,     22.816)    ! p.269
    ! O2: The CASRN is incorrect, and the corresponding equation for H
    !     is incorrect. The first term should be -178.763753281 not -187.07794.
    !     conv := 101325 (2 1.00794e-3 + 0.0159994) => 1825.398246
    !     -64.21517 - 23.24323 ln(100) - ln(conv) => -178.763753281
    CALL CalcH("O2",              "7782-44-7", "battino81",  "A+B/T+Cln(T)",      -178.76375,       8.391236e3, 23.24323)  ! p.270
    CALL CalcH("Rn",             "10043-92-2", "clever79b",  "A+BT+CT^2",         12.3530,          -0.162594,  2.2433e-4) ! p.271
    CALL CalcH("CBr3F",            "353-54-8", "V",          "A",                 1.48e-6)                                 ! p.273
    ! CBr4: using average of (1.18e-5+1.22e-5)/2 = 1.20e-5
    CALL CalcH("CBr4",             "558-13-4", "V",          "A",                 1.20e-5)                                 ! p.274
    !----------------------
    ! the regression here is just a test to check that the values given by ref1943 are correct:
    !chem = "trichlorofluoromethane" ; casrn = "75-69-4" ! CFCl3 R11
    !ndata = 9
    !ALLOCATE(temp(ndata), Harray(ndata))
    !temp = (/ 273.2, 278.2, 283.2, 288.2, 293.2, 298.2, 303.2, 308.2, 313.2 /)
    !Harray = (/ 7.25e-7, 4.68e-7, 3.00e-7, 1.92e-7, 1.25e-7, 0.844e-7, 0.621e-7, &
    !  0.517e-7, 0.487e-7 /) * HbpSI_TO_HcpSI
    !CALL HTdep(temp, Harray, Hominus, mindHR)
    !CALL Output(Hominus, mindHR, r2)
    !DEALLOCATE(temp, Harray)
    !----------------------
    CALL CalcH("CCl3F",             "75-69-4", "V",          "A+B/T",             -36.699,          6135.4)                ! p.275
    CALL CalcH("CCl4",              "56-23-5", "V",          "A+B/T",             -29.033,          4229.3)                ! p.277
    CALL CalcH("CHBrCl2",           "75-27-4", "L",          "A+B/T",             -24.690,          3710.0)                ! p.278
    CALL CalcH("CHBr2Cl",          "124-48-1", "L",          "A+B/T",             -26.348,          4383.0)                ! p.279
    CALL CalcH("CHBr3",             "75-25-2", "V",          "A+B/T",             -28.667,          5286.7)                ! p.280
    CALL CalcH("CHCl3",             "67-66-3", "V",          "A+B/T",             -27.672,          4415.8)                ! p.281
    CALL CalcH("CHI3",              "75-47-8", "V",          "A",                 6.22e-6)                                 ! p.283
    CALL CalcH("CH2Br2",            "74-95-3", "V",          "A+B/T",             -25.363,          4208.5)                ! p.284
    CALL CalcH("CH2Cl2",            "75-09-2", "V",          "A+B/T",             -26.183,          4064.2)                ! p.285
    CALL CalcH("CH3I",              "74-88-4", "V",          "A+B/T",             -25.273,          3605.9)                ! p.290
    CALL CalcH("CH4",               "74-82-8", "clever87",   "A+B/T'+Cln(T')+DT'", &
      -106.651,         132.821,    51.91445,   -4.25831)                                                                   ! p.291
    CALL CalcH("CH3OH",             "67-56-1", "L",          "A+B/T",             -21.446,          4500.3)                ! p.292
    CALL CalcH("C2Cl4",            "127-18-4", "L",          "A+B/T",             -28.395,          4196.5)                ! p.294
    CALL CalcH("C2HBrClF3",        "151-67-7", "L",          "A+B/T",             -30.0893,         4679.4)                ! p.296
    ! see SUBROUTINE ref2442 for next value:
    !CALL CalcH("C2HBrF4",         "124-72-1", "ref2442",    "A",                 1.24e-7)                                 ! p.297
    CALL CalcH("C2HCl3",            "79-01-6", "L",          "A+B/T",             -28.051,          4259.1)                ! p.297
    CALL CalcH("C2H2Cl2",           "75-35-4", "L",          "A+B/T",             -27.392,          3775.2)                ! p.300
    CALL CalcH("cis-C2H2Cl2",      "156-59-2", "L",          "A+B/T",             -26.171,          3961.6)                ! p.302
    CALL CalcH("trans-C2H2Cl2",    "156-60-5", "L",          "A+B/T",             -27.814,          4191.9)                ! p.303
    CALL CalcH("C2H2Cl4",          "630-20-6", "V",          "A+B/T",             -29.198,          5017.3)                ! p.305
    CALL CalcH("111-C2H3Cl3",       "71-55-6", "L",          "A+B/T",             -27.223,          3856.9)                ! p.306
    CALL CalcH("112-C2H3Cl3",       "79-00-5", "L",          "A+B/T",             -25.472,          4212.1)                ! p.308
    CALL CalcH("C2H3N",             "75-05-8", "L",          "A+B/T",             -21.218,          4061.2)                ! p.309
    CALL CalcH("C2H4",              "74-85-1", "hayduk94",   "A+B/T+Cln(T)",      -186.696,         9.22101e3,  24.3792)   ! p.311
    CALL CalcH("11-C2H4Cl2",        "75-34-3", "L",          "A+B/T",             -26.791,          4050.1)                ! p.312
    CALL CalcH("12-C2H4Cl2",       "107-06-2", "L",          "A+B/T",             -26.0965,         4319.8)                ! p.313
    !----------------------
    ! C2H5I: incorrect regression, data from Table is used
    ! p.315
    chem = "iodoethane" ; casrn = "75-03-6" ! C2H5I
    ndata = 7
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 273.2, 278.2, 283.2, 288.2, 293.2, 298.2, 303.2 /)
    Harray = (/ 5.43e-6, 4.00e-6, 3.02e-6, 2.32e-6, 1.82e-6, 1.46e-6, 1.20e-6 /) * HbpSI_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn), "The regression given by \citet{"//TRIM(ref)// &
      "} does not produce the data in their table. Thus the regression was recalculated.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)
    !----------------------
    CALL CalcH("C2H6",              "74-84-0", "hayduk82",   "A+B/T+Cln(T)",      -258.322,         1.269559e4, 34.74128)  ! p.316
    CALL CalcH("C2H5OH",            "64-17-5", "L",          "A+B/T",             -25.659,          5749.9)                ! p.317
    !----------------------
    ! DMSO data are not used
    chem = "DMSO" ; casrn = "67-68-5"
    type = "?"
    CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn), "It is unclear how \citet{"//TRIM(ref)//"} obtained the data. "// &
      "Apparently, limiting activity coefficients "// &
      "$\gamma^\infty$ were taken from \citet{2443} but a source for vapor pressure data is not mentioned. "// &
      "Also, the $\gamma^\infty$ values listed in the table are different from those found in the original paper.")         ! p.318
    CALL Output(DUMMY)
    !----------------------
    CALL CalcH("DMS",               "75-18-3", "L",          "A+B/T",             -24.207,          3588.7)                ! p.319
    CALL CalcH("isoflurane",     "26675-46-7", "L",          "A",                 2.40e-7)                                 ! p.320
    CALL CalcH("enflurane",      "13838-16-9", "L",          "A",                 3.0e-7)                                  ! p.320
    CALL CalcH("C3H3N",            "107-13-1", "V",          "A+B/T",             -20.771,          3388.7)                ! p.321
    CALL CalcH("C3H4Cl2F2O",        "76-38-0", "L",          "A+B/T",             -26.567,          4122.0)                ! p.322
    CALL CalcH("acetone",           "67-64-1", "L",          "A+B/T",             -27.221,          5657.5)                ! p.324
    CALL CalcH("C3H8",              "74-98-6", "hayduk86",   "A+B/T+Cln(T)",      -291.338,         1.44345e4,  39.4740)   ! p.325
    CALL CalcH("1-propanol",        "71-23-8", "L",          "A+B/T",             -27.507,          6246.8)                ! p.325
    CALL CalcH("2-propanol",        "67-63-0", "L",          "A+B/T",             -27.480,          6179.2)                ! p.326
    CALL CalcH("C4Cl6",             "87-68-3", "L",          "A+B/T",             -24.313,          3075.2)                ! p.327
    CALL CalcH("fluoxene",         "406-90-6", "L",          "A+B/T",             -27.909,          4018.5)                ! p.328
    CALL CalcH("MEK",               "78-93-3", "L",          "A+B/T",             -23.889,          4571.7)                ! p.330
    CALL CalcH("isobutane",         "75-28-5", "hayduk86",   "A+B/T+Cln(T)",      -383.440,         1.83044e4,  53.4651, &
      note = "It is unclear why the value given by \citet{"//TRIM(ref)//"} is about 3 "// &
      "times higher than that given by \citet{477} (and others), even though "// &
      "both refer to \citet{hayduk86}.")                                                                                    ! p.332
    CALL CalcH("butane",           "106-97-8", "hayduk86",   "A+B/T+Cln(T)",      -288.034,         1.4604e4,   38.7599)   ! p.332
    CALL CalcH("1-butanol",         "71-36-3", "L",          "A+B/T",             -27.756,          6255.0)                ! p.333
    CALL CalcH("2-butanol",         "78-92-2", "L",          "A+B/T",             -31.820,          7432.5)                ! p.334
    CALL CalcH("C6HCl5",           "608-93-5", "V",          "A",                 3.56e-5)                                 ! p.336
    CALL CalcH("C6HCl5",           "608-93-5", "V",          "A",                 2.40e-5)                                 ! p.336
    CALL CalcH("C6HCl5O",           "87-86-5", "V",          "A",                 1.06e-5)                                 ! p.337
    CALL CalcH("C6HCl5O",           "87-86-5", "E",          "A",                 1.8e-3)                                  ! p.337
    CALL CalcH("1235-C6H2Cl4",     "634-90-2", "V",          "A",                 2.11e-6)                                 ! p.339
    CALL CalcH("1235-C6H2Cl4",     "634-90-2", "V",          "A",                 1.76e-6)                                 ! p.339
    CALL CalcH("1245-C6H2Cl4",      "95-94-3", "V",          "A",                 2.78e-7)                                 ! p.339
    CALL CalcH("1245-C6H2Cl4",      "95-94-3", "V",          "A",                 1.08e-6)                                 ! p.339
    CALL CalcH("123-C6H3Cl3",       "87-61-6", "V",          "A",                 5.86e-6)                                 ! p.340
    CALL CalcH("123-C6H3Cl3",       "87-61-6", "V",          "A",                 2.14e-6)                                 ! p.340
    CALL CalcH("124-C6H3Cl3",      "120-82-1", "V",          "A",                 7.10e-6)                                 ! p.341
    CALL CalcH("124-C6H3Cl3",      "120-82-1", "V",          "A",                 8.65e-6)                                 ! p.341
    CALL CalcH("135-C6H3Cl3",      "108-70-3", "V",          "A",                 1.36e-6)                                 ! p.341
    CALL CalcH("135-C6H3Cl3",      "108-70-3", "V",          "A",                 8.48e-7)                                 ! p.341
    CALL CalcH("245-C6H3Cl3O",      "95-95-4", "V",          "A",                 4.66e-4)                                 ! p.342
    CALL CalcH("12-C6H4Cl2",        "95-50-1", "L",          "A+B/T",             -29.527,          5258.8)                ! p.343
    CALL CalcH("13-C6H4Cl2",       "541-73-1", "L",          "A+B/T",             -27.076,          4319.9)                ! p.344
    CALL CalcH("14-C6H4Cl2",       "106-46-7", "L",          "A+B/T",             -27.230,          4447.9)                ! p.345
    CALL CalcH("C6H5Br",           "108-86-1", "L",          "A+B/T",             -26.283,          4195.0)                ! p.347
    CALL CalcH("C6H5Cl",           "108-90-7", "V",          "A+B/T",             -20.930,          2440.7)                ! p.349
    CALL CalcH("2-C6H5ClO",         "95-57-8", "V",          "A",                 1.23e-3)                                 ! p.350
    CALL CalcH("3-C6H5ClO",        "108-43-0", "V",          "A",                 7.36e-3)                                 ! p.350
    ! see SUBROUTINE ref2451 for next value:
    !CALL CalcH("3-C6H5ClO",       "108-43-0", "ref2451",    "A+B/T",             -24.953,          6432.8)                ! p.350
    CALL CalcH("4-C6H5ClO",        "106-48-9", "V",          "A",                 1.22e-2)                                 ! p.351
    ! see SUBROUTINE ref2451 for next value:
    !CALL CalcH("4-C6H5ClO",       "106-48-9", "ref2451",    "A+B/T",             -36.742,          11059.)                ! p.351
    CALL CalcH("toluene",          "108-88-3", "shaw89",     "A+B/T",             -27.551,          4286.9)                ! p.352
    CALL CalcH("styrene",          "100-42-5", "C",          "A+B/T",             -25.190,          3787.9)                ! p.354
    CALL CalcH("o-xylene",          "95-47-6", "L",          "A+B/T",             -27.007,          4196.7)                ! p.355
    CALL CalcH("m-xylene",         "108-38-3", "shaw89",     "A+B/T",             -27.569,          4267.5)                ! p.356
    CALL CalcH("p-xylene",         "106-42-3", "L",          "A+B/T",             -27.407,          4240.0)                ! p.357
    CALL CalcH("C8H10",            "100-41-4", "L",          "A+B/T",             -29.658,          4828.3)                ! p.358
    CALL CalcH("123-tri-Mebenz",   "526-73-8", "L",          "A+B/T",             -28.980,          4816.6)                ! p.362
    CALL CalcH("124-tri-Mebenz",    "95-63-6", "L",          "A+B/T",             -23.644,          3084.8)                ! p.363
    CALL CalcH("C10H8",             "91-20-3", "L",          "A+B/T",             -28.605,          5329.3)                ! p.367
    CALL CalcH("1-C11H10",          "90-12-0", "L",          "A+B/T",             -31.061,          6060.0)                ! p.372
    CALL CalcH("2-C11H10",          "91-57-6", "L",          "A+B/T",             -29.799,          5629.3)                ! p.373
    CALL CalcH("acenaphthylene",   "208-96-8", "L",          "A+B/T",             -31.867,          6727.4)                ! p.380
    CALL CalcH("acenaphthene",      "83-32-9", "L",          "A+B/T",             -31.499,          6466.4)                ! p.382
    CALL CalcH("fluorene",          "86-73-7", "L",          "A+B/T",             -29.402,          6036.2)                ! p.383
    CALL CalcH("anthracene",       "120-12-7", "L",          "A+B/T",             -27.620,          5654.3)                ! p.385
    CALL CalcH("phenanthrene",      "85-01-8", "L",          "A+B/T",             -22.366,          4169.5)                ! p.386
    !----------------------
    ! The regression here uses only the data from Lau et al. (called Ref. 174 in the Table):
    ! KHpc [atm*m3/mol]
    chem = "metolachlor" ; casrn = "51218-45-2" ! C{15}H{22}ClNO2
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 293.2, 298.2, 303.2, 313.2 /)
    Harray = 1. / ( atm * (/ 9.0e-9, 1.39e-8, 3.8e-8, 1.98e-7 /) )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL SettypeX("lau95")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)
    !----------------------
    CALL CalcH("metolachlor",    "51218-45-2", "L",          "A+B/T",             -51.5808,         15235.)                ! p.388
    CALL CalcH("fluoranthene",     "206-44-0", "L",          "A+B/T",             -23.705,          4824.6)                ! p.389
    CALL CalcH("pyrene",           "129-00-0", "L",          "A+B/T",             -23.401,          4794.8)                ! p.389
    CALL CalcH("benzanthracene",    "56-55-3", "L",          "A+B/T",             -33.349,          7854.5)                ! p.390
    !----------------------
    ! errata from http://www.henrys-law.org/fogg-sangster-errata.html
    CALL erratum("CH2BrCl", "74-97-5", &
      "285", "The data in their table look strange (9.70R) and are not used here.")
    CALL erratum("C3H3Br", "106-96-7", &
      "321", "Data from \citet{1330} are cited with a typo. The value at " // &
      "313.2~\unit{K} should probably be 4.78\E{-6}, not 4.78\E{-2}.")
    CALL erratum("isopropylbenzene", "98-82-8", &
      "365", "Data from \citet{2904} are cited incorrectly, giving " // &
      "the same values at 308.2~\unit{K} and 318.2~\unit{K}, respectively.")
    CALL erratum("C12H5Cl3O2", "39227-58-2", &
      "376", "Data from \citet{644} are cited incorrectly, it should be 3.64, not 3.84.")

  CONTAINS

    SUBROUTINE erratum (chem_, casrn_, page, text)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      CHARACTER(LEN=*), INTENT(IN)           :: page, text
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "W"
      CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
        "Erratum for page " // page // " of \citet{" // TRIM(ref) // "}: " // text)
      CALL Output(DUMMY)
    END SUBROUTINE erratum

    SUBROUTINE CalcH (chem_, casrn_, type_or_xref, formula, A, B, C, D, note)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      CHARACTER(LEN=*), INTENT(IN)           :: type_or_xref
      CHARACTER(LEN=*), INTENT(IN)           :: formula
      REAL,             INTENT(IN)           :: A
      REAL,             INTENT(IN), OPTIONAL :: B, C, D
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: note

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      ! If type_or_xref has only 1 character, it is the type.
      ! Else, it is the xref and the type is "X":
      IF (LEN(type_or_xref)==1) THEN
        type = type_or_xref
      ELSE
        CALL SettypeX(type_or_xref)
      ENDIF

      IF (PRESENT(note)) THEN
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn), note)
      ENDIF

      IF (casrn_=="630-08-0") THEN ! CO
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 260 of \citet{" // TRIM(ref) // "}: " // &
          "The corresponding equation in preferred units is incorrect. " // &
          "The last term must be divided by 10000 (i.e.\ 0.0704, not 704.")
      ENDIF
      IF (casrn_=="7722-84-1") THEN ! H2O2
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 264 of \citet{" // TRIM(ref) // "}: " // &
          "The second value from their Ref.\ [10] refers to " // &
          "291.15~\unit{K}, not 281.15~\unit{K}.")
      ENDIF
      IF (casrn_=="7440-59-7") THEN ! He
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 265 of \citet{" // TRIM(ref) // "}: " // &
          "The corresponding equation is incorrect. The " // &
          "second term should not be divided by 100~\unit{K}.")
      ENDIF
      IF (casrn_=="10102-43-9") THEN ! NO
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 269 of \citet{" // TRIM(ref) // "}: " // &
          "The equation is incorrect and not consistent with the " // &
          "corresponding equation for $\ln(x)$: The temperature in the " // &
          "last term must be divided by 100 (i.e.\ $\ln(T/100)$ " // &
          "not $\ln(T)$) and an additional term of $\ln(100)$ must be added.")
      ENDIF
      IF (casrn_=="7782-44-7") THEN ! O2
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 270 of \citet{" // TRIM(ref) // "}: " // &
          "The CAS registry number is incorrect, and the " // &
          "corresponding equation is incorrect. The first term " // &
          "should be $-178.763753281$, not $-187.07794$.")
      ENDIF
      IF (casrn_=="558-13-4") THEN ! CBr4
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 274 of \citet{" // TRIM(ref) // "}: " // &
          "The value in the table is $k_\text{H}$, not $\ln k_\text{H}$.")
        CALL MakeNoteOtherTemp("303")
      ENDIF
      IF (casrn_=="74-98-6") THEN ! C3H8
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 325 of \citet{" // TRIM(ref) // "}: " // &
          "The second term in the equation describing the recommended " // &
          "data should be a division by T, not a multiplication, i.e.\ 1.44345E4/$T$.")
      ENDIF
      IF (casrn_=="95-50-1") THEN ! 1,2-dichlorobenzene
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 344 of \citet{" // TRIM(ref) // "}: " // &
          "The reference [89] seems to be incorrect, it " // &
          "does not contain 1,2-dichlorobenzene.")
      ENDIF
      IF (casrn_=="95-57-8") THEN ! 2-C6H5ClO
        CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-erratum", &
          "Erratum for page 350 of \citet{" // TRIM(ref) // "}: " // &
          "The equation describing the recommended temperature-dependent " // &
          "data appears to be incorrect and is not used here.")
      ENDIF

      SELECT CASE(formula)
      CASE ("A")
        IF (PRESENT(B)) STOP 'ERROR in ref1943'
        CALL Output(A*HbpSI_TO_HcpSI)
      CASE ("A+B/T")
        IF (PRESENT(C)) STOP 'ERROR in ref1943'
        CALL Output(EXP(A+B/T0)*HbpSI_TO_HcpSI, B)
      CASE ("A+B/T'+Cln(T')")
        IF (PRESENT(D)) STOP 'ERROR in ref1943'
        Hominus = EXP( A + B/(T0/100.) + C * LOG(T0/100.) ) * HbpSI_TO_HcpSI
        mindHR = 100.*B - C*T0
        CALL Output(Hominus, mindHR)
      CASE ("A+B/T+Cln(T)")
        IF (PRESENT(D)) STOP 'ERROR in ref1943'
        Hominus = EXP( A + B/T0 + C * LOG(T0) ) * HbpSI_TO_HcpSI
        mindHR = B - C*T0
        CALL Output(Hominus, mindHR)
      CASE ("A+BT+CT^2")
        IF (PRESENT(D)) STOP 'ERROR in ref1943'
        Hominus = EXP( A + B*T0 + C*T0**2 ) * HbpSI_TO_HcpSI
        mindHR = -B*T0**2 - 2.*C*T0**3
        CALL Output(Hominus, mindHR)
      CASE ("A+B/T+C/T^2+D/T^3")
        Hominus = EXP( A + B/T0 + C/T0**2 + D/T0**3 ) * HbpSI_TO_HcpSI
        mindHR = B + 2.*C/T0 + 3.*D/T0**2
        CALL Output(Hominus, mindHR)
      CASE ("A+B/T'+Cln(T')+DT'")
        Hominus = EXP( A + B/(T0/100.) + C*LOG(T0/100.) + D*T0/100. ) * HbpSI_TO_HcpSI
        mindHR = 100.*B - C*T0 - D*T0**2/100.
        CALL Output(Hominus, mindHR)
      CASE ("A+B/T+Cln(T)+DT")
        Hominus = EXP( A + B/T0 + C*LOG(T0) + D*T0 ) * HbpSI_TO_HcpSI
        mindHR = B - C*T0 - D*T0**2
        CALL Output(Hominus, mindHR)
      CASE DEFAULT
        PRINT *, 'ERROR in formula for ref1943: ', formula
        STOP
      END SELECT

    END SUBROUTINE CalcH

  END SUBROUTINE ref1943

  !---------------------------------------------------------------------------

  SUBROUTINE ref1945 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1945"
    type = "L"

    ! data from Table 5-4:
    CALL CalcH("O2",                  "7782-44-7", 1.27E-3,    -161.6,     8160.,   22.39)
    CALL CalcH("O3",                 "10028-15-6", 1.03E-2,    -14.08,     2830.)
    CALL CalcH("H",                  "12385-13-6", 2.6E-4)
    CALL CalcH("OH",                  "3352-57-6", 39.)
    CALL CalcH("HO2",                 "3170-83-0", 690.)
    CALL CalcH("H2O2",                "7722-84-1", 7.73E4,     -13.27,     7310.)
    CALL CalcH("N2",                  "7727-37-9", 6.52E-4,    -177.1,     8640.,   24.71)
    CALL CalcH("NH3",                 "7664-41-7", 60.2,       -9.84,      4160.)
    CALL CalcH("NH2Cl",              "10599-90-3", 87.,        -15.51,     5960.)
    CALL CalcH("NHCl2",               "3400-09-7", 29.,        -10.68,     4180.)
    CALL CalcH("NCl3",               "10025-85-1", 0.10,       -16.17,     4130.)
    CALL CalcH("NO",                 "10102-43-9", 1.92E-3,    -157.1,     7950.,   21.298)
    CALL CalcH("NO2",                "10102-44-0", 1.4E-2)
    CALL CalcH("NO3",                "12033-49-7", 3.8E-2)
    CALL CalcH("N2O",                "10024-97-2", 2.42E-2,    -148.1,     8610.,   20.266)
    CALL CalcH("CO",                   "630-08-0", 9.81E-4,    -178.0,     8750.,   24.875)
    CALL CalcH("CO2",                  "124-38-9", 3.38E-2,    -145.1,     8350.,   19.960)
    CALL CalcH("CH4",                   "74-82-8", 1.41E-3,    -194.7,     9750.,   27.274)
    CALL CalcH("C2H6",                  "74-84-0", 1.88E-3,    -240.2,     12420.,  33.744)
    CALL CalcH("C3H8",                  "74-98-6", 1.51E-3,    -281.1,     14510.,  39.652)
    CALL CalcH("C4H10",                "106-97-8", 1.24E-3,    -269.9,     14330.,  37.734)
    CALL CalcH("CH3CH(CH3)CH3",         "75-28-5", 9.18E-4,    -360.6,     18020.,  51.444)
    CALL CalcH("C2H4",                  "74-85-1", 5.96E-3,    -154.6,     8540.,   21.202)
    CALL CalcH("C2H2",                  "74-86-2", 4.14E-2,    -145.8,     7880.,   20.384)
    CALL CalcH("CH3F",                 "593-53-3", 6.15E-2,    -9.478,     1990.)
    CALL CalcH("CH3Cl",                 "74-87-3", 0.127,      -13.13,     3270.)
    CALL CalcH("CH3Br",                 "74-83-9", 0.173,      -12.16,     3100.)
    CALL CalcH("CH3I",                  "74-88-4", 0.200,      -13.52,     3550.)
    CALL CalcH("CH2Cl2",                "75-09-2", 0.366,      -14.68,     4080.)
    CALL CalcH("CHCl3",                 "67-66-3", 0.255,      -16.48,     4510.)
    CALL CalcH("CHCl2Br",               "75-27-4", 0.409,      -18.32,     5200.)
    CALL CalcH("CHClBr2",              "124-48-1", 0.868,      -18.67,     5530.)
    CALL CalcH("CHBr3",                 "75-25-2", 1.76,       -16.79,     5170.)
    CALL CalcH("CF2Cl2",                "75-71-8", 3.09E-3,    -17.41,     3470.)
    CALL CalcH("CFCl3",                 "75-69-4", 1.07E-2,    -15.74,     3340.)
    CALL CalcH("CCl4",                  "56-23-5", 3.47E-2,    -17.38,     4180.)
    CALL CalcH("CH3OH",                 "67-56-1", 220.,       -12.08,     5210.)
    CALL CalcH("CH3CH2OH",              "64-17-5", 200.,       -16.98,     6630.)
    CALL CalcH("n-C3H7OH",              "71-23-8", 130.,       -20.16,     7470.)
    CALL CalcH("iso-C3H7OH",            "67-63-0", 130.,       -20.15,     7450.)
    CALL CalcH("n-C4H9OH",              "71-36-3", 127.,       -19.34,     7210.)
    CALL CalcH("iso-C4H9OH",            "78-83-1", 102.)
    CALL CalcH("sec-C4H9OH",            "78-92-2", 110.,       -19.65,     7260.)
    CALL CalcH("tert-C4H9OH",           "75-65-0", 70.,        -23.63,     8310.)
    CALL CalcH("CH3OOH",              "3031-73-0", 300.,       -11.99,     5280.)
    CALL CalcH("HOCH2OOH",           "15932-89-5", 1.7E6,      -18.79,     9870.)
    CALL CalcH("HCHO",                  "50-00-0", 3.23E3,     -15.73,     7100.)
    CALL CalcH("CH3CHO",                "75-07-0", 12.9,       -17.19,     5890.)
    CALL CalcH("C2H5CHO",              "123-38-6", 10.0,       -12.20,     4330.)
    CALL CalcH("C3H7CHO",              "123-72-8", 9.6,        -18.59,     6220.)
    CALL CalcH("CH3COCH3",              "67-64-1", 28.1,       -13.62,     5050.)
    CALL CalcH("C2H5COCH3",             "78-93-3", 18.,        -16.40,     5740.)
    CALL CalcH("HC(O)OH",               "64-18-6", 8.9E3,      -11.40,     6100.)
    CALL CalcH("CH3C(O)OH",             "64-19-7", 4.1E3,      -12.50,     6200.)
    CALL CalcH("CH3C(O)C(O)OH",        "127-17-3", 3.11E5,     -4.417,     5090.)
    CALL CalcH("CH3CN",                 "75-05-8", 52.8,       -9.35,      3970.)
    CALL CalcH("CH3NO2",                "75-52-5", 34.6,       -9.92,      4010.)
    CALL CalcH("C2H5NO2",               "79-24-3", 21.7,       -11.80,     4430.)
    CALL CalcH("C3H7NO2",              "108-03-2", 13.1,       -13.22,     4710.)
    CALL CalcH("CH3CH(NO2)CH3",         "79-46-9", 8.42,       -13.02,     4520.)
    CALL CalcH("CH3ONO2",              "598-58-3", 2.0,        -15.20,     4740.)
    CALL CalcH("C2H5ONO2",             "625-58-1", 1.59,       -17.50,     5360.)
    CALL CalcH("1-C3H7ONO2",           "627-13-4", 1.10,       -18.31,     5490.)
    CALL CalcH("2-C3H7ONO2",          "1712-64-7", 0.791,      -18.20,     5360.)
    CALL CalcH("1-C4H9ONO2",           "928-45-0", 1.01,       -19.40,     5790.)
    CALL CalcH("2-C4H9ONO2",           "924-52-7", 0.648,      -18.59,     5410.)
    CALL CalcH("CH3C(O)O2NO2",        "2278-22-0", 2.8,        -18.15,     5730.)
    CALL CalcH("O2NOC2H4ONO2",         "628-96-6", 640.)
    CALL CalcH("HOC2H4ONO2",         "16051-48-2", 3.99E4)
    CALL CalcH("HOCH2CH(ONO2)CH3",   "20266-74-4", 7.3E3)
    CALL CalcH("CH3CH(OH)CH2ONO2",   "20266-65-3", 6.7E3)
    CALL CalcH("CH3CH(ONO2)CH2ONO2",  "6423-43-4", 175.)
    CALL CalcH("CH3C(O)CH2ONO2",      "6745-71-7", 1.01E3)
    CALL CalcH("Cl",                 "22537-15-1", 2.3)
    CALL CalcH("Cl2",                 "7782-50-5", 9.29E-2,    -134.4,     7590.,   18.702)
    CALL CalcH("ClO",                "14989-30-1", 0.71)
    CALL CalcH("Cl2O",                "7791-21-1", 17.,        -3.23,      1810.)
    CALL CalcH("ClO2",               "10049-04-4", 1.01,       -11.65,     3470.)
    CALL CalcH("HOCl",                "7790-92-3", 660.,       -13.2,      5880.)
    CALL CalcH("Br2",                 "7726-95-6", 0.725,      -15.05,     4390.)
    CALL CalcH("BrCl",               "13863-41-7", 0.98,       -18.9,      5630.)
    CALL CalcH("SO2",                 "7446-09-5", 1.36,       -39.72,     4250.,   4.525)
    CALL CalcH("H2S",                 "7783-06-4", 0.102,      -145.2,     8120.,   20.296)
    CALL CalcH("CS2",                   "75-15-0", 0.062,      -17.05,     4250.)
    CALL CalcH("COS",                  "463-58-1", 2.02E-2,    -15.68,     3510.)
    CALL CalcH("CH3SH",                 "74-93-1", 0.39,       -12.42,     3420.)
    CALL CalcH("C2H5SH",                "75-08-1", 0.28,       -13.82,     3740.)
    CALL CalcH("CH3SCH3",               "75-18-3", 0.54,       -12.19,     3460.)
    CALL CalcH("CH3S(O)CH3",            "67-68-5", 9.9E4)

    chem = "CH3C(O)O2" ; casrn = "36709-10-1"
    CALL Output(0.1*Hcp_TO_HcpSI, limit="<")

    chem = "HOBr" ; casrn = "13517-11-8"
    CALL Output(1.3E2*Hcp_TO_HcpSI, limit=">")

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H298, A, B, C)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H298
      REAL, OPTIONAL,   INTENT(IN) :: A, B, C

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      Hominus   = H298*Hcp_TO_HcpSI
      IF (PRESENT(C)) THEN
        ! T-dep with 3 parameters:
        ! ln(H) = A + B/T + C*ln(T)
        ! analytical derivative:
        ! d ln(H) / d (1/T) = B - C*T
        CALL consistency_check(Hominus, EXP(A+B/T0+C*LOG(T0))*Hcp_TO_HcpSI, &
          "The H298 and A,B,C data listed in Table 5.4")
        mindHR = B - C*T0
        CALL Output(Hominus, mindHR)
      ELSE
        IF (PRESENT(B)) THEN
          ! T-dep with 2 parameters A, B
          CALL consistency_check(Hominus, EXP(A+B/T0)*Hcp_TO_HcpSI, &
          "The H298 and A,B data listed in Table 5.4")
          mindHR = B
          CALL Output(Hominus, mindHR)
        ELSE
          ! no T-dep available
          CALL Output(Hominus)
        ENDIF
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref1945

  !---------------------------------------------------------------------------

  SUBROUTINE ref1992 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1992"
    type = "M"
    chem = "phosgene" ; casrn = "75-44-5" ! CCl2O

    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 15., 25., 35., 45.5 /) + CtoK
    Harray = (/ 0.109, 0.069, 0.046, 0.027 /)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1992

  !---------------------------------------------------------------------------

  ! ref1994 only a new fuction for Tdep

  !---------------------------------------------------------------------------

  SUBROUTINE ref1995 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1995"
    type = "V"

    CALL CalcH("malathion",             "121-75-5", 3.9E-8) ! C{10}H{19}O6PS2
    CALL CalcH("$\alpha$-endosulfan",   "959-98-8", 6.7E-6) ! endosulfan I
    CALL CalcH("$\beta$-endosulfan",  "33213-65-9", 6.2E-7) ! endosulfan II
    CALL CalcH("fenvalerate",         "51630-58-1", 1.4E-7)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(1./(atm*H))
    END SUBROUTINE CalcH

  END SUBROUTINE ref1995

  !---------------------------------------------------------------------------

  SUBROUTINE ref1996 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "1996"
    type = "M"

    CALL CalcH("nitromethane",    "75-52-5", (/ 126., 203., 303.,   447. /) ) ! CH3NO2
    CALL CalcH("nitroethane",     "79-24-3", (/ 196., 329., 526.,   788. /) ) ! C2H5NO2
    CALL CalcH("1-nitropropane", "108-03-2", (/ 317., 559., 909.,  1395. /) ) ! C3H7NO2
    CALL CalcH("2-nitropropane",  "79-46-9", (/ 500., 859., 1370., 2070. /) ) ! CH3CH(NO2)CH3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H12)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: H12
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(H12)
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = (/ 293.15, 303.15, 313.15, 323.15 /)
      Harray = KHpx_TIMES_HcpSI / (H12*1.E3/atm)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1996

  !---------------------------------------------------------------------------

END MODULE Henry_ref2000

!*****************************************************************************
!                                  end of file
!*****************************************************************************
