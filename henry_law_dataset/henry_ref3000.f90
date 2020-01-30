!*****************************************************************************
!                   Time-stamp: <2015-04-22 14:05:11 sander>
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

MODULE henry_ref3000

  USE henry_mem
  USE henry_util
  IMPLICIT NONE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ref2512 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2512"
    type = "M"

    chem = "dimethyl sulfide" ; casrn = "75-18-3" ! CH3SCH3 DMS
    CALL Output(4.80E-1*Hcp_TO_HcpSI,3730.)

    chem = "ozone" ; casrn = "10028-15-6" ! O3
    CALL Output(1.07E-2*Hcp_TO_HcpSI,2330.)

  END SUBROUTINE ref2512

  !---------------------------------------------------------------------------

  SUBROUTINE ref2521 ! KHcc [1]
    IMPLICIT NONE

    ref = "2521"

    ! the note "mydiff" is added when the value for diff was calculated
    ! here instead of taken from ref2521.

    CALL CalcH(   1, "methane",                                         "74-82-8",  1.21  ,  1.46  ,  0.25, "479")
    CALL CalcH(   2, "ethane",                                          "74-84-0",  1.30  ,  1.34  ,  0.04, "479")
    CALL CalcH(   3, "propane",                                         "74-98-6",  1.46  ,  1.44  , -0.02, "479")
    CALL CalcH(   4, "butane",                                         "106-97-8",  1.54  ,  1.52  , -0.02, "479")
    CALL CalcH(   5, "pentane",                                        "109-66-0",  1.61  ,  1.71  ,  0.10, "479")
    CALL CalcH(   6, "hexane",                                         "110-54-3",  1.72  ,  1.82  ,  0.10, "479")
    CALL CalcH(   7, "heptane",                                        "142-82-5",  1.87  ,  1.96  ,  0.09, "479")
    CALL CalcH(   8, "octane",                                         "111-65-9",  2.01  ,  2.11  ,  0.10, "1500")
    CALL CalcH(   9, "nonane",                                         "111-84-2",  2.13  ,  2.3   ,  0.17, "479")
    CALL CalcH(  10, "decane",                                         "124-18-5",  2.27  ,  2.32  ,  0.05, "1501")
    CALL CalcH(  11, "undecane",                                      "1120-21-4",  2.42  ,  2.86  ,  0.44, "479")
    CALL CalcH(  12, "dodecane",                                       "112-40-3",  2.57  ,  2.54  , -0.03, "2528")
    CALL CalcH(  13, "tridecane",                                      "629-50-5",  2.71)
    CALL CalcH(  14, "tetradecane",                                    "629-59-4",  2.86)
    CALL CalcH(  15, "pentadecane",                                    "629-62-9",  3.00)
    CALL CalcH(  16, "hexadecane",                                     "544-76-3",  3.15)
    CALL CalcH(  17, "heptadecane",                                    "629-78-7",  3.27)
    CALL CalcH(  18, "octadecane",                                     "593-45-3",  3.44)
    CALL CalcH(  19, "nonadecane",                                     "629-92-5",  3.49)
    CALL CalcH(  20, "eicosane",                                       "112-95-8",  3.62)
    CALL CalcH(  21, "heneiosane",                                     "629-94-7",  3.74)
    CALL CalcH(  22, "docosane",                                       "629-97-0",  3.87)
    CALL CalcH(  23, "tricosane",                                      "638-67-5",  3.99)
    CALL CalcH(  24, "tetracosane",                                    "646-31-1",  4.11)
    CALL CalcH(  25, "pentacosane",                                    "629-99-2",  4.44)
    CALL CalcH(  26, "hexacosane",                                     "630-01-3",  4.58)
    CALL CalcH(  27, "heptacosnae",                                    "593-49-7",  4.72)
    CALL CalcH(  28, "octacosane",                                     "630-02-4",  4.86)
    CALL CalcH(  29, "nonacosane",                                     "630-03-5",  5.00)
    CALL CalcH(  30, "n-triacontane",                                  "638-68-6",  5.14)
    CALL CalcH(  31, "dotriacontane",                                  "544-85-4",  5.42)
    CALL CalcH(  32, "pentatricontane",                                "630-07-9",  5.84)
    CALL CalcH(  33, "octatricontane",                                "7194-85-6",  6.26)
    CALL CalcH(  34, "propane, 2-methyl-",                              "75-28-5",  1.86  ,  1.70  , -0.16, "1500")
    CALL CalcH(  35, "propane, 2,2-dimethyl-",                         "463-82-1",  2.21  ,  1.84  , -0.37, "1500")
    CALL CalcH(  36, "butane, 2-methyl-",                               "78-78-4",  1.80  ,  1.75  , -0.05, "1910")
    CALL CalcH(  37, "butane, 2,2-dimethyl-",                           "75-83-2",  2.07  ,  1.77  , -0.30, "642")
    CALL CalcH(  38, "butane, 2,3-dimethyl-",                           "79-29-8",  1.88  ,  1.72  , -0.16, "642")
    CALL CalcH(  39, "butane, 2,2,3-trimethyl-",                       "464-06-2",  2.09  ,  2.0   , -0.09, "789")
    CALL CalcH(  40, "butane, 2,2,3,3-tetramethyl-",                   "594-82-1",  2.08  ,  2.2   ,  0.12, "789")
    CALL CalcH(  41, "pentane, 2-methyl-",                             "107-83-5",  1.92  ,  1.81  , -0.11, "C")
    CALL CalcH(  42, "pentane, 3-methyl-",                              "96-14-0",  1.81  ,  1.84  ,  0.03, "479")
    CALL CalcH(  43, "pentane, 2,2-dimethyl-",                         "590-35-2",  2.20  ,  2.11  , -0.09, "479")
    CALL CalcH(  44, "pentane, 2,3-dimethyl-",                         "565-59-3",  1.92  ,  1.85  , -0.07, "479")
    CALL CalcH(  45, "pentane, 2,4-dimethyl-",                         "108-08-7",  2.27  ,  2.08  , -0.19, "479")
    CALL CalcH(  46, "pentane, 3,3-dimethyl-",                         "562-49-2",  2.00  ,  1.88  , -0.12, "479")
    CALL CalcH(  47, "pentane, 3-ethyl-",                              "617-78-7",  1.88  ,  2.02  ,  0.14, "789")
    CALL CalcH(  48, "pentane, 3,3-diethyl-",                         "1067-20-5",  1.99  ,  1.88  , -0.11, "789")
    CALL CalcH(  49, "pentane, 2,3,4-trimethyl-",                      "565-75-3",  2.11  ,  1.88  , -0.23, "479")
    CALL CalcH(  50, "pentane, 2,2,3-trimethyl-",                      "564-02-3",  2.17  ,  2.2   ,  0.03, "789")
    CALL CalcH(  51, "pentane, 2,3,3-trimethyl-",                      "560-21-4",  2.05  ,  2.23  ,  0.18, "789")
    CALL CalcH(  52, "pentane, 3-ethyl-2-methyl-",                     "609-26-7",  2.05  ,  2.19  ,  0.14, "789")
    CALL CalcH(  53, "pentane, 3-ethyl-3-methyl-",                    "1067-08-9",  1.95  ,  2.25  ,  0.30, "789")
    CALL CalcH(  54, "pentane, 2,2,4-trimethyl-",                      "540-84-1",  2.51  ,  2.09  , -0.42, "479")
    CALL CalcH(  55, "pentane, 3-ethyl,2,2-dimethyl-",               "16747-32-3",  2.33  ,  2.35  ,  0.02, "789")
    CALL CalcH(  56, "pentane, 3-ethyl,2,3-dimethyl-",               "16747-33-4",  2.06  ,  2.43  ,  0.37, "789")
    CALL CalcH(  57, "pentane, 3-ethyl,2,4-dimethyl-",                "1068-87-7",  2.32  ,  2.35  ,  0.03, "789")
    CALL CalcH(  58, "pentane, 2,2,3,3-tetramethyl-",                 "7154-79-2",  2.05  ,  2.41  ,  0.36, "789")
    CALL CalcH(  59, "pentane, 2,2,3,4-tetramethyl-",                 "1186-53-4",  2.32  ,  2.38  ,  0.06, "789")
    CALL CalcH(  60, "pentane, 2,2,4,4-tetramethyl-",                 "1070-87-7",  2.65  ,  2.33  , -0.32, "789")
    CALL CalcH(  61, "pentane, 2,3,3,4-tetramethyl-",                "16747-38-9",  2.17  ,  2.41  ,  0.24, "789")
    CALL CalcH(  62, "pentane, 2,2,3,3,4-pentamethyl-",              "16747-44-7",  2.49  ,  2.61  ,  0.12, "789")
    CALL CalcH(  63, "pentane, 2,2,3,4,4-pentamethyl-",              "16747-45-8",  2.67  ,  2.61  , -0.06, "789")
    CALL CalcH(  64, "pentane, 3-ethyl-2,2,3-trimethyl-",            "52897-17-3",  2.29  ,  2.61  ,  0.32, "789")
    CALL CalcH(  65, "pentane, 3-ethyl-2,2,4-trimethyl-",            "52897-18-4",  2.53  ,  2.5   , -0.03, "789")
    CALL CalcH(  66, "pentane, 3-ethyl-2,3,4-trimethyl-",            "52897-19-5",  2.43  ,  2.57  ,  0.14, "789")
    CALL CalcH(  67, "pentane, 3,3-diethyl-2-methyl-",               "52897-16-2",  2.25  ,  2.57  ,  0.32, "789")
    CALL CalcH(  68, "pentane, 3-isopropyl-2,4-dimethyl-",           "13475-79-1",  2.60  ,  2.5   , -0.10, "789")
    CALL CalcH(  69, "hexane, 2-methyl-",                              "591-76-4",  2.04  ,  2.15  ,  0.11, "479")
    CALL CalcH(  70, "hexane, 3-methyl-",                              "589-34-4",  1.95  ,  1.99  ,  0.04, "479")
    CALL CalcH(  71, "hexane, 2,2 dimethyl-",                          "590-73-8",  2.32  ,  2.15  , -0.17, "789")
    CALL CalcH(  72, "hexane, 2,3 dimethyl-",                          "584-94-1",  2.08  ,  2.19  ,  0.11, "789")
    CALL CalcH(  73, "hexane, 3,3 dimethyl-",                          "563-16-6",  2.15  ,  2.19  ,  0.04, "789")
    CALL CalcH(  74, "hexane, 2,4-dimethyl",                           "589-43-5",  2.32  ,  2.16  , -0.16, "789")
    CALL CalcH(  75, "hexane, 2,5-dimethyl",                           "592-13-2",  2.37  ,  2.13  , -0.24, "789")
    CALL CalcH(  76, "hexane, 3,4-dimethyl",                           "583-48-2",  2.03  ,  2.22  ,  0.19, "789")
    CALL CalcH(  77, "hexane, 3-ethyl-",                               "619-99-8",  2.04  ,  2.19  ,  0.15, "789")
    CALL CalcH(  78, "hexane, 3-ethyl,2-methyl-",                    "16789-46-1",  2.24  ,  2.33  ,  0.09, "789")
    CALL CalcH(  79, "hexane, 4-ethyl,2-methyl-",                     "3074-75-7",  2.42  ,  2.29  , -0.13, "789")
    CALL CalcH(  80, "hexane, 3-ethyl,3-methyl-",                     "3074-76-8",  2.10  ,  2.38  ,  0.28, "789")
    CALL CalcH(  81, "hexane, 3-ethyl,4-methyl-",                     "3074-77-9",  2.12  ,  2.35  ,  0.23, "789")
    CALL CalcH(  82, "hexane, 2,2,3-trimethyl-",                     "16747-25-4",  2.33  ,  2.33  ,  0.00, "789")
    CALL CalcH(  83, "hexane, 2,2,4-trimethyl-",                     "16747-26-5",  2.57  ,  2.29  , -0.28, "789")
    CALL CalcH(  84, "hexane, 2,2,5-trimethyl-",                      "3522-94-9",  2.65  ,  2.15  , -0.50, "479")
    CALL CalcH(  85, "hexane, 2,3,3-trimethyl-",                     "16747-28-7",  2.22  ,  2.38  ,  0.16, "789")
    CALL CalcH(  86, "hexane, 2,3,4-trimethyl-",                       "921-47-1",  2.19  ,  2.35  ,  0.16, "789")
    CALL CalcH(  87, "hexane, 2,3,5-trimethyl-",                      "1069-53-0",  2.47  ,  2.3   , -0.17, "789")
    CALL CalcH(  88, "hexane, 2,4,4-trimethyl-",                     "16747-30-1",  2.46  ,  2.33  , -0.13, "789")
    CALL CalcH(  89, "hexane, 3,3,4-trimethyl-",                     "16747-31-2",  2.15  ,  2.38  ,  0.23, "789")
    CALL CalcH(  90, "hexane, 2,2,3,3-tetramethyl-",                 "13475-81-5",  2.35  ,  2.53  ,  0.18, "789")
    CALL CalcH(  91, "hexane, 2,2,3,4-tetramethyl-",                 "52897-08-2",  2.51  ,  2.53  ,  0.02, "789")
    CALL CalcH(  92, "hexane, 2,2,3,5-tetramethyl-",                 "52897-09-3",  2.68  ,  2.41  , -0.27, "789")
    CALL CalcH(  93, "hexane, 2,2,4,4-tetramethyl-",                 "51750-65-3",  2.66  ,  2.53  , -0.13, "789")
    CALL CalcH(  94, "hexane, 2,2,4,5-tetramethyl-",                 "16747-42-5",  2.70  ,  2.43  , -0.27, "789")
    CALL CalcH(  95, "hexane, 2,2,5,5-tetramethyl-",                  "1071-81-4",  2.94  ,  2.35  , -0.59, "789")
    CALL CalcH(  96, "hexane, 2,3,3,4-tetramethyl-",                 "52897-10-6",  2.45  ,  2.53  ,  0.08, "789")
    CALL CalcH(  97, "hexane, 2,3,3,5-tetramethyl-",                 "52897-11-7",  2.64  ,  2.46  , -0.18, "789")
    CALL CalcH(  98, "hexane, 2,3,4,4-tetramethyl-",                 "52897-12-8",  2.49  ,  2.53  ,  0.04, "789")
    CALL CalcH(  99, "hexane, 2,3,4,5-tetramethyl-",                 "52897-15-1",  2.54  ,  2.46  , -0.08, "789")
    CALL CalcH( 100, "hexane, 3,3,4,4-tetramethyl-",                  "5171-84-6",  2.26  ,  2.61  ,  0.35, "789")
    CALL CalcH( 101, "hexane, 3-ethyl-2,2-dimethyl-",                "20291-91-2",  2.35  ,  2.46  ,  0.11, "789")
    CALL CalcH( 102, "hexane, 4-ethyl-2,2-dimethyl-",                "52896-99-8",  2.58  ,  2.41  , -0.17, "789")
    CALL CalcH( 103, "hexane, 3-ethyl-2,3-dimethyl-",                "52897-00-4",  2.30  ,  2.5   ,  0.20, "789")
    CALL CalcH( 104, "hexane, 4-ethyl-2,3-dimethyl-",                "52897-01-5",  2.41  ,  2.46  ,  0.05, "789")
    CALL CalcH( 105, "hexane, 3-ethyl-2,4-dimethyl-",                 "7220-26-0",  2.40  ,  2.46  ,  0.06, "789")
    CALL CalcH( 106, "hexane, 4-ethyl-2,4-dimethyl-",                "52897-03-7",  2.49  ,  2.5   ,  0.01, "789")
    CALL CalcH( 107, "hexane, 3-ethyl-2,5-dimethyl-",                "52897-04-8",  2.57  ,  2.43  , -0.14, "789")
    CALL CalcH( 108, "hexane, 3-ethyl-2,2-dimethyl-",                "20291-91-2",  2.35  ,  2.46  ,  0.11, "789")
    CALL CalcH( 109, "hexane, 4-ethyl-3,3-dimethyl-",                "52897-05-9",  2.32  ,  2.5   ,  0.18, "789")
    CALL CalcH( 110, "hexane, 3-ethyl-3,4-dimethyl-",                "52897-06-0",  2.27  ,  2.5   ,  0.23, "789")
    CALL CalcH( 111, "hexane, 3,3-diethyl-",                         "17302-02-2",  2.07  ,  2.5   ,  0.43, "789")
    CALL CalcH( 112, "hexane, 3,4-diethyl-",                         "19398-77-7",  2.34  ,  2.46  ,  0.12, "789")
    CALL CalcH( 113, "hexane, 3-isopropyl-2-methyl-",                "62016-13-1",  2.42  ,  2.57  ,  0.15, "789")
    CALL CalcH( 114, "heptane, 2-methyl-",                             "592-27-8",  2.17  ,  2.18  ,  0.01, "479")
    CALL CalcH( 115, "heptane, 3-methyl-",                             "589-81-1",  2.09  ,  2.18  ,  0.09, "789")
    CALL CalcH( 116, "heptane, 4-methyl-",                             "589-53-7",  2.13  ,  2.18  ,  0.05, "789")
    CALL CalcH( 117, "heptane, 2,3-dimethyl-",                        "3074-71-3",  2.23  ,  2.33  ,  0.10, "789")
    CALL CalcH( 118, "heptane, 2,2-dimethyl-",                        "1071-26-7",  2.47  ,  2.29  , -0.18, "789")
    CALL CalcH( 119, "heptane, 2,4-dimethyl-",                        "2213-23-2",  2.46  ,  2.29  , -0.17, "789")
    CALL CalcH( 120, "heptane, 2,5-dimethyl-",                        "2216-30-0",  2.44  ,  2.3   , -0.14, "789")
    CALL CalcH( 121, "heptane, 2,6-dimethyl",                         "1072-05-5",  2.51  ,  2.29  , -0.22, "789")
    CALL CalcH( 122, "heptane, 3,3-dimethyl-",                        "4032-86-4",  2.24  ,  2.33  ,  0.09, "789")
    CALL CalcH( 123, "heptane, 3,4-dimethyl-",                         "922-28-1",  2.19  ,  2.35  ,  0.16, "789")
    CALL CalcH( 124, "heptane, 3,5-dimethyl-",                         "926-82-9",  2.42  ,  2.3   , -0.12, "789")
    CALL CalcH( 125, "heptane, 4,4-dimethyl-",                        "1068-19-5",  2.29  ,  2.33  ,  0.04, "789")
    CALL CalcH( 126, "heptane, 2,2,3-trimethyl-",                    "52896-92-1",  2.40  ,  2.43  ,  0.03, "789")
    CALL CalcH( 127, "heptane, 2,3,3-trimethyl-",                    "52896-93-2",  2.40  ,  2.46  ,  0.06, "789")
    CALL CalcH( 128, "heptane, 2,3,4-trimethyl-",                    "52896-95-4",  2.40  ,  2.46  ,  0.06, "789")
    CALL CalcH( 129, "heptane, 2,3,5-trimethyl-",                    "20278-85-7",  2.56  ,  2.46  , -0.10, "789")
    CALL CalcH( 130, "heptane, 2,3,6-trimethyl-",                     "4032-93-3",  2.58  ,  2.41  , -0.17, "789")
    CALL CalcH( 131, "heptane, 2,4,4-trimethyl-",                     "4032-92-2",  2.55  ,  2.43  , -0.12, "789")
    CALL CalcH( 132, "heptane, 2,4,5-trimethyl-",                    "20278-84-6",  2.55  ,  2.43  , -0.12, "789")
    CALL CalcH( 133, "heptane, 2,4,6-trimethyl-",                     "2613-61-8",  2.73  ,  2.35  , -0.38, "789")
    CALL CalcH( 134, "heptane, 2,5,5-trimethyl",                      "1189-99-7",  2.58  ,  2.43  , -0.15, "789")
    CALL CalcH( 135, "heptane, 3,3,4-trimethyl-",                    "20278-87-9",  2.33  ,  2.5   ,  0.17, "789")
    CALL CalcH( 136, "heptane, 3,3,5-trimethyl-",                     "7154-80-5",  2.54  ,  2.46  , -0.08, "789")
    CALL CalcH( 137, "heptane, 3,4,4-trimethyl-",                    "20278-88-0",  2.33  ,  2.5   ,  0.17, "789")
    CALL CalcH( 138, "heptane, 3,4,5-trimethyl-",                    "20278-89-1",  2.25  ,  2.46  ,  0.21, "789")
    CALL CalcH( 139, "heptane, 3-ethyl-2-methyl-",                   "14676-29-0",  2.31  ,  2.43  ,  0.12, "789")
    CALL CalcH( 140, "heptane, 4-ethyl-2-methyl-",                   "52896-88-5",  2.46  ,  2.41  , -0.05, "789")
    CALL CalcH( 141, "heptane, 5-ethyl-2-methyl-",                   "13475-78-0",  2.47  ,  2.41  , -0.06, "789")
    CALL CalcH( 142, "heptane, 3-ethyl-3-methyl-",                   "17302-01-1",  2.26  ,  2.46  ,  0.20, "789")
    CALL CalcH( 143, "heptane, 4-ethyl-3-methyl-",                   "52896-89-6",  2.26  ,  2.46  ,  0.20, "789")
    CALL CalcH( 144, "heptane, 3-ethyl-5-methyl-",                   "52896-90-9",  2.49  ,  2.41  , -0.08, "789")
    CALL CalcH( 145, "heptane, 3-ethyl-4-methyl-",                   "52896-91-0",  2.27  ,  2.46  ,  0.19, "789")
    CALL CalcH( 146, "heptane, 4-ethyl-4-methyl-",                   "17302-04-4",  2.22  ,  2.46  ,  0.24, "789")
    CALL CalcH( 147, "heptane, 3-ethyl-",                            "15869-80-4",  2.19  ,  2.33  ,  0.14, "789")
    CALL CalcH( 148, "heptane, 4-ethyl-",                             "2216-32-2",  2.20  ,  2.33  ,  0.13, "789")
    CALL CalcH( 149, "heptane, 4-propyl",                             "3178-29-8",  2.39  ,  2.38  , -0.01, "789")
    CALL CalcH( 150, "heptane, 4-isopropyl-",                        "52896-87-4",  2.29  ,  2.43  ,  0.14, "789")
    CALL CalcH( 151, "octane, 2-methyl-",                             "3221-61-2",  2.32  ,  2.29  , -0.03, "789")
    CALL CalcH( 152, "octane, 3-methyl-",                             "2216-33-3",  2.23  ,  2.3   ,  0.07, "789")
    CALL CalcH( 153, "octane, 4-methyl-",                             "2216-34-4",  2.24  ,  2.61  ,  0.37, "479")
    CALL CalcH( 154, "octane, 2,2-dimethyl-",                        "15869-87-1",  2.50  ,  2.38  , -0.12, "789")
    CALL CalcH( 155, "octane, 2,3-dimethyl-",                         "7146-60-3",  2.37  ,  2.43  ,  0.06, "789")
    CALL CalcH( 156, "octane, 2,4-dimethyl-",                         "4032-94-4",  2.51  ,  2.38  , -0.13, "789")
    CALL CalcH( 157, "octane, 2,5-dimethyl-",                        "15869-89-3",  2.50  ,  2.41  , -0.09, "789")
    CALL CalcH( 158, "octane, 2,6-dimethyl-",                         "2051-30-1",  2.53  ,  2.41  , -0.12, "789")
    CALL CalcH( 159, "octane, 2,7-dimethyl-",                         "1072-16-8",  2.60  ,  2.38  , -0.22, "789")
    CALL CalcH( 160, "octane, 3,3-dimethyl-",                         "4110-44-5",  2.38  ,  2.43  ,  0.05, "789")
    CALL CalcH( 161, "octane, 3,4-dimethyl-",                        "15869-92-8",  2.31  ,  2.43  ,  0.12, "789")
    CALL CalcH( 162, "octane, 3,5-dimethyl-",                        "15869-93-9",  2.47  ,  2.41  , -0.06, "789")
    CALL CalcH( 163, "octane, 3,6-dimethyl-",                        "15869-94-0",  2.48  ,  2.41  , -0.07, "789")
    CALL CalcH( 164, "octane, 4,4-dimethyl-",                        "15869-95-1",  2.43  ,  2.43  ,  0.00, "789")
    CALL CalcH( 165, "octane, 4,5-dimethyl-",                        "15869-96-2",  2.28  ,  2.43  ,  0.15, "789")
    CALL CalcH( 166, "octane, 3-ethyl-",                              "5881-17-4",  2.27  ,  2.41  ,  0.14, "789")
    CALL CalcH( 167, "octane, 4-ethyl-",                             "15869-86-0",  2.23  ,  2.41  ,  0.18, "789")
    CALL CalcH( 168, "nonane, 2-methyl-",                              "871-83-0",  2.43  ,  2.35  , -0.08, "789")
    CALL CalcH( 169, "nonane, 3-methyl-",                             "5911-04-6",  2.37  ,  2.38  ,  0.01, "789")
    CALL CalcH( 170, "nonane, 4-methyl-",                            "17301-94-9",  2.39  ,  2.38  , -0.01, "789")
    CALL CalcH( 171, "nonane, 5-methyl-",                            "15869-85-9",  2.38  ,  2.38  ,  0.00, "789")
    CALL CalcH( 172, "ethene",                                          "74-85-1",  1.14  ,  0.97  , -0.17, "1500")
    CALL CalcH( 173, "propene",                                        "115-07-1",  1.08  ,  0.97  , -0.11, "1500")
    CALL CalcH( 174, "1-butene",                                       "106-98-9",  1.08  ,  1.01  , -0.07, "1500")
    CALL CalcH( 175, "1-pentene",                                      "109-67-1",  1.20  ,  1.23  ,  0.03, "1500")
    CALL CalcH( 176, "1-hexene",                                       "592-41-6",  1.34  ,  1.16  , -0.18, "1500")
    CALL CalcH( 177, "1-heptene",                                      "592-76-7",  1.50  ,  1.22  , -0.28, "1500")
    CALL CalcH( 178, "1-octene",                                       "111-66-0",  1.64  ,  1.41  , -0.23, "1500")
    CALL CalcH( 179, "1-nonene",                                       "124-11-8",  1.79  ,  1.51  , -0.28, "1500")
    CALL CalcH( 180, "1-decene",                                       "872-05-9",  1.98  ,  2.04  ,  0.06, "2528")
    CALL CalcH( 181, "1-undecene",                                     "821-95-4",  2.26)
    CALL CalcH( 182, "1-dodecene",                                     "112-41-4",  2.42  ,  1.92  , -0.50, "2440")
    CALL CalcH( 183, "2-butene",                                       "107-01-7",  0.90  ,  0.96  ,  0.06, "791")
    CALL CalcH( 184, "2-pentene",                                      "109-68-2",  1.05  ,  0.96  , -0.09, "1501")
    CALL CalcH( 185, "2-heptene",                                      "592-77-8",  1.37  ,  1.23  , -0.14, "1501")
    CALL CalcH( 186, "1-propene, 2-methyl-",                           "115-11-7",  0.67  ,  0.94  ,  0.27, "1501")
    CALL CalcH( 187, "1-butene, 3-methyl-",                            "563-45-1",  1.44  ,  1.34  , -0.10, "1501")
    CALL CalcH( 188, "1-butene, 2,3-dimethyl-",                        "563-78-0",  1.37  ,  1.29  , -0.08, "?")
    CALL CalcH( 189, "2-butene, 2-methyl-",                            "513-35-9",  0.73  ,  0.96  ,  0.23, "1501")
    CALL CalcH( 190, "1-pentene, 2-methyl-",                           "763-29-1",  1.26  ,  1.08  , -0.18, "1501")
    CALL CalcH( 191, "1-pentene, 4-methyl-",                           "691-37-2",  1.54  ,  1.41  , -0.13, "642")
    CALL CalcH( 192, "1,3-butadiene",                                  "106-99-0",  0.35  ,  0.41  ,  0.06, "190")
    CALL CalcH( 193, "1,2-butadiene",                                  "590-19-2",  0.58  ,  0.54  , -0.04, "2440")
    CALL CalcH( 194, "1,3-butadiene, 2-methyl",                         "78-79-5",  0.18  ,  0.5   ,  0.32, "1501")
    CALL CalcH( 195, "1,3-butadiene, 2,3-dimethyl",                    "513-81-5",  0.33  ,  0.29  , -0.04, "1501")
    CALL CalcH( 196, "1,2-pentadiene",                                 "591-95-7",  0.62  ,  0.59  , -0.03, "2440")
    CALL CalcH( 197, "2,3-pentadiene",                                 "591-96-8",  0.57  ,  0.59  ,  0.02, "2440")
    CALL CalcH( 198, "1,4-pentadiene",                                 "591-93-5",  0.61  ,  0.68  ,  0.07, "642")
    CALL CalcH( 199, "1,5-hexadiene",                                  "592-42-7",  0.84  ,  0.74  , -0.10, "479")
    CALL CalcH( 200, "1,6-heptadiene",                                "3070-53-9",  0.94  ,  0.44  , -0.5 , "wrongref")
    CALL CalcH( 201, "acetylene",                                       "74-86-2", -0.54  , -0.01  ,  0.53, "2440")
    CALL CalcH( 202, "propyne",                                         "74-99-7", -0.17  , -0.35  , -0.18, "1500")
    CALL CalcH( 203, "1-butyne",                                       "107-00-6",  0.05  , -0.1   , -0.15, "789")
    CALL CalcH( 204, "1-pentyne",                                      "627-19-0",  0.23  ,  0.01  , -0.22, "1500")
    CALL CalcH( 205, "1-hexyne",                                       "693-02-7",  0.38  ,  0.21  , -0.17, "1500")
    CALL CalcH( 206, "1-heptyne",                                      "628-71-7",  0.55  ,  0.44  , -0.11, "1500")
    CALL CalcH( 207, "1-octyne",                                       "629-05-0",  0.80  ,  0.52  , -0.28, "1500")
    CALL CalcH( 208, "1-nonyne",                                      "3452-09-3",  0.96  ,  0.77  , -0.19, "1500")
    CALL CalcH( 209, "2-butyne",                                       "503-17-3", -0.67  , -0.03  ,  0.64, "789")
    CALL CalcH( 210, "2-pentyne",                                      "627-21-4", -0.42  ,  0.05  ,  0.47, "789")
    CALL CalcH( 211, "2-hexyne",                                       "764-35-2", -0.16  ,  0.24  ,  0.40, "789")
    CALL CalcH( 212, "3-hexyne",                                       "928-49-4", -0.17  ,  0.21  ,  0.38, "789")
    CALL CalcH( 213, "2-octyne",                                      "2809-67-8",  0.27)
    CALL CalcH( 214, "vinyl acetylene",                                "689-97-4", -0.42  ,  0.03  ,  0.45, "190")
    CALL CalcH( 215, "biacetylene",                                    "460-12-8", -1.33  , -0.67  ,  0.66, "789")
    CALL CalcH( 216, "cyclopropane",                                    "75-19-4",  0.45  ,  0.55  ,  0.10, "1500")
    CALL CalcH( 217, "cyclopentane",                                   "287-92-3",  0.57  ,  0.88  ,  0.31, "1500")
    CALL CalcH( 218, "cyclohexane",                                    "110-82-7",  0.63  ,  0.9   ,  0.27, "1500")
    CALL CalcH( 219, "cycloheptane",                                   "291-64-5",  0.90  ,  0.6   , -0.30, "2528")
    CALL CalcH( 220, "cyclooctane",                                    "292-64-8",  0.73)
    CALL CalcH( 221, "cyclopentane, methyl-",                           "96-37-7",  0.96  ,  1.18  ,  0.22, "2528")
    CALL CalcH( 222, "cyclopentane, propyl-",                         "2040-96-2",  1.31  ,  1.56  ,  0.25, "1501")
    CALL CalcH( 223, "cyclopentane, pentyl-",                         "3741-00-2",  1.64  ,  1.87  ,  0.23, "1501")
    CALL CalcH( 224, "cyclopentane, 1,1,2-trimethyl",                 "4259-00-1",  1.77  ,  1.8   ,  0.03, "479")
    CALL CalcH( 225, "cyclohexane, methyl-",                           "108-87-2",  1.06  ,  1.24  ,  0.18, "642")
    CALL CalcH( 226, "cyclohexane, ethyl-",                           "1678-91-7",  1.24  ,  1.29  ,  0.05, "1501")
    CALL CalcH( 227, "cyclohexane, 1,2-dimethyl-",                     "583-57-3",  1.46  ,  1.16  , -0.30, "642")
    CALL CalcH( 228, "cyclohexane, 1,4-dimethyl-",                     "589-90-2",  1.44  ,  1.55  ,  0.11, "1910")
    CALL CalcH( 229, "cyclohexane, cyclohexane-",                       "92-51-3",  1.12)
    CALL CalcH( 230, "cycloheptane, methyl-",                         "4126-78-7",  1.28)
    CALL CalcH( 231, "hydrindane",                                     "496-10-6",  0.66)
    CALL CalcH( 232, "adamantane",                                     "281-23-2",  0.58)
    CALL CalcH( 233, "decalin",                                         "91-17-8",  0.79  ,  0.75  , -0.04, "1156")
    CALL CalcH( 234, "cyclopentene",                                   "142-29-0",  0.12  ,  0.41  ,  0.29, "1501")
    CALL CalcH( 235, "cyclohexene",                                    "110-83-8",  0.21  ,  0.27  ,  0.06, "1501")
    CALL CalcH( 236, "cycloheptene",                                   "628-92-2",  0.50)
    CALL CalcH( 237, "cyclopentene, 1-methyl",                         "693-89-0",  0.23)
    CALL CalcH( 238, "cyclohexene, 1-methyl",                          "591-49-1",  0.32  ,  0.49  ,  0.17, "1501")
    CALL CalcH( 239, "cyclohexene, vinyl",                            "2622-21-1", -0.28  ,  0.29  ,  0.57, "2440")
    CALL CalcH( 240, "cyclohexene, 4-ethenyl-",                        "100-40-3",  0.35  ,  0.28  , -0.07, "2530")
    CALL CalcH( 241, "limonene",                                       "138-86-3",  0.58  ,  0.05  , -0.53, "2440")
    CALL CalcH( 242, "alpha-pinene",                                    "80-56-8",  1.12  ,  1.06  , -0.06, "C")
    CALL CalcH( 243, "1,3-cyclopentadiene",                            "542-92-7", -0.48)
    CALL CalcH( 244, "1,3-cyclohexadien",                              "592-57-4", -0.45)
    CALL CalcH( 245, "1,4-cyclohexadien",                              "628-41-1", -0.30  , -0.41  , -0.11, "C")
    CALL CalcH( 246, "1,3-cycloheptadien",                            "4054-38-0", -0.19)
    CALL CalcH( 247, "1,5-cyclooctadien",                              "111-78-4",  0.03)
    CALL CalcH( 248, "1,3,5-cycloheptatriene",                         "544-25-2", -0.97  , -0.73  ,  0.24, "1501")
    CALL CalcH( 249, "1,3,5,7-cyclooctatriene",                        "629-20-9", -1.95  , -1.67  ,  0.28, "1501")
    CALL CalcH( 250, "dicyclopentadiene",                               "77-73-6",  1.16)
    CALL CalcH( 251, "diphenylmethane",                                "101-81-5", -1.73  , -2.04  , -0.31, "1145")
    CALL CalcH( 252, "benzene",                                         "71-43-2", -0.63  , -0.63  ,  0.00, "1500")
    CALL CalcH( 253, "toluene",                                        "108-88-3", -0.57  , -0.65  , -0.08, "1500")
    CALL CalcH( 254, "styrene",                                        "100-42-5", -0.90  , -0.91  , -0.01, "1501")
    CALL CalcH( 255, "phenylacetylene",                                "536-74-3", -0.98  , -1.59  , -0.61, "2530")
    CALL CalcH( 256, "benzene, allyl-",                                "300-57-2", -0.73  , -0.55  ,  0.18, "984")
    CALL CalcH( 257, "alpha-methylstyrene",                             "98-83-9", -0.78  , -0.91  , -0.13, "1501")
    ! "vinyl toluene" is a mix of isomers and not used here:
    !CALL CalcH( 258, "vinyl toluene",                               "25013-15-4", -0.78  , -0.92  , -0.14, "2530")
    CALL CalcH( 259, "beta-methylstyrene",                             "873-66-5", -0.86)
    CALL CalcH( 260, "styrene, 3-methyl",                              "100-80-1", -0.89  , -0.8   ,  0.09, "789")
    CALL CalcH( 261, "styrene, 4-methyl",                              "622-97-9", -0.93  , -0.93  ,  0.00, "789")
    CALL CalcH( 262, "benzene, ethyl-",                                "100-41-4", -0.53  , -0.58  , -0.05, "1500")
    CALL CalcH( 263, "benzene, propyl-",                               "103-65-1", -0.39  , -0.39  ,  0.00, "1500")
    CALL CalcH( 264, "benzene, butyl-",                                "104-51-8", -0.28  , -0.29  , -0.01, "1500")
    CALL CalcH( 265, "benzene, pentyl-",                               "538-68-1", -0.18  , -0.17  ,  0.01, "1500")
    CALL CalcH( 266, "benzene, hexyl-",                               "1077-16-3", -0.08  , -0.03  ,  0.05, "1500")
    CALL CalcH( 267, "benzene, heptyl-",                              "1078-71-3",  0.01)
    CALL CalcH( 268, "benzene, octyl-",                               "2189-60-8",  0.10)
    CALL CalcH( 269, "benzene, 1,2-dimethyl-",                          "95-47-6", -0.70  , -0.66  ,  0.04, "1500")
    CALL CalcH( 270, "benzene, 1,3-dimethyl-",                         "108-38-3", -0.56  , -0.61  , -0.05, "1500")
    CALL CalcH( 271, "benzene, 1,4-dimethyl-",                         "106-42-3", -0.58  , -0.59  , -0.01, "1500")
    CALL CalcH( 272, "benzene, 1,3,5-trimethyl-",                      "108-67-8", -0.54  , -0.66  , -0.12, "1500")
    CALL CalcH( 273, "benzene, 1,2,4-trimethyl-",                       "95-63-6", -0.71  , -0.63  ,  0.08, "1500")
    CALL CalcH( 274, "benzene, 1,2,3-trimethyl-",                      "526-73-8", -0.88  , -0.89  , -0.01, "1500")
    CALL CalcH( 275, "benzene, 1,2,4,5-tetramethyl-",                   "95-93-2", -0.86  , -0.99  , -0.13, "479")
    ! the diff calculation was rounded to one decimal, it should be 0.029 (Halil, pers. comm. 2010):
    CALL CalcH( 276, "benzene, 1,2-diethyl-",                          "135-01-3", -0.50  , -0.471 ,  0.02, "C")
    CALL CalcH( 277, "benzene, 1,3-diethyl-",                          "141-93-5", -0.38  , -0.97  , -0.59, "C")
    CALL CalcH( 278, "benzene, 1,4-diethyl-",                          "105-05-5", -0.42  , -0.69  , -0.27, "984")
    CALL CalcH( 279, "benzene, 1-ethyl-2-methyl-",                     "611-14-3", -0.64  , -0.76  , -0.12, "479")
    CALL CalcH( 280, "benzene, 1-ethyl-3-methyl",                      "620-14-4", -0.52)
    CALL CalcH( 281, "benzene, 1-ethyl-4-methyl",                      "622-96-8", -0.54  , -0.7   , -0.16, "479")
    CALL CalcH( 282, "benzene, isopropyl",                              "98-82-8", -0.33  , -0.22  ,  0.11, "1501")
    CALL CalcH( 283, "benzene, sec-butyl",                             "135-98-8", -0.33  , -0.33  ,  0.00, "1501")
    CALL CalcH( 284, "benzene, isobutyl",                              "538-93-2", -0.24  ,  0.12  ,  0.36, "1501")
    CALL CalcH( 285, "benzene, t-butyl",                                "98-06-6", -0.28  , -0.32  , -0.04, "1501")
    CALL CalcH( 286, "benzene, t-amyl",                               "2049-95-8", -0.39  , -0.13  ,  0.26, "642")
    CALL CalcH( 287, "benzene, 1-methyl-2-isopropyl",                  "527-84-4", -0.48  , -0.33  ,  0.15, "2531")
    CALL CalcH( 288, "benzene, 1-methyl-3-isopropyl",                  "535-77-3", -0.33  , -0.46  , -0.13, "2440")
    CALL CalcH( 289, "benzene, 1-methyl-4-isopropyl-",                  "99-87-6", -0.34  , -0.5   , -0.16, "479")
    CALL CalcH( 290, "benzene, pentamethyl-",                          "700-12-9", -1.28  , -1.72  , -0.44, "2528")
    CALL CalcH( 291, "benzene, hexamethyl-",                            "87-85-4", -1.33  , -1.48  , -0.15, "2528")
    CALL CalcH( 292, "biphenyl",                                        "92-52-4", -1.51  , -1.87  , -0.36, "1501")
    CALL CalcH( 293, "biphenyl, 2-methyl",                             "643-58-3", -1.41)
    CALL CalcH( 294, "biphenyl, 3-methyl",                             "643-93-6", -1.56)
    CALL CalcH( 295, "biphenyl, 4-methyl",                             "644-08-6", -1.60)
    CALL CalcH( 296, "naphthalene",                                     "91-20-3", -1.72  , -1.76  , -0.04, "1501")
    CALL CalcH( 297, "naphthalene, 1-methyl-",                          "90-12-0", -1.84  , -1.79  ,  0.05, "1501")
    CALL CalcH( 298, "naphthalene, 2-methyl-",                          "91-57-6", -1.81  , -1.78  ,  0.03, "1145")
    CALL CalcH( 299, "naphthalene, 1,3-dimethyl-",                     "575-41-7", -1.86  , -1.81  ,  0.05, "1910")
    CALL CalcH( 300, "naphthalene, 1,4-dimethyl-",                     "571-58-4", -2.04  , -2.07  , -0.03, "1910")
    CALL CalcH( 301, "naphthalene, 1,5-dimethyl-",                     "571-61-9", -1.91  , -1.85  ,  0.06, "1910")
    CALL CalcH( 302, "naphthalene, 2,3-dimethyl-",                     "581-40-8", -1.95  , -2.04  , -0.09, "1910")
    CALL CalcH( 303, "naphthalene, 2,6-dimethyl-",                     "581-42-0", -1.90  , -1.93  , -0.03, "1910")
    CALL CalcH( 304, "naphthalene, 1-ethyl-",                         "1127-76-0", -1.84  , -1.76  ,  0.08, "1910")
    CALL CalcH( 305, "naphthalene, 2-ethyl-",                          "939-27-5", -1.68  , -1.59  ,  0.09, "789")
    CALL CalcH( 306, "indane",                                         "496-11-7", -1.46  , -1.07  ,  0.39, "2530")
    CALL CalcH( 307, "tetralin",                                       "119-64-2", -1.47  , -1.11  ,  0.36, "1156")
    CALL CalcH( 308, "anthracene",                                     "120-12-7", -2.91  , -2.9   ,  0.01, "1501")
    CALL CalcH( 309, "anthracene, 9-methyl",                           "779-02-2", -3.02)
    CALL CalcH( 310, "pyrene",                                         "129-00-0", -2.76  , -3.32  , -0.56, "1501")
    CALL CalcH( 311, "fluoranthene",                                   "206-44-0", -3.04  , -3.42  , -0.38, "1146")
    CALL CalcH( 312, "9h-fluorene",                                     "86-73-7", -2.36  , -2.46  , -0.10, "1501")
    CALL CalcH( 313, "acenaphthene",                                    "83-32-9", -2.74  , -2.31  ,  0.43, "1501")
    CALL CalcH( 314, "triphenylene",                                   "217-59-4", -3.85  , -4.13  , -0.28, "2532")
    CALL CalcH( 315, "acenaphthylene",                                 "208-96-8", -2.42  , -2.33  ,  0.09, "479")
    CALL CalcH( 316, "phenanthrene",                                    "85-01-8", -2.81  , -2.85  , -0.04, "1501")
    CALL CalcH( 317, "phenanthrene, 1-methyl-",                        "832-69-9", -2.91  , -2.68  ,  0.23, "2530")
    CALL CalcH( 318, "9,10-dihydro-phenanthrene",                      "776-35-2", -2.01  , -2.4   , -0.39, "2489")
    CALL CalcH( 319, "benz[a]anthracene",                               "56-55-3", -4.04  , -3.49  ,  0.55, "479")
    CALL CalcH( 320, "benzo[a]pyrene",                                  "50-32-8", -3.86  , -4.01  , -0.15, "1501")
    CALL CalcH( 321, "benzo[a]fluoranthene",                           "203-33-8", -4.23  , -4.62  , -0.39, "1146")
    CALL CalcH( 322, "benzo[k]fluoranthene",                           "207-08-9", -4.30  , -4.53  , -0.23, "1146")
    CALL CalcH( 323, "benzo[b]fluoranthene",                           "205-99-2", -4.14  , -4.48  , -0.34, "1146")
    CALL CalcH( 324, "benzo(ghi)perylene",                             "191-24-2", -3.81  , -4.78  , -0.97, "1146")
    CALL CalcH( 325, "dibenz[a,c]anthracene",                          "215-58-7", -4.68  , -5.43  , -0.75, "2532")
    CALL CalcH( 326, "dibenz[a,h]anthracene",                           "53-70-3", -4.47  , -5.22  , -0.75, "2532")
    CALL CalcH( 327, "dibenz[a,j]anthracene",                          "224-41-9", -5.33  , -5.21  ,  0.12, "2532")
    CALL CalcH( 328, "picene",                                         "213-46-7", -4.19  , -5.17  , -0.98, "2532")
    CALL CalcH( 329, "chrysene",                                       "218-01-9", -3.95  , -4.2   , -0.25, "1501")
    CALL CalcH( 330, "preylene",                                       "198-55-0", -3.76  , -4.28  , -0.52, "2532")
    CALL CalcH( 331, "indeno [1,2,3-cd] pyrene",                       "193-39-5", -4.09  , -4.85  , -0.76, "1146")
    CALL CalcH( 332, "methylamine",                                     "74-89-5", -3.49  , -3.34  ,  0.15, "1500")
    CALL CalcH( 333, "ethylamine",                                      "75-04-7", -3.29  , -3.3   , -0.01, "1500")
    CALL CalcH( 334, "propylamine",                                    "107-10-8", -3.08  , -3.22  , -0.14, "1500")
    CALL CalcH( 335, "butylamine",                                     "109-73-9", -2.85  , -3.11  , -0.26, "1500")
    CALL CalcH( 336, "pentylamine",                                    "110-58-7", -2.61  , -2.99  , -0.38, "1500")
    CALL CalcH( 337, "hexylamine",                                     "111-26-2", -2.96  , -2.9   ,  0.06, "1500")
    CALL CalcH( 338, "heptylamine",                                    "111-68-2", -3.05  , -2.78  ,  0.27, "1500")
    CALL CalcH( 339, "octylamine",                                     "111-86-4", -3.03  , -2.68  ,  0.35, "1500")
    CALL CalcH( 340, "isopropanamine",                                  "75-31-0", -2.72  , -2.74  , -0.02, "C")
    CALL CalcH( 341, "isobutylamine",                                   "78-81-9", -2.78  , -3.25  , -0.47, "C")
    CALL CalcH( 342, "sec-butylamine",                               "13952-84-6", -2.61  , -2.21  ,  0.40, "C")
    CALL CalcH( 343, "t-butylamine",                                    "75-64-9", -2.09  , -2.84  , -0.75, "C")
    CALL CalcH( 344, "isopentylamine",                                 "107-85-7", -2.74)
    CALL CalcH( 345, "1-hexanamine, 2-ethyl-",                         "104-75-6", -2.96  , -2.39  ,  0.57, "2530")
    CALL CalcH( 346, "dimethylamine",                                  "124-40-3", -3.17  , -3.15  ,  0.02, "1500")
    CALL CalcH( 347, "diethylamine",                                   "109-89-7", -2.64  , -2.99  , -0.35, "1500")
    CALL CalcH( 348, "dipropylamine",                                  "142-84-7", -2.45  , -2.68  , -0.23, "1500")
    CALL CalcH( 349, "di-isopropylamine",                              "108-18-9", -2.19  , -2.36  , -0.17, "1500")
    CALL CalcH( 350, "dibutylamine",                                   "111-92-2", -2.77  , -2.38  ,  0.39, "1500")
    CALL CalcH( 351, "n-methylpropylamine",                            "627-35-0", -2.67)
    CALL CalcH( 352, "n-methylisopropylamine",                        "4747-21-1", -2.53)
    CALL CalcH( 353, "n-methylbutylamine",                             "110-68-9", -2.43)
    CALL CalcH( 354, "trimethylamine",                                  "75-50-3", -1.96  , -2.35  , -0.39, "1500")
    CALL CalcH( 355, "triethylamine",                                  "121-44-8", -2.33  , -2.36  , -0.03, "1500")
    CALL CalcH( 356, "tripropylamine",                                 "102-69-2", -2.22  , -1.81  ,  0.41, "C")
    CALL CalcH( 357, "pyrrolidine",                                    "123-75-1", -4.17  , -4.01  ,  0.16, "1910")
    CALL CalcH( 358, "pyrrolidine, n-methyl-",                         "120-94-5", -2.74  , -2.91  , -0.17, "1910")
    CALL CalcH( 359, "cyclohexanamine, n,n-dimethyl-",                  "98-94-2", -3.10  , -3.    ,  0.10, "2530")
    CALL CalcH( 360, "cyclohexanamine",                                "108-91-8", -3.22  , -3.37  , -0.15, "1195")
    CALL CalcH( 361, "cyclohexanamine, 3-methyl",                     "6850-35-7", -3.42)
    CALL CalcH( 362, "1h-azepine, hexahydro-",                         "111-49-9", -4.20  , -3.6   ,  0.60, "1910")
    CALL CalcH( 363, "pyrrole",                                        "109-97-7", -3.25  , -3.43  , -0.18, "2232")
    CALL CalcH( 364, "indole",                                         "120-72-9", -4.35  , -4.56  , -0.21, "howard97")
    CALL CalcH( 365, "pyrrole, n-methyl",                               "96-54-8", -1.35)
    CALL CalcH( 366, "piperidine",                                     "110-89-4", -4.26  , -3.74  ,  0.52, "1910")
    CALL CalcH( 367, "piperidine, n-methyl-",                          "626-67-5", -3.08  , -2.77  ,  0.31, "1501")
    CALL CalcH( 368, "piperidine, n-ethyl-",                           "766-09-6", -2.98)
    CALL CalcH( 369, "ethylenediamine",                                "107-15-3", -7.14  , -7.1   ,  0.04, "789")
    CALL CalcH( 370, "imidazol, n-methyl",                             "616-47-7", -5.45)
    CALL CalcH( 371, "hydrazine, phenyl-",                             "100-63-0", -6.23  , -6.73  , -0.50, "2530")
    CALL CalcH( 372, "allylamine",                                     "107-11-9", -3.77  , -3.13  ,  0.64, "C")
    CALL CalcH( 373, "hexamethylenetetramine",                         "100-97-0", -7.51  , -7.16  ,  0.35, "2528")
    CALL CalcH( 374, "aniline",                                         "62-53-3", -4.10  , -4.03  ,  0.07, "1500")
    CALL CalcH( 375, "aniline, n-methyl-",                             "100-61-8", -3.57  , -3.44  ,  0.13, "984")
    CALL CalcH( 376, "aniline, n-ethyl-",                              "103-69-5", -3.24  , -3.38  , -0.14, "2530")
    CALL CalcH( 377, "aniline, n,n-dimethyl-",                         "121-69-7", -2.39  , -2.53  , -0.14, "1500")
    CALL CalcH( 378, "aniline, n,n-diethyl-",                           "91-66-7", -2.39  , -2.09  ,  0.30, "2530")
    CALL CalcH( 379, "aniline , n,n,4-trimethyl-",                      "99-97-8", -2.54  , -2.68  , -0.14, "2530")
    CALL CalcH( 380, "aniline, 2-methyl",                               "95-53-4", -3.88  , -4.06  , -0.18, "984")
    CALL CalcH( 381, "aniline,3-methyl",                               "108-44-1", -4.08  , -4.14  , -0.06, "1195")
    CALL CalcH( 382, "aniline, 4-methyl-",                             "106-49-0", -4.12  , -4.04  ,  0.08, "984")
    CALL CalcH( 383, "aniline, 3,4-dimethyl-",                          "95-64-7", -4.22  , -4.12  ,  0.10, "2525")
    CALL CalcH( 384, "aniline, 2,6-dimethyl-",                          "87-62-7", -3.91  , -3.82  ,  0.09, "984")
    CALL CalcH( 385, "aniline, 2,6-diethyl-",                          "579-66-8", -3.35  , -4.32  , -0.97, "2530")
    CALL CalcH( 386, "aniline, 2,4,5-trimethyl-",                      "137-17-7", -4.17  , -3.98  ,  0.19, "2530")
    CALL CalcH( 387, "aniline, n-phenyl",                              "122-39-4", -3.87  , -3.9   , -0.03, "1145")
    CALL CalcH( 388, "aniline, 2-amino",                                "95-54-5", -6.46  , -6.47  , -0.01, "2528")
    CALL CalcH( 389, "aniline, 3-amino",                               "108-45-2", -8.44  , -8.39  ,  0.05, "2530")
    CALL CalcH( 390, "1-naphthalenamine",                              "134-32-7", -4.87  , -5.34  , -0.47, "984")
    CALL CalcH( 391, "2-naphthalenamine",                               "91-59-8", -5.30  , -5.48  , -0.18, "984")
    CALL CalcH( 392, "pyridine",                                       "110-86-1", -3.27  , -3.44  , -0.17, "1500")
    CALL CalcH( 393, "pyridine, 2-methyl-",                            "109-06-8", -3.01  , -3.4   , -0.39, "1500")
    CALL CalcH( 394, "pyridine, 3-methyl-",                            "108-99-6", -3.34  , -3.5   , -0.16, "1500")
    CALL CalcH( 395, "pyridine, 4-methyl-",                            "108-89-4", -3.35  , -3.62  , -0.27, "1500")
    CALL CalcH( 396, "pyridine, 2-ethyl-",                             "100-71-0", -2.86  , -3.18  , -0.32, "1500")
    CALL CalcH( 397, "pyridine, 3-ethyl-",                             "536-78-7", -3.22  , -3.37  , -0.15, "1500")
    CALL CalcH( 398, "pyridine, 4-ethyl",                              "536-75-4", -3.24  , -3.47  , -0.23, "1500")
    CALL CalcH( 399, "pyridine, 2,3-dimethyl-",                        "583-61-9", -3.19  , -3.54  , -0.35, "1500")
    CALL CalcH( 400, "pyridine, 2,4-dimethyl-",                        "108-47-4", -3.10  , -3.57  , -0.47, "1500")
    CALL CalcH( 401, "pyridine, 2,5-dimethyl-",                        "589-93-5", -3.15  , -3.46  , -0.31, "1500")
    CALL CalcH( 402, "pyridine, 2,6-dimethyl-",                        "108-48-5", -3.05  , -3.37  , -0.32, "1500")
    CALL CalcH( 403, "pyridine, 3,4-dimethyl-",                        "583-58-4", -3.52  , -3.83  , -0.31, "1500")
    CALL CalcH( 404, "pyridine, 3,5-dimethyl-",                        "591-22-0", -3.38  , -3.55  , -0.17, "1500")
    CALL CalcH( 405, "pyridine, 2,4, 6-trimethyl-",                    "108-75-8", -3.13  , -3.44  , -0.31, "C")
    CALL CalcH( 406, "pyridine, 5-ethyl-2-methyl-",                    "104-90-5", -3.04  , -3.09  , -0.05, "2530")
    CALL CalcH( 407, "pyridine, 4-t-butyl",                           "3978-81-2", -2.98  , -3.27  , -0.29, "1500")
    CALL CalcH( 408, "pyrazine, 2-methyl-",                            "109-08-0", -4.08  , -4.04  ,  0.04, "984")
    CALL CalcH( 409, "pyrazine, 2-ethyl-",                           "13925-00-3", -3.82  , -4.01  , -0.19, "984")
    CALL CalcH( 410, "pyrazine, 2-isobutyl-",                        "29460-93-3", -3.60  , -3.68  , -0.08, "2530")
    CALL CalcH( 411, "pyrimidine",                                     "289-95-2", -4.40)
    CALL CalcH( 412, "quinoline",                                       "91-22-5", -4.20  , -4.2   ,  0.00, "1501")
    CALL CalcH( 413, "isoquinoline",                                   "119-65-3", -4.36  , -3.94  ,  0.42, "2440")
    CALL CalcH( 414, "hydrogen cyanide",                                "74-90-8", -1.98  , -2.27  , -0.29, "715")
    ! the diff calculation was written incorrectly, it should be 0.22 (Halil, pers. comm. 2010):
    CALL CalcH( 415, "acetonitrile",                                    "75-05-8", -3.28  , -3.06  ,  0.27, "789")
    CALL CalcH( 416, "propanenitrile",                                 "107-12-0", -3.09  , -2.82  ,  0.27, "1500")
    CALL CalcH( 417, "butanenitrile",                                  "109-74-0", -2.94  , -2.67  ,  0.27, "1500")
    CALL CalcH( 418, "pentanenitrile",                                 "110-59-8", -2.82  , -2.58  ,  0.24, "1500")
    CALL CalcH( 419, "hexanenitrile",                                  "628-73-9", -2.75)
    CALL CalcH( 420, "heptanenitrile",                                 "629-08-3", -2.60)
    CALL CalcH( 421, "octanenitrile",                                  "124-12-9", -2.50)
    CALL CalcH( 422, "nonanenitrile",                                 "2243-27-8", -2.41)
    CALL CalcH( 423, "decanenitrile",                                 "1975-78-6", -2.30)
    CALL CalcH( 424, "undecanenitrile",                               "2244-07-7", -2.18)
    CALL CalcH( 425, "propanenitrile, 2-methyl-",                       "78-82-0", -2.67)
    CALL CalcH( 426, "cyanogen",                                       "460-19-5", -0.81  , -0.67  ,  0.14, "789")
    CALL CalcH( 427, "adiponitrile",                                   "111-69-3", -6.73  , -7.29  , -0.56, "2528")
    CALL CalcH( 428, "2-propenenitrile",                               "107-13-1", -1.74  , -2.25  , -0.51, "3074")
    CALL CalcH( 429, "2-propenenitrile, 2-methyl-",                    "126-98-7", -1.62  , -1.98  , -0.36, "2530")
    CALL CalcH( 430, "caynocyclohexane",                               "766-05-2", -3.26)
    CALL CalcH( 431, "acetonitrile, phenyl",                           "140-29-4", -4.40)
    CALL CalcH( 432, "benzonitrile",                                   "100-47-0", -2.78  , -3.01  , -0.23, "1500")
    CALL CalcH( 433, "methane, nitro-",                                 "75-52-5", -2.93  , -2.95  , -0.02, "1500")
    CALL CalcH( 434, "ethane, nitro-",                                  "79-24-3", -2.74  , -2.7   ,  0.04, "1500")
    CALL CalcH( 435, "propane, 1-nitro-",                              "108-03-2", -2.57  , -2.45  ,  0.12, "1500")
    CALL CalcH( 436, "butane, 1-nitro-",                               "627-05-4", -2.38  , -2.27  ,  0.11, "1500")
    CALL CalcH( 437, "pentane, 1-nitro-",                              "628-05-7", -2.17  , -2.07  ,  0.10, "1500")
    CALL CalcH( 438, "hexane, 1-nitro-",                               "646-14-0", -2.05)
    CALL CalcH( 439, "propane, 2-nitro-",                               "79-46-9", -2.25  , -2.3   , -0.05, "1500")
    CALL CalcH( 440, "cyclohexane, 1-nitro-",                         "1122-60-7", -2.78)
    CALL CalcH( 441, "nitrobenzene",                                    "98-95-3", -2.74  , -3.02  , -0.28, "C")
    CALL CalcH( 442, "nitrobenzene, 2-methyl",                          "88-72-2", -2.54  , -3.27  , -0.73, "1195")
    CALL CalcH( 443, "nitrobenzene, 3-methyl",                          "99-08-1", -2.65  , -3.4   , -0.75, "1195")
    CALL CalcH( 444, "nitrobenzene, 4-methyl",                          "99-99-0", -2.70  , -3.6   , -0.90, "1195")
    CALL CalcH( 445, "formamide, n-methyl-",                           "123-39-7", -6.14  , -6.08  ,  0.06, "2530")
    CALL CalcH( 446, "formamide, n,n-dimethyl-",                        "68-12-2", -5.05  , -5.73  , -0.68, "1501")
    CALL CalcH( 447, "acetamide",                                       "60-35-5", -7.02  , -6.73  ,  0.29, "2530")
    CALL CalcH( 448, "acetamide, n,n-dimethyl-",                       "127-19-5", -5.62  , -6.25  , -0.63, "2531")
    CALL CalcH( 449, "acetamide, n-butyl-",                           "1119-49-9", -6.11  , -6.83  , -0.72, "1501")
    CALL CalcH( 450, "urethane",                                        "51-79-6", -4.42  , -5.56  , -1.14, "2530")
    CALL CalcH( 451, "acrylamide",                                      "79-06-1", -7.01  , -7.37  , -0.36, "2530")
    CALL CalcH( 452, "acetamide, 2-chloro-n,n-di-2-propenyl-",          "93-71-0", -5.38  , -5.34  ,  0.04, "2530")
    CALL CalcH( 453, "acetamide, 2,2-dichloro-n,n-di-2-propenyl-",   "37764-25-3", -4.89  , -4.85  ,  0.04, "2528")
    CALL CalcH( 454, "benzamide",                                       "55-21-0", -6.97  , -8.07  , -1.10, "1501")
    CALL CalcH( 455, "methyl nitrate",                                 "598-58-3", -2.19  , -1.69  ,  0.50, "541")
    CALL CalcH( 456, "ethyl nitrate",                                  "625-58-1", -1.99  , -1.59  ,  0.40, "541")
    CALL CalcH( 457, "propyl nitrate",                                 "627-13-4", -1.79  , -1.43  ,  0.36, "541")
    CALL CalcH( 458, "isopropyl nitrate",                             "1712-64-7", -1.63  , -1.29  ,  0.34, "541")
    CALL CalcH( 459, "butyl nitrate",                                  "928-45-0", -1.63  , -1.18  ,  0.45, "541")
    CALL CalcH( 460, "iso-butyl nitrate",                              "543-29-3", -1.59  , -1.02  ,  0.57, "541")
    CALL CalcH( 461, "t-butyl nitrate",                                "540-80-7", -1.29  , -1.24  ,  0.05, "541")
    CALL CalcH( 462, "isoamyl nitrate",                                "543-87-3", -1.49  , -1.04  ,  0.45, "541")
    CALL CalcH( 463, "pentyl nitrate",                                "1002-16-0", -1.50  , -1.17  ,  0.33, "1024")
    CALL CalcH( 464, "2-pentyl nitrate",                             "21981-48-6", -1.37  , -1.08  ,  0.29, "1024")
    CALL CalcH( 465, "3-pentyl nitrate",                             "82944-59-0", -1.36  , -0.93  ,  0.43, "1024")
    CALL CalcH( 466, "hexyl nitrate",                                "20633-11-8", -1.37  , -1.22  ,  0.15, "1024")
    CALL CalcH( 467, "1,2-ethane dinitrate",                           "628-96-6", -4.31  , -4.19  ,  0.12, "541")
    CALL CalcH( 468, "1,2-propane dinitrate",                         "6423-43-4", -3.82  , -3.62  ,  0.20, "541")
    CALL CalcH( 469, "1,3-propane dinitrate",                         "3457-90-7", -4.04  , -3.52  ,  0.52, "541")
    CALL CalcH( 470, "1,4-butane dinitrate",                          "3457-91-8", -3.83  , -3.59  ,  0.24, "541")
    CALL CalcH( 471, "n-nitrosodiethylamine-",                          "55-18-5", -3.99  , -3.83  ,  0.16, "3082")
    CALL CalcH( 472, "n-nitrosodimethylamine-",                         "62-75-9", -4.37  , -4.1   ,  0.27, "3082")
    CALL CalcH( 473, "n-nitrosodipropylamine-",                        "621-64-7", -4.16  , -3.66  ,  0.50, "3082")
    CALL CalcH( 474, "n-nitroso-di-iso-propylamine-",                  "601-77-4", -2.93  , -3.5   , -0.57, "3082")
    CALL CalcH( 475, "n-nitrosodibutylamine",                          "924-16-3", -3.27  , -3.27  ,  0.00, "3082")
    CALL CalcH( 476, "morpholine, n-nitroso-",                          "59-89-2", -6.35  , -6.09  ,  0.26, "3082")
    CALL CalcH( 477, "pyrrolidine, 1-nitroso-",                        "930-55-2", -4.93  , -5.7   , -0.77, "3082")
    CALL CalcH( 478, "benzenemethanamine, n-methyl-n-nitroso",         "937-40-6", -5.46  , -3.29  ,  2.17, "3082")
    CALL CalcH( 479, "piperidine, 1-nitroso-",                         "100-75-4", -4.85  , -4.46  ,  0.39, "3082")
    CALL CalcH( 480, "formaldehyde",                                    "50-00-0", -1.84  , -2.02  , -0.18, "1500")
    CALL CalcH( 481, "acetaldehyde",                                    "75-07-0", -2.44  , -2.57  , -0.13, "1500")
    CALL CalcH( 482, "propanal",                                       "123-38-6", -2.47  , -2.52  , -0.05, "1500")
    CALL CalcH( 483, "butanal",                                        "123-72-8", -2.35  , -2.33  ,  0.02, "1500")
    CALL CalcH( 484, "pentanal",                                       "110-62-3", -2.25  , -2.22  ,  0.03, "1500")
    CALL CalcH( 485, "hexanal",                                         "66-25-1", -2.16  , -2.06  ,  0.10, "1500")
    CALL CalcH( 486, "heptanal",                                       "111-71-7", -1.96  , -1.96  ,  0.00, "1500")
    CALL CalcH( 487, "octanal",                                        "124-13-0", -1.98  , -1.68  ,  0.30, "1500")
    CALL CalcH( 488, "nonanal",                                        "124-19-6", -1.77  , -1.52  ,  0.25, "1500")
    CALL CalcH( 489, "decanal",                                        "112-31-2", -1.81  , -1.18  ,  0.63, "?")
    CALL CalcH( 490, "propanal, 2-methyl-",                             "78-84-2", -2.24  , -2.1   ,  0.14, "1500")
    CALL CalcH( 491, "butanal, 3-methyl-",                             "590-86-3", -2.26  , -1.76  ,  0.50, "2530")
    CALL CalcH( 492, "hexanal, 2-ethyl-",                              "123-05-7", -1.83  , -1.49  ,  0.34, "2530")
    CALL CalcH( 493, "2-propenal",                                     "107-02-8", -2.37  , -2.3   ,  0.07, "1910")
    CALL CalcH( 494, "2-propenal, 2-methyl",                            "78-85-3", -2.37  , -2.02  ,  0.35, "985")
    CALL CalcH( 495, "2-butenal",                                     "4170-30-3", -2.38  , -3.1   , -0.72, "1500")
    CALL CalcH( 496, "2-hexenal",                                      "505-57-7", -2.19  , -2.7   , -0.51, "1500")
    CALL CalcH( 497, "2-heptenal",                                    "2463-63-0", -2.09)
    CALL CalcH( 498, "2-octenal",                                     "2363-89-5", -2.01  , -2.52  , -0.51, "1500")
    ! 499 is same species as 500:
    !CALL CalcH( 499, "2,4-hexadienal",                                "4488-48-6", -2.98  , -3.4   , -0.42, "1497")
    CALL CalcH( 500, "2,4-hexadienal, (e,e)-",                         "142-83-6", -2.98  , -3.38  , -0.40, "2530")
    CALL CalcH( 501, "acetone",                                         "67-64-1", -2.55  , -2.8   , -0.25, "1500")
    CALL CalcH( 502, "2-butanone",                                      "78-93-3", -2.52  , -2.72  , -0.20, "1500")
    CALL CalcH( 503, "2-pentanone",                                    "107-87-9", -2.41  , -2.58  , -0.17, "1500")
    CALL CalcH( 504, "2-hexanone",                                     "591-78-6", -2.31  , -2.41  , -0.10, "1500")
    CALL CalcH( 505, "2-heptanone",                                    "110-43-0", -2.19  , -2.23  , -0.04, "1500")
    CALL CalcH( 506, "2-octanone",                                     "111-13-7", -2.10  , -2.11  , -0.01, "1500")
    CALL CalcH( 507, "2-nonanone",                                     "821-55-6", -2.01  , -1.83  ,  0.18, "1500")
    CALL CalcH( 508, "2-decanone",                                     "693-54-9", -1.92  , -1.72  ,  0.20, "1500")
    CALL CalcH( 509, "2-undecanone",                                   "112-12-9", -1.83  , -1.58  ,  0.25, "1500")
    CALL CalcH( 510, "3-pentanone",                                     "96-22-0", -2.36  , -2.5   , -0.14, "1500")
    CALL CalcH( 511, "3-hexanone",                                     "589-38-8", -2.23  , -2.29  , -0.06, "1500")
    CALL CalcH( 512, "4-heptanone",                                    "123-19-3", -2.08  , -2.14  , -0.06, "1500")
    CALL CalcH( 513, "4-octanone",                                     "589-63-9", -1.95  , -1.92  ,  0.03, "1500")
    CALL CalcH( 514, "5-nonanone",                                     "502-56-7", -1.82  , -1.94  , -0.12, "1500")
    CALL CalcH( 515, "6-undecanone",                                   "927-49-1", -1.58  , -2.03  , -0.45, "2530")
    CALL CalcH( 516, "ethanone, 1-cyclopropyl-",                       "765-43-5", -3.08  , -3.38  , -0.30, "642")
    CALL CalcH( 517, "ethanone, 1-cyclohexane",                        "823-76-7", -3.01  , -2.86  ,  0.15, "642")
    CALL CalcH( 518, "1-propanone, 1-phenyl-",                          "93-55-0", -3.33  , -3.19  ,  0.14, "2530")
    CALL CalcH( 519, "2-butanone, 3-methyl-",                          "563-80-4", -2.32  , -2.38  , -0.06, "1500")
    CALL CalcH( 520, "2-butanone, 3,3-dimethyl-",                       "75-97-8", -2.07  , -2.28  , -0.21, "642")
    CALL CalcH( 521, "2-butanone, 3-ene",                               "78-94-4", -2.65  , -2.71  , -0.06, "985")
    CALL CalcH( 522, "3-pentanone, 2-methyl-",                         "565-69-5", -2.21  , -2.18  ,  0.03, "2530")
    CALL CalcH( 523, "2-pentanone, 3-methyl-",                         "565-61-7", -2.26  , -2.51  , -0.25, "2530")
    CALL CalcH( 524, "2-pentanone, 4-methyl",                          "108-10-1", -2.34  , -2.24  ,  0.10, "1500")
    CALL CalcH( 525, "2-hexanone, 5-methyl-",                          "110-12-3", -2.28  , -2.21  ,  0.07, "2530")
    CALL CalcH( 526, "3-pentanone, 2,4-dimethyl-",                     "565-80-0", -1.94  , -2.01  , -0.07, "642")
    CALL CalcH( 527, "3-penten-2-one, 4-methyl-",                      "141-79-7", -2.65  , -2.66  , -0.01, "2440")
    CALL CalcH( 528, "4-heptanone, 2,6-dimethyl-",                     "108-83-8", -1.89  , -2.3   , -0.41, "2530")
    CALL CalcH( 529, "cyclopentanone",                                 "120-92-3", -3.45  , -3.45  ,  0.00, "1500")
    CALL CalcH( 530, "cyclohexanone",                                  "108-94-1", -3.40  , -3.6   , -0.20, "1500")
    CALL CalcH( 531, "cycloheptanone",                                 "502-42-1", -3.24)
    CALL CalcH( 532, "2,3-butanedione",                                "431-03-8", -3.97  , -3.67  ,  0.30, "gaffney84")
    CALL CalcH( 533, "acetylacetone",                                  "123-54-6", -4.62  , -3.85  ,  0.77, "2530")
    CALL CalcH( 534, "isophorone",                                      "78-59-1", -3.23  , -3.55  , -0.32, "2530")
    CALL CalcH( 535, "2-pentanone, 4-methoxy-4-methyl-",               "107-70-0", -3.65  , -4.08  , -0.43, "2530")
    CALL CalcH( 536, "dimethyl phthalate",                             "131-11-3", -5.62  , -4.86  ,  0.76, "usepa82")
    CALL CalcH( 537, "diethyl phthalate",                               "84-66-2", -5.28  , -4.47  ,  0.81, "usepa82")
    CALL CalcH( 538, "dibutyl phthalate",                               "84-74-2", -4.86  , -4.14  ,  0.72, "2533")
    CALL CalcH( 539, "di-2-ethylhexyl adipate",                        "103-23-1", -3.03  , -3.22  , -0.19, "1145")
    CALL CalcH( 540, "benzaldehyde",                                   "100-52-7", -3.28  , -2.95  ,  0.33, "984")
    CALL CalcH( 541, "benzaldehyde, 4-methyl-",                        "104-87-0", -3.29  , -3.13  ,  0.16, "984")
    CALL CalcH( 542, "benzaldehyde, 2-hydroxy-",                        "90-02-8", -4.61  , -3.62  ,  0.99, "2440")
    CALL CalcH( 543, "benzaldehyde, 3-hydroxy-",                       "100-83-4", -7.12  , -6.97  ,  0.15, "1500")
    CALL CalcH( 544, "benzaldehyde, 4-hydroxy-",                       "123-08-0", -6.34  , -6.48  , -0.14, "1500")
    CALL CalcH( 545, "acetophenone",                                    "98-86-2", -3.45  , -3.36  ,  0.09, "1500")
    CALL CalcH( 546, "acetophenone, 4-methyl",                         "122-00-9", -3.47  , -3.45  ,  0.02, "1501")
    CALL CalcH( 547, "acetophenone, 4-methoxy",                        "100-06-1", -4.23  , -3.23  ,  1.00, "984")
    CALL CalcH( 548, "formic acid, methyl ester",                      "107-31-3", -2.16  , -2.04  ,  0.12, "1500")
    CALL CalcH( 549, "formic acid, ethyl ester",                       "109-94-4", -1.89  , -1.88  ,  0.01, "1500")
    CALL CalcH( 550, "formic acid, 1-isopropyl ester",                 "625-55-8", -1.72  , -1.48  ,  0.24, "1500")
    CALL CalcH( 551, "formic acid, propyl ester",                      "110-74-7", -1.75  , -1.82  , -0.07, "1500")
    CALL CalcH( 552, "formic acid, 2-isobutyl ester",                  "542-55-2", -1.70  , -1.63  ,  0.07, "1500")
    CALL CalcH( 553, "formic acid, 3-isoamyl ester",                   "110-45-2", -1.63  , -1.56  ,  0.07, "1500")
    CALL CalcH( 554, "formic acid, pentyl ester",                      "638-49-3", -1.51  , -1.26  ,  0.25, "642")
    CALL CalcH( 555, "formic acid, hexyl ester",                       "629-33-4", -1.42  , -1.08  ,  0.34, "642")
    CALL CalcH( 556, "acetic acid, methyl ester",                       "79-20-9", -2.20  , -2.3   , -0.10, "1500")
    CALL CalcH( 557, "acetic acid, ethyl ester",                       "141-78-6", -1.95  , -2.16  , -0.21, "1500")
    CALL CalcH( 558, "acetic acid, isopropyl ester",                   "108-21-4", -1.80  , -1.94  , -0.14, "1500")
    CALL CalcH( 559, "acetic acid, propyl ester",                      "109-60-4", -1.86  , -2.05  , -0.19, "1500")
    CALL CalcH( 560, "acetic acid, isobutyl ester",                    "110-19-0", -1.83  , -1.73  ,  0.10, "1500")
    CALL CalcH( 561, "acetic acid, butyl ester",                       "123-86-4", -1.76  , -1.94  , -0.18, "1500")
    CALL CalcH( 562, "acetic acid, isoamyl ester",                     "123-92-2", -1.81  , -1.62  ,  0.19, "1500")
    CALL CalcH( 563, "acetic acid, pentyl ester",                      "628-63-7", -1.70  , -1.84  , -0.14, "1500")
    CALL CalcH( 564, "acetic acid, hexyl ester",                       "142-92-7", -1.54  , -1.66  , -0.12, "1500")
    CALL CalcH( 565, "acetic acid, vinyl ester",                       "108-05-4", -2.23  , -1.62  ,  0.61, "usepa82")
    CALL CalcH( 566, "acetic acid, allyl ester",                       "591-87-7", -2.24)
    CALL CalcH( 567, "propanoic acid, methyl ester",                   "554-12-1", -1.98  , -2.15  , -0.17, "1500")
    CALL CalcH( 568, "propanoic acid, ethyl ester",                    "105-37-3", -1.81  , -1.97  , -0.16, "1500")
    CALL CalcH( 569, "propanoic acid, propyl ester",                   "106-36-5", -1.69  , -1.79  , -0.10, "1500")
    CALL CalcH( 570, "propanoic acid, isopropyl ester",                "637-78-5", -1.63  , -1.61  ,  0.02, "2530")
    CALL CalcH( 571, "propanoic acid, pentyl ester",                   "624-54-4", -1.48  , -1.55  , -0.07, "1500")
    CALL CalcH( 572, "propanoic acid, 2,2-di-me-me ester",             "598-98-1", -1.62  , -1.76  , -0.14, "642")
    CALL CalcH( 573, "propanoic acid, 2-mepropyl ester",               "540-42-1", -1.65  , -1.17  ,  0.48, "642")
    CALL CalcH( 574, "propanoic acid, 2-mel-2-mep ester",               "97-85-8", -1.49  , -1.24  ,  0.25, "1500")
    CALL CalcH( 575, "butanoic acid, methyl ester",                    "623-42-7", -1.84  , -2.08  , -0.24, "1500")
    CALL CalcH( 576, "butanoic acid, ethyl ester",                     "105-54-4", -1.70  , -1.83  , -0.13, "1500")
    CALL CalcH( 577, "butanoic acid, propyl ester",                    "105-66-8", -1.55  , -1.67  , -0.12, "1500")
    CALL CalcH( 578, "butanoic acid, 2-methylpropyl ester",            "539-90-2", -1.51  , -1.24  ,  0.27, "1500")
    CALL CalcH( 579, "2-propenoic acid, methyl ester",                  "96-33-3", -2.13  , -2.09  ,  0.04, "2440")
    CALL CalcH( 580, "2-propenoic acid, ethyl ester",                  "140-88-5", -1.94  , -1.96  , -0.02, "2440")
    CALL CalcH( 581, "methacrylic acid, iso-butyl ester",               "97-86-9", -1.71  , -1.65  ,  0.06, "2530")
    CALL CalcH( 582, "2-ethylhexyl acrylate",                          "103-11-7", -1.49  , -1.74  , -0.25, "2530")
    CALL CalcH( 583, "ethyl methacrylate",                              "97-63-2", -1.86  , -1.6   ,  0.26, "C")
    CALL CalcH( 584, "butyl methacrylate",                              "97-88-1", -1.64  , -1.68  , -0.04, "2530")
    CALL CalcH( 585, "4-methyl-2-pentyl acetate",                      "108-84-9", -1.42  , -1.44  , -0.02, "2530")
    CALL CalcH( 586, "acrylate, butyl",                                "141-32-2", -1.70  , -1.6   ,  0.10, "2440")
    CALL CalcH( 587, "acrylate, isobutyl",                             "106-63-8", -1.77  , -1.53  ,  0.24, "2440")
    CALL CalcH( 588, "ethanol, 2-ethoxy-acetate",                      "111-15-9", -3.67  , -4.21  , -0.54, "2440")
    CALL CalcH( 589, "isobutanoic, ethyl ester",                        "97-62-1", -1.69  , -1.63  ,  0.06, "1500")
    CALL CalcH( 590, "pentanoic acid, methyl ester",                   "624-24-8", -1.74  , -1.88  , -0.14, "754")
    CALL CalcH( 591, "pentanoic acid, ethyl ester",                    "539-82-2", -1.58  , -1.85  , -0.27, "642")
    CALL CalcH( 592, "hexanoic acid, methyl ester",                    "106-70-7", -1.65  , -1.83  , -0.18, "1500")
    CALL CalcH( 593, "hexanoic acid, ethyl ester",                     "123-66-0", -1.45  , -1.64  , -0.19, "1500")
    CALL CalcH( 594, "heptanoic acid, ethyl ester",                    "106-30-9", -1.36  , -1.7   , -0.34, "2482")
    CALL CalcH( 595, "octanoic acid, methyl ester",                    "111-11-5", -1.46  , -1.5   , -0.04, "642")
    CALL CalcH( 596, "decanoic acid, methyl ester",                    "110-42-9", -1.28  , -1.54  , -0.26, "977")
    CALL CalcH( 597, "dodecanoic acid, methyl ester",                  "111-82-0", -1.08  , -1.31  , -0.23, "977")
    CALL CalcH( 598, "dodecanoic acid, ethyl ester",                   "106-33-2", -0.88  , -1.28  , -0.40, "977")
    CALL CalcH( 599, "dodecanoic acid, propyl ester",                 "3681-78-5", -0.72  , -1.28  , -0.56, "977")
    CALL CalcH( 600, "dodecanoic acid, butyl ester",                   "106-18-3", -0.58  , -1.25  , -0.67, "977")
    CALL CalcH( 601, "tetradecanoic acid, methyl ester",               "124-10-7", -0.88  , -1.1   , -0.22, "977")
    CALL CalcH( 602, "hexadecanoic acid, methyl ester",                "112-39-0", -0.66  , -0.87  , -0.21, "977")
    CALL CalcH( 603, "octadecanoic acid, methyl ester",                "112-61-8", -0.42  , -0.62  , -0.20, "977")
    CALL CalcH( 604, "dodecanoic acid, 2-ethylhexyl ester",          "20292-08-4", -0.33  , -0.88  , -0.55, "977")
    CALL CalcH( 605, "ethanedioic acid, dimethyl ester",               "553-90-2", -4.23  , -3.9   ,  0.33, "2530")
    CALL CalcH( 606, "ethylacetoacetate",                              "607-97-6", -3.92  , -4.32  , -0.40, "2440")
    CALL CalcH( 607, "methyl methacrylate",                             "80-62-6", -2.04  , -1.84  ,  0.20, "2530")
    CALL CalcH( 608, "malonate, diethyl",                              "105-53-3", -3.99  , -3.98  ,  0.01, "2440")
    CALL CalcH( 609, "methyl maleate",                                 "624-48-6", -4.76  , -4.52  ,  0.24, "2530")
    CALL CalcH( 610, "methyl linolenate",                              "301-00-8", -1.25  , -2.83  , -1.58, "977")
    CALL CalcH( 611, "methyl linolate",                                "112-63-0", -1.08  , -2.19  , -1.11, "977")
    CALL CalcH( 612, "methy oleate",                                   "112-62-9", -0.79  , -1.5   , -0.71, "977")
    CALL CalcH( 613, "methyl erucate",                                "1120-34-9", -0.31  , -1.11  , -0.80, "977")
    CALL CalcH( 614, "diethyl carbonate",                              "105-58-8", -2.23  , -2.42  , -0.19, "2440")
    CALL CalcH( 615, "butanoic acid, 3-oxo-methyl ester",              "105-45-3", -4.63  , -4.93  , -0.30, "2530")
    CALL CalcH( 616, "butanoic acid, 3-oxo-ethyl ester",               "141-97-9", -4.43  , -4.29  ,  0.14, "2530")
    CALL CalcH( 617, "ethylene glycol diacetate",                      "111-55-7", -4.50  , -5.33  , -0.83, "2440")
    CALL CalcH( 618, "diethyl pimelate",                              "2050-20-6", -3.57  , -4.72  , -1.15, "2530")
    CALL CalcH( 619, "diethyl succinate",                              "123-25-1", -4.00  , -4.65  , -0.65, "2530")
    CALL CalcH( 620, "ethanol, 2-(2-butoxyethoxy)- acetate",           "124-17-4", -5.01  , -5.15  , -0.14, "2530")
    CALL CalcH( 621, "benzoic acid, methyl ester",                      "93-58-3", -2.85  , -3.14  , -0.29, "1500")
    CALL CalcH( 622, "benzoic acid, ethyl ester",                       "93-89-0", -2.67  , -2.67  ,  0.00, "1500")
    CALL CalcH( 623, "butanoic acid, 3-methyl-, ethyl ester",          "108-64-5", -1.61  , -1.52  ,  0.09, "2530")
    CALL CalcH( 624, "dimethyl ether",                                 "115-10-6", -1.66  , -1.39  ,  0.27, "1500")
    CALL CalcH( 625, "diethyl ether",                                   "60-29-7", -1.24  , -1.17  ,  0.07, "1501")
    CALL CalcH( 626, "dipropyl ether",                                 "111-43-3", -1.17  , -1.03  ,  0.14, "2528")
    CALL CalcH( 627, "di-isopropyl ether",                             "108-20-3", -0.96  , -1.01  , -0.05, "2528")
    CALL CalcH( 628, "di-butyl ether",                                 "142-96-1", -0.89  , -0.61  ,  0.28, "642")
    CALL CalcH( 629, "di-isobutyl ether",                              "628-55-7", -0.96)
    CALL CalcH( 630, "di-pentyl ether",                                "693-65-2", -0.83)
    CALL CalcH( 631, "di-isopentyl ether",                             "544-01-4", -0.91  , -1.21  , -0.30, "2530")
    CALL CalcH( 632, "di-hexyl ether",                                 "112-58-3", -0.66)
    CALL CalcH( 633, "methane, t-butyl ether",                        "1634-04-4", -0.98  , -1.62  , -0.64, "1501")
    CALL CalcH( 634, "methane, propyl ether-",                         "557-17-5", -1.43  , -1.22  ,  0.21, "642")
    CALL CalcH( 635, "methane, isopropyl ether-",                      "598-53-8", -1.31  , -1.47  , -0.16, "642")
    CALL CalcH( 636, "methane, dimethoxy",                             "109-87-5", -2.76  , -2.15  ,  0.61, "2482")
    CALL CalcH( 637, "ethane, 1,2-dimethoxy",                          "110-71-4", -3.12)
    CALL CalcH( 638, "methane, bis (2-chloro ethoxy)",                 "111-91-1", -3.92  , -4.34  , -0.42, "usepa82")
    CALL CalcH( 639, "bis (2-chloro ethyl) ether",                     "111-44-4", -2.85  , -3.06  , -0.21, "usepa82")
    CALL CalcH( 640, "bis (2-chloro isopropyl) ether",                 "108-60-1", -2.25  , -2.21  ,  0.04, "usepa82")
    CALL CalcH( 641, "ethane, methoxy-",                               "540-67-0", -1.56  , -1.54  ,  0.02, "1501")
    CALL CalcH( 642, "ethane, propyl ether-",                          "628-32-0", -1.29  , -1.28  ,  0.01, "howard97")
    CALL CalcH( 643, "butane, 2-ethoxy-2-methyl-",                     "919-94-8", -0.78  , -0.09  ,  0.69, "2530")
    CALL CalcH( 644, "butane, 2-methoxy-",                            "6795-87-5", -1.19  , -1.2   , -0.01, "2530")
    CALL CalcH( 645, "vinylisobutyl ether",                            "109-53-5",  0.33  , -0.8   , -1.13, "2530")
    CALL CalcH( 646, "divinyl ether",                                  "109-93-3",  0.03  , -0.15  , -0.18, "984")
    CALL CalcH( 647, "ethane, 1,2-diethoxy-",                          "629-14-1", -2.98  , -2.47  ,  0.51, "howard97")
    CALL CalcH( 648, "ethane, 1,1-diethoxy-",                          "105-57-7", -2.15  , -2.39  , -0.24, "2440")
    CALL CalcH( 649, "2-butoxyethyl acetate",                          "112-07-2", -3.50  , -3.49  ,  0.01, "2530")
    CALL CalcH( 650, "1,2-dibutoxyethane",                             "112-48-1", -2.55  , -3.21  , -0.66, "2530")
    CALL CalcH( 651, "propylene oxide",                                 "75-56-9", -1.63  , -2.11  , -0.48, "usepa82")
    CALL CalcH( 652, "1,3-dioxolane",                                  "646-06-0", -3.56  , -2.98  ,  0.58, "2530")
    CALL CalcH( 653, "1,4-dioxane",                                    "123-91-1", -3.91  , -3.71  ,  0.20, "1501")
    CALL CalcH( 654, "1,3-dioxane",                                    "505-22-6", -3.71  , -3.02  ,  0.69, "1910")
    CALL CalcH( 655, "ethylene oxide",                                  "75-21-8", -1.99  , -2.22  , -0.23, "2534")
    CALL CalcH( 656, "furan, tetrahydro",                              "109-99-9", -2.44  , -2.55  , -0.11, "1500")
    CALL CalcH( 657, "furan, tetrahydro-2-methyl-",                     "96-47-9", -2.18  , -2.42  , -0.24, "1910")
    CALL CalcH( 658, "furan, tetrahydro-2,5-dimethyl-",               "1003-38-9", -1.88  , -2.14  , -0.26, "1910")
    CALL CalcH( 659, "furan",                                          "110-00-9", -0.76  , -0.64  ,  0.12, "789")
    CALL CalcH( 660, "benzofuran",                                     "271-89-6", -1.67)
    CALL CalcH( 661, "furfural",                                        "98-01-1", -4.17  , -3.84  ,  0.33, "2440")
    CALL CalcH( 662, "furfuryl alcohol",                                "98-00-0", -4.92  , -5.39  , -0.47, "2440")
    CALL CalcH( 663, "2h-pyran, tetrahydro-",                          "142-68-7", -2.45  , -2.29  ,  0.16, "1500")
    CALL CalcH( 664, "oxirane, phenyl-",                                "96-09-3", -2.79  , -3.19  , -0.40, "1145")
    CALL CalcH( 665, "methyl cyclohexyl ether",                        "931-56-6", -1.88)
    CALL CalcH( 666, "anisole",                                        "100-66-3", -1.35  , -1.8   , -0.45, "1500")
    CALL CalcH( 667, "diphenyl ether",                                 "101-84-8", -1.63  , -1.92  , -0.29, "2530")
    CALL CalcH( 668, "anisole, p-propenyl",                            "104-46-1", -1.70  , -2.37  , -0.67, "2440")
    CALL CalcH( 669, "benzene, ethoxy-",                               "103-73-1", -1.21  , -1.63  , -0.42, "1500")
    CALL CalcH( 670, "diethyleneglycol, dibutyl ether",                "112-73-2", -3.94  , -3.96  , -0.02, "2440")
    CALL CalcH( 671, "methyleugenol",                                   "93-15-2", -3.95  , -3.62  ,  0.33, "2530")
    CALL CalcH( 672, "formic acid",                                     "64-18-6", -5.76  , -5.34  ,  0.42, "511")
    CALL CalcH( 673, "acetic acid",                                     "64-19-7", -5.50  , -5.11  ,  0.39, "511")
    CALL CalcH( 674, "propanoic acid",                                  "79-09-4", -5.24  , -4.73  ,  0.51, "642")
    CALL CalcH( 675, "butanoic acid",                                  "107-92-6", -5.04  , -4.66  ,  0.38, "1500")
    CALL CalcH( 676, "pentanoic acid",                                 "109-52-4", -4.91  , -4.52  ,  0.39, "1500")
    CALL CalcH( 677, "hexanoic acid",                                  "142-62-1", -4.78  , -4.56  ,  0.22, "1500")
    CALL CalcH( 678, "heptanoic acid",                                 "111-14-8", -4.63  , -4.52  ,  0.11, "1500")
    CALL CalcH( 679, "octanoic acid",                                  "124-07-2", -4.51  , -4.44  ,  0.07, "1500")
    CALL CalcH( 680, "nonanoic acid",                                  "112-05-0", -4.39  , -4.23  ,  0.16, "C")
    CALL CalcH( 681, "decanoic acid",                                  "334-48-5", -4.28  , -4.21  ,  0.07, "C")
    CALL CalcH( 682, "undecanoic acid",                                "112-37-8", -4.16)
    CALL CalcH( 683, "dodecanoic acid",                                "143-07-7", -4.05)
    CALL CalcH( 684, "octadecanoic acid",                               "57-11-4", -3.32  , -4.69  , -1.37, "2530")
    CALL CalcH( 685, "acetic acid, phenyl",                            "103-82-2", -6.39  , -5.76  ,  0.63, "2530")
    CALL CalcH( 686, "propanoic acid, 2-methyl",                        "79-31-2", -4.80  , -4.43  ,  0.37, "493")
    CALL CalcH( 687, "pentanoic acid, 2-methyl-",                       "97-61-0", -4.43  , -3.92  ,  0.51, "2530")
    CALL CalcH( 688, "butanoic acid, 2-methyl",                        "116-53-0", -4.61  , -4.47  ,  0.14, "1500")
    CALL CalcH( 689, "butanoic acid, 3-methyl",                        "503-74-2", -4.84  , -4.48  ,  0.36, "493")
    CALL CalcH( 690, "propanoic acid, 2,2-dimethyl",                    "75-98-9", -4.46  , -3.93  ,  0.53, "493")
    CALL CalcH( 691, "butanoic acid, 2-ethyl-",                         "88-09-5", -4.35  , -4.17  ,  0.18, "2530")
    CALL CalcH( 692, "hexanoic acid, 2-ethyl-",                        "149-57-5", -3.93  , -3.75  ,  0.18, "2530")
    CALL CalcH( 693, "propenoic acid",                                  "79-10-7", -4.74  , -4.77  , -0.03, "2440")
    CALL CalcH( 694, "propenoic acid, 2-methyl",                        "79-41-4", -4.67  , -4.8   , -0.13, "493")
    CALL CalcH( 695, "oxalic acid",                                    "144-62-7", -6.77  , -8.22  , -1.45, "2530")
    CALL CalcH( 696, "pentanedioic acid",                              "110-94-1", -10.74 , -9.67  ,  1.07, "2530")
    CALL CalcH( 697, "pyruvic acid",                                   "127-17-3", -6.81  , -6.87  , -0.06, "493")
    CALL CalcH( 698, "hexanedioic acid",                               "124-04-9", -10.79 , -9.7   ,  1.09, "2530")
    CALL CalcH( 699, "peroxyacetic acid",                               "79-21-0", -4.64  , -4.22  ,  0.42, "311")
    CALL CalcH( 700, "crotonic acid",                                 "3724-65-0", -4.75  , -4.74  ,  0.01, "2530")
    CALL CalcH( 701, "isocrotonic acid",                               "503-64-0", -4.75  , -5.55  , -0.80, "2530")
    CALL CalcH( 702, "benzoic acid",                                    "65-85-0", -5.77  , -5.63  ,  0.14, "1145")
   !CALL CalcH( 703, "water",                                         "7732-18-5", -4.67  , -4.6   ,  0.07, "1500")
    CALL CalcH( 704, "hydrogen peroxide",                             "7722-84-1", -5.20  , -6.2   , -1.00, "488")
    CALL CalcH( 705, "peroxide, methyl",                              "3031-73-0", -3.35  , -3.73  , -0.38, "516")
    CALL CalcH( 706, "peroxide, ethyl",                               "3031-74-1", -3.16  , -3.89  , -0.73, "516")
    CALL CalcH( 707, "peroxide, t-butyl",                              "110-05-4",  0.51  ,  0.31  , -0.20, "2530")
    CALL CalcH( 708, "cumene hydroperoxide",                            "80-15-9", -3.76  , -5.04  , -1.28, "2530")
    CALL CalcH( 709, "methanol",                                        "67-56-1", -3.69  , -3.74  , -0.05, "1500")
    CALL CalcH( 710, "ethanol",                                         "64-17-5", -3.43  , -3.67  , -0.24, "1500")
    CALL CalcH( 711, "1-propanol",                                      "71-23-8", -3.24  , -3.56  , -0.32, "1500")
    CALL CalcH( 712, "1-butanol",                                       "71-36-3", -3.14  , -3.46  , -0.32, "1500")
    CALL CalcH( 713, "1-pentanol",                                      "71-41-0", -3.05  , -3.35  , -0.30, "1500")
    CALL CalcH( 714, "1-hexanol",                                      "111-27-3", -2.96  , -3.23  , -0.27, "1500")
    CALL CalcH( 715, "1-heptanol",                                     "111-70-6", -2.87  , -3.09  , -0.22, "1500")
    CALL CalcH( 716, "1-octanol",                                      "111-87-5", -2.79  , -3.    , -0.21, "1500")
    CALL CalcH( 717, "1-nonanol",                                      "143-08-8", -2.73  , -2.85  , -0.12, "1500")
    CALL CalcH( 718, "1-decanol",                                      "112-30-1", -2.70  , -2.67  ,  0.03, "1500")
    CALL CalcH( 719, "1-undecanol",                                    "112-42-5", -2.46)
    CALL CalcH( 720, "1-dodecanol",                                    "112-53-8", -2.57  , -2.43  ,  0.14, "?") !mydiff
    CALL CalcH( 721, "1-hexadecanol",                                  "124-29-8", -1.99  , -2.51  , -0.52, "2530")
    CALL CalcH( 722, "1-tetradecanol",                                 "112-72-1", -2.19  , -2.17  ,  0.02, "2530")
    CALL CalcH( 723, "1-octadecanol",                                  "112-92-5", -1.79  , -1.44  ,  0.35, "2530")
    CALL CalcH( 724, "2-propanol",                                      "67-63-0", -3.03  , -3.48  , -0.45, "1500")
    CALL CalcH( 725, "2-butanol",                                       "78-92-2", -2.98  , -3.39  , -0.41, "1500")
    CALL CalcH( 726, "2-pentanol",                                    "6032-29-7", -2.89  , -3.22  , -0.33, "1500")
    CALL CalcH( 727, "2-hexanol",                                      "626-93-7", -2.79  , -2.98  , -0.19, "?") !mydiff
    CALL CalcH( 728, "2-heptanol",                                     "543-49-7", -2.70  , -2.62  ,  0.08, "?") !mydiff
    CALL CalcH( 729, "2-octanol",                                      "123-96-6", -2.62  , -2.82  , -0.20, "3074")
    CALL CalcH( 730, "3-pentanol",                                     "584-02-1", -2.90  , -2.86  ,  0.04, "1500")
    CALL CalcH( 731, "3-hexanol",                                      "623-37-0", -2.84  , -2.98  , -0.14, "1500")
    CALL CalcH( 732, "t-butanol",                                       "75-65-0", -2.74  , -3.28  , -0.54, "1195")
    CALL CalcH( 733, "cyclopentanol",                                   "96-41-3", -3.70  , -4.03  , -0.33, "1500")
    CALL CalcH( 734, "cyclohexanol",                                   "108-93-0", -3.91  , -4.01  , -0.10, "1500")
    CALL CalcH( 735, "cycloheptanol",                                  "502-41-0", -3.40  , -4.02  , -0.62, "1500")
    CALL CalcH( 736, "cyclododecanol",                                "1724-39-6", -3.61  , -3.9   , -0.29, "2530")
    CALL CalcH( 737, "ethanol, 2-phenyl",                               "60-12-8", -4.68  , -4.98  , -0.30, "642")
    CALL CalcH( 738, "ethanol, 2-(phenylmethoxy)-",                    "622-08-2", -5.57  , -5.29  ,  0.28, "2530")
    CALL CalcH( 739, "ethanol, 2-phenoxy",                             "122-99-6", -4.92  , -5.7   , -0.78, "2530")
    CALL CalcH( 740, "ethanol, 2-methoxy-",                            "109-86-4", -4.71  , -4.96  , -0.25, "1500")
    CALL CalcH( 741, "ethanol, 2-ethoxy",                              "110-80-5", -4.60  , -4.91  , -0.31, "1501")
    CALL CalcH( 742, "ethanol, 2-propoxy",                            "2807-30-9", -4.40  , -4.7   , -0.30, "1501")
    CALL CalcH( 743, "ethanol, 2-butoxy",                              "111-76-2", -4.28  , -4.59  , -0.31, "1500")
    CALL CalcH( 744, "propanol, 2-methyl-",                             "78-83-1", -3.10  , -3.3   , -0.20, "1500")
    CALL CalcH( 745, "2-propanol, 1-methoxy",                          "107-98-2", -4.49  , -4.45  ,  0.04, "C")
    CALL CalcH( 746, "propanol, 3-phenyl",                             "122-97-4", -4.54  , -5.08  , -0.54, "1501")
    CALL CalcH( 747, "propanol, 2,2-dimethyl",                          "75-84-3", -2.89  , -3.09  , -0.20, "588")
    CALL CalcH( 748, "butanol, 2-methyl-",                             "137-32-6", -2.99  , -3.22  , -0.23, "2530")
    CALL CalcH( 749, "butanol, 2-methyl-, (s)",                       "1565-80-6", -2.99  , -3.24  , -0.25, "1500")
    CALL CalcH( 750, "butanol, 4-phenyl",                             "3360-41-6", -4.46)
    CALL CalcH( 751, "butanol, 3-methyl-",                             "123-51-3", -3.06  , -3.24  , -0.18, "1500")
    CALL CalcH( 752, "butanol, 2-ethyl-",                               "97-95-0", -3.07  , -2.66  ,  0.41, "2530")
    CALL CalcH( 753, "2-butanol, 2-methyl-",                            "75-85-4", -2.83  , -3.25  , -0.42, "1500")
    CALL CalcH( 754, "2-butanol, 3-methyl-",                           "598-75-4", -2.88  , -3.13  , -0.25, "2530")
    CALL CalcH( 755, "2-butanol, 2,3-methyl-",                         "594-60-5", -2.69  , -2.87  , -0.18, "1500")
    CALL CalcH( 756, "pentanol, 2-methyl",                             "105-30-6", -3.04  , -2.74  ,  0.30, "2530")
    CALL CalcH( 757, "pentanol, 3-methyl-",                            "589-35-5", -2.97  , -3.19  , -0.22, "1500")
    CALL CalcH( 758, "2-pentanol, 2-methyl",                           "590-36-3", -2.90  , -2.88  ,  0.02, "1500")
    CALL CalcH( 759, "3-pentanol, 2-methyl",                           "565-67-3", -2.91  , -2.85  ,  0.06, "1500")
    CALL CalcH( 760, "3-pentanol, 3-methyl-",                           "77-74-7", -2.72  , -2.85  , -0.13, "1500")
    CALL CalcH( 761, "2-pentanol, 4-methyl-",                          "108-11-2", -2.81  , -2.74  ,  0.07, "1500")
    CALL CalcH( 762, "1-hexanol, 2-ethyl-",                            "104-76-7", -2.88  , -2.95  , -0.07, "2530")
    CALL CalcH( 763, "4-heptanol, 2,6-dimethyl-",                      "108-82-7", -2.63  , -2.26  ,  0.37, "2530")
    CALL CalcH( 764, "cyclohexanol, 2-methyl-",                        "583-59-5", -3.51  , -3.49  ,  0.02, "2530")
    CALL CalcH( 765, "ethylene glycol",                                "107-21-1", -6.25  , -7.11  , -0.86, "789")
    CALL CalcH( 766, "diethylene glycol",                              "111-46-6", -7.78  , -7.88  , -0.10, "789")
    CALL CalcH( 767, "propane, 1,3-diol",                              "504-63-2", -6.00  , -7.35  , -1.35, "641")
    CALL CalcH( 768, "butane, 1,4-diol",                               "110-63-4", -7.30  , -7.65  , -0.35, "789")
    CALL CalcH( 769, "pentane, 1,5-diol",                              "111-29-5", -7.28  , -7.99  , -0.71, "588")
    CALL CalcH( 770, "hexane, 1,3-diol, 2-ethyl-",                      "94-96-2", -5.44  , -6.23  , -0.79, "2530")
    CALL CalcH( 771, "pentanol, 4-methyl-3-oxa-",                      "109-59-1", -4.29  , -4.41  , -0.12, "2530")
    CALL CalcH( 772, "butyl lactate",                                  "138-22-7", -5.20  , -3.94  ,  1.26, "2528")
    CALL CalcH( 773, "allyl alcohol",                                  "107-18-6", -3.84  , -3.69  ,  0.15, "1500")
    CALL CalcH( 774, "2-propyn-1-ol",                                  "107-19-7", -4.13  , -4.31  , -0.18, "2530")
    CALL CalcH( 775, "3-butene-2-ol, 2-methyl",                        "115-18-4", -3.17  , -3.2   , -0.03, "1004")
    CALL CalcH( 776, "2-butene-1-ol",                                 "6117-91-5", -3.82  , -3.87  , -0.05, "1004")
    CALL CalcH( 777, "3-butyn-2-ol, 2-methyl-",                        "115-19-5", -3.40  , -3.78  , -0.38, "2530")
    CALL CalcH( 778, "pentynol, methyl",                                "77-75-8", -3.39  , -3.37  ,  0.02, "2530")
    CALL CalcH( 779, "2,6-dimethylocta-2,7-dien-6-ol",                  "78-70-6", -3.23  , -3.04  ,  0.19, "2530")
    CALL CalcH( 780, "benzyl alcohol",                                 "100-51-6", -4.73  , -4.86  , -0.13, "1910")
    CALL CalcH( 781, "phenol",                                         "108-95-2", -4.04  , -4.85  , -0.81, "1500")
    CALL CalcH( 782, "phenol, 2-methyl-",                               "95-48-7", -4.12  , -4.31  , -0.19, "1500")
    CALL CalcH( 783, "phenol, 3-methyl-",                              "108-39-4", -3.98  , -4.33  , -0.35, "1500")
    CALL CalcH( 784, "phenol, 4-methyl-",                              "106-44-5", -4.02  , -4.39  , -0.37, "1500")
    CALL CalcH( 785, "phenol, 3-ethyl-",                               "620-17-7", -3.93  , -4.59  , -0.66, "642")
    CALL CalcH( 786, "phenol, 4-ethyl-",                               "123-07-9", -3.97  , -4.5   , -0.53, "1501")
    CALL CalcH( 787, "phenol, 4-propyl-",                              "645-56-7", -3.88  , -4.33  , -0.45, "1501")
    CALL CalcH( 788, "phenol, 2-isopropyl-",                            "88-69-7", -3.84)
    CALL CalcH( 789, "phenol, 4-t-butyl-",                              "98-54-4", -3.71  , -4.34  , -0.63, "984")
    CALL CalcH( 790, "phenol, 2,3-dimethyl-",                          "526-75-0", -4.16  , -4.52  , -0.36, "1501")
    CALL CalcH( 791, "phenol, 2,4-dimethyl-",                          "105-67-9", -4.10  , -4.41  , -0.31, "1501")
    CALL CalcH( 792, "phenol, 2,5-dimethyl-",                           "95-87-4", -4.11  , -4.34  , -0.23, "1501")
    CALL CalcH( 793, "phenol, 2,6-dimethyl-",                          "576-26-1", -4.36  , -3.86  ,  0.50, "984")
    CALL CalcH( 794, "phenol, 3-methyl-5-ethyl",                       "698-71-5", -3.85  , -4.54  , -0.69, "2530")
    CALL CalcH( 795, "phenol, 2-isopropyl-5-methyl-",                   "89-83-8", -3.84  , -4.69  , -0.85, "2530")
    CALL CalcH( 796, "phenol, 3,4-dimethyl-",                           "95-65-8", -4.04  , -4.77  , -0.73, "984")
    CALL CalcH( 797, "phenol, 3,5-dimethyl-",                          "108-68-9", -3.90  , -4.6   , -0.70, "984")
    CALL CalcH( 798, "phenol, 2,3,6-trimethyl-",                      "2416-94-6", -4.42  , -3.78  ,  0.64, "2530")
    CALL CalcH( 799, "phenol, 2,4,6-trimethyl-",                       "527-60-6", -4.36  , -3.96  ,  0.40, "2530")
    CALL CalcH( 800, "phenol, 2-phenyl",                                "90-43-7", -4.89  , -4.35  ,  0.54, "2530")
    CALL CalcH( 801, "1-naphthalenol",                                  "90-15-3", -5.23  , -5.63  , -0.40, "984")
    CALL CalcH( 802, "2-naphthalenol",                                 "135-19-3", -5.24  , -5.95  , -0.71, "984")
    CALL CalcH( 803, "phenol, 1,2-dihydroxy",                          "120-80-9", -6.47  , -6.8   , -0.33, "636")
    CALL CalcH( 804, "phenol, 1,3-dihydroxy",                          "108-46-3", -8.12  , -8.2   , -0.08, "usepa82")
    CALL CalcH( 805, "phenol, 1,4-dihydroxy",                          "123-31-9", -7.96  , -8.68  , -0.72, "1145")
    CALL CalcH( 806, "phenol, 2-methoxy-",                              "90-05-1", -4.11  , -4.32  , -0.21, "1142")
    CALL CalcH( 807, "phenol, 3-methoxy",                              "150-19-6", -5.50  , -5.62  , -0.12, "1142")
    CALL CalcH( 808, "phenol, 2,6-dimethoxy-",                          "91-10-1", -5.94  , -5.01  ,  0.93, "2530")
    CALL CalcH( 809, "phenol, 2-methoxy-4-methyl-",                     "93-51-6", -4.11  , -4.25  , -0.14, "2530")
    CALL CalcH( 810, "methanethiol",                                    "74-93-1", -0.94  , -1.    , -0.06, "1500")
    CALL CalcH( 811, "ethanethiol",                                     "75-08-1", -0.99  , -0.84  ,  0.15, "1500")
    CALL CalcH( 812, "propanethiol",                                   "107-03-9", -0.92  , -0.78  ,  0.14, "1500")
    CALL CalcH( 813, "butanethiol",                                    "109-79-5", -0.83  , -0.73  ,  0.10, "1500")
    CALL CalcH( 814, "pentanethiol",                                   "110-66-7", -0.75)
    CALL CalcH( 815, "hexanethiol",                                    "111-31-9", -0.68)
    CALL CalcH( 816, "heptanethiol",                                  "1639-09-4", -0.82)
    CALL CalcH( 817, "octanethiol",                                    "111-88-6", -0.52)
    CALL CalcH( 818, "nonanethiol",                                   "1455-21-6", -0.46)
    CALL CalcH( 819, "decanethiol",                                    "143-10-2", -0.39  , -0.18  ,  0.21, "2440")
    CALL CalcH( 820, "2-propanethiol",                                  "75-33-2", -0.71)
    CALL CalcH( 821, "1-propanethiol, 2-methyl-",                      "513-44-0", -0.78)
    CALL CalcH( 822, "2-propanethiol, 2-methyl-",                       "75-66-1", -0.18)
    CALL CalcH( 823, "allylthiol",                                     "870-23-5", -1.46)
    CALL CalcH( 824, "diallyl sulfide",                                "592-88-1", -1.39)
    CALL CalcH( 825, "dimethyl sulfide",                                "75-18-3", -1.47  , -0.63  ,  0.84, "1500")
    CALL CalcH( 826, "ethane, methylthio-",                            "624-89-5", -1.33  , -1.1   ,  0.23, "984")
    CALL CalcH( 827, "dipropyl sulfide",                               "111-47-7", -0.92  , -0.94  , -0.02, "1500")
    CALL CalcH( 828, "diisopropyl sulfide",                            "625-80-9", -0.61  , -0.89  , -0.28, "1500")
    CALL CalcH( 829, "disulfide, dimethyl",                            "624-92-0", -1.87  , -1.35  ,  0.52, "1500")
    CALL CalcH( 830, "diethyl sulfide",                                "352-93-2", -1.17  , -1.07  ,  0.10, "1500")
    CALL CalcH( 831, "disulfide, diethyl",                             "110-81-6", -1.46  , -1.2   ,  0.26, "1500")
    CALL CalcH( 832, "thiophene",                                      "110-02-1", -0.55  , -1.04  , -0.49, "1500")
    CALL CalcH( 833, "thiophene, 2-methyl",                            "554-14-3", -0.55  , -1.01  , -0.46, "1500")
    CALL CalcH( 834, "thiophene, 3-methyl-",                           "616-44-4", -0.63  , -0.52  ,  0.11, "2530")
    CALL CalcH( 835, "benzenethiol",                                   "108-98-5", -2.01  , -1.87  ,  0.14, "1501")
    CALL CalcH( 836, "phenyl methyl sulfide",                          "100-68-5", -2.16  , -2.02  ,  0.14, "1501")
    CALL CalcH( 837, "methane, fluoro-",                               "593-53-3",  0.64  , -0.11  , -0.75, "479")
    CALL CalcH( 838, "ethane, fluoro-",                                "353-36-6", -0.08  , -0.03  ,  0.05, "789")
    CALL CalcH( 839, "propane fluoro-",                                "460-13-9", -0.15  , -0.18  , -0.03, "789")
    CALL CalcH( 840, "propane, 2-fluoro-",                             "420-26-8",  0.21  , -0.16  , -0.37, "789")
    CALL CalcH( 841, "hexane, fluoro-",                                "661-11-0",  0.18)
    CALL CalcH( 842, "octane, fluoro-",                                "463-11-6",  0.44)
    CALL CalcH( 843, "methane, difluoro-",                              "75-10-5", -0.32  , -0.33  , -0.01, "789")
    CALL CalcH( 844, "ethane, 1,1-difluoro-",                           "75-37-6",  0.15  , -0.08  , -0.23, "642")
    CALL CalcH( 845, "methane, trifluoro-",                             "75-46-7",  0.30  ,  0.59  ,  0.29, "2482")
    CALL CalcH( 846, "carbon tetrafluoride",                            "75-73-0",  1.64  ,  2.32  ,  0.68, "642")
    CALL CalcH( 847, "ethane, 1,1,1,2-tetrafluoro-",                   "811-97-2",  0.62  ,  0.35  , -0.27, "921")
    CALL CalcH( 848, "ethane, hexafluoro-",                             "76-16-4",  1.51  ,  2.92  ,  1.41, "642")
    CALL CalcH( 849, "propane, perfluoro-",                             "76-19-7",  1.57  ,  3.14  ,  1.57, "2528")
    CALL CalcH( 850, "perfluoropentane-",                              "678-26-2",  1.82)
    CALL CalcH( 851, "heptane, hexadecafluoro-",                       "335-57-9",  3.32)
    CALL CalcH( 852, "nonane, eicosafluoro-",                          "375-96-2",  4.95)
    CALL CalcH( 853, "ethene, 1,1-difluoro-",                           "75-38-7",  0.90  ,  1.22  ,  0.32, "2440")
    CALL CalcH( 854, "ethene, 2,2,2-trifluoroethoxy-",                 "406-90-6",  0.63  ,  0.1   , -0.53, "1500")
    CALL CalcH( 855, "ethene, tetrafluoro-",                           "116-14-3",  1.33  ,  1.41  ,  0.08, "789")
    CALL CalcH( 856, "propene, hexafluoro-",                           "116-15-4",  1.05  ,  2.15  ,  1.10, "190")
    CALL CalcH( 857, "cyclobutane, perfluoro-",                        "115-25-3",  1.64  ,  2.21  ,  0.57, "789")
    CALL CalcH( 858, "cyclohexane, fluoro-",                           "372-46-3", -0.52)
    CALL CalcH( 859, "benzene, trifluoromethyl",                        "98-08-8", -0.51  , -0.18  ,  0.33, "479")
    CALL CalcH( 860, "benzene, fluoro-",                               "462-06-6", -0.69  , -0.58  ,  0.11, "1500")
    CALL CalcH( 861, "benzene, 1,2-difluoro-",                         "367-11-3", -0.74  , -0.54  ,  0.20, "789")
    CALL CalcH( 862, "benzene, 1,3-difluoro-",                         "372-18-9", -0.52  , -0.5   ,  0.02, "789")
    CALL CalcH( 863, "benzene, 1,4-difluoro-",                         "540-36-3", -0.66  , -0.5   ,  0.16, "789")
    CALL CalcH( 864, "benzene, 1,2,3,5-tetrafluoro-",                 "2367-82-0", -0.09  , -0.08  ,  0.01, "2530")
    CALL CalcH( 865, "benzene, 1,2,4,5-tetrafluoro-",                  "327-54-8", -0.24  , -0.12  ,  0.12, "2530")
    CALL CalcH( 866, "methane, chloro-",                                "74-87-3", -0.41  , -0.4   ,  0.01, "1500")
    CALL CalcH( 867, "ethane, chloro",                                  "75-00-3", -0.49  , -0.46  ,  0.03, "1500")
    CALL CalcH( 868, "propane, 1-chloro-",                             "540-54-5", -0.42  , -0.24  ,  0.18, "1500")
    CALL CalcH( 869, "propane, 2-chloro-",                              "75-29-6", -0.17  , -0.18  , -0.01, "1500")
    CALL CalcH( 870, "propane, 2-chloro-2-methyl-",                    "507-20-0",  0.26  ,  0.8   ,  0.54, "1500")
    CALL CalcH( 871, "propane, 1-chloro-2-methyl-",                    "513-36-0", -0.26  , -0.2   ,  0.06, "789")
    CALL CalcH( 872, "butane, 1-chloro-",                              "109-69-3", -0.35  , -0.12  ,  0.23, "1500")
    CALL CalcH( 873, "butane, 2-chloro-",                               "78-86-4", -0.19  , -0.12  ,  0.07, "789")
    CALL CalcH( 874, "pentane, 1-chloro-",                             "543-59-9", -0.26  , -0.05  ,  0.21, "1500")
    CALL CalcH( 875, "pentane, 2-chloro-",                             "625-29-6", -0.08  ,  0.05  ,  0.13, "642")
    CALL CalcH( 876, "pentane, 3-chloro-",                             "616-20-6", -0.07  ,  0.03  ,  0.10, "642")
    CALL CalcH( 877, "hexane, 1-chloro-",                              "544-10-5", -0.18  ,  0.    ,  0.18, "1500")
    CALL CalcH( 878, "hexane, 2-chloro",                               "638-28-8", -0.02  , -0.09  , -0.07, "yaws2005")
    CALL CalcH( 879, "hexane, 3-chloro",                              "2346-81-8", -0.09  , -0.09  ,  0.00, "yaws2005")
    CALL CalcH( 880, "heptane, 1-chloro-",                             "629-06-1", -0.10  ,  0.21  ,  0.31, "1500")
    CALL CalcH( 881, "heptane, 3-chloromethyl-",                       "123-04-6", -0.05  , -0.89  , -0.84, "2530")
    CALL CalcH( 882, "heptane, 2-chloro",                             "1001-89-4",  0.08  ,  0.01  , -0.07, "yaws2005")
    CALL CalcH( 883, "heptane, 3-chloro",                              "999-52-0",  0.08  ,  0.05  , -0.03, "yaws2005")
    CALL CalcH( 884, "heptane, 4-chloro",                              "998-95-8",  0.06  ,  0.05  , -0.01, "yaws2005")
    CALL CalcH( 885, "octane, 1-chloro",                               "111-85-3", -0.02  ,  0.19  ,  0.21, "yaws2005")
    CALL CalcH( 886, "octane, 2-chloro",                               "628-61-5",  0.12  ,  0.17  ,  0.05, "yaws2005")
    CALL CalcH( 887, "nonane, 1-chloro",                              "2473-01-0",  0.06  ,  0.39  ,  0.33, "yaws2005")
    CALL CalcH( 888, "nonane, 2-chloro",                              "2216-36-6",  0.13  ,  0.17  ,  0.04, "yaws2005")
    CALL CalcH( 889, "nonane, 5-chloro",                             "28123-70-8",  0.19  ,  0.26  ,  0.07, "yaws2005")
    CALL CalcH( 890, "decane, 1-chloro",                              "1002-69-3",  0.20  ,  0.39  ,  0.19, "yaws2005")
    CALL CalcH( 891, "undecane, 1-chloro",                            "2473-03-2",  0.24  ,  0.37  ,  0.13, "yaws2005")
    CALL CalcH( 892, "dodecane, 1-chloro",                             "112-52-7",  0.33  ,  0.25  , -0.08, "yaws2005")
    CALL CalcH( 893, "tridecane, 1-chloro",                            "822-13-9",  0.46  ,  0.15  , -0.31, "yaws2005")
    CALL CalcH( 894, "tetradecane, 1-chloro",                         "2425-54-9",  0.52  ,  0.02  , -0.50, "yaws2005")
    CALL CalcH( 895, "methylene chloride",                              "75-09-2", -1.35  , -0.96  ,  0.39, "1500")
    CALL CalcH( 896, "ethane, 1,1-dichloro-",                           "75-34-3", -0.90  , -0.62  ,  0.28, "1500")
    CALL CalcH( 897, "ethane, 1,2-dichloro-",                          "107-06-2", -1.41  , -1.31  ,  0.10, "1500")
    CALL CalcH( 898, "propane, 1,3-dichloro-",                         "142-28-9", -1.65  , -1.39  ,  0.26, "1500")
    CALL CalcH( 899, "propane, 1,2-dichloro-",                          "78-87-5", -1.13  , -0.93  ,  0.20, "1500")
    CALL CalcH( 900, "butane, 1,1-dichloro",                           "541-33-3", -0.79  , -0.5   ,  0.29, "642")
    CALL CalcH( 901, "butane, 1,4-dichloro-",                          "110-56-5", -1.81  , -1.7   ,  0.11, "1500")
    CALL CalcH( 902, "butane, 2,3-dichloro",                          "7581-97-7", -0.84  , -0.79  ,  0.05, "yaws2005")
    CALL CalcH( 903, "pentane, 1,2-dichloro",                         "1674-33-5", -0.88  , -1.08  , -0.20, "yaws2005")
    CALL CalcH( 904, "pentane, 2,3-dichloro",                          "600-11-3", -0.84  , -0.86  , -0.02, "yaws2005")
    CALL CalcH( 905, "pentane, 1,5-dichloro-",                         "628-76-2", -1.70  , -1.64  ,  0.06, "1150")
    CALL CalcH( 906, "decane, 1,10-dichloro-",                        "2162-98-3", -1.12  , -0.69  ,  0.43, "982")
    CALL CalcH( 907, "dodecane, 1,12-dichloro",                       "3922-28-9", -0.89  , -0.59  ,  0.30, "982")
    CALL CalcH( 908, "chloroform",                                      "67-66-3", -0.90  , -0.79  ,  0.11, "1500")
    CALL CalcH( 909, "ethane, 1,1,2-trichloro-",                        "79-00-5", -1.58  , -1.46  ,  0.12, "1500")
    CALL CalcH( 910, "ethane, 1,1,1-trichloro-",                        "71-55-6", -0.35  , -0.14  ,  0.21, "1500")
    CALL CalcH( 911, "propane, 1,1,1-trichloro-",                     "7789-89-1", -0.42  , -0.97  , -0.55, "yaws2005")
    CALL CalcH( 912, "propane, 1,1,2-trichloro-",                      "598-77-6", -1.29  , -1.55  , -0.26, "yaws2005")
    CALL CalcH( 913, "propane, 1,2,3-trichloro-",                       "96-18-4", -1.98  , -1.73  ,  0.25, "yaws2005")
    CALL CalcH( 914, "carbon tetrachloride",                            "56-23-5", -0.13  ,  0.06  ,  0.19, "1500")
    CALL CalcH( 915, "ethane, 1,1,1,2-tetrachloro-",                   "630-20-6", -0.99  , -0.94  ,  0.05, "1500")
    CALL CalcH( 916, "ethane, 1,1,2,2-tetrachloro-",                    "79-34-5", -1.68  , -1.83  , -0.15, "1500")
    CALL CalcH( 917, "decane, 1,2,9,10-tetrachloro-",               "205646-11-3", -1.53  , -2.14  , -0.61, "982")
    CALL CalcH( 918, "undecane, 1,2,10,11-tetrachloro-",            "210049-49-3", -1.43  , -1.59  , -0.16, "982")
    CALL CalcH( 919, "ethane, pentachloro-",                            "76-01-7", -1.18  , -1.02  ,  0.16, "1501")
    CALL CalcH( 920, "ethane, hexachloro-",                             "67-72-1", -0.98  , -0.8   ,  0.18, "2482")
    CALL CalcH( 921, "ethene, chloro-",                                 "75-01-4",  0.28  , -0.05  , -0.33, "1500")
    CALL CalcH( 922, "1-propene, 3-chloro-",                           "107-05-1", -1.00  , -0.42  ,  0.58, "1501")
    CALL CalcH( 923, "ethene, 1,1-dichloro-",                           "75-35-4",  0.50  ,  0.18  , -0.32, "1500")
    CALL CalcH( 924, "ethene, 1,2-dichloro-",                          "540-59-0",  0.04  , -0.57  , -0.61, "1500")
    CALL CalcH( 925, "1-propene, 1,2-dichloro",                        "563-54-2",  0.12  , -0.56  , -0.68, "2530")
    CALL CalcH( 926, "1-propene, 1,3-dichloro-",                       "542-75-6", -1.15  , -1.16  , -0.01, "C")
    CALL CalcH( 927, "1-propene, 2,3-dichloro-",                        "78-88-6", -1.08  , -0.84  ,  0.24, "479")
    CALL CalcH( 928, "2-butene, 1,4-dichloro-, (e)-",                  "110-57-6", -2.25  , -1.55  ,  0.70, "2530")
    CALL CalcH( 929, "trichloroethylene",                               "79-01-6",  0.13  , -0.32  , -0.45, "1500")
    CALL CalcH( 930, "tetrachloroethylene",                            "127-18-4",  0.38  , -0.1   , -0.48, "1500")
    CALL CalcH( 931, "1-propene, 1,1,2,3,3,3-hexachloro-",            "1888-71-7", -0.39  , -0.7   , -0.31, "2530")
    CALL CalcH( 932, "1,3-butadiene, hexachloro-",                      "87-68-3", -0.19  , -0.38  , -0.19, "C")
    CALL CalcH( 933, "cyclohexane, chloro",                            "542-18-7", -0.90)
    CALL CalcH( 934, "lindane",                                         "58-89-9", -4.12  , -4.09  ,  0.03, "wrongref")
    CALL CalcH( 935, "cyclopentadiene, hexachloro-",                    "77-47-4", -0.76  ,  0.06  ,  0.82, "2530")
    CALL CalcH( 936, "benzene, chloromethyl-",                         "100-44-7", -1.87  , -1.59  ,  0.28, "479")
    CALL CalcH( 937, "benzene, dichloromethyl-",                        "98-87-3", -1.99  , -1.77  ,  0.22, "2530")
    CALL CalcH( 938, "benzene, chloro-",                               "108-90-7", -1.00  , -0.84  ,  0.16, "1500")
    CALL CalcH( 939, "styrene, 2-chloro",                             "2039-87-4", -1.19)
    CALL CalcH( 940, "benzene, 1-chloro-2-methyl-",                     "95-49-8", -1.03  , -0.84  ,  0.19, "1500")
    CALL CalcH( 941, "benzene, 1-chloro-3-methyl-",                    "108-41-8", -0.97  , -0.17  ,  0.80, "2528")
    CALL CalcH( 942, "benzene, 1-chloro-4-methyl-",                    "106-43-4", -1.00  , -0.72  ,  0.28, "2528")
    CALL CalcH( 943, "benzene, 1-chloro-2-nitro-",                      "88-73-3", -2.88  , -3.4   , -0.52, "1195")
    CALL CalcH( 944, "benzene, 1-chloro-4-nitro-",                     "100-00-5", -2.76  , -3.68  , -0.92, "1195")
    CALL CalcH( 945, "benzene, 1-chloro-4-phenoxy-",                  "7005-72-3", -1.88  , -2.35  , -0.47, "howard97")
    CALL CalcH( 946, "benzene, 1,2-dichloro-",                          "95-50-1", -1.31  , -1.11  ,  0.20, "1500")
    CALL CalcH( 947, "benzene, 1,3-dichloro-",                         "541-73-1", -1.07  , -0.83  ,  0.24, "1500")
    CALL CalcH( 948, "benzene, 1,4-dichloro-",                         "106-46-7", -1.21  , -0.97  ,  0.24, "1500")
    CALL CalcH( 949, "benzene, 1,2-dichloro-4-methyl-",                 "95-75-0", -1.29  , -0.96  ,  0.33, "2530")
    CALL CalcH( 950, "benzene, 1,2,3-trichloro-",                       "87-61-6", -1.30  , -0.91  ,  0.39, "1501")
    CALL CalcH( 951, "benzene, 1,2,4-trichloro-",                      "120-82-1", -1.39  , -1.24  ,  0.15, "usepa82")
    CALL CalcH( 952, "benzene, 1,3,5-trichloro-",                      "108-70-3", -1.06  , -1.19  , -0.13, "479")
    CALL CalcH( 953, "benzene, 1-methyl-2,4,5-trichloro-",            "6639-30-1", -1.47  , -1.42  ,  0.05, "1139")
    CALL CalcH( 954, "benzene, 1-methyl-2,3,6-trichloro-",            "2077-46-5", -1.55  , -1.41  ,  0.14, "1139")
    CALL CalcH( 955, "benzene, 1,2,4-trichloro-3-methoxy-",          "50375-10-5", -1.65  , -1.92  , -0.27, "1139")
    CALL CalcH( 956, "benzene, 1,2,4,5-tetrachloro-",                   "95-94-3", -1.36  , -1.39  , -0.03, "479")
    CALL CalcH( 957, "benzene, 1,2,3,4-tetrachloro-",                  "634-66-2", -1.33  , -1.55  , -0.22, "479")
    CALL CalcH( 958, "benzene, 1,2,3,5-tetrachloro-",                  "634-90-2", -1.28  , -1.19  ,  0.09, "479")
    CALL CalcH( 959, "benzene, pentachloro-",                          "608-93-5", -1.25  , -1.52  , -0.27, "1146")
    CALL CalcH( 960, "benzene, pentachloromethyl-",                    "877-11-2", -1.61  , -1.35  ,  0.26, "1139")
    CALL CalcH( 961, "styrene, octachloro-",                         "29082-74-4", -1.60  , -2.27  , -0.67, "1145")
    CALL CalcH( 962, "benzene, hexachloro-",                           "118-74-1", -1.69  , -1.41  ,  0.28, "C")
    CALL CalcH( 963, "naphthalene, 1-chloro-",                          "90-13-1", -2.15  , -2.07  ,  0.08, "yaws2005")
    CALL CalcH( 964, "naphthalene, 2-chloro-",                          "91-58-7", -2.17  , -1.89  ,  0.28, "2339")
    CALL CalcH( 965, "ethane, bromo-",                                  "74-96-4", -0.92  , -0.54  ,  0.38, "1500")
    CALL CalcH( 966, "propane, 1-bromo-",                              "106-94-5", -0.84  , -0.41  ,  0.43, "1500")
    CALL CalcH( 967, "propane, 2-bromo-",                               "75-26-3", -0.56  , -0.35  ,  0.21, "1500")
    CALL CalcH( 968, "butane, 1-bromo-",                               "109-65-9", -0.74  , -0.29  ,  0.45, "1500")
    CALL CalcH( 969, "butane, 2-bromo-",                                "78-76-2", -0.54  , -0.02  ,  0.52, "1500")
    CALL CalcH( 970, "pentane, 1-bromo-",                              "110-53-2", -0.65  , -0.07  ,  0.58, "1500")
    CALL CalcH( 971, "hexane, 1-bromo-",                               "111-25-1", -0.56  ,  0.13  ,  0.69, "1500")
    CALL CalcH( 972, "heptane, 1-bromo-",                              "629-04-9", -0.47  ,  0.25  ,  0.72, "1500")
    CALL CalcH( 973, "octane, 1-bromo-",                               "111-83-1", -0.38  ,  0.38  ,  0.76, "1500")
    CALL CalcH( 974, "nonane, 1-bromo-",                               "693-58-3", -0.29)
    CALL CalcH( 975, "propane, 1-bromo-2-methyl-",                      "78-77-3", -0.70  , -0.02  ,  0.68, "1500")
    CALL CalcH( 976, "propane, 2-bromo-2-methyl-",                     "507-19-7", -0.11  ,  0.12  ,  0.23, "789")
    CALL CalcH( 977, "butane, 1-bromo-3-methyl-",                      "107-82-4", -0.65  ,  0.15  ,  0.80, "2482")
    CALL CalcH( 978, "methane, dibromo-",                               "74-95-3", -1.97  , -1.44  ,  0.53, "1500")
    CALL CalcH( 979, "ethane-1,2-dibromo-",                            "106-93-4", -1.99  , -1.54  ,  0.45, "789")
    CALL CalcH( 980, "propane, 1,2-dibromo-3-chloro",                   "96-12-8", -2.35  , -2.22  ,  0.13, "1145")
    CALL CalcH( 981, "propane, 1,2-dibromo-",                           "78-75-1", -1.67  , -1.43  ,  0.24, "642")
    CALL CalcH( 982, "propane, 1,3-dibromo-",                          "109-64-8", -2.25  , -1.44  ,  0.81, "2482")
    CALL CalcH( 983, "butane, 1,4-dibromo-",                           "110-52-1", -2.26  , -1.42  ,  0.84, "2530")
    CALL CalcH( 984, "allyl bromide",                                  "106-95-6", -1.33  , -0.62  ,  0.71, "1500")
    CALL CalcH( 985, "cyclohexane, 1-bromo-",                          "108-85-0", -1.24)
    CALL CalcH( 986, "cyclohexene, 1-bromo",                          "2044-08-8", -0.70)
    CALL CalcH( 987, "cyclohexene, 1-bromo-4-methyl",                "31053-84-6", -0.55)
    CALL CalcH( 988, "methane, tribromo-",                              "75-25-2", -1.26  , -1.56  , -0.30, "1500")
    CALL CalcH( 989, "methane, tetrabromo-",                           "558-13-4", -0.71  , -1.69  , -0.98, "C")
    CALL CalcH( 990, "ethane-1,1,2,2-tetrabromo-",                      "79-27-6", -2.77  , -3.3   , -0.53, "2440")
    CALL CalcH( 991, "benzene, bromomethyl-",                          "100-39-0", -2.13  , -1.74  ,  0.39, "1500")
    CALL CalcH( 992, "benzene, bromo-",                                "108-86-1", -1.11  , -1.07  ,  0.04, "1500")
    CALL CalcH( 993, "benzene, 1-bromo-2-methyl-",                      "95-46-5", -1.12)
    CALL CalcH( 994, "benzene, 1-bromo-3-methyl-",                     "591-17-3", -1.11)
    CALL CalcH( 995, "benzene, 1-bromo-4-methyl-",                     "106-38-7", -1.14  , -1.02  ,  0.12, "1500")
    CALL CalcH( 996, "benzene, 1-bromo-2-ethyl-",                     "1973-22-4", -1.03  , -0.87  ,  0.16, "642")
    CALL CalcH( 997, "benzene, 1-bromo-2-isopropyl",                  "7073-94-1", -0.79  , -0.62  ,  0.17, "642")
    CALL CalcH( 998, "benzene, 1,3-dibromo-",                          "108-36-1", -1.35  , -1.1   ,  0.25, "479")
    CALL CalcH( 999, "benzene, 1,4-dibromo-",                          "106-37-6", -1.48  , -1.44  ,  0.04, "1932")
    CALL CalcH(1000, "benzene, 1,2,4-tribromo-",                       "615-54-3", -1.66  , -1.85  , -0.19, "1932")
    CALL CalcH(1001, "benzene, 1,2,4,5-tetrabromo-",                   "636-28-2", -1.69  , -2.23  , -0.54, "1932")
    CALL CalcH(1002, "benzene, hexabromo-",                             "87-82-1", -1.48  , -2.92  , -1.44, "1932")
    CALL CalcH(1003, "naphthalene, bromo-",                             "90-11-9", -2.31  , -1.93  ,  0.38, "2535")
    CALL CalcH(1004, "benzene, 1-bromo-4-chloro-",                     "106-39-8", -1.35  , -1.23  ,  0.12, "479")
    CALL CalcH(1005, "methane, iodo",                                   "74-88-4", -0.71  , -0.65  ,  0.06, "1500")
    CALL CalcH(1006, "ethane, iodo",                                    "75-03-6", -0.67  , -0.54  ,  0.13, "1500")
    CALL CalcH(1007, "propane,1-iodo-",                                "107-08-4", -0.59  , -0.39  ,  0.20, "1500")
    CALL CalcH(1008, "propane,2-iodo-",                                 "75-30-9", -0.29  , -0.43  , -0.14, "789")
    CALL CalcH(1009, "butane, iodo-",                                  "542-69-8", -0.49  , -0.18  ,  0.31, "1500")
    CALL CalcH(1010, "pentane, iodo-",                                 "628-17-1", -0.39  , -0.1   ,  0.29, "1500")
    CALL CalcH(1011, "hexane, iodo-",                                  "638-45-9", -0.31  ,  0.06  ,  0.37, "1500")
    CALL CalcH(1012, "heptane, iodo",                                 "4282-40-0", -0.22  ,  0.2   ,  0.42, "1500")
    CALL CalcH(1013, "butane, 2-iodo-",                                "513-48-4", -0.24)
    CALL CalcH(1014, "methane, di-iodo-",                               "75-11-6", -2.26  , -1.84  ,  0.42, "1500")
    CALL CalcH(1015, "methane, tri-iodo",                               "75-47-8", -0.52  , -0.92  , -0.40, "789")
    CALL CalcH(1016, "allyl iodide",                                   "556-56-9", -0.97)
    CALL CalcH(1017, "iodocyclohexane",                                "626-62-0", -0.99)
    CALL CalcH(1018, "iodocyclohexene",                              "17497-53-9", -1.01)
    CALL CalcH(1019, "benzene, iodo-",                                 "591-50-4", -1.55  , -1.28  ,  0.27, "1500")
    CALL CalcH(1020, "methane, chloro-fluoro-",                        "593-70-4", -0.93  , -0.57  ,  0.36, "789")
    CALL CalcH(1021, "methane, chloro-iodo-",                          "593-71-5", -1.70  , -1.34  ,  0.36, "583")
    CALL CalcH(1022, "methane, chloro-difluoro-",                       "75-45-6", -0.17  ,  0.08  ,  0.25, "789")
    CALL CalcH(1023, "methane, bormochloro-difluoro-",                 "353-59-3",  0.67  ,  0.85  ,  0.18, "2440")
    CALL CalcH(1024, "methane, dichloro-fluoro-",                       "75-43-4", -0.59  , -0.67  , -0.08, "789")
    CALL CalcH(1025, "methane, dibromo-chloro-",                       "124-48-1", -1.13  , -1.47  , -0.34, "usepa82")
    CALL CalcH(1026, "methane, bromo-dichloro-",                        "75-27-4", -0.99  , -1.28  , -0.29, "C")
    CALL CalcH(1027, "methane, bromo-trifluoro-",                       "75-63-8",  1.10  ,  1.11  ,  0.01, "642")
    CALL CalcH(1028, "ethane, 1-bromo-2-chloro-",                      "107-04-0", -1.64  , -1.43  ,  0.21, "2482")
    CALL CalcH(1029, "ethane, 1,1,2,2-tetrachloro-difluoro-",           "76-12-0", -0.10  ,  0.61  ,  0.71, "642")
    CALL CalcH(1030, "ethane, 1,1-dichloro-tetrafluoro-",              "374-07-2",  0.66  ,  1.73  ,  1.07, "C")
    CALL CalcH(1031, "ethane, 1,2-dichloro-tetrafluoro-",               "76-14-2",  0.68  ,  1.69  ,  1.01, "789")
    CALL CalcH(1032, "ethane, chloropentafluoro-",                      "76-15-3",  1.08  ,  2.03  ,  0.95, "789")
    CALL CalcH(1033, "ethane, 2-chloro-1,1,1-trifluoro-",               "75-88-7",  0.13  ,  0.04  , -0.09, "642")
    CALL CalcH(1034, "ethane, 1,1,1,2-tetrachlorodifluoro-",            "76-11-9", -0.10  ,  0.8   ,  0.90, "2530")
    CALL CalcH(1035, "ethane, 2,2-dichloro-1,1,1-trifluoro-",          "306-83-2", -0.09  ,  0.13  ,  0.22, "2530")
    CALL CalcH(1036, "fluroxene",                                      "461-24-5", -0.25  ,  0.10  ,  0.35, "1500")
    CALL CalcH(1037, "methane, trichloro-fluoro-",                      "75-69-4",  0.38  ,  0.6   ,  0.22, "C")
    CALL CalcH(1038, "ethane, 1,1,2-trichloro-1,2,2-trifluoro",         "76-13-1",  0.35  ,  1.3   ,  0.95, "1500")
    CALL CalcH(1039, "methane, dichloro-difluoro-",                     "75-71-8",  0.87  ,  1.05  ,  0.18, "C")
    CALL CalcH(1040, "methane, chloro-trifluoro-",                      "75-72-9",  1.19  ,  1.75  ,  0.56, "C")
    CALL CalcH(1041, "halothane",                                      "151-67-7", -0.34  , -0.08  ,  0.26, "1500")
    CALL CalcH(1042, "teflurane",                                      "124-72-1",  0.28  ,  0.37  ,  0.09, "1501")
    CALL CalcH(1043, "isoflurane",                                   "26675-46-7", -0.02  ,  0.07  ,  0.09, "1501")
    CALL CalcH(1044, "methoxyflurane",                                  "76-38-0", -1.01  , -0.82  ,  0.19, "1500")
    CALL CalcH(1045, "enflurane",                                    "13838-16-9", -0.23)
    CALL CalcH(1046, "4,4'-ddd",                                        "72-54-8", -3.71  , -3.55  ,  0.16, "2530")
    CALL CalcH(1047, "4,4'-dde",                                        "72-55-9", -2.65  , -2.75  , -0.10, "2530")
    CALL CalcH(1048, "2-pcb",                                         "2051-60-7", -1.75  , -1.8   , -0.05, "1194")
    CALL CalcH(1049, "3-pcb",                                         "2051-61-8", -1.96  , -1.76  ,  0.20, "850")
    CALL CalcH(1050, "4-pcb",                                         "2051-62-9", -1.98  , -2.    , -0.02, "850")
    CALL CalcH(1051, "2,2'-pcb",                                     "13029-08-8", -1.80  , -1.87  , -0.07, "850")
    CALL CalcH(1052, "2,3-pcb",                                      "16605-91-7", -2.07  , -2.01  ,  0.06, "2530")
    CALL CalcH(1053, "2,3'-pcb",                                     "25569-80-6", -2.14  , -2.    ,  0.14, "850")
    CALL CalcH(1054, "2,4-pcb",                                      "33284-50-3", -1.99  , -1.7   ,  0.29, "850")
    CALL CalcH(1055, "2,4'-pcb",                                     "34883-43-7", -2.15  , -1.68  ,  0.47, "850")
    CALL CalcH(1056, "2,5-pcb",                                      "34883-39-1", -2.01  , -2.3   , -0.29, "850")
    CALL CalcH(1057, "2,6-pcb",                                      "33146-45-1", -1.87  , -2.01  , -0.14, "2530")
    CALL CalcH(1058, "3,3'-pcb",                                      "2050-67-1", -2.35  , -2.27  ,  0.08, "850")
    CALL CalcH(1059, "3,4'-pcb",                                      "2974-90-5", -2.37  , -2.39  , -0.02, "850")
    CALL CalcH(1060, "3,4-pcb",                                       "2974-92-7", -2.28  , -2.22  ,  0.06, "2530")
    CALL CalcH(1061, "3,5-pcb",                                      "34883-41-5", -2.09  , -1.86  ,  0.23, "2530")
    CALL CalcH(1062, "4,4'-pcb",                                      "2050-68-2", -2.38  , -2.4   , -0.02, "850")
    CALL CalcH(1063, "2,2',4-pcb",                                   "37680-66-3", -1.96  , -1.73  ,  0.23, "850")
    CALL CalcH(1064, "2,2',3-pcb",                                   "38444-78-9", -2.14  , -1.68  ,  0.46, "850")
    CALL CalcH(1065, "2,2',5-pcb",                                   "37680-65-2", -2.06  , -1.67  ,  0.39, "850")
    CALL CalcH(1066, "2,2',6-pcb",                                   "38444-73-4", -2.13  , -2.01  ,  0.12, "2530")
    CALL CalcH(1067, "2,4',6-pcb",                                   "38444-77-8", -2.27  , -2.07  ,  0.20, "2530")
    CALL CalcH(1068, "2,4,4'-pcb",                                    "7012-37-5", -2.41  , -1.95  ,  0.46, "1146")
    CALL CalcH(1069, "2,4,5-pcb",                                    "15862-07-4", -2.29  , -2.21  ,  0.08, "850")
    CALL CalcH(1070, "2,4,6-pcb",                                    "35693-92-6", -1.86  , -1.9   , -0.04, "850")
    CALL CalcH(1071, "2,3,4-pcb",                                    "55702-46-0", -2.29  , -1.94  ,  0.35, "850")
    CALL CalcH(1072, "2,3',5-pcb",                                   "38444-81-4", -2.39  , -1.93  ,  0.46, "850")
    CALL CalcH(1073, "2',3,4-pcb",                                   "38444-86-9", -2.42  , -2.17  ,  0.25, "2530")
    CALL CalcH(1074, "2,3,4'-pcb",                                   "38444-85-8", -2.47  , -2.22  ,  0.25, "2530")
    CALL CalcH(1075, "2,3',5'-pcb",                                  "37680-68-5", -2.26  , -2.07  ,  0.19, "2530")
    CALL CalcH(1076, "2,3,6-pcb",                                    "55702-45-9", -2.07  , -2.03  ,  0.04, "2530")
    CALL CalcH(1077, "2,4',5-pcb",                                   "16606-02-3", -2.40  , -2.09  ,  0.31, "2530")
    CALL CalcH(1078, "3,3',5-pcb",                                   "38444-87-0", -2.47  , -2.14  ,  0.33, "2530")
    CALL CalcH(1079, "3,4,4'-pcb",                                   "38444-90-5", -2.66  , -2.37  ,  0.29, "2530")
    CALL CalcH(1080, "2,3,3'-pcb",                                   "38444-84-7", -2.46  , -2.17  ,  0.29, "2530")
    CALL CalcH(1081, "2,2',3,3'-pcb",                                "38444-93-8", -2.45  , -2.15  ,  0.30, "850")
    CALL CalcH(1082, "2,2',4,4'-pcb",                                 "2437-79-8", -2.09  , -2.39  , -0.30, "850")
    CALL CalcH(1083, "2,2',5,5'-pcb",                                "35693-99-3", -2.29  , -1.95  ,  0.34, "850")
    CALL CalcH(1084, "2,2',5,6'-pcb",                                "41464-41-9", -2.34  , -1.98  ,  0.36, "850")
    CALL CalcH(1085, "3,3',4,4'-pcb",                                "32598-13-3", -2.95  , -3.4   , -0.45, "850")
    CALL CalcH(1086, "2,2',3,4'-pcb",                                "36559-22-5", -2.25  , -2.22  ,  0.03, "2530")
    CALL CalcH(1087, "2,2',3,4-pcb",                                 "52663-59-9", -2.23  , -2.22  ,  0.01, "2530")
    CALL CalcH(1088, "2,2',3,5'-pcb",                                "41464-39-5", -2.38  , -2.22  ,  0.16, "2530")
    CALL CalcH(1089, "2,2',4,5'-pcb",                                "41464-40-8", -2.18  , -2.05  ,  0.13, "2530")
    CALL CalcH(1090, "2,2',4,6'-pcb",                                "68194-04-7", -2.26  , -2.22  ,  0.04, "2530")
    CALL CalcH(1091, "2,2',6,6'-pcb",                                "15968-05-5", -2.46  , -2.07  ,  0.39, "2530")
    CALL CalcH(1092, "2,3',4,4'-pcb",                                "32598-10-0", -2.71  , -2.29  ,  0.42, "2530")
    CALL CalcH(1093, "2,3',4,5-pcb",                                 "73575-53-8", -2.68  , -2.37  ,  0.31, "2530")
    CALL CalcH(1094, "2,3',4',5-pcb",                                "32598-11-1", -2.70  , -2.37  ,  0.33, "2530")
    CALL CalcH(1095, "2,3,4,6-pcb",                                  "54230-22-7", -2.00  , -2.05  , -0.05, "2530")
    CALL CalcH(1096, "2,3',4,6-pcb",                                 "60233-24-1", -2.29  , -2.05  ,  0.24, "2530")
    CALL CalcH(1097, "2,3,4',6-pcb",                                 "52663-58-8", -2.44  , -2.22  ,  0.22, "2530")
    CALL CalcH(1098, "2,4,4',5-pcb",                                 "32690-93-0", -2.69  , -2.38  ,  0.31, "2530")
    CALL CalcH(1099, "2,3,5,6-pcb",                                  "33284-54-7", -2.12  , -1.83  ,  0.29, "2530")
    CALL CalcH(1100, "3,3',4,5'-pcb",                                "41464-48-6", -2.79  , -2.42  ,  0.37, "2530")
    CALL CalcH(1101, "2,2',3,4,5-pcb",                               "55312-69-1", -2.27  , -1.46  ,  0.81, "850")
    CALL CalcH(1102, "2,2',3,4,5'-pcb",                              "38380-02-8", -2.48  , -2.31  ,  0.17, "850")
    CALL CalcH(1103, "2,2',3,4,4'-pcb",                              "65510-45-4", -2.36  , -2.55  , -0.19, "2530")
    CALL CalcH(1104, "2,2',3',4,5-pcb",                              "41464-51-1", -2.56  , -2.5   ,  0.06, "2530")
    CALL CalcH(1105, "2,2',4,5,6'-pcb",                              "68194-06-9", -2.34  , -2.42  , -0.08, "2530")
    CALL CalcH(1106, "2,3',4,4',6-pcb",                              "56558-17-9", -2.61  , -2.5   ,  0.11, "2530")
    CALL CalcH(1107, "2,4,5,3',5'-pcb",                              "68194-12-7", -2.79  , -2.62  ,  0.17, "2530")
    CALL CalcH(1108, "2,2',4,5,5' -pcb",                             "37680-73-2", -2.47  , -2.42  ,  0.05, "2530")
    CALL CalcH(1109, "2,2',3,3'4,4'-,pcb",                           "38380-07-3", -2.66  , -2.6   ,  0.06, "850")
    CALL CalcH(1110, "2,2',3,3'5,5'-,pcb",                           "35694-04-3", -2.65  , -2.8   , -0.15, "850")
    CALL CalcH(1111, "2,2',3,3'6,6'-,pcb",                           "38411-22-2", -2.83  , -2.6   ,  0.23, "850")
    CALL CalcH(1112, "2,2',3,3',4,5'-pcb",                           "52663-66-8", -2.68  , -2.8   , -0.12, "2530")
    CALL CalcH(1113, "2,3,4,5,2',3'-pcb",                            "55215-18-4", -2.60  , -2.91  , -0.31, "2530")
    CALL CalcH(1114, "2,2',3,3',4,6'-pcb",                           "38380-05-1", -2.73  , -2.73  ,  0.00, "2530")
    CALL CalcH(1115, "2,2',3,3',5,6'-pcb",                           "52744-13-5", -2.76  , -2.62  ,  0.14, "2530")
    CALL CalcH(1116, "2,2',3,4,5,6'-pcb",                            "68194-15-0", -2.61  , -2.78  , -0.17, "2530")
    CALL CalcH(1117, "2,2',3,3',5,6--pcb",                           "52704-70-8", -2.69  , -2.68  ,  0.01, "2530")
    CALL CalcH(1118, "2,2',3,3',4,6-pcb",                            "61798-70-7", -2.60  , -2.78  , -0.18, "2530")
    CALL CalcH(1119, "2,2',3,4,5,5'-pcb",                            "52712-04-6", -2.52  , -3.01  , -0.49, "2530")
    CALL CalcH(1120, "2,2',3,4',5,5'-pcb",                           "51908-16-8", -2.69  , -2.97  , -0.28, "2530")
    CALL CalcH(1121, "2,3,4,2',4',5'-pcb",                           "35065-28-2", -2.66  , -3.05  , -0.39, "2530")
    CALL CalcH(1122, "2,2',3,5,5',6-pcb",                            "52663-63-5", -2.60  , -2.6   ,  0.00, "2530")
    CALL CalcH(1123, "2,3,3',4,5,5'-pcb",                            "39635-35-3", -2.81  , -3.07  , -0.26, "2530")
    CALL CalcH(1124, "2,3,3',4,5,6-pcb",                             "41411-62-5", -2.46  , -3.07  , -0.61, "2530")
    CALL CalcH(1125, "2,3,3',4',5,6-pcb",                            "74472-44-9", -2.80  , -3.19  , -0.39, "2530")
    CALL CalcH(1126, "2,3,3',5,5',6-pcb",                            "74472-46-1", -2.60  , -2.91  , -0.31, "2530")
    CALL CalcH(1127, "2,3,4,4',5,6-pcb",                             "41411-63-6", -2.48  , -2.11  ,  0.37, "2530")
    CALL CalcH(1128, "2,2',4,4',5,5'-pcb",                           "35065-27-1", -2.66  , -3.01  , -0.35, "2530")
    CALL CalcH(1129, "2,2',3,4,5,5',6-pcb",                          "52712-05-7", -2.58  , -3.17  , -0.59, "2530")
    CALL CalcH(1130, "2,2',3,3',4,5,6,-pcb",                         "68194-13-8", -2.66  , -2.66  ,  0.00, "2530")
    CALL CalcH(1131, "2,2',3,3',5,5',6-pcb",                         "52663-67-9", -2.82  , -3.01  , -0.19, "2530")
    CALL CalcH(1132, "2,3,5,6,2',3',6'-pcb",                         "52663-64-6", -2.89  , -2.99  , -0.10, "2530")
    CALL CalcH(1133, "2,2',3,4,4',5,5'-pcb",                         "35065-29-3", -2.71  , -3.37  , -0.66, "2530")
    CALL CalcH(1134, "2,2',3,3',4,4',5-pcb",                         "35065-30-6", -2.71  , -3.42  , -0.71, "2530")
    CALL CalcH(1135, "2,2',3,3',4,5,6'-pcb",                         "38411-25-5", -2.81  , -3.22  , -0.41, "2530")
    CALL CalcH(1136, "2,2',3,3',4,5,6,-pcb",                         "68194-16-1", -2.66  , -3.22  , -0.56, "2530")
    CALL CalcH(1137, "2,2',3,3',4,4',6'-pcb",                        "52663-71-5", -2.72  , -2.9   , -0.18, "850")
    CALL CalcH(1138, "2,2',3,3',5,5',6,6'-,pcb",                      "2136-99-4", -2.96  , -3.3   , -0.34, "850")
    CALL CalcH(1139, "2,2',3,3',4,4',5,5'-pcb",                      "35694-08-7", -2.75  , -3.37  , -0.62, "2530")
    CALL CalcH(1140, "2,2',3,3',4,4',5,6'-pcb",                      "42740-50-1", -2.74  , -3.37  , -0.63, "2530")
    CALL CalcH(1141, "2,3,4,5,6,2',3',4'-pcb",                       "52663-78-2", -2.77  , -3.33  , -0.56, "2530")
    CALL CalcH(1142, "2,2',3,3',4',5,5',6-pcb",                      "52663-75-9", -2.83  , -3.37  , -0.54, "2530")
    CALL CalcH(1143, "2,2',3,3',4,5,5',6-pcb",                       "68194-17-2", -2.79  , -3.22  , -0.43, "2530")
    CALL CalcH(1144, "2,2',3,3',4,5',6,6'-pcb",                      "40186-71-8", -2.85  , -3.14  , -0.29, "2530")
    CALL CalcH(1145, "decachloro-pcb",                                "2051-24-3", -2.89  , -2.3   ,  0.59, "850")
    CALL CalcH(1146, "cyanogen, chloro",                               "506-77-4", -1.47  , -1.1   ,  0.37, "789")
    CALL CalcH(1147, "hypochlorous acid",                             "7790-92-3", -4.13  , -3.8   ,  0.33, "489")
    CALL CalcH(1148, "acetaldehyde, hydroxy-",                         "141-46-8", -6.21  , -6.01  ,  0.20, "1910")
    CALL CalcH(1149, "ethanol, 2-fluoro-",                             "371-62-0", -3.80  , -4.12  , -0.32, "2530")
    CALL CalcH(1150, "sulfate, dimethyl",                               "77-78-1", -4.23  , -3.77  ,  0.46, "2440")
    CALL CalcH(1151, "dmso",                                            "67-68-5", -7.03  , -7.21  , -0.18, "1501")
    CALL CalcH(1152, "morpholine",                                     "110-91-8", -5.61  , -5.26  ,  0.35, "984")
    CALL CalcH(1153, "morpholine, n-methyl-",                          "109-02-4", -4.15  , -4.64  , -0.49, "984")
    CALL CalcH(1154, "2-propanone, 1-chloro-",                          "78-95-5", -3.34  , -3.16  ,  0.18, "495")
    CALL CalcH(1155, "cyclopropanoic acid, methyl ester",             "2868-37-3", -2.44  , -3.01  , -0.57, "1501")
    CALL CalcH(1156, "2-chloroethyl, vinyl ether",                     "110-75-8", -0.75  , -0.43  ,  0.32, "usepa82")
    CALL CalcH(1157, "2,3-propylene oxide, 1-chloro",                  "106-89-8", -2.39  , -2.84  , -0.45, "usepa82")
    CALL CalcH(1158, "acetic acid, fluoro-",                           "144-49-0", -6.13  , -6.29  , -0.16, "896")
    CALL CalcH(1159, "acetic acid, difluoro-",                         "381-73-7", -5.25  , -5.87  , -0.62, "896")
    CALL CalcH(1160, "acetic acid, trifluoro-",                         "76-05-1", -3.00  , -5.33  , -2.33, "896")
    CALL CalcH(1161, "acetic acid, chloro",                             "79-11-8", -6.34  , -6.43  , -0.09, "896")
    CALL CalcH(1162, "acetic acid, dichloro",                           "79-43-6", -5.98  , -6.47  , -0.49, "896")
    CALL CalcH(1163, "acetic acid, trichloro",                          "76-03-9", -4.07  , -6.22  , -2.15, "896")
    CALL CalcH(1164, "ethanol, 2,2,2-trifluoro-",                       "75-89-8", -3.18  , -2.76  ,  0.42, "1500")
    CALL CalcH(1165, "propanol, 2,2,3,3-tetrafluoro-",                  "76-37-9", -3.17  , -3.59  , -0.42, "727")
    CALL CalcH(1166, "propanol, 2,2,3,3,3-pentafluoro-",               "422-05-9", -2.76  , -3.04  , -0.28, "727")
    CALL CalcH(1167, "2-propanol, 1,3-dichloro-",                       "96-23-1", -4.81  , -4.15  ,  0.66, "1145")
    CALL CalcH(1168, "2-propanol, 1,1,1,3,3,3-hexafluoro-",            "920-66-1", -1.80  , -2.76  , -0.96, "727")
    CALL CalcH(1169, "2-propanol, 1,1,1-trifluoro-",                   "374-01-6", -2.74  , -3.05  , -0.31, "727")
    CALL CalcH(1170, "ethane, 1,1-dichloro-1-nitro-",                  "594-72-9", -1.70  , -1.26  ,  0.44, "2530")
    CALL CalcH(1171, "methane, trichloronitro-",                        "76-06-2", -0.79  , -1.08  , -0.29, "wrongref")
    CALL CalcH(1172, "2-nitro-ethanol",                                "625-48-9", -5.59  , -5.99  , -0.40, "1024")
    CALL CalcH(1173, "1-nitro-2-propanol",                            "3156-73-8", -5.29  , -5.43  , -0.14, "727")
    CALL CalcH(1174, "2-nitro-1-propanol",                            "2902-96-7", -5.39  , -5.04  ,  0.35, "727")
    CALL CalcH(1175, "2-nitro-3-butanol",                             "6270-16-2", -5.15  , -5.39  , -0.24, "727")
    CALL CalcH(1176, "1-nitro-2-butanol",                             "3156-74-9", -5.26  , -5.15  ,  0.11, "727")
    CALL CalcH(1177, "2-nitro-1-butanol",                              "609-31-4", -5.27  , -5.17  ,  0.10, "727")
    CALL CalcH(1178, "nitrooxyacetone",                               "6745-71-7", -5.48  , -4.39  ,  1.09, "727")
    CALL CalcH(1179, "1,2-butanediol, 1-nitrate",                   "147794-11-4", -5.18  , -5.13  ,  0.05, "2530")
    CALL CalcH(1180, "1,2-propanediol, 1-nitrate",                   "20266-65-3", -5.37  , -5.41  , -0.04, "2530")
    CALL CalcH(1181, "1,2-butanediol, 2-nitrate",                   "147794-12-5", -5.17  , -5.15  ,  0.02, "2530")
    CALL CalcH(1182, "1,2-propanediol, 2-nitrate",                   "20266-74-4", -5.33  , -5.02  ,  0.31, "2530")
    CALL CalcH(1183, "2,3-butanediol, mononitrate",                 "147794-10-3", -5.13  , -5.37  , -0.24, "2530")
    CALL CalcH(1184, "1,2-ethanediol, mononitrate",                  "16051-48-2", -5.63  , -5.96  , -0.33, "2530")
    CALL CalcH(1185, "peroxyacetylnitrate",                           "2278-22-0", -3.74  , -1.93  ,  1.81, "2530")
    CALL CalcH(1186, "ethanol, 2,2'-oxybis-dinitrate",                 "693-21-0", -5.44  , -4.78  ,  0.66, "2530")
    CALL CalcH(1187, "nitroglycerine",                                  "55-63-0", -4.99  , -5.37  , -0.38, "2440")
    CALL CalcH(1188, "acetic acid, chloro-methyl ester",                "96-34-4", -2.76  , -3.01  , -0.25, "2530")
    CALL CalcH(1189, "acetic acid, cyano-ethyl ester",                 "105-56-6", -5.28  , -4.91  ,  0.37, "2440")
    CALL CalcH(1190, "ethanol, 2-bis (isopropyl) amino-",               "96-80-0", -5.67  , -4.12  ,  1.55, "2530")
    CALL CalcH(1191, "isoxazole",                                      "288-14-2", -2.78  , -2.98  , -0.20, "2530")
    CALL CalcH(1192, "5-methyl-3 (2h)-isoxazolone",                  "10004-44-1", -7.09  , -7.05  ,  0.04, "2530")
    CALL CalcH(1193, "bde-15",                                        "2050-47-7", -2.35  , -2.78  , -0.43, "2536")
    CALL CalcH(1194, "bde-28",                                       "41318-75-6", -2.54  , -3.11  , -0.57, "2536")
    CALL CalcH(1195, "bde-47",                                        "5436-43-1", -2.73  , -3.35  , -0.62, "2536")
    CALL CalcH(1196, "bde-99",                                       "60348-60-9", -3.03  , -3.67  , -0.64, "2536")
    CALL CalcH(1197, "bde-100",                                     "189084-64-8", -2.96  , -3.81  , -0.85, "2536")
    CALL CalcH(1198, "bde-153",                                      "68631-49-2", -3.32  , -3.86  , -0.54, "2536")
    CALL CalcH(1199, "bde-154",                                     "207122-15-4", -3.25  , -3.99  , -0.74, "wrongref")
    CALL CalcH(1200, "dbp-br3cl3a",                                 "400766-93-0", -4.37  , -4.21  ,  0.16, "2469")
    CALL CalcH(1201, "dbp-br3cl3b",                                 "666856-68-4", -4.37  , -4.89  , -0.52, "2469")
    CALL CalcH(1202, "dbp-br4cl2",                                  "253798-64-0", -4.66  , -4.81  , -0.15, "2469")
    CALL CalcH(1203, "dbp-br5cl",                                   "400767-00-2", -4.87  , -5.54  , -0.67, "2469")
    CALL CalcH(1204, "dbp-br6",                                     "253798-63-9", -5.10  , -6.07  , -0.97, "2469")
    CALL CalcH(1205, "diphenylether, 3-chloro-",                      "6452-49-9", -1.83  , -2.47  , -0.64, "2530")
    CALL CalcH(1206, "1,2,3,4-tetrachlorodibenzo[b,e][1,4] dioxin",  "30746-58-8", -3.54  , -3.07  ,  0.47, "2537")
    CALL CalcH(1207, "1,2,3,7-tetrachlorodibenzo-p-dioxin",          "67028-18-6", -3.63  , -3.49  ,  0.14, "2537")
    CALL CalcH(1208, "1,2,4-trichlorodibenzo-p-dioxin",              "39227-58-2", -3.51  , -2.82  ,  0.69, "2537")
    CALL CalcH(1209, "2,7-dichlorodibenzo-p-dioxin",                 "33857-26-0", -3.26  , -2.6   ,  0.66, "2537")
    CALL CalcH(1210, "1,3,6,8-tetrachlorodibenzo-p-dioxin",          "33423-92-6", -3.48  , -3.53  , -0.05, "2537")
    CALL CalcH(1211, "aniline, 2-chloro-",                              "95-51-2", -3.76  , -3.6   ,  0.16, "984")
    CALL CalcH(1212, "aniline, 3-chloro-",                             "108-42-9", -4.28  , -4.27  ,  0.01, "984")
    CALL CalcH(1213, "aniline, 4-chloro-",                             "106-47-8", -4.33  , -4.33  ,  0.00, "984")
    CALL CalcH(1214, "aniline, 2-nitro-",                               "88-74-4", -4.89  , -5.41  , -0.52, "984")
    CALL CalcH(1215, "aniline, 3-nitro-",                               "99-09-2", -6.82  , -6.49  ,  0.33, "984")
    CALL CalcH(1216, "aniline, 4-nitro-",                              "100-01-6", -6.74  , -7.54  , -0.80, "984")
    CALL CalcH(1217, "aniline, 2-chloro-4-nitro-",                     "121-87-9", -6.06  , -6.39  , -0.33, "2530")
    CALL CalcH(1218, "aniline, 2-methoxy-",                             "90-04-0", -4.84  , -4.49  ,  0.35, "984")
    CALL CalcH(1219, "aniline, 3-methoxy-",                            "536-90-3", -5.64  , -5.35  ,  0.29, "984")
    CALL CalcH(1220, "aniline, 4-methoxy-",                            "104-94-9", -5.53  , -5.49  ,  0.04, "984")
    CALL CalcH(1221, "pyridine, 2-chloro-",                            "109-09-1", -3.16  , -3.22  , -0.06, "984")
    CALL CalcH(1222, "pyridine, 3-chloro-",                            "626-60-8", -3.01  , -2.94  ,  0.07, "984")
    CALL CalcH(1223, "pyridine, 2-chloro-6-trichloromethyl-",         "1929-82-4", -3.64  , -3.06  ,  0.58, "2530")
    CALL CalcH(1224, "pyridine, 3-cyano-",                             "100-54-9", -4.60  , -4.95  , -0.35, "1501")
    CALL CalcH(1225, "pyridine, 4-cyano-",                             "100-48-1", -4.63  , -4.42  ,  0.21, "1501")
    CALL CalcH(1226, "pyridine, 3-acetyl-",                            "350-03-8", -5.67  , -6.06  , -0.39, "1501")
    CALL CalcH(1227, "pyridine, 4-acetyl-",                           "1122-54-9", -5.68  , -5.59  ,  0.09, "1501")
    CALL CalcH(1228, "pyridine, 3-formaldehyde-",                      "500-22-1", -5.41  , -5.21  ,  0.20, "1501")
    CALL CalcH(1229, "pyridine, 4-formylaldehyde-",                    "872-85-5", -5.41  , -5.14  ,  0.27, "1501")
    CALL CalcH(1230, "pyrazine, 2-ethyl-3-methoxy-",                 "25680-58-4", -4.79  , -3.22  ,  1.57, "1497")
    CALL CalcH(1231, "pyrazine, 2-methoxy-3-isobutyl-",              "25773-40-4", -4.56  , -2.68  ,  1.88, "1497")
    CALL CalcH(1232, "nitrobenzene , 3-chloro-",                       "121-73-3", -2.72  , -3.24  , -0.52, "2530")
    CALL CalcH(1233, "nitrobenzene, 4-chloro-",                        "100-00-5", -2.76  , -3.68  , -0.92, "2530")
    CALL CalcH(1234, "nitrobenzene, 3,6-dichloro-",                     "89-61-2", -2.88  , -3.29  , -0.41, "2530")
    CALL CalcH(1235, "nitrobenzene, 3,4-dichloro-",                     "99-54-7", -2.83  , -3.46  , -0.63, "2530")
    CALL CalcH(1236, "methyl salicylate",                              "119-36-8", -4.64  , -2.38  ,  2.26, "2530")
    CALL CalcH(1237, "phenol, 2-fluoro-",                              "367-12-4", -3.76  , -3.88  , -0.12, "984")
    CALL CalcH(1238, "phenol, 3-fluoro-",                              "372-20-3", -4.35)
    CALL CalcH(1239, "phenol, 4-fluoro-",                              "371-41-5", -4.29  , -4.54  , -0.25, "984")
    CALL CalcH(1240, "phenol, 2,6-di-fluoro-",                       "28177-48-2", -3.24)
    CALL CalcH(1241, "phenol, 2-chloro-",                               "95-57-8", -4.02  , -3.47  ,  0.55, "usepa82")
    CALL CalcH(1242, "phenol, 3-chloro-",                              "108-43-0", -4.60  , -4.85  , -0.25, "984")
    CALL CalcH(1243, "phenol, 4-chloro",                               "106-48-9", -4.52  , -4.57  , -0.05, "2530")
    CALL CalcH(1244, "phenol, 4-chloro-2-methyl-",                    "1570-64-5", -4.59  , -5.32  , -0.73, "2530")
    CALL CalcH(1245, "phenol, 4-chloro-3-methyl-",                      "59-50-7", -4.48  , -4.98  , -0.50, "984")
    CALL CalcH(1246, "phenol, 2,4-dichloro",                           "120-83-2", -4.31  , -3.57  ,  0.74, "usepa82")
    CALL CalcH(1247, "phenol, 3,5-dichloro-",                          "591-35-5", -5.06  , -4.98  ,  0.08, "2530")
    CALL CalcH(1248, "phenol, 2,4,5-trichloro-",                        "95-95-4", -4.70  , -4.16  ,  0.54, "2530")
    CALL CalcH(1249, "phenol, 2,4,6-trichloro-",                        "88-06-2", -3.73  , -3.96  , -0.23, "2530")
    CALL CalcH(1250, "phenol, 2-bromo-",                                "95-56-7", -4.02)
    CALL CalcH(1251, "phenol, 3-bromo-",                               "591-20-8", -4.76)
    CALL CalcH(1252, "phenol, 4-bromo-",                               "106-41-2", -4.61  , -5.2   , -0.59, "1501")
    CALL CalcH(1253, "phenol, 2-iodo-",                                "533-58-4", -4.23  , -4.55  , -0.32, "984")
    CALL CalcH(1254, "phenol, 3-iodo-",                                "626-02-8", -5.24)
    CALL CalcH(1255, "phenol, 4-iodo-",                                "540-38-5", -5.06)
    CALL CalcH(1256, "phenol, 2-nitro-",                                "88-75-5", -4.12  , -3.24  ,  0.88, "1501")
    CALL CalcH(1257, "phenol, 4-nitro-",                               "100-02-7", -7.18  , -7.81  , -0.63, "941")
    CALL CalcH(1258, "phenol, 3-nitro-",                               "554-84-7", -7.37  , -7.06  ,  0.31, "1501")
    CALL CalcH(1259, "phenol, 2-cyano",                                "611-20-1", -4.84)
    CALL CalcH(1260, "phenol, 3-cyano",                                "873-62-1", -8.00  , -7.08  ,  0.92, "1501")
    CALL CalcH(1261, "phenol, 4-cyano",                                "767-00-0", -7.53  , -7.46  ,  0.07, "1501")
    CALL CalcH(1262, "molinate",                                      "2212-67-1", -4.26  , -4.11  ,  0.15, "1142")
    CALL CalcH(1263, "chlordane",                                       "57-74-9", -2.12  , -2.47  , -0.35, "901")
    ! toxaphene is a mixture, not a pure substance:
    ! CALL CalcH(1264, "toxaphene",                                     "8001-35-2", -4.56  , -3.86  ,  0.70, "853")
    CALL CalcH(1265, "heptachlor epoxides",                           "1024-57-3", -4.26  , -3.13  ,  1.13, "C")
    CALL CalcH(1266, "heptachlor",                                      "76-44-8", -1.78  , -1.22  ,  0.56, "1195")
    CALL CalcH(1267, "mirex",                                         "2385-85-5", -0.98  , -1.46  , -0.48, "852")
    CALL CalcH(1268, "aldrin",                                         "309-00-2", -1.33  , -1.7   , -0.37, "C")
    CALL CalcH(1269, "endrin",                                          "72-20-8", -3.45  , -3.35  ,  0.10, "2530")
    CALL CalcH(1270, "dieldrin",                                        "60-57-1", -3.45  , -2.62  ,  0.83, "C")
    CALL CalcH(1271, "alachlor",                                     "15972-60-8", -6.88  , -6.47  ,  0.41, "901")
    CALL CalcH(1272, "ddt",                                             "50-29-3", -3.22  , -3.45  , -0.23, "695")
    CALL CalcH(1273, "parathion",                                       "56-38-2", -4.21  , -5.55  , -1.34, "479")
    CALL CalcH(1274, "malathion",                                      "121-75-5", -5.58  , -6.7   , -1.12, "479")
    CALL CalcH(1275, "chlorpyrifos, methyl",                          "5598-13-0", -3.21  , -3.79  , -0.58, "479")
    CALL CalcH(1276, "chlorpyrifos",                                  "2921-88-2", -2.74  , -3.10  , -0.36, "1157")
    CALL CalcH(1277, "fenitrothion",                                   "122-14-5", -4.12  , -4.4   , -0.28, "479")
    CALL CalcH(1278, "dicapthon",                                     "2463-84-5", -4.21  , -5.03  , -0.82, "479")
    CALL CalcH(1279, "ronnel",                                         "299-84-3", -2.15  , -3.07  , -0.92, "479")
    CALL CalcH(1280, "leptophos",                                    "21609-90-5", -4.81  , -3.97  ,  0.84, "479")
    CALL CalcH(1281, "butachlor",                                    "23184-66-9", -6.23  , -5.68  ,  0.55, "2538")
    CALL CalcH(1282, "chlorothalonil",                                "1897-45-6", -4.16  , -4.1   ,  0.06, "2229")
    CALL CalcH(1283, "1-naphthalenol, methylcarbamate",                 "63-25-2", -6.53  , -6.75  , -0.22, "howard97")
    CALL CalcH(1284, "pendimethalin",                                "40487-42-1", -4.08  , -4.46  , -0.38, "1144")
    CALL CalcH(1285, "pentachloronitrobenzene",                         "82-68-8", -2.23  , -2.71  , -0.48, "howard97")
    CALL CalcH(1286, "methazole",                                    "20354-26-1", -8.08  , -4.9   ,  3.18, "2539")
    CALL CalcH(1287, "carvone",                                       "6485-40-1", -3.30)
    CALL CalcH(1288, "bifenthrin",                                   "82657-04-3", -4.07  , -4.39  , -0.32, "C")
    CALL CalcH(1289, "butylate",                                      "2008-41-5", -3.16  , -2.44  ,  0.72, "2530")
    CALL CalcH(1290, "eucalyptol",                                     "470-82-6", -1.73  , -2.33  , -0.60, "2530")
    CALL CalcH(1291, "chlorfluazuron",                               "71422-67-8", -11.98 , -10.12 ,  1.86, "2530")
    CALL CalcH(1292, "2,4-d, ethyl ester",                             "533-23-3", -3.47  , -3.86  , -0.39, "2530")
    CALL CalcH(1293, "2,4-d, methyl ester",                           "1928-38-7", -3.64  , -3.93  , -0.29, "2530")
    CALL CalcH(1294, "dimethenamid",                                 "87674-68-8", -6.05  , -5.46  ,  0.59, "2530")
    CALL CalcH(1295, "s-ethyldipropylthiocarbamate",                   "759-94-4", -3.31  , -3.17  ,  0.14, "2530")
    CALL CalcH(1296, "ethyl chloroacetate",                            "105-39-5", -2.45  , -2.76  , -0.31, "2530")
    CALL CalcH(1297, "cycloate",                                      "1134-23-2", -3.96  , -3.54  ,  0.42, "2530")
    CALL CalcH(1298, "endosulfan",                                     "115-29-7", -4.89  , -4.78  ,  0.11, "2530")
    CALL CalcH(1299, "beta-endosulfan",                              "33213-65-9", -4.89  , -4.78  ,  0.11, "2530")
    CALL CalcH(1300, "fluquinconazole",                             "136426-54-5", -12.14 , -8.89  ,  3.25, "2530")
    CALL CalcH(1301, "methoxychlor",                                    "72-43-5", -3.84  , -5.06  , -1.22, "2530")
    CALL CalcH(1302, "metolachlor",                                  "51218-45-2", -7.19  , -6.44  ,  0.75, "2530")
    CALL CalcH(1303, "naproanilide",                                 "52570-16-8", -8.60  , -9.18  , -0.58, "2530")
    CALL CalcH(1304, "paraldehyde",                                    "123-63-7", -2.95  , -3.14  , -0.19, "2530")
    CALL CalcH(1305, "pebulate",                                      "1114-71-2", -3.20  , -1.99  ,  1.21, "2530")
    CALL CalcH(1306, "oxirane, phenoxymethyl-",                        "122-60-1", -3.18  , -4.46  , -1.28, "2530")
    CALL CalcH(1307, "piperonal",                                      "120-57-0", -6.01  , -4.62  ,  1.39, "2530")
    CALL CalcH(1308, "plinol",                                       "11039-70-6", -3.49  , -3.04  ,  0.45, "2530")
    CALL CalcH(1309, "pretilchlor",                                  "51218-49-6", -7.05  , -6.3   ,  0.75, "2530")
    CALL CalcH(1310, "propamocarb",                                  "24579-73-5", -6.59  , -7.2   , -0.61, "2530")
    CALL CalcH(1311, "propylene glycol methyl ether acetate",          "108-65-6", -3.39  , -3.83  , -0.44, "2530")
    CALL CalcH(1312, "sulfallate",                                      "95-06-7", -4.71  , -3.56  ,  1.15, "2530")
    CALL CalcH(1313, "tetrahydropyran-2-methanol",                     "100-72-1", -5.35  , -6.19  , -0.84, "2530")
    CALL CalcH(1314, "trifluralin",                                   "1582-09-8", -3.63  , -2.17  ,  1.46, "2530")
    CALL CalcH(1315, "alpha-terpineols",                                "98-55-5", -3.95  , -3.22  ,  0.73, "2530")
    CALL CalcH(1316, "vernolate",                                     "1929-77-7", -3.21  , -2.88  ,  0.33, "2530")
    CALL CalcH(1317, "parathion methyl",                               "298-00-0", -4.57  , -5.37  , -0.80, "2454")
    CALL CalcH(1318, "ancymidol",                                    "12771-68-5", -10.07 , -9.21  ,  0.86, "2539")
    CALL CalcH(1319, "simazine",                                       "122-34-9", -6.63  , -7.48  , -0.85, "2526")
    CALL CalcH(1320, "atrazine",                                      "1912-24-9", -6.25  , -7.0   , -0.75, "2526")
    CALL CalcH(1321, "propazine",                                      "139-40-2", -5.95  , -7.15  , -1.20, "2526")
    CALL CalcH(1322, "terbuthylazine",                                "5915-41-3", -5.86  , -5.72  ,  0.14, "2526")
    CALL CalcH(1323, "cyanazine",                                    "21725-46-2", -9.20  , -9.72  , -0.52, "2526")
    CALL CalcH(1324, "desmetryn",                                     "1014-69-3", -7.74  , -7.7   ,  0.04, "2526")
    CALL CalcH(1325, "simetryn",                                      "1014-70-6", -7.85  , -7.72  ,  0.13, "2526")
    CALL CalcH(1326, "ametryne",                                       "834-12-8", -7.47  , -7.17  ,  0.30, "2526")
    CALL CalcH(1327, "prometryn",                                     "7287-19-6", -6.27  , -6.45  , -0.18, "2526")
    CALL CalcH(1328, "terbutryn",                                      "886-50-0", -7.05  , -6.23  ,  0.82, "2526")
    CALL CalcH(1329, "dimethamrtryne",                               "22936-75-0", -7.31  , -6.41  ,  0.90, "2526")
    CALL CalcH(1330, "dipropetryn",                                   "4147-51-7", -6.17  , -6.08  ,  0.09, "2526")
    CALL CalcH(1331, "metoprotryne",                                   "841-06-5", -8.57  , -7.88  ,  0.69, "2526")
    CALL CalcH(1332, "simetone",                                       "673-04-1", -7.58  , -8.09  , -0.51, "2526")
    CALL CalcH(1333, "atratone",                                      "1610-17-9", -7.20  , -7.24  , -0.04, "2526")
    CALL CalcH(1334, "prometon",                                      "1610-18-0", -6.83  , -7.27  , -0.44, "2526")
    CALL CalcH(1335, "terbumeton",                                   "33693-04-8", -6.78  , -6.72  ,  0.06, "2526")
    CALL CalcH(1336, "secbumeton",                                   "26259-45-0", -7.09  , -6.84  ,  0.25, "2526")

  CONTAINS

    SUBROUTINE CalcH(number, chem_, casrn_, calc, obs, diff, xref)

      IMPLICIT NONE
      INTEGER,                    INTENT(IN) :: number
      CHARACTER(LEN=*),           INTENT(IN) :: chem_
      CHARACTER(LEN=*),           INTENT(IN) :: casrn_
      REAL,                       INTENT(IN) :: calc
      REAL,             OPTIONAL, INTENT(IN) :: obs, diff
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: xref ! ref for experimental value

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      IF (PRESENT(obs)) THEN
        IF (ABS(obs-calc-diff)>10.*EPSILON(0.)) THEN
          CALL PrintWarning("Incorrect diff. for number "//TRIM(str(number)))
        ENDIF
        ! use value only if it is in a paper that I don't have:
        IF (unread_bib(xref)) THEN
          IF (xref=="C") THEN
            type = "C"
            CALL Output(KHcc_TIMES_HcpSI_atT0/10.**obs)
          ELSE
            IF ((xref/="?").AND.(xref/="wrongref")) THEN
              CALL SettypeX(xref)
              CALL Output(KHcc_TIMES_HcpSI_atT0/10.**obs)
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      type = "Q"
      CALL Output(KHcc_TIMES_HcpSI_atT0/10.**calc)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2521

  !---------------------------------------------------------------------------

  SUBROUTINE ref2522 ! KHcc [1]
    IMPLICIT NONE

    ref = "2522"
    type = "M"

    CALL CalcH("methyl {tert}-butyl ether", "1634-04-4", 6.8475, 2901.4) ! CH3OC(CH3)3 MTBE
    CALL CalcH("benzene",                     "71-43-2", 8.1648, 2889.4) ! C6H6
    CALL CalcH("trichloroethene",             "79-01-6", 9.9697, 3287.5) ! C2HCl3 trichloroethylene
    CALL CalcH("methylbenzene",              "108-88-3", 9.8077, 3345.9) ! C6H5CH3 toluene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / (EXP(A-B/T0))
      mindHR = B + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2522

  !---------------------------------------------------------------------------

  SUBROUTINE ref2524 ! KHcc [1]
    IMPLICIT NONE

    ref = "2524"
    type = "M"

    CALL CalcH("ethanal",           "75-07-0", -6088., 14.49) ! CH3CHO acetaldehyde
    CALL CalcH("propanal",         "123-38-6", -5546., 12.86) ! C2H5CHO propionaldehyde
    CALL CalcH("butanal",          "123-72-8", -5859., 14.26) ! C3H7CHO butyraldehyde
    CALL CalcH("pentanal",         "110-62-3", -5848., 14.44) ! C4H{9}CHO valeraldehyde
    CALL CalcH("2-methylpropenal",  "78-85-3", -4002.,  8.65) ! C4H6O methacrolein
    CALL CalcH("propanone",         "67-64-1", -5052., 10.46) ! CH3COCH3 acetone
    CALL CalcH("butanone",          "78-93-3", -4905., 10.18) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("2-pentanone",      "107-87-9", -5380., 12.04) ! C3H7COCH3
    CALL CalcH("3-pentanone",       "96-22-0", -5316., 11.87) ! C2H5COC2H5
    CALL CalcH("3-buten-2-one",     "78-94-4", -4534.,  8.74) ! C4H6O methyl vinyl ketone; MVK
    CALL CalcH("ethane nitrile",    "75-05-8", -3735.,  5.37) ! CH3CN acetonitrile
    CALL CalcH("propane nitrile",  "107-12-0", -4350.,  7.87) ! C2H5CN propionitrile
    CALL CalcH("butane nitrile",   "109-74-0", -4816.,  9.64) ! C3H7CN butyronitrile

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / (EXP(A/T0+B))
      mindHR = -A + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2524

  !---------------------------------------------------------------------------

  SUBROUTINE ref2525 ! KHcc [1]
    IMPLICIT NONE

    ref = "2525"
    type = "M"

    CALL CalcH("aminobenzene",                "62-53-3", -4.09) ! C6H7N aniline
    CALL CalcH("4-methylaniline",            "106-49-0", -4.04) ! C7H9N $p$-toluidine
    CALL CalcH("3,4-dimethylbenzenamine",     "95-64-7", -4.12)
    CALL CalcH("2,4,5-trimethylbenzenamine", "137-17-7", -3.98)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / (10.**logH)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2525

  !---------------------------------------------------------------------------

  SUBROUTINE ref2526 ! Hcc [1]
    IMPLICIT NONE

    ref = "2526"
    type = "Q"

    CALL CalcH("simazine",         "122-34-9", 7.25)
    CALL CalcH("atrazine",        "1912-24-9", 7.10)
    CALL CalcH("propazine",        "139-40-2", 7.00)
    CALL CalcH("terbuthylazine",  "5915-41-3", 6.35)
    CALL CalcH("cyanazine",      "21725-46-2", 9.70)
    CALL CalcH("desmetryn",       "1014-69-3", 7.70)
    CALL CalcH("simetryn",        "1014-70-6", 7.40)
    CALL CalcH("ametryn",          "834-12-8", 7.10)
    CALL CalcH("prometryn",       "7287-19-6", 6.80)
    CALL CalcH("terbutryn",        "886-50-0", 6.60)
    CALL CalcH("dimethametryn",  "22936-75-0", 6.40)
    CALL CalcH("dipropetryn",     "4147-51-7", 6.60)
    CALL CalcH("aziprotryn",      "4658-28-0", 6.00)
    CALL CalcH("methoprotryn",     "841-06-5", 8.70)
    CALL CalcH("simeton",          "673-04-1", 7.80)
    CALL CalcH("atraton",         "1610-17-9", 7.45)
    CALL CalcH("prometon",        "1610-18-0", 7.10)
    CALL CalcH("terbumeton",     "33693-04-8", 6.60)
    CALL CalcH("secbumeton",     "26259-45-0", 7.25)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: logKw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI_atT0 * 10.**logKw
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2526

  !---------------------------------------------------------------------------

  SUBROUTINE ref2527 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2527"
    type = "C"

    CALL CalcH("methanal",            "50-00-0", 4.6E3) ! HCHO formaldehyde
    CALL CalcH("2-hydroxyethanal",   "141-46-8", 1.0E5) ! HOCH2CHO hydroxyacetaldehyde
    CALL CalcH("ethanedial",         "107-22-2", 1.4E6) ! OHCCHO glyoxal
    CALL CalcH("propanonal",          "78-98-8", 3.7E4) ! CH3COCHO methylglyoxal; pyruvaldehyde
    CALL CalcH("1-hydroxypropanone", "116-09-6", 7.8E3) ! CH3COCH2OH hydroxyacetone

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("295")
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      IF (casrn_=="107-22-2") CALL MakeNote("RCHOdiol")
      CALL Output(Hcp_TO_HcpSI * H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2527

  !---------------------------------------------------------------------------

  SUBROUTINE ref2529 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2529"

    CALL CalcH("hexachlorocyclopentadiene", "77-47-4", 0.027, 0.016)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc_M, KHpc_V)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpc_M
      REAL,             INTENT(IN) :: KHpc_V

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      CALL Output(1. / (atm*KHpc_M))
      type = "V"
      CALL Output(1. / (atm*KHpc_V))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2529

  !---------------------------------------------------------------------------

  SUBROUTINE ref2531 ! KHcc [1]
    IMPLICIT NONE

    ref = "2531"
    type = "Q"

    CALL CalcH("N,N-dimethylmethanamide",  "68-12-2", -5.60) ! C3H7NO N,N-dimethylformamide
    CALL CalcH("propanone",                "67-64-1", -2.79) ! CH3COCH3 acetone
    CALL CalcH("N,N-dimethylacetamide",   "127-19-5", -5.95) ! C4H9NO
    CALL CalcH("dimethylsulfoxide",        "67-68-5", -7.22) ! CH3SOCH3 DMSO
    CALL CalcH("pentyl ethanoate",        "628-63-7", -1.76) ! CH3COOC5H{11} amyl acetate
    CALL CalcH("methanol",                 "67-56-1", -3.77) ! CH3OH
    CALL CalcH("2-propanol",               "67-63-0", -3.52) ! C3H7OH isopropanol

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKgw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logKgw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / (10.**logKgw)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2531

  !---------------------------------------------------------------------------

  SUBROUTINE ref2532 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2532"
    type = "Q"

    CALL CalcH("benz[$a$]anthracene",      "56-55-3", 0.18)  ! C{18}H{12}
    CALL CalcH("perylene",                "198-55-0", 0.095) ! C{20}H{12} dibenz[$de$,$kl$]anthracene
    CALL CalcH("naphthacene",              "92-24-0", 0.24)  ! C{18}H{12} 2,3-benzanthracene
    CALL CalcH("triphenylene",            "217-59-4", 0.32)  ! C{18}H{12} benzo[$l$]phenanthrene
    CALL CalcH("benzo[$e$]pyrene",        "192-97-2", 0.068) ! C{20}H{12}
    CALL CalcH("picene",                  "213-46-7", 0.013) ! C{22}H{14}
    CALL CalcH("dibenz[$a,h$]anthracene",  "53-70-3", 0.012) ! C{22}H{14}
    CALL CalcH("dibenz[$a,j$]anthracene", "224-41-9", 0.012) ! C{22}H{14}
    CALL CalcH("dibenz[$a,c$]anthracene", "215-58-7", 0.007) ! C{22}H{14}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("293")
      CALL Output(KHpcSI_TIMES_HcpSI/H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2532

  !---------------------------------------------------------------------------

  SUBROUTINE ref2533 ! KHcc [1]
    IMPLICIT NONE

    ref = "2533"
    type = "M"

    CALL CalcH("hexachlorobenzene", "118-74-1", 29.E-3)   ! C6Cl6
    CALL CalcH("dibutyl phthalate",  "84-74-2", 0.074E-3) ! C{16}H{22}O4

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      CALL MakeNoteOtherTemp("296")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2533

  !---------------------------------------------------------------------------

  SUBROUTINE ref2534 ! KHpx [atm]
    IMPLICIT NONE

    chem = "oxirane" ; casrn = "75-21-8" ! C2H4O ethylene oxide
    ref = "2534"
    type = "M"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10., 20., 30. /) + CtoK
    Harray = KHpx_TIMES_HcpSI / (/ 5.3, 7.9, 11.2 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2534

  !---------------------------------------------------------------------------

  SUBROUTINE ref2536 ! KHcc [1]
    IMPLICIT NONE

    ref = "2536"
    type = "R"

    CALL CalcH("bde-15",    "2050-47-7", -2.78)
    CALL CalcH("bde-28",   "41318-75-6", -3.11)
    CALL CalcH("bde-47",    "5436-43-1", -3.35)
    CALL CalcH("bde-99",   "60348-60-9", -3.67)
    CALL CalcH("bde-100", "189084-64-8", -3.81)
    CALL CalcH("bde-153",  "68631-49-2", -3.86)

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, lokKAW)
      IMPLICIT NONE
      CHARACTER(LEN=*),           INTENT(IN) :: chem_
      CHARACTER(LEN=*),           INTENT(IN) :: casrn_
      REAL,                       INTENT(IN) :: lokKAW
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0/10.**lokKAW)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2536

  !---------------------------------------------------------------------------

  SUBROUTINE ref2537 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2537"
    type = "M"

    CALL CalcH("naphthalene",     "91-20-3", 3.76E-4) ! C{10}H8
    CALL CalcH("anthracene",     "120-12-7", 3.65E-5) ! C{14}H{10}
    CALL CalcH("1,3,6,8-TCDD", "33423-92-6", 6.81E-5) ! C{12}H4Cl4O2 PCDD-1368

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(1. / (atm*KHpc))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2537

  !---------------------------------------------------------------------------

  SUBROUTINE ref2538 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2538"
    type = "M"

    CALL CalcH("butachlor",    "23184-66-9", 6.1E-3) ! C{17}H{26}ClNO2
    CALL CalcH("carbaryl",        "63-25-2", 2.8E-4) ! C{12}H{11}NO2
    CALL CalcH("chlorpropham",   "101-21-3", 4.3E-2) ! C{10}H{12}ClNO2
    CALL CalcH("diazinon",       "333-41-5", 9.1E-2) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("edifenphos",   "17109-49-8", 2.0E-4)
    CALL CalcH("fenitrothion",   "122-14-5", 1.2E-2) ! C9H{12}NO5PS
    CALL CalcH("fenobucarb",    "3766-81-2", 6.5E-3)
    CALL CalcH("iprobenphos",  "26087-47-8", 3.9E-3)
    CALL CalcH("malathion",      "121-75-5", 1.5E-3) ! C{10}H{19}O6PS2
    CALL CalcH("molinate",      "2212-67-1", 1.3E-1) ! C9H{17}NOS
    CALL CalcH("thiobencarb",  "28249-77-6", 5.2E-2) ! C{12}H{16}ClNOS
    CALL CalcH("trifluralin",   "1582-09-8", 1.1   ) ! C{13}H{16}F3N3O4
    CALL CalcH("xylylcarb",     "2425-10-7", 1.1E-2)

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

  END SUBROUTINE ref2538

  !---------------------------------------------------------------------------

  SUBROUTINE ref2540 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2540"
    type = "L"

    ! H from Tabs. 17 and 18, Delta_UAW from Tabs. 19 and 20:
    CALL CalcH("PCB-3",    "2051-62-9", 36.0, 23.6, 44.9, 48.5)
    CALL CalcH("PCB-8",   "34883-43-7", 26.2, 22.8, 47.4, 49.8)
    CALL CalcH("PCB-15",   "2050-68-2", 14.2, 13.4, 47.4, 53.4)
    CALL CalcH("PCB-28",   "7012-37-5", 33.1, 30.5, 50.0, 52.3)
    CALL CalcH("PCB-29",  "15862-07-4", 32.6, 30.2, 49.9, 53.6)
    CALL CalcH("PCB-31",  "16606-02-3", 36.8, 34.2, 48.6, 52.8)
    CALL CalcH("PCB-52",  "35693-99-3", 28.2, 25.1, 52.0, 54.1)
    CALL CalcH("PCB-61",  "33284-53-6", 20.6, 20.0, 52.3, 57.8)
    CALL CalcH("PCB-101", "37680-73-2", 31.4, 24.1, 54.4, 59.7)
    CALL CalcH("PCB-105", "32598-14-4", 33.6, 13.8, 54.4, 60.1)
    CALL CalcH("PCB-118", "31508-00-6", 32.0, 14.5, 54.4, 60.5)
    CALL CalcH("PCB-138", "35065-28-2", 39.5, 30.1, 56.2, 61.3)
    CALL CalcH("PCB-153", "35065-27-1", 25.0, 19.8, 56.2, 62.8)
    CALL CalcH("PCB-155", "33979-03-2", 76.5, 91.4, 56.3, 60.6)
    CALL CalcH("PCB-180", "35065-29-3", 5.84, 8.13, 57.9, 63.6)
    CALL CalcH("PCB-194", "35694-08-7", 6.79, 4.40, 59.5, 65.8)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H_LDV, H_FAV, Delta_UAW_LDV, Delta_UAW_FAV)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H_LDV, H_FAV, Delta_UAW_LDV, Delta_UAW_FAV
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/H_LDV
      mindHR = 1E3*Delta_UAW_LDV/Rgas + T0 ! eqn (5) on p. 1585 of ref2540
      CALL MakeNote("LDV", "Literature-derived value.")
      CALL Output(Hominus, mindHR)
      Hominus    = KHpcSI_TIMES_HcpSI/H_FAV
      mindHR = 1E3*Delta_UAW_FAV/Rgas + T0 ! eqn (5) on p. 1585 of ref2540
      CALL MakeNote("FAV", "Final adjusted value.")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2540

  !---------------------------------------------------------------------------

  SUBROUTINE ref2542 ! KHpc [atm*L/mol]
    IMPLICIT NONE

    ref = "2542"
    type = "Q"

    CALL CalcH("chlorobenzene",              "108-90-7",  0.81) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",         "95-50-1",  0.64) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",        "541-73-1",  0.64) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",        "106-46-7",  0.64) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,2,3-trichlorobenzene",      "87-61-6",  0.45) ! C6H3Cl3
    CALL CalcH("1,2,4-trichlorobenzene",     "120-82-1",  0.45) ! C6H3Cl3
    CALL CalcH("1,3,5-trichlorobenzene",     "108-70-3",  0.45) ! C6H3Cl3
    CALL CalcH("1,2,3,4-tetrachlorobenzene", "634-66-2",  0.24) ! C6H2Cl4
    CALL CalcH("1,2,3,5-tetrachlorobenzene", "634-90-2",  0.24) ! C6H2Cl4
    CALL CalcH("1,2,4,5-tetrachlorobenzene",  "95-94-3",  0.24) ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",         "608-93-5",  0.02) ! C6HCl5
    CALL CalcH("hexachlorobenzene",          "118-74-1", -0.22) ! C6Cl6

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, predicted)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: predicted
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI / (10.**predicted))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2542

  !---------------------------------------------------------------------------

  SUBROUTINE ref2549 ! KHpx [atm]
    IMPLICIT NONE
    ref = "2549"
    type = "M"
    chem = "ozone" ; casrn = "10028-15-6" ! O3
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" found that $\H$ depends "// &
      "on the concentration of \chem{OH^-}.")
    CALL Output(DUMMY)
  END SUBROUTINE ref2549

  !---------------------------------------------------------------------------

  SUBROUTINE ref2550 ! special definition (-lg s and p_sat)
    IMPLICIT NONE

    ref = "2550"

    CALL CalcH("ethane",                         "74-84-0", MC*2.+MH*6.,           ps=54.,                                ber=2.75)
    CALL CalcH("propane",                        "74-98-6", MC*3.+MH*8.,           ps=9.6,                      exp=3.26, ber=3.25)
    CALL CalcH("butane",                        "106-97-8", MC*4.+MH*10.,          ps=2.5,                      exp=3.75, ber=3.75)
    CALL CalcH("isobutane",                      "75-28-5", MC*4.+MH*10.,          ps=3.5,                      exp=3.7,  ber=3.65)
    CALL CalcH("ethyne",                         "74-86-2", MC*2.+MH*2.,           ps=48.,                                ber=1.2)
    CALL CalcH("propyne",                        "74-99-7", MC*3.+MH*4.,           ps=5.8,                      exp=1.81, ber=1.70)
    CALL CalcH("propene",                       "115-07-1", MC*3.+MH*6.,           ps=11.5,                     exp=2.66, ber=2.65)
    CALL CalcH("cyclopropane",                   "75-19-4", MC*3.+MH*6.,           ps=6.4,                      exp=2.45          )
    CALL CalcH("butadiyne",                     "460-12-8", MC*4.+MH*2.,                    gkg=10.                               )
    CALL CalcH("1-butyne",                      "107-00-6", MC*4.+MH*6.,           ps=1.8,                                ber=2.20)
    CALL CalcH("1,3-butadiene",                 "106-99-0", MC*4.+MH*6.,           ps=2.7,  gkg=0.85,           exp=2.75          )
    CALL CalcH("1-butene",                      "106-98-9", MC*4.+MH*8.,           ps=3.0,                      exp=3.15, ber=3.15)
    CALL CalcH("{cis}-2-butene",                "590-18-1", MC*4.+MH*8.,           ps=2.1,                      exp=3.18, ber=3.15)
    CALL CalcH("{trans}-2-butene",              "624-64-6", MC*4.+MH*8.,           ps=2.3,                      exp=3.29, ber=3.15)
    CALL CalcH("bromomethane",                   "74-83-9", MC+MH*3.+MBr,          ps=2.5,                                ber=1.72)
    CALL CalcH("chloromethane",                  "74-87-3", MC+MH*3.+MCl,          ps=5.7,                                ber=1.60)
    CALL CalcH("chloroethene",                   "75-01-4", MC*2.+MH*3.+MCl,       ps=3.9,                                ber=1.70)
    CALL CalcH("chloroethane",                   "75-00-3", MC*2.+MH*5.+MCl,       ps=1.6,  gkg=4.1,                      ber=2.10)
    CALL CalcH("tetrafluoromethane",             "75-73-0", MC+MF*4.,              ps=166., gkg=0.017,                    ber=2.62)
    CALL CalcH("bromotrifluoromethane",          "75-63-8", MC+MF*3.+MBr,          ps=17.,  gkg=0.3,                      ber=2.84)
    CALL CalcH("chlorotrifluoromethane",         "75-72-9", MC+MF*3.+MCl,          ps=35.,  gkg=0.06,                     ber=2.72)
    CALL CalcH("dichlorodifluoromethane",        "75-71-8", MC+MF*2.+MCl*2.,       ps=6.4,  gkg=0.28,                     ber=2.81)
    CALL CalcH("trichlorofluoromethane",         "75-69-4", MC+MF+MCl*3.,          ps=1.03,                     exp=2.85, ber=2.91)
    CALL CalcH("trifluoromethane",               "75-46-7", MC+MH+MF*3.,           ps=49.,  gkg=0.74,                     ber=1.42)
    CALL CalcH("chlorodifluoromethane",          "75-45-6", MC+MH+MF*2.+MCl,       ps=10.,                      exp=1.55, ber=1.51)
    CALL CalcH("chlorofluoromethane",           "593-70-4", MC+MH*2.+MF+MCl,       ps=3.4,                                ber=1.36)
    CALL CalcH("fluoromethane",                 "593-53-3", MC+MH*3.+MF,           ps=38.,                                ber=1.11)
    CALL CalcH("tetrafluoroethene",             "116-14-3", MC*2.+MF*4.,                    gkg=0.1,   T="303"                    )
    CALL CalcH("chloropentafluoroethane",        "76-15-3", MC*2.+MF*5.+MCl,       ps=9.,   gkg=0.05,                     ber=3.52)
    CALL CalcH("1,1-dichlorotetrafluoroethane", "374-07-2", MC*2.+MF*4.+MCl*2.,    ps=2.1,  gkg=0.1,   T="294",           ber=3.62)
    CALL CalcH("1,2-dichlorotetrafluoroethane",  "76-14-2", MC*2.+MF*4.+MCl*2.,    ps=2.1,  gkg=0.14,  T="293",           ber=3.62)
    CALL CalcH("2-chloro-1,1,1-trifluoroethane", "75-88-7", MC*2.+MH*2.+MF*3.+MCl, ps=1.9,  gkg=4.4,                      ber=2.18)
    CALL CalcH("1-chloro-1,1-difluoroethane",    "75-68-3", MC*2.+MH*3.+MF*2.+MCl, ps=3.3,  gkg=1.9,   T="294",           ber=2.31)
    CALL CalcH("1,1-difluoroethane",             "75-37-6", MC*2.+MH*4.+MF*2.,     ps=6.,   gkg=3.2,   T="294",           ber=1.76)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, M, ps, gkg, T, exp, ber)
      IMPLICIT NONE
      CHARACTER(LEN=*),             INTENT(IN) :: chem_
      CHARACTER(LEN=*),             INTENT(IN) :: casrn_
      REAL,                         INTENT(IN) :: M      ! molar mass (g/mol)
      REAL,             OPTIONAL,   INTENT(IN) :: ps     ! p_sat
      REAL,             OPTIONAL,   INTENT(IN) :: gkg    ! solubility in g/kg (per at)
      CHARACTER(LEN=*), OPTIONAL,   INTENT(IN) :: T      ! other temperature
      REAL,             OPTIONAL,   INTENT(IN) :: exp    ! experimental -lg(s)
      REAL,             OPTIONAL,   INTENT(IN) :: ber    ! berechnet -lg(s)

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      IF (PRESENT(ps)) THEN
        IF (PRESENT(exp)) THEN
          type = "V"
          Hominus = Hcp_TO_HcpSI * rhoH2O / (10.**exp * 1E3 * M * ps)
          CALL Output(Hominus)
        ENDIF
        IF (PRESENT(ber)) THEN
          type = "Q"
          Hominus = Hcp_TO_HcpSI * rhoH2O / (10.**ber * 1E3 * M * ps)
          CALL Output(Hominus)
        ENDIF
      ENDIF

      IF (PRESENT(gkg)) THEN
        IF (PRESENT(T)) THEN
          CALL MakeNoteOtherTemp(T)
        ENDIF
        type = "C"
        Hominus = Hcp_TO_HcpSI * gkg * rhoH2O / (1E6 * M)
        CALL Output(Hominus)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref2550

  !---------------------------------------------------------------------------

  SUBROUTINE ref2551 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "2551"
    type = "M"

    CALL CalcH("propyne",        "74-99-7", 1090., -3.43, & ! methyl acetylene
      (/ 0., 0., 30., 30., 45., 45., 60. /), &
      (/ 3.64, 3.90, 1.42, 1.53, 1.03, 1.04, 0.76 /))
    CALL CalcH("1-butyne",      "107-00-6",  833., -2.60, & ! ethyl acetylene
      (/ 0., 30., 30., 60. /), &
      (/ 2.87, 1.70, 1.39, 0.80 /))
    CALL CalcH("3-buten-1-yne", "689-97-4",  767., -2.64, & ! vinyl acetylene
      (/ 0., 0., 30., 30., 45., 45., 60. /), &
      (/ 1.47, 1.50, 0.76, 0.78, 0.59, 0.60, 0.47 /))

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a, b, temp_, alpha)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: a, b
      REAL, DIMENSION(:), INTENT(IN) :: temp_, alpha
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp_+CtoK, alpha*alpha_TO_HcpSI, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      ! compare with regression made by the authors:
      ! lg(alpha) = a/T + b with alpha = Bunsen coefficient
      CALL consistency_check(10.**(a/T0+b) * alpha_TO_HcpSI, Hominus, &
        "Regression and individual data points")
      CALL consistency_check(a * LOG(10.), mindHR, &
        "Regression and individual data points of temperature dependence")
    END SUBROUTINE CalcH

  END SUBROUTINE ref2551

  !---------------------------------------------------------------------------

  SUBROUTINE ref2552 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2552"
    type = "M"

    CALL CalcH("chloromethane",         "74-87-3", &
      (/ 283.4, 296.7, 310.1, 310.6, 332.4 /), &
      (/ 0.167, 0.0979, 0.0714, 0.0691, 0.0452 /) ) ! CH3Cl methyl chloride
    CALL CalcH("chlorofluoromethane",  "593-70-4", &
      (/ 283.4, 307.6, 332.4, 352.3 /), &
      (/ 0.244, 0.116, 0.0666, 0.0488 /) ) ! CH2FCl R31
    CALL CalcH("chlorodifluoromethane", "75-45-6", &
      (/ 283.4, 296.7, 314.1, 332.4, 352.3 /), &
      (/ 0.0606, 0.0354, 0.0201, 0.0138, 0.0105 /) ) ! CHF2Cl R22

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, K)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, K
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, K*Hcp_TO_HcpSI, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2552

  !---------------------------------------------------------------------------

  SUBROUTINE ref2554 ! KHpx [Pa]
    IMPLICIT NONE

    ref = "2554"
    type = "V"

    CALL CalcH("octane",         "111-65-9", 147E8,  -40.2E3) ! C8H{18}
    CALL CalcH("1-chlorooctane", "111-85-3", 2.97E8, -51.0E3) ! C8H{17}Cl
    CALL CalcH("1-bromooctane",  "111-83-1", 2.35E8, -38.6E3) ! C8H{17}Br

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH, hyd_H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KH, hyd_H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = cH2O / KH
      mindHR = -hyd_H / Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2554

  !---------------------------------------------------------------------------

  SUBROUTINE ref2556 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2556"

    ! (toxaphene 6.3E-2)
    CALL CalcH("heptachlor",                     "76-44-8", "X", 2.3E-3) ! C{10}H5Cl7
    CALL CalcH("lindane",                        "58-89-9", "X", 3.2E-7) ! gamma-hch
    ! aroclor is a mixture, not a pure substance:
    !CALL CalcH("aroclor1254",                 "11097-69-1", "X", 2.3E-3) ! C{12}HxCl{(10-x)}
    CALL CalcH("1-hydroxy-2,4-dimethylbenzene", "105-67-9", "C", 5.9E-7) ! C8H{10}O 2,4-xylenol; 2,4-dimethylphenol
    CALL CalcH("hydroxybenzene",                "108-95-2", "X", 2.7E-7) ! C6H5OH phenol
    CALL CalcH("hydroxypentachlorobenzene",      "87-86-5", "X", 2.1E-6) ! C6HCl5O pentachlorophenol
    CALL CalcH("dimethyl phthalate",            "131-11-3", "X", 4.2E-7) ! C{10}H{10}O4
    CALL CalcH("diethyl phthalate",              "84-66-2", "C", 1.7E-5) ! C{12}H{14}O4
    CALL CalcH("dibutyl phthalate",              "84-74-2", "X", 6.3E-5) ! C{16}H{22}O4
    CALL CalcH("di-(2-ethylhexyl)-phthalate",   "117-81-7", "C", 1.2E-7) ! C{24}H{38}O4
    CALL CalcH("butyl benzyl phthalate",         "85-68-7", "E", 1.0E-6) ! C{19}H{20}O4
    CALL CalcH("dioctyl phthalate",             "117-84-0", "E", 1.0E-6) ! C{24}H{38}O4
    CALL CalcH("naphthalene",                    "91-20-3", "X", 3.6E-4) ! C{10}H8
    CALL CalcH("acenaphthene",                   "83-32-9", "X", 1.9E-4) ! C{12}H{10}
    CALL CalcH("2,3-benzindene",                 "86-73-7", "X", 2.1E-4) ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",                   "85-01-8", "X", 1.3E-4) ! C{14}H{10}
    CALL CalcH("anthracene",                    "120-12-7", "X", 1.4E-3) ! C{14}H{10}
    CALL CalcH("benz[$a$]anthracene",            "56-55-3", "C", 1.2E-7) ! C{18}H{12}
    CALL CalcH("chrysene",                      "218-01-9", "C", 1.5E-6) ! C{18}H{12}
    CALL CalcH("benzo[$jk$]fluorene",           "206-44-0", "C", 1.0E-5) ! C{16}H{10} fluoranthene
    CALL CalcH("pyrene",                        "129-00-0", "C", 1.3E-6) ! C{16}H{10}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = 1. / (atm*H)
      IF (type_=="E") THEN
        ! upper limit of KHpc is lower limit of Hcp:
        CALL Output(Hominus, limit=">")
      ELSE
        IF (type_=="X") CALL SettypeX("mccarty80")
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2556

  !---------------------------------------------------------------------------

  SUBROUTINE ref2557 ! KHpx [GPa]
    IMPLICIT NONE

    ref = "2557"
    type = "M"

    CALL CalcH("neon",    "7440-01-9", -7.259, 6.950, -1.3826,  0.0538) ! Ne
    CALL CalcH("argon",   "7440-37-1", -9.520, 8.830, -1.8959,  0.0698) ! Ar
    CALL CalcH("krypton", "7439-90-9", -6.292, 5.612, -0.8881, -0.0458) ! Kr
    CALL CalcH("xenon",   "7440-63-3", -3.902, 2.439,  0.3863, -0.2211) ! Xe
    CALL CalcH("methane",   "74-82-8", -8.681, 7.837, -1.5090,  0.0206) ! CH4

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A0, A1, A2, A3)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A0, A1, A2, A3
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = cH2O / (1E9*EXP(A0+A1/(1E-3*T0)+A2/(1E-3*T0)**2+A3/(1E-3*T0)**3))
      mindHR = - (1E3*A1 + 2E6*A2/T0 + 3E9*A3/T0**2)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2557

  !---------------------------------------------------------------------------

  ! ref2558 old data, mainly high-temp

  !---------------------------------------------------------------------------

  SUBROUTINE ref2563 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2563"
    type = "V"

    CALL CalcH("PCB-1",        "2051-60-7",  9.50, 2301.4)
    CALL CalcH("PCB-2",        "2051-61-8",  9.30, 2325.9)
    CALL CalcH("PCB-3",        "2051-62-9",  9.24, 2205.2)
    CALL CalcH("PCB-4",       "13029-08-8", 10.14, 2383.4)
    CALL CalcH("PCB-5",       "16605-91-7",  9.69, 2385.8)
    CALL CalcH("PCB-6",       "25569-80-6",  9.95, 2548.1)
    CALL CalcH("PCB-7",       "33284-50-3",  9.79, 2463.2)
    CALL CalcH("PCB-8",       "34883-43-7",  9.88, 2450.4)
    CALL CalcH("PCB-9",       "34883-39-1",  9.84, 2429.6)
    CALL CalcH("PCB-10",      "33146-45-1", 10.16, 2457.3)
    CALL CalcH("PCB-11",       "2050-67-1",  9.76, 2470.6)
    CALL CalcH("PCB-12",       "2974-92-7",  9.43, 2305.9)
    CALL CalcH("PCB-13",       "2974-90-5",  9.68, 2567.0)
    CALL CalcH("PCB-14",      "34883-41-5",  9.59, 2391.9)
    CALL CalcH("PCB-15",       "2050-68-2",  9.61, 2124.3)
    CALL CalcH("PCB-16",      "38444-78-9", 10.28, 2523.8)
    CALL CalcH("PCB-17",      "37680-66-3", 10.37, 2675.0)
    CALL CalcH("PCB-18",      "37680-65-2", 10.42, 2507.9)
    CALL CalcH("PCB-19",      "38444-73-4", 10.50, 2354.4)
    CALL CalcH("PCB-20",      "38444-84-7", 10.09, 2503.6)
    CALL CalcH("PCB-21",      "55702-46-0",  9.92, 2251.0)
    CALL CalcH("PCB-22",      "38444-85-8", 10.00, 2414.2)
    CALL CalcH("PCB-23",      "55720-44-0", 10.18, 2495.6)
    CALL CalcH("PCB-24",      "55702-45-9", 10.23, 2419.7)
    CALL CalcH("PCB-25",      "55712-37-3", 10.18, 2572.3)
    CALL CalcH("PCB-26",      "38444-81-4", 10.25, 2562.6)
    CALL CalcH("PCB-27",      "38444-76-7", 10.34, 2631.8)
    CALL CalcH("PCB-28",       "7012-37-5", 10.11, 2547.1)
    CALL CalcH("PCB-29",      "15862-07-4", 10.09, 2378.1)
    CALL CalcH("PCB-30",      "35693-92-6", 10.30, 2453.3)
    CALL CalcH("PCB-31",      "16606-02-3", 10.16, 2466.1)
    CALL CalcH("PCB-32",      "38444-77-8", 10.23, 2475.1)
    CALL CalcH("PCB-33",      "38444-86-9", 10.01, 2420.4)
    CALL CalcH("PCB-34",      "37680-68-5", 10.29, 2508.3)
    CALL CalcH("PCB-35",      "37680-69-6",  9.83, 2413.7)
    CALL CalcH("PCB-36",      "38444-87-0", 10.01, 2418.0)
    CALL CalcH("PCB-37",      "38444-90-5",  9.76, 2350.1)
    CALL CalcH("PCB-38",      "53555-66-1",  9.73, 2329.8)
    CALL CalcH("PCB-39",      "38444-88-1", 10.02, 2410.9)
    CALL CalcH("PCB-40",      "38444-93-8", 10.49, 2309.3)
    CALL CalcH("PCB-41",      "52663-59-9", 10.87, 2708.8)
    CALL CalcH("PCB-42",      "36559-22-5", 10.59, 2541.6)
    CALL CalcH("PCB-43",      "70362-46-8", 11.13, 2756.0)
    CALL CalcH("PCB-44",      "41464-39-5", 10.65, 2596.5)
    CALL CalcH("PCB-45",      "70362-45-7", 11.17, 2608.0)
    CALL CalcH("PCB-46",      "41464-47-5", 10.70, 2282.9)
    CALL CalcH("PCB-47",       "2437-79-8", 10.68, 2675.2)
    CALL CalcH("PCB-48",      "70362-47-9", 11.05, 2634.8)
    CALL CalcH("PCB-49",      "41464-40-8", 10.73, 2578.0)
    CALL CalcH("PCB-50",      "62796-65-0", 11.25, 2756.4)
    CALL CalcH("PCB-51",      "68194-04-7", 10.80, 2743.6)
    CALL CalcH("PCB-52",      "35693-99-3", 10.80, 2495.0)
    CALL CalcH("PCB-53",      "41464-41-9", 10.86, 2405.5)
    CALL CalcH("PCB-54",      "15968-05-5", 10.91, 2066.0)
    CALL CalcH("PCB-55",      "74338-24-2", 10.67, 2579.1)
    CALL CalcH("PCB-56",      "41464-43-1", 10.22, 2358.2)
    CALL CalcH("PCB-57",      "70424-67-8", 10.93, 2648.1)
    CALL CalcH("PCB-58",      "41464-49-7", 10.50, 2344.5)
    CALL CalcH("PCB-59",      "74472-33-6", 11.02, 2881.9)
    CALL CalcH("PCB-60",      "33025-41-1", 10.60, 2404.4)
    CALL CalcH("PCB-61",      "33284-53-6", 10.50, 2441.3)
    CALL CalcH("PCB-62",      "54230-22-7", 10.83, 2588.6)
    CALL CalcH("PCB-63",      "74472-34-7", 10.86, 2633.7)
    CALL CalcH("PCB-64",      "52663-58-8", 10.91, 2626.3)
    CALL CalcH("PCB-65",      "33284-54-7", 10.76, 2519.6)
    CALL CalcH("PCB-66",      "32598-10-0", 10.32, 2323.0)
    CALL CalcH("PCB-67",      "73575-53-8", 10.85, 2701.1)
    CALL CalcH("PCB-68",      "73575-52-7", 10.59, 2518.7)
    CALL CalcH("PCB-69",      "60233-24-1", 11.15, 2808.1)
    CALL CalcH("PCB-70",      "32598-11-1", 10.27, 2346.1)
    CALL CalcH("PCB-71",      "41464-46-4", 10.45, 2594.5)
    CALL CalcH("PCB-72",      "41464-42-0", 10.65, 2459.3)
    CALL CalcH("PCB-73",      "74338-23-1", 10.72, 2541.6)
    CALL CalcH("PCB-74",      "32690-93-0", 10.77, 2518.4)
    CALL CalcH("PCB-75",      "32598-12-2", 11.08, 2800.4)
    CALL CalcH("PCB-76",      "70362-48-0", 10.62, 2378.8)
    CALL CalcH("PCB-77",      "32598-13-3",  9.96, 2008.9)
    CALL CalcH("PCB-78",      "70362-49-1", 10.45, 2432.3)
    CALL CalcH("PCB-79",      "41464-48-6", 10.24, 2331.6)
    CALL CalcH("PCB-80",      "33284-52-5", 10.45, 2212.8)
    CALL CalcH("PCB-81",      "70362-50-4", 10.42, 2301.0)
    CALL CalcH("PCB-82",      "52663-62-4", 11.00, 2536.7)
    CALL CalcH("PCB-83",      "60145-20-2", 11.26, 2726.5)
    CALL CalcH("PCB-84",      "52663-60-2", 11.30, 2584.9)
    CALL CalcH("PCB-85",      "65510-45-4", 11.10, 2847.7)
    CALL CalcH("PCB-86",      "55312-69-1", 11.49, 2814.8)
    CALL CalcH("PCB-87",      "38380-02-8", 11.16, 2616.0)
    CALL CalcH("PCB-88",      "55215-17-3", 11.90, 2946.8)
    CALL CalcH("PCB-89",      "73575-57-2", 11.22, 2641.0)
    CALL CalcH("PCB-90",      "68194-07-0", 11.36, 2887.8)
    CALL CalcH("PCB-91",      "68194-05-8", 11.40, 2815.4)
    CALL CalcH("PCB-92",      "52663-61-3", 11.41, 2830.1)
    CALL CalcH("PCB-93",      "73575-56-1", 11.90, 2836.6)
    CALL CalcH("PCB-94",      "73575-55-0", 11.48, 2721.7)
    CALL CalcH("PCB-95",      "38379-99-6", 11.47, 2678.6)
    CALL CalcH("PCB-96",      "73575-54-9", 11.52, 2521.5)
    CALL CalcH("PCB-97",      "41464-51-1", 11.18, 2716.8)
    CALL CalcH("PCB-98",      "60233-25-2", 11.48, 2750.0)
    CALL CalcH("PCB-99",      "38380-01-7", 11.27, 2859.6)
    CALL CalcH("PCB-100",     "39485-83-1", 11.57, 2849.1)
    CALL CalcH("PCB-101",     "37680-73-2", 11.32, 2763.6)
    CALL CalcH("PCB-102",     "68194-06-9", 11.39, 2740.6)
    CALL CalcH("PCB-103",     "60145-21-3", 11.64, 2846.8)
    CALL CalcH("PCB-104",     "56558-16-8", 11.67, 2718.3)
    CALL CalcH("PCB-105",     "32598-14-4", 10.67, 2495.8)
    CALL CalcH("PCB-106",     "70424-69-0", 11.30, 2810.9)
    CALL CalcH("PCB-107",     "70424-68-9", 11.01, 2673.6)
    CALL CalcH("PCB-108",     "70362-41-3", 10.90, 2538.4)
    CALL CalcH("PCB-109",     "74472-35-8", 11.47, 2874.0)
    CALL CalcH("PCB-110",     "38380-03-9", 11.06, 2779.4)
    CALL CalcH("PCB-111",     "39635-32-0", 11.28, 2711.5)
    CALL CalcH("PCB-112",     "74472-36-9", 11.72, 2866.3)
    CALL CalcH("PCB-113",     "68194-10-5", 11.32, 2835.2)
    CALL CalcH("PCB-114",     "74472-37-0", 11.22, 2777.1)
    CALL CalcH("PCB-115",     "74472-38-1", 11.64, 2998.1)
    CALL CalcH("PCB-116",     "18259-05-7", 11.77, 2802.2)
    CALL CalcH("PCB-117",     "68194-11-6", 11.48, 2581.7)
    CALL CalcH("PCB-118",     "31508-00-6", 10.92, 2606.3)
    CALL CalcH("PCB-119",     "56558-17-9", 11.22, 2802.8)
    CALL CalcH("PCB-120",     "68194-12-7", 11.21, 2622.7)
    CALL CalcH("PCB-121",     "56558-18-0", 11.49, 2808.9)
    CALL CalcH("PCB-122",     "76842-07-4", 10.82, 2520.0)
    CALL CalcH("PCB-123",     "65510-44-3", 10.92, 2532.1)
    CALL CalcH("PCB-124",     "70424-70-3", 10.97, 2570.6)
    CALL CalcH("PCB-125",     "74472-39-2", 11.04, 2506.7)
    CALL CalcH("PCB-126",     "57465-28-8", 10.63, 2337.8)
    CALL CalcH("PCB-127",     "39635-33-1", 10.83, 2435.9)
    CALL CalcH("PCB-128",     "38380-07-3", 10.98, 2629.7)
    CALL CalcH("PCB-129",     "55215-18-4", 11.54, 2787.2)
    CALL CalcH("PCB-130",     "52663-66-8", 11.67, 2814.0)
    CALL CalcH("PCB-131",     "61798-70-7", 11.95, 2812.4)
    CALL CalcH("PCB-132",     "38380-05-1", 11.71, 2779.2)
    CALL CalcH("PCB-133",     "35694-04-3", 11.92, 2841.5)
    CALL CalcH("PCB-134",     "52704-70-8", 11.95, 2799.8)
    CALL CalcH("PCB-135",     "52744-13-5", 11.97, 2878.4)
    CALL CalcH("PCB-136",     "38411-22-2", 12.00, 2747.3)
    CALL CalcH("PCB-137",     "35694-06-5", 11.64, 2950.4)
    CALL CalcH("PCB-138",     "35065-28-2", 11.58, 2935.7)
    CALL CalcH("PCB-139",     "56030-56-9", 11.90, 2998.3)
    CALL CalcH("PCB-140",     "59291-64-4", 12.00, 3053.2)
    CALL CalcH("PCB-141",     "52712-04-6", 11.70, 2893.3)
    CALL CalcH("PCB-142",     "41411-61-4", 12.41, 2985.5)
    CALL CalcH("PCB-143",     "68194-15-0", 11.76, 2853.5)
    CALL CalcH("PCB-144",     "68194-14-9", 12.10, 3039.6)
    CALL CalcH("PCB-145",     "74472-40-5", 12.15, 2782.5)
    CALL CalcH("PCB-146",     "51908-16-8", 11.84, 2961.0)
    CALL CalcH("PCB-147",     "68194-13-8", 12.04, 2841.0)
    CALL CalcH("PCB-148",     "74472-41-6", 12.13, 3049.0)
    CALL CalcH("PCB-149",     "38380-04-0", 11.89, 2950.7)
    CALL CalcH("PCB-150",     "68194-08-1", 12.17, 2975.6)
    CALL CalcH("PCB-151",     "52663-63-5", 12.11, 2929.9)
    CALL CalcH("PCB-152",     "68194-09-2", 12.15, 2757.0)
    CALL CalcH("PCB-153",     "35065-27-1", 11.75, 2915.3)
    CALL CalcH("PCB-154",     "60145-22-4", 12.06, 3071.2)
    CALL CalcH("PCB-155",     "33979-03-2", 12.43, 3008.5)
    CALL CalcH("PCB-156",     "38380-08-4", 11.28, 2698.5)
    CALL CalcH("PCB-157",     "69782-90-7", 11.22, 2559.5)
    CALL CalcH("PCB-158",     "74472-42-7", 11.70, 2881.4)
    CALL CalcH("PCB-159",     "39635-35-3", 11.47, 2665.3)
    CALL CalcH("PCB-160",     "41411-62-5", 12.44, 3081.9)
    CALL CalcH("PCB-161",     "74472-43-8", 11.95, 2956.2)
    CALL CalcH("PCB-162",     "39635-34-2", 11.40, 2675.0)
    CALL CalcH("PCB-163",     "74472-44-9", 11.69, 2795.4)
    CALL CalcH("PCB-164",     "74472-45-0", 11.52, 2831.2)
    CALL CalcH("PCB-165",     "74472-46-1", 11.90, 2772.7)
    CALL CalcH("PCB-166",     "41411-63-6", 11.98, 2839.2)
    CALL CalcH("PCB-167",     "52663-72-6", 11.39, 2759.5)
    CALL CalcH("PCB-168",     "59291-65-5", 11.70, 2898.7)
    CALL CalcH("PCB-169",     "32774-16-6", 10.90, 2236.6)
    CALL CalcH("PCB-170",     "35065-30-6", 11.78, 2884.2)
    CALL CalcH("PCB-171",     "52663-71-5", 12.19, 3073.1)
    CALL CalcH("PCB-172",     "52663-74-8", 12.04, 2969.9)
    CALL CalcH("PCB-173",     "68194-16-1", 12.30, 2806.4)
    CALL CalcH("PCB-174",     "38411-25-5", 12.08, 2925.4)
    CALL CalcH("PCB-175",     "40186-70-7", 12.45, 3118.3)
    CALL CalcH("PCB-176",     "52663-65-7", 12.61, 3142.8)
    CALL CalcH("PCB-177",     "52663-70-4", 12.07, 2864.4)
    CALL CalcH("PCB-178",     "52663-67-9", 12.42, 3111.0)
    CALL CalcH("PCB-179",     "52663-64-6", 12.61, 3050.4)
    CALL CalcH("PCB-180",     "35065-29-3", 11.80, 2977.2)
    CALL CalcH("PCB-181",     "74472-47-2", 12.40, 3126.9)
    CALL CalcH("PCB-182",     "60145-23-5", 12.26, 3124.4)
    CALL CalcH("PCB-183",     "52663-69-1", 12.37, 3206.1)
    CALL CalcH("PCB-184",     "74472-48-3", 12.64, 3144.6)
    CALL CalcH("PCB-185",     "52712-05-7", 12.46, 3026.8)
    CALL CalcH("PCB-186",     "74472-49-4", 12.51, 2830.3)
    CALL CalcH("PCB-187",     "52663-68-0", 12.37, 3129.7)
    CALL CalcH("PCB-188",     "74487-85-7", 12.66, 3082.2)
    CALL CalcH("PCB-189",     "39635-31-9", 11.59, 2744.6)
    CALL CalcH("PCB-190",     "41411-64-7", 12.06, 3048.4)
    CALL CalcH("PCB-191",     "74472-50-7", 12.16, 3127.0)
    CALL CalcH("PCB-192",     "74472-51-8", 12.32, 2985.8)
    CALL CalcH("PCB-193",     "69782-91-8", 12.02, 2949.9)
    CALL CalcH("PCB-194",     "35694-08-7", 12.11, 2985.0)
    CALL CalcH("PCB-195",     "52663-78-2", 12.47, 3076.7)
    CALL CalcH("PCB-196",     "42740-50-1", 12.50, 3208.9)
    CALL CalcH("PCB-197",     "33091-17-7", 12.90, 3279.8)
    CALL CalcH("PCB-198",     "68194-17-2", 12.60, 3024.6)
    CALL CalcH("PCB-199",     "52663-75-9", 12.64, 3030.8)
    CALL CalcH("PCB-200",     "52663-73-7", 12.63, 3134.1)
    CALL CalcH("PCB-201",     "40186-71-8", 12.83, 3249.2)
    CALL CalcH("PCB-202",      "2136-99-4", 12.91, 3162.7)
    CALL CalcH("PCB-203",     "52663-76-0", 12.78, 3366.5)
    CALL CalcH("PCB-204",     "74472-52-9", 13.35, 3391.7)
    CALL CalcH("PCB-205",     "74472-53-0", 12.30, 2963.3)
    CALL CalcH("PCB-206",     "40186-72-9", 13.30, 3168.8)
    CALL CalcH("PCB-207",     "52663-79-3", 13.71, 3268.0)
    CALL CalcH("PCB-208",     "52663-77-1", 13.71, 3335.1)
    CALL CalcH("PCB-209",      "2051-24-3", 13.72, 3143.6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Ah, Bh)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Ah, Bh
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/(10.**(Ah-Bh/T0))
      mindHR = Bh * LOG(10.)
      CALL Output(Hominus,mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2563

  !---------------------------------------------------------------------------

  SUBROUTINE ref2564 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2564"
    type = "M"

    ! Tab. 3:
    CALL CalcH("PCB-1",      "2051-60-7", 33.,  44.)
    CALL CalcH("PCB-3",      "2051-62-9", 28.,  34.)
    CALL CalcH("PCB-8",     "34883-43-7", 39.,  52.)
    CALL CalcH("PCB-15",     "2050-68-2", 20.,  30.)
    CALL CalcH("PCB-28",     "7012-37-5", 44.,  70.)
    CALL CalcH("PCB-47",     "2437-79-8", 4.7, 110.)
    CALL CalcH("PCB-77",    "32598-13-3", 32.,  55.)
    CALL CalcH("PCB-99",    "38380-01-7", 45., 240.)
    CALL CalcH("PCB-118",   "31508-00-6", 88., 180.)
    ! Tab. 4:
    CALL CalcH("PBDE-3",      "101-55-3", 20.,  23.)
    CALL CalcH("PBDE-15",    "2050-47-7", 12.,  14.)
    CALL CalcH("PBDE-28",   "41318-75-6", 9.5,  13.)
    CALL CalcH("PBDE-47",    "5436-43-1", 6.4,  5.8)
    CALL CalcH("PBDE-99",   "60348-60-9", 1.6,  3.0)
    CALL CalcH("PBDE-100", "189084-64-8", 3.0,  3.1)
    CALL CalcH("PBDE-118", "446254-77-9", 1.6,  1.3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, MGSM, IGSM)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: MGSM, IGSM
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNote("2564mgsm", "Modified gas-stripping method (MGSM), see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHpcSI_TIMES_HcpSI/MGSM)
      CALL MakeNote("2564igsm", "Integrated gas-stripping method (IGSM), see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHpcSI_TIMES_HcpSI/IGSM)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2564

  !---------------------------------------------------------------------------

  SUBROUTINE ref2565 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2565"
    type = "M"

    CALL CalcH("PCB-77",  "32598-13-3", 11.0)
    CALL CalcH("PCB-81",  "70362-50-4", 11.3)
    CALL CalcH("PCB-105", "32598-14-4",  5.6)
    CALL CalcH("PCB-114", "74472-37-0", 18.8)
    CALL CalcH("PCB-118", "31508-00-6", 17.4)
    CALL CalcH("PCB-123", "65510-44-3", 21.8)
    CALL CalcH("PCB-126", "57465-28-8", 10.0)
    CALL CalcH("PCB-156", "38380-08-4", 14.8)
    CALL CalcH("PCB-157", "69782-90-7", 16.6)
    CALL CalcH("PCB-167", "52663-72-6", 12.8)
    CALL CalcH("PCB-169", "32774-16-6", 12.3)
    CALL CalcH("PCB-189", "39635-31-9", 11.9)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/HLC)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2565

  !---------------------------------------------------------------------------

  SUBROUTINE ref2566 ! KHcc [1]
    IMPLICIT NONE

    ref = "2566"
    type = "M"

    CALL CalcH("benzenecarboxylic acid",        "65-85-0", 1.39E-6) ! C6H5COOH benzoic acid
    CALL CalcH("pentadecafluorooctanoic acid", "335-67-1", 1.02E-3) ! PFOA

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KAW)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KAW
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KAW
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2566

  !---------------------------------------------------------------------------

  SUBROUTINE ref2570 ! KHpc [bar*L/mol]
    IMPLICIT NONE

    ref = "2570"
    type = "?"

    chem = "mercury dichloride" ; casrn = "7487-94-7" ! HgCl2
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 333., 353. /)
    Harray = Hcp_TO_HcpSI / ( (/ 2E-5, 1E-4 /) * bar/atm )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("highTextrapol")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2570

  !---------------------------------------------------------------------------

  SUBROUTINE ref2571 ! KHcc [1]
    IMPLICIT NONE

    ref = "2571"

    CALL CalcH("mercury dibromide",  "7789-47-1", "C", 3.164449E10, -7.44E3) ! HgBr2
    CALL CalcH("mercury dichloride", "7487-94-7", "R", 2.324502E09, -7.44E3) ! HgCl2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 * 1E6 / (A * (1.-(T0-Tstp)/Tstp) * EXP(B/T0))
      ! calculate analytical derivative (see also util/derivative.f90):
      mindHR = 1. / (2.*Tstp/T0**2-1./T0) + B
      mindHR = -mindHR + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2571

  !---------------------------------------------------------------------------

  SUBROUTINE ref2572 ! KHpx [bar]
    IMPLICIT NONE

    ref = "2572"
    type = "M"
    chem = "mercury dichloride" ; casrn = "7487-94-7" ! HgCl2
    Hominus = cH2O / (5.37E-4 * bar)
    CALL MakeNoteOtherTemp("333")
    CALL Output(Hominus)

  END SUBROUTINE ref2572

  !---------------------------------------------------------------------------

  SUBROUTINE ref2576 ! KHcc [1]
    IMPLICIT NONE

    ref = "2576"

    ! Tab. 2:
    chem = "hydroxymethylmercury" ; casrn = "1184-57-2" ! CH3HgOH
    type = "M"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 15., 20. /) + CtoK
    Harray = (/ 1.73E-7, 2.69E-7 /)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    ! Tab. 3:
    CALL CalcH("mercury dichloride",  "7487-94-7", 6.4E-7) ! HgCl2
    CALL CalcH("mercury dibromide",   "7789-47-1", 7.7E-6) ! HgBr2
    CALL CalcH("mercury diiodide",    "7774-29-0", 2.1E-4) ! HgI2
    CALL CalcH("chloromethylmercury",  "115-09-3", 2.7E-5) ! CH3HgCl
    CALL CalcH("bromomethylmercury",   "506-83-2", 1.1E-4) ! CH3BrHg
    CALL CalcH("iodomethylmercury",    "143-36-2", 7.0E-4) ! CH3HgI
    CALL CalcH("dimethylmercury",      "593-74-8", 0.13)   ! C2H6Hg

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / H
      type = "?"
      CALL MakeNote("morethanoneref")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2576

  !---------------------------------------------------------------------------

  SUBROUTINE ref2577 ! Hcc [1]
    IMPLICIT NONE
    REAL :: Delta_G, Delta_Hw, Delta_Cpw, logKw_calc

    ref = "2577"

    ! "MedChem data base", p.438:
    chem = "dimethylmercury" ; casrn = "593-74-8" ! C2H6Hg
    type = "C"
    CALL Output(Hcc_TO_HcpSI_atT0 * 10.**0.72)

    ! p. 438:
    chem = "chlorophenylmercury" ; casrn = "100-56-1" ! C6H5ClHg
    type = "V"
    CALL Output(Hcc_TO_HcpSI_atT0 * 10.**5.97)

    ! Tab. 3:                                              E,     S,    A,    B,    V,      L
    ! supplement, Tab. S5:
    !    273   293    298    303    323    343    353    363    373
    CALL CalcH("Hg",            "7439-97-6", "Q",  0.46,  0.85,  0.43, 0.00, 0.04, 0.3400, 1.721)
    CALL CalcH("HgCl2",         "7487-94-7", "Q",  8.02,  1.25,  1.97, 0.50, 0.40, 0.6278, 4.765)
    CALL CalcH("HgBr2",         "7789-47-1", "Q",  7.04,  1.55,  1.71, 0.43, 0.37, 0.7330, 5.332)
    CALL CalcH("HgI2",          "7774-29-0", "Q",  5.69,  2.09,  1.35, 0.28, 0.37, 0.8994, 6.316)
    CALL CalcH("MeHgCl",         "115-09-3", "Q",  4.72,  0.98,  1.48, 0.13, 0.29, 0.6463, 4.037, &
      (/ 5.41, 4.84,  4.72,  4.60,  4.18,  3.84,  3.69,  3.55,  3.43 /))
    CALL CalcH("EtHgCl",         "107-27-7", "Q",  4.58,  1.03,  1.50, 0.13, 0.28, 0.7872, 4.569, &
      (/ 5.32, 4.71,  4.58,  4.45,  4.01,  3.66,  3.51,  3.37,  3.26 /))
    CALL CalcH("PrHgCl",        "2440-40-6", "Q",  4.46,  1.01,  1.52, 0.13, 0.27, 0.9281, 5.065, &
      (/ 5.26, 4.60,  4.46,  4.33,  3.86,  3.50,  3.35,  3.22,  3.10 /))
    CALL CalcH("iso-PrHgCl",   "30615-19-1", "Q",  4.39,  0.98,  1.49, 0.10, 0.30, 0.9281, 5.02)
    CALL CalcH("BuHgCl",         "543-63-5", "Q",  4.34,  1.00,  1.52, 0.13, 0.27, 1.0690, 5.549, &
      (/ 5.21, 4.49,  4.34,  4.20,  3.71,  3.33,  3.18,  3.05,  2.94 /))
    CALL CalcH("PeHgCl",         "544-15-0", "Q",  4.24,  1.00,  1.52, 0.13, 0.27, 1.2099, 5.959, &
      (/ 5.17, 4.40,  4.24,  4.09,  3.58,  3.19,  3.04,  2.91,  2.80 /))
    CALL CalcH("MeHgBr",         "506-83-2", "Q",  3.94,  1.13,  1.40, 0.10, 0.21, 0.6989, 4.182, &
      (/ 4.58, 4.05,  3.94,  3.83,  3.46,  3.15,  3.03,  2.91,  2.81 /))
    CALL CalcH("EtHgBr",         "107-26-6", "Q",  3.87,  1.18,  1.39, 0.12, 0.20, 0.8398, 4.709, &
      (/ 4.57, 3.99,  3.87,  3.75,  3.35,  3.03,  2.90,  2.79,  2.69 /))
    CALL CalcH("MeHgI",          "143-36-2", "Q",  3.69,  1.40,  1.31, 0.07, 0.20, 0.7821, 4.839, &
      (/ 4.29, 3.76,  3.65,  3.54,  3.17,  2.88,  2.76,  2.64,  2.56 /))
    CALL CalcH("EtHgI",         "2440-42-8", "Q",  3.79,  1.45,  1.38, 0.07, 0.20, 0.9230, 5.40,  &
      (/ 4.49, 3.91,  3.79,  3.67,  3.28,  2.97,  2.84,  2.73,  2.64 /))
    CALL CalcH("Me2Hg",          "593-74-8", "Q",  0.41,  0.705, 0.62, 0.00, 0.00, 0.6648, 2.983, &
      (/ 0.83, 0.48,  0.41,  0.35,  0.15,  0.02, -0.02, -0.05, -0.07 /))
    CALL CalcH("Et2Hg",          "627-44-1", "Q",  0.41,  0.814, 0.70, 0.00, 0.00, 0.9466, 4.13,  &
      (/ 0.96, 0.50,  0.41,  0.33,  0.07, -0.08, -0.13, -0.16, -0.17 /))
    CALL CalcH("Pr2Hg",          "628-85-3", "Q",  0.14,  0.764, 0.69, 0.00, 0.00, 1.2284, 5.18,  &
      (/ 0.82, 0.25,  0.14,  0.04, -0.26, -0.44, -0.49, -0.52, -0.52 /))
    CALL CalcH("iso-Pr2Hg",     "1071-39-2", "Q", -0.01,  0.760, 0.60, 0.00, 0.00, 1.2284, 5.10)
    CALL CalcH("Bu2Hg",          "629-35-6", "Q", -0.14,  0.733, 0.68, 0.00, 0.00, 1.5102, 6.25,  &
      (/ 0.67, 0.00, -0.13, -0.25, -0.60, -0.80, -0.85, -0.87, -0.88 /))
    CALL CalcH("PhHgCl",         "100-56-1", "Q",  6.36,  1.65,  1.82, 0.17, 0.43, 1.1132, 6.841, &
      (/ 7.36, 6.54,  6.36,  6.19,  5.59,  5.11,  4.90,  4.72,  4.55 /))
    CALL CalcH("PhHgBr",        "1192-89-8", "Q",  5.66,  1.80,  1.75, 0.15, 0.33, 1.1658, 7.142, &
      (/ 6.60, 5.83,  5.66,  5.50,  4.96,  4.52,  4.39,  4.18,  4.03 /))
    CALL CalcH("PhHgI",          "823-04-1", "Q",  5.35,  2.07,  1.75, 0.06, 0.32, 1.2490, 7.770, &
      (/ 6.26, 5.51,  5.35,  5.20,  4.68,  4.27,  4.10,  3.95,  3.82 /))
    CALL CalcH("Ph2Hg",          "587-85-9", "Q",  5.84,  2.05,  1.63, 0.00, 0.60, 1.5986, 8.845, &
      (/ 7.02, 6.02,  5.81,  5.61,  4.98,  4.38,  4.16,  3.97,  3.81 /))
    CALL CalcH("MeOCH2CH2HgCl",  "123-88-6", "Q",  6.98,  1.07,  1.68, 0.13, 0.75, 0.9868, 5.364)

    ! Tab. 4:
    CALL CalcH("HgCl2", "7487-94-7", "V", 7.60)
    CALL CalcH("HgCl2", "7487-94-7", "V", 8.02)
    CALL CalcH("HgBr2", "7789-47-1", "V", 6.46)
    CALL CalcH("HgI2",  "7774-29-0", "V", 5.15)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, logKw0, E, S, A, B, V, L, logKw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: logKw0
      REAL, OPTIONAL,   INTENT(IN) :: E, S, A, B, V, L
      REAL, OPTIONAL,   INTENT(IN) :: logKw(9)
      INTEGER :: i
      REAL, PARAMETER :: TS5(9) = & ! temperature in Tab. 5
        (/ 273., 293., 298., 303., 323., 343., 353., 363., 373. /)
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI_atT0 * 10.**logKw0
      IF (PRESENT(E)) THEN
        ! Eqn. (9):
        Delta_Hw = -6.952 + 1.412*E - 2.859*S - 34.086*A - 42.686*B - 22.720*V
        ! Eqn. (10):
        Delta_Cpw = 103.6 - 65.3*E - 91.6*S - 3.1*A - 119.3*B + 404.0*V
        ! The analytical derivative is:
        ! dln(H)/d(1/T) = - DeltaH/R + (T0-T) * Delta_Cpw/R
        ! At T=T0, this reduces to simply -DeltaH/R.
        mindHR = -1E3*Delta_Hw/Rgas + T0 ! see ref958, eqn (34) why T0 is added
        ! for gnuplot:
        ! WRITE(77,*)
        ! WRITE(77,'(2A)') "# ", chem_
        ! WRITE(77,'(A,F10.6)') "# Delta_Hw  = ", Delta_Hw
        ! WRITE(77,'(A,F10.6)') "# Delta_Cpw = ", Delta_Cpw
        ! WRITE(77,'(A,F10.6)') "# logKw0    = ", logKw0
        IF (PRESENT(logKw)) THEN
          ! PRINT *, chem_
          ! calculate logKw at temperatures TS5 listed in Tab. S5:
          DO i = 1, 9
            ! Eqn. (8):
            Delta_G = (TS5(i)/T0) * (-Rgas*T0*LOG(10.)*logKw0) &
              - ((TS5(i)-T0)/T0) * 1E3 * Delta_Hw &
              + Delta_Cpw * (TS5(i)-T0-TS5(i)*LOG(TS5(i)/T0))
            logKw_calc = (Delta_G / (-Rgas*TS5(i))) / LOG(10.)
            ! WRITE(*,'(F5.0,A,F6.2,A,F6.2)') &
            !   TS5(i), " K, calc/tab = ", logKw_calc, " /", logKw(i)
            ! for gnuplot:
            ! WRITE(77,'(A,I1,A,F5.0)') "# T", i, "     = ", TS5(i)
            ! WRITE(77,'(A,I1,A,F6.2)') "# logKw", i, " = ", logKw(i)
          ENDDO
        ENDIF
        SELECT CASE(casrn_)
        CASE ("7439-97-6")
          CALL SettypeX("clever87b")
          CALL Output(Hominus, mindHR)
        CASE ("115-09-3")
          ! H value for MeHgCl is from ref2288, as cited by ref2576.
          ! (only a citation, thus not used here)
          CALL MakeNote("2577-LFER", &
            "Temperature dependence calculated using linear free "// &
            "energy relationships (LFER).")
          CALL Output(DUMMY, mindHR)
        CASE ("506-83-2")
          ! H value for MeHgBr is from ref2576.
          ! (only a citation, thus not used here)
          CALL MakeNote("2577-LFER", &
            "Temperature dependence calculated using linear free "// &
            "energy relationships (LFER).")
          CALL Output(DUMMY, mindHR)
        CASE DEFAULT
          CALL MakeNote(TRIM(ref), &
            "Calculated using linear free energy relationships (LFER).")
          CALL Output(Hominus, mindHR)
        END SELECT
      ELSE
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2577

  !---------------------------------------------------------------------------

  SUBROUTINE ref2578 ! KHpx [atm]
    IMPLICIT NONE
    REAL, PARAMETER :: A = 135.636
    REAL, PARAMETER :: B = 8031.1
    REAL, PARAMETER :: C = 42.8448
    ref = "2578"
    type = "V"
    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    Hominus = KHpx_TIMES_HcpSI/10.**(A-B/T0-C*LOG10(T0))
    ! analytical derivative:
    mindHR = B * LOG(10.) - C * T0
    CALL Output(Hominus, mindHR)
  END SUBROUTINE ref2578

  !---------------------------------------------------------------------------

  SUBROUTINE ref2580 ! Hcc [1]
    IMPLICIT NONE

    ref = "2580"
    type = "V"

    CALL CalcH("octane",                           "111-65-9", -2.67  ) ! C8H{18}
    CALL CalcH("methylcyclohexane",                "108-87-2", -1.91  ) ! C6H{11}CH3
    CALL CalcH("ethylcyclohexane",                "1678-91-7", -1.74  ) ! C8H{16}
    CALL CalcH("{trans}-1,2-dimethylcyclohexane", "6876-23-9", -1.85  ) ! C6H{10}(CH3)2
    CALL CalcH("{cis}-1,2-dimethylcyclohexane",   "2207-01-4", -1.94  ) ! C6H{10}(CH3)2
    CALL CalcH("benzene",                           "71-43-2", -0.065 ) ! C6H6 average
    !CALL CalcH("benzene",                           "71-43-2", -0.07  ) ! C6H6
    CALL CalcH("methanol",                          "67-56-1",  2.275 ) ! CH3OH average (2.32 + 2.23) / 2 => 2.275
    CALL CalcH("ethanol",                           "64-17-5",  2.065 ) ! C2H5OH average (2.1 + 2.03) / 2 => 2.065
    CALL CalcH("1-propanol",                        "71-23-8",  1.88  ) ! C3H7OH
    CALL CalcH("propanone",                         "67-64-1",  1.64  ) ! CH3COCH3 acetone average (1.7 + 1.58) / 2 => 1.64
    CALL CalcH("butanone",                          "78-93-3",  1.44  ) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("ethane nitrile",                    "75-05-8",  1.96  ) ! CH3CN acetonitrile
    !CALL CalcH("benzene",                           "71-43-2", -0.06  ) ! C6H6
    CALL CalcH("ethylbenzene",                     "100-41-4", -0.39  ) ! C6H5C2H5
    CALL CalcH("1,4-dimethylbenzene",              "106-42-3", -0.35  ) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("1,3,5-trimethylbenzene",           "108-67-8", -0.45  ) ! C6H3(CH3)3 mesitylene
    CALL CalcH("butylbenzene",                     "104-51-8", -0.61  ) ! C6H5C4H9

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logKw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI_atT0 * 10.**logKw
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2580

  !---------------------------------------------------------------------------

  ! SUBROUTINE ref2596 ! KHcc [1]
  ! not used because the full data set was presented by ref2580
  ! END SUBROUTINE ref2596

  !---------------------------------------------------------------------------

  SUBROUTINE ref2597 ! KHpx [atm]
    IMPLICIT NONE

    ref = "2597"
    type = "Q"

    CALL CalcH("2-furancarboxaldehyde",  "98-01-1", 13.08, 7.54,  58.4  ) ! C5H4O2 furfural
    CALL CalcH("benzaldehyde",          "100-52-7", 7.75,  20.76, 271.3 ) ! C6H5CHO
    CALL CalcH("2-phenylethanol",        "60-12-8", 1.19,  2.28,  194.6 ) ! C8H{10}O
    CALL CalcH("phenylacetaldehyde",    "122-78-1", 2.64,  5.32,  203.8 ) ! C8H{10}O

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, p0, K_inf, gamma_inf)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: p0, K_inf, gamma_inf
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! what the authors call the "limiting separation factor" k^infinity
      ! appears to be identical to KHpx in [atm]
      ! gamma_inf = "infinite dilution activity coefficient" = "limiting activity coefficient"
      Hominus = KHpx_TIMES_HcpSI / K_inf
      CALL consistency_check(Hominus, cH2O/(1E3*p0*gamma_inf), &
        "Values in Table 3")
      CALL MakeNoteOtherTemp("373")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2597

  !---------------------------------------------------------------------------

  SUBROUTINE ref2598 ! KHpx [atm]
    IMPLICIT NONE

    ref = "2598"
    type = "Q"

    CALL CalcH("2-phenylethanol",        "60-12-8", 13.857, 2.28, 167. ) ! C8H{10}O
    CALL CalcH("phenylacetaldehyde",    "122-78-1", 23.,    5.32, 234. ) ! C8H{10}O

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, p0, K_inf, gamma_inf)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: p0, K_inf, gamma_inf
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! what the authors call the "limiting separation factor" k^infinity
      ! appears to be identical to KHpx in [atm]
      ! gamma_inf = "infinite dilution activity coefficient" = "limiting activity coefficient"
      ! 1 mbar = 100 Pa
      Hominus = KHpx_TIMES_HcpSI / K_inf
      CALL consistency_check(Hominus, cH2O/(100.*p0*gamma_inf), &
        "Values in Table 3")
      CALL MakeNoteOtherTemp("373")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2598

  !---------------------------------------------------------------------------

  SUBROUTINE ref2599 ! KHpx [atm]
    IMPLICIT NONE

    ref = "2599"
    type = "Q"

    CALL CalcH("2-furancarboxaldehyde",           "98-01-1", 13.51, 7.54, 56.5  ) ! C5H4O2 furfural
    CALL CalcH("gamma-nonalactone",              "104-61-0", 55.8,  3.10, 5.6   )
    CALL CalcH("benzaldehyde",                   "100-52-7", 8.38, 20.76, 251.  ) ! C6H5CHO
    CALL CalcH("3,7-dimethyl-1,6-octadien-3-ol",  "78-70-6", 3.49, 35.46, 1029. ) ! C{10}H{18}O linalool

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, p0, K_inf, gamma_inf)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: p0, K_inf, gamma_inf
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! what the authors call the "limiting separation factor" k^infinity
      ! appears to be identical to KHpx in [atm]
      ! gamma_inf = "infinite dilution activity coefficient" = "limiting activity coefficient"
      Hominus = KHpx_TIMES_HcpSI / K_inf
      CALL consistency_check(Hominus, cH2O/(1E3*p0*gamma_inf), &
        "Values in Table 4")
      CALL MakeNoteOtherTemp("373")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2599

  !---------------------------------------------------------------------------

  SUBROUTINE ref2600 ! KHpx [atm]
    IMPLICIT NONE

    ref = "2600"
    type = "Q"

    CALL CalcH("hexanal",           "66-25-1",  40.40, 48.3, 121.1 ) ! C5H{11}CHO
    CALL CalcH("2-methylbutanal",   "96-17-3", 128.4,  57.5,  45.4 ) ! C5H{10}O
    CALL CalcH("3-methylbutanal",  "590-86-3", 121.1,  55.5,  46.4 ) ! C5H{10}O isovaleraldehyde
    CALL CalcH("dimethyl sulfide",  "75-18-3", 572.4,  75.6,  13.4 ) ! CH3SCH3 DMS

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, p0, K_inf, gamma_inf)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: p0, K_inf, gamma_inf
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! what the authors call the "limiting separation factor" k^infinity
      ! appears to be identical to KHpx in [atm]
      ! gamma_inf = "infinite dilution activity coefficient" = "limiting activity coefficient"
      Hominus = KHpx_TIMES_HcpSI / K_inf
      CALL consistency_check(Hominus, cH2O/(1E3*p0*gamma_inf), &
        "Values in Table 4")
      CALL MakeNoteOtherTemp("372")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2600

  !---------------------------------------------------------------------------

  SUBROUTINE ref2601 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "2601"
    type = "M"

    CALL CalcH("N-methylmethanamide",     "123-39-7", 34.0606,  -37.3482, -11.7869, 0.       ) ! C2H5NO N-methylformamide
    CALL CalcH("N-methylacetamide",        "79-16-3", 45.42529, -46.6756, -14.0990, -2.81867 )
    CALL CalcH("N,N-dimethylmethanamide",  "68-12-2", 40.2666,  -36.7266, -7.0191,  -4.62646 ) ! C3H7NO N,N-dimethylformamide
    CALL CalcH("N,N-dimethylacetamide",   "127-19-5", 48.6627,  -50.7278, -21.8138, 0.       ) ! C4H9NO

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL,             INTENT(IN) :: A, B, C, D
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
      ! see also gnuplot/ref2601.gnu
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2601

  !---------------------------------------------------------------------------

  SUBROUTINE ref2610 ! KHpx [GPa and bar]
    IMPLICIT NONE

    ref = "2610"

    ! Tab. 1:
    type = "M"
    ndata = 9
    ALLOCATE(temp(ndata), Harray(ndata))
    chem = "hexafluoroethane" ; casrn = "76-16-4" ! C2F6
    temp = (/ 287.704, 293.424, 298.464, 303.710, 308.606, 313.743, 318.436, &
      323.540, 328.238 /)
    Harray = cH2O / ( (/ 56.99, 76.12, 91.83, 106.84, 119.10, 130.02, &
      137.02, 142.80, 146.46 /) * 1E9) ! GPa
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    ! Tab. 5, using only data at 280 K and 300 K:
    type = "Q"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 280., 300. /)
    ! ---
    chem = "tetrafluoromethane" ; casrn = "75-73-0" ! CF4
    Harray = cH2O / ( EXP( (/ 13.4, 13.2 /) ) * bar) ! bar
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    ! ---
    chem = "hexafluoroethane"   ; casrn = "76-16-4" ! C2F6
    Harray = cH2O / ( EXP( (/ 12.7, 13.1 /) ) * bar) ! bar
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    ! ---
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2610

  !---------------------------------------------------------------------------

  SUBROUTINE ref2611_2614 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ! info about Tables:
    !  2.2.2     ps   pl   cs    cl     H=p/c
    !  3.2.2     ps   pl   cs    cl     H=p/c
    !  4.2.2     ps   pl   cs    cl     H=p/c
    !  5.2.2 ### ps   pl   cs    cl     H=p/c    exptl
    !  6.2.2     ps   pl   cs    cl     H=p/c
    !  7.2.2     ps   pl   cs    cl     H=p/c
    !  8.2.2     ps   pl   cs    cl     H=p/c
    !  9.2.2     ps   pl   cs    cl     H=p/c
    ! 10.2.2     ps   pl   cs    cl     H=p/c
    ! 11.2.2 ### cl   pl   H=p/c exptla exptlb   exptlc
    ! 12.2.2 ### cl   pl   H=p/c exptla exptlb   exptlc   exptld
    ! 13.2.2 ### cs   cl   ps    pl     H=p/c    exptla   calcd
    ! 14.2.2     ps   pl   cs    cl     H=p/c
    ! 15.2.2 ### ps   pl   cs    cl     H=p/c    exptla   exptlb
    ! 16.2.2 ### ps   pl   cs    cl     H=p/c    exptl
    ! 17.2.2     ps   pl   cs    cl     H=p/c
    ! 18.2.2     ps   pl   cs    cl     H=p/c
    ! 19.2.2     ps   pl   cs    cl     H=p/c

    ref = "2611"

    ! Table 2.2.1+2.2.2                                               ps          pl        cs         cl         H=p/c
    CALL CalcH("isobutane (2-methylpropane)",            "75-28-5",  357000.,    357000.,  0.8413,    0.8413,    120435.)
    CALL CalcH("2,2-dimethylpropane (neopentane)",      "463-82-1",  172000.,    172000.,  0.4602,    0.4602,    220195.)
    CALL CalcH("n-butane",                              "106-97-8",  243000.,    243000.,  1.0564,    1.0564,    95915.)
    CALL CalcH("2-methylbutane (isopentane)",            "78-78-4",  91640.,     91640.,   0.6625,    0.6625,    138320.)
    CALL CalcH("2,2-dimethylbutane",                     "75-83-2",  42600.,     42600.,   0.2135,    0.2135,    199515.)
    CALL CalcH("2,3-dimethylbutane",                     "79-29-8",  32010.,     32010.,   0.2216,    0.2216,    144422.)
    CALL CalcH("2,2,3-trimethylbutane",                 "464-06-2",  13652.,     13652.,   0.0437,    0.0437,    312320.)
    CALL CalcH("n-pentane",                             "109-66-0",  68400.,     68400.,   0.5336,    0.5336,    128180.)
    CALL CalcH("2-methylpentane (isohexane)",           "107-83-5",  28200.,     28200.,   0.1601,    0.1601,    176097.)
    CALL CalcH("3-methylpentane",                        "96-14-0",  25300.,     25300.,   0.1485,    0.1485,    170330.)
    CALL CalcH("2,2-dimethylpentane",                   "590-35-2",  14000.,     14000.,   0.0439,    0.0439,    318825.)
    CALL CalcH("2,4-dimethylpentane",                   "108-08-7",  13100.,     13100.,   0.0405,    0.0405,    323312.)
    CALL CalcH("3,3-dimethylpentane",                   "562-49-2",  10940.,     10940.,   0.0593,    0.0593,    184550.)
    CALL CalcH("2,2,4-trimethylpentane (isooctane)",    "540-84-1",  6560.,      6560.,    0.0214,    0.0214,    307110.)
    CALL CalcH("2,3,4-trimethylpentane",                "565-75-3",  3600.,      3600.,    0.0119,    0.0119,    205614.)
    CALL CalcH("n-hexane",                              "110-54-3",  20200.,     20200.,   0.1102,    0.1102,    183235.)
    CALL CalcH("2-methylhexane (isoheptane)",           "591-76-4",  8780.,      8780.,    0.0253,    0.0253,    346370.)
    CALL CalcH("3-methylhexane",                        "589-34-4",  8210.,      8210.,    0.0329,    0.0329,    249290.)
    CALL CalcH("2,2,5-trimethylhexane",                "3522-94-9",  2210.,      2210.,    0.0090,    0.0090,    246472.)
    CALL CalcH("n-heptane",                             "142-82-5",  6110.,      6110.,    0.0292,    0.0292,    208955.)
    CALL CalcH("2-methylheptane",                       "592-27-8",  2600.,      2600.,    0.00744,   0.0074,    349410.)
    CALL CalcH("n-octane",                              "111-65-9",  1800.,      1800.,    0.005778,  0.0058,    311536.)
    CALL CalcH("n-nonane",                              "111-84-2",  571.,       571.,     0.001715,  0.0017,    332880.)
    CALL CalcH("n-decane",                              "124-18-5",  175.,       175.,     0.000365,  0.00037,   478840.)
    CALL CalcH("n-undecane",                           "1120-21-4",  52.2,       52.2,     0.000026,  0.000026,  2039835.)
    CALL CalcH("n-dodecane",                            "112-40-3",  18.02,      18.02,    0.000022,  0.000022,  829570.)
    CALL CalcH("cyclopentane",                          "287-92-3",  42400.,     42400.,   2.3669,    2.3669,    17915.)
    CALL CalcH("methylcyclopentane",                     "96-37-7",  18300.,     18300.,   0.5109,    0.5109,    35815.)
    CALL CalcH("1,1,3-trimethylcyclopentane",          "4516-69-2",  5300.,      5300.,    0.0332,    0.0332,    159440.)
    CALL CalcH("propylcyclopentane",                   "2040-96-2",  1640.,      1640.,    0.0182,    0.0182,    90210.)
    CALL CalcH("pentylcyclopentane",                   "3741-00-2",  152.,       152.,     0.0008,    0.0008,    185395.)
    CALL CalcH("cyclohexane",                           "110-82-7",  13014.,     13014.,   0.6892,    0.6892,    18885.)
    CALL CalcH("methylcyclohexane",                     "108-87-2",  6180.,      6180.,    0.1538,    0.1538,    40185.)
    CALL CalcH("1,2-cis-dimethylcyclohexane",          "2207-01-4",  1930.,      1930.,    0.0535,    0.0535,    36095.)
    CALL CalcH("1,4-trans-dimethylcyclohexane",        "2207-04-7",  3020.,      3020.,    0.0342,    0.0342,    88250.)
    CALL CalcH("1,1,3-trimethylcyclohexane",           "3073-66-3",  1480.,      1480.,    0.0140,    0.0140,    105560.)
    CALL CalcH("cycloheptane",                          "291-64-5",  2924.,      2924.,    0.2393,    0.2393,    12220.)
    CALL CalcH("cyclooctane",                           "292-64-8",  748.,       748.,     0.0517,    0.0517,    14470.)
    CALL CalcH("2-methylpropene",                       "115-11-7",  304000.,    304000.,  4.6875,    4.6875,    21620.)
    CALL CalcH("1-butene",                              "106-98-9",  297000.,    297000.,  3.9567,    3.9567,    25610.)
    CALL CalcH("3-methyl-1-butene",                     "563-45-1",  120000.,    120000.,  1.8536,    1.8536,    54670.)
    CALL CalcH("2-methyl-2-butene",                     "513-35-9",  62410.,     62410.,   4.634,     4.634,     13470.)
    CALL CalcH("1-pentene",                             "109-67-1",  85000.,     85000.,   2.1103,    2.1103,    40280.)
    CALL CalcH("2-methyl-1-pentene",                    "763-29-1",  26000.,     26000.,   0.9268,    0.9268,    28050.)
    CALL CalcH("4-methyl-1-pentene",                    "691-37-2",  36100.,     36100.,   0.5703,    0.5703,    63295.)
    CALL CalcH("1-hexene",                              "592-41-6",  24800.,     24800.,   0.5941,    0.5941,    41743.)
    CALL CalcH("1-heptene",                             "592-76-7",  7510.,      7510.,    0.1864,    0.1864,    40295.)
    CALL CalcH("1-octene",                              "111-66-0",  2320.,      2320.,    0.0241,    0.0241,    96420.)
    CALL CalcH("1-nonene",                              "124-11-8",  712.,       712.,     0.0089,    0.0089,    80250.)
    CALL CalcH("1,3-butadiene",                         "106-99-0",  281000.,    281000.,  13.588,    13.588,    7458.)
    CALL CalcH("2-methyl-1,3-butadiene (isoprene)",      "78-79-5",  73300.,     73300.,   9.4248,    9.4248,    7780.)
    CALL CalcH("2,3-dimethyl-1,3-butadiene",            "513-81-5",  20160.,     20160.,   3.9809,    3.9809,    5065.)
    CALL CalcH("1,4-pentadiene",                        "591-93-5",  98000.,     98000.,   8.1917,    8.1917,    11965.)
    CALL CalcH("1,5-hexadiene",                         "592-42-7",  29690.,     29690.,   2.0574,    2.0574,    14430.)
    CALL CalcH("1-butyne",                              "107-00-6",  188000.,    188000.,  53.059,    53.059,    1910.)
    CALL CalcH("1-pentyne",                             "627-19-0",  57600.,     57600.,   23.048,    23.048,    2500.)
    CALL CalcH("1-hexyne",                              "693-02-7",  18140.,     18140.,   4.3830,    4.3830,    4140.)
    CALL CalcH("1-heptyne",                             "628-71-7",  7500.,      7000.,    0.9774,    0.9774,    7675.)
    CALL CalcH("1-octyne",                              "629-05-0",  1715.,      1715.,    0.2178,    0.2178,    7875.)
    CALL CalcH("cyclopentene",                          "142-29-0",  50710.,     50706.,   7.8540,    7.8540,    6455.)
    CALL CalcH("cyclohexene",                           "110-83-8",  11850.,     11850.,   2.5928,    2.5928,    4570.)
    CALL CalcH("1-methylcyclohexene",                   "591-49-1",  4689.,      4689.,    0.5407,    0.5407,    8670.)
    CALL CalcH("cycloheptene",                          "628-92-2",  2670.,      2670.,    0.6863,    0.6863,    3890.)
    CALL CalcH("cyclooctene",                           "931-88-4",  1010.,      1010.,    0.2078,    0.2078,    4860.)
    CALL CalcH("1,4-cyclohexadiene",                    "628-41-1",  9009.,      9009.,    9.9840,    9.9840,    902.)
    CALL CalcH("1,3,5-cycloheptatriene",                "544-25-2",  3140.,      3140.,    7.7376,    7.7376,    467.)
    CALL CalcH("d-limonene",                           "5989-27-5",  270.,       270.,     0.1013,    0.1013,    2665.)

    ! Table 3.2.1+3.2.2                                      ps       pl       cs       cl       H=p/c
    CALL CalcH("benzene",                       "71-43-2",  12700.,  12700.,  22.788,  22.788,  557.)
    CALL CalcH("toluene",                      "108-88-3",  3800.,   3800.,   5.590,   5.590,   680.)
    CALL CalcH("ethylbenzene",                 "100-41-4",  1270.,   1270.,   1.431,   1.431,   887.)
    CALL CalcH("o-xylene",                      "95-47-6",  1170.,   1170.,   2.072,   2.072,   565.)
    CALL CalcH("m-xylene",                     "108-38-3",  1100.,   1100.,   1.507,   1.507,   730.)
    CALL CalcH("p-xylene",                     "106-42-3",  1170.,   1170.,   2.024,   2.024,   578.)
    CALL CalcH("1,2,3-trimethylbenzene",       "526-73-8",  200.,    200.,    0.582,   0.582,   343.)
    CALL CalcH("1,2,4-trimethylbenzene",        "95-63-6",  270.,    270.,    0.474,   0.474,   569.)
    CALL CalcH("1,3,5-trimethylbenzene",       "108-67-8",  325.,    325.,    0.416,   0.416,   781.)
    CALL CalcH("n-propylbenzene",              "103-65-1",  450.,    450.,    0.433,   0.433,   1040.)
    CALL CalcH("isopropylbenzene",              "98-82-8",  610.,    610.,    0.416,   0.416,   1466.)
    CALL CalcH("1-ethyl-2-methylbenzene",      "611-14-3",  330.,    330.,    0.624,   0.624,   529.)
    CALL CalcH("1-ethyl-4-methylbenzene",      "622-96-8",  395.,    395.,    0.790,   0.790,   500.)
    CALL CalcH("isopropyl-4-methylbenzene",     "99-87-6",  204.,    204.,    0.253,   0.253,   805.)
    CALL CalcH("n-butylbenzene",               "104-51-8",  137.,    137.,    0.103,   0.103,   1332.)
    CALL CalcH("isobutylbenzene",              "538-93-2",  250.,    250.,    0.075,   0.075,   3322.)
    CALL CalcH("sec-butylbenzene",             "135-98-8",  240.,    240.,    0.127,   0.127,   1890.)
    CALL CalcH("tert-butylbenzene",             "98-06-6",  286.,    286.,    0.224,   0.224,   1280.)
    CALL CalcH("1,2,4,5-tetramethylbenzene",    "95-93-2",  66.,     66.,     0.026,   0.026,   2546.)
    CALL CalcH("n-pentylbenzene",              "538-68-1",  44.,     44.,     0.026,   0.026,   1694.)
    CALL CalcH("n-hexylbenzene",              "1077-16-3",  13.61,   13.61,   0.006,   0.006,   2165.)

    ! Table 4.2.1+4.2.2                                     ps         pl        cs            cl         H=p/c
    CALL CalcH("indan",                       "496-11-7",  197.,      197.,     846.2E-3,     846.2E-3,  232.8)
    CALL CalcH("naphthalene",                  "91-20-3",  10.4,      36.24,    241.9E-3,     842.7E-3,  43.00)
    CALL CalcH("1-methylnaphthalene",          "90-12-0",  8.84,      8.84,     196.9E-3,     196.9E-3,  44.89)
    CALL CalcH("2-methylnaphthalene",          "91-57-6",  9.0,       11.2,     175.8E-3,     218.4E-3,  51.19)
    CALL CalcH("1,4-dimethylnaphthalene",     "571-58-4",  2.27,      2.27,     72.97E-3,     72.97E-3,  31.11)
    CALL CalcH("2,3-dimethylnaphthalene",     "581-40-8",  1.0,       6.10,     16.00E-3,     97.58E-3,  62.49)
    CALL CalcH("2,6-dimethylnaphthalene",     "581-42-0",  1.4,       10.0,     10.88E-3,     77.73E-3,  128.7)
    CALL CalcH("1-ethylnaphthalene",         "1127-76-0",  2.51,      2.51,     64.65E-3,     64.65E-3,  38.82)
    CALL CalcH("2-ethylnaphthalene",          "939-27-5",  4.0,       4.0,      51.21E-3,     51.21E-3,  78.11)
    CALL CalcH("1,4,5-trimethylnaphthalene", "2131-41-1",  0.681,     1.61,     12.33E-3,     29.09E-3,  55.21)
    CALL CalcH("biphenyl",                     "92-52-4",  1.3,       3.50,     45.39E-3,     122.4E-3,  28.64)
    CALL CalcH("diphenylmethane",             "101-81-5",  0.0885,    0.0893,   95.10E-3,     95.10E-3,  0.931)
    CALL CalcH("bibenzyl",                    "103-29-7",  0.406,     0.756,    23.98E-3,     44.65E-3,  16.93)
    CALL CalcH("trans-stilbene",              "103-30-0",  0.065,     0.613,    1.609E-3,     15.18E-3,  40.40)
    CALL CalcH("acenaphthylene",              "208-96-8",  0.9,       4.14,     107.2E-3,     485.0E-3,  8.396)
    CALL CalcH("acenaphthene",                 "83-32-9",  0.3,       1.41,     24.64E-3,     115.7E-3,  12.17)
    CALL CalcH("fluorene",                     "86-73-7",  0.09,      0.682,    11.43E-3,     85.60E-3,  7.873)
    CALL CalcH("phenanthrene",                 "85-01-8",  0.02,      0.107,    6.172E-3,     33.00E-3,  3.240)
    CALL CalcH("anthracene",                  "120-12-7",  0.001,     0.0746,   0.252E-3,     18.84E-3,  3.961)
    CALL CalcH("9-methylanthracene",          "779-02-2",  0.00224,   0.00803,  1.358E-3,     4.866E-3,  1.650)
    CALL CalcH("9,10-dimethylanthracene",     "781-43-1",  1.53E-4,   5.50E-3,  0.271E-3,     9.765E-3,  0.564)
    CALL CalcH("pyrene",                      "129-00-0",  0.0006,    0.0119,   0.652E-3,     12.89E-3,  0.919)
    CALL CalcH("fluoranthene",                "206-44-0",  0.00123,   8.42E-3,  1.286E-3,     8.805E-3,  0.957)
    CALL CalcH("chrysene",                    "218-01-9",  5.70E-7,   1.07E-4,  0.00876E-3,   1.599E-3,  0.065)
    CALL CalcH("triphenylene",                "217-59-4",  2.30E-9,   1.21E-4,  0.188E-3,     9.325E-3,  0.012)
    CALL CalcH("p-terphenyl",                  "92-94-4",  4.86E-9,   3.47E-4,  0.0782E-3,    5.583E-3,  0.062)
    CALL CalcH("naphthacene",                  "92-24-0",  7.30E-9,   1.33E-5,  0.00263E-3,   4.779E-3,  2.77E-3)
    CALL CalcH("benz[a]anthracene",            "56-55-3",  2.80E-5,   5.98E-4,  0.0482E-3,    1.030E-3,  0.581)
    CALL CalcH("benzo[k]fluoranthene",        "207-08-9",  5.20E-8,   3.97E-9,  0.00317E-3,   0.242E-3,  0.016)
    CALL CalcH("benzo[a]pyrene",               "50-32-8",  7.00E-7,   2.38E-5,  0.0151E-3,    0.512E-3,  0.046)
    CALL CalcH("benzo[e]pyrene",              "192-97-2",  7.40E-7,   2.53E-5,  0.0159E-3,    0.543E-3,  0.047)
    CALL CalcH("perylene",                    "198-55-0",  1.40E-8,   4.23E-9,  0.00159E-3,   0.479E-3,  8.83E-3)
    CALL CalcH("7,12-dmba",                    "57-97-6",  3.84E-8,   3.45E-7,  0.195E-3,     1.757E-3,  1.97E-4)
    CALL CalcH("9,10-dmba",                 "58429-99-5",  3.73E-7,   3.33E-9,  0.170E-3,     1.543E-3,  2.20E-3)
    CALL CalcH("3-mca",                        "56-49-5",  1.03E-9,   3.42E-5,  0.00708E-3,   0.235E-3,  0.145)
    CALL CalcH("dibenz[a,c]anthracene",       "215-58-7",  1.30E-9,   7.84E-8,  0.00575E-3,   0.336E-3,  2.26E-4)
    CALL CalcH("dibenz[a,h]anthracene",        "53-70-3",  3.70E-10,  9.27E-8,  0.00216E-3,   0.540E-3,  1.72E-4)
    CALL CalcH("coronene",                    "191-07-1",  2.0E-10,   2.22E-9,  0.000466E-3,  5.179E-3,  4.29E-4)

    ref = "2612"

    ! Table 5.2.1+5.2.2                                                        ps        pl        cs      cl      H=p/c    exptl
    CALL tab522("chloromethane (methyl chloride)",                  "74-87-3", 570000.,  570000.,  105.5,  105.5,  960.7,   894.)
    CALL tab522("dichloromethane",                                  "75-09-2", 58000.,   58000.,   155.4,  155.4,  373.2,   300.)
    CALL tab522("trichloromethane (chloroform)",                    "67-66-3", 26200.,   26200.,   68.69,  68.69,  381.4,   427.)
    CALL tab522("tetrachloromethane (carbon tetrachloride)",        "56-23-5", 15250.,   15250.,   5.200,  5.200,  2932.,   2989.)
    CALL tab522("chloroethane (ethyl chloride)",                    "75-00-3", 16000.,   16000.,   88.35,  88.35,  181.1,   1023.)
    CALL tab522("1,1-dichloroethane",                               "75-34-3", 30260.,   30260.,   50.93,  50.93,  594.1,   569.)
    CALL tab522("1,2-dichloroethane",                              "107-06-2", 10540.,   10540.,   86.90,  86.90,  121.3,   143.)
    CALL tab522("1,1,1-trichloroethane",                            "71-55-6", 16500.,   16500.,   9.670,  9.670,  1706.,   1763.)
    CALL tab522("1,1,2-trichloroethane",                            "79-00-5", 3220.,    3220.,    34.41,  34.41,  93.59,   92.2)
    CALL tab522("1,1,1,2-tetrachloroethane",                       "630-20-6", 1580.,    1580.,    6.375,  6.375,  247.8,   DUMMY)
    CALL tab522("1,1,2,2-tetrachloroethane",                        "79-34-5", 793.,     793.,     16.86,  16.86,  47.03,   25.7)
    CALL tab522("pentachloroethane",                                "76-01-7", 590.,     590.,     2.422,  2.422,  243.6,   DUMMY)
    CALL tab522("hexachloroethane",                                 "67-72-1", 50.,      1923.,    0.2112, 8.123,  236.7,   846.)
    CALL tab522("1-chloropropane (n-propyl chloride)",             "540-54-5", 46000.,   46000.,   31.83,  31.83,  1445.,   DUMMY)
    CALL tab522("2-chloropropane",                                  "75-29-6", 68700.,   68700.,   38.20,  38.20,  1798.,   DUMMY)
    CALL tab522("1,2-dichloropropane",                              "78-87-5", 6620.,    6620.,    24.25,  24.25,  273.0,   287.)
    CALL tab522("1,2,3-trichloropropane",                           "96-18-4", 492.,     492.,     12.86,  12.86,  38.26,   DUMMY)
    CALL tab522("1-chlorobutane (n-butyl chloride)",               "109-69-3", 13700.,   13700.,   6.644,  6.644,  2062.,   1537.)
    CALL tab522("2-chlorobutane",                                   "78-86-4", 20210.,   20210.,   10.80,  10.80,  1871.,   2267.)
    CALL tab522("1-chloropentane (n-amyl chloride)",               "543-59-9", 4142.,    4142.,    1.858,  1.858,  2230.,   2375.)
    CALL tab522("chloroethene (vinyl chloride)",                    "75-01-4", 354600.,  354600.,  44.21,  44.21,  8021.,   2685.)
    CALL tab522("1,1-dichloroethene",                               "75-35-4", 80500.,   80500.,   34.49,  34.49,  2334.,   2624.)
    CALL tab522("cis-1,2-dichloroethene",                          "156-59-2", 27000.,   27000.,   36.10,  36.10,  747.8,   460.)
    CALL tab522("trans-1,2-dichloroethene",                        "156-60-5", 44400.,   44400.,   64.57,  64.57,  687.6,   958.)
    CALL tab522("trichloroethylene",                                "79-01-6", 9900.,    9900.,    8.372,  8.372,  1182.,   1034.)
    CALL tab522("tetrachloroethylene",                             "127-18-4", 2415.,    2415.,    0.904,  0.904,  2670.,   1733.)
    CALL tab522("hexachloro-1,3-butadiene",                         "87-68-3", 20.0,     20.0,     0.013,  0.013,  1630.,   DUMMY)
    CALL tab522("hexachlorocyclopentadiene",                        "77-47-4", 10.9,     10.9,     0.0066, 0.0066, 1652.,   DUMMY)
    CALL tab522("bromomethane",                                     "74-83-9", 217700.,  217700.,  160.3,  160.3,  631.9,   DUMMY)
    CALL tab522("dibromomethane",                                   "74-95-3", 6034.,    6034.,    65.82,  65.82,  91.67,   86.13)
    CALL tab522("tribromomethane",                                  "75-25-2", 727.,     727.,     12.27,  12.27,  59.27,   46.61)
    CALL tab522("bromoethane (ethyl bromide)",                      "74-96-4", 62500.,   62500.,   82.04,  82.04,  1235.,   DUMMY)
    CALL tab522("1,2-dibromoethane",                               "106-93-4", 1500.,    1500.,    22.10,  22.10,  67.87,   65.86)
    CALL tab522("1-bromopropane (n-propyl bromide)",               "106-94-5", 18440.,   18440.,   4.862,  4.862,  3792.,   DUMMY)
    CALL tab522("2-bromopropane",                                   "75-26-3", 31940.,   31940.,   25.09,  25.09,  1273.,   DUMMY)
    CALL tab522("1,2-dibromopropane",                               "78-75-1", 1040.,    1040.,    7.073,  7.073,  147.0,   DUMMY)
    CALL tab522("iodomethane (methyl iodide)",                      "74-88-4", 54000.,   54000.,   97.89,  97.89,  551.6,   541.)
    CALL tab522("iodoethane (ethyl iodide)",                        "75-03-6", 18160.,   18160.,   25.91,  25.91,  700.9,   DUMMY)
    CALL tab522("1-iodopropane (n-propyl iodide)",                 "107-08-4", 5745.,    5745.,    6.183,  6.183,  929.2,   DUMMY)
    CALL tab522("1-iodobutane (n-butyl iodide)",                   "542-69-8", 1848.,    1848.,    0.989,  0.989,  1868.,   DUMMY)
    CALL tab522("bromochloromethane",                               "74-97-5", 19600.,   19600.,   114.2,  114.2,  171.6,   DUMMY)
    CALL tab522("bromodichloromethane",                             "75-27-4", 6670.,    6670.,    27.47,  27.47,  242.8,   162.)
    CALL tab522("dibromochloromethane",                            "124-48-1", DUMMY,    DUMMY,    19.20,  19.20,  86.13,   DUMMY)
    CALL tab522("chlorodifluoromethane (HCFC-22)",                  "75-45-6", 1044000., 1044000., 33.53,  33.53,  3022.,   DUMMY)
    CALL tab522("dichlorodifluoromethane (CFC-12)",                 "75-71-8", 651000.,  651000.,  2.481,  2.481,  40840.,  DUMMY)
    CALL tab522("trichlorofluoromethane (CFC-11)",                  "75-69-4", 102200.,  102200.,  7.862,  7.862,  12890.,  10243.)
    CALL tab522("1,1,2-trichloro-1,2,2-trifluoroethane (CFC-113)",  "76-13-1", 48320.,   48320.,   0.886,  0.886,  114375., 32323.)

    ! Table 6.2.1+6.2.2                                     ps       pl       cs         cl         H=p/c
    CALL CalcH("chlorobenzene",               "108-90-7",  1580.,   1580.,   4.3000,    4.2300,    367.)
    CALL CalcH("1,2-dichlorobenzene",          "95-50-1",  170.,    170.,    0.9523,    0.9523,    178.)
    CALL CalcH("1,3-dichlorobenzene",         "541-73-1",  260.,    260.,    0.8163,    0.8163,    318.)
    CALL CalcH("1,4-dichlorobenzene",         "106-46-7",  130.,    245.,    0.544,     1.0268,    242.)
    CALL CalcH("1,2,3-trichlorobenzene",       "87-61-6",  28.,     50.72,   0.1157,    0.2100,    242.)
    CALL CalcH("1,2,4-trichlorobenzene",      "120-82-1",  40.,     40.,     0.2204,    0.2204,    277.)
    CALL CalcH("1,3,5-trichlorobenzene",      "108-70-3",  25.,     78.05,   0.0292,    0.0686,    856.)
    CALL CalcH("1,2,3,4-tetrachlorobenzene",  "634-66-2",  4.0,     6.64,    0.0361,    0.0600,    111.)
    CALL CalcH("1,2,3,5-tetrachlorobenzene",  "634-90-2",  9.8,     19.01,   0.0167,    0.0324,    588.)
    CALL CalcH("1,2,4,5-tetrachlorobenzene",   "95-94-3",  0.72,    9.56,    0.00588,   0.0781,    122.)
    CALL CalcH("pentachlorobenzene",          "608-93-5",  0.22,    0.8730,  0.00260,   0.0103,     85.)
    CALL CalcH("hexachlorobenzene",           "118-74-1",  0.0023,  0.23,    0.0000176, 0.00176,   131.)

    ! Table 7.2.1+7.2.2                  ps        pl       cs           cl         H=p/c
    CALL CalcH("PCB-0",      "92-52-4", 1.3,      3.50,    45.39E-3,    122.4E-3,  28.64)
    CALL CalcH("PCB-1",    "2051-60-7", 2.04,     2.5,     29.15E-3,    35.73E-3,  69.97)
    CALL CalcH("PCB-2",    "2051-61-8", 1.,       1.,      13.25E-3,    13.24E-3,  75.46)
    CALL CalcH("PCB-3",    "2051-62-9", 0.271,    0.91,    6.36E-3,     21.42E-3,  42.60)
    CALL CalcH("PCB-4",   "13029-08-8", 0.265,    0.59,    4.482E-3,    10.00E-3,  59.12)
    CALL CalcH("PCB-7",   "33284-50-3", 0.254,    0.254,   5.603E-3,    5.603E-3,  45.33)
    CALL CalcH("PCB-9",   "34883-39-1", 0.18,     0.18,    8.960E-3,    8.960E-3,  20.08)
    CALL CalcH("PCB-11",   "2050-67-1", 0.027,    0.030,   1.587E-3,    1.736E-3,  17.02)
    CALL CalcH("PCB-15",   "2050-68-2", 0.0048,   0.080,   0.269E-3,    4.460E-3,  17.84)
    CALL CalcH("PCB-18",  "37680-65-2", 0.143,    0.220,   1.553E-3,    2.386E-3,  92.07)
    CALL CalcH("PCB-29",  "15862-07-4", 0.132,    0.441,   0.544E-3,    1.818E-3,  242.8)
    CALL CalcH("PCB-30",  "35693-92-6", 0.0384,   0.090,   0.777E-3,    1.810E-3,  49.45)
    CALL CalcH("PCB-33",  "38444-86-9", 0.0136,   0.003,   0.311E-3,    0.684E-3,  43.78)
    CALL CalcH("PCB-40",  "38444-93-8", 0.00225,  0.0197,  0.103E-3,    0.901E-3,  21.90)
    CALL CalcH("PCB-47",   "2437-79-8", 0.0054,   0.02,    0.308E-3,    1.142E-3,  17.52)
    CALL CalcH("PCB-52",  "35693-99-3", 0.0049,   0.02,    0.103E-3,    0.418E-3,  47.69)
    CALL CalcH("PCB-77",  "32598-13-3", 5.88E-5,  0.002,   0.00342E-3,  0.114E-3,  17.16)
    CALL CalcH("PCB-86",  "55312-69-1", 0.00927,  0.0504,  0.0613E-3,   0.333E-3,  151.3)
    CALL CalcH("PCB-87",  "38380-02-8", 0.000304, 0.00227, 0.0123E-3,   0.0914E-3, 24.81)
    CALL CalcH("PCB-101", "37680-73-2", 0.00109,  0.00364, 0.0306E-3,   0.102E-3,  35.58)
    CALL CalcH("PCB-104", "56558-16-8", DUMMY,    0.00434, 0.0478E-3,   0.185E-3,  23.43)
    CALL CalcH("PCB-128", "38380-07-3", 1.98E-5,  0.00034, 0.00166E-3,  0.0286E-3, 11.91)
    CALL CalcH("PCB-153", "35065-27-1", 0.000119, 0.0007,  0.00277E-3,  0.0163E-3, 42.94)
    CALL CalcH("PCB-155", "33979-03-2", 0.00048,  0.00345, 0.00554E-3,  0.0399E-3, 86.62)
    CALL CalcH("PCB-171", "52663-71-5", 2.73E-5,  0.00022, 0.00506E-3,  0.0408E-3, 5.396)
    CALL CalcH("PCB-202",  "2136-99-4", 2.66E-5,  0.0006,  0.0007E-3,   0.0158E-3, 38.11)
    CALL CalcH("PCB-206", "40186-72-9", 1.96E-7,  1.17E-5, 0.000237E-3, 0.0141E-3, 0.8271)
    CALL CalcH("PCB-209",  "2051-24-3", 5.02E-8,  3.06E-5, 2.00E-9,     0.0122E-3, 25.03)

    ! Table 8.2.1+8.2.2                                                     ps          pl        cs            cl           H=p/c
    CALL CalcH("dibenzo-p-dioxin",                             "262-12-4", 0.055,      0.474,    4.696E-3,     40.48E-3,    11.71)
    CALL CalcH("1-chlorodibenzo-p-dioxin",                   "39227-53-7", 0.012,      0.074,    1.907E-3,     11.77E-3,    6.292)
    CALL CalcH("2-chlorodibenzo-p-dioxin",                   "39227-54-8", 0.017,      0.0730,   1.350E-3,     5.717E-3,    12.60)
    CALL CalcH("2,3-dichlorodibenzo-p-dioxin",               "29446-15-9", 0.00039,    0.00901,  0.0589E-3,    1.360E-3,    6.624)
    CALL CalcH("2,7-dichlorodibenzo-p-dioxin",               "33857-26-0", 0.00012,    0.00811,  0.0148E-3,    0.788E-3,    8.098)
    CALL CalcH("2,8-dichlorodibenzo-p-dioxin",               "38964-22-6", 0.00014,    0.00241,  0.0660E-3,    1.138E-3,    2.122)
    CALL CalcH("1,2,4-trichlorodibenzo-p-dioxin",            "39227-58-2", 0.0001,     0.00105,  0.0293E-3,    0.306E-3,    3.419)
    CALL CalcH("1,2,3,4-tetrachlorodibenzo-p-dioxin",        "30746-58-8", 6.40E-6,    2.60E-4,  0.0017E-3,    0.0694E-3,   3.747)
    CALL CalcH("1,2,3,7-tetrachlorodibenzo-p-dioxin",        "67028-18-6", 1.00E-6,    2.77E-5,  0.0013E-3,    0.0361E-3,   0.766)
    CALL CalcH("1,3,6,8-tetrachlorodibenzo-p-dioxin",        "30746-58-8", 7.00E-7,    5.60E-5,  0.000994E-3,  0.0795E-3,   0.704)
    CALL CalcH("2,3,7,8-tetrachlorodibenzo-p-dioxin",         "1746-01-6", 2.00E-7,    8.93E-5,  0.00006E-3,   0.0268E-3,   3.336)
    CALL CalcH("1,2,3,4,7-pentachlorodibenzo-p-dioxin",      "39227-61-7", 8.80E-8,    4.09E-6,  0.000331E-3,  0.0154E-3,   0.266)
    CALL CalcH("1,2,3,4,7,8-hexachlorodibenzo-p-dioxin",     "39227-28-6", 5.10E-9,    1.38E-6,  1.54E-8,      0.00416E-3,  0.332)
    CALL CalcH("1,2,3,4,6,7,8-heptachlorodibenzo-p-dioxin",  "35822-46-9", 7.50E-10,   1.70E-7,  5.64E-9,      0.00128E-3,  0.133)
    CALL CalcH("octachlorodibenzo-p-dioxin",                  "3268-87-9", 1.10E-10,   1.11E-7,  1.61E-10,     0.00016E-3,  0.683)

    ! Table 9.2.1+9.2.2                                                   ps         pl        cs          cl          H=p/c
    CALL CalcH("dibenzofuran",                             "132-64-9",   0.400,     1.606,    28.240E-3,  114.42E-3,  14.16)
    CALL CalcH("2,8-dichlorodibenzofuran",                "5409-83-6",   0.00039,   1.46E-2,  0.0612E-3,  2.246E-3,   6.377)
    CALL CalcH("2,3,7,8-tetrachlorodibenzofuran",        "51207-31-9",   2.00E-6,   1.92E-4,  1.37E-6,    0.132E-3,   1.461)
    CALL CalcH("2,3,4,7,8-pentachlorodibenzofuran",      "57117-31-4",   3.50E-7,   1.68E-5,  6.93E-7,    0.0332E-3,  0.505)
    CALL CalcH("1,2,3,4,7,8-hexachlorodibenzofuran",     "70648-26-9",   3.20E-8,   2.99E-6,  2.20E-8,    2.06E-6,    1.454)
    CALL CalcH("1,2,3,4,6,7,8-heptachlorodibenzofuran",  "67562-39-4",   4.70E-9,   5.59E-7,  3.30E-9,    3.92E-7,    1.425)
    CALL CalcH("octachlorodibenzofuran",                 "39001-02-0",   5.0E-10,   9.65E-8,  2.61E-9,    5.05E-8,    0.191)

    ref = "2613"

    ! Table 10.2.1+10.2.2                                          ps        pl        cs       cl       H=p/c
    CALL CalcH("dimethyl ether (methyl ether)",      "115-10-6",  600000.,  600000.,  7662.,   7662.,   13.22)
    CALL CalcH("diethyl ether (ethyl ether)",         "60-29-7",  71600.,   71600.,   816.2,   816.2,   87.72)
    CALL CalcH("methyl t-butyl ether (mtbe)",       "1634-04-4",  33500.,   33500.,   476.5,   476.5,   70.31)
    CALL CalcH("di-n-propyl ether",                  "111-43-3",  8334.,    8334.,    32.35,   32.35,   257.6)
    CALL CalcH("di-isopropyl ether",                 "108-20-3",  20000.,   20000.,   77.31,   77.31,   258.7)
    CALL CalcH("butyl ethyl ether",                  "628-81-9",  8200.,    8200.,    63.61,   63.61,   128.9)
    CALL CalcH("di-n-butyl ether",                   "142-96-1",  850.,     850.,     1.766,   1.776,   481.3)
    CALL CalcH("1,2-propylene oxide",                 "75-56-9",  71000.,   71000.,   8196.,   8196.,   8.663)
    CALL CalcH("furan",                              "110-00-9",  80000.,   80000.,   146.9,   146.9,   544.6)
    CALL CalcH("tetrahydropyran",                    "142-68-7",  9536.,    9536.,    995.0,   995.0,   9.584)
    CALL CalcH("epichlorohydrin",                    "106-89-8",  2400.,    2400.,    771.2,   711.2,   3.37)
    CALL CalcH("bis(chloromethyl)ether",             "542-88-1",  4000.,    4000.,    191.4,   191.4,   20.90)
    CALL CalcH("bis(2-chloroethyl)ether",            "111-44-4",  206.,     206.,     71.32,   71.32,   2.888)
    CALL CalcH("bis(2-chloroisopropyl)ether",        "108-60-1",  104.,     104.,     9.938,   9.938,   10.46)
    CALL CalcH("2-chloroethyl vinyl ether",          "110-75-8",  3566.,    3566.,    140.8,   140.8,   25.33)
    CALL CalcH("bis(2-chloroethoxy)methane",         "111-91-1",  21.6,     21.6,     468.1,   468.1,   0.0461)
    CALL CalcH("anisole (methoxybenzene)",           "100-66-3",  472.,     472.,     14.80,   14.80,   31.90)
    CALL CalcH("phenetole (ethoxybenzene)",          "103-73-1",  204.,     204.,     4.658,   4.658,   43.80)
    CALL CalcH("styrene oxide",                       "96-09-3",  40.,      40.,      23.30,   23.0,    1.716)
    CALL CalcH("diphenyl ether",                     "101-84-8",  2.93,     3.05,     0.1099,  0.1146,  26.67)

    ! Table 11.2.1+11.2.2                                       cl      pl      H=p/c   exptla  exptlb exptlc
    CALL tab1122("methanol",                          "67-56-1", DUMMY,  16210., DUMMY,  0.45,   DUMMY, 0.45)
    CALL tab1122("ethanol",                           "64-17-5", DUMMY,  7800.,  DUMMY,  0.53,   DUMMY, 0.527)
    CALL tab1122("propanol (n-propyl alcohol)",       "71-23-8", DUMMY,  2780.,  DUMMY,  DUMMY,  DUMMY, 0.751)
    CALL tab1122("isopropanol (i-propyl alcohol)",    "67-63-0", DUMMY,  5700.,  DUMMY,  DUMMY,  DUMMY, 0.80)
    CALL tab1122("1-butanol (n-butyl alcohol)",       "71-36-3", 998.4,  900.,   0.9014, 0.80,   0.892, 0.80)
    CALL tab1122("isobutanol (i-butyl alcohol)",      "78-83-1", 1093.,  1500.,  1.3726, 0.99,   DUMMY, 0.99)
    CALL tab1122("sec-butyl alcohol",                 "78-92-2", 2442.,  2300.,  0.9419, 0.80,   DUMMY, 0.918)
    CALL tab1122("tert-butyl alcohol",                "75-65-0", DUMMY,  5500.,  DUMMY,  1.46,   DUMMY, 1.46)
    CALL tab1122("1-pentanol (n-amyl alcohol)",       "71-41-0", 249.6,  300.,   1.202,  1.314,  DUMMY, DUMMY)
    CALL tab1122("2-pentanol",                      "6032-29-7", 510.5,  777.,   1.522,  DUMMY,  DUMMY, DUMMY)
    CALL tab1122("1-hexanol",                        "111-27-3", 58.72,  110.,   1.873,  1.562,  1.735, DUMMY)
    CALL tab1122("1-heptanol",                       "111-70-6", 14.97,  24.,    1.603,  1.909,  DUMMY, DUMMY)
    CALL tab1122("1-octanol (n-octyl alcohol)",      "111-87-5", 4.146,  11.,    2.653,  2.454,  2.48,  DUMMY)
    ! value for ethylene glycol is NOT from Butler et al.; value not used here:
    !CALL tab1122("ethylene glycol",                 "107-21-1", DUMMY,  12.,    DUMMY,  0.006,  DUMMY, DUMMY)
    CALL tab1122("cyclohexanol",                     "108-93-0", 379.4,  85.,    0.224,  DUMMY,  DUMMY, DUMMY)
    CALL tab1122("benzyl alcohol",                   "100-51-6", 0.7398, 12.,    16.22,  DUMMY,  DUMMY, DUMMY)

    ! Table 12.2.1+12.2.2                                        cl      pl       H=p/c    exptla exptlb  exptlc exptld
    CALL tab1222("methanal (formaldehyde)",            "50-00-0", DUMMY,  517000., DUMMY,   DUMMY, 0.0298, DUMMY, 0.0341)
    CALL tab1222("ethanal (acetaldehyde)",             "75-07-0", DUMMY,  121300., DUMMY,   6.69,  6.80,   8.00,  8.90)
    CALL tab1222("propanal (propionaldehyde)",        "123-38-6", 5338.,  42400.,  7.944,   7.44,  8.40,   7.51,  DUMMY)
    CALL tab1222("butanal (n-butyraldehyde)",         "123-72-8", 984.7,  15200.,  15.44,   11.65, 11.65,  DUMMY, DUMMY)
    CALL tab1222("pentanal (n-valeraldehyde)",        "110-62-3", DUMMY,  2180.,   DUMMY,   14.87, 15.59,  DUMMY, DUMMY)
    CALL tab1222("hexanal",                            "66-25-1", 50.12,  DUMMY,   DUMMY,   21.57, 19.49,  DUMMY, DUMMY)
    CALL tab1222("2-propenal (acrolein)",             "107-02-8", 3710.,  36500.,  9.838,   13.37, DUMMY,  13.17, DUMMY)
    CALL tab1222("2-butenal",                         "123-73-9", 222.6,  5100.,   22.91,   DUMMY, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("furfural (2-furaldehyde)",           "98-01-1", 826.4,  310.,    0.3751,  DUMMY, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("benzaldehyde",                      "100-52-7", 28.27,  174.,    6.155,   2.28,  DUMMY,  DUMMY, 2.71)
    CALL tab1222("acetone",                            "67-64-1", DUMMY,  30800.,  DUMMY,   3.97,  DUMMY,  3.93,  3.07)
    CALL tab1222("2-butanone (methyl ethyl ketone)",   "78-93-3", 3328.4, 12100.,  3.635,   9.71,  DUMMY,  5.76,  DUMMY)
    CALL tab1222("2-pentanone",                       "107-87-9", 690.8,  4720.,   6.833,   6.44,  DUMMY,  DUMMY, DUMMY)
    CALL tab1222("3-pentanone",                        "96-22-0", 394.7,  4700.,   11.91,   DUMMY, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("methyl isobutyl ketone (mibk)",     "108-10-1", 169.7,  2600.,   15.32,   DUMMY, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("2-hexanone",                        "591-78-6", 174.7,  1600.,   9.157,   DUMMY, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("2-heptanone",                       "110-43-0", 37.66,  500.,    13.28,   14.73, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("2-octanone",                        "111-13-7", 8.814,  180.,    20.42,   19.09, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("cyclohexanone",                     "108-94-1", 234.4,  620.,    2.646,   DUMMY, DUMMY,  DUMMY, DUMMY)
    CALL tab1222("acetophenone",                       "98-86-2", 45.78,  45.,     0.983,   DUMMY, DUMMY,  DUMMY, 0.92)
    CALL tab1222("benzophenone",                      "119-61-9", 2.541,  0.151,   0.059,   DUMMY, DUMMY,  DUMMY, DUMMY)

    ! Table 13.2.1+13.2.2                                        cs      cl      ps       pl      H=p/c    exptla    calcd
    CALL tab1322("formic acid",                     "64-18-6",    DUMMY,  DUMMY,  5750.,   5750.,  DUMMY,   0.01832,  DUMMY)
    CALL tab1322("acetic acid",                     "64-19-7",    DUMMY,  DUMMY,  2079.,   2079.,  DUMMY,   0.01825,  0.0285)
    CALL tab1322("propionic acid",                  "79-09-4",    DUMMY,  DUMMY,  435.,    435.,   DUMMY,   0.018,    0.0431)
    CALL tab1322("butyric acid",                    "107-92-6",   DUMMY,  DUMMY,  84.,     84.,    DUMMY,   0.0222,   0.0650)
    CALL tab1322("isobutyric acid",                 "79-31-2",    258.8,  258.8,  185.,    185.,   0.715,   0.0897,   DUMMY)
    CALL tab1322("n-valeric acid",                  "109-52-4",   235.0,  235.0,  19.,     19.,    0.081,   0.0478,   0.099)
    CALL tab1322("3-methylbutanoic acid",           "503-74-2",   40.14,  40.14,  25.,     25.,    0.623,   0.0844,   DUMMY)
    CALL tab1322("hexanoic acid (caproic acid)",    "142-62-1",   82.47,  82.47,  5.,      5.,     0.0606,  0.0768,   0.149)
    CALL tab1322("octanoic acid",                   "124-07-2",   0.553,  0.553,  3.72,    3.72,   6.723,   DUMMY,    0.338)
    CALL tab1322("2-methylpropenoic acid",          "79-41-4",    103.4,  103.4,  100.,    100.,   0.9673,  DUMMY,    DUMMY)
    CALL tab1322("benzoic acid",                    "65-85-0",    27.84,  250.8,  0.11,    0.991,  3.95E-3, DUMMY,    DUMMY)
    CALL tab1322("3-methylbenzoic acid",            "99-04-7",    9.152,  62.26,  1.39,    9.456,  0.1512,  DUMMY,    DUMMY)
    CALL tab1322("phenylacetic acid",               "103-82-2",   121.9,  390.8,  0.83,    2.660,  6.81E-3, DUMMY,    DUMMY)
    CALL tab1322("salicylic acid",                  "69-72-7",    16.65,  344.0,  0.0208,  0.440,  1.25E-3, DUMMY,    DUMMY)
    CALL tab1322("2,4-dichlorophenoxyacetic acid",  "94-75-7",    4.026,  54.71,  8.0E-5,  0.001,  1.99E-5, DUMMY,    DUMMY)

    ! Table 14.2.1+14.2.2                                               ps          pl       cs       cl       H=p/c
    CALL CalcH("phenol",                               "108-95-2",     47.,        67.66,   938.9,   1345.,   0.0500)
    CALL CalcH("o-cresol (2-methylphenol)",            "95-48-7",      DUMMY,      41.,     240.4,   237.32,  0.1489)
    CALL CalcH("m-cresol (3-methylphenol)",            "108-39-4",     DUMMY,      16.,     203.4,   203.4,   0.0786)
    CALL CalcH("p-cresol (4-methylphenol)",            "106-44-5",     DUMMY,      13.,     184.9,   230.6,   0.0564)
    CALL CalcH("2,3-dimethylphenol",                   "526-75-0",     DUMMY,      8.05,    49.11,   143.6,   0.0560)
    CALL CalcH("2,4-dimethylphenol",                   "105-67-9",     DUMMY,      13.02,   71.99,   71.99,   0.1808)
    CALL CalcH("2,5-dimethylphenol",                   "95-87-4",      DUMMY,      10.68,   26.00,   80.00,   0.1335)
    CALL CalcH("2,6-dimethylphenol",                   "576-26-1",     DUMMY,      32.82,   51.00,   81.60,   0.4022)
    CALL CalcH("3,4-dimethylphenol",                   "95-65-8",      DUMMY,      2.241,   41.75,   103.3,   0.0217)
    CALL CalcH("3,5-dimethylphenol",                   "108-68-9",     DUMMY,      3.76,    45.02,   107.2,   0.0351)
    CALL CalcH("2,3,5-trimethylphenol",                "697-82-5",     DUMMY,      2.43,    5.874,   28.24,   0.0860)
    CALL CalcH("2,4,6-trimethylphenol",                "527-60-6",     DUMMY,      19.58,   8.81,    26.07,   0.7511)
    CALL CalcH("3,4,5-trimethylphenol",                "527-54-8",     DUMMY,      2.144,   11.31,   73.91,   0.0290)
    CALL CalcH("o-ethylphenol",                        "90-00-6",      DUMMY,      20.4,    114.9,   114.9,   0.1775)
    CALL CalcH("p-ethylphenol",                        "123-07-9",     DUMMY,      5.,      65.32,   102.7,   0.0487)
    CALL CalcH("4-propylphenol",                       "645-56-7",     DUMMY,      7.3,     12.69,   12.69,   0.5753)
    CALL CalcH("2-isopropylpheonol",                   "88-69-7",      DUMMY,      12.45,   32.48,   32.48,   0.3834)
    CALL CalcH("4-sec-butylphenol",                    "99-71-8",      DUMMY,      4.,      6.391,   14.59,   0.2741)
    CALL CalcH("4-tert-butylphenol",                   "98-54-4",      DUMMY,      1.24,    3.861,   20.11,   0.0617)
    CALL CalcH("4-octylphenol",                        "1806-26-4",    DUMMY,      0.071,   0.0611,  0.0917,  0.7743)
    CALL CalcH("4-nonylphenol",                        "104-40-5",     DUMMY,      0.1,     0.0246,  0.0362,  2.764)
    CALL CalcH("1-naphthol",                           "90-15-3",      DUMMY,      0.5,     3.038,   14.75,   0.0339)
    CALL CalcH("2-naphthol",                           "135-19-3",     DUMMY,      0.4,     5.133,   45.42,   8.81E-3)
    CALL CalcH("2-phenylphenol (2-hydroxybiphenyl)",   "90-43-7",      DUMMY,      30.,     4.113,   8.568,   3.501)
    CALL CalcH("4-phenylphenol (4-hydroxybiphenyl)",   "92-69-3",      DUMMY,      8.7,     0.0576,  1.391,   6.256)
    CALL CalcH("2-chlorophenol",                       "95-57-8",      132.,       132.,    191.7,   191.7,   0.6884)
    CALL CalcH("3-chlorophenol",                       "108-43-0",     35.,        41.57,   171.1,   203.2,   0.2045)
    CALL CalcH("4-chlorophenol",                       "106-48-9",     20.,        29.90,   210.0,   313.9,   0.0952)
    CALL CalcH("2,4-dichlorophenol",                   "120-83-2",     12.,        18.87,   27.61,   43.41,   0.4347)
    CALL CalcH("2,6-dichlorophenol",                   "87-65-0",      12.,        32.09,   16.10,   43.06,   0.7451)
    CALL CalcH("2,3,4-trichlorophenol",                "15950-66-0",   1.,         3.74,    2.532,   9.484,   0.3949)
    CALL CalcH("2,3,5-trichlorophenol",                "933-78-8",     1.,         2.31,    2.532,   5.848,   0.3949)
    CALL CalcH("2,4,5-trichlorophenol",                "95-95-4",      2.5,        6.76,    4.801,   12.98,   0.5207)
    CALL CalcH("2,4,6-trichlorophenol",                "88-06-2",      1.25,       3.44,    2.198,   5.941,   0.5687)
    CALL CalcH("2,3,4,5-tetrachlorophenol",            "4901-51-3",    0.1,        0.79,    0.7158,  5.637,   0.1397)
    CALL CalcH("2,3,4,6-tetrachlorophenol",            "58-90-2",      0.28,       0.78,    0.7892,  2.180,   0.3548)
    CALL CalcH("2,3,5,6-tetrachlorophenol",            "935-95-5",     0.1,        0.78,    0.4312,  3.292,   0.2319)
    CALL CalcH("pentachlorophenol",                    "87-86-5",      4.15E-3,    0.12,    0.0526,  1.524,   0.0789)
    CALL CalcH("2-nitrophenol",                        "88-75-5",      DUMMY,      20.,     7.764,   12.15,   1.646)
    CALL CalcH("catechol (1,2-dihydroxybenzenene)",    "120-80-9",     DUMMY,      1.34,    408.7,   2462.,   5.44E-4)
    CALL CalcH("resorcinol (1,3-dihydroxybenzene)",    "108-46-3",     0.0118,     0.079,   999.0,   6705.,   1.18E-5)
    CALL CalcH("hydroquinone",                         "123-31-9",     2.55E-3,    0.071,   635.7,   17758.,  4.01E-6)
    CALL CalcH("2-methoxyphenol (guaiacol)",           "90-05-1",      20.825,     24.4,    199.8,   233.9,   0.1042)
    CALL CalcH("4,5-dichloroguaiacol",                 "2460-49-3",    0.570,      1.72,    2.98,    9.000,   0.1913)
    CALL CalcH("3,4,5-trichloroguaiacol",              "57057-83-7",   0.163,      0.64,    1.363,   5.344,   0.1996)
    CALL CalcH("4,5,6-trichloroguaiacol",              "2668-24-8",    0.032,      0.23,    0.2374,  1.733,   0.1348)
    CALL CalcH("tetrachloroguaiacol",                  "2539-17-5",    0.016,      0.14,    0.0993,  0.8785,  0.1612)

    ! Table 15.2.1+15.2.2                                    ps       pl       cs        cl        H=p/c   exptla  exptlb
    CALL tab1522("methyl formate",               "107-31-3",  78060.,  78060.,  3830.,    3830.,    20.38,  DUMMY,  DUMMY)
    CALL tab1522("ethyl formate",                "109-94-4",  32370.,  32600.,  1593.,    1593.,    20.32,  DUMMY,  DUMMY)
    CALL tab1522("propyl formate",               "110-74-7",  11030.,  11030.,  232.7,    232.7,    47.40,  DUMMY,  DUMMY)
    CALL tab1522("isobutyl formate",             "542-55-2",  5343.,   5400.,   97.91,    97.91,    54.57,  DUMMY,  DUMMY)
    CALL tab1522("methyl acetate",                "79-20-9",  28800.,  28800.,  3307.,    3307.,    8.708,  0.65,   13.06)
    CALL tab1522("vinyl acetate",                "108-05-4",  14100.,  14100.,  232.3,    232.3,    60.69,  DUMMY,  DUMMY)
    CALL tab1522("ethyl acetate",                "141-78-6",  12600.,  12600.,  917.1,    917.1,    13.74,  DUMMY,  17.2)
    CALL tab1522("propyl acetate",               "109-60-4",  4500.,   4500.,   205.6,    205.6,    21.88,  DUMMY,  22.09)
    CALL tab1522("butyl acetate",                "123-86-4",  1600.,   1600.,   51.65,    51.65,    30.98,  DUMMY,  29.506)
    CALL tab1522("isobutyl acetate",             "110-19-0",  2860.,   2860.,   54.24,    54.24,    52.73,  DUMMY,  DUMMY)
    CALL tab1522("pentyl acetate",               "628-63-7",  550.,    550.,    13.06,    13.06,    42.12,  DUMMY,  35.94)
    CALL tab1522("isopentyl acetate",            "123-92-2",  600.,    600.,    15.36,    15.36,    39.06,  DUMMY,  DUMMY)
    CALL tab1522("hexyl acetate",                "142-92-7",  670.,    670.,    3.446,    3.467,    193.2,  DUMMY,  DUMMY)
    CALL tab1522("2-ethylhexyl acetate",         "103-09-3",  53.,     53.,     0.5712,   0.5712,   92.79,  DUMMY,  DUMMY)
    CALL tab1522("methyl propionate",            "554-12-1",  11600.,  11600.,  707.9,    707.9,    16.39,  17.6,   DUMMY)
    CALL tab1522("ethyl propionate",             "105-37-3",  4966.,   4966.,   188.0,    188.0,    26.42,  DUMMY,  DUMMY)
    CALL tab1522("methyl butyrate",              "623-42-7",  DUMMY,   DUMMY,   DUMMY,    DUMMY,    DUMMY,  20.82,  DUMMY)
    CALL tab1522("ethyl butyrate",               "105-54-4",  2300.,   2300.,   55.96,    55.96,    41.10,  DUMMY,  DUMMY)
    CALL tab1522("methyl pentanoate",            "624-24-8",  DUMMY,   DUMMY,   DUMMY,    DUMMY,    DUMMY,  32.22,  DUMMY)
    CALL tab1522("methyl hexanoate",             "106-70-7",  DUMMY,   DUMMY,   DUMMY,    DUMMY,    DUMMY,  37.18,  DUMMY)
    CALL tab1522("methyl octanoate",             "111-11-5",  DUMMY,   DUMMY,   DUMMY,    DUMMY,    DUMMY,  79.32,  DUMMY)
    CALL tab1522("methyl acrylate",               "96-33-3",  11000.,  11000.,  573.8,    573.8,    19.17,  DUMMY,  DUMMY)
    CALL tab1522("ethyl acrylate",               "140-88-5",  5100.,   5100.,   149.82,   149.82,   34.041, DUMMY,  DUMMY)
    CALL tab1522("methyl methacrylate",           "80-62-6",  5100.,   5100.,   155.8,    155.8,    32.73,  DUMMY,  DUMMY)
    CALL tab1522("methyl benzoate",               "93-58-3",  52.58,   52.28,   15.42,    15.42,    3.389,  DUMMY,  DUMMY)
    CALL tab1522("ethyl benzoate",                "93-89-0",  24.,     24.,     2.331,    2.331,    10.30,  DUMMY,  DUMMY)
    CALL tab1522("benzyl benzoate",              "120-51-4",  0.043,   0.043,   0.0754,   0.0754,   0.5704, DUMMY,  DUMMY)
    CALL tab1522("dimethyl phthalate (DMP)",     "131-11-3",  0.22,    0.22,    20.60,    20.60,    0.0107, DUMMY,  DUMMY)
    CALL tab1522("diethyl phthalate (DEP)",       "84-66-2",  0.22,    0.22,    4.859,    4.859,    0.0453, DUMMY,  DUMMY)
    CALL tab1522("di-n-butyl phthalate (DBP)",    "84-74-2",  0.00187, 0.00187, 0.0402,   0.0402,   0.0465, DUMMY,  DUMMY)
    CALL tab1522("di-n-octyl phthalate (DOP)",   "117-84-0",  1.33E-5, 1.33E-5, 1.28E-6,  1.28E-6,  10.39,  DUMMY,  DUMMY)
    CALL tab1522("bis(2-ethylhexyl)-phthalate",  "117-81-7",  1.33E-5, 1.33E-5, 7.68E-6,  7.68E-6,  1.731,  DUMMY,  DUMMY)
    CALL tab1522("butyl benzyl phthalate",        "85-68-7",  0.00115, 0.00115, 0.0086,   0.0086,   0.1335, DUMMY,  DUMMY)

    ref = "2614"

    ! Table 16.2.1+16.2.2                                   ps       pl       cs        cl        H=p/c     exptl
    CALL tab1622("acetonitrile",                  "75-05-8", 11840.,  11840.,  DUMMY,    DUMMY,    DUMMY,    2.75)
    CALL tab1622("propionitrile",                "107-12-0", 5950.,   5950.,   1870.0,   1870.0,   3.182,    3.8)
    CALL tab1622("butyronitrile",                "109-74-0", 2546.,   2546.,   477.5,    477.5,    DUMMY,    5.263)
    CALL tab1622("benzonitrile",                 "100-47-0", 100.,    100.,    19.39,    19.39,    5.156,    DUMMY)
    CALL tab1622("acrylonitrile",                "107-13-1", 11000.,  11000.,  1423.,    1423.,    7.731,    11.14)
    CALL tab1622("adiponitrile",                 "111-69-3", 0.3066,  0.3066,  73.96,    73.96,    0.0041,   DUMMY)
    CALL tab1622("methylamine",                   "74-89-5", 357300., 357300., DUMMY,    DUMMY,    DUMMY,    1.125)
    CALL tab1622("dimethylamine",                "124-40-3", 206200., 206200., DUMMY,    DUMMY,    DUMMY,    1.8)
    CALL tab1622("trimethylamine",                "75-50-3", 219300., 219300., DUMMY,    DUMMY,    DUMMY,    6.67)
    CALL tab1622("ethylamine",                    "75-04-7", 141650., 141650., DUMMY,    DUMMY,    DUMMY,    1.012)
    CALL tab1622("diethylamine",                 "109-89-7", 31490.,  31490.,  DUMMY,    DUMMY,    DUMMY,    2.60)
    CALL tab1622("triethylamine",                "121-44-8", 7610.,   7610.,   540.,     540.,     14.099,   DUMMY)
    CALL tab1622("N-propylamine",                "107-10-8", 40740.,  40740.,  DUMMY,    DUMMY,    DUMMY,    1.274)
    CALL tab1622("N-butylamine",                 "109-73-9", 13650.,  13650.,  DUMMY,    DUMMY,    DUMMY,    1.526)
    CALL tab1622("di-N-butylamine",              "111-92-2", 304.,    304.,    36.37,    36.37,    8.359,    DUMMY)
    CALL tab1622("tributylamine",                "102-82-9", 5330.,   5330.,   0.216,    0.216,    2.47e4,   DUMMY)
    CALL tab1622("diphenylamine",                "122-39-4", 0.0612,  0.115,   1.773,    3.338,    0.035,    DUMMY)
    CALL tab1622("aniline",                       "62-53-3", 65.19,   65.19,   387.4,    387.35,   0.168,    12.16)
    CALL tab1622("2-chloroaniline",               "95-51-2", 22.66,   22.66,   29.79,    29.79,    0.761,    DUMMY)
    CALL tab1622("3-chloroaniline",              "108-42-9", 9.53,    9.530,   42.64,    42.64,    0.223,    DUMMY)
    CALL tab1622("4-chloroaniline",              "106-47-8", 2.33,    6.873,   23.52,    69.37,    0.099,    DUMMY)
    CALL tab1622("3,4-dichloroaniline",           "95-76-1", 1.3,     3.746,   0.568,    1.637,    2.289,    DUMMY)
    CALL tab1622("o-toluidine",                   "95-53-4", 13.3,    13.30,   139.98,   139.98,   0.095,    DUMMY)
    CALL tab1622("m-toluidine",                  "108-44-1", 36.,     36.0,    140.26,   140.26,   0.257,    DUMMY)
    CALL tab1622("p-toluidine",                  "106-49-0", 45.,     61.48,   68.59,    93.70,    0.656,    DUMMY)
    CALL tab1622("N,N'-dimethylaniline",         "121-69-7", 107.,    107.0,   9.119,    9.119,    11.734,   DUMMY)
    CALL tab1622("2,4-xylidine",                  "95-68-1", 20.5,    20.50,   48.69,    48.69,    0.421,    DUMMY)
    CALL tab1622("2,6-xylidine",                  "87-62-7", 670.,    670.0,   38.79,    38.79,    17.275,   DUMMY)
    CALL tab1622("4-ethylaniline",               "589-16-2", 13.5,    13.50,   42.09,    42.09,    0.321,    DUMMY)
    CALL tab1622("N,N'-diethylaniline",           "91-66-7", 9.7,     9.70,    4.49,     4.49,     2.161,    DUMMY)
    CALL tab1622("benzidine",                     "92-87-5", 1.0E-6,  1.06E-5, 2.17,     23.1,     4.61E-7,  DUMMY)
    CALL tab1622("3,3'-dichlorobenzidine",        "91-94-1", 5.6E-5,  6.41E-4, 0.0122,   0.140,    0.005,    DUMMY)
    CALL tab1622("N,N'-bianiline",               "122-66-7", 0.0035,  DUMMY,   0.0014,   0.0154,   3.45E-4,  DUMMY)
    CALL tab1622("nitrobenzene",                  "98-95-3", 20.,     20.0,    15.43,    15.43,    1.296,    DUMMY)
    CALL tab1622("1,3-dinitrobenzene",            "99-65-0", 0.0081,  0.0348,  3.25,     13.94,    0.002,    DUMMY)
    CALL tab1622("1,4-dinitrobenzene",           "100-25-4", 13.3,    386.63,  2.63,     76.43,    5.059,    DUMMY)
    CALL tab1622("2-nitrotoluene",                "88-72-2", 17.9,    17.90,   4.75,     4.75,     3.768,    DUMMY)
    CALL tab1622("3-nitrotoluene",                "99-08-1", 27.2,    27.20,   3.64,     3.64,     7.473,    DUMMY)
    CALL tab1622("4-nitrotoluene",                "99-99-0", 0.653,   1.2004,  1.86,     3.41,     0.352,    DUMMY)
    CALL tab1622("2,4-dinitrotoluene (DNT)",     "121-14-2", 0.133,   0.3705,  1.48,     4.13,     0.090,    DUMMY)
    CALL tab1622("2,6-dinitrotoluene",           "606-20-2", 0.0767,  0.1952,  DUMMY,    DUMMY,    0.070,    DUMMY)
    CALL tab1622("1-nitronaphthalene",            "86-57-7", 0.702,   0.072,   0.057,    0.132,    3.50,     DUMMY)
    CALL tab1622("acetamide (ethanamide)",        "60-35-5", 2.44,    8.3562,  6907.1,   23650.,   3.53E-4,  DUMMY)
    CALL tab1622("acrylamide",                    "79-06-1", 0.415,   1.5900,  2884.,    11050.,   1.44E-4,  DUMMY)
    CALL tab1622("benzamide",                     "55-21-0", 0.00522, 0.0544,  1692.,    17630.,   4.52E-5,  DUMMY)
    CALL tab1622("urea",                          "57-13-6", 0.0016,  0.0186,  16650.,   1.93e5,   9.61E-8,  DUMMY)
    CALL tab1622("N-nitrosodimethylamine",        "62-75-9", DUMMY,   DUMMY,   DUMMY,    DUMMY,    DUMMY,    3.343)
    CALL tab1622("di-N-propyl nitrosamine",      "621-64-7", 27.,     DUMMY,   76.04,    DUMMY,    0.355,    DUMMY)
    CALL tab1622("diphenyl nitrosamine",          "86-30-6", 13.33,   34.27,   0.116,    0.299,    114.6,    DUMMY)
    CALL tab1622("1h-pyrrole",                   "109-97-7", 1100.,   1100.,   670.7,    670.7,    1.640,    DUMMY)
    CALL tab1622("indole",                       "120-72-9", 2.24,    4.187,   16.00,    29.90,    0.140,    DUMMY)
    CALL tab1622("pyridine",                     "110-86-1", 2775.,   2775.,   DUMMY,    DUMMY,    DUMMY,    0.895)
    CALL tab1622("2-methylpyridine",             "109-06-8", 1496.,   1496.,   DUMMY,    DUMMY,    DUMMY,    1.01)
    CALL tab1622("3-picoline",                   "108-99-6", 1333.,   1333.,   DUMMY,    DUMMY,    DUMMY,    0.788)
    CALL tab1622("4-picoline",                   "108-89-4", 757.,    757.,    DUMMY,    DUMMY,    DUMMY,    0.601)
    CALL tab1622("2,3-dimethylpyridine",         "583-61-9", 426.,    DUMMY,   970.6,    DUMMY,    DUMMY,    0.725)
    CALL tab1622("2,4-dimethylpyridine",         "108-47-4", 456.,    456.,    DUMMY,    DUMMY,    DUMMY,    0.678)
    CALL tab1622("2,6-dimethylpyridine",         "108-48-5", 746.,    746.,    DUMMY,    DUMMY,    DUMMY,    1.06)
    CALL tab1622("2,4,6-trimethylpyridine",      "108-75-8", 5170.,   5170.,   294.6,    294.6,    17.549,   DUMMY)
    CALL tab1622("quinoline",                     "91-22-5", 1.21,    1.21,    47.31,    47.31,    0.026,    DUMMY)
    CALL tab1622("isoquinoline",                 "119-65-3", 670.,    693.,    35.00,    36.20,    19.141,   DUMMY)
    CALL tab1622("benzo[f]quinoline",             "85-02-9", DUMMY,   0.0067,  0.42,     2.02,     0.0096,   DUMMY)
    CALL tab1622("9h-carbazole",                  "86-74-8", 0.0933,  14.976,  0.006,    0.989,    15.146,   DUMMY)
    CALL tab1622("acridine",                     "260-94-6", 0.0065,  0.0451,  0.215,    1.492,    0.030,    DUMMY)
    CALL tab1622("carbon disulfide",              "75-15-0", 48210.,  48210.,  27.584,   27.584,   1747.75,  DUMMY)
    CALL tab1622("dimethyl sulfoxide (DMSO)",     "67-68-5", 80.0,    80.0,    354.8,    354.8,    0.225,    DUMMY)
    CALL tab1622("dimethyl sulfone",              "67-71-0", 5.16,    34.17,   DUMMY,    DUMMY,    200.83,   DUMMY)
    CALL tab1622("dimethyl sulfide",              "75-18-3", 64650.,  64650.,  321.9,    DUMMY,    7.72,     DUMMY)
    CALL tab1622("dimethyl disulfide",           "624-92-0", 4000.,   4000.,   66.88,    66.88,    59.81,    DUMMY)
    CALL tab1622("diethyl sulfide",              "352-93-2", 7782.,   7782.,   DUMMY,    DUMMY,    1.95,     DUMMY)
    CALL tab1622("ethanethiol",                   "75-08-1", 70000.,  DUMMY,   DUMMY,    DUMMY,    289.94,   DUMMY)
    CALL tab1622("1-butanethiol",                "109-79-5", 6070.,   6070.,   6.62,     6.62,     916.94,   DUMMY)
    CALL tab1622("thiophene",                    "110-02-1", 10620.,  8000.,   35.833,   35.833,   223.3,    224.)
    CALL tab1622("benzo[b]thiophene",             "95-15-8", 26.66,   26.7,    0.969,    1.107,    24.1,     DUMMY)
    CALL tab1622("dibenzothiophene",             "132-65-0", 0.267,   DUMMY,   0.006,    DUMMY,    44.3,     DUMMY)

    ! Table 17.2.1+17.2.2                                ps          pl        cs         cl         H=p/c
    CALL CalcH("alachlor",                "15972-60-8", 0.0020,     2.88E-3,   0.890,    1.281,     0.0022)
    CALL CalcH("ametryn",                   "834-12-8", 0.0001,     3.92E-4,   0.814,    3.191,     1.23E-4)
    CALL CalcH("amitrole",                   "61-82-5", 5.50E-7,    1.14E-5,   3330.,    68850.,    1.65E-10)
    CALL CalcH("atrazine",                 "1912-24-9", 4.00E-5,    1.19E-3,   0.139,    4.140,     2.88E-4)
    CALL CalcH("barban",                    "101-27-9", 5.00E-5,    1.60E-4,   0.043,    0.1362,    1.17E-3)
    CALL CalcH("benefin",                  "1861-40-1", 0.0088,     0.0226,    0.003,    0.0077,    29.4)
    CALL CalcH("bifenox",                 "42576-02-3", 3.20E-4,    1.25E-3,   0.0010,   0.0040,    0.313)
    CALL CalcH("bromacil",                  "314-40-9", 4.00E-5,    8.46E-4,   3.121,    66.018,    1.28E-5)
    CALL CalcH("bromoxynil",               "1689-84-5", 6.40E-4,    0.0307,    0.469,    22.54,     1.36E-3)
    CALL CalcH("sec-bumeton",             "26259-45-0", 0.00097,    3.98E-3,   2.756,    11.31,     3.52E-4)
    CALL CalcH("butachlor",               "23184-66-9", 6.0E-4,     6.00E-4,   0.074,    0.074,     8.14E-3)
    CALL CalcH("butralin",                "33629-47-9", 0.0017,     3.86E-3,   0.0034,   7.69E-3,   0.502)
    CALL CalcH("butylate",                 "2008-41-5", 1.73,       1.73,      0.182,    0.182,     8.36)
    CALL CalcH("chloramben",                "133-90-4", 0.93,       51.19,     3.398,    187.05,    0.274)
    CALL CalcH("chlorbromuron",           "13360-45-7", 5.33E-5,    2.69E-4,   0.170,    0.858,     3.13E-4)
    CALL CalcH("chlorfenac",                 "85-34-7", 1.,         19.75,     0.835,    16.50,     1.20)
    CALL CalcH("chlorpropham",              "101-21-3", 0.001,      0.001,     0.417,    0.600,     2.40E-3)
    CALL CalcH("chlorsufuron",            "64902-72-3", 6.13E-4,    0.019,     19.56,    609.4,     3.13E-5)
    CALL CalcH("chlortoluron",            "15545-48-9", 1.70E-5,    2.80E-4,   0.329,    5.418,     5.17E-5)
    CALL CalcH("cyanazine",               "21725-46-2", 2.13E-7,    5.41E-6,   0.710,    18.03,     3.00E-7)
    CALL CalcH("2,4-d",                      "94-75-7", 8.0E-5,     1.11E-3,   1.810,    25.12,     4.42E-5)
    CALL CalcH("2,4-d (a)",                  "94-75-7", 0.001,      0.0139,    4.026,    55.92,     2.48E-4)
    CALL CalcH("dalapon",                    "75-99-0", 1.0E-5,     1.0E-5,    3510.,    3510.,     2.85E-9)
    CALL CalcH("diallate",                 "2303-16-4", 0.02,       0.0224,    0.185,    0.207,     0.108)
    CALL CalcH("dicamba",                  "1918-00-9", 0.0045,     0.0349,    20.36,    158.1,     2.21E-4)
    CALL CalcH("dichlobenil",              "1194-65-6", 0.07,       1.076,     0.105,    1.609,     0.669)
    CALL CalcH("dichlorophen",               "97-23-4", 1.30E-8,    4.24E-7,   0.111,    3.635,     1.17E-7)
    CALL CalcH("dichloroprop",              "120-36-5", 0.0004,     3.25E-3,   1.489,    12.099,    2.69E-4)
    CALL CalcH("dichlorprop-p",           "15165-67-0", 6.20E-5,    5.65E-4,   2.510,    22.855,    2.47E-5)
    CALL CalcH("diclofop-methyl",         "51338-27-3", 4.67E-4,    6.57E-4,   2.34E-3,  3.30E-3,   0.199)
    CALL CalcH("dinitramine",             "29091-05-2", 0.00048,    2.59E-3,   0.0031,   0.017,     0.155)
    CALL CalcH("dinoseb",                    "88-85-7", 0.01,       0.0141,    0.208,    0.293,     0.048)
    CALL CalcH("diphenamid",                "957-51-7", 4.0E-6,     4.95E-5,   1.087,    13.46,     3.68E-6)
    ! diquat is a cation, it is not specified which salt is meant here:
    !CALL CalcH("diquat",                  "2764-72-9", 1.30E-5,    0.0170,    3800.,    4.96e6,    3.42E-9)
    CALL CalcH("diuron",                    "330-54-1", 9.2E-5,     1.9E-3,    0.172,    3.630,     6.83E-4)
    CALL CalcH("eptc",                      "759-94-4", 2.,         2.0,       1.954,    1.954,     1.023)
    CALL CalcH("fenopro (h., g.r.)",         "93-72-1", 1.33E-5,    4.54E-4,   0.519,    17.73,     2.56E-5)
    CALL CalcH("fenuron",                   "101-42-8", 0.0267,     0.305,     23.14,    264.7,     1.15E-3)
    CALL CalcH("fluchloralin",            "33245-39-5", 0.004,      6.03E-3,   0.00281,  0.0042,    1.343)
    CALL CalcH("fluometuron",              "2164-17-2", 6.70E-5,    1.61E-3,   0.388,    9.292,     1.73E-4)
    CALL CalcH("fluridone",               "59756-60-4", 1.3E-5,     2.5E-4,    0.036,    0.7039,    0.357)
    CALL CalcH("fluorodifen",             "15457-05-3", 9.5E-6,     4.47E-6,   0.0061,   0.0293,    3.13)
    CALL CalcH("glyphosate",               "1071-83-6", 4.0E-5,     2.15E-3,   70.96,    3818.4,    5.64E-7)
    CALL CalcH("ioxynil",                  "1689-83-4", 0.001,      0.066,     0.135,    8.904,     7.42E-3)
    CALL CalcH("isopropalin",             "33820-53-0", 0.0019,     0.0019,    3.56E-4,  3.56E-4,   5.34)
    CALL CalcH("isoproturon",             "34123-59-6", 3.30E-6,    6.52E-5,   0.267,    5.266,     1.24E-5)
    CALL CalcH("linuron",                   "330-55-2", 0.023,      6.74E-2,   0.301,    1.449,     7.54E-2)
    CALL CalcH("MCPA",                       "94-74-6", 0.0002,     1.70E-3,   8.001,    68.05,     2.50E-5)
    CALL CalcH("MCPA-thioethyl",          "25319-90-8", 0.021,      0.0309,    0.0094,   0.0138,    2.234)
    CALL CalcH("MCPB",                       "94-81-5", 5.77E-5,    3.18E-4,   0.179,    0.989,     3.22E-4)
    CALL CalcH("mecoprop",                 "7085-19-0", 3.1E-4,     1.53E-3,   2.89,     14.23,     7.43E-5)
    CALL CalcH("mecoprop-p",              "16484-77-8", 4.0E-4,     1.97E-3,   4.007,    19.73,     9.98E-5)
    CALL CalcH("metobromuron",             "3060-89-7", 4.0E-4,     2.02E-3,   1.274,    6.416,     3.14E-4)
    CALL CalcH("metolachlor",             "51218-45-2", 0.0042,     4.20E-3,   1.80,     1.80,      2.33E-3)
    CALL CalcH("metoxuron",               "19937-59-8", 0.0043,     0.0439,    2.965,    30.26,     1.45E-3)
    CALL CalcH("molinate",                 "2212-67-1", 0.75,       0.750,     5.179,    5.179,     0.145)
    CALL CalcH("monolinuron",              "1746-81-2", 0.02,       0.0732,    3.425,    12.54,     5.84E-3)
    CALL CalcH("monuron",                   "150-68-5", 6.66E-5,    2.12E-3,   1.007,    32.08,     6.62E-5)
    CALL CalcH("nitralin",                 "4726-14-1", 0.2,        3.526,     0.0014,   0.0255,    138.2)
    CALL CalcH("oryzalin",                "19044-88-3", 1.30E-6,    1.87E-5,   0.0069,   0.0995,    1.88E-4)
    CALL CalcH("pebulate",                 "1114-71-2", 1.2,        1.20,      0.452,    0.452,     2.653)
    CALL CalcH("picloram",                 "1918-02-1", 6.0E-5,     4.98E-3,   1.781,    147.7,     3.37E-5)
    CALL CalcH("profluralin",             "26399-36-0", 0.009,      0.0116,    2.88E-4,  3.7E-4,    31.35)
    CALL CalcH("prometon",                 "1610-18-0", 0.0003,     1.38E-3,   3.329,    15.31,     9.01E-5)
    CALL CalcH("prometryn",                "7287-19-6", 0.0001,     8.70E-4,   0.199,    1.730,     5.03E-4)
    CALL CalcH("pronamide",               "23950-58-5", 0.011,      0.208,     0.059,    1.105,     0.188)   ! same as propyzamide
    CALL CalcH("propachlor",               "1918-16-7", 0.03,       0.0958,    2.834,    9.055,     0.011)
    CALL CalcH("propanil",                  "709-98-8", 0.005,      0.0230,    0.917,    4.218,     5.45E-3)
    CALL CalcH("propazine",                 "139-40-2", 3.90E-6,    2.89E-4,   0.037,    2.766,     1.04E-3)
    CALL CalcH("propyzamide",             "23950-58-5", 5.80E-5,    1.15E-3,   0.059,    1.157,     9.90E-4) ! same as pronamide
    CALL CalcH("pyrazon (chloridazon)",    "1698-60-8", 7.,         441.8,     1.625,    102.5,     4.309)
    CALL CalcH("simazine",                  "122-34-9", 8.50E-6,    8.27E-4,   0.025,    2.412,     3.43E-4)
    CALL CalcH("simetryne",                "1014-70-6", 9.47E-5,    3.55E-4,   2.110,    7.904,     4.49E-5)
    CALL CalcH("2,4,5-t",                    "93-76-5", 0.005,      0.0922,    0.861,    15.89,     5.81E-3)
    CALL CalcH("terbacil",                 "5902-51-2", 5.0E-5,     1.56E-3,   3.277,    102.1,     1.53E-5)
    CALL CalcH("terbumeton",              "33693-04-8", 2.70E-4,    2.57E-3,   0.577,    5.500,     4.68E-4)
    CALL CalcH("terbuthylazine",           "5915-41-3", 1.50E-4,    4.89E-3,   0.037,    1.206,     4.05E-3)
    CALL CalcH("terbutryn",                 "886-50-0", 0.00013,    8.04E-4,   0.091,    0.564,     1.43E-3)
    CALL CalcH("thiobencarb",             "28249-77-6", 2.2,        2.20,      0.074,    0.0741,    29.69)
    CALL CalcH("triallate",                "2303-17-5", 0.015,      0.0164,    0.013,    0.0144,    1.14)
    CALL CalcH("trifluralin",              "1582-09-8", 0.026,      0.0259,    1.49E-3,  2.57E-3,   10.08)
    CALL CalcH("vernolate",                "1929-77-7", 0.90,       0.90,      0.443,    0.443,     2.034)

    ! Table 18.2.1+18.2.2                               ps         pl          cs         cl         H=p/c
    CALL CalcH("acephate",               "30560-19-1", 2.26E-4,   8.96E-4,    4465.,     17710.,    5.06E-8)
    CALL CalcH("aldicarb",                 "116-06-3", 0.004,     0.0216,     31.54,     170.,      1.27E-4)
    CALL CalcH("aldrin",                   "309-00-2", 0.005,     0.0302,     5.48E-5,   3.31E-4,   91.23)
    CALL CalcH("aminocarb",               "2032-59-9", 0.00227,   0.0109,     4.39,      21.1,      5.17E-4)
    CALL CalcH("azinphos-methyl",           "86-50-0", 3.0E-5,    9.05E-5,    0.0945,    0.285,     3.17E-4)
    CALL CalcH("bendiocarb",             "22781-23-3", 6.6E-4,    7.21E-3,    0.179,     1.96,      3.68E-3)
    CALL CalcH("carbaryl",                  "63-25-2", 2.67E-5,   3.83E-4,    0.596,     8.56,      4.48E-5)
    CALL CalcH("carbofuran",              "1563-66-2", 8.0E-5,    1.41E-3,    1.59,      28.0,      5.04E-5)
    CALL CalcH("cis-chlordane",           "5103-71-9", 4.0E-4,    2.65E-3,    1.37E-4,   9.07E-4,   0.342)
    CALL CalcH("trans-chlordane",         "5103-74-2", 5.2E-4,    3.15E-3,    1.37E-4,   8.30E-4,   0.262)
    CALL CalcH("chlorfenvinphos",          "470-90-6", 1.0E-4,    1.0E-4,     0.345,     0.345,     2.90E-4)
    CALL CalcH("chlorpyrifos",            "2921-88-2", 0.00227,   3.34E-3,    2.08E-3,   3.07E-3,   1.09)
    CALL CalcH("chlorpyrifos-methyl",     "5598-13-0", 0.006,     9.68E-3,    0.0148,    0.0238,    0.407)
    CALL CalcH("crotoxyphos",             "7700-17-6", 0.0019,    1.90E-3,    3.18,      3.18,      5.97E-4)
    ! not used, isomer not specified:
    !CALL CalcH("cypermethrin",          "52315-07-8", 1.87E-7,   6.62E-7,    9.61E-6,   3.40E-5,   0.0195)
    CALL CalcH("alpha-cypermethrin",     "67375-30-8", 2.30E-7,   8.21E-7,    2.40E-5,   8.41E-5,   0.0098)
    CALL CalcH("beta-cypermethrin",      "65731-84-2", 1.80E-7,   5.13E-7,    2.24E-4,   6.4E-4,    8.02E-7)
    CALL CalcH("delta-cypermethrin",     "52315-07-8", 2.50E-7,   2.5E-7,     1.08E-4,   1.08E-4,   2.31E-3)
    CALL CalcH("p,p'-DDD",                  "72-54-8", 1.30E-4,   6.93E-4,    1.56E-4,   1.08E-3,   0.640)
    CALL CalcH("p,p'-DDE",                  "72-55-9", 8.66E-4,   3.72E-3,    1.26E-4,   5.40E-4,   7.95)
    CALL CalcH("o,p'-DDE",                "3424-82-6", 8.0E-4,    3.44E-3,    3.14E-4,   1.35E-3,   2.54)
    CALL CalcH("p,p'-DDT",                  "50-29-3", 2.0E-5,    1.35E-4,    1.55E-5,   1.11E-4,   2.36)
    CALL CalcH("o,p'-DDT",                 "789-02-6", 2.53E-5,   1.72E-4,    7.33E-5,   4.96E-4,   0.347)
    CALL CalcH("deltamethrin",           "52918-63-5", 1.0E-5,    5.52E-5,    3.96E-6,   2.18E-5,   2.53)
    ! demeton is a mixture of demeton-o and demeton-s, not a pure substance:
    !CALL CalcH("demeton",                 "8065-48-3", 0.0347,    0.0347,     0.232,     0.232,     0.15)
    CALL CalcH("demeton-s-methyl",         "919-86-8", 0.04,      0.040,      14.3,      14.3,      2.79E-3)
    CALL CalcH("dialifor",               "10311-84-9", 6.50E-5,   1.73E-4,    4.57E-4,   1.22E-3,   0.14)
    CALL CalcH("diazinon",                 "333-41-5", 0.008,     8.0E-3,     0.197,     0.197,     0.0406)
    CALL CalcH("dicapthon",               "2463-84-5", 5.0E-4,    1.19E-3,    0.021,     0.05,      0.0238)
    CALL CalcH("dichlofenthion",            "97-17-6", 25.,       25.0,       7.93E-4,   7.93E-4,   31646.)
    CALL CalcH("dichlorvos",                "62-73-7", 7.02,      7.02,       36.20,     36.20,     0.194)
    CALL CalcH("dicrotophos",              "141-66-2", 0.0213,    0.0213,     4216.,     4216.,     5.05E-6)
    CALL CalcH("dieldrin",                  "60-57-1", 0.0005,    0.016,      4.46E-4,   0.0142,    1.120)
    CALL CalcH("diflubenzuron",          "35367-38-5", 1.20E-7,   1.31E-5,    2.57E-4,   0.0281,    4.66E-4)
    CALL CalcH("dimethoate",                "60-51-5", 0.01,      0.019,      87.23,     163.2,     1.15E-4)
    CALL CalcH("dinoseb",                   "88-85-7", 10.,       14.07,      0.196,     0.275,     51.11)
    CALL CalcH("disulfoton",               "298-04-4", 0.02,      0.132,      0.0911,    0.603,     0.220)
    CALL CalcH("dnoc",                     "534-52-1", 0.011,     0.044,      1.013,     4.063,     0.0109)
    CALL CalcH("endosulfan",               "115-29-7", 0.0013,    DUMMY,      1.23E-3,   DUMMY,     1.06)
    CALL CalcH("endrin",                    "72-20-8", 2.0E-5,    1.32E-3,    6.04E-4,   0.0399,    0.0331)
    CALL CalcH("ethion",                   "563-12-2", 1.5E-4,    1.50E-4,    4.68E-3,   4.68E-3,   0.0320)
    CALL CalcH("ethoprophos",            "13194-48-4", 0.0507,    0.0507,     3.095,     3.095,     0.0164)
    CALL CalcH("fenitrothion",             "122-14-5", 1.3E-4,    1.30E-4,    0.108,     0.108,     1.20E-3)
    CALL CalcH("fenoxycarb",             "79127-80-3", 1.70E-6,   3.29E-6,    0.0199,    0.039,     8.54E-5)
    CALL CalcH("fenthion",                  "55-38-9", 0.004,     4.0E-3,     0.180,     0.180,     0.0223)
    CALL CalcH("fenvalerate",            "51630-58-1", 4.27E-6,   4.27E-6,    2.02E-4,   2.02E-4,   0.0211)
    CALL CalcH("flucythrinate",          "70124-77-5", 1.20E-6,   1.20E-6,    1.11E-3,   1.11E-3,   1.08E-3)
    CALL CalcH("fonofos",                  "944-22-9", 0.045,     0.045,      0.0650,    0.065,     0.693)
    CALL CalcH("heptachlor",                "76-44-8", 0.053,     0.267,      1.50E-4,   7.56E-4,   353.4)
    CALL CalcH("alpha-HCH'",               "319-84-6", 0.003,     0.10,       3.44E-3,   0.115,     0.872)
    CALL CalcH("beta-HCH'",                "319-85-7", 4.0E-5,    0.0264,     3.44E-4,   0.227,     0.116)
    CALL CalcH("gamma-HCH'",               "319-86-8", 0.002,     0.0268,     0.0275,    0.369,     0.0727)
    CALL CalcH("isofenphos",             "25311-71-1", 4.4E-4,    4.4E-4,     0.0521,    0.0521,    8.45E-3)
    CALL CalcH("isophorone",                "78-59-1", 50.,       50.0,       86.83,     86.83,     0.576)
    CALL CalcH("kepone",                   "143-50-0", 2.93E-5,   0.05,       0.0061,    10.02,     0.005)
    CALL CalcH("leptophos",              "21609-90-5", 3.0E-6,    6.08E-6,    1.21E-5,   2.46E-5,   0.247)
    CALL CalcH("lindane",                   "58-89-9", 0.00374,   0.0274,     0.0251,    0.184,     0.149)
    CALL CalcH("malathion",                "121-75-5", 0.001,     0.001,      0.439,     0.439,     2.28E-3)
    CALL CalcH("methiocarb",              "2032-65-7", 0.016,     0.130,      0.133,     1.082,     0.120)
    CALL CalcH("methomyl",               "16752-77-5", 0.0067,    0.0229,     358.,      1223.,     1.87E-5)
    CALL CalcH("methoxychlor",              "72-43-5", 0.00013,   5.46E-4,    1.30E-4,   5.47E-4,   0.999)
    CALL CalcH("mevinphos",               "7786-34-7", 0.017,     0.0170,     268.,      2677.,     6.35E-6)
    CALL CalcH("mirex",                   "2385-85-5", 0.0001,    3.545,      1.19E-7,   4.22E-3,   839.4)
    CALL CalcH("monocrotophos",           "6923-22-4", 0.00933,   0.0185,     448.,      8870.,     2.08E-6)
    CALL CalcH("oxamyl",                 "23135-22-0", 0.0306,    0.173,      1290.,     7261.,     2.38E-5)
    CALL CalcH("parathion",                 "56-38-2", 6.0E-4,    6.0E-4,     0.0426,    0.0426,    0.0141)
    CALL CalcH("parathion-methyl",         "298-00-0", 0.002,     2.69E-3,    0.095,     0.128,     0.0211)
    CALL CalcH("pentachlorophenol",         "87-86-5", 0.00415,   0.12,       0.053,     1.565,     0.79)
    CALL CalcH("permethrin",             "52645-53-1", 1.70E-6,   2.34E-6,    1.53E-5,   2.11E-5,   0.111)
    CALL CalcH("phenthoate",              "2597-03-7", 3.5E-4,    3.50E-4,    0.0343,    0.034,     0.0102)
    CALL CalcH("phorate",                  "298-02-2", 0.085,     0.085,      0.0845,    0.084,     1.01)
    CALL CalcH("phosmet",                  "732-11-6", 6.0E-5,    1.75E-4,    0.0788,    0.229,     7.62E-4)
    CALL CalcH("phosphamidon",           "13171-21-6", 0.003,     0.003,      8.34E-3,   0.0083,    0.360)
    CALL CalcH("pirimicarb",             "23103-98-2", 0.003,     0.0133,     9.232,     41.03,     3.25E-4)
    CALL CalcH("profenofos",             "41198-08-7", 1.2E-4,    1.20E-4,    0.0749,    0.075,     1.60E-3)
    CALL CalcH("propoxur",                 "114-26-1", 1.70E-5,   7.73E-5,    8.603,     39.12,     1.98E-6)
    ! pyrethrins is a mixture, not a pure substance:
    !CALL CalcH("pyrethrins",              "8003-34-7", 1.33E-6,   DUMMY,      3.05E-6,   DUMMY,     0.437)
    CALL CalcH("ronnel",                   "299-84-3", 0.107,     0.154,      1.87E-3,   2.69E-3,   57.35)
    CALL CalcH("sulfotep",                "3689-24-5", 0.0227,    0.0227,     0.0776,    0.0776,    0.293)
    CALL CalcH("terbacil",                "5902-51-2", 4.13E-5,   1.29E-3,    3.276,     102.06,    1.26E-5)
    CALL CalcH("terbufos",               "13071-79-9", 0.0427,    0.0427,     0.0173,    0.017,     2.463)
    CALL CalcH("thiobencarb",            "28249-77-6", 0.00293,   DUMMY,      0.0745,    DUMMY,     0.0393)
    CALL CalcH("thiodicarb",             "59669-26-0", 0.00431,   0.117,      0.0987,    2.68,      0.0437)
    ! toxaphene is a mixture, not a pure substance:
    !CALL CalcH("toxaphene",              "8001-35-2", 0.0009,    DUMMY,      1.21E-3,   DUMMY,     0.745)
    CALL CalcH("trichlorfon",               "52-68-6", 0.001,     3.83E-3,    598.,      2290.,     1.67E-6)
    CALL CalcH("zinophos",                 "297-97-2", 0.4,       0.40,       4.029,     4.03,      0.0993)

    ! Table 19.2.1+19.2.2                            ps          pl         cs       cl       H=p/c
    CALL CalcH("anilazine",             "101-05-3", 8.20E-7,    1.77E-5,   0.0290,  0.628,   2.82E-5)
    CALL CalcH("benalaxyl",           "71626-11-4", 0.00133,    4.65E-3,   0.1137,  0.398,   0.012)
    CALL CalcH("benodanil",           "15310-01-7", 1.00E-8,    1.28E-7,   0.0062,  0.0793,  1.62E-6)
    CALL CalcH("benomyl",             "17804-35-2", 1.33E-8,    1.82E-7,   0.0069,  0.0945,  1.93E-6)
    CALL CalcH("bitertanol",          "55179-31-2", 1.00E-6,    8.31E-6,   0.0118,  0.0984,  8.45E-5)
    CALL CalcH("diastereoisomer a",   "70585-36-3", 2.20E-9,    2.80E-8,   0.0069,  0.0874,  3.20E-7)
    CALL CalcH("diastereoisomer b",   "70585-38-5", 2.50E-9,    3.86E-8,   0.0038,  0.0585,  6.60E-7)
    CALL CalcH("bupirimate",          "41483-43-6", 0.00067,    1.21E-3,   0.0695,  0.126,   9.64E-3)
    CALL CalcH("captan",                "133-06-2", 1.10E-5,    4.23E-4,   0.0170,  0.653,   6.48E-4)
    CALL CalcH("carbendazim",         "10605-21-7", 6.50E-8,    3.82E-5,   0.0418,  24.60,   1.55E-6)
    CALL CalcH("carboxin",             "5234-68-4", 1.30E-5,    5.98E-5,   0.829,   3.811,   1.57E-5)
    CALL CalcH("chloroneb",            "2675-77-6", 0.40,       4.788,     0.0386,  0.462,   173.8)
    CALL CalcH("chloropicrin",           "76-06-2", 2400.,      2400.,     13.81,   13.81,   197.3)
    CALL CalcH("chlorothalonil",       "1897-45-6", 0.133,      22.86,     0.0023,  0.388,   58.94)
    CALL CalcH("dazomet (fum.)",        "533-74-4", 4.0E-4,     2.47E-3,   18.48,   114.3,   2.16E-5)
    CALL CalcH("dichlofluanid",        "1085-98-9", 2.10E-5,    1.32E-4,   0.0039,  0.0245,  5.38E-3)
    CALL CalcH("dithianon",            "3347-22-6", 6.60E-5,    6.28E-4,   0.0017,  0.1605,  0.0391)
    CALL CalcH("edifenphos",          "17109-49-8", 0.013,      0.013,     0.180,   0.180,   0.072)
    CALL CalcH("ethirimol",           "23947-60-6", 2.67E-4,    5.78E-3,   0.956,   20.68,   2.79E-4)
    CALL CalcH("etridiazole",          "2593-15-9", 0.013,      0.013,     0.202,   0.202,   0.0644)
    CALL CalcH("fenarimol",           "60168-88-9", 2.93E-5,    2.44E-4,   0.0423,  0.351,   6.93E-4)
    CALL CalcH("fenfuram",            "24691-80-3", 2.0E-5,     1.39E-4,   0.497,   3.444,   4.02E-5)
    CALL CalcH("fenpropimorph",       "67564-91-4", 0.0023,     2.30E-3,   0.0142,  0.014,   0.162)
    CALL CalcH("folpet",                "133-07-3", 0.0013,     0.0414,    0.0034,  0.107,   0.386)
    CALL CalcH("imazalil",            "35554-44-0", 9.30E-6,    1.75E-5,   4.71,    8.853,   1.97E-6)
    CALL CalcH("metalaxyl",           "57837-19-1", 7.47E-4,    2.18E-3,   30.08,   87.71,   2.48E-5)
    CALL CalcH("oxycarboxin",          "5259-88-1", 0.00133,    0.0139,    3.741,   39.06,   3.56E-4)
    CALL CalcH("penconazole",         "66246-88-6", 0.00021,    4.66E-4,   0.257,   0.570,   8.18E-4)
    CALL CalcH("procymidone",         "32809-16-8", 0.0187,     0.4534,    0.0158,  0.384,   1.181)
    CALL CalcH("propiconazole",       "60207-90-1", 5.60E-5,    5.60E-5,   0.321,   0.321,   1.74E-4)
    CALL CalcH("quintozene",             "82-68-8", 0.0066,     0.104,     0.0015,  0.023,   4.430)
    CALL CalcH("thiabendazole",         "148-79-8", 5.33E-7,    3.13E-4,   0.249,   146.1,   2.14E-6)
    CALL CalcH("thiophanate-methyl",  "23564-05-8", 1.30E-5,    3.70E-4,   0.0102,  0.291,   1.27E-3)
    CALL CalcH("thiram",                "137-26-8", 0.00133,    0.0209,    0.125,   1.963,   0.0107)
    CALL CalcH("tolclofos-methyl",    "57018-04-9", 0.0573,     0.196,     0.001,   0.0034,  57.51)
    CALL CalcH("tolylfluanid",          "731-27-1", 1.6E-5,     8.06E-5,   0.0026,  0.0131,  6.17E-3)
    CALL CalcH("triadimefon",         "43121-43-3", 2.0E-6,     7.37E-6,   0.243,   0.897,   8.22E-6)
    CALL CalcH("triadimenol",         "55219-65-3", 4.13E-8,    4.03E-7,   0.159,   1.549,   2.60E-7)
    CALL CalcH("tricyclazole",        "41814-78-2", 2.67E-5,    1.09E-3,   8.46,    346.2,   3.16E-6)
    CALL CalcH("triflumizole",        "99387-89-0", 1.47E-6,    3.53E-6,   36.16,   86.90,   4.07E-8)
    CALL CalcH("triforine",           "26644-46-2", 2.67E-5,    5.16E-4,   0.069,   1.332,   3.87E-4)
    CALL CalcH("vinclozolin",         "50471-44-8", 1.33E-5,    8.81E-5,   3.495,   23.14,   3.81E-6)
    CALL CalcH("warfarin",               "81-81-2", 1.55E-4,    3.43E-3,   0.0551,  1.221,   2.81E-3)
    CALL CalcH("zineb",               "12122-67-7", 1.33E-5,    DUMMY,     0.0363,  DUMMY,   3.67E-4)
    CALL CalcH("ziram",                 "137-30-4", 1.0E-6,     1.53E-4,   0.213,   32.61,   4.70E-6)

    ! create notes for some special species:
    ! (make sure they don't produce any output in SUBROUTINE CalcH)
    chem = "dinoseb" ; casrn = "88-85-7" ; type = "W"
    CALL MakeNote("2614_88-85-7", &
      TRIM(citet())//" list two values for dinoseb which differ by "// &
      "a factor of 1000. It is unclear which number is correct (if any) "// &
      "and the data are not reproduced here.")
    CALL Output(DUMMY)

    chem = "thiobencarb" ; casrn = "28249-77-6" ; type = "W"
    CALL MakeNote("2614_28249-77-6", &
      TRIM(citet())//" list two values for thiobencarb which differ "// &
      "by a large factor. It is unclear which number is correct (if "// &
      "any) and the data are not reproduced here.")
    CALL Output(DUMMY)

    chem = "pronamide" ; casrn = "23950-58-5" ; type = "W"
    CALL MakeNote("2614_23950-58-5", &
      "Although pronamide and propyzamide are the same species, "//TRIM(citet())// &
      " list two different values for them. It is unclear which number is "// &
      "correct (if any) and the data are not reproduced here.")
    CALL Output(DUMMY)

  CONTAINS

    SUBROUTINE tab522 (chem_, casrn_, ps, pl, cs, cl, H, exptl)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: ps, pl, cs, cl, H, exptl
      CALL CalcH(chem_, casrn_, ps, pl, cs, cl, H)
      ! Tab. 5.2.2 contains no info where exptl data are from
      CALL CalcHexptl(chem_, casrn_, exptl, "?")
    END SUBROUTINE tab522

    SUBROUTINE tab1122 (chem_, casrn_, cl, pl, H, exptla, exptlb, exptlc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: cl, pl, H, exptla, exptlb, exptlc
      CALL CalcH(chem_, casrn_, DUMMY, pl, DUMMY, cl, H)
      CALL CalcHexptl(chem_, casrn_, exptla, "712")
      CALL CalcHexptl(chem_, casrn_, exptlb, "754")
      CALL CalcHexptl(chem_, casrn_, exptlc, "483")
    END SUBROUTINE tab1122

    SUBROUTINE tab1222 (chem_, casrn_, cl, pl, H, exptla, exptlb, exptlc, exptld)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: cl, pl, H, exptla, exptlb, exptlc, exptld
      CALL CalcH(chem_, casrn_, DUMMY, pl, DUMMY, cl, H)
      CALL CalcHexptl(chem_, casrn_, exptla, "whichref") ! could be 712 or 754
      CALL CalcHexptl(chem_, casrn_, exptlb, "630")
      CALL CalcHexptl(chem_, casrn_, exptlc, "483")
      CALL CalcHexptl(chem_, casrn_, exptld, "484")
    END SUBROUTINE tab1222

    SUBROUTINE tab1322 (chem_, casrn_, cs, cl, ps, pl, H, exptla, calcd)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: cs, cl, ps, pl, H, exptla, calcd
      CALL CalcH(chem_, casrn_, ps, pl, cs, cl, H)
      CALL CalcHexptl(chem_, casrn_, exptla, "2231")
      CALL CalcHexptl(chem_, casrn_, calcd, "642")
    END SUBROUTINE tab1322

    SUBROUTINE tab1522 (chem_, casrn_, ps, pl, cs, cl, H, exptla, exptlb)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: ps, pl, cs, cl, H, exptla, exptlb
      CALL CalcH(chem_, casrn_, ps, pl, cs, cl, H)
      CALL CalcHexptl(chem_, casrn_, exptla, "754")
      CALL CalcHexptl(chem_, casrn_, exptlb, "628")
    END SUBROUTINE tab1522

    SUBROUTINE tab1622 (chem_, casrn_, ps, pl, cs, cl, H, exptl)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: ps, pl, cs, cl, H, exptl
      CALL CalcH(chem_, casrn_, ps, pl, cs, cl, H)
      ! Tab. 16.2.2 contains no info where exptl data are from
      CALL CalcHexptl(chem_, casrn_, exptl, "?")
    END SUBROUTINE tab1622

    SUBROUTINE CalcHexptl (chem_, casrn_, H_exptl, xref)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H_exptl
      CHARACTER(LEN=*), INTENT(IN) :: xref ! ref for experimental value

      IF (H_exptl<0.) RETURN
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      SELECT CASE(xref)
      CASE ("?")
        type = "?"
        CALL Output(KHpcSI_TIMES_HcpSI/H_exptl)
      CASE ("whichref")
        type = "?"
        CALL MakeNote("whichref")
        CALL Output(KHpcSI_TIMES_HcpSI/H_exptl)
      CASE DEFAULT
        ! don't do anything here, use data from original papers instead
        !CALL SettypeX(xref)
        !CALL Output(KHpcSI_TIMES_HcpSI/H_exptl)
      END SELECT
    END SUBROUTINE CalcHexptl

    SUBROUTINE CalcH (chem_, casrn_, ps, pl, cs, cl, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: ps, pl, cs, cl, H
      REAL :: Hs, Hl

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "V"

      ! these species already have special notes:
      IF (casrn_=="88-85-7")    RETURN
      IF (casrn_=="28249-77-6") RETURN
      IF (casrn_=="23950-58-5") RETURN

      ! calculate H from ps/cs:
      IF ((ps>0.).AND.(cs>0.)) THEN
        IF (ps>p0) THEN
          Hs = p0 / cs
        ELSE
          Hs = ps / cs
        ENDIF
      ELSE
        Hs = DUMMY ! dummy value
      ENDIF
      ! calculate H from pl/cl:
      IF ((pl>0.).AND.(cl>0.)) THEN
        IF (pl>p0) THEN
          Hl = p0 / cl
        ELSE
          Hl = pl / cl
        ENDIF
      ELSE
        Hl = DUMMY ! dummy value
      ENDIF
      ! check if deviation to at least one p/c calculation is < 10%:
      IF (H>0.) THEN
        IF (((ABS((Hs-H)/H)<0.1).OR.(Hs<0.)) .AND. &
          ((ABS((Hl-H)/H)<0.1).OR.(Hl<0.))) THEN
          ! activate next lines to show info:
          !WRITE(*,'(A,1PG10.4,A,1PG10.4,A,1PG10.4,A)') 'OK: ps/cs=', &
          !  Hs, ', pl/cl=', Hl, ', H=', H, ' '//TRIM(chem)
          CALL Output(KHpcSI_TIMES_HcpSI/H)
        ELSE
          ! activate next lines to show info:
          !WRITE(*,'(A,1PG10.4,A,1PG10.4,A,1PG10.4,A)') '--: ps/cs=', &
          !  Hs, ', pl/cl=', Hl, ', H=', H, ' '//TRIM(chem)
          CALL MakeNote(TRIM(ref)//"_pc_problem", &
            TRIM(citet())//" list a vapor pressure $p$, a solubility "// &
            "$c$, and a Henry's law constant calculated as $p/c$. "// &
            "However, the data are internally inconsistent and deviate by "// &
            "more than 10 \%.")
          CALL Output(DUMMY)
        ENDIF
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2611_2614

  !---------------------------------------------------------------------------

  SUBROUTINE ref2625 ! KHpx [bar]
    IMPLICIT NONE

    ref = "2625"
    type = "M"
    chem = "chlorine (molecular)" ; casrn = "7782-50-5" ! Cl2
    mindHR = 3072.7
    Hominus = cH2O / ( EXP(12.98-mindHR/T0) * bar)
    CALL MakeNote(TRIM(ref), &
      "Extrapolated from data measured between 70~\unit{\degree C} "// &
      "and 110~\unit{\degree C}.")
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2625

  !---------------------------------------------------------------------------

  SUBROUTINE ref2626 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2626"
    type = "L"

    ! data from Table 5-4:
    CALL CalcH("oxygen",                           "7782-44-7",  1.27E-3, -161.6,  8160.,  22.39  ) ! O2
    CALL CalcH("ozone",                            "10028-15-6", 1.03E-2, -14.08,  2830.          ) ! O3
    CALL CalcH("hydrogen atom",                    "12385-13-6", 2.6E-4                           ) ! H
    CALL CalcH("hydroxyl radical",                 "3352-57-6",  39.                              ) ! OH
    CALL CalcH("hydroperoxy radical",              "3170-83-0",  690.                             ) ! HO2
    CALL CalcH("hydrogen peroxide",                "7722-84-1",  8.44E4,  -14.16,  7600.          ) ! H2O2
    CALL CalcH("nitrogen",                         "7727-37-9",  6.52E-4, -177.1,  8640.,  24.71  ) ! N2
    CALL CalcH("ammonia",                          "7664-41-7",  60.2,    -9.84,   4160.          ) ! NH3
    CALL CalcH("nitrogen trifluoride",             "7783-54-2",  7.96E-4, -242.8,  12100., 34.236 ) ! NF3
    CALL CalcH("dinitrogen tetrafluoride",         "10036-47-2", 8.53E-4, -332.7,  16610., 47.370 ) ! N2F4
    CALL CalcH("chloramine",                       "10599-90-3", 87.,     -15.51,  5960.          ) ! NH2Cl
    CALL CalcH("dichloroamine",                    "3400-09-7",  29.,     -10.68,  4180.          ) ! NHCl2
    CALL CalcH("nitrogen trichloride",             "10025-85-1", 0.10,    -16.17,  4130.          ) ! NCl3
    CALL CalcH("nitrogen monoxide",                "10102-43-9", 1.92E-3, -157.1,  7950.,  21.298 ) ! NO
    CALL CalcH("nitrogen dioxide",                 "10102-44-0", 1.2E-2,  -12.32,  2360.          ) ! NO2
    CALL CalcH("nitrogen trioxide",                "12033-49-7", 3.8E-2                           ) ! NO3
    CALL CalcH("dinitrogen monoxide",              "10024-97-2", 2.42E-2, -148.1,  8610.,  20.266 ) ! N2O
    CALL CalcH("hydrazoic acid",                   "7782-79-8",  12.0,    -10.19,  3780.          ) ! HN3
    CALL CalcH("carbon monoxide",                  "630-08-0",   9.81E-4, -178.0,  8750.,  24.875 ) ! CO
    CALL CalcH("carbon dioxide",                   "124-38-9",   3.38E-2, -145.1,  8350.,  19.960 ) ! CO2
    CALL CalcH("methane",                          "74-82-8",    1.41E-3, -194.7,  9750.,  27.274 ) ! CH4
    CALL CalcH("ethane",                           "74-84-0",    1.88E-3, -240.2,  12420., 33.744 ) ! C2H6
    CALL CalcH("propane",                          "74-98-6",    1.51E-3, -281.1,  14510., 39.652 ) ! C3H8
    CALL CalcH("butane",                           "106-97-8",   1.24E-3, -269.9,  14330., 37.734 ) ! n-C4H10
    CALL CalcH("2-methylpropane",                  "75-28-5",    9.18E-4, -360.6,  18020., 51.444 ) ! CH3CH(CH3)CH3
    CALL CalcH("ethene",                           "74-85-1",    5.96E-3, -154.6,  8540.,  21.202 ) ! C2H4
    CALL CalcH("ethyne",                           "74-86-2",    4.14E-2, -271.8,  13430., 39.237 ) ! C2H2
    CALL CalcH("fluoromethane",                    "593-53-3",   6.15E-2, -9.478,  1990.          ) ! CH3F
    CALL CalcH("chloromethane",                    "74-87-3",    0.127,   -13.13,  3270.          ) ! CH3Cl
    CALL CalcH("bromomethane",                     "74-83-9",    0.173,   -12.16,  3100.          ) ! CH3Br
    CALL CalcH("iodomethane",                      "74-88-4",    0.200,   -13.52,  3550.          ) ! CH3I
    CALL CalcH("dichloromethane",                  "75-09-2",    0.366,   -14.68,  4080.          ) ! CH2Cl2
    CALL CalcH("trichloromethane",                 "67-66-3",    0.255,   -16.48,  4510.          ) ! CHCl3
    CALL CalcH("bromodichloromethane",             "75-27-4",    0.409,   -18.32,  5200.          ) ! CHCl2Br
    CALL CalcH("dibromochloromethane",             "124-48-1",   0.868,   -18.67,  5530.          ) ! CHClBr2
    CALL CalcH("tribromomethane",                  "75-25-2",    1.76,    -16.79,  5170.          ) ! CHBr3
    CALL CalcH("dichlorodifluoromethane",          "75-71-8",    3.09E-3, -17.41,  3470.          ) ! CF2Cl2
    CALL CalcH("trichlorofluoromethane",           "75-69-4",    1.07E-2, -15.74,  3340.          ) ! CFCl3
    CALL CalcH("tetrachloromethane",               "56-23-5",    3.47E-2, -17.38,  4180.          ) ! CCl4
    CALL CalcH("trifluoromethane",                 "75-46-7",    0.0134,  -15.25,  3260.          ) ! CHF3
    CALL CalcH("chlorodifluoromethane",            "75-45-6",    0.0346,  -300.2,  16150., 42.60  ) ! CHClF2
    CALL CalcH("chlorotrifluoromethane",           "75-72-9",    0.0010,  -12.53,  1660.          ) ! CF3Cl
    CALL CalcH("tetrafluoromethane",               "75-73-0",    2.11E-4, -313.47, 15140., 44.62  ) ! CF4
    CALL CalcH("methanol",                         "67-56-1",    203.,    -97.53,  9240.,  12.16  ) ! CH3OH
    CALL CalcH("ethanol",                          "64-17-5",    190.,    -162.9,  12900., 21.91  ) ! CH3CH2OH
    CALL CalcH("1-propanol",                       "71-23-8",    142.,    -217.7,  15800., 29.76  ) ! n-C3H7OH
    CALL CalcH("2-propanol",                       "67-63-0",    130.,    -20.15,  7450.          ) ! iso-C3H7OH
    CALL CalcH("1-butanol",                        "71-36-3",    123.,    -265.7,  18400., 36.64  ) ! n-C4H9OH
    CALL CalcH("2-methyl-1-propanol",              "78-83-1",    102.                             ) ! iso-C4H9OH
    CALL CalcH("2-butanol",                        "78-92-2",    110.,    -19.65,  7260.          ) ! sec-C4H9OH
    CALL CalcH("2-methyl-2-propanol",              "75-65-0",    70.,     -23.63,  8310.          ) ! tert-C4H9OH
    CALL CalcH("methyl hydroperoxide",             "3031-73-0",  300.,    -11.99,  5280.          ) ! CH3OOH
    CALL CalcH("hydroxymethyl hydroperoxide",      "15932-89-5", 1.7E6,   -18.79,  9870.          ) ! HOCH2OOH
    CALL CalcH("ethanoic peroxyacid",              "79-21-0",    837.,    -11.07,  5310.          ) ! CH3C(O)OOH
    CALL CalcH("ethyl hydroperoxide",              "3031-74-1",  336.,    -14.28,  5995.          ) ! C2H5OOH
    CALL CalcH("methanal",                         "50-00-0",    3.23E3,  -15.73,  7100.          ) ! HCHO
    CALL CalcH("ethanal",                          "75-07-0",    12.9,    -17.19,  5890.          ) ! CH3CHO
    CALL CalcH("propanal",                         "123-38-6",   10.0,    -12.20,  4330.          ) ! C2H5CHO
    CALL CalcH("butanal",                          "123-72-8",   9.6,     -18.59,  6220.          ) ! C3H7CHO
    CALL CalcH("ethanedial",                       "107-22-2",   4.19E5,  -12.15,  7480.          ) ! CHOCHO
    CALL CalcH("propanone",                        "67-64-1",    27.8,    -15.23,  5530.          ) ! CH3COCH3
    CALL CalcH("butanone",                         "78-93-3",    18.,     -16.40,  5740.          ) ! C2H5COCH3
    CALL CalcH("2,3-butanedione",                  "431-03-8",   74.,     -14.66,  5650.          ) ! CH3C(O)C(O)CH3
    CALL CalcH("methanoic acid",                   "64-18-6",    8.9E3,   -11.40,  6100.          ) ! HC(O)OH
    CALL CalcH("ethanoic acid",                    "64-19-7",    4.1E3,   -12.50,  6200.          ) ! CH3C(O)OH
    CALL CalcH("hydroxyethanoic acid",             "79-14-1",    2.83E4,  -3.26,   4030.          ) ! HC(OH)C(O)OH formula should be H2C(OH)C(O)OH
    CALL CalcH("oxoethanoic acid",                 "298-12-4",   1.09E4,  -6.84,   4810.          ) ! HC(O)C(O)OH
    CALL CalcH("2-oxopropanoic acid",              "127-17-3",   3.11E5,  -4.417,  5090.          ) ! CH3C(O)C(O)OH
    CALL CalcH("methyl methanoate",                "107-31-3",   4.2,     -11.88,  3970.          ) ! CH3OC(O)H
    CALL CalcH("ethyl methanoate",                 "109-94-4",   3.4,     -14.11,  4570.          ) ! C2H5OC(O)H
    CALL CalcH("propyl methanoate",                "110-74-7",   2.6,     -15.99,  5050.          ) ! n-C3H7OC(O)H
    CALL CalcH("ethyl ethanoate",                  "141-78-6",   6.0,     -17.97,  5890.          ) ! C2H5OC(O)CH3
    CALL CalcH("ethane nitrile",                   "75-05-8",    52.8,    -9.35,   3970.          ) ! CH3CN
    CALL CalcH("nitromethane",                     "75-52-5",    34.6,    -9.92,   4010.          ) ! CH3NO2
    CALL CalcH("nitroethane",                      "79-24-3",    21.7,    -11.80,  4430.          ) ! C2H5NO2
    CALL CalcH("1-nitropropane",                   "108-03-2",   13.1,    -13.22,  4710.          ) ! C3H7NO2
    CALL CalcH("2-nitropropane",                   "79-46-9",    8.42,    -13.02,  4520.          ) ! CH3CH(NO2)CH3
    CALL CalcH("methyl nitrate",                   "598-58-3",   2.0,     -15.20,  4740.          ) ! CH3ONO2
    CALL CalcH("ethyl nitrate",                    "625-58-1",   1.59,    -17.50,  5360.          ) ! C2H5ONO2
    CALL CalcH("1-propyl nitrate",                 "627-13-4",   1.10,    -18.31,  5490.          ) ! 1-C3H7ONO2
    CALL CalcH("2-propyl nitrate",                 "1712-64-7",  0.791,   -18.20,  5360.          ) ! 2-C3H7ONO2
    CALL CalcH("1-butyl nitrate",                  "928-45-0",   1.01,    -19.40,  5790.          ) ! 1-C4H9ONO2
    CALL CalcH("2-butyl nitrate",                  "924-52-7",   0.648,   -18.59,  5410.          ) ! 2-C4H9ONO2
    CALL CalcH("peroxyacetyl nitrate",             "2278-22-0",  2.8,     -18.15,  5730.          ) ! CH3C(O)O2NO2
    CALL CalcH("1,2-ethanediol dinitrate",         "628-96-6",   640.                             ) ! O2NOC2H4ONO2
    CALL CalcH("2-nitrooxyethanol",                "16051-48-2", 3.99E4                           ) ! HOC2H4ONO2
    CALL CalcH("2-nitrooxy-1-propanol",            "20266-74-4", 7.3E3                            ) ! HOCH2CH(ONO2)CH3
    CALL CalcH("1-nitrooxy-2-propanol",            "20266-65-3", 6.7E3                            ) ! CH3CH(OH)CH2ONO2
    CALL CalcH("1,2-propanediol dinitrate",        "6423-43-4",  175.                             ) ! CH3CH(ONO2)CH2ONO2
    CALL CalcH("1-nitrooxy-2-propanone",           "6745-71-7",  1.01E3                           ) ! CH3C(O)CH2ONO2
    CALL CalcH("trichloronitromethane",            "76-06-2",    0.48                             ) ! CCl3NO2
    CALL CalcH("2,2,2-trifluoroethanol",           "75-89-8",    47.7,    -17.00,  6220.          ) ! CF3CH2OH
    CALL CalcH("2,2,3,3-tetrafluoro-1-propanol",   "76-37-9",    141.,    -18.53,  7000.          ) ! CHF2CF2CH2OH
    CALL CalcH("2,2,3,3,3-pentafluoro-1-propanol", "422-05-9",   14.6,    -11.71,  4290.          ) ! CF3CF2CH2OH
    CALL CalcH("1,1,1-trifluoro-2-propanone",      "421-50-1",   138.,    -24.92,  8900.          ) ! CF3C(O)CH3
    CALL CalcH("fluoroethanoic acid",              "144-49-0",   8.1E4                            ) ! CFH2COOH
    CALL CalcH("difluoroethanoic acid",            "381-73-7",   3.0E4,   -12.71,  6870.          ) ! CF2HCOOH
    CALL CalcH("trifluoroethanoic acid",           "76-05-1",    9.0E3,   -22.20,  9330.          ) ! CF3COOH
    CALL CalcH("methyl trifluoroacetate",          "431-47-0",   0.11,    -20.16,  5250.          ) ! CF3COOCH3
    CALL CalcH("2,2,2-trifluoroethyl methanoate",  "32042-38-9", 0.55,    -16.33,  4690.          ) ! CF3CH2OCOH
    CALL CalcH("2,2,2-trifluoroethyl ethanoate",   "406-95-1",   0.56,    -18.15,  5240.          ) ! CH3COOCH2CF3
    CALL CalcH("ethyl trifluoroacetate",           "383-63-1",   0.09,    -18.95,  4930.          ) ! C2H5OCOCF3
    CALL CalcH("chloro-2-propanone",               "78-95-5",    59.,     -14.08,  5410.          ) ! CH2ClCOCH3
    CALL CalcH("chloroethanoic acid",              "79-11-8",    1.1E5,   -21.08,  9740.          ) ! CClH2COOH
    CALL CalcH("dichloroethanoic acid",            "79-43-6",    1.2E5,   -15.18,  8010.          ) ! CCl2HCOOH
    CALL CalcH("trichloroethanoic acid",           "76-03-9",    7.4E4,   -17.83,  8660.          ) ! CCl3COOH
    CALL CalcH("chlorodifluoroethanoic acid",      "76-04-0",    2.5E4,   -24.26,  10250.         ) ! CClF2COOH
    CALL CalcH("bromoethanoic acid",               "79-08-3",    1.5E5,   -19.12,  9260.          ) ! CBrH2COOH
    CALL CalcH("dibromoethanoic acid",             "631-64-1",   2.3E5,   -17.64,  8940.          ) ! CBr2HCOOH
    CALL CalcH("tribromoethanoic acid",            "75-96-7",    3.0E5,   -17.6,   9000.          ) ! CBr3COOH
    CALL CalcH("chlorine atom",                    "22537-15-1", 2.3                              ) ! Cl
    CALL CalcH("chlorine (molecular)",             "7782-50-5",  9.29E-2, -134.4,  7590.,  18.702 ) ! Cl2
    CALL CalcH("monochlorine monoxide",            "14989-30-1", 0.71                             ) ! ClO
    CALL CalcH("dichlorine monoxide",              "7791-21-1",  17.,     -3.23,   1810.          ) ! Cl2O
    CALL CalcH("chlorine dioxide",                 "10049-04-4", 1.01,    -11.65,  3470.          ) ! ClO2
    CALL CalcH("hypochlorous acid",                "7790-92-3",  660.,    -13.2,   5880.          ) ! HOCl
    CALL CalcH("bromine (molecular)",              "7726-95-6",  0.725,   -15.05,  4390.          ) ! Br2
    CALL CalcH("bromine chloride",                 "13863-41-7", 0.98,    -18.9,   5630.          ) ! BrCl
    CALL CalcH("sulfur dioxide",                   "7446-09-5",  1.36,    -39.72,  4250.,  4.525  ) ! SO2
    CALL CalcH("hydrogen sulfide",                 "7783-06-4",  0.102,   -145.2,  8120.,  20.296 ) ! H2S
    CALL CalcH("carbon disulfide",                 "75-15-0",    0.062,   -17.05,  4250.          ) ! CS2
    CALL CalcH("carbon oxide sulfide",             "463-58-1",   2.02E-2, -15.68,  3510.          ) ! COS
    CALL CalcH("methanethiol",                     "74-93-1",    0.39,    -12.42,  3420.          ) ! CH3SH
    CALL CalcH("ethanethiol",                      "75-08-1",    0.28,    -13.82,  3740.          ) ! C2H5SH
    CALL CalcH("dimethyl sulfide",                 "75-18-3",    0.54,    -12.19,  3460.          ) ! CH3SCH3
    CALL CalcH("dimethylsulfoxide",                "67-68-5",    9.9E4                            ) ! CH3SOCH3
    CALL CalcH("methyl isothiocyanate",            "556-61-6",   17.                              ) ! CH3NCS

    chem = "CH3C(O)O2" ; casrn = "36709-10-1"
    CALL Output(0.1*Hcp_TO_HcpSI, limit="<")

    chem = "HOBr" ; casrn = "13517-11-8"
    CALL Output(1.3E2*Hcp_TO_HcpSI, limit=">")

    ! data for NOCl not used, it is only a citation of Scheer et al.:
    ! chem = "nitrosyl chloride" ; casrn = "2696-92-6"
    ! CALL Output(0.05*Hcp_TO_HcpSI, limit=">")

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
      IF (casrn_=="107-22-2") CALL MakeNote("RCHOdiol")
      IF (casrn_=="7782-79-8") CALL MakeNote(TRIM(ref)//"typo", &
        "There is a typo in "//TRIM(citet())// &
        ": The value for $A$ should be $-10.19$, not $10.19$.")
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

  END SUBROUTINE ref2626

  !---------------------------------------------------------------------------

  ! ref2627 only solubility of CS2 [g/L] at unknown partial pressure

  !---------------------------------------------------------------------------

  ! ref2628 only solubility of CS2 [g/L] at unknown partial pressure

  !---------------------------------------------------------------------------

  SUBROUTINE ref2629 ! KHpx [MPa]
    IMPLICIT NONE

    REAL, PARAMETER :: A = 90.44
    REAL, PARAMETER :: B = 0.010845
    REAL, PARAMETER :: C = -3.792E3
    REAL, PARAMETER :: D = -29.5008
    ref = "2629"
    type = "W"
    chem = "hydrogen sulfide" ; casrn = "7783-06-4" ! H2S
    ! T-dep with 4 parameters:
    Hominus    = 1E-6 * cH2O / (10.**(A + B*T0 + C/T0 + D*LOG10(T0)))
    ! mindHR = analytical solution of d ln(Hominus) / d (1/T) at 298.15 K:
    mindHR = - (-B*T0**2 + C - D*T0/LOG(10.)) * LOG(10.)
    CALL MakeNote(TRIM(ref), "The parameter fit for the temperature " // &
      "dependence is incorrect. A corrected version was later presented " // &
      "by \citet{2630}.")
    !CALL Output(Hominus, mindHR)
    CALL Output(DUMMY)

  END SUBROUTINE ref2629

  !---------------------------------------------------------------------------

  SUBROUTINE ref2630 ! KHpx [MPa]
    IMPLICIT NONE

    REAL, PARAMETER :: A = 84.44
    REAL, PARAMETER :: B = 0.0101845
    REAL, PARAMETER :: C = -3.792E3
    REAL, PARAMETER :: D = -29.5008
    ref = "2630"
    type = "R"
    chem = "hydrogen sulfide" ; casrn = "7783-06-4" ! H2S
    ! T-dep with 4 parameters:
    Hominus    = 1E-6 * cH2O / (10.**(A + B*T0 + C/T0 + D*LOG10(T0)))
    ! mindHR = analytical solution of d ln(Hominus) / d (1/T) at 298.15 K:
    mindHR = - (-B*T0**2 + C - D*T0/LOG(10.)) * LOG(10.)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2630

  !---------------------------------------------------------------------------

  SUBROUTINE ref2631 ! KHcc [1]
    IMPLICIT NONE

    ref = "2631"
    chem = "carbon disulfide" ; casrn = "75-15-0" ! CS2
    type = "M"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0.5, 8.0, 16.0, 24.0, 32.0 /) + CtoK
    Harray = (/ 0.22, 0.33, 0.45, 0.59, 0.88 /)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2631

  !---------------------------------------------------------------------------

  SUBROUTINE ref2645 ! KHcc [1]
    IMPLICIT NONE

    ref = "2645"
    type = "M"

    ALLOCATE(temp(3))
    temp = (/ 5., 15., 25. /) + CtoK
    CALL CalcH("benzene",                      "71-43-2", (/ 0.092, 0.150, 0.237 /) ) ! C6H6
    CALL CalcH("methylbenzene",               "108-88-3", (/ 0.099, 0.164, 0.262 /) ) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",                "100-41-4", (/ 0.098, 0.179, 0.314 /) ) ! C6H5C2H5
    ! m/p-Xylene isomer mix, data not used here            (/ 0.117, 0.189, 0.295 /)
    CALL CalcH("1,2-dimethylbenzene",          "95-47-6", (/ 0.076, 0.125, 0.199 /) ) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("methyl {tert}-butyl ether",  "1634-04-4", (/ 0.012, 0.021, 0.036 /) ) ! CH3OC(CH3)3 MTBE
    CALL CalcH("ethyl {tert}-butyl ether",    "637-92-3", (/ 0.014, 0.031, 0.064 /) ) ! C2H5OC(CH3)3
    DEALLOCATE(temp)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KAW)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KAW
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, KHcc_TO_HcpSI(KAW,temp), Hominus, mindHR)
      CALL MakeNote(TRIM(ref), TRIM(citet())//" also provide data for "// &
        "supercooled water. Here, only data above 0~\unit{\degree C} "// &
        "were used to calculate the temperature dependence.")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2645

  !---------------------------------------------------------------------------

  SUBROUTINE ref2646 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2646"
    type = "M"

    CALL CalcH("methyl {tert}-butyl ether", "1634-04-4", 4745., 12.6, 5041., 9.83 ) ! CH3OC(CH3)3 MTBE
    CALL CalcH("ethyl {tert}-butyl ether",   "637-92-3", 6215., 18.1, 6510., 15.4 ) ! C2H5OC(CH3)3
    CALL CalcH("2-methoxy-2-methylbutane",   "994-05-8", 6228., 17.8, 6522., 15.1 ) ! C6H{14}O {tert}-amyl methyl ether
    CALL CalcH("diisopropyl ether",          "108-20-3", 6084., 18.1, 6378., 15.4 ) ! C3H7OC3H7
    CALL CalcH("TBF",                        "762-75-4", 3348., 7.67, 3640., 4.95 )
    CALL CalcH("methyl ethanoate",            "79-20-9", 4141., 8.79, 4455., 6.13 ) ! CH3COOCH3 methyl acetate

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A_KHcc, B_KHcc, A_KHpc, B_KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A_KHcc, B_KHcc, A_KHpc, B_KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*EXP(-A_KHpc/T0+B_KHpc))
      CALL consistency_check(Hominus, KHcc_TIMES_HcpSI_atT0/EXP(-A_KHcc/T0+B_KHcc), &
        "Different types of Henry's law constants")
      CALL consistency_check(A_KHpc, A_KHcc+T0, &
        "Temperature dependences of different types of Henry's law constants")
      CALL Output(Hominus, A_KHpc)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2646

  !---------------------------------------------------------------------------

  SUBROUTINE ref2647 ! KHcc [1]
    IMPLICIT NONE

    ref = "2647"
    chem = "methyl {tert}-butyl ether" ; casrn = "1634-04-4" ! CH3OC(CH3)3 MTBE
    type = "M"
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 276.15, 278.15, 283.15, 288.15, 293.15, 298.15 /)
    Harray = KHcc_TO_HcpSI( (/ 0.0091, 0.0112, 0.0117, 0.0177, 0.0224, 0.0292 /) ,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2647

  !---------------------------------------------------------------------------

  SUBROUTINE ref2802 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2802"
    type = "M"

    CALL CalcH("methyl methanoate",               "107-31-3",   4.2,  -33.E3) ! HCOOCH3 methyl formate
    CALL CalcH("ethyl methanoate",                "109-94-4",   3.4,  -38.E3) ! HCOOC2H5 ethyl formate
    CALL CalcH("propyl methanoate",               "110-74-7",   2.6,  -42.E3) ! HCOOC3H7 propyl formate
    CALL CalcH("ethyl ethanoate",                 "141-78-6",   6.0,  -49.E3) ! CH3COOC2H5 ethyl acetate
    CALL CalcH("2,2,2-trifluoroethyl methanoate", "32042-38-9", 0.55, -39.E3) ! CF3CH2OCOH
    CALL CalcH("ethyl trifluoroacetate",          "383-63-1",   0.09, -41.E3) ! C2H5OCOCF3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, dHsol)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      REAL,             INTENT(IN) :: dHsol
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI * H, -dHsol/Rgas)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2802

  !---------------------------------------------------------------------------

  SUBROUTINE ref2803 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2803"
    type = "M"

    CALL CalcH("2,2,2-trifluoroethyl ethanoate", "406-95-1", 0.58, -44.E3) ! CH3COOCH2CF3
    CALL CalcH("methyl trifluoroacetate",        "431-47-0", 0.12, -41.E3) ! CF3COOCH3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, dHsol)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      REAL,             INTENT(IN) :: dHsol
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI * H, -dHsol/Rgas)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2803

  !---------------------------------------------------------------------------

  SUBROUTINE ref2804 ! KHpc [atm/M]
    IMPLICIT NONE

    ref = "2804"
    type = "M"
    CALL CalcH("trichloronitromethane", "76-06-2",  2.1) ! CCl3NO2 chloropicrin
    CALL CalcH("methyl isothiocyanate", "556-61-6", 0.06)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI / KH)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2804

  !---------------------------------------------------------------------------

  SUBROUTINE ref2805 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2805"
    type = "M"
    chem = "isocyanic acid" ; casrn = "75-13-8"
    CALL Output(21.*Hcp_TO_HcpSI)

  END SUBROUTINE ref2805

  !---------------------------------------------------------------------------

  SUBROUTINE ref2806 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2806"
    type = "M"

    CALL CalcH("hydrazoic acid", "7782-79-8", 12.0, -31.E3) ! HN3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, deltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      REAL,             INTENT(IN) :: deltaH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI * H, -deltaH/Rgas)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2806

  !---------------------------------------------------------------------------

  SUBROUTINE ref2807 ! KHpc [atm/M]
    IMPLICIT NONE

    ref = "2807"
    chem = "hydrazoic acid" ; casrn = "7782-79-8"
    type = "M"
    Hominus = Hcp_TO_HcpSI / 0.100
    CALL MakeNoteOtherTemp("303")
    CALL Output(Hominus)

  END SUBROUTINE ref2807

  !---------------------------------------------------------------------------

  SUBROUTINE ref2808 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2808"
    type = "M"

    CALL CalcH("2,2,2-trifluoroethanol",           "75-89-8",  (/ 276., 283., 291., 299. /), (/ 255., 143., 81.7, 44.5 /)) ! CF3CH2OH
    CALL CalcH("2,2,3,3-tetrafluoro-1-propanol",   "76-37-9",  (/ 275., 283., 291., 299. /), (/ 991., 499., 259., 129. /)) ! CHF2CF2CH2OH
    CALL CalcH("2,2,3,3,3-pentafluoro-1-propanol", "422-05-9", (/ 275., 285., 291., 299. /), (/ 49.1, 28.3, 21.0, 14.0 /)) ! CF3CF2CH2OH

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, temp_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, H
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      ndata = SIZE(H)
      ALLOCATE(Harray(ndata))
      Harray = Hcp_TO_HcpSI * H
      CALL HTdep(temp_, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2808

  !---------------------------------------------------------------------------

  SUBROUTINE ref2814 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2814"
    type = "M"

    CALL CalcH("$\alpha$-1,2,3,4,5,6-hexachlorocyclohexane",       "319-84-6", 17.4, -5516.) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",        "58-89-9",  9.6, -3266.) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("1,1,1-trichloro-2,2-bis-(4-chlorophenyl)-ethane",   "50-29-3", 25.1, -7453.) ! C{14}H9Cl5 DDT; p,p'-DDT
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethane",      "72-54-8", 17.1, -5071.) ! C{14}H{10}Cl4 p,p'-DDD
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethene",      "72-55-9", 27.8, -7738.) ! C{14}H8Cl4 p,p'-DDE
    CALL CalcH("$\alpha$-endosulfan",                              "959-98-8", 13.7, -4157.) ! C9H6Cl6O3S endosulfan I
    CALL CalcH("$\beta$-endosulfan",                             "33213-65-9",  9.6, -3737.) ! C9H6Cl6O3S endosulfan II
    CALL CalcH("chlorpyrifos",                                    "2921-88-2", 27.7, -7802.) ! C9H{11}Cl3NO3PS
    CALL CalcH("aldrin",                                           "309-00-2", 16.8, -3868.) ! C{12}H8Cl6
    CALL CalcH("dieldrin",                                          "60-57-1", 19.4, -5758.) ! C{12}H8OCl6
    CALL CalcH("endrin",                                            "72-20-8", 14.9, -4620.) ! C{12}H8Cl6O
    CALL CalcH("{cis}-chlordane",                                 "5103-71-9", 22.4, -6108.) ! C{10}H6Cl8 $\alpha$-chlordane
    CALL CalcH("{trans}-chlordane",                               "5103-74-2", 28.2, -7584.) ! C{10}H6Cl8 $\gamma$-chlordane
    CALL CalcH("{trans}-nonachlor",                              "39765-80-5", 29.1, -7953.) ! C{10}H5Cl9
    CALL CalcH("{cis}-nonachlor",                                 "5103-73-1", 16.7, -5076.) ! C{10}H5Cl9
    CALL CalcH("heptachlor",                                        "76-44-8", 18.2, -4252.) ! C{10}H5Cl7
    CALL CalcH("heptachlorepoxide",                               "1024-57-3", 18.2, -5221.) ! C{10}H5Cl7O

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/(EXP(A+B/T0))
      mindHR = -B
      CALL Output(Hominus,mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2814

  !---------------------------------------------------------------------------

  SUBROUTINE ref2815 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2815"
    type = "M"
    chem = "S-ethyl dipropylthiocarbamate" ; casrn = "759-94-4" ! C9H{19}NOS eptam
    mindHR = 9134.
    Hominus = EXP(mindHR/T0-26.6)*Hcp_TO_HcpSI
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref2815

  !---------------------------------------------------------------------------

  SUBROUTINE ref2816 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2816"
    type = "M"
    chem = "pentadecafluorooctanoic acid" ; casrn = "335-67-1" ! C8HF{15}O2 perfluorooctanoic acid; PFOA
    CALL Output(5.0*Hcp_TO_HcpSI)

  END SUBROUTINE ref2816

  !---------------------------------------------------------------------------

  SUBROUTINE ref2817 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2817"
    type = "M"

    CALL CalcH("methanal",     "50-00-0",  -13.4, 6423.) ! HCHO formaldehyde
    CALL CalcH("benzaldehyde", "100-52-7", -17.5, 6258.) ! C6H5CHO

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      CALL Output(Hcp_TO_HcpSI*EXP(A+B/T0), B)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2817

  !---------------------------------------------------------------------------

  SUBROUTINE ref2818 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2818"
    type = "M"

    CALL CalcH("metolachlor", "51218-45-2", 3.0E-11, 10200.) ! C{15}H{22}ClNO2
    CALL CalcH("diazinon",      "333-41-5", 7.2E-15, 11900.) ! C{12}H{21}N2O3PS dimpylate

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI*A*EXP(B/T0), B)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2818

  !---------------------------------------------------------------------------

  SUBROUTINE ref2819 ! KHcc [1]
    IMPLICIT NONE

    ref = "2819"
    type = "M"

    CALL CalcH("carbazole",    "86-74-8",  1.01, -3982.) ! C{12}H9N
    CALL CalcH("phenanthrene", "85-01-8", 18.36, -7419.) ! C{14}H{10}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / (EXP(A+B/T0))
      ! calculate analytical derivative (see also util/derivative.f90):
      mindHR = -B + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2819

  !---------------------------------------------------------------------------

  SUBROUTINE ref2820 ! KHcc [1]
    IMPLICIT NONE
    REAL :: Tdep

    ref = "2820"
    type = "M"
    chem = "mercury" ; casrn = "7439-97-6" ! Hg
    Tdep = 2404.3
    Hominus = KHcc_TIMES_HcpSI_atT0 / EXP(-Tdep/T0+6.92)
    mindHR = Tdep + T0 ! see ref958, eqn (34) why T0 is added to mindHR
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2820

  !---------------------------------------------------------------------------

  SUBROUTINE ref2821 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2821"
    type = "M"
    chem = "N,N-dichloromethylamine" ; casrn = "7651-91-4"
    mindHR = 4298.11
    Hominus    = KHpcSI_TIMES_HcpSI/(EXP(20.14-mindHR/T0))
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref2821

  !---------------------------------------------------------------------------

  SUBROUTINE ref2822 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2822"

    CALL CalcH("propanone", "67-64-1", "M", 29., 5100.) ! CH3COCH3 acetone
    CALL CalcH("propanone", "67-64-1", "L", 33., 5300.) ! CH3COCH3 acetone

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI*A, B)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2822

  !---------------------------------------------------------------------------

  SUBROUTINE ref2823 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2823"
    type = "M"

    CALL CalcH("dimethyl sulfide",        "75-18-3", 0.47) ! CH3SCH3 DMS
    CALL CalcH("ethyl methyl sulfide",   "624-89-5", 0.43) ! C3H8S
    CALL CalcH("diethyl sulfide",        "352-93-2", 0.35) ! C2H5SC2H5
    CALL CalcH("allyl methyl sulfide", "10152-76-8", 0.43) ! C4H8S
    CALL CalcH("dimethyl disulfide",     "624-92-0", 0.59) ! CH3SSCH3
    CALL CalcH("diethyl disulfide",      "110-81-6", 0.37) ! C2H5SSC2H5
    CALL CalcH("dipropyl disulfide",     "629-19-6", 0.24) ! C6H14S2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI*HLC)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2823

  !---------------------------------------------------------------------------

  SUBROUTINE ref2824 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2824"
    type = "M"

    CALL CalcH("tetrachloroethene",        "127-18-4",  152.2,  10547., -21.23) ! C2Cl4 tetrachloroethylene
    CALL CalcH("trichloroethene",           "79-01-6",  98.26,   7936., -13.39) ! C2HCl3 trichloroethylene
    CALL CalcH("trichloromethane",          "67-66-3",  136.4,   9647., -19.23) ! CHCl3 chloroform
    CALL CalcH("1,1,1-trichloroethane",     "71-55-6",  537.7,  27579., -78.84) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1-dichloroethane",        "75-34-3",  132.7,   8917., -18.96) ! CHCl2CH3
    CALL CalcH("dichloromethane",           "75-09-2", -114.7,  -2908.,  17.39) ! CH2Cl2 methylene chloride
    CALL CalcH("tetrachloromethane",        "56-23-5",  460.6,  24398., -67.08) ! CCl4 carbontetrachloride
    CALL CalcH("1,2-dichloroethane",       "107-06-2",  659.0,  34827., -96.36) ! CH2ClCH2Cl
    CALL CalcH("($Z$)-1,2-dichloroethene", "156-59-2",  161.6,  10739., -23.01) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("chloromethane",             "74-87-3",  13.35,   2509., -1.661) ! CH3Cl methyl chloride
    CALL CalcH("chloroethane",              "75-00-3",  252.6,  14112., -36.79) ! C2H5Cl
    CALL CalcH("chloroethene",              "75-01-4", -132.4,  -3775.,  20.36) ! CH2CHCl vinyl chloride

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, C
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*EXP(A-B/T0+C*LOG(T0)))
      mindHR = B+C*T0 ! d(lnH)/d(1/T) = -delta H/R
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2824

  !---------------------------------------------------------------------------

  SUBROUTINE ref2825 ! KHcc [1]
    IMPLICIT NONE

    ref = "2825"
    type = "M"

    CALL CalcH("propanone",    "67-64-1", (/ 0.0047, 0.0067, 0.0096, 0.0132 /) ) ! CH3COCH3 acetone
    CALL CalcH("butanone",     "78-93-3", (/ 0.0079, 0.0117, 0.0173, 0.0236 /) ) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("2-pentanone", "107-87-9", (/ 0.0123, 0.0187, 0.0278, 0.0397 /) ) ! C3H7COCH3
    CALL CalcH("2-hexanone",  "591-78-6", (/ 0.0165, 0.0253, 0.0370, 0.0592 /) ) ! C6H{12}O
    CALL CalcH("2-heptanone", "110-43-0", (/ 0.0246, 0.0376, 0.0614, 0.0999 /) ) ! C7H{14}O

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ALLOCATE(temp(4))
      temp = (/ 323., 333., 343., 353. /)
      CALL HTdep(temp, KHcc_TO_HcpSI(H,temp), Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2825

  !---------------------------------------------------------------------------

  SUBROUTINE ref2826 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "2826"
    type = "M"

    CALL CalcH("N-methyl-2-pyrrolidone", "872-50-4", 48.9642, -52.6210, -21.9565 ) ! C5H9NO
    CALL CalcH("pyridine",               "110-86-1", 50.7195, -46.7951, -26.6745 ) ! C5H5N
    CALL CalcH("piperidine",             "110-89-4", 65.8177, -62.8464, -36.3248 ) ! C5H{10}NH

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, AH, BH, CH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: AH, BH, CH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = cH2O / (1E3*EXP(AH+BH))
      mindHR = -BH*T0 + CH*T0
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2826

  !---------------------------------------------------------------------------

  SUBROUTINE ref2827 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2827"
    type = "M"
    chem = "hydrogen peroxide" ; casrn = "7722-84-1" ! H2O2
    mindHR = 7024.
    Hominus = EXP(mindHR/T0-11.97)*Hcp_TO_HcpSI
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref2827

  !---------------------------------------------------------------------------

  SUBROUTINE ref2828 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2828"
    type = "L"

    CALL CalcH("nitrogen",                "7727-37-9", 6.482E-4, 1563.) ! N2
    CALL CalcH("oxygen",                  "7782-44-7", 1.246E-3, 1738.) ! O2
    CALL CalcH("argon",                   "7440-37-1", 1.397E-3, 1683.) ! Ar
    CALL CalcH("methane",                   "74-82-8", 1.407E-3, 1864.) ! CH4
    CALL CalcH("carbon monoxide",          "630-08-0", 9.857E-4, 1348.) ! CO
    CALL CalcH("dinitrogen monoxide",    "10024-97-2", 2.42E-2,  2665.) ! N2O nitrous oxide; laughing gas
    CALL CalcH("hydrogen peroxide",       "7722-84-1", 9.17E4,   6605.) ! H2O2
    CALL CalcH("methyl hydroperoxide",    "3031-73-0", 2.94E2,   5219.) ! CH3OOH methylperoxide
    CALL CalcH("nitrogen monoxide",      "10102-43-9", 1.95E-3,  1598.) ! NO nitric oxide
    CALL CalcH("nitrogen dioxide",       "10102-44-0", 1.0E-2         ) ! NO2
    CALL CalcH("peroxyacetyl nitrate",    "2278-22-0", 2.95,     5701.) ! CH3COOONO2 PAN
    CALL CalcH("carbon oxide sulfide",     "463-58-1", 2.1E-2,   3262.) ! OCS carbonyl sulfide
    CALL CalcH("carbon disulfide",          "75-15-0", 6.2E-2,   3902.) ! CS2
    CALL CalcH("dimethyl sulfide",          "75-18-3", 0.565,    3502.) ! CH3SCH3 DMS
    CALL CalcH("sulfur hexafluoride",     "2551-62-4", 2.44E-4,  3060.) ! SF6
    CALL CalcH("methanal",                  "50-00-0", 3.2E3,    6775.) ! HCHO formaldehyde
    CALL CalcH("trichlorofluoromethane",    "75-69-4", 1.11E-2,  3449.) ! CFCl3 R11
    CALL CalcH("dichlorodifluoromethane",   "75-71-8", 3.03E-3,  3420.) ! CF2Cl2 R12
    CALL CalcH("tetrafluoromethane",        "75-73-0", 2.09E-4,  2258.) ! CF4 carbontetrafluoride

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H298, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H298
      REAL, OPTIONAL,   INTENT(IN) :: B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      Hominus   = H298*Hcp_TO_HcpSI
      IF (PRESENT(B)) THEN
        CALL Output(Hominus, B) ! T-dep with 2 parameters A, B
      ELSE
        CALL Output(Hominus) ! no T-dep available
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2828

  !---------------------------------------------------------------------------

  SUBROUTINE ref2829 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2829"
    type = "M"
    chem = "methanal" ; casrn = "50-00-0" ! HCHO formaldehyde
    mindHR = 1641.3
    Hominus = Hcp_TO_HcpSI / EXP(-mindHR/T0-3.089)
    CALL MakeNote("HCHOdiol")
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref2829

  !---------------------------------------------------------------------------

  SUBROUTINE ref2830 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "2830"
    type = "M"

    CALL CalcH("trichlorofluoromethane",  "75-69-4", &
      (/ 0.0, 2.6, 3.5, 5.2, 5.3, 5.4, 7.0, 7.2, 7.4, 7.9, 10.0, 10.2, 10.4, &
      10.5, 12.3, 15.0, 15.1, 15.2, 17.9, 18.0, 19.7, 20.2, 20.7, 25.2, 25.5, 27.9, 31.6 /), &
      (/ 0.603, 0.517, 0.493, 0.451, 0.448, 0.460, 0.402, 0.415, 0.411, 0.390, &
      0.331, 0.343, 0.344, 0.352, 0.313, 0.276, 0.274, 0.274, 0.247, 0.233, &
      0.231, 0.212, 0.218, 0.177, 0.175, 0.158, 0.142 /) ) ! CFCl3 R11
    CALL CalcH("dichlorodifluoromethane", "75-71-8", &
      (/ 0.0,   2.6,   5.6,   10.4,   15.2,   18.0,   19.7,   20.7,   25.2,   25.5,   31.6 /),  &
      (/ 0.153, 0.133, 0.117, 0.0942, 0.0773, 0.0646, 0.0646, 0.0646, 0.0505, 0.0532, 0.0434 /) ) ! CF2Cl2 R12

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
      CALL MakeNote("seawater")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2830

  !---------------------------------------------------------------------------

  SUBROUTINE ref2831 ! Kuenen coefficient S [cm3/g]
    IMPLICIT NONE

    ref = "2831"
    type = "M"

    CALL CalcH("argon",                "7440-37-1", &
      (/ 5.,   10., 15.,  20.,  25.,  35.,  45.,  50. /), &
      (/ 47.6, 42.3, 38.0, 34.5, 31.7, 27.3, 24.3, 23.1 /) ) ! Ar
    CALL CalcH("tetrafluoromethane",     "75-73-0", &
      (/ 2.4,  5.0,  10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0 /), &
      (/ 9.00, 8.26, 7.00, 5.94, 5.30, 4.74, 4.30, 3.95, 3.75, 3.52, 3.39 /) ) ! CF4
    CALL CalcH("sulfur hexafluoride",  "2551-62-4", &
      (/ 2.5,   5.0,  10.0, 12.5, 15.0, 17.5, 20.0, 22.5, 25.0, 27.5, 30.0, 35.0, 40.0,  45.0, 50.0 /), &
      (/ 12.86, 11.35, 9.13, 8.23, 7.51, 6.90, 6.28, 5.88, 5.45, 5.07, 4.76, 4.29, 3.99, 3.73, 3.52 /) ) ! SF6
    CALL CalcH("nitrogen trifluoride", "7783-54-2", &
      (/ 5.0,   10.0,  15.0,  20.0,  25.0,  30.0,  35.0,  40.0,  45.0,  50.0  /), &
      (/ 31.19, 26.17, 22.70, 20.03, 17.97, 15.94, 14.79, 13.69, 12.85, 12.22 /) ) ! NF3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, S)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, S
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(S)
      ALLOCATE(Harray(ndata))
      Harray = 1E-3 * S * S_TO_HcpSI
      CALL HTdep(temp_+CtoK, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2831

  !---------------------------------------------------------------------------

  SUBROUTINE ref2832 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2832"
    type = "M"
    chem = "dinitrogen monoxide" ; casrn = "10024-97-2" ! N2O nitrous oxide; laughing gas
    ndata = 27
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ &
      0.29, 0.29, 0.29, 0.29, 0.29, 10.01, 10.00, 10.00, 10.00, 10.01, 10.00, &
      19.96, 19.98, 19.98, 19.97, 19.97, 19.98, 30.20, 30.19, 30.21, 30.21, &
      30.22, 40.08, 40.08, 40.09, 40.08, 40.09 /) + CtoK
    Harray = 1E-2 * Hcp_TO_HcpSI * (/  &
      5.870, 5.858, 5.870, 5.870, 5.858, 4.016, 4.017, 4.007, 4.010, 4.013, &
      4.019, 2.882, 2.880, 2.873, 2.878, 2.879, 2.875, 2.156, 2.153, 2.155, &
      2.149, 2.151, 1.693, 1.692, 1.691, 1.696, 1.694 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2832

  !---------------------------------------------------------------------------

  SUBROUTINE ref2833 ! KHpc [atm*cm3/mol]
    IMPLICIT NONE

    ref = "2833"
    type = "M"
    chem = "dinitrogen monoxide" ; casrn = "10024-97-2" ! N2O nitrous oxide; laughing gas
    Hominus = 2.44E-5 * 1E3 * Hcp_TO_HcpSI
    CALL Output(Hominus)

  END SUBROUTINE ref2833

  !---------------------------------------------------------------------------

  SUBROUTINE ref2834 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ! The absorption coefficient beta is not defined in this paper but
    ! it is assumed that it has the same definition as in ref3026.

    ref = "2834"
    type = "M"

    CALL CalcH("nitrogen monoxide", "10102-43-9", &
      (/ 0.07, 0.07, 0.07, 10.02, 10.05, 9.99, 19.93, 20.02, 20.12, 30.02, &
      29.99, 30.02, 39.91, 39.95, 40.02 /), &
      (/ 0.07380, 0.07376, 0.07345, 0.05712, 0.05706, 0.05700, 0.04705, 0.04705, &
      0.04703, 0.03999, 0.04011, 0.03999, 0.03509, 0.03519, 0.03499 /) ) ! NO nitric oxide
    CALL CalcH("carbon monoxide",     "630-08-0", &
      (/ 0.17, 0.17, 0.17, 0.07, 0.02, 0.02, 10.02, 10.01, 10.02, 10.10, 10.03, &
      10.07, 20.03, 20.01, 20.00, 19.97, 20.02, 20.02, 29.97, 30.03, 30.08, &
      30.02, 30.04, 30.02, 30.10, 30.07, 30.07, 39.77, 39.68, 39.62, 40.00, &
      39.99, 39.97, 40.02, 39.97, 39.95 /), &
      (/ 0.03513, 0.03515, 0.03511, 0.03540, 0.03550, 0.03544, 0.02803, 0.02797, &
      0.02803, 0.02829, 0.02824, 0.02824, 0.02313, 0.02312, 0.02320, 0.02315, &
      0.02326, 0.02328, 0.01997, 0.01997, 0.01994, 0.02001, 0.02000, 0.01985, &
      0.02001, 0.02001, 0.01997, 0.01775, 0.01775, 0.01778, 0.01780, 0.01779, &
      0.01769, 0.01773, 0.01782, 0.01780 /) ) ! CO
    CALL CalcH("methane",              "74-82-8", &
      (/ 0.25, 0.30, 0.23, 0.27, 0.28, 9.98, 10.00, 10.00, 20.08, 20.00, 20.00, &
      19.98, 20.05, 20.02, 20.00, 20.00, 20.00, 29.95, 30.10, 30.00, 40.02, &
      40.03, 40.00 /), &
      (/ 0.05528, 0.05523, 0.05515, 0.05516, 0.05511, 0.04178, 0.04186, 0.04171, &
      0.03307, 0.03312, 0.03321, 0.03311, 0.03305, 0.03298, 0.03302, 0.03307, &
      0.03299, 0.02764, 0.02761, 0.02759, 0.02375, 0.02363, 0.02367 /) ) ! CH4
    CALL CalcH("ethane",               "74-84-0", &
      (/ 0.40, 0.42, 0.48, 0.25, 0.28, 0.32, 10.03, 10.02, 10.03, 19.93, 20.03, &
      20.02, 20.00, 20.00, 20.00, 30.00, 30.00, 30.00, 30.00, 30.00, 30.00, &
      39.98, 39.95, 40.00 /), &
      (/ 0.09692, 0.09716, 0.09707, 0.09791, 0.09737, 0.09723, 0.06548, 0.06557, &
      0.06555, 0.04747, 0.04745, 0.04740, 0.04707, 0.04707, 0.04699, 0.03634, &
      0.03632, 0.03639, 0.03608, 0.03616, 0.03615, 0.02913, 0.02915, 0.02921 /) ) ! C2H6

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

  END SUBROUTINE ref2834

  !---------------------------------------------------------------------------

  SUBROUTINE ref2835 ! special definition [mass%/atm]
    IMPLICIT NONE

    REAL, PARAMETER :: M_CO2 = MC+MO*2.
    REAL, PARAMETER :: M_OCS = MC+MO+MS
    REAL, PARAMETER :: M_CS2 = MC+MS*2.

    ref = "2835"

    ! CO2:
    !CALL CalcH("carbon dioxide",      "124-38-9", (/ 0.335, 0.232, 0.169 /), M_CO2, "2841")

    ! OCS:
    CALL CalcH("carbon oxide sulfide", "463-58-1", (/ 0.356, 0.224, 0.149 /), M_OCS, "winkler07")

    ! CS2:
    !CALL CalcH("carbon disulfide",     "75-15-0", (/ 0.258, 0.239, 0.217 /), M_CS2, "863")
    chem = "carbon disulfide" ; casrn = "75-15-0" ! CS2
    type = "?"
    ! The Rex data change by a factor of 3 going from 0 to 20 C. The
    ! data here change only a little bit.
    CALL MakeNote("2835rex", &
      TRIM(citet())//" converted data from \citet{863} to another unit. However, "// &
      "this was apparently not done correctly.")
    CALL Output(DUMMY)

    ! CS2:
    !CALL CalcH("carbon disulfide",     "75-15-0", (/ 0.204, 0.194, 0.179 /), M_CS2, "2628")
    chem = "carbon disulfide" ; casrn = "75-15-0" ! CS2
    type = "?"
    CALL MakeNote("2835chancel", &
      TRIM(citet())//" present data from \citet{2628}. However, in that paper only the "// &
      "solubility at an unknown partial pressure of \chem{CS_2} was measured.")
    CALL Output(DUMMY)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, solub, MX, xref)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: solub
      REAL,               INTENT(IN) :: MX
      CHARACTER(LEN=*),   INTENT(IN) :: xref ! ref for experimental value
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      ALLOCATE(temp(3), Harray(3))
      temp = (/ 0., 10., 20. /) + CtoK
      Harray = solub * percent * rhoH2O / (atm * MX)
      CALL HTdep(temp, Harray , Hominus, mindHR)
      CALL SettypeX(xref)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2835

  !---------------------------------------------------------------------------

  SUBROUTINE ref2836 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2836"
    type = "V"

    CALL CalcH("metolachlor",              "51218-45-2", 2.44E-3) ! C{15}H{22}ClNO2
    CALL CalcH("terbuthylazine",            "5915-41-3", 4.05E-3) ! C9H{16}ClN5
    CALL CalcH("isoproturon",              "34123-59-6", 1.05E-5) ! C{12}H{18}N2O
    CALL CalcH("desethylterbuthylazine",   "30125-63-4", 4.60E-4)
    CALL CalcH("monodesmethylisoproturon", "34123-57-4", 3.57E-6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/H
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2836

  !---------------------------------------------------------------------------

  SUBROUTINE ref2837 ! KHpc [atm*cm3/mol]
    IMPLICIT NONE

    ref = "2837"
    type = "M"

    ! Several values for toluene are presented; data are not used here.
    CALL CalcH("butanone",                      "78-93-3", 62.2) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("4-methyl-2-pentanone",         "108-10-1", 256.) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("2-pentanone",                  "107-87-9", 88.5) ! C3H7COCH3
    CALL CalcH("2-heptanone",                  "110-43-0", 159.) ! C7H{14}O
    CALL CalcH("1-butanol",                     "71-36-3", 9.16) ! C4H9OH
    CALL CalcH("2-propanol",                    "67-63-0", 8.73) ! C3H7OH isopropanol
    CALL CalcH_Tdep("3-oxa-1-heptanol",        "111-76-2", &
      (/ 20., 22., 23., 25., 25., 30., 30. /), &
      (/ 0.41, 0.630, 0.678, 0.81, 0.816, 1.01, 1.35 /) ) ! butyl cellosolve
    CALL CalcH_Tdep("2-butoxyethyl ethanoate", "112-07-2", &
      (/ 20., 25. /), (/ 1.27, 5.46 /) ) ! butyl cellosolve acetate
    CALL CalcH("butyl carbitol",               "112-34-5", 0.0072)
    CALL CalcH("N-methyl-2-pyrrolidone",       "872-50-4", 0.0032) ! C5H9NO
    CALL CalcH("methanal",                      "50-00-0", 0.1, "295") ! HCHO formaldehyde

  CONTAINS

    SUBROUTINE CalcH_Tdep (chem_, casrn_, temp_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(H)
      ALLOCATE(Harray(ndata))
      Harray = 1E3 * Hcp_TO_HcpSI / H
      CALL HTdep(temp_+CtoK, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH_Tdep

    SUBROUTINE CalcH (chem_, casrn_, H, othertemp)
      IMPLICIT NONE
      CHARACTER(LEN=*),           INTENT(IN) :: chem_
      CHARACTER(LEN=*),           INTENT(IN) :: casrn_
      REAL,                       INTENT(IN) :: H
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: othertemp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1E3 * Hcp_TO_HcpSI / H
      IF (PRESENT(othertemp)) THEN
        CALL MakeNoteOtherTemp(othertemp)
      ENDIF
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2837

  !---------------------------------------------------------------------------

  SUBROUTINE ref2838 ! Hx2p [1/atm]
    IMPLICIT NONE

    ref = "2838"

    ! NH3 data are from ref0620 and not reproduced here

    ! HNO3 data are from ref2839 and not reproduced here

    chem = "sulfuric acid" ; casrn = "7664-93-9" ! H2SO4
    type = "V"
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" estimate a Henry's law constant of" // &
      " 5\E{11}~\unit{atm^{-1}} at 303.15 K for the reaction" // &
      " \chem{H_2SO_4(g)} $\rightleftharpoons$ 2 \chem{H^+(aq)} +" // &
      " \chem{SO_4^{2-}(aq)} but don't give a definition for it." // &
      " Probably it is defined as $x^2(\chem{H^+})\times" // &
      " x(\chem{SO_4^{2-}})/p(\chem{H_2SO_4})$, where $x$ is the" // &
      " aqueous-phase mixing ratio.")
    CALL Output(DUMMY)

  END SUBROUTINE ref2838

  !---------------------------------------------------------------------------

  SUBROUTINE ref2839 ! Hx2p [1/atm]
    IMPLICIT NONE
    REAL :: A,B,C,D,E

    ref = "2839"
    type = "T"
    chem = "nitric acid" ; casrn = "7697-37-2" ! HNO3
    A = 385.972199
    B = - 3020.3522
    C = - 71.001998
    D = 0.131442311
    E = - 0.420928363E-4
    ! Hx2p(HNO3) = x(H+) * x(NO3-) / p(HNO3)
    Hominus = EXP(A + B/T0 + C*LOG(T0) + D*T0 + E*T0**2) * cH2O**2/atm
    ! mindHR = analytical solution of d ln(Hominus) / d (1/T) at 298.15 K:
    mindHR = B - C*T0 - D*T0**2 - 2*E*T0**3
    CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn), &
      "$\Hprime$~= $"//TRIM(Hprime_string(Hominus))// &
      "\times\exp\left("//TRIM(mindHR_string(mindHR))//"~\unit{K} "// &
      "\left(\DS\frac{1}{T}-\frac{1}{T^{\ominus}}\right)\right)$~"// &
      TRIM(Hprime_unit_ol_TeX))
    CALL Output(DUMMY)

  END SUBROUTINE ref2839

  !---------------------------------------------------------------------------

  SUBROUTINE ref2840 ! Hx2p [1/atm]
    IMPLICIT NONE

    ref = "2840"
    type = "T"

    CALL CalcH("hydrogen chloride", "7647-01-0", 6.4954, -9.0027E3, -65.346, 0.078178) ! HCl
    CALL CalcH("hydrogen bromide", "10035-10-6", 12.506, -10.241E3, -67.177, 0.078178) ! HBr

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, C, D
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      ! x2p(HX) = x(H+) * x(NO3-) / p(HX)
      !   = A + B * (1/T0-1/T) + C * (T0/T-1+ln(T/T0)) + D * ((T0/T-1)*T0+T-T0)
      ! at T0, only "A" is needed. All other terms are zero:
      Hominus = EXP(A) * cH2O**2/atm
      ! mindHR = analytical solution of d ln(Hominus) / d (1/T) at 298.15 K:
      !   = -B + C * (T0-T) + D * (T0-T)^2
      ! at T0, only "B" is needed. All other terms are zero:
      mindHR = -B
      CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn), &
        "$\Hprime$~= $"//TRIM(Hprime_string(Hominus))// &
        "\times\exp\left("//TRIM(mindHR_string(mindHR))//"~\unit{K} "// &
        "\left(\DS\frac{1}{T}-\frac{1}{T^{\ominus}}\right)\right)$~"//TRIM(Hprime_unit_ol_TeX))
      CALL Output(DUMMY)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2840

  !---------------------------------------------------------------------------

  SUBROUTINE ref2841 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "2841"
    type = "M"

    ! for comparison with ref2835, use data at 0, 10, and 20 C:
    !CALL CalcH("carbon dioxide", "124-38-9", &
    !  (/0.,10.,20./), (/1.713,1.194,0.878/))

    CALL CalcH("carbon dioxide", "124-38-9", &
      (/ 0.1, 3.8, 6.8, 12.3, 14.8, 18.9, 21.2, 22.6, 26.2, 31.1, 37.3, 40.4, &
      43.9, 49.8, 61.4 /), &
      (/ 1.7062, 1.4922, 1.3411, 1.0985, 1.0232, 0.9128, 0.8497, 0.8137, 0.7314, &
      0.6487, 0.5629, 0.5304, 0.4905, 0.4382, 0.3508 /) ) ! CO2

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

  END SUBROUTINE ref2841

  !---------------------------------------------------------------------------

  SUBROUTINE ref2891 ! Hbp [mol/(kg*bar)]
    IMPLICIT NONE

    ref = "2891"
    type = "M"

    ! Tab. 1:
    CALL CalcH("diethyl ether-d10",                "2679-89-2", 1.314,  6535.)
    CALL CalcH("propanone-2-13C",                  "3881-06-9", 31.1,   5278.)
    CALL CalcH("dichloromethane-d2",               "1665-00-5", 0.379,  4564.)
    CALL CalcH("nitromethane-13C",                "32480-00-5", 48.52,  5029.)
    CALL CalcH("hexafluorobenzene",                 "392-56-3", 0.055,  5204.)
    CALL CalcH("tetrahydrofuran-d8",               "1693-74-9", 23.49,  8036.)
    CALL CalcH("ethyl ethanoate-1-13C",            "3424-59-7", 7.165,  6534.)
    CALL CalcH("pentafluorobenzene",                "363-72-4", 0.075,  4762.)
    CALL CalcH("benzene-d6",                       "1076-43-3", 0.181,  3979.)
    CALL CalcH("1,2-dichloroethane-d4",           "17060-07-0", 0.87,   4299.)
    CALL CalcH("fluorobenzene",                     "462-06-6", 0.162,  3882.)
    CALL CalcH("1,4-difluorobenzene",               "540-36-3", 0.163,  3862.)
    CALL CalcH("1,2-dichloropropane-d6",          "93952-08-0", 0.358,  4584.)
    CALL CalcH("1,4-dioxane-d8",                  "17647-74-4", 278.0,  6832.)
    CALL CalcH("methylbenzene-d8",                 "2037-26-5", 0.197,  4276.)
    CALL CalcH("pyridine-d5",                      "7291-22-7", 417.2, 10204.)
    CALL CalcH("1,1,2-trichloroethane-d3",       "171086-93-4", 1.304,  5073.)
    CALL CalcH("1,2-dibromoethane-d4",            "22581-63-1", 1.589,  4848.)
    CALL CalcH("chlorobenzene-d5",                 "3114-55-4", 0.36,   4472.)
    CALL CalcH("1,2-dimethylbenzene-d10",         "56004-61-6", 0.304,  4708.)
    CALL CalcH("4-bromofluorobenzene",              "460-00-4", 0.533,  4437.)
    CALL CalcH("bromobenzene-d5",                  "4165-57-5", 0.651,  4157.)
    CALL CalcH("1,2-dichlorobenzene-d4",           "2199-69-1", 0.819,  4201.)
    CALL CalcH("decafluorobiphenyl",                "434-90-2", 0.674,  3560.)
    CALL CalcH("nitrobenzene-d5",                  "4165-60-0", 84.93,  7479.)
    CALL CalcH("1-phenylethanone-d5",             "28077-64-7", 233.2, 10278.)
    CALL CalcH("1,2,4-trichlorobenzene-d3",        "2199-72-6", 0.979,  4628.)
    CALL CalcH("naphthalene-d8",                   "1146-65-2", 3.484,  5331.)
    CALL CalcH("1-methylnaphthalene-d10",         "38072-94-5", 4.652,  5397.)
    CALL CalcH("methylcyclohexane-d14",           "10120-28-2", 0.031,  5637.)
    CALL CalcH("2-chloroethanol-d4",             "117067-62-6", 498.3,  8738.)
    CALL CalcH("ethylbenzene-d10",                "25837-05-2", 0.202,  4236.)
    CALL CalcH("1,2,3-trichlorobenzene-d3",        "3907-98-0", 1.465,  4587.)
    CALL CalcH("3,5-di-tert-butyltoluene",        "15181-11-0", 0.373,  9122.)
    CALL CalcH("3,5-dibromotoluene",               "1611-92-3", 1.741,  4826.)
    CALL CalcH("azulene",                           "275-51-4", 14.88,  7764.)
    CALL CalcH("alpha,alpha-dichloro-o-xylene",     "612-12-4", 10.41, 11086.)
    CALL CalcH("chloroethene-d3",                  "6745-35-3", 0.038,  3087.)
    CALL CalcH("butanone-1,1,1,3,3-d5",           "24313-50-6", 37.14,  8164.)
    CALL CalcH("2-hexanone-1,1,1,3,3-d5",          "4840-82-8", 16.77,  9044.)

    ! Tab. 2:
    CALL CalcH("dichlorodifluoromethane",             "75-71-8", 0.0126, 5476.)
    CALL CalcH("chloromethane",                       "74-87-3", 0.0792, 2383.)
    CALL CalcH("chloroethene",                        "75-01-4", 0.0393, 3193.)
    CALL CalcH("bromomethane",                        "74-83-9", 0.1266, 2798.)
    CALL CalcH("chloroethane",                        "75-00-3", 0.0857, 3181.)
    CALL CalcH("trichlorofluoromethane",              "75-69-4", 0.0281, 5077.)
    CALL CalcH("diethyl ether",                       "60-29-7", 1.137,  6585.)
    CALL CalcH("1,1,2-trichlorotrifluoroethane",      "76-13-1", 0.0205, 5695.)
    CALL CalcH("1,1-dichloroethene",                  "75-35-4", 0.0408, 4636.)
    CALL CalcH("iodomethane",                         "74-88-4", 0.1815, 3245.)
    CALL CalcH("3-chloro-1-propene",                 "107-05-1", 0.1309, 4496.)
    CALL CalcH("ethane nitrile",                      "75-05-8", 60.18,  6261.)
    CALL CalcH("methyl ethanoate",                    "79-20-9", 11.84,  7493.)
    CALL CalcH("carbon disulfide",                    "75-15-0", 0.0573, 3815.)
    CALL CalcH("dichloromethane",                     "75-09-2", 0.4009, 3909.)
    CALL CalcH("methyl {tert}-butyl ether",         "1634-04-4", 1.67,   9082.)
    CALL CalcH("2-propenenitrile",                   "107-13-1", 11.83,  6784.)
    CALL CalcH("($E$)-1,2-dichloroethene",           "156-60-5", 0.1023, 4003.)
    CALL CalcH("1,1-dichloroethane",                  "75-34-3", 0.1985, 3901.)
    CALL CalcH("2,2-dichloropropane",                "594-20-7", 0.0443, 7448.)
    CALL CalcH("propane nitrile",                    "107-12-0", 42.84,  6248.)
    CALL CalcH("($Z$)-1,2-dichloroethene",           "156-59-2", 0.2692, 3816.)
    CALL CalcH("2-methyl-2-propene nitrile",         "126-98-7", 5.453,  6671.)
    CALL CalcH("trichloromethane",                    "67-66-3", 0.2764, 4453.)
    CALL CalcH("bromochloromethane",                  "74-97-5", 0.6649, 4716.)
    CALL CalcH("cyclohexane",                        "110-82-7", 0.0318, 5430.)
    CALL CalcH("1,1,1-trichloroethane",               "71-55-6", 0.0689, 4023.)
    CALL CalcH("1,1-dichloropropene",                "563-58-6", 0.0615, 4226.)
    CALL CalcH("tetrachloromethane",                  "56-23-5", 0.0497, 4459.)
    CALL CalcH("1,2-dichloroethane",                 "107-06-2", 0.8195, 4376.)
    CALL CalcH("benzene",                             "71-43-2", 0.1831, 3844.)
    CALL CalcH("trichloroethene",                     "79-01-6", 0.1187, 4695.)
    CALL CalcH("methylcyclohexane",                  "108-87-2", 0.0318, 5344.)
    CALL CalcH("1,2-dichloropropane",                 "78-87-5", 0.4286, 4425.)
    CALL CalcH("methyl methacrylate",                 "80-62-6", 4.328,  7685.)
    CALL CalcH("dibromomethane",                      "74-95-3", 1.222,  4988.)
    CALL CalcH("bromodichloromethane",                "75-27-4", 0.5175, 4651.)
    CALL CalcH("1,4-dioxane",                        "123-91-1", 232.1,  6620.)
    CALL CalcH("4-methyl-2-pentanone",               "108-10-1", 9.994,  8696.)
    CALL CalcH("{trans}-1,3-dichloropropene",      "10061-02-6", 0.5821, 4781.)
    CALL CalcH("methylbenzene",                      "108-88-3", 0.2129, 4393.)
    CALL CalcH("{cis}-1,3-dichloropropene",        "10061-01-5", 0.9569, 5455.)
    CALL CalcH("2-hexanone",                         "591-78-6", 15.37,  8595.)
    CALL CalcH("1,1,2-trichloroethane",               "79-00-5", 1.394,  5400.)
    CALL CalcH("1,3-dichloropropane",                "142-28-9", 1.296,  5282.)
    CALL CalcH("tetrachloroethene",                  "127-18-4", 0.0995, 4589.)
    CALL CalcH("dibromochloromethane",               "124-48-1", 1.071,  5267.)
    CALL CalcH("1,2-dibromoethane",                  "106-93-4", 1.732,  5532.)
    CALL CalcH("chlorobenzene",                      "108-90-7", 0.3688, 4378.)
    CALL CalcH("1,1,1,2-tetrachloroethane",          "630-20-6", 0.478,  4790.)
    CALL CalcH("ethylbenzene",                       "100-41-4", 0.2033, 4138.)
    CALL CalcH("1,2-dimethylbenzene",                 "95-47-6", 0.3202, 4472.)
    CALL CalcH("ethenylbenzene",                     "100-42-5", 0.4455, 4632.)
    CALL CalcH("(2-propyl)-benzene",                  "98-82-8", 0.1396, 4865.)
    CALL CalcH("tribromomethane",                     "75-25-2", 2.173,  6306.)
    CALL CalcH("($Z$)-1,4-dichloro-2-butene",       "1476-11-5", 3.04,   9372.)
    CALL CalcH("1,1,2,2-tetrachloroethane",           "79-34-5", 3.335,  7160.)
    CALL CalcH("1,2,3-trichloropropane",              "96-18-4", 4.233,  7213.)
    CALL CalcH("propylbenzene",                      "103-65-1", 0.188,  4533.)
    CALL CalcH("bromobenzene",                       "108-86-1", 0.598,  4256.)
    CALL CalcH("($E$)-1,4-dichloro-2-butene",        "110-57-6", 3.501,  6557.)
    CALL CalcH("1,3,5-trimethylbenzene",             "108-67-8", 0.2349, 5124.)
    CALL CalcH("1-chloro-2-methylbenzene",            "95-49-8", 0.3182, 4065.)
    CALL CalcH("1-chloro-4-methylbenzene",           "106-43-4", 0.4071, 4213.)
    CALL CalcH("(1,1-dimethylethyl)-benzene",         "98-06-6", 0.1578, 4738.)
    CALL CalcH("(1-methylpropyl)-benzene",           "135-98-8", 0.1321, 4584.)
    CALL CalcH("pentachloroethane",                   "76-01-7", 0.5908, 5413.)
    CALL CalcH("1,2,4-trimethylbenzene",              "95-63-6", 0.3209, 5155.)
    CALL CalcH("1-methyl-4-(1-methylethyl)-benzene",  "99-87-6", 0.1808, 4935.)
    CALL CalcH("1,3-dichlorobenzene",                "541-73-1", 0.5257, 4826.)
    CALL CalcH("1,4-dichlorobenzene",                "106-46-7", 0.5816, 4588.)
    CALL CalcH("butylbenzene",                       "104-51-8", 0.1963, 4488.)
    CALL CalcH("1,2-dichlorobenzene",                 "95-50-1", 0.8016, 4231.)
    CALL CalcH("1-phenylethanone",                    "98-86-2", 97.41,  6796.)
    CALL CalcH("1,2-dibromo-3-chloropropane",         "96-12-8", 9.743,  7076.)
    CALL CalcH("nitrobenzene",                        "98-95-3", 63.99,  7520.)
    CALL CalcH("1,2,4-trichlorobenzene",             "120-82-1", 1.069,  5124.)
    CALL CalcH("hexachlorobutadiene",                 "87-68-3", 0.2357, 6176.)
    CALL CalcH("naphthalene",                         "91-20-3", 3.321,  6102.)
    CALL CalcH("1,2,3-trichlorobenzene",              "87-61-6", 1.537,  4833.)
    CALL CalcH("2-methylnaphthalene",                 "91-57-6", 3.543,  5546.)
    CALL CalcH("1-methylnaphthalene",                 "90-12-0", 4.412,  5854.)
    CALL CalcH("2-propanol",                          "67-63-0", 108.6,  8359.)
    CALL CalcH("2-methyl-2-propanol",                 "75-65-0", 143.2,  7876.)
    CALL CalcH("2-propyn-1-ol",                      "107-19-7", 378.2,  7386.)
    CALL CalcH("2-methoxyethanol",                   "109-86-4", 437.6,  7479.)
    CALL CalcH("3-oxa-1-heptanol",                   "111-76-2", 350.2,  7657.)
    CALL CalcH("2-methyl-1-hexanol",                 "624-22-6", 68.92, 11227.)
    CALL CalcH("(chloromethyl)-benzene",             "100-44-7", 1.96,   7213.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC, constant)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC, constant
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(HLC*rhoH2O/bar, constant)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2891

  !---------------------------------------------------------------------------

  ! ref2894

  ! Most of the data from this internal document was later published in
  ! a peer-reviewed journal by Ashworth et al. (1988) = ref1156.

  ! Apparently, the phenol data were not published by Ashworth et al.
  ! (1988). It is not used here because there are already enough other
  ! measurements for phenol available in the peer-reviewed literature.

  !---------------------------------------------------------------------------

  SUBROUTINE ref2895 ! Hcc [1]
    IMPLICIT NONE

    ref = "2895"
    type = "M"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 298.15, 308.15, 323.15 /)
    CALL CalcH("tribromomethane",           "75-25-2", (/ 20.9, 18.8, 15.4 /)) ! CHBr3 bromoform
    CALL CalcH("1,2-dibromoethane",        "106-93-4", (/ 30.9, 15.9, 14.9 /)) ! C2H4Br2
    CALL CalcH("1,1,2,2-tetrabromoethane",  "79-27-6", (/ 25.1, 22.6, 21.8 /)) ! C2H2Br4
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

  END SUBROUTINE ref2895

  !---------------------------------------------------------------------------

  SUBROUTINE ref2896 ! KHcc [1]
    IMPLICIT NONE

    ref = "2896"
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 30., 40., 50., 60. /) + CtoK
    CALL CalcH("1,1,1-trichloroethane", "71-55-6", (/ 0.750, 1.147, 1.467, 1.948 /)) ! TCA
    CALL CalcH("trichloroethene",       "79-01-6", (/ 0.427, 0.693, 0.922, 1.273 /)) ! TCE
    CALL CalcH("methylbenzene",        "108-88-3", (/ 0.251, 0.420, 0.558, 0.774 /)) ! toluene
    CALL CalcH("tetrachloroethene",    "127-18-4", (/ 0.800, 1.303, 1.773, 2.491 /)) ! PCE
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = KHcc_TO_HcpSI(KHcc,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2896

  !---------------------------------------------------------------------------

  SUBROUTINE ref2897 ! KHcc [1]
    IMPLICIT NONE

    ref = "2897"
    type = "M"
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 1.8, 21.6, 40.0, 50.0, 60.0, 70.0 /) + CtoK
    CALL CalcH("tetrachloroethene",        "127-18-4", (/ 0.22, 0.64, 1.33, 1.77, 2.52, 4.16 /)) ! C2Cl4
    CALL CalcH("trichloroethene",           "79-01-6", (/ 0.14, 0.35, 0.74, 1.00, 1.31, 2.01 /)) ! C2HCl3
    CALL CalcH("($Z$)-1,2-dichloroethene", "156-59-2", (/ 0.09, 0.14, 0.29, 0.37, 0.48, 0.67 /)) ! CHClCHCl
    CALL CalcH("($E$)-1,2-dichloroethene", "156-60-5", (/ 0.16, 0.34, 0.69, 0.89, 1.14, 1.67 /)) ! CHClCHCl
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = KHcc_TO_HcpSI(KHcc,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2897

  !---------------------------------------------------------------------------

  SUBROUTINE ref2899 ! KHcc [1]
    IMPLICIT NONE

    ref = "2899"
    type = "M"

    CALL CalcH("1,1,2-trichloroethane", "79-00-5", 0.035) ! 1,1,2-TCE
    CALL CalcH("1,1-dichloroethane",    "75-34-3", 0.198) ! 1,1-DCE
    CALL CalcH("1,2-dichloropropane",   "78-87-5", 0.095) ! 1,2-DCP
    CALL CalcH("naphthalene",           "91-20-3", 0.010) ! naphthalene
    CALL CalcH("1,4-dimethylbenzene",  "106-42-3", 0.197) ! p-xylene
    CALL CalcH("methylbenzene",        "108-88-3", 0.193) ! toluene

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

  END SUBROUTINE ref2899

  !---------------------------------------------------------------------------

  SUBROUTINE ref2900 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2900"

    CALL CalcH("acenaphthene",                  "83-32-9", 0.241)         ! C{12}H{10}
    CALL CalcH("benzene",                       "71-43-2", 5.55,   5.48)  ! C6H6
    CALL CalcH("tetrachloromethane",            "56-23-5", 30.2,   28.6)  ! CCl4 carbontetrachloride
    CALL CalcH("chlorobenzene",                "108-90-7", 3.93,   3.70)  ! C6H5Cl
    CALL CalcH("1,2,4-trichlorobenzene",       "120-82-1", 1.42,   2.32)  ! C6H3Cl3
    CALL CalcH("hexachlorobenzene",            "118-74-1", 1.70)          ! C6Cl6
    CALL CalcH("1,2-dichloroethane",           "107-06-2", 1.10,   1.35)  ! CH2ClCH2Cl
    CALL CalcH("1,1,1-trichloroethane",         "71-55-6", 4.92,   4.08)  ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("hexachloroethane",              "67-72-1", 9.85)          ! C2Cl6
    CALL CalcH("1,1-dichloroethane",            "75-34-3", 5.45,   5.54)  ! CHCl2CH3
    CALL CalcH("trichloromethane",              "67-66-3", 3.39,   3.23)  ! CHCl3 chloroform
    CALL CalcH("1,2-dichlorobenzene",           "95-50-1", 1.94,   2.00)  ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",          "541-73-1", 2.63,   2.96)  ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",          "106-46-7", 2.72)          ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,1-dichloroethene",            "75-35-4", 15.0,   15.1)  ! CH2CCl2
    CALL CalcH("($E$)-1,2-dichloroethene",     "156-60-5", 5.32,   4.05)  ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("1,2-dichloropropane",           "78-87-5", 2.82,   2.75)  ! C3H6Cl2
    CALL CalcH("1,3-dichloropropene",          "542-75-6", 3.55,   1.35)  ! C3H4Cl2
    CALL CalcH("ethylbenzene",                 "100-41-4", 6.44,   6.44)  ! C6H5C2H5
    CALL CalcH("dichloromethane",               "75-09-2", 3.19,   3.04)  ! CH2Cl2 methylene chloride
    CALL CalcH("tribromomethane",               "75-25-2", 0.532,  0.595) ! CHBr3 bromoform
    CALL CalcH("bromodichloromethane",          "75-27-4", 2.12)          ! CHCl2Br
    CALL CalcH("trichlorofluoromethane",        "75-69-4", 58.3,   104.)  ! CFCl3 R11
    CALL CalcH("dibromochloromethane",         "124-48-1", 0.783)         ! CHClBr2
    CALL CalcH("hexachlorobutadiene",           "87-68-3", 10.3,   25.7)  ! CCl2CClCClCCl2
    CALL CalcH("hexachlorocyclopentadiene",     "77-47-4", 16.4,   36.2)  ! C5Cl6
    CALL CalcH("nitrobenzene",                  "98-95-3", 0.024,  0.023) ! C6H5NO2
    CALL CalcH("2-methyl-4,6-dinitrophenol",   "534-52-1", 0.0014)        ! C7H6N2O5 4,6-dinitro-$o$-cresol
    CALL CalcH("hydroxybenzene",               "108-95-2", 0.0013)        ! C6H5OH phenol
    CALL CalcH("acenaphthylene",               "208-96-8", 0.114)         ! C{12}H8
    CALL CalcH("2,3-benzindene",                "86-73-7", 0.117)         ! C{13}H{10} fluorene
    CALL CalcH("tetrachloroethene",            "127-18-4", 28.7,   28.5)  ! C2Cl4 tetrachloroethylene
    CALL CalcH("methylbenzene",                "108-88-3", 5.93,   6.44)  ! C6H5CH3 toluene
    CALL CalcH("trichloroethene",               "79-01-6", 11.7,   11.7)  ! C2HCl3 trichloroethylene
    CALL CalcH("aldrin",                       "309-00-2", 0.496)         ! C{12}H8Cl6
    CALL CalcH("dieldrin",                      "60-57-1", 0.058)         ! C{12}H8OCl6
    CALL CalcH("chlordane",                     "57-74-9", 0.048)         ! C{10}H6Cl8
    CALL CalcH("heptachlor",                    "76-44-8", 1.48)          ! C{10}H5Cl7
    CALL CalcH("heptachlorepoxide",           "1024-57-3", 0.032)         ! C{10}H5Cl7O
    ! aroclor is a mixture, not a pure substance:
    !CALL CalcH("aroclor1254",               "11097-69-1", 8.37)          ! C{12}HxCl{(10-x)}
    !CALL CalcH("toxaphene",                  "8001-35-2", 4.89)          ! mixture, not pure species

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H_M, H_V)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H_M
      REAL, OPTIONAL,   INTENT(IN) :: H_V
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      Hominus = 1. / (atm*1E-3*H_M)
      CALL Output(Hominus)
      IF (PRESENT(H_V)) THEN
        type = "V"
        Hominus = 1. / (atm*1E-3*H_V)
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2900

  !---------------------------------------------------------------------------

  SUBROUTINE ref2901 ! KHcc [1]
    IMPLICIT NONE

    ref = "2901"
    type = "M"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 19.9, 29.75 /) + CtoK
    CALL CalcH("trichloroethene",     "79-01-6", (/ 0.375, 0.453 /), 1768.) ! TCE, trichloroethylene
    CALL CalcH("1,1-dichloroethane",  "75-34-3", (/ 0.220, 0.268 /), 1851.) ! 1,1-DCA, 1,1-dichloroethane
    CALL CalcH("methylbenzene",      "108-88-3", (/ 0.189, 0.375 /), 8100.) ! toluene
    CALL CalcH("trichloromethane",    "67-66-3", (/ 0.153, 0.185 /), 1863.) ! chloroform
    CALL CalcH("1,2-dichloroethane", "107-06-2", (/ 0.059, 0.080 /), 4384.) ! 1,2-DCA, 1.2-dichloroethane
    CALL CalcH("diethyl ether",       "60-29-7", (/ 0.047, 0.070 /), 4968.) ! diethyl ether
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc, B1)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHcc
      REAL,               INTENT(IN) :: B1
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = KHcc_TO_HcpSI(KHcc,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL MakeNote(TRIM(ref), "The temperature dependence is recalculated" &
        // " using the data in Table 4 of " // TRIM(citet()) // &
        " and not taken from their Table 5.")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2901

  !---------------------------------------------------------------------------

  SUBROUTINE ref2902 ! KHcc [1]
    IMPLICIT NONE

    ref = "2902"
    type = "M"

    CALL CalcH("trichloroethene",  "79-01-6", 0.397) ! TCE
    CALL CalcH("methylbenzene",   "108-88-3", 0.261) ! C6H5CH3 toluene

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

  END SUBROUTINE ref2902

  !---------------------------------------------------------------------------

  SUBROUTINE ref2903 ! KHcc [1]
    IMPLICIT NONE

    ref = "2903"
    type = "M"

    CALL CalcH("trichloromethane",   "67-66-3", 0.125) ! CHCl3 chloroform
    CALL CalcH("tetrachloromethane", "56-23-5", 0.975) ! CCl4 carbontetrachloride
    CALL CalcH("hexachloroethane",   "67-72-1", 0.119) ! C2Cl6

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TO_HcpSI(KHcc,20.+CtoK)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2903

  !---------------------------------------------------------------------------

  SUBROUTINE ref2904 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2904"
    type = "M"

    CALL CalcH("1,1-dichloroethene",                   "75-35-4", 0.586,   1319.) ! CH2CCl2
    CALL CalcH("dichloromethane",                      "75-09-2", 3.923,   2940.) ! CH2Cl2 methylene chloride
    CALL CalcH("($E$)-1,2-dichloroethene",            "156-60-5", 1.620,   1889.) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("1,1-dichloroethane",                   "75-34-3", 2.487,   2296.) ! CHCl2CH3
    CALL CalcH("2,2-dichloropropane",                 "594-20-7", -3.157,   333.) ! C3H6Cl2
    CALL CalcH("($Z$)-1,2-dichloroethene",            "156-59-2", 3.635,   2731.) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("trichloromethane",                     "67-66-3", 4.695,   3078.) ! CHCl3 chloroform
    CALL CalcH("bromochloromethane",                   "74-97-5", 7.648,   4270.) ! CH2BrCl
    CALL CalcH("1,1,1-trichloroethane",                "71-55-6", 1.109,   1590.) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1-dichloropropene",                 "563-58-6", 1.372,   1603.) ! C3H4Cl2
    CALL CalcH("tetrachloromethane",                   "56-23-5", 1.702,   1637.) ! CCl4 carbontetrachloride
    CALL CalcH("1,2-dichloroethane",                  "107-06-2", 4.855,   3367.) ! CH2ClCH2Cl
    CALL CalcH("benzene",                              "71-43-2", 2.714,   2363.) ! C6H6
    CALL CalcH("trichloroethene",                      "79-01-6", 1.611,   1893.) ! C2HCl3 trichloroethylene
    CALL CalcH("1,2-dichloropropane",                  "78-87-5", 5.273,   3382.) ! C3H6Cl2
    CALL CalcH("bromodichloromethane",                 "75-27-4", 7.355,   4073.) ! CHCl2Br
    CALL CalcH("dibromomethane",                       "74-95-3", 8.477,   4651.) ! CH2Br2
    CALL CalcH("{cis}-1,3-dichloropropene",         "10061-01-5", 6.989,   4009.) ! C3H4Cl2
    CALL CalcH("methylbenzene",                       "108-88-3", 3.353,   2540.) ! C6H5CH3 toluene
    CALL CalcH("{trans}-1,3-dichloropropene",       "10061-02-6", 8.995,   4747.) ! C3H4Cl2
    CALL CalcH("1,1,2-trichloroethane",                "79-00-5", 9.121,   4815.) ! CHCl2CH2Cl
    CALL CalcH("1,3-dichloropropane",                 "142-28-9", 8.678,   4674.) ! C3H6Cl2
    CALL CalcH("tetrachloroethene",                   "127-18-4", 1.989,   1859.) ! C2Cl4 tetrachloroethylene
    CALL CalcH("dibromochloromethane",                "124-48-1", 9.185,   4796.) ! CHClBr2
    CALL CalcH("1,2-dibromoethane",                   "106-93-4", 9.950,   5208.) ! C2H4Br2 ethylene dibromide
    CALL CalcH("chlorobenzene",                       "108-90-7", 5.273,   3275.) ! C6H5Cl
    CALL CalcH("1,1,1,2-tetrachloroethane",           "630-20-6", 6.810,   3841.) ! CCl3CH2Cl
    CALL CalcH("ethylbenzene",                        "100-41-4", 3.371,   2492.) ! C6H5C2H5
    CALL CalcH("1,3-dimethylbenzene",                 "108-38-3", 3.596,   2572.) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",                 "106-42-3", 3.596,   2572.) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("1,2-dimethylbenzene",                  "95-47-6", 5.209,   3129.) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("ethenylbenzene",                      "100-42-5", 6.912,   3838.) ! C8H8 styrene
    CALL CalcH("tribromomethane",                      "75-25-2", 6.808,   4195.) ! CHBr3 bromoform
    CALL CalcH("(2-propyl)-benzene",                   "98-82-8", 2.742,   2205.) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("1,1,2,2-tetrachloroethane",            "79-34-5", 13.95,   6470.) ! CHCl2CHCl2
    CALL CalcH("1,2,3-trichloropropane",               "96-18-4", 8.739,   4976.) ! C3H5Cl3
    CALL CalcH("propylbenzene",                       "103-65-1", 3.141,   2340.) ! C6H5C3H7
    CALL CalcH("bromobenzene",                        "108-86-1", 6.972,   3928.) ! C6H5Br
    CALL CalcH("1,3,5-trimethylbenzene",              "108-67-8", 4.128,   2734.) ! C6H3(CH3)3 mesitylene
    CALL CalcH("1-chloro-2-methylbenzene",             "95-49-8", 4.875,   3097.) ! C7H7Cl $o$-chlorotoluene
    CALL CalcH("1-chloro-4-methylbenzene",            "106-43-4", 6.319,   3579.) ! C7H7Cl $p$-chlorotoluene
    CALL CalcH("(1,1-dimethylethyl)-benzene",          "98-06-6", 2.351,   2060.) ! C6H5C4H9 {tert}-butylbenzene
    CALL CalcH("1,2,4-trimethylbenzene",               "95-63-6", 5.724,   3331.) ! C6H3(CH3)3
    CALL CalcH("(1-methylpropyl)-benzene",            "135-98-8", 2.403,   2006.) ! C6H5C4H9 {sec}-butylbenzene
    CALL CalcH("1-methyl-4-(1-methylethyl)-benzene",   "99-87-6", 3.226,   2348.) ! C{10}H{14} $p$-cymene; $p$-isopropyltoluene
    CALL CalcH("1,3-dichlorobenzene",                 "541-73-1", 7.665,   4057.) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",                 "106-46-7", 8.815,   4465.) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("butylbenzene",                        "104-51-8", 3.444,   2376.) ! C6H5C4H9
    CALL CalcH("1,2-dichlorobenzene",                  "95-50-1", 9.247,   4678.) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,2-dibromo-3-chloropropane",          "96-12-8", 23.05,  10100.) ! C3H5Br2Cl
    CALL CalcH("1,2,4-trichlorobenzene",              "120-82-1", 10.94,   5199.) ! C6H3Cl3
    CALL CalcH("hexachlorobutadiene",                  "87-68-3", 3.116,   2201.) ! CCl2CClCClCCl2
    CALL CalcH("naphthalene",                          "91-20-3", 8.705,   4823.) ! C{10}H8
    CALL CalcH("1,2,3-trichlorobenzene",               "87-61-6", 16.19,   7011.) ! C6H3Cl3

    ! activate check_regression to explain the typo for
    ! 1,1,2,2-tetrachloroethane which must be 13.95, not 1.395
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/25.4,35.,45.,55.,60./) + CtoK
    CALL check_regression( "98-82-8", (/ 0.407, 0.467, 0.552, 0.698, 0.781 /) )
    CALL check_regression( "79-34-5", (/ 0.014, 0.044, 0.079, 0.109, 0.132 /) )
    CALL check_regression( "96-18-4", (/ 0.015, 0.023, 0.040, 0.057, 0.077 /) )
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE check_regression (casrn_, Hc)
      CHARACTER(LEN=*), INTENT(IN)   :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: Hc
      casrn = casrn_ ! make value global, so HTdep will find it
      Harray = KHcc_TO_HcpSI(Hc,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      !PRINT *, LOG(1./(Hominus*atm))+mindHR/T0, mindHR
    END SUBROUTINE check_regression

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: A, B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*EXP(A-B/T0))
      mindHR = B + T0 ! see ref958, eqn (34) why T0 is added to mindHR
      IF (casrn_=="79-34-5") CALL MakeNote(TRIM(ref),"The value for $A$ in " // &
      "the table of " // TRIM(citet()) // " is incorrect. Recalculating the " // &
      "regression, it can be seen that it should be 13.95 and not 1.395.")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2904

  !---------------------------------------------------------------------------

  SUBROUTINE ref2905 ! miscellaneous definitions
    IMPLICIT NONE

    ref = "2905"

    ! Tab. 2, KHcc [1]:
    CALL CalcH_Tab2("hexane",  "110-54-3", 1.7391) ! C6H{14}
    CALL CalcH_Tab2("heptane", "142-82-5", 1.8692) ! C7H{16}
    CALL CalcH_Tab2("octane",  "111-65-9", 2.0815) ! C8H{18}
    CALL CalcH_Tab2("nonane",  "111-84-2", 2.3281) ! C9H{20}

    ! Tab. 3+4, KHpc [kPa*m3/mol]:
    CALL CalcH_Tab34("benzene",                   "71-43-2", 0.5537, (/ 0.4882, 0.6360, 1.0315 /)) ! benzene
    CALL CalcH_Tab34("methylbenzene",            "108-88-3", 0.6731, (/ 0.5066, 0.7444, 1.2260 /)) ! toluene
    CALL CalcH_Tab34("1,4-dimethylbenzene",      "106-42-3", 0.6875, (/ 0.7197, 0.9516, 1.4925 /)) ! p-xylene
    CALL CalcH_Tab34("chlorobenzene",            "108-90-7", 0.3588, (/ 0.4093, 0.5500, 0.6043 /)) ! chlorobenzene
    CALL CalcH_Tab34("1,2-dichlorobenzene",       "95-50-1", 0.1164, (/ 0.1201, 0.2125, 0.3199 /)) ! o-dichlorobenzene
    CALL CalcH_Tab34("dichloromethane",           "75-09-2", 0.2531, (/ 0.1507, 0.2405, 0.3298 /)) ! dichloromethane
    CALL CalcH_Tab34("trichloromethane",          "67-66-3", 0.4061, (/ 0.2674, 0.3817, 0.5700 /)) ! trichloromethane
    CALL CalcH_Tab34("tetrachloromethane",        "56-23-5", 2.3399, (/ 1.8411, 2.6375, 3.3224 /)) ! tetrachloromethane
    CALL CalcH_Tab34("($Z$)-1,2-dichloroethene", "156-59-2", 0.7472, (/ 0.5561, 0.7612, 1.1167 /)) ! cis-dichloroethylene
    CALL CalcH_Tab34("($E$)-1,2-dichloroethene", "156-60-5", 0.6832, (/ 0.2473, 0.6691, 0.9998 /)) ! trans-dichloroethylene
    CALL CalcH_Tab34("trichloroethene",           "79-01-6", 1.0113, (/ 0.7375, 1.1440, 1.6739 /)) ! trichloroethylene
    CALL CalcH_Tab34("tetrachloroethene",        "127-18-4", 2.9106, (/ 1.0335, 2.0670, 2.6527 /)) ! tetrachloroethylene

    ! Tab. 5 [kPa*m3/mol]:
    ! MTBE:
    CALL CalcH_Tab5("methyl {tert}-butyl ether", "1634-04-4", 0.0615, "V") ! CH3OC(CH3)3 MTBE
    CALL CalcH_Tab5("methyl {tert}-butyl ether", "1634-04-4", 0.0429, "M") ! CH3OC(CH3)3 MTBE
    ! TAME:
    CALL CalcH_Tab5("2-methoxy-2-methylbutane",   "994-05-8", 0.1228, "V") ! TAME, tert-amyl methyl ether
    CALL CalcH_Tab5("2-methoxy-2-methylbutane",   "994-05-8", 0.1428, "M") ! TAME, tert-amyl methyl ether

  CONTAINS

    SUBROUTINE CalcH_Tab2 (chem_, casrn_, log10KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: log10KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      Hominus = KHcc_TIMES_HcpSI_atT0/(10.**log10KHcc)
      CALL MakeNote(TRIM(ref), &
      "Apparently, the values in Table 2 of " // TRIM(citet()) // &
      " show $\log_{10}(K_{\rm aw})$ and not $K_{\rm aw}$ as " // &
      "their figure caption states.")
      CALL Output(Hominus)
    END SUBROUTINE CalcH_Tab2

    SUBROUTINE CalcH_Tab34 (chem_, casrn_, KHpc_cal, KHpc_meas)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: KHpc_cal
      REAL, DIMENSION(:), INTENT(IN) :: KHpc_meas
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! calculated value from Tab. 3:
      type = "V"
      CALL Output(1E-3/KHpc_cal)
      ! measured values from Tab. 4:
      type = "M"
      ndata = 3
      ALLOCATE(temp(ndata))
      temp = (/ 15., 25., 35. /) + CtoK
      CALL HTdep(temp, 1E-3/KHpc_meas, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp)
    END SUBROUTINE CalcH_Tab34

    SUBROUTINE CalcH_Tab5 (chem_, casrn_, KHpc, type_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpc
      CHARACTER,        INTENT(IN) :: type_
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      CALL Output(1E-3/KHpc)
    END SUBROUTINE CalcH_Tab5

  END SUBROUTINE ref2905

  !---------------------------------------------------------------------------

  SUBROUTINE ref2906 ! KHpx [Torr]
    IMPLICIT NONE

    ref = "2906"
    type = "M"

    CALL CalcH("benzene",        "71-43-2", 2.44E5) ! C6H6
    CALL CalcH("methylbenzene", "108-88-3", 2.52E5) ! C6H5CH3 toluene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, k)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: k
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = cH2O / (mmHg * k)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2906

  !---------------------------------------------------------------------------

  ! ref2907

  ! Not used, the paper contains no original data, only references to
  ! other data collections.

  !---------------------------------------------------------------------------

  SUBROUTINE ref2909 ! Hcc [1]
    IMPLICIT NONE

    ref = "2909"
    type = "M"

    CALL CalcH("pentane", "109-66-0", &
      (/ 15.1, 15.1, 20., 20., 25., 25., 30., 35. /), &
      (/ 0.0307, 0.0307, 0.0242, 0.0250, 0.0197, 0.0194, 0.0175, 0.0146 /)) ! C5H{12}
    CALL CalcH("hexane",  "110-54-3", &
      (/ 14.52, 14.52, 20.05, 20.05, 25., 25., 30.2, 30.2, 34.9, 34.9 /), &
      (/ 0.0231, 0.0232, 0.0180, 0.0179, 0.0142, 0.0144, 0.0117, 0.0119, 0.0097, &
      0.0100 /)) ! C6H{14}
    CALL CalcH("heptane", "142-82-5", &
      (/ 15.3, 15.3, 15.3, 20.05, 20.05, 25.04, 29.8, 29.8, 35.05, 34.83, 34.83 /), &
      (/ 0.0171, 0.0176, 0.0176, 0.0128, 0.0130, 0.0099, 0.00785, 0.00795, &
      0.00658, 0.00631, 0.00674 /)) ! C7H{16}
    CALL CalcH("octane",  "111-65-9", &
      (/ 14.8, 14.8, 20.05, 20.05, 25.1, 25.1, 30.1, 34.92 /), &
      (/ 0.0140, 0.0136, 0.0094, 0.0095, 0.00694, 0.00701, 0.00557, 0.00436 /)) ! C8H{18}
    CALL CalcH("nonane",  "111-84-2", &
      (/ 14.8, 14.8, 20.05, 20.05 /), &
      (/ 0.0098, 0.0108, 0.0066, 0.0067 /)) ! C9H{20}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, Hcc)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, Hcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(Hcc)
      ALLOCATE(Harray(ndata))
      Harray = Hcc_TO_HcpSI(Hcc,temp_+CtoK)
      CALL HTdep(temp_+CtoK, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2909

  !---------------------------------------------------------------------------

  SUBROUTINE ref2910 ! KHpx [GPa]
    IMPLICIT NONE

    ref = "2910"
    type = "M"

    CALL CalcH("pentane", "109-66-0", &
      (/ 273.2, 283.2, 298.2, 313.2, 343.2, 373.2 /), &
      (/ 1.8, 3.5, 6.1, 8.9, 14.9, 19.3 /) ) ! C5H{12}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, KHpx)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, KHpx
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(KHpx)
      ALLOCATE(Harray(ndata))
      Harray = cH2O / (1E9*KHpx)
      CALL HTdep(temp_, Harray, Hominus, mindHR)
      CALL MakeNote(TRIM(ref), &
        TRIM(citet())//" also contains high-temperature data. However, only " // &
        "data up to 373.2\,K were used here to calculate the temperature dependence.")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2910

  !---------------------------------------------------------------------------

  SUBROUTINE ref2911 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2911"
    type = "M"

    CALL CalcH("benzene",      "71-43-2", 132.977, -9463.47, -1.50638e-5, -16.9273) ! C6H6
    CALL CalcH("cyclohexane", "110-82-7", 244.272, -13539.9, -2.03342e-6, -33.6554) ! C6H{12}
    CALL CalcH("hexane",      "110-54-3", 413.539, -21622.5, -1.26465e-6, -58.2501) ! C6H{14}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, C, D
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! T-dep with 4 parameters:
      Hominus    = cH2O / (EXP(A + B/T0 + C*T0**2 + D*LOG(T0)) * 1.E6)
      ! mindHR = analytical solution of d ln(Hominus) / d (1/T) at 298.15 K:
      mindHR = - (B - 2*C*T0**3 - D*T0)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2911

  !---------------------------------------------------------------------------

  SUBROUTINE ref2912 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2912"
    type = "M"

    CALL CalcH("ethylbenzene",      "100-41-4", 146.152, -10780.8, -16.9701e-6, -18.3797) ! C6H5C2H5
    CALL CalcH("ethylcyclohexane", "1678-91-7", 325.570, -18496.5, -10.9666e-6, -44.7690) ! C8H{16}
    CALL CalcH("octane",            "111-65-9", 357.733, -19363.1, -9.04865e-6, -49.5296) ! C8H{18}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B, C, D
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! T-dep with 4 parameters:
      Hominus    = cH2O / (EXP(A + B/T0 + C*T0**2 + D*LOG(T0)) * 1.E6)
      ! mindHR = analytical solution of d ln(Hominus) / d (1/T) at 298.15 K:
      mindHR = - (B - 2*C*T0**3 - D*T0)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2912

  !---------------------------------------------------------------------------

  SUBROUTINE ref2913 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2913"
    type = "M"
    chem = "methyl hydroperoxide" ; casrn = "3031-73-0" ! CH3OOH methylperoxide
    mindHR = 4386.
    Hominus = Hcp_TO_HcpSI * EXP(mindHR/T0-9.19)
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref2913

  !---------------------------------------------------------------------------

  SUBROUTINE ref2914 ! KHpc [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2914"
    type = "M"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 4., 25. /) + CtoK
    CALL CalcH("2,3-benzindene",  "86-73-7", (/ 1.92, 12.7 /)) ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",    "85-01-8", (/ 0.92,  6.3 /)) ! C{14}H{10}
    CALL CalcH("anthracene",     "120-12-7", (/ 1.24,  6.5 /)) ! C{14}H{10}
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KHpc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = KHpcSI_TIMES_HcpSI/KHpc
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2914

  !---------------------------------------------------------------------------

  SUBROUTINE ref2915 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2915"

    ! Tab. 1 Type "V":
    CALL CalcH("Dibenzo-p-dioxin",                "262-12-4", 2.983, "V")
    CALL CalcH("1-Monochloro dibenzo-p-dioxin", "39227-53-7", 3.406, "V")
    CALL CalcH("2,3-Dichloro dibenzo-p-dioxin", "29446-15-9", 4.174, "V")
    CALL CalcH("Dimethyl phthalate",              "131-11-3", 6.310, "V")
    CALL CalcH("Diallyl phthalate",               "131-17-9", 6.544, "V")
    CALL CalcH("Di-n-butyl phthalate",             "84-74-2", 6.431, "V")
    CALL CalcH("Butyl benzyl phthalate",           "85-68-7", 6.293, "V")
    CALL CalcH("Di (2-ethyl hexyl) phthalate",    "117-81-7", 6.140, "V")
    CALL CalcH("Diisodecyl phthalate",          "26761-40-0", 6.583, "V")
    CALL CalcH("Diundecyl phthalate",            "3648-20-2", 6.523, "V")

    ! Tab. 2,3, Type "Q":
    CALL CalcH("Dibenzo-p-dioxin",                                "262-12-4", 3.430, "Q")
    CALL CalcH("1-Monochloro dibenzo-p-dioxin",                 "39227-53-7", 3.836, "Q")
    CALL CalcH("2-Monochloro dibenzo-p-dioxin",                 "39227-54-8", 3.999, "Q")
    CALL CalcH("2,3-Dichloro dibenzo-p-dioxin",                 "29446-15-9", 4.402, "Q")
    CALL CalcH("2,7-Dichloro dibenzo-p-dioxin",                 "33857-26-0", 4.021, "Q")
    CALL CalcH("2,8-Dichloro dibenzo-p-dioxin",                 "38964-22-6", 4.231, "Q")
    CALL CalcH("1,2,4-Trichloro dibenzo-p-dioxin",              "39227-58-2", 4.484, "Q")
    CALL CalcH("1,2,3,4-Tetrachloro dibenzo-p-dioxin",          "30746-58-8", 4.808, "Q")
    CALL CalcH("1,2,3,7-Tetrachloro dibenzo-p-dioxin",          "67028-18-6", 4.644, "Q")
    CALL CalcH("1,3,6,8-Tetrachloro dibenzo-p-dioxin",          "33423-92-6", 4.475, "Q")
    CALL CalcH("2,3,7,8-Tetrachloro dibenzo-p-dioxin",           "1746-01-6", 4.523, "Q")
    CALL CalcH("1,2,3,4,7-Pentachloro dibenzo-p-dioxin",        "39227-61-7", 4.853, "Q")
    CALL CalcH("1,2,3,7,8-Pentachloro dibenzo-p-dioxin",        "40321-76-4", 4.809, "Q")
    CALL CalcH("1,2,3,4,7,8-Hexachloro dibenzo-p-dioxin",       "39227-28-6", 4.891, "Q")
    CALL CalcH("1,2,3,6,7,8-Hexachloro dibenzo-p-dioxin",       "57653-85-7", 4.876, "Q")
    CALL CalcH("1,2,3,7,8,9-Hexachloro dibenzo-p-dioxin",       "19408-74-3", 5.063, "Q")
    CALL CalcH("1,2,3,4,6,7,8-Heptachloro dibenzo-p-dioxin",    "35822-46-9", 5.164, "Q")
    CALL CalcH("Octachloro dibenzo-p-dioxin",                    "3268-87-9", 5.237, "Q")
    CALL CalcH("Dibenzo-p-furan",                                 "132-64-9", 3.922, "Q")
    CALL CalcH("2,8-Dichloro dibenzo-p-furan",                   "5409-83-6", 4.413, "Q")
    CALL CalcH("1,2,7,8-Tetrachloro dibenzo-p-furan",           "58802-20-3", 5.044, "Q")
    CALL CalcH("2,3,7,8-Tetrachloro dibenzo-p-furan",           "51207-31-9", 4.865, "Q")
    CALL CalcH("2,3,4,7,8-Pentachloro dibenzo-p-furan",         "57117-31-4", 5.201, "Q")
    CALL CalcH("1,2,3,8,9-Pentachloro dibenzo-p-furan",         "83704-54-5", 5.298, "Q")
    CALL CalcH("1,2,3,4,7,8-Hexachloro dibenzo-p-furan",        "70648-26-9", 5.303, "Q")
    CALL CalcH("1,2,3,4,8,9-Hexachloro dibenzo-p-furan",        "92341-07-6", 5.436, "Q")
    CALL CalcH("1,2,3,6,7,8-Hexachloro dibenzo-p-furan",        "57117-44-9", 5.341, "Q")
    CALL CalcH("1,2,3,7,8,9-Hexachloro dibenzo-p-furan",        "72918-21-9", 5.422, "Q")
    CALL CalcH("2,3,4,6,7,8-Hexachloro dibenzo-p-furan",        "60851-34-5", 5.497, "Q")
    CALL CalcH("1,2,3,4,6,7,8-Heptachloro dibenzo-p-furan",     "67562-39-4", 5.598, "Q")
    CALL CalcH("1,2,3,4,7,8,9-Heptachloro dibenzo-p-furan",     "55673-89-7", 5.508, "Q")
    CALL CalcH("Octachloro dibenzo-p-furan",                    "39001-02-0", 5.699, "Q")
    CALL CalcH("Dimethyl phthalate",                              "131-11-3", 5.986, "Q")
    CALL CalcH("Diethyl phthalate",                                "84-66-2", 5.766, "Q")
    CALL CalcH("Diallyl phthalate",                               "131-17-9", 6.239, "Q")
    CALL CalcH("Dipropyl phthalate",                              "131-16-8", 6.383, "Q")
    CALL CalcH("Di-n-butyl phthalate",                             "84-74-2", 6.574, "Q")
    CALL CalcH("Diisobutyl phthalate",                             "84-69-5", 6.501, "Q")
    CALL CalcH("Butyl benzyl phthalate",                           "85-68-7", 6.509, "Q")
    CALL CalcH("Dihexyl phthalate",                                "84-75-3", 6.198, "Q")
    CALL CalcH("Di-n-octyl phthalate",                            "117-84-0", 5.809, "Q")
    CALL CalcH("Butyl 2-ethyl hexyl phthalate",                    "85-69-8", 6.843, "Q")
    CALL CalcH("Decyl hexyl phthalate",                         "25724-58-7", 7.221, "Q")
    CALL CalcH("Di (2-ethyl hexyl) phthalate",                    "117-81-7", 6.398, "Q")
    CALL CalcH("Diisooctyl phthalate",                          "27554-26-3", 7.180, "Q")
    CALL CalcH("Diisononyl phthalate",                          "28553-12-0", 6.530, "Q")
    CALL CalcH("Diisodecyl phthalate",                          "26761-40-0", 6.390, "Q")
    !CALL CalcH("Di-C9-11-branched alkyl phthalate",            "68515-49-1", 6.279, "Q") ! mixture?
    CALL CalcH("Diundecyl phthalate",                            "3648-20-2", 6.148, "Q")
    CALL CalcH("Diheptyl phthalate",                             "3648-21-3", 4.957, "Q")
    CALL CalcH("Dinonyl phthalate",                                "84-76-4", 6.482, "Q")
    !CALL CalcH("Heptyl nonyl phthalate",                      "111381-89-6", 7.150, "Q") ! mixture?
    !CALL CalcH("Heptyl undecyl phthalate",                    "111381-90-9", 5.789, "Q") ! mixture?
    !CALL CalcH("Nonyl undecyl phthalate",                     "111381-91-0", 5.823, "Q") ! mixture?
    CALL CalcH("Ditridecyl phthalate",                            "119-06-2", 6.905, "Q")
    !CALL CalcH("Di-C11-14-branched alkyl phthalate",           "68515-47-9", 7.099, "Q") ! mixture?

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, minlogH, type_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: minlogH
      CHARACTER,        INTENT(IN) :: type_
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      CALL Output(1. / (atm*10.**(-minlogH)))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2915

  !---------------------------------------------------------------------------

  SUBROUTINE ref2917 ! Hcc [1]
    IMPLICIT NONE

    ref = "2917"
    type = "M"

    CALL CalcH("benzene",               "71-43-2", 2.78 ) ! C6H6
    CALL CalcH("methylbenzene",        "108-88-3", 2.23 ) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",         "100-41-4", 1.69 ) ! C6H5C2H5
    CALL CalcH("propylbenzene",        "103-65-1", 1.30 ) ! C6H5C3H7
    CALL CalcH("1,2-dimethylbenzene",   "95-47-6", 2.63 ) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",  "108-38-3", 1.66 ) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",  "106-42-3", 1.57 ) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("(2-propyl)-benzene",    "98-82-8", 1.44 ) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("ethenylbenzene",       "100-42-5", 4.68 ) ! C8H8 styrene
    CALL CalcH("2-propenylbenzene",    "300-57-2", 3.55 ) ! C9H{10} Allylbenzene
    CALL CalcH("propanone",             "67-64-1", 395. ) ! CH3COCH3 acetone
    CALL CalcH("butanone",              "78-93-3", 254. ) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("2-pentanone",          "107-87-9", 166. ) ! C3H7COCH3
    CALL CalcH("3-pentanone",           "96-22-0", 181. ) ! C2H5COC2H5
    CALL CalcH("2-hexanone",           "591-78-6", 111. ) ! C6H{12}O
    CALL CalcH("4-methyl-2-pentanone", "108-10-1", 79.  ) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("2-heptanone",          "110-43-0", 96.  ) ! C7H{14}O Methyl pentyl ketone

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lambda_A)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: lambda_A
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(lambda_A, 37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2917

  !---------------------------------------------------------------------------

  SUBROUTINE ref2918 ! Hcc [1]
    IMPLICIT NONE

    ref = "2918"
    type = "M"

    CALL CalcH("dichloromethane",            "75-09-2", 7.2  ) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",           "67-66-3", 3.5  ) ! CHCl3 chloroform
    CALL CalcH("tetrachloromethane",         "56-23-5", 0.25 ) ! CCl4 carbontetrachloride
    CALL CalcH("1,1-dichloroethane",         "75-34-3", 2.7  ) ! CHCl2CH3
    CALL CalcH("1,2-dichloroethane",        "107-06-2", 11.3 ) ! CH2ClCH2Cl
    CALL CalcH("1,1,1-trichloroethane",      "71-55-6", 0.93 ) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1,2-trichloroethane",      "79-00-5", 17.1 ) ! CHCl2CH2Cl
    CALL CalcH("1,1,1,2-tetrachloroethane", "630-20-6", 5.5  ) ! CCl3CH2Cl
    CALL CalcH("1,1,2,2-tetrachloroethane",  "79-34-5", 35.7 ) ! CHCl2CHCl2
    CALL CalcH("($Z$)-1,2-dichloroethene",  "156-59-2", 2.9  ) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("($E$)-1,2-dichloroethene",  "156-60-5", 2.1  ) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",            "79-01-6", 1.3  ) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",         "127-18-4", 0.43 ) ! C2Cl4 tetrachloroethylene
    CALL CalcH("1-chloropropane",           "540-54-5", 1.1  ) ! C3H7Cl
    CALL CalcH("1,2-dichloropropane",        "78-87-5", 5.4  ) ! C3H6Cl2
    CALL CalcH("1-chlorobutane",            "109-69-3", 0.86 ) ! C4H9Cl
    CALL CalcH("1-chloropentane",           "543-59-9", 0.70 ) ! C5H{11}Cl
    CALL CalcH("chlorobenzene",             "108-90-7", 4.1  ) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",        "95-50-1", 9.0  ) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",       "541-73-1", 5.5  ) ! C6H4Cl2 $m$-dichlorobenzene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, W)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: W
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(W, 37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2918

  !---------------------------------------------------------------------------

  SUBROUTINE ref2921 ! KHpc [MPa*m3/mol]
    IMPLICIT NONE

    ref = "2921"
    type = "M"

    CALL CalcH("methyl {tert}-butyl ether", "1634-04-4", 3.13E-5 ) ! CH3OC(CH3)3 MTBE
    CALL CalcH("benzene",                     "71-43-2", 2.83E-4 ) ! C6H6
    CALL CalcH("methylbenzene",              "108-88-3", 3.58E-4 ) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",               "100-41-4", 7.04E-4 ) ! C6H5C2H5
    CALL CalcH("1,2-dimethylbenzene",         "95-47-6", 4.58E-4 ) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("nitrobenzene",                "98-95-3", 6.93E-6 ) ! C6H5NO2
    CALL CalcH("trichloroethene",             "79-01-6", 7.54E-4 ) ! C2HCl3 trichloroethylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H1)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H1
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI / (H1 * 1E6)
      CALL Output(Hominus)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2921

  !---------------------------------------------------------------------------

  SUBROUTINE ref2922 ! Hcc [1]
    IMPLICIT NONE

    ref = "2922"
    type = "M"

    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10., 15., 20., 25., 30. /) + CtoK
    CALL CalcH("benzene",              "71-43-2", (/ 7.6, 6.2, 4.8, 4.0, 3.4 /) ) ! C6H6
    CALL CalcH("methylbenzene",       "108-88-3", (/ 8.1, 6.0, 4.6, 3.6, 2.9 /) ) ! C6H5CH3 toluene
    CALL CalcH("1,3-dimethylbenzene", "108-38-3", (/ 9.7, 7.4, 5.9, 4.0, 3.9 /) ) ! C6H4(CH3)2 $m$-xylene
    DEALLOCATE(temp,Harray)

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

  END SUBROUTINE ref2922

  !---------------------------------------------------------------------------

  SUBROUTINE ref2923 ! Hcc [1]
    IMPLICIT NONE

    ref = "2923"
    type = "M"

    CALL CalcH("cyclopentene",             "142-29-0", 1915.,  6.989 ) ! C5H8
    CALL CalcH("cyclohexene",              "110-83-8", 1691.,  5.860 ) ! C6H{10}
    CALL CalcH("2,2-dichloropropane",      "594-20-7", 3637., 11.496 ) ! C3H6Cl2
    CALL CalcH("benzene",                   "71-43-2", 3920., 11.663 ) ! C6H6
    CALL CalcH("methylbenzene",            "108-88-3", 3977., 11.926 ) ! C6H5CH3 toluene
    CALL CalcH("($E$)-1-chloro-2-butene", "4894-61-5", 2707.,  7.045 ) ! C4H7Cl {trans}-1-chloro-2-butene
    CALL CalcH("($Z$)-1-chloro-2-butene", "4628-21-1", 2503.,  7.282 ) ! C4H7Cl {cis}-1-chloro-2-butene
    CALL CalcH("chlorocyclohexane",        "542-18-7", 2982.,  8.069 ) ! C6H{11}Cl

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a, b)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: a, b
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI_atT0 * EXP(a/T0-b)
      mindHR = a + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2923

  !---------------------------------------------------------------------------

  SUBROUTINE ref2924 ! Hcc [1]
    IMPLICIT NONE

    ref = "2924"
    type = "M"

    CALL CalcH("sulfur hexafluoride",     "2551-62-4", 0.0037 ) ! SF6
    CALL CalcH("2,2,4-trimethylpentane",   "540-84-1", 0.0119 ) ! C8H{18} isooctane
    CALL CalcH("heptane",                  "142-82-5", 0.0155 ) ! C7H{16}
    CALL CalcH("hexane",                   "110-54-3", 0.0172 ) ! C6H{14}
    CALL CalcH("butane",                   "106-97-8", 0.0206 ) ! C4H{10}
    CALL CalcH("propane",                   "74-98-6", 0.0250 ) ! C3H8
    CALL CalcH("methane",                   "74-82-8", 0.0302 ) ! CH4
    CALL CalcH("ethane",                    "74-84-0", 0.0331 ) ! C2H6
    CALL CalcH("cyclohexane",              "110-82-7", 0.0871 ) ! C6H{12}
    CALL CalcH("cyclopropane",              "75-19-4", 0.2018 ) ! C3H6
    CALL CalcH("enflurane",              "13838-16-9", 0.6949 ) ! C3H2ClF5O
    CALL CalcH("halothane",                "151-67-7", 0.7270 ) ! C2HBrClF3
    CALL CalcH("1,1,1-trichloroethane",     "71-55-6", 0.8241 ) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("trichloroethene",           "79-01-6", 1.420  ) ! C2HCl3 trichloroethylene
    CALL CalcH("benzene",                   "71-43-2", 5.059  ) ! C6H6
    CALL CalcH("trichloromethane",          "67-66-3", 5.505  ) ! CHCl3 chloroform
    CALL CalcH("diisopropyl ether",        "108-20-3", 7.293  ) ! C3H7OC3H7
    CALL CalcH("dichloromethane",           "75-09-2", 8.705  ) ! CH2Cl2 methylene chloride
    CALL CalcH("diethyl ether",             "60-29-7", 16.36  ) ! C2H5OC2H5
    CALL CalcH("1,2-dichloroethane",       "107-06-2", 19.68  ) ! CH2ClCH2Cl
    CALL CalcH("ethyl ethanoate",          "141-78-6", 111.7  ) ! CH3COOC2H5 ethyl acetate
    CALL CalcH("ethanal",                   "75-07-0", 184.2  ) ! CH3CHO acetaldehyde
    CALL CalcH("propanone",                 "67-64-1", 308.   ) ! CH3COCH3 acetone

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, K)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: K
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(K,37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2924

  !---------------------------------------------------------------------------

  SUBROUTINE ref2925 ! Hcc [1]
    IMPLICIT NONE

    ref = "2925"
    type = "M"

    CALL CalcH("methanol",   "67-56-1", 5200. ) ! CH3OH
    CALL CalcH("ethanol",    "64-17-5", 4500. ) ! C2H5OH
    CALL CalcH("1-propanol", "71-23-8", 3600. ) ! C3H7OH
    CALL CalcH("1-butanol",  "71-36-3", 3100. ) ! C4H9OH

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, distrib_ratio)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: distrib_ratio
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI_atT0 * distrib_ratio
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2925

  !---------------------------------------------------------------------------

  SUBROUTINE ref2926 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "2926"
    type = "M"

    CALL CalcH("methoxybenzene",               "100-66-3", & ! C6H5OCH3 anisole
      (/ 280.95, 293.16, 308.15, 322.90 /), &
      (/ 8.35E2, 1.48E3, 2.95E3, 5.72E3 /) )
    CALL CalcH("1,2-difluorobenzene",          "367-11-3", & ! C6H4F2 $o$-difluorobenzene
      (/ 298.19, 308.12, 317.90, 322.89 /), &
      (/ 4.35E4, 6.65E4, 9.33E4, 1.05E5 /) )
    CALL CalcH("1-bromo-4-methylbenzene",      "106-38-7", & ! BrC6H4CH3 $p$-bromotoluene
      (/ 293.10, 298.16, 308.10, 318.03, 322.91 /), &
      (/ 1.18E4, 1.71E4, 2.76E4, 4.03E4, 5.28E4 /) )
    CALL CalcH("1,2,3-trichlorobenzene",        "87-61-6", & ! C6H3Cl3
      (/ 280.95, 293.06, 308.14, 322.99 /), &
      (/ 3.36E3, 6.84E3, 1.44E4, 2.81E4 /) )
    CALL CalcH("1-methyl-2,4-dichlorobenzene",  "95-73-8", & ! C7H6Cl2 2,4-dichlorotoluene
      (/ 280.94, 293.09, 308.15, 323.10 /), &
      (/ 7.14E3, 1.68E4, 3.25E4, 7.54E4 /) )

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, temp_, KH_kPa)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, KH_kPa
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      ndata = SIZE(KH_kPa)
      ALLOCATE(Harray(ndata))
      Harray = cH2O / (1E3*KH_kPa)
      CALL HTdep(temp_, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2926

  !---------------------------------------------------------------------------

  SUBROUTINE ref2927 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2927"
    type = "Q"

    CALL CalcH("tribromomethyl peroxide",      "_CAS-04", 1.9E5  ) ! CBr3O2H
    CALL CalcH("dibromomethyl peroxide",       "_CAS-05", 2.24E4 ) ! CHBr2O2H
    CALL CalcH("bromomethyl peroxide",         "_CAS-06", 2.58E3 ) ! CH2BrO2H
    CALL CalcH("formyl bromide",             "7726-11-6", 74.    ) ! CHBrO
    CALL CalcH("carbonyl bromide",            "593-95-3", 21.5   ) ! CBr2O
    CALL CalcH("tribromomethyl peroxynitrate", "_CAS-07", 401.   ) ! CBr3O2NO2
    CALL CalcH("dibromomethyl peroxynitrate",  "_CAS-08", 304.   ) ! CHBr2O2NO2
    CALL CalcH("bromomethyl peroxynitrate",    "_CAS-09", 35.    ) ! CH2BrO2NO2
    CALL CalcH("tribromomethanol",           "5405-30-1", 1.5E5  ) ! CBr3OH
    CALL CalcH("dibromomethanol",              "_CAS-10", 1.73E4 ) ! CHBr2OH
    CALL CalcH("bromomethanol",                "_CAS-11", 2.0E3  ) ! CH2BrOH

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, k0H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: k0H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcp_TO_HcpSI * k0H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2927

  !---------------------------------------------------------------------------

  SUBROUTINE ref2928 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "2928"
    type = "M"
    chem = "hydrogen sulfide" ; casrn = "7783-06-4" ! H2S
    ndata = 24
    ALLOCATE(temp(ndata), Harray(ndata))
    ! using data up to 61 C from Table 1:
    temp = (/ 27.5, 27.5, 26.5, 26.5, 23.5, 23.5, 24., 24., 25., 25., 25., &
      25., 60., 60., 59., 59., 59., 59., 61., 61., 60., 60., 60., 60. /) + CtoK
    Harray = (/ 0.09196, 0.09097, 0.09906, 0.09601, 0.10212, 0.10229, 0.10087, &
      0.10172, 0.08889, 0.09194, 0.09302, 0.08742, 0.04723, 0.04308, 0.04490, &
      0.04466, 0.04438, 0.04473, 0.04163, 0.04183, 0.04005, 0.04070, 0.03967, &
      0.04363 /) &
       * rhoH2O / atm
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2928

  !---------------------------------------------------------------------------

  SUBROUTINE ref2929 ! KHpc [atm*L/mol)]
    IMPLICIT NONE

    ref = "2929"
    type = "M"
    chem = "hydrogen sulfide" ; casrn = "7783-06-4" ! H2S
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    ! Table 1:
    temp = (/ 25., 40., 60. /) + CtoK
    Harray = Hcp_TO_HcpSI / (/ 10.6, 14.62, 19.17 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2929

  !---------------------------------------------------------------------------

  SUBROUTINE ref2930 ! KHcc [1]
    IMPLICIT NONE

    ref = "2930"
    type = "M"

    CALL CalcH("methylbenzene",     "108-88-3", 0.264  ) ! C6H5CH3 toluene
    CALL CalcH("chlorobenzene",     "108-90-7", 0.138  ) ! C6H5Cl
    CALL CalcH("methylcyclohexane", "108-87-2", 2.77   ) ! C6H{11}CH3
    CALL CalcH("trichloroethene",    "79-01-6", 0.484  ) ! C2HCl3 trichloroethylene
    CALL CalcH("butane nitrile",    "109-74-0", 0.0030 ) ! C3H7CN butyronitrile

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / H
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2930

  !---------------------------------------------------------------------------

  SUBROUTINE ref2931 ! KHpx [1E4 Pa]
    IMPLICIT NONE

    ref = "2931"
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 293.15, 303.15, 313.15, 323.15 /)
    CALL CalcH("benzene",                   "71-43-2", & ! C6H6
      (/ 2512.65, 4013.57,  6065.12,  8914.52 /))
    CALL CalcH("chlorobenzene",            "108-90-7", & ! C6H5Cl
      (/ 1549.90, 2156.07,  2619.42,  2809.97 /))
    CALL CalcH("trichloroethene",           "79-01-6", & ! C2HCl3 trichloroethylene
      (/ 4312.96, 7640.23, 11532.00, 17072.58 /))
    CALL CalcH("($E$)-1,2-dichloroethene", "156-60-5", & ! CHClCHCl {trans}-1,2-dichloroethene
      (/ 4418.50, 7770.78, 11572.75, 16514.26 /))
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, k1)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: k1
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = cH2O / (1E4*k1)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2931

  !---------------------------------------------------------------------------

  SUBROUTINE ref2932 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2932"
    type = "Q"

    CALL CalcH("2-chlorobiphenyl",                          "2051-60-7", 59.0,  46.2 ) ! C12H9Cl PCB-1
    CALL CalcH("3-chlorobiphenyl",                          "2051-61-8", 27.0,  32.4 ) ! C12H9Cl PCB-2
    CALL CalcH("4-chlorobiphenyl",                          "2051-62-9", 46.8,  29.8 ) ! C12H9Cl PCB-3
    CALL CalcH("2,2'-dichlorobiphenyl",                    "13029-08-8", 37.1,  47.3 ) ! C12H8Cl2 PCB-4
    CALL CalcH("2,3-dichlorobiphenyl",                     "16605-91-7", 26.1,  24.6 ) ! C12H8Cl2 PCB-5
    CALL CalcH("2,3'-dichlorobiphenyl",                    "25569-80-6", 29.8,  30.4 ) ! C12H8Cl2 PCB-6
    CALL CalcH("2,4-dichlorobiphenyl",                     "33284-50-3", 45.3,  33.8 ) ! C12H8Cl2 PCB-7
    CALL CalcH("2,4'-dichlorobiphenyl",                    "34883-43-7", 59.8,  29.3 ) ! C12H8Cl2 PCB-8
    CALL CalcH("2,5-dichlorobiphenyl",                     "34883-39-1", 32.6,  38.2 ) ! C12H8Cl2 PCB-9
    CALL CalcH("2,6-dichlorobiphenyl",                     "33146-45-1", 53.5,  42.7 ) ! C12H8Cl2 PCB-10
    CALL CalcH("3,3'-dichlorobiphenyl",                     "2050-67-1", 13.6,  23.3 ) ! C12H8Cl2 PCB-11
    CALL CalcH("3,4-dichlorobiphenyl",                      "2974-92-7", 20.7,  23.3 ) ! C12H8Cl2 PCB-12
    CALL CalcH("3,4'-dichlorobiphenyl",                     "2974-90-5", 27.3,  22.9 ) ! C12H8Cl2 PCB-13
    CALL CalcH("3,5-dichlorobiphenyl",                     "34883-41-5", 14.9,  31.6 ) ! C12H8Cl2 PCB-14
    CALL CalcH("4,4'-dichlorobiphenyl",                     "2050-68-2", 47.4,  20.8 ) ! C12H8Cl2 PCB-15
    CALL CalcH("2,2',3-trichlorobiphenyl",                 "38444-78-9", 16.4,  22.5 ) ! C12H7Cl3 PCB-16
    CALL CalcH("2,2',4-trichlorobiphenyl",                 "37680-66-3", 28.5,  31.3 ) ! C12H7Cl3 PCB-17
    CALL CalcH("2,2',5-trichlorobiphenyl",                 "37680-65-2", 20.5,  33.0 ) ! C12H7Cl3 PCB-18
    CALL CalcH("2,2',6-trichlorobiphenyl",                 "38444-73-4", 33.7,  41.4 ) ! C12H7Cl3 PCB-19
    CALL CalcH("2,3,3'-trichlorobiphenyl",                 "38444-84-7", 13.2,  15.3 ) ! C12H7Cl3 PCB-20
    CALL CalcH("2,3,4-trichlorobiphenyl",                  "55702-46-0", 20.1,  17.0 ) ! C12H7Cl3 PCB-21
    CALL CalcH("2,3,4'-trichlorobiphenyl",                 "38444-85-8", 26.5,  15.7 ) ! C12H7Cl3 PCB-22
    CALL CalcH("2,3,5-trichlorobiphenyl",                  "55720-44-0", 14.4,  21.4 ) ! C12H7Cl3 PCB-23
    CALL CalcH("2,3,6-trichlorobiphenyl",                  "55702-45-9", 23.7,  22.5 ) ! C12H7Cl3 PCB-24
    CALL CalcH("2,3',4-trichlorobiphenyl",                 "55712-37-3", 22.9,  22.4 ) ! C12H7Cl3 PCB-25
    CALL CalcH("2,3',5-trichlorobiphenyl",                 "38444-81-4", 16.5,  23.5 ) ! C12H7Cl3 PCB-26
    CALL CalcH("2,3',6-trichlorobiphenyl",                 "38444-76-7", 27.1,  23.9 ) ! C12H7Cl3 PCB-27
    CALL CalcH("2,4,4'-trichlorobiphenyl",                  "7012-37-5", 45.9,  21.4 ) ! C12H7Cl3 PCB-28
    CALL CalcH("2,4,5-trichlorobiphenyl",                  "15862-07-4", 25.1,  23.7 ) ! C12H7Cl3 PCB-29
    CALL CalcH("2,4,6-trichlorobiphenyl",                  "35693-92-6", 41.1,  39.6 ) ! C12H7Cl3 PCB-30
    CALL CalcH("2,4',5-trichlorobiphenyl",                 "16606-02-3", 33.1,  24.6 ) ! C12H7Cl3 PCB-31
    CALL CalcH("2,4',6-trichlorobiphenyl",                 "38444-77-8", 54.3,  24.5 ) ! C12H7Cl3 PCB-32
    CALL CalcH("2,3',4'-trichlorobiphenyl",                "38444-86-9", 13.0,  20.2 ) ! C12H7Cl3 PCB-33
    CALL CalcH("2,3',5'-trichlorobiphenyl",                "37680-68-5",  9.4,  25.5 ) ! C12H7Cl3 PCB-34
    CALL CalcH("3,3',4-trichlorobiphenyl",                 "37680-69-6", 10.5,  17.0 ) ! C12H7Cl3 PCB-35
    CALL CalcH("3,3',5-trichlorobiphenyl",                 "38444-87-0",  7.5,  21.5 ) ! C12H7Cl3 PCB-36
    CALL CalcH("3,4,4'-trichlorobiphenyl",                 "38444-90-5", 21.0,  16.5 ) ! C12H7Cl3 PCB-37
    CALL CalcH("3,4,5-trichlorobiphenyl",                  "53555-66-1", 11.4,  19.1 ) ! C12H7Cl3 PCB-38
    CALL CalcH("3,4',5-trichlorobiphenyl",                 "38444-88-1", 15.1,  22.5 ) ! C12H7Cl3 PCB-39
    CALL CalcH("2,2',3,3'-tetrachlorobiphenyl",            "38444-93-8",  8.3,  10.3 ) ! C12H6Cl4 PCB-40
    CALL CalcH("2,2',3,4-tetrachlorobiphenyl",             "52663-59-9", 12.6,  14.1 ) ! C12H6Cl4 PCB-41
    CALL CalcH("2,2',3,4'-tetrachlorobiphenyl",            "36559-22-5", 16.7,  14.9 ) ! C12H6Cl4 PCB-42
    CALL CalcH("2,2',3,5-tetrachlorobiphenyl",             "70362-46-8",  9.1,  16.4 ) ! C12H6Cl4 PCB-43
    CALL CalcH("2,2',3,5'-tetrachlorobiphenyl",            "41464-39-5",  8.2,  15.0 ) ! C12H6Cl4 PCB-44
    CALL CalcH("2,2',3,6-tetrachlorobiphenyl",             "70362-45-7", 14.9,  19.5 ) ! C12H6Cl4 PCB-45
    CALL CalcH("2,2',3,6'-tetrachlorobiphenyl",            "41464-47-5", 15.0,  17.6 ) ! C12H6Cl4 PCB-46
    CALL CalcH("2,2',4,4'-tetrachlorobiphenyl",             "2437-79-8", 28.9,  20.7 ) ! C12H6Cl4 PCB-47
    CALL CalcH("2,2',4,5-tetrachlorobiphenyl",             "70362-47-9", 15.8,  18.9 ) ! C12H6Cl4 PCB-48
    CALL CalcH("2,2',4,5'-tetrachlorobiphenyl",            "41464-40-8", 14.3,  22.0 ) ! C12H6Cl4 PCB-49
    CALL CalcH("2,2',4,6-tetrachlorobiphenyl",             "62796-65-0", 25.9,  35.9 ) ! C12H6Cl4 PCB-50
    CALL CalcH("2,2',4,6'-tetrachlorobiphenyl",            "68194-04-7", 26.0,  25.2 ) ! C12H6Cl4 PCB-51
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl",            "35693-99-3", 10.3,  21.9 ) ! C12H6Cl4 PCB-52
    CALL CalcH("2,2',5,6'-tetrachlorobiphenyl",            "41464-41-9", 18.7,  24.6 ) ! C12H6Cl4 PCB-53
    CALL CalcH("2,2',6,6'-tetrachlorobiphenyl",            "15968-05-5", 30.7,  36.5 ) ! C12H6Cl4 PCB-54
    CALL CalcH("2,3,3',4-tetrachlorobiphenyl",             "74338-24-2", 10.1,  10.7 ) ! C12H6Cl4 PCB-55
    CALL CalcH("2,3,3',4'-tetrachlorobiphenyl",            "41464-43-1", 13.4,  10.3 ) ! C12H6Cl4 PCB-56
    CALL CalcH("2,3,3',5-tetrachlorobiphenyl",             "70424-67-8",  7.3,  12.8 ) ! C12H6Cl4 PCB-57
    CALL CalcH("2,3,3',5'-tetrachlorobiphenyl",            "41464-49-7",  6.6,  12.3 ) ! C12H6Cl4 PCB-58
    CALL CalcH("2,3,3',6-tetrachlorobiphenyl",             "74472-33-6", 12.0,  12.1 ) ! C12H6Cl4 PCB-59
    CALL CalcH("2,3,4,4'-tetrachlorobiphenyl",             "33025-41-1", 20.3,  10.9 ) ! C12H6Cl4 PCB-60
    CALL CalcH("2,3,4,5-tetrachlorobiphenyl",              "33284-53-6", 11.1,  11.9 ) ! C12H6Cl4 PCB-61
    CALL CalcH("2,3,4,6-tetrachlorobiphenyl",              "54230-22-7", 18.2,  15.7 ) ! C12H6Cl4 PCB-62
    CALL CalcH("2,3,4',5-tetrachlorobiphenyl",             "74472-34-7", 14.6,  14.0 ) ! C12H6Cl4 PCB-63
    CALL CalcH("2,3,4',6-tetrachlorobiphenyl",             "52663-58-8", 24.0,  13.0 ) ! C12H6Cl4 PCB-64
    CALL CalcH("2,3,5,6-tetrachlorobiphenyl",              "33284-54-7", 13.1,  10.1 ) ! C12H6Cl4 PCB-65
    CALL CalcH("2,3',4,4'-tetrachlorobiphenyl",            "32598-10-0", 23.2,  14.7 ) ! C12H6Cl4 PCB-66
    CALL CalcH("2,3',4,5-tetrachlorobiphenyl",             "73575-53-8", 12.7,  14.7 ) ! C12H6Cl4 PCB-67
    CALL CalcH("2,3',4,5'-tetrachlorobiphenyl",            "73575-52-7", 11.5,  19.2 ) ! C12H6Cl4 PCB-68
    CALL CalcH("2,3',4,6-tetrachlorobiphenyl",             "60233-24-1", 20.8,  22.3 ) ! C12H6Cl4 PCB-69
    CALL CalcH("2,3',4',5-tetrachlorobiphenyl",            "32598-11-1", 16.7,  15.7 ) ! C12H6Cl4 PCB-70
    CALL CalcH("2,3',4',6-tetrachlorobiphenyl",            "41464-46-4", 27.4,  14.3 ) ! C12H6Cl4 PCB-71
    CALL CalcH("2,3',5,5'-tetrachlorobiphenyl",            "41464-42-0",  8.3,  18.9 ) ! C12H6Cl4 PCB-72
    CALL CalcH("2,3',5',6-tetrachlorobiphenyl",            "74338-23-1", 13.6,  16.8 ) ! C12H6Cl4 PCB-73
    CALL CalcH("2,4,4',5-tetrachlorobiphenyl",             "32690-93-0", 25.4,  15.3 ) ! C12H6Cl4 PCB-74
    CALL CalcH("2,4,4',6-tetrachlorobiphenyl",             "32598-12-2", 41.7,  22.6 ) ! C12H6Cl4 PCB-75
    CALL CalcH("2,3',4',5'-tetrachlorobiphenyl",           "70362-48-0",  7.2,  14.2 ) ! C12H6Cl4 PCB-76
    CALL CalcH("3,3',4,4'-tetrachlorobiphenyl",            "32598-13-3", 10.6,  12.5 ) ! C12H6Cl4 PCB-77
    CALL CalcH("3,3',4,5-tetrachlorobiphenyl",             "70362-49-1",  5.8,  13.3 ) ! C12H6Cl4 PCB-78
    CALL CalcH("3,3',4,5'-tetrachlorobiphenyl",            "41464-48-6",  5.3,  15.9 ) ! C12H6Cl4 PCB-79
    CALL CalcH("3,3',5,5'-tetrachlorobiphenyl",            "33284-52-5",  3.8,  19.1 ) ! C12H6Cl4 PCB-80
    CALL CalcH("3,4,4',5-tetrachlorobiphenyl",             "70362-50-4", 11.6,  13.9 ) ! C12H6Cl4 PCB-81
    CALL CalcH("2,2',3,3',4-pentachlorobiphenyl",          "52663-62-4",  6.4,   6.6 ) ! C12H5Cl5 PCB-82
    CALL CalcH("2,2',3,3',5-pentachlorobiphenyl",          "60145-20-2",  4.6,   7.3 ) ! C12H5Cl5 PCB-83
    CALL CalcH("2,2',3,3',6-pentachlorobiphenyl",          "52663-60-2",  7.5,   8.0 ) ! C12H5Cl5 PCB-84
    CALL CalcH("2,2',3,4,4'-pentachlorobiphenyl",          "65510-45-4", 12.8,   9.5 ) ! C12H5Cl5 PCB-85
    CALL CalcH("2,2',3,4,5-pentachlorobiphenyl",           "55312-69-1",  7.0,   8.6 ) ! C12H5Cl5 PCB-86
    CALL CalcH("2,2',3,4,5'-pentachlorobiphenyl",          "38380-02-8",  6.3,   9.7 ) ! C12H5Cl5 PCB-87
    CALL CalcH("2,2',3,4,6-pentachlorobiphenyl",           "55215-17-3",  8.2,  12.8 ) ! C12H5Cl5 PCB-88
    CALL CalcH("2,2',3,4,6'-pentachlorobiphenyl",          "73575-57-2", 11.5,  10.2 ) ! C12H5Cl5 PCB-89
    CALL CalcH("2,2',3,4',5-pentachlorobiphenyl",          "68194-07-0",  9.2,  11.3 ) ! C12H5Cl5 PCB-90
    CALL CalcH("2,2',3,4',6-pentachlorobiphenyl",          "68194-05-8", 15.1,  12.1 ) ! C12H5Cl5 PCB-91
    CALL CalcH("2,2',3,5,5'-pentachlorobiphenyl",          "52663-61-3",  4.6,  10.5 ) ! C12H5Cl5 PCB-92
    CALL CalcH("2,2',3,5,6-pentachlorobiphenyl",           "73575-56-1",  8.2,   7.6 ) ! C12H5Cl5 PCB-93
    CALL CalcH("2,2',3,5,6'-pentachlorobiphenyl",          "73575-55-0",  8.3,  11.0 ) ! C12H5Cl5 PCB-94
    CALL CalcH("2,2',3,5',6-pentachlorobiphenyl",          "38379-99-6",  7.5,  11.1 ) ! C12H5Cl5 PCB-95
    CALL CalcH("2,2',3,6,6'-pentachlorobiphenyl",          "73575-54-9", 13.6,  15.3 ) ! C12H5Cl5 PCB-96
    CALL CalcH("2,2',3,4',5'-pentachlorobiphenyl",         "41464-51-1",  8.0,   8.7 ) ! C12H5Cl5 PCB-97
    CALL CalcH("2,2',3,4',6'-pentachlorobiphenyl",         "60233-25-2", 13.1,  15.3 ) ! C12H5Cl5 PCB-98
    CALL CalcH("2,2',4,4',5-pentachlorobiphenyl",          "38380-01-7", 16.0,  12.7 ) ! C12H5Cl5 PCB-99
    CALL CalcH("2,2',4,4',6-pentachlorobiphenyl",          "39485-83-1", 26.2,  21.8 ) ! C12H5Cl5 PCB-100
    CALL CalcH("2,2',4,5,5'-pentachlorobiphenyl",          "37680-73-2",  7.9,  12.7 ) ! C12H5Cl5 PCB-101
    CALL CalcH("2,2',4,5,6'-pentachlorobiphenyl",          "68194-06-9", 14.4,  13.0 ) ! C12H5Cl5 PCB-102
    CALL CalcH("2,2',4,5',6-pentachlorobiphenyl",          "60145-21-3", 13.0,  21.6 ) ! C12H5Cl5 PCB-103
    CALL CalcH("2,2',4,6,6'-pentachlorobiphenyl",          "56558-16-8", 23.6,  29.6 ) ! C12H5Cl5 PCB-104
    CALL CalcH("2,3,3',4,4'-pentachlorobiphenyl",          "32598-14-4", 10.3,   7.1 ) ! C12H5Cl5 PCB-105
    CALL CalcH("2,3,3',4,5-pentachlorobiphenyl",           "70424-69-0",  5.6,   7.2 ) ! C12H5Cl5 PCB-106
    CALL CalcH("2,3,3',4',5-pentachlorobiphenyl",          "70424-68-9",  5.1,   8.6 ) ! C12H5Cl5 PCB-107
    CALL CalcH("2,3,3',4,5'-pentachlorobiphenyl",          "70362-41-3",  9.2,   8.8 ) ! C12H5Cl5 PCB-108
    CALL CalcH("2,3,3',4,6-pentachlorobiphenyl",           "74472-35-8",  7.4,   8.5 ) ! C12H5Cl5 PCB-109
    CALL CalcH("2,3,3',4',6-pentachlorobiphenyl",          "38380-03-9", 12.1,   7.3 ) ! C12H5Cl5 PCB-110
    CALL CalcH("2,3,3',5,5'-pentachlorobiphenyl",          "39635-32-0",  3.7,  12.9 ) ! C12H5Cl5 PCB-111
    CALL CalcH("2,3,3',5,6-pentachlorobiphenyl",           "74472-36-9",  6.6,   5.1 ) ! C12H5Cl5 PCB-112
    CALL CalcH("2,3,3',5',6-pentachlorobiphenyl",          "68194-10-5",  6.0,   8.2 ) ! C12H5Cl5 PCB-113
    CALL CalcH("2,3,4,4',5-pentachlorobiphenyl",           "74472-37-0", 11.2,   7.8 ) ! C12H5Cl5 PCB-114
    CALL CalcH("2,3,4,4',6-pentachlorobiphenyl",           "74472-38-1", 18.5,   9.1 ) ! C12H5Cl5 PCB-115
    CALL CalcH("2,3,4,5,6-pentachlorobiphenyl",            "18259-05-7", 10.1,   5.7 ) ! C12H5Cl5 PCB-116
    CALL CalcH("2,3,4',5,6-pentachlorobiphenyl",           "68194-11-6", 13.3,   5.9 ) ! C12H5Cl5 PCB-117
    CALL CalcH("2,3',4,4',5-pentachlorobiphenyl",          "31508-00-6", 12.8,   9.9 ) ! C12H5Cl5 PCB-118
    CALL CalcH("2,3',4,4',6-pentachlorobiphenyl",          "56558-17-9", 21.1,  13.5 ) ! C12H5Cl5 PCB-119
    CALL CalcH("2,3',4,5,5'-pentachlorobiphenyl",          "68194-12-7",  6.4,  12.0 ) ! C12H5Cl5 PCB-120
    CALL CalcH("2,3',4,5',6-pentachlorobiphenyl",          "56558-18-0", 10.4,  16.2 ) ! C12H5Cl5 PCB-121
    CALL CalcH("2,3,3',4',5'-pentachlorobiphenyl",         "76842-07-4",  3.6,   7.0 ) ! C12H5Cl5 PCB-122
    CALL CalcH("2,3',4,4',5'-pentachlorobiphenyl",         "65510-44-3",  7.3,  10.7 ) ! C12H5Cl5 PCB-123
    CALL CalcH("2,3',4',5,5'-pentachlorobiphenyl",         "70424-70-3",  3.6,  10.6 ) ! C12H5Cl5 PCB-124
    CALL CalcH("2,3',4',5',6-pentachlorobiphenyl",         "74472-39-2",  6.6,   8.7 ) ! C12H5Cl5 PCB-125
    CALL CalcH("3,3',4,4',5-pentachlorobiphenyl",          "57465-28-8",  5.9,   9.8 ) ! C12H5Cl5 PCB-126
    CALL CalcH("3,3',4,5,5'-pentachlorobiphenyl",          "39635-33-1",  2.9,  11.9 ) ! C12H5Cl5 PCB-127
    CALL CalcH("2,2',3,3',4,4'-hexachlorobiphenyl",        "38380-07-3",  6.5,   4.2 ) ! C12H4Cl6 PCB-128
    CALL CalcH("2,2',3,3',4,5-hexachlorobiphenyl",         "55215-18-4",  3.5,   3.8 ) ! C12H4Cl6 PCB-129
    CALL CalcH("2,2',3,3',4,5'-hexachlorobiphenyl",        "52663-66-8",  3.2,   4.8 ) ! C12H4Cl6 PCB-130
    CALL CalcH("2,2',3,3',4,6-hexachlorobiphenyl",         "61798-70-7",  5.8,   5.3 ) ! C12H4Cl6 PCB-131
    CALL CalcH("2,2',3,3',4,6'-hexachlorobiphenyl",        "38380-05-1",  5.8,   4.7 ) ! C12H4Cl6 PCB-132
    CALL CalcH("2,2',3,3',5,5'-hexachlorobiphenyl",        "35694-04-3",  2.3,   4.9 ) ! C12H4Cl6 PCB-133
    CALL CalcH("2,2',3,3',5,6-hexachlorobiphenyl",         "52704-70-8",  4.2,   3.1 ) ! C12H4Cl6 PCB-134
    CALL CalcH("2,2',3,3',5,6'-hexachlorobiphenyl",        "52744-13-5",  4.2,   4.8 ) ! C12H4Cl6 PCB-135
    CALL CalcH("2,2',3,3',6,6'-hexachlorobiphenyl",        "38411-22-2",  6.9,   6.2 ) ! C12H4Cl6 PCB-136
    CALL CalcH("2,2',3,4,4',5-hexachlorobiphenyl",         "35694-06-5",  7.1,   5.9 ) ! C12H4Cl6 PCB-137
    CALL CalcH("2,2',3,4,4',5'-hexachlorobiphenyl",        "35065-28-2",  6.4,   5.6 ) ! C12H4Cl6 PCB-138
    CALL CalcH("2,2',3,4,4',6-hexachlorobiphenyl",         "56030-56-9", 11.6,   7.9 ) ! C12H4Cl6 PCB-139
    CALL CalcH("2,2',3,4,4',6'-hexachlorobiphenyl",        "59291-64-4", 11.7,   9.0 ) ! C12H4Cl6 PCB-140
    CALL CalcH("2,2',3,4,5,5'-hexachlorobiphenyl",         "52712-04-6",  3.5,   5.6 ) ! C12H4Cl6 PCB-141
    CALL CalcH("2,2',3,4,5,6-hexachlorobiphenyl",          "41411-61-4",  6.3,   4.2 ) ! C12H4Cl6 PCB-142
    CALL CalcH("2,2',3,4,5,6'-hexachlorobiphenyl",         "68194-15-0",  6.4,   5.4 ) ! C12H4Cl6 PCB-143
    CALL CalcH("2,2',3,4,5',6-hexachlorobiphenyl",         "68194-14-9",  5.7,   7.4 ) ! C12H4Cl6 PCB-144
    CALL CalcH("2,2',3,4,6,6'-hexachlorobiphenyl",         "74472-40-5", 10.4,   9.3 ) ! C12H4Cl6 PCB-145
    CALL CalcH("2,2',3,4',5,5'-hexachlorobiphenyl",        "51908-16-8",  4.6,   6.2 ) ! C12H4Cl6 PCB-146
    CALL CalcH("2,2',3,4',5,6-hexachlorobiphenyl",         "68194-13-8",  8.4,   4.8 ) ! C12H4Cl6 PCB-147
    CALL CalcH("2,2',3,4',5,6'-hexachlorobiphenyl",        "74472-41-6",  8.4,   9.8 ) ! C12H4Cl6 PCB-148
    CALL CalcH("2,2',3,4',5',6-hexachlorobiphenyl",        "38380-04-0",  7.6,   6.0 ) ! C12H4Cl6 PCB-149
    CALL CalcH("2,2',3,4',6,6'-hexachlorobiphenyl",        "68194-08-1", 13.8,  12.5 ) ! C12H4Cl6 PCB-150
    CALL CalcH("2,2',3,5,5',6-hexachlorobiphenyl",         "52663-63-5",  4.1,   4.2 ) ! C12H4Cl6 PCB-151
    CALL CalcH("2,2',3,5,6,6'-hexachlorobiphenyl",         "68194-09-2",  7.5,   5.3 ) ! C12H4Cl6 PCB-152
    CALL CalcH("2,2',4,4',5,5'-hexachlorobiphenyl",        "35065-27-1",  8.0,   7.4 ) ! C12H4Cl6 PCB-153
    CALL CalcH("2,2',4,4',5,6'-hexachlorobiphenyl",        "60145-22-4", 14.6,  11.5 ) ! C12H4Cl6 PCB-154
    CALL CalcH("2,2',4,4',6,6'-hexachlorobiphenyl",        "33979-03-2", 23.9,  23.7 ) ! C12H4Cl6 PCB-155
    CALL CalcH("2,3,3',4,4',5-hexachlorobiphenyl",         "38380-08-4",  5.7,   4.9 ) ! C12H4Cl6 PCB-156
    CALL CalcH("2,3,3',4,4',5'-hexachlorobiphenyl",        "69782-90-7",  5.2,   5.0 ) ! C12H4Cl6 PCB-157
    CALL CalcH("2,3,3',4,4',6-hexachlorobiphenyl",         "74472-42-7",  9.3,   5.2 ) ! C12H4Cl6 PCB-158
    CALL CalcH("2,3,3',4,5,5'-hexachlorobiphenyl",         "39635-35-3",  2.8,   5.6 ) ! C12H4Cl6 PCB-159
    CALL CalcH("2,3,3',4,5,6-hexachlorobiphenyl",          "41411-62-5",  5.1,   3.0 ) ! C12H4Cl6 PCB-160
    CALL CalcH("2,3,3',4,5',6-hexachlorobiphenyl",         "74472-43-8",  4.6,   5.9 ) ! C12H4Cl6 PCB-161
    CALL CalcH("2,3,3',4',5,5'-hexachlorobiphenyl",        "39635-34-2",  3.7,   5.7 ) ! C12H4Cl6 PCB-162
    CALL CalcH("2,3,3',4',5,6-hexachlorobiphenyl",         "74472-44-9",  6.7,   3.2 ) ! C12H4Cl6 PCB-163
    CALL CalcH("2,3,3',4',5',6-hexachlorobiphenyl",        "74472-45-0",  6.1,   4.4 ) ! C12H4Cl6 PCB-164
    CALL CalcH("2,3,3',5,5',6-hexachlorobiphenyl",         "74472-46-1",  3.3,   3.4 ) ! C12H4Cl6 PCB-165
    CALL CalcH("2,3,4,4',5,6-hexachlorobiphenyl",          "41411-63-6", 10.2,   3.4 ) ! C12H4Cl6 PCB-166
    CALL CalcH("2,3',4,4',5,5'-hexachlorobiphenyl",        "52663-72-6",  6.4,   6.9 ) ! C12H4Cl6 PCB-167
    CALL CalcH("2,3',4,4',5',6-hexachlorobiphenyl",        "59291-65-5", 10.6,   8.4 ) ! C12H4Cl6 PCB-168
    CALL CalcH("3,3',4,4',5,5'-hexachlorobiphenyl",        "32774-16-6",  2.9,   7.6 ) ! C12H4Cl6 PCB-169
    CALL CalcH("2,2',3,3',4,4',5-heptachlorobiphenyl",     "35065-30-6",  3.6,   2.5 ) ! C12H3Cl7 PCB-170
    CALL CalcH("2,2',3,3',4,4',6-heptachlorobiphenyl",     "52663-71-5",  5.9,   3.2 ) ! C12H3Cl7 PCB-171
    CALL CalcH("2,2',3,3',4,5,5'-heptachlorobiphenyl",     "52663-74-8",  1.8,   2.6 ) ! C12H3Cl7 PCB-172
    CALL CalcH("2,2',3,3',4,5,6-heptachlorobiphenyl",      "68194-16-1",  3.2,   1.7 ) ! C12H3Cl7 PCB-173
    CALL CalcH("2,2',3,3',4,5,6'-heptachlorobiphenyl",     "38411-25-5",  3.2,   2.3 ) ! C12H3Cl7 PCB-174
    CALL CalcH("2,2',3,3',4,5',6-heptachlorobiphenyl",     "40186-70-7",  2.9,   3.3 ) ! C12H3Cl7 PCB-175
    CALL CalcH("2,2',3,3',4,6,6'-heptachlorobiphenyl",     "52663-65-7",  5.3,   3.9 ) ! C12H3Cl7 PCB-176
    CALL CalcH("2,2',3,3',4,5',6'-heptachlorobiphenyl",    "52663-70-4",  4.2,   1.9 ) ! C12H3Cl7 PCB-177
    CALL CalcH("2,2',3,3',5,5',6-heptachlorobiphenyl",     "52663-67-9",  2.1,   1.8 ) ! C12H3Cl7 PCB-178
    CALL CalcH("2,2',3,3',5,6,6'-heptachlorobiphenyl",     "52663-64-6",  3.8,   2.1 ) ! C12H3Cl7 PCB-179
    CALL CalcH("2,2',3,4,4',5,5'-heptachlorobiphenyl",     "35065-29-3",  3.6,   3.3 ) ! C12H3Cl7 PCB-180
    CALL CalcH("2,2',3,4,4',5,6-heptachlorobiphenyl",      "74472-47-2",  6.4,   2.6 ) ! C12H3Cl7 PCB-181
    CALL CalcH("2,2',3,4,4',5,6'-heptachlorobiphenyl",     "60145-23-5",  6.5,   4.7 ) ! C12H3Cl7 PCB-182
    CALL CalcH("2,2',3,4,4',5',6-heptachlorobiphenyl",     "52663-69-1",  5.8,   4.0 ) ! C12H3Cl7 PCB-183
    CALL CalcH("2,2',3,4,4',6,6'-heptachlorobiphenyl",     "74472-48-3", 10.6,   7.6 ) ! C12H3Cl7 PCB-184
    CALL CalcH("2,2',3,4,5,5',6-heptachlorobiphenyl",      "52712-05-7",  3.2,   2.3 ) ! C12H3Cl7 PCB-185
    CALL CalcH("2,2',3,4,5,6,6'-heptachlorobiphenyl",      "74472-49-4",  5.8,   2.7 ) ! C12H3Cl7 PCB-186
    CALL CalcH("2,2',3,4',5,5',6-heptachlorobiphenyl",     "52663-68-0",  4.2,   2.3 ) ! C12H3Cl7 PCB-187
    CALL CalcH("2,2',3,4',5,6,6'-heptachlorobiphenyl",     "74487-85-7",  7.6,   4.4 ) ! C12H3Cl7 PCB-188
    CALL CalcH("2,3,3',4,4',5,5'-heptachlorobiphenyl",     "39635-31-9",  2.9,   3.3 ) ! C12H3Cl7 PCB-189
    CALL CalcH("2,3,3',4,4',5,6-heptachlorobiphenyl",      "41411-64-7",  5.2,   1.9 ) ! C12H3Cl7 PCB-190
    CALL CalcH("2,3,3',4,4',5',6-heptachlorobiphenyl",     "74472-50-7",  4.7,   3.1 ) ! C12H3Cl7 PCB-191
    CALL CalcH("2,3,3',4,5,5',6-heptachlorobiphenyl",      "74472-51-8",  2.6,   2.0 ) ! C12H3Cl7 PCB-192
    CALL CalcH("2,3,3',4',5,5',6-heptachlorobiphenyl",     "69782-91-8",  3.4,   1.8 ) ! C12H3Cl7 PCB-193
    CALL CalcH("2,2',3,3',4,4',5,5'-octachlorobiphenyl",   "35694-08-7",  1.8,   1.4 ) ! C12H2Cl8 PCB-194
    CALL CalcH("2,2',3,3',4,4',5,6-octachlorobiphenyl",    "52663-78-2",  3.2,   1.0 ) ! C12H2Cl8 PCB-195
    CALL CalcH("2,2',3,3',4,4',5,6'-octachlorobiphenyl",   "42740-50-1",  2.9,   1.6 ) ! C12H2Cl8 PCB-196
    CALL CalcH("2,2',3,3',4,4',6,6'-octachlorobiphenyl",   "33091-17-7",  5.4,   2.4 ) ! C12H2Cl8 PCB-197
    CALL CalcH("2,2',3,3',4,5,5',6-octachlorobiphenyl",    "68194-17-2",  1.6,   1.0 ) ! C12H2Cl8 PCB-198
    CALL CalcH("2,2',3,3',4,5,5',6'-octachlorobiphenyl",   "52663-75-9",  1.6,   1.1 ) ! C12H2Cl8 PCB-199
    CALL CalcH("2,2',3,3',4,5,6,6'-octachlorobiphenyl",    "52663-73-7",  2.9,   1.3 ) ! C12H2Cl8 PCB-200
    CALL CalcH("2,2',3,3',4,5',6,6'-octachlorobiphenyl",   "40186-71-8",  2.7,   0.9 ) ! C12H2Cl8 PCB-201
    CALL CalcH("2,2',3,3',5,5',6,6'-octachlorobiphenyl",    "2136-99-4",  1.9,   0.7 ) ! C12H2Cl8 PCB-202
    CALL CalcH("2,2',3,4,4',5,5',6-octachlorobiphenyl",    "52663-76-0",  3.2,   1.3 ) ! C12H2Cl8 PCB-203
    CALL CalcH("2,2',3,4,4',5,6,6'-octachlorobiphenyl",    "74472-52-9",  5.9,   2.2 ) ! C12H2Cl8 PCB-204
    CALL CalcH("2,3,3',4,4',5,5',6-octachlorobiphenyl",    "74472-53-0",  2.6,   1.1 ) ! C12H2Cl8 PCB-205
    CALL CalcH("2,2',3,3',4,4',5,5',6-nonachlorobiphenyl", "40186-72-9",  1.6,   0.5 ) ! C12HCl9  PCB-206
    CALL CalcH("2,2',3,3',4,4',5,6,6'-nonachlorobiphenyl", "52663-79-3",  3.0,   0.7 ) ! C12HCl9  PCB-207
    CALL CalcH("2,2',3,3',4,5,5',6,6'-nonachlorobiphenyl", "52663-77-1",  1.5,   0.4 ) ! C12HCl9  PCB-208
    CALL CalcH("decachlorobiphenyl",                        "2051-24-3",  1.5,   0.2 ) ! C12Cl10  PCB-209

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC_PCR, HLC_PLSR)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC_PCR, HLC_PLSR
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      CALL MakeNote(TRIM(ref)//"PCR", "Calculated with the " // &
        "principal component regression (PCR) method, see " &
        //TRIM(citet())//" for details.")
      Hominus    = KHpcSI_TIMES_HcpSI/HLC_PCR
      CALL Output(Hominus)

      CALL MakeNote(TRIM(ref)//"PLSR", "Calculated with the " // &
        "partial least-square regression (PLSR) method, see " &
        //TRIM(citet())//" for details.")
      Hominus    = KHpcSI_TIMES_HcpSI/HLC_PLSR
      CALL Output(Hominus)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2932

  !---------------------------------------------------------------------------

  SUBROUTINE ref2933 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "2933"
    type = "M"
    chem = "carbon oxide sulfide" ; casrn = "463-58-1" ! OCS carbonyl sulfide
    ! since the authors compare their number to that from Winkler, I assume
    ! that they use the same definition (Bunsen coefficient alpha)
    CALL Output(0.54*alpha_TO_HcpSI)

  END SUBROUTINE ref2933

  !---------------------------------------------------------------------------

  ! ref2934 only citation of data from a book

  !---------------------------------------------------------------------------

  SUBROUTINE ref2935 ! KHpcSI [kPa*m3/mol]
    IMPLICIT NONE

    ref = "2935"
    type = "M"

    CALL CalcH("hexane",                     "110-54-3", 66.0846,  163.8118 ) ! C6H{14}
    CALL CalcH("heptane",                    "142-82-5", 89.0841,  220.8251 ) ! C7H{16}
    CALL CalcH("octane",                     "111-65-9", 120.1987, 297.9508 ) ! C8H{18}
    CALL CalcH("nonane",                     "111-84-2", 185.4385, 459.6685 ) ! C9H{20}
    CALL CalcH("benzene",                     "71-43-2", 0.2345,   0.5806   ) ! C6H6
    CALL CalcH("methylbenzene",              "108-88-3", 0.2721,   0.6748   ) ! C6H5CH3 toluene
    CALL CalcH("1,4-dimethylbenzene",        "106-42-3", 0.2818,   0.6991   ) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("ethylbenzene",               "100-41-4", 0.3277,   0.9432   ) ! C6H5C2H5
    CALL CalcH("butylbenzene",               "104-51-8", 0.5485,   1.3578   ) ! C6H5C4H9
    CALL CalcH("pentylbenzene",              "538-68-1", 0.6568,   1.6282   ) ! C6H5C5H{11}
    CALL CalcH("chlorobenzene",              "108-90-7", 0.2175,   0.5370   ) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",         "95-50-1", 0.0874,   0.2127   ) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,2,4-trichlorobenzene",     "120-82-1", 0.1513,   0.3752   ) ! C6H3Cl3
    CALL CalcH("1,2,3,4-tetrachlorobenzene", "634-66-2", 0.1165,   0.2890   ) ! C6H2Cl4
    CALL CalcH("dichloromethane",             "75-09-2", 0.0985,   0.2441   ) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",            "67-66-3", 0.1496,   0.3708   ) ! CHCl3 chloroform
    CALL CalcH("tetrachloromethane",          "56-23-5", 1.0540,   2.6120   ) ! CCl4 carbontetrachloride
    CALL CalcH("($Z$)-1,2-dichloroethene",   "156-59-2", 0.2721,   0.6745   ) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("($E$)-1,2-dichloroethene",   "156-60-5", 0.2561,   0.6348   ) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",             "79-01-6", 0.4256,   1.0550   ) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",          "127-18-4", 0.7582,   1.8794   ) ! C2Cl4 tetrachloroethylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KAW, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KAW, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/(1E3*H)
      CALL consistency_check(Hominus, KHcc_TIMES_HcpSI_atT0/KAW, &
        "Different types of Henry's law constants")
      CALL Output(Hominus)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2935

  !---------------------------------------------------------------------------

  SUBROUTINE ref2936 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2936"
    type = "M"

    ndata = 9
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 273.15, 278.15, 283.15, 288.15, 293.15, 298.15, 303.15, &
      308.15, 313.15 /)
    CALL CalcH("cyclooctane", "292-64-8", & ! C8H{16}
      (/ 157., 231., 329., 455., 614., 807., 1040., 1300., 1600. /), &
      (/ 1.25, 1.80, 2.52, 3.43, 4.55, 5.88, 7.47, 9.20, 11.2 /))
    CALL CalcH("cyclooctene", "931-88-4", & ! C8H{14}
      (/ 65.8, 90.3, 122., 161., 210., 269., 340., 423., 521. /), &
      (/ 0.522, 0.704, 0.931, 1.21, 1.55, 1.96, 2.44, 2.99, 3.64 /))
    CALL CalcH("ethylcyclohexane", "1678-91-7", & ! C8H{16}
      (/ 579., 833., 1160., 1580., 2100., 2720., 3450., 4280., 5210. /), &
      (/ 4.59, 6.49, 8.88, 11.9, 15.6, 19.8, 24.8, 30.3, 36.4 /))
    CALL CalcH("{cis}-1,2-dimethylcyclohexane", "2207-01-4", & ! C6H{10}(CH3)2
      (/ 401., 587., 833., 1150., 1540., 2010., 2570., 3210., 3930. /), &
      (/ 3.18, 4.57, 6.37, 8.66, 11.4, 14.7, 18.5, 22.7, 27.5 /))
    CALL CalcH("{trans}-1,2-dimethylcyclohexane", "6876-23-9", & ! C6H{10}(CH3)2
      (/ 736., 1060., 1470., 2000., 2640., 3400., 4290., 5290., 6400. /), &
      (/ 5.84, 8.23, 11.3, 15.0, 19.5, 24.8, 30.8, 37.4, 44.7 /))
    CALL CalcH("2,2-dimethylhexane", "590-73-8", & ! C8H{18}
      (/ 3810., 5940., 8830., 12600., 17200., 22600., 28700., 35300., 41900. /), &
      (/ 30.2, 46.2, 67.6, 94.6, 127., 165., 206., 249., 293. /))
    CALL CalcH("2,5-dimethylhexane", "592-13-2", & ! C8H{18}
      (/ 4260., 6560., 9590., 13400., 17800., 22700., 27800., 32900., 37500. /), &
      (/ 33.8, 51.1, 73.4, 101., 132., 165., 200., 232., 262. /))
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH_MPa, Kaw)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KH_MPa, Kaw
      INTEGER :: i
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = KHcc_TO_HcpSI(Kaw,temp)
      DO i = 1, SIZE(KH_MPa)
        CALL consistency_check(Harray(i), cH2O/(1E6*KH_MPa(i)), &
          "Different types of Henry's law constants")
      ENDDO
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2936

  !---------------------------------------------------------------------------

  SUBROUTINE ref2937 ! KHpc [atm*cm3/mol]
    IMPLICIT NONE

    ref = "2937"
    type = "M"

    CALL CalcH("benzene",              "71-43-2", 0.26,   6.4E3 ) ! C6H6
    CALL CalcH("methylbenzene",       "108-88-3", 0.26,   6.4E3 ) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",        "100-41-4", 0.16,   4.0E3 ) ! C6H5C2H5
    CALL CalcH("1,2-dimethylbenzene",  "95-47-6", 0.15,   3.6E3 ) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,4-dimethylbenzene", "106-42-3", 0.24,   5.8E3 ) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("naphthalene",          "91-20-3", 0.019,  4.6E2 ) ! C{10}H8
    CALL CalcH("acenaphthene",         "83-32-9", 3.7E-3, 91.   ) ! C{12}H{10}
    CALL CalcH("anthracene",          "120-12-7", 3.5E-3, 86.   ) ! C{14}H{10}
    CALL CalcH("phenanthrene",         "85-01-8", 1.6E-3, 40.   ) ! C{14}H{10}
    CALL CalcH("benz[$a$]anthracene",  "56-55-3", 4.1E-5, 1.0   ) ! C{18}H{12}
    CALL CalcH("chrysene",            "218-01-9", 4.3E-5, 1.05  ) ! C{18}H{12}

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, K2, K_H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: K2, K_H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1E6 / (atm*K_H)
      CALL consistency_check(Hominus, KHcc_TIMES_HcpSI_atT0/K2, &
        "Different types of Henry's law constants")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2937

  !---------------------------------------------------------------------------

  SUBROUTINE ref2938 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2938"
    type = "M"

    CALL CalcH("benzene",                   "71-43-2", 31.0, 225., "298", 298.15) ! C6H6
    CALL CalcH("methylbenzene",            "108-88-3", 35.4, 258., "298", 298.15) ! C6H5CH3 toluene
    CALL CalcH("ethenylbenzene",           "100-42-5", 16.5, 120., "298", 298.15) ! C8H8 styrene
    CALL CalcH("1,2-dimethylbenzene",       "95-47-6", 25.8, 188., "298", 298.15) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",      "108-38-3", 36.6, 266., "298", 298.15) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",      "106-42-3", 37.7, 274., "298", 298.15) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("trichloromethane",          "67-66-3", 18.4, 136., "293", 293.15) ! CHCl3 chloroform
    CALL CalcH("1,1,2-trichloroethane",     "79-00-5", 3.65, 27.0, "293", 293.15) ! CHCl2CH2Cl
    CALL CalcH("1,2-dichloropropane",       "78-87-5", 12.6, 93.3, "293", 293.15) ! C3H6Cl2
    CALL CalcH("tetrachloroethene",        "127-18-4", 64.4, 477., "293", 293.15) ! C2Cl4 tetrachloroethylene
    CALL CalcH("1-chlorobutane",           "109-69-3", 82.5, 611., "293", 293.15) ! C4H9Cl
    CALL CalcH("chlorobenzene",            "108-90-7", 15.4, 114., "293", 293.15) ! C6H5Cl
    CALL CalcH("dibromomethane",            "74-95-3", 3.83, 28.4, "293", 293.15) ! CH2Br2
    CALL CalcH("1,2-dibromoethane",        "106-93-4", 2.89, 21.4, "293", 293.15) ! C2H4Br2 ethylene dibromide
    CALL CalcH("diisopropyl ether",        "108-20-3", 11.8, 85.9, "298", 298.15) ! C3H7OC3H7
    CALL CalcH("methyl tert-pentyl ether", "994-05-8", 5.37, 39.1, "298", 298.15) ! C6H{14}O {tert}-amyl methyl ether

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH_MPa, Kaw, temp_string, temp)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KH_MPa, Kaw
      CHARACTER(LEN=*), INTENT(IN) :: temp_string
      REAL,             INTENT(IN) :: temp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TO_HcpSI(Kaw/1E3,temp)
      CALL consistency_check(Hominus, cH2O/(1E6*KH_MPa), &
        "Different types of Henry's law constants")
      IF (temp_string/="298") CALL MakeNoteOtherTemp(temp_string)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2938

  !---------------------------------------------------------------------------

  SUBROUTINE ref2939 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2939"
    type = "M"

    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 298., 353. /)
    CALL CalcH("1,1-dichloro-1-fluoroethane",        "1717-00-6", (/ 0.030, 0.0070 /), -23.) ! CH3CCl2F
    CALL CalcH("1-chloro-1,1-difluoroethane",          "75-68-3", (/ 0.015, 0.0038 /), -22.) ! CH3CClF2
    CALL CalcH("2,2-dichloro-1,1,1-trifluoroethane",  "306-83-2", (/ 0.023, 0.0065 /), -21.) ! CF3CHCl2
    CALL CalcH("1-chloro-1,2,2,2-tetrafluoroethane", "2837-89-0", (/ 0.011, 0.0026 /), -23.) ! CF3CHClF
    CALL CalcH("CF3CF2CHCl2",                         "422-56-0", (/ 0.010, 0.0016 /), -30.) ! CF3CF2CHCl2
    CALL CalcH("CClF2CF2CHClF",                       "507-55-1", (/ 0.011, 0.0022 /), -25.) ! CClF2CF2CHClF
    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H_Matm, DeltaHsol)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: H_Matm
      REAL,               INTENT(IN) :: DeltaHsol
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = Hcp_TO_HcpSI * H_Matm
      CALL HTdep(temp, Harray, Hominus, mindHR)
        CALL consistency_check(mindHR, -1E3*DeltaHsol/Rgas, &
          "Temperature dependence")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2939

  !---------------------------------------------------------------------------

  SUBROUTINE ref2940 ! Hcc [1]
    IMPLICIT NONE

    ref = "2940"
    type = "M"

    CALL CalcH("trichloromethane",      "67-66-3",  3.66) ! CHCl3 chloroform
    CALL CalcH("bromodichloromethane",  "75-27-4",  7.43) ! CHCl2Br
    CALL CalcH("dibromochloromethane", "124-48-1", 11.83) ! CHClBr2
    CALL CalcH("tribromomethane",       "75-25-2", 24.71) ! CHBr3 bromoform

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KWA)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: KWA
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hcc_TO_HcpSI(KWA, 37.+CtoK)
      CALL MakeNoteOtherTemp("310") ! 37 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2940

  !---------------------------------------------------------------------------

  ! ref2941 only high temp, T>=313 K

  !---------------------------------------------------------------------------

  SUBROUTINE ref2942 ! KHpx [atm]
    IMPLICIT NONE

    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    ref = "2942"
    type = "M"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10., 15., 20., 25., 30. /) + CtoK
    Harray = KHpx_TIMES_HcpSI / (/ 144., 182., 236., 302., 377. /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2942

  !---------------------------------------------------------------------------

  ! ref2943 only solubility; unclear what units of Fig. 4 are

  !---------------------------------------------------------------------------

  SUBROUTINE ref2944 ! KHpx [kPa]
    IMPLICIT NONE
    REAL :: KH_kPa

    chem = "aminobenzene" ; casrn = "62-53-3" ! C6H7N aniline
    ref = "2944"
    type = "M"
    KH_kPa = 12.38 * 0.0042 / 0.0010
    ! 12.38 * 0.0042 / 0.0010 => 51996.
    ! 12.45 * 0.0115 / 0.0030 => 47725.
    ! 12.53 * 0.0180 / 0.0050 => 45108.
    ! 12.57 * 0.0241 / 0.0070 => 43276.7142857
    ! 12.63 * 0.0275 / 0.0100 => 34732.5
    Hominus = cH2O/(1E3*KH_kPa)
    CALL MakeNoteOtherTemp("323")
    CALL Output(Hominus)

  END SUBROUTINE ref2944

  !---------------------------------------------------------------------------

  SUBROUTINE ref2945 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2945"
    type = "M"
    chem = "2,3,7,8-tetrachlorodibenzofuran" ; casrn = "51207-31-9" ! PCDF-2378
    Hominus = KHpcSI_TIMES_HcpSI/1.7
    CALL Output(Hominus)

  END SUBROUTINE ref2945

  !---------------------------------------------------------------------------

  SUBROUTINE ref2946 ! KHpc [mmHg*L/mol]
    IMPLICIT NONE

    ref = "2946"
    type = "V"
    chem = "2,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin" ; casrn = "1746-01-6" ! C{12}H4Cl4O2 PCDD-2378
    Hominus = 1. / (12.*mmHg*dm3)
    CALL Output(Hominus)

  END SUBROUTINE ref2946

  !---------------------------------------------------------------------------

  SUBROUTINE ref2947 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2947"
    type = "V"
    chem = "2,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin" ; casrn = "1746-01-6" ! C{12}H4Cl4O2 PCDD-2378
    Hominus = KHpcSI_TIMES_HcpSI/(2.1E-6*atm)
    CALL Output(Hominus)

  END SUBROUTINE ref2947

  !---------------------------------------------------------------------------

  SUBROUTINE ref2948 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2948"
    type = "L"

    CALL CalcH("hexachlorobenzene",     "118-74-1", 52.,   65.,   47.70, 50.49 ) ! C6Cl6
    CALL CalcH("pentachlorobenzene",    "608-93-5", 74.,   72.,   40.60, 43.83 ) ! C6HCl5
    CALL CalcH("p,p'-DDT",               "50-29-3", 1.1,   1.1                 ) ! C{14}H9Cl5 DDT; p,p'-DDT
    CALL CalcH("p,p'-DDE",               "72-55-9", 4.2,   4.2                 ) ! C{14}H8Cl4 p,p'-DDE
    CALL CalcH("p,p'-DDD",               "72-54-8", 0.67,  0.50                ) ! C{14}H{10}Cl4 p,p'-DDD
    CALL CalcH("{cis}-chlordane",      "5103-71-9", 6.0,   5.7                 ) ! C{10}H6Cl8 $\alpha$-chlordane
    CALL CalcH("{trans}-chlordane",    "5103-74-2", 6.0,   6.8                 ) ! C{10}H6Cl8 $\beta$-chlordane; $\gamma$-chlordane
    CALL CalcH("heptachlor",             "76-44-8", 30.,   38.                 ) ! C{10}H5Cl7
    CALL CalcH("heptachlorepoxide",    "1024-57-3", 2.1,   1.7                 ) ! C{10}H5Cl7O
    CALL CalcH("aldrin",                "309-00-2", 15.,   23.                 ) ! C{12}H8Cl6
    CALL CalcH("dieldrin",               "60-57-1", 1.0,   1.1                 ) ! C{12}H8OCl6
    CALL CalcH("endrin",                 "72-20-8", 0.64,  1.1                 ) ! C{12}H8Cl6O
    CALL CalcH("$\alpha$-endosulfan",   "959-98-8", 0.72,  0.70                ) ! C9H6Cl6O3S endosulfan I
    CALL CalcH("$\beta$-endosulfan",  "33213-65-9", 0.040, 0.045               ) ! C9H6Cl6O3S endosulfan II

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H_LDV, H_FAV, Delta_UAW_LDV, Delta_UAW_FAV)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H_LDV, H_FAV
      REAL, OPTIONAL,   INTENT(IN) :: Delta_UAW_LDV, Delta_UAW_FAV
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/H_LDV
      CALL MakeNote("LDV", "Literature-derived value.")
      IF (PRESENT(Delta_UAW_LDV)) THEN
        mindHR = 1E3*Delta_UAW_LDV/Rgas + T0 ! eqn (5) on p. 1585 of ref2540
        CALL Output(Hominus, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF
      Hominus    = KHpcSI_TIMES_HcpSI/H_FAV
      CALL MakeNote("FAV", "Final adjusted value.")
      IF (PRESENT(Delta_UAW_FAV)) THEN
        mindHR = 1E3*Delta_UAW_FAV/Rgas + T0 ! eqn (5) on p. 1585 of ref2540
        CALL Output(Hominus, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2948

  !---------------------------------------------------------------------------

  SUBROUTINE ref2949 ! KHpc [kPa*m3/mol]
    IMPLICIT NONE
    ref = "2949"
    type = "Q"

    CALL CalcH("dibenzo[$b$,$e$][1,4]dioxin",                             "262-12-4", 1.8,  1.96) ! C{12}H8O2 dibenzo-$p$-dioxin
    CALL CalcH("1-chlorodibenzo[$b$,$e$][1,4]dioxin",                   "39227-53-7", 2.11, 2.24) ! C{12}H7ClO2 PCDD-1
    CALL CalcH("2-chlorodibenzo[$b$,$e$][1,4]dioxin",                   "39227-54-8", 2.12, 2.34) ! C{12}H7ClO2 PCDD-2
    CALL CalcH("1,2-dichlorodibenzo[$b$,$e$][1,4]dioxin",                 "_PCDD-12", 2.44, 2.51) ! C{12}H6Cl2O2 PCDD-12
    CALL CalcH("1,3-dichlorodibenzo[$b$,$e$][1,4]dioxin",               "50585-39-2", 2.35, 2.58) ! C{12}H6Cl2O2 PCDD-13
    CALL CalcH("1,4-dichlorodibenzo[$b$,$e$][1,4]dioxin",                 "_PCDD-14", 2.38, 2.50) ! C{12}H6Cl2O2 PCDD-14
    CALL CalcH("1,6-dichlorodibenzo[$b$,$e$][1,4]dioxin",               "38178-38-0", 2.4,  2.50) ! C{12}H6Cl2O2 PCDD-16
    CALL CalcH("1,7-dichlorodibenzo[$b$,$e$][1,4]dioxin",                 "_PCDD-17", 2.41, 2.56) ! C{12}H6Cl2O2 PCDD-17
    CALL CalcH("1,8-dichlorodibenzo[$b$,$e$][1,4]dioxin",                 "_PCDD-18", 2.41, 2.58) ! C{12}H6Cl2O2 PCDD-18
    CALL CalcH("1,9-dichlorodibenzo[$b$,$e$][1,4]dioxin",                 "_PCDD-19", 2.42, 2.73) ! C{12}H6Cl2O2 PCDD-19
    CALL CalcH("2,3-dichlorodibenzo[$b$,$e$][1,4]dioxin",               "29446-15-9", 2.42, 2.60) ! C{12}H6Cl2O2 PCDD-23
    CALL CalcH("2,7-dichlorodibenzo[$b$,$e$][1,4]dioxin",               "33857-26-0", 2.41, 2.55) ! C{12}H6Cl2O2 PCDD-27
    CALL CalcH("2,8-dichlorodibenzo[$b$,$e$][1,4]dioxin",               "38964-22-6", 2.41, 2.64) ! C{12}H6Cl2O2 PCDD-28
    CALL CalcH("1,2,3-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-123", 2.7,  2.75) ! C{12}H5Cl3O2 PCDD-123
    CALL CalcH("1,2,4-trichlorodibenzo[$b$,$e$][1,4]dioxin",            "39227-58-2", 2.64, 2.74) ! C{12}H5Cl3O2 PCDD-124
    CALL CalcH("1,2,6-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-126", 2.7,  2.71) ! C{12}H5Cl3O2 PCDD-126
    CALL CalcH("1,2,7-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-127", 2.71, 2.66) ! C{12}H5Cl3O2 PCDD-127
    CALL CalcH("1,2,8-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-128", 2.71, 2.78) ! C{12}H5Cl3O2 PCDD-128
    CALL CalcH("1,2,9-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-129", 2.72, 2.96) ! C{12}H5Cl3O2 PCDD-129
    CALL CalcH("1,3,6-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-136", 2.62, 2.80) ! C{12}H5Cl3O2 PCDD-136
    CALL CalcH("1,3,7-trichlorodibenzo[$b$,$e$][1,4]dioxin",            "67028-17-5", 2.63, 2.83) ! C{12}H5Cl3O2 PCDD-137
    CALL CalcH("1,3,8-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-138", 2.63, 2.75) ! C{12}H5Cl3O2 PCDD-138
    CALL CalcH("1,3,9-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-139", 2.64, 3.01) ! C{12}H5Cl3O2 PCDD-139
    CALL CalcH("1,4,6-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-146", 2.64, 2.97) ! C{12}H5Cl3O2 PCDD-146
    CALL CalcH("1,4,7-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-147", 2.65, 2.78) ! C{12}H5Cl3O2 PCDD-147
    CALL CalcH("1,7,8-trichlorodibenzo[$b$,$e$][1,4]dioxin",             "_PCDD-178", 2.69, 2.79) ! C{12}H5Cl3O2 PCDD-178
    CALL CalcH("2,3,7-trichlorodibenzo[$b$,$e$][1,4]dioxin",            "33857-28-2", 2.69, 2.75) ! C{12}H5Cl3O2 PCDD-237
    CALL CalcH("1,2,3,4-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "30746-58-8", 2.94, 2.87) ! C{12}H4Cl4O2 PCDD-1234
    CALL CalcH("1,2,3,6-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1236", 2.94, 2.91) ! C{12}H4Cl4O2 PCDD-1236
    CALL CalcH("1,2,3,7-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "67028-18-6", 2.94, 2.83) ! C{12}H4Cl4O2 PCDD-1237
    CALL CalcH("1,2,3,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "53555-02-5", 2.94, 2.85) ! C{12}H4Cl4O2 PCDD-1238
    CALL CalcH("1,2,3,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1239", 2.96, 3.14) ! C{12}H4Cl4O2 PCDD-1239
    CALL CalcH("1,2,4,6-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1246", 2.91, 3.15) ! C{12}H4Cl4O2 PCDD-1246
    CALL CalcH("1,2,4,7-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1247", 2.89, 2.85) ! C{12}H4Cl4O2 PCDD-1247
    CALL CalcH("1,2,4,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1248", 2.89, 2.95) ! C{12}H4Cl4O2 PCDD-1248
    CALL CalcH("1,2,4,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1249", 2.91, 3.17) ! C{12}H4Cl4O2 PCDD-1249
    CALL CalcH("1,2,6,7-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1267", 2.98, 2.76) ! C{12}H4Cl4O2 PCDD-1267
    CALL CalcH("1,2,6,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1268", 2.91, 2.95) ! C{12}H4Cl4O2 PCDD-1268
    CALL CalcH("1,2,6,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1269", 2.94, 3.15) ! C{12}H4Cl4O2 PCDD-1269
    CALL CalcH("1,2,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "34816-53-0", 2.89, 2.83) ! C{12}H4Cl4O2 PCDD-1278
    CALL CalcH("1,2,7,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1279", 2.92, 3.08) ! C{12}H4Cl4O2 PCDD-1279
    CALL CalcH("1,2,8,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1289", 2.99, 3.13) ! C{12}H4Cl4O2 PCDD-1289
    CALL CalcH("1,3,6,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "33423-92-6", 2.83, 2.94) ! C{12}H4Cl4O2 PCDD-1368
    CALL CalcH("1,3,6,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1369", 2.87, 3.22) ! C{12}H4Cl4O2 PCDD-1369
    CALL CalcH("1,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "50585-46-1", 2.89, 2.90) ! C{12}H4Cl4O2 PCDD-1378
    CALL CalcH("1,3,7,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "62470-53-5", 2.85, 3.23) ! C{12}H4Cl4O2 PCDD-1379
    CALL CalcH("1,4,6,9-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1469", 2.9,  3.42) ! C{12}H4Cl4O2 PCDD-1469
    CALL CalcH("1,4,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "_PCDD-1478", 2.91, 2.96) ! C{12}H4Cl4O2 PCDD-1478
    CALL CalcH("2,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",         "1746-01-6", 2.95, 2.79) ! C{12}H4Cl4O2 PCDD-2378
    CALL CalcH("1,2,3,4,6-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-12346", 3.18, 3.25) ! C{12}H3Cl5O2 PCDD-12346
    CALL CalcH("1,2,3,4,7-pentachlorodibenzo[$b$,$e$][1,4]dioxin",      "39227-61-7", 3.16, 2.91) ! C{12}H3Cl5O2 PCDD-12347
    CALL CalcH("1,2,3,6,7-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-12367", 3.19, 2.89) ! C{12}H3Cl5O2 PCDD-12367
    CALL CalcH("1,2,3,6,8-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-12368", 3.12, 2.98) ! C{12}H3Cl5O2 PCDD-12368
    CALL CalcH("1,2,3,7,8-pentachlorodibenzo[$b$,$e$][1,4]dioxin",      "40321-76-4", 3.18, 2.83) ! C{12}H3Cl5O2 PCDD-12378
    CALL CalcH("1,2,4,6,7-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-12467", 3.16, 3.16) ! C{12}H3Cl5O2 PCDD-12467
    CALL CalcH("1,2,4,6,8-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-12468", 3.09, 3.33) ! C{12}H3Cl5O2 PCDD-12468
    CALL CalcH("1,2,4,6,9-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-12469", 3.12, 3.56) ! C{12}H3Cl5O2 PCDD-12469
    CALL CalcH("1,2,4,7,8-pentachlorodibenzo[$b$,$e$][1,4]dioxin",      "58802-08-7", 3.13, 2.96) ! C{12}H3Cl5O2 PCDD-12478
    CALL CalcH("1,3,4,6,7-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-13467", 3.14, 3.28) ! C{12}H3Cl5O2 PCDD-13467
    CALL CalcH("1,3,4,6,8-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-13468", 3.08, 3.25) ! C{12}H3Cl5O2 PCDD-13468
    CALL CalcH("1,4,6,7,8-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-14678", 3.14, 3.29) ! C{12}H3Cl5O2 PCDD-14678
    CALL CalcH("2,3,4,6,7-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-23467", 3.19, 3.15) ! C{12}H3Cl5O2 PCDD-23467
    CALL CalcH("2,3,4,6,8-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "_PCDD-23468", 3.12, 3.19) ! C{12}H3Cl5O2 PCDD-23468
    CALL CalcH("1,2,3,4,6,7-hexachlorodibenzo[$b$,$e$][1,4]dioxin",   "_PCDD-123467", 3.4,  3.19) ! C{12}H2Cl6O2 PCDD-123467
    CALL CalcH("1,2,3,4,6,8-hexachlorodibenzo[$b$,$e$][1,4]dioxin",   "_PCDD-123468", 3.34, 3.26) ! C{12}H2Cl6O2 PCDD-123468
    CALL CalcH("1,2,3,4,6,9-hexachlorodibenzo[$b$,$e$][1,4]dioxin",   "_PCDD-123469", 3.36, 3.60) ! C{12}H2Cl6O2 PCDD-123469
    CALL CalcH("1,2,3,4,7,8-hexachlorodibenzo[$b$,$e$][1,4]dioxin",     "39227-28-6", 3.37, 2.84) ! C{12}H2Cl6O2 PCDD-123478
    CALL CalcH("1,2,3,6,7,8-hexachlorodibenzo[$b$,$e$][1,4]dioxin",     "57653-85-7", 3.38, 2.84) ! C{12}H2Cl6O2 PCDD-123678
    CALL CalcH("1,2,4,6,7,8-hexachlorodibenzo[$b$,$e$][1,4]dioxin",   "_PCDD-124678", 3.34, 3.23) ! C{12}H2Cl6O2 PCDD-124678
    CALL CalcH("1,2,4,6,7,9-hexachlorodibenzo[$b$,$e$][1,4]dioxin",     "39227-62-8", 3.32, 3.55) ! C{12}H2Cl6O2 PCDD-124679
    CALL CalcH("1,3,4,6,7,8-hexachlorodibenzo[$b$,$e$][1,4]dioxin",   "_PCDD-134678", 3.34, 3.25) ! C{12}H2Cl6O2 PCDD-134678
    CALL CalcH("1,3,4,6,7,9-hexachlorodibenzo[$b$,$e$][1,4]dioxin",   "_PCDD-134679", 3.3,  3.64) ! C{12}H2Cl6O2 PCDD-134679
    CALL CalcH("2,3,4,6,7,8-hexachlorodibenzo[$b$,$e$][1,4]dioxin",   "_PCDD-234678", 3.38, 3.08) ! C{12}H2Cl6O2 PCDD-234678
    CALL CalcH("1,2,3,4,6,7,8-heptachlorodibenzo[$b$,$e$][1,4]dioxin",  "35822-46-9", 3.56, 3.08) ! C{12}HCl7O2 PCDD-1234678
    CALL CalcH("1,2,3,4,6,7,9-heptachlorodibenzo[$b$,$e$][1,4]dioxin",  "58200-70-7", 3.53, 3.51) ! C{12}HCl7O2 PCDD-1234679
    CALL CalcH("octachlorodibenzo[$b$,$e$][1,4]dioxin",                  "3268-87-9", 3.72, 3.29) ! C{12}Cl8O2 PCDD-12346789

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H_GCRI, H_SOFA)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H_GCRI, H_SOFA
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/(1E3*10.**(-H_GCRI))
      CALL MakeNote(TRIM(ref)//"GCRI", "Based on gas chromatograph retention indices (GC-RIs).")
      CALL Output(Hominus)
      ! SOFA values are taken from ref1112 and not repeated here:
      ! Hominus    = KHpcSI_TIMES_HcpSI/(1E3*10.**(-H_SOFA))
      ! CALL MakeNote(TRIM(ref)//"SOFA", "Solubility parameters for fate analysis (SOFA) method.")
      ! CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2949

  !---------------------------------------------------------------------------

  SUBROUTINE ref2950 ! KHcc [1]
    IMPLICIT NONE

    ref = "2950"
    type = "M"

    CALL CalcH("benzene",              "71-43-2", 0.18) ! C6H6
    CALL CalcH("methylbenzene",       "108-88-3", 0.20) ! C6H5CH3 toluene
    CALL CalcH("chlorobenzene",       "108-90-7", 0.12) ! C6H5Cl
    CALL CalcH("bromobenzene",        "108-86-1", 0.08) ! C6H5Br
    CALL CalcH("1,3-dichlorobenzene", "541-73-1", 0.11) ! C6H4Cl2 $m$-dichlorobenzene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Kaw)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: Kaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / Kaw
      CALL MakeNoteOtherTemp("295")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2950

  !---------------------------------------------------------------------------

  SUBROUTINE ref2951 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2951"
    type = "M"

    CALL CalcH("tetrachloroethene",     "127-18-4", 9.703, 4308.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a, b)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: a, b
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = 1. / (atm*EXP(a-b/T0))
      mindHR = b
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2951

  !---------------------------------------------------------------------------

  SUBROUTINE ref2952 ! KHcc [1]
    IMPLICIT NONE

    ref = "2952"
    type = "M"

    CALL CalcH("trichlorofluoromethane", "75-69-4", (/10.,15.,20.,25./)+CtoK, (/2.60,3.10,3.40,3.60/) ) ! CFCl3 R11
    CALL CalcH("tetrachloromethane",     "56-23-5", (/10.,15.,20.,25./)+CtoK, (/0.75,0.90,1.05,1.30/) ) ! CCl4 carbontetrachloride
    CALL CalcH("iodomethane",            "74-88-4", (/10.,15.,20.,25./)+CtoK, (/0.12,0.15,0.17,0.20/) ) ! CH3I methyl iodide
    CALL CalcH("trichloromethane",       "67-66-3", (/20.,25./)+CtoK,         (/0.112,0.166/) )         ! CHCl3 chloroform

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(H)
      ALLOCATE(Harray(ndata))
      CALL HTdep(temp_, KHcc_TO_HcpSI(H,temp_), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2952

  !---------------------------------------------------------------------------

  SUBROUTINE ref2953 ! KHcc [1]
    IMPLICIT NONE

    ref = "2953"
    type = "M"

    ALLOCATE(temp(9))
    temp = (/ 2., 6., 10., 18., 25., 30., 40., 50., 60. /) + CtoK
    CALL CalcH("trichloromethane",     "67-66-3", &
      (/ 0.0472, 0.0652, 0.0770, 0.1265, 0.1644, 0.2089, 0.2957, 0.4169, 0.5405 /)         ) ! CHCl3 chloroform
    CALL CalcH("1,1-dichloroethane",   "75-34-3", &
      (/ 0.076,  0.1036, 0.1206, 0.1869, 0.2390, 0.3019, 0.4066, 0.5480, 0.6885 /)         ) ! CHCl2CH3
    CALL CalcH("trichloroethene",      "79-01-6", &
      (/ 0.1076, 0.1502, 0.1889, 0.2966, 0.4013, 0.5188, 0.7187, 1.0293, 1.3002 /)         ) ! C2HCl3 trichloroethylene
    ! cited from ref1107: 1,2-dichloropropane (/ 0.0360, 0.0470, 0.0570, 0.0840, 0.1120, 0.1356, 0.2016, 0.2828, 0.3660, 0.4299 /)
    ! cited from ref1107: chlorobenzene (/ 0.0490, 0.0670, 0.0780, 0.1170, 0.1550, 0.1876, 0.2903, 0.4016, 0.5127, 0.6325 /)
    CALL CalcH("benzene",              "71-43-2", &
      (/ 0.0753, 0.1015, 0.1186, 0.1823, 0.2313, 0.2936, 0.3966, 0.5420, 0.6656 /)         ) ! C6H6
    CALL CalcH("methylbenzene",       "108-88-3", &
      (/ 0.0740, 0.1019, 0.1295, 0.1970, 0.2612, 0.3263, 0.4663, 0.6544, 0.8254 /)         ) ! C6H5CH3 toluene
    DEALLOCATE(temp)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, KHcc_TO_HcpSI(H,temp), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2953

  !---------------------------------------------------------------------------

  SUBROUTINE ref2954 ! KHpx [Pa]
    IMPLICIT NONE

    ref = "2954"
    type = "M"

    CALL CalcH("benzene",        "71-43-2", 2.22) ! C6H6
    CALL CalcH("methylbenzene", "108-88-3", 3.99) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",  "100-41-4", 5.14) ! C6H5C2H5

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, ki)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: ki
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = cH2O / (1E7*ki)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2954

  !---------------------------------------------------------------------------

  SUBROUTINE ref2955 ! KHcc [1]
    IMPLICIT NONE

    ref = "2955"
    type = "M"

    CALL CalcH("D-limonene",       "5989-27-5", 1.57    ) ! C{10}H{16} R-(+)-limonene
    CALL CalcH("$\beta$-pinene",    "127-91-3", 2.54    ) ! C{10}H{16}
    CALL CalcH("cyclohexane",       "110-82-7", 5.07    ) ! C6H{12}
    CALL CalcH("methanol",           "67-56-1", 0.00052 ) ! CH3OH
    CALL CalcH("2-propanol",         "67-63-0", 0.00059 ) ! C3H7OH isopropanol
    CALL CalcH("decanal",           "112-31-2", 0.094   ) ! C9H{19}CHO
    CALL CalcH("trichloroethene",    "79-01-6", 0.429   ) ! C2HCl3 trichloroethylene
    CALL CalcH("dichloromethane",    "75-09-2", 0.128   ) ! CH2Cl2 methylene chloride
    CALL CalcH("methylbenzene",     "108-88-3", 0.281   ) ! C6H5CH3 toluene
    CALL CalcH("geranyl nitrile",  "5146-66-7", 0.014   ) !
    CALL CalcH("1,4-cineole",       "470-67-7", 0.0104  ) ! C{10}H{18}O 1,4-cineole
    CALL CalcH("diethyl ether",      "60-29-7", 0.00425 ) ! C2H5OC2H5
    CALL CalcH("butyl ethanoate",   "123-86-4", 0.0193  ) ! CH3COOC4H9 butyl acetate
    CALL CalcH("butanone",           "78-93-3", 0.00425 ) ! C2H5COCH3 methyl ethyl ketone; MEK

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Harray)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: Harray
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / Harray
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2955

  !---------------------------------------------------------------------------

  SUBROUTINE ref2956 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2956"
    type = "V"

    CALL CalcH("dimethyl phthalate",          "131-11-3", 1.1E-6) ! C{10}H{10}O4
    CALL CalcH("diethyl phthalate",            "84-66-2", 2.0E-8) ! C{12}H{14}O4
    CALL CalcH("dibutyl phthalate",            "84-74-2", 1.3E-6) ! C{16}H{22}O4
    CALL CalcH("dioctyl phthalate",           "117-84-0", 5.5E-6) ! C{24}H{38}O4
    CALL CalcH("di-(2-ethylhexyl)-phthalate", "117-81-7", 4.4E-7) ! C{24}H{38}O4

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

  END SUBROUTINE ref2956

  !---------------------------------------------------------------------------

  SUBROUTINE ref2957 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2957"
    type = "V"

    CALL CalcH("dimethyl phthalate",             "131-11-3", -5.40, 9.78E-3) ! DMP
    CALL CalcH("diethyl phthalate",               "84-66-2", -5.01, 2.44E-2) ! DEP
    CALL CalcH("diallyl phthalate",              "131-17-9", -4.76, 4.28E-2) ! DAP
    CALL CalcH("dipropyl phthalate",             "131-16-8", -4.64, 5.69E-2) ! DPP
    CALL CalcH("dibutyl phthalate",               "84-74-2", -4.27, 0.133  ) ! DnBP
    CALL CalcH("diisobutyl phthalate",            "84-69-5", -4.27, 0.133  ) ! DIBP
    CALL CalcH("dipropyl phthalate",             "131-16-8", -3.91, 0.302  ) ! DnPP
    CALL CalcH("butyl benzyl phthalate",          "85-68-7", -4.08, 0.205  ) ! BBP
    CALL CalcH("dihexyl phthalate",               "84-75-3", -3.53, 0.726  ) ! DHP
    CALL CalcH("diheptyl phthalate",            "3648-21-3", -3.17, 1.69   ) ! DIHpP
    CALL CalcH("dioctyl phthalate",              "117-84-0", -2.80, 3.95   ) ! DnOP
    CALL CalcH("butyl 2-ethylhexyl phthalate",    "85-69-8", -3.73, 0.466  ) ! BOP
    ! mixture, not used here                                  -2.61, 6.05     ! 610P
    CALL CalcH("di-(2-ethylhexyl)-phthalate",    "117-81-7", -2.80, 3.95   ) ! DEHP
    CALL CalcH("diisooctyl phthalate",         "27554-26-3", -2.80, 3.95   ) ! DIOP
    CALL CalcH("dinonyl phthalate",               "84-76-4", -2.43, 9.26   ) ! DnNP
    CALL CalcH("diisononyl phthalate",         "28553-12-0", -2.43, 9.26   ) ! DINP
    CALL CalcH("didecyl phthalate",               "84-77-5", -2.06, 21.6   ) ! DnDP
    CALL CalcH("diisodecyl phthalate",         "26761-40-0", -2.06, 21.6   ) ! DIDP
    ! mixture, not used here                                  -2.43, 9.26     ! D711P
    CALL CalcH("diundecyl phthalate",           "3648-20-2", -1.69, 50.5   ) ! DUP
    CALL CalcH("ditridecyl phthalate",           "119-06-2", -0.95, 275.   ) ! DTDP

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, logKAW, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: logKAW, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/H
      IF (casrn_=="131-16-8") CALL MakeNote(TRIM(ref), &
        "Dipropyl phthalate is listed twice with different values.")
      CALL consistency_check(Hominus, KHcc_TIMES_HcpSI_atT0/(10.**logKAW), &
        "$K_{\rm AW}$ and $H$")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2957

  !---------------------------------------------------------------------------

  SUBROUTINE ref2958 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2958"
    type = "V"

    CALL CalcH("dimethyl phthalate",           "131-11-3", 1.22E-07) ! DMP
    CALL CalcH("diethyl phthalate",             "84-66-2", 2.66E-07) ! DEP
    CALL CalcH("diallyl phthalate",            "131-17-9", 2.85E-07) ! DAP
    CALL CalcH("dipropyl phthalate",           "131-16-8", 3.05E-07) ! DPP
    CALL CalcH("dibutyl phthalate",             "84-74-2", 8.83E-07) ! DnBP
    CALL CalcH("diisobutyl phthalate",          "84-69-5", 1.83E-07) ! DIBP
    CALL CalcH("butyl benzyl phthalate",        "85-68-7", 7.61E-07) ! BBP
    CALL CalcH("dihexyl phthalate",             "84-75-3", 4.40E-05) ! DHP
    CALL CalcH("dioctyl phthalate",            "117-84-0", 1.03E-04) ! DnOP
    CALL CalcH("butyl 2-ethylhexyl phthalate",  "85-69-8", 4.00E-07) ! BOP
    CALL CalcH("di-(2-ethylhexyl)-phthalate",  "117-81-7", 1.71E-05) ! DEHP

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

  END SUBROUTINE ref2958

  !---------------------------------------------------------------------------

  SUBROUTINE ref2959 ! special definition

    IMPLICIT NONE
    REAL :: c, p
    ref = "2959"
    type = "V"
    chem = "bis(2-methoxyethyl) phthalate" ; casrn = "117-82-8"
    c = rhoH2O * 10. * 0.85 / 282. ! c [mol/m3]
    p = 0.01 * mmHg ! p [Pa]
    Hominus = c / p
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hominus)
    ! data for other species is not used because:
    ! - either p and c are not at the same temperature
    ! - or only upper or lower limits are given for p and/or c

  END SUBROUTINE ref2959

  !---------------------------------------------------------------------------

  SUBROUTINE ref2960 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2960"
    type = "V"

    CALL CalcH("4,5-dichlorocatechol",        "3428-24-8", 0.408, 526.7, 0.00078)
    CALL CalcH("4,5-dichloroguaiacol",        "2460-49-3", 1.54,  3.46,  0.44   )
    CALL CalcH("3,4,5-trichlorocatechol",    "56961-20-7", 0.106, 25.79, 0.0041 )
    CALL CalcH("3,4,5-trichloroguaiacol",    "57057-83-7", 0.640, 5.47,  0.12   )
    CALL CalcH("3,4,5-trichloroveratrole",   "16766-29-3", 0.415, 0.11,  3.77   )
    CALL CalcH("4,5,6-trichloroguaiacol",     "2668-24-8", 0.249, 1.84,  0.14   )
    CALL CalcH("tetrachlorocatechol",         "1198-55-6", 0.068, 1.97,  0.035  )
    CALL CalcH("tetrachloroguaiacol",         "2539-17-5", 0.138, 0.90,  0.15   )
    CALL CalcH("tetrachloroveratrole",         "944-61-6", 0.286, 0.026, 11.0   )
    CALL CalcH("3-chlorosyringol",           "18113-22-9", 0.825, 34.26, 0.024  )
    CALL CalcH("2-chlorosyringaldehyde",     "76341-69-0", 0.079, 7.45,  0.011  )
    CALL CalcH("3,5-dichlorosyringol",       "78782-46-4", 0.465, 6.71,  0.069  )
    CALL CalcH("2,6-dichlorosyringaldehyde", "76330-06-8", 0.025, 6.84,  0.0037 )
    CALL CalcH("trichlorosyringol",           "2539-26-6", 0.077, 3.5,   0.022  )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, PL, CL, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: PL, CL, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/H
      CALL consistency_check(H, PL/CL, &
        "The ratio $P_{\rm L}/C_{\rm L}$ and $H$")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2960

  !---------------------------------------------------------------------------

  SUBROUTINE ref2961 ! KHcc [1]
    IMPLICIT NONE

    ref = "2961"
    type = "M"

    CALL CalcH("methanol",    "67-56-1",  8.969, 5206.8, -3.69) ! CH3OH
    CALL CalcH("ethanol",     "64-17-5", 10.173, 5531.6, -3.64) ! C2H5OH
    CALL CalcH("1-propanol",  "71-23-8", 11.830, 5923.2, -3.49) ! C3H7OH
    CALL CalcH("1-butanol",   "71-36-3", 12.141, 5892.0, -3.31) ! C4H9OH
    CALL CalcH("1-pentanol",  "71-41-0", 14.233, 6559.6, -3.37) ! C5H{11}OH
    CALL CalcH("1-hexanol",  "111-27-3", 11.705, 5538.7, -2.98) ! C6H{14}O

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a, b, logHc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: a, b, logHc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / (EXP(a-b/T0))
      mindHR = b + T0 ! see ref958, eqn (34) why T0 is added to mindHR
      CALL consistency_check(10.**logHc, EXP(a-b/T0), &
        "Data in Table 1 and 2")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2961

  !---------------------------------------------------------------------------

  SUBROUTINE ref2963 ! Hcc [1] = Ostwald coefficient
    IMPLICIT NONE

    ref = "2963"
    type = "M"
    chem = "ethane nitrile" ; casrn = "75-05-8" ! CH3CN acetonitrile
    CALL Output(Hcc_TO_HcpSI_atT0*1.21E3)

    END SUBROUTINE ref2963

  !---------------------------------------------------------------------------

  SUBROUTINE ref2964 ! KHpx [Pa] and KHcc [1]
    IMPLICIT NONE

    ref = "2964"

    CALL CalcH("ethanal",               "75-07-0", 4.62E+05, 2.9E-03, 2.7E-03) ! CH3CHO acetaldehyde
    CALL CalcH("dimethyl sulfide",      "75-18-3", 1.31E+07, 8.1E-02, 2.5E-02) ! CH3SCH3 DMS
    CALL CalcH("2,3-butanedione",      "431-03-8", 9.11E+04, 5.7E-04, 3.9E-04) ! CH3COCOCH3 biacetyl; dimethylglycol
    CALL CalcH("2,5-dimethylpyrazine", "123-32-0", 1.01E+04, 6.3E-05, 5.7E-05)
    CALL CalcH("menthone",              "89-80-5", 1.11E+06, 6.9E-03, 7.1E-03)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, V, Q, M)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: V, Q, M
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "V"
      CALL Output(cH2O / V) ! KHpx [Pa]
      type = "Q"
      CALL Output(KHcc_TIMES_HcpSI_atT0 / Q) ! KHcc [1]
      type = "M"
      CALL Output(KHcc_TIMES_HcpSI_atT0 / M) ! KHcc [1]
    END SUBROUTINE CalcH

  END SUBROUTINE ref2964

  !---------------------------------------------------------------------------

  SUBROUTINE ref2965 ! KHcc [1]
    IMPLICIT NONE

    ref = "2965"

    CALL CalcH("alachlor",            "15972-60-8", -6.22, -6.39) ! C{14}H{20}ClNO2
    CALL CalcH("atrazine",             "1912-24-9", -6.68       ) ! C8H{14}ClN5
    CALL CalcH("chlorpyrifos",         "2921-88-2", -3.64, -3.72) ! C9H{11}Cl3NO3PS
    CALL CalcH("dacthal",              "1861-32-1", -4.04       )
    CALL CalcH("diazinon",              "333-41-5", -5.06, -5.36) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("disulfoton",            "298-04-4", -4.44       ) ! C8H{19}O2PS3
    CALL CalcH("$\alpha$-endosulfan",   "959-98-8", -3.55, -3.54) ! C9H6Cl6O3S endosulfan I
    CALL CalcH("metolachlor",         "51218-45-2", -6.27, -6.25) ! C{15}H{22}ClNO2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, LogKaw_FAV, LogKaw_LDV)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: LogKaw_FAV
      REAL, OPTIONAL,   INTENT(IN) :: LogKaw_LDV
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "L"
      CALL MakeNote("FAV", "Final adjusted value.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**LogKaw_FAV))
      IF (PRESENT(LogKaw_LDV)) THEN
        type = "L"
        CALL MakeNote("LDV", "Literature-derived value.")
        CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**LogKaw_LDV))
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2965

  !---------------------------------------------------------------------------

  SUBROUTINE ref2966 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2966"
    type = "V"

    CALL CalcH("methanol",                              "67-56-1", 0.354  ) ! CH3OH
    CALL CalcH("hydroxybenzene",                       "108-95-2", 0.095  ) ! C6H5OH phenol
    CALL CalcH("2-nitrophenol",                         "88-75-5", 1.27   ) ! HOC6H4(NO2)
    CALL CalcH("4-nitrophenol",                        "100-02-7", 0.00005) ! HOC6H4(NO2)
    CALL CalcH("(2,4-dichlorophenoxy)-ethanoic acid",   "94-75-7", 0.552  ) ! C8H6Cl2O3 (2,4-dichlorophenoxy)-acetic acid; 2,4-D
    CALL CalcH("atrazine",                            "1912-24-9", 0.0003 ) ! C8H{14}ClN5
    CALL CalcH("2,4,5-trichlorophenoxyethanoic acid",   "93-76-5", 0.0058 ) ! C8H5Cl3O3
    CALL CalcH("hydroxypentachlorobenzene",             "87-86-5", 0.044  ) ! C6HCl5O pentachlorophenol
    CALL CalcH("hexachlorobenzene",                    "118-74-1", 7.14   ) ! C6Cl6
    CALL CalcH("perylene",                             "198-55-0", 0.440  ) ! C{20}H{12} dibenz[$de$,$kl$]anthracene
    CALL CalcH("di-(2-ethylhexyl)-phthalate",          "117-81-7", 18.6   ) ! C{24}H{38}O4

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/H
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2966

  !---------------------------------------------------------------------------

  SUBROUTINE ref2967 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2967"
    type = "V"

    CALL CalcH("atrazine",                                    "1912-24-9", 1E-3  ) ! C8H{14}ClN5
    CALL CalcH("chlorpyrifos",                                "2921-88-2", 6E-1  ) ! C9H{11}Cl3NO3PS
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",    "58-89-9", 3E-1  ) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("isoproturon",                                "34123-59-6", 9E-6  ) ! C{12}H{18}N2O isoproturon
    CALL CalcH("parathion",                                     "56-38-2", 2E-2  ) ! C{10}H{14}NO5PS E 605
    CALL CalcH("pirimor",                                    "23103-98-2", 1.7E-4) ! C{11}H{18}N4O2 pirimicarb
    CALL CalcH("propiconazole",                              "60207-90-1", 4E-4  ) ! C{15}H{17}Cl2N3O2
    CALL CalcH("propoxur",                                     "114-26-1", 1.4E-4) ! C{11}H{15}NO3
    CALL CalcH("terbuthylazine",                              "5915-41-3", 4.1E-3) ! C9H{16}ClN5
    CALL CalcH("vinclozoline",                               "50471-44-8", 1.1E-2) ! C{12}H9Cl2NO3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/H
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2967

  !---------------------------------------------------------------------------

  SUBROUTINE ref2968 ! Hcc [1]
    IMPLICIT NONE

    ref = "2968"
    type = "V"

    CALL CalcH("S-ethyl dipropylthiocarbamate",                "759-94-4", 1.84E3) ! C9H{19}NOS eptam
    CALL CalcH("2,6-dichlorobenzenenitrile",                  "1194-65-6", 3.48E3) ! C6H3Cl2CN dichlobenil
    CALL CalcH("diazinon",                                     "333-41-5", 3.29E4) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",    "58-89-9", 1.96E4) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("isazophos",                                  "42509-80-8", 2.73E5)
    CALL CalcH("DDT",                                           "50-29-3", 3.26E2) ! C{14}H9Cl5 DDT; p,p'-DDT
    CALL CalcH("parathion",                                     "56-38-2", 3.3E4 ) ! C{10}H{14}NO5PS E 605
    CALL CalcH("metolachlor",                                "51218-45-2", 2.63E6) ! C{15}H{22}ClNO2
    CALL CalcH("methidathion",                                 "950-37-8", 1.45E7) ! C6H{11}N2O4PS3
    CALL CalcH("monuron",                                      "150-68-5", 4.2E7 ) ! C9H{11}ClN2O
    CALL CalcH("metalaxyl",                                  "57837-19-1", 2.11E8) ! C{15}H{21}NO4

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

  END SUBROUTINE ref2968

  !---------------------------------------------------------------------------

  SUBROUTINE ref2969 ! KHcc [1]
    IMPLICIT NONE

    ref = "2969"
    type = "M"

    CALL CalcH("2-propanol",          "67-63-0", 2.24E-3, 3.91E-1, 7.09E-4         ) ! C3H7OH isopropanol
    CALL CalcH("propanone",           "67-64-1", 4E-3,    4.29E-1, 7.65E-4         ) ! CH3COCH3 acetone
    CALL CalcH("1,2-dichloroethane", "107-06-2", 7.47E-2, 7.12E-1, 9.51E-2, 4.98E-2) ! CH2ClCH2Cl
    CALL CalcH("methylbenzene",      "108-88-3", 3.87E-1, 2.42,    5.19E-1, 1.72E-1) ! C6H5CH3 toluene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, epics, linear, direct, nonlin)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: epics, linear, direct
      REAL, OPTIONAL,   INTENT(IN) :: nonlin
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! 1) EPICS
      CALL MakeNote(TRIM(ref)//"epics", &
        "Value obtained by applying the EPICS method, see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / epics)
      ! 2) equilibrium static cell - linear
      CALL MakeNote(TRIM(ref)//"linear", &
        "Value obtained by applying the static cell (linear form) method, see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / linear)
      ! 3) direct phase concentration ratio
      CALL MakeNote(TRIM(ref)//"direct", &
        "Value obtained by applying the direct phase concentration ratio method, see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / direct)
      ! 4) equilibrium static cell - non-linear
      IF (PRESENT(nonlin)) THEN
        CALL MakeNote(TRIM(ref)//"nonlin", &
        "Value obtained by applying the static cell (non-linear form) method, see " &
        //TRIM(citet())//" for details.")
        CALL Output(KHcc_TIMES_HcpSI_atT0 / nonlin)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref2969

  !---------------------------------------------------------------------------

  SUBROUTINE ref2970 ! HcpSI [mol*m3/Pa]
    IMPLICIT NONE

    ref = "2970"
    type = "M"

    CALL CalcH("benzene",                 "71-43-2", 1.8E-3) ! C6H6
    CALL CalcH("methylbenzene",          "108-88-3", 1.5E-3) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",           "100-41-4", 1.4E-3) ! C6H5C2H5
    CALL CalcH("chlorobenzene",          "108-90-7", 2.4E-3) ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",     "95-50-1", 6.3E-3) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",    "541-73-1", 2.9E-3) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",    "106-46-7", 3.3E-3) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,2-dimethylbenzene",     "95-47-6", 2.3E-3) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",    "108-38-3", 1.4E-3) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",    "106-42-3", 1.4E-3) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("1,2,4-trimethylbenzene",  "95-63-6", 1.7E-3) ! C6H3(CH3)3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HcpSI)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: HcpSI
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = HcpSI
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2970

  !---------------------------------------------------------------------------

  SUBROUTINE ref2971 ! KHcc [1]
    IMPLICIT NONE

    ref = "2971"
    type = "V"

    ! Table 2:
    CALL CalcH("4-nitrophenol", "100-02-7", 4.3E-9) ! HOC6H4(NO2)
    CALL CalcH("chlorobenzene", "108-90-7", 1.5E-1) ! C6H5Cl
    CALL CalcH("DDT",            "50-29-3", 1.1E-3) ! C{14}H9Cl5 DDT; p,p'-DDT
    CALL CalcH("Flon 11",        "75-69-4", 4.5   ) ! CFCl3 R11

    ! Table 3:
    CALL CalcH("aminobenzene",                                  "62-53-3", 1.2E-4) ! C6H7N aniline
    CALL CalcH("2-methylbenzenamine",                           "95-53-4", 1.2E-4) ! C7H9N 2-methylaniline; $o$-toluidine
    CALL CalcH("4-methylbenzenamine",                          "106-49-0", 2.6E-4) ! C7H9N 4-methylaniline; $p$-toluidine
    CALL CalcH("nitrobenzene",                                  "98-95-3", 5.2E-4) ! C6H5NO2
    CALL CalcH("(dimethylamino)-benzene",                      "121-69-7", 2.6E-3) ! C8H{11}N N,N-dimethylaniline
    CALL CalcH("tributylphosphate",                            "126-73-8", 8.4E-5) ! C{12}H{27}O4P
    CALL CalcH("1,2,4-trichlorobenzene",                       "120-82-1", 1.6E-1) ! C6H3Cl3
    CALL CalcH("BHT",                                          "128-37-0", 1.4E-1)
    CALL CalcH("hexachlorobenzene",                            "118-74-1", 2.5E-2) ! C6Cl6
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethene",  "72-55-9", 7.9E-3) ! C{14}H8Cl4 p,p'-DDE
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethane",  "72-54-8", 8.8E-4) ! C{14}H{10}Cl4 p,p'-DDD

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0 / H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2971

  !---------------------------------------------------------------------------

  SUBROUTINE ref2972 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "2972"
    type = "M"

    CALL CalcH("carbaryl",      "63-25-2", 1E-7) ! C{12}H{11}NO2
    CALL CalcH("carbofuran",  "1563-66-2", 1E-7) ! C{12}H{15}NO3
    CALL CalcH("MCPA",          "94-74-6", 1E-7)
    CALL CalcH("hexazinone", "51235-04-2", 1E-7)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*H)
      ! upper limit of KHpc is lower limit of Hcp:
      ! CALL MakeNote(TRIM(ref), &
      !   TRIM(citet())//" found that $\Hsymbol$("//chem_//") $"//morethan// &
      !   "$ "//TRIM(H_TeX(Hominus))//".")
      CALL Output(Hominus, limit=">")
    END SUBROUTINE CalcH

  END SUBROUTINE ref2972

  !---------------------------------------------------------------------------

  SUBROUTINE ref2973 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2973"
    type = "V"

    CALL CalcH("cypermethrin",  "52315-07-8", 8E-2) ! C{22}H{19}Cl2NO3
    CALL CalcH("deltamethrin",  "52918-63-5", 5E-1) ! C{22}H{19}Br2NO3
    CALL CalcH("dichlofluanid",  "1085-98-9", 4E-5) ! C9H{11}Cl2FN2O2S2
    ! dinocap is a mixture (not used)          5E-4
    CALL CalcH("fenpropathrin", "39515-41-8", 6E-2)
    CALL CalcH("parathion",        "56-38-2", 2E-2) ! C{10}H{14}NO5PS E 605
    CALL CalcH("pirimor",       "23103-98-2", 2E-4) ! C{11}H{18}N4O2 pirimicarb

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/H
      CALL MakeNoteOtherTemp("293")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2973

  !---------------------------------------------------------------------------

  SUBROUTINE ref2974 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2974"
    type = "V"

    ! Table I:
    CALL CalcH("$\alpha$-1,2,3,4,5,6-hexachlorocyclohexane",      "319-84-6", 1.1 ) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("$\beta$-1,2,3,4,5,6-hexachlorocyclohexane",       "319-85-7", 0.07) ! C6H6Cl6 $\beta$-lindane
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",       "58-89-9", 0.1 ) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("1,1,1-trichloro-2,2-bis-(4-chlorophenyl)-ethane",  "50-29-3", 6.  )
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethene",     "72-55-9", 34. )
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethane",     "72-54-8", 9.  )
    ! Table VII:
    CALL CalcH("tetrachloromethane",                               "56-23-5", 1.5E4) ! CCl4 carbontetrachloride
    CALL CalcH("hexachloroethane",                                 "67-72-1", 4.5E3) ! C2Cl6
    CALL CalcH("hexachlorobutadiene",                              "87-68-3", 1.1E3) ! CCl2CClCClCCl2
    ! alpha-HCH is already in Table I:                             "319-84-6", 1.1  ) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("hexachlorobenzene",                               "118-74-1", 1.4E2) ! C6Cl6

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/H
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2974

  !---------------------------------------------------------------------------

  SUBROUTINE ref2975 ! KHcc [1]
    IMPLICIT NONE

    ref = "2975"
    type = "V"

    CALL CalcH("trichloromethane",                                 "67-66-3", 7.4E-2) ! CHCl3 chloroform
    CALL CalcH("1,2,4-trichlorobenzene",                          "120-82-1", 0.084 ) ! C6H3Cl3
    CALL CalcH("1,2,3,4-tetrachlorobenzene",                      "634-66-2", 0.069 ) ! C6H2Cl4
    CALL CalcH("hexachlorobenzene",                               "118-74-1", 0.016 ) ! C6Cl6
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",       "58-89-9", 1.1E-4) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethene",     "72-55-9", 5.3E-4) ! C{14}H8Cl4 p,p'-DDE
    CALL CalcH("mirex",                                          "2385-85-5", 7.0E-3) ! C{10}Cl{12} dodecachloropentacyclodecane
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl",                 "35693-99-3", 3.3E-3) ! C{12}H6Cl4 PCB-52
    CALL CalcH("benzo[$a$]pyrene",                                 "50-32-8", 3.2E-6) ! C{20}H{12} benz[$a$]pyrene
    CALL CalcH("benzo[$jk$]fluorene",                             "206-44-0", 1.9E-4) ! C{16}H{10} fluoranthene
    CALL CalcH("2,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin", "1746-01-6", 7E-4  ) ! C{12}H4Cl4O2 PCDD-2378

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("283")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2975

  !---------------------------------------------------------------------------

  SUBROUTINE ref2976 ! Hcc [1]
    IMPLICIT NONE

    ref = "2976"
    type = "V"

    CALL CalcH("DDT",                                        "50-29-3", 1518.) ! C{14}H9Cl5 DDT; p,p'-DDT
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane", "58-89-9", 1657.) ! C6H6Cl6 $\gamma$-lindane

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(Hcc_TO_HcpSI_atT0 * H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2976

  !---------------------------------------------------------------------------

  SUBROUTINE ref2977 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2977"
    type = "V"

    CALL CalcH("HCB",                                                         "118-74-1", 88. ) ! C6Cl6
    CALL CalcH("$\alpha$-1,2,3,4,5,6-hexachlorocyclohexane",                  "319-84-6", 0.43) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",                   "58-89-9", 0.17) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("1,1,1-trichloro-2,2-bis-(4-chlorophenyl)-ethane",              "50-29-3", 2.9 ) ! C{14}H9Cl5 DDT; p,p'-DDT
    CALL CalcH("1,1,1-trichloro-2-(2-chlorophenyl)-2-(4-chlorophenyl)ethane", "789-02-6", 53. ) ! C{14}H9Cl5 o,p'-DDT
    CALL CalcH("1,1-dichloro-2,2-bis-(4-chlorophenyl)-ethene",                 "72-55-9", 6.2 ) ! C{14}H8Cl4 p,p'-DDE

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI/H
      CALL MakeNoteOtherTemp("293")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2977

  !---------------------------------------------------------------------------

  SUBROUTINE ref2978 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    ref = "2978"
    type = "L"

    CALL CalcH("$\alpha$-1,2,3,4,5,6-hexachlorocyclohexane", "319-84-6", 0.652,  0.735) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("$\beta$-1,2,3,4,5,6-hexachlorocyclohexane",  "319-85-7", 0.0374, 0.037) ! C6H6Cl6 $\beta$-lindane
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",  "58-89-9", 0.272,  0.306) ! C6H6Cl6 $\gamma$-lindane

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H_LDV, H_FAV)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H_LDV, H_FAV
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpcSI_TIMES_HcpSI/H_LDV
      CALL MakeNote("LDV", "Literature-derived value.")
      CALL Output(Hominus)
      Hominus    = KHpcSI_TIMES_HcpSI/H_FAV
      CALL MakeNote("FAV", "Final adjusted value.")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2978

  !---------------------------------------------------------------------------

  SUBROUTINE ref2979 ! special definition (dyne*cm/g)
    IMPLICIT NONE

    ref = "2979"

    ! molar masses are from NIST webbook
    ! Table 2:
    CALL CalcH("tetrachloromethane",                            "56-23-5", 5.9E7,  153.823, "?", "297") ! CCl4 carbontetrachloride
    CALL CalcH("1,1,1-trichloroethane",                         "71-55-6", 4.8E7,  133.404, "?", "297") ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("tetrachloroethene",                            "127-18-4", 2.1E7,  165.833, "?", "297") ! C2Cl4 tetrachloroethylene
    CALL CalcH("1,2-dichloropropane",                           "78-87-5", 1.5E7,  112.986, "?", "297") ! C3H6Cl2
    CALL CalcH("1,2-dichlorobenzene",                           "95-50-1", 1.1E7,  147.002, "?", "297") ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,2-dichloroethane",                           "107-06-2", 8.3E6,   98.959, "?", "297") ! CH2ClCH2Cl
    CALL CalcH("1,2-dibromoethane",                            "106-93-4", 3.0E6,  187.861, "?", "297") ! C2H4Br2 ethylene dibromide
    CALL CalcH("1,1,2,2-tetrachloroethane",                     "79-34-5", 2.0E6,  167.849, "?", "297") ! CHCl2CHCl2
    ! Table 3:
    CALL CalcH("2-hydroxychlorobenzene",                        "95-57-8", 7.54E4, 128.556, "?", "297") ! C6H5ClO $o$-chlorophenol
    CALL CalcH("4-hydroxychlorobenzene",                       "106-48-9", 7.07E3, 128.556, "?", "297") ! C6H5ClO $p$-chlorophenol
    ! Table 4:
    CALL CalcH("2,2'-dichlorobiphenyl",                      "13029-08-8", 1.7E6,  223.098, "V") ! C{12}H8Cl2 PCB-4
    CALL CalcH("4,4'-dichlorobiphenyl",                       "2050-68-2", 4.5E5,  223.098, "V") ! C{12}H8Cl2 PCB-15
    CALL CalcH("$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane",    "58-89-9", 1.1E4,  290.830, "V") ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("parathion",                                     "56-38-2", 3.3E3,  291.261, "V") ! C{10}H{14}NO5PS E 605

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H, M, type_, T)
      IMPLICIT NONE
      CHARACTER(LEN=*),             INTENT(IN) :: chem_
      CHARACTER(LEN=*),             INTENT(IN) :: casrn_
      REAL,                         INTENT(IN) :: H
      REAL,                         INTENT(IN) :: M      ! molar mass (g/mol)
      CHARACTER,                    INTENT(IN) :: type_
      CHARACTER(LEN=*), OPTIONAL,   INTENT(IN) :: T ! other temperature

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI / (1E-7*H*M)
      IF (PRESENT(T)) THEN
        CALL MakeNoteOtherTemp(T)
      ENDIF
      CALL Output(Hominus)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2979

  !---------------------------------------------------------------------------

  SUBROUTINE ref2980 ! KHpx [Torr]
    IMPLICIT NONE

    chem = "cyclohexane" ; casrn = "110-82-7" ! C6H{12}
    ref = "2980"
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 15., 25., 35., 42.5 /) + CtoK
    Harray = cH2O / (mmHg * (/ 4.83E6, 7.84E6, 11.73E6, 15.13E6 /))
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref2980

  !---------------------------------------------------------------------------

  SUBROUTINE ref2981 ! Hcc [1]
    IMPLICIT NONE

    ref = "2981"
    type = "M"
    ALLOCATE(temp(9))
    temp = (/ 4.50, 6.33, 7.06, 8.96, 11.75, 12.10, 15.10, 17.93, 20.06 /) + CtoK
    CALL CalcH("benzene",        "71-43-2", (/ 12.33, 11.10, 10.45, 9.52, 8.17, 8.03, 6.91, 6.18, 5.51 /)) ! C6H6
    CALL CalcH("methylbenzene", "108-88-3", (/ 12.92, 11.42, 10.68, 9.61, 8.05, 7.92, 6.68, 5.85, 5.14 /)) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",  "100-41-4", (/ 12.31, 10.70,  9.91, 8.82, 7.20, 7.11, 5.85, 5.04, 4.36 /)) ! C6H5C2H5
    DEALLOCATE(temp)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, K)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: K
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, Hcc_TO_HcpSI(K,temp), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2981

  !---------------------------------------------------------------------------

  SUBROUTINE ref2982 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "2982"
    type = "M"

    CALL CalcH("benzene",        "71-43-2", (/ 322.00, 342.00, 362.00 /), (/ 70.7, 112.3, 160.0 /)) ! C6H6
    CALL CalcH("methylbenzene", "108-88-3", (/ 322.00, 342.00, 362.00 /), (/ 85.5, 138.8, 214.9 /)) ! C6H5CH3 toluene
    CALL CalcH("chlorobenzene", "108-90-7", (/ 323.15, 333.15, 343.15 /), (/ 43.5,  53.3,  55.1 /)) ! C6H5Cl
    CALL CalcH("bromobenzene",  "108-86-1", (/ 303.15, 323.15, 343.15 /), (/ 15.9,  32.3,  48.4 /)) ! C6H5Br

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, Hci)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, Hci
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(Hci)
      ALLOCATE(Harray(ndata))
      Harray = cH2O / (1E6*Hci)
      CALL HTdep(temp_, Harray, Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2982

  !---------------------------------------------------------------------------

  SUBROUTINE ref2983 ! KHcc [1]
    IMPLICIT NONE

    ref = "2983"
    type = "M"

    CALL CalcH("benzene",              "71-43-2", 0.229) ! C6H6
    CALL CalcH("methylbenzene",       "108-88-3", 0.274) ! C6H5CH3 toluene
    CALL CalcH("1,2-dimethylbenzene",  "95-47-6", 0.239) ! C6H4(CH3)2 $o$-xylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TO_HcpSI(H,296.)
      CALL MakeNoteOtherTemp("296")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2983

  !---------------------------------------------------------------------------

  SUBROUTINE ref2984 ! KHpx [1E7 Pa]
    IMPLICIT NONE

    ref = "2984"
    type = "M"
    ALLOCATE(temp(4))
    temp = (/ 15., 25., 35., 45. /) + CtoK
    CALL CalcH("benzene",                 "71-43-2", (/ 2.20, 3.39, 4.87, 7.04 /)) ! C6H6
    CALL CalcH("methylbenzene",          "108-88-3", (/ 2.12, 3.49, 5.44, 7.98 /)) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",           "100-41-4", (/ 2.32, 4.13, 6.73, 10.1 /)) ! C6H5C2H5
    CALL CalcH("1,3,5-trimethylbenzene", "108-67-8", (/ 3.04, 5.16, 8.34, 13.7 /)) ! C6H3(CH3)3 mesitylene
    DEALLOCATE(temp)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KH)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: KH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, cH2O/(1E7*KH), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2984

  !---------------------------------------------------------------------------

  SUBROUTINE ref2986 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2986"
    type = "M"
    chem = "carbon suboxide" ; casrn = "504-64-3"
    Hominus = (1.08+1.08+1.56+1.552)/4. * Hcp_TO_HcpSI ! pH = 2,4,6,8
    CALL MakeNote(TRIM(ref), "Average of 4 pH-dependent values.")
    CALL Output(Hominus)

  END SUBROUTINE ref2986

  !---------------------------------------------------------------------------

  SUBROUTINE ref2987 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "2987"
    type = "M"

    CALL CalcH("methylbenzene",  "108-88-3", (/ 283., 288., 293., 298. /), &
      (/ 0.38, 0.24, 0.23, 0.17 /) ) ! C6H5CH3 toluene
    CALL CalcH("benzenenitrile", "100-47-0", (/ 283., 288., 293., 298. /), &
      (/ 74.2, 52.9, 42.0, 29.0 /) ) ! C6H5CN benzonitrile

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, Hcp)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, Hcp
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, Hcp*Hcp_TO_HcpSI, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2987

  !---------------------------------------------------------------------------

  SUBROUTINE ref2989 ! KHcc [1]
    IMPLICIT NONE

    ref = "2989"
    type = "L"

    !                                                         LDV    FAV
    CALL CalcH("Nap   naphthalene",               "91-20-3", -1.72, -1.73)
    CALL CalcH("Acy   acenaphthylene",           "208-96-8", -2.31, -2.41)
    CALL CalcH("Ace   acenaphthene",              "83-32-9", -2.25, -2.24)
    CALL CalcH("Fluo  2,3-benzindene",            "86-73-7", -2.42, -2.44)
    CALL CalcH("Phe   phenanthrene",              "85-01-8", -2.75, -2.76)
    CALL CalcH("Ant   anthracene",               "120-12-7", -2.69, -2.69)
    CALL CalcH("Pyr   pyrene",                   "129-00-0", -3.27, -3.27)
    CALL CalcH("Flu   benzo[$jk$]fluorene",      "206-44-0", -3.23, -3.27)
    CALL CalcH("Chry  chrysene",                 "218-01-9", -3.75, -3.82)
    CALL CalcH("BaA   benz[$a$]anthracene",       "56-55-3", -3.55, -3.59)
    CALL CalcH("BbF   benzo[$b$]fluoranthene",   "205-99-2", -4.57, -4.58)
    CALL CalcH("BkF   benzo[$k$]fluoranthene",   "207-08-9", -4.62, -4.64)
    CALL CalcH("BaP   benzo[$a$]pyrene",          "50-32-8", -4.69, -4.51)
    CALL CalcH("BghiP benzo[$ghi$]perylene",     "191-24-2", -4.87, -4.77)
    CALL CalcH("IP    indeno[1,2,3-$cd$]pyrene", "193-39-5", -4.85, -4.70)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, LogKaw_LDV, LogKaw_FAV)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: LogKaw_LDV
      REAL,             INTENT(IN) :: LogKaw_FAV
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNote("LDV", "Literature-derived value.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**LogKaw_LDV))
      CALL MakeNote("FAV", "Final adjusted value.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**LogKaw_FAV))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2989

  !---------------------------------------------------------------------------

  SUBROUTINE ref2990 ! KHcc [1]
    IMPLICIT NONE

    ref = "2990"
    type = "Q"

    ! Table 1:
    CALL CalcH( "BTBPE",   "37853-59-1", -5.2)
    CALL CalcH( "EHTeBB", "183658-27-7", -3.6)
    CALL CalcH( "TBPH",    "26040-51-7", -6.0)
    CALL CalcH( "PBT",        "87-83-2", -3.0)
    CALL CalcH( "HBB",        "87-82-1", -3.0)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lgKaw)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: lgKaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2990

  !---------------------------------------------------------------------------

  SUBROUTINE ref2991 ! KHcc [1]
    IMPLICIT NONE

    ref = "2991"
    type = "E"

    ! Table 1:
    CALL CalcH("PFBS",    "375-73-5", -3.7)
    CALL CalcH("PFHxS",   "355-46-4", -3.1)
    ! PFOS  -2.4 value taken from ref2992
    ! PFHxA -3.0 value taken from ref2992
    ! PFHpA -2.7 value taken from ref2992
    ! PFOA  -2.4 value taken from ref2992
    ! PFNA  -2.0 value taken from ref2992
    ! PFDA  -1.8 value taken from ref2992
    CALL CalcH("PFDoDA",  "307-55-1", -1.2)
    CALL CalcH("PFTeDA",  "376-06-7", -0.6)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lgKaw)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: lgKaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw))

    END SUBROUTINE CalcH

  END SUBROUTINE ref2991

  !---------------------------------------------------------------------------

  SUBROUTINE ref2992 ! KHcc [1]
    IMPLICIT NONE

    ref = "2992"

    ! estimated value from Table 2:
    type = "E"
    chem = "10-2FTOH" ; casrn = "865-86-1"
    CALL MakeNote(TRIM(ref), "Extrapolated based on number of carbons.")
    CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**1.60))

    ! Only data from the recommended methods (COSMOtherm new SPARC) are
    ! used here.
    type = "Q"
    ! Table 2:
    CALL CalcH("4-2FTOH",                      "2043-47-2", -0.03,  1.12)
    CALL CalcH("6-2FTOH",                       "647-42-7",  0.16,  1.36)
    CALL CalcH("8-2FTOH",                       "678-39-7",  0.85,  1.39)
    CALL CalcH("10-2FTOH",                      "865-86-1",  0.94,  0.89)
    ! Table 6:
    CALL CalcH("MeFOSE",                      "24448-09-7", -3.08, -0.72)
    CALL CalcH("EtFOSE",                       "1691-99-2", -2.15, -0.47)
    CALL CalcH("MeFOSEA",                     "25268-77-3", -2.04, -0.73)
    CALL CalcH("perfluorohexanoic acid",        "307-24-4", -3.04, -2.48)
    CALL CalcH("perfluoroheptanoic acid",       "375-85-9", -2.66, -2.15)
    CALL CalcH("perfluorooctanoic acid",        "335-67-1", -2.37, -1.69)
    CALL CalcH("perfluorononanoic acid",        "375-95-1", -2.03, -1.12)
    CALL CalcH("perfluorodecanoic acid",        "335-76-2", -1.79, -0.45)
    CALL CalcH("perfluoroundecanoic acid",     "2058-94-8", -1.52,  0.33)
    CALL CalcH("EtFOSA",                       "4151-50-2", -1.20, -1.27)
    CALL CalcH("perfluorooctansulfonic acid",  "1763-23-1", -2.40, -1.06)
    CALL CalcH("perfluorooctane sulfonamide",   "754-91-6", -3.92,  1.71)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lgKaw_COSMO, lgKaw_SPARC)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: lgKaw_COSMO
      REAL,             INTENT(IN) :: lgKaw_SPARC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNote(TRIM(ref)//"SPARC", "Calculated using the new SPARC method, see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw_COSMO))
      CALL MakeNote(TRIM(ref)//"COSMO", "Calculated using the COSMOtherm method, see " &
        //TRIM(citet())//" for details.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw_SPARC))

    END SUBROUTINE CalcH

  END SUBROUTINE ref2992

  !---------------------------------------------------------------------------

  SUBROUTINE ref2993 ! KHcc [1]
    IMPLICIT NONE

    ref = "2993"

    type = "M"
    CALL CalcH( "4:2 FTOH", "2043-47-2", -1.52)
    CALL CalcH( "6:2 FTOH",  "647-42-7", -0.56)
    type = "V"
    CALL CalcH( "8:2 FTOH",  "678-39-7",  0.58)

    ! Table 3:
    type = "Q"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 5., 15., 25. /) + CtoK
    CALL CalcH_Tdep( "4:2 FTOH",   "2043-47-2", (/ -2.01, -1.60, -1.26 /) )
    CALL CalcH_Tdep( "6:2 FTOH",    "647-42-7", (/ -1.51, -1.03, -0.67 /) )
    CALL CalcH_Tdep( "8:2 FTOH",    "678-39-7", (/ -0.90, -0.37,  0.00 /) )
    CALL CalcH_Tdep( "10:2 FTOH",   "865-86-1", (/ -0.44,  0.16,  0.56 /) )
    CALL CalcH_Tdep( "4:2 FTO",   "19430-93-4", (/  1.77,  2.01,  2.20 /) )
    CALL CalcH_Tdep( "6:2 FTO",   "25291-17-2", (/  2.36,  2.66,  2.87 /) )
    CALL CalcH_Tdep( "8:2 FTO",   "21652-58-4", (/  2.85,  3.22,  3.45 /) )
    CALL CalcH_Tdep( "10:2 FTO",  "30389-25-4", (/  3.39,  3.81,  4.07 /) )
    DEALLOCATE(temp, Harray)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lgKaw)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: lgKaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw))
    END SUBROUTINE CalcH

    SUBROUTINE CalcH_Tdep (chem_, casrn_, lgKaw)

      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: lgKaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH_Tdep

  END SUBROUTINE ref2993

  !---------------------------------------------------------------------------

  SUBROUTINE ref2994 ! KHcc [1]
    IMPLICIT NONE

    ref = "2994"
    type = "Q"

    ! Table 1:
    CALL CalcH("F10H2",         "_CAS-58", 5.9)
    CALL CalcH("F6H8",      "133331-77-8", 2.8)
    CALL CalcH("F6H14",     "154628-00-9", 3.2)
    CALL CalcH("F6H16",     "133310-71-1", 3.3)
    CALL CalcH("F8H16",     "117146-18-6", 5.0)
    CALL CalcH("F10H16",        "_CAS-57", 7.1)
    CALL CalcH("F12H14",     "93454-73-0", 9.4)
    CALL CalcH("F12H16",        "_CAS-59", 9.7)
    CALL CalcH("F12H16ene",     "_CAS-56", 6.7)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lgKaw)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: lgKaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw))

    END SUBROUTINE CalcH

  END SUBROUTINE ref2994

  !---------------------------------------------------------------------------

  SUBROUTINE ref2995 ! KHcc [1]
    IMPLICIT NONE

    ref = "2995"
    type = "Q"

    ! problem cases:
    ! - 183 and 361: same molecule but different results
    ! - 164 and 499: same molecule but different results (CAS 335-67-1 is correct,
    !   SMILES strings are different but point to same molecule)
    ! - SMILES and CAS don't fit:
    !   "372",  "30171-80-3",
    !   "420",  "56984-96-4"
    !   "436",  "63314-79-4" Should this be delta-muscenone? However, the
    !                        SMILES string only shows C_15H_26O and
    !                        delta-muscenone is C_16H_28O.
    !   "461",  "68187-86-0"
    !   "472",  "68412-68-0"
    !   "473",  "68412-69-1"
    !   "485",  "68937-40-6"
    !   "491",  "69882-11-7"
    !   "495",  "70983-60-7"
    !   "496",  "71608-61-2"
    !   "501",  "74499-35-7"
    !   "516", "106276-78-2"
    !   "526", "155613-93-7"

    !            Index           CAS       EPI    SPARC    COSMO   ABSOLV
    CALL CalcH( "1",       "50-29-3",   -3.20,   -3.19,   -4.60,   -2.70)
    CALL CalcH( "2",       "53-19-0",   -2.75,   -3.60,   -4.44,   -2.98)
    CALL CalcH( "3",       "58-89-9",   -1.98,   -4.28,   -5.07,   -2.97)
    CALL CalcH( "4",       "58-90-2",   -5.16,   -2.01,   -3.99,   -3.88)
    CALL CalcH( "5",       "59-50-7",   -4.73,   -4.50,   -4.84,   -5.36)
    CALL CalcH( "6",       "60-09-3",   -6.67,   -6.90,   -9.26,   -5.93)
    CALL CalcH( "7",       "60-11-7",   -5.02,   -5.01,   -5.31,   -4.40)
    CALL CalcH( "8",       "66-71-7",   -8.44,   -7.39,   -8.46,   -6.04)
    CALL CalcH( "9",       "67-72-1",   -0.77,   -0.64,   -0.68,   -0.99)
    CALL CalcH( "10",      "68-36-0",   -3.29,   -1.96,   -2.44,   -1.16)
    CALL CalcH( "11",      "70-30-4",  -10.45,   -8.79,   -7.48,   -9.21)
    CALL CalcH( "12",      "75-89-8",   -2.94,   -2.78,   -3.97,   -2.07)
    CALL CalcH( "13",      "76-05-1",   -3.75,   -2.60,   -4.30,   -3.98)
    CALL CalcH( "14",      "76-16-4",    2.99,    1.58,    2.68,    2.32)
    CALL CalcH( "15",      "76-19-7",    3.72,    1.60,    3.03,    2.95)
    CALL CalcH( "16",      "76-60-8",  -16.58,   -9.41,  -12.65,  -13.61)
    CALL CalcH( "17",      "77-08-7",  -17.53,  -13.60,  -14.69,  -13.83)
    CALL CalcH( "18",      "77-47-4",   -1.06,   -1.12,   -1.55,   -1.59)
    CALL CalcH( "19",      "77-71-4",   -6.95,   -8.61,  -10.10,   -8.61)
    CALL CalcH( "20",      "78-11-5",   -9.31,   -7.44,   -8.29,   -6.95)
    CALL CalcH( "21",      "78-33-1",   -4.55,   -0.32,   -6.59,   -4.94)
    CALL CalcH( "22",      "78-50-2",   -0.31,   -0.96,   -7.93,    3.64)
    CALL CalcH( "23",      "78-63-7",   -0.98,   -0.29,   -2.50,   -2.93)
    CALL CalcH( "24",      "79-27-6",   -3.15,   -2.86,   -3.03,   -2.57)
    CALL CalcH( "25",      "79-94-7",  -11.02,   -4.99,   -8.30,   -6.60)
    CALL CalcH( "26",      "80-04-6",   -4.38,   -8.18,   -7.92,   -5.66)
    CALL CalcH( "27",      "80-07-9",   -5.25,   -7.21,   -8.09,   -6.88)
    CALL CalcH( "28",      "81-03-8",   -0.27,   -0.68,   -0.72,    0.02)
    CALL CalcH( "29",      "81-14-1",   -7.71,   -5.81,   -4.32,   -6.09)
    CALL CalcH( "30",      "81-15-2",   -7.37,   -4.14,   -2.08,   -5.58)
    CALL CalcH( "31",      "82-68-8",   -3.71,   -1.76,   -1.74,   -2.74)
    CALL CalcH( "32",      "83-41-0",   -2.98,   -2.85,   -2.81,   -2.41)
    CALL CalcH( "33",      "83-66-9",   -6.24,   -3.77,   -2.73,   -5.06)
    CALL CalcH( "34",      "84-15-1",   -2.89,   -2.31,   -3.26,   -4.00)
    CALL CalcH( "35",      "84-51-5",   -6.72,   -6.02,   -5.59,   -7.44)
    CALL CalcH( "36",      "84-65-1",   -6.89,   -6.14,   -5.63,   -7.80)
    CALL CalcH( "37",      "84-66-2",   -4.79,   -5.56,   -5.83,   -5.14)
    CALL CalcH( "38",      "85-22-3",   -2.49,   -2.95,   -1.91,   -2.38)
    CALL CalcH( "39",      "85-29-0",   -4.36,   -4.23,   -5.03,   -5.18)
    CALL CalcH( "40",      "85-56-3",   -8.93,   -7.95,  -11.29,   -9.72)
    CALL CalcH( "41",      "86-20-4",   -5.91,   -6.23,   -6.42,   -5.79)
    CALL CalcH( "42",      "86-57-7",   -4.07,   -4.02,   -3.61,   -4.07)
    CALL CalcH( "43",      "86-98-6",   -4.81,   -4.23,   -3.56,   -4.24)
    CALL CalcH( "44",      "87-10-5",   -9.38,   -9.46,   -9.61,   -9.46)
    CALL CalcH( "45",      "87-61-6",   -1.05,   -1.23,   -1.61,   -1.11)
    CALL CalcH( "46",      "87-68-3",   -0.35,   -0.09,   -0.76,   -1.63)
    CALL CalcH( "47",      "87-82-1",   -3.06,   -3.06,   -2.17,   -3.22)
    CALL CalcH( "48",      "87-84-3",   -4.41,   -5.44,   -6.64,   -4.49)
    CALL CalcH( "49",      "87-86-5",   -5.29,   -2.17,   -4.21,   -4.00)
    CALL CalcH( "50",      "88-04-0",   -4.68,   -4.58,   -4.69,   -5.10)
    CALL CalcH( "51",      "88-06-2",   -5.03,   -1.84,   -3.34,   -3.38)
    CALL CalcH( "52",      "88-27-7",   -7.08,   -5.78,   -3.50,   -5.08)
    CALL CalcH( "53",      "88-30-2",   -6.11,   -7.22,   -7.98,   -6.46)
    CALL CalcH( "54",      "88-41-5",   -1.39,   -1.97,   -3.12,   -1.24)
    CALL CalcH( "55",      "88-66-4",   -1.65,   -2.26,   -2.13,   -1.52)
    CALL CalcH( "56",      "88-72-2",   -3.02,   -2.78,   -2.80,   -2.64)
    CALL CalcH( "57",      "88-73-3",   -3.19,   -2.58,   -3.49,   -3.06)
    CALL CalcH( "58",      "88-85-7",   -5.54,   -6.11,   -5.52,   -6.03)
    CALL CalcH( "59",      "89-32-7",   -6.51,  -14.54,   -8.08,   -9.38)
    CALL CalcH( "60",      "89-58-7",   -2.98,   -2.80,   -2.73,   -2.41)
    CALL CalcH( "61",      "89-61-2",   -3.32,   -2.58,   -3.26,   -3.18)
    CALL CalcH( "62",      "89-63-4",   -5.30,   -5.63,   -6.74,   -5.86)
    CALL CalcH( "63",      "89-87-2",   -2.98,   -2.88,   -3.03,   -2.41)
    CALL CalcH( "64",      "90-30-2",   -5.38,   -5.06,   -4.46,   -5.84)
    CALL CalcH( "65",      "92-59-1",   -3.43,   -3.42,   -3.06,   -4.22)
    CALL CalcH( "66",      "92-66-0",   -2.17,   -2.23,   -2.63,   -2.94)
    CALL CalcH( "67",      "92-70-6",   -7.25,   -7.47,   -8.97,   -7.31)
    CALL CalcH( "68",      "92-84-2",   -5.94,   -6.23,   -5.38,   -6.03)
    CALL CalcH( "69",      "93-55-0",   -3.27,   -3.25,   -3.59,   -3.38)
    CALL CalcH( "70",      "93-58-3",   -2.85,   -2.95,   -3.37,   -3.18)
    CALL CalcH( "71",      "93-89-0",   -2.72,   -2.71,   -3.10,   -3.08)
    CALL CalcH( "72",      "93-97-0",   -4.24,   -5.96,   -7.21,   -6.20)
    CALL CalcH( "73",      "94-36-0",   -3.84,   -5.42,   -6.01,   -7.03)
    CALL CalcH( "74",      "95-16-9",   -4.82,   -3.84,   -4.45,   -3.69)
    CALL CalcH( "75",      "95-73-8",   -0.88,   -1.13,   -1.22,   -0.66)
    CALL CalcH( "76",      "95-82-9",   -4.37,   -3.81,   -3.82,   -4.68)
    CALL CalcH( "77",      "95-93-2",   -0.49,   -0.80,   -0.68,   -0.01)
    CALL CalcH( "78",      "95-94-3",   -1.18,   -1.32,   -1.66,   -1.08)
    CALL CalcH( "79",      "96-13-9",   -5.59,   -5.44,   -4.49,   -4.53)
    CALL CalcH( "80",      "96-99-1",   -7.89,   -6.71,   -6.59,   -8.36)
    CALL CalcH( "81",      "97-00-7",   -5.59,   -4.17,   -4.12,   -4.99)
    CALL CalcH( "82",      "98-07-7",   -1.97,   -1.25,   -1.70,   -1.07)
    CALL CalcH( "83",      "98-09-9",   -3.45,   -4.62,   -4.22,   -5.59)
    CALL CalcH( "84",      "98-15-7",    0.15,   -0.84,   -0.55,    0.47)
    CALL CalcH( "85",      "98-19-1",   -0.16,   -0.05,   -0.28,    0.17)
    CALL CalcH( "86",      "98-23-7",   -0.12,   -0.36,   -0.35,    0.43)
    CALL CalcH( "87",      "98-46-4",   -2.12,   -2.70,   -2.15,   -1.31)
    CALL CalcH( "88",      "98-51-1",   -0.20,   -0.11,   -0.52,   -0.07)
    CALL CalcH( "89",      "98-56-6",    0.15,   -0.89,   -0.56,    0.47)
    CALL CalcH( "90",      "98-59-9",   -3.40,   -4.64,   -4.48,   -5.36)
    CALL CalcH( "91",      "98-73-7",   -4.94,   -5.05,   -5.95,   -5.02)
    CALL CalcH( "92",      "98-87-3",   -1.52,   -1.92,   -2.43,   -1.41)
    CALL CalcH( "93",      "99-04-7",   -5.31,   -5.49,   -6.10,   -5.42)
    CALL CalcH( "94",      "99-08-1",   -3.02,   -2.79,   -3.01,   -2.64)
    CALL CalcH( "95",      "99-30-9",   -6.77,   -5.23,   -6.62,   -6.55)
    CALL CalcH( "96",      "99-51-4",   -2.98,   -2.88,   -3.30,   -2.41)
    CALL CalcH( "97",      "99-54-7",   -3.32,   -2.89,   -3.06,   -3.22)
    CALL CalcH( "98",      "99-75-2",   -2.81,   -2.98,   -3.63,   -2.94)
    CALL CalcH( "99",      "99-94-5",   -5.31,   -5.53,   -6.34,   -5.42)
    CALL CalcH( "100",     "99-99-0",   -3.02,   -2.84,   -3.35,   -2.64)
    CALL CalcH( "101",    "100-00-5",   -3.19,   -2.87,   -3.18,   -3.06)
    CALL CalcH( "102",    "100-97-0",    0.82,   -9.16,   -6.36,  -11.13)
    CALL CalcH( "103",    "101-02-0",   -4.66,   -2.04,   -5.56,   -8.24)
    CALL CalcH( "104",    "101-05-3",   -4.86,   -6.48,   -4.37,   -7.13)
    CALL CalcH( "105",    "101-14-4",   -8.87,   -7.93,   -7.86,   -9.38)
    CALL CalcH( "106",    "101-20-2",   -8.73,   -7.09,  -11.25,  -10.65)
    CALL CalcH( "107",    "101-37-1",   -4.76,   -6.66,   -5.67,   -8.01)
    CALL CalcH( "108",    "101-63-3",   -7.13,   -5.75,   -6.87,   -7.45)
    CALL CalcH( "109",    "102-09-0",   -2.46,   -4.59,   -3.37,   -5.46)
    CALL CalcH( "110",    "102-36-3",   -2.28,   -4.05,   -1.59,   -3.73)
    CALL CalcH( "111",    "104-12-1",   -2.15,   -4.01,   -1.38,   -3.92)
    CALL CalcH( "112",    "104-83-6",   -1.20,   -2.27,   -2.31,   -1.18)
    CALL CalcH( "113",    "104-90-5",   -3.33,   -2.97,   -3.24,   -2.19)
    CALL CalcH( "114",    "108-77-0",   -4.70,   -4.78,   -2.89,   -4.29)
    CALL CalcH( "115",    "108-78-1",  -11.11,  -12.22,  -13.16,  -12.32)
    CALL CalcH( "116",    "108-80-5",  -12.45,   -8.92,  -14.02,  -11.00)
    CALL CalcH( "117",    "110-03-2",   -4.53,   -6.67,   -6.29,   -5.07)
    CALL CalcH( "118",    "111-44-4",   -2.11,   -2.84,   -2.04,   -1.06)
    CALL CalcH( "119",    "115-25-3",    3.48,    2.40,    2.26,    2.59)
    CALL CalcH( "120",    "115-27-5",   -5.44,   -7.88,   -7.56,  -10.98)
    CALL CalcH( "121",    "115-28-6",  -11.91,  -12.89,  -12.98,  -11.26)
    CALL CalcH( "122",    "115-29-7",   -5.43,   -4.77,   -5.42,  -11.76)
    CALL CalcH( "123",    "115-32-2",   -7.64,   -5.88,   -5.36,   -6.90)
    CALL CalcH( "124",    "115-39-9",  -16.67,   -9.10,  -13.36,  -14.12)
    CALL CalcH( "125",    "116-16-5",   -5.41,   -0.35,   -2.19,   -2.65)
    CALL CalcH( "126",    "116-66-5",   -5.08,   -4.53,   -3.27,   -3.80)
    CALL CalcH( "127",    "117-08-8",   -4.11,   -7.66,   -5.67,   -5.03)
    CALL CalcH( "128",    "117-96-4",  -15.94,  -12.13,  -20.46,  -19.91)
    CALL CalcH( "129",    "118-69-4",   -0.88,   -1.33,   -1.02,   -0.66)
    CALL CalcH( "130",    "118-74-1",   -1.44,   -1.18,   -1.60,   -1.40)
    CALL CalcH( "131",    "118-79-6",   -5.84,   -2.57,   -4.19,   -4.28)
    CALL CalcH( "132",    "118-90-1",   -5.31,   -4.90,   -5.39,   -5.42)
    CALL CalcH( "133",    "119-10-8",   -4.25,   -3.60,   -5.17,   -3.82)
    CALL CalcH( "134",    "119-61-9",   -4.10,   -3.86,   -4.95,   -4.92)
    CALL CalcH( "135",    "120-46-7",   -7.27,   -6.30,   -8.23,   -7.52)
    CALL CalcH( "136",    "120-61-6",   -5.04,   -5.50,   -5.12,   -5.36)
    CALL CalcH( "137",    "120-82-1",   -1.05,   -1.28,   -1.57,   -1.06)
    CALL CalcH( "138",    "120-83-2",   -4.90,   -4.30,   -3.44,   -4.06)
    CALL CalcH( "139",    "121-14-2",   -5.42,   -4.60,   -4.09,   -4.58)
    CALL CalcH( "140",    "121-17-5",   -2.25,   -2.48,   -2.45,   -1.41)
    CALL CalcH( "141",    "121-73-3",   -3.19,   -2.84,   -2.84,   -3.06)
    CALL CalcH( "142",    "121-86-8",   -3.15,   -2.92,   -2.96,   -2.80)
    CALL CalcH( "143",    "121-87-9",   -6.64,   -6.51,   -8.22,   -6.72)
    CALL CalcH( "144",    "123-17-1",   -2.40,   -2.45,   -2.17,   -1.81)
    CALL CalcH( "145",    "123-48-8",    2.05,    2.89,    1.36,    1.43)
    CALL CalcH( "146",    "126-63-6",   -1.71,    1.81,   -8.13,    0.72)
    CALL CalcH( "147",    "128-63-2",   -5.07,   -5.04,   -3.19,   -5.21)
    CALL CalcH( "148",    "131-09-9",   -7.02,   -6.22,   -5.53,   -7.91)
    !CALL CalcH( "149",    "131-49-7",  -15.94,  -12.13,  -20.46,  -19.91) ! not used, same as 117-96-4
    CALL CalcH( "150",    "133-14-2",   -4.36,   -5.54,   -6.61,   -6.94)
    CALL CalcH( "151",    "133-49-3",   -2.32,   -1.83,   -3.51,   -1.74)
    CALL CalcH( "152",    "136-60-7",   -2.48,   -2.40,   -3.11,   -2.90)
    CALL CalcH( "153",    "139-60-6",   -5.16,   -4.16,   -3.64,   -4.68)
    CALL CalcH( "154",    "140-66-9",   -3.74,   -3.76,   -4.41,   -3.64)
    CALL CalcH( "155",    "145-39-1",   -4.92,   -3.72,   -2.06,   -3.44)
    CALL CalcH( "156",    "147-82-0",   -5.31,   -3.81,   -3.17,   -5.47)
    CALL CalcH( "157",    "149-30-4",   -5.83,   -6.84,   -5.73,   -4.72)
    CALL CalcH( "158",    "150-50-5",   -3.03,   -0.17,   -2.56,   -2.10)
    CALL CalcH( "159",    "311-89-7",    6.35,    6.07,    5.35,    6.18)
    CALL CalcH( "160",    "319-84-6",   -1.98,   -4.28,   -5.00,   -2.97)
    CALL CalcH( "161",    "320-72-9",   -6.50,   -6.03,   -8.27,   -5.73)
    CALL CalcH( "162",    "328-84-7",    0.02,   -1.12,   -0.69,   0.24)
    CALL CalcH( "163",    "329-01-1",   -1.08,   -3.79,   -0.51,   -2.20)
    CALL CalcH( "164",    "335-67-1",    0.57,   -1.40,   -1.49,   -0.44)
    CALL CalcH( "165",    "338-84-1",    8.51,    8.59,    6.08,    8.29)
    CALL CalcH( "166",    "344-04-7",   -0.72,    0.39,    0.47,   -0.22)
    CALL CalcH( "167",    "354-33-6",    2.10,    1.31,    0.85,    1.29)
    CALL CalcH( "168",    "354-58-5",    1.04,    0.29,    0.84,    1.13)
    CALL CalcH( "169",    "375-22-4",   -2.31,   -3.25,   -2.79,   -3.20)
    CALL CalcH( "170",    "376-14-7",    0.10,   -0.98,   -3.58,   -2.01)
    CALL CalcH( "171",    "383-07-3",    0.15,   -1.01,   -3.66,   -2.03)
    CALL CalcH( "172",    "393-75-9",   -4.66,   -3.84,   -2.62,   -3.35)
    CALL CalcH( "173",    "422-56-0",    1.31,    0.13,    0.55,    1.01)
    CALL CalcH( "174",    "515-40-2",   -0.70,   -1.37,   -1.61,   -0.61)
    CALL CalcH( "175",    "527-53-7",   -0.49,   -0.74,   -0.73,   -0.01)
    CALL CalcH( "176",    "528-29-0",   -5.46,   -4.90,   -4.81,   -4.83)
    CALL CalcH( "177",    "534-52-1",   -5.90,   -6.76,   -4.67,   -6.25)
    CALL CalcH( "178",    "540-84-1",    2.09,    2.38,    1.27,    1.39)
    CALL CalcH( "179",    "545-06-2",   -4.26,   -1.68,   -0.98,   -1.41)
    CALL CalcH( "180",    "589-15-1",   -1.95,   -2.83,   -2.69,   -1.77)
    CALL CalcH( "181",    "593-60-2",   -0.30,   -0.28,   -0.08,   -0.31)
    CALL CalcH( "182",    "594-42-3",   -2.01,   -0.23,   -0.37,   -1.12)
    CALL CalcH( "183",    "602-01-7",   -5.42,   -4.74,   -4.37,   -4.58)
    CALL CalcH( "184",    "603-35-0",   -6.03,   -1.37,   -4.50,   -4.08)
    CALL CalcH( "185",    "603-48-5",   -8.20,   -7.88,   -5.94,   -9.45)
    CALL CalcH( "186",    "606-20-2",   -5.42,   -4.72,   -4.03,   -4.58)
    CALL CalcH( "187",    "606-21-3",   -5.59,   -4.03,   -4.25,   -4.99)
    CALL CalcH( "188",    "608-71-9",   -6.64,   -3.47,   -4.74,   -5.52)
    CALL CalcH( "189",    "608-93-5",   -1.31,   -1.23,   -1.66,   -1.24)
    CALL CalcH( "190",    "609-93-8",   -5.90,   -6.93,   -5.34,   -4.30)
    CALL CalcH( "191",    "610-39-9",   -5.42,   -4.98,   -4.88,   -4.58)
    CALL CalcH( "192",    "611-06-3",   -3.32,   -2.60,   -3.32,   -2.85)
    CALL CalcH( "193",    "611-19-8",   -1.20,   -2.25,   -2.71,   -1.18)
    CALL CalcH( "194",    "611-70-1",   -3.15,   -2.98,   -3.62,   -3.31)
    CALL CalcH( "195",    "614-45-9",   -2.07,   -2.65,   -4.31,   -4.13)
    CALL CalcH( "196",    "618-62-2",   -3.32,   -2.70,   -2.44,   -2.85)
    CALL CalcH( "197",    "619-15-8",   -5.42,   -4.65,   -3.54,   -4.58)
    CALL CalcH( "198",    "620-22-4",   -2.63,   -2.93,   -3.34,   -2.66)
    CALL CalcH( "199",    "623-03-0",   -2.80,   -2.97,   -3.59,   -3.05)
    CALL CalcH( "200",    "626-39-1",   -1.86,   -2.00,   -1.79,   -1.81)
    CALL CalcH( "201",    "632-79-1",   -5.18,   -9.04,   -5.78,   -6.30)
    CALL CalcH( "202",    "634-66-2",   -1.18,   -1.28,   -1.72,   -1.06)
    CALL CalcH( "203",    "634-93-5",   -4.50,   -3.19,   -3.01,   -4.57)
    CALL CalcH( "204",    "635-22-3",   -6.64,   -6.42,   -6.77,   -6.94)
    CALL CalcH( "205",    "636-30-6",   -4.50,   -3.77,   -4.37,   -4.64)
    CALL CalcH( "206",    "636-53-3",   -4.79,   -4.68,  -10.85,   -5.14)
    CALL CalcH( "207",    "644-97-3",   -3.21,   -0.80,   -2.19,   -1.91)
    CALL CalcH( "208",    "647-42-7",    0.79,   -1.37,   -0.93,    1.61)
    CALL CalcH( "209",    "678-39-7",    2.23,    0.19,   -0.26,    2.97)
    CALL CalcH( "210",    "690-39-1",    2.52,    1.02,    0.36,    2.18)
    CALL CalcH( "211",    "709-98-8",   -6.74,   -6.30,   -6.97,   -7.32)
    CALL CalcH( "212",    "717-74-8",    0.21,    0.36,   -0.11,    0.19)
    CALL CalcH( "213",    "719-32-4",   -4.40,   -4.68,   -1.93,   -6.52)
    CALL CalcH( "214",    "732-26-3",   -3.40,   -2.14,   -1.91,   -2.12)
    CALL CalcH( "215",    "791-28-6",   -7.67,   -8.06,  -10.42,   -2.80)
    CALL CalcH( "216",    "793-24-8",   -6.86,   -5.99,   -4.99,   -6.76)
    CALL CalcH( "217",    "827-94-1",   -7.31,   -5.63,   -6.68,   -7.15)
    CALL CalcH( "218",    "836-30-6",   -6.77,   -5.62,   -7.85,   -6.80)
    CALL CalcH( "219",    "840-65-3",   -6.05,   -6.80,  -10.81,   -6.51)
    CALL CalcH( "220",    "847-51-8",  -11.69,  -10.76,   -8.58,  -10.15)
    CALL CalcH( "221",    "865-86-1",    3.67,    2.17,    0.44,    4.39)
    CALL CalcH( "222",    "944-61-6",   -3.63,   -3.70,   -3.81,   -3.27)
    CALL CalcH( "223",    "954-16-5",   -3.97,   -3.97,   -4.57,   -4.17)
    CALL CalcH( "224",   "1124-05-6",   -0.83,   -1.47,   -1.06,   -0.76)
    CALL CalcH( "225",   "1134-04-9",   -3.58,   -2.22,   -2.24,   -2.76)
    CALL CalcH( "226",   "1154-59-2",   -8.71,   -8.76,   -9.98,   -8.60)
    CALL CalcH( "227",   "1163-19-5",   -6.31,   -6.01,   -6.51,   -6.22)
    CALL CalcH( "228",   "1176-74-5",  -14.40,  -10.49,  -13.89,  -11.94)
    CALL CalcH( "229",   "1185-09-7",   -2.34,   -0.96,   -1.67,   -2.22)
    CALL CalcH( "230",   "1201-30-5",   -5.25,   -2.35,   -2.40,   -3.01)
    CALL CalcH( "231",   "1203-86-7",   -4.69,   -4.15,   -3.41,   -4.17)
    CALL CalcH( "232",   "1222-05-5",   -2.27,   -4.31,   -2.32,   -1.39)
    CALL CalcH( "233",   "1237-53-2",   -6.49,   -6.52,   -4.24,   -6.49)
    CALL CalcH( "234",     "88-72-2",   -3.02,   -2.78,   -2.74,   -2.64) ! CAS corrected
    CALL CalcH( "235",    "137-99-5",   -2.59,   -2.97,   -3.24,   -2.79) ! CAS corrected
    CALL CalcH( "236",     "51-28-5",   -5.95,   -6.19,   -4.07,   -6.51) ! CAS corrected
    CALL CalcH( "237",   "1478-61-1",   -7.63,   -9.53,   -8.71,   -7.12)
    CALL CalcH( "238",   "1539-04-4",   -5.90,   -8.03,   -7.82,   -8.28)
    CALL CalcH( "239",   "1568-80-5",   -9.56,   -9.41,   -9.74,   -9.31)
    CALL CalcH( "240",   "1577-03-3",   -4.08,   -3.73,   -4.31,   -3.56)
    CALL CalcH( "241",   "1585-07-5",   -0.89,   -1.18,   -1.02,   -1.11)
    CALL CalcH( "242",   "1633-22-3",   -1.85,   -2.32,   -3.37,   -2.03)
    CALL CalcH( "243",   "1691-99-2",   -1.63,   -1.33,   -1.19,   -2.91)
    CALL CalcH( "244",   "1724-39-6",   -2.96,   -3.93,   -4.30,   -2.12)
    CALL CalcH( "245",   "1737-93-5",   -3.60,   -0.28,   -0.59,   -1.83)
    CALL CalcH( "246",   "1742-14-9",   -1.41,   -1.66,   -2.21,   -1.08)
    CALL CalcH( "247",   "1763-23-1",   -0.35,   -1.33,   -2.60,   -1.39)
    CALL CalcH( "248",   "1770-80-5",   -6.16,   -5.54,   -7.06,   -6.30)
    CALL CalcH( "249",   "1799-84-4",    1.08,   -0.60,   -0.21,    1.07)
    CALL CalcH( "250",   "1817-13-6",   -3.32,   -3.49,   -3.39,   -3.15)
    CALL CalcH( "251",   "1817-47-6",   -2.77,   -2.52,   -2.98,   -2.54)
    CALL CalcH( "252",   "1817-73-8",   -7.98,   -5.83,   -6.65,   -8.15)
    CALL CalcH( "253",   "1836-75-5",   -4.98,   -4.84,   -5.49,   -5.45)
    CALL CalcH( "254",   "1860-26-0",   -0.24,   -1.48,    1.82,    0.04)
    CALL CalcH( "255",   "1869-77-8",    1.48,   -0.64,   -4.03,   -1.87)
    CALL CalcH( "256",   "1897-45-6",   -5.21,   -3.56,   -5.23,   -4.83)
    CALL CalcH( "257",   "1918-02-1",  -10.28,   -3.80,   -8.35,  -11.59)
    CALL CalcH( "258",   "1929-82-4",   -3.19,   -3.66,   -3.83,   -3.02)
    CALL CalcH( "259",   "1984-58-3",   -2.15,   -1.55,   -2.54,   -2.08)
    CALL CalcH( "260",   "1996-88-9",    3.96,    1.59,    0.87,    3.80)
    CALL CalcH( "261",   "2012-81-9",   -3.76,   -4.04,   -3.98,   -2.86)
    CALL CalcH( "262",   "2043-47-2",   -0.65,   -2.50,   -1.31,    0.23)
    CALL CalcH( "263",   "2043-53-0",    5.54,    3.72,    1.30,    5.25)
    CALL CalcH( "264",   "2043-55-2",    2.66,    0.86,    0.33,    2.59)
    CALL CalcH( "265",   "2043-57-4",    4.10,    2.07,    0.87,    3.97)
    CALL CalcH( "266",   "2084-69-7",   -0.02,   -0.52,   -0.90,    0.17)
    CALL CalcH( "267",   "2095-01-4",   -7.19,   -7.23,   -5.18,   -5.71)
    CALL CalcH( "268",   "2095-02-5",   -7.19,   -7.24,   -5.19,   -5.71)
    CALL CalcH( "269",   "2144-53-8",    2.52,    0.34,    0.48,    2.44)
    CALL CalcH( "270",   "2144-54-9",    5.40,    3.11,    1.56,    5.23)
    CALL CalcH( "271",   "2157-19-9",   -7.28,   -9.87,   -8.51,   -8.65)
    CALL CalcH( "272",   "2167-23-9",   -1.47,    0.82,   -1.60,   -3.44)
    CALL CalcH( "273",   "2176-62-7",   -0.59,   -1.52,   -1.80,   -2.64)
    CALL CalcH( "274",   "2263-09-4",   -1.38,   -1.01,   -1.97,   -2.73)
    CALL CalcH( "275",   "2276-90-6",  -16.04,  -13.08,  -20.02,  -19.67)
    CALL CalcH( "276",   "2379-74-0",  -10.90,  -10.47,   -8.13,   -9.77)
    CALL CalcH( "277",   "2402-79-1",   -0.46,   -1.92,   -2.32,   -2.68)
    CALL CalcH( "278",   "3032-55-1",   -6.74,   -5.55,   -6.78,   -4.92)
    CALL CalcH( "279",   "3064-70-8",   -6.31,   -1.49,   -6.88,   -4.40)
    CALL CalcH( "280",   "3084-48-8",   -1.05,   -0.86,   -8.16,    3.06)
    CALL CalcH( "281",   "3194-55-6",   -4.15,   -5.63,   -7.15,   -4.21)
    CALL CalcH( "282",   "3209-22-1",   -3.32,   -2.53,   -3.38,   -3.22)
    CALL CalcH( "283",   "3236-71-3",  -12.32,  -11.19,  -11.71,  -12.89)
    CALL CalcH( "284",   "3278-89-5",   -2.97,   -2.52,   -2.69,   -3.19)
    CALL CalcH( "285",   "3294-03-9",   -9.05,   -7.49,  -10.73,   -8.64)
    CALL CalcH( "286",   "3322-93-8",   -2.77,   -3.86,   -4.40,   -2.79)
    CALL CalcH( "287",   "3380-34-5",   -6.69,   -5.15,   -6.54,   -6.31)
    CALL CalcH( "288",   "3401-80-7",   -6.50,   -6.93,   -4.45,   -5.73)
    CALL CalcH( "289",   "3407-42-9",   -3.18,   -4.61,   -4.43,   -2.82)
    CALL CalcH( "290",   "3438-16-2",   -6.71,   -4.97,   -7.37,   -7.17)
    CALL CalcH( "291",   "3457-61-2",   -1.53,   -1.08,   -1.60,   -2.68)
    CALL CalcH( "292",   "3734-48-3",   -1.69,   -1.19,   -3.74,   -3.02)
    CALL CalcH( "293",   "3738-00-9",   -1.70,   -2.86,   -2.21,   -0.42)
    CALL CalcH( "294",   "3741-80-8",  -11.78,   -6.62,   -6.76,  -11.99)
    CALL CalcH( "295",   "3772-55-2",   -4.32,   -5.66,   -4.78,   -3.25)
    CALL CalcH( "296",   "4062-60-6",   -5.95,   -3.75,   -3.39,   -4.48)
    CALL CalcH( "297",   "4083-64-1",   -2.63,   -4.90,   -4.22,   -8.00)
    CALL CalcH( "298",   "4151-50-2",    2.34,    0.47,    1.63,   -0.97)
    CALL CalcH( "299",   "4162-45-2",  -11.14,  -11.58,  -13.18,  -11.80)
    CALL CalcH( "300",   "4378-61-4",  -10.16,   -8.83,  -10.01,  -11.44)
    CALL CalcH( "301",   "4827-55-8",   -5.17,   -5.72,   -7.11,   -5.60)
    CALL CalcH( "302",   "5216-25-1",   -2.10,   -1.64,   -1.93,   -1.23)
    CALL CalcH( "303",   "5242-49-9",  -12.27,  -10.19,   -8.49,  -11.37)
    CALL CalcH( "304",   "5329-12-4",   -6.88,   -4.96,   -4.45,   -7.13)
    CALL CalcH( "305",   "5848-93-1",   -4.59,   -4.67,   -3.26,   -3.02)
    CALL CalcH( "306",   "6014-75-1",    6.84,    4.91,    2.25,    6.61)
    CALL CalcH( "307",   "6144-04-3",   -1.15,   -1.25,   -2.77,   -2.35)
    CALL CalcH( "308",   "6145-73-9",   -5.61,   -0.53,   -6.22,   -2.97)
    CALL CalcH( "309",   "6165-52-2",   -1.50,   -1.60,   -2.11,   -1.58)
    CALL CalcH( "310",   "6281-14-7",   -1.63,   -9.00,   -8.47,   -7.57)
    CALL CalcH( "311",   "6358-30-1",  -10.30,  -15.65,  -10.19,  -13.41)
    CALL CalcH( "312",   "6373-31-5",  -13.77,  -11.01,  -12.39,  -13.06)
    CALL CalcH( "313",   "6928-67-2",   -5.07,   -4.87,   -4.40,   -4.84)
    CALL CalcH( "314",   "6940-53-0",   -5.65,   -4.59,   -5.69,   -4.98)
    CALL CalcH( "315",   "7347-19-5",   -5.86,   -4.60,   -7.03,   -5.51)
    CALL CalcH( "316",   "7397-06-0",   -0.16,   -0.25,   -0.35,    0.17)
    CALL CalcH( "317",  "10469-09-7",   -6.96,   -3.78,   -5.47,   -8.01)
    CALL CalcH( "318",  "10594-03-3",   -3.05,   -4.70,   -4.19,   -1.74)
    CALL CalcH( "319",  "16423-68-0",  -16.98,  -11.76,  -14.33,  -13.10) ! CAS corrected
    CALL CalcH( "320",     "57-74-9",   -2.54,   -2.08,   -4.77,   -3.57) ! CAS corrected
    CALL CalcH( "321",  "13080-86-9",  -11.69,  -11.84,  -11.41,  -13.88)
    !CALL CalcH( "322",  "13087-53-1",  -16.04,  -13.08,  -20.02,  -19.67) ! not used, same as 2276-90-6
    CALL CalcH( "323",  "13171-00-1",   -2.89,   -3.77,   -3.89,   -2.34)
    CALL CalcH( "324",  "13301-61-6",  -10.82,  -10.09,  -10.80,   -8.26)
    CALL CalcH( "325",  "13472-08-7",   -8.05,   -5.36,   -4.58,   -3.04)
    CALL CalcH( "326",  "13473-26-2",  -16.56,  -11.68,  -14.30,  -11.74)
    CALL CalcH( "327",  "13475-82-6",    2.58,    3.25,    1.22,    1.90)
    CALL CalcH( "328",  "13560-89-9",   -3.52,   -2.24,   -6.71,   -5.06)
    CALL CalcH( "329",  "13654-09-6",   -5.77,   -5.87,   -5.76,   -6.09)
    CALL CalcH( "330",  "13674-84-5",   -5.61,    0.32,   -7.95,   -2.97)
    CALL CalcH( "331",  "13674-87-8",   -6.97,   -2.01,  -10.52,   -3.87)
    CALL CalcH( "332",  "13705-05-0",   -4.51,   -4.50,   -3.38,   -4.63)
    CALL CalcH( "333",  "13886-99-2",    1.11,   -0.89,   -1.73,    1.70)
    CALL CalcH( "334",  "14121-36-9",   -0.46,   -2.45,   -2.29,   -2.86)
    CALL CalcH( "335",  "14143-60-3",   -7.60,   -6.44,   -7.35,   -8.40)
    CALL CalcH( "336",  "14650-24-9",   -0.02,   -0.91,   -3.80,   -2.13)
    CALL CalcH( "337",  "14737-80-5",   -6.56,   -6.47,   -5.01,   -6.88)
    CALL CalcH( "338",  "15086-94-9",  -16.04,  -11.58,  -13.83,  -11.85)
    CALL CalcH( "339",  "15323-35-0",   -2.89,   -3.70,   -4.11,   -2.39)
    !CALL CalcH( "340",  "15905-32-5",  -16.98,  -11.80,  -15.67,  -13.10) ! not used, same as 16423-68-0
    CALL CalcH( "341",  "24231-46-7",  -41.47,  -43.53,  -30.02,  -40.73) ! CAS corrected
    CALL CalcH( "342",  "16714-68-4",   -1.54,   -2.26,   -3.19,   -1.33)
    CALL CalcH( "343",  "17527-29-6",    2.32,    0.32,    0.15,    2.23)
    CALL CalcH( "344",  "17700-09-3",   -3.45,   -2.52,   -2.70,   -3.00)
    CALL CalcH( "345",  "17741-60-5",    5.20,    3.11,    1.23,    5.04)
    CALL CalcH( "346",  "17824-83-8",   -4.27,   -3.63,   -3.30,   -4.58)
    CALL CalcH( "347",  "19430-93-4",    3.66,    2.09,    1.67,    3.05)
    CALL CalcH( "348",  "19438-61-0",   -3.54,   -8.20,   -4.94,   -4.95)
    CALL CalcH( "349",  "19870-74-7",   -0.80,   -0.78,   -1.28,   -0.47)
    CALL CalcH( "350",  "19889-37-3",   -4.03,   -4.13,   -5.75,   -3.61)
    CALL CalcH( "351",  "20566-35-2",  -13.95,  -14.57,  -16.89,  -14.15)
    CALL CalcH( "352",  "20749-68-2",   -9.08,  -13.27,   -7.15,   -8.68)
    CALL CalcH( "353",  "21145-77-7",   -2.76,   -3.77,   -4.29,   -2.29)
    CALL CalcH( "354",  "21850-44-2",   -8.78,   -8.00,   -8.63,   -8.33)
    CALL CalcH( "355",  "22810-10-2",   -0.22,   -1.12,   -0.51,    0.21)
    CALL CalcH( "356",  "24261-19-6",   -7.70,   -6.27,   -8.10,   -8.10)
    CALL CalcH( "357",  "24448-09-7",   -1.75,   -1.46,   -1.67,   -3.03)
    CALL CalcH( "358",  "24549-06-2",   -3.90,   -3.71,   -3.40,   -3.32)
    CALL CalcH( "359",     "_CAS-78",    0.38,   -0.16,   -0.77,    0.62) ! CAS corrected
    CALL CalcH( "360",   "2781-00-2",   -2.40,   -1.65,   -2.85,   -4.33) ! CAS corrected
    CALL CalcH( "361",    "602-01-7",   -5.42,   -4.75,   -4.45,   -4.58) ! CAS corrected
    CALL CalcH( "362",  "25327-89-3",   -5.28,   -4.51,   -5.63,   -5.67)
    CALL CalcH( "363",  "25550-98-5",   -3.67,   -3.80,   -6.61,   -0.99)
    CALL CalcH( "364",     "92-94-4",   -2.89,   -2.77,   -3.44,   -4.00) ! CAS corrected
    CALL CalcH( "365",  "26266-77-3",   -2.67,   -4.77,   -4.81,   -2.70)
    CALL CalcH( "366",  "26748-41-4",   -0.39,   -1.07,   -2.49,   -1.76)
    CALL CalcH( "367",  "56310-11-3",   -2.71,   -2.86,   -4.05,   -3.01)
    CALL CalcH( "368",  "27676-62-6",  -24.18,  -15.52,  -13.92,  -18.31)
    CALL CalcH( "369",  "27905-45-9",    3.76,    1.58,    0.56,    3.61)
    CALL CalcH( "370",   "2198-75-6",   -1.93,   -2.24,   -2.02,   -2.07) ! CAS corrected
    CALL CalcH( "371",  "30125-47-4",  -17.65,  -16.38,  -14.42,  -18.01)
    !CALL CalcH( "372",  "30171-80-3",   -5.31,   -4.25,   -4.73,   -4.13) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "373",     "_CAS-77",   -3.92,   -5.19,   -6.39,   -4.19) ! CAS corrected
    CALL CalcH( "374",     "_CAS-79",   -3.44,   -4.95,   -6.01,   -3.87) ! CAS corrected
    CALL CalcH( "375",  "31160-64-2",   -0.80,   -0.89,   -8.12,    3.24)
    CALL CalcH( "376",  "31160-66-4",   -0.55,   -0.93,   -8.00,    3.46)
    CALL CalcH( "377",  "31506-32-8",    2.22,    0.29,    0.89,   -1.09)
    CALL CalcH( "378",  "31570-04-4",   -2.18,    0.79,   -5.58,   -4.16)
    CALL CalcH( "379",  "60348-60-9",   -4.32,   -3.96,   -5.47,   -4.77) ! CAS corrected
    CALL CalcH( "380",  "32536-52-0",   -5.51,   -5.26,   -6.21,   -5.30)
    CALL CalcH( "381",  "32588-74-2",  -18.12,  -17.62,  -13.37,  -19.07)
    CALL CalcH( "382",  "32588-76-4",  -18.83,  -14.76,  -12.94,  -16.76)
    CALL CalcH( "383",  "33294-14-3",  -11.51,  -10.07,  -10.36,   -9.49)
    CALL CalcH( "384",  "33704-59-5",    1.21,    2.21,   -0.42,    0.74)
    CALL CalcH( "385",  "33704-60-8",    1.65,    2.56,   -0.21,    1.06)
    CALL CalcH( "386",  "33704-61-9",   -2.24,   -1.22,   -4.70,   -2.08)
    CALL CalcH( "387",  "34362-49-7",    8.09,    6.99,   -0.21,    7.77)
    CALL CalcH( "388",  "34395-24-9",    6.65,    4.91,   -0.83,    6.39)
    CALL CalcH( "389",  "34449-89-3",   -4.51,   -4.34,   -2.55,   -5.71)
    CALL CalcH( "390",  "34454-97-2",   -4.64,   -4.44,   -3.06,   -5.82)
    CALL CalcH( "391",  "34455-03-3",   -3.07,   -3.06,   -1.88,   -4.34)
    CALL CalcH( "392",  "34832-88-7",   -3.84,   -4.76,   -7.41,   -3.94)
    CALL CalcH( "393",  "34893-92-0",   -2.28,   -3.84,   -1.31,   -3.25)
    CALL CalcH( "394",  "35578-47-3",   -7.30,   -6.81,   -5.68,   -8.50)
    CALL CalcH( "395",  "36483-57-5",   -6.28,   -4.27,   -4.42,   -3.41)
    CALL CalcH( "396",  "36483-60-0",   -4.72,   -5.26,   -5.82,   -4.80)
    CALL CalcH( "397",  "37853-59-1",   -6.52,   -5.26,   -6.44,   -6.41)
    CALL CalcH( "398",  "38970-72-8",    2.14,    2.28,   -0.42,    1.32)
    CALL CalcH( "399",  "39239-77-5",    5.11,    4.58,    1.12,    5.77)
    CALL CalcH( "400",  "39549-27-4",   -3.76,   -3.85,   -5.37,   -2.91)
    CALL CalcH( "401",  "39635-79-5",  -14.56,   -9.38,  -10.16,  -10.48)
    CALL CalcH( "402",  "39638-32-9",   -1.87,    0.09,   -3.37,   -0.59)
    CALL CalcH( "403",  "21825-03-6",  -11.27,   -5.35,   -7.50,   -6.92) ! CAS corrected
    CALL CalcH( "404",  "40088-47-9",   -3.92,   -3.40,   -4.60,   -4.43)
    CALL CalcH( "405",  "41424-36-6",   -3.04,   -2.69,   -2.90,   -2.68)
    CALL CalcH( "406",  "42757-55-1",  -12.31,  -12.11,  -14.66,  -12.20)
    CALL CalcH( "407",  "42874-63-5",   -4.39,   -6.43,   -9.75,   -4.76)
    CALL CalcH( "408",  "50594-44-0",   -5.83,   -6.57,   -7.96,   -5.89)
    CALL CalcH( "409",  "50594-77-9",   -3.43,   -4.78,   -4.86,   -3.95)
    CALL CalcH( "410",  "50598-28-2",   -2.28,   -3.58,   -2.74,   -4.79)
    CALL CalcH( "411",  "51282-49-6",   -5.38,   -5.47,   -6.96,   -5.22)
    CALL CalcH( "412",  "51772-35-1",   -3.20,   -3.38,   -3.35,   -4.44)
    CALL CalcH( "413",     "_CAS-76",   -3.67,   -3.94,   -6.41,   -3.18) ! CAS corrected
    CALL CalcH( "414",  "52314-67-7",   -1.60,   -1.38,   -2.60,   -3.34)
    CALL CalcH( "415",  "52434-90-9",  -16.31,  -11.21,  -11.83,  -13.49)
    CALL CalcH( "416",  "52907-07-0",  -18.79,  -14.55,  -15.15,  -18.59)
    CALL CalcH( "417",  "54949-74-5",   -2.10,   -2.96,   -2.00,   -0.56)
    CALL CalcH( "418",  "55701-05-8",   -4.68,   -5.35,   -8.18,   -5.18)
    CALL CalcH( "419",  "55794-20-2",   -4.09,   -1.24,   -5.51,   -5.78)
    !CALL CalcH( "420",  "56984-96-4",   -2.08,   -2.16,   -2.36,   -0.98) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "421",  "57583-54-7",  -10.92,   -1.55,  -11.81,  -19.99)
    CALL CalcH( "422",  "57913-35-6",   -4.41,   -6.85,   -8.22,   -5.89)
    CALL CalcH( "423",  "58965-66-5",   -9.57,   -9.01,   -9.72,   -9.22)
    CALL CalcH( "424",  "59017-64-0",  -38.82,  -30.53,  -32.70,  -42.25)
    CALL CalcH( "425",  "59808-78-5",   -1.20,   -3.01,   -3.57,   -1.86)
    CALL CalcH( "426",  "60466-61-7",   -1.61,   -2.41,   -2.70,   -1.86)
    CALL CalcH( "427",  "60699-51-6",    6.55,    7.45,    1.82,    7.15)
    CALL CalcH( "428",  "60825-26-5",   -4.17,   -4.88,   -7.06,   -5.94)
    CALL CalcH( "429",  "60825-27-6",   -4.05,   -4.63,   -4.75,   -5.89)
    CALL CalcH( "430",  "61262-53-1",   -8.12,   -6.33,   -5.73,   -6.45)
    CALL CalcH( "431",  "61702-88-3",   -0.51,   -0.47,   -2.13,   -1.23)
    CALL CalcH( "432",   "1706-50-9",    1.78,    1.43,   -0.63,    0.65) ! CAS corrected
    CALL CalcH( "433",  "61898-95-1",   -2.18,   -2.11,   -2.50,   -2.63)
    CALL CalcH( "434",  "62111-47-1",   -1.99,   -1.94,   -2.32,   -2.13)
    CALL CalcH( "435",  "62265-99-0",   -5.05,   -4.94,   -4.07,   -4.38)
    !CALL CalcH( "436",  "63314-79-4",   -1.63,   -2.52,   -4.59,   -1.94) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "437",  "63734-62-3",   -6.20,   -5.91,   -8.72,   -6.86)
    CALL CalcH( "438",  "63936-56-1",   -5.91,   -5.58,   -6.44,   -5.71)
    CALL CalcH( "439",  "64667-33-0",   -3.22,   -2.83,   -6.18,   -1.66)
    CALL CalcH( "440",  "64800-83-5",   -1.42,   -1.47,   -2.20,   -1.66)
    CALL CalcH( "441",  "65104-65-6",    9.43,   14.27,    3.22,    9.94)
    CALL CalcH( "442",  "65104-67-8",    7.99,   10.78,    2.52,    8.56)
    CALL CalcH( "443",  "65208-34-6",   -7.58,   -9.22,   -5.43,   -8.82)
    CALL CalcH( "444",  "65925-28-2",   -3.12,   -3.59,   -2.97,   -2.32)
    CALL CalcH( "445",  "66068-84-6",   -3.18,   -4.65,   -5.04,   -2.82)
    CALL CalcH( "446",  "66072-32-0",   -3.18,   -4.39,   -5.03,   -2.59)
    CALL CalcH( "447",  "66346-01-8",   -3.42,   -3.25,   -4.02,   -2.98)
    CALL CalcH( "448",  "67567-23-1",   -3.85,   -0.96,   -4.87,   -5.56)
    CALL CalcH( "449",  "67584-54-7",   -1.56,   -2.76,   -1.65,   -4.11)
    CALL CalcH( "450",  "67584-55-8",   -3.10,   -3.12,   -5.31,   -5.08)
    CALL CalcH( "451",  "67584-56-9",   -2.38,   -2.69,   -5.27,   -4.38)
    CALL CalcH( "452",  "67584-57-0",   -1.66,   -2.17,   -4.93,   -3.70)
    CALL CalcH( "453",  "67584-59-2",   -2.91,   -3.09,   -2.67,   -4.86)
    CALL CalcH( "454",  "68084-62-8",   -0.94,   -1.58,   -4.39,   -3.02)
    CALL CalcH( "455",  "68140-18-1",    0.89,   -0.53,   -0.56,    1.52)
    !CALL CalcH( "456",  "68140-19-2",    0.89,   -0.53,   -0.56,    1.52) ! not used, same as 68140-18-1
    CALL CalcH( "457",  "68140-20-5",    2.33,   -0.07,    0.11,    2.33)
    CALL CalcH( "458",  "68140-21-6",    4.62,    1.79,   -0.33,    4.49)
    CALL CalcH( "459",  "68140-48-7",   -2.76,   -3.90,   -4.04,   -2.50)
    CALL CalcH( "460",  "68155-66-8",   -1.79,   -2.87,   -4.42,   -2.00)
    !CALL CalcH( "461",  "68187-86-0",    0.43,   -2.62,   -2.44,    0.28) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "462",  "68188-12-5",    1.94,    0.51,    0.03,    1.91)
    CALL CalcH( "463",  "68258-90-2",   -2.56,   -3.29,   -3.59,   -2.33)
    CALL CalcH( "464",  "68258-91-3",   -2.10,   -2.68,   -3.60,   -1.74)
    CALL CalcH( "465",  "68259-14-3",    1.50,   -0.59,    0.52,   -1.77)
    CALL CalcH( "466",  "68259-15-4",    0.78,   -1.36,    0.21,   -2.47)
    CALL CalcH( "467",  "68298-13-5",    0.06,   -2.04,   -0.14,   -3.19)
    !CALL CalcH( "468",  "68391-08-2",    2.23,    0.19,   -0.10,    2.97) ! not used, same as 678-39-7
    CALL CalcH( "469",  "68399-95-1",   -7.22,   -6.19,   -7.71,   -7.21)
    CALL CalcH( "470",  "68400-79-3",    0.69,   -8.16,   -8.67,   -5.61)
    !CALL CalcH( "471",  "68412-40-8",   -0.59,   -1.52,   -1.80,   -2.64) ! duplicate data, see 2176-62-7
    !CALL CalcH( "472",  "68412-68-0",   -3.36,    2.25,   -7.70,    0.22) ! SMILES and CAS don't fit (not used)
    !CALL CalcH( "473",  "68412-69-1",    2.90,    7.96,   -1.39,    6.36) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "474",  "68555-72-6",   -3.79,   -3.76,   -2.17,   -5.01)
    CALL CalcH( "475",  "68555-73-7",   -2.35,   -2.25,   -1.57,   -3.66)
    CALL CalcH( "476",  "68555-74-8",   -3.92,   -3.86,   -2.71,   -5.14)
    CALL CalcH( "477",  "68555-75-9",   -3.20,   -3.17,   -2.36,   -4.39)
    CALL CalcH( "478",  "68555-76-0",   -2.47,   -2.37,   -2.05,   -3.71)
    CALL CalcH( "479",  "68555-77-1",   -3.72,   -4.89,   -3.43,   -6.17)
    !CALL CalcH( "480",  "68608-14-0",  -10.40,  -13.23,   -8.62,  -18.56) ! mixture (not used)
    CALL CalcH( "481",  "68608-79-7",   -2.31,   -3.07,   -2.56,   -2.85)
    CALL CalcH( "482",    "104-72-3",    0.49,    0.45,    0.07,    0.16) ! CAS corrected
    CALL CalcH( "483",  "68814-02-8",   -8.35,   -9.24,   -6.62,   -9.57)
    CALL CalcH( "484",  "68928-80-3",   -5.11,   -4.81,   -6.14,   -5.08)
    !CALL CalcH( "485",  "68937-40-6",   -4.55,   -0.32,   -7.13,   -4.94) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "486",  "68954-01-8",  -17.58,  -22.23,  -15.76,  -18.61)
    CALL CalcH( "487",  "36876-13-8",   -1.20,   -1.09,   -1.90,   -1.69) ! CAS 69009-90-1 corrected
    CALL CalcH( "488",  "69045-78-9",   -3.19,   -3.73,   -3.28,   -3.02)
    CALL CalcH( "489",  "69045-83-6",   -3.32,   -3.01,   -2.64,   -3.18)
    CALL CalcH( "490",  "69116-73-0",   -2.16,   -0.10,    0.19,   -2.50)
    !CALL CalcH( "491",  "69882-11-7",   -7.90,   -7.20,   -8.87,   -7.46) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "492",  "10469-02-0",   -2.22,   -1.95,   -1.95,   -2.45) ! CAS corrected
    CALL CalcH( "493",  "70321-86-7",  -13.25,   -9.16,  -10.54,  -10.34)
    CALL CalcH( "494",  "70516-41-5",  -11.32,  -11.69,  -11.94,  -12.30)
    !CALL CalcH( "495",  "70983-60-7",   -4.61,   -2.82,   -0.93,   -2.17) ! SMILES and CAS don't fit (not used)
    !CALL CalcH( "496",  "71608-61-2",   -2.54,   -2.01,   -6.55,   -1.49) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "497",     "_CAS-80",   -1.54,   -2.04,   -3.04,   -2.20) ! CAS corrected
    CALL CalcH( "498",    "375-85-9",   -0.15,   -2.09,   -1.73,   -1.14) ! CAS 72623-77-9 corrected
    CALL CalcH( "499",    "335-67-1",    0.57,   -1.40,   -1.72,   -0.44) ! CAS corrected
    CALL CalcH( "500",  "73588-42-8",   -2.32,   -2.18,   -3.56,   -0.86)
    !CALL CalcH( "501",  "74499-35-7",   -3.24,   -3.61,   -3.90,   -3.69) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "502",  "75147-20-5",   -2.99,   -1.98,   -2.29,   -2.64)
    CALL CalcH( "503",  "75150-13-9",   -5.31,   -4.24,   -5.11,   -4.13)
    CALL CalcH( "504",  "76649-15-5",   -5.61,   -0.25,   -7.14,   -2.97)
    !CALL CalcH( "505",  "77098-07-8",  -13.95,  -14.57,  -13.35,  -14.15) ! mixture (not used)
    CALL CalcH( "506",  "78068-85-6",    0.22,   -0.79,   -0.33,    0.56)
    CALL CalcH( "507",  "84632-65-5",  -12.93,   -9.98,  -16.68,  -15.73)
    CALL CalcH( "508",  "84852-53-9",   -5.58,   -6.34,   -5.33,   -5.63)
    CALL CalcH( "509",  "85702-64-3",  -13.02,  -18.91,   -8.78,  -13.34)
    CALL CalcH( "510",    "307-49-3",    9.48,    7.54,    5.53,    7.83) ! CAS corrected
    CALL CalcH( "511",  "90801-18-6",   -6.90,   -8.40,   -9.57,   -8.07)
    CALL CalcH( "512",  "92484-07-6",  -13.35,  -12.10,  -13.19,  -17.25)
    CALL CalcH( "513",  "94334-64-2",  -11.02,  -10.69,  -10.73,  -11.62)
    CALL CalcH( "514",  "95823-36-2",   -4.03,   -4.53,   -6.60,   -3.61)
    CALL CalcH( "515", "100404-06-6",   -2.71,   -2.80,   -3.55,   -3.01)
    !CALL CalcH( "516", "106276-78-2",  -15.81,  -13.93,  -13.48,  -14.94) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "517", "118400-71-8",    3.51,    1.65,   -1.67,    3.06)
    CALL CalcH( "518", "119345-02-7",   -0.92,   -0.53,   -1.62,   -2.28)
    CALL CalcH( "519", "125904-11-2",   -1.35,   -1.37,   -1.26,   -1.56)
    CALL CalcH( "520", "125997-20-8",   -6.58,   -1.50,   -7.04,   -3.31)
    CALL CalcH( "521", "129188-99-4",   -9.04,   -8.90,   -9.83,   -9.29)
    CALL CalcH( "522", "130097-36-8",   -1.97,   -2.35,   -3.79,   -3.50) ! SMILES probably incorrect
    CALL CalcH( "523", "138495-42-8",    3.96,    1.10,    0.35,    2.65)
    CALL CalcH( "524", "145556-04-3",   -3.68,   -3.68,   -5.45,   -3.01)
    CALL CalcH( "525", "151574-12-8",   -7.97,   -7.45,   -9.47,  -10.57)
    !CALL CalcH( "526", "155613-93-7",   -6.90,   -7.31,   -5.87,   -5.39) ! SMILES and CAS don't fit (not used)
    CALL CalcH( "527", "163702-05-4",    1.73,    1.51,    1.73,    2.13)
    CALL CalcH( "528", "163702-06-5",    1.73,    0.93,    1.70,    2.09)
    CALL CalcH( "529", "163702-07-6",    1.61,    1.49,    1.68,    2.01)

CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lgKaw_EPI, lgKaw_SPARC, lgKaw_COSMO, lgKaw_ABSOLV)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: lgKaw_EPI
      REAL,             INTENT(IN) :: lgKaw_SPARC
      REAL,             INTENT(IN) :: lgKaw_COSMO
      REAL,             INTENT(IN) :: lgKaw_ABSOLV
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNote("supplement")
      CALL MakeNote(TRIM(ref)//"EPI", "Calculated using the EPI Suite (v4.0) method.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw_EPI))
      CALL MakeNote("supplement")
      CALL MakeNote(TRIM(ref)//"SPARC", "Calculated using the SPARC (v4.2) method.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw_SPARC))
      CALL MakeNote("supplement")
      CALL MakeNote(TRIM(ref)//"COSMO", "Calculated using the COSMOtherm (v2.1) method.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw_COSMO))
      CALL MakeNote("supplement")
      CALL MakeNote(TRIM(ref)//"ABSOLV", "Calculated using the ABSOLV (ADMEBoxes v4.1) method.")
      CALL Output(KHcc_TIMES_HcpSI_atT0 / (10.**lgKaw_ABSOLV))
    END SUBROUTINE CalcH

  END SUBROUTINE ref2995

  !---------------------------------------------------------------------------

  SUBROUTINE ref2996 ! KHcc [1]
    IMPLICIT NONE

    ref = "2996"
    type = "M"

    CALL CalcH("benzene",         "71-43-2", (/ 45., 50., 60., 70., 80. /),      (/ 2.99, 3.03, 3.17, 3.25, 3.36 /)      )
    CALL CalcH("methylbenzene",  "108-88-3", (/ 45., 50., 60., 70., 80. /),      (/ 2.98, 3.08, 3.16, 3.26, 3.37 /)      )
    CALL CalcH("chlorobenzene",  "108-90-7", (/ 45., 50., 60., 70., 80. /),      (/ 2.79, 2.87, 2.98, 3.09, 3.20 /)      )
    CALL CalcH("4:2 FTOH",      "2043-47-2", (/ 35., 45., 55., 65., 75., 85. /), (/ 95., 142., 202., 268., 339., 469. /) )
    CALL CalcH("6:2 FTOH",       "647-42-7", (/ 45., 55., 65., 75., 85. /),      (/ 131., 226., 368., 558., 815. /)      )
    CALL CalcH("8:2 FTOH",       "678-39-7", (/ 65., 75., 85., 90. /),           (/ 356., 634., 1126., 1548. /)          )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, Kaw)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, Kaw
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp+CtoK, KHcc_TO_HcpSI(Kaw,temp), Hominus, mindHR)
      CALL MakeNote(TRIM(ref), "Extrapolated from data above 298 K.")
      CALL Output(Hominus, mindHR, r2)

    END SUBROUTINE CalcH

  END SUBROUTINE ref2996

  !---------------------------------------------------------------------------

  SUBROUTINE ref2997 ! KHpcSI [Pa*m3/mol] and Hcc [1]
    IMPLICIT NONE

    ref = "2997"
    type = "M"

    ! Table 2:
    CALL CalcH("1-butanol",   "71-36-3", (/ 50., 60., 70., 80., 90. /), &
      (/ 630., 373., 190., 135., 93.  /), (/ 4.3,  7.4,  15.0, 21.8, 32.6/))
    CALL CalcH("1-pentanol",  "71-41-0", (/ 50., 60., 70., 80., 90. /), &
      (/ 410., 244., 140., 91.,  58.  /), (/ 6.5,  11.4, 20.4, 32.3, 52.3/))
    CALL CalcH("1-hexanol",  "111-27-3", (/ 50., 60., 70., 80., 90. /), &
      (/ 283., 159., 94.,  61.,  39.  /), (/ 9.5,  17.4, 30.4, 48.1, 78. /))
    CALL CalcH("1-heptanol", "111-70-6", (/ 50., 60., 70., 80., 90. /), &
      (/ 190., 110., 63.,  39.5, 25.  /), (/ 14.2, 25.1, 45.5, 74.3, 122./))
    CALL CalcH("1-octanol",  "111-87-5", (/ 50., 60., 70., 80., 90. /), &
      (/ 105., 69.,  42.6, 24.7, 16.3 /), (/ 25.6, 40.1, 67.2, 119., 189./))
    CALL CalcH("1-nonanol",  "143-08-8", (/ 50., 60., 70., 80., 90. /), &
      (/ 65.5, 45.5, 26.1, 16.7, 9.6  /), (/ 41.1, 61.0, 109., 176., 350./))
    CALL CalcH("1-decanol",  "112-30-1", (/ 60., 70., 80.           /), &
      (/ 28.9, 18.1, 12.6             /), (/ 95.9, 159., 234.            /))

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, KawC, KawP)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, KawC, KawP
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! KawC should be KwaC
      !DO i = 1, SIZE(KawC)
      !  CALL consistency_check(Hcc_TO_HcpSI(KawC(i),temp(i)+CtoK), KHpcSI_TIMES_HcpSI/KawP(i), &
      !    "KawC and KawP from Table 2", .TRUE.)
      !ENDDO
      ! 10% diff for 1-nonanol at 90 C: Maybe 9.6 should be 8.6?
      CALL HTdep(temp+CtoK, KHpcSI_TIMES_HcpSI/KawP, Hominus, mindHR)
      CALL MakeNote(TRIM(ref), "Extrapolated from data above 298 K.")
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2997

  !---------------------------------------------------------------------------

  SUBROUTINE ref2999 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "2999"
    type = "M"

    ! Table 1:
    CALL CalcH("1-butanol",   "71-36-3", (/ 298.15, 305.55, 323.15, 343.15 /), (/ 1.0,  1.6,  5.92,  18.8 /))
    CALL CalcH("1-pentanol",  "71-41-0", (/ 298.15, 305.55, 323.15, 343.15 /), (/ 1.2,  2.2,  8.5,   26.7 /))
    CALL CalcH("1-hexanol",  "111-27-3", (/ 298.15, 305.55, 323.15, 343.15 /), (/ 1.70, 3.1,  12.7,  40.1 /))
    CALL CalcH("1-heptanol", "111-70-6", (/ 298.15, 305.55, 323.15, 343.15 /), (/ 2.5,  4.8,  19.11, 60.  /))
    CALL CalcH("1-octanol",  "111-87-5", (/ 298.15, 305.55, 323.15, 343.15 /), (/ 4.5,  8.5,  31.42, 92.1 /))
    CALL CalcH("1-nonanol",  "143-08-8", (/ 298.15, 305.55, 323.15, 343.15 /), (/ 8.5,  15.7, 54.1,  133.3/))
    CALL CalcH("1-decanol",  "112-30-1", (/ 298.15, 305.55, 323.15, 343.15 /), (/ 11.4, 24.,  91.,   211. /))

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp, H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL HTdep(temp, KHpcSI_TIMES_HcpSI/H, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref2999

  !---------------------------------------------------------------------------

END MODULE Henry_ref3000

!*****************************************************************************
!                                  end of file
!*****************************************************************************
