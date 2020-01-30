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

MODULE henry_ref1500

  USE henry_mem
  USE henry_util
  IMPLICIT NONE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ref1004 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1004"
    type = "M"

    chem = "2-methylpropenal" ; casrn = "78-85-3" ! methacrolein
    CALL Output(6.5*Hcp_TO_HcpSI)

    chem = "3-buten-2-one" ; casrn = "78-94-4" ! C4H6O methyl vinyl ketone, MVK
    CALL Output(41.*Hcp_TO_HcpSI)

    chem = "2-methyl-3-buten-2-ol" ; casrn = "115-18-4"
    CALL MakeNoteOtherTemp("303")
    CALL Output(65.*Hcp_TO_HcpSI)

  END SUBROUTINE ref1004

  !---------------------------------------------------------------------------

  SUBROUTINE ref1014 ! special definition [kPa/mass%]
    IMPLICIT NONE

    REAL, PARAMETER :: M_R125  = MC*2.+MH+MF*5.
    REAL, PARAMETER :: M_R22   = MC+MH+MF*2.+MCl
    REAL, PARAMETER :: M_R134a = MC*2.+MH*2.+MF*4.
    REAL, PARAMETER :: M_R152a = MC*2.+MH*4.+MF*2.
    REAL, PARAMETER :: M_R124  = MC*2.+MH+MF*4.+MCl
    REAL, PARAMETER :: M_R142B = MC*2.+MH*3.+MF*2.+MCl
    REAL, PARAMETER :: M_R123  = MC*2.+MH+MF*3.+MCl*2.
    REAL, PARAMETER :: M_R141B = MC*2.+MH*3.+MF+MCl*2.
    REAL, PARAMETER :: M_MCF   = MC*2.+MH*3.+MCl*3.

    ref = "1014"
    ! probably all the quoted references only contain maximum solubilities which
    ! were then divided by the vapor pressure
    type = "V"

    CALL CalcH("R125", "354-33-6", -22.88, 4750., M_R125)

    chem = "chlorodifluoromethane" ; casrn = "75-45-6" ! CHF2Cl R22
    ndata = 14
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/10.,15.,20.,25.,30.,35.,40.,45.,50.,55.,60.,65.,70.,75./)
    temp = temp + CtoK
    Harray = (/171.,227.,288.,354.,425.,498.,573.,650.,727.,805.,883., &
      961.,1038.,1114./)
    Harray = (1./Harray) * percent/kilo * rhoH2O/M_R22
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("498R22", &
      "The temperature dependence was recalculated from the data on p.~20 of " &
      //TRIM(citet())//".")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    CALL CalcH("1,1,1,2-tetrafluoroethane",    "811-97-2",  -15.35, 2633., M_R134a)
    CALL CalcH("1,1-difluoroethane",           "75-37-6",   -13.6,  2300., M_R152a)
    CALL CalcH("1-Cl-1,2,2,2-tetra-F-ethane",  "2837-89-0", -17.39, 3229., M_R124)
    CALL CalcH("1-chloro-1,1-difluoroethane",  "75-68-3",   -15.11, 2544., M_R142B)
    CALL CalcH("2,2-di-Cl-1,1,1-tri-F-ethane", "306-83-2",  -14.06, 2570., M_R123)
    CALL CalcH("1,1-dichloro-1-fluoroethane",  "1717-00-6", -24.61, 5248., M_R141B)
    CALL CalcH("1,1,1-trichloroethane",        "71-55-6",   -20.29, 4655., M_MCF)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, A, B, MX)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: A, B, MX

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = EXP(A+B/T0) * percent/kilo * rhoH2O/MX
      CALL Output(Hominus, B)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1014

  !---------------------------------------------------------------------------

  SUBROUTINE ref1018 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1018"
    type = "M"
    chem = "bromine chloride" ; casrn = "13863-41-7" ! BrCl
    CALL MakeNoteOtherTemp("290")
    CALL Output(1.5*Hcp_TO_HcpSI)

  END SUBROUTINE ref1018

  !---------------------------------------------------------------------------

  SUBROUTINE ref1024 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1024"

    type = "V"
    CALL CalcH("1C3  ", "627-13-4",    93.0)
    CALL CalcH("1C4  ", "928-45-0",   117.4)
    CALL CalcH("1C5  ", "1002-16-0",  248.8)
    CALL CalcH("1C6  ", "20633-11-8", 274.4)
    CALL CalcH("2C3  ", "1712-64-7",  122.8)
    CALL CalcH("2C4  ", "924-52-7",   156.0)
    CALL CalcH("2C5  ", "21981-48-6", 206.7)
    CALL CalcH("3C5  ", "82944-59-0", 205.0)

    type = "M"
    CALL CalcH("1C3  ", "627-13-4",   128.9, 11.422, 4271.)
    CALL CalcH("1C4  ", "928-45-0",   156.2, 13.839, 4945.)
    CALL CalcH("1C5  ", "1002-16-0",  168.6, 17.205, 5965.)
    CALL CalcH("1C6  ", "20633-11-8", 151.2, 18.404, 6364.)
    CALL CalcH("2C3  ", "1712-64-7",  163.6, 10.655, 3956.)
    CALL CalcH("2C4  ", "924-52-7",   228.1               )
    CALL CalcH("2C5  ", "21981-48-6", 302.4, 13.812, 4775.)
    CALL CalcH("3C5  ", "82944-59-0", 277.6, 14.393, 4959.)
    CALL CalcH("3M1C4", "543-87-3",   223.1, 16.206, 5581.)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, KHpcSI, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL,             INTENT(IN) :: KHpcSI
      REAL, OPTIONAL,   INTENT(IN) :: A, B
      REAL :: H_calc

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it

      Hominus = KHpcSI_TIMES_HcpSI / KHpcSI
      IF (PRESENT(A)) THEN
        ! check if the values of H and KGW diverge by more than 1%:
        H_calc = KHcc_TIMES_HcpSI_atT0 / EXP(A-B/T0)
        IF (ABS(Hominus-H_calc)/Hominus>0.01) THEN
          !PRINT *, TRIM(chem), H_calc, Hominus, ABS(Hominus-H_calc)/Hominus
          CALL PrintWarning("Internal inconsistency > 1%")
        ENDIF
        mindHR = B + T0 ! see ref958, eqn (34) why T0 is added
        CALL Output(H_calc, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref1024

  !---------------------------------------------------------------------------

  SUBROUTINE ref1026 ! KHcc [1]
    IMPLICIT NONE

    ref = "1026"
    type = "M"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))

    temp = (/5.,10.,15.,20.,25./) + CtoK

    chem = "chloromethane" ; casrn = "74-87-3" ! CH3Cl methyl chloride
    Harray = (/.229,.274,.327,.388,.457/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "dichloromethane" ; casrn = "75-09-2" ! CH2Cl2
    Harray = (/.047,.060,.077,.097,.122/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "trichloromethane" ; casrn = "67-66-3" ! CHCl3 chloroform
    Harray = (/.072,.095,.124,.160,.206/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "tetrachloroethene" ; casrn = "127-18-4" ! C2Cl4 tetrachloroethylene
    Harray = (/.295,.404,.548,.736,.978/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "trichloroethene" ; casrn = "79-01-6" ! C2HCl3 trichloroethylene
    Harray = (/.174,.234,.310,.408,.532/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1026

  !---------------------------------------------------------------------------

  !SUBROUTINE ref1032
    ! some HLC in Tabs. 24 and 25.
    ! not used here (only citations of other papers, no new results)
  !END SUBROUTINE ref1032

  !---------------------------------------------------------------------------

  SUBROUTINE ref1038 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1038"

    chem = "bromine chloride" ; casrn = "13863-41-7" ! BrCl
    type = "M"
    mindHR = 46.8E3/Rgas
    CALL Output(0.94*Hcp_TO_HcpSI,mindHR)

    chem = "molecular bromine" ; casrn = "7726-95-6" ! Br2
    type = "?"
    CALL MakeNote("whichref")
    CALL MakeNote("1038tdep", &
      "The value of $\Delta H^{\circ}$ listed in Table 2 of "// &
      TRIM(citet())//" is incorrect.")
    CALL Output(0.77*Hcp_TO_HcpSI)

    chem = "molecular chlorine" ; casrn = "7782-50-5" ! Cl2
    type = "?"
    CALL MakeNote("whichref")
    CALL MakeNote("1038tdep")
    CALL Output(0.092*Hcp_TO_HcpSI)

  END SUBROUTINE ref1038

  !---------------------------------------------------------------------------

  SUBROUTINE ref1046 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1046"
    type = "M"

    chem = "trichloroethene" ; casrn = "79-01-6" ! C2HCl3 trichloroethylene
    mindHR = 3738.
    Hominus    = 1./(atm*EXP(7.99 - mindHR/T0))
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref1046

  !---------------------------------------------------------------------------

  SUBROUTINE ref1056 ! Hbp [mol/(kg*bar)]
    IMPLICIT NONE

    ref = "1056"
    type = "M"
    chem = "nitrous acid" ; casrn = "7782-77-6" ! HNO2
    mindHR = 4932.
    Hominus    = 3.06E-6 * EXP(mindHR/T0) * rhoH2O/bar
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref1056

  !---------------------------------------------------------------------------

  SUBROUTINE ref1060
    IMPLICIT NONE

    ref = "1060"
    type = "M"
    chem = "nitrosyl chloride" ; casrn = "2696-92-6"
    CALL Output(0.05*Hcp_TO_HcpSI, limit=">")

  END SUBROUTINE ref1060

  !---------------------------------------------------------------------------

  SUBROUTINE ref1063 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "1063"
    type = "M"

    chem = "2-oxopropanoic acid" ; casrn = "127-17-3" ! CH3COCOOH pyruvic acid
    Hominus = 3.1E5*Hbp_TO_HcpSI
    CALL Output(Hominus)

    chem = "2-Methyl-2-propenoic acid" ; casrn = "79-41-4" ! methacrylic acid
    Hominus = 2.58E3*Hbp_TO_HcpSI
    CALL Output(Hominus)

  END SUBROUTINE ref1063

  !---------------------------------------------------------------------------

  SUBROUTINE ref1068 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1068"
    type = "V"

    CALL CalcH("hydroxybenzene",                "108-95-2", 1.1E-7)
    CALL CalcH("1-hydroxy-2-methylbenzene",      "95-48-7", 2.8E-7)
    CALL CalcH("1-hydroxy-3-methylbenzene",     "108-39-4", 2.0E-7)
    CALL CalcH("1-hydroxy-4-methylbenzene",     "106-44-5", 2.2E-7)
    CALL CalcH("1-hydroxy-2-methoxybenzene",     "90-05-1", 2.4E-7)
    CALL CalcH("2,6-dimethylphenol",            "576-26-1", 1.9E-6)
    CALL CalcH("2-nitrophenol",                  "88-75-5", 3.5E-6)
    CALL CalcH("1,3-dimethyl-4-hydroxybenzene", "105-67-9", 6.3E-7)
    CALL CalcH("2,5-dimethylphenol",             "95-87-4", 2.6E-7)
    CALL CalcH("2,4-dichlorophenol",            "120-83-2", 1.1E-6)
    CALL CalcH("3,5-dimethylphenol",            "108-68-9", 1.6E-7)
    CALL CalcH("2,3-dimethylphenol",            "526-75-0", 2.0E-7)
    CALL CalcH("3,4-dimethylphenol",             "95-65-8", 9.3E-8)
    CALL CalcH("2,4,5-trichlorophenol",          "95-95-4", 1.3E-6)
    CALL CalcH("2,4,6-trichlorophenol",          "88-06-2", 1.3E-6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*KHpc)
      CALL MakeNoteOtherTemp("281")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1068

  !---------------------------------------------------------------------------

  SUBROUTINE ref1071 ! Hb2p [mol^2/(kg^2*atm)]
    IMPLICIT NONE

    ref = "1071"
    chem = "hydrogen chloride" ; casrn = "7647-01-0" ! HCl
    type = "L"
    CALL MakeNote(TRIM(ref), &
      "$\Hprime$~= "//TRIM(Hprime_string(2.04E6*Hb2p_TO_Hc2pSI))// &
      "~"//TRIM(Hprime_unit_ol_TeX))
    CALL Output(DUMMY)

  END SUBROUTINE ref1071

  !---------------------------------------------------------------------------

  SUBROUTINE ref1072 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "1072"
    type = "M"
    chem = "sulfur dioxide" ; casrn = "7446-09-5" ! SO2
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,18.,25.,35.,50./)
    temp = temp + CtoK
    Harray = (/3.28,2.2,1.55,1.23,0.89,0.56/)
    Harray = Harray * Hbp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1072

  !---------------------------------------------------------------------------

  SUBROUTINE ref1073 ! KHcc [1]
    IMPLICIT NONE

    ref = "1073"
    type = "M"

    chem = "2-butanone" ; casrn = "78-93-3" ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL Output(KHcc_TIMES_HcpSI_atT0/2.1E-3)

    chem = "1-butanol" ; casrn = "71-36-3" ! C4H9OH
    CALL Output(KHcc_TIMES_HcpSI_atT0/2.9E-3)

    chem = "pyridine" ; casrn = "110-86-1" ! C5H5N
    CALL Output(KHcc_TIMES_HcpSI_atT0/7.4E-4)

    chem = "3-methylpyridine" ; casrn = "108-99-6"
    CALL Output(KHcc_TIMES_HcpSI_atT0/9.7E-4)

    chem = "2,6-dimethylpyrazine" ; casrn = "108-50-9"
    CALL Output(KHcc_TIMES_HcpSI_atT0/4.1E-4)

  END SUBROUTINE ref1073

  !---------------------------------------------------------------------------

  SUBROUTINE ref1076 ! KHpx [atm]
    IMPLICIT NONE
    REAL :: DeltaH, DeltaS

    ref = "1076"
    type = "T"
    chem = "sulfuric acid" ; casrn = "7664-93-9" ! H2SO4
    DeltaH = 84.4E3
    DeltaS = 135.2
    mindHR = DeltaH / Rgas
    Hominus    = KHpx_TIMES_HcpSI / EXP((DeltaS-DeltaH/T0)/Rgas)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref1076

  !---------------------------------------------------------------------------

  SUBROUTINE ref1096 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1096"
    type = "V"

    ! p. 580:
    CALL CalcH("DE",          "101-84-8",    8.71)   ! diphenyl ether
    CALL CalcH("PCDE-1",     "2689-07-8",   32.36)
    CALL CalcH("PCDE-2",     "6452-49-9",    8.13)
    CALL CalcH("PCDE-3",     "7005-72-3",    9.12)
    CALL CalcH("PCDE-5",       "_PCDE-5",    4.17)
    CALL CalcH("PCDE-7",    "51892-26-3",    5.25)
    CALL CalcH("PCDE-8",       "_PCDE-8",   30.90)
    CALL CalcH("PCDE-9",       "_PCDE-9",   12.59)
    CALL CalcH("PCDE-10",   "28419-69-4",   19.95)
    CALL CalcH("PCDE-12",     "_PCDE-12",    9.12)
    CALL CalcH("PCDE-13",     "_PCDE-13",    7.94)
    CALL CalcH("PCDE-14",     "_PCDE-14",   15.49)
    CALL CalcH("PCDE-15",    "2444-89-5",    4.68)
    CALL CalcH("PCDE-17",     "_PCDE-17",    2.24)
    CALL CalcH("PCDE-21",     "_PCDE-21",    3.63)
    CALL CalcH("PCDE-22",     "_PCDE-22",    3.09)
    CALL CalcH("PCDE-23",     "_PCDE-23",    4.57)
    CALL CalcH("PCDE-24",     "_PCDE-24",   33.88)
    CALL CalcH("PCDE-25",     "_PCDE-25",    6.61)
    CALL CalcH("PCDE-28",     "_PCDE-28",   33.88)
    CALL CalcH("PCDE-29",     "_PCDE-29",  112.20)
    CALL CalcH("PCDE-30",     "_PCDE-30",   70.79)
    CALL CalcH("PCDE-31",   "65075-00-5",    6.31)
    CALL CalcH("PCDE-32",     "_PCDE-32",   23.99)
    CALL CalcH("PCDE-33",     "_PCDE-33",    2.95)
    CALL CalcH("PCDE-35",     "_PCDE-35",    4.47)
    CALL CalcH("PCDE-37",     "_PCDE-37",    6.31)
    CALL CalcH("PCDE-38",     "_PCDE-38",  109.65)
    CALL CalcH("PCDE-39",     "_PCDE-39",    6.31)
    CALL CalcH("PCDE-41",     "_PCDE-41",   18.20)
    CALL CalcH("PCDE-42",     "_PCDE-42",   17.38)
    CALL CalcH("PCDE-47",   "28076-73-5",   34.67)
    CALL CalcH("PCDE-48",     "_PCDE-48",   63.10)
    CALL CalcH("PCDE-49",     "_PCDE-49",   38.90)
    CALL CalcH("PCDE-55",     "_PCDE-55",   42.66)
    CALL CalcH("PCDE-56",     "_PCDE-56",   22.91)
    CALL CalcH("PCDE-60",     "_PCDE-60",   27.54)
    CALL CalcH("PCDE-61",     "_PCDE-61",  151.36)
    CALL CalcH("PCDE-62",     "_PCDE-62",  109.65)
    CALL CalcH("PCDE-63",     "_PCDE-63",   67.61)
    ! p. 581:
    CALL CalcH("PCDE-64",     "_PCDE-64",   26.30)
    CALL CalcH("PCDE-65",     "_PCDE-65",  107.15)
    CALL CalcH("PCDE-66",   "61328-46-9",   40.74)
    CALL CalcH("PCDE-67",     "_PCDE-67",  112.20)
    CALL CalcH("PCDE-68",     "_PCDE-68",  100.00)
    CALL CalcH("PCDE-70",     "_PCDE-70",   56.23)
    CALL CalcH("PCDE-71",     "_PCDE-71",   21.88)
    CALL CalcH("PCDE-74",   "61328-45-8",   52.48)
    CALL CalcH("PCDE-75",     "_PCDE-75",   60.26)
    CALL CalcH("PCDE-77",   "56348-72-2",   24.55)
    CALL CalcH("PCDE-79",     "_PCDE-79",   95.50)
    CALL CalcH("PCDE-81",     "_PCDE-81",   67.61)
    CALL CalcH("PCDE-82",     "_PCDE-82",   12.02)
    CALL CalcH("PCDE-85",   "71585-37-0",   19.05)
    CALL CalcH("PCDE-87",     "_PCDE-87",   45.71)
    CALL CalcH("PCDE-89",     "_PCDE-89",   15.49)
    CALL CalcH("PCDE-90",     "_PCDE-90",   64.57)
    CALL CalcH("PCDE-91",     "_PCDE-91",   25.70)
    CALL CalcH("PCDE-97",     "_PCDE-97",   30.20)
    CALL CalcH("PCDE-99",   "60123-64-0",   54.95)
    CALL CalcH("PCDE-100", "104294-16-8",   46.77)
    CALL CalcH("PCDE-101", "131138-21-1",   63.10)
    CALL CalcH("PCDE-102",   "_PCDE-102",   26.92)
    CALL CalcH("PCDE-105",  "85918-31-6",   23.99)
    CALL CalcH("PCDE-108",   "_PCDE-108",   67.61)
    CALL CalcH("PCDE-109",   "_PCDE-109",   72.44)
    CALL CalcH("PCDE-110",   "_PCDE-110",   27.54)
    CALL CalcH("PCDE-114",   "_PCDE-114",   81.26)
    CALL CalcH("PCDE-115",   "_PCDE-115",   87.10)
    CALL CalcH("PCDE-116",   "_PCDE-116",  151.36)
    CALL CalcH("PCDE-117",   "_PCDE-117",   91.20)
    CALL CalcH("PCDE-118",   "_PCDE-118",   64.56)
    CALL CalcH("PCDE-119",   "_PCDE-119",   75.86)
    CALL CalcH("PCDE-120",   "_PCDE-120",  208.93)
    CALL CalcH("PCDE-123",   "_PCDE-123",   74.13)
    CALL CalcH("PCDE-126",  "94339-59-0",  100.00)
    CALL CalcH("PCDE-128",  "71585-39-2",   12.02)
    CALL CalcH("PCDE-130",   "_PCDE-130",   66.07)
    CALL CalcH("PCDE-132",   "_PCDE-132",   16.22)
    CALL CalcH("PCDE-137",  "71585-36-9",   54.95)
    CALL CalcH("PCDE-138",  "71585-38-1",   34.67)
    CALL CalcH("PCDE-139",   "_PCDE-139",  102.33)
    CALL CalcH("PCDE-140", "106220-82-0",   33.88)
    CALL CalcH("PCDE-146",   "_PCDE-146",   97.92)
    CALL CalcH("PCDE-147",   "_PCDE-147",  100.00)
    CALL CalcH("PCDE-149",   "_PCDE-149",   32.36)
    CALL CalcH("PCDE-153",  "71859-30-8",   79.43)
    CALL CalcH("PCDE-154", "106220-81-9",   70.79)
    CALL CalcH("PCDE-156",   "_PCDE-156",   81.28)
    CALL CalcH("PCDE-157",   "_PCDE-157",   36.31)
    CALL CalcH("PCDE-163",   "_PCDE-163",   63.10)
    ! p. 582:
    CALL CalcH("PCDE-166",   "_PCDE-166",  199.53)
    CALL CalcH("PCDE-167", "131138-20-0",  120.23)
    CALL CalcH("PCDE-170",   "_PCDE-170",   50.12)
    CALL CalcH("PCDE-174",   "_PCDE-174",   56.12)
    CALL CalcH("PCDE-177",   "_PCDE-177",   72.44)
    CALL CalcH("PCDE-180",  "83992-69-2",  199.53)
    CALL CalcH("PCDE-181",   "_PCDE-181",  295.12)
    CALL CalcH("PCDE-187",   "_PCDE-187",  134.89)
    CALL CalcH("PCDE-189",   "_PCDE-189",  162.18)
    CALL CalcH("PCDE-190",   "_PCDE-190",  173.78)
    CALL CalcH("PCDE-194",   "_PCDE-194",  234.42)
    CALL CalcH("PCDE-195",   "_PCDE-195",  562.34)
    CALL CalcH("PCDE-199",   "_PCDE-199",  380.19)
    CALL CalcH("PCDE-203",   "_PCDE-203",  436.51)
    CALL CalcH("PCDE-206",  "83992-73-8", 1949.84)
    CALL CalcH("PCDE-209",  "31710-30-2", 14125.37)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI / H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1096

  !---------------------------------------------------------------------------

  SUBROUTINE ref1107 ! KHcc [1]
    IMPLICIT NONE

    ref = "1107"
    type = "M"

    CALL CalcH("1,1-dichloroethene",              "75-35-4",  1.086,   -3871.,  13.083) ! CH2CCl2
    CALL CalcH("cyclohexane",                    "110-82-7",  7.331,   -4164.,  16.014) ! C6H{12}
    CALL CalcH("trichloroethene",                 "79-01-6",  0.415,   -4553.,  14.415) ! C2HCl3 trichloroethylene
    CALL CalcH("methylbenzene",                  "108-88-3",  0.268,   -4362.,  13.329) ! C6H5CH3 toluene
    CALL CalcH("1,3,5-trichlorobenzene",         "108-70-3",  0.214,   -3819.,  11.341) ! C6H3Cl3
    CALL CalcH("1,2,3-trichlorobenzene",          "87-61-6",  0.108,   -3927.,  10.989) ! C6H3Cl3
    CALL CalcH("1,1,2-trichlorotrifluoroethane",  "76-13-1", 13.49,    -4002.,  16.068) ! C2F3Cl3 R113
    CALL CalcH("1,2-dichloropropane",             "78-87-5",  0.112,   -3980.,  11.181) ! C3H6Cl2
    CALL CalcH("1,1,2-trichloroethane",           "79-00-5",  0.037,   -4420.,  11.547) ! CHCl2CH2Cl
    CALL CalcH("chlorobenzene",                  "108-90-7",  0.155,   -4041.,  11.722) ! C6H5Cl
    CALL CalcH("1,2,4-trichlorobenzene",         "120-82-1",  0.159,   -3178.,   8.892) ! C6H3Cl3
    CALL CalcH("hexachlorobutadiene",             "87-68-3",  0.624,   -4578.,  14.922) ! CCl2CClCClCCl2
    CALL CalcH("fluorobenzene",                  "462-06-6",  0.269,   -3968.,  12.039) ! C6H5F
    CALL CalcH("methoxybenzene",                 "100-66-3",  0.015,   -4466.,  10.820) ! C6H5OCH3 anisole
    CALL CalcH("naphthalene",                     "91-20-3",  0.031,   -3288.,   7.552) ! C{10}H8
    CALL CalcH("biphenyl",                        "92-52-4",  0.053,   -4201.,  11.204) ! (C6H5)2
    CALL CalcH("pyridine",                       "110-86-1",  0.0086,   2599., -13.451) ! C5H5N
    CALL CalcH("nitrobenzene",                    "98-95-3",  0.013,  -10921.,  32.598) ! C6H5NO2
    CALL CalcH("3-hexanone",                     "589-38-8",  0.052,    -529.,  -1.143) ! C6H{12}O
    CALL CalcH("ethyl ethanoate",                "141-78-6",  0.040,    -315.,  -2.154) ! CH3COOC2H5 ethyl acetate

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc, a, b)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc ! from Tab. 2
      REAL,             INTENT(IN)           :: a, b    ! from Tab. 3
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / EXP(a/T0+b)
      mindHR = -a + T0 ! see ref958, eqn (34) why T0 is added
      IF ((casrn_=="92-52-4").OR.(casrn_=="589-38-8").OR.(casrn_=="141-78-6")) THEN
        CALL MakeNote("1107notused","The values of "//TRIM(citet())// &
          " are not used here because, according to them, the calculated" // &
          " regression does not match the theoretical expectation for" // &
          " this species.")
        CALL Output(DUMMY)
      ELSE
        CALL consistency_check(Hominus, KHcc_TIMES_HcpSI_atT0/KHcc, &
          "The data listed in Tabs.~2 and 3")
        CALL Output(Hominus, mindHR)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref1107

  !---------------------------------------------------------------------------

  SUBROUTINE ref1110 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "1110"
    type = "M"
    chem = "glutaraldehyde" ; casrn = "111-30-8"
    mindHR = 9187.99
    Hominus    = cH2O / (1E3*EXP(29.1352-mindHR/T0))
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref1110

  !---------------------------------------------------------------------------

  SUBROUTINE ref1112 ! KHpc [kPa*m3/mol]
    IMPLICIT NONE

    ref = "1112"

    CALL CalcH("dibenzofuran",    "132-64-9", 1.67, 1.86)
    CALL CalcH("PCDF-1",        "84761-86-4", 1.92)
    CALL CalcH("PCDF-2",        "51230-49-0", 2.04)
    CALL CalcH("PCDF-3",        "25074-67-3", 2.11)
    CALL CalcH("PCDF-4",        "74992-96-4", 1.95)
    CALL CalcH("PCDF-12",       "64126-85-8", 2.17)
    CALL CalcH("PCDF-13",       "94538-00-8", 2.31)
    CALL CalcH("PCDF-14",       "94538-01-9", 2.17)
    CALL CalcH("PCDF-16",       "74992-97-5", 2.16)
    CALL CalcH("PCDF-17",       "94538-02-0", 2.29)
    CALL CalcH("PCDF-18",       "81638-37-1", 2.40)
    CALL CalcH("PCDF-19",       "70648-14-5", 2.31)
    CALL CalcH("PCDF-23",       "64126-86-9", 2.36)
    CALL CalcH("PCDF-24",       "24478-74-8", 2.27)
    CALL CalcH("PCDF-26",       "60390-27-4", 2.25)
    CALL CalcH("PCDF-27",       "74992-98-6", 2.30)
    CALL CalcH("PCDF-28",        "5409-83-6", 2.34, 2.20)
    CALL CalcH("PCDF-34",       "94570-83-9", 2.27)
    CALL CalcH("PCDF-36",       "74918-40-4", 2.34)
    CALL CalcH("PCDF-37",       "58802-21-4", 2.47)
    CALL CalcH("PCDF-46",       "64560-13-0", 2.35)
    CALL CalcH("PCDF-123",      "83636-47-9", 2.46)
    CALL CalcH("PCDF-124",      "24478-73-7", 2.39)
    CALL CalcH("PCDF-126",      "64560-15-2", 2.36)
    CALL CalcH("PCDF-127",      "83704-37-4", 2.37)
    CALL CalcH("PCDF-128",      "83704-34-1", 2.59)
    CALL CalcH("PCDF-129",      "83704-38-5", 2.68)
    CALL CalcH("PCDF-134",      "82911-61-3", 2.45)
    CALL CalcH("PCDF-136",      "83704-39-6", 2.52)
    CALL CalcH("PCDF-137",      "64560-16-3", 2.61)
    CALL CalcH("PCDF-138",      "76621-12-0", 2.62)
    CALL CalcH("PCDF-139",      "83704-40-9", 2.64)
    CALL CalcH("PCDF-146",      "82911-60-2", 2.55)
    CALL CalcH("PCDF-147",      "83704-41-0", 2.50)
    CALL CalcH("PCDF-148",      "64560-14-1", 2.59)
    CALL CalcH("PCDF-149",      "70648-13-4", 2.54)
    CALL CalcH("PCDF-234",      "57117-34-7", 2.49)
    CALL CalcH("PCDF-236",       "_PCDF-236", 2.53)
    CALL CalcH("PCDF-237",      "58802-17-8", 2.55)
    CALL CalcH("PCDF-238",      "57117-32-5", 2.49)
    CALL CalcH("PCDF-239",       "_PCDF-239", 2.66)
    CALL CalcH("PCDF-246",      "58802-14-5", 2.62)
    CALL CalcH("PCDF-247",      "83704-42-1", 2.49)
    CALL CalcH("PCDF-248",      "54589-71-8", 2.51)
    CALL CalcH("PCDF-249",       "_PCDF-249", 2.61)
    CALL CalcH("PCDF-346",       "_PCDF-346", 2.63)
    CALL CalcH("PCDF-347",      "83704-44-3", 2.59)
    CALL CalcH("PCDF-348",       "_PCDF-348", 2.40)
    CALL CalcH("PCDF-349",      "83704-46-5", 2.43)
    CALL CalcH("PCDF-1234",     "24478-72-6", 2.56)
    CALL CalcH("PCDF-1236",     "83704-21-6", 2.61)
    CALL CalcH("PCDF-1237",     "83704-22-7", 2.59)
    CALL CalcH("PCDF-1238",     "62615-08-1", 2.70)
    CALL CalcH("PCDF-1239",     "83704-23-8", 2.90)
    CALL CalcH("PCDF-1246",     "71998-73-7", 2.71)
    CALL CalcH("PCDF-1247",     "83719-40-8", 2.54)
    CALL CalcH("PCDF-1248",     "64126-87-0", 2.74)
    CALL CalcH("PCDF-1249",     "_PCDF-1249", 2.87)
    CALL CalcH("PCDF-1267",     "83704-25-0", 2.45)
    CALL CalcH("PCDF-1268",     "83710-07-0", 2.74)
    CALL CalcH("PCDF-1269",     "_PCDF-1269", 2.85)
    CALL CalcH("PCDF-1278",     "58802-20-3", 2.68)
    CALL CalcH("PCDF-1279",     "83704-26-1", 2.84)
    CALL CalcH("PCDF-1289",     "_PCDF-1289", 2.98)
    CALL CalcH("PCDF-1346",     "83704-27-2", 2.80)
    CALL CalcH("PCDF-1347",     "70648-16-7", 2.71)
    CALL CalcH("PCDF-1348",     "92341-04-3", 2.70)
    CALL CalcH("PCDF-1349",     "_PCDF-1349", 2.76)
    CALL CalcH("PCDF-1367",     "57117-36-9", 2.71)
    CALL CalcH("PCDF-1368",     "71998-72-6", 2.79)
    CALL CalcH("PCDF-1369",     "83690-98-6", 2.83)
    CALL CalcH("PCDF-1378",     "57117-35-8", 2.81)
    CALL CalcH("PCDF-1379",     "64560-17-4", 2.90)
    CALL CalcH("PCDF-1467",     "66794-59-0", 2.77)
    CALL CalcH("PCDF-1468",     "82911-58-8", 2.93)
    CALL CalcH("PCDF-1469",     "_PCDF-1469", 2.90)
    CALL CalcH("PCDF-1478",     "83704-29-4", 2.81)
    CALL CalcH("PCDF-1678",     "_PCDF-1678", 2.76)
    CALL CalcH("PCDF-2346",     "83704-30-7", 2.79)
    CALL CalcH("PCDF-2347",     "83704-31-8", 2.64)
    CALL CalcH("PCDF-2348",     "83704-32-9", 2.55)
    CALL CalcH("PCDF-2367",     "57117-39-2", 2.61)
    CALL CalcH("PCDF-2368",     "57117-37-0", 2.62)
    CALL CalcH("PCDF-2378",     "51207-31-9", 2.57, 2.93)
    CALL CalcH("PCDF-2467",     "57117-38-1", 2.73)
    CALL CalcH("PCDF-2468",     "58802-19-0", 2.82)
    CALL CalcH("PCDF-3467",     "57117-40-5", 2.85)
    CALL CalcH("PCDF-12346",    "83704-47-6", 2.85)
    CALL CalcH("PCDF-12347",    "83704-48-7", 2.65)
    CALL CalcH("PCDF-12348",    "67517-48-0", 2.74)
    CALL CalcH("PCDF-12349",    "83704-49-8", 2.99)
    CALL CalcH("PCDF-12367",    "57117-42-7", 2.62)
    CALL CalcH("PCDF-12368",    "83704-51-2", 2.81)
    CALL CalcH("PCDF-12369",    "83704-52-3", 3.03)
    CALL CalcH("PCDF-12378",    "57117-41-6", 2.72)
    CALL CalcH("PCDF-12379",    "83704-53-4", 3.00)
    CALL CalcH("PCDF-12389",    "83704-54-5", 3.04)
    CALL CalcH("PCDF-12467",    "83704-50-1", 2.76)
    CALL CalcH("PCDF-12468",    "69698-57-3", 3.02)
    CALL CalcH("PCDF-12469",    "70648-24-7", 3.17)
    CALL CalcH("PCDF-12478",    "58802-15-6", 2.79)
    CALL CalcH("PCDF-12479",    "71998-74-8", 2.99)
    CALL CalcH("PCDF-12489",    "70648-23-6", 3.11)
    CALL CalcH("PCDF-12679",    "70872-82-1", 2.90)
    CALL CalcH("PCDF-13467",    "83704-36-3", 2.95)
    CALL CalcH("PCDF-13468",    "83704-55-6", 3.01)
    CALL CalcH("PCDF-13469",    "70648-15-6", 3.08)
    CALL CalcH("PCDF-13478",    "58802-16-7", 2.85)
    CALL CalcH("PCDF-13479",    "70648-20-3", 2.98)
    CALL CalcH("PCDF-13678",    "70648-21-4", 2.88)
    CALL CalcH("PCDF-14678",   "_PCDF-14678", 3.05)
    CALL CalcH("PCDF-23467",    "57117-43-8", 2.84)
    CALL CalcH("PCDF-23468",    "67481-22-5", 2.82)
    CALL CalcH("PCDF-23478",    "57117-31-4", 2.59, 3.24)
    CALL CalcH("PCDF-23489",   "_PCDF-23489", 2.72)
    CALL CalcH("PCDF-123467",   "79060-60-9", 2.83)
    CALL CalcH("PCDF-123468",   "69698-60-8", 2.99)
    CALL CalcH("PCDF-123469",   "91538-83-9", 3.25)
    CALL CalcH("PCDF-123478",   "70648-26-9", 2.72, 2.58)
    CALL CalcH("PCDF-123479",   "91538-84-0", 3.04)
    CALL CalcH("PCDF-123489",   "92341-07-6", 3.06)
    CALL CalcH("PCDF-123678",   "57117-44-9", 2.72, 2.96)
    CALL CalcH("PCDF-123679",   "92341-06-5", 3.02)
    CALL CalcH("PCDF-123689",   "75198-38-8", 3.13)
    CALL CalcH("PCDF-123789",   "72918-21-9", 3.02)
    CALL CalcH("PCDF-124678",   "67562-40-7", 2.97)
    CALL CalcH("PCDF-124679",   "75627-02-0", 3.19)
    CALL CalcH("PCDF-124689",   "69698-59-5", 3.38)
    CALL CalcH("PCDF-134678",   "71998-75-9", 3.05)
    CALL CalcH("PCDF-134679",   "92341-05-4", 3.20)
    CALL CalcH("PCDF-234678",   "60851-34-5", 2.75)
    CALL CalcH("PCDF-1234678",  "67562-39-4", 2.85, 2.46)
    CALL CalcH("PCDF-1234679",  "70648-25-8", 3.19)
    CALL CalcH("PCDF-1234689",  "69698-58-4", 3.29)
    CALL CalcH("PCDF-1234789",  "55673-89-7", 3.00)
    CALL CalcH("PCDF-12346789", "39001-02-0", 3.11, 2.88)

    CALL CalcH("DD",              "262-12-4", 1.96, 1.93) ! dibenzo-p-dioxin
    CALL CalcH("PCDD-1",        "39227-53-7", 2.24, 2.20)
    CALL CalcH("PCDD-2",        "39227-54-8", 2.34, 1.90)
    CALL CalcH("PCDD-12",         "_PCDD-12", 2.51)
    CALL CalcH("PCDD-13",       "50585-39-2", 2.58)
    CALL CalcH("PCDD-14",         "_PCDD-14", 2.50)
    CALL CalcH("PCDD-16",       "38178-38-0", 2.50)
    CALL CalcH("PCDD-17",         "_PCDD-17", 2.56)
    CALL CalcH("PCDD-18",         "_PCDD-18", 2.58)
    CALL CalcH("PCDD-19",         "_PCDD-19", 2.73)
    CALL CalcH("PCDD-23",       "29446-15-9", 2.60, 2.18)
    CALL CalcH("PCDD-27",       "33857-26-0", 2.55, 2.09)
    CALL CalcH("PCDD-28",       "38964-22-6", 2.64, 2.67)
    CALL CalcH("PCDD-123",       "_PCDD-123", 2.75)
    CALL CalcH("PCDD-124",      "39227-58-2", 2.74, 2.42)
    CALL CalcH("PCDD-126",       "_PCDD-126", 2.71)
    CALL CalcH("PCDD-127",       "_PCDD-127", 2.66)
    CALL CalcH("PCDD-128",       "_PCDD-128", 2.78)
    CALL CalcH("PCDD-129",       "_PCDD-129", 2.96)
    CALL CalcH("PCDD-136",       "_PCDD-136", 2.80)
    CALL CalcH("PCDD-137",      "67028-17-5", 2.83)
    CALL CalcH("PCDD-138",       "_PCDD-138", 2.75)
    CALL CalcH("PCDD-139",       "_PCDD-139", 3.01)
    CALL CalcH("PCDD-146",       "_PCDD-146", 2.97)
    CALL CalcH("PCDD-147",       "_PCDD-147", 2.78)
    CALL CalcH("PCDD-178",       "_PCDD-178", 2.79)
    CALL CalcH("PCDD-237",      "33857-28-2", 2.75)
    CALL CalcH("PCDD-1234",     "30746-58-8", 2.87, 2.52)
    CALL CalcH("PCDD-1236",     "_PCDD-1236", 2.91)
    CALL CalcH("PCDD-1237",     "67028-18-6", 2.83, 3.24)
    CALL CalcH("PCDD-1238",     "53555-02-5", 2.85)
    CALL CalcH("PCDD-1239",     "_PCDD-1239", 3.14)
    CALL CalcH("PCDD-1246",     "_PCDD-1246", 3.15)
    CALL CalcH("PCDD-1247",     "_PCDD-1247", 2.85)
    CALL CalcH("PCDD-1248",     "_PCDD-1248", 2.95)
    CALL CalcH("PCDD-1249",     "_PCDD-1249", 3.17)
    CALL CalcH("PCDD-1267",     "_PCDD-1267", 2.76)
    CALL CalcH("PCDD-1268",     "_PCDD-1268", 2.95)
    CALL CalcH("PCDD-1269",     "_PCDD-1269", 3.15)
    CALL CalcH("PCDD-1278",     "34816-53-0", 2.83)
    CALL CalcH("PCDD-1279",     "_PCDD-1279", 3.08)
    CALL CalcH("PCDD-1289",     "_PCDD-1289", 3.13)
    CALL CalcH("PCDD-1368",     "33423-92-6", 2.94, 3.15)
    CALL CalcH("PCDD-1369",     "_PCDD-1369", 3.22)
    CALL CalcH("PCDD-1378",     "50585-46-1", 2.90)
    CALL CalcH("PCDD-1379",     "62470-53-5", 3.23)
    CALL CalcH("PCDD-1469",     "_PCDD-1469", 3.42)
    CALL CalcH("PCDD-1478",     "_PCDD-1478", 2.96)
    CALL CalcH("PCDD-2378",      "1746-01-6", 2.79, 2.48)
    CALL CalcH("PCDD-12346",   "_PCDD-12346", 3.25)
    CALL CalcH("PCDD-12347",    "39227-61-7", 2.91, 3.65)
    CALL CalcH("PCDD-12367",   "_PCDD-12367", 2.89)
    CALL CalcH("PCDD-12368",   "_PCDD-12368", 2.98)
    CALL CalcH("PCDD-12378",    "40321-76-4", 2.83)
    CALL CalcH("PCDD-12467",   "_PCDD-12467", 3.16)
    CALL CalcH("PCDD-12468",   "_PCDD-12468", 3.33)
    CALL CalcH("PCDD-12469",   "_PCDD-12469", 3.56)
    CALL CalcH("PCDD-12478",    "58802-08-7", 2.96)
    CALL CalcH("PCDD-13467",   "_PCDD-13467", 3.28)
    CALL CalcH("PCDD-13468",   "_PCDD-13468", 3.25)
    CALL CalcH("PCDD-14678",   "_PCDD-14678", 3.29)
    CALL CalcH("PCDD-23467",   "_PCDD-23467", 3.15)
    CALL CalcH("PCDD-23468",   "_PCDD-23468", 3.19)
    CALL CalcH("PCDD-123467", "_PCDD-123467", 3.19)
    CALL CalcH("PCDD-123468", "_PCDD-123468", 3.26)
    CALL CalcH("PCDD-123469", "_PCDD-123469", 3.60)
    CALL CalcH("PCDD-123478",   "39227-28-6", 2.84, 3.20)
    CALL CalcH("PCDD-123678",   "57653-85-7", 2.84)
    CALL CalcH("PCDD-124678", "_PCDD-124678", 3.23)
    CALL CalcH("PCDD-124679",   "39227-62-8", 3.55)
    CALL CalcH("PCDD-134678", "_PCDD-134678", 3.25)
    CALL CalcH("PCDD-134679", "_PCDD-134679", 3.64)
    CALL CalcH("PCDD-234678", "_PCDD-234678", 3.08)
    CALL CalcH("PCDD-1234678",  "35822-46-9", 3.08, 3.36)
    CALL CalcH("PCDD-1234679",  "58200-70-7", 3.51)
    CALL CalcH("PCDD-12346789",  "3268-87-9", 3.29, 2.88)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, minlogH_type_Q, minlogH_type_V)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: minlogH_type_Q
      REAL, OPTIONAL,   INTENT(IN) :: minlogH_type_V

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      type = "Q"
      Hominus = KHpcSI_TIMES_HcpSI * (10.**minlogH_type_Q) / 1000.
      CALL Output(Hominus)

      IF (PRESENT(minlogH_type_V)) THEN
        type = "V"
        Hominus = KHpcSI_TIMES_HcpSI * (10.**minlogH_type_V) / 1000.
        CALL Output(Hominus)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref1112

  !---------------------------------------------------------------------------

  SUBROUTINE ref1113 ! KHcc [1]
    IMPLICIT NONE

    ref = "1113"
    type = "M"

    ! erratum for vinyl chloride:
    chem = "chloroethene" ; casrn = "75-01-4"
    CALL MakeNote(TRIM(ref), TRIM(citet())// &
      " show vinyl chloride in their Table 2 but most probably " // &
      "they meant to refer to dichloromethane instead.")
    CALL Output(DUMMY)

    CALL CalcH("1,1-dichloroethene",    "75-35-4", 0.916) ! CH2CCl2
    CALL CalcH("dichloromethane",       "75-09-2", 0.117)
    CALL CalcH("trichloromethane",      "67-66-3", 0.135) ! CHCl3 chloroform
    CALL CalcH("1,1,1-trichloroethane", "71-55-6", 0.571) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("tetrachloromethane",    "56-23-5", 1.010) ! CCl4 carbontetrachloride
    CALL CalcH("trichloroethene",       "79-01-6", 0.369) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",    "127-18-4", 0.647) ! C2Cl4 tetrachloroethylene
    CALL CalcH("1,4-dichlorobenzene",  "106-46-7", 0.162) ! C6H4Cl2 $p$-dichlorobenzene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc
      IF (casrn_=="75-09-2") CALL MakeNote(TRIM(ref))
      CALL MakeNoteOtherTemp("293")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1113

  !---------------------------------------------------------------------------

  SUBROUTINE ref1114 ! KHcc [1] and KHpx [atm]
    IMPLICIT NONE

    ref = "1114"
    type = "M"

    CALL CalcH("benzene",            "71-43-2", 0.180, 7.44, 1448.) ! C6H6
    CALL CalcH("methylbenzene",     "108-88-3", 0.196, 7.89, 1565.) ! C6H5CH3 toluene
    CALL CalcH("trichloroethene",    "79-01-6", 0.291, 8.62, 1736.) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene", "127-18-4", 0.517, 9.09, 1795.) ! C2Cl4 tetrachloroethylene

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hc, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: Hc, A, B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      Hominus = KHcc_TIMES_HcpSI_atT0 / Hc
      CALL Output(Hominus)

      Hominus = KHpx_TIMES_HcpSI/(10.**(A-B/T0))
      mindHR = B * LOG(10.)
      CALL MakeNote("seawater")
      CALL Output(Hominus, mindHR)

    END SUBROUTINE CalcH

  END SUBROUTINE ref1114

  !---------------------------------------------------------------------------

  SUBROUTINE ref1115 ! KHpx [atm] and KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1115"
    type = "Q"

    ! C1:
    CALL CalcH("methanol",                  "67-56-1", 3.7424E-01, 6.7363E-06) ! CH3OH
    ! C2:
    CALL CalcH("ethanol",                   "64-17-5", 4.1267E-01, 7.4280E-06) ! C2H5OH
    ! C3:
    CALL CalcH("1-propanol",                "71-23-8", 4.4129E-01, 7.9431E-06) ! C3H7OH
    CALL CalcH("2-propanol",                "67-63-0", 6.1305E-01, 1.1035E-05) ! C3H7OH isopropanol
    ! C4:
    CALL CalcH("1-butanol",                 "71-36-3", 4.8685E-01, 8.7634E-06) ! C4H9OH
    CALL CalcH("2-methyl-1-propanol",       "78-83-1", 6.5536E-01, 1.1797E-05) ! C4H{10}O isobutanol
    CALL CalcH("2-butanol",                 "78-92-2", 4.7289E-01, 8.5121E-06) ! C4H{10}O {sec}-butanol
    CALL CalcH("2-methyl-2-propanol",       "75-65-0", 1.8141E+00, 3.2653E-05) ! C4H{10}O {tert}-butanol
    ! C5:
    CALL CalcH("1-pentanol",                "71-41-0", 7.1022E-01, 1.2784E-05) ! C5H{11}OH amylalcohol
    CALL CalcH("2-pentanol",              "6032-29-7", 8.4200E-01, 1.5156E-05) ! C5H{12}O {sec}-pentanol
    CALL CalcH("3-pentanol",               "584-02-1", 1.0416E+00, 1.8749E-05) ! C5H{12}O
    CALL CalcH("2-methyl-1-butanol",       "137-32-6", 6.5537E-01, 1.1797E-05) ! C5H{12}O isopentanol
    CALL CalcH("2-methyl-2-butanol",        "75-85-4", 8.9700E-01, 1.6146E-05) ! C5H{12}O {tert}-pentanol
    CALL CalcH("3-methyl-1-butanol",       "123-51-3", 7.4253E-01, 1.3366E-05) ! C5H{12}O
    CALL CalcH("3-methyl-2-butanol",       "598-75-4", 1.0041E+00, 1.8075E-05) ! C5H{12}O
    ! C6:
    CALL CalcH("1-hexanol",                "111-27-3", 1.1721E+00, 2.1097E-05)
    CALL CalcH("2-hexanol",                "626-93-7", 1.3101E+00, 2.3582E-05)
    CALL CalcH("3-hexanol",                "623-37-0", 1.3300E+00, 2.3940E-05)
    CALL CalcH("2-methyl-1-pentanol",      "105-30-6", 1.7548E+00, 3.1586E-05)
    CALL CalcH("2-methyl-2-pentanol",      "590-36-3", 1.1020E+00, 1.9836E-05)
    CALL CalcH("2-methyl-3-pentanol",      "565-67-3", 1.4734E+00, 2.6521E-05)
    CALL CalcH("3-methyl-2-pentanol",      "565-60-6", 1.9365E+00, 3.4856E-05)
    CALL CalcH("3-methyl-3-pentanol",       "77-74-7", 7.8243E-01, 1.4084E-05)
    CALL CalcH("4-methyl-2-pentanol",      "108-11-2", 2.9485E+00, 5.3073E-05)
    CALL CalcH("2-ethyl-1-butanol",         "97-95-0", 1.1323E+00, 2.0382E-05)
    CALL CalcH("2,2-dimethyl-1-butanol",  "1185-33-7", 1.9199E+00, 3.4559E-05)
    CALL CalcH("2,3-dimethyl-1-butanol", "19550-30-2", 6.6532E-01, 1.1976E-05)
    CALL CalcH("3,3-dimethyl-2-butanol",   "464-07-3", 1.1232E+00, 2.0217E-05)
    ! C7:
    CALL CalcH("1-heptanol",                                  "111-70-6", 1.0500E+00, 1.8900E-05)
    CALL CalcH("2-heptanol",                                  "543-49-7", 4.5676E+00, 8.2216E-05)
    CALL CalcH("3-heptanol",                                  "589-82-2", 2.6007E+00, 4.6813E-05)
    CALL CalcH("4-heptanol",                                  "589-55-9", 2.5300E+00, 4.5540E-05)
    CALL CalcH("2-methyl-1-hexanol",                          "624-22-6", 3.2028E+00, 5.7651E-05)
    CALL CalcH("3-methyl-1-hexanol",                        "13231-81-7", 4.1937E+00, 7.5487E-05)
    CALL CalcH("4-methyl-1-hexanol",                          "818-49-5", 4.3249E+00, 7.7849E-05)
    CALL CalcH("5-methyl-1-hexanol",                          "627-98-5", 1.9208E+00, 3.4574E-05)
    CALL CalcH("2-methyl-2-hexanol",                          "625-23-0", 8.5765E-01, 1.5438E-05)
    CALL CalcH("3-methyl-2-hexanol",                         "2313-65-7", 1.1249E+00, 2.0249E-05)
    CALL CalcH("4-methyl-2-hexanol",                         "2313-61-3", 1.0973E+00, 1.9751E-05)
    CALL CalcH("5-methyl-2-hexanol",                          "627-59-8", 1.3135E+00, 2.3643E-05)
    CALL CalcH("2-methyl-3-hexanol",                          "617-29-8", 9.4745E-01, 1.7054E-05)
    CALL CalcH("3-methyl-3-hexanol",                          "597-96-6", 7.1349E-01, 1.2843E-05)
    CALL CalcH("4-methyl-3-hexanol",                          "615-29-2", 1.0444E+00, 1.8799E-05)
    CALL CalcH("5-methyl-3-hexanol",                          "623-55-2", 1.0191E+00, 1.8344E-05)
    CALL CalcH("2-ethyl-1-pentanol",                        "27522-11-8", 1.6193E+00, 2.9148E-05)
    CALL CalcH("3-ethyl-1-pentanol",                        "66225-51-2", 1.6193E+00, 2.9148E-05)
    CALL CalcH("2,2-dimethyl-1-pentanol",                    "2370-12-9", 1.6707E+00, 3.0073E-05)
    CALL CalcH("2,3-dimethyl-1-pentanol",                   "10143-23-4", 1.5341E+00, 2.7614E-05)
    CALL CalcH("2,4-dimethyl-1-pentanol",                    "6305-71-1", 1.6396E+00, 2.9513E-05)
    CALL CalcH("3,3-dimethyl-1-pentanol",                   "19264-94-9", 1.5760E+00, 2.8368E-05)
    CALL CalcH("3,4-dimethyl-1-pentanol",                    "6570-87-2", 1.5760E+00, 2.8368E-05)
    CALL CalcH("4,4-dimethyl-1-pentanol",                    "3121-79-7", 1.4204E+00, 2.5567E-05)
    CALL CalcH("3-ethyl-2-pentanol",                          "609-27-8", 1.1249E+00, 2.0249E-05)
    CALL CalcH("2,3-dimethyl-2-pentanol",                    "4911-70-0", 6.3815E-01, 1.1487E-05)
    CALL CalcH("2,4-dimethyl-2-pentanol",                     "625-06-9", 9.5128E-01, 1.7123E-05)
    CALL CalcH("3,3-dimethyl-2-pentanol",                   "19781-24-9", 9.9451E-01, 1.7901E-05)
    CALL CalcH("3,4-dimethyl-2-pentanol",                   "64502-86-9", 1.1534E+00, 2.0762E-05)
    CALL CalcH("4,4-dimethyl-2-pentanol",                    "6144-93-0", 8.0189E-01, 1.4434E-05)
    CALL CalcH("3-ethyl-3-pentanol",                          "597-49-9", 5.0701E-01, 9.1262E-06)
    CALL CalcH("2,2-dimethyl-3-pentanol",                    "3970-62-5", 1.3495E+00, 2.4290E-05)
    CALL CalcH("2,3-dimethyl-3-pentanol",                     "595-41-5", 5.9113E-01, 1.0640E-05)
    CALL CalcH("2,4-dimethyl-3-pentanol",                     "600-36-2", 1.4238E+00, 2.5628E-05)
    CALL CalcH("2-ethyl-2-methyl-1-butanol",                "18371-13-6", 1.2766E+00, 2.2980E-05)
    CALL CalcH("2-ethyl-3-methyl-1-butanol",                "32444-34-1", 1.4544E+00, 2.6179E-05)
    CALL CalcH("2,2,3-trimethyl-1-butanol",                 "55505-23-2", 1.2766E+00, 2.2980E-05)
    CALL CalcH("2,3,3-trimethyl-1-butanol",                 "36794-64-6", 1.3798E+00, 2.4837E-05)
    CALL CalcH("2,3,3-trimethyl-2-butanol",                   "594-83-2", 1.9887E+00, 3.5796E-05)
    ! C8:
    CALL CalcH("1-octanol",                                   "111-87-5", 1.3907E+00, 2.5032E-05)
    CALL CalcH("2-octanol",                                   "123-96-6", 1.7931E+00, 3.2276E-05)
    CALL CalcH("3-octanol",                                   "589-98-0", 1.7879E+00, 3.2182E-05)
    CALL CalcH("4-octanol",                                   "589-62-8", 1.8964E+00, 3.4135E-05)
    CALL CalcH("2-methyl-1-heptanol",                       "60435-70-3", 1.5916E+00, 2.8649E-05)
    CALL CalcH("3-methyl-1-heptanol",                        "1070-32-2", 2.5733E+00, 4.6320E-05)
    CALL CalcH("4-methyl-1-heptanol",                         "817-91-4", 2.3438E+00, 4.2189E-05)
    CALL CalcH("5-methyl-1-heptanol",                        "7212-53-5", 2.6261E+00, 4.7270E-05)
    CALL CalcH("6-methyl-1-heptanol",                        "1653-40-3", 2.7264E+00, 4.9076E-05)
    CALL CalcH("2-methyl-2-heptanol",                         "625-25-2", 1.0651E+00, 1.9173E-05)
    CALL CalcH("3-methyl-2-heptanol",                       "31367-46-1", 1.3840E+00, 2.4913E-05)
    CALL CalcH("4-methyl-2-heptanol",                       "56298-90-9", 1.6271E+00, 2.9288E-05)
    CALL CalcH("5-methyl-2-heptanol",                       "54630-50-1", 1.6418E+00, 2.9553E-05)
    CALL CalcH("6-methyl-2-heptanol",                        "4730-22-7", 1.6418E+00, 2.9553E-05)
    CALL CalcH("2-methyl-3-heptanol",                       "18720-62-2", 1.4455E+00, 2.6019E-05)
    CALL CalcH("3-methyl-3-heptanol",                        "5582-82-1", 1.8855E+00, 3.3939E-05)
    CALL CalcH("4-methyl-3-heptanol",                       "14979-39-6", 1.0286E+00, 1.8515E-05)
    CALL CalcH("5-methyl-3-heptanol",                       "18720-65-5", 9.8055E-01, 1.7650E-05)
    CALL CalcH("2-methyl-4-heptanol",                       "21570-35-4", 1.3840E+00, 2.4913E-05)
    CALL CalcH("3-methyl-4-heptanol",                        "1838-73-9", 1.3296E+00, 2.3933E-05)
    CALL CalcH("4-methyl-4-heptanol",                         "598-01-6", 1.2015E+00, 2.1627E-05)
    CALL CalcH("2-ethyl-1-hexanol",                           "104-76-7", 1.2790E+00, 2.3023E-05)
    CALL CalcH("2,2-dimethyl-1-hexanol",                     "2370-13-0", 1.1046E+00, 1.9882E-05)
    CALL CalcH("2,4-dimethyl-1-hexanol",                     "3965-59-1", 1.1899E+00, 2.1418E-05)
    CALL CalcH("2,5-dimethyl-1-hexanol",                     "6886-16-4", 1.3446E+00, 2.4202E-05)
    CALL CalcH("3,5-dimethyl-1-hexanol",                    "13501-73-0", 1.5257E+00, 2.7462E-05)
    ! assuming 9.6974E-01 instead of 9.69741E-0 to make data consistent with othe column:
    CALL CalcH("3-ethyl-2-hexanol",                         "24448-19-9", 9.6974E-01, 1.7455E-05)
    CALL CalcH("2,3-dimethyl-2-hexanol",                    "19550-03-9", 7.7989E-01, 1.4038E-05)
    CALL CalcH("2,4-dimethyl-2-hexanol",                    "42328-76-7", 6.1134E-01, 1.1004E-05)
    CALL CalcH("2,5-dimethyl-2-hexanol",                     "3730-60-7", 6.3975E-01, 1.1516E-05)
    CALL CalcH("3,4-dimethyl-2-hexanol",                    "19550-05-1", 1.0571E+00, 1.9028E-05)
    CALL CalcH("3,5-dimethyl-2-hexanol",                    "66576-27-0", 7.7781E-01, 1.4001E-05)
    CALL CalcH("5,5-dimethyl-2-hexanol",                    "31841-77-7", 9.1656E-01, 1.6498E-05)
    CALL CalcH("3-ethyl-3-hexanol",                           "597-76-2", 7.5735E-01, 1.3632E-05)
    CALL CalcH("4-ethyl-3-hexanol",                         "19780-44-0", 8.6704E-01, 1.5607E-05)
    CALL CalcH("2,2-dimethyl-3-hexanol",                     "4209-90-9", 7.0177E-01, 1.2632E-05)
    CALL CalcH("2,3-dimethyl-3-hexanol",                     "4166-46-5", 7.4148E-01, 1.3347E-05)
    CALL CalcH("2,4-dimethyl-3-hexanol",                    "13432-25-2", 7.7781E-01, 1.4001E-05)
    CALL CalcH("2,5-dimethyl-3-hexanol",                    "19550-07-3", 7.5735E-01, 1.3632E-05)
    CALL CalcH("3,4-dimethyl-3-hexanol",                    "19550-08-4", 6.3170E-01, 1.1371E-05)
    CALL CalcH("3,5-dimethyl-3-hexanol",                     "4209-91-0", 6.3170E-01, 1.1371E-05)
    CALL CalcH("4,4-dimethyl-3-hexanol",                    "19550-09-5", 7.5735E-01, 1.3632E-05)
    CALL CalcH("5,5-dimethyl-3-hexanol",                    "66576-31-6", 6.4794E-01, 1.1663E-05)
    CALL CalcH("2-propyl-1-pentanol",                       "58175-57-8", 1.3446E+00, 2.4202E-05)
    CALL CalcH("2-ethyl-2-methyl-1-pentanol",                "5970-63-8", 1.2681E+00, 2.2826E-05)
    CALL CalcH("2-ethyl-4-methyl-1-pentanol",                 "106-67-2", 1.2451E+00, 2.2412E-05)
    CALL CalcH("2,2,3-trimethyl-1-pentanol",                "57409-53-7", 1.1722E+00, 2.1099E-05)
    CALL CalcH("2,2,4-trimethyl-1-pentanol",                  "123-44-4", 9.7805E-01, 1.7605E-05)
    CALL CalcH("2,3,4-trimethyl-1-pentanol",                 "6570-88-3", 1.5257E+00, 2.7462E-05)
    CALL CalcH("2,4,4-trimethyl-1-pentanol",                "16325-63-6", 1.0571E+00, 1.9028E-05)
    CALL CalcH("3-ethyl-2-methyl-2-pentanol",               "19780-63-3", 7.3370E-01, 1.3207E-05)
    CALL CalcH("3-ethyl-4-methyl-2-pentanol",               "66576-23-6", 8.6704E-01, 1.5607E-05)
    CALL CalcH("2,3,3-trimethyl-2-pentanol",                "23171-85-9", 7.7781E-01, 1.4001E-05)
    CALL CalcH("2,3,4-trimethyl-2-pentanol",                "66576-26-9", 7.3757E-01, 1.3276E-05)
    CALL CalcH("2,4,4-trimethyl-2-pentanol",                  "690-37-9", 5.4961E-01, 9.8931E-06)
    CALL CalcH("3,3,4-trimethyl-2-pentanol",                "19411-41-7", 8.9136E-01, 1.6045E-05)
    CALL CalcH("3,4,4-trimethyl-2-pentanol",                "10575-56-1", 7.3757E-01, 1.3276E-05)
    CALL CalcH("3-ethyl-2-methyl-3-pentanol",                 "597-05-7", 7.7781E-01, 1.4001E-05)
    CALL CalcH("2,2,3-trimethyl-3-pentanol",                 "7294-05-5", 5.0516E-01, 9.0928E-06)
    CALL CalcH("2,2,4-trimethyl-3-pentanol",                 "5162-48-1", 6.1596E-01, 1.1087E-05)
    CALL CalcH("2,3,4-trimethyl-3-pentanol",                 "3054-92-0", 7.1844E-01, 1.2932E-05)
    CALL CalcH("3-methyl-2-(1-methylethyl)-1-butanol",      "18593-92-5", 1.1210E+00, 2.0178E-05)
    ! C9:
    CALL CalcH("1-nonanol",                                   "143-08-8", 1.7190E+00, 3.0943E-05)
    CALL CalcH("2-nonanol",                                   "628-99-9", 1.0152E+00, 1.8274E-05)
    CALL CalcH("3-nonanol",                                   "624-51-1", 1.8290E+00, 3.2921E-05)
    CALL CalcH("4-nonanol",                                  "5932-79-6", 1.7348E+00, 3.1226E-05)
    CALL CalcH("5-nonanol",                                   "623-93-8", 1.8521E+00, 3.3338E-05)
    CALL CalcH("6-methyl-1-octanol",                        "38514-05-5", 2.6634E+00, 4.7941E-05)
    CALL CalcH("7-methyl-1-octanol",                         "2430-22-0", 2.6634E+00, 4.7941E-05)
    CALL CalcH("2-methyl-2-octanol",                          "628-44-4", 1.1293E+00, 2.0327E-05)
    CALL CalcH("2-methyl-3-octanol",                        "26533-34-6", 1.3306E+00, 2.3951E-05)
    CALL CalcH("3-methyl-3-octanol",                         "5340-36-3", 1.5373E+00, 2.7671E-05)
    CALL CalcH("2-methyl-4-octanol",                        "40575-41-5", 1.3306E+00, 2.3951E-05)
    CALL CalcH("3-methyl-4-octanol",                        "26533-35-7", 1.1915E+00, 2.1447E-05)
    CALL CalcH("4-methyl-4-octanol",                        "23418-37-3", 1.2243E+00, 2.2038E-05)
    CALL CalcH("3-ethyl-1-heptanol",                         "3525-25-5", 2.7594E+00, 4.9669E-05)
    CALL CalcH("2,2-dimethyl-1-heptanol",                   "14250-79-4", 1.6824E+00, 3.0283E-05)
    CALL CalcH("2,6-dimethyl-2-heptanol",                   "13254-34-7", 9.4351E-01, 1.6983E-05)
    CALL CalcH("4,6-dimethyl-2-heptanol",                   "51079-52-8", 1.7894E+00, 3.2209E-05)
    CALL CalcH("5,6-dimethyl-2-heptanol",                   "58795-24-7", 1.6824E+00, 3.0283E-05)
    CALL CalcH("3-ethyl-3-heptanol",                        "19780-41-7", 1.2654E+00, 2.2778E-05)
    CALL CalcH("2,3-dimethyl-3-heptanol",                   "19549-71-4", 1.0176E+00, 1.8316E-05)
    CALL CalcH("2,6-dimethyl-3-heptanol",                   "19549-73-6", 1.0440E+00, 1.8792E-05)
    CALL CalcH("4-ethyl-4-heptanol",                          "597-90-0", 1.1598E+00, 2.0877E-05)
    CALL CalcH("2,2-dimethyl-4-heptanol",                   "66793-99-5", 1.0176E+00, 1.8316E-05)
    CALL CalcH("2,4-dimethyl-4-heptanol",                   "19549-77-0", 9.5294E-01, 1.7153E-05)
    CALL CalcH("2,6-dimethyl-4-heptanol",                     "108-82-7", 3.1883E+00, 5.7389E-05)
    CALL CalcH("3,3-dimethyl-4-heptanol",                   "19549-78-1", 2.4813E+00, 4.4664E-05)
    CALL CalcH("2,5-dimethyl-4-heptanol",                      "_CAS-35", 3.4697E+00, 6.2454E-05)
    CALL CalcH("3-ethyl-2-methyl-1-hexanol",                "66794-01-2", 4.0838E+00, 7.3509E-05)
    CALL CalcH("2-ethyl-4-methyl-1-hexanol",                "66794-06-7", 4.5190E+00, 8.1342E-05)
    CALL CalcH("3,4,4-trimethyl-1-hexanol",                 "66793-73-5", 3.9506E+00, 7.1111E-05)
    CALL CalcH("3,5,5-trimethyl-1-hexanol",                  "3452-97-9", 4.2228E+00, 7.6010E-05)
    CALL CalcH("4,5,5-trimethyl-1-hexanol",                 "66793-75-7", 5.7854E+00, 1.0414E-04)
    CALL CalcH("3-ethyl-2-methyl-2-hexanol",                "66794-02-3", 2.6314E+00, 4.7366E-05)
    CALL CalcH("3-ethyl-2-methyl-3-hexanol",                "66794-03-4", 3.1667E+00, 5.7001E-05)
    CALL CalcH("3-ethyl-5-methyl-3-hexanol",                  "597-77-3", 2.2123E+00, 3.9821E-05)
    CALL CalcH("2,2,3-trimethyl-3-hexanol",                  "5340-41-0", 2.2824E+00, 4.1083E-05)
    CALL CalcH("2,2,4-trimethyl-3-hexanol",                 "66793-89-3", 2.0344E+00, 3.6620E-05)
    CALL CalcH("2,2,5-trimethyl-3-hexanol",                  "3970-60-3", 1.5988E+00, 2.8779E-05)
    CALL CalcH("2,4,4-trimethyl-3-hexanol",                 "66793-92-8", 2.1509E+00, 3.8716E-05)
    CALL CalcH("3,4,4-trimethyl-3-hexanol",                 "66793-74-6", 1.8743E+00, 3.3737E-05)
    CALL CalcH("4-methyl-2-propyl-1-pentanol",              "54004-41-0", 4.0838E+00, 7.3509E-05)
    CALL CalcH("4-methyl-2-(1-methylethyl)-1-pentanol",     "55505-24-3", 3.4697E+00, 6.2454E-05)
    CALL CalcH("2-ethyl-2,4-dimethyl-1-pentanol",           "66793-98-4", 3.5826E+00, 6.4487E-05)
    CALL CalcH("3,3,4,4-tetramethyl-2-pentanol",            "66793-88-2", 2.7933E+00, 5.0279E-05)
    ! The values for 2-methyl-3-isopropyl-3-pentanol (2.6314E+00,
    ! 4.7366E-05) are not used here because this species would be the same
    ! as 3-ethyl-2,4-dimethyl-3-pentanol, why appears two lines later.
    CALL CalcH("3-ethyl-2,2-dimethyl-3-pentanol",           "66793-96-2", 2.3419E+00, 4.2154E-05)
    CALL CalcH("3-ethyl-2,4-dimethyl-3-pentanol",            "3970-59-0", 2.6237E+00, 4.7226E-05)
    CALL CalcH("2,2,3,4-tetramethyl-3-pentanol",            "29772-39-2", 2.3554E+00, 4.2397E-05)
    CALL CalcH("2,2,4,4-tetramethylpentan-3-ol",            "14609-79-1", 1.9258E+00, 3.4665E-05)
    ! C10:
    CALL CalcH("1-decanol",                                   "112-30-1", 2.6573E+00, 4.7831E-05)
    CALL CalcH("2-decanol",                                  "1120-06-5", 1.0101E+00, 1.8182E-05)
    CALL CalcH("4-decanol",                                  "2051-31-2", 1.0210E+00, 1.8378E-05)
    CALL CalcH("5-decanol",                                  "5205-34-5", 7.5156E-01, 1.3528E-05)
    CALL CalcH("2-methyl-1-nonanol",                        "40589-14-8", 1.6174E+00, 2.9113E-05)
    CALL CalcH("2-methyl-3-nonanol",                        "26533-33-5", 9.6464E-01, 1.7364E-05)
    CALL CalcH("2,2-dimethyl-1-octanol",                     "2370-14-1", 9.5119E-01, 1.7121E-05)
    CALL CalcH("3,7-dimethyl-1-octanol",                      "106-21-8", 1.0977E+00, 1.9759E-05)
    CALL CalcH("3-ethyl-3-octanol",                          "2051-32-3", 7.0504E-01, 1.2691E-05)
    CALL CalcH("2,3-dimethyl-3-octanol",                    "19781-10-3", 5.2476E-01, 9.4457E-06)
    CALL CalcH("2,7-dimethyl-3-octanol",                    "66719-55-9", 6.0477E-01, 1.0886E-05)
    CALL CalcH("3,6-dimethyl-3-octanol",                      "151-19-9", 5.7017E-01, 1.0263E-05)
    CALL CalcH("3,7-dimethyl-3-octanol",                       "78-69-3", 6.4236E-01, 1.1562E-05)
    CALL CalcH("2,2-dimethyl-4-octanol",                    "66719-52-6", 5.3829E-01, 9.6893E-06)
    CALL CalcH("4,7-dimethyl-4-octanol",                    "19781-13-6", 5.7017E-01, 1.0263E-05)
    CALL CalcH("2-propyl-1-heptanol",                       "10042-59-8", 1.3726E+00, 2.4706E-05)
    CALL CalcH("3-(1-methylethyl)-1-heptanol",              "38514-15-7", 1.2369E+00, 2.2264E-05)
    CALL CalcH("2,5,6-trimethyl-2-heptanol",                "66256-48-2", 5.8712E-01, 1.0568E-05)
    CALL CalcH("3-ethyl-2-methyl-3-heptanol",               "66719-37-7", 5.8712E-01, 1.0568E-05)
    CALL CalcH("2,2,3-trimethyl-3-heptanol",                "29772-40-5", 4.8169E-01, 8.6704E-06)
    CALL CalcH("3,5,5-trimethyl-3-heptanol",                "66256-50-6", 6.3459E-01, 1.1423E-05)
    CALL CalcH("4-propyl-4-heptanol",                        "2198-72-3", 6.0477E-01, 1.0886E-05)
    CALL CalcH("4-(1-methylethyl)-4-heptanol",              "51200-82-9", 5.3829E-01, 9.6893E-06)
    CALL CalcH("2,2,4-trimethyl-4-heptanol",                "57233-31-5", 4.2225E-01, 7.6006E-06)
    CALL CalcH("2,4,6-trimethyl-4-heptanol",                "60836-07-9", 4.3325E-01, 7.7986E-06)
    CALL CalcH("2-butyl-1-hexanol",                          "2768-15-2", 1.2275E+00, 2.2096E-05)
    CALL CalcH("4-methyl-2-propyl-1-hexanol",               "66256-62-0", 9.5119E-01, 1.7121E-05)
    CALL CalcH("4-methyl-2-(1-methylethyl)-1-hexanol",      "66719-41-3", 7.5156E-01, 1.3528E-05)
    CALL CalcH("5-methyl-2-(1-methylethyl)-1-hexanol",       "2051-33-4", 1.1389E+00, 2.0500E-05)
    CALL CalcH("2,3,4,4-tetramethyl-2-hexanol",             "66256-66-4", 5.3829E-01, 9.6893E-06)
    CALL CalcH("2-methyl-3-(1-methylethyl)-3-hexanol",      "51200-81-8", 5.7017E-01, 1.0263E-05)
    CALL CalcH("4-ethyl-2,2-dimethyl-3-hexanol",            "66719-47-9", 4.9501E-01, 8.9102E-06)
    CALL CalcH("2,2,3,4-tetramethyl-3-hexanol",             "66256-63-1", 5.7017E-01, 1.0263E-05)
    CALL CalcH("2,2,4,4-tetramethyl-3-hexanol",             "66256-65-3", 5.3829E-01, 9.6893E-06)
    CALL CalcH("2,2,5,5-tetramethyl-3-hexanol",             "55073-86-4", 3.2441E-01, 5.8394E-06)
    CALL CalcH("2,3,4,4-tetramethyl-3-hexanol",             "66256-67-5", 7.5156E-01, 1.3528E-05)
    CALL CalcH("3,4,4,5-tetramethyl-3-hexanol",             "66256-39-1", 7.7640E-01, 1.3975E-05)
    CALL CalcH("3,4,5,5-tetramethyl-3-hexanol",             "66256-40-4", 6.2317E-01, 1.1217E-05)
    CALL CalcH("4-methyl-2-(2-methylpropyl)-1-pentanol",    "22417-45-4", 8.2948E-01, 1.4931E-05)
    CALL CalcH("2,4-dimethyl-3-propyl-3-pentanol",         "500001-19-4", 4.5654E-01, 8.2178E-06)
    CALL CalcH("2,4-dimethyl-3-(1-methylethyl)-3-pentanol", "51200-83-0", 6.1387E-01, 1.1050E-05)
    CALL CalcH("3-ethyl-2,2,4-trimethyl-3-pentanol",        "66256-41-5", 5.5391E-01, 9.9704E-06)
    CALL CalcH("2,2,3,4,4-pentamethyl-3-pentanol",           "5857-69-2", 6.1204E-01, 1.1017E-05)
    ! >C10:
    CALL CalcH("1-undecanol",                                 "112-42-5", 2.5098E+00, 4.5176E-05)
    CALL CalcH("1-dodecanol",                                 "112-53-8", 2.8812E+00, 5.1861E-05)
    CALL CalcH("1-tridecanol",                                "112-70-9", 4.6644E+00, 8.3959E-05)
    CALL CalcH("1-tetradecanol",                              "112-72-1", 5.7773E+00, 1.0399E-04)
    CALL CalcH("1-pentadecanol",                              "629-76-5", 2.1777E+00, 3.9199E-05)
    CALL CalcH("1-hexadecanol",                               "124-29-8", 5.4271E+00, 9.7688E-05)
    CALL CalcH("1-heptadecanol",                             "1454-85-9", 1.2147E+01, 2.1865E-04)
    CALL CalcH("1-octadecanol",                               "112-92-5", 1.7371E+02, 3.1267E-03)
    CALL CalcH("1-nonadecanol",                              "1454-84-8", 5.5330E+00, 9.9594E-05)
    CALL CalcH("1-eicosanol",                                 "629-96-9", 3.0844E+01, 5.5520E-04)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpx, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpx, KHpc
      REAL :: percent_diff

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      ! compare to data in second column:
      percent_diff = 100.*ABS(1.- (1./(atm*KHpc)) / (KHpx_TIMES_HcpSI/KHpx) )
      ! a small difference of about 0.38 % is probably due to a
      ! different c(H2O) used by ref1115:
      IF (percent_diff>0.5) THEN
        PRINT *, "WARNING for ref1115: difference = ", percent_diff, &
          " %; CASRN = ", TRIM(casrn)
      ENDIF
      IF (casrn_=="60435-70-3") THEN
        CALL MakeNote("1115_60435-70-3", &
          "It is assumed here that entry number 72 in Table 1 of "// &
          TRIM(citet())//" refers to 2-methyl-1-heptanol, not "// &
          "2-methyl-2-heptanol.")
      ENDIF
      CALL Output(KHpx_TIMES_HcpSI/KHpx)

    END SUBROUTINE CalcH

  END SUBROUTINE ref1115

  !---------------------------------------------------------------------------

  ! ref1116 EtH and PrH seem to be recalculations of data from from 2558

  !---------------------------------------------------------------------------

  SUBROUTINE ref1124 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1124"
    type = "M"

    chem = "dimethylsulfoxide" ; casrn = "67-68-5" ! CH3SOCH3 DMSO
    CALL Output(1E6*Hcp_TO_HcpSI, limit=">")

  END SUBROUTINE ref1124

  !---------------------------------------------------------------------------

  SUBROUTINE ref1125 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1125"
    type = "M"

    chem = "CF3CFO" ; casrn = "354-34-7"
    CALL MakeNoteOtherTemp("278")
    CALL Output(0.96*Hcp_TO_HcpSI)

    chem = "CF3CClO" ; casrn = "354-32-5"
    CALL MakeNoteOtherTemp("278")
    CALL Output(0.27*Hcp_TO_HcpSI)

    chem = "CCl2O" ; casrn = "75-44-5"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 278., 298. /)
    Harray = (/ 0.15, 0.06 /) * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "CF2O" ; casrn = "353-50-4"
    CALL MakeNoteOtherTemp("278")
    CALL Output(1.0*Hcp_TO_HcpSI)

    chem = "CCl3CClO" ; casrn = "76-02-8"
    CALL MakeNoteOtherTemp("278")
    CALL Output(2.0*Hcp_TO_HcpSI)

  END SUBROUTINE ref1125

  !---------------------------------------------------------------------------

  SUBROUTINE ref1126 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1126"
    type = "M"
    chem = "trichloroacetylchloride" ; casrn = "76-02-8" ! CCl3COCl
    CALL Output(2.*Hcp_TO_HcpSI)

  END SUBROUTINE ref1126

  !---------------------------------------------------------------------------

  SUBROUTINE ref1127 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1127"
    type = "M"

    chem = "trifluoroacetylfluoride" ; casrn = "354-34-7" ! CF3COF
    CALL MakeNoteOtherTemp("284")
    CALL Output(3.*Hcp_TO_HcpSI)

    chem = "trifluoroacetylchloride" ; casrn = "354-32-5" ! CF3COCl
    CALL MakeNoteOtherTemp("284")
    CALL Output(2.*Hcp_TO_HcpSI)

  END SUBROUTINE ref1127

  !---------------------------------------------------------------------------

  SUBROUTINE ref1137
    IMPLICIT NONE
    REAL :: A, B, C, D

    ref = "1137"

    chem = "carbon dioxide" ; casrn = "124-38-9" ! CO2
    type = "R"
    A = 155.1699
    B = -8477.711
    C = -21.95743
    D = 0.005780748
    Hominus = rhoH2O/(EXP(A+B/T0+C*LOG(T0)+D*T0)*atm)
    mindHR = -(B-C*T0-D*T0*T0) ! analytical derivative
    CALL Output(Hominus, mindHR)

    chem = "hydrogen chloride" ; casrn = "7647-01-0"
    type = "R"
    CALL Output(rhoH2O/(0.651E-3*atm))

  END SUBROUTINE ref1137

  !---------------------------------------------------------------------------

  SUBROUTINE ref1138 ! Hcc [1]
    IMPLICIT NONE

    ref = "1138"
    type = "M"

    chem = "dinitrogen trioxide" ; casrn = "10544-73-7" ! N2O3
    CALL Output(609. * Hcc_TO_HcpSI_atT0)

    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 25., 15. /) + CtoK

    chem = "nitrogen monoxide" ; casrn = "10102-43-9" ! NO
    Harray = Hcc_TO_HcpSI( (/ 0.0473, 0.0542 /), temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "dinitrogen tetroxide" ; casrn = "10544-72-6" ! N2O4
    Harray = Hcc_TO_HcpSI( (/ 40.4, 58.5 /), temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "nitrous acid" ; casrn = "7782-77-6" ! HNO2
    Harray = Hcc_TO_HcpSI( (/ 907., 2500. /), temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1138

  !---------------------------------------------------------------------------

  SUBROUTINE ref1139
    IMPLICIT NONE

    ref = "1139"
    type = "M"

    CALL CalcH("1,3-dichlorobenzene",                 "541-73-1", 18.)
    CALL CalcH("1,4-dichlorobenzene",                 "106-46-7", 15.)
    CALL CalcH("1,2-dichlorobenzene",                  "95-50-1", 12.)
    CALL CalcH("1,3,5-trichlorobenzene",              "108-70-3", 19.)
    CALL CalcH("1,2,4-trichlorobenzene",              "120-82-1", 12.)
    CALL CalcH("1,2,3-trichlorobenzene",               "87-61-6", 8.9)
    CALL CalcH("1,2,4,5-tetrachlorobenzene",           "95-94-3", 10.)
    CALL CalcH("1,2,3,4-tetrachlorobenzene",          "634-66-2", 6.9)
    CALL CalcH("pentachlorobenzene",                  "608-93-5", 7.1)
    CALL CalcH("hexachlorobenzene",                   "118-74-1", 4.8)
    CALL CalcH("1-methyl-2,4,5-trichlorobenzene",    "6639-30-1", 15.)
    CALL CalcH("1-methyl-2,3,6-trichlorobenzene",    "2077-46-5", 15.)
    CALL CalcH("pentachloromethylbenzene",            "877-11-2", 7.7)
    CALL CalcH("hexachlorobutadiene",                  "87-68-3", 43.)
    CALL CalcH("octachlorostyrene",                 "29082-74-4", 1.3)
    CALL CalcH("2,2',5-trichlorobiphenyl",          "37680-65-2", 2.0)
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl",     "35693-99-3", 1.2)
    CALL CalcH("2,2',3,3'-tetrachlorobiphenyl",     "38444-93-8", 1.2)
    CALL CalcH("2,2',4,5,5'-pentachlorobiphenyl",   "37680-73-2", 0.7)
    CALL CalcH("2,2',4,4',5,5'-hexachlorobiphenyl", "35065-27-1", 0.6)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, KHpc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: KHpc

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = 1E4 / (KHpc*atm)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1139

  !---------------------------------------------------------------------------

  SUBROUTINE ref1140 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1140"
    type = "M"

    ! mostly from Tab. 3:
    CALL CalcH("PCB-4",   "13029-08-8", 3.37E-4,  3)
    CALL CalcH("PCB-7",   "33284-50-3", 3.48E-4,  4)
    CALL CalcH("PCB-9",   "34883-39-1", 3.88E-4,  3)
    CALL CalcH("PCB-11",   "2050-67-1", 2.33E-4,  3)
    CALL CalcH("PCB-12",   "2974-92-7", 2.05E-4,  3)
    CALL CalcH("PCB-15",   "2050-68-2", 1.99E-4,  3)
    CALL CalcH("PCB-18",  "37680-65-2", 3.80E-4,  4)
    CALL CalcH("PCB-26",  "38444-81-4", 3.25E-4,  3)
    CALL CalcH("PCB-28",   "7012-37-5", 3.16E-4,  4)
    CALL CalcH("PCB-30",  "35693-92-6", 6.49E-4,  3)
    CALL CalcH("PCB-40",  "38444-93-8", 2.02E-4,  3)
    CALL CalcH("PCB-52",  "35693-99-3", 3.42E-4,  3)
    CALL CalcH("PCB-53",  "41464-41-9", 4.06E-4,  3)
    CALL CalcH("PCB-54",  "15968-05-5", 5.50E-4,  3)
    CALL CalcH("PCB-77",  "32598-13-3", 0.94E-4,  3)
    CALL CalcH("PCB-101", "37680-73-2", 2.51E-4,  3)
    CALL CalcH("PCB-104", "56558-16-8", 8.97E-4,  3)
    CALL CalcH("PCB-128", "38380-07-3", 0.302E-4, 3)
    CALL CalcH("PCB-153", "35065-27-1", 1.32E-4,  3)
    CALL CalcH("PCB-155", "33979-03-2", 7.55E-4,  3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHpc, table)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: KHpc
      INTEGER,          INTENT(IN) :: table

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm*KHpc)
      IF (table==3) THEN
        CALL MakeNote(TRIM(ref), &
          "The same data were also published in \citet{643}.")
      ENDIF
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1140

  !---------------------------------------------------------------------------

  SUBROUTINE ref1141 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1141"
    type = "Q"

    CALL CalcH("PCB-5",   "16605-91-7", 2.82E-4)
    CALL CalcH("PCB-6",   "25569-80-6", 3.89E-4)
    CALL CalcH("PCB-8",   "34883-43-7", 3.09E-4)
    CALL CalcH("PCB-10",  "33146-45-1", 4.70E-4)
    CALL CalcH("PCB-13",   "2974-90-5", 3.15E-4)
    CALL CalcH("PCB-14",  "34883-41-5", 4.89E-4)
    CALL CalcH("PCB-16",  "38444-78-9", 2.77E-4)
    CALL CalcH("PCB-17",  "37680-66-3", 4.01E-4)
    CALL CalcH("PCB-19",  "38444-73-4", 4.45E-4)
    CALL CalcH("PCB-20",  "38444-84-7", 3.03E-4)
    CALL CalcH("PCB-21",  "55702-46-0", 2.11E-4)
    CALL CalcH("PCB-22",  "38444-85-8", 2.23E-4)
    CALL CalcH("PCB-23",  "55720-44-0", 3.51E-4)
    CALL CalcH("PCB-24",  "55702-45-9", 3.39E-4)
    CALL CalcH("PCB-25",  "55712-37-3", 4.27E-4)
    CALL CalcH("PCB-27",  "38444-76-7", 4.93E-4)
    CALL CalcH("PCB-29",  "15862-07-4", 2.67E-4)
    CALL CalcH("PCB-31",  "16606-02-3", 2.81E-4)
    CALL CalcH("PCB-32",  "38444-77-8", 4.12E-4)
    CALL CalcH("PCB-33",  "38444-86-9", 2.17E-4)
    CALL CalcH("PCB-34",  "37680-68-5", 5.05E-4)
    CALL CalcH("PCB-35",  "37680-69-6", 2.22E-4)
    CALL CalcH("PCB-36",  "38444-87-0", 5.08E-4)
    CALL CalcH("PCB-37",  "38444-90-5", 1.44E-4)
    CALL CalcH("PCB-38",  "53555-66-1", 2.07E-4)
    CALL CalcH("PCB-39",  "38444-88-1", 4.30E-4)
    CALL CalcH("PCB-41",  "52663-59-9", 2.06E-4)
    CALL CalcH("PCB-42",  "36559-22-5", 3.16E-4)
    CALL CalcH("PCB-43",  "70362-46-8", 3.48E-4)
    CALL CalcH("PCB-44",  "41464-39-5", 2.51E-4)
    CALL CalcH("PCB-45",  "70362-45-7", 3.15E-4)
    CALL CalcH("PCB-46",  "41464-47-5", 3.62E-4)
    CALL CalcH("PCB-47",   "2437-79-8", 4.39E-4)
    CALL CalcH("PCB-48",  "70362-47-9", 2.63E-4)
    CALL CalcH("PCB-49",  "41464-40-8", 3.74E-4)
    CALL CalcH("PCB-50",  "62796-65-0", 5.79E-4)
    CALL CalcH("PCB-51",  "68194-04-7", 4.84E-4)
    CALL CalcH("PCB-55",  "74338-24-2", 2.32E-4)
    CALL CalcH("PCB-56",  "41464-43-1", 1.31E-4)
    CALL CalcH("PCB-57",  "70424-67-8", 3.71E-4)
    CALL CalcH("PCB-58",  "41464-49-7", 4.19E-4)
    CALL CalcH("PCB-59",  "74472-33-6", 3.61E-4)
    CALL CalcH("PCB-60",  "33025-41-1", 1.52E-4)
    CALL CalcH("PCB-61",  "33284-53-6", 1.73E-4)
    CALL CalcH("PCB-62",  "54230-22-7", 3.66E-4)
    CALL CalcH("PCB-63",  "74472-34-7", 2.92E-4)
    CALL CalcH("PCB-64",  "52663-58-8", 2.80E-4)
    CALL CalcH("PCB-65",  "33284-54-7", 3.06E-4)
    CALL CalcH("PCB-66",  "32598-10-0", 2.55E-4)
    CALL CalcH("PCB-67",  "73575-53-8", 2.88E-4)
    CALL CalcH("PCB-68",  "73575-52-7", 5.43E-4)
    CALL CalcH("PCB-69",  "60233-24-1", 6.27E-4)
    CALL CalcH("PCB-70",  "32598-11-1", 1.89E-4)
    CALL CalcH("PCB-71",  "41464-46-4", 3.21E-4)
    CALL CalcH("PCB-72",  "41464-42-0", 4.77E-4)
    CALL CalcH("PCB-73",  "74338-23-1", 6.10E-4)
    CALL CalcH("PCB-74",  "32690-93-0", 2.09E-4)
    CALL CalcH("PCB-75",  "32598-12-2", 5.46E-4)
    CALL CalcH("PCB-76",  "70362-48-0", 2.23E-4)
    CALL CalcH("PCB-78",  "70362-49-1", 2.26E-4)
    CALL CalcH("PCB-79",  "41464-48-6", 3.36E-4)
    CALL CalcH("PCB-80",  "33284-52-5", 6.22E-4)
    CALL CalcH("PCB-81",  "70362-50-4", 1.48E-4)
    CALL CalcH("PCB-82",  "52663-62-4", 1.22E-4)
    CALL CalcH("PCB-83",  "60145-20-2", 2.63E-4)
    CALL CalcH("PCB-84",  "52663-60-2", 2.32E-4)
    CALL CalcH("PCB-85",  "65510-45-4", 2.45E-4)
    CALL CalcH("PCB-86",  "55312-69-1", 1.70E-4)
    CALL CalcH("PCB-87",  "38380-02-8", 1.80E-4)
    CALL CalcH("PCB-88",  "55215-17-3", 3.42E-4)
    CALL CalcH("PCB-89",  "73575-57-2", 2.91E-4)
    CALL CalcH("PCB-90",  "68194-07-0", 3.86E-4)
    CALL CalcH("PCB-91",  "68194-05-8", 3.53E-4)
    CALL CalcH("PCB-92",  "52663-61-3", 3.22E-4)
    CALL CalcH("PCB-93",  "73575-56-1", 2.84E-4)
    CALL CalcH("PCB-94",  "73575-55-0", 4.34E-4)
    CALL CalcH("PCB-95",  "38379-99-6", 2.90E-4)
    CALL CalcH("PCB-96",  "73575-54-9", 3.81E-4)
    CALL CalcH("PCB-97",  "41464-51-1", 1.79E-4)
    CALL CalcH("PCB-98",  "60233-25-2", 4.96E-4)
    CALL CalcH("PCB-99",  "38380-01-7", 3.01E-4)
    CALL CalcH("PCB-100", "39485-83-1", 6.18E-4)
    CALL CalcH("PCB-102", "68194-06-9", 3.48E-4)
    CALL CalcH("PCB-103", "60145-21-3", 5.55E-4)
    CALL CalcH("PCB-105", "32598-14-4", 0.60E-4)
    CALL CalcH("PCB-106", "70424-69-0", 1.94E-4)
    CALL CalcH("PCB-107", "70424-68-9", 2.00E-4)
    CALL CalcH("PCB-108", "70362-41-3", 3.48E-4)
    CALL CalcH("PCB-109", "74472-35-8", 3.89E-4)
    CALL CalcH("PCB-110", "38380-03-9", 1.89E-4)
    CALL CalcH("PCB-111", "39635-32-0", 4.88E-4)
    CALL CalcH("PCB-112", "74472-36-9", 3.29E-4)
    CALL CalcH("PCB-113", "68194-10-5", 4.78E-4)
    CALL CalcH("PCB-114", "74472-37-0", 1.14E-4)
    CALL CalcH("PCB-115", "74472-38-1", 3.07E-4)
    CALL CalcH("PCB-116", "18259-05-7", 2.31E-4)
    CALL CalcH("PCB-117", "68194-11-6", 2.48E-4)
    CALL CalcH("PCB-118", "31508-00-6", 1.16E-4)
    CALL CalcH("PCB-119", "56558-17-9", 4.55E-4)
    CALL CalcH("PCB-120", "68194-12-7", 4.04E-4)
    CALL CalcH("PCB-121", "56558-18-0", 7.44E-4)
    CALL CalcH("PCB-122", "76842-07-4", 1.37E-4)
    CALL CalcH("PCB-123", "65510-44-3", 2.62E-4)
    CALL CalcH("PCB-124", "70424-70-3", 1.95E-4)
    CALL CalcH("PCB-125", "74472-39-2", 3.28E-4)
    CALL CalcH("PCB-126", "57465-28-8", 0.54E-4)
    CALL CalcH("PCB-127", "39635-33-1", 3.40E-4)
    CALL CalcH("PCB-129", "55215-18-4", 0.85E-4)
    CALL CalcH("PCB-130", "52663-66-8", 1.92E-4)
    CALL CalcH("PCB-131", "61798-70-7", 2.59E-4)
    CALL CalcH("PCB-132", "38380-05-1", 1.61E-4)
    CALL CalcH("PCB-133", "35694-04-3", 3.34E-4)
    CALL CalcH("PCB-134", "52704-70-8", 2.00E-4)
    CALL CalcH("PCB-135", "52744-13-5", 3.04E-4)
    CALL CalcH("PCB-136", "38411-22-2", 2.52E-4)
    CALL CalcH("PCB-137", "35694-06-5", 2.08E-4)
    CALL CalcH("PCB-138", "35065-28-2", 1.07E-4)
    CALL CalcH("PCB-139", "56030-56-9", 3.81E-4)
    CALL CalcH("PCB-140", "59291-64-4", 4.25E-4)
    CALL CalcH("PCB-141", "52712-04-6", 1.43E-4)
    CALL CalcH("PCB-142", "41411-61-4", 2.09E-4)
    CALL CalcH("PCB-143", "68194-15-0", 2.56E-4)
    CALL CalcH("PCB-144", "68194-14-9", 3.18E-4)
    CALL CalcH("PCB-145", "74472-40-5", 4.08E-4)
    CALL CalcH("PCB-146", "51908-16-8", 2.49E-4)
    CALL CalcH("PCB-147", "68194-13-8", 3.22E-4)
    CALL CalcH("PCB-148", "74472-41-6", 5.68E-4)
    CALL CalcH("PCB-149", "38380-04-0", 2.18E-4)
    CALL CalcH("PCB-150", "68194-08-1", 5.15E-4)
    CALL CalcH("PCB-151", "52663-63-5", 2.59E-4)
    CALL CalcH("PCB-152", "68194-09-2", 3.51E-4)
    CALL CalcH("PCB-154", "60145-22-4", 4.82E-4)
    CALL CalcH("PCB-156", "38380-08-4", 0.22E-4)
    CALL CalcH("PCB-157", "69782-90-7", 0.66E-4)
    CALL CalcH("PCB-158", "74472-42-7", 2.16E-4)
    CALL CalcH("PCB-159", "39635-35-3", 3.10E-4)
    CALL CalcH("PCB-160", "41411-62-5", 2.54E-4)
    CALL CalcH("PCB-161", "74472-43-8", 5.06E-4)
    CALL CalcH("PCB-162", "39635-34-2", 2.06E-4)
    CALL CalcH("PCB-163", "74472-44-9", 1.57E-4)
    CALL CalcH("PCB-164", "74472-45-0", 1.97E-4)
    CALL CalcH("PCB-165", "74472-46-1", 4.46E-4)
    CALL CalcH("PCB-166", "41411-63-6", 1.72E-4)
    CALL CalcH("PCB-167", "52663-72-6", 1.23E-4)
    CALL CalcH("PCB-168", "59291-65-5", 4.63E-4)
    CALL CalcH("PCB-169", "32774-16-6", 0.59E-4)

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

  END SUBROUTINE ref1141

  !---------------------------------------------------------------------------

  SUBROUTINE ref1142
    IMPLICIT NONE

    ref = "1142"


    chem = "1-hydroxy-2-methoxybenzene" ; casrn = "90-05-1"
    type = "V"
    CALL Output(1./0.13)
    type = "M"
    CALL Output(1./0.13)
    type = "M"
    CALL Output(1./0.11,63.1E3/Rgas)

    chem = "4-methyl-2-methoxyphenol" ; casrn = "93-51-6"
    type = "V"
    CALL Output(1./0.096)
    type = "M"
    CALL Output(1./0.13)
    type = "M"
    CALL Output(1./0.14,61.6E3/Rgas)

    chem = "1,3-dimethoxy-2-hydroxybenzene" ; casrn = "91-10-1" ! 2,6-DMP
    type = "V"
    CALL Output(1./8E-3)
    type = "M"
    CALL Output(1./0.027)
    type = "M"
    CALL Output(1./0.020,55.9E3/Rgas)

    chem = "molinate" ; casrn = "2212-67-1"
    type = "V"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./0.095)
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./0.46)
    type = "M"
    mindHR = 60.8E3/Rgas
    CALL Output(1./(0.39*EXP(mindHR*(1/293.15-1/T0))),mindHR)

  END SUBROUTINE ref1142

  !---------------------------------------------------------------------------

  SUBROUTINE ref1143 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1143"
    type = "T"

    CALL CalcH("PCB-28",   "7012-37-5", 11.97, 3100.)
    CALL CalcH("PCB-52",  "35693-99-3", 13.15, 3352.)
    CALL CalcH("PCB-77",  "32598-13-3", 12.86, 3213.)
    CALL CalcH("PCB-101", "37680-73-2", 13.55, 3531.)
    CALL CalcH("PCB-105", "32598-14-4", 13.62, 3601.)
    CALL CalcH("PCB-118", "31508-00-6", 13.44, 3535.)
    CALL CalcH("PCB-126", "57465-28-8", 14.03, 3830.)
    CALL CalcH("PCB-138", "35065-28-2", 13.93, 3757.)
    CALL CalcH("PCB-153", "35065-27-1", 14.05, 3662.)
    CALL CalcH("PCB-169", "32774-16-6", 14.76, 3910.)
    CALL CalcH("PCB-180", "35065-29-3", 14.71, 3910.)

    CALL CalcH("PCDE-99",   "60123-64-0", 10.34, 2659.)
    CALL CalcH("PCDE-100", "104294-16-8", 10.34, 2524.)
    CALL CalcH("PCDE-137",  "71585-36-9", 11.09, 2794.)
    CALL CalcH("PCDE-138",  "71585-38-1", 11.08, 2840.)
    CALL CalcH("PCDE-153",  "71859-30-8", 11.11, 2724.)
    CALL CalcH("PCDE-154", "106220-81-9", 11.02, 2582.)
    CALL CalcH("PCDE-167", "131138-20-0", 11.12, 2705.)
    CALL CalcH("PCDE-180",  "83992-69-2", 11.70, 2973.)
    CALL CalcH("PCDE-182",  "88467-63-4", 11.74, 2761.)
    CALL CalcH("PCDE-184", "106220-84-2", 12.12, 3406.)
    CALL CalcH("PCDE-196",  "85918-38-3", 12.44, 3094.)
    CALL CalcH("PCDE-197", "117948-62-6", 12.34, 3049.)

    CALL CalcH("alpha-hexachlorocyclohexane",       "319-84-6",  7.98, 1714.)
    CALL CalcH("lindane",                            "58-89-9", 11.58, 3093.)
    CALL CalcH("hexachlorobenzene",                 "118-74-1",  7.97, 1622.)
    CALL CalcH("oxychlordane",                    "27304-13-8",  7.48, 1866.)
    CALL CalcH("trans-chlordane",                  "5103-74-2", 12.84, 3098.)
    CALL CalcH("cis-chlordane",                    "5103-71-9", 12.92, 3160.)
    CALL CalcH("trans-nonachlor",                 "39765-80-5", 14.10, 3279.)
    CALL CalcH("p,p'-DDE",                           "72-55-9", 12.62, 3291.)
    CALL CalcH("p,p'-DDT",                           "50-29-3", 13.02, 3369.)
    CALL CalcH("p,p'-DDD",                           "72-54-8", 12.20, 3180.)

    CALL CalcH("2378-TeCDD",                       "1746-01-6",  8.86, 1574.)
    CALL CalcH("12378-PeCDD",                     "40321-76-4",  7.94, 1089.)
    CALL CalcH("123478-HxCDD",                    "39227-28-6",  8.14, 1254.)
    CALL CalcH("123678-HxCDD",                    "57653-85-7",  8.35, 1236.)
    CALL CalcH("123789-HxCDD",                    "19408-74-3",  7.58, 1184.)
    CALL CalcH("1234678-HpCDD",                   "35822-46-9",  7.56, 1024.)
    CALL CalcH("OCDD",                             "3268-87-9",  8.34, 1009.)
    CALL CalcH("2378-TeCDF",                      "51207-31-9",  8.01, 1598.)
    CALL CalcH("12378-PeCDF",                     "57117-41-6",  7.44, 1306.)
    CALL CalcH("23478-PeCDF",                     "57117-31-4",  6.82, 1244.)
    CALL CalcH("123478-HxCDF",                    "70648-26-9",  6.90, 1048.)
    CALL CalcH("123678-HxCDF",                    "57117-44-9",  7.79, 1439.)
    CALL CalcH("123789-HxCDF",                    "72918-21-9",  7.01, 1136.)
    CALL CalcH("234678-HxCDF",                    "60851-34-5",  7.21, 1124.)
    CALL CalcH("124678-HxCDF",                    "67562-40-7",  6.80,  985.)
    CALL CalcH("123468-HxCDF",                    "69698-60-8",  7.04, 1020.)
    CALL CalcH("124689-HxCDF",                    "69698-59-5",  7.40, 1117.)
    CALL CalcH("1234678-HpCDF",                   "67562-39-4",  6.53,  674.)
    CALL CalcH("1234789-HpCDF",                   "55673-89-7",  6.32,  912.)
    CALL CalcH("1234689-HpCDF",                   "69698-58-4",  6.03,  765.)
    CALL CalcH("OCDF",                            "39001-02-0",  7.20, 1060.)

    CALL CalcH("naphthalene",                        "91-20-3",  5.07,  922.)
    CALL CalcH("biphenyl",                           "92-52-4",  6.33, 1255.)
    CALL CalcH("acenaphthylene",                    "208-96-8",  8.22, 2178.)
    CALL CalcH("acenaphthene",                       "83-32-9",  5.63, 1240.)
    CALL CalcH("fluorene",                           "86-73-7",  6.97, 1590.)
    CALL CalcH("phenanthrene",                       "85-01-8",  8.14, 2120.)
    CALL CalcH("anthracene",                        "120-12-7",  6.91, 1363.)
    CALL CalcH("fluoranthene",                      "206-44-0",  8.23, 2336.)
    CALL CalcH("pyrene",                            "129-00-0",  9.17, 2475.)
    CALL CalcH("benz[a]anthracene",                  "56-55-3",  9.67, 2641.)
    CALL CalcH("chrysene",                          "218-01-9",  9.98, 2770.)
    CALL CalcH("benzo[b]fluoranthene",              "205-99-2",  9.83, 3275.)
    CALL CalcH("benzo[k]fluoranthene",              "207-08-9",  9.83, 2979.)
    CALL CalcH("benzo[e]pyrene",                    "192-97-2", 11.64, 3600.)
    CALL CalcH("benzo[a]pyrene",                     "50-32-8", 12.02, 3558.)
    CALL CalcH("perylene",                          "198-55-0",  9.84, 2752.)
    CALL CalcH("indeno[cd]pyrene",                  "193-39-5", 10.36, 3208.)
    CALL CalcH("dibenz[ah]anthracene",               "53-70-3", 11.23, 3371.)
    CALL CalcH("benzo[ghi]perylene",                "191-24-2", 12.83, 4006.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, AH, BH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: AH, BH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (10.**(AH-BH/T0))
      mindHR = BH * LOG(10.)
    CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1143

  !---------------------------------------------------------------------------

  SUBROUTINE ref1144 ! KHcc [1]
    IMPLICIT NONE

    ref = "1144"
    type = "M"

    CALL CalcH("naphthalene",              "91-20-3", 3.0E-2)
    CALL CalcH("1-methylnaphthalene",      "90-12-0", 2.5E-2)
    CALL CalcH("2-methylnaphthalene",      "91-57-6", 1.3E-2)
    CALL CalcH("biphenyl",                 "92-52-4", 7.9E-3) ! (C6H5)2
    CALL CalcH("2,3-benzindene",           "86-73-7", 2.6E-3) ! C{13}H{10} fluorene
    CALL CalcH("acenaphthene",             "83-32-9", 2.6E-3) ! C{12}H{10}
    CALL CalcH("acenaphthylene",          "208-96-8", 4.6E-3) ! C{12}H8
    CALL CalcH("phenanthrene",             "85-01-8", 9.6E-4) ! C{14}H{10}
    CALL CalcH("anthracene",              "120-12-7", 7.9E-4) ! C{14}H{10}
    CALL CalcH("chlorpyrifos",           "2921-88-2", 1.7E-4) ! C9H{11}Cl3NO3PS
    CALL CalcH("DEF",                      "78-48-8", 1.2E-5) ! C{12}H{27}OPS3 DEF
    CALL CalcH("pendimethalin",         "40487-42-1", 3.5E-5) ! C{13}H{19}N3O4
    CALL CalcH("parathion",                "56-38-2", 3.5E-6) ! C{10}H{14}NO5PS
    CALL CalcH("methylparathion",         "298-00-0", 2.5E-6) ! C{8}H{10}NO5PS
    CALL CalcH("malathion",               "121-75-5", 2.0E-7) ! C{10}H{19}O6PS2
    CALL CalcH("2,2'-dichlorobiphenyl", "13029-08-8", 1.0E-2) ! C{12}H8Cl2 PCB-4
    CALL CalcH("4,4'-dichlorobiphenyl",  "2050-68-2", 3.9E-3) ! C{12}H8Cl2 PCB-15

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

  END SUBROUTINE ref1144

  !---------------------------------------------------------------------------

  SUBROUTINE ref1145
    IMPLICIT NONE

    ref = "1145"

    CALL CalcH("methane",                         "74-82-8",   -1.429, -1.229, "V")
    CALL CalcH("octane",                          "111-65-9",  -2.117, -2.090, "V")
    CALL CalcH("methylcyclohexane",               "108-87-2",  -1.243, -1.141, "V")
    CALL CalcH("{cis}-1,2-dimethylcyclohexane",   "2207-01-4", -1.161, -1.264, "V")
    CALL CalcH("1-octene",                        "111-66-0",  -1.590, -1.412, "V")
    CALL CalcH("2,3-dimethyl-1,3-butadiene",      "513-81-5",  -0.310, -0.893, "V")
    CALL CalcH("1-nonyne",                        "3452-09-3", -0.764, -0.556, "V")
    CALL CalcH("methanal",                        "50-00-0",    5.166,  5.395, "")
    CALL CalcH("pentanal",                        "110-62-3",   2.221,  2.188, "")
    CALL CalcH("nonanal",                         "124-19-6",   1.523,  1.696, "")
    CALL CalcH("ethanedioic acid",                "144-62-7",   8.233,  9.006, "")
    CALL CalcH("3-hexanol",                       "623-37-0",   2.757,  3.143, "V")
    CALL CalcH("4-methyl-2-pentanol",             "108-11-2",   2.717,  3.143, "V")
    CALL CalcH("2-octanol",                       "123-96-6",   2.824,  2.897, "V")
    CALL CalcH("cyclohexanol",                    "108-93-0",   3.943,  3.699, "V")
    CALL CalcH("trichloroethanal",                "75-87-6",    6.925,  6.632, "")
    CALL CalcH("isopentyl ethanoate",             "123-92-2",   1.708,  1.652, "V")
    CALL CalcH("isopropyl propanoate",            "637-78-5",   1.627,  1.775, "V")
    CALL CalcH("propyl butanoate",                "105-66-8",   1.596,  1.652, "V")
    CALL CalcH("ethyl pentanoate",                "539-82-2",   1.848,  1.652, "V")
    CALL CalcH("ethyl heptanoate",                "106-30-9",   1.700,  1.406, "V")
    CALL CalcH("methyl propyl ether",             "557-17-5",   1.218,  1.207, "V")
    CALL CalcH("phenyloxirane",                   "96-09-3",    3.190,  3.402, "V")
    CALL CalcH("2-hexanone",                      "591-78-6",   2.408,  2.324, "V")
    CALL CalcH("5-nonanone",                      "502-56-7",   1.920,  1.955, "V")
    CALL CalcH("cyclohexanone",                   "108-94-1",   3.039,  2.680, "V")
    CALL CalcH("1,4-dihydroxybenzene",            "123-31-9",   8.804,  8.623, "V")
    CALL CalcH("hexamethyleneimine",              "111-49-9",   3.600,  3.031, "")
    CALL CalcH("3-chloropentane",                 "616-20-6",  -0.030, -0.018, "V")
    CALL CalcH("pentachloroethane",               "76-01-7",    1.118,  1.664, "V")
    CALL CalcH("chloropentafluoroethane",         "76-15-3",   -2.037, -2.542, "V")
    CALL CalcH("1,2-dibromo-3-chloropropane",     "96-12-8",    2.222,  1.605, "V")
    CALL CalcH("1,3-dichloro-2-propanol",         "96-23-1",    4.154,  4.618, "V")
    CALL CalcH("2,5-dimethylpyridine",            "589-93-5",   3.450,  3.455, "")
    CALL CalcH("butylbenzene",                    "104-51-8",   0.273,  0.245, "V")
    CALL CalcH("hexylbenzene",                    "1077-16-3",  0.051, -0.001, "V")
    CALL CalcH("di-(2-ethylhexyl)-phthalate",     "117-81-7",   3.220,  3.316, "V")
    CALL CalcH("methyl benzoate",                 "93-58-3",    2.838,  2.848, "V")
    CALL CalcH("diphenylmethane",                 "101-81-5",   2.043,  1.707, "V")
    CALL CalcH("octachlorostyrene",               "29082-74-4", 2.273,  2.027, "")
    CALL CalcH("2,3,6-trichloroanisole",          "50375-10-5", 1.929,  2.276, "")
    CALL CalcH("benzenecarboxylic acid",          "65-85-0",    5.635,  5.353, "V")
    CALL CalcH("1,2,4-trichlorobenzene",          "120-82-1",   1.236,  1.048, "C")
    CALL CalcH("1,3,5-trichlorobenzene",          "108-70-3",   1.109,  1.048, "")
    CALL CalcH("1,2,3,4-tetrachlorobenzene",      "634-66-2",   1.549,  1.178, "")
    CALL CalcH("1,2,3,5-tetrachlorobenzene",      "634-90-2",   1.193,  1.178, "C")
    CALL CalcH("1,2,4,5-tetrachlorobenzene",      "95-94-3",    1.388,  1.178, "")
    CALL CalcH("1-methyl-2,3,6-trichlorobenzene", "2077-46-5",  1.211,  1.005, "")
    CALL CalcH("1-methyl-2,4,5-trichlorobenzene", "6639-30-1",  1.211,  1.005, "")
    CALL CalcH("pentachloromethylbenzene",        "877-11-2",   1.501,  1.265, "")
    CALL CalcH("(dimethylamino)-benzene",         "121-69-7",   2.503,  3.426, "V")
    CALL CalcH("1-amino-2-chlorobenzene",         "95-51-2",    3.766,  4.239, "V")
    CALL CalcH("1-amino-4-chlorobenzene",         "106-47-8",   4.798,  4.239, "V")
    CALL CalcH("3-nitrobenzenamine",              "99-09-2",    6.230,  6.513, "V")
    CALL CalcH("diphenylamine",                   "122-39-4",   3.940,  4.367, "V")
    CALL CalcH("1-hydroxy-3-methylbenzene",       "108-39-4",   4.452,  4.597, "V")
    CALL CalcH("1,3-dimethyl-4-hydroxybenzene",   "105-67-9",   4.087,  4.554, "V")
    CALL CalcH("hydroxypentachlorobenzene",       "87-86-5",    6.000,  5.291, "") ! 2nd value from ref2448
    CALL CalcH("4,4'-dichlorobiphenyl",           "2050-68-2",  2.300,  2.032, "")
    CALL CalcH("3,3'-dichlorobiphenyl",           "2050-67-1",  2.021,  2.032, "")
    CALL CalcH("2,3',5-trichlorobiphenyl",        "38444-81-4", 1.920,  2.162, "")
    CALL CalcH("3,3',4,4'-tetrachlorobiphenyl",   "32598-13-3", 2.550,  2.293, "")
    CALL CalcH("2,2',4,5,5'-pentachlorobiphenyl", "37680-73-2", 1.950,  2.423, "")
    CALL CalcH("2-methylnaphthalene",             "91-57-6",    1.781,  1.625, "V")
    CALL CalcH("2,3-dimethylnaphthalene",         "581-40-8",   1.425,  1.582, "C")
    CALL CalcH("benzo[b]pyridine",                "91-22-5",    4.170,  4.551, "V")
    CALL CalcH("alachlor",                        "15972-60-8", 6.468,  8.310, "")
    CALL CalcH("aldrin",                          "309-00-2",   1.700,  1.801, "C")
    CALL CalcH("carbaryl",                        "63-25-2",    6.750,  6.892, "V")
    CALL CalcH("chlorothalonil",                  "1897-45-6",  5.097,  5.208, "")
    CALL CalcH("chlorpyrifos",                    "2921-88-2",  3.770,  5.786, "")
    CALL CalcH("diazinon",                        "333-41-5",   5.335,  5.547, "")
    CALL CalcH("heptachlor",                      "76-44-8",    1.218,  2.144, "C")
    CALL CalcH("pentachloronitrobenzene",         "82-68-8",    3.824,  3.712, "")

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, lwapc1, lwapc2, type_, string)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: lwapc1, lwapc2
      CHARACTER(LEN=*), INTENT(IN)           :: type_
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: string

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it

      ! known LWAPC:
      IF (TRIM(type_)/="") THEN
        type = type_
        IF (PRESENT(string)) CALL Settypex(TRIM(string))
        IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
        CALL Output(Hcc_TO_HcpSI(10.**lwapc1,T0))
      ENDIF

      ! bond-estimated LWAPC:
      type = "Q"
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      CALL Output(Hcc_TO_HcpSI(10.**lwapc2,T0))

    END SUBROUTINE CalcH

  END SUBROUTINE ref1145

  !---------------------------------------------------------------------------

  SUBROUTINE ref1146
    IMPLICIT NONE

    ref = "1146"
    type = "M"

    ! Table 1:

    chem = "1,3,5-trichlorobenzene" ; casrn = "108-70-3" ! C6H3Cl3
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./192.)

    chem = "1,2,4-trichlorobenzene" ; casrn = "120-82-1" ! C6H3Cl3
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./101.)

    chem = "1,2,3-trichlorobenzene" ; casrn = "87-61-6" ! C6H3Cl3
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./72.)

    chem = "1,2,3,5-tetrachlorobenzene" ; casrn = "634-90-2" ! C6H2Cl4
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./99.)

    ! Table 2:

    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 14.8,20.1,22.1,24.2,34.8,50.5 /) + CtoK

    chem = "1,2,3,4-tetrachlorobenzene" ; casrn = "634-66-2"
    Harray = 1. / (/ 48.5,52.0,68.1,70.9,127.9,276.2 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "pentachlorobenzene" ; casrn = "608-93-5"
    Harray = 1. / (/ 37.4,49.4,68.1,66.7,124.1,276.2 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "hexachlorobenzene" ; casrn = "118-74-1"
    Harray = 1. / (/ 23.6,30.0,46.6,52.5,88.3,217.2 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10.4,20.0,30.1,34.9,42.1,47.9 /) + CtoK
    chem = "2,5-dichlorobiphenyl" ; casrn = "34883-39-1"
    Harray = 1. / (/ 16.1,29.6,58.2,82.2,123.0,165.4 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    ndata = 7
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10.4,20.0,30.1,34.9,42.1,47.9,48.4 /) + CtoK

    chem = "2,4,4'-trichlorobiphenyl" ; casrn = "7012-37-5"
    Harray = 1. / (/ 8.7,21.2,47.4,50.3,70.8,120.6,122.2 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "2,2',5,5'-tetrachlorobiphenyl" ; casrn = "35693-99-3"
    Harray = 1. / (/ 8.6,16.4,37.4,38.8,68.7,109.2,120.6 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10.0,20.0,35.0,40.1,45.0,55.0 /) + CtoK

    chem = "fluoranthene" ; casrn = "206-44-0"
    Harray = 1. / (/ 0.26,0.64,1.63,2.38,5.84,6.23 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "benzo[b]fluoranthene" ; casrn = "205-99-2"
    Harray = 1. / (/ 0.025,0.051,0.119,0.151,0.208,0.370 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "benzo[k]fluoranthene" ; casrn = "207-08-9"
    Harray = 1. / (/ 0.022,0.043,0.107,0.138,0.198,0.403 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "benzo[a]pyrene" ; casrn = "50-32-8"
    Harray = 1. / (/ 0.022,0.034,0.074,0.092,0.110,0.239 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "benzo[ghi]perylene" ; casrn = "191-24-2"
    Harray = 1. / (/ 0.019,0.027,0.052,0.054,0.066,0.087 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "indeno[1,2,3-cd]pyrene" ; casrn = "193-39-5"
    Harray = 1. / (/ 0.018,0.029,0.057,0.061,0.077,0.105 /)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1146

  !---------------------------------------------------------------------------

  SUBROUTINE ref1148 ! Hcc [1]
    IMPLICIT NONE

    ref = "1148"
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/293.15,303.15,313.15,323.15/)

    CALL CalcH("benzene",                 "71-43-2", (/ 5.418,3.446,2.396,1.686 /) )
    CALL CalcH("chlorobenzene",          "108-90-7", (/ 8.898,6.479,5.494,5.274 /) )
    CALL CalcH("trichloroethene",         "79-01-6", (/ 3.153,1.859,1.294,0.930 /) )
    CALL CalcH("(E)-1,2-dichloroethene", "156-60-5", (/ 3.196,1.865,1.276,0.889 /) )

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, k0)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: k0

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = Hcc_TO_HcpSI(k0,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1148

  !---------------------------------------------------------------------------

  SUBROUTINE ref1149 ! Hcc [1]
    IMPLICIT NONE

    ref = "1149"
    type = "M"
    chem = "molecular iodine" ; casrn = "7553-56-2" ! I2
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 25.,40.,60. /) + CtoK
    Harray = Hcc_TO_HcpSI( (/ 70.,35.,17. /) , temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref1149

  !---------------------------------------------------------------------------

  SUBROUTINE ref1150 ! KHpx [atm]
    IMPLICIT NONE

    ref = "1150"
    type = "M"

    CALL CalcH("1,1,1-trichloroethane",     "71-55-6", 21.68, 4375.)
    CALL CalcH("1,1-dichloroethene",        "75-35-4", 23.12, 4618.)
    CALL CalcH("trichloroethene",           "79-01-6", 21.89, 4647.)
    CALL CalcH("tetrachloroethene",        "127-18-4", 22.68, 4735.)
    CALL CalcH("dichloromethane",           "75-09-2", 17.42, 3645.)
    CALL CalcH("trichloromethane",          "67-66-3", 18.97, 4046.)
    CALL CalcH("tetrachloromethane",        "56-23-5", 22.22, 4438.)
    CALL CalcH("1,2-dichloroethane",       "107-06-2", 16.05, 3539.)
    CALL CalcH("1,1,2-trichloroethane",     "79-00-5", 16.20, 3690.)
    CALL CalcH("1,1,2,2-tetrachloroethane", "79-34-5", 14.91, 3547.)
    CALL CalcH("1,2-dichloropropane",       "78-87-5", 19.60, 4333.)
    CALL CalcH("1,3-dichloropropane",      "142-28-9", 17.13, 3917.)
    CALL CalcH("1,2,3-trichloropropane",    "96-18-4", 14.61, 3477.)
    CALL CalcH("1-chlorobutane",           "109-69-3", 18.51, 3482.)
    CALL CalcH("2-chlorobutane",            "78-86-4", 22.29, 4499.)
    CALL CalcH("1,4-dichlorobutane",       "110-56-5", 13.79, 3128.)
    CALL CalcH("1-chloropentane",          "543-59-9", 23.04, 4727.)
    CALL CalcH("1,5-dichloropentane",      "628-76-2",  8.79, 1597.)
    CALL CalcH("1-chlorohexane",           "544-10-5", 22.16, 4459.)
    CALL CalcH("benzene",                   "71-43-2", 19.02, 3964.)
    CALL CalcH("chlorobenzene",            "108-90-7", 16.83, 3466.)
    CALL CalcH("methylbenzene",            "108-88-3", 18.46, 3751.)
    CALL CalcH("1-chloro-2-methylbenzene",  "95-49-8", 17.18, 3545.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      mindHR = B
      Hominus = KHpx_TIMES_HcpSI / (EXP(A-B/T0))
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1150

  !---------------------------------------------------------------------------

  SUBROUTINE ref1151 ! Hcc [1]
    IMPLICIT NONE

    ref = "1151"

    chem = "dinitrogen tetroxide" ; casrn = "10544-72-6" ! N2O4
    type = "M"
    CALL Output(77. * Hcc_TO_HcpSI_atT0)

    chem = "nitrogen dioxide" ; casrn = "10102-44-0" ! NO2
    type = "?"
    CALL Output(1.0 * Hcc_TO_HcpSI_atT0)

    chem = "nitrogen monoxide" ; casrn = "10102-43-9" ! NO nitric oxide
    type = "?"
    CALL Output(0.046 * Hcc_TO_HcpSI_atT0)

  END SUBROUTINE ref1151

  !---------------------------------------------------------------------------

  SUBROUTINE ref1152
    IMPLICIT NONE

    ref = "1152"
    type = "M"

    ! Table 5:

    chem = "alpha-1,2,3,4,5,6-hexachlorocyclohexane" ; casrn = "319-84-6"
    mindHR = 2810. * LOG(10.)
    Hominus    = 10.**(2810./T0-9.31)
    CALL Output(Hominus, mindHR)

    chem = "gamma-1,2,3,4,5,6-hexachlorocyclohexane" ; casrn = "58-89-9"
    mindHR = 2382. * LOG(10.)
    Hominus    = 10.**(2382./T0-7.54)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref1152

  !---------------------------------------------------------------------------

  SUBROUTINE ref1153 ! KHpx [bar]
    IMPLICIT NONE
    REAL, PARAMETER :: A0 = 4.800
    REAL, PARAMETER :: A1 = 3934.40
    REAL, PARAMETER :: A2 = -941290.2

    ref = "1153"
    type = "L"
    chem = "carbon dioxide" ; casrn = "124-38-9" ! CO2
    ! ln(H) = A0 + A1/T + A2/T^2
    Hominus    = cH2O / (EXP(A0+A1/T0+A2/(T0*T0)) * bar)
    ! mindHR = d ln(H) / d(1/T) = A1 + 2 A2/T
    mindHR = - (A1 + 2. * A2 / T0)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref1153

  !---------------------------------------------------------------------------

  SUBROUTINE ref1155 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1155"
    type = "M"

    CALL CalcH("2-nitrophenol",                "88-75-5", 1.3E+00)
    CALL CalcH("4-nitrophenol",               "100-02-7", 1.3E-03)
    CALL CalcH("3-methyl-2-nitrophenol",     "4920-77-8", 3.1E-01)
    CALL CalcH("4-methyl-2-nitrophenol",      "119-33-5", 1.5E+00)
    CALL CalcH("5-methyl-2-nitrophenol",      "700-38-9", 1.3E+00)
    CALL CalcH("6-methyl-2-nitrophenol",    "13073-29-5", 3.4E+00)
    CALL CalcH("4-sec-Butyl-2-nitrophenol",  "3555-18-8", 9.7E+00)
    CALL CalcH("3-methyl-4-nitrophenol",     "2581-34-2", 1.6E-03)
    CALL CalcH("2,4-dinitrophenol",            "51-28-5", 8.7E-03)
    CALL CalcH("DNOC",                        "534-52-1", 2.3E-02)
    CALL CalcH("4-methyl-2,6-dinitrophenol",  "609-93-8", 5.4E-03)
    CALL CalcH("dinoseb",                      "88-85-7", 4.5E-01)
    CALL CalcH("hydroxybenzene",              "108-95-2", 6.5E-02)
    CALL CalcH("1-hydroxy-2-methylbenzene",    "95-48-7", 1.4E-01)
    CALL CalcH("1-hydroxy-4-methylbenzene",   "106-44-5", 7.5E-02)
    CALL CalcH("5-fluoro-2-nitrophenol",      "446-36-6", 2.0E+00)
    CALL CalcH("4-methoxy-2-nitrophenol",    "1568-70-3", 1.9E-01)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("293")
      CALL Output(1./H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1155

  !---------------------------------------------------------------------------

  SUBROUTINE ref1156 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1156"
    type = "M"

    CALL CalcH("nonane",                         "111-84-2", -0.1847,  202.1)
    CALL CalcH("hexane",                         "110-54-3",   25.25,  7530.)
    CALL CalcH("2-methylpentane",                "107-83-5",   2.959,  957.2)
    CALL CalcH("cyclohexane",                    "110-82-7",   9.141,  3238.)
    CALL CalcH("chlorobenzene",                  "108-90-7",   3.469,  2689.)
    CALL CalcH("1,2-dichlorobenzene",             "95-50-1",  -1.518,  1422.)
    CALL CalcH("1,3-dichlorobenzene",            "541-73-1",   2.882,  2564.)
    CALL CalcH("1,4-dichlorobenzene",            "106-46-7",   3.373,  2720.)
    CALL CalcH("1,2-dimethylbenzene",             "95-47-6",   5.541,  3220.)
    CALL CalcH("1,4-dimethylbenzene",            "106-42-3",   6.931,  3520.)
    CALL CalcH("1,3-dimethylbenzene",            "108-38-3",   6.280,  3337.)
    CALL CalcH("propylbenzene",                  "103-65-1",   7.835,  3681.)
    CALL CalcH("ethylbenzene",                   "100-41-4",   11.92,  4994.)
    CALL CalcH("methylbenzene",                  "108-88-3",   5.133,  3024.)
    CALL CalcH("benzene",                         "71-43-2",   5.534,  3194.)
    CALL CalcH("(2-propyl)-benzene",              "98-82-8",   5.557,  3179.)
    CALL CalcH("1,1-dichloroethane",              "75-34-3",   5.484,  3137.)
    CALL CalcH("1,2-dichloroethane",             "107-06-2",  -1.371,  1522.)
    CALL CalcH("1,1,1-trichloroethane",           "71-55-6",   7.351,  3399.)
    CALL CalcH("1,1,2-trichloroethane",           "79-00-5",   9.320,  4843.)
    CALL CalcH("(Z)-1,2-dichloroethene",         "156-59-2",   5.164,  3143.)
    CALL CalcH("(E)-1,2-dichloroethene",         "156-60-5",   5.333,  2964.)
    CALL CalcH("tetrachloroethene",              "127-18-4",   10.65,  4368.)
    CALL CalcH("trichloroethene",                 "79-01-6",   7.845,  3702.)
    CALL CalcH("tetralin",                       "119-64-2",   11.83,  5392.)
    CALL CalcH("decalin",                         "91-17-8",   11.85,  4125.)
    CALL CalcH("chloroethene",                    "75-01-4",   6.138,  2931.)
    CALL CalcH("chloroethane",                    "75-00-3",   4.265,  2580.)
    CALL CalcH("hexachloroethane",                "67-72-1",   3.744,  2550.)
    CALL CalcH("tetrachloromethane",              "56-23-5",   9.739,  3951.)
    CALL CalcH("1,3,5-trimethylbenzene",         "108-67-8",   7.241,  3628.)
    CALL CalcH("1,2-dibromoethane",              "106-93-4",   5.703,  3876.)
    CALL CalcH("1,1-dichloroethene",              "75-35-4",   6.123,  2907.)
    CALL CalcH("dichloromethane",                 "75-09-2",   8.483,  4268.)
    CALL CalcH("trichloromethane",                "67-66-3",   11.41,  5030.)
    CALL CalcH("1,1,2,2-tetrachloroethane",       "79-34-5",   1.726,  2810.)
    CALL CalcH("1,2-dichloropropane",             "78-87-5",   9.843,  4708.)
    CALL CalcH("dibromochloromethane",           "124-48-1",   14.62,  6373.)
    CALL CalcH("1,2,4-trichlorobenzene",         "120-82-1",   7.361,  4028.)
    CALL CalcH("1,3-dimethyl-4-hydroxybenzene",  "105-67-9",  -16.34, -3307.)
    CALL CalcH("1,1,2-trichlorotrifluoroethane",  "76-13-1",   9.649,  3243.)
    CALL CalcH("2-butanone",                      "78-93-3",  -26.32, -5214.)
    CALL CalcH("4-methyl-2-pentanone",           "108-10-1",  -7.157,  160.6)
    CALL CalcH("2-methoxyethanol",               "109-86-4",  -6.050, -873.8)
    CALL CalcH("trichlorofluoromethane",          "75-69-4",   9.480,  3513.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: A, B

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      mindHR = B
      Hominus = 1. / (EXP(A-B/T0)*atm)
      CALL Makenote("1156", &
        "The value is most probably taken from the report by \citet{2894}.")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1156

  !---------------------------------------------------------------------------

  SUBROUTINE ref1157 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "1157"

    ! Tab. 3:
    CALL CalcH_Tab3("propenal",          "107-02-8", 5.47,    "C",           "20")    ! CH2CHCHO acrolein
    CALL CalcH_Tab3("aldicarb",          "116-06-3", 0.529,   "C",           "20")
    CALL CalcH_Tab3("aldrin",            "309-00-2", 1.42,    "C",           "20")    ! C{12}H8Cl6
    CALL CalcH_Tab3("aldrin",            "309-00-2", 1.64,    "C")                    ! C{12}H8Cl6
    CALL CalcH_Tab3("aldrin",            "309-00-2", 38.,     "C",           "20-25") ! C{12}H8Cl6
    CALL CalcH_Tab3("aldrin",            "309-00-2", 50.,     "C",           "20")    ! C{12}H8Cl6
    CALL CalcH_Tab3("chlordane",          "57-74-9", 4.92,    "C")                    ! C{10}H6Cl8
    CALL CalcH_Tab3("chlordane",          "57-74-9", 9.12,    "C")                    ! C{10}H6Cl8
    CALL CalcH_Tab3("mitotane",           "53-19-0", 0.0018,  "C",           "25-30")
    CALL CalcH_Tab3("o,p'-DDE",         "3424-82-6", 7.3,     "C",           "20")
    CALL CalcH_Tab3("p,p'-DDE",           "72-55-9", 2.2,     "C",           "20-25") ! C{14}H8Cl4 DDE
    CALL CalcH_Tab3("DDT",                "50-29-3", 6.02,    "C",           "20-25") ! C{14}H9Cl5 DDT
    CALL CalcH_Tab3("dieldrin",           "60-57-1", 0.02,    "C",           "20")    ! C{12}H8OCl6
    CALL CalcH_Tab3("dieldrin",           "60-57-1", 0.046,   "C",           "20-25") ! C{12}H8OCl6
    CALL CalcH_Tab3("dieldrin",           "60-57-1", 1.02,    "C",           "20-25") ! C{12}H8OCl6
    CALL CalcH_Tab3("dieldrin",           "60-57-1", 5.84,    "C")                    ! C{12}H8OCl6
    CALL CalcH_Tab3("dieldrin",           "60-57-1", 21.3,    "C")                    ! C{12}H8OCl6
    CALL CalcH_Tab3("endosulfan",        "959-98-8", 1.09,    "C")                    ! C9H6Cl6O3S
    CALL CalcH_Tab3("endrin",             "72-20-8", 0.00018, "C")                    ! C{12}H8Cl6O
    CALL CalcH_Tab3("hexachlorobenzene", "118-74-1", 68.2,    "C",           "20")    ! C6Cl6
    CALL CalcH_Tab3("alpha-bhc",         "319-84-6", 0.55,    "C",           "22-25") ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH_Tab3("beta-bhc",          "319-85-7", 0.018,   "C",           "20-25") ! C6H6Cl6 $\beta$-lindane
    CALL CalcH_Tab3("$\delta$-lindane",  "319-86-8", 0.018,   "C",           "20-25")
    CALL CalcH_Tab3("isophorone",         "78-59-1", 0.55,    "C",           "20")    ! C9H{14}O isophorone
    CALL CalcH_Tab3("lindane",            "58-89-9", 0.05,    "C",           "20")    ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH_Tab3("lindane",            "58-89-9", 0.2,     "C",           "20-25") ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH_Tab3("lindane",            "58-89-9", 0.73,    "C")                    ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH_Tab3("mirex",            "2385-85-5", 1013.,   "C",           "20")    ! C{10}Cl{12} dodecachloropentacyclodecane
    ! toxaphene is a mixture, not a pure substance:
    ! CALL CalcH_Tab3("toxaphene",        "8001-35-2", 490.,    "C")                    ! C{10}H{10}Cl8
    ! CALL CalcH_Tab3("toxaphene",        "8001-35-2", 6380.,   "kavanaugh80", "20")    ! C{10}H{10}Cl8

    ! Tab. 4:
    type = "V"
    CALL CalcH("propenal",                           "107-02-8", 7.74      ) ! CH2CHCHO acrolein
    CALL CalcH("alachlor",                         "15972-60-8", 0.0062    ) ! C{14}H{20}ClNO2
    CALL CalcH("aldicarb",                           "116-06-3", 0.00032   )
    CALL CalcH("aldrin",                             "309-00-2", 91.23     ) ! C{12}H8Cl6
    CALL CalcH("ametryn",                            "834-12-8", 0.00012   ) ! C9H{17}N5S
    CALL CalcH("atrazine",                          "1912-24-9", 0.00029   ) ! C8H{14}ClN5
    CALL CalcH("azinphos-methyl",                     "86-50-0", 0.0032    )
    CALL CalcH("benfluralin",                       "1861-40-1", 1.34      )
    CALL CalcH("bromacil",                           "314-40-9", 0.0019    )
    CALL CalcH("butylate",                          "2008-41-5", 0.56      )
    CALL CalcH("captan",                             "133-06-2", 0.60      )
    CALL CalcH("carbaryl",                            "63-25-2", 0.0013    ) ! C{12}H{11}NO2
    CALL CalcH("carbofuran",                        "1563-66-2", 0.00051   )
    CALL CalcH("carbophenothion",                    "786-19-6", 0.046     )
    CALL CalcH("chlordane",                           "57-74-9", 9.02      ) ! C{10}H6Cl8
    CALL CalcH("chlorfenvinphos",                    "470-90-6", 0.00028   )
    CALL CalcH("chloropicrin",                        "76-06-2", 197.27    )
    CALL CalcH("chlorpropham",                       "101-21-3", 0.0021    )
    CALL CalcH("chlorpyrifos",                      "2921-88-2", 1.75      ) ! C9H{11}Cl3NO3PS
    CALL CalcH("2,4-D",                               "94-75-7", 0.55      ) ! C8H6Cl2O3 (2,4-dichlorophenoxy)-acetic acid
    CALL CalcH("mitotane",                            "53-19-0", 0.64      )
    CALL CalcH("p,p'-DDD",                            "72-54-8", 0.64      ) ! C{14}H{10}Cl4 DDD
    CALL CalcH("o,p'-DDE",                          "3424-82-6", 2.54      )
    CALL CalcH("p,p'-DDE",                            "72-55-9", 7.95      ) ! C{14}H8Cl4 DDE
    CALL CalcH("DDT",                                 "50-29-3", 2.36      ) ! C{14}H9Cl5 DDT
    !CALL CalcH("demeton",                            "298-03-3", 0.13      )
    ! demeton is a mixture of demeton-O and demeton-S, value not used here
    CALL CalcH("dialifor",                         "10311-84-9", 0.14      )
    CALL CalcH("diallate",                          "2303-16-4", 0.25      )
    CALL CalcH("diazinon",                           "333-41-5", 0.067     ) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("dicamba",                           "1918-00-9", 0.00012   )
    CALL CalcH("dicapthon",                         "2463-84-5", 0.024     ) ! C8H9NO5ClPS
    CALL CalcH("2,6-dichlorobenzoic acid nitrile",  "1194-65-6", 0.67      )
    CALL CalcH("dichlofenthion",                      "97-17-6", 31645.57  )
    CALL CalcH("dichlorvos",                          "62-73-7", 0.19      ) ! C4H7Cl2O4P dichlorvos
    CALL CalcH("dieldrin",                            "60-57-1", 1.12      ) ! C{12}H8OCl6
    CALL CalcH("dimethoate",                          "60-51-5", 0.00011   )
    CALL CalcH("dinitramine",                      "29091-05-2", 0.16      )
    CALL CalcH("dinoseb",                             "88-85-7", 51.11     ) ! C{10}H{12}N2O5 dinoseb
    CALL CalcH("disulfoton",                         "298-04-4", 0.22      )
    CALL CalcH("diuron",                             "330-54-1", 0.0012    )
    CALL CalcH("dnoc",                               "534-52-1", 0.011     ) ! C7H6N2O5 6-methyl-2,4-dinitrophenol
    CALL CalcH("endosulfan",                         "959-98-8", 2.98      ) ! C9H6Cl6O3S
    CALL CalcH("endrin",                              "72-20-8", 0.033     ) ! C{12}H8Cl6O
    CALL CalcH("eptam",                              "759-94-4", 1.02      )
    CALL CalcH("ethion",                             "563-12-2", 0.032     )
    CALL CalcH("fenitrothion",                       "122-14-5", 0.0036    ) ! C9H{12}NO5PS
    CALL CalcH("fenthion",                            "55-38-9", 0.022     )
    CALL CalcH("fenuron",                            "101-42-8", 0.00027   )
    CALL CalcH("heptachlor",                          "76-44-8", 112.01    ) ! C{10}H5Cl7
    CALL CalcH("hexachlorobenzene",                  "118-74-1", 7.12      ) ! C6Cl6
    CALL CalcH("alpha-bhc",                          "319-84-6", 0.87      ) ! C6H6Cl6 $\alpha$-lindane
    CALL CalcH("beta-bhc",                           "319-85-7", 0.12      ) ! C6H6Cl6 $\beta$-lindane
    CALL CalcH("$\delta$-lindane",                   "319-86-8", 0.073     )
    CALL CalcH("isophorone",                          "78-59-1", 0.58      ) ! C9H{14}O isophorone
    CALL CalcH("leptophos",                        "21609-90-5", 0.25      ) ! C{13}H{10}O3BrCl2P
    CALL CalcH("lindane",                             "58-89-9", 0.13      ) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("linuron",                            "330-55-2", 0.0054    )
    CALL CalcH("malathion",                          "121-75-5", 0.0023    ) ! C{10}H{19}O6PS2
    CALL CalcH("methomyl",                         "16752-77-5", 0.000065  )
    CALL CalcH("methylchlorpyrifos",                "5598-13-0", 0.34      ) ! C7H7NO3Cl3PS
    CALL CalcH("methylparathion",                    "298-00-0", 0.0211    ) ! C{8}H{10}NO5PS
    CALL CalcH("mirex",                             "2385-85-5", 839.37    ) ! C{10}Cl{12} dodecachloropentacyclodecane
    CALL CalcH("monuron",                            "150-68-5", 0.0030    )
    CALL CalcH("nitralin",                          "4726-14-1", 138.15    )
    CALL CalcH("oxamyl",                           "23135-22-0", 0.00026   )
    CALL CalcH("paradichlorobenzene",                "106-46-7", 262.5     ) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("parathion",                           "56-38-2", 0.012     ) ! C{10}H{14}NO5PS
    CALL CalcH("pcp",                                 "87-86-5", 0.044     ) ! C6HCl5O pentachlorophenol
    CALL CalcH("pebulate",                          "1114-71-2", 11.67     )
    CALL CalcH("phenyl mercuric acetate",             "62-38-4", 0.000067  )
    CALL CalcH("phorate",                            "298-02-2", 0.65      )
    CALL CalcH("phosmet",                            "732-11-6", 0.00095   )
    CALL CalcH("phosphamidon",                     "13171-21-6", 0.36      )
    CALL CalcH("picloram",                          "1918-02-1", 0.000034  )
    CALL CalcH("pirimor",                          "23103-98-2", 0.00032   )
    CALL CalcH("profluralin",                      "26399-36-0", 39.07     )
    CALL CalcH("prometone",                         "1610-18-0", 0.000090  )
    CALL CalcH("prometryn",                         "7287-19-6", 0.00050   ) ! C{10}H{19}N5S
    CALL CalcH("propachlor",                        "1918-16-7", 0.011     )
    CALL CalcH("propanil",                           "709-98-8", 0.0036    )
    CALL CalcH("propazine",                          "139-40-2", 0.00010   )
    CALL CalcH("propoxur",                           "114-26-1", 0.13      )
    CALL CalcH("pyrazon",                           "1698-60-8", 4.31      )
    CALL CalcH("ronnel",                             "299-84-3", 3.22      ) ! C8H8O3Cl3PS
    CALL CalcH("secbumeton",                       "26259-45-0", 0.00035   )
    CALL CalcH("simazine",                           "122-34-9", 0.00034   ) ! C7H{12}ClN5
    CALL CalcH("2,4,5-trichlorophenoxyacetic acid",   "93-76-5", 0.0058    )
    CALL CalcH("terbacil",                          "5902-51-2", 0.000018  )
    ! data for sodium trichloroacetate (650-51-1, 0.00000035) not used
    ! because it is unlikely that this salt vaporizes
    CALL CalcH("terbutryn",                          "886-50-0", 0.0013    ) ! C{10}H{19}N5S
    ! toxaphene is a mixture, not a pure substance:
    ! CALL CalcH("toxaphene",                         "8001-35-2", 0.42      ) ! C{10}H{10}Cl8
    CALL CalcH("triallate",                         "2303-17-5", 1.02      )
    CALL CalcH("trichlorfon",                         "52-68-6", 0.0000017 ) ! C4H8Cl3O4P trichlorfon
    CALL CalcH("trifluralin",                       "1582-09-8", 4.02      ) ! C{13}H{16}F3N3O4
    CALL CalcH("vernolate",                         "1929-77-7", 2.05      )
    CALL CalcH("zinophos",                           "297-97-2", 0.083     )

  CONTAINS

    SUBROUTINE CalcH_Tab3 (CHEM_, CASRN_, KHpcSI, type_or_xref, T_range)
      IMPLICIT NONE
      CHARACTER(LEN=*),           INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),           INTENT(IN) :: CASRN_
      REAL,                       INTENT(IN) :: KHpcSI
      CHARACTER(LEN=*),           INTENT(IN) :: type_or_xref
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: T_range

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI / KHpcSI

      ! If type_or_xref has only 1 character, it is the type.
      ! Else, it is the xref and the type is "X":
      IF (LEN(type_or_xref)==1) THEN
        type = type_or_xref
      ELSE
        CALL SettypeX(type_or_xref)
      ENDIF

      IF (PRESENT(T_range)) THEN
        SELECT CASE(T_range)
        CASE ("20")
          CALL MakeNoteOtherTemp("293") ! at 20 C
        CASE ("20-25")
          CALL MakeNote(TRIM(ref)//"-"//T_range, &
            "Value for $T$~= 293\dots 298~\unit{K}.")
        CASE ("22-25")
          CALL MakeNote(TRIM(ref)//"-"//T_range, &
            "Value for $T$~= 295\dots 298~\unit{K}.")
        CASE ("25-30")
          CALL MakeNote(TRIM(ref)//"-"//T_range, &
            "Value for $T$~= 298\dots 303~\unit{K}.")
        CASE DEFAULT
          PRINT *, 'ERROR in T_range for ref1157: ', T_range
          STOP
        END SELECT
      ENDIF

      CALL Output(Hominus)
    END SUBROUTINE CalcH_Tab3

    SUBROUTINE CalcH (CHEM_, CASRN_, KHpcSI)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL,             INTENT(IN) :: KHpcSI
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI / KHpcSI
      CALL MakeNoteOtherTemp("293") ! at 20 C
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1157

  !---------------------------------------------------------------------------

  SUBROUTINE ref1182 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "1182"
    type = "L"

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0" ! CH3COOONO2 PAN
    mindHR = 5810.
    Hominus    = EXP(1.07)*Hbp_TO_HcpSI
    CALL Output(Hominus,mindHR)

    chem = "pernitric acid" ; casrn = "26404-66-0" ! HNO4
    mindHR = 8400.
    Hominus    = EXP(3.69)*Hbp_TO_HcpSI
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref1182

  !---------------------------------------------------------------------------

  SUBROUTINE ref1194 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1194"
    type = "M"

    CALL CalcH("PCB-6",   "25569-80-6", 2.5E-4)
    CALL CalcH("PCB-12",   "2974-92-7", 1.4E-4)
    CALL CalcH("PCB-18",  "37680-65-2", 2.5E-4)
    CALL CalcH("PCB-19",  "38444-73-4", 2.3E-4)
    CALL CalcH("PCB-24",  "55702-45-9", 2.2E-4)
    CALL CalcH("PCB-28",   "7012-37-5", 2.0E-4)
    CALL CalcH("PCB-29",  "15862-07-4", 2.0E-4)
    CALL CalcH("PCB-31",  "16606-02-3", 1.9E-4)
    CALL CalcH("PCB-36",  "38444-87-0", 1.7E-4)
    CALL CalcH("PCB-37",  "38444-90-5", 1.0E-4)
    CALL CalcH("PCB-40",  "38444-93-8", 1.0E-4)
    CALL CalcH("PCB-47",   "2437-79-8", 1.9E-4)
    CALL CalcH("PCB-52",  "35693-99-3", 2.0E-4)
    CALL CalcH("PCB-54",  "15968-05-5", 2.0E-4)
    CALL CalcH("PCB-62",  "54230-22-7", 2.1E-4)
    CALL CalcH("PCB-67",  "73575-53-8", 1.0E-4)
    CALL CalcH("PCB-70",  "32598-11-1", 1.0E-4)
    CALL CalcH("PCB-74",  "32690-93-0", 1.0E-4)
    CALL CalcH("PCB-85",  "65510-45-4", 0.66E-4)
    CALL CalcH("PCB-97",  "41464-51-1", 0.74E-4)
    CALL CalcH("PCB-99",  "38380-01-7", 0.78E-4)
    CALL CalcH("PCB-102", "68194-06-9", 0.90E-4)
    CALL CalcH("PCB-120", "68194-12-7", 0.56E-4)
    CALL CalcH("PCB-128", "38380-07-3", 0.13E-4)
    CALL CalcH("PCB-129", "55215-18-4", 0.29E-4)
    CALL CalcH("PCB-130", "52663-66-8", 0.37E-4)
    CALL CalcH("PCB-132", "38380-05-1", 0.44E-4)
    CALL CalcH("PCB-134", "52704-70-8", 0.49E-4)
    CALL CalcH("PCB-135", "52744-13-5", 0.56E-4)
    CALL CalcH("PCB-136", "38411-22-2", 0.88E-4)
    CALL CalcH("PCB-138", "35065-28-2", 0.21E-4)
    CALL CalcH("PCB-141", "52712-04-6", 0.23E-4)
    CALL CalcH("PCB-146", "51908-16-8", 0.25E-4)
    CALL CalcH("PCB-147", "68194-13-8", 0.51E-4)
    CALL CalcH("PCB-151", "52663-63-5", 0.59E-4)
    CALL CalcH("PCB-153", "35065-27-1", 0.23E-4)
    CALL CalcH("PCB-159", "39635-35-3", 0.20E-4)
    CALL CalcH("PCB-160", "41411-62-5", 0.20E-4)
    CALL CalcH("PCB-163", "74472-44-9", 0.15E-4)
    CALL CalcH("PCB-165", "74472-46-1", 0.29E-4)
    CALL CalcH("PCB-170", "35065-30-6", 0.09E-4)
    CALL CalcH("PCB-172", "52663-74-8", 0.13E-4)
    CALL CalcH("PCB-173", "68194-16-1", 0.14E-4)
    CALL CalcH("PCB-174", "38411-25-5", 0.14E-4)
    CALL CalcH("PCB-178", "52663-67-9", 0.23E-4)
    CALL CalcH("PCB-179", "52663-64-6", 0.24E-4)
    CALL CalcH("PCB-180", "35065-29-3", 0.10E-4)
    CALL CalcH("PCB-185", "52712-05-7", 0.16E-4)
    CALL CalcH("PCB-194", "35694-08-7", 0.10E-4)
    CALL CalcH("PCB-195", "52663-78-2", 0.11E-4)
    CALL CalcH("PCB-196", "42740-50-1", 0.10E-4)
    CALL CalcH("PCB-198", "68194-17-2", 0.14E-4)
    CALL CalcH("PCB-199", "52663-75-9", 0.10E-4)
    CALL CalcH("PCB-201", "40186-71-8", 0.17E-4)
    CALL CalcH("PCB-202",  "2136-99-4", 0.18E-4)

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

  END SUBROUTINE ref1194

  !---------------------------------------------------------------------------

  SUBROUTINE ref1195 ! KHcc [1]
    IMPLICIT NONE

    ref = "1195"
    type = "M"

    ! add a fourth, optional parameter if value is an upper limit of KHcc
    CALL CalcH("benzene",                         "71-43-2", 1.1E-1)    ! 1
    CALL CalcH("toluene",                        "108-88-3", 2.4E-1)    ! 2
    CALL CalcH("naphthalene",                     "91-20-3", 2.3E-2)    ! 3
    CALL CalcH("1-methylnaphthalene",             "90-12-0", 2.1E-2)    ! 4
    CALL CalcH("2-methylnaphthalene",             "91-57-6", 2.5E-2)    ! 5
    CALL CalcH("1-ethylnaphthalene",            "1127-76-0", 2.8E-2)    ! 6
    CALL CalcH("2-ethylnaphthalene",             "939-27-5", 2.2E-2)    ! 7
    CALL CalcH("pyrene",                         "129-00-0", 2.0E-4)    ! 8
    CALL CalcH("benzo(a)pyrene",                  "50-32-8", 3.0E-5)    ! 9
    CALL CalcH("nitrobenzene",                    "98-95-3", 3.5E-4)    ! 10
    CALL CalcH("1-methyl-2-nitrobenzene",         "88-72-2", 5.1E-4)    ! 11
    CALL CalcH("1-methyl-3-nitrobenzene",         "99-08-1", 3.8E-4)    ! 12
    CALL CalcH("1-methyl-4-nitrobenzene",         "99-99-0", 2.3E-4)    ! 13
    CALL CalcH("1-chloro-2-nitrobenzene",         "88-73-3", 3.8E-4)    ! 14
    CALL CalcH("1-chloro-3-nitrobenzene",        "121-73-3", 5.5E-4)    ! 15
    CALL CalcH("1-chloro-4-nitrobenzene",        "100-00-5", 2.0E-4)    ! 16
    CALL CalcH("1,4-dichloro-2-nitrobenzene",     "89-61-2", 4.9E-4)    ! 17
    CALL CalcH("1,2-dichloro-4-nitrobenzene",     "99-54-7", 3.3E-4)    ! 18
    CALL CalcH("1,3-dinitrobenzene",              "99-65-0", 2.0E-6)    ! 19
    CALL CalcH("1-methyl-2,4-dinitrobenzene",    "121-14-2", 2.2E-6)    ! 20
    CALL CalcH("2-nitroaniline",                  "88-74-4", 2.4E-6)    ! 21
    CALL CalcH("4-nitroaniline",                 "100-01-6", 4.7E-8)    ! 22
    CALL CalcH("2-chloro-4-nitroaniline",        "121-87-9", 3.9E-7)    ! 23
    CALL CalcH("1-nitronaphthalene",              "86-57-7", 7.2E-5)    ! 24
    CALL CalcH("aniline",                         "62-53-3", 7.8E-5)    ! 25
    CALL CalcH("2-methylaniline",                 "95-53-4", 8.1E-5)    ! 26
    CALL CalcH("3-methylaniline",                "108-44-1", 6.8E-5)    ! 27
    CALL CalcH("4-methylaniline",                "106-49-0", 3.1E-5)    ! 28
    CALL CalcH("n-ethylaniline",                 "103-69-5", 4.0E-4)    ! 29
    CALL CalcH("4-methoxyaniline",               "104-94-9", 2.7E-6)    ! 30
    CALL CalcH("3-chloroaniline",                "108-42-9", 4.1E-5)    ! 31
    CALL CalcH("alpha-hch",                      "319-84-6", 5.0E-4)    ! 32
    CALL CalcH("beta-hch",                       "319-85-7", 1.8E-5)    ! 33
    CALL CalcH("gamma-hch",                       "58-89-9", 2.1E-4)    ! 34
    CALL CalcH("p,p'-ddd",                        "72-54-8", 2.7E-4)    ! 35
    CALL CalcH("p,p'-dde",                        "72-55-9", 1.7E-3)    ! 36
    CALL CalcH("p,p'-ddt",                        "50-29-3", 3.4E-4)    ! 37
    CALL CalcH("dieldrin",                        "60-57-1", 4.1E-4)    ! 38
    CALL CalcH("endrin",                          "72-20-8", 2.6E-4)    ! 39
    CALL CalcH("aldrin",                         "309-00-2", 1.8E-3)    ! 40
    CALL CalcH("alpha-endosulfan",               "959-98-8", 2.9E-4)    ! 41
    CALL CalcH("beta-endosulfan",              "33213-65-9", 1.6E-4)    ! 42
    CALL CalcH("methoxychlor",                    "72-43-5", 8.3E-6)    ! 43
    CALL CalcH("heptachlor",                      "76-44-8", 1.2E-2)    ! 44
    CALL CalcH("heptachlorepoxid",              "1024-57-3", 8.6E-4)    ! 45
    CALL CalcH("hexachlorobenzene",              "118-74-1", 9.6E-3)    ! 46
    CALL CalcH("phenol",                         "108-95-2", 9.7E-5, 1) ! 47
    CALL CalcH("2-methylphenol",                  "95-48-7", 6.4E-5)    ! 48
    CALL CalcH("3-methylphenol",                 "108-39-4", 3.5E-5)    ! 49
    CALL CalcH("4-methylphenol",                 "106-44-5", 1.4E-4, 1) ! 50
    CALL CalcH("methanol",                        "67-56-1", 2.5E-4)    ! 51
    CALL CalcH("ethanol",                         "64-17-5", 3.0E-4)    ! 52
    CALL CalcH("1-propanol",                      "71-23-8", 1.5E-4)    ! 53
    CALL CalcH("2-propanol",                      "67-63-0", 4.4E-4)    ! 54
    CALL CalcH("1-butanol",                       "71-36-3", 3.5E-4)    ! 55
    CALL CalcH("2-methyl-1-propanol",             "78-83-1", 3.6E-4)    ! 56
    CALL CalcH("2-methyl-2-propanol",             "75-65-0", 3.7E-4)    ! 57
    CALL CalcH("1-hexanol",                      "111-27-3", 4.1E-4)    ! 58
    CALL CalcH("1-heptanol",                     "111-70-6", 4.7E-4)    ! 59
    CALL CalcH("1-octanol",                      "111-87-5", 6.2E-4)    ! 60
    CALL CalcH("1-decanol",                      "112-30-1", 1.3E-3)    ! 61
    CALL CalcH("1-dodecanol",                    "112-53-8", 9.1E-4)    ! 62
    CALL CalcH("cyclohexanol",                   "108-93-0", 1.8E-4)    ! 63
    CALL CalcH("2-methylcyclohexanol",           "583-59-5", 3.1E-4)    ! 64
    CALL CalcH("3-methylcyclohexanol",           "591-23-1", 1.5E-4)    ! 65
    CALL CalcH("cyclododecanol",                "1724-39-6", 1.2E-4)    ! 66
    CALL CalcH("3,7-dimethyl-1,6-octadien-3-ol",  "78-70-6", 8.8E-4)    ! 67
    CALL CalcH("2-methyl-3-buten-2-ol",          "115-18-4", 8.5E-4)    ! 68
    CALL CalcH("2-methyl-3-butyn-2-ol",          "115-19-5", 1.6E-4)    ! 69
    CALL CalcH("phenylmethanol",                 "100-51-6", 1.1E-5, 1) ! 70
    CALL CalcH("2-phenylethanol",                 "60-12-8", 1.1E-5, 1) ! 71
    CALL CalcH("3-phenyl-1-propanol",            "122-97-4", 2.3E-6, 1) ! 72
    CALL CalcH("4-phenyl-1-butanol",            "3360-41-6", 6.0E-5, 1) ! 73
    CALL CalcH("1,2-butanediol",                 "584-03-2", 1.2E-6, 1) ! 74
    CALL CalcH("1,4-butanediol",                 "110-63-4", 4.5E-7, 1) ! 75
    CALL CalcH("2-butene-1,4-diol",              "110-64-5", 1.2E-6, 1) ! 76
    CALL CalcH("1,4-dihydroxy-2-butyne",         "110-65-6", 2.0E-7, 1) ! 77
    CALL CalcH("1-propanamine",                  "107-10-8", 8.1E-4)    ! 78
    CALL CalcH("n-butylamine",                   "109-73-9", 7.2E-4)    ! 79
    CALL CalcH("di-n-butylamine",                "111-92-2", 4.0E-4)    ! 80
    CALL CalcH("tri-n-butylamine",               "102-82-9", 1.0E-3)    ! 81
    CALL CalcH("1-tridecanamine",               "2869-34-3", 4.5E-3)    ! 82
    CALL CalcH("cyclohexanamine",                "108-91-8", 1.7E-4)    ! 83
    CALL CalcH("n,n-dimethylcyclohexylamine",     "98-94-2", 9.6E-4)    ! 84
    CALL CalcH("1-naphthylamine",                "134-32-7", 2.5E-6)    ! 85
    CALL CalcH("n,n-dimethylaminododecane",      "112-18-5", 1.0E-4, 1) ! 86

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

  END SUBROUTINE ref1195

  !---------------------------------------------------------------------------

  SUBROUTINE ref1200 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1200"
    type = "M"

    chem = "methyl hydroperoxide" ; casrn = "3031-73-0" ! MHP
    CALL MakeNoteOtherTemp("278")
    CALL Output(1197.7*Hcp_TO_HcpSI)

    chem = "ethyl hydroperoxide" ; casrn = "3031-74-1" ! EHP
    CALL MakeNoteOtherTemp("278")
    CALL Output(1132.9*Hcp_TO_HcpSI)

    chem = "ethanoic peroxyacid" ; casrn = "79-21-0" ! PAA
    CALL MakeNoteOtherTemp("278")
    CALL Output(2394.2*Hcp_TO_HcpSI)

    chem = "methanoic peroxyacid" ; casrn = "107-32-4" ! PFA
    CALL MakeNoteOtherTemp("278")
    CALL Output(2894.5*Hcp_TO_HcpSI)

  END SUBROUTINE ref1200

  !---------------------------------------------------------------------------

  SUBROUTINE ref1217
    IMPLICIT NONE

    ref = "1217"
    type = "M"

    CALL CalcH("1-nitrooxy-2-butanol",  "147794-11-4",  9000.)
    CALL CalcH("2-nitrooxy-1-butanol",  "147794-12-5",  8900.)
    CALL CalcH("4-nitrooxy-2-butanol",  "_CAS-51",     13600.)
    CALL CalcH("3-nitrooxy-1-butanol",  "_CAS-52",     14000.)
    CALL CalcH("4-nitrooxy-1-butanol",  "_CAS-53",     29000.)
    CALL CalcH("5-nitrooxy-2-pentanol", "_CAS-54",     36700.)
    CALL CalcH("4-nitrooxy-1-pentanol", "_CAS-55",     20500.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hominus_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Hominus_

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = Hominus_ * Hcp_TO_HcpSI
      CALL MakeNoteOtherTemp("291")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1217

  !---------------------------------------------------------------------------

  SUBROUTINE ref1229 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "1229"
    type = "M"
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/10.,15.,20.,25.,30./) + CtoK

    CALL CalcH("benzene",        "71-43-2", (/ 2.86, 3.75, 4.54, 5.96, 7.31 /))
    CALL CalcH("methylbenzene", "108-88-3", (/ 2.89, 3.85, 4.92, 6.51, 8.27 /))
    CALL CalcH("ethylbenzene",  "100-41-4", (/ 3.02, 4.22, 5.75, 7.84, 10.3 /))
    CALL CalcH("propylbenzene", "103-65-1", (/ 4.35, 6.21, 8.37, 11.6, 15.3 /))
    CALL CalcH("butylbenzene",  "104-51-8", (/ 5.35, 8.17, 11.0, 16.7, 21.4 /))

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, KH_)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: KH_

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Harray = 1E3/(KH_*atm)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1229

  !---------------------------------------------------------------------------

  SUBROUTINE ref1230 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1230"
    type = "R"
    chem = "chlorine (molecular)" ; casrn = "7782-50-5" ! Cl2
    CALL Output(7.61E-2*Hcp_TO_HcpSI)

  END SUBROUTINE ref1230

  !---------------------------------------------------------------------------

  SUBROUTINE ref1257 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1257"
    type = "C"
    chem = "pernitric acid" ; casrn = "26404-66-0" ! HNO4
    CALL Output(1.4E4*Hcp_TO_HcpSI) ! Tab. 3

  END SUBROUTINE ref1257

  !---------------------------------------------------------------------------

  SUBROUTINE ref1323 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1323"
    type = "M"
    chem = "bromine chloride" ; casrn = "13863-41-7" ! BrCl
    CALL MakeNoteOtherTemp("275")
    CALL Output(6.3*Hcp_TO_HcpSI, limit="<")

  END SUBROUTINE ref1323

  !---------------------------------------------------------------------------

  SUBROUTINE ref1330 ! KHcc [1]
    IMPLICIT NONE
    REAL :: A, B, C

    ref = "1330"

    chem = "bromomethane" ; casrn = "74-83-9" ! CH3Br methyl bromide
    type = "?"
    CALL Output(KHcc_TIMES_HcpSI_atT0/0.24) ! Tab. 1

    chem = "{cis}-1,3-dichloropropene" ; casrn = "10061-01-5" ! C3H4Cl2
    type = "?"
    CALL Output(KHcc_TIMES_HcpSI_atT0/0.074) ! Tab. 1

    chem = "{trans}-1,3-dichloropropene" ; casrn = "10061-02-6" ! C3H4Cl2
    type = "?"
    CALL Output(KHcc_TIMES_HcpSI_atT0/0.043) ! Tab. 1

    chem = "3-bromo-propyne" ; casrn = "106-96-7" ! propargyl bromide C3H3Br
    type = "M"
    ! This reference defines H = -0.0154 + 636 exp(-2756/T) which
    ! includes a strange offset "-0.0154", meaning that H becomes
    ! negative at low temperature (about 259 K). Therefore, the
    ! temperature dependence used here is "delta(ln(H))/delta(1/T)" at T0:
    A = -0.0154
    B = 636.
    C = -2756.
    ! calculate analytical derivative (see also util/derivative.f90):
    mindHR = -B*C*EXP(C/T0)/(A+B*EXP(C/T0))
    mindHR = mindHR + T0 ! see Fig. 6 and see ref958, eqn (34) why T0 is added
    CALL Output(KHcc_TIMES_HcpSI_atT0/0.046,mindHR)

  END SUBROUTINE ref1330

  !---------------------------------------------------------------------------

  SUBROUTINE ref1331 ! KHcc [1]
    IMPLICIT NONE

    ref = "1331"
    type = "M"

    ! Data from Table 2.
    ! Data for solubility in CTAB and H3PO4 are not used here.
    ! Data at 20 C (and also at 25 C) are not used because they are already
    ! extrapolated, not measured.

    CALL CalcH("propanone",                    "67-64-1", &
      (/ 88.3, 59.0, 49.0, 39.5 /), (/ 25.5, 7.5, 4.6, 2.8 /) )
    CALL CalcH("2,6-dichlorobenzenenitrile", "1194-65-6", &
      (/ 88.3, 78.5, 68.7, 59.0 /), (/ 15.7, 12.7, 7.6, 4.7 /) )
    CALL CalcH("trichloroethene",              "79-01-6", &
      (/ 46.0, 41.5, 36.9 /),        (/ 1410., 1080., 880. /)  )
    CALL CalcH("methylbenzene",               "108-88-3", &
      (/ 46.0, 41.5, 36.9 /),        (/ 1050., 870., 660. /)   )

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
      temp = temp_ + CtoK
      Harray = KHcc_TO_HcpSI(1E-3*H,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1331

  !---------------------------------------------------------------------------

  SUBROUTINE ref1340 ! Hcp [M/atm]
    IMPLICIT NONE
    REAL :: DeltaH, DeltaS

    ref = "1340"
    type = "M"

    chem = "fluoromethane" ; casrn = "593-53-3" ! CH3F
    DeltaH = -4143. ! [cal/mol]
    DeltaS = -19.52 ! [cal/(mol*K)]
    CALL Output(EXP(-cal*(DeltaH/T0-DeltaS)/Rgas)*Hcp_TO_HcpSI,-cal*DeltaH/Rgas)

    chem = "chloromethane" ; casrn = "74-87-3" ! CH3Cl methyl chloride
    DeltaH = -5212. ! [cal/mol]
    DeltaS = -21.93 ! [cal/(mol*K)]
    CALL Output(EXP(-cal*(DeltaH/T0-DeltaS)/Rgas)*Hcp_TO_HcpSI,-cal*DeltaH/Rgas)

    chem = "bromomethane" ; casrn = "74-83-9" ! CH3Br methyl bromide
    DeltaH = -5175. ! [cal/mol]
    DeltaS = -21.04 ! [cal/(mol*K)]
    CALL Output(EXP(-cal*(DeltaH/T0-DeltaS)/Rgas)*Hcp_TO_HcpSI,-cal*DeltaH/Rgas)

    chem = "iodomethane" ; casrn = "74-88-4" ! CH3I
    DeltaH = -5916. ! [cal/mol]
    DeltaS = -23.25 ! [cal/(mol*K)]
    CALL Output(EXP(-cal*(DeltaH/T0-DeltaS)/Rgas)*Hcp_TO_HcpSI,-cal*DeltaH/Rgas)

  END SUBROUTINE ref1340

  !---------------------------------------------------------------------------

  SUBROUTINE ref1342 ! KHcc [1]
    IMPLICIT NONE

    ref = "1342"
    type = "M"

    CALL CalcH("chloromethane", "74-87-3", (/ 0., 22. /), (/ .16, .34 /) )
    CALL CalcH("bromomethane",  "74-83-9", (/ 0., 22. /), (/ .09, .21 /) )
    CALL CalcH("iodomethane",   "74-88-4", (/ 0., 22. /), (/ .07, .18 /) )

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
      temp = temp_ + CtoK
      Harray = KHcc_TO_HcpSI(H,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1342

  !---------------------------------------------------------------------------

  SUBROUTINE ref1442 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1442"

    chem = "methylperoxy radical" ; casrn = "2143-58-0"
    type = "E"
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" assume $\H(\chem{ROO}) = "// &
      "\H(\chem{ROOH}) \times \H(\chem{HO_2}) / "// &
      "\H(\chem{H_2O_2})$.")
    CALL Output(15.*Hcp_TO_HcpSI, 3700.)

    chem = "hydroxymethylperoxy radical" ; casrn = "27828-51-9"
    type = "E"
    CALL MakeNote(TRIM(ref))
    CALL Output(8.05E4*Hcp_TO_HcpSI, 8200.)

  END SUBROUTINE ref1442

  !---------------------------------------------------------------------------

  SUBROUTINE ref1481 ! KHpx [atm]
    IMPLICIT NONE

    ref = "1481"
    type = "M"
    chem = "hydrogen cyanide" ; casrn = "74-90-8" ! HCN hydrocyanic acid
    CALL MakeNoteOtherTemp("293")
    CALL Output(KHpx_TIMES_HcpSI/7.24)

  END SUBROUTINE ref1481

  !---------------------------------------------------------------------------

  SUBROUTINE ref1484 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "1484"
    type = "M"

    chem = "aminobenzene" ; casrn = "62-53-3" ! C6H7N aniline
    CALL MakeNoteOtherTemp("283")
    CALL Output(120.*Hcp_TO_HcpSI)

    chem = "hydroxybenzene" ; casrn = "108-95-2" ! C6H5OH phenol
    CALL MakeNoteOtherTemp("283")
    CALL Output(430.*Hcp_TO_HcpSI)

  END SUBROUTINE ref1484

  !---------------------------------------------------------------------------

  SUBROUTINE ref1492 ! Hcc [1]
    IMPLICIT NONE

    ref = "1492"
    type = "M"

    chem = "benzene" ; casrn = "71-43-2" ! C6H6

    CALL MakeNoteOtherTemp("293")
    CALL Output(Hcc_TO_HcpSI(3.,20.+CtoK))

    chem = "propanone" ; casrn = "67-64-1"
    CALL Output(Hcc_TO_HcpSI(620.,T0))
    CALL Output(Hcc_TO_HcpSI(800.,T0))

    chem = "2-pentanone" ; casrn = "107-87-9" ! C3H7COCH3
    CALL Output(Hcc_TO_HcpSI(430.,T0))
    CALL MakeNoteOtherTemp("313")
    CALL Output(Hcc_TO_HcpSI(281.,40.+CtoK))

    chem = "2-butanone" ; casrn = "78-93-3"
    CALL Output(Hcc_TO_HcpSI(280.,T0))

    chem = "ethanal" ; casrn = "75-07-0" ! CH3CHO acetaldehyde
    CALL MakeNoteOtherTemp("283")
    CALL Output(Hcc_TO_HcpSI(600.,10.+CtoK))

  END SUBROUTINE ref1492

  !---------------------------------------------------------------------------

  SUBROUTINE ref1497 ! KHpx [atm]
    IMPLICIT NONE

    ref = "1497"

    ! Tables 2, 3:
    CALL CalcH("nonane",          "111-84-2", "V",   7.42)
    CALL CalcH("decane",          "124-18-5", "V",   7.44)
    CALL CalcH("undecane",       "1120-21-4", "V",   8.25) ! C11H24
    CALL CalcH("dodecane",        "112-40-3", "V",   7.72) ! C12H26
    CALL CalcH("tetradecane",     "629-59-4", "V",   6.64) ! C14H30
    CALL CalcH("hexadecane",      "544-76-3", "V",   5.88) ! C16H34
    CALL CalcH("octadecane",      "593-45-3", "V",   3.88) ! C18H38
    CALL CalcH("eicosane",        "112-95-8", "V",   2.19) ! C20H42
    CALL CalcH("hexacosane",      "630-01-3", "V",  -3.25) ! C26H54
    CALL CalcH("hexatriacontane", "630-06-8", "V", -12.55) ! C36H74

    ! Tables 9, 10:
    CALL CalcH("benzene",          "71-43-2", "V", 3.39,  -7.59) ! C6H6
    CALL CalcH("methylbenzene",   "108-88-3", "V", 3.48,  -8.67) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",    "100-41-4", "V", 3.61,  -9.62) ! C6H5C2H5
    CALL CalcH("propylbenzene",   "103-65-1", "V", 3.75, -10.50) ! C6H5C3H7
    CALL CalcH("butylbenzene",    "104-51-8", "V", 3.91)         ! C6H5C4H9
    CALL CalcH("pentylbenzene",   "538-68-1", "V", 4.04)         ! C6H5C5H{11}
    CALL CalcH("hexylbenzene",   "1077-16-3", "V", 4.21)         ! C6H5C6H{13}

    ! Table 12:
    chem = "methylamine";   casrn = "74-89-5"; type = "?"
    CALL MakeNote("whichref")
    CALL Output(DUMMY, 10.82E3*cal / Rgas)

    chem = "ethylamine";    casrn = "75-04-7"; type = "?"
    CALL MakeNote("whichref")
    CALL Output(DUMMY, 12.83E3*cal / Rgas)

    chem = "1-propylamine"; casrn = "107-10-8"; type = "?"
    CALL MakeNote("whichref")
    CALL Output(DUMMY, 13.38E3*cal / Rgas)

    chem = "1-butylamine";  casrn = "109-73-9"; type = "?"
    CALL MakeNote("whichref")
    CALL Output(DUMMY, 14.11E3*cal / Rgas)

    chem = "1-pentylamine"; casrn = "110-58-7"; type = "?"
    CALL MakeNote("whichref")
    CALL Output(DUMMY, 14.85E3*cal / Rgas)

    chem = "1-hexylamine";  casrn = "111-26-2"; type = "?"
    CALL MakeNote("whichref")
    CALL Output(DUMMY, 15.72E3*cal / Rgas)

    ! Table 13:
    CALL CalcH("2-decanone", "693-54-9", "V", 1.92)

    ! Table 14:
    chem = "2-nonanone"; casrn = "821-55-6"; type = "V"
    CALL Output(DUMMY, 15.01E3*cal / Rgas)

    ! Table 16:
    CALL CalcH("1-chloropropane", "540-54-5", "V", 3.94) ! C3H7Cl
    CALL CalcH("1-chlorobutane",  "109-69-3", "V", 4.11) ! C4H9Cl
    CALL CalcH("1-chloropentane", "543-59-9", "V", 4.21) ! C5H{11}Cl
    CALL CalcH("1-chloroheptane", "629-06-1", "V", 4.56) ! C7H15Cl
    CALL CalcH("bromoethane",      "74-96-4", "V", 3.54) ! C2H5Br
    CALL CalcH("1-bromopropane",  "106-94-5", "V", 3.71) ! C3H7Br
    CALL CalcH("1-bromobutane",   "109-65-9", "V", 3.87) ! C4H9Br
    CALL CalcH("1-bromopentane",  "110-53-2", "V", 4.18) ! C5H{11}Br
    CALL CalcH("1-bromohexane",   "111-25-1", "V", 4.45) ! C6H13Br
    CALL CalcH("1-bromoheptane",  "629-04-9", "V", 4.61) ! C7H15Br
    CALL CalcH("1-bromooctane",   "111-83-1", "V", 4.79) ! C8H17Br
    CALL CalcH("iodomethane",      "74-88-4", "V", 3.37) ! CH3I
    CALL CalcH("iodoethane",       "75-03-6", "V", 3.54) ! C2H5I
    CALL CalcH("1-iodopropane",   "107-08-4", "V", 3.74) ! C3H7I
    CALL CalcH("1-iodobutane",    "542-69-8", "V", 4.03) ! C4H9I
    CALL CalcH("1-iodoheptane",  "4282-40-0", "V", 4.54) ! C7H15I

    ! Tables 18 (mindHR from Table 20):
    CALL CalcH("1-heptanol",     "111-70-6", "V", 0.06, -17.24) ! C7H{15}OH
    CALL CalcH("1-nonanol",      "143-08-8", "V", 0.39)         ! C9H{19}OH
    CALL CalcH("1-decanol",      "112-30-1", "V", 0.63)         ! C{10}H{21}OH
    CALL CalcH("1-dodecanol",    "112-53-8", "V", 0.81, -19.38) ! C{12}H{25}OH
    CALL CalcH("1-tetradecanol", "112-72-1", "R", 0.54)         ! C{14}H{29}OH
    CALL CalcH("1-pentadecanol", "629-76-5", "V", 0.53)         ! C{15}H{31}OH
    CALL CalcH("1-hexadecanol",  "124-29-8", "R", 0.27)         ! C{16}H{33}OH
    CALL CalcH("1-octadecanol",  "112-92-5", "R", 0.21)         ! C{18}H{37}OH

    ! Table 20:
    chem = "methanol" ; casrn = "67-56-1" ; type = "V" ! CH3OH
    CALL Output(DUMMY, 10.69E3*cal / Rgas)
    chem = "ethanol" ; casrn = "64-17-5" ; type = "V" ! C2H5OH
    CALL Output(DUMMY, 12.53E3*cal / Rgas)
    chem = "1-propanol" ; casrn = "71-23-8" ; type = "V" ! C3H7OH
    CALL Output(DUMMY, 13.77E3*cal / Rgas)
    chem = "1-butanol" ; casrn = "71-36-3" ; type = "V" ! C4H9OH
    CALL Output(DUMMY, 14.73E3*cal / Rgas)
    chem = "1-pentanol" ; casrn = "71-41-0" ; type = "V" ! C5H{11}OH amylalcohol
    CALL Output(DUMMY, 15.46E3*cal / Rgas)
    chem = "1-hexanol" ; casrn = "111-27-3" ; type = "V" ! C6H{13}OH
    CALL Output(DUMMY, 16.30E3*cal / Rgas)
    ! 1-heptanol: see above
    chem = "1-octanol" ; casrn = "111-87-5" ; type = "V" ! C8H{17}OH
    CALL Output(DUMMY, 17.72E3*cal / Rgas)
    ! 1-dodecanol: see above

    ! Table 21:
    CALL CalcH("methyl nonanoate", "1731-84-6", "V", 2.58)
    CALL CalcH("methyl decanoate",  "110-42-9", "V", 3.05)

    ! Table 23:
    CALL CalcH("ethyl methanoate", "109-94-4", "V", 1.70) ! HCOOC2H5
    CALL CalcH("ethyl propanoate", "105-37-3", "V", 1.59) ! C2H5COOC2H5
    CALL CalcH("ethyl butanoate",  "105-54-4", "V", 1.77) ! C3H7COOC2H5
    CALL CalcH("ethyl pentanoate", "539-82-2", "V", 1.77) ! C4H9COOC2H5
    CALL CalcH("ethyl hexanoate",  "123-66-0", "V", 2.03) ! C5H{11}COOC2H5
    CALL CalcH("ethyl heptanoate", "106-30-9", "V", 1.95) ! C6H{13}COOC2H5
    CALL CalcH("ethyl octanoate",  "106-32-1", "V", 2.26) ! C7H{15}COOC2H5
    CALL CalcH("ethyl nonanoate",  "123-29-5", "V", 2.23) ! C8H{17}COOC2H5
    CALL CalcH("ethyl decanoate",  "110-38-3", "V", 2.04) ! C9H{19}COOC2H5

    ! Table 24:
    CALL CalcH("propyl propanoate", "106-36-5", "V", 1.83) ! C2H5COOC3H7
    CALL CalcH("pentyl propanoate", "624-54-4", "V", 2.16) ! C2H5COOC5H{11}

    ! Table 26:
    chem = "methanoic acid" ; casrn =  "64-18-6" ; type = "V" ! HCOOH formic acid
    CALL Output(DUMMY, 11.3E3*cal / Rgas)
    chem = "ethanoic acid" ;  casrn =  "64-19-7" ; type = "V" ! CH3COOH acetic acid
    CALL Output(DUMMY, 12.6E3*cal / Rgas)
    chem = "propanoic acid" ; casrn =  "79-09-4" ; type = "V" ! C2H5COOH propionic acid
    CALL Output(DUMMY, 13.5E3*cal / Rgas)
    chem = "butanoic acid" ;  casrn = "107-92-6" ; type = "V" ! C3H7COOH butyric acid
    CALL Output(DUMMY, 14.2E3*cal / Rgas)
    chem = "pentanoic acid" ; casrn = "109-52-4" ; type = "V" ! C4H9COOH
    CALL Output(DUMMY, 15.0E3*cal / Rgas)
    chem = "hexanoic acid" ;  casrn = "142-62-1" ; type = "V" ! C5H{11}COOH caproic acid
    CALL Output(DUMMY, 17.3E3*cal / Rgas)
    chem = "heptanoic acid" ; casrn = "111-14-8" ; type = "V" ! C7H{14}O2
    CALL Output(DUMMY, 16.8E3*cal / Rgas)
    chem = "octanoic acid" ;  casrn = "124-07-2" ; type = "V" ! C8H{16}O2 caprylic acid
    CALL Output(DUMMY, 19.1E3*cal / Rgas)

    chem = "methanoic acid" ; casrn =  "64-18-6" ; type = "R" ! HCOOH formic acid
    CALL MakeNote("1497smoothed", &
      TRIM(citet())//" smoothed the values from a plot "// &
      "of enthalpy against carbon number.")
    CALL Output(DUMMY, 11.09E3*cal / Rgas)
    chem = "ethanoic acid" ;  casrn =  "64-19-7" ; type = "R" ! CH3COOH acetic acid
    CALL MakeNote("1497smoothed")
    CALL Output(DUMMY, 12.28E3*cal / Rgas)
    chem = "propanoic acid" ; casrn =  "79-09-4" ; type = "R" ! C2H5COOH propionic acid
    CALL MakeNote("1497smoothed")
    CALL Output(DUMMY, 13.45E3*cal / Rgas)
    chem = "butanoic acid" ;  casrn = "107-92-6" ; type = "R" ! C3H7COOH butyric acid
    CALL MakeNote("1497smoothed")
    CALL Output(DUMMY, 14.50E3*cal / Rgas)
    chem = "pentanoic acid" ; casrn = "109-52-4" ; type = "R" ! C4H9COOH
    CALL MakeNote("1497smoothed")
    CALL Output(DUMMY, 15.32E3*cal / Rgas)
    chem = "hexanoic acid" ;  casrn = "142-62-1" ; type = "R" ! C5H{11}COOH caproic acid
    CALL MakeNote("1497smoothed")
    CALL Output(DUMMY, 16.08E3*cal / Rgas)
    chem = "heptanoic acid" ; casrn = "111-14-8" ; type = "R" ! C7H{14}O2
    CALL MakeNote("1497smoothed")
    CALL Output(DUMMY, 16.96E3*cal / Rgas)
    chem = "octanoic acid" ;  casrn = "124-07-2" ; type = "R" ! C8H{16}O2 caprylic acid
    CALL MakeNote("1497smoothed")
    CALL Output(DUMMY, 17.75E3*cal / Rgas)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, DeltaGs, DeltaHs)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: DeltaGs ! [kcal/mol]
      REAL, OPTIONAL,   INTENT(IN) :: DeltaHs ! [kcal/mol]

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = type_   ! make value global, so Output will find it
      Hominus = KHpx_TIMES_HcpSI/EXP(DeltaGs*1E3*cal/(Rgas*T0))
      IF (PRESENT(DeltaHs)) THEN
        mindHR = - DeltaHs*1E3*cal / Rgas
        CALL Output(Hominus, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref1497

  !---------------------------------------------------------------------------

  SUBROUTINE ref1498 ! KHpx [atm]
    IMPLICIT NONE

    ref = "1498"
    type = "L"

    CALL CalcH("helium",  "7440-59-7", -32.159320,   188439.5775,  -9092.5831, 217.2949, 0.02073005 )
    CALL CalcH("neon",    "7440-01-9", -31.319000,   100087.1196,  -8655.5190, 211.9653, 0.02046359 )
    CALL CalcH("argon",   "7440-37-1", -27.640136,  -346288.6764,  -5628.4350, 184.9362, 0.01981807 )
    CALL CalcH("krypton", "7439-90-9", -25.486258,  -595763.1239,  -3763.1116, 168.7724, 0.01933865 )
    CALL CalcH("xenon",   "7440-63-3", -22.881360,  -895575.7251,  -1477.2317, 149.2465, 0.01877962 )
    CALL CalcH("radon",  "10043-92-2", -20.699446, -1147624.8910,    266.3758, 133.1968, 0.01829329 )
    CALL CalcH("methane",   "74-82-8", -17.445182,  -747802.5725,   -830.8624, 118.0172, 0.01058042 )
    CALL CalcH("ethane",    "74-84-0", -39.404916,        0.,     -13024.7473, 274.8992, 0.01202726 )
    CALL CalcH("propane",   "74-98-6", -47.859600,        0.,     -15724.0149, 331.4318, 0.01512000 )
    CALL CalcH("butane",   "106-97-8", -48.021800,        0.,     -16649.9444, 337.4615, 0.00893454 )
    CALL CalcH("pentane",  "109-66-0", -46.305000,        0.,     -17209.5507, 332.6820, 0.         )
    CALL CalcH("hexane",   "110-54-3", -54.122700,        0.,     -19985.4066, 386.8057, 0.         )
    CALL CalcH("heptane",  "142-82-5", -57.730854,        0.,     -21301.7220, 412.1001, 0.         )
    CALL CalcH("octane",   "111-65-9", -61.339000,        0.,     -22618.0369, 437.4121, 0.         )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, L, A, B, C, D)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: L, A, B, C, D
      REAL :: KHpx0
      REAL :: dlnHd1T ! d ln(H) / d (1/T)

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! calculation of d ln(1/x) / dx :
      ! outer: 1 / (1/x) = x
      ! inner: -1/x^2
      ! outer*inner = - 1/x
      ! ln(H) = L ln(T) + A/T^2 + B/T + C + DT
      ! substitute x:= 1/T
      ! ln(H) = L ln(1/x) + A x^2 + B x + C + D/x
      ! d ln(H) / d (1/T) = d ln(H) / dx = -L/x + 2 Ax + B - D/x^2
      !                                    = -LT + 2 A/T + B - DT^2
      ! see also ref1498.gnu

      KHpx0 = EXP(L*LOG(T0) + A/T0**2 + B/T0 + C + D*T0)
      dlnHd1T = -L*T0 + 2.*A/T0 + B - D*T0**2
      CALL Output(KHpx_TIMES_HcpSI/KHpx0, -dlnHd1T)
    END SUBROUTINE CalcH

  END SUBROUTINE ref1498

  !---------------------------------------------------------------------------

  SUBROUTINE ref1500 ! Hcc [1]
    IMPLICIT NONE

    ref = "1500"
    type = "?"

    ! page 292:
    CALL CalcH("helium",                                              "7440-59-7", -2.023 ) ! He
    CALL CalcH("neon",                                                "7440-01-9", -1.958 ) ! Ne
    CALL CalcH("argon",                                               "7440-37-1", -1.467 ) ! Ar
    CALL CalcH("krypton",                                             "7439-90-9", -1.213 ) ! Kr
    CALL CalcH("xenon",                                               "7440-63-3", -0.972 ) ! Xe
    CALL CalcH("radon",                                              "10043-92-2", -0.646 ) ! Rn
    CALL CalcH("methane",                                               "74-82-8", -1.452 ) ! CH4
    CALL CalcH("ethane",                                                "74-84-0", -1.336 ) ! C2H6
    CALL CalcH("propane",                                               "74-98-6", -1.436 ) ! C3H8
    CALL CalcH("butane",                                               "106-97-8", -1.518 ) ! C4H{10}
    CALL CalcH("2-methylpropane",                                       "75-28-5", -1.70  ) ! HC(CH3)3 isobutane
    CALL CalcH("pentane",                                              "109-66-0", -1.704 ) ! C5H{12}
    CALL CalcH("hexane",                                               "110-54-3", -1.821 ) ! C6H{14}
    CALL CalcH("heptane",                                              "142-82-5", -1.962 ) ! C7H{16}
    CALL CalcH("octane",                                               "111-65-9", -2.109 ) ! C8H{18}
    CALL CalcH("cyclopropane",                                          "75-19-4", -0.55  ) ! C3H6
    CALL CalcH("cyclopentane",                                         "287-92-3", -0.88  ) ! C5H{10}
    CALL CalcH("cyclohexane",                                          "110-82-7", -0.90  ) ! C6H{12}
    CALL CalcH("dimethylpropane",                                      "463-82-1", -1.84  ) ! C(CH3)4 neopentane
    CALL CalcH("3,3-diethylpentane",                                  "1067-20-5", -1.63  ) ! C9H{20}
    CALL CalcH("tetramethylsilane",                                     "75-76-3", -2.23  ) ! C4H{12}Si !NEW
    CALL CalcH("tetraethylsilane",                                     "631-36-7", -2.03  ) ! C8H{20}Si !NEW
    CALL CalcH("tetramethylstannane",                                  "594-27-4", -1.62  ) ! C4H{12}Sn tetramethyltin
    CALL CalcH("tetraethylstannane",                                   "597-64-8", -1.82  ) ! C8H{20}Sn tetraethyltin
    CALL CalcH("ethene",                                                "74-85-1", -0.94  ) ! C2H4 ethylene
    CALL CalcH("propene",                                              "115-07-1", -0.97  ) ! C3H6 propylene
    CALL CalcH("1-butene",                                             "106-98-9", -1.01  ) ! C4H8
    CALL CalcH("1-pentene",                                            "109-67-1", -1.23  ) ! C5H{10}
    CALL CalcH("1-hexene",                                             "592-41-6", -1.16  ) ! C6H{12}
    CALL CalcH("1-heptene",                                            "592-76-7", -1.22  ) ! C7H{14}
    CALL CalcH("1-octene",                                             "111-66-0", -1.41  ) ! C8H{16}
    CALL CalcH("1-nonene",                                             "124-11-8", -1.51  ) ! C9H{18}
    CALL CalcH("ethyne",                                                "74-86-2",  0.01  ) ! C2H2 acetylene
    CALL CalcH("propyne",                                               "74-99-7",  0.35  ) ! CH3CCH
    CALL CalcH("1-butyne",                                             "107-00-6",  0.12  ) ! C2H5CCH ethylacetylene
    CALL CalcH("1-pentyne",                                            "627-19-0", -0.01  ) ! C3H7CCH
    CALL CalcH("1-hexyne",                                             "693-02-7", -0.21  ) ! C4H9CCH
    CALL CalcH("1-heptyne",                                            "628-71-7", -0.44  ) ! C5H{11}CCH
    CALL CalcH("1-octyne",                                             "629-05-0", -0.52  ) ! C6H{13}CCH
    CALL CalcH("1-nonyne",                                            "3452-09-3", -0.77  ) ! C7H{15}CCH
    CALL CalcH("benzene",                                               "71-43-2",  0.65  ) ! C6H6
    CALL CalcH("methylbenzene",                                        "108-88-3",  0.58  ) ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",                                         "100-41-4",  0.48  ) ! C6H5C2H5
    CALL CalcH("propylbenzene",                                        "103-65-1",  0.38  ) ! C6H5C3H7
    CALL CalcH("butylbenzene",                                         "104-51-8",  0.27  ) ! C6H5C4H9
    CALL CalcH("pentylbenzene",                                        "538-68-1",  0.17  ) ! C6H5C5H{11}
    CALL CalcH("hexylbenzene",                                        "1077-16-3",  0.03  ) ! C6H5C6H{13}
    CALL CalcH("1,2-dimethylbenzene",                                   "95-47-6",  0.67  ) ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",                                  "108-38-3",  0.52  ) ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",                                  "106-42-3",  0.54  ) ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("1,2,3-trimethylbenzene",                               "526-73-8",  0.71  ) ! C6H3(CH3)3
    CALL CalcH("1,2,4-trimethylbenzene",                                "95-63-6",  0.59  ) ! C6H3(CH3)3
    CALL CalcH("1,3,5-trimethylbenzene",                               "108-67-8",  0.50  ) ! C6H3(CH3)3 mesitylene
    CALL CalcH("(2-propyl)-benzene",                                    "98-82-8",  0.34  ) ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("naphthalene",                                           "91-20-3",  1.76  ) ! C{10}H8
    CALL CalcH("2,3-benzindene",                                        "86-73-7",  2.46  ) ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",                                          "85-01-8",  2.83  ) ! C{14}H{10}
    CALL CalcH("pyrene",                                               "129-00-0",  3.35  ) ! C{16}H{10}
    CALL CalcH("propanone",                                             "67-64-1",  2.79  ) ! CH3COCH3 acetone
    CALL CalcH("2-butanone",                                            "78-93-3",  2.72  ) ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("2-pentanone",                                          "107-87-9",  2.58  ) ! C3H7COCH3
    CALL CalcH("2-hexanone",                                           "591-78-6",  2.41  ) ! C6H{12}O
    CALL CalcH("2-heptanone",                                          "110-43-0",  2.23  ) ! C5H{11}COCH3
    CALL CalcH("2-octanone",                                           "111-13-7",  2.11  ) ! C6H{13}COCH3
    CALL CalcH("2-nonanone",                                           "821-55-6",  1.83  ) ! C7H{15}COCH3
    CALL CalcH("2-decanone",                                           "693-54-9",  1.72  ) ! C8H{17}COCH3
    CALL CalcH("2-undecanone",                                         "112-12-9",  1.58  ) ! C9H{19}COCH3
    CALL CalcH("3-pentanone",                                           "96-22-0",  2.50  ) ! C2H5COC2H5
    ! page 293:
    CALL CalcH("4-Heptanone",                                          "123-19-3",  2.14  ) ! C7H{14}O !NEW
    CALL CalcH("5-nonanone",                                           "502-56-7",  1.94  ) ! C9H{18}O dibutyl ketone
    CALL CalcH("3-methyl-2-butanone",                                  "563-80-4",  2.38  ) ! C5H{10}O isopropyl methyl ketone
    CALL CalcH("4-methyl-2-pentanone",                                 "108-10-1",  2.24  ) ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("cyclopentanone",                                       "120-92-3",  3.45  ) ! C5H8O !NEW
    CALL CalcH("cyclohexanone",                                        "108-94-1",  3.60  ) ! C6H{10}O
    CALL CalcH("1-phenylethanone",                                      "98-86-2",  3.36  ) ! C6H5COCH3 acetophenone
    CALL CalcH("methanal",                                              "50-00-0",  2.02  ) ! HCHO formaldehyde
    CALL CalcH("ethanal",                                               "75-07-0",  2.57  ) ! CH3CHO acetaldehyde
    CALL CalcH("propanal",                                             "123-38-6",  2.52  ) ! C2H5CHO propionaldehyde
    CALL CalcH("butanal",                                              "123-72-8",  2.33  ) ! C3H7CHO butyraldehyde
    CALL CalcH("pentanal",                                             "110-62-3",  2.22  ) ! C4H{9}CHO valeraldehyde
    CALL CalcH("hexanal",                                               "66-25-1",  2.06  ) ! C5H{11}CHO
    CALL CalcH("heptanal",                                             "111-71-7",  1.96  ) ! C6H{13}CHO
    CALL CalcH("octanal",                                              "124-13-0",  1.68  ) ! C7H{15}CHO
    CALL CalcH("nonanal",                                              "124-19-6",  1.52  ) ! C8H{17}CHO
    CALL CalcH("2-methylpropanal",                                      "78-84-2",  2.10  ) ! C4H8O
    CALL CalcH("benzaldehyde",                                         "100-52-7",  2.95  ) ! C6H5CHO
    CALL CalcH("{trans}-2-butenal",                                    "123-73-9",  3.10  ) ! CH3CHCHCHO crotonaldehyde
    CALL CalcH("dimethyl ether",                                       "115-10-6",  1.39  ) ! CH3OCH3
    CALL CalcH("diethyl ether",                                         "60-29-7",  1.17  ) ! C2H5OC2H5
    CALL CalcH("dipropyl ether",                                       "111-43-3",  0.85  ) ! C3H7OC3H7
    CALL CalcH("diisopropyl ether",                                    "108-20-3",  0.39  ) ! C3H7OC3H7
    CALL CalcH("dibutyl ether",                                        "142-96-1",  0.61  ) ! C4H9OC4H9
    CALL CalcH("tetrahydrofuran",                                      "109-99-9",  2.55  ) ! C4H8O THF
    CALL CalcH("tetrahydropyran",                                      "142-68-7",  2.29  ) ! C5H{10}O THP
    CALL CalcH("methoxybenzene",                                       "100-66-3",  1.80  ) ! C6H5OCH3 anisole
    CALL CalcH("ethoxybenzene",                                        "103-73-1",  1.63  ) ! C8H{10}O phenetole !NEW
    CALL CalcH("methyl methanoate",                                    "107-31-3",  2.04  ) ! HCOOCH3 methyl formate
    CALL CalcH("methyl ethanoate",                                      "79-20-9",  2.30  ) ! CH3COOCH3 methyl acetate
    CALL CalcH("methyl propanoate",                                    "554-12-1",  2.15  ) ! C2H5COOCH3 methyl propionate
    CALL CalcH("methyl butanoate",                                     "623-42-7",  2.08  ) ! C3H7COOCH3 methyl butyrate
    CALL CalcH("methyl pentanoate",                                    "624-24-8",  1.88  ) ! C4H9COOCH3
    CALL CalcH("methyl hexanoate",                                     "106-70-7",  1.83  ) ! C5H{11}COOCH3
    CALL CalcH("ethyl methanoate",                                     "109-94-4",  1.88  ) ! HCOOC2H5 ethyl formate
    CALL CalcH("ethyl ethanoate",                                      "141-78-6",  2.16  ) ! CH3COOC2H5 ethyl acetate
    CALL CalcH("ethyl propanoate",                                     "105-37-3",  1.97  ) ! C2H5COOC2H5 ethyl propionate
    CALL CalcH("ethyl butanoate",                                      "105-54-4",  1.83  ) ! C3H7COOC2H5 ethyl butyrate
    CALL CalcH("ethyl pentanoate",                                     "539-82-2",  1.83  ) ! C4H9COOC2H5
    CALL CalcH("ethyl hexanoate",                                      "123-66-0",  1.64  ) ! C5H{11}COOC2H5
    CALL CalcH("ethyl heptanoate",                                     "106-30-9",  1.70  ) ! C6H{13}COOC2H5
    CALL CalcH("propyl methanoate",                                    "110-74-7",  1.82  ) ! HCOOC3H7 propyl formate
    CALL CalcH("propyl ethanoate",                                     "109-60-4",  2.05  ) ! CH3COOC3H7 propyl acetate
    CALL CalcH("propyl propanoate",                                    "106-36-5",  1.79  ) ! C2H5COOC3H7 propyl propionate
    CALL CalcH("propyl butanoate",                                     "105-66-8",  1.67  ) ! C3H7COOC3H7 propyl butyrate
    CALL CalcH("butyl ethanoate",                                      "123-86-4",  1.94  ) ! CH3COOC4H9 butyl acetate
    CALL CalcH("pentyl ethanoate",                                     "628-63-7",  1.84  ) ! CH3COOC5H{11} amyl acetate
    CALL CalcH("pentyl propanoate",                                    "624-54-4",  1.55  ) ! C2H5COOC5H{11} amyl propionate
    CALL CalcH("hexyl ethanoate",                                      "142-92-7",  1.66  ) ! CH3COOC6H{13} hexyl acetate
    CALL CalcH("isopropyl methanoate",                                 "625-55-8",  1.48  ) ! HCOOC3H7 isopropyl formate
    CALL CalcH("isopropyl ethanoate",                                  "108-21-4",  1.94  ) ! CH3COOC3H7 isopropyl acetate
    CALL CalcH("isopropyl propanoate",                                 "637-78-5",  1.63  ) ! C2H5COOC3H7 isopropyl propionate
    CALL CalcH("(2-methylpropyl)-methanoate",                          "542-55-2",  1.63  ) ! HCOOC4H9 isobutyl formate
    CALL CalcH("(2-methylpropyl)-ethanoate",                           "110-19-0",  1.73  ) ! CH3COOC4H9 isobutyl acetate
    CALL CalcH("isopentyl methanoate",                                 "110-45-2",  1.56  ) ! HCOOC5H{11} isoamyl formate
    CALL CalcH("isopentyl ethanoate",                                  "123-92-2",  1.62  ) ! CH3COOC5H{11} isoamyl acetate
    CALL CalcH("2-methylpropyl 2-methylpropanoate",                     "97-85-8",  1.24  ) ! C8H{16}O2 isobutyl isobutyrate !NEW
    CALL CalcH("methyl benzoate",                                       "93-58-3",  3.14  ) ! C6H5COOCH3
    CALL CalcH("ethyl benzoate",                                        "93-89-0",  2.67  ) ! C6H5COOC2H5
    CALL CalcH("4-methyl-1,3-dioxolan-2-one",                          "108-32-7",  5.54  ) ! C4H6O3 propylene carbonate !NEW
    CALL CalcH("methanol",                                              "67-56-1",  3.74  ) ! CH3OH
    CALL CalcH("ethanol",                                               "64-17-5",  3.67  ) ! C2H5OH
    CALL CalcH("1-propanol",                                            "71-23-8",  3.56  ) ! C3H7OH
    CALL CalcH("1-butanol",                                             "71-36-3",  3.46  ) ! C4H9OH
    CALL CalcH("1-pentanol",                                            "71-41-0",  3.35  ) ! C5H{11}OH amylalcohol
    CALL CalcH("1-hexanol",                                            "111-27-3",  3.23  ) ! C6H{13}OH
    CALL CalcH("1-heptanol",                                           "111-70-6",  3.09  ) ! C7H{15}OH
    ! page 294:
    CALL CalcH("1-octanol",                                            "111-87-5",  3.00  ) ! C8H{17}OH
    CALL CalcH("1-nonanol",                                            "143-08-8",  2.85  ) ! C9H{19}OH
    CALL CalcH("1-decanol",                                            "112-30-1",  2.67  ) ! C{10}H{21}OH
    CALL CalcH("2-propanol",                                            "67-63-0",  3.48  ) ! C3H7OH isopropanol
    CALL CalcH("2-butanol",                                             "78-92-2",  3.39  ) ! C4H{10}O {sec}-butanol
    CALL CalcH("2-methyl-1-propanol",                                   "78-83-1",  3.30  ) ! C4H{10}O isobutanol
    CALL CalcH("2-methyl-2-propanol",                                   "75-65-0",  3.28  ) ! C4H{10}O {tert}-butanol
    CALL CalcH("2-pentanol",                                          "6032-29-7",  3.22  ) ! C5H{12}O {sec}-pentanol
    CALL CalcH("2-methyl-1-butanol",                                   "137-32-6",  3.24  ) ! C5H{12}O isopentanol
    CALL CalcH("2-methyl-2-butanol",                                    "75-85-4",  3.25  ) ! C5H{12}O {tert}-pentanol
    CALL CalcH("3-pentanol",                                           "584-02-1",  3.19  ) ! C5H{12}O !NEW
    CALL CalcH("3-methyl-1-butanol",                                   "123-51-3",  3.24  ) ! C5H{12}O !NEW
    CALL CalcH("3-hexanol",                                            "623-37-0",  2.98  ) ! C6H{14}O
    CALL CalcH("4-methyl-2-pentanol",                                  "108-11-2",  2.74  ) ! C6H{14}O
    CALL CalcH("2-methyl-2-pentanol",                                  "590-36-3",  2.88  ) ! C6H{14}O
    CALL CalcH("2-methyl-3-pentanol",                                  "565-67-3",  2.85  ) ! C6H{14}O
    CALL CalcH("cyclopentanol",                                         "96-41-3",  4.03  ) ! C5H{10}O !NEW
    CALL CalcH("cyclohexanol",                                         "108-93-0",  4.01  ) ! C6H{11}OH
    CALL CalcH("cycloheptanol",                                        "502-41-0",  4.02  ) ! C7H{14}O !NEW
    CALL CalcH("2-propen-1-ol",                                        "107-18-6",  3.69  ) ! C3H5OH allyl alcohol
    CALL CalcH("2,2,2-trifluoroethanol",                                "75-89-8",  3.15  ) ! CF3CH2OH
    CALL CalcH("1,1,1,3,3,3-hexafluoro-2-propanol",                    "920-66-1",  2.76  ) ! CF3CHOHCF3
    CALL CalcH("(hydroxymethyl)-benzene",                              "100-51-6",  4.65  ) ! C6H5CH2OH benzyl alcohol
    CALL CalcH("hydroxybenzene",                                       "108-95-2",  4.60  ) ! C6H5OH phenol
    CALL CalcH("1-hydroxy-2-methylbenzene",                             "95-48-7",  4.30  ) ! HOC6H4CH3 2-cresol; $o$-cresol
    CALL CalcH("1-hydroxy-3-methylbenzene",                            "108-39-4",  4.03  ) ! HOC6H4CH3 3-cresol; $m$-cresol
    CALL CalcH("1-hydroxy-4-methylbenzene",                            "106-44-5",  4.50  ) ! HOC6H4CH3 4-cresol; $p$-cresol
    CALL CalcH("4-{tert}-butylphenol",                                  "98-54-4",  4.34  ) ! (CH3)3CC6H4OH
    CALL CalcH("4-bromophenol",                                        "106-41-2",  5.23  ) ! HOC6H4Br
    CALL CalcH("2-nitrophenol",                                         "88-75-5",  3.24  ) ! HOC6H4(NO2)
    CALL CalcH("3-nitrophenol",                                        "554-84-7",  7.06  ) ! HOC6H4(NO2)
    CALL CalcH("4-nitrophenol",                                        "100-02-7",  7.81  ) ! HOC6H4(NO2)
    CALL CalcH("3-hydroxybenzoic acid nitrile",                        "873-62-1",  6.97  ) ! C7H5NO 3-cyanophenol !NEW
    CALL CalcH("4-hydroxybenzoic acid nitrile",                        "767-00-0",  7.46  ) ! C7H5NO 4-cyanophenol !NEW
    CALL CalcH("3-hydroxybenzaldehyde",                                "100-83-4",  6.97  ) ! C6H4(OH)CHO 3-formylphenol
    CALL CalcH("4-hydroxybenzaldehyde",                                "123-08-0",  7.68  ) ! C6H4(OH)CHO 4-formylphenol
    CALL CalcH("ethanoic acid",                                         "64-19-7",  4.91  ) ! CH3COOH acetic acid
    CALL CalcH("propanoic acid",                                        "79-09-4",  4.74  ) ! C2H5COOH propionic acid
    CALL CalcH("butanoic acid",                                        "107-92-6",  4.66  ) ! C3H7COOH butyric acid
    CALL CalcH("pentanoic acid",                                       "109-52-4",  4.52  ) ! C4H9COOH
    CALL CalcH("hexanoic acid",                                        "142-62-1",  4.56  ) ! C5H{11}COOH caproic acid
    CALL CalcH("heptanoic acid",                                       "111-14-8",  4.52  ) ! C7H{14}O2
    CALL CalcH("octanoic acid",                                        "124-07-2",  4.44  ) ! C8H{16}O2 caprylic acid
    CALL CalcH("3-methylbutanoic acid",                                "503-74-2",  4.47  ) ! (CH3)2CHCH2COOH
    CALL CalcH("ammonia",                                             "7664-41-7",  3.15  ) ! NH3
    CALL CalcH("methanamine",                                           "74-89-5",  3.34  ) ! CH3NH2 methylamine
    CALL CalcH("ethanamine",                                            "75-04-7",  3.30  ) ! C2H5NH2 ethylamine
    CALL CalcH("1-propanamine",                                        "107-10-8",  3.22  ) ! C3H7NH2 1-propylamine
    CALL CalcH("1-butanamine",                                         "109-73-9",  3.11  ) ! C4H9NH2 1-butylamine
    CALL CalcH("1-pentanamine",                                        "110-58-7",  3.00  ) ! C5H{11}NH2 1-pentylamine
    CALL CalcH("1-hexanamine",                                         "111-26-2",  2.90  ) ! C6H{13}NH2 1-hexylamine
    CALL CalcH("1-heptanamine",                                        "111-68-2",  2.78  ) ! C7H{17}N 1-heptylamine
    CALL CalcH("1-octanamine",                                         "111-86-4",  2.68  ) ! C8H{19}N 1-octylamine
    CALL CalcH("cyclohexanamine",                                      "108-91-8",  3.37  ) ! C6H{13}N
    CALL CalcH("dimethylamine",                                        "124-40-3",  3.15  ) ! (CH3)2NH
    CALL CalcH("diethylamine",                                         "109-89-7",  2.99  ) ! (C2H5)2NH
    CALL CalcH("dipropylamine",                                        "142-84-7",  2.68  ) ! (C3H7)2NH
    CALL CalcH("dibutylamine",                                         "111-92-2",  2.38  ) ! (C4H9)2NH
    CALL CalcH("N-(1-methylethyl)-2-propanamine",                      "108-18-9",  2.36  ) ! C6H{15}N diisopropylamine !NEW
    CALL CalcH("trimethylamine",                                        "75-50-3",  2.35  ) ! (CH3)3N
    CALL CalcH("triethylamine",                                        "121-44-8",  2.36  ) ! (C2H5)3N
    CALL CalcH("aminobenzene",                                          "62-53-3",  4.03  ) ! C6H7N aniline
    ! page 295:
    CALL CalcH("(dimethylamino)-benzene",                              "121-69-7",  2.53  ) ! C8H{11}N N,N-dimethylaniline
    CALL CalcH("pyridine",                                             "110-86-1",  3.44  ) ! C5H5N
    CALL CalcH("2-methylpyridine",                                     "109-06-8",  3.39  ) ! C5H4NCH3 2-picoline; $\alpha$-picoline
    CALL CalcH("3-methylpyridine",                                     "108-99-6",  3.50  ) ! C5H4NCH3 3-picoline; $\beta$-picoline
    CALL CalcH("4-methylpyridine",                                     "108-89-4",  3.61  ) ! C5H4NCH3
    CALL CalcH("2-ethylpyridine",                                      "100-71-0",  3.17  ) ! C5H4NC2H5
    CALL CalcH("3-ethylpyridine",                                      "536-78-7",  3.37  ) ! C5H4NC2H5
    CALL CalcH("4-ethylpyridine",                                      "536-75-4",  3.47  ) ! C5H4NC2H5
    CALL CalcH("2,3-dimethylpyridine",                                 "583-61-9",  3.53  ) ! C5H3N(CH3)2
    CALL CalcH("2,4-dimethylpyridine",                                 "108-47-4",  3.56  ) ! C5H3N(CH3)2
    CALL CalcH("2,5-dimethylpyridine",                                 "589-93-5",  3.45  ) ! C5H3N(CH3)2
    CALL CalcH("2,6-dimethylpyridine",                                 "108-48-5",  3.37  ) ! C5H3N(CH3)2
    CALL CalcH("3,4-dimethylpyridine",                                 "583-58-4",  3.82  ) ! C5H3N(CH3)2
    CALL CalcH("3,5-dimethylpyridine",                                 "591-22-0",  3.54  ) ! C5H3N(CH3)2
    CALL CalcH("4-(1,1-dimethylethyl)-pyridine",                      "3978-81-2",  3.27  ) ! C9H{13}N 4-{tert}-butylpyridine !NEW
    CALL CalcH("nitromethane",                                          "75-52-5",  2.95  ) ! CH3NO2
    CALL CalcH("nitroethane",                                           "79-24-3",  2.72  ) ! C2H5NO2
    CALL CalcH("1-nitropropane",                                       "108-03-2",  2.45  ) ! C3H7NO2
    CALL CalcH("1-nitro-butane",                                       "627-05-4",  2.27  ) ! C4H9NO2 !NEW
    CALL CalcH("1-nitro-pentane",                                      "628-05-7",  2.07  ) ! C5H{11}NO2 !NEW
    CALL CalcH("2-nitropropane",                                        "79-46-9",  2.30  ) ! CH3CH(NO2)CH3
    CALL CalcH("nitrobenzene",                                          "98-95-3",  3.02  ) ! C6H5NO2
    CALL CalcH("2-nitrotoluene",                                        "88-72-2",  2.63  ) ! C6H4(NO2)CH3
    CALL CalcH("3-nitrotoluene",                                        "99-08-1",  2.53  ) ! C6H4(NO2)CH3
    CALL CalcH("ethane nitrile",                                        "75-05-8",  2.85  ) ! CH3CN acetonitrile
    CALL CalcH("propane nitrile",                                      "107-12-0",  2.82  ) ! C2H5CN propionitrile
    CALL CalcH("butane nitrile",                                       "109-74-0",  2.67  ) ! C3H7CN butyronitrile
    CALL CalcH("pentane nitrile",                                      "110-59-8",  2.58  ) ! C4H9CN !NEW
    CALL CalcH("benzenenitrile",                                       "100-47-0",  3.01  ) ! C6H5CN benzonitrile
    CALL CalcH("chloromethane",                                         "74-87-3",  0.40  ) ! CH3Cl methyl chloride
    CALL CalcH("chloroethane",                                          "75-00-3",  0.46  ) ! C2H5Cl
    CALL CalcH("1-chloropropane",                                      "540-54-5",  0.24  ) ! C3H7Cl
    CALL CalcH("1-chlorobutane",                                       "109-69-3",  0.12  ) ! C4H9Cl
    CALL CalcH("1-chloropentane",                                      "543-59-9",  0.05  ) ! C5H{11}Cl
    CALL CalcH("1-chlorohexane",                                       "544-10-5",  0.00  ) ! C6H{13}Cl
    CALL CalcH("1-chloroheptane",                                      "629-06-1", -0.21  ) ! C7H{15}Cl
    CALL CalcH("2-chloropropane",                                       "75-29-6",  0.18  ) ! C3H7Cl
    CALL CalcH("2-chlorobutane",                                        "78-86-4",  0.00  ) ! C4H9Cl
    CALL CalcH("2-chloro-2-methyl-propane",                            "507-20-0", -0.80  ) ! C4H9Cl !NEW
    CALL CalcH("2-chloropentane",                                      "625-29-6", -0.05  ) ! C5H{11}Cl
    CALL CalcH("3-chloropentane",                                      "616-20-6", -0.03  ) ! C5H{11}Cl
    CALL CalcH("dichloromethane",                                       "75-09-2",  0.96  ) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",                                      "67-66-3",  0.79  ) ! CHCl3 chloroform
    CALL CalcH("tetrachloromethane",                                    "56-23-5", -0.06  ) ! CCl4 carbontetrachloride
    CALL CalcH("1,2-dichloroethane",                                   "107-06-2",  1.31  ) ! CH2ClCH2Cl
    CALL CalcH("1,1-dichloroethane",                                    "75-34-3",  0.62  ) ! CHCl2CH3
    CALL CalcH("1,1,1-trichloroethane",                                 "71-55-6",  0.14  ) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1,2-trichloroethane",                                 "79-00-5",  1.46  ) ! CHCl2CH2Cl
    CALL CalcH("1,1,2,2-tetrachloroethane",                             "79-34-5",  1.81  ) ! CHCl2CHCl2
    CALL CalcH("1,1,1,2-tetrachloroethane",                            "630-20-6",  0.94  ) ! CCl3CH2Cl
    CALL CalcH("pentachloroethane",                                     "76-01-7",  1.02  ) ! CHCl2CCl3
    CALL CalcH("1,2-dichloropropane",                                   "78-87-5",  0.93  ) ! C3H6Cl2
    CALL CalcH("1,3-dichloropropane",                                  "142-28-9",  1.39  ) ! C3H6Cl2
    CALL CalcH("1,4-dichlorobutane",                                   "110-56-5",  1.70  ) ! C4H8Cl2
    CALL CalcH("bromomethane",                                          "74-83-9",  0.60  ) ! CH3Br methyl bromide
    CALL CalcH("bromoethane",                                           "74-96-4",  0.54  ) ! C2H5Br
    CALL CalcH("1-bromopropane",                                       "106-94-5",  0.41  ) ! C3H7Br
    CALL CalcH("1-bromobutane",                                        "109-65-9",  0.29  ) ! C4H9Br
    CALL CalcH("1-bromopentane",                                       "110-53-2",  0.07  ) ! C5H{11}Br
    CALL CalcH("1-bromohexane",                                        "111-25-1", -0.13  ) ! C6H{13}Br
    CALL CalcH("1-bromoheptane",                                       "629-04-9", -0.25  ) ! C7H{15}Br
    CALL CalcH("1-bromooctane",                                        "111-83-1", -0.38  ) ! C8H{17}Br
    CALL CalcH("2-bromopropane",                                        "75-26-3",  0.35  ) ! C3H7Br
    CALL CalcH("1-bromo-2-methylpropane",                               "78-77-3",  0.02  ) ! C4H9Br
    ! page 296:
    CALL CalcH("2-bromo-2-methylpropane",                              "507-19-7", -0.62  ) ! C4H9Br
    CALL CalcH("dibromomethane",                                        "74-95-3",  1.44  ) ! CH2Br2
    CALL CalcH("tribromomethane",                                       "75-25-2",  1.56  ) ! CHBr3 bromoform
    CALL CalcH("1,2-dibromoethane",                                    "106-93-4",  1.71  ) ! C2H4Br2 ethylene dibromide
    CALL CalcH("iodoethane",                                            "75-03-6",  0.54  ) ! C2H5I
    CALL CalcH("1-iodopropane",                                        "107-08-4",  0.39  ) ! C3H7I
    CALL CalcH("1-iodobutane",                                         "542-69-8",  0.18  ) ! C4H9I
    CALL CalcH("1-iodopentane",                                        "628-17-1",  0.10  ) ! C5H{11}I !NEW
    CALL CalcH("1-iodohexane",                                         "638-45-9", -0.06  ) ! C6H{13}I !NEW
    CALL CalcH("1-iodoheptane",                                       "4282-40-0", -0.20  ) ! C7H{15}I
    CALL CalcH("diiodomethane",                                         "75-11-6",  1.84  ) ! CH2I2
    CALL CalcH("1,1,2-trichlorotrifluoroethane",                        "76-13-1", -1.30  ) ! C2F3Cl3 R113
    CALL CalcH("1-bromo-1,2,2,2-tetrafluoroethane",                    "124-72-1", -0.37  ) ! C2HBrF4 teflurane !NEW
    CALL CalcH("1-bromo-1-chloro-2,2,2-trifluoroethane",               "151-67-7",  0.08  ) ! C2HBrClF3 halothane !NEW
    CALL CalcH("2,2-dichloro-1,1-difluoro-1-methoxyethane",             "76-38-0",  0.82  ) ! C3H4Cl2F2O methoxyflurane !NEW
    CALL CalcH("1-chloro-2,2,2-trifluoroethyl difluoromethyl ether", "26675-46-7", -0.07  ) ! C3H2ClF5O isoflurane !NEW
    CALL CalcH("(2,2,2-trifluoroethoxy)-ethene",                       "406-90-6",  0.10  ) ! CF3CH2OCHCH2 fluoroxene !NEW
    CALL CalcH("chloroethene",                                          "75-01-4",  0.05  ) ! CH2CHCl vinyl chloride
    CALL CalcH("1,1-dichloroethene",                                    "75-35-4", -0.18  ) ! CH2CCl2
    CALL CalcH("(Z)-1,2-dichloroethene",                               "156-59-2",  0.51  ) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("(E)-1,2-dichloroethene",                               "156-60-5",  0.57  ) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",                                       "79-01-6",  0.32  ) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",                                    "127-18-4", -0.07  ) ! C2Cl4 tetrachloroethylene
    CALL CalcH("3-chloro-1-propene",                                   "107-05-1",  0.42  ) ! C3H5Cl
    CALL CalcH("3-bromo-1-propene",                                    "106-95-6",  0.63  ) ! C3H5Br
    CALL CalcH("(chloromethyl)-benzene",                               "100-44-7",  1.41  ) ! C6H5CH2Cl benzylchloride
    CALL CalcH("(bromomethyl)-benzene",                                "100-39-0",  1.74  ) ! C7H7Br benzyl bromide !NEW
    CALL CalcH("fluorobenzene",                                        "462-06-6",  0.58  ) ! C6H5F
    CALL CalcH("chlorobenzene",                                        "108-90-7",  0.84  ) ! C6H5Cl
    CALL CalcH("1-chloro-2-methylbenzene",                              "95-49-8",  0.84  ) ! C7H7Cl $o$-chlorotoluene
    CALL CalcH("1,2-dichlorobenzene",                                   "95-50-1",  1.10  ) ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",                                  "541-73-1",  0.83  ) ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",                                  "106-46-7",  0.97  ) ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("bromobenzene",                                         "108-86-1",  1.07  ) ! C6H5Br
    CALL CalcH("1-bromo-4-methylbenzene",                              "106-38-7",  1.02  ) ! BrC6H4CH3 $p$-bromotoluene
    CALL CalcH("iodobenzene",                                          "591-50-4",  1.28  ) ! C6H5I
    CALL CalcH("methanethiol",                                          "74-93-1",  1.00  ) ! CH3SH methyl mercaptan
    CALL CalcH("ethanethiol",                                           "75-08-1",  0.84  ) ! C2H5SH ethyl mercaptan
    CALL CalcH("1-propanethiol",                                       "107-03-9",  0.78  ) ! C3H7SH propyl mercaptan
    CALL CalcH("1-butanethiol",                                        "109-79-5",  0.73  ) ! C4H9SH butyl mercaptan
    CALL CalcH("dimethyl sulfide",                                      "75-18-3",  0.63  ) ! CH3SCH3 DMS
    CALL CalcH("diethyl sulfide",                                      "352-93-2",  1.07  ) ! C2H5SC2H5
    CALL CalcH("dipropyl sulfide",                                     "111-47-7",  0.94  ) ! C3H7SC3H7
    CALL CalcH("di-(2-propyl)-sulfide",                                "625-80-9",  0.89  ) ! (C3H7)2S
    CALL CalcH("dimethyl disulfide",                                   "624-92-0",  1.35  ) ! CH3SSCH3
    CALL CalcH("diethyl disulfide",                                    "110-81-6",  1.20  ) ! C2H5SSC2H5
    CALL CalcH("thiophene",                                            "110-02-1",  1.04  ) ! C4H4S
    CALL CalcH("2-methylthiophene",                                    "554-14-3",  1.01  ) ! CH3C4H3S
    CALL CalcH("benzenethiol",                                         "108-98-5",  1.87  ) ! C6H5SH thiophenol

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

  END SUBROUTINE ref1500

  !---------------------------------------------------------------------------

END MODULE Henry_ref1500

!*****************************************************************************
!                                  end of file
!*****************************************************************************
