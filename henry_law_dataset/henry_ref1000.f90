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

MODULE henry_ref1000

  USE henry_mem
  USE henry_util
  IMPLICIT NONE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ref0502 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "502"
    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0"
    type = "M"
    mindHR = 6513.
    Hominus = 10.**(-9.04) * EXP(mindHR/T0) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0502

  !---------------------------------------------------------------------------

  SUBROUTINE ref0505
    IMPLICIT NONE

    ref = "505"
    chem = "carbon monoxide" ; casrn = "630-08-0"
    type = "M"
    CALL Output(0.18*alpha_TO_HcpSI)

  END SUBROUTINE ref0505

  !---------------------------------------------------------------------------

  SUBROUTINE ref0507
    IMPLICIT NONE

    ref = "507"

    chem = "methanal" ; casrn = "50-00-0"
    CALL SettypeX("bell66")
    CALL MakeNote("HCHOdiol")
    CALL Output(2.97E3*Hcp_TO_HcpSI, 7194.) ! p. 85

    chem = "pernitric acid" ; casrn = "26404-66-0"
    type = "E"
    CALL MakeNote("507HNO4", &
      TRIM(citet())//" assumed the solubility to be comparable to that of \chem{HNO_3}.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0507

  !---------------------------------------------------------------------------

  SUBROUTINE ref0508 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "508"

    chem = "ethanoic acid" ; casrn = "64-19-7"
    type = "T"
    CALL Output(8.8E3*Hcp_TO_HcpSI)

    chem = "methanoic acid" ; casrn = "64-18-6"
    type = "T"
    CALL Output(5.6E3*Hcp_TO_HcpSI)

  END SUBROUTINE ref0508

  !---------------------------------------------------------------------------

  SUBROUTINE ref0510 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "510"

    ! only those species that are incorrect and those where I don't have the
    ! original reference are calculated here

    chem = "ammonia" ; casrn = "7664-41-7" ! NH3
    CALL SettypeX("vankrevelen49")
    Hominus = 6.2E1 * Hcp_TO_HcpSI
    CALL Output(Hominus)

    ! citations from ref2894 are not used because they look strange and
    ! may be incorrect
    ! chem = "hydroxybenzene" ; casrn = "108-95-2" ! C6H5OH phenol
    ! CALL SettypeX("2894")
    ! Hominus = 7.8E-2 * Hcp_TO_HcpSI
    ! CALL Output(Hominus)
    !
    ! chem = "2-butanone" ; casrn = "78-93-3" ! methyl ethyl ketone, MEK
    ! CALL SettypeX("2894")
    ! CALL MakeNote_range(4.1*Hcp_TO_HcpSI,7.7*Hcp_TO_HcpSI)
    ! CALL Output(DUMMY)
    !
    ! chem = "4-methyl-2-pentanone" ; casrn = "108-10-1" ! methyl isobutyl ketone
    ! CALL SettypeX("2894")
    ! CALL MakeNote_range(2.6*Hcp_TO_HcpSI,5.2*Hcp_TO_HcpSI)
    ! CALL Output(DUMMY)

    chem = "methanoic acid" ; casrn = "64-18-6" ! HCOOH formic acid
    CALL SettypeX("johnson90")
    Hominus = 7.6E3 * Hcp_TO_HcpSI
    CALL Output(Hominus)

    type = "C"
    chem = "dimethylsulfoxide" ; casrn = "67-68-5" ! CH3SOCH3 DMSO
    Hominus = 1.4E3 * Hcp_TO_HcpSI
    CALL Output(Hominus)

    type = "W"
    chem = "dipropylamine" ; casrn = "142-84-7" ! (C3H7)2NH
    CALL IncorrectCitation("721","dipropylamine")
    CALL Output(DUMMY)

    type = "W"
    chem = "trans-2-octenal" ; casrn = "2548-87-0" ! C5H{11}CHCHCHO
    CALL IncorrectCitation("755","trans-2-octenal")
    CALL Output(DUMMY)

    type = "?"
    chem = "methyl propanoate" ; casrn = "554-12-1" ! C2H5COOCH3
    Hominus = 5.89 * Hcp_TO_HcpSI
    mindHR = 41.4E3/Rgas
    CALL MakeNote("510mepr", &
      TRIM(citet())//" gives \citet{628} as the source. "// &
      "However, no data were found in that reference.")
    CALL Output(Hominus, mindHR)

    type = "?"
    chem = "4-tert-butylphenol" ; casrn = "98-54-4" ! (CH3)3CC6H4OH
    Hominus = 1.5e1 * Hcp_TO_HcpSI
    CALL MakeNote("510buph", &
      TRIM(citet())//" gives \citet{754} as the source. "// &
      "However, no data were found in that reference.")
    CALL Output(Hominus)

    type = "?"
    chem = "1,4-dioxane" ; casrn = "123-91-1" ! C4H8O2 dioxane
    Hominus = 2.05e2 * Hcp_TO_HcpSI
    CALL MakeNote("510diox714", &
      TRIM(citet())//" gives \citet{714} as the source. "// &
      "However, no data were found in that reference.")
    CALL Output(Hominus)

    type = "?"
    chem = "1,4-dioxane" ; casrn = "123-91-1" ! C4H8O2 dioxane
    Hominus = 2.21e2 * Hcp_TO_HcpSI
    CALL MakeNote("510diox728", &
      TRIM(citet())//" gives \citet{728} as the source. "// &
      "However, no data were found in that reference.")
    CALL Output(Hominus)

    type = "?"
    chem = "methyl methanoate" ; casrn = "107-31-3" ! HCOOCH3 methyl formate
    Hominus = 4.5 * Hcp_TO_HcpSI
    CALL MakeNote("from720", &
      "\citet{720} is quoted as the source. However, only "// &
      "activity coefficients and no vapor pressures are listed there.")
    CALL Output(Hominus)

  END SUBROUTINE ref0510

  !---------------------------------------------------------------------------

  SUBROUTINE ref0511 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "511"

    chem = "methanoic acid" ; casrn = "64-18-6"
    type = "M"
    mindHR = 51E3/Rgas
    CALL Output(8.9E3*Hcp_TO_HcpSI, mindHR)

    chem = "ethanoic acid" ; casrn = "64-19-7"
    type = "M"
    mindHR = 52E3/Rgas
    CALL Output(4.1E3*Hcp_TO_HcpSI, mindHR)

    chem = "methanoic acid" ; casrn = "64-18-6"
    type = "C"
    CALL Output(5.2E3*Hcp_TO_HcpSI)

    chem = "ethanoic acid" ; casrn = "64-19-7"
    type = "C"
    CALL Output(5.2E3*Hcp_TO_HcpSI)

  END SUBROUTINE ref0511

  !---------------------------------------------------------------------------

  SUBROUTINE ref0512 ! KHpc [atm/M]
    IMPLICIT NONE

    ref = "512"

    chem = "dimethyl sulfide" ; casrn = "75-18-3"
    type = "M"
    mindHR = 3463.
    Hominus = EXP(mindHR/T0-12.2) * Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0512

  !---------------------------------------------------------------------------

  SUBROUTINE ref0513 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "513"

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0"
    type = "M"
    CALL MakeNoteOtherTemp("283")
    CALL Output(5.0*Hcp_TO_HcpSI)

  END SUBROUTINE ref0513

  !---------------------------------------------------------------------------

  SUBROUTINE ref0514 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "514"
    type = "T"
    chem = "dinitrogen pentoxide" ; casrn = "10102-03-1" ! N2O5 nitric anhydride
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/230.,273./)
    Harray = (/60.,6./)*Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("514N2O5", &
      "This value was extrapolated from data at $T$ = 230~\unit{K} and "// &
      "$T$ = 273~\unit{K}.")
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0514

  !---------------------------------------------------------------------------

  SUBROUTINE ref0515 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "515"
    type = "T"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/249.,275.,298./)

    chem = "hydroxyl radical" ; casrn = "3352-57-6" ! OH
    Harray = (/500.,100.,30./)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    ! different mindHR results when using deltaG and deltaS values
    CALL Output(Hominus, mindHR, r2)

    chem = "hydroperoxy radical" ; casrn = "3170-83-0" ! HO2
    Harray = (/2.E5,2.E4,4.E3/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0515

  !---------------------------------------------------------------------------

  SUBROUTINE ref0516 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "516"
    type = "M"

    chem = "hydrogen peroxide" ; casrn = "7722-84-1"
    CALL Output(8.33E4*Hcp_TO_HcpSI, 7379.)

    chem = "methyl hydroperoxide" ; casrn = "3031-73-0"
    CALL Output(311.*Hcp_TO_HcpSI, 5241.)

    chem = "hydroxymethyl hydroperoxide" ; casrn = "15932-89-5"
    CALL Output(1.67E6*Hcp_TO_HcpSI, 9652.)

    chem = "ethanoic peroxyacid" ; casrn = "79-21-0"
    CALL Output(837.*Hcp_TO_HcpSI, 5308.)

    chem = "ethyl hydroperoxide" ; casrn = "3031-74-1"
    CALL Output(336.*Hcp_TO_HcpSI, 5995.)

  END SUBROUTINE ref0516

  !---------------------------------------------------------------------------

  SUBROUTINE ref0520 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "520"
    type = "M"

    chem = "methanoic acid" ; casrn = "64-18-6"
    CALL MakeNote(TRIM(ref), &
      "The value given here was measured at a liquid phase mixing "// &
      "ratio of 1~\unit{\mu mol/mol}. "//TRIM(citet())//" found that the "// &
      "Henry's law constant changes at higher concentrations.")
    CALL Output(13.4E3*Hcp_TO_HcpSI)

    chem = "ethanoic acid" ; casrn = "64-19-7"
    CALL MakeNote(TRIM(ref))
    CALL Output(9.3E3*Hcp_TO_HcpSI)

    chem = "propanoic acid" ; casrn = "79-09-4"
    CALL MakeNote(TRIM(ref))
    CALL Output(6.2E3*Hcp_TO_HcpSI)

    chem = "2-methyl propanoic acid" ; casrn = "79-31-2"
    CALL MakeNote(TRIM(ref))
    CALL Output(5.7E3*Hcp_TO_HcpSI)

  END SUBROUTINE ref0520

  !---------------------------------------------------------------------------

  SUBROUTINE ref0522 ! KHcc [1]
    IMPLICIT NONE

    ref = "522"

    CALL CalcH("dimethyl sulfide",      "75-18-3",  &
      avg((/1637.3,1635.6,1598.2/)),  avg((/4.354,4.358,4.205/)) )
    CALL CalcH("diethyl sulfide",       "352-93-2", &
      avg((/1939.7,2175.4,1926.8/)),  avg((/5.434,6.140,5.3/))   )
    CALL CalcH("dipropyl sulfide",      "111-47-7", &
      avg((/1656.6,1954.8/)),         avg((/4.618,5.678/))       )
    CALL CalcH("di-(2-propyl)-sulfide", "625-80-9", &
      avg((/2089.4,2000.3,2059.8/)),  avg((/6.123,5.864,6.038/)) )
    CALL CalcH("dimethyl disulfide",    "624-92-0", &
      avg((/1657.1,1854.4/)),         avg((/4.211,4.828/))       )
    CALL CalcH("diethyl disulfide",     "110-81-6", &
      avg((/1608.9,1865.0/)),         avg((/4.190,5.071/))       )
    CALL CalcH("thiophene",             "110-02-1", &
      avg((/1563.6,1580.0,1661.9/)),  avg((/4.199,4.277,4.542/)) )
    CALL CalcH("2-methylthiophene",     "554-14-3", &
      avg((/1651.04,1741.1,1886.6/)), avg((/4.535,4.838,5.320/)) )
    CALL CalcH("methanethiol",          "74-93-1",  &
      1347.1,                         3.537                      )
    CALL CalcH("ethanethiol",           "75-08-1",  &
      1486.1,                         4.147                      )
    CALL CalcH("1-propanethiol",        "107-03-9", &
      1552.2,                         4.428                      )
    CALL CalcH("1-butanethiol",         "109-79-5", &
      1655.9,                         4.823                      )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, b, a)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: b, a
      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      type   = "M"
      Hominus    = 10.**(b/T0-a) * Hcc_TO_HcpSI_atT0
      mindHR = b * LOG(10.) + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0522

  !---------------------------------------------------------------------------

  SUBROUTINE ref0523 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "523"
    type = "?"

    CALL CalcH("oxygen",               "7782-44-7", 1.3E-3)        ! O2
    CALL CalcH("nitrogen monoxide",   "10102-43-9", 1.9e-3)        ! NO
    CALL CalcH("C2H4",                   "74-85-1", 4.9e-3)
    CALL CalcH("ozone",               "10028-15-6", 9.4e-3, 5.04)  ! O3
    ! see ref2549 for O3 from Roth and Sullivan
    CALL CalcH("NO2",                 "10102-44-0", 1.e-2)
    CALL CalcH("dinitrogen monoxide", "10024-97-2", 2.5e-2)        ! N2O
    CALL CalcH("carbon dioxide",        "124-38-9", 3.4e-2, 4.846) ! CO2
    CALL CalcH("sulfur dioxide",       "7446-09-5", 1.24,   6.247) ! SO2
    CALL CalcH("nitrous acid",         "7782-77-6", 49.)           ! HNO2
    CALL CalcH("ammonia",              "7664-41-7", 62.,    8.17)  ! NH3
    CALL CalcH("hydrogen chloride",    "7647-01-0", 2.5e3)         ! HCl
    CALL CalcH("HCHO",                   "50-00-0", 6.3e3)
    CALL CalcH("hydrogen peroxide",    "7722-84-1", 7.1e4,  14.5)  ! H2O2
    CALL CalcH("nitric acid",          "7697-37-2", 2.1e5)         ! HNO3

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H_, mindHr_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: H_
      REAL, INTENT(IN), OPTIONAL   :: mindHr_
      REAL, PARAMETER :: CONV = kcal/Rgas

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = H_ * Hcp_TO_HcpSI
      CALL MakeNote("whichref")
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      IF (PRESENT(mindHR_)) THEN ! show bad value
        CALL Output(Hominus, mindHR_*CONV)
      ELSE
        CALL Output(Hominus)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref0523

  !---------------------------------------------------------------------------

  SUBROUTINE ref0524 ! KHcc [1]
    IMPLICIT NONE

    ref = "524"
    type = "M"

    CALL CalcH("OCS", "463-58-1", 12.722, -3496.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a1, a2)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: a1, a2
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHcc_TIMES_HcpSI_atT0 / (EXP(a1+a2/T0))
      mindhr = -a2 + T0 ! see ref958, eqn (34) why T0 is added
      CALL MakeNote("seawater")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0524

  !---------------------------------------------------------------------------

  SUBROUTINE ref0525 ! special definition [ml(STD-air)/liter(aq)]
    IMPLICIT NONE

    ref = "525"
    type = "M"
    chem = "oxygen" ; casrn = "7782-44-7" ! O2
    ndata = 21
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.48,.52,.64,5.21,10.10,10.22,14.55,14.72,14.97,14.99,15.12,15.14&
      &,20.08,20.1,25.1,25.35,25.36,29.8,29.81,34.76,34.82/)
    temp = temp + CtoK
    Harray = (/10.12,10.088,10.062,8.858,7.863,7.839,7.139,7.116,7.078,7.079,7.054&
      &,7.057,6.34,6.344,5.761,5.734,5.742,5.301,5.308,4.867,4.868/)
    ! 22400 ml/mole at STP; 20.94% O2 in air
    Harray = Harray / 22400. / 0.2094 * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0525

  !---------------------------------------------------------------------------

  ! ref0526 only cites ref1072, no new data

  !---------------------------------------------------------------------------

  SUBROUTINE ref0527 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "527"

    chem = "hydrogen peroxide" ; casrn = "7722-84-1"
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1.42E5*Hcp_TO_HcpSI)

  END SUBROUTINE ref0527

  !---------------------------------------------------------------------------

  SUBROUTINE ref0528 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "528"

    chem = "ozone" ; casrn = "10028-15-6"
    type = "M"

    ! using function HTdep:
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/3.5, 19.8/) + CtoK
    Harray   = (/0.480, 0.323/) * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0528

  !---------------------------------------------------------------------------

  SUBROUTINE ref0529
    IMPLICIT NONE

    ref = "529"
    type = "C"

    chem = "methane" ; casrn = "74-82-8" ! CH4
    CALL Output(10**(-2.87)*Hcp_TO_HcpSI)
    chem = "ethane" ; casrn = "74-84-0" ! C2H6
    CALL Output(10**(-2.74)*Hcp_TO_HcpSI)
    chem = "propane" ; casrn = "74-98-6" ! C3H8
    CALL Output(10**(-2.86)*Hcp_TO_HcpSI)
    chem = "butane" ; casrn = "106-97-8" ! C4H{10}
    CALL Output(10**(-2.95)*Hcp_TO_HcpSI)
    chem = "ethene" ; casrn = "74-85-1" ! C2H4 ethylene
    CALL Output(10**(-2.32)*Hcp_TO_HcpSI)
    chem = "propene" ; casrn = "115-07-1" ! C3H6 propylene
    CALL Output(10**(-2.03)*Hcp_TO_HcpSI)
    chem = "ethyne" ; casrn = "74-86-2" ! C2H2 acetylene
    CALL Output(10**(-1.38)*Hcp_TO_HcpSI)

  END SUBROUTINE ref0529

  !---------------------------------------------------------------------------

  SUBROUTINE ref0530 ! Hb2p [mol^2/(kg^2*atm)]
    IMPLICIT NONE

    ref = "530"
    type = "T"

    ! T-dep given as: ln(H)=a+b/T+cT --> d(lnH)/d(1/T) = b-cT^2

    chem = "HF" ; casrn = "7664-39-3"
    Hominus = 9.61 * Hb2p_TO_Hc2pSI
    mindHR = 4918.0344+0.02790*T0**2
    CALL MakeNoteHb2p(Hominus,mindHR)
    CALL Output(DUMMY)

    chem = "HCl" ; casrn = "7647-01-0"
    Hominus = 2.04E6 * Hb2p_TO_Hc2pSI
    mindHR = 5977.5014+0.03401*T0**2
    CALL MakeNoteHb2p(Hominus,mindHR)
    CALL Output(DUMMY)

    chem = "HBr" ; casrn = "10035-10-6"
    Hominus = 1.32E9 * Hb2p_TO_Hc2pSI
    mindHR = 7117.0552+0.03512*T0**2
    CALL MakeNoteHb2p(Hominus,mindHR)
    CALL Output(DUMMY)

    chem = "HI" ; casrn = "10034-85-2"
    Hominus = 2.5E9 * Hb2p_TO_Hc2pSI
    mindHR = 6689.1418+0.03522*T0**2
    CALL MakeNoteHb2p(Hominus,mindHR)
    CALL Output(DUMMY)

    chem = "HNO3" ; casrn = "7697-37-2"
    Hominus = 2.45E6 * Hb2p_TO_Hc2pSI
    mindHR = 6137.3984+0.02876*T0**2
    CALL MakeNoteHb2p(Hominus,mindHR)
    CALL Output(DUMMY)

  CONTAINS

    SUBROUTINE MakeNoteHb2p(Hominus_,mindHR_)
      IMPLICIT NONE
      REAL, INTENT(IN) :: Hominus_, mindHR_
      CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn), &
        "$\Hprime$~= $"//TRIM(Hprime_string(Hominus_))// &
        "\times\exp\left("//TRIM(mindHR_string(mindHR_))//"~\unit{K} "// &
        "\left(\DS\frac{1}{T}-\frac{1}{T^{\ominus}}\right)\right)$~"//TRIM(Hprime_unit_ol_TeX))
    END SUBROUTINE MakeNoteHb2p

  END SUBROUTINE ref0530

  !---------------------------------------------------------------------------

  SUBROUTINE ref0531 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "531"

    chem = "trichloromethane" ; casrn = "67-66-3"
    type = "C"
    CALL Output(1./(atm*3.6E-3))
    type = "C"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./(atm*4.8E-3))
    type = "M"
    mindHR = 5200.
    Hominus    = 1./(atm*EXP(11.9 - mindHR/T0))
    CALL Output(Hominus, mindHR)

    chem = "bromodichloromethane" ; casrn = "75-27-4"
    type = "C"
    CALL Output(1./(atm*2.3E-3))
    type = "C"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./(atm*2.1E-3))
    type = "M"
    mindHR = 5210.
    Hominus    = 1./(atm*EXP(11.3 - mindHR/T0))
    CALL Output(Hominus, mindHR)

    chem = "dibromochloromethane" ; casrn = "124-48-1"
    type = "C"
    CALL Output(1./(atm*8.4E-4))
    type = "C"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1./(atm*8.7E-4))
    type = "M"
    mindHR = 5210.
    Hominus    = 1./(atm*EXP(10.7 - mindHR/T0))
    CALL Output(Hominus, mindHR)

    chem = "tribromomethane" ; casrn = "75-25-2"
    type = "C"
    CALL Output(1./(atm*5.8E-4))
    type = "M"
    mindHR = 5670.
    Hominus    = 1./(atm*EXP(11.6 - mindHR/T0))
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0531

  !---------------------------------------------------------------------------

  SUBROUTINE ref0532 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "532"
    type = "M"

    CALL CalcH("tetrachloroethene",      "127-18-4", 12.45,  4918.)
    CALL CalcH("trichloroethene",        "79-01-6",  11.37,  4780.)
    CALL CalcH("1,1-dichloroethene",     "75-35-4",   8.845, 3729.)
    CALL CalcH("(Z)-1,2-dichloroethene", "156-59-2",  8.479, 4192.)
    CALL CalcH("(E)-1,2-dichloroethene", "156-60-5",  9.341, 4182.)
    CALL CalcH("chloroethene",           "75-01-4",   7.385, 3286.)
    CALL CalcH("1,1,1-trichloroethane",  "71-55-6",   9.777, 4133.)
    CALL CalcH("1,1-dichloroethane",     "75-34-3",   8.637, 4128.)
    CALL CalcH("chloroethane",           "75-00-3",   5.974, 3120.)
    CALL CalcH("tetrachloromethane",     "56-23-5",  11.29,  4411.)
    CALL CalcH("trichloromethane",       "67-66-3",   9.843, 4612.)
    CALL CalcH("dichloromethane",        "75-09-2",   6.653, 3817.)
    CALL CalcH("chloromethane",          "74-87-3",   9.358, 4215.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: A
      REAL, INTENT(IN)             :: B
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = 1. / (atm * EXP(A-B/T0))
      mindHR = B
      CALL Output(Hominus, mindHR)

    END SUBROUTINE CalcH

  END SUBROUTINE ref0532

  !---------------------------------------------------------------------------

  SUBROUTINE ref0533 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "533"
    type = "M"
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))

    chem = "1-butyl nitrate" ; casrn = "928-45-0"
    temp = (/295.0, 279.4/)
    Harray   = (/1.26, 3.94/) * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "2-butyl nitrate" ; casrn = "924-52-7"
    temp = (/295.9, 279.4/)
    Harray   = (/0.74, 2.27/) * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0533

  !---------------------------------------------------------------------------

  SUBROUTINE ref0536 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "536"

    chem = "nitryl chloride" ; casrn = "13444-90-1"
    type = "E"
    CALL MakeNote("536ClNO2", &
      "Derived as a fitting parameter used in numerical modeling.")
    CALL Output(2.4E-2*Hcp_TO_HcpSI)

  END SUBROUTINE ref0536

  !---------------------------------------------------------------------------

  SUBROUTINE ref0538 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "538"
    type = "C"

    chem = "methanoic acid" ; casrn = "64-18-6"
    CALL Output(5.2E3*Hcp_TO_HcpSI)
    CALL Output(5.39E3*Hcp_TO_HcpSI)

    chem = "ethanoic acid" ; casrn = "64-19-7"
    CALL Output(5.24E3*Hcp_TO_HcpSI)
    CALL Output(8.61E3*Hcp_TO_HcpSI)

  END SUBROUTINE ref0538

  !---------------------------------------------------------------------------

  ! ref0539 only cites PhD thesis ref3119. Note that the unit in the
  ! paper should be [(mol/m3)/(mol/m3)] and not [pptv/pptv]. The Henry's
  ! law constant cannot be defined via mixing ratios.

  !---------------------------------------------------------------------------

  SUBROUTINE ref0540 ! KHpb [atm*kg/mol]
    IMPLICIT NONE

    ref = "540"
    type = "L"

    CALL CalcH("ammonia",          "7664-41-7",   -157.552,  28.1001, -0.049227,  -149.006)
    CALL CalcH("carbon dioxide",   "124-38-9",   -6789.04,  -11.4519, -0.010454,    94.4914)
    CALL CalcH("hydrogen sulfide", "7783-06-4", -13236.8,   -55.0551,  0.0595651,  342.595)
    CALL CalcH("sulfur dioxide",   "7446-09-5",  -5578.8,    -8.76152, 0.,          68.418)
    CALL CalcH("hydrogen cyanide", "74-90-8",   -49068.8,  -241.82,    0.315014,  1446.005)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, B1, B2, B3, B4)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: B1, B2, B3, B4
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = Hbp_TO_HcpSI / EXP(B1/T0+B2*LOG(T0)+B3*T0+B4)
      mindHR = -(B1-B2*T0-B3*T0**2) ! analytical d(ln H)/d(1/T)
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0540

  !---------------------------------------------------------------------------

  SUBROUTINE ref0541 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "541"
    type = "M"

    ! Table II:
    CALL CalcH("methyl nitrate",            "598-58-3",       2.64,  -39.40E3)
    CALL CalcH("ethyl nitrate",             "625-58-1",       2.18,  -44.53E3)
    CALL CalcH("1-propyl nitrate",          "627-13-4",       1.55,  -45.62E3)
    CALL CalcH("2-propyl nitrate",          "1712-64-7",      1.08,  -44.53E3)
    CALL CalcH("1-butyl nitrate",           "928-45-0",       1.42,  -48.11E3)
    CALL CalcH("2-butyl nitrate",           "924-52-7",       0.89,  -45.02E3)
    CALL CalcH("2-methyl-1-nitropropane",   "543-29-3",       0.96,  -43.62E3) ! isobutyl nitrate
    CALL CalcH("1-pentyl nitrate",          "1002-16-0",      1.2            )
    CALL CalcH("2-pentyl nitrate",          "21981-48-6",     0.53,  -52.29E3)
    ! Table IV:
    CALL CalcH("1,2-ethanediol dinitrate",  "628-96-6",     640.             )
    CALL CalcH("2-nitrooxy ethanol",        "16051-48-2", 39900.             )
    CALL CalcH("2-nitrooxy-1-propanol",     "20266-74-4",  6700.             )
    CALL CalcH("2-nitrooxy-1-propanol",     "20266-74-4",  7300.             )
    CALL CalcH("1-nitrooxy-2-propanol",     "20266-65-3",  6700.             )
    CALL CalcH("1-nitrooxy-2-propanol",     "20266-65-3",  7300.             )
    CALL CalcH("1,2-propanediol dinitrate", "6423-43-4",    175.             )
    CALL CalcH("1-nitrooxy-2-propanone",    "6745-71-7",   1010.             )

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, H293, DeltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H293
      REAL, OPTIONAL,   INTENT(IN) :: DeltaH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      IF ((casrn_=="20266-74-4").OR.(casrn_=="20266-65-3")) THEN
        CALL MakeNote("541iso", &
          TRIM(citet())//" were unable to assign the values to the isomers.")
      ENDIF
      IF (PRESENT(DeltaH)) THEN
        mindhr = - DeltaH / Rgas
        Hominus    = H293 * EXP(mindHR*(1/T0-1/293.)) * Hcp_TO_HcpSI
        CALL Output(Hominus, mindHR)
      ELSE
        CALL MakeNoteOtherTemp("293")
        CALL Output(H293*Hcp_TO_HcpSI)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref0541

  !---------------------------------------------------------------------------

  SUBROUTINE ref0542 ! only DUMMY
    IMPLICIT NONE

    ref = "542"

    chem = "2-methyl-2-propanol" ; casrn = "75-65-0"
    type = "M"
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" found that {\it tert}-butanol does not obey Henry's "// &
      "law at $c "//morethan//"$ 3.8~\unit{mM}.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0542

  !---------------------------------------------------------------------------

  SUBROUTINE ref0544 ! Hcc [1]
    IMPLICIT NONE

    ref = "544"
    type = "M"

    CALL CalcH("chloromethane",            "74-87-3",   3.3)
    CALL CalcH("dichloromethane",          "75-09-2",   8.1)
    CALL CalcH("trichloromethane",         "67-66-3",   8.6)
    CALL CalcH("tetrachloromethane",       "56-23-5",   1.1)
    CALL CalcH("1,2-dichloroethane",      "107-06-2",  26.4)
    CALL CalcH("1,1,1-trichloroethane",    "71-55-6",   0.71)
    CALL CalcH("chloroethene",             "75-01-4",   0.02)
    CALL CalcH("1,1-dichloroethene",       "75-35-4",   0.16)
    CALL CalcH("trichloroethene",          "79-01-6",   2.74)
    CALL CalcH("tetrachloroethene",       "127-18-4",   1.22)
    CALL CalcH("trichlorofluoromethane",   "75-69-4",   0.03)
    CALL CalcH("dichlorodifluoromethane",  "75-71-8",   0.06)
    CALL CalcH("hexachlorobutadiene",      "87-68-3",   0.97)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Hcc

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNote(TRIM(ref), &
        "The same data were also published in \citet{543}.")
      IF (casrn_=="75-01-4") THEN
        CALL MakeNoteOtherTemp("283")
        CALL Output(Hcc_TO_HcpSI(Hcc,283.))
      ELSE
        CALL MakeNoteOtherTemp("293")
        CALL Output(Hcc_TO_HcpSI(Hcc,293.))
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref0544

  !---------------------------------------------------------------------------

  SUBROUTINE ref0545 ! KHcc [1]
    IMPLICIT NONE
    REAL          :: KHcc
    INTEGER       :: i
    CHARACTER(STRLEN_VLONG) :: seenote_

    ref = "545"
    ndata = 40
    OPEN (10,FILE="input/ref0545.dat",STATUS="OLD")
    DO i = 1, ndata
      READ (10,*) KHcc, chem, casrn, type, seenote_
      IF (TRIM(seenote_)/="") THEN
        IF (TRIM(seenote_)=="545noTdep") THEN
          CALL MakeNote("545noTdep", &
            "Values at different temperatures are from different sources. "// &
            "Thus a temperature dependence was not calculated.")
        ELSE
          CALL MakeNoteOtherTemp(TRIM(seenote_))
        ENDIF
      ENDIF
      Hominus =  KHcc_TIMES_HcpSI_atT0/KHcc
      CALL Output(Hominus)
    ENDDO
    CLOSE(10)

  END SUBROUTINE ref0545

  !---------------------------------------------------------------------------

  SUBROUTINE ref0548
    IMPLICIT NONE

    ref = "548"
    type = "M"

    CALL CalcH("2-nitrooxy ethanol",     "16051-48-2", 38800., -71.9E3)
    CALL CalcH("1-nitrooxy-2-propanol",  "20266-65-3", 10900., -82.8E3)
    CALL CalcH("2-nitrooxy-1-propanol",  "20266-74-4",  4500., -73.0E3)
    CALL CalcH("2-nitrooxy-3-butanol",  "147794-10-3", 10100., -79.0E3)
    CALL CalcH("1-nitrooxy-2-butanol",  "147794-11-4",  5800., -76.3E3)
    CALL CalcH("2-nitrooxy-1-butanol",  "147794-12-5",  6000., -79.6E3)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hominus_, DeltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: Hominus_, DeltaH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      mindhr = - DeltaH / Rgas
      Hominus    = Hominus_ * Hcp_TO_HcpSI
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0548

  !---------------------------------------------------------------------------

  SUBROUTINE ref0559 ! KHcc [1]
    IMPLICIT NONE

    ref = "559"

    chem = "nitrogen monoxide" ; casrn = "10102-43-9"
    type = "M"
    CALL MakeNoteOtherTemp("297")
    CALL Output(KHcc_TO_HcpSI(31.,297.15))

  END SUBROUTINE ref0559

  !---------------------------------------------------------------------------

  SUBROUTINE ref0563 ! KHcc [1]
    IMPLICIT NONE

    ref = "563"

    ! see SUBROUTINE ref3122 for this data set:
    ! chem = "sulfur dioxide" ; casrn = "7446-09-5"
    ! CALL SettypeX("terraglio67")
    ! CALL Output(KHcc_TIMES_HcpSI_atT0/3.8e-2)

    chem = "dinitrogen monoxide" ; casrn = "10024-97-2"
    type = "?"
    CALL Output(KHcc_TIMES_HcpSI_atT0/1.6)

    ! see SUBROUTINE ref1911 for this data set:
    ! chem = "carbon monoxide" ; casrn = "630-08-0"
    ! CALL MakeNote("seawater")
    ! CALL SettypeX("1911")
    ! CALL Output(KHcc_TIMES_HcpSI_atT0/50.)

    chem = "methane" ; casrn = "74-82-8"
    type = "C"
    CALL Output(KHcc_TIMES_HcpSI_atT0/42.)

    chem = "tetrachloromethane" ; casrn = "56-23-5"
    type = "C"
    CALL Output(KHcc_TIMES_HcpSI_atT0/1.08)

    chem = "trichlorofluoromethane" ; casrn = "75-69-4"
    type = "C"
    CALL Output(KHcc_TIMES_HcpSI_atT0/5.)

    chem = "iodomethane" ; casrn = "74-88-4"
    type = "C"
    CALL Output(KHcc_TIMES_HcpSI_atT0/0.24)

  END SUBROUTINE ref0563

  !---------------------------------------------------------------------------

  SUBROUTINE ref0582
    IMPLICIT NONE
    REAL :: A, B, D

    ref = "582"

    chem = "hypoiodous acid" ; casrn = "14332-21-9"
    type = "C"
    Hominus = Hcc_TO_HcpSI(1e4,295.15)
    CALL Output(Hominus, limit=">")

    chem = "molecular iodine" ; casrn = "7553-56-2"
    type = "R"
    B = 4220.5
    A = -19.991
    D = 0.02583
    Hominus = 10.**(B/T0+A+D*T0) * Hcc_TO_HcpSI_atT0
    mindHR =  LOG(10.) * (B - D*T0**2) ! analytical d(ln H)/d(1/T)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0582

  !---------------------------------------------------------------------------

  SUBROUTINE ref0583 ! KHcc [1]
    IMPLICIT NONE

    ref = "583"
    type = "M"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))

    temp = (/0.,10.,20./) + CtoK

    chem = "iodomethane" ; casrn = "74-88-4"
    Harray = (/0.0763,0.1361,0.2245/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "trichloromethane" ; casrn = "67-66-3"
    Harray = (/0.0556,0.0908,0.1446/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "dibromomethane" ; casrn = "74-95-3"
    Harray = (/0.0112,0.0205,0.0336/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "bromodichloromethane" ; casrn = "75-27-4"
    Harray = (/0.0246,0.0452,0.0788/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "chloroiodomethane" ; casrn = "593-71-5"
    Harray = (/0.0122,0.0217,0.0356/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "dibromochloromethane" ; casrn = "124-48-1"
    Harray = (/0.0124,0.0239,0.0421/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "tribromomethane" ; casrn = "75-25-2"
    Harray = (/0.0063,0.0124,0.0217/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    chem = "diiodomethane" ; casrn = "75-11-6"
    Harray = (/0.0038,0.0074,0.0132/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    ! different temperature range for next species:
    temp = (/0.,3.,6./) + CtoK
    chem = "chloromethane" ; casrn = "74-87-3"
    Harray = (/0.1728,0.1977,0.2187/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote("seawater")
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0583

  !---------------------------------------------------------------------------

  SUBROUTINE ref0584 ! KHcc [1]
    IMPLICIT NONE

    ref = "584"
    type = "M"

    CALL CalcH("trichloromethane",      "67-66-3",  12.012, -4142.)
    CALL CalcH("tetrachloromethane",    "56-23-5",  13.722, -4073.)
    CALL CalcH("1,1-dichloroethane",    "75-34-3",  11.727, -3975.)
    CALL CalcH("1,2-dichloroethane",    "107-06-2", 11.377, -4329.)
    CALL CalcH("1,1,1-trichloroethane", "71-55-6",  12.351, -3834.)
    CALL CalcH("trichloroethene",       "79-01-6",  11.121, -3648.)
    CALL CalcH("tetrachloroethene",     "127-18-4", 14.655, -4528.)
    CALL CalcH("benzene",               "71-43-2",  10.577, -3640.)
    CALL CalcH("methylbenzene",         "108-88-3", 12.150, -4064.)
    CALL CalcH("ethylbenzene",          "100-41-4", 14.001, -4567.)
    CALL CalcH("1,2-dimethylbenzene",   "95-47-6",  12.400, -4232.)
    CALL CalcH("1,3-dimethylbenzene",   "108-38-3", 12.123, -4026.)
    CALL CalcH("1,4-dimethylbenzene",   "106-42-3", 13.597, -4479.)

    ! from other refs:
    chem = "methylbenzene" ; casrn = "108-88-3"
    CALL SettypeX("mcauliffe71")
    CALL Output(KHcc_TIMES_HcpSI_atT0/0.272)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, c, a)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: c, a

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHcc_TIMES_HcpSI_atT0 / (EXP(a/T0+c))
      mindhr = -a + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0584

  !---------------------------------------------------------------------------

  SUBROUTINE ref0585 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "585"

    chem = "nitrogen trioxide" ; casrn = "12033-49-7"
    type = "M"
    CALL MakeNote("585NO3", &
      "Value obtained by estimating the diffusion coefficient for "// &
      "\chem{NO_3} to be $D$~= 1.0\E{-5}~\unit{cm^2/s}.")
    CALL Output(0.6*Hcp_TO_HcpSI)

  END SUBROUTINE ref0585

  !---------------------------------------------------------------------------

  SUBROUTINE ref0586 ! Hxp [1/atm]
    IMPLICIT NONE

    ref = "586"

    CALL CalcH("carbon oxide sulfide",  "463-58-1", 0.000396, 4195.)
    CALL CalcH("hydrogen sulfide",     "7783-06-4", 0.00157,  4134.)
    CALL CalcH("dimethyl sulfide",       "75-18-3", 0.00861,  6096.)
    CALL CalcH("methanethiol",           "74-93-1", 0.00366,  5559.)
    CALL CalcH("carbon disulfide",       "75-15-0", 0.00099,  5500.)

    ! Aneja [2624] just took the value from Dacey [512] which is not
    ! repeated here.
    !chem = "dimethyl sulfide" ; casrn = "75-18-3"
    !CALL SettypeX("2624")
    !CALL Output(0.00861*Hxp_TO_HcpSI, 7048.*cal/Rgas)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, Hprime, A)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: Hprime, A

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      type = "M"
      Hominus = Hprime * Hxp_TO_HcpSI
      mindHR = A * cal / Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0586

  !---------------------------------------------------------------------------

  SUBROUTINE ref0588 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "588"
    type = "E"

    chem = "ethanedioic acid" ; casrn = "144-62-7"
    CALL MakeNote(TRIM(ref), &
      "Value obtained by "//TRIM(citet())//" using the group contribution method.")
    CALL Output(5E8*Hcp_TO_HcpSI)

    chem = "propanedioic acid" ; casrn = "141-82-2"
    CALL MakeNote(TRIM(ref))
    CALL Output(4E8*Hcp_TO_HcpSI)

    chem = "butanedioic acid" ; casrn = "110-15-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E8*Hcp_TO_HcpSI)

    chem = "pentanedioic acid" ; casrn = "110-94-1"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E8*Hcp_TO_HcpSI)

    chem = "hexanedioic acid" ; casrn = "124-04-9"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E8*Hcp_TO_HcpSI)

    chem = "{cis}-butenedioic acid" ; casrn = "110-16-7"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E9*Hcp_TO_HcpSI)

    chem = "2-buten-1-ol" ; casrn = "6117-91-5"
    CALL MakeNote(TRIM(ref))
    CALL Output(3.0E2*Hcp_TO_HcpSI)

    chem = "2,2-dimethyl-1-propanol" ; casrn = "75-84-3"
    CALL MakeNote(TRIM(ref))
    CALL Output(5E1*Hcp_TO_HcpSI)

    chem = "(hydroxymethyl)benzene" ; casrn = "100-51-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(9E3*Hcp_TO_HcpSI)

    chem = "1,2-propanediol" ; casrn = "57-55-6"
    CALL MakeNote(TRIM(ref))
    CALL MakeNote_range(1E5*Hcp_TO_HcpSI,6E6*Hcp_TO_HcpSI)
    CALL Output(DUMMY)

    chem = "1,2,3-propanetriol" ; casrn = "56-81-5"
    CALL MakeNote(TRIM(ref))
    CALL MakeNote_range(6E8*Hcp_TO_HcpSI,4E11*Hcp_TO_HcpSI)
    CALL Output(DUMMY)

    chem = "1,3-butanediol" ; casrn = "107-88-0"
    CALL MakeNote(TRIM(ref))
    CALL Output(5E6*Hcp_TO_HcpSI)

    chem = "1,4-butanediol" ; casrn = "110-63-4"
    CALL MakeNote(TRIM(ref))
    CALL MakeNote_range(1E5*Hcp_TO_HcpSI,5E6*Hcp_TO_HcpSI)
    CALL Output(DUMMY)

    chem = "2,3-butanediol" ; casrn = "513-85-9"
    CALL MakeNote(TRIM(ref))
    CALL MakeNote_range(4E4*Hcp_TO_HcpSI,4E6*Hcp_TO_HcpSI)
    CALL Output(DUMMY)

    chem = "1,2,3-butanetriol" ; casrn = "4435-50-1"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E11*Hcp_TO_HcpSI)

    chem = "1,2,4-butanetriol" ; casrn = "3068-00-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E11*Hcp_TO_HcpSI)

    chem = "1,2,3,4-butanetetrol" ; casrn = "_CAS-82"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E16*Hcp_TO_HcpSI)

    chem = "1,5-pentanediol" ; casrn = "111-29-5"
    CALL MakeNote(TRIM(ref))
    CALL Output(4E6*Hcp_TO_HcpSI)

    chem = "2,3-pentanediol" ; casrn = "42027-23-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E6*Hcp_TO_HcpSI)

    chem = "2,4-pentanediol" ; casrn = "625-69-4"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E6*Hcp_TO_HcpSI)

    chem = "1,2,3,4,5-pentanepentol" ; casrn = "_CAS-81"
    CALL MakeNote(TRIM(ref))
    CALL Output(9E20*Hcp_TO_HcpSI)

    chem = "1,6-hexanediol" ; casrn = "629-11-8"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E6*Hcp_TO_HcpSI)

    chem = "2,5-hexanediol" ; casrn = "2935-44-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E6*Hcp_TO_HcpSI)

    chem = "2-methyl-1,3-pentanediol" ; casrn = "149-31-5"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E6*Hcp_TO_HcpSI)

    chem = "2-methyl-2,4-pentanediol" ; casrn = "107-41-5"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E6*Hcp_TO_HcpSI)

    chem = "1,2,6-hexanetriol" ; casrn = "106-69-4"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E11*Hcp_TO_HcpSI)

    chem = "1,2,3,4,5,6-hexahydroxy hexane" ; casrn = "_CAS-83"
    CALL MakeNote(TRIM(ref))
    CALL Output(4E25*Hcp_TO_HcpSI)

    chem = "1,2,4,5-cyclohexanetetrol" ; casrn = "35652-37-0"
    CALL MakeNote(TRIM(ref))
    CALL Output(4E16*Hcp_TO_HcpSI)

    chem = "1,2,3,4,5,6-hexahydroxycyclohexane" ; casrn = "87-89-8"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E26*Hcp_TO_HcpSI)

    chem = "1,7-heptanediol" ; casrn = "629-30-1"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E6*Hcp_TO_HcpSI)

    chem = "2,4-heptanediol" ; casrn = "20748-86-1"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E6*Hcp_TO_HcpSI)

    chem = "2,2-diethyl-1,3-propanediol" ; casrn = "115-76-4"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E6*Hcp_TO_HcpSI)

    chem = "2-ethyl-1,3-hexanediol" ; casrn = "94-96-2"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E6*Hcp_TO_HcpSI)

    chem = "1,2,3,4,5-pentahydroxy heptane" ; casrn = "_CAS-01"
    CALL MakeNote(TRIM(ref))
    CALL Output(5E20*Hcp_TO_HcpSI)

    chem = "1,2,3,4,6-pentahydroxy heptane" ; casrn = "_CAS-02"
    CALL MakeNote(TRIM(ref))
    CALL Output(4E20*Hcp_TO_HcpSI)

    chem = "1,2,3,5,7-pentahydroxy heptane" ; casrn = "_CAS-03"
    CALL MakeNote(TRIM(ref))
    CALL Output(5E20*Hcp_TO_HcpSI)

    chem = "1,2,3,4,5,6-hexahydroxy heptane" ; casrn = "688007-16-1"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E25*Hcp_TO_HcpSI)

    chem = "3-oxapentane-1,5-diol" ; casrn = "111-46-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E9*Hcp_TO_HcpSI)

    chem = "3,6-dioxaoctane-1,8-diol" ; casrn = "112-27-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(9E11*Hcp_TO_HcpSI)

    chem = "oxoethanoic acid" ; casrn = "298-12-4" ! OHCCOOH glyoxylic acid
    CALL MakeNote(TRIM(ref))
    CALL Output(9E3*Hcp_TO_HcpSI)

    chem = "3-oxopropanoic acid" ; casrn = "926-61-4"
    CALL MakeNote(TRIM(ref))
    CALL Output(7E3*Hcp_TO_HcpSI)

    chem = "4-oxobutanoic acid" ; casrn = "692-29-5"
    CALL MakeNote(TRIM(ref))
    CALL Output(5E3*Hcp_TO_HcpSI)

    chem = "5-oxopentanoic acid" ; casrn = "5746-02-1"
    CALL MakeNote(TRIM(ref))
    CALL Output(4E3*Hcp_TO_HcpSI)

    chem = "ethyl methyl ether" ; casrn = "540-67-0"
    CALL MakeNote(TRIM(ref))
    CALL Output(9E-1*Hcp_TO_HcpSI)

    chem = "hydroxybutanedioic acid" ; casrn = "6915-15-7"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E13*Hcp_TO_HcpSI)

    chem = "2-hydroxy-1,2,3-propanetricarboxylic acid" ; casrn = "77-92-9"
    CALL MakeNote(TRIM(ref))
    CALL Output(3E18*Hcp_TO_HcpSI)

    chem = "2-oxopentanedioic acid" ; casrn = "328-50-7"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E9*Hcp_TO_HcpSI)

    chem = "2-hydroxypropanoic acid" ; casrn = "50-21-5"
    CALL MakeNote(TRIM(ref))
    CALL Output(7E7*Hcp_TO_HcpSI)

    chem = "2,3-dihydroxybutanedioic acid" ; casrn = "87-69-4"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E18*Hcp_TO_HcpSI)

    chem = "2,3-dihydroxypropanal" ; casrn = "367-47-5"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E10*Hcp_TO_HcpSI)

    chem = "glutamic acid" ; casrn = "617-65-2"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E13*Hcp_TO_HcpSI)

    chem = "asparagine" ; casrn = "70-47-3"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E13*Hcp_TO_HcpSI)

    chem = "serine" ; casrn = "302-84-1"
    CALL MakeNote(TRIM(ref))
    CALL Output(4E12*Hcp_TO_HcpSI)

    chem = "glutamine" ; casrn = "56-85-9"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E13*Hcp_TO_HcpSI)

    chem = "glycine" ; casrn = "56-40-6"
    CALL MakeNote(TRIM(ref))
    CALL Output(9E7*Hcp_TO_HcpSI)

    chem = "arginine" ; casrn = "74-79-3"
    CALL MakeNote(TRIM(ref))
    CALL Output(1E17*Hcp_TO_HcpSI)

    chem = "alanine" ; casrn = "302-72-7"
    CALL MakeNote(TRIM(ref))
    CALL Output(6E7*Hcp_TO_HcpSI)

    chem = "leucine" ; casrn = "328-39-2"
    CALL MakeNote(TRIM(ref))
    CALL Output(2E7*Hcp_TO_HcpSI)

  END SUBROUTINE ref0588

  !---------------------------------------------------------------------------

  SUBROUTINE ref0592
    IMPLICIT NONE

    ref = "592"

    chem = "nitrous acid" ; casrn = "7782-77-6"
    type = "M"
    CALL Output(49.*Hcp_TO_HcpSI, 9.7*kcal/Rgas)

  END SUBROUTINE ref0592

  !---------------------------------------------------------------------------

  SUBROUTINE ref0598 ! KHcc [1]
    IMPLICIT NONE

    ref = "598"
    type = "M"
    CALL CalcH("CHCl3",                  "67-66-3",  9.154, 3051.)
    CALL CalcH("CCl4",                   "56-23-5",  5.569, 1639.)
    CALL CalcH("trichloroethene",        "79-01-6",  6.664, 2141.)
    CALL CalcH("1,2,3-trichloropropane", "96-18-4",  3.351, 1606.)
    CALL CalcH("tetrachloroethene",      "127-18-4", 7.481, 2279.)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, A, B)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: A, B
      REAL                         :: KHcc

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      KHcc = 10.**(A-B/T0)
      Hominus = KHcc_TIMES_HcpSI_atT0/KHcc
      mindHR = LOG(10.)*B+T0 ! see ref958, eqn (34) why T0 is added to mindHR
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0598

  !---------------------------------------------------------------------------

  SUBROUTINE ref0599 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "599"
    type = "M"

    CALL CalcH("benzene",                "71-43-2", &
      (/25.,30.,40.,45.,50./),(/5.28E-3,6.76E-3,8.78E-3,1.22E-2,1.43E-2/))
    CALL CalcH("methylbenzene",         "108-88-3", &
      (/25.,30.,40.,45.,50./),(/6.43E-3,8.38E-3,1.12E-2,1.42E-2,1.55E-2/))
    CALL CalcH("ethylbenzene",          "100-41-4", &
      (/25.,30.,40./),        (/7.78E-3,1.02E-2,1.64E-2/))
    ! not used: MPX because it is an isomer mix (m- and p-xylene)
    CALL CalcH("1,2-dimethylbenzene",    "95-47-6", &
      (/25.,30.,40.,45.,50./),(/4.99E-3,6.28E-3,1.09E-2,1.06E-2,1.16E-2/))
    CALL CalcH("1,1,1-trichloroethane",  "71-55-6", &
      (/25.,30.,40.,45.,50./),(/1.76E-2,2.18E-2,2.64E-2,3.55E-2,4.11E-2/))
    CALL CalcH("trichloroethene",        "79-01-6", &
      (/25.,30.,40.,45.,50./),(/1.03E-2,1.31E-2,1.65E-2,2.24E-2,2.63E-2/))
    CALL CalcH("tetrachloroethene",     "127-18-4", &
      (/25.,30.,40.,45./),    (/1.70E-2,2.31E-2,3.03E-2,3.81E-2/))
    CALL CalcH("Me t-Bu ether",        "1634-04-4", &
      (/25.,30.,40.,45.,50./),(/5.28E-4,1.19E-3,2.21E-3,3.63E-3,4.08E-3/))

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, temp_, KH_)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, KH_

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      ndata = SIZE(KH_)
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = temp_ + CtoK
      Harray = 1./(KH_*atm)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0599

  !---------------------------------------------------------------------------

  SUBROUTINE ref0601 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "601"

    CALL CalcH("tetrachloroethene",     "127-18-4", 13.12,  5119.)
    CALL CalcH("1,1,1-trichloroethane",  "71-55-6", 10.21,  4262.)
    CALL CalcH("trichloroethene",        "79-01-6", 11.94,  4929.)
    CALL CalcH("trichloromethane",       "67-66-3",  8.553, 4180.)
    CALL CalcH("dichloromethane",        "75-09-2",  8.200, 4191.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, a, b)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: a, b

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      type   = "M"
      Hominus    = 1. / (atm*EXP(a-b/T0))
      mindHR = b
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0601

  !---------------------------------------------------------------------------

  SUBROUTINE ref0608 ! KHcc [1]
    IMPLICIT NONE

    ref = "608"
    type = "M"

    CALL CalcH("trichlorofluoromethane", "75-69-4", 2372.,  9.25) ! CFCl3 R11
    CALL CalcH("iodomethane",            "74-88-4", 3541., 10.34) ! CH3I methyl iodide
    CALL CalcH("trichloromethane",       "67-66-3", 3649., 10.63) ! CHCl3 chloroform
    CALL CalcH("1,1,1-trichloroethane",  "71-55-6", 2915.,  9.15) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("tetrachloromethane",     "56-23-5", 2918.,  9.77) ! CCl4 carbontetrachloride

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Tdep, A)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: Tdep, A
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ! see ref958, eqn (34) why T0 is added to mindHR
      mindHR = Tdep + T0
      Hominus    = KHcc_TIMES_HcpSI_atT0 / EXP(A-Tdep/T0)
      IF (casrn_=="67-66-3") CALL MakeNote("seawater")
      CALL MakeNote(TRIM(ref),"Probably an interpolation of the data from "// &
        "\citet{2952}.")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0608

  !---------------------------------------------------------------------------

  SUBROUTINE ref0615 ! KHpx [MPa]
    IMPLICIT NONE
    REAL :: a, b, c, d, e

    ref = "615"

    chem = "hydrogen sulfide" ; casrn = "7783-06-4"
    type = "L"
    a = -3.3747
    b =  0.072437
    c = -1.10765e-4
    d = -1549.159
    e =  0.144237
    Hominus = cH2O / (1E6 * EXP(a+b*T0+c*T0**2+d/T0+e*LOG(T0)))
    mindHR = b*T0**2+2.*c*T0**3-d+e*T0
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0615

  !---------------------------------------------------------------------------

  SUBROUTINE ref0617 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "617"
    type = "M"
    ! data from Tables V and VI:

    chem = "propanone" ; casrn = "67-64-1"
    CALL Output(27.0*Hcp_TO_HcpSI, 43.95E3/Rgas)

    chem = "ethanal" ; casrn = "75-07-0"
    CALL Output(13.4*Hcp_TO_HcpSI, 47.15E3/Rgas)

    ! note that in Tab. VI the values 34.1 and 29.4 are interchanged!
    chem = "ethane nitrile" ; casrn = "75-05-8"
    CALL Output(53.3*Hcp_TO_HcpSI, 34.14E3/Rgas)

  END SUBROUTINE ref0617

  !---------------------------------------------------------------------------

  SUBROUTINE ref0619 ! ln(Hbp) [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "619"
    type = "M"
    chem = "trifluoroethanoic acid" ; casrn = "76-05-1" ! CF3COOH
    Hominus = EXP(9.099)*Hbp_TO_HcpSI
    mindHR = 9.328E3
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0619

  !---------------------------------------------------------------------------

  SUBROUTINE ref0620 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE
    REAL :: a, b, d

    ref = "620"

    chem = "ammonia" ; casrn = "7664-41-7"
    type = "M"
    a = -8.09694
    b = 3917.507
    d = -0.00314
    Hominus = EXP(a+b/T0+d*T0) * Hbp_TO_HcpSI
    mindHr = b-d*T0**2
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0620

  !---------------------------------------------------------------------------

  SUBROUTINE ref0622
    IMPLICIT NONE

    ref = "622"
    type = "M"

    chem = "diethyl ether" ; casrn = "60-29-7"
    CALL Output(cH2O/(1E6*4.82))

    chem = "diisopropyl ether" ; casrn = "108-20-3"
    CALL Output(cH2O/(1E6*11.6))

    chem = "benzene" ; casrn = "71-43-2"
    CALL Output(cH2O/(1E6*27.1))

    chem = "methylbenzene" ; casrn = "108-88-3"
    CALL Output(cH2O/(1E6*35.6))

    chem = "trichloroethene" ; casrn = "79-01-6"
    CALL Output(cH2O/(1E6*44.1))

    chem = "cyclohexene" ; casrn = "110-83-8"
    CALL Output(cH2O/(1E6*220.))

  END SUBROUTINE ref0622

  !---------------------------------------------------------------------------

  SUBROUTINE ref0624 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "624"
    type = "M"

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0"
    CALL MakeNoteOtherTemp("293")
    CALL Output(4.1*Hcp_TO_HcpSI)

    chem = "peroxypropionyl nitrate" ; casrn = "5796-89-4"
    CALL MakeNoteOtherTemp("293")
    CALL Output(2.9*Hcp_TO_HcpSI)

    chem = "peroxy-n-butyryl nitrate" ; casrn = "27746-48-1"
    CALL MakeNoteOtherTemp("293")
    CALL Output(2.3*Hcp_TO_HcpSI)

    chem = "peroxy-2-propenoyl nitrate" ; casrn = "88181-75-3"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1.7*Hcp_TO_HcpSI)

    chem = "peroxy-isobutyryl nitrate" ; casrn = "65424-60-4"
    CALL MakeNoteOtherTemp("293")
    CALL Output(1.0*Hcp_TO_HcpSI)

  END SUBROUTINE ref0624

  !---------------------------------------------------------------------------

  SUBROUTINE ref0625 ! Hbp [mol/(kg*bar)]
    IMPLICIT NONE

    ref = "625"

    chem = "nitrous acid" ; casrn = "7782-77-6"
    type = "M"
    CALL Output(47.2*rhoH2O/bar, 4873.)

  END SUBROUTINE ref0625

  !---------------------------------------------------------------------------

  SUBROUTINE ref0626 ! KHpb [atm*kg/mol]
    IMPLICIT NONE
    REAL :: A, B, C, D

    ! (data in Tab 2 was calculatd using eqn on p. 5755)
    ref = "626"
    type = "M"
    chem = "molecular bromine" ; casrn = "7726-95-6" ! Br2
    A = 49.707
    B = 0.01701
    C = -18.45
    D = -2676.6
    Hominus = 10.**(A + B*T0 + C*LOG10(T0) + D/T0)
    Hominus = (1./Hominus)*Hbp_TO_HcpSI
    mindHR = LOG(10.)*B*T0*T0 + C*T0 - LOG(10.)*D ! d(lnH)/d(1/T) = -delta H/R
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0626

  !---------------------------------------------------------------------------

  SUBROUTINE ref0627 ! KHpb [atm*kg/mol]
    IMPLICIT NONE

    ref = "627"

    chem = "molecular bromine" ; casrn = "7726-95-6"
    type = "M"
    CALL Output(Hbp_TO_HcpSI/1.45)

  END SUBROUTINE ref0627

  !---------------------------------------------------------------------------

  SUBROUTINE ref0628 ! Hcc [1]
    IMPLICIT NONE

    ref = "628"

    CALL CalcH("methyl ethanoate",  "79-20-9",  5.27, 39.2)
    CALL CalcH("ethyl ethanoate",  "141-78-6",  6.94, 41.4)
    CALL CalcH("propyl ethanoate", "109-60-4",  8.91, 43.2)
    CALL CalcH("butyl ethanoate",  "123-86-4", 11.5,  47.6)
    CALL CalcH("pentyl ethanoate", "628-63-7", 14.5,  51.4)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, K, DeltaE)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: K, DeltaE

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      type   = "M"
      Hominus    = KHcc_TIMES_HcpSI_atT0 / (1E-3*K)
      mindHR = 1E3*DeltaE/Rgas + T0 ! see ref958, eqn (34) why T0 is added
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0628

  !---------------------------------------------------------------------------

  SUBROUTINE ref0629 ! KHpx [Torr]
    IMPLICIT NONE

    ref = "629"

    chem = "ethane nitrile" ; casrn = "75-05-8"
    type = "L"
    mindHR = 3535.
    Hominus = cH2O / (mmHg * 1.25E8*EXP(-mindHR/T0))
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0629

  !---------------------------------------------------------------------------

  SUBROUTINE ref0630 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "630"
    type = "M"

    CALL CalcH("methanal",      "50-00-0", &
      (/10.,25.,30.,35.,40.,45./), (/9.9E3,3.4E3,2.0E3,1.5E3,1.1E3,0.81E3/))
    CALL CalcH("ethanal",       "75-07-0", &
      (/10.,25.,30.,35.,45./),     (/44.5,14.9,12.3,9.6,6.5/))
    CALL CalcH("propanal",     "123-38-6", &
      (/10.,25.,30.,35.,45./),     (/38.9,12.2,9.5,7.2,4.3/))
    CALL CalcH("butanal",      "123-72-8", &
      (/10.,25.,30.,35.,45./),     (/31.3,8.7,6.4,4.9,2.8/))
    CALL CalcH("pentanal",     "110-62-3", &
      (/10.,25.,30.,35.,45./),     (/20.5,6.5,4.2,3.1,1.8/))
    CALL CalcH("hexanal",       "66-25-1", &
      (/10.,25.,30.,35.,45./),     (/15.9,5.2,3.2,2.3,1.3/))
    CALL CalcH("heptanal",     "111-71-7", &
      (/10.,25.,35.,45./),         (/12.4,3.4,1.4,0.7/))
    CALL CalcH("octanal",      "124-13-0", &
      (/10.,25.,35.,45./),         (/7.8,2.3,0.92,0.45/))
    CALL CalcH("nonanal",      "124-19-6", &
      (/10.,25.,35.,45./),         (/3.1,1.2,0.46,0.24/))
    CALL CalcH("decanal",      "112-31-2", &
      (/25.,35.,45./),             (/0.63,0.22,0.1/))
    CALL CalcH("benzaldehyde", "100-52-7", &
      (/10.,25.,30.,35.,45./),     (/92.5,44.4,33.6,25.7,15.8/))
    CALL CalcH("propanone",     "67-64-1", &
      (/10.,25.,30.,35.,45./),     (/71.3,34.6,27.7,22.1,16.4/))
    CALL CalcH("2-butanone",    "78-93-3", &
      (/10.,25.,30.,35.,45./),     (/49.,19.8,14.1,10.9,7.1/))

    chem = "ethanedial"; casrn = "107-22-2"
    CALL MakeNote("RCHOdiol")
    CALL MakeNote("seawater")
    CALL Output(3.6E5*Hcp_TO_HcpSI)

    chem = "propanonal"; casrn =  "78-98-8"
    CALL MakeNote("seawater")
    CALL Output(3.2E4*Hcp_TO_HcpSI)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, temp_, Hominus_)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, Hominus_

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      ndata = SIZE(Hominus_)
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = temp_ + CtoK
      Harray = Hominus_ * Hcp_TO_HcpSI
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL MakeNote(TRIM(ref), &
        "Data from Table 1 by "//TRIM(citet())// &
        " were used to redo the regression "// &
        "analysis. The data for acetone in their Table 2 are incorrect.")
      IF (casrn_=="50-00-0") CALL MakeNote("HCHOdiol")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0630

  !---------------------------------------------------------------------------

  SUBROUTINE ref0631 ! KHpx [MPa]
    IMPLICIT NONE
    REAL :: a, b, c, d

    ref = "631"

    chem = "carbon dioxide" ; casrn = "124-38-9"
    type = "L"

    a = -6.8346
    b =  1.2817E4
    c = -3.7668E6
    d =  2.997E8
    Hominus = cH2O / (1E6 * EXP(a+b/T0+c/T0**2+d/T0**3))
    mindHR = -(b+2*c/T0+3*d/T0**2)
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0631

  !---------------------------------------------------------------------------

  SUBROUTINE ref0632 ! KHpx [atm]
    IMPLICIT NONE
    REAL :: Tdep

    ref = "632"

    chem = "ethane nitrile" ; casrn = "75-05-8"
    type = "M"
    Tdep = 1772.
    mindHR = LOG(10.) * Tdep
    Hominus = KHpx_TIMES_HcpSI / (10.**(5.96-Tdep/T0))
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0632

  !---------------------------------------------------------------------------

  SUBROUTINE ref0633 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "633"
    type = "V"

    CALL CalcH("benzene",                                     "71-43-2", 557.)   ! C6H6
    CALL CalcH("methylbenzene",                              "108-88-3", 680.)   ! C6H5CH3 toluene
    CALL CalcH("ethylbenzene",                               "100-41-4", 887.)   ! C6H5C2H5
    CALL CalcH("1,2-dimethylbenzene",                         "95-47-6", 565.)   ! C6H4(CH3)2 $o$-xylene
    CALL CalcH("1,3-dimethylbenzene",                        "108-38-3", 730.)   ! C6H4(CH3)2 $m$-xylene
    CALL CalcH("1,4-dimethylbenzene",                        "106-42-3", 578.)   ! C6H4(CH3)2 $p$-xylene
    CALL CalcH("1,2,3-trimethylbenzene",                     "526-73-8", 343.)   ! C6H3(CH3)3
    CALL CalcH("1,2,4-trimethylbenzene",                      "95-63-6", 569.)   ! C6H3(CH3)3
    CALL CalcH("1,3,5-trimethylbenzene",                     "108-67-8", 781.)   ! C6H3(CH3)3 mesitylene
    CALL CalcH("propylbenzene",                              "103-65-1", 1040.)  ! C6H5C3H7
    CALL CalcH("(2-propyl)-benzene",                          "98-82-8", 1466.)  ! C6H5C3H7 isopropylbenzene; cumene
    CALL CalcH("1-ethyl-2-methylbenzene",                    "611-14-3", 529.)   ! C6H4CH3C2H5 $o$-ethyltoluene
    CALL CalcH("1-ethyl-4-methylbenzene",                    "622-96-8", 500.)   ! C6H4CH3C2H5 $p$-ethyltoluene
    CALL CalcH("1-methyl-4-(1-methylethyl)-benzene",          "99-87-6", 805.)   ! C{10}H{14} $p$-cymene
    CALL CalcH("butylbenzene",                               "104-51-8", 1332.)  ! C6H5C4H9
    CALL CalcH("(2-methylpropyl)-benzene",                   "538-93-2", 3322.)  ! C6H5C4H9 isobutylbenzene
    CALL CalcH("(1-methylpropyl)-benzene",                   "135-98-8", 1890.)  ! C6H5C4H9 {sec}-butylbenzene
    CALL CalcH("(1,1-dimethylethyl)-benzene",                 "98-06-6", 1280.)  ! C6H5C4H9 {tert}-butylbenzene
    CALL CalcH("1,2,4,5-tetramethylbenzene",                  "95-93-2", 2546.)  ! C6H2(CH3)4
    CALL CalcH("pentylbenzene",                              "538-68-1", 1694.)  ! C6H5C5H{11}
    CALL CalcH("hexylbenzene",                              "1077-16-3", 2165.)  ! C6H5C6H{13}

    CALL CalcH("chlorobenzene",                              "108-90-7", 368.)   ! C6H5Cl
    CALL CalcH("1,2-dichlorobenzene",                         "95-50-1", 244.)   ! C6H4Cl2 $o$-dichlorobenzene
    CALL CalcH("1,3-dichlorobenzene",                        "541-73-1", 376.)   ! C6H4Cl2 $m$-dichlorobenzene
    CALL CalcH("1,4-dichlorobenzene",                        "106-46-7", 160.)   ! C6H4Cl2 $p$-dichlorobenzene
    CALL CalcH("1,2,3-trichlorobenzene",                      "87-61-6", 242.)   ! C6H3Cl3
    CALL CalcH("1,2,4-trichlorobenzene",                     "120-82-1", 277.)   ! C6H3Cl3
    CALL CalcH("1,3,5-trichlorobenzene",                     "108-70-3", 1096.)  ! C6H3Cl3
    CALL CalcH("1,2,3,4-tetrachlorobenzene",                 "634-66-2", 144.)   ! C6H2Cl4
    CALL CalcH("1,2,3,5-tetrachlorobenzene",                 "634-90-2", 588.)   ! C6H2Cl4
    CALL CalcH("1,2,4,5-tetrachlorobenzene",                  "95-94-3", 122.)   ! C6H2Cl4
    CALL CalcH("pentachlorobenzene",                         "608-93-5", 85.)    ! C6HCl5
    CALL CalcH("hexachlorobenzene",                          "118-74-1", 131.)   ! C6Cl6

    CALL CalcH("biphenyl",                                    "92-52-4", 53.5)   ! (C6H5)2
    CALL CalcH("2-chlorobiphenyl",                          "2051-60-7", 70.1)   ! C{12}H9Cl PCB-1
    CALL CalcH("3-chlorobiphenyl",                          "2051-61-8", 75.55)  ! C{12}H9Cl PCB-2
    CALL CalcH("4-chlorobiphenyl",                          "2051-62-9", 42.56)  ! C{12}H9Cl PCB-3
    CALL CalcH("2,2'-dichlorobiphenyl",                    "13029-08-8", 59.17)  ! C{12}H8Cl2 PCB-4
    CALL CalcH("2,4-dichlorobiphenyl",                     "33284-50-3", 45.39)  ! C{12}H8Cl2 PCB-7
    CALL CalcH("2,5-dichlorobiphenyl",                     "34883-39-1", 20.1)   ! C{12}H8Cl2 PCB-9
    CALL CalcH("3,3'-dichlorobiphenyl",                     "2050-67-1", 17.26)  ! C{12}H8Cl2 PCB-11
    CALL CalcH("4,4'-dichlorobiphenyl",                     "2050-68-2", 17.)    ! C{12}H8Cl2 PCB-15
    CALL CalcH("2,2',5-trichlorobiphenyl",                 "37680-65-2", 92.21)  ! C{12}H7Cl3 PCB-18
    CALL CalcH("2,4,5-trichlorobiphenyl",                  "15862-07-4", 24.29)  ! C{12}H7Cl3 PCB-29
    CALL CalcH("2,4,6-trichlorobiphenyl",                  "35693-92-6", 49.51)  ! C{12}H7Cl3 PCB-30
    CALL CalcH("2,3',4'-trichlorobiphenyl",                "38444-86-9", 43.67)  ! C{12}H7Cl3 PCB-33

    CALL CalcH("2,2',3,3'-tetrachlorobiphenyl",            "38444-93-8", 21.94)  ! C{12}H6Cl4 PCB-40
    CALL CalcH("2,2',4,4'-tetrachlorobiphenyl",             "2437-79-8", 17.38)  ! C{12}H6Cl4 PCB-47
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl",            "35693-99-3", 47.59)  ! C{12}H6Cl4 PCB-52
    CALL CalcH("3,3',4,4'-tetrachlorobiphenyl",            "32598-13-3", 1.72)   ! C{12}H6Cl4 PCB-77
    CALL CalcH("2,2',3,4,5-pentachlorobiphenyl",           "55312-69-1", 151.4)  ! C{12}H5Cl5 PCB-86
    CALL CalcH("2,2',3,4,5'-pentachlorobiphenyl",          "38380-02-8", 24.81)  ! C{12}H5Cl5 PCB-87
    CALL CalcH("2,2',4,5,5'-pentachlorobiphenyl",          "37680-73-2", 35.48)  ! C{12}H5Cl5 PCB-101
    CALL CalcH("2,2',4,6,6'-pentachlorobiphenyl",          "56558-16-8", 13.98)  ! C{12}H5Cl5 PCB-104

    CALL CalcH("2,2',3,3',4,4'-hexachlorobiphenyl",        "38380-07-3", 11.91)  ! C{12}H4Cl6 PCB-128
    CALL CalcH("2,2',4,4',5,5'-hexachlorobiphenyl",        "35065-27-1", 42.9)   ! C{12}H4Cl6 PCB-153
    CALL CalcH("2,2',4,4',6,6'-hexachlorobiphenyl",        "33979-03-2", 86.616) ! C{12}H4Cl6 PCB-155
    CALL CalcH("2,2',3,3',4,4',6-heptachlorobiphenyl",     "52663-71-5", 5.4)    ! C{12}H3Cl7 PCB-171
    CALL CalcH("2,2',3,3',5,5',6,6'-octachlorobiphenyl",    "2136-99-4", 38.08)  ! C{12}H2Cl8 PCB-202
    CALL CalcH("2,2',3,3',4,4',5,5',6-nonachlorobiphenyl", "40186-72-9", 82.20)  ! C{12}HCl9 PCB-206
    CALL CalcH("decachlorobiphenyl",                        "2051-24-3", 20.84)  ! C{12}Cl{10} PCB-209

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/HLC)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0633

  !---------------------------------------------------------------------------

  ! ref0634 PAH, polychlorinated dioxins, dibenzofuranes (if I get a
  ! copy of this book, enter new SUBROUTINE here and then deactivate
  ! type X entries from ref3034 with l_output = .FALSE.)

  !---------------------------------------------------------------------------

  SUBROUTINE ref0635 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE
    REAL, PARAMETER :: NA = DUMMY ! not applicable

    ref = "635"

    CALL CalcH("2-methylpropane",                             "75-28-5", 120450.                      ) ! HC(CH3)3 isobutane
    CALL CalcH("dimethylpropane",                            "463-82-1", 220199.                      ) ! C(CH3)4 neopentane
    CALL CalcH("butane",                                     "106-97-8", 95929.                       ) ! C4H{10}
    CALL CalcH("2-methylbutane",                              "78-78-4", 479118.                      ) ! C5H{12} isopentane
    CALL CalcH("2,2-dimethylbutane",                          "75-83-2", 199502.                      ) ! C6H{14}
    CALL CalcH("2,3-dimethylbutane",                          "79-29-8", 144414.                      ) ! C6H{14}
    CALL CalcH("2,2,3-trimethylbutane",                      "464-06-2", 312344.                      ) ! C7H{16}
    CALL CalcH("pentane",                                    "109-66-0", 128183.                      ) ! C5H{12}
    CALL CalcH("2-methylpentane",                            "107-83-5", 176087.                      ) ! C6H{14} isohexane
    CALL CalcH("3-methylpentane",                             "96-14-0", 170320.                      ) ! C6H{14}
    CALL CalcH("2,2-dimethylpentane",                        "590-35-2", 318850.                      ) ! C7H{16}
    CALL CalcH("2,3-dimethylpentane",                        "565-59-3", 175224.                      ) ! C7H{16}
    CALL CalcH("2,4-dimethylpentane",                        "108-08-7", 323338.                      ) ! C7H{16}
    CALL CalcH("3,3-dimethylpentane",                        "562-49-2", 184562.                      ) ! C7H{16}
    CALL CalcH("2,2,4-trimethylpentane",                     "540-84-1", 307110.                      ) ! C8H{18} isooctane
    CALL CalcH("2,3,4-trimethylpentane",                     "565-75-3", 205614.                      ) ! C8H{18}

    CALL CalcH("hexane",                                     "110-54-3", 183225.                      ) ! C6H{14}
    CALL CalcH("2-methylhexane",                             "591-76-4", 346395.                      ) ! C7H{16} isoheptane
    CALL CalcH("3-methylhexane",                             "589-34-4", 249310.                      ) ! C7H{16}
    CALL CalcH("2,2,5-trimethylhexane",                     "3522-94-9", 246482.                      ) ! C9H{20}
    CALL CalcH("heptane",                                    "142-82-5", 208970.                      ) ! C7H{16}
    CALL CalcH("2-methylheptane",                            "592-27-8", 349409.                      ) ! C8H{18}
    CALL CalcH("octane",                                     "111-65-9", 311536.                      ) ! C8H{18}
    CALL CalcH("nonane",                                     "111-84-2", 332893.                      ) ! C9H{20}
    CALL CalcH("decane",                                     "124-18-5", 478861.                      ) ! C{10}H{22}
    CALL CalcH("2-methylpropene",                            "115-11-7", 21617.                       ) ! C4H8 isobutene
    CALL CalcH("1-butene",                                   "106-98-9", 25610.                       ) ! C4H8
    CALL CalcH("3-methyl-1-butene",                          "563-45-1", 54669.                       ) ! C5H{10}
    CALL CalcH("1-pentene",                                  "109-67-1", 40283.                       ) ! C5H{10}
    CALL CalcH("2-methyl-1-pentene",                         "763-29-1", 28053.                       ) ! C6H{12}
    CALL CalcH("4-methyl-1-pentene",                         "691-37-2", 63295.                       ) ! C6H{12}
    CALL CalcH("1-hexene",                                   "592-41-6", 41743.                       ) ! C6H{12}
    CALL CalcH("1-heptene",                                  "592-76-7", 40295.                       ) ! C7H{14}
    CALL CalcH("{trans}-2-heptene",                        "14686-13-6", 42222.                       ) ! C7H{14}

    CALL CalcH("1-octene",                                   "111-66-0", 96323.                       ) ! C8H{16}
    CALL CalcH("1-nonene",                                   "124-11-8", 80253.                       ) ! C9H{18}
    CALL CalcH("1-decene",                                   "872-05-9", 302983.                      ) ! C{10}H{20}
    CALL CalcH("1,3-butadiene",                              "106-99-0", 20679.                       ) ! C4H6
    CALL CalcH("2-methyl-1,3-butadiene",                      "78-79-5", 7779.                        ) ! C5H8 isoprene
    CALL CalcH("2,3-dimethyl-1,3-butadiene",                 "513-81-5", 5065.                        ) ! C6H{10}
    CALL CalcH("1,4-pentadiene",                             "591-93-5", 11965.                       ) ! C5H8
    CALL CalcH("1-pentyne",                                  "627-19-0", 2500.                        ) ! C3H7CCH
    CALL CalcH("1-hexyne",                                   "693-02-7", 4139.                        ) ! C4H9CCH
    CALL CalcH("1-heptyne",                                  "628-71-7", 4473.                        ) ! C5H{11}CCH
    CALL CalcH("1-octyne",                                   "629-05-0", 7875.                        ) ! C6H{13}CCH
    CALL CalcH("cyclopentane",                               "287-92-3", 19064.                       ) ! C5H{10}
    CALL CalcH("methylcyclopentane",                          "96-37-7", 36670.                       ) ! C5H9CH3

    CALL CalcH("propylcyclopentane",                        "2040-96-2", 90208.                       ) ! C5H9C3H7
    CALL CalcH("pentylcyclopentane",                        "3741-00-2", 185387.                      ) ! C5H9C5H{11}
    CALL CalcH("1,1,3-trimethylcyclopentane",               "4516-69-2", 159440.                      ) ! C5H7(CH3)3
    CALL CalcH("cyclohexane",                                "110-82-7", 19433.                       ) ! C6H{10}
    CALL CalcH("methylcyclohexane",                          "108-87-2", 43344.                       ) ! C6H{11}CH3
    CALL CalcH("1,2-dimethylcyclohexane",                    "583-57-3", 48122.                       ) ! C6H{10}(CH3)2
    CALL CalcH("{trans}-1,2-dimethylcyclohexane",           "6876-23-9", 77735.                       ) ! C6H{10}(CH3)2
    CALL CalcH("{trans}-1,4-dimethylcyclohexane",           "2207-04-7", 88248.                       ) ! C6H{10}(CH3)2
    CALL CalcH("1,1,3-trimethylcyclohexane",                "3073-66-3", 105557.                      ) ! C9H{18}
    CALL CalcH("cycloheptane",                               "291-64-5", 9590.                        ) ! C7H{14}
    CALL CalcH("cyclooctane",                                "292-64-8", 10696.                       ) ! C8H{16}
    CALL CalcH("(Z)-bicyclo[4.4.0]decane",                   "493-01-6", 2311.                        ) ! C{10}H{18} {cis}-decahydronaphthalene; {cis}-decalin
    CALL CalcH("(E)-bicyclo[4.4.0]decane",                   "493-02-7", 3644.                        ) ! C{10}H{18} {trans}-decahydronaphthalene; {trans}-decalin
    CALL CalcH("cyclopentene",                               "142-29-0", 6456.                        ) ! C5H8
    CALL CalcH("cyclohexene",                                "110-83-8", 4570.                        ) ! C6H{10}
    CALL CalcH("cycloheptene",                               "628-92-2", 4900.                        ) ! C7H{12}

    CALL CalcH("1,4-cyclohexadiene",                         "628-41-1", 1032.                        ) ! C6H8 1,4-dihydrobenzene
    CALL CalcH("1,3,5-cycloheptatriene",                     "544-25-2", 467.                         ) ! C7H8
    CALL CalcH("ethenylbenzene",                             "100-42-5", 305.48                       ) ! C8H8 styrene
    CALL CalcH("1,2,3,4-tetrahydronaphthalene",              "119-64-2", 469.                         ) ! C{10}H{12} tetralin
    CALL CalcH("diphenylmethane",                            "101-81-5", 0.93                         ) ! C{13}H{12} 1,1'-methylenebisbenzene
    CALL CalcH("1,2-diphenylethane",                         "103-29-7", 16.9                         ) ! C{14}H{14} dibenzyl

    CALL CalcH("chloromethane",                               "74-87-3", 977.25                       ) ! CH3Cl methyl chloride
    CALL CalcH("dichloromethane",                             "75-09-2", 168.73,         300.         ) ! CH2Cl2 methylene chloride
    CALL CalcH("trichloromethane",                            "67-66-3", 382.07,         427.         ) ! CHCl3 chloroform
    CALL CalcH("tetrachloromethane",                          "56-23-5", 2932.19,        2989.        ) ! CCl4 carbontetrachloride
    CALL CalcH("chloroethane",                                "75-00-3", 181.11,         1023.        ) ! C2H5Cl
    CALL CalcH("1,1-dichloroethane",                          "75-34-3", 628.18,         633.         ) ! CHCl2CH3
    CALL CalcH("1,2-dichloroethane",                         "107-06-2", 121.20,         143.         ) ! CH2ClCH2Cl
    CALL CalcH("1,1,1-trichloroethane",                       "71-55-6", 1472.42,        1763.        ) ! CH3CCl3 methylchloroform; MCF
    CALL CalcH("1,1,2-trichloroethane",                       "79-00-5", 97.77,          92.2         ) ! CHCl2CH2Cl
    CALL CalcH("1,1,1,2-tetrachloroethane",                  "630-20-6", 241.09                       ) ! CCl3CH2Cl
    CALL CalcH("1,1,2,2-tetrachloroethane",                   "79-34-5", 44.94,          25.7         ) ! CHCl2CHCl2
    CALL CalcH("pentachloroethane",                           "76-01-7", 252.9                        ) ! CHCl2CCl3
    CALL CalcH("hexachloroethane",                            "67-72-1", NA,             846.         ) ! C2Cl6
    CALL CalcH("1-chloropropane",                            "540-54-5", 1408.26                      ) ! C3H7Cl
    CALL CalcH("2-chloropropane",                             "75-29-6", 1832.16                      ) ! C3H7Cl
    CALL CalcH("1,2-dichloropropane",                         "78-87-5", 267.14,         287.         ) ! C3H6Cl2
    CALL CalcH("1,2,3-trichloropropane",                      "96-18-4", 38.26                        ) ! C3H5Cl3
    CALL CalcH("1-chlorobutane",                             "109-69-3", 2062.13,        1537.        ) ! C4H9Cl
    CALL CalcH("2-chlorobutane",                              "78-86-4", 1870.84,        2267.        ) ! C4H9Cl
    CALL CalcH("1-chloropentane",                            "543-59-9", 2229.99,        2375.        ) ! C5H{11}Cl

    CALL CalcH("chloroethene",                                "75-01-4", 8021.17,        2685.        ) ! CH2CHCl vinyl chloride
    CALL CalcH("1,1-dichloroethene",                          "75-35-4", 2333.63,        2624.        ) ! CH2CCl2
    CALL CalcH("(Z)-1,2-dichloroethene",                     "156-59-2", 747.82,         460.         ) ! CHClCHCl {cis}-1,2-dichloroethene
    CALL CalcH("(E)-1,2-dichloroethene",                     "156-60-5", 687.56,         958.         ) ! CHClCHCl {trans}-1,2-dichloroethene
    CALL CalcH("trichloroethene",                             "79-01-6", 1183.70,        1034.        ) ! C2HCl3 trichloroethylene
    CALL CalcH("tetrachloroethene",                          "127-18-4", 2669.86,        1733.        ) ! C2Cl4 tetrachloroethylene
    CALL CalcH("3-chloro-1-propene",                         "107-05-1", 2154.                        ) ! C3H5Cl allyl chloride
    CALL CalcH("2-chloro-1,3-butadiene",                     "126-99-8", 21.30                        ) ! C4H5Cl
    CALL CalcH("hexachlorobutadiene",                         "87-68-3", 1540.                        ) ! CCl2CClCClCCl2
    CALL CalcH("hexachlorocyclopentadiene",                   "77-47-4", 1670.                        ) ! C5Cl6
    CALL CalcH("bromomethane",                                "74-83-9", 632.                         ) ! CH3Br methyl bromide
    CALL CalcH("dibromomethane",                              "74-95-3", 141.25,         86.13        ) ! CH2Br2
    CALL CalcH("tribromomethane",                             "75-25-2", 59.27,          46.61        ) ! CHBr3 bromoform
    CALL CalcH("bromoethane",                                 "74-96-4", 1235.                        ) ! C2H5Br
    CALL CalcH("1,2-dibromoethane",                          "106-93-4", 470.,           65.86        ) ! C2H4Br2 ethylene dibromide
    CALL CalcH("1-bromopropane",                             "106-94-5", 3792.                        ) ! C3H7Br
    CALL CalcH("2-bromopropane",                              "75-26-3", 1273.                        ) ! C3H7Br
    CALL CalcH("1,3-dibromopropane",                         "109-64-8", 896.34                       ) ! C3H6Br2

    CALL CalcH("1-bromo-3-methylbutane",                     "107-82-4", 2061.                        ) ! C5H{11}Br
    CALL CalcH("iodomethane",                                 "74-88-4", 551.66,         541.         ) ! CH3I methyl iodide
    CALL CalcH("diiodomethane",                               "75-11-6", 31.74                        ) ! CH2I2
    CALL CalcH("iodoethane",                                  "75-03-6", 521.14                       ) ! C2H5I
    CALL CalcH("1-iodopropane",                              "107-08-4", 929.22                       ) ! C3H7I
    CALL CalcH("1-iodobutane",                               "542-69-8", 1868.51                      ) ! C4H9I
    CALL CalcH("bromochloromethane",                          "74-97-5", 171.60                       ) ! CH2BrCl
    CALL CalcH("bromodichloromethane",                        "75-27-4", 242.79,         162.         ) ! CHCl2Br
    CALL CalcH("dibromochloromethane",                       "124-48-1", NA,             86.13        ) ! CHClBr2
    CALL CalcH("chlorotrifluoromethane",                      "75-72-9", 6874.                        ) ! CF3Cl R13
    CALL CalcH("chlorodifluoromethane",                       "75-45-6", 3022.                        ) ! CHF2Cl R22
    CALL CalcH("dichlorofluoromethane",                       "75-43-4", 555.,           26639.       ) ! CHFCl2 R21
    CALL CalcH("dichlorodifluoromethane",                     "75-71-8", 40860.                       ) ! CF2Cl2 R12

    CALL CalcH("trichlorofluoromethane",                      "75-69-4", 12900.,         10243.       ) ! CFCl3 R11
    CALL CalcH("1,2-dichlorotetrafluoroethane",               "76-14-2", 126660.                      ) ! C2F4Cl2 R114
    CALL CalcH("1,1,2-trichlorotrifluoroethane",              "76-13-1", 113840.,        32323.       ) ! C2F3Cl3 R113
    CALL CalcH("chloropentafluoroethane",                     "76-15-3", 259810.                      ) ! C2F5Cl R115
    CALL CalcH("fluorobenzene",                              "462-06-6", 704.                         ) ! C6H5F
    CALL CalcH("bromobenzene",                               "108-86-1", 211.40                       ) ! C6H5Br
    CALL CalcH("iodobenzene",                                "591-50-4", 78.00                        ) ! C6H5I
    CALL CalcH("chlorobenzene",                              "108-90-7", 368.,           382.         ) ! C6H5Cl

    CALL CalcH("dimethyl ether",                             "115-10-6", 7.83                         ) ! CH3OCH3
    CALL CalcH("diethyl ether",                               "60-29-7", 87.86                        ) ! C2H5OC2H5
    CALL CalcH("methyl {tert}-butyl ether",                 "1634-04-4", 70.31                        ) ! CH3OC(CH3)3 MTBE
    CALL CalcH("dipropyl ether",                             "111-43-3", 257.6                        ) ! C3H7OC3H7
    CALL CalcH("diisopropyl ether",                          "108-20-3", 258.7                        ) ! C3H7OC3H7
    CALL CalcH("dibutyl ether",                              "142-96-1", 481.3                        ) ! C4H9OC4H9
    CALL CalcH("1-ethoxy-butane",                            "628-81-9", 128.9                        ) ! C6H{14}O ethyl butyl ether
    CALL CalcH("oxirane",                                     "75-21-8", 11.65                        ) ! C2H4O ethylene oxide
    CALL CalcH("1,2-epoxypropane",                            "75-56-9", 8.66                         ) ! C3H6O propyleneoxide
    CALL CalcH("oxacyclopentadiene",                         "110-00-9", 544.6                        ) ! C4H4O furan; furfuran
    CALL CalcH("2-methyltetrahydrofuran",                     "96-47-9", 667.5                        ) ! CH3C4H7O
    CALL CalcH("tetrahydropyran",                            "142-68-7", 9.584                        ) ! C5H{10}O THP
    CALL CalcH("diphenyl ether",                             "101-84-8", 26.67                        ) ! C{12}H{10}O
    CALL CalcH("methoxybenzene",                             "100-66-3", 25.15                        ) ! C6H5OCH3 anisole
    CALL CalcH("phenyloxirane",                               "96-09-3", 1.72                         ) ! C8H8O styrene oxide

    CALL CalcH("(chloromethyl)oxirane",                      "106-89-8", 3.38                         ) ! C3H5ClO epichlorohydrin
    CALL CalcH("bis-(chloromethyl) ether",                   "542-88-1", 20.90                        ) ! C2H4Cl2O
    CALL CalcH("1,5-dichloro-3-oxapentane",                  "111-44-4", 2.888                        ) ! C4H8Cl2O bis-(2-chloroethyl)-ether
    CALL CalcH("bis-(2-chloroisopropyl) ether",              "108-60-1", 10.46                        ) ! C6H{12}Cl2O DCIP
    CALL CalcH("(2-chloroethoxy)-ethene",                    "110-75-8", 25.330                       ) ! C4H7ClO 2-chloroethylvinylether
    CALL CalcH("4-chlorodiphenyl ether",                    "7005-72-3", 22.327                       ) ! C{12}H9ClO PCDE-3
    CALL CalcH("4-bromodiphenyl ether",                      "101-55-3", 10.380                       ) ! C{12}H9BrO PBDE-3
    CALL CalcH("bis-(2-chloroethoxy)-methane",               "111-91-1", 0.462                        ) ! C5H{10}Cl2O2

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, HLC1, HLC2)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC1
      REAL, OPTIONAL,   INTENT(IN) :: HLC2
      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it
      IF (ABS(HLC1-DUMMY) > TINY(0.)) THEN
        type = "V"
        CALL Output(KHpcSI_TIMES_HcpSI/HLC1)
      ENDIF
      IF (PRESENT(HLC2)) THEN
        type = "?"
        CALL Output(KHpcSI_TIMES_HcpSI/HLC2)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref0635

  !---------------------------------------------------------------------------

  SUBROUTINE ref0636 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "636"

    ! Some grey-literature references that are cited here:
    ! hawthorne84 = PhD thesis University of Colorado, Boulder, Colorado
    ! mabey82     = W. R. Mabey, J. H. Smith, R. T. Podoll, H. L. Johnson, T.
    !               Mill, T.-W. Chou, J. Cates, I. Waight Partridge, H. Jaber &
    !               D. Vandenberg
    !               Aquatic Fate Process Data for Organic Priority Pollutants
    !               EPA Report No. 440/4-81-014, US EPA, Office of Water
    !               Regulations and Standards, Washington DC, 1982

    ! chapter 2: alcohols
    CALL CalcH("propanol",           "71-23-8", 0.620,    "496",         "X") ! entrainment-method
    CALL CalcH("propanol",           "71-23-8", 0.683,    "642",         "X") ! exptl
    CALL CalcH("propanol",           "71-23-8", 0.699,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("propanol",           "71-23-8", 0.710,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("propanol",           "71-23-8", 0.751,    "483",         "X") ! headspace-GC
    CALL CalcH("propanol",           "71-23-8", 0.925,    "756",         "X") ! computed
    CALL CalcH("propanol",           "71-23-8", 0.683,    "731",         "X") ! quoted
    CALL CalcH("propanol",           "71-23-8", 0.942,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("isopropanol",        "67-63-0", 0.815,    "713",         "X") ! partial-pressure
    CALL CalcH("isopropanol",        "67-63-0", 0.820,    "642",         "X") ! quoted-exptl
    CALL CalcH("isopropanol",        "67-63-0", 1.159,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("isopropanol",        "67-63-0", 0.710,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("isopropanol",        "67-63-0", 1.131,    "756",         "X") ! computed
    CALL CalcH("isopropanol",        "67-63-0", 0.820,    "731",         "X") ! quoted
    CALL CalcH("isopropanol",        "67-63-0", 0.715,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("1-butanol",          "71-36-3", 0.866,    "713",         "X") ! partial-pressure
    CALL CalcH("1-butanol",          "71-36-3", 0.731,    "496",         "X") ! entrainment-method
    CALL CalcH("1-butanol",          "71-36-3", 0.892,    "754",         "X") ! shake-flask
    CALL CalcH("1-butanol",          "71-36-3", 0.868,    "754",         "X") ! quoted
    CALL CalcH("1-butanol",          "71-36-3", 0.860,    "642",         "X") !
    CALL CalcH("1-butanol",          "71-36-3", 0.964,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("1-butanol",          "71-36-3", 1.057,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("1-butanol",          "71-36-3", 0.564,    "2233",        "X") !
    CALL CalcH("1-butanol",          "71-36-3", 0.80,     "483",         "X") ! headspace-GC
    CALL CalcH("1-butanol",          "71-36-3", 0.860,    "737",         "X") ! quoted
    CALL CalcH("1-butanol",          "71-36-3", 1.010,    "737",         "X") ! calculated
    CALL CalcH("1-butanol",          "71-36-3", 0.860,    "731",         "X") ! quoted
    CALL CalcH("1-butanol",          "71-36-3", 1.160,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("isobutanol",         "78-83-1", 1.20,     "713",         "X") ! partial-pressure
    CALL CalcH("isobutanol",         "78-83-1", 1.214,    "642",         "X") ! quoted-exptl
    CALL CalcH("isobutanol",         "78-83-1", 1.159,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("isobutanol",         "78-83-1", 1.057,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("isobutanol",         "78-83-1", 0.992,    "483",         "X") ! headspace-GC
    CALL CalcH("isobutanol",         "78-83-1", 1.214,    "737",         "X") ! quoted
    CALL CalcH("isobutanol",         "78-83-1", 1.186,    "737",         "X") ! calculated
    CALL CalcH("2-butanol",          "78-92-2", 1.040,    "713",         "X") ! partial-pressure
    CALL CalcH("2-butanol",          "78-92-2", 1.033,    "642",         "X") ! quoted-exptl
    CALL CalcH("2-butanol",          "78-92-2", 1.600,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("2-butanol",          "78-92-2", 1.057,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2-butanol",          "78-92-2", 0.80,     "483",         "X") ! headspace-GC
    CALL CalcH("2-butanol",          "78-92-2", 1.033,    "737",         "X") ! quoted
    CALL CalcH("2-butanol",          "78-92-2", 1.107,    "737",         "X") ! calculated
    CALL CalcH("t-butanol",          "75-65-0", 1.21,     "713",         "X") ! partial-pressure
    CALL CalcH("t-butanol",          "75-65-0", 1.214,    "642",         "X") ! quoted-exptl
    CALL CalcH("t-butanol",          "75-65-0", 1.057,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("t-butanol",          "75-65-0", 1.057,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("t-butanol",          "75-65-0", 1.46,     "483",         "X") ! headspace-GC
    CALL CalcH("t-butanol",          "75-65-0", 1.214,    "737",         "X") ! quoted
    CALL CalcH("t-butanol",          "75-65-0", 1.426,    "737",         "X") ! calculated
    CALL CalcH("pentanol",           "71-41-0", 1.324,    "713",         "X") ! partial-pressure
    CALL CalcH("pentanol",           "71-41-0", 1.271,    "642",         "X") ! quoted-exptl
    CALL CalcH("pentanol",           "71-41-0", 1.426,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("pentanol",           "71-41-0", 1.600,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("pentanol",           "71-41-0", 1.04,     "2233",        "X") ! calculated-p-over-c
    CALL CalcH("pentanol",           "71-41-0", 1.271,    "737",         "X") ! quoted
    CALL CalcH("pentanol",           "71-41-0", 1.271,    "737",         "X") ! calculated
    CALL CalcH("pentanol",           "71-41-0", 1.236,    "756",         "X") ! computed
    CALL CalcH("pentanol",           "71-41-0", 1.271,    "731",         "X") ! quoted
    CALL CalcH("pentanol",           "71-41-0", 1.600,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("hexanol",           "111-27-3", 1.562,    "713",         "X") ! partial-pressure
    CALL CalcH("hexanol",           "111-27-3", 1.735,    "754",         "X") ! shake-flask
    CALL CalcH("hexanol",           "111-27-3", 1.562,    "754",         "X") ! quoted
    CALL CalcH("hexanol",           "111-27-3", 1.562,    "642",         "X") ! quoted-exptl
    CALL CalcH("hexanol",           "111-27-3", 1.880,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("hexanol",           "111-27-3", 2.367,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("hexanol",           "111-27-3", 1.564,    "737",         "X") ! quoted
    CALL CalcH("hexanol",           "111-27-3", 1.600,    "737",         "X") ! calculated
    CALL CalcH("hexanol",           "111-27-3", 1.896,    "756",         "X") ! computed
    CALL CalcH("hexanol",           "111-27-3", 1.564,    "731",         "X") ! quoted
    CALL CalcH("hexanol",           "111-27-3", 2.64,     "731",         "X") ! calculated-molecular-structure
    CALL CalcH("heptanol",          "111-70-6", 1.909,    "713",         "X") ! partial-pressure
    CALL CalcH("heptanol",          "111-70-6", 1.880,    "642",         "X") ! quoted-exptl
    CALL CalcH("heptanol",          "111-70-6", 2.656,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("heptanol",          "111-70-6", 3.583,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("heptanol",          "111-70-6", 1.880,    "737",         "X") ! quoted
    CALL CalcH("heptanol",          "111-70-6", 2.015,    "737",         "X") ! calculated
    CALL CalcH("heptanol",          "111-70-6", 1.880,    "731",         "X") ! quoted
    CALL CalcH("heptanol",          "111-70-6", 5.55,     "731",         "X") ! calculated-molecular-structure
    CALL CalcH("octanol",           "111-87-5", 2.454,    "713",         "X") ! partial-pressure
    CALL CalcH("octanol",           "111-87-5", 2.479,    "754",         "X") ! shake-flask
    CALL CalcH("octanol",           "111-87-5", 2.54,     "754",         "X") ! calculated-p-over-c
    CALL CalcH("octanol",           "111-87-5", 2.422,    "642",         "X") ! quoted-exptl
    CALL CalcH("octanol",           "111-87-5", 3.344,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("octanol",           "111-87-5", 6.085,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("octanol",           "111-87-5", 2.422,    "737",         "X") ! quoted
    CALL CalcH("octanol",           "111-87-5", 2.537,    "737",         "X") ! calculated
    CALL CalcH("octanol",           "111-87-5", 1.609,    "756",         "X") ! computed
    CALL CalcH("ethylene glycol",   "107-21-1", 0.006,    "642",         "X") ! quoted-exptl
    CALL CalcH("ethylene glycol",   "107-21-1", 5.81E-6,  "642",         "X") ! calculated-group-contribution
    CALL CalcH("ethylene glycol",   "107-21-1", 2.37E-6,  "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2-propen-1-ol",     "107-18-6", 0.506,    "642",         "X") ! quoted-exptl
    CALL CalcH("2-propen-1-ol",     "107-18-6", 0.510,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2-propen-1-ol",     "107-18-6", 0.564,    "756",         "X") ! omputed
    CALL CalcH("cyclohexanol",      "108-93-0", 0.581,    "642",         "X") ! quoted-exptl
    CALL CalcH("cyclohexanol",      "108-93-0", 2.48,     "642",         "X") ! calculated-group-contribution
    CALL CalcH("cyclohexanol",      "108-93-0", 2.37,     "642",         "X") ! calculated-bond-contribution
    CALL CalcH("cyclohexanol",      "108-93-0", 0.281,    "howard93",    "X") ! calculated-p-over-c
    CALL CalcH("cyclohexanol",      "108-93-0", 0.581,    "731",         "X") ! quoted
    CALL CalcH("cyclohexanol",      "108-93-0", 0.278,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("benzyl alcohol",    "100-51-6", 0.0231,   "642",         "X") ! quoted-estimated
    CALL CalcH("benzyl alcohol",    "100-51-6", 0.0396,   "howard93",    "X") ! calculated-p-over-c
    ! chapter 3: aldehydes + ketones
    CALL CalcH("methanal",           "50-00-0", 0.0331,   "485",         "X") !
    CALL CalcH("methanal",           "50-00-0", 0.0169,   "715",         "X") ! quoted
    CALL CalcH("ethanal",            "75-07-0", 6.692,    "754",         "X") ! shake-flask
    CALL CalcH("ethanal",            "75-07-0", 8.924,    "754",         "X") ! quoted
    CALL CalcH("ethanal",            "75-07-0", 6.672,    "642",         "X") ! quoted-exptl
    CALL CalcH("ethanal",            "75-07-0", 5.946,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("ethanal",            "75-07-0", 5.423,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("ethanal",            "75-07-0", 6.75,     "715",         "X") ! quoted
    CALL CalcH("ethanal",            "75-07-0", 8.90,     "484",         "X") ! gas-stripping-GC
    CALL CalcH("ethanal",            "75-07-0", 6.80,     "630",         "X") ! gas-stripping-HPLC
    CALL CalcH("ethanal",            "75-07-0", 10.18,    "756",         "X") ! computed
    CALL CalcH("propanal",          "123-38-6", 7.436,    "754",         "X") ! shake-flask
    CALL CalcH("propanal",          "123-38-6", 10.91,    "754",         "X") ! quoted
    CALL CalcH("propanal",          "123-38-6", 7.486,    "642",         "X") ! quoted-exptl
    CALL CalcH("propanal",          "123-38-6", 8.399,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("propanal",          "123-38-6", 8.208,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("propanal",          "123-38-6", 13.77,    "483",         "X") ! headspace-GC
    CALL CalcH("propanal",          "123-38-6", 8.31,     "630",         "X") ! gas-stripping-HPLC
    CALL CalcH("propanal",          "123-38-6", 7.486,    "731",         "X") ! quoted
    CALL CalcH("propanal",          "123-38-6", 13.62,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("butanal",           "123-72-8", 11.650,   "754",         "X") ! shake-flask
    CALL CalcH("butanal",           "123-72-8", 12.890,   "754",         "X") ! quoted
    CALL CalcH("butanal",           "123-72-8", 11.590,   "642",         "X") ! quoted-exptl
    CALL CalcH("butanal",           "123-72-8", 11.594,   "642",         "X") ! calculated-group-contribution
    CALL CalcH("butanal",           "123-72-8", 12.140,   "642",         "X") ! calculated-bond-contribution
    CALL CalcH("butanal",           "123-72-8", 11.65,    "630",         "X") ! gas-stripping-HPLC
    CALL CalcH("butanal",           "123-72-8", 11.59,    "731",         "X") ! quoted
    CALL CalcH("butanal",           "123-72-8", 11.59,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("2-propenal",        "107-02-8", 12.36,    "715",         "X") ! quoted
    CALL CalcH("2-propenal",        "107-02-8", 0.446,    "howard89",    "X") ! quoted
    CALL CalcH("benzaldehyde",      "100-52-7", 2.781,    "642",         "X") ! quoted-exptl
    CALL CalcH("benzaldehyde",      "100-52-7", 1.494,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("benzaldehyde",      "100-52-7", 2.815,    "715",         "X") ! quoted
    CALL CalcH("benzaldehyde",      "100-52-7", 2.71,     "484",         "X") ! gas-stripping-GC
    CALL CalcH("benzaldehyde",      "100-52-7", 2.28,     "630",         "X") ! gas-stripping-HPLC
    CALL CalcH("acetone",            "67-64-1", 3.342,    "713",         "X") ! partial-pressure
    CALL CalcH("acetone",            "67-64-1", 3.960,    "496",         "X") ! shake-flask
    CALL CalcH("acetone",            "67-64-1", 3.97,     "754",         "X") ! shake-flask
    CALL CalcH("acetone",            "67-64-1", 4.140,    "754",         "X") ! quoted
    CALL CalcH("acetone",            "67-64-1", 4.020,    "642",         "X") ! quoted-exptl
    CALL CalcH("acetone",            "67-64-1", 3.929,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("acetone",            "67-64-1", 2.912,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("acetone",            "67-64-1", 4.104,    "2234",        "X") ! calculated
    CALL CalcH("acetone",            "67-64-1", 3.334,    "2234",        "X") ! calculated
    CALL CalcH("acetone",            "67-64-1", 3.648,    "2234",        "X") ! calculated-p-over-c
    CALL CalcH("acetone",            "67-64-1", 3.93,     "483",         "X") ! headspace-GC
    CALL CalcH("acetone",            "67-64-1", 3.378,    "715",         "X") ! quoted
    CALL CalcH("acetone",            "67-64-1", 2.93,     "630",         "X") ! gas-stripping-HPLC
    CALL CalcH("acetone",            "67-64-1", 3.070,    "495",         "X") ! gas-stripping-GC
    CALL CalcH("acetone",            "67-64-1", 4.331,    "756",         "X") ! computed
    CALL CalcH("2-butanone",         "78-93-3", 4.71,     "755",         "X") ! shake-flask
    CALL CalcH("2-butanone",         "78-93-3", 4.723,    "642",         "X") ! quoted-exptl
    CALL CalcH("2-butanone",         "78-93-3", 5.549,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("2-butanone",         "78-93-3", 4.408,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2-butanone",         "78-93-3", 6.191,    "2234",        "X") ! calculated
    CALL CalcH("2-butanone",         "78-93-3", 4.215,    "2234",        "X") ! calculated
    CALL CalcH("2-butanone",         "78-93-3", 4.356,    "hawthorne84", "C") ! gas-stripping
    CALL CalcH("2-butanone",         "78-93-3", 5.76,     "483",         "X") ! headspace-GC
    CALL CalcH("2-butanone",         "78-93-3", 13.17,    "1156",        "X") ! epics
    CALL CalcH("2-butanone",         "78-93-3", 5.21,     "630",         "X") ! gas-stripping-HPLC
    CALL CalcH("2-pentanone",       "107-87-9", 6.44,     "754",         "X") ! partial-pressure
    CALL CalcH("2-pentanone",       "107-87-9", 6.520,    "642",         "X") ! quoted-exptl
    CALL CalcH("2-pentanone",       "107-87-9", 7.660,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("2-pentanone",       "107-87-9", 6.520,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2-pentanone",       "107-87-9", 3.83,     "2234",        "X") ! calculated
    CALL CalcH("2-pentanone",       "107-87-9", 5.34,     "2234",        "X") ! calculated
    CALL CalcH("2-pentanone",       "107-87-9", 3.20,     "2233",        "X") ! calculated-p-over-c
    CALL CalcH("2-pentanone",       "107-87-9", 5.876,    "hawthorne84", "C") ! gas-stripping
    CALL CalcH("2-pentanone",       "107-87-9", 10.13,    "2232",        "X") ! gas-stripping
    CALL CalcH("2-pentanone",       "107-87-9", 6.52,     "731",         "X") ! quoted
    CALL CalcH("2-pentanone",       "107-87-9", 8.021,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("3-pentanone",        "96-22-0", 3.617,    "2234",        "X") ! calculated
    CALL CalcH("3-pentanone",        "96-22-0", 5.340,    "2234",        "X") ! calculated
    CALL CalcH("3-pentanone",        "96-22-0", 8.834,    "howard93",    "X") ! calculated-p-over-c
    CALL CalcH("3-pentanone",        "96-22-0", 8.132,    "",            "V") !
    CALL CalcH("4Me-2-pentanone",   "108-10-1", 39.52,    "1156",        "X") ! epics
    CALL CalcH("4Me-2-pentanone",   "108-10-1", 9.523,    "howard90",    "X") ! calculated-p-over-c
    CALL CalcH("2-hexanone",        "591-78-6", 9.689,    "1145",        "X") ! calculated-p-over-c
    CALL CalcH("2-hexanone",        "591-78-6", 11.76,    "1145",        "X") ! calculated-bond-contribution
    CALL CalcH("2-hexanone",        "591-78-6", 9.695,    "howard93",    "X") ! calculated-p-over-c
    CALL CalcH("2-heptanone",       "110-43-0", 14.63,    "754",         "X") ! partial-pressure
    CALL CalcH("2-heptanone",       "110-43-0", 14.6,     "642",         "X") ! quoted-exptl
    CALL CalcH("2-heptanone",       "110-43-0", 14.94,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("2-heptanone",       "110-43-0", 14.6,     "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2-heptanone",       "110-43-0", 5.715,    "2234",        "X") ! calculated
    CALL CalcH("2-heptanone",       "110-43-0", 8.552,    "2234",        "X") ! calculated
    CALL CalcH("2-heptanone",       "110-43-0", 9.12,     "2233",        "X") ! calculated-p-over-c
    CALL CalcH("2-heptanone",       "110-43-0", 14.6,     "731",         "X") ! quoted
    CALL CalcH("2-heptanone",       "110-43-0", 16.0,     "731",         "X") ! calculated-molecular-structure
    CALL CalcH("cyclohexanone",     "108-94-1", 1.216,    "2232",        "X") !
    CALL CalcH("cyclohexanone",     "108-94-1", 2.266,    "1145",        "X") ! calculated-p-over-c
    CALL CalcH("cyclohexanone",     "108-94-1", 5.179,    "1145",        "X") ! estimated-bond-contribution
    CALL CalcH("acetophenone",       "98-86-2", 1.082,    "642",         "X") ! quoted-exptl
    CALL CalcH("acetophenone",       "98-86-2", 0.784,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("acetophenone",       "98-86-2", 0.921,    "495",         "X") ! gas-stripping-GC
    ! chapter 5: RCOOH
    CALL CalcH("HCOOH",              "64-18-6", 0.017,    "715",         "X") ! quoted
    CALL CalcH("HCOOH",              "64-18-6", 0.112,    "756",         "X") ! computed
    CALL CalcH("HCOOH",              "64-18-6", 0.0183,   "2230",        "X") !
    CALL CalcH("MeCOOH",             "64-19-7", 0.0303,   "713",         "X") ! partial-pressure
    CALL CalcH("MeCOOH",             "64-19-7", 0.0305,   "642",         "X") ! quoted-exptl
    CALL CalcH("MeCOOH",             "64-19-7", 0.030,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("MeCOOH",             "64-19-7", 0.028,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("MeCOOH",             "64-19-7", 0.0101,   "715",         "X") ! effective-pH4
    CALL CalcH("MeCOOH",             "64-19-7", 0.0350,   "737",         "X") ! quoted
    CALL CalcH("MeCOOH",             "64-19-7", 0.0254,   "737",         "X") ! calculated
    CALL CalcH("MeCOOH",             "64-19-7", 0.121,    "756",         "X") ! computed
    CALL CalcH("MeCOOH",             "64-19-7", 0.0183,   "2230",        "X") !
    CALL CalcH("MeCOOH",             "64-19-7", 0.0184,   "2231",        "X") !
    CALL CalcH("MeCOOH",             "64-19-7", 0.0285,   "2231",        "X") !
    CALL CalcH("MeCOOH",             "64-19-7", 0.030,    "731",         "X") ! quoted
    CALL CalcH("MeCOOH",             "64-19-7", 0.0431,   "731",         "X") ! calculated-molecular-structure
    CALL CalcH("EtCOOH",             "79-09-4", 0.0445,   "713",         "X") ! partial-pressure
    CALL CalcH("EtCOOH",             "79-09-4", 0.045,    "642",         "X") ! quoted-exptl
    CALL CalcH("EtCOOH",             "79-09-4", 0.042,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("EtCOOH",             "79-09-4", 0.043,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("EtCOOH",             "79-09-4", 0.0451,   "737",         "X") ! quoted
    CALL CalcH("EtCOOH",             "79-09-4", 0.0298,   "737",         "X") ! calculated
    CALL CalcH("EtCOOH",             "79-09-4", 0.018,    "2230",        "X") !
    CALL CalcH("EtCOOH",             "79-09-4", 0.018,    "2231",        "X") !
    CALL CalcH("EtCOOH",             "79-09-4", 0.0431,   "2231",        "X") !
    CALL CalcH("PrCOOH",            "107-92-6", 0.0542,   "713",         "X") ! partial-pressure
    CALL CalcH("PrCOOH",            "107-92-6", 0.0542,   "642",         "X") ! quoted-exptl
    CALL CalcH("PrCOOH",            "107-92-6", 0.0590,   "642",         "X") ! calculated-group-contribution
    CALL CalcH("PrCOOH",            "107-92-6", 0.0650,   "642",         "X") ! calculated-bond-contribution
    CALL CalcH("PrCOOH",            "107-92-6", 0.0542,   "737",         "X") ! quoted
    CALL CalcH("PrCOOH",            "107-92-6", 0.0375,   "737",         "X") ! calculated
    CALL CalcH("PrCOOH",            "107-92-6", 0.0222,   "2230",        "X") !
    CALL CalcH("PrCOOH",            "107-92-6", 0.0222,   "2231",        "X") !
    CALL CalcH("PrCOOH",            "107-92-6", 0.0654,   "2231",        "X") !
    CALL CalcH("PrCOOH",            "107-92-6", 0.0542,   "731",         "X") ! quoted
    CALL CalcH("PrCOOH",            "107-92-6", 0.358,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("BuCOOH",            "109-52-4", 0.0478,   "2230",        "X") !
    CALL CalcH("BuCOOH",            "109-52-4", 0.0478,   "2231",        "X") !
    CALL CalcH("BuCOOH",            "109-52-4", 0.0618,   "2231",        "X") !
    CALL CalcH("BuCOOH",            "109-52-4", 0.0989,   "2231",        "X") !
    CALL CalcH("PeCOOH",            "142-62-1", 0.0768,   "2230",        "X") !
    CALL CalcH("PeCOOH",            "142-62-1", 0.0768,   "2231",        "X") !
    CALL CalcH("PeCOOH",            "142-62-1", 0.0873,   "2231",        "X") !
    CALL CalcH("PeCOOH",            "142-62-1", 0.149,    "2231",        "X") !
    CALL CalcH("benzoic acid",       "65-85-0", 0.00709,  "howard89",    "X") ! quoted
    CALL CalcH("benzoic acid",       "65-85-0", 0.00575,  "1145",        "X") ! calculated-p-over-c
    CALL CalcH("benzoic acid",       "65-85-0", 0.0110,   "1145",        "X") ! calculated-bond-contribution
    CALL CalcH("benzoic acid",       "65-85-0", 4.15E-3,  "756",         "X") ! computed
    CALL CalcH("phenylaceticacid",  "103-82-2", 0.0056,   "",            "V") !
    CALL CalcH("salicylic acid",     "69-72-7", 0.00144,  "",            "V") !
    CALL CalcH("diClphenoxyacacid",  "94-75-7", 0.55,     "1157",        "X") ! calculated-p-over-c
    CALL CalcH("diClphenoxyacacid",  "94-75-7", 1.39E-5,  "howard91b",   "X") ! quoted
    CALL CalcH("diClphenoxyacacid",  "94-75-7", 1.03E-3,  "howard91b",   "X") ! calculated-bond-contribution
    ! chapter 7: N+S
    CALL CalcH("acetonitrile",       "75-05-8", 3.501,    "642",         "X") ! exptl
    CALL CalcH("acetonitrile",       "75-05-8", 2.781,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("acetonitrile",       "75-05-8", 2.033,    "756",         "X") ! computed
    CALL CalcH("propionitrile",     "107-12-0", 3.80,     "713",         "X") ! partial-pressure
    CALL CalcH("propionitrile",     "107-12-0", 3.752,    "642",         "X") ! quoted exptl
    CALL CalcH("propionitrile",     "107-12-0", 3.752,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("propionitrile",     "107-12-0", 4.114,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("propionitrile",     "107-12-0", 5.947,    "howard90",    "X") ! quoted
    CALL CalcH("acrylonitrile",     "107-13-1", 8.918,    "mabey82",     "C") ! calculated-p-over-c
    CALL CalcH("acrylonitrile",     "107-13-1", 9.420,    "2235",        "X") ! quoted
    CALL CalcH("benzonitrile",      "100-47-0", 55.32,    "756",         "X") ! computed
    CALL CalcH("benzonitrile",      "100-47-0", 5.311,    "",            "V") !
    CALL CalcH("dimethylamine",     "124-40-3", 1.796,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("dimethylamine",     "124-40-3", 1.030,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("dimethylamine",     "124-40-3", 1.80,     "731",         "X") ! quoted
    CALL CalcH("dimethylamine",     "124-40-3", 2.718,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("trimethylamine",     "75-50-3", 6.672,    "642",         "X") ! quoted-exptl
    CALL CalcH("trimethylamine",     "75-50-3", 12.713,   "642",         "X") ! calculated-group-contribution
    CALL CalcH("trimethylamine",     "75-50-3", 2.159,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("trimethylamine",     "75-50-3", 10.57,    "731",         "X") ! quoted
    CALL CalcH("trimethylamine",     "75-50-3", 15.64,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("ethylamine",         "75-04-7", 1.012,    "713",         "X") ! partial-pressure
    CALL CalcH("ethylamine",         "75-04-7", 0.683,    "642",         "X") ! quoted-exptl
    CALL CalcH("ethylamine",         "75-04-7", 0.859,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("ethylamine",         "75-04-7", 0.730,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("ethylamine",         "75-04-7", 1.033,    "731",         "X") ! quoted
    CALL CalcH("ethylamine",         "75-04-7", 0.421,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("diethylamine",      "109-89-7", 2.537,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("diethylamine",      "109-89-7", 2.370,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("propylamine",       "107-10-8", 1.274,    "713",         "X") ! partial-pressure
    CALL CalcH("propylamine",       "107-10-8", 0.784,    "642",         "X") ! quoted-exptl
    CALL CalcH("propylamine",       "107-10-8", 0.732,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("propylamine",       "107-10-8", 1.242,    "731",         "X") ! quoted
    CALL CalcH("propylamine",       "107-10-8", 0.637,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("butylamine",        "109-73-9", 1.526,    "713",         "X") ! partial-pressure
    CALL CalcH("butylamine",        "109-73-9", 1.528,    "642",         "X") ! quoted-exptl
    CALL CalcH("butylamine",        "109-73-9", 1.676,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("butylamine",        "109-73-9", 1.680,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("butylamine",        "109-73-9", 1.528,    "731",         "X") ! quoted
    CALL CalcH("butylamine",        "109-73-9", 0.880,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("aniline",            "62-53-3", 12.16,    "howard89",    "X") ! measured yoshida83 quoted
    CALL CalcH("2-chloroaniline",    "95-51-2", 0.425,    "1145",        "X") ! calculated-p-over-c
    CALL CalcH("2-chloroaniline",    "95-51-2", 0.143,    "1145",        "X") ! estimated-bond-contribution
    CALL CalcH("4-chloroaniline",   "106-47-8", 1.084,    "howard89",    "X") ! calculated-p-over-c
    CALL CalcH("4-chloroaniline",   "106-47-8", 0.0395,   "1145",        "X") ! calculated-p-over-c
    CALL CalcH("4-chloroaniline",   "106-47-8", 0.143,    "1145",        "X") ! estimated-bond-contribution
    CALL CalcH("o-toluidine",        "95-53-4", 0.095,    "",            "V") !
    CALL CalcH("diphenylamine",     "122-39-4", 0.285,    "howard91",    "X") ! calculated-p-over-c
    CALL CalcH("diphenylamine",     "122-39-4", 0.106,    "1145",        "X") ! estimated-bond-contribution
    CALL CalcH("benzidine",          "92-87-5", 0.0394,   "mabey82",     "C") ! calculated-p-over-c-at12C
    CALL CalcH("3,3-diClbenzidine",  "91-94-1", 0.0811,   "mabey82",     "C") ! calculated-p-over-c
    CALL CalcH("nitrobenzene",       "98-95-3", 2.367,    "642",         "X") ! exptl
    CALL CalcH("nitrobenzene",       "98-95-3", 4.511,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("nitrobenzene",       "98-95-3", 4.723,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("nitrobenzene",       "98-95-3", 1.327,    "mabey82",     "C") ! calculated-p-over-c
    CALL CalcH("nitrobenzene",       "98-95-3", 2.367,    "731",         "X") ! quoted
    CALL CalcH("nitrobenzene",       "98-95-3", 5.06,     "731",         "X") ! calculated-molecular-structure
    CALL CalcH("2-nitrotoluene",     "88-72-2", 5.811,    "642",         "X") ! quoted-exptl
    CALL CalcH("2-nitrotoluene",     "88-72-2", 4.723,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("2-nitrotoluene",     "88-72-2", 4.616,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2,4dinitrotoluene", "121-14-2", 0.0160,   "2236",        "X") ! calculated-p-over-c
    CALL CalcH("2,4dinitrotoluene", "121-14-2", 0.456,    "mabey82",     "C") ! calculated-p-over-c
    CALL CalcH("2,4dinitrotoluene", "121-14-2", 32.229,   "2235",        "X") ! quoted
    CALL CalcH("2,6dinitrotoluene", "606-20-2", 0.800,    "mabey82",     "C") ! calculated-p-over-c
    CALL CalcH("2,6dinitrotoluene", "606-20-2", 32.23,    "2235",        "X") ! quoted
    CALL CalcH("diMenitrosoamine",   "62-75-9", 3.344,    "mabey82",     "C") ! calculated
    CALL CalcH("diNPrnitrosoamine", "621-64-7", 0.638,    "mabey82",     "C") ! calculated-p-over-c
    CALL CalcH("diPh-nitrosoamine",  "86-30-6", 66.87,    "mabey82",     "C") ! calculated-p-over-c
    CALL CalcH("pyridine",          "110-86-1", 0.895,    "719",         "X") ! volatility-ratio-transpiration-method
    CALL CalcH("pyridine",          "110-86-1", 0.900,    "642",         "X") ! quoted-exptl
    CALL CalcH("pyridine",          "110-86-1", 0.595,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("pyridine",          "110-86-1", 0.766,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("pyridine",          "110-86-1", 1.317,    "2232",        "X") ! quoted
    CALL CalcH("pyridine",          "110-86-1", 1.114,    "2232",        "X") ! modified-gas-stripping
    CALL CalcH("pyridine",          "110-86-1", 1.120,    "756",         "X") ! computed
    CALL CalcH("pyridine",          "110-86-1", 0.900,    "731",         "X") ! quoted
    CALL CalcH("pyridine",          "110-86-1", 0.305,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("2-methylpyridine",  "109-06-8", 1.01,     "719",         "X") ! volatility-ratio-transpiration-method
    CALL CalcH("2-methylpyridine",  "109-06-8", 0.821,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("2-methylpyridine",  "109-06-8", 0.749,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("2-methylpyridine",  "109-06-8", 2.900,    "756",         "X") ! computed
    CALL CalcH("3-methylpyridine",  "108-99-6", 0.788,    "719",         "X") ! volatility-ratio-transpiration-method
    CALL CalcH("3-methylpyridine",  "108-99-6", 0.784,    "642",         "X") ! quoted-exptl
    CALL CalcH("3-methylpyridine",  "108-99-6", 0.637,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("3-methylpyridine",  "108-99-6", 0.749,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("3-methylpyridine",  "108-99-6", 1.836,    "756",         "X") ! computed
    CALL CalcH("2,3-dimepyridine",  "583-61-9", 0.725,    "719",         "X") ! volatility-ratio-transpiration-method
    CALL CalcH("2,3-dimepyridine",  "583-61-9", 0.732,    "642",         "X") ! quoted-exptl
    CALL CalcH("2,3-dimepyridine",  "583-61-9", 0.859,    "642",         "X") ! calculated-group-contribution
    CALL CalcH("2,3-dimepyridine",  "583-61-9", 0.732,    "642",         "X") ! calculated-bond-contribution
    CALL CalcH("quinoline",          "91-22-5", 0.0253,   "smith80",     "X") ! calculated-p-over-c
    CALL CalcH("quinoline",          "91-22-5", 0.168,    "1145",        "X") ! calculated-p-over-c
    CALL CalcH("quinoline",          "91-22-5", 0.0697,   "1145",        "X") ! estimated-bond-contribution
    CALL CalcH("carbazole",          "86-74-8", 16.0,     "smith80",     "X") ! calculated-p-over-c
    CALL CalcH("ethanethiol",        "75-08-1", 278.13,   "642",         "X") ! quoted-exptl
    CALL CalcH("ethanethiol",        "75-08-1", 298.02,   "642",         "X") ! calculated-group-contribution
    CALL CalcH("ethanethiol",        "75-08-1", 366.64,   "642",         "X") ! calculated-bond-contribution
    CALL CalcH("ethanethiol",        "75-08-1", 360.3,    "522",         "X") !
    CALL CalcH("ethanethiol",        "75-08-1", 292.37,   "756",         "X") ! computed
    CALL CalcH("ethanethiol",        "75-08-1", 278.,     "731",         "X") ! quoted
    CALL CalcH("ethanethiol",        "75-08-1", 96.44,    "731",         "X") ! calculated-molecular-structure
    CALL CalcH("1-butanethiol",     "109-79-5", 460.7,    "522",         "X") !
    CALL CalcH("benzo[b]thiophene",  "95-15-8", 28.0,     "smith80",     "X") ! calculated-p-over-c
    ! from printed copies:
    ! Table 2.2:
    CALL CalcH("1-butanol",                "71-36-3",  1.373, "", "V") ! C4H9OH
    CALL CalcH("2-methyl-1-propanol",      "78-83-1",  1.373, "", "V") ! C4H{10}O isobutanol
    CALL CalcH("2-butanol",                "78-92-2",  0.942, "", "V") ! C4H{10}O {sec}-butanol
    CALL CalcH("1-pentanol",               "71-41-0",  1.202, "", "V") ! C5H{11}OH amylalcohol
    CALL CalcH("2-pentanol",             "6032-29-7",  1.522, "", "V") ! C5H{12}O {sec}-pentanol
    CALL CalcH("1-hexanol",               "111-27-3",  1.873, "", "V") ! C6H{13}OH
    CALL CalcH("1-heptanol",              "111-70-6",  1.603, "", "V") ! C7H{15}OH
    CALL CalcH("1-octanol",               "111-87-5",  4.213, "", "V") ! C8H{17}OH
    CALL CalcH("(hydroxymethyl)-benzene", "100-51-6", 16.221, "", "V") ! C6H5CH2OH benzyl alcohol
    CALL CalcH("cyclohexanol",            "108-93-0",  0.224, "", "V") ! C6H{11}OH
    ! Table 3.2:
    CALL CalcH("propanal",                "123-38-6", 79.438, "", "V") ! C2H5CHO propionaldehyde
    CALL CalcH("butanal",                 "123-72-8", 15.437, "", "V") ! C3H7CHO butyraldehyde
    CALL CalcH("propenal",                "107-02-8", 98.382, "", "V") ! CH2CHCHO acrolein
    CALL CalcH("{trans}-2-butenal",       "123-73-9", 22.914, "", "V") ! CH3CHCHCHO crotonaldehyde
    CALL CalcH("2-furancarboxaldehyde",    "98-01-1",  0.375, "", "V") ! C5H4O2 furfural
    CALL CalcH("benzaldehyde",            "100-52-7",  6.155, "", "V") ! C6H5CHO
    CALL CalcH("butanone",                 "78-93-3",  3.636, "", "V") ! C2H5COCH3 methyl ethyl ketone; MEK
    CALL CalcH("4-methyl-2-pentanone",    "108-10-1", 15.319, "", "V") ! (CH3)2CHCH2COCH3 methyl isobutyl ketone; MIBK
    CALL CalcH("2-pentanone",             "107-87-9",  6.833, "", "V") ! C3H7COCH3
    CALL CalcH("3-pentanone",              "96-22-0", 11.907, "", "V") ! C2H5COC2H5
    CALL CalcH("2-hexanone",              "591-78-6",  9.157, "", "V") ! C6H{12}O
    CALL CalcH("2-heptanone",             "110-43-0", 13.278, "", "V") ! C5H{11}COCH3
    CALL CalcH("2-octanone",              "111-13-7", 20.423, "", "V") ! C6H{13}COCH3
    CALL CalcH("cyclohexanone",           "108-94-1",  2.646, "", "V") ! C6H{10}O
    CALL CalcH("1-phenylethanone",         "98-86-2",  0.983, "", "V") ! C6H5COCH3 acetophenone
    ! p. 404, 409:
    CALL CalcH("catechol",                "120-80-9",  0.022, "", "V")
    CALL CalcH("hydroquinone",            "123-31-9",   4E-6, "", "V")
    ! Table 4.2:
    CALL CalcH("hydroxybenzene",              "108-95-2", 0.0539, "", "V") ! C6H5OH phenol
    CALL CalcH("2-hydroxychlorobenzene",       "95-57-8", 0.6884, "", "V") ! C6H5ClO $o$-chlorophenol
    CALL CalcH("3-hydroxychlorobenzene",      "108-43-0", 0.2045, "", "V") ! C6H5ClO $m$-chlorophenol
    CALL CalcH("4-hydroxychlorobenzene",      "106-48-9", 0.0952, "", "V") ! C6H5ClO $p$-chlorophenol
    CALL CalcH("2,4-dichlorophenol",          "120-83-2", 0.4347, "", "V") ! C6H4Cl2O
    CALL CalcH("2,6-dichlorophenol",           "87-65-0", 0.2999, "", "V") ! C6H4Cl2O
    CALL CalcH("2,3,4-trichlorophenol",     "15950-66-0", 0.3949, "", "V") ! C6H3Cl3O
    CALL CalcH("2,3,5-trichlorophenol",       "933-78-8", 0.3949, "", "V") ! C6H3Cl3O
    CALL CalcH("2,4,5-trichlorophenol",        "95-95-4", 0.5207, "", "V") ! C6H3Cl3O
    CALL CalcH("2,4,6-trichlorophenol",        "88-06-2", 0.5687, "", "V") ! C6H3Cl3O
    CALL CalcH("2,3,4,5-tetrachlorophenol",  "4901-51-3", 0.1397, "", "V")
    CALL CalcH("2,3,4,6-tetrachlorophenol",    "58-90-2", 0.3548, "", "V")
    CALL CalcH("2,3,5,6-tetrachlorophenol",   "935-95-5", 0.2319, "", "V")
    CALL CalcH("hydroxypentachlorobenzene",    "87-86-5", 0.0790, "", "V") ! C6HCl5O pentachlorophenol
    CALL CalcH("1-hydroxy-2-methylbenzene",    "95-48-7", 0.1559, "", "V") ! HOC6H4CH3 2-cresol; $o$-cresol
    CALL CalcH("1-hydroxy-3-methylbenzene",   "108-39-4", 0.0913, "", "V") ! HOC6H4CH3 3-cresol; $m$-cresol
    CALL CalcH("1-hydroxy-4-methylbenzene",   "106-44-5", 0.0653, "", "V") ! HOC6H4CH3 4-cresol; $p$-cresol
    CALL CalcH("2,3-dimethylphenol",          "526-75-0", 0.0540, "", "V") ! C8H{10}O 2,3-xylenol
    CALL CalcH("2,4-dimethylphenol",          "105-67-9", 0.1817, "", "V") ! C8H{10}O 2,4-xylenol
    CALL CalcH("2,5-dimethylphenol",           "95-87-4", 0.1353, "", "V") ! C8H{10}O 2,5-xylenol
    CALL CalcH("2,6-dimethylphenol",          "576-26-1", 0.3831, "", "V") ! C8H{10}O 2,6-xylenol
    CALL CalcH("3,4-dimethylphenol",           "95-65-8", 0.0212, "", "V") ! C8H{10}O 3,4-xylenol
    CALL CalcH("3,5-dimethylphenol",          "108-68-9", 0.0322, "", "V") ! C8H{10}O 3,5-xylenol
    CALL CalcH("2,3,5-trimethylphenol",       "697-82-5", 0.0811, "", "V")
    CALL CalcH("2,4,6-trimethylphenol",       "527-60-6", 0.7027, "", "V")
    CALL CalcH("3,4,5-trimethylphenol",       "527-54-8", 0.0264, "", "V")
    CALL CalcH("4-{sec}-butylphenol",          "99-71-8", 0.2305, "", "V")
    CALL CalcH("4-{tert}-butylphenol",         "98-54-4", 0.0476, "", "V") ! (CH3)3CC6H4OH
    CALL CalcH("1-hydroxy-4-octylbenzene",   "1806-26-4", 0.4916, "", "V")
    CALL CalcH("1-hydroxy-4-nonylbenzene",    "104-40-5", 1.5705, "", "V")
    ! Table 5.2:
    CALL CalcH("pentanoic acid",              "109-52-4", 0.081,   "", "V") ! C4H9COOH
    CALL CalcH("3-methylbutanoic acid",       "503-74-2", 0.623,   "", "V") ! (CH3)2CHCH2COOH
    CALL CalcH("hexanoic acid",               "142-62-1", 0.606,   "", "V") ! C5H{11}COOH caproic acid
    CALL CalcH("octanoic acid",               "124-07-2", 6.723,   "", "V") ! C8H{16}O2 caprylic acid
    CALL CalcH("stearic acid",                 "57-11-4", 4.05e-6, "", "V")
    CALL CalcH("benzenecarboxylic acid",       "65-85-0", 0.484,   "", "V") ! C6H5COOH benzoic acid
    CALL CalcH("3-methylbenzoic acid",         "99-04-7", 6.934,   "", "V")
    CALL CalcH("benzeneacetic acid",          "103-82-2", 0.072,   "", "V") ! C8H8O2 phenylacetic acid
    CALL CalcH("2-hydroxy-benzoic acid",       "69-72-7", 0.558,   "", "V") ! C7H6O3 salicylic acid
    CALL CalcH("2,4-D",                        "94-75-7", 3.41e-3, "", "V") ! C8H6Cl2O3 (2,4-dichlorophenoxy)acetic acid
    ! Table 6.2:
    CALL CalcH("methyl methanoate",           "107-31-3",  20.365, "", "V") ! HCOOCH3 methyl formate
    CALL CalcH("ethyl methanoate",            "109-94-4",  20.466, "", "V") ! HCOOC2H5 ethyl formate
    CALL CalcH("(2-methylpropyl)-methanoate", "542-55-2",  55.150, "", "V") ! HCOOC4H9 isobutyl formate
    CALL CalcH("methyl ethanoate",             "79-20-9",   8.708, "", "V") ! CH3COOCH3 methyl acetate
    CALL CalcH("ethenyl ethanoate",           "108-05-4",  60.693, "", "V") ! CH3COOCHCH2 vinyl acetate
    CALL CalcH("ethyl ethanoate",             "141-78-6",  13.739, "", "V") ! CH3COOC2H5 ethyl acetate
    CALL CalcH("propyl ethanoate",            "109-60-4",  21.885, "", "V") ! CH3COOC3H7 propyl acetate
    CALL CalcH("butyl ethanoate",             "123-86-4",  30.976, "", "V") ! CH3COOC4H9 butyl acetate
    CALL CalcH("(2-methylpropyl)-ethanoate",  "110-19-0",  52.733, "", "V") ! CH3COOC4H9 isobutyl acetate
    CALL CalcH("pentyl ethanoate",            "628-63-7",  42.119, "", "V") ! CH3COOC5H{11} amyl acetate
    CALL CalcH("isopentyl ethanoate",         "123-92-2",  39.056, "", "V") ! CH3COOC5H{11} isoamyl acetate
    CALL CalcH("hexyl ethanoate",             "142-92-7", 193.245, "", "V") ! CH3COOC6H{13} hexyl acetate
    CALL CalcH("2-ethylhexyl ethanoate",      "103-09-3",  92.786, "", "V")
    CALL CalcH("methyl propanoate",           "554-12-1",  16.386, "", "V") ! C2H5COOCH3 methyl propionate
    CALL CalcH("ethyl propanoate",            "105-37-3",  26.384, "", "V") ! C2H5COOC2H5 ethyl propionate
    CALL CalcH("ethyl butanoate",             "105-54-4",  41.103, "", "V") ! C3H7COOC2H5 ethyl butyrate
    CALL CalcH("methyl propenoate",            "96-33-3",  19.170, "", "V")
    CALL CalcH("ethyl propenoate",            "140-88-5",  34.041, "", "V")
    CALL CalcH("methyl methacrylate",          "80-62-6",  32.090, "", "V") ! C5H8O2
    CALL CalcH("methyl benzoate",              "93-58-3",   3.389, "", "V") ! C6H5COOCH3
    CALL CalcH("ethyl benzoate",               "93-89-0",  10.298, "", "V") ! C6H5COOC2H5
    CALL CalcH("benzyl benzoate",             "120-51-4",   0.570, "", "V")
    CALL CalcH("dimethyl phthalate",          "131-11-3",   0.011, "", "V") ! C{10}H{10}O4
    CALL CalcH("diethyl phthalate",            "84-66-2",   0.010, "", "V") ! C{12}H{14}O4
    CALL CalcH("dibutyl phthalate",            "84-74-2",   0.050, "", "V") ! C{16}H{22}O4
    CALL CalcH("dioctyl phthalate",           "117-84-0",   0.570, "", "V")
    CALL CalcH("butyl benzyl phthalate",       "85-68-7",   0.128, "", "V")
    CALL CalcH("di-(2-ethylhexyl)-phthalate", "117-81-7",   0.027, "", "V") ! C{24}H{38}O4
    ! Table 7.2:
    CALL CalcH("propane nitrile",             "107-12-0", 3.182,   "", "V") ! C2H5CN propionitrile
    CALL CalcH("benzenenitrile",              "100-47-0", 5.156,   "", "V") ! C6H5CN benzonitrile
    CALL CalcH("2-propenenitrile",            "107-13-1", 7.731,   "", "V") ! C3H3N acrylonitrile
    CALL CalcH("adiponitrile",                "111-69-3", 0.0041,  "", "V")
    CALL CalcH("ethanamide",                   "60-35-5", 3.53e-4, "", "V") ! C2H5NO acetamide
    CALL CalcH("acrylamide",                   "79-06-1", 1.44e-4, "", "V") ! C3H5NO
    CALL CalcH("benzamide",                    "55-21-0", 4.52e-5, "", "V") ! C7H7NO
    CALL CalcH("urea",                         "57-13-6", 9.61e-8, "", "V") ! CH4N2O
    CALL CalcH("triethylamine",               "121-44-8", 14.099,  "", "V") ! (C2H5)3N
    CALL CalcH("dibutylamine",                "111-92-2", 8.359,   "", "V") ! (C4H9)2NH
    CALL CalcH("tributylamine",               "102-82-9", 2.47e4,  "", "V") ! C{12}H{27}N
    CALL CalcH("diphenylamine",               "122-39-4", 0.035,   "", "V") ! C{12}H{11}N
    CALL CalcH("aminobenzene",                 "62-53-3", 0.168,   "", "V") ! C6H7N aniline
    CALL CalcH("1-amino-2-chlorobenzene",      "95-51-2", 0.761,   "", "V") ! C6H6ClN $o$-chloroaniline
    CALL CalcH("1-amino-3-chlorobenzene",     "108-42-9", 0.223,   "", "V") ! C6H6ClN $m$-chloroaniline
    CALL CalcH("1-amino-4-chlorobenzene",     "106-47-8", 0.099,   "", "V") ! C6H6ClN $p$-chloroaniline
    CALL CalcH("3,4-dichloro-benzenamine",     "95-76-1", 2.289,   "", "V") ! C6H5Cl2N 3,4-dichloroaniline
    CALL CalcH("2-methylaniline",              "95-53-4", 0.095,   "", "V") ! C7H9N $o$-toluidine
    CALL CalcH("3-methylaniline",             "108-44-1", 0.257,   "", "V") ! C7H9N $m$-toluidine
    CALL CalcH("4-methylaniline",             "106-49-0", 0.656,   "", "V") ! C7H9N $p$-toluidine
    CALL CalcH("(dimethylamino)-benzene",     "121-69-7", 11.734,  "", "V") ! C8H{11}N N,N-dimethylaniline
    CALL CalcH("2,4-dimethylbenzenamine",      "95-68-1", 0.421,   "", "V")
    CALL CalcH("2,6-dimethylbenzenamine",      "87-62-7", 17.275,  "", "V")
    CALL CalcH("4-ethylaniline",              "589-16-2", 0.321,   "", "V")
    CALL CalcH("(diethylamino)-benzene",       "91-66-7", 2.161,   "", "V")
    CALL CalcH("benzidine",                    "92-87-5", 4.61e-7, "", "V")
    CALL CalcH("3,3'-dichlorobenzidine",       "91-94-1", 0.005,   "", "V")
    CALL CalcH("1,2-diphenylhydrazine",       "122-66-7", 3.45e-4, "", "V")
    CALL CalcH("nitrobenzene",                 "98-95-3", 1.296,   "", "V")
    CALL CalcH("1,3-dinitrobenzene",           "99-65-0", 0.002,   "", "V")
    CALL CalcH("1,4-dinitrobenzene",          "100-25-4", 5.059,   "", "V")
    CALL CalcH("2-nitrotoluene",               "88-72-2", 3.768,   "", "V")
    CALL CalcH("3-nitrotoluene",               "99-08-1", 7.473,   "", "V")
    CALL CalcH("4-nitrotoluene",               "99-99-0", 0.352,   "", "V")
    CALL CalcH("1-methyl-2,4-dinitrobenzene", "121-14-2", 0.090,   "", "V")
    CALL CalcH("2-methyl-1,3-dinitrobenzene", "606-20-2", 0.070,   "", "V")
    CALL CalcH("1-nitronaphthalene",           "86-57-7", 3.50,    "", "V")
    CALL CalcH("N-nitrosodipropylamine",      "621-64-7", 0.355,   "", "V") ! C6H{14}N2O
    CALL CalcH("N-nitrosodiphenylamine",       "86-30-6", 114.6,   "", "V") ! C{12}H{10}N2O
    CALL CalcH("pyrrole",                     "109-97-7", 1.640,   "", "V") ! C4H5N
    CALL CalcH("indole",                      "120-72-9", 0.140,   "", "V") ! C8H7N
    CALL CalcH("2,4,6-trimethylpyridine",     "108-75-8", 17.549,  "", "V")
    CALL CalcH("quinoline",                    "91-22-5", 0.026,   "", "V")
    CALL CalcH("isoquinoline",                "119-65-3", 19.141,  "", "V")
    CALL CalcH("benzo[f]quinoline",            "85-02-9", 0.0096,  "", "V") ! C{13}H9N
    CALL CalcH("carbazole",                    "86-74-8", 15.146,  "", "V") ! C{12}H9N
    CALL CalcH("acridine",                    "260-94-6", 0.030,   "", "V") ! C{13}H9N
    CALL CalcH("carbon disulfide",             "75-15-0", 1747.75, "", "V") ! CS2
    CALL CalcH("dimethylsulfoxide",            "67-68-5", 0.225,   "", "V") ! CH3SOCH3 DMSO
    CALL CalcH("dimethylsulfone",              "67-71-0", 200.83,  "", "V") ! CH3SO2CH3 DMSO2
    CALL CalcH("dimethyl sulfide",             "75-18-3", 7.72,    "", "V") ! CH3SCH3 DMS
    CALL CalcH("dimethyl disulfide",          "624-92-0", 59.81,   "", "V") ! CH3SSCH3
    CALL CalcH("ethanethiol",                  "75-08-1", 289.94,  "", "V") ! C2H5SH ethyl mercaptan
    CALL CalcH("1-butanethiol",               "109-79-5", 916.94,  "", "V") ! C4H9SH butyl mercaptan
    CALL CalcH("thiophene",                   "110-02-1", 223.3,   "", "V") ! C4H4S
    CALL CalcH("benzo[b]thiophene",            "95-15-8", 24.1,    "", "V") ! C8H6S
    CALL CalcH("dibenzothiophene",            "132-65-0", 44.3,    "", "V") ! C{12}H8S

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, HLC, xref, type_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      CHARACTER(LEN=*), INTENT(IN) :: xref ! ref for experimental value
      CHARACTER(LEN=*), INTENT(IN) :: type_

      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it
      type   = type_   ! make value global, so Output will find it
      IF (type=="X") THEN
        ! only use value here if it is in a paper that I don't have:
        IF (unread_bib(TRIM(xref))) THEN
          CALL SettypeX(xref)
          CALL Output(KHpcSI_TIMES_HcpSI/HLC)
        ENDIF
      ELSE
        CALL Output(KHpcSI_TIMES_HcpSI/HLC)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref0636

  !---------------------------------------------------------------------------

  SUBROUTINE ref0637 ! Hcc [1] and KHcc [1]
    IMPLICIT NONE

    ref = "637"

    ! from Table I: ! Hcc [1]
    type = "M"

    chem = "benzene" ; casrn = "71-43-2"
    CALL Output(4.17*Hcc_TO_HcpSI_atT0)

    chem = "methylbenzene" ; casrn = "108-88-3"
    CALL Output(3.16*Hcc_TO_HcpSI_atT0)

    chem = "chlorobenzene" ; casrn = "108-90-7"
    CALL Output(6.49*Hcc_TO_HcpSI_atT0)

    chem = "1,3-dichlorobenzene" ; casrn = "541-73-1"
    CALL Output(8.38*Hcc_TO_HcpSI_atT0)

    chem = "dichloromethane" ; casrn = "75-09-2"
    CALL Output(10.1*Hcc_TO_HcpSI_atT0)

    chem = "trichloromethane" ; casrn = "67-66-3"
    CALL Output(6.28*Hcc_TO_HcpSI_atT0)

    chem = "tetrachloromethane" ; casrn = "56-23-5"
    CALL Output(0.897*Hcc_TO_HcpSI_atT0)

    chem = "1,1,1-trichloroethane" ; casrn = "71-55-6"
    CALL Output(1.32*Hcc_TO_HcpSI_atT0)

    chem = "tetrachloroethene" ; casrn = "127-18-4"
    CALL Output(1.55*Hcc_TO_HcpSI_atT0)

    chem = "trichloroethene" ; casrn = "79-01-6"
    CALL Output(2.61*Hcc_TO_HcpSI_atT0)

    chem = "1-bromobutane" ; casrn = "109-65-9"
    CALL Output(1.15*Hcc_TO_HcpSI_atT0)

    chem = "1,2-dichloroethane" ; casrn = "107-06-2"
    CALL Output(20.6*Hcc_TO_HcpSI_atT0)

    chem = "methyl methanoate" ; casrn = "107-31-3"
    CALL Output(101.*Hcc_TO_HcpSI_atT0)

    chem = "propanone" ; casrn = "67-64-1"
    CALL Output(659.*Hcc_TO_HcpSI_atT0)

    ! from Table II  ! KHcc [1]
    ! species that are taken from Table 1 are not repeated here
    type = "?"

    chem = "2-methylheptane" ; casrn = "592-27-8"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(2.180))

    chem = "cycloheptane" ; casrn = "291-64-5"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-0.588))

    chem = "cyclooctane" ; casrn = "292-64-8"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(0.616))

    chem = "ethylbenzene" ; casrn = "100-41-4"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-0.447))

    chem = "(2-propyl)-benzene" ; casrn = "98-82-8"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-0.228))

    chem = "fluorobenzene" ; casrn = "462-06-6"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-0.474))

    chem = "1-chlorobutane" ; casrn = "109-69-3"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-0.140))

    chem = "diethyl ether" ; casrn = "60-29-7"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-1.280))

    chem = "dipropyl ether" ; casrn = "111-43-3"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-0.663))

    chem = "ethyl methanoate" ; casrn = "109-94-4"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-0.548))

    chem = "ethyl ethanoate" ; casrn = "141-78-6"
    CALL MakeNote("whichref")
    CALL Output(KHcc_TIMES_HcpSI_atT0/10.**(-2.340))

  END SUBROUTINE ref0637

  !---------------------------------------------------------------------------

  SUBROUTINE ref0638 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "638"

    CALL CalcH("hydroxybenzene",                "108-95-2", 0.054,  "",         "V") ! C6H5OH phenol
    CALL CalcH("hydroxybenzene",                "108-95-2", 0.0718, "2291",     "X") ! C6H5OH phenol
    CALL CalcH("hydroxybenzene",                "108-95-2", 0.132,  "",         "C") ! C6H5OH phenol
    CALL CalcH("hydroxybenzene",                "108-95-2", 0.0402, "howard89", "X") ! C6H5OH phenol
    CALL CalcH("2-hydroxychlorobenzene",         "95-57-8", 0.688,  "",         "V") ! C6H5ClO $o$-chlorophenol
    CALL CalcH("2-hydroxychlorobenzene",         "95-57-8", 0.0567, "howard89", "X") ! C6H5ClO $o$-chlorophenol
    CALL CalcH("3-hydroxychlorobenzene",        "108-43-0", 0.205,  "",         "V") ! C6H5ClO $m$-chlorophenol
    CALL CalcH("3-hydroxychlorobenzene",        "108-43-0", 0.0567, "howard89", "X") ! C6H5ClO $m$-chlorophenol
    CALL CalcH("4-hydroxychlorobenzene",        "106-48-9", 0.0952, "",         "V") ! C6H5ClO $p$-chlorophenol
    CALL CalcH("4-hydroxychlorobenzene",        "106-48-9", 0.0567, "howard89", "X") ! C6H5ClO $p$-chlorophenol
    CALL CalcH("2,4-dichlorophenol",            "120-83-2", 0.434,  "",         "V") ! C6H4Cl2O
    CALL CalcH("2,4,6-trichlorophenol",          "88-06-2", 0.523,  "2292",     "X") ! C6H3Cl3O
    CALL CalcH("2,4,6-trichlorophenol",          "88-06-2", 0.0622, "howard89", "X") ! C6H3Cl3O
    CALL CalcH("1-hydroxy-2-methylbenzene",      "95-48-7", 0.162,  "howard89", "X") ! HOC6H4CH3 2-cresol; $o$-cresol
    CALL CalcH("1-hydroxy-3-methylbenzene",     "108-39-4", 0.0882, "howard89", "X") ! HOC6H4CH3 3-cresol; $m$-cresol
    CALL CalcH("1-hydroxy-4-methylbenzene",     "106-44-5", 0.0973, "howard89", "X") ! HOC6H4CH3 4-cresol; $p$-cresol
    CALL CalcH("1-hydroxy-2,4-dimethylbenzene", "105-67-9", 0.0638, "howard89", "X") ! C8H{10}O 2,4-xylenol; 2,4-dimethylphenol
    CALL CalcH("1-hydroxy-2,6-dimethylbenzene", "576-26-1", 0.38,   "",         "V") ! C8H{10}O 2,6-xylenol; 2,6-dimethylphenol
    CALL CalcH("1-hydroxy-3,4-dimethylbenzene",  "95-65-8", 0.0212, "",         "V") ! C8H{10}O 3,4-xylenol; 3,4-dimethylphenol
    CALL CalcH("1-hydroxy-3,5-dimethylbenzene", "108-68-9", 0.0408, "",         "V") ! C8H{10}O 3,5-xylenol; 3,5-dimethylphenol

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, HLC, xref, type_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: HLC
      CHARACTER(LEN=*), INTENT(IN) :: xref ! ref for experimental value
      CHARACTER(LEN=*), INTENT(IN) :: type_

      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it
      type   = type_   ! make value global, so Output will find it
      IF (type=="X") THEN
        ! only use value here if it is in a paper that I don't have:
        IF (unread_bib(TRIM(xref))) THEN
          CALL SettypeX(xref)
          CALL Output(KHpcSI_TIMES_HcpSI/HLC)
        ENDIF
      ELSE
        CALL Output(KHpcSI_TIMES_HcpSI/HLC)
      ENDIF

    END SUBROUTINE CalcH

  END SUBROUTINE ref0638

  !---------------------------------------------------------------------------

  SUBROUTINE ref0640 ! Hcc [1]
    IMPLICIT NONE

    ref = "640"

    CALL CalcH("dichloromethane",    "75-09-2",    19.,   8.1)
    CALL CalcH("trichloromethane",   "67-66-3",    14.,   9.6)
    CALL CalcH("tetrachloromethane", "56-23-5",     1.9,  9.8)
    CALL CalcH("1,2-dichloroethane", "107-06-2",   36.,   8.2)
    CALL CalcH("benzene",            "71-43-2",     9.6,  8.4)
    CALL CalcH("methylbenzene",      "108-88-3",    9.7,  9.1)
    CALL CalcH("ethylbenzene",       "100-41-4",    9.6, 12.1)
    CALL CalcH("fluorobenzene",      "462-06-6",    6.8,  8.1)
    CALL CalcH("chlorobenzene",      "108-90-7",   13.5,  9.2)
    CALL CalcH("dipropyl ether",     "111-43-3",   20.,  17.6)
    CALL CalcH("methyl methanoate",  "107-31-3",  170.,   7.5)
    CALL CalcH("ethyl methanoate",   "109-94-4",    9.0,  8.5)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, Hcc, DeltaHs)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: Hcc, DeltaHs
      REAL, PARAMETER              :: T12 = 12.5+CtoK
      REAL :: Hominus_T12, Hcc0

      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      type   = "M"
      ! calc Hcp at T12=12.5 C:
      Hominus_T12 = Hcc_TO_HcpSI(Hcc,T12)
      ! calc Hcc at T0:
      Hcc0 = Hcc * EXP((-DeltaHs*kcal/Rgas)*(1/T12-1/T0))
      ! convert Hcc to Hcp at T0:
      Hominus = Hcc0 * Hcc_TO_HcpSI_atT0
      ! apply "H(T) = H(T0) * exp(mindHR*(1/T-1/T0))" to get mindHR:
      mindHR = LOG(Hominus_T12/Hominus)/(1/T12-1/T0)
      ! compare with analytical solution:
      ! print *, TRIM(chem_), Hominus, mindHr
      ! mindHR = DeltaHs*kcal/Rgas + log(T12/T0)/(1/T0-1/T12)
      ! print *, TRIM(chem_), Hominus, mindHr ; print *
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0640

  !---------------------------------------------------------------------------

  SUBROUTINE ref0641 ! KHcc [1]
    IMPLICIT NONE

    ref = "641"

    chem = "ammonia" ; casrn = "7664-41-7"
    type = "?"
    CALL MakeNote("641amine", &
      TRIM(citet())//" gives \citet{793} as the source. "// &
      "However, no data were found in that reference.")
    CALL Output(KHcc_TIMES_HcpSI_atT0/7.7e-4)

    chem = "methylamine" ; casrn = "74-89-5"
    type = "?"
    CALL MakeNote("641amine")
    CALL Output(KHcc_TIMES_HcpSI_atT0/2.9e-4)

    chem = "1,2-ethanediol" ; casrn = "107-21-1"
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(KHcc_TO_HcpSI(1.02e-7, 293.15))

    chem = "ethanal" ; casrn = "75-07-0"
    CALL SettypeX("2223")
    CALL Output(KHcc_TIMES_HcpSI_atT0/2.69e-3)

    chem = "1,3-propanediol" ; casrn = "504-63-2"
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(KHcc_TO_HcpSI(4.5e-8, 293.15))

    chem = "ethanolamine" ; casrn = "141-43-5"
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(KHcc_TO_HcpSI(6.8e-9, 293.15))

  END SUBROUTINE ref0641

  !---------------------------------------------------------------------------

  SUBROUTINE ref0642 ! lg(Hcc) with Hcc [1]
    IMPLICIT NONE
    REAL          :: exptl
    INTEGER       :: i
    CHARACTER(STRLEN_VLONG) :: xref_

    ref = "642"

    ndata = 292
    OPEN (10,FILE="input/ref0642.dat",STATUS="OLD")
    DO i = 1, ndata
      READ (10,*) exptl, type, xref_, chem, casrn
      ! do not repeat data that is already listed from original paper:
      IF ((TRIM(type)=="X").AND.(TRIM(xref_)/="2223")) CYCLE
      IF (TRIM(type)=="X") CALL SettypeX(TRIM(xref_))
      Hominus =  (10.**exptl)*Hcc_TO_HcpSI_atT0
      IF (TRIM(casrn)=="594-60-5") THEN
        CALL MakeNote("642dimebu", &
          "The species is probably 2,3-dimethyl-2-butanol and "// &
          "not 2,3-dimethylbutanol as listed in "//TRIM(citet())//".")
      ENDIF
      IF (TRIM(casrn)=="107-31-3") THEN
        CALL MakeNote("from720")
      ENDIF
      CALL Output(Hominus)
    ENDDO
    CLOSE(10)

  END SUBROUTINE ref0642

  !---------------------------------------------------------------------------

  SUBROUTINE ref0643 ! KHpc [1E4*atm*m3/mol]
    IMPLICIT NONE

    ref = "643"

    type = "M"
    chem="2,2'-dichlorobiphenyl";             casrn="13029-08-8"; CALL Output(1./(3.37E-4*atm))  ! PCB-004
    chem="2,5-dichlorobiphenyl";              casrn="34883-39-1"; CALL Output(1./(3.88E-4*atm))  ! PCB-009
    chem="3,3'-dichlorobiphenyl";             casrn="2050-67-1";  CALL Output(1./(2.33E-4*atm))  ! PCB-011
    chem="3,4-dichlorobiphenyl";              casrn="2974-92-7";  CALL Output(1./(2.05E-4*atm))  ! PCB-012
    chem="4,4'-dichlorobiphenyl";             casrn="2050-68-2";  CALL Output(1./(1.99E-4*atm))  ! PCB-015
    chem="2,3',5-trichlorobiphenyl";          casrn="38444-81-4"; CALL Output(1./(3.25E-4*atm))  ! PCB-026
    chem="2,4,6-trichlorobiphenyl";           casrn="35693-92-6"; CALL Output(1./(6.49E-4*atm))  ! PCB-030
    chem="2,2',3,3'-tetrachlorobiphenyl";     casrn="38444-93-8"; CALL Output(1./(2.02E-4*atm))  ! PCB-040
    chem="2,2',5,5'-tetrachlorobiphenyl";     casrn="35693-99-3"; CALL Output(1./(3.42E-4*atm))  ! PCB-052
    chem="2,2',5,6'-tetrachlorobiphenyl";     casrn="41464-41-9"; CALL Output(1./(4.06E-4*atm))  ! PCB-054
    chem="2,2',6,6'-tetrachlorobiphenyl";     casrn="15968-05-5"; CALL Output(1./(5.5E-4*atm))   ! PCB-053
    chem="3,3',4,4'-tetrachlorobiphenyl";     casrn="32598-13-3"; CALL Output(1./(0.94E-4*atm))  ! PCB-077
    chem="2,2',4,5,5'-pentachlorobiphenyl";   casrn="37680-73-2"; CALL Output(1./(2.51E-4*atm))  ! PCB-101
    chem="2,2',4,6,6'-pentachlorobiphenyl";   casrn="56558-16-8"; CALL Output(1./(8.97E-4*atm))  ! PCB-104
    chem="2,2',3,3',4,4'-hexachlorobiphenyl"; casrn="38380-07-3"; CALL Output(1./(0.302E-4*atm)) ! PCB-128
    chem="2,2',4,4',5,5'-hexachlorobiphenyl"; casrn="35065-27-1"; CALL Output(1./(1.32E-4*atm))  ! PCB-153
    chem="2,2',4,4',6,6'-hexachlorobiphenyl"; casrn="33979-03-2"; CALL Output(1./(7.55E-4*atm))  ! PCB-155

    type = "C"
    chem="4,4'-dichlorobiphenyl";             casrn="2050-68-2";  CALL Output(1./(1.45E-4*atm))  ! PCB-015
    chem="2,2',6,6'-tetrachlorobiphenyl";     casrn="15968-05-5"; CALL Output(1./(1.48E-4*atm))  ! PCB-054
    chem="2,2',4,4',5,5'-hexachlorobiphenyl"; casrn="35065-27-1"; CALL Output(1./(1.23E-4*atm))  ! PCB-153
    chem="2,2',4,4',6,6'-hexachlorobiphenyl"; casrn="33979-03-2"; CALL Output(1./(1.15E-4*atm))  ! PCB-155

    chem = "2,2'-dichlorobiphenyl" ; casrn = "13029-08-8" ! PCB-004
    CALL SettypeX("murphy83")
    CALL MakeNote("atroomT")
    CALL Output(1./(2.2E-4*atm))

    chem = "4,4'-dichlorobiphenyl" ; casrn = "2050-68-2" ! PCB-015
    CALL SettypeX("murphy83")
    CALL MakeNote("atroomT")
    CALL Output(1./(3E-4*atm))

    chem = "2,2',5,5'-tetrachlorobiphenyl" ; casrn = "35693-99-3" ! PCB-052
    CALL SettypeX("murphy83")
    CALL MakeNote("atroomT")
    CALL Output(1./(2.6E-4*atm))

    chem = "2,2',3,3',4,4'-hexachlorobiphenyl" ; casrn = "38380-07-3" ! PCB-128
    CALL SettypeX("murphy83")
    CALL MakeNote("atroomT")
    CALL Output(1./(5E-4*atm))

    chem = "2,2',4,4',5,5'-hexachlorobiphenyl" ; casrn = "35065-27-1" ! PCB-153
    CALL SettypeX("murphy83")
    CALL MakeNote("atroomT")
    CALL Output(1./(3.5E-4*atm))

    ! data from other refs in Tab. II:
    ! c    040 = 1.2e-4
    ! c    052 = 1.2e-4
    ! c    101 = 0.7e-4
    ! c    153 = 0.6e-4
    ! g    004 = 5.5e-4
    ! g    009 = 3.27e-4
    ! g    011 = 1.34e-4
    ! g    012 = 0.948e-4
    ! g    015 = 1.09e-4
    ! g    026 = 2.8e-4
    ! g    030 = 3.68e-4
    ! g    040 = 2e-4
    ! g    052 = 5.25e-4
    ! g    053 = 2.56e-4
    ! g    054 = 18.6e-4
    ! g    077 = 0.431e-4
    ! g    101 = 3.23e-4
    ! g    104 = 18.3e-4
    ! g    128 = 0.676e-4
    ! g    153 = 1.77e-4
    ! g    155 = 15.5e-4

  END SUBROUTINE ref0643

  !---------------------------------------------------------------------------

  SUBROUTINE ref0644
    IMPLICIT NONE

    ref = "644"

    type = "M"

    chem = "2,7-dichlorodibenzo[b,e][1,4]dioxin" ; casrn = "33857-26-0"
    CALL Output(1./5.96)

    chem = "1,2,4-trichlorodibenzo[b,e][1,4]dioxin" ; casrn = "39227-58-2"
    CALL Output(1./3.64)

    chem = "1,2,3,4-tetrachlorodibenzo[b,e][1,4]dioxin" ; casrn = "30746-58-8"
    CALL Output(1./2.02)

  END SUBROUTINE ref0644

  !---------------------------------------------------------------------------

  SUBROUTINE ref0669
    IMPLICIT NONE

    ref = "669"

    chem = "peroxy-2-propenoyl nitrate" ; casrn = "88181-75-3"
    type = "W"
    CALL MakeNote(TRIM(ref), &
      "Comparing the value with that from the cited publication "// &
      "\citep{624}, it can be seen that the unit and the temperature "// &
      "listed in Table 3 of "//TRIM(citet())//" are incorrect.")
    CALL Output(DUMMY)

    chem = "peroxy-isobutyryl nitrate" ; casrn = "65424-60-4"
    type = "?"
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

    chem = "peroxy-n-butyryl nitrate" ; casrn = "27746-48-1"
    type = "?"
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0"
    type = "?"
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

    chem = "peroxypropionyl nitrate" ; casrn = "5796-89-4"
    type = "?"
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

  END SUBROUTINE ref0669

  !---------------------------------------------------------------------------

  SUBROUTINE ref0670
    IMPLICIT NONE

    ref = "670"

    chem = "pernitric acid" ; casrn = "26404-66-0"
    type = "M"
    CALL Output(4E3*Hcp_TO_HcpSI)

  END SUBROUTINE ref0670

  !---------------------------------------------------------------------------

  SUBROUTINE ref0671
    IMPLICIT NONE

    ref = "671"
    type = "M"

    chem = "carbonyl fluoride" ; casrn = "353-50-4"
    CALL Output(35.*Hcp_TO_HcpSI)

    chem = "trifluoroacetylfluoride" ; casrn = "354-34-7"
    CALL Output(3.*Hcp_TO_HcpSI)

    chem = "trifluoroacetylchloride" ; casrn = "354-32-5"
    CALL Output(2.*Hcp_TO_HcpSI)

    chem = "trichloroacetylchloride" ; casrn = "76-02-8"
    CALL Output(2.*Hcp_TO_HcpSI)

  END SUBROUTINE ref0671

  !---------------------------------------------------------------------------

  SUBROUTINE ref0672
    IMPLICIT NONE

    ref = "672"
    type = "W"

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0" ! PAN
    CALL IncorrectCitation("624","peroxyacetyl nitrate")
    CALL Output(DUMMY)

    chem = "peroxypropionyl nitrate" ; casrn = "5796-89-4"
    CALL IncorrectCitation("624","peroxypropionyl nitrate")
    CALL Output(DUMMY)

    chem = "peroxy-n-butyl nitrate" ; casrn = "27746-48-1"
    CALL IncorrectCitation("624","peroxy-$n$-butyl nitrate")
    CALL Output(DUMMY)

    chem = "peroxymethacryloyl nitrate" ; casrn = "88181-75-3"
    CALL IncorrectCitation("624","peroxymethacryloyl nitrate")
    CALL Output(DUMMY)

    chem = "peroxy-i-butyl nitrate" ; casrn = "65424-60-4"
    CALL IncorrectCitation("624","peroxy-$i$-butyl nitrate")
    CALL Output(DUMMY)

  END SUBROUTINE ref0672

  !---------------------------------------------------------------------------

  ! ref0683 p.526, Tab. 6 (only citing other studies, no new species)

  !---------------------------------------------------------------------------

  SUBROUTINE ref0695
    IMPLICIT NONE

    ref = "695"
    type = "?"

    chem = "dieldrin" ; casrn = "60-57-1"
    CALL MakeNoteOtherTemp("288")
    CALL Output(5800.*Hcp_TO_HcpSI)

    chem = "gamma-1,2,3,4,5,6-hexachlorocyclohexane" ; casrn = "58-89-9"
    CALL MakeNoteOtherTemp("288")
    CALL Output(2230.*Hcp_TO_HcpSI)

    chem = "aldrin" ; casrn = "309-00-2"
    CALL MakeNoteOtherTemp("288")
    CALL Output(85.*Hcp_TO_HcpSI)

    chem = "1,1,1-trichloro-2,2-bis-(4-chlorophenyl)ethane" ; casrn = "50-29-3"
    CALL MakeNoteOtherTemp("288")
    CALL Output(28.*Hcp_TO_HcpSI)

    chem = "mercury" ; casrn = "7439-97-6"
    CALL MakeNoteOtherTemp("288")
    CALL Output(0.093*Hcp_TO_HcpSI)

  END SUBROUTINE ref0695

  !---------------------------------------------------------------------------

  ! ref0710 SO2, CO2

  !---------------------------------------------------------------------------

  SUBROUTINE ref0712 ! KHpx [mmHg]
    IMPLICIT NONE

    ref = "712"

    chem = "methanol" ; casrn = "67-56-1"
    type = "M"
    CALL MakeNote("improved711", &
      "This paper supersedes earlier work with more concentrated "// &
      "solutions \citep{711}.")
    CALL Output(cH2O / (mmHg * 184. ))

    chem = "ethanol" ; casrn = "64-17-5"
    type = "M"
    CALL Output(cH2O / (mmHg * 218. ))

    chem = "1-propanol" ; casrn = "71-23-8"
    type = "M"
    CALL MakeNote("improved711")
    CALL Output(cH2O / (mmHg * 291. ))

    chem = "2-propanol" ; casrn = "67-63-0"
    type = "M"
    CALL Output(cH2O / (mmHg * 339. ))

    chem = "1-butanol" ; casrn = "71-36-3"
    type = "V"
    CALL Output(cH2O / (mmHg * 360. ))

    chem = "1-butanol" ; casrn = "71-36-3"
    type = "M"
    CALL MakeNote("improved711")
    CALL Output(cH2O / (mmHg * 359. ))

    chem = "2-methyl-1-propanol" ; casrn = "78-83-1"
    type = "M"
    CALL Output(cH2O / (mmHg * 499. ))

    chem = "2-butanol" ; casrn = "78-92-2"
    type = "M"
    CALL Output(cH2O / (mmHg * 431. ))

    chem = "2-methyl-2-propanol" ; casrn = "75-65-0"
    type = "M"
    CALL Output(cH2O / (mmHg * 503. ))

    chem = "1-pentanol" ; casrn = "71-41-0"
    type = "V"
    CALL Output(cH2O / (mmHg * 547. ))

    chem = "1-pentanol" ; casrn = "71-41-0"
    type = "M"
    CALL Output(cH2O / (mmHg * 532. ))

    chem = "2-methyl-1-butanol" ; casrn = "137-32-6"
    type = "M"
    CALL Output(cH2O / (mmHg * 593. ))

    chem = "2-pentanol" ; casrn = "6032-29-7"
    type = "M"
    CALL Output(cH2O / (mmHg * 622. ))

    chem = "2-methyl-2-butanol" ; casrn = "75-85-4"
    type = "M"
    CALL Output(cH2O / (mmHg * 582. ))

    chem = "1-hexanol" ; casrn = "111-27-3"
    type = "V"
    CALL Output(cH2O / (mmHg * 649. ))

    chem = "1-heptanol" ; casrn = "111-70-6"
    type = "V"
    CALL Output(cH2O / (mmHg * 798. ))

    chem = "1-octanol" ; casrn = "111-87-5"
    type = "V"
    CALL Output(cH2O / (mmHg * 1020.))

  END SUBROUTINE ref0712

  !---------------------------------------------------------------------------

  SUBROUTINE ref0713
    IMPLICIT NONE

    ref = "713"

    chem = "ethylamine" ; casrn = "75-04-7"
    type = "M"
    CALL Output(cH2O / (mmHg *  421.))   ! EtNH2

    chem = "propylamine" ; casrn = "107-10-8"
    type = "M"
    CALL Output(cH2O / (mmHg *  530.))   ! nPrNH2

    chem = "butylamine" ; casrn = "109-73-9"
    type = "M"
    CALL Output(cH2O / (mmHg *  639.))   ! nBuNH2

    ! data not used here, see SUBROUTINE ref1913:
    !chem = "ethanoic acid" ; casrn = "64-19-7" ! HAc
    !CALL SettypeX("1913")
    !CALL Output(cH2O / (mmHg *   12.6))  ! MeCOOH

    chem = "propanoic acid" ; casrn = "79-09-4"
    type = "M"
    CALL Output(cH2O / (mmHg *   18.5))  ! EtCOOH

    chem = "butanoic acid" ; casrn = "107-92-6"
    type = "M"
    CALL Output(cH2O / (mmHg *   22.5))  ! PrCOOH

    chem = "propane nitrile" ; casrn = "107-12-0"
    type = "M"
    CALL Output(cH2O / (mmHg * 1579.))   ! EtCN

    chem = "butane nitrile" ; casrn = "109-74-0"
    type = "M"
    CALL Output(cH2O / (mmHg * 2189.))   ! PrCN

    chem = "methyl ethanoate" ; casrn = "79-20-9"
    type = "M"
    CALL Output(cH2O / (mmHg * 3832.))   ! MeAc

    chem = "ethyl ethanoate" ; casrn = "141-78-6"
    type = "M"
    CALL Output(cH2O / (mmHg * 5580.))   ! EtAc

    chem = "propanone" ; casrn = "67-64-1"
    type = "R"
    CALL Output(cH2O / (mmHg * 1390.))   ! acetone

    chem = "1,2-ethanediol" ; casrn = "107-21-1"
    type = "M"
    CALL MakeNote("bad713", &
      "\citet{588} say that this value is unreliable.")
    CALL Output(cH2O / (mmHg *    2.55)) ! glycol

    chem = "1,2,3-propanetriol" ; casrn = "56-81-5"
    type = "M"
    CALL MakeNote("bad713")
    CALL Output(cH2O / (mmHg *    0.71)) ! glycerol

    type = "V"

    chem = "diethyl ether" ; casrn = "60-29-7"
    CALL Output(cH2O / ( mmHg * EXP(6240.*cal/(Rgas*T0)) )) ! EtOEt

    chem = "ethyl propyl ether" ; casrn = "628-32-0"
    CALL Output(cH2O / ( mmHg * EXP(6390.*cal/(Rgas*T0)) )) ! EtOPr

    chem = "dipropyl ether" ; casrn = "111-43-3"
    CALL Output(cH2O / ( mmHg * EXP(7050.*cal/(Rgas*T0)) )) ! PrOPr

    chem = "propyl ethanoate" ; casrn = "109-60-4"
    CALL Output(cH2O / ( mmHg * EXP(5350.*cal/(Rgas*T0)) )) ! PrAc

    chem = "methane" ; casrn = "74-82-8"
    CALL Output(cH2O / ( mmHg * EXP(9080.*cal/(Rgas*T0)) )) ! MeH

    chem = "ethane" ; casrn = "74-84-0"
    CALL Output(cH2O / ( mmHg * EXP(9000.*cal/(Rgas*T0)) )) ! EtH

    chem = "butane" ; casrn = "106-97-8"
    CALL Output(cH2O / ( mmHg * EXP(9460.*cal/(Rgas*T0)) )) ! nBuH

  END SUBROUTINE ref0713

  !---------------------------------------------------------------------------

  SUBROUTINE ref0714 ! Hcp [M/mmHg]
    IMPLICIT NONE

    ref = "714"

    chem = "methanol" ; casrn = "67-56-1"
    CALL SettypeX("timmermans60")
    CALL Output(0.283/(mmHg*dm3))

    chem = "ethanol" ; casrn = "64-17-5"
    CALL SettypeX("timmermans60")
    CALL Output(0.211/(mmHg*dm3))

    chem = "2-propanol" ; casrn = "67-63-0"
    type = "R"
    CALL Output(0.227/(mmHg*dm3))

    chem = "dimethyl ether" ; casrn = "115-10-6"
    type = "R"
    CALL Output(0.00130/(mmHg*dm3))

    chem = "diethyl ether" ; casrn = "60-29-7"
    type = "V"
    CALL Output(0.00149/(mmHg*dm3))

    chem = "diisopropyl ether" ; casrn = "108-20-3"
    type = "V"
    CALL Output(0.00013/(mmHg*dm3))

    chem = "hydroxybenzene" ; casrn = "108-95-2"
    type = "R"
    CALL Output(0.640/(mmHg*dm3))

    chem = "methoxybenzene" ; casrn = "100-66-3"
    type = "R"
    CALL Output(0.00031/(mmHg*dm3))

    chem = "hydrogen sulfide" ; casrn = "7783-06-4"
    type = "R"
    CALL Output(0.000134/(mmHg*dm3))

    chem = "methanethiol" ; casrn = "74-93-1"
    type = "M"
    CALL Output(0.000434/(mmHg*dm3))

    chem = "dimethyl sulfide" ; casrn = "75-18-3"
    type = "V"
    CALL Output(0.000728/(mmHg*dm3))

    chem = "thiophenol" ; casrn = "108-98-5"
    type = "V"
    CALL Output(0.0040/(mmHg*dm3))

    chem = "thioanisole" ; casrn = "100-68-5"
    type = "V"
    CALL Output(0.0054/(mmHg*dm3))

    chem = "hydrogen cyanide" ; casrn = "74-90-8"
    type = "R"
    CALL Output(0.0122/(mmHg*dm3))

    chem = "ethane nitrile" ; casrn = "75-05-8"
    type = "R"
    CALL Output(0.038/(mmHg*dm3))

    chem = "propanone" ; casrn = "67-64-1"
    type = "R"
    CALL Output(0.0041/(mmHg*dm3))

    chem = "2-butanone" ; casrn = "78-93-3"
    type = "R"
    CALL Output(0.0094/(mmHg*dm3))

    chem = "hydrogen" ; casrn = "1333-74-0"
    type = "R"
    CALL Output(1.03E-6/(mmHg*dm3))

    chem = "methane" ; casrn = "74-82-8"
    type = "R"
    CALL Output(1.88E-6/(mmHg*dm3))

  END SUBROUTINE ref0714

  !---------------------------------------------------------------------------

  SUBROUTINE ref0715
    IMPLICIT NONE

    ref = "715"

    chem = "methanal" ; casrn = "50-00-0"
    CALL MakeNote("HCHOdiol")
    CALL SettypeX("gaffney84")
    CALL Output(6E3*Hcp_TO_HcpSI)

    chem = "ethanal" ; casrn = "75-07-0"
    CALL SettypeX("gaffney84")
    CALL Output(15.*Hcp_TO_HcpSI)

    chem = "propenal" ; casrn = "107-02-8"
    CALL SettypeX("gaffney84")
    CALL Output(8.2*Hcp_TO_HcpSI)

    chem = "{trans}-2-butenal" ; casrn = "123-73-9"
    CALL SettypeX("gaffney84")
    CALL Output(51.*Hcp_TO_HcpSI)

    chem = "methanoic acid" ; casrn = "64-18-6"
    CALL SettypeX("gaffney84")
    CALL MakeNote("715pH4", "Value at pH = 4.")
    CALL Output(6E3*Hcp_TO_HcpSI)

    chem = "ethanedioic acid" ; casrn = "144-62-7"
    CALL SettypeX("gaffney84")
    CALL MakeNote("715pH4")
    CALL Output(7E6*Hcp_TO_HcpSI)

    chem = "ethanoic acid" ; casrn = "64-19-7"
    CALL SettypeX("gaffney84")
    CALL MakeNote("715pH4")
    CALL Output(1E4*Hcp_TO_HcpSI)

    chem = "methanol" ; casrn = "67-56-1"
    CALL SettypeX("gaffney84")
    CALL Output(220.*Hcp_TO_HcpSI)

    chem = "ethanol" ; casrn = "64-17-5"
    CALL SettypeX("gaffney84")
    CALL Output(200.*Hcp_TO_HcpSI)

    chem = "hydroxybenzene" ; casrn = "108-95-2"
    CALL SettypeX("gaffney84")
    CALL Output(3E3*Hcp_TO_HcpSI)

    chem = "1-hydroxy-2-methylbenzene" ; casrn = "95-48-7"
    CALL SettypeX("gaffney84")
    CALL Output(830.*Hcp_TO_HcpSI)

    chem = "1-hydroxy-4-methylbenzene" ; casrn = "106-44-5"
    CALL SettypeX("gaffney84")
    CALL Output(1E3*Hcp_TO_HcpSI)

    chem = "peroxyacetyl nitrate" ; casrn = "2278-22-0"
    CALL SettypeX("gaffney84")
    CALL Output(3.6*Hcp_TO_HcpSI)

    chem = "hydrogen cyanide" ; casrn = "74-90-8"
    CALL SettypeX("gaffney84")
    CALL MakeNote("715pH4")
    CALL Output(7.5*Hcp_TO_HcpSI)

    chem = "ethane nitrile" ; casrn = "75-05-8"
    CALL SettypeX("gaffney84")
    CALL Output(29.*Hcp_TO_HcpSI)

    chem = "nitromethane" ; casrn = "75-52-5"
    CALL SettypeX("gaffney84")
    CALL Output(35.*Hcp_TO_HcpSI)

    chem = "nitroethane" ; casrn = "79-24-3"
    CALL SettypeX("gaffney84")
    CALL Output(21.*Hcp_TO_HcpSI)

    chem = "propanone" ; casrn = "67-64-1"
    CALL SettypeX("gaffney84")
    CALL Output(30.*Hcp_TO_HcpSI)

    chem = "dimethyl sulfide" ; casrn = "75-18-3"
    CALL SettypeX("gaffney84")
    CALL Output(0.62*Hcp_TO_HcpSI)

    chem = "2,3-butanedione" ; casrn = "431-03-8"
    CALL SettypeX("gaffney84")
    CALL Output(190.*Hcp_TO_HcpSI)

    chem = "benzaldehyde" ; casrn = "100-52-7"
    CALL SettypeX("gaffney84")
    CALL Output(36.*Hcp_TO_HcpSI)

    chem = "3-hydroxybenzaldehyde" ; casrn = "100-83-4"
    CALL SettypeX("gaffney84")
    CALL Output(4E5*Hcp_TO_HcpSI)

    chem = "3-nitrophenol" ; casrn = "554-84-7"
    CALL SettypeX("gaffney84")
    CALL Output(5E5*Hcp_TO_HcpSI)

  END SUBROUTINE ref0715

  !---------------------------------------------------------------------------

  SUBROUTINE ref0719 ! special definition (DeltaG and DeltaH for KHpx [atm])
    IMPLICIT NONE

    ref = "719"

    ! Converting alpha from Table 1:
    ! Hcp (in mol/(m3*Pa)) = cH2O / (psat(temp)*alpha)
    ! where psat is the saturation pressure of water

    ! Here, the data from Table 2 are used.

    CALL CalcH("pyridine",             "110-86-1", "M", -0.423, 11.65)
    CALL CalcH("2-methylpyridine",     "109-06-8", "M", -0.355, 12.64)
    CALL CalcH("3-methylpyridine",     "108-99-6", "M", -0.498, 12.60)
    CALL CalcH("4-methylpyridine",     "108-89-4", "M", -0.659, 12.82)
    CALL CalcH("2-ethylpyridine",      "100-71-0", "M", -0.057, 13.31)
    CALL CalcH("3-ethylpyridine",      "536-78-7", "M", -0.328, 12.78)
    CALL CalcH("4-ethylpyridine",      "536-75-4", "M", -0.462, 12.48)
    CALL CalcH("2,3-dimethylpyridine", "583-61-9", "M", -0.549, 13.79)
    CALL CalcH("2,4-dimethylpyridine", "108-47-4", "M", -0.587, 14.02)
    CALL CalcH("2,5-dimethylpyridine", "589-93-5", "M", -0.441, 14.00)
    CALL CalcH("2,6-dimethylpyridine", "108-48-5", "M", -0.325, 14.44)
    CALL CalcH("3,4-dimethylpyridine", "583-58-4", "M", -0.945, 13.54)
    CALL CalcH("3,5-dimethylpyridine", "591-22-0", "M", -0.566, 13.45)

    CALL CalcH("benzene",               "71-43-2", "V",  3.385,  7.56)
    CALL CalcH("methylbenzene",        "108-88-3", "V",  3.391,  8.55)
    CALL CalcH("ethylbenzene",         "100-41-4", "V",  3.476,  9.83)
    CALL CalcH("1,3-dimethylbenzene",  "108-38-3", "V",  3.436,  9.92)
    CALL CalcH("1,4-dimethylbenzene",  "106-42-3", "V",  3.456,  9.79)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, type_, dG, mindH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER,        INTENT(IN) :: type_
      REAL,             INTENT(IN) :: dG
      REAL,             INTENT(IN) :: mindH
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type  = type_  ! make value global, so Output will find it

      CALL MakeNote(TRIM(ref), &
        "Calculated using $G_h$ and $H_h$ from Table 2 in "// &
        TRIM(citet())//". Note that the thermodynamic functions in that "// &
        "table are not based on their $\alpha$ in Table 1. Instead, the "// &
        "expression $\exp(-G_h/(RT))$ yields the Henry's law constant $\Hxp$ "// &
        "in the unit \unit{1/atm}.")
      Hominus = KHpx_TIMES_HcpSI / EXP(dG*kcal/(Rgas*T0))
      mindHR = kcal * mindH / Rgas
      CALL Output(Hominus, mindHR)

    END SUBROUTINE CalcH

  END SUBROUTINE ref0719

  !---------------------------------------------------------------------------

  SUBROUTINE ref0721 ! Hxp [1/mmHg]
    IMPLICIT NONE

    ref = "721"
    type = "M"

    ! primary
    chem = "methylamine" ; casrn = "74-89-5"
    CALL Output((9E-4/0.421   ) * cH2O/mmHg) ! MeNH2

    chem = "ethylamine" ; casrn = "75-04-7"
    CALL Output((9E-4/0.466   ) * cH2O/mmHg) ! EtNH2

    chem = "propylamine" ; casrn = "107-10-8"
    CALL Output((9E-4/0.564   ) * cH2O/mmHg) ! PrNH2

    chem = "butylamine" ; casrn = "109-73-9"
    CALL Output((9E-4/0.662   ) * cH2O/mmHg) ! BuNH2

    chem = "pentylamine" ; casrn = "110-58-7"
    CALL Output((7.5E-5/0.077 ) * cH2O/mmHg) ! PeNH2

    chem = "hexylamine" ; casrn = "111-26-2"
    CALL Output((7.5E-5/0.085 ) * cH2O/mmHg) ! HxNH2

    ! secondary
    chem = "dimethylamine" ; casrn = "124-40-3"
    CALL Output((7.1E-5/0.053 ) * cH2O/mmHg) ! DiMeNH2

    chem = "diethylamine" ; casrn = "109-89-7"
    CALL Output((6.6E-5/0.071 ) * cH2O/mmHg) ! DiEtNH2

    chem = "dipropylamine" ; casrn = "142-84-7"
    CALL Output((6.6E-5/0.142 ) * cH2O/mmHg) ! DiPrNH2

    chem = "dibutylamine" ; casrn = "111-92-2"
    CALL Output((6.6E-5/0.248 ) * cH2O/mmHg) ! DiBuNH2

    ! tertiary
    chem = "trimethylamine" ; casrn = "75-50-3"
    CALL Output((9E-5/0.3934  ) * cH2O/mmHg) ! TriMeNH2

    chem = "triethylamine" ; casrn = "121-44-8"
    CALL Output((7.1E-5/0.4463) * cH2O/mmHg) ! TriEtNH2

  END SUBROUTINE ref0721

  !---------------------------------------------------------------------------

  SUBROUTINE ref0722 ! special definition (DeltaG and DeltaH for KHpx [atm])
    IMPLICIT NONE

    ref = "722"
    type = "T"

    CALL CalcH("pyrrolidine",          "123-75-1", -1206., -15193.)
    CALL CalcH("piperidine",           "110-89-4",  -833., -15634.)
    CALL CalcH("hexamethyleneimine",   "111-49-9",  -634., -16314.)
    CALL CalcH("N-methyl-pyrrolidine", "120-94-5",   297., -15158.)
    CALL CalcH("N-methyl-piperidine",  "626-67-5",   382., -15720.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, dH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpx_TIMES_HcpSI / EXP(dG*cal/(Rgas*T0))
      mindHR = -dH * cal/Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0722

  !---------------------------------------------------------------------------

  SUBROUTINE ref0723 ! special definition (DeltaG and DeltaH for KHpx [atm])
    IMPLICIT NONE

    ref = "723"
    type = "T"

    CALL CalcH("tetrahydrofuran",               "109-99-9",  805., -11296.)
    CALL CalcH("2-methyltetrahydrofuran",        "96-47-9",  973., -12280.)
    CALL CalcH("2,5-dimethyltetrahydrofuran",  "1003-38-9", 1355., -13456.)
    CALL CalcH("tetrahydropyran",               "142-68-7", 1151., -11682.)
    CALL CalcH("1,3-dioxolane",                 "646-06-0",  180.,  -9540.)
    CALL CalcH("1,4-dioxane",                   "123-91-1", -780., -11466.)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, dH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = KHpx_TIMES_HcpSI / EXP(dG*cal/(Rgas*T0))
      mindHR = -dH * cal/Rgas
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0723

  !---------------------------------------------------------------------------

  SUBROUTINE ref0724 ! special definition (DeltaG and DeltaH)
    IMPLICIT NONE

    ref = "724"
    type = "T"

    ! cal=4.184
    ! kcal=1e3*cal
    ! conv(dG,dH)=(Round(Henry(dG),2),Round(mindHR(dH),2))
    ! mindHR(dH)=-dH*kcal/Rgas
    ! Henry(dG)=MatmTOSI(exp(-dG*kcal/(Rgas*T0)))

    CALL CalcH("hydroxybenzene",        "108-95-2",  -4.72, -13.61)
    CALL CalcH("4-bromophenol",         "106-41-2",  -5.24, -16.27)
    CALL CalcH("4-hydroxybenzaldehyde", "123-08-0",  -8.58, -17.13)
    CALL CalcH("4-nitrophenol",         "100-02-7",  -8.76, -18.04)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, dH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = Hcp_TO_HcpSI * EXP(-dG*kcal/(Rgas*T0))
      mindHR = -dH * kcal/Rgas
      CALL MakeNote("assumedMatm")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0724

  !---------------------------------------------------------------------------

  SUBROUTINE ref0725 ! special definition (DeltaG and DeltaH)
    IMPLICIT NONE

    ref = "725"
    type = "M"

    CALL CalcH("1-hydroxy-4-methylbenzene", "106-44-5", -4.24, -14.29)
    CALL CalcH("4-{tert}-butylphenol",       "98-54-4", -4.03, -15.25)
    CALL CalcH("1-hydroxy-2-methylbenzene",  "95-48-7", -3.98, -14.52)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, dH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, dH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus    = Hcp_TO_HcpSI * EXP(-dG*kcal/(Rgas*T0))
      mindHR = -dH * kcal/Rgas
      CALL MakeNote("assumedMatm")
      CALL Output(Hominus, mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0725

  !---------------------------------------------------------------------------

  SUBROUTINE ref0726 ! Hcp [M/atm] can be calculated
    IMPLICIT NONE
    REAL, PARAMETER ::MDMS = MC*2.+MH*6.+MS

    ref = "726"

    chem = "dimethyl sulfide" ; casrn = "75-18-3"
    type = "M"
    Hominus = (1.2e-11 / MDMS) / 1.2e-9
    CALL Output(Hominus*Hcp_TO_HcpSI)

  END SUBROUTINE ref0726

  !---------------------------------------------------------------------------

  SUBROUTINE ref0727 ! special definition (DeltaG for KHpx [atm])
    IMPLICIT NONE

    ref = "727"
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/288.,298.,308.,318./)

    chem = "2,2,2-trifluoroethanol" ; casrn = "75-89-8"
    Harray = KHpx_TIMES_HcpSI / EXP((/ -1856., -126.,1498.,3027. /)/(Rgas*temp))
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "2,2,3,3-tetrafluoro-1-propanol" ; casrn = "76-37-9"
    Harray = KHpx_TIMES_HcpSI / EXP((/ -4518.,-2586.,-800., 851. /)/(Rgas*temp))
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "2,2,3,3,3-pentafluoro-1-propanol" ; casrn = "422-05-9"
    Harray = KHpx_TIMES_HcpSI / EXP((/ -1307.,  505.,2184.,3747. /)/(Rgas*temp))
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "1,1,1-trifluoro-2-propanol" ; casrn = "374-01-6"
    Harray = KHpx_TIMES_HcpSI / EXP((/ -1380.,  487.,2251.,3923. /)/(Rgas*temp))
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "1,1,1,3,3,3-hexafluoro-2-propanol" ; casrn = "920-66-1"
    Harray = KHpx_TIMES_HcpSI / EXP((/    59., 2120.,4042.,5848. /)/(Rgas*temp))
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0727

  !---------------------------------------------------------------------------

  SUBROUTINE ref0728
    IMPLICIT NONE

    ref = "728"

    ! Table II (25C assumed, as in Table 3):

    chem = "propanone" ; casrn = "67-64-1"
    type = "M"
    CALL Output(Hcc_TO_HcpSI_atT0*611.)

    chem = "2-butanone" ; casrn = "78-93-3"
    type = "M"
    CALL Output(Hcc_TO_HcpSI_atT0*440.)

    ! Table IV (some at 20C, some at 25C):

    chem = "ethanethiol" ; casrn = "75-08-1"
    type = "M"
    CALL Output(Hcc_TO_HcpSI(5.4, 20.+CtoK))

    chem = "dimethyl sulfide" ; casrn = "75-18-3"
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hcc_TO_HcpSI(14.8, 20.+CtoK))

    chem = "dimethyl disulfide" ; casrn = "624-92-0"
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hcc_TO_HcpSI(20.2, 20.+CtoK))

    chem = "diethyl disulfide" ; casrn = "110-81-6"
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hcc_TO_HcpSI(11.4, 20.+CtoK))

    chem = "benzene" ; casrn = "71-43-2"
    type = "M"
    CALL Output(Hcc_TO_HcpSI_atT0*4.5)

    chem = "methylbenzene" ; casrn = "108-88-3"
    type = "M"
    CALL Output(Hcc_TO_HcpSI_atT0*4.7)

    ! Table IV, recalculated from their ref 4:

    chem = "dimethyl sulfide" ; casrn = "75-18-3"
    type = "R"
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hcc_TO_HcpSI(17., 20.+CtoK))

    chem = "dimethyl disulfide" ; casrn = "624-92-0"
    type = "R"
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hcc_TO_HcpSI(22., 20.+CtoK))

  END SUBROUTINE ref0728

  !---------------------------------------------------------------------------

  SUBROUTINE ref0729 ! KHcc [1]
    IMPLICIT NONE

    ref = "729"
    type = "M"

    chem = "2-butanone" ; casrn = "78-93-3"
    CALL MakeNoteOtherTemp("303")
    CALL Output(KHcc_TO_HcpSI(3.9e-3, 30.+CtoK))

    chem = "nitroethane" ; casrn = "79-24-3"
    CALL MakeNoteOtherTemp("303")
    CALL Output(KHcc_TO_HcpSI(2.89e-4, 30.+CtoK))

    chem = "1-butanol" ; casrn = "71-36-3"
    CALL MakeNoteOtherTemp("303")
    CALL Output(KHcc_TO_HcpSI(7.46e-4, 30.+CtoK))

    chem = "1,4-dioxane" ; casrn = "123-91-1"
    CALL MakeNoteOtherTemp("303")
    CALL Output(KHcc_TO_HcpSI(2.78e-4, 30.+CtoK))

  END SUBROUTINE ref0729

  !---------------------------------------------------------------------------

  SUBROUTINE ref0730 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "730"

    chem = "dimethyl sulfide" ; casrn = "75-18-3"
    type = "C"
    CALL MakeNote("seawater")
    CALL Output(10.*alpha_TO_HcpSI)

  END SUBROUTINE ref0730

  !---------------------------------------------------------------------------

  SUBROUTINE ref0732 ! KHpx [mmHg]
    IMPLICIT NONE

    ref = "732"

    chem = "carbon oxide sulfide" ; casrn = "463-58-1"
    ndata = 7
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,5.,10.,15.,20.,25.,30./) + CtoK
    Harray = cH2O / (mmHg * (/0.7020,0.8911,1.121,1.38,1.662,1.967,2.314/) * 1E6)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    DEALLOCATE(temp, Harray)
    CALL SettypeX("winkler07")
    CALL Output(Hominus, mindHR, r2)

    chem = "nitrogen monoxide" ; casrn = "10102-43-9" ! NO
    ndata = 31
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15., &
      16.,17.,18.,19.,20.,21.,22.,23.,24.,25.,26.,27.,28.,29.,30./) + CtoK
    Harray = cH2O / (mmHg * (/ &
      12.808, 13.160, 13.520, 13.886, 14.247, 14.634, 15.012, &
      15.398, 15.770, 16.170, 16.540, 16.918, 17.284, 17.639, &
      17.996, 18.353, 18.707, 19.030, 19.396, 19.728, 20.055, &
      20.402, 20.756, 21.112, 21.455, 21.806, 22.155, 22.497, &
      22.841, 23.176, 23.511 /) * 1E6)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    DEALLOCATE(temp, Harray)
    type = "C"
    CALL Output(Hominus, mindHR, r2)

  END SUBROUTINE ref0732

  !---------------------------------------------------------------------------

  SUBROUTINE ref0735 ! special definition
    IMPLICIT NONE
    REAL, PARAMETER :: lambda__TO_HcpSI = 1E-3 * rhoH2O / (Rgas*Tstp)

    ref = "735"

    ! Description of lambda:
    ! lambda describes solubility, i.e. aq/g
    ! gas phase is in "at" = 735.56 Torr = 735.56*101325/760 Pa = 98066.6 Pa
    ! aqueous phase is in "Ncm3/g" = "Nliter/kg"
    ! 1 "Nliter" = number of moles in 1 liter at Tstp = 273.15 K and p0 = 1 at
    ! n/V=p0/RT = 98066.6/(8.31441*273.15) mol = 43.181 mol/m3
    ! => 1 "Nliter" = 43.181E-3 mol
    ! 1 "kg solvent mass" = VH2O / rhoH2O
    ! => 1 "(Nliter/kg)/at" = ((1E-3*p0/R*Tstp)/(1/rhoH2O))/p0 "mol/(m3*Pa)"
    !                       = 1E-3*rhoH2O/(R*Tstp) "mol/(m3*Pa)"

    chem = "difluorine monoxide" ; casrn = "7783-41-7"
    type = "C"
    Hominus = 0.066 * lambda__TO_HcpSI
    CALL Output(Hominus)

    ! p. 1-29:

    ! see SUBROUTINE ref0862
    ! chem = "carbon oxide sulfide" ; casrn = "463-58-1"
    ! ndata = 3
    ! ALLOCATE(temp(ndata), Harray(ndata))
    ! temp = (/0.,10.,20./) + CtoK
    ! Harray = (/1.41,0.65,0.47/) * lambda__TO_HcpSI
    ! CALL HTdep(temp, Harray, Hominus, mindHR)
    ! DEALLOCATE(temp, Harray)
    ! CALL SettypeX("rohrbach1873")
    ! CALL Output(Hominus, mindHR, r2)

    ! see SUBROUTINE ref2933
    ! CALL SettypeX("stock17")
    ! CALL MakeNoteOtherTemp("293")
    ! Hominus = 0.48 * lambda__TO_HcpSI
    ! CALL Output(Hominus)

    ! see SUBROUTINE ref0869
    ! CALL SettypeX("869")
    ! CALL MakeNoteOtherTemp("287")
    ! Hominus = 0.74 * lambda__TO_HcpSI
    ! CALL Output(Hominus)

    ! see ref863 for these numbers:
    ! chem = "carbon disulfide" ; casrn = "75-15-0"
    ! ndata = 3
    ! ALLOCATE(temp(ndata), Harray(ndata))
    ! temp = (/0.,10.,20./) + CtoK
    ! Harray = (/4.24,2.40,1.58/) * lambda__TO_HcpSI
    ! CALL HTdep(temp, Harray, Hominus, mindHR)
    ! DEALLOCATE(temp, Harray)
    ! CALL SettypeX("863")
    ! CALL Output(Hominus, mindHR, r2)

    chem = "carbon disulfide" ; casrn = "75-15-0"
    type = "?"
    ! ndata = 4
    ! ALLOCATE(temp(ndata), Harray(ndata))
    ! temp = (/0.,10.,20.,30./) + CtoK
    ! Harray = (/3.35,1.95,1.30,0.75/) * lambda__TO_HcpSI
    ! CALL HTdep(temp, Harray, Hominus, mindHR)
    ! DEALLOCATE(temp, Harray)
    ! CALL SettypeX("2835")
    ! CALL Output(Hominus, mindHR, r2)
    CALL MakeNote("735CS2a", &
      TRIM(citet())//" present data based on \citet{2835}. However, these "// &
      "data appear to be incorrect.")
    CALL Output(DUMMY)

    chem = "molecular chlorine" ; casrn = "7782-50-5"
    type = "?"
    CALL MakeNote("735Cl2", &
      TRIM(citet())//" claim that \chem{Cl_2} does not obey Henry's law. "// &
      "Looking at their interpolation formula, however, it seems that "// &
      "this is only because they did not consider the equilibrium "// &
      "\chem{Cl_2} + \chem{H_2O} \EQ\ \chem{HOCl} + \chem{HCl}.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0735

  !---------------------------------------------------------------------------

  ! ref0745 no new species

  !---------------------------------------------------------------------------

  SUBROUTINE ref0747
    IMPLICIT NONE
    INTEGER :: i
    REAL :: KHcc, B
    CHARACTER(3), DIMENSION(15), PARAMETER :: tab6str = &
      (/ "ash", "bis", "erv", "fit", "g5a", "g5b", "go7", &
      "han", "hun", "lar", "lei", "mun", "nic", "rob", "tan" /)
    CHARACTER(2), DIMENSION(3), PARAMETER :: tab10str = &
      (/ "sd", "jq", "bn" /)
    CHARACTER(2), DIMENSION(4), PARAMETER :: tab12str = &
      (/ "kt", "us", "bu", "ex" /)

    ref = "747"

    ! Table 6:
    DO i = 1, 15 ! loop through 14 refs and fit
      OPEN (10,FILE="input/ref0747/747"//tab6str(i)//"6h.dat",STATUS="OLD") ! Hcc
      OPEN (20,FILE="input/ref0747/747"//tab6str(i)//"6b.dat",STATUS="OLD") ! B
      ! Alkanes (16)
      CALL tab6("dichloromethane",            "75-09-2")
      CALL tab6("tribromomethane",            "75-25-2")
      CALL tab6("dibromochloromethane",      "124-48-1")
      CALL tab6("bromodichloromethane",       "75-27-4")
      CALL tab6("trichloromethane",           "67-66-3")
      CALL tab6("trichlorofluoromethane",     "75-69-4")
      CALL tab6("tetrachloromethane",         "56-23-5")
      CALL tab6("chloroethane",               "75-00-3")
      CALL tab6("1,1-dichloroethane",         "75-34-3")
      CALL tab6("1,2-dichloroethane",        "107-06-2")
      CALL tab6("1,1,1-trichloroethane",      "71-55-6")
      CALL tab6("1,1,2-trichloroethane",      "79-00-5")
      CALL tab6("1,1,2,2-tetrachloroethane",  "79-34-5")
      CALL tab6("hexachloroethane",           "67-72-1")
      CALL tab6("1,2-dichloropropane",        "78-87-5")
      CALL tab6("1,2,3-trichloropropane",     "96-18-4")
      ! Alkenes (6)
      CALL tab6("chloroethene",               "75-01-4")
      CALL tab6("1,1-dichloroethene",         "75-35-4")
      CALL tab6("(Z)-1,2-dichloroethene",    "156-59-2")
      CALL tab6("(E)-1,2-dichloroethene",    "156-60-5")
      CALL tab6("trichloroethene",            "79-01-6")
      CALL tab6("tetrachloroethene",         "127-18-4")
      ! Monoaromatic (8)
      CALL tab6("benzene",                    "71-43-2")
      CALL tab6("chlorobenzene",             "108-90-7")
      CALL tab6("1,2-dichlorobenzene",        "95-50-1")
      CALL tab6("methylbenzene",             "108-88-3")
      CALL tab6("1,3-dimethylbenzene",       "108-38-3")
      CALL tab6("1,2-dimethylbenzene",        "95-47-6")
      CALL tab6("1,4-dimethylbenzene",       "106-42-3")
      CALL tab6("ethylbenzene",              "100-41-4")
      CLOSE(10)
      CLOSE(20)
    ENDDO

    ! Table 7:
    OPEN (10,FILE="input/ref0747/747all7h.dat",STATUS="OLD") ! Hcc
    OPEN (20,FILE="input/ref0747/747all7b.dat",STATUS="OLD") ! B
    DO i = 1, 5 ! loop through 4 refs and fit
      ! Aldehydes (3)
      CALL MakeNote("HCHOdiol")
      CALL tab7("methanal",      "50-00-0")
      CALL tab7("ethanal",       "75-07-0")
      CALL tab7("benzaldehyde", "100-52-7")
      ! Ketones (2)
      CALL tab7("propanone",     "67-64-1")
      CALL tab7("2-butanone",    "78-93-3")
    ENDDO
    CLOSE(10)
    CLOSE(20)

    ! Table 8:
    CALL tab8("iodomethane",                    "74-88-4",      1538., 1.76E-01, "608")
    CALL tab8("chloromethane",                  "74-87-3",      1702., 2.74E-01, "532")
    CALL tab8("dichloromethane",                "75-09-2",      1644., 9.04E-02, "")
    CALL tab8("tribromomethane",                "75-25-2",      2120., 1.75E-02, "")
    CALL tab8("dibromochloromethane",           "124-48-1",     2273., 3.50E-02, "")
    CALL tab8("bromodichloromethane",           "75-27-4",      2130., 7.60E-02, "")
    CALL tab8("trichloromethane",               "67-66-3",      1838., 1.27E-01, "")
    CALL tab8("dichlorodifluoromethane",        "75-71-8",      1399., 1.09E+01, "2243")
    CALL tab8("trichlorofluoromethane",         "75-69-4",      1214., 3.29E+00, "")
    CALL tab8("tetrachloromethane",             "56-23-5",      1680., 9.65E-01, "")
    CALL tab8("chloroethane",                   "75-00-3",      1110., 4.18E-01, "")
    CALL tab8("1,2-dibromoethane",              "106-93-4",     1556., 2.25E-02, "1156")
    CALL tab8("1,1-dichloroethane",             "75-34-3",      1425., 2.16E-01, "")
    CALL tab8("1,2-dichloroethane",             "107-06-2",     1716., 4.51E-02, "")
    CALL tab8("1,1,1-trichloroethane",          "71-55-6",      1572., 5.69E-01, "")
    CALL tab8("1,1,2-trichloroethane",          "79-00-5",      1989., 2.74E-02, "")
    CALL tab8("1,1,2,2-tetrachloroethane",      "79-34-5",      1255., 1.40E-02, "")
    CALL tab8("1,1,2-trichlorotrifluoroethane", "76-13-1",      1281., 1.01E+01, "1156")
    CALL tab8("hexachloroethane",               "67-72-1",      2320., 1.17E-01, "")
    CALL tab8("1,2-dichloropropane",            "78-87-5",      1730., 9.51E-02, "")
    CALL tab8("1,3-dichloropropane",            "142-28-9",     1577., 3.20E-02, "1150")
    CALL tab8("1,2,3-trichloropropane",         "96-18-4",      1496., 9.73E-03, "")
    CALL tab8("1-chlorobutane",                 "109-69-3",     1388., 5.68E-01, "1150")
    CALL tab8("2-chlorobutane",                 "78-86-4",      1829., 7.74E-01, "1150")
    CALL tab8("1,4-dichlorobutane",             "110-56-5",     1234., 1.70E-02, "1150")
    CALL tab8("1-chloropentane",                "543-59-9",     1928., 7.53E-01, "1150")
    CALL tab8("1,5-dichloropentane",            "628-76-2",      569., 2.10E-02, "1150")
    CALL tab8("hexane",                         "110-54-3",     3143., 2.68E+01, "1156")
    CALL tab8("2-methylpentane",                "107-83-5",      288., 3.06E-01, "1156")
    CALL tab8("1-chlorohexane",                 "544-10-5",     1812., 7.79E-01, "1150")
    CALL tab8("2-methylhexane",                 "591-76-4",    -1669., 2.62E+01, "903")
    CALL tab8("heptane",                        "142-82-5",     1491., 2.79E+01, "903")
    CALL tab8("octane",                         "111-65-9",     3263., 8.96E+00, "903")
    CALL tab8("nonane",                         "111-84-2",      -39., 1.73E+01, "1156")
    CALL tab8("chloroethene",                   "75-01-4",      1223., 8.91E-01, "")
    CALL tab8("1,1-dichloroethene",             "75-35-4",      1586., 9.75E-01, "")
    CALL tab8("(Z)-1,2-dichloroethene",         "156-59-2",     1559., 1.40E-01, "")
    CALL tab8("(E)-1,2-dichloroethene",         "156-60-5",     1669., 3.59E-01, "")
    CALL tab8("trichloroethene",                "79-01-6",      1860., 3.18E-01, "")
    CALL tab8("tetrachloroethene",              "127-18-4",     1946., 5.41E-01, "")
    CALL tab8("cyclopentane",                   "287-92-3",     1302., 5.25E+00, "903")
    CALL tab8("cyclohexane",                    "110-82-7",     1279., 6.18E+00, "1156")
    CALL tab8("methylcyclohexane",              "108-87-2",     3836., 2.63E+00, "903")
    CALL tab8("benzene",                        "71-43-2",      1648., 1.99E-01, "")
    CALL tab8("bromobenzene",                   "108-86-1",     2233., 5.72E-02, "903")
    CALL tab8("chlorobenzene",                  "108-90-7",     1507., 1.22E-01, "")
    CALL tab8("1,2-dichlorobenzene",            "95-50-1",      2436., 5.47E-02, "")
    CALL tab8("1,3-dichlorobenzene",            "541-73-1",      986., 1.18E-01, "1156")
    CALL tab8("1,4-dichlorobenzene",            "106-46-7",     1054., 1.13E-01, "1156")
    CALL tab8("1,2,4-trichlorobenzene",         "120-82-1",     1622., 7.04E-02, "1156")
    CALL tab8("1,2,3,4-tetrachlorobenzene",     "634-66-2",     1945., 2.40E-02, "1146")
    CALL tab8("pentachlorobenzene",             "608-93-5",     2132., 2.16E-02, "1146")
    CALL tab8("hexachlorobenzene",              "118-74-1",     2377., 1.44E-02, "1146")
    CALL tab8("methylbenzene",                  "108-88-3",     1606., 2.22E-01, "")
    CALL tab8("2-hydroxychlorobenzene",         "95-57-8",      1409., 1.21E-01, "1150")
    CALL tab8("ethenylbenzene",                 "100-42-5",     1935., 1.07E-01, "2270")
    CALL tab8("1,3-dimethylbenzene",            "108-38-3",     1701., 2.42E-01, "")
    CALL tab8("1,2-dimethylbenzene",            "95-47-6",      1628., 1.69E-01, "")
    CALL tab8("1,4-dimethylbenzene",            "106-42-3",     1517., 2.63E-01, "")
    CALL tab8("ethylbenzene",                   "100-41-4",     2104., 2.47E-01, "")
    CALL tab8("1,2,4-trimethylbenzene",         "95-63-6",      1697., 2.17E-01, "903")
    CALL tab8("1,3,5-trimethylbenzene",         "108-67-8",     1448., 2.45E-01, "1156")
    CALL tab8("(2-propyl)-benzene",             "98-82-8",      1253., 2.10E-01, "1156")
    CALL tab8("propylbenzene",                  "103-65-1",     1471., 3.70E-01, "1156")
    CALL tab8("(2-propyl)-benzene",             "98-82-8",      1276., 3.94E-01, "903")
    CALL tab8("1,2,3,4-tetrahydronaphthalene",  "119-64-2",     2215., 5.86E-02, "1156")
    CALL tab8("decahydronaphthalene",           "91-17-8",      1664., 4.51E+00, "1156")
    CALL tab8("2-methylnaphthalene",            "91-57-6",       399., 7.64E+00, "903")
    CALL tab8("fluoranthene",                   "206-44-0",     2868., 2.47E-04, "1146")
    CALL tab8("benzo[b]fluoranthene",           "205-99-2",     2245., 1.99E-05, "1146")
    CALL tab8("benzo[k]fluoranthene",           "207-08-9",     2421., 1.73E-05, "1146")
    CALL tab8("benzo[a]pyrene",                 "50-32-8",      1927., 1.44E-05, "1146")
    CALL tab8("benzo[ghi]perylene",             "191-24-2",     1258., 1.14E-05, "1146")
    CALL tab8("indeno[1,2,3-cd]pyrene",         "193-39-5",     1455., 1.17E-05, "1146")
    CALL tab8("2,5-dichlorobiphenyl",           "34883-39-1",   2331., 1.27E-02, "1146")
    CALL tab8("2,4,4'-trichlorobiphenyl",       "7012-37-5",    2467., 8.09E-03, "1146")
    CALL tab8("2,2',5,5'-tetrachlorobiphenyl",  "35693-99-3",   2530., 6.96E-03, "1146")
    ! aroclor is a mixture, not a pure substance:
    !CALL tab8("aroclor1242",                    "53469-21-9",   4339., 1.16E-02, "853")
    !CALL tab8("aroclor1254",                    "11097-69-1",   4099., 7.90E-03, "853")
    !CALL tab8("aroclor1260",                    "11096-82-5",   4104., 7.07E-03, "853")
    CALL tab8("dodecachloropentacyclodecane",   "2385-85-5",    4585., 1.81E-02, "852")
    CALL tab8("$\alpha$-1,2,3,4,5,6-HCH",       "319-84-6",     2682., 2.17E-04, "1152")
    CALL tab8("$\gamma$-1,2,3,4,5,6-HCH",       "58-89-9",      2254., 1.06E-04, "1152")
    CALL tab8("1-hydroxy-2-methoxybenzene",     "90-05-1",      3144., 2.97E-05, "1142")
    CALL tab8("1,3-dimethyl-4-hydroxybenzene",  "105-67-9",    -1563., 2.64E-01, "1156")
    CALL tab8("4-methyl-2-methoxyphenol",       "93-51-6",      3066., 3.82E-05, "1142")
    CALL tab8("1,3-dimethoxy-2-hydroxybenzene", "91-10-1",      2768., 5.68E-06, "1142")
    CALL tab8("2-methoxyethanol",               "109-86-4",     -507., 1.93E+00, "1156")
    CALL tab8("methanal",                       "50-00-0",      2840., 8.61E-06, "")
    CALL tab8("ethanal",                        "75-07-0",      2307., 2.11E-03, "")
    CALL tab8("2-hydroxyethanal",               "141-46-8",     1850., 7.80E-07, "484")
    CALL tab8("trichloroethanal",               "75-87-6",      1368., 1.02E-07, "484")
    CALL tab8("propanal",                       "123-38-6",     2337., 2.25E-03, "630")
    CALL tab8("propanonal",                     "78-98-8",      3121., 7.84E-06, "484")
    CALL tab8("butanal",                        "123-72-8",     2571., 2.98E-03, "630")
    CALL tab8("pentanal",                       "110-62-3",     2623., 4.44E-03, "630")
    CALL tab8("hexanal",                        "66-25-1",      2689., 5.78E-03, "630")
    CALL tab8("benzaldehyde",                   "100-52-7",     1947., 7.94E-04, "")
    CALL tab8("heptanal",                       "111-71-7",     3122., 8.41E-03, "630")
    CALL tab8("octanal",                        "124-13-0",     3084., 1.13E-02, "630")
    CALL tab8("nonanal",                        "124-19-6",     2799., 2.73E-02, "630")
    CALL tab8("decanal",                        "112-31-2",     3610., 4.57E-02, "630")
    CALL tab8("propanone",                      "67-64-1",      1856., 1.05E-03, "")
    CALL tab8("chloro-2-propanone",             "78-95-5",      2223., 4.92E-04, "495")
    CALL tab8("1,1,1-trifluoro-2-propanone",    "421-50-1",     3670., 1.92E-04, "495")
    CALL tab8("2-butanone",                     "78-93-3",      2054., 1.58E-03, "")
    CALL tab8("2,3-butanedione",                "431-03-8",     2323., 3.94E-04, "495")
    CALL tab8("4-methyl-2-pentanone",           "108-10-1",      -57., 1.87E-02, "1156")
    CALL tab8("1-phenylethanone",               "98-86-2",      2452., 3.09E-04, "495")
    CALL tab8("methanoic acid",                 "64-18-6",      2327., 5.63E-06, "2230")
    CALL tab8("ethanoic acid",                  "64-19-7",      2626., 5.32E-06, "2230")
    CALL tab8("2-oxopropanoic acid",            "127-17-3",     2149., 1.01E-07, "2230")
    CALL tab8("pentanoic acid",                 "109-52-4",     2868., 1.27E-05, "2230")
    CALL tab8("hexanoic acid",                  "142-62-1",     2446., 2.39E-05, "2230")
    CALL tab8("diethyl ether",                  "60-29-7",      2158., 3.90E-02, "2901")
    CALL tab8("methyl {tert}-butyl ether",      "1634-04-4",    3178., 1.69E-02, "599")
    CALL tab8("peroxyacetyl nitrate",           "2278-22-0",    2704., 1.03E-02, "502")
    CALL tab8("dimethyl sulfide",               "75-18-3",      1598., 5.66E-02, "522")
    CALL tab8("diethyl sulfide",                "352-93-2",     1927., 5.34E-02, "522")
    CALL tab8("dipropyl sulfide",               "111-47-7",     1955., 1.02E-01, "522")
    CALL tab8("di-(2-propyl)-sulfide",          "625-80-9",     2060., 1.03E-01, "522")
    CALL tab8("dimethyl disulfide",             "624-92-0",     1854., 3.18E-02, "522")
    CALL tab8("diethyl disulfide",              "110-81-6",     1865., 5.12E-02, "522")
    CALL tab8("thiophene",                      "110-02-1",     1662., 7.46E-02, "522")
    CALL tab8("2-methylthiophene",              "554-14-3",     1887., 7.66E-02, "522")
    CALL tab8("methanethiol",                   "74-93-1",      1347., 8.74E-02, "522")
    CALL tab8("ethanethiol",                    "75-08-1",      1486., 1.20E-01, "522")
    CALL tab8("1-propanethiol",                 "107-03-9",     1552., 1.36E-01, "522")
    CALL tab8("1-butanethiol",                  "109-79-5",     1656., 1.49E-01, "522")
    CALL tab8("molinate",                       "2212-67-1",    3024., 1.63E-04, "1142")

    ! Table 10:
    DO i = 1, 3 ! loop through 3 refs
      OPEN (10,FILE="input/ref0747/747"//tab10str(i)//"10h.dat",STATUS="OLD") ! Hcc
      OPEN (20,FILE="input/ref0747/747"//tab10str(i)//"10b.dat",STATUS="OLD") ! B
      CALL tab10("dichloromethane",            "75-09-2")
      CALL tab10("tribromomethane",            "75-25-2")
      CALL tab10("dibromochloromethane",      "124-48-1")
      CALL tab10("bromodichloromethane",       "75-27-4")
      CALL tab10("trichloromethane",           "67-66-3")
      CALL tab10("tetrachloromethane",         "56-23-5")
      CALL tab10("1,1-dichloroethane",         "75-34-3")
      CALL tab10("1,2-dichloroethane",        "107-06-2")
      CALL tab10("1,1,1-trichloroethane",      "71-55-6")
      CALL tab10("1,1,2-trichloroethane",      "79-00-5")
      CALL tab10("1,1,2,2-tetrachloroethane",  "79-34-5")
      CALL tab10("1,2-dichloropropane",        "78-87-5")
      CALL tab10("1,1-dichloroethene",         "75-35-4")
      CALL tab10("(Z)-1,2-dichloroethene",    "156-59-2")
      CALL tab10("(E)-1,2-dichloroethene",    "156-60-5")
      CALL tab10("trichloroethene",            "79-01-6")
      CALL tab10("benzene",                    "71-43-2")
      CALL tab10("chlorobenzene",             "108-90-7")
      CALL tab10("propanone",                  "67-64-1")
      CALL tab10("ethanal",                    "75-07-0")
      CALL tab10("propanal",                  "123-38-6")
      CALL tab10("butanal",                   "123-72-8")
      CLOSE(10)
      CLOSE(20)
    ENDDO

    ! Table 11:
    OPEN (10,FILE="input/ref0747/747es11h.dat",STATUS="OLD") ! Hcc
    OPEN (20,FILE="input/ref0747/747es11b.dat",STATUS="OLD") ! B
    CALL tab11("dibromomethane",             "74-95-3", 3)
    CALL tab11("dibromomethane",             "74-95-3", 4)
    CALL tab11("1,1,2,2-tetrachloroethane",  "79-34-5", 3)
    CALL tab11("1,1,2,2-tetrachloroethane",  "79-34-5", 4)
    CALL tab11("methanol",                   "67-56-1", 1)
    CALL tab11("ethanol",                    "64-17-5", 1)
    CALL tab11("2-butanone",                 "78-93-3", 2)
    CALL tab11("2-pentanone",               "107-87-9", 2)
    CALL tab11("3-pentanone",                "96-22-0", 2)
    CALL tab11("2-heptanone",               "110-43-0", 2)
    CALL tab11("ethyl ethanoate",           "141-78-6", 2)
    CALL tab11("propyl ethanoate",          "109-60-4", 2)
    CALL tab11("isopropyl ethanoate",       "108-21-4", 2)
    CALL tab11("butyl ethanoate",           "123-86-4", 2)
    CALL tab11("hydroxybenzene",            "108-95-2", 2)
    CALL tab11("1-hydroxy-2-methylbenzene",  "95-48-7", 2)
    CALL tab11("1-hydroxy-3-methylbenzene", "108-39-4", 2)
    CALL tab11("1-hydroxy-4-methylbenzene", "106-44-5", 2)
    CLOSE(10)
    CLOSE(20)

    ! Table 12:
    DO i = 1, 4 ! loop through 3 refs plus direct experimental measurement
      OPEN (10,FILE="input/ref0747/747"//tab12str(i)//"12h.dat",STATUS="OLD") ! Hcc
      OPEN (20,FILE="input/ref0747/747"//tab12str(i)//"12b.dat",STATUS="OLD") ! B
      CALL tab12("chloromethane",                 "74-87-3",    "532")
      CALL tab12("dichloromethane",               "75-09-2",    "")
      CALL tab12("tribromomethane",               "75-25-2",    "")
      CALL tab12("dibromochloromethane",          "124-48-1",   "")
      CALL tab12("bromodichloromethane",          "75-27-4",    "")
      CALL tab12("trichloromethane",              "67-66-3",    "")
      CALL tab12("dichlorodifluoromethane",       "75-71-8",    "") ! see Tab. 8
      CALL tab12("trichlorofluoromethane",        "75-69-4",    "")
      CALL tab12("tetrachloromethane",            "56-23-5",    "")
      CALL tab12("chloroethane",                  "75-00-3",    "")
      CALL tab12("1,2-dibromoethane",             "106-93-4",   "1156")
      CALL tab12("1,1-dichloroethane",            "75-34-3",    "")
      CALL tab12("1,2-dichloroethane",            "107-06-2",   "")
      CALL tab12("1,1,1-trichloroethane",         "71-55-6",    "")
      CALL tab12("1,1,2-trichloroethane",         "79-00-5",    "")
      CALL tab12("1,1,2,2-tetrachloroethane",     "79-34-5",    "")
      CALL tab12("hexachloroethane",              "67-72-1",    "")
      CALL tab12("1,2-dichloropropane",           "78-87-5",    "")
      CALL tab12("1,1-dichloroethene",            "75-35-4",    "")
      CALL tab12("(E)-1,2-dichloroethene",        "156-60-5",   "")
      CALL tab12("trichloroethene",               "79-01-6",    "")
      CALL tab12("tetrachloroethene",             "127-18-4",   "")
      CALL tab12("cyclohexane",                   "110-82-7",   "1156")
      CALL tab12("benzene",                       "71-43-2",    "")
      CALL tab12("chlorobenzene",                 "108-90-7",   "")
      CALL tab12("1,2-dichlorobenzene",           "95-50-1",    "")
      CALL tab12("1,3-dichlorobenzene",           "541-73-1",   "1156")
      CALL tab12("1,4-dichlorobenzene",           "106-46-7",   "1156")
      CALL tab12("1,2,4-trichlorobenzene",        "120-82-1",   "1156")
      CALL tab12("hexachlorobenzene",             "118-74-1",   "1146")
      CALL tab12("methylbenzene",                 "108-88-3",   "")
      CALL tab12("1-chloro-2-methylbenzene",      "95-49-8",    "1150")
      CALL tab12("ethenylbenzene",                "100-42-5",   "") ! see Tab. 8
      CALL tab12("xylenes",                  "", "") ! isomer mix, not used
      CALL tab12("ethylbenzene",                  "100-41-4",   "")
      CALL tab12("benzo[k]fluoranthene",          "207-08-9",   "1146")
      CALL tab12("benzo[a]pyrene",                "50-32-8",    "1146")
      ! aroclor is a mixture, not a pure substance:
      CALL tab12("aroclor1242",                   "53469-21-9", "853")
      CALL tab12("aroclor1254",                   "11097-69-1", "853")
      CALL tab12("aroclor1260",                   "11096-82-5", "853")
      CALL tab12("1,3-dimethyl-4-hydroxybenzene", "105-67-9",   "1156")
      CALL tab12("ethanal",                       "75-07-0",    "")
      CALL tab12("ethanoic acid",                 "64-19-7",    "") ! see Tab. 8
      CALL tab12("methanethiol",                  "74-93-1",    "522")
      CLOSE(10)
      CLOSE(20)
    ENDDO

    ! Table 13:
    OPEN (10,FILE="input/ref0747/747all13.dat",STATUS="OLD")
    ! Hydrocarbons (16)
    CALL tab13("methane",                               "74-82-8")
    CALL tab13("bromomethane",                          "74-83-9")
    CALL tab13("chlorodifluoromethane",                 "75-45-6")
    CALL tab13("1,3-dichloropropene",                  "542-75-6")
    CALL tab13("hexachlorobutadiene",                   "87-68-3")
    CALL tab13("hexachlorocyclopentadiene",             "77-47-4")
    CALL tab13("naphthalene",                           "91-20-3")
    CALL tab13("2-chloronaphthalene",                   "91-58-7")
    CALL tab13("acenaphthene",                          "83-32-9")
    CALL tab13("2,3-benzindene",                        "86-73-7") ! fluorene
    CALL tab13("anthracene",                           "120-12-7")
    CALL tab13("phenanthrene",                          "85-01-8")
    ! aroclor is a mixture, not a pure substance:
    CALL tab13("aroclor1221",                        "11104-28-2")
    CALL tab13("aroclor1248",                        "12672-29-6")
    CALL tab13("aroclor1268",                        "11100-14-4")
    ! (equimolar aroclor mixture not used because it contains several species)
    ! Pesticides (1)
    CALL tab13("3,5,5-trimethyl-2-cyclohexen-1-one",    "78-59-1")
    ! Phenols (7)
    CALL tab13("hydroxybenzene",                       "108-95-2")
    CALL tab13("1,3-dihydroxybenzene",                 "108-46-3")
    CALL tab13("2-hydroxychlorobenzene",                "95-57-8")
    CALL tab13("2,4-dichlorophenol",                   "120-83-2")
    CALL tab13("2,4,6-trichlorophenol",                 "88-06-2")
    CALL tab13("hydroxypentachlorobenzene",             "87-86-5")
    ! (methylphenol not used because it contains several isomers)
    ! Alcohols (1)
    CALL tab13("2-propen-1-ol",                        "107-18-6")
    ! Aldehydes (2)
    CALL tab13("propenal",                             "107-02-8")
    CALL tab13("{trans}-2-butenal",                    "123-73-9")
    ! Acids (2)
    CALL tab13("hexanedioic acid",                     "124-04-9")
    CALL tab13("benzenecarboxylic acid",                "65-85-0")
    ! Esters (5)
    CALL tab13("ethenyl ethanoate",                    "108-05-4")
    CALL tab13("butyl ethanoate",                      "123-86-4")
    CALL tab13("isopentyl ethanoate",                  "123-92-2")
    CALL tab13("dimethyl phthalate",                   "131-11-3")
    CALL tab13("diethyl phthalate",                     "84-66-2")
    ! Ethers (4)
    CALL tab13("(2-chloroethoxy)-ethene",              "110-75-8")
    CALL tab13("1,5-dichloro-3-oxapentane",            "111-44-4")
    CALL tab13("bis(2-chloroethoxy)methane",           "111-91-1")
    CALL tab13("bis(2-chloroisopropyl)ether",          "108-60-1")
    ! Nitrogen compounds (10)
    CALL tab13("diethylamine",                         "109-89-7")
    CALL tab13("benzo[b]pyridine",                      "91-22-5")
    CALL tab13("2-propenenitrile",                     "107-13-1")
    CALL tab13("nitrobenzene",                          "98-95-3")
    CALL tab13("2-nitrophenol",                         "88-75-5")
    CALL tab13("4-nitrophenol",                        "100-02-7")
    CALL tab13("2-nitrotoluene",                        "88-72-2")
    CALL tab13("3-nitrotoluene",                        "99-08-1")
    CALL tab13("4-nitrotoluene",                        "99-99-0")
    CALL tab13("1-methyl-2,4-dinitrobenzene",          "121-14-2")
    ! Sulfur compounds (1)
    CALL tab13("carbon disulfide",                      "75-15-0")
    ! Oxygen compounds (3)
    CALL tab13("carbon dioxide",                       "124-38-9")
    CALL tab13("1-chloro-2,3-epoxypropane",            "106-89-8")
    CALL tab13("1,2-epoxypropane",                      "75-56-9")
    ! Inorganics (8)
    CALL tab13("ammonia",                             "7664-41-7")
    CALL tab13("molecular chlorine",                  "7782-50-5")
    CALL tab13("chlorine dioxide",                   "10049-04-4")
    CALL tab13("hydrogen sulfide",                    "7783-06-4")
    CALL tab13("nitrogen",                            "7727-37-9")
    CALL tab13("oxygen",                              "7782-44-7")
    CALL tab13("ozone",                              "10028-15-6")
    CALL tab13("sulfur dioxide",                      "7446-09-5")
    CLOSE(10)

    chem = "2-methylpentane" ; casrn = "107-83-5"
    type = "R"
    CALL MakeNote("747Mepentane", &
      "In their Table 8, "//TRIM(citet())// &
      " incorrectly cite a value given by \citet{1156}.")
    CALL Output(DUMMY)

  CONTAINS

    SUBROUTINE tab6 (chem_, casrn_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_

      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      READ (10,*) KHcc
      READ (20,*) B
      IF (KHcc>0.) THEN
        SELECT CASE(tab6str(i))
        CASE ("ash") ; CALL SettypeX("1156")
        CASE ("bis") ; CALL SettypeX("2270")
        CASE ("erv") ; CALL SettypeX("2271")
        CASE ("fit") ; type = "L"
        CASE ("g5a") ; CALL SettypeX("2269")
        CASE ("g5b") ; CALL SettypeX("2269")
        CASE ("go7") ; CALL SettypeX("532")
        CASE ("han") ; CALL SettypeX("903")
        CASE ("hun") ; CALL SettypeX("608")
        CASE ("lar") ; CALL SettypeX("2901")
        CASE ("lei") ; CALL SettypeX("1150")
        CASE ("mun") ; CALL SettypeX("2243")
        CASE ("nic") ; CALL SettypeX("531")
        CASE ("rob") ; CALL SettypeX("599")
        CASE ("tan") ; CALL SettypeX("598")
        END SELECT
        CALL CalcH
      ENDIF
    END SUBROUTINE tab6

    SUBROUTINE tab7 (chem_, casrn_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_

      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      READ (10,*) KHcc
      READ (20,*) B
      IF (KHcc>0.) THEN
        SELECT CASE(i)
        CASE (1) ; CALL SettypeX("1156")
        CASE (2) ; CALL SettypeX("484")
        CASE (3) ; CALL SettypeX("630")
        CASE (4) ; CALL SettypeX("495")
        CASE (5) ; type = "L"
        END SELECT
        CALL CalcH
      ENDIF
    END SUBROUTINE tab7

    SUBROUTINE tab8 (chem_, casrn_, B_, KHcc_, exp_ref)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: B_, KHcc_
      CHARACTER(LEN=*), INTENT(IN) :: exp_ref ! ref for experimental value

      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it
      B      = B_      ! make value global, so Output will find it
      KHcc = KHcc_ ! make value global, so Output will find it
      IF ((TRIM(casrn)/="").AND.(exp_ref/="")) THEN
        CALL SettypeX(TRIM(exp_ref))
        CALL CalcH
      ENDIF
    END SUBROUTINE tab8

    SUBROUTINE tab10 (chem_, casrn_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_

      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      READ (10,*) KHcc
      READ (20,*) B
      IF ((KHcc>0.).AND.(TRIM(casrn)/="")) THEN
        SELECT CASE(i)
        CASE (1)
          CALL SettypeX("859")
        CASE (2)
          CALL SettypeX("1154")
        CASE (3)
          CALL SettypeX("1147")
        END SELECT
        CALL CalcH(force_output=.TRUE.)
      ENDIF
    END SUBROUTINE tab10

    SUBROUTINE tab11 (chem_, casrn_, exp_ref)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      INTEGER,          INTENT(IN) :: exp_ref ! ref for experimental value

      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      READ (10,*) KHcc
      READ (20,*) B
      SELECT CASE(exp_ref)
      CASE (1)
        CALL SettypeX("859")
        CALL CalcH(force_output=.TRUE.)
      CASE (2)
        CALL SettypeX("1154")
        CALL CalcH(force_output=.TRUE.)
      CASE (3)
        ! see SUBROUTINE ref0856:
        ! CALL SettypeX("856")
        ! CALL CalcH(force_output=.TRUE.)
      CASE (4)
        ! see SUBROUTINE ref0855:
        ! CALL SettypeX("855")
        ! CALL CalcH(force_output=.TRUE.)
      END SELECT
    END SUBROUTINE tab11

    SUBROUTINE tab12 (chem_, casrn_, exp_ref)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: exp_ref ! ref for experimental value

      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      READ (10,*) KHcc
      READ (20,*) B

      ! aroclor is a mixture, not a pure substance:
      IF ((casrn_=="53469-21-9").OR.(casrn_=="11097-69-1").OR.(casrn_=="11096-82-5")) RETURN

      IF ((KHcc>0.).AND.(TRIM(casrn)/="")) THEN
        SELECT CASE(i)
        CASE (1)
          ! not used, see ref2907:
          ! CALL SettypeX("kavanaugh80")
          ! CALL CalcH
        CASE (2)
          CALL SettypeX("usepa82")
          CALL CalcH
        CASE (3)
          CALL SettypeX("850")
          CALL CalcH
        CASE (4)
          IF (exp_ref/="") THEN
            CALL SettypeX(TRIM(exp_ref))
            CALL CalcH
          ENDIF
        END SELECT
      ENDIF
    END SUBROUTINE tab12

    SUBROUTINE tab13 (chem_, casrn_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL :: Hcc_measured

      chem   = chem_  ! make value global, so Output will find it
      casrn  = casrn_ ! make value global, so Output will find it
      READ (10,*) B, KHcc, i,  Hcc_measured

      ! aroclor is a mixture, not a pure substance:
      IF ((casrn_=="11104-28-2").OR.(casrn_=="12672-29-6").OR.(casrn_=="11100-14-4")) RETURN

      SELECT CASE(i)
      CASE (1)
        ! not used, see ref2907:
        ! CALL SettypeX("kavanaugh80")
        ! CALL CalcH
      CASE (2)
        CALL SettypeX("usepa82")
        CALL CalcH
      CASE (3)
        CALL SettypeX("850")
        CALL CalcH
      END SELECT
      !IF (Hcc_measured>0.) THEN
      !  CALL SettypeX("1145")
      !  CALL Output(KHcc_TIMES_HcpSI_atT0/Hcc_measured)
      !ENDIF
    END SUBROUTINE tab13

    SUBROUTINE CalcH(force_output)
      ! before calling this subroutine, KHcc and B must be defined
      IMPLICIT NONE
      LOGICAL, OPTIONAL, INTENT(IN) :: force_output ! force output
      LOGICAL :: l_output
      REAL, PARAMETER :: T20 = 20.+CtoK
      REAL :: Hominus_T20, KHcc0

      ! calc Hcp at T20=20 C:
      Hominus_T20 = KHcc_TO_HcpSI(KHcc,T20)
      ! calc KHcc at T0:
      KHcc0 = KHcc * 10.**(-B*(1./T0-1./T20))
      ! convert KHcc to Hcp at T0:
      Hominus = KHcc_TIMES_HcpSI_atT0 / KHcc0
      ! apply "H(T) = H(T0) * exp(mindHR*(1/T-1/T0))" to get mindHR:
      mindHR = LOG(Hominus_T20/Hominus)/(1/T20-1/T0)
      ! compare with analytical solution:
      !print *, TRIM(chem_), Hominus, mindHr
      !mindHR = log(10.)*B + log(T20/T0)/(1/T0-1/T20)
      !print *, TRIM(chem_), Hominus, mindHr ; print *
      IF (PRESENT(force_output)) THEN
        l_output = force_output
      ELSE
        l_output = .FALSE.
      ENDIF
      IF ((type/="X").OR.(unread_bib(TRIM(xref)))) l_output = .TRUE.
      IF (l_output) THEN
        CALL Output(Hominus, mindHR)
      ELSE
        seenote = "" ! reset so that next CALL Output won't use it
        type    = "" ! reset to invalid value that will be overwritten
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref0747

  !---------------------------------------------------------------------------

  SUBROUTINE ref0750 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "750"
    type = "R"
    chem = "molecular chlorine" ; casrn = "7782-50-5" ! Cl2
    CALL Output(0.0623*Hcp_TO_HcpSI, 3180.)

  END SUBROUTINE ref0750

  !---------------------------------------------------------------------------

  SUBROUTINE ref0752
    IMPLICIT NONE

    ref = "752"

    chem = "sulfuric acid" ; casrn = "7664-93-9"
    type = "M"
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" give partial pressures of \chem{H_2SO_4} over a "// &
      "concentrated solution (e.g., $10^{-7}$~\unit{mmHg} for 70 "// &
      "weight-percent at 298~\unit{K}). Extrapolating this to dilute "// &
      "solutions can only be considered an order-of-magnitude "// &
      "approximation for $\H$.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0752

  !---------------------------------------------------------------------------

  SUBROUTINE ref0753 ! KHpc [atm/M]
    IMPLICIT NONE
    REAL :: H

    ref = "753"
    type = "R"
    chem = "molecular bromine" ; casrn = "7726-95-6" ! Br2
    ndata = 8
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/0.,10.,20.,25.,30.,40.,50.,60./)
    temp = temp + CtoK
    Harray = (/.36,0.6,1.06,1.35,1.68,2.52,2.66,5.17/) ! KHpc from Tab 2
    Harray = (1./Harray) * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "molecular bromine" ; casrn = "7726-95-6" ! Br2
    ! non-linear regression with nlreg, fitting data in Table 1
    ! Parameter H = 1.06; (start value)
    ! Function C = p/H + (3.95e-9*p/H)^(1/3);
    !          p(exp)   p(calc)
    ! 0.33e-3  2.4e-4    2.4e-4
    ! 0.38e-3  2.6e-4    2.9e-4
    ! 0.43e-3  3.6e-4    3.4e-4
    ! 0.56e-3  3.9e-4    4.6e-4
    ! 0.82e-3  7.6e-4    7.1e-4
    ! 0.84e-3  7.2e-4    7.4e-4
    ! 0.90e-3  8.3e-4    7.8e-4
    ! 1.03e-3  8.3e-4    9.2e-4
    ! 1.21e-3  9.5e-4   11.0e-4
    ! 1.36e-3 12.3e-4   12.5e-4
    ! 1.48e-3 14.5e-4   13.7e-4
    H = 1.03
    Hominus = (1./H) * Hcp_TO_HcpSI
    type = "M"
    CALL MakeNoteOtherTemp("293")
    CALL Output(Hominus)

  END SUBROUTINE ref0753

  !---------------------------------------------------------------------------

  SUBROUTINE ref0754 ! KHcc [1]
    IMPLICIT NONE

    ref = "754"

    CALL CalcH("propanone",         "67-64-1",  1.6e-3)
    CALL CalcH("2-butanone",        "78-93-3",  1.9e-3)
    CALL CalcH("2-pentanone",       "107-87-9", 2.6e-3)
    CALL CalcH("2-heptanone",       "110-43-0", 5.9e-3)
    CALL CalcH("2-octanone",        "111-13-7", 7.7e-3)
    CALL CalcH("2-nonanone",        "821-55-6", 15.e-3)
    CALL CalcH("2-undecanone",      "112-12-9", 26.e-3)
    CALL CalcH("ethanal",           "75-07-0",  2.7e-3)
    CALL CalcH("propanal",          "123-38-6", 3.0e-3)
    CALL CalcH("butanal",           "123-72-8", 4.7e-3)
    CALL CalcH("pentanal",          "110-62-3", 6.0e-3)
    CALL CalcH("hexanal",           "66-25-1",  8.7e-3)
    CALL CalcH("heptanal",          "111-71-7", 11.e-3)
    CALL CalcH("octanal",           "124-13-0", 21.e-3)
    CALL CalcH("nonanal",           "124-19-6", 30.e-3)
    CALL CalcH("methyl ethanoate",  "79-20-9",  4.7e-3)
    CALL CalcH("methyl propanoate", "554-12-1", 7.1e-3)
    CALL CalcH("methyl butanoate",  "623-42-7", 8.4e-3)
    CALL CalcH("methyl pentanoate", "624-24-8", 13.e-3)
    CALL CalcH("methyl hexanoate",  "106-70-7", 15.e-3)
    CALL CalcH("methyl octanoate",  "111-11-5", 32.e-3)
    CALL CalcH("1-butanol",         "71-36-3",  3.6e-4)
    CALL CalcH("1-hexanol",         "111-27-3", 7.0e-4)
    CALL CalcH("1-octanol",         "111-87-5", 10.e-4)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, K)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: K

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      CALL Output(KHcc_TIMES_HcpSI_atT0/K)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0754

  !---------------------------------------------------------------------------

  SUBROUTINE ref0755 ! KHcc [1]
    IMPLICIT NONE

    ref = "755"
    type = "M"

    chem = "{trans}-2-butenal" ; casrn = "123-73-9"
    CALL Output(KHcc_TIMES_HcpSI_atT0/8e-4)

    chem = "{trans}-2-hexenal" ; casrn = "6728-26-3"
    CALL Output(KHcc_TIMES_HcpSI_atT0/2e-3)

    chem = "{trans}-2-octenal" ; casrn = "2548-87-0"
    CALL Output(KHcc_TIMES_HcpSI_atT0/3e-3)

    chem = "{trans}-{trans}-2,4-hexadienal" ; casrn = "142-83-6"
    CALL Output(KHcc_TIMES_HcpSI_atT0/4e-4)

    chem = "2-methylpyrazine" ; casrn = "109-08-0"
    CALL Output(KHcc_TIMES_HcpSI_atT0/9e-5)

    chem = "2-ethylpyrazine" ; casrn = "13925-00-3"
    CALL Output(KHcc_TIMES_HcpSI_atT0/10e-5)

    chem = "2-isobutylpyrazine" ; casrn = "29460-92-2"
    CALL Output(KHcc_TIMES_HcpSI_atT0/2e-4)

    chem = "2-ethyl-3-methoxypyrazine" ; casrn = "25680-58-4"
    CALL Output(KHcc_TIMES_HcpSI_atT0/6e-4)

    chem = "2-isobutyl-3-methoxypyrazine" ; casrn = "24683-00-9"
    CALL Output(KHcc_TIMES_HcpSI_atT0/2e-3)

  END SUBROUTINE ref0755

  !---------------------------------------------------------------------------

  ! ref0756 is not used because it has been updated by ref0789

  !---------------------------------------------------------------------------

  SUBROUTINE ref0759
    IMPLICIT NONE

    ref = "759"
    type = "M"
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 10.,25.,45.,5.,25.,30. /) + CtoK

    chem = "bromine chloride" ; casrn = "13863-41-7"
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" measured the solubility in concentrated salt "// &
      "solutions (natural brines).")
    Harray = KHcc_TO_HcpSI ( (/ 0.57,0.9,2.16,0.38,0.86,1.26 /)/100. , temp )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "molecular bromine" ; casrn = "7726-95-6"
    CALL MakeNote(TRIM(ref))
    Harray = KHcc_TO_HcpSI ( (/ 1.36,2.21,4.49,0.96,2.17,2.69 /)/100. , temp )
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0759

  !---------------------------------------------------------------------------

  SUBROUTINE ref0763
    IMPLICIT NONE

    ref = "763"

    chem = "molecular iodine" ; casrn = "7553-56-2"
    type = "R"
    mindHR = 9.2 * kcal / Rgas
    Hominus    = EXP(mindHR/T0-28.6*cal/Rgas) * Hcp_TO_HcpSI
    CALL Output(Hominus,mindHR)

    CALL CalcH("fluorine atom",       "14762-94-8", 2.3, 0.8)
    CALL CalcH("chlorine atom",       "22537-15-1", 2.5, 2.9)
    CALL CalcH("bromine atom",        "10097-32-2", 2.0, 3.6)
    CALL CalcH("iodine atom",         "14362-44-8", 3.0, 4.6)
    CALL CalcH("nitrogen dioxide",    "10102-44-0", 2.0, 3.6)
    CALL CalcH("nitrogen trioxide",   "12033-49-7", 2.0, 3.9)
    CALL CalcH("cyano radical",        "2074-87-5", 1.5, 2.8)
    CALL CalcH("hydroxyl radical",     "3352-57-6", -2., 6.2)
    CALL CalcH("hydroperoxy radical",  "3170-83-0", -5., 9.6)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, dG, mindH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: dG, mindH

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "T"
      CALL MakeNote(TRIM(ref), &
        "Calculated from correlation between the polarizabilities "// &
        "and solubilities of stable gases. The temperature "// &
        "dependence is an estimate of the upper limit.")
      Hominus    = EXP(-dG*kcal/(Rgas*T0)) * Hcp_TO_HcpSI
      mindHR = mindH*kcal/Rgas
      CALL Output(Hominus,mindHR)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0763

  !---------------------------------------------------------------------------

  SUBROUTINE ref0764
    IMPLICIT NONE

    ref = "764"
    type = "M"

    chem = "helium" ; casrn = "7440-59-7"
    ! only data from 11.0 to 27.4 C are used:
    CALL CalcH ( (/ 11.0,12.0,13.2,16.6,21.7,24.7,27.4 /), &
      (/ 9.04,8.97,8.87,8.71,8.53,8.47,8.45 /) )

    chem = "neon" ; casrn = "7440-01-9"
    ! only data from 11.5 to 32.1 C are used:
    CALL CalcH ( (/ 11.5,15.0,19.8,24.4,28.8,31.3,32.1 /), &
      (/ 11.3,10.9,10.6,10.2,10.1,9.93,9.93 /) )

    chem = "argon" ; casrn = "7440-37-1"
    CALL CalcH ( (/ 10.7,14.3,18.2,24.4,29.8,36.6,39.4,47.8,49.5, &
      56.3,66.3,71.3,74.1 /), &
      (/ 40.9,37.2,34.6,31.1,28.3,25.9,25.1,23.1,22.6,21.3,20.2,19.4,19.1 /) )

    chem = "krypton" ; casrn = "7439-90-9"
    CALL CalcH ( (/ 6.6,8.0,12.6,15.5,18.2,21.2,24.7,30.2,35.4,41.4, &
      46.0,51.9,57.9,59.8,64.7,67.5,71.5,73.9,74.9 /), &
      (/ 82.0,79.4,69.5,65.6,61.7,58.2,53.5,48.9,44.3,40.6,38.3,35.9, &
      33.3,32.8,31.6,30.7,29.7,29.5,29.3 /) )

    chem = "xenon" ; casrn = "7440-63-3"
    CALL CalcH ( (/ 12.7,15.1,15.5,19.6,30.2,40.0,40.5,47.9,59.0,71.7 /), &
      (/ 135.5,123.6,121.1,110.9,84.5,69.3,67.8,59.4,50.8,43.8 /) )

    chem = "tetrafluoromethane" ; casrn = "75-73-0"
    CALL CalcH ( (/ 6.7,13.1,19.0,24.7,38.5 /), &
      (/ 6.5,5.6,5.0,4.5,3.7 /) )

  CONTAINS

    SUBROUTINE CalcH (temp_, Hominus_)
      IMPLICIT NONE
      REAL, DIMENSION(:), INTENT(IN) :: temp_, Hominus_

      ! I assume that "N.T.P." refers to 273.15 K
      ! 1E-6 converts from "c.c." to m3
      CALL HTdep(temp_+CtoK, Hominus_*1E-6*rhoH2O/(Rgas*TSTP), Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)

    END SUBROUTINE CalcH

  END SUBROUTINE ref0764

  !---------------------------------------------------------------------------

  ! ref0765 O2, N2, H2, He, Xe, CH4

  !---------------------------------------------------------------------------

  ! ref0766 no new species

  !---------------------------------------------------------------------------

  SUBROUTINE ref0769 ! Hcp [M/mmHg]
    IMPLICIT NONE

    ref = "769"
    chem = "sulfuric acid" ; casrn = "7664-93-9"
    type = "M"
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" give partial pressures of \chem{H_2SO_4} over a "// &
      "concentrated solution (e.g., $2.6\times 10^{-9}$~\unit{Pa} for "// &
      "54.1 weight-percent at 298~\unit{K}). Extrapolating this to dilute "// &
      "solutions can only be considered an order-of-magnitude "// &
      "approximation for $\H$.")
    CALL Output(DUMMY)

  END SUBROUTINE ref0769

  !---------------------------------------------------------------------------

  SUBROUTINE ref0776 ! Hcp [M/mmHg]
    IMPLICIT NONE

    ref = "776"
    chem = "ethylenediamine" ; casrn = "107-15-3"
    type = "M"
    CALL Output(1./(0.0013*mmHg*1e-3))

  END SUBROUTINE ref0776

  !---------------------------------------------------------------------------

  SUBROUTINE ref0789 ! KHpcSI [atm*m3/mol] and KHpx
    IMPLICIT NONE
    REAL          :: KHpx, KHpc, Hominusb
    INTEGER       :: i
    CHARACTER(STRLEN_VLONG) :: seenote_

    ref = "789"
    ndata = 700
    type = "?"
    OPEN (10,FILE="input/ref0789.dat",STATUS="OLD")
    DO i = 1, ndata
      READ (10,*) KHpx, KHpc, chem, casrn, seenote_
      IF (KHpx>1e37) CYCLE ! ignore dummy lines in input file
      IF (i==1) THEN
        CALL MakeNote(TRIM(ref), &
          TRIM(citet())//" give several references for the Henry's law "// &
          "constants but don't assign them to specific species.")
      ELSE
        CALL MakeNote(TRIM(ref))
      ENDIF
      IF (TRIM(seenote_)/="") CALL MakeNoteOtherTemp(TRIM(seenote_))
      Hominus = KHpx_TIMES_HcpSI/KHpx
      ! test if the data in the two columns are consistent:
      Hominusb = 1./(atm*KHpc)
      CALL consistency_check(Hominus, Hominusb, &
        "Different types of Henry's law constants") ! , verbose=.TRUE.
!!$      IF (ABS(Hominusb-Hominus)/Hominus>0.01) THEN
!!$        CALL MakeNote("incorrect")
!!$        Hominus = DUMMY
!!$      ENDIF
      CALL Output(Hominus)
    ENDDO
    CLOSE(10)

  END SUBROUTINE ref0789

  !---------------------------------------------------------------------------

  SUBROUTINE ref0790 ! Hcp [M/mmHg]
    IMPLICIT NONE

    ref = "790"
    type = "V"

    chem = "benzene" ; casrn = "71-43-2"
    CALL Output(2.41E-4*1E3/mmHg)

    chem = "methylbenzene" ; casrn = "108-88-3"
    CALL Output(2.39E-4*1E3/mmHg)

    chem = "ethylbenzene" ; casrn = "100-41-4"
    CALL Output(2.06E-4*1E3/mmHg)

    chem = "1,3-dimethylbenzene" ; casrn = "108-38-3"
    CALL Output(2.22E-4*1E3/mmHg)

    chem = "1,4-dimethylbenzene" ; casrn = "106-42-3"
    CALL Output(2.12E-4*1E3/mmHg)

    chem = "naphthalene" ; casrn = "91-20-3"
    CALL Output(25.0E-4*1E3/mmHg)

    chem = "biphenyl" ; casrn = "92-52-4"
    CALL Output(16.0E-4*1E3/mmHg)

  END SUBROUTINE ref0790

  !---------------------------------------------------------------------------

  SUBROUTINE ref0791 ! Hcc [1]
    IMPLICIT NONE

    ref = "791"

    ! Tab. I: only data for pure water is used here but data for AgNO3
    ! solutions could be added as well
    type = "M"
    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0.,13.,23. /) + CtoK

    chem = "benzene" ; casrn = "71-43-2"
    Harray = Hcc_TO_HcpSI( (/ 14.3,7.,4.3  /) , temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "methylbenzene" ; casrn = "108-88-3"
    Harray = Hcc_TO_HcpSI( (/ 23.2,9.2,4.7 /) , temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "1,4-dimethylbenzene" ; casrn = "106-42-3"
    Harray = Hcc_TO_HcpSI( (/ 27.,11.4,6.4 /) , temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "1,2-dimethylbenzene" ; casrn = "95-47-6"
    Harray = Hcc_TO_HcpSI( (/ 35.5,15.,8.2 /) , temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "benzene-d6" ; casrn = "1076-43-3"
    Harray = Hcc_TO_HcpSI( (/ 15.0,7.4,4.5 /) , temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

    ! Tab. II data from Irmann is not used here, see ref2550:
    ! chem = "{cis}-2-butene" ; casrn = "590-18-1"
    ! CALL SettypeX("irrmann65")
    ! CALL Output(0.106*Hcc_TO_HcpSI_atT0)
    ! chem = "{trans}-2-butene" ; casrn = "624-64-6"
    ! CALL SettypeX("irrmann65")
    ! CALL Output(0.109*Hcc_TO_HcpSI_atT0)

  END SUBROUTINE ref0791

  !---------------------------------------------------------------------------

  SUBROUTINE ref0792 ! Hcc [1]
    IMPLICIT NONE

    ref = "792"
    type = "M"
    ! T=25C assumed

    chem = "ethanol" ; casrn = "64-17-5"
    CALL Output(5647.*Hcc_TO_HcpSI_atT0)

    chem = "2-butanone" ; casrn = "78-93-3"
    CALL Output( 469.*Hcc_TO_HcpSI_atT0)

    chem = "1,4-dioxane" ; casrn = "123-91-1"
    CALL Output(5396.*Hcc_TO_HcpSI_atT0)

    chem = "nitromethane" ; casrn = "75-52-5"
    CALL Output(1105.*Hcc_TO_HcpSI_atT0)

  END SUBROUTINE ref0792

  !---------------------------------------------------------------------------

  SUBROUTINE ref0794 ! Hcp [M/mmHg]
    IMPLICIT NONE

    ref = "794"
    type = "V"

    chem = "trimethoxymethane" ; casrn = "149-73-5"
    CALL Output(3.24/35.4*(1E3/mmHg))

    chem = "methyl {tert}-butyl ether" ; casrn = "1634-04-4"
    CALL Output(0.55/245.*(1E3/mmHg))

    chem = "1,1,1-trimethoxyethane" ; casrn = "1445-45-0"
    CALL Output(2.04/24. *(1E3/mmHg))

  END SUBROUTINE ref0794

  !---------------------------------------------------------------------------

  SUBROUTINE ref0795 ! KHpx [mmHg]
    IMPLICIT NONE

    ref = "795"
    type = "M"

    chem = "tetrahydrofuran" ; casrn = "109-99-9"
    CALL Output(0.05*cH2O/(93.4*mmHg))

    chem = "diethyl ether" ; casrn = "60-29-7"
    CALL Output(0.002*cH2O/(105.8*mmHg))

  END SUBROUTINE ref0795

  !---------------------------------------------------------------------------

  SUBROUTINE ref0796
    IMPLICIT NONE

    ref = "796"

    chem = "3,3'-thiobis-1-propene" ; casrn = "592-88-1"
    type = "M"
    CALL MakeNote(TRIM(ref), &
      "Value extracted from their Figure 46.")
    CALL Output(0.42*Hcp_TO_HcpSI)

  END SUBROUTINE ref0796

  !---------------------------------------------------------------------------

  SUBROUTINE ref0797 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "797"

    chem = "pernitric acid" ; casrn = "26404-66-0"
    type = "T"
    CALL Output(12600.*Hcp_TO_HcpSI, 57.1E3/Rgas)

    ! Table 4:
    chem = "hydroperoxy radical" ; casrn = "3170-83-0"
    type = "R"
    CALL Output(5760.*Hcp_TO_HcpSI)

  END SUBROUTINE ref0797

  !---------------------------------------------------------------------------

  SUBROUTINE ref0799 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "799"
    type = "M"

    chem = "bis(hydroxymethyl)peroxide" ; casrn = "17088-73-2"
    CALL Output(1E7*Hcp_TO_HcpSI, limit=">")

    chem = "hydrogen peroxide" ; casrn = "7722-84-1"
    CALL MakeNote("799h2o2", &
      "This value was measured at low pH. It is superseded by a later "// &
      "publication of the same group \citep{311}.")
    CALL Output(DUMMY)

    chem = "hydroxymethyl hydroperoxide" ; casrn = "15932-89-5"
    mindHR = 10240.
    Hominus    = EXP(mindHR/T0-20.03) * Hcp_TO_HcpSI
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref0799

  !---------------------------------------------------------------------------

  SUBROUTINE ref0800 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "800"
    type = "M"

    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/10.,22./)
    temp = temp + CtoK

    chem = "hydrogen peroxide" ; casrn = "7722-84-1" ! H2O2
    Harray = (/2.73E5,1.07E5/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "hydroxymethyl hydroperoxide" ; casrn = "15932-89-5" ! HOCH2OOH HMHP,HMP
    Harray = (/6.2E5,5.0E5/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    chem = "bis(hydroxymethyl)peroxide" ; casrn = "17088-73-2" ! HOCH2OOCH2OH BHMP
    Harray = (/20.E5,6.E5/)
    Harray = Harray * Hcp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0800

  !---------------------------------------------------------------------------

  ! ref0821 SO2

  !---------------------------------------------------------------------------

  ! ref0823 HNO4 in sulf acid

  !---------------------------------------------------------------------------

  SUBROUTINE ref0824 ! Hbp [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "824"
    chem = "dimethylsulfoxide" ; casrn = "67-68-5" ! CH3SOCH3 DMSO
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 7., 15., 21., 35. /) + CtoK
    Harray = (/ 125000., 117000., 99100., 82800. /) * Hbp_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0824

  !---------------------------------------------------------------------------

  SUBROUTINE ref0834 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "834"
    chem = "sulfur dioxide" ; casrn = "7446-09-5"
    type = "L"
    mindHR = LOG(10.) * 1376.1
    Hominus = 1.242*Hcp_TO_HcpSI
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0834

  !---------------------------------------------------------------------------

  SUBROUTINE ref0837 ! KHpx [atm]
    IMPLICIT NONE
    REAL :: a, b

    ref = "837"
    type = "M"

    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    a = 7.15
    b = 1397.
    Hominus = KHpx_TIMES_HcpSI/(10.**(a-b/T0))
    mindHR = LOG(10.) * b
    CALL Output(Hominus, mindHR)

    chem = "methylbenzene" ; casrn = "108-88-3" ! C6H5CH3 toluene
    a = 7.94
    b = 1621.
    Hominus = KHpx_TIMES_HcpSI/(10.**(a-b/T0))
    mindHR = LOG(10.) * b
    CALL Output(Hominus, mindHR)

    chem = "trichloroethene" ; casrn = "79-01-6" ! C2HCl3 trichloroethylene
    a = 8.19
    b = 1642.
    Hominus = KHpx_TIMES_HcpSI/(10.**(a-b/T0))
    mindHR = LOG(10.) * b
    CALL Output(Hominus, mindHR)

    chem = "tetrachloroethene" ; casrn = "127-18-4" ! C2Cl4 tetrachloroethylene
    a = 9.06
    b = 1822.
    Hominus = KHpx_TIMES_HcpSI/(10.**(a-b/T0))
    mindHR = LOG(10.) * b
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0837

  !---------------------------------------------------------------------------

  SUBROUTINE ref0849 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "849"
    type = "M"

    ! aroclor is a mixture, not a pure substance:
    !chem = "aroclor1242" ; casrn = "53469-21-9" ! C{12}HxCl{(10-x)}
    !CALL MakeNote_range(KHpcSI_TIMES_HcpSI/(atm*4.1E-4), &
    !  KHpcSI_TIMES_HcpSI/(atm*2.0E-4))
    !CALL Output(DUMMY)

    !chem = "aroclor1254" ; casrn = "11097-69-1" ! C{12}HxCl{(10-x)}
    !CALL MakeNote_range(KHpcSI_TIMES_HcpSI/(atm*7.3E-4), &
    !  KHpcSI_TIMES_HcpSI/(atm*4.7E-4))
    !CALL Output(DUMMY)

    chem = "2',3,4-trichlorobiphenyl" ; casrn = "38444-86-9"
    CALL Output(KHpcSI_TIMES_HcpSI/(atm*3.9E-4))

    chem = "2,2',5,5'-tetrachlorobiphenyl" ; casrn = "35693-99-3"
    CALL MakeNote_range(KHpcSI_TIMES_HcpSI/(atm*5.3E-4), &
      KHpcSI_TIMES_HcpSI/(atm*3.1E-4))
    CALL Output(DUMMY)

    chem = "2,2',4,5,5'-pentachlorobiphenyl" ; casrn = "37680-73-2"
    CALL MakeNote_range(KHpcSI_TIMES_HcpSI/(atm*3.5E-4), &
      KHpcSI_TIMES_HcpSI/(atm*1.1E-4))
    CALL Output(DUMMY)

  END SUBROUTINE ref0849

  !---------------------------------------------------------------------------

  SUBROUTINE ref0850 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "850"
    type = "V"

    ! data from Tab. I not used: aroclor is a mixture, not a pure
    ! substance

    ! Table III (supplement):
    CALL CalcH("biphenyl",                                     "92-52-4", .200E-4, .148E+0, .135E-3) ! (C6H5)2 
    CALL CalcH("2-chlorobiphenyl",                           "2051-60-7", .914E-5, .321E-1, .285E-3) ! C{12}H9Cl PCB-1
    CALL CalcH("3-chlorobiphenyl",                           "2051-61-8", .357E-5, .250E-1, .143E-3) ! C{12}H9Cl PCB-2
    CALL CalcH("4-chlorobiphenyl",                           "2051-62-9", .316E-5, .245E-1, .129E-3) ! C{12}H9Cl PCB-3
    CALL CalcH("2,2'-dichlorobiphenyl",                     "13029-08-8", .418E-5, .760E-2, .550E-3) ! C{12}H8Cl2 PCB-4
    CALL CalcH("2,3-dichlorobiphenyl",                      "16605-91-7", .149E-5, .762E-2, .195E-3) ! C{12}H8Cl2 PCB-5
    CALL CalcH("2,3'-dichlorobiphenyl",                     "25569-80-6", .163E-5, .597E-2, .274E-3) ! C{12}H8Cl2 PCB-6
    CALL CalcH("2,4-dichlorobiphenyl",                      "33284-50-3", .173E-5, .587E-2, .294E-3) ! C{12}H8Cl2 PCB-7
    CALL CalcH("2,4'-dichlorobiphenyl",                     "34883-43-7", .145E-5, .589E-2, .246E-3) ! C{12}H8Cl2 PCB-8
    CALL CalcH("2,5-dichlorobiphenyl",                      "34883-39-1", .195E-5, .596E-2, .327E-3) ! C{12}H8Cl2 PCB-9
    CALL CalcH("2,6-dichlorobiphenyl",                      "33146-45-1", .360E-5, .764E-2, .472E-3) ! C{12}H8Cl2 PCB-10
    CALL CalcH("3,3'-dichlorobiphenyl",                      "2050-67-1", .638E-6, .474E-2, .134E-3) ! C{12}H8Cl2 PCB-11
    CALL CalcH("3,4-dichlorobiphenyl",                       "2974-92-7", .525E-6, .553E-2, .948E-4) ! C{12}H8Cl2 PCB-12
    CALL CalcH("3,4'-dichlorobiphenyl",                      "2974-90-5", .565E-6, .465E-2, .122E-3) ! C{12}H8Cl2 PCB-13
    CALL CalcH("3,5-dichlorobiphenyl",                      "34883-41-5", .775E-6, .471E-2, .165E-3) ! C{12}H8Cl2 PCB-14
    CALL CalcH("4,4'-dichlorobiphenyl",                      "2050-68-2", .501E-6, .457E-2, .109E-3) ! C{12}H8Cl2 PCB-15
    CALL CalcH("2,2',3-trichlorobiphenyl",                  "38444-78-9", .681E-6, .196E-2, .347E-3) ! C{12}H7Cl3 PCB-16
    CALL CalcH("2,2',4-trichlorobiphenyl",                  "37680-66-3", .790E-6, .153E-2, .515E-3) ! C{12}H7Cl3 PCB-17
    CALL CalcH("2,2',5-trichlorobiphenyl",                  "37680-65-2", .892E-6, .156E-2, .573E-3) ! C{12}H7Cl3 PCB-18
    CALL CalcH("2,2',6-trichlorobiphenyl",                  "38444-73-4", .165E-5, .134E-2, .123E-2) ! C{12}H7Cl3 PCB-19
    CALL CalcH("2,3,3'-trichlorobiphenyl",                  "38444-84-7", .266E-6, .156E-2, .170E-3) ! C{12}H7Cl3 PCB-20
    CALL CalcH("2,3,4-trichlorobiphenyl",                   "55702-46-0", .266E-6, .182E-2, .146E-3) ! C{12}H7Cl3 PCB-21
    CALL CalcH("2,3,4'-trichlorobiphenyl",                  "38444-85-8", .236E-6, .154E-2, .152E-3) ! C{12}H7Cl3 PCB-22
    CALL CalcH("2,3,5-trichlorobiphenyl",                   "55720-44-0", .397E-6, .156E-2, .254E-3) ! C{12}H7Cl3 PCB-23
    CALL CalcH("2,3,6-trichlorobiphenyl",                   "55702-45-9", .859E-6, .197E-2, .436E-3) ! C{12}H7Cl3 PCB-24
    CALL CalcH("2,3',4-trichlorobiphenyl",                  "55712-37-3", .309E-6, .123E-2, .252E-3) ! C{12}H7Cl3 PCB-25
    CALL CalcH("2,3',5-trichlorobiphenyl",                  "38444-81-4", .348E-6, .124E-2, .280E-3) ! C{12}H7Cl3 PCB-26
    CALL CalcH("2,3',6-trichlorobiphenyl",                  "38444-76-7", .644E-6, .157E-2, .411E-3) ! C{12}H7Cl3 PCB-27
    CALL CalcH("2,4,4'-trichlorobiphenyl",                   "7012-37-5", .273E-6, .121E-2, .225E-3) ! C{12}H7Cl3 PCB-28
    CALL CalcH("2,4,5-trichlorobiphenyl",                   "15862-07-4", .361E-6, .145E-2, .250E-3) ! C{12}H7Cl3 PCB-29
    CALL CalcH("2,4,6-trichlorobiphenyl",                   "35693-92-6", .934E-6, .154E-2, .606E-3) ! C{12}H7Cl3 PCB-30
    CALL CalcH("2,4',5-trichlorobiphenyl",                  "16606-02-3", .309E-6, .123E-2, .251E-3) ! C{12}H7Cl3 PCB-31
    CALL CalcH("2,4',6-trichlorobiphenyl",                  "38444-77-8", .570E-6, .155E-2, .368E-3) ! C{12}H7Cl3 PCB-32
    CALL CalcH("2,3',4'-trichlorobiphenyl",                 "38444-86-9", .240E-6, .144E-2, .167E-3) ! C{12}H7Cl3 PCB-33
    CALL CalcH("2,3',5'-trichlorobiphenyl",                 "37680-68-5", .355E-6, .124E-2, .287E-3) ! C{12}H7Cl3 PCB-34
    CALL CalcH("3,3',4-trichlorobiphenyl",                  "37680-69-6", .937E-7, .117E-2, .802E-4) ! C{12}H7Cl3 PCB-35
    CALL CalcH("3,3',5-trichlorobiphenyl",                  "38444-87-0", .139E-6, .101E-2, .138E-3) ! C{12}H7Cl3 PCB-36
    CALL CalcH("3,4,4'-trichlorobiphenyl",                  "38444-90-5", .830E-7, .115E-2, .724E-4) ! C{12}H7Cl3 PCB-37
    CALL CalcH("3,4,5-trichlorobiphenyl",                   "53555-66-1", .103E-6, .137E-2, .752E-4) ! C{12}H7Cl3 PCB-38
    CALL CalcH("3,4',5-trichlorobiphenyl",                  "38444-88-1", .123E-6, .988E-3, .124E-3) ! C{12}H7Cl3 PCB-39
    CALL CalcH("2,2',3,3'-tetrachlorobiphenyl",             "38444-93-8", .111E-6, .553E-3, .200E-3) ! C{12}H6Cl4 PCB-40
    CALL CalcH("2,2',3,4-tetrachlorobiphenyl",              "52663-59-9", .122E-6, .514E-3, .236E-3) ! C{12}H6Cl4 PCB-41
    CALL CalcH("2,2',3,4'-tetrachlorobiphenyl",             "36559-22-5", .129E-6, .442E-3, .291E-3) ! C{12}H6Cl4 PCB-42
    CALL CalcH("2,2',3,5-tetrachlorobiphenyl",              "70362-46-8", .181E-6, .447E-3, .406E-3) ! C{12}H6Cl4 PCB-43
    CALL CalcH("2,2',3,5'-tetrachlorobiphenyl",             "41464-39-5", .145E-6, .448E-3, .324E-3) ! C{12}H6Cl4 PCB-44
    CALL CalcH("2,2',3,6-tetrachlorobiphenyl",              "70362-45-7", .393E-6, .394E-3, .998E-3) ! C{12}H6Cl4 PCB-45
    CALL CalcH("2,2',3,6'-tetrachlorobiphenyl",             "41464-47-5", .268E-6, .389E-3, .689E-3) ! C{12}H6Cl4 PCB-46
    CALL CalcH("2,2',4,4'-tetrachlorobiphenyl",              "2437-79-8", .149E-6, .353E-3, .423E-3) ! C{12}H6Cl4 PCB-47
    CALL CalcH("2,2',4,5-tetrachlorobiphenyl",              "70362-47-9", .165E-6, .416E-3, .398E-3) ! C{12}H6Cl4 PCB-48
    CALL CalcH("2,2',4,5'-tetrachlorobiphenyl",             "41464-40-8", .168E-6, .358E-3, .471E-3) ! C{12}H6Cl4 PCB-49
    CALL CalcH("2,2',4,6-tetrachlorobiphenyl",              "62796-65-0", .427E-6, .315E-3, .136E-2) ! C{12}H6Cl4 PCB-50
    CALL CalcH("2,2',4,6'-tetrachlorobiphenyl",             "68194-04-7", .311E-6, .312E-3, .999E-3) ! C{12}H6Cl4 PCB-51
    CALL CalcH("2,2',5,5'-tetrachlorobiphenyl",             "35693-99-3", .190E-6, .362E-3, .525E-3) ! C{12}H6Cl4 PCB-52
    CALL CalcH("2,2',5,6'-tetrachlorobiphenyl",             "41464-41-9", .351E-6, .316E-3, .111E-2) ! C{12}H6Cl4 PCB-53
    CALL CalcH("2,2',6,6'-tetrachlorobiphenyl",             "15968-05-5", .650E-6, .349E-3, .186E-2) ! C{12}H6Cl4 PCB-54
    CALL CalcH("2,3,3',4-tetrachlorobiphenyl",              "74338-24-2", .475E-7, .417E-3, .114E-3) ! C{12}H6Cl4 PCB-55
    CALL CalcH("2,3,3',4'-tetrachlorobiphenyl",             "41464-43-1", .391E-7, .419E-3, .933E-4) ! C{12}H6Cl4 PCB-56
    CALL CalcH("2,3,3',5-tetrachlorobiphenyl",              "70424-67-8", .709E-7, .363E-3, .195E-3) ! C{12}H6Cl4 PCB-57
    CALL CalcH("2,3,3',5'-tetrachlorobiphenyl",             "41464-49-7", .577E-7, .363E-3, .159E-3) ! C{12}H6Cl4 PCB-58
    CALL CalcH("2,3,3',6-tetrachlorobiphenyl",              "74472-33-6", .153E-6, .449E-3, .342E-3) ! C{12}H6Cl4 PCB-59
    CALL CalcH("2,3,4,4'-tetrachlorobiphenyl",              "33025-41-1", .421E-7, .413E-3, .102E-3) ! C{12}H6Cl4 PCB-60
    CALL CalcH("2,3,4,5-tetrachlorobiphenyl",               "33284-53-6", .551E-7, .486E-3, .113E-3) ! C{12}H6Cl4 PCB-61
    CALL CalcH("2,3,4,6-tetrachlorobiphenyl",               "54230-22-7", .156E-6, .517E-3, .303E-3) ! C{12}H6Cl4 PCB-62
    CALL CalcH("2,3,4',5-tetrachlorobiphenyl",              "74472-34-7", .628E-7, .359E-3, .175E-3) ! C{12}H6Cl4 PCB-63
    CALL CalcH("2,3,4',6-tetrachlorobiphenyl",              "52663-58-8", .136E-6, .444E-3, .306E-3) ! C{12}H6Cl4 PCB-64
    CALL CalcH("2,3,5,6-tetrachlorobiphenyl",               "33284-54-7", .149E-6, .556E-3, .269E-3) ! C{12}H6Cl4 PCB-65
    CALL CalcH("2,3',4,4'-tetrachlorobiphenyl",             "32598-10-0", .453E-7, .335E-3, .135E-3) ! C{12}H6Cl4 PCB-66
    CALL CalcH("2,3',4,5-tetrachlorobiphenyl",              "73575-53-8", .646E-7, .338E-3, .191E-3) ! C{12}H6Cl4 PCB-67
    CALL CalcH("2,3',4,5'-tetrachlorobiphenyl",             "73575-52-7", .670E-7, .291E-3, .230E-3) ! C{12}H6Cl4 PCB-68
    CALL CalcH("2,3',4,6-tetrachlorobiphenyl",              "60233-24-1", .167E-6, .359E-3, .465E-3) ! C{12}H6Cl4 PCB-69
    CALL CalcH("2,3',4',5-tetrachlorobiphenyl",             "32598-11-1", .512E-7, .339E-3, .151E-3) ! C{12}H6Cl4 PCB-70
    CALL CalcH("2,3',4',6-tetrachlorobiphenyl",             "41464-46-4", .946E-7, .420E-3, .225E-3) ! C{12}H6Cl4 PCB-71
    CALL CalcH("2,3',5,5'-tetrachlorobiphenyl",             "41464-42-0", .756E-7, .295E-3, .256E-3) ! C{12}H6Cl4 PCB-72
    CALL CalcH("2,3',5',6-tetrachlorobiphenyl",             "74338-23-1", .140E-6, .364E-3, .384E-3) ! C{12}H6Cl4 PCB-73
    CALL CalcH("2,4,4',5-tetrachlorobiphenyl",              "32690-93-0", .572E-7, .335E-3, .171E-3) ! C{12}H6Cl4 PCB-74
    CALL CalcH("2,4,4',6-tetrachlorobiphenyl",              "32598-12-2", .148E-6, .355E-3, .417E-3) ! C{12}H6Cl4 PCB-75
    CALL CalcH("2,3',4',5'-tetrachlorobiphenyl",            "70362-48-0", .470E-7, .392E-3, .120E-3) ! C{12}H6Cl4 PCB-76
    CALL CalcH("3,3',4,4'-tetrachlorobiphenyl",             "32598-13-3", .138E-7, .319E-3, .431E-4) ! C{12}H6Cl4 PCB-77
    CALL CalcH("3,3',4,5-tetrachlorobiphenyl",              "70362-49-1", .184E-7, .323E-3, .568E-4) ! C{12}H6Cl4 PCB-78
    CALL CalcH("3,3',4,5'-tetrachlorobiphenyl",             "41464-48-6", .204E-7, .279E-3, .729E-4) ! C{12}H6Cl4 PCB-79
    CALL CalcH("3,3',5,5'-tetrachlorobiphenyl",             "33284-52-5", .301E-7, .244E-3, .123E-3) ! C{12}H6Cl4 PCB-80
    CALL CalcH("3,4,4',5-tetrachlorobiphenyl",              "70362-50-4", .163E-7, .318E-3, .513E-4) ! C{12}H6Cl4 PCB-81
    CALL CalcH("2,2',3,3',4-pentachlorobiphenyl",           "52663-62-4", .198E-7, .160E-3, .123E-3) ! C{12}H5Cl5 PCB-82
    CALL CalcH("2,2',3,3',5-pentachlorobiphenyl",           "60145-20-2", .295E-7, .141E-3, .210E-3) ! C{12}H5Cl5 PCB-83
    CALL CalcH("2,2',3,3',6-pentachlorobiphenyl",           "52663-60-2", .640E-7, .125E-3, .510E-3) ! C{12}H5Cl5 PCB-84
    CALL CalcH("2,2',3,4,4'-pentachlorobiphenyl",           "65510-45-4", .230E-7, .131E-3, .176E-3) ! C{12}H5Cl5 PCB-85
    CALL CalcH("2,2',3,4,5-pentachlorobiphenyl",            "55312-69-1", .126E-6, .152E-3, .830E-3) ! C{12}H5Cl5 PCB-86
    CALL CalcH("2,2',3,4,5'-pentachlorobiphenyl",           "38380-02-8", .259E-7, .132E-3, .196E-3) ! C{12}H5Cl5 PCB-87
    CALL CalcH("2,2',3,4,6-pentachlorobiphenyl",            "55215-17-3", .159E-6, .118E-3, .135E-2) ! C{12}H5Cl5 PCB-88
    CALL CalcH("2,2',3,4,6'-pentachlorobiphenyl",           "73575-57-2", .479E-7, .117E-3, .411E-3) ! C{12}H5Cl5 PCB-89
    CALL CalcH("2,2',3,4',5-pentachlorobiphenyl",           "68194-07-0", .343E-7, .115E-3, .298E-3) ! C{12}H5Cl5 PCB-90
    CALL CalcH("2,2',3,4',6-pentachlorobiphenyl",           "68194-05-8", .742E-7, .103E-3, .722E-3) ! C{12}H5Cl5 PCB-91
    CALL CalcH("2,2',3,5,5'-pentachlorobiphenyl",           "52663-61-3", .387E-7, .116E-3, .332E-3) ! C{12}H5Cl5 PCB-92
    CALL CalcH("2,2',3,5,6-pentachlorobiphenyl",            "73575-56-1", .149E-6, .126E-3, .119E-2) ! C{12}H5Cl5 PCB-93
    CALL CalcH("2,2',3,5,6'-pentachlorobiphenyl",           "73575-55-0", .715E-7, .103E-3, .697E-3) ! C{12}H5Cl5 PCB-94
    CALL CalcH("2,2',3,5',6-pentachlorobiphenyl",           "38379-99-6", .838E-7, .103E-3, .814E-3) ! C{12}H5Cl5 PCB-95
    CALL CalcH("2,2',3,6,6'-pentachlorobiphenyl",           "73575-54-9", .155E-6, .113E-3, .138E-2) ! C{12}H5Cl5 PCB-96
    CALL CalcH("2,2',3,4',5'-pentachlorobiphenyl",          "41464-51-1", .269E-7, .132E-3, .204E-3) ! C{12}H5Cl5 PCB-97
    CALL CalcH("2,2',3,4',6'-pentachlorobiphenyl",          "60233-25-2", .696E-7, .102E-3, .680E-3) ! C{12}H5Cl5 PCB-98
    CALL CalcH("2,2',4,4',5-pentachlorobiphenyl",           "38380-01-7", .312E-7, .108E-3, .289E-3) ! C{12}H5Cl5 PCB-99
    CALL CalcH("2,2',4,4',6-pentachlorobiphenyl",           "39485-83-1", .807E-7, .841E-4, .960E-3) ! C{12}H5Cl5 PCB-100
    CALL CalcH("2,2',4,5,5'-pentachlorobiphenyl",           "37680-73-2", .353E-7, .109E-3, .323E-3) ! C{12}H5Cl5 PCB-101
    CALL CalcH("2,2',4,5,6'-pentachlorobiphenyl",           "68194-06-9", .652E-7, .963E-4, .677E-3) ! C{12}H5Cl5 PCB-102
    CALL CalcH("2,2',4,5',6-pentachlorobiphenyl",           "60145-21-3", .911E-7, .843E-4, .108E-2) ! C{12}H5Cl5 PCB-103
    CALL CalcH("2,2',4,6,6'-pentachlorobiphenyl",           "56558-16-8", .168E-6, .920E-4, .183E-2) ! C{12}H5Cl5 PCB-104
    CALL CalcH("2,3,3',4,4'-pentachlorobiphenyl",           "32598-14-4", .698E-8, .124E-3, .561E-4) ! C{12}H5Cl5 PCB-105
    CALL CalcH("2,3,3',4,5-pentachlorobiphenyl",            "70424-69-0", .493E-7, .126E-3, .393E-3) ! C{12}H5Cl5 PCB-106
    CALL CalcH("2,3,3',4',5-pentachlorobiphenyl",           "70424-68-9", .104E-7, .110E-3, .950E-4) ! C{12}H5Cl5 PCB-107
    CALL CalcH("2,3,3',4,5'-pentachlorobiphenyl",           "70362-41-3", .103E-7, .109E-3, .944E-4) ! C{12}H5Cl5 PCB-108
    CALL CalcH("2,3,3',4,6-pentachlorobiphenyl",            "74472-35-8", .622E-7, .133E-3, .469E-3) ! C{12}H5Cl5 PCB-109
    CALL CalcH("2,3,3',4',6-pentachlorobiphenyl",           "38380-03-9", .225E-7, .133E-3, .169E-3) ! C{12}H5Cl5 PCB-110
    CALL CalcH("2,3,3',5,5'-pentachlorobiphenyl",           "39635-32-0", .154E-7, .964E-4, .160E-3) ! C{12}H5Cl5 PCB-111
    CALL CalcH("2,3,3',5,6-pentachlorobiphenyl",            "74472-36-9", .583E-7, .142E-3, .412E-3) ! C{12}H5Cl5 PCB-112
    CALL CalcH("2,3,3',5',6-pentachlorobiphenyl",           "68194-10-5", .333E-7, .117E-3, .285E-3) ! C{12}H5Cl5 PCB-113
    CALL CalcH("2,3,4,4',5-pentachlorobiphenyl",            "74472-37-0", .437E-7, .124E-3, .351E-3) ! C{12}H5Cl5 PCB-114
    CALL CalcH("2,3,4,4',6-pentachlorobiphenyl",            "74472-38-1", .551E-7, .131E-3, .420E-3) ! C{12}H5Cl5 PCB-115
    CALL CalcH("2,3,4,5,6-pentachlorobiphenyl",             "18259-05-7", .337E-7, .186E-3, .181E-3) ! C{12}H5Cl5 PCB-116
    CALL CalcH("2,3,4',5,6-pentachlorobiphenyl",            "68194-11-6", .516E-7, .140E-3, .368E-3) ! C{12}H5Cl5 PCB-117
    CALL CalcH("2,3',4,4',5-pentachlorobiphenyl",           "31508-00-6", .949E-8, .103E-3, .923E-4) ! C{12}H5Cl5 PCB-118
    CALL CalcH("2,3',4,4',6-pentachlorobiphenyl",           "56558-17-9", .245E-7, .108E-3, .226E-3) ! C{12}H5Cl5 PCB-119
    CALL CalcH("2,3',4,5,5'-pentachlorobiphenyl",           "68194-12-7", .140E-7, .905E-4, .155E-3) ! C{12}H5Cl5 PCB-120
    CALL CalcH("2,3',4,5',6-pentachlorobiphenyl",           "56558-18-0", .363E-7, .954E-4, .380E-3) ! C{12}H5Cl5 PCB-121
    CALL CalcH("2,3,3',4',5'-pentachlorobiphenyl",          "76842-07-4", .766E-8, .125E-3, .613E-4) ! C{12}H5Cl5 PCB-122
    CALL CalcH("2,3',4,4',5'-pentachlorobiphenyl",          "65510-44-3", .889E-8, .102E-3, .869E-4) ! C{12}H5Cl5 PCB-123
    CALL CalcH("2,3',4',5,5'-pentachlorobiphenyl",          "70424-70-3", .100E-7, .103E-3, .971E-4) ! C{12}H5Cl5 PCB-124
    CALL CalcH("2,3',4',5',6-pentachlorobiphenyl",          "74472-39-2", .185E-7, .125E-3, .148E-3) ! C{12}H5Cl5 PCB-125
    CALL CalcH("3,3',4,4',5-pentachlorobiphenyl",           "57465-28-8", .270E-8, .984E-4, .274E-4) ! C{12}H5Cl5 PCB-126
    CALL CalcH("3,3',4,5,5'-pentachlorobiphenyl",           "39635-33-1", .399E-8, .872E-4, .458E-4) ! C{12}H5Cl5 PCB-127
    CALL CalcH("2,2',3,3',4,4'-hexachlorobiphenyl",         "38380-07-3", .354E-8, .523E-4, .676E-4) ! C{12}H4Cl6 PCB-128
    CALL CalcH("2,2',3,3',4,5-hexachlorobiphenyl",          "55215-18-4", .205E-7, .527E-4, .390E-3) ! C{12}H4Cl6 PCB-129
    CALL CalcH("2,2',3,3',4,5'-hexachlorobiphenyl",         "52663-66-8", .528E-8, .465E-4, .114E-3) ! C{12}H4Cl6 PCB-130
    CALL CalcH("2,2',3,3',4,6-hexachlorobiphenyl",          "61798-70-7", .259E-7, .418E-4, .620E-3) ! C{12}H4Cl6 PCB-131
    CALL CalcH("2,2',3,3',4,6'-hexachlorobiphenyl",         "38380-05-1", .114E-7, .414E-4, .276E-3) ! C{12}H4Cl6 PCB-132
    CALL CalcH("2,2',3,3',5,5'-hexachlorobiphenyl",         "35694-04-3", .788E-8, .414E-4, .190E-3) ! C{12}H4Cl6 PCB-133
    CALL CalcH("2,2',3,3',5,6-hexachlorobiphenyl",          "52704-70-8", .243E-7, .443E-4, .548E-3) ! C{12}H4Cl6 PCB-134
    CALL CalcH("2,2',3,3',5,6'-hexachlorobiphenyl",         "52744-13-5", .171E-7, .369E-4, .462E-3) ! C{12}H4Cl6 PCB-135
    CALL CalcH("2,2',3,3',6,6'-hexachlorobiphenyl",         "38411-22-2", .369E-7, .401E-4, .920E-3) ! C{12}H4Cl6 PCB-136
    CALL CalcH("2,2',3,4,4',5-hexachlorobiphenyl",          "35694-06-5", .238E-7, .439E-4, .543E-3) ! C{12}H4Cl6 PCB-137
    CALL CalcH("2,2',3,4,4',5'-hexachlorobiphenyl",         "35065-28-2", .481E-8, .440E-4, .109E-3) ! C{12}H4Cl6 PCB-138
    CALL CalcH("2,2',3,4,4',6-hexachlorobiphenyl",          "56030-56-9", .301E-7, .350E-4, .861E-3) ! C{12}H4Cl6 PCB-139
    CALL CalcH("2,2',3,4,4',6'-hexachlorobiphenyl",         "59291-64-4", .124E-7, .346E-4, .359E-3) ! C{12}H4Cl6 PCB-140
    CALL CalcH("2,2',3,4,5,5'-hexachlorobiphenyl",          "52712-04-6", .269E-7, .443E-4, .607E-3) ! C{12}H4Cl6 PCB-141
    CALL CalcH("2,2',3,4,5,6-hexachlorobiphenyl",           "41411-61-4", .337E-7, .475E-4, .708E-3) ! C{12}H4Cl6 PCB-142
    CALL CalcH("2,2',3,4,5,6'-hexachlorobiphenyl",          "68194-15-0", .498E-7, .396E-4, .126E-2) ! C{12}H4Cl6 PCB-143
    CALL CalcH("2,2',3,4,5',6-hexachlorobiphenyl",          "68194-14-9", .340E-7, .350E-4, .970E-3) ! C{12}H4Cl6 PCB-144
    CALL CalcH("2,2',3,4,6,6'-hexachlorobiphenyl",          "74472-40-5", .628E-7, .379E-4, .166E-2) ! C{12}H4Cl6 PCB-145
    CALL CalcH("2,2',3,4',5,5'-hexachlorobiphenyl",         "51908-16-8", .718E-8, .392E-4, .183E-3) ! C{12}H4Cl6 PCB-146
    CALL CalcH("2,2',3,4',5,6-hexachlorobiphenyl",          "68194-13-8", .282E-7, .370E-4, .762E-3) ! C{12}H4Cl6 PCB-147
    CALL CalcH("2,2',3,4',5,6'-hexachlorobiphenyl",         "74472-41-6", .185E-7, .310E-4, .599E-3) ! C{12}H4Cl6 PCB-148
    CALL CalcH("2,2',3,4',5',6-hexachlorobiphenyl",         "38380-04-0", .155E-7, .351E-4, .442E-3) ! C{12}H4Cl6 PCB-149
    CALL CalcH("2,2',3,4',6,6'-hexachlorobiphenyl",         "68194-08-1", .402E-7, .336E-4, .120E-2) ! C{12}H4Cl6 PCB-150
    CALL CalcH("2,2',3,5,5',6-hexachlorobiphenyl",          "52663-63-5", .318E-7, .371E-4, .858E-3) ! C{12}H4Cl6 PCB-151
    CALL CalcH("2,2',3,5,6,6'-hexachlorobiphenyl",          "68194-09-2", .588E-7, .402E-4, .146E-2) ! C{12}H4Cl6 PCB-152
    CALL CalcH("2,2',4,4',5,5'-hexachlorobiphenyl",         "35065-27-1", .654E-8, .370E-4, .177E-3) ! C{12}H4Cl6 PCB-153
    CALL CalcH("2,2',4,4',5,6'-hexachlorobiphenyl",         "60145-22-4", .169E-7, .293E-4, .577E-3) ! C{12}H4Cl6 PCB-154
    CALL CalcH("2,2',4,4',6,6'-hexachlorobiphenyl",         "33979-03-2", .437E-7, .281E-4, .155E-2) ! C{12}H4Cl6 PCB-155
    CALL CalcH("2,3,3',4,4',5-hexachlorobiphenyl",          "38380-08-4", .724E-8, .420E-4, .173E-3) ! C{12}H4Cl6 PCB-156
    CALL CalcH("2,3,3',4,4',5'-hexachlorobiphenyl",         "69782-90-7", .137E-8, .420E-4, .326E-4) ! C{12}H4Cl6 PCB-157
    CALL CalcH("2,3,3',4,4',6-hexachlorobiphenyl",          "74472-42-7", .914E-8, .441E-4, .207E-3) ! C{12}H4Cl6 PCB-158
    CALL CalcH("2,3,3',4,5,5'-hexachlorobiphenyl",          "39635-35-3", .107E-7, .374E-4, .287E-3) ! C{12}H4Cl6 PCB-159
    CALL CalcH("2,3,3',4,5,6-hexachlorobiphenyl",           "41411-62-5", .131E-7, .530E-4, .248E-3) ! C{12}H4Cl6 PCB-160
    CALL CalcH("2,3,3',4,5',6-hexachlorobiphenyl",          "74472-43-8", .135E-7, .392E-4, .345E-3) ! C{12}H4Cl6 PCB-161
    CALL CalcH("2,3,3',4',5,5'-hexachlorobiphenyl",         "39635-34-2", .204E-8, .375E-4, .545E-4) ! C{12}H4Cl6 PCB-162
    CALL CalcH("2,3,3',4',5,6-hexachlorobiphenyl",          "74472-44-9", .856E-8, .468E-4, .183E-3) ! C{12}H4Cl6 PCB-163
    CALL CalcH("2,3,3',4',5',6-hexachlorobiphenyl",         "74472-45-0", .442E-8, .446E-4, .991E-4) ! C{12}H4Cl6 PCB-164
    CALL CalcH("2,3,3',5,5',6-hexachlorobiphenyl",          "74472-46-1", .127E-7, .416E-4, .305E-3) ! C{12}H4Cl6 PCB-165
    CALL CalcH("2,3,4,4',5,6-hexachlorobiphenyl",           "41411-63-6", .116E-7, .525E-4, .222E-3) ! C{12}H4Cl6 PCB-166
    CALL CalcH("2,3',4,4',5,5'-hexachlorobiphenyl",         "52663-72-6", .186E-8, .354E-4, .526E-4) ! C{12}H4Cl6 PCB-167
    CALL CalcH("2,3',4,4',5',6-hexachlorobiphenyl",         "59291-65-5", .481E-8, .371E-4, .129E-3) ! C{12}H4Cl6 PCB-168
    CALL CalcH("3,3',4,4',5,5'-hexachlorobiphenyl",         "32774-16-6", .529E-9, .341E-4, .155E-4) ! C{12}H4Cl6 PCB-169
    CALL CalcH("2,2',3,3',4,4',5-heptachlorobiphenyl",      "35065-30-6", .367E-8, .194E-4, .190E-3) ! C{12}H3Cl7 PCB-170
    CALL CalcH("2,2',3,3',4,4',6-heptachlorobiphenyl",      "52663-71-5", .463E-8, .158E-4, .294E-3) ! C{12}H3Cl7 PCB-171
    CALL CalcH("2,2',3,3',4,5,5'-heptachlorobiphenyl",      "52663-74-8", .548E-8, .174E-4, .314E-3) ! C{12}H3Cl7 PCB-172
    CALL CalcH("2,2',3,3',4,5,6-heptachlorobiphenyl",       "68194-16-1", .548E-8, .185E-4, .295E-3) ! C{12}H3Cl7 PCB-173
    CALL CalcH("2,2',3,3',4,5,6'-heptachlorobiphenyl",      "38411-25-5", .119E-7, .158E-4, .753E-3) ! C{12}H3Cl7 PCB-174
    CALL CalcH("2,2',3,3',4,5',6-heptachlorobiphenyl",      "40186-70-7", .691E-8, .141E-4, .489E-3) ! C{12}H3Cl7 PCB-175
    CALL CalcH("2,2',3,3',4,6,6'-heptachlorobiphenyl",      "52663-65-7", .150E-7, .152E-4, .987E-3) ! C{12}H3Cl7 PCB-176
    CALL CalcH("2,2',3,3',4,5',6'-heptachlorobiphenyl",     "52663-70-4", .434E-8, .166E-4, .261E-3) ! C{12}H3Cl7 PCB-177
    CALL CalcH("2,2',3,3',5,5',6-heptachlorobiphenyl",      "52663-67-9", .648E-8, .149E-4, .435E-3) ! C{12}H3Cl7 PCB-178
    CALL CalcH("2,2',3,3',5,6,6'-heptachlorobiphenyl",      "52663-64-6", .140E-7, .160E-4, .878E-3) ! C{12}H3Cl7 PCB-179
    CALL CalcH("2,2',3,4,4',5,5'-heptachlorobiphenyl",      "35065-29-3", .499E-8, .166E-4, .300E-3) ! C{12}H3Cl7 PCB-180
    CALL CalcH("2,2',3,4,4',5,6-heptachlorobiphenyl",       "74472-47-2", .636E-8, .158E-4, .403E-3) ! C{12}H3Cl7 PCB-181
    CALL CalcH("2,2',3,4,4',5,6'-heptachlorobiphenyl",      "60145-23-5", .129E-7, .135E-4, .958E-3) ! C{12}H3Cl7 PCB-182
    CALL CalcH("2,2',3,4,4',5',6-heptachlorobiphenyl",      "52663-69-1", .630E-8, .135E-4, .467E-3) ! C{12}H3Cl7 PCB-183
    CALL CalcH("2,2',3,4,4',6,6'-heptachlorobiphenyl",      "74472-48-3", .163E-7, .130E-4, .125E-2) ! C{12}H3Cl7 PCB-184
    CALL CalcH("2,2',3,4,5,5',6-heptachlorobiphenyl",       "52712-05-7", .718E-8, .158E-4, .454E-3) ! C{12}H3Cl7 PCB-185
    CALL CalcH("2,2',3,4,5,6,6'-heptachlorobiphenyl",       "74472-49-4", .133E-7, .170E-4, .781E-3) ! C{12}H3Cl7 PCB-186
    CALL CalcH("2,2',3,4',5,5',6-heptachlorobiphenyl",      "52663-68-0", .590E-8, .142E-4, .416E-3) ! C{12}H3Cl7 PCB-187
    CALL CalcH("2,2',3,4',5,6,6'-heptachlorobiphenyl",      "74487-85-7", .153E-7, .136E-4, .112E-2) ! C{12}H3Cl7 PCB-188
    CALL CalcH("2,3,3',4,4',5,5'-heptachlorobiphenyl",      "39635-31-9", .142E-8, .160E-4, .890E-4) ! C{12}H3Cl7 PCB-189
    CALL CalcH("2,3,3',4,4',5,6-heptachlorobiphenyl",       "41411-64-7", .193E-8, .194E-4, .993E-4) ! C{12}H3Cl7 PCB-190
    CALL CalcH("2,3,3',4,4',5',6-heptachlorobiphenyl",      "74472-50-7", .285E-8, .167E-4, .171E-3) ! C{12}H3Cl7 PCB-191
    CALL CalcH("2,3,3',4,5,5',6-heptachlorobiphenyl",       "74472-51-8", .285E-8, .175E-4, .163E-3) ! C{12}H3Cl7 PCB-192
    CALL CalcH("2,3,3',4',5,5',6-heptachlorobiphenyl",      "69782-91-8", .168E-8, .176E-4, .957E-4) ! C{12}H3Cl7 PCB-193
    CALL CalcH("2,2',3,3',4,4',5,5'-octachlorobiphenyl",    "35694-08-7", .381E-8, .813E-5, .469E-3) ! C{12}H2Cl8 PCB-194
    CALL CalcH("2,2',3,3',4,4',5,6-octachlorobiphenyl",     "52663-78-2", .979E-9, .777E-5, .126E-3) ! C{12}H2Cl8 PCB-195
    CALL CalcH("2,2',3,3',4,4',5,6'-octachlorobiphenyl",    "42740-50-1", .481E-8, .677E-5, .710E-3) ! C{12}H2Cl8 PCB-196
    CALL CalcH("2,2',3,3',4,4',6,6'-octachlorobiphenyl",    "33091-17-7", .607E-8, .654E-5, .928E-3) ! C{12}H2Cl8 PCB-197
    CALL CalcH("2,2',3,3',4,5,5',6-octachlorobiphenyl",     "68194-17-2", .146E-8, .706E-5, .207E-3) ! C{12}H2Cl8 PCB-198
    CALL CalcH("2,2',3,3',4,5,5',6'-octachlorobiphenyl",    "52663-75-9", .316E-8, .751E-5, .421E-3) ! C{12}H2Cl8 PCB-199
    CALL CalcH("2,2',3,3',4,5,6,6'-octachlorobiphenyl",     "52663-73-7", .450E-8, .681E-5, .661E-3) ! C{12}H2Cl8 PCB-200
    CALL CalcH("2,2',3,3',4,5',6,6'-octachlorobiphenyl",    "40186-71-8", .450E-8, .707E-5, .637E-3) ! C{12}H2Cl8 PCB-201
    CALL CalcH("2,2',3,3',5,5',6,6'-octachlorobiphenyl",     "2136-99-4", .533E-8, .712E-5, .748E-3) ! C{12}H2Cl8 PCB-202
    CALL CalcH("2,2',3,4,4',5,5',6-octachlorobiphenyl",     "52663-76-0", .133E-8, .677E-5, .196E-3) ! C{12}H2Cl8 PCB-203
    CALL CalcH("2,2',3,4,4',5,6,6'-octachlorobiphenyl",     "74472-52-9", .344E-8, .654E-5, .526E-3) ! C{12}H2Cl8 PCB-204
    CALL CalcH("2,3,3',4,4',5,5',6-octachlorobiphenyl",     "74472-53-0", .379E-9, .815E-5, .464E-4) ! C{12}H2Cl8 PCB-205
    CALL CalcH("2,2',3,3',4,4',5,5',6-nonachlorobiphenyl",  "40186-72-9", .102E-8, .372E-5, .273E-3) ! C{12}HCl9 PCB-206
    CALL CalcH("2,2',3,3',4,4',5,6,6'-nonachlorobiphenyl",  "52663-79-3", .128E-8, .361E-5, .355E-3) ! C{12}HCl9 PCB-207
    CALL CalcH("2,2',3,3',4,5,5',6,6'-nonachlorobiphenyl",  "52663-77-1", .120E-8, .374E-5, .321E-3) ! C{12}HCl9 PCB-208
    CALL CalcH("decachlorobiphenyl",                         "2051-24-3", .271E-9, .219E-5, .123E-3) ! C{12}Cl{10} PCB-209

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, vp, as, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: vp ! vapor pressure
      REAL, INTENT(IN)             :: as ! aqueous solubility
      REAL, INTENT(IN)             :: H
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL consistency_check(vp/as, H, &
        "Recalculated values ($p/c$") ! add ",.TRUE." for verbose mode
      Hominus = 1. / (atm * H)
      CALL Output(Hominus)

    END SUBROUTINE CalcH

  END SUBROUTINE ref0850

  !---------------------------------------------------------------------------

  SUBROUTINE ref0852 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "852"
    chem = "dodecachloropentacyclodecane" ; casrn = "2385-85-5"
    type = "M"
    mindHR = LOG(10.) * 4711.
    Hominus = KHpcSI_TIMES_HcpSI/(atm*(10.**(12.709-4711./T0)))
    CALL Output(Hominus,mindHR)

  END SUBROUTINE ref0852

  !---------------------------------------------------------------------------

  SUBROUTINE ref0853 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "853"
    type = "M"

    ! Table I:
    CALL CalcH("PCB-4",   "13029-08-8", 2.98E-4)
    CALL CalcH("PCB-6",   "25569-80-6", 3.04E-4)
    CALL CalcH("PCB-7",   "33284-50-3", 3.59E-4)
    CALL CalcH("PCB-8",   "34883-43-7", 2.80E-4)
    CALL CalcH("PCB-16",  "38444-78-9", 2.38E-4)
    CALL CalcH("PCB-17",  "37680-66-3", 3.26E-4)
    CALL CalcH("PCB-18",  "37680-65-2", 2.99E-4)
    CALL CalcH("PCB-19",  "38444-73-4", 3.03E-4)
    CALL CalcH("PCB-22",  "38444-85-8", 1.99E-4)
    CALL CalcH("PCB-24",  "55702-45-9", 3.17E-4)
    CALL CalcH("PCB-25",  "55712-37-3", 4.04E-4)
    CALL CalcH("PCB-26",  "38444-81-4", 3.39E-4)
    CALL CalcH("PCB-27",  "38444-76-7", 2.81E-4)
    CALL CalcH("PCB-28",   "7012-37-5", 2.64E-4)
    CALL CalcH("PCB-31",  "16606-02-3", 2.64E-4)
    CALL CalcH("PCB-33",  "38444-86-9", 2.24E-4)
    CALL CalcH("PCB-37",  "38444-90-5", 1.52E-4)
    CALL CalcH("PCB-40",  "38444-93-8", 1.61E-4)
    CALL CalcH("PCB-41",  "52663-59-9", 2.01E-4)
    CALL CalcH("PCB-42",  "36559-22-5", 1.99E-4)
    CALL CalcH("PCB-44",  "41464-39-5", 1.89E-4)
    CALL CalcH("PCB-46",  "41464-47-5", 2.57E-4)
    CALL CalcH("PCB-48",  "70362-47-9", 2.55E-4)
    CALL CalcH("PCB-49",  "41464-40-8", 2.76E-4)
    CALL CalcH("PCB-52",  "35693-99-3", 2.38E-4)
    CALL CalcH("PCB-53",  "41464-41-9", 2.83E-4)
    CALL CalcH("PCB-56",  "41464-43-1", 1.62E-4)
    CALL CalcH("PCB-60",  "33025-41-1", 1.62E-4)
    CALL CalcH("PCB-63",  "74472-34-7", 2.89E-4)
    CALL CalcH("PCB-64",  "52663-58-8", 1.70E-4)
    CALL CalcH("PCB-66",  "32598-10-0", 2.01E-4)
    CALL CalcH("PCB-70",  "32598-11-1", 1.88E-4)
    CALL CalcH("PCB-74",  "32690-93-0", 2.09E-4)
    CALL CalcH("PCB-76",  "70362-48-0", 1.28E-4)
    CALL CalcH("PCB-82",  "52663-62-4", 1.17E-4)
    CALL CalcH("PCB-83",  "60145-20-2", 1.64E-4)
    CALL CalcH("PCB-84",  "52663-60-2", 1.74E-4)
    CALL CalcH("PCB-85",  "65510-45-4", 1.65E-4)
    CALL CalcH("PCB-87",  "38380-02-8", 1.27E-4)
    CALL CalcH("PCB-91",  "68194-05-8", 2.71E-4)
    CALL CalcH("PCB-95",  "38379-99-6", 1.98E-4)
    CALL CalcH("PCB-97",  "41464-51-1", 1.49E-4)
    CALL CalcH("PCB-99",  "38380-01-7", 2.14E-4)
    CALL CalcH("PCB-101", "37680-73-2", 1.79E-4)
    CALL CalcH("PCB-107", "70424-68-9", 0.57E-4)
    CALL CalcH("PCB-110", "38380-03-9", 1.06E-4)
    CALL CalcH("PCB-114", "74472-37-0", 0.69E-4)
    CALL CalcH("PCB-118", "31508-00-6", 0.85E-4)
    CALL CalcH("PCB-122", "76842-07-4", 0.60E-4)
    CALL CalcH("PCB-124", "70424-70-3", 0.53E-4)
    CALL CalcH("PCB-128", "38380-07-3", 0.57E-4)
    CALL CalcH("PCB-130", "52663-66-8", 1.07E-4)
    CALL CalcH("PCB-131", "61798-70-7", 0.65E-4)
    CALL CalcH("PCB-134", "52704-70-8", 0.97E-4)
    CALL CalcH("PCB-135", "52744-13-5", 1.40E-4)
    CALL CalcH("PCB-136", "38411-22-2", 2.25E-4)
    CALL CalcH("PCB-137", "35694-06-5", 0.68E-4)
    CALL CalcH("PCB-138", "35065-28-2", 0.75E-4)
    CALL CalcH("PCB-141", "52712-04-6", 0.97E-4)
    CALL CalcH("PCB-144", "68194-14-9", 1.40E-4)
    CALL CalcH("PCB-146", "51908-16-8", 0.88E-4)
    CALL CalcH("PCB-149", "38380-04-0", 1.48E-4)
    CALL CalcH("PCB-151", "52663-63-5", 1.57E-4)
    CALL CalcH("PCB-153", "35065-27-1", 0.99E-4)
    CALL CalcH("PCB-158", "74472-42-7", 0.43E-4)
    CALL CalcH("PCB-170", "35065-30-6", 0.15E-4)
    CALL CalcH("PCB-174", "38411-25-5", 0.49E-4)
    CALL CalcH("PCB-176", "52663-65-7", 0.90E-4)
    CALL CalcH("PCB-177", "52663-70-4", 0.33E-4)
    CALL CalcH("PCB-178", "52663-67-9", 0.65E-4)
    CALL CalcH("PCB-180", "35065-29-3", 0.32E-4)
    CALL CalcH("PCB-183", "52663-69-1", 0.68E-4)
    CALL CalcH("PCB-187", "52663-68-0", 0.82E-4)

    ! Table II:
    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/4.,20./) + CtoK

    !chem = "aroclor1242" ; casrn = "53469-21-9"
    !Harray = KHpcSI_TIMES_HcpSI / ( atm * 1E-4 * (/ 0.37, 2.8 /) )
    !CALL HTdep(temp, Harray, Hominus, mindHR)
    !CALL Output(Hominus, mindHR, r2)

    !chem = "aroclor1254" ; casrn = "11097-69-1"
    !Harray = KHpcSI_TIMES_HcpSI / ( atm * 1E-4 * (/ 0.28, 1.9 /) )
    !CALL HTdep(temp, Harray, Hominus, mindHR)
    !CALL Output(Hominus, mindHR, r2)

    !chem = "aroclor1260" ; casrn = "11096-82-5"
    !Harray = KHpcSI_TIMES_HcpSI / ( atm * 1E-4 * (/ 0.25, 1.7 /) )
    !CALL HTdep(temp, Harray, Hominus, mindHR)
    !CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

    ! toxaphene is a mixture, not a pure substance:
    ! CALL CalcH("toxaphene", "8001-35-2", 0.06)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL, INTENT(IN)             :: HLC
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("293")
      CALL Output(1./(atm*HLC))

    END SUBROUTINE CalcH

  END SUBROUTINE ref0853

  !---------------------------------------------------------------------------

  SUBROUTINE ref0854 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "854"
    type = "V"

    CALL CalcH("dibenzo[$b$,$e$][1,4]dioxin",                            "262-12-4", 12.29)  ! C{12}H8O2 dibenzo-$p$-dioxin
    CALL CalcH("1-chlorodibenzo[$b$,$e$][1,4]dioxin",                  "39227-53-7",  8.38)  ! C{12}H7ClO2 PCDD-1
    CALL CalcH("2-chlorodibenzo[$b$,$e$][1,4]dioxin",                  "39227-54-8", 14.82)  ! C{12}H7ClO2 PCDD-2
    CALL CalcH("2,3-dichlorodibenzo[$b$,$e$][1,4]dioxin",              "29446-15-9",  6.61)  ! C{12}H6Cl2O2 PCDD-23
    CALL CalcH("2,7-dichlorodibenzo[$b$,$e$][1,4]dioxin",              "33857-26-0",  8.11)  ! C{12}H6Cl2O2 PCDD-27
    CALL CalcH("2,8-dichlorodibenzo[$b$,$e$][1,4]dioxin",              "38964-22-6",  2.13)  ! C{12}H6Cl2O2 PCDD-28
    CALL CalcH("1,2,4-trichlorodibenzo[$b$,$e$][1,4]dioxin",           "39227-58-2",  3.84)  ! C{12}H5Cl3O2 PCDD-124
    CALL CalcH("1,2,3,4-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",       "30746-58-8",  3.77)  ! C{12}H4Cl4O2 PCDD-1234
    CALL CalcH("1,2,3,7-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",       "67028-18-6",  0.77)  ! C{12}H4Cl4O2 PCDD-1237
    CALL CalcH("1,3,6,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",       "33423-92-6",  0.71)  ! C{12}H4Cl4O2 PCDD-1368

    ! three values are given for next species:
    CALL CalcH("2,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "1746-01-6",  1.63)  ! C{12}H4Cl4O2 PCDD-2378
    CALL CalcH("2,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "1746-01-6",  3.34)  ! C{12}H4Cl4O2 PCDD-2378
    CALL CalcH("2,3,7,8-tetrachlorodibenzo[$b$,$e$][1,4]dioxin",        "1746-01-6", 10.34)  ! C{12}H4Cl4O2 PCDD-2378

    CALL CalcH("1,2,3,4,7-pentachlorodibenzo[$b$,$e$][1,4]dioxin",     "39227-61-7",  0.264) ! C{12}H3Cl5O2 PCDD-12347
    CALL CalcH("1,2,3,4,7,8-hexachlorodibenzo[$b$,$e$][1,4]dioxin",    "39227-28-6",  4.52)  ! C{12}H2Cl6O2 PCDD-123478
    CALL CalcH("1,2,3,4,6,7,8-heptachlorodibenzo[$b$,$e$][1,4]dioxin", "35822-46-9",  0.133) ! C{12}HCl7O2 PCDD-1234678
    CALL CalcH("octachlorodibenzo[$b$,$e$][1,4]dioxin",                 "3268-87-9",  0.683) ! C{12}Cl8O2 PCDD-12346789

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      REAL,             INTENT(IN) :: H
      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it
      CALL Output(KHpcSI_TIMES_HcpSI/H)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0854

  !---------------------------------------------------------------------------

  SUBROUTINE ref0855 ! KHpx [kPa]
    IMPLICIT NONE

    ref = "855"

    CALL CalcH("dichloromethane",            "75-09-2", &
      (/10.,20.,30./),(/7090.,11600.,17100./))
    CALL CalcH("trichloromethane",           "67-66-3", &
      (/20.,35.,50./),(/17000.,32900.,58100./))
    CALL CalcH("tetrachloromethane",         "56-23-5", &
      (/20.,30.,40./),(/147000.,244000.,368000./))
    CALL CalcH("1,1-dichloroethane",         "75-34-3", &
      (/20.,35.,45./),(/26500.,55300.,68100./))
    CALL CalcH("1,2-dichloroethane",        "107-06-2", &
      (/20.,35.,50./),(/5090.,10300.,17700./))
    CALL CalcH("1,1,1-trichloroethane",      "71-55-6", &
      (/20.,30.,40./),(/76600.,111000.,164000./))
    CALL CalcH("1,1,2-trichloroethane",      "79-00-5", &
      (/20.,35.,50./),(/3730.,7550.,13000./))
    CALL CalcH("1,1,1,2-tetrachloroethane", "630-20-6", &
      (/20.,30.,40./),(/10900.,17800.,31100./))
    CALL CalcH("1,1,2,2-tetrachloroethane",  "79-34-5", &
      (/20.,30.,40./),(/2240.,3170.,6660./))
    CALL CalcH("(Z)-1,2-dichloroethene",    "156-59-2", &
      (/20.,30.,40./),(/18400.,28800.,41700./))
    CALL CalcH("(E)-1,2-dichloroethene",    "156-60-5", &
      (/20.,30.,40./),(/42700.,68800.,103000./))
    CALL CalcH("trichloroethene",            "79-01-6", &
      (/20.,30.,40./),(/42000.,64000.,106000./))
    CALL CalcH("1,2-dichloropropane",        "78-87-5", &
      (/20.,30.,40./),(/12000.,19300.,27600./))
    CALL CalcH("1,3-dichloropropene",       "542-75-6", &
      (/20.,30.,40./),(/6760.,11000.,16800./))
    CALL CalcH("dibromomethane",             "74-95-3", &
      (/20.,35.,50./),(/4040.,7840.,14000./))
    CALL CalcH("tribromomethane",            "75-25-2", &
      (/20.,35.,50./),(/1890.,3980.,11500./))

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, temp_, dpdx)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, dpdx

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      type = "M"
      ndata = SIZE(dpdx)
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = temp_ + CtoK
      Harray = cH2O / (1E3*dpdx)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0855

  !---------------------------------------------------------------------------

  SUBROUTINE ref0856 ! KHpc [atm*m3/mol]
    IMPLICIT NONE

    ref = "856"
    type = "M"

    CALL CalcH("bromodichloromethane",       "75-27-4", &
      (/ 20., 30., 40. /),      (/ 0.0016, 0.0026, 0.0040 /) )
    CALL CalcH("tribromomethane",            "75-25-2", &
      (/ 20., 30., 40. /),      (/ 0.0004, 0.0007, 0.0012 /) )
    CALL CalcH("tetrachloromethane",         "56-23-5", &
      (/ 20., 30., 35., 40. /), (/ 0.0204, 0.0337, 0.0382, 0.0452 /) )
    CALL CalcH("dibromochloromethane",      "124-48-1", &
      (/ 20., 30., 40. /),      (/ 0.0008, 0.0014, 0.0022 /) )
    CALL CalcH("dibromomethane",             "74-95-3", &
      (/ 20., 30., 35., 40. /), (/ 0.0007, 0.0011, 0.0014, 0.0017 /) )
    CALL CalcH("1,1-dichloroethane",         "75-34-3", &
      (/ 20., 30., 40. /),      (/ 0.0046, 0.0070, 0.0102 /) )
    CALL CalcH("1,2-dichloroethane",        "107-06-2", &
      (/ 20., 30., 35., 40. /), (/ 0.0010, 0.0015, 0.0018, 0.0022 /) )
    CALL CalcH("1,1-dichloroethene",         "75-35-4", &
      (/ 20., 30., 40. /),      (/ 0.0229, 0.0337, 0.0475 /) )
    CALL CalcH("(Z)-1,2-dichloroethene",    "156-59-2", &
      (/ 20., 30., 40. /),      (/ 0.0032, 0.0049, 0.0073 /) )
    CALL CalcH("(E)-1,2-dichloroethene",    "156-60-5", &
      (/ 20., 30., 40. /),      (/ 0.0079, 0.0118, 0.0177 /) )
    CALL CalcH("dichloromethane",            "75-09-2", &
      (/ 20., 30., 35., 40. /), (/ 0.0021, 0.0031, 0.0037, 0.0045 /) )
    CALL CalcH("1,2-dichloropropane",        "78-87-5", &
      (/ 20., 30., 40. /),      (/ 0.0021, 0.0032, 0.0048 /) )
    CALL CalcH("1,1,1,2-tetrachloroethane", "630-20-6", &
      (/ 20., 30., 35., 40. /), (/ 0.0017, 0.0028, 0.0036, 0.0046 /) )
    CALL CalcH("1,1,2,2-tetrachloroethane",  "79-34-5", &
      (/ 20., 30., 35., 40. /), (/ 0.0003, 0.0005, 0.0006, 0.0009 /) )
    CALL CalcH("1,1,1-trichloroethane",      "71-55-6", &
      (/ 20., 30., 35., 40. /), (/ 0.0126, 0.0200, 0.0235, 0.0281 /) )
    CALL CalcH("1,1,2-trichloroethane",      "79-00-5", &
      (/ 20., 30., 40. /),      (/ 0.0007, 0.0011, 0.0017 /) )
    CALL CalcH("trichloroethene",            "79-01-6", &
      (/ 20., 30., 40. /),      (/ 0.0070, 0.0114, 0.0173 /) )

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, temp_, H)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, H

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      type = "M"
      ndata = SIZE(H)
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = temp_ + CtoK
      Harray = KHpcSI_TIMES_HcpSI / ( atm * H )
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0856

  !---------------------------------------------------------------------------

  SUBROUTINE ref0857 ! Hcc [1]
    IMPLICIT NONE

    ref = "857"

    CALL CalcH("tetrachloroethene",    "127-18-4", (/40.,60.,70.,80./), (/1.48,1.27,0.78,0.87/))
    CALL CalcH("1,1,1-trichloroethane", "71-55-6", (/40.,60.,70.,80./), (/1.65,1.47,1.26,1.18/))
    CALL CalcH("1,2-dimethylbenzene",   "95-47-6", (/40.,60.,70.,80./), (/2.44,1.31,1.01,0.99/))
    CALL CalcH("cyclohexane",          "110-82-7", (/40.,60.,70.,80./), (/0.07,0.05,0.03,0.02/))
    CALL CalcH("hexane",               "110-54-3", (/40.,60.,70.    /), (/0.14,0.043,0.012/))
    CALL CalcH("ethyl ethanoate",      "141-78-6", (/40.,60.,70.,80./), (/62.4,29.3,21.8,17.5/))
    CALL CalcH("butyl ethanoate",      "123-86-4", (/40.,60.,70.,80./), (/31.4,13.6,9.82,7.58/))
    CALL CalcH("methylbenzene",        "108-88-3", (/40.,60.,70.,80./), (/2.82,1.77,1.49,1.27/))
    CALL CalcH("2-propanol",            "67-63-0", (/40.,60.,70.,80./), (/825.,286.,179.,117./))
    CALL CalcH("4-methyl-2-pentanone", "108-10-1", (/40.,60.,70.,80./), (/54.3,22.8,16.2,11.8/))
    CALL CalcH("1,4-dioxane",          "123-91-1", (/40.,60.,70.,80./), (/1618.,642.,412.,288./))
    CALL CalcH("1-butanol",             "71-36-3", (/40.,60.,70.,80./), (/647.,238.,144.,98.9/))

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, temp_, K)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: temp_, K

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      type = "M"
      ndata = SIZE(K)
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = temp_ + CtoK
      Harray = Hcc_TO_HcpSI(K,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL MakeNote(TRIM(ref), &
      "Extrapolated from data measured between 40~\unit{\degree C} "// &
      "and 80~\unit{\degree C}.")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0857

  !---------------------------------------------------------------------------

  SUBROUTINE ref0858 ! Hcc [1]
    IMPLICIT NONE

    ref = "858"

    CALL CalcH("benzene",        "71-43-2", (/ 2.90,2.27,1.71,1.66 /) )
    CALL CalcH("methylbenzene", "108-88-3", (/ 2.37,1.86,1.52,1.21 /) )
    CALL CalcH("chlorobenzene", "108-90-7", (/ 4.26,3.21,2.56,1.86 /) )
    CALL CalcH("2-butanone",     "78-93-3", (/ 144.7,69.2,51.1,37.1 /) )

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, K)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: CHEM_
      CHARACTER(LEN=*),   INTENT(IN) :: CASRN_
      REAL, DIMENSION(:), INTENT(IN) :: K

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      type = "M"
      ndata = SIZE(K)
      ALLOCATE(temp(ndata), Harray(ndata))
      temp = (/ 318.16, 333.16, 343.16, 353.16 /)
      Harray = Hcc_TO_HcpSI(K,temp)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL MakeNote("highTextrapol")
      CALL Output(Hominus, mindHR, r2)
      DEALLOCATE(temp, Harray)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0858

  !---------------------------------------------------------------------------

  ! ref0861 Br2, I2 (old data) note that 753 is based on it

  !---------------------------------------------------------------------------

  SUBROUTINE ref0862 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ! The absorption coefficient beta is not defined in this paper but
    ! it is assumed that it has the same definition as in ref3026.

    ref = "862"

    type = "M"
    CALL CalcH("argon", "7440-37-1", &
      (/ 0., 10., 20., 30., 40. /), &
      (/ 0.053, 0.042, 0.035, 0.030, 0.027 /) ) ! Ar

    type = "M"
    CALL CalcH("bromine (molecular)", "7726-95-6", &
      (/ 0., 10., 20., 30., 40., 50., 60. /), &
      (/ 60.5, 35.1, 21.3, 13.8, 9.4, 6.5, 4.9 /) ) ! Br2

    type = "M"
    CALL CalcH("hydrogen sulfide", "7783-06-4", &
      (/ 0., 10., 20., 30., 40., 50., 60. /), &
      (/ 4.621, 3.362, 2.554, 2.014, 1.642, 1.376, 1.176 /) ) ! H2S

    type = "M"
    CALL CalcH("carbon oxide sulfide", "463-58-1", &
      (/ 0., 10., 20., 30. /), &
      (/ 1.333, 0.835, 0.561, 0.403 /) ) ! OCS carbonyl sulfide

    type = "V"
    CALL CalcH("carbon disulfide", "75-15-0", &
      (/ 0., 10., 20., 30. /), &
      (/ 3.573, 2.189, 1.346, 0.799 /) ) ! CS2

    type = "M"
    CALL CalcH("ethyne", "74-86-2", &
      (/ 0., 10., 20., 30. /), &
      (/ 1.73, 1.31, 1.03, 0.84 /) ) ! C2H2 acetylene

    type = "V"
    CALL CalcH("trichloromethane", "67-66-3", &
      (/ 0., 10., 20., 30. /), &
      (/ 23.09, 10.78, 6.28, 4.17 /) ) ! CHCl3 chloroform

    type = "M"
    CALL CalcH("ethene", "74-85-1", &
      (/ 0., 10., 20., 30. /), &
      (/ 0.226, 0.162, 0.122, 0.098 /) ) ! C2H4 ethylene

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

  END SUBROUTINE ref0862

  !---------------------------------------------------------------------------

  SUBROUTINE ref0863 ! special definition, similar to alpha
    IMPLICIT NONE

    ref = "863"
    type = "M"
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0.,10.,20.,30. /) + CtoK

    CALL CalcH("dichloromethane",     "75-09-2", MC+MH*2.+MCl*2., &
      (/ 0.1219, 0.0702, 0.0435, 0.0291 /) )
    ! chloroform dry and chloroform humid yield the same results:
    ! CALL CalcH("trichloromethane",    "67-66-3", MC+MH+MCl*3., &
    !   (/ 0.1319, 0.0693, 0.0396, 0.0249 /) )
    CALL CalcH("trichloromethane",    "67-66-3", MC+MH+MCl*3., &
      (/ 0.1325, 0.0688, 0.0397, 0.0246 /) )
    CALL CalcH("tetrachloromethane",  "56-23-5", MC+MCl*4., &
      (/ 0.0222, 0.0113, 0.0066, 0.0046 /) )
    CALL CalcH("dibromomethane",      "74-95-3", MC+MH*2.+MBr*2., &
      (/ 0.7754, 0.4270, 0.2511, 0.1577 /) )
    CALL CalcH("iodomethane",         "74-88-4", MC+MH*3.+MI, &
      (/ 0.0843, 0.0500, 0.0325, 0.0224 /) )
    CALL CalcH("1,2-dichloroethane", "107-06-2", MC*2.+MH*4.+MCl*2., &
      (/ 0.3400, 0.1835, 0.1082, 0.0695 /) )
    CALL CalcH("1,1-dichloroethane",  "75-34-3", MC*2.+MH*4.+MCl*2., &
      (/ 0.0712, 0.0392, 0.0229, 0.0148 /) )
    CALL CalcH("bromoethane",         "74-96-4", MC*2.+MH*5.+MBr, &
      (/ 0.0491, 0.0286, 0.0180, 0.0120 /) )
    CALL CalcH("iodoethane",          "75-03-6", MC*2.+MH*5.+MI, &
      (/ 0.0817, 0.0462, 0.0285, 0.0189 /) )
    CALL CalcH("1-chloropropane",    "540-54-5", MC*3.+MH*7.+MCl, &
      (/ 0.0248, 0.0133, 0.0073, 0.0051 /) )
    CALL CalcH("2-chloropropane",     "75-29-6", MC*3.+MH*7.+MCl, &
      (/ 0.0175, 0.0094, 0.0053, 0.0037 /) )
    CALL CalcH("1-bromopropane",     "106-94-5", MC*3.+MH*7.+MBr, &
      (/ 0.0546, 0.0290, 0.0168, 0.0109 /) )
    CALL CalcH("2-bromopropane",      "75-26-3", MC*3.+MH*7.+MBr, &
      (/ 0.0460, 0.0249, 0.0137, 0.0091 /) )
    CALL CalcH("1-iodopropane",      "107-08-4", MC*3.+MH*7.+MI, &
      (/ 0.0740, 0.0399, 0.0231, 0.0142 /) )
    CALL CalcH("2-iodopropane",       "75-30-9", MC*3.+MH*7.+MI, &
      (/ 0.0602, 0.0315, 0.0188, 0.0115 /) )
    CALL CalcH("carbon disulfide",    "75-15-0", MC+MS*2., &
      (/ 0.0155, 0.0092, 0.0051, 0.0034 /) )

    DEALLOCATE(temp, Harray)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, M, Hominus_)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: chem_
      CHARACTER(LEN=*),   INTENT(IN) :: casrn_
      REAL,               INTENT(IN) :: M ! molar mass [kg/mol]
      REAL, DIMENSION(:), INTENT(IN) :: Hominus_

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Harray = Hominus_ * 1E3 / (atm * M)
      CALL HTdep(temp, Harray, Hominus, mindHR)
      CALL Output(Hominus, mindHR, r2)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0863

  !---------------------------------------------------------------------------

  SUBROUTINE ref0869 ! special definition [ml(gas)/ml(H2O)]
    IMPLICIT NONE

    ref = "869"
    type = "M"
    chem = "carbon oxide sulfide" ; casrn = "463-58-1"
    Hominus = 0.8 / (Rgas*(13.5+CtoK))
    CALL MakeNoteOtherTemp("287")
    CALL Output(Hominus)

  END SUBROUTINE ref0869

  !---------------------------------------------------------------------------

  SUBROUTINE ref0871 ! Bunsen coefficient alpha [1]
    IMPLICIT NONE

    ref = "871"
    chem = "molecular bromine" ; casrn = "7726-95-6"
    type = "M"
    ndata = 9
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 0.00, 9.94, 20.46, 30.38, 40.31, 50.25, 60.04, 69.98, 80.22 /) &
      + CtoK
    Harray = (/ 60.53, 35.22, 20.87, 13.65, 9.22, 6.50, 4.84, 3.82, 2.94 /) &
      * alpha_TO_HcpSI
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0871

  !---------------------------------------------------------------------------

  SUBROUTINE ref0892 ! KHcc in [1]
    IMPLICIT NONE

    ref = "892"
    type = "M"

    chem = "iodomethane" ; casrn = "74-88-4" ! CH3I
    CALL MakeNoteOtherTemp("294")
    CALL Output(KHcc_TO_HcpSI(0.21,21.+CtoK))

    chem = "bromomethane" ; casrn = "74-83-9" ! CH3Br methyl bromide
    CALL MakeNoteOtherTemp("294")
    CALL Output(KHcc_TO_HcpSI(0.30,21.+CtoK))

  END SUBROUTINE ref0892

  !---------------------------------------------------------------------------

  SUBROUTINE ref0893 ! KHcc [1]
    IMPLICIT NONE

    ref = "893"

    CALL CalcH("trifluralin",          "1582-09-8", 4.23E-3) ! C{13}H{16}F3N3O4
    CALL CalcH("chlorpyrifos",         "2921-88-2", 1.30E-4) ! C9H{11}Cl3NO3PS
    ! thiodan (endo I + endo II)                     2.65E-3  ! not used (isomer mix of endosulfanes)
    CALL CalcH("$\alpha$-endosulfan",   "959-98-8", 2.72E-3) ! endosulfan I (endo I) ???
    CALL CalcH("$\beta$-endosulfan",  "33213-65-9", 3.6E-4)  ! endosulfan II (endo II) ???
    CALL CalcH("metolachlor",         "51218-45-2", 3.21E-6) ! C{15}H{22}ClNO2
    CALL CalcH("methylparathion",       "298-00-0", 1.57E-6) ! C{8}H{10}NO5PS
    CALL CalcH("2,4-D",                  "94-75-7", 2.79E-3, "pH 1") ! pH 1
    CALL CalcH("2,4-D",                  "94-75-7", 3.5E-4,  "pH 7") ! pH 7

    chem = "metolachlor" ; casrn = "51218-45-2" ! C{15}H{22}ClNO2
    type = "C"
    CALL Output(KHcc_TIMES_HcpSI_atT0/3.37E-7)

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, HLC, seenote_)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: HLC
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: seenote_

      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      type = "M"
      IF (PRESENT(seenote_)) CALL MakeNote(TRIM(ref),"Measured at "//TRIM(seenote_)//".")
      CALL MakeNoteOtherTemp("293")
      CALL Output(KHcc_TIMES_HcpSI_atT0/HLC)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0893

  !---------------------------------------------------------------------------

  SUBROUTINE ref0895 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "895"
    type = "M"
    chem = "nitrogen trioxide" ; casrn = "12033-49-7" ! NO3
    CALL Output(1.8*Hcp_TO_HcpSI)

  END SUBROUTINE ref0895

  !---------------------------------------------------------------------------

  SUBROUTINE ref0896 ! ln(Hbp) [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "896"
    type = "M"
    CALL CalcH("fluoroethanoic acid",         "144-49-0", 11.3)
    CALL CalcH("chloroethanoic acid",         "79-11-8",  11.59, -81.E3)
    CALL CalcH("bromoethanoic acid",          "79-08-3",  11.94, -77.E3)
    CALL CalcH("difluoroethanoic acid",       "381-73-7", 10.32, -57.1E3)
    CALL CalcH("dichloroethanoic acid",       "79-43-6",  11.69, -66.6E3)
    CALL CalcH("dibromoethanoic acid",        "631-64-1", 12.33, -74.3E3)
    CALL CalcH("tribromoethanoic acid",       "75-96-7",  12.61, -75.E3)
    CALL CalcH("chlorodifluoroethanoic acid", "76-04-0",  10.11, -85.2E3)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, lnH, DeltaH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: lnH
      REAL, INTENT(IN), OPTIONAL   :: DeltaH

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = EXP(lnH)*Hbp_TO_HcpSI
      IF (PRESENT(DeltaH)) THEN
        mindHR = -DeltaH/Rgas
        CALL Output(Hominus, mindHR)
      ELSE
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref0896

  !---------------------------------------------------------------------------

  ! ref0899 Cl,Br-HC's (only citations of grey literature)

  !---------------------------------------------------------------------------

  SUBROUTINE ref0900 ! KHcc [1]
    IMPLICIT NONE

    ref = "900"
    type = "M"

    CALL CalcH("lindane",     "58-89-9", 8.2E-5) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("diazinon",   "333-41-5", 4.6E-6) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("alachlor", "15972-60-8", 3.4E-7) ! C{14}H{20}ClNO2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, KHcc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: KHcc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TO_HcpSI(KHcc, 23+CtoK)
      CALL MakeNoteOtherTemp("296")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0900

  !---------------------------------------------------------------------------

  SUBROUTINE ref0901 ! KHcc [1]
    IMPLICIT NONE

    ref = "901"
    type = "M"

    CALL CalcH("lindane",        "58-89-9", (8.1E-5+8.5E-5)/2.        ) ! C6H6Cl6 $\gamma$-lindane
    CALL CalcH("DDT",            "50-29-3", (5.2E-4+5.4E-4)/2., 3.5E-4) ! C{14}H9Cl5 DDT
    CALL CalcH("chlordane",      "57-74-9", (2.2E-3+2.4E-3)/2., 3.4E-3) ! assuming that t-chlordane = chlordane
    CALL CalcH("trifluralin",  "1582-09-8", (1.9E-3+2.3E-3)/2., 2.4E-3) ! C{13}H{16}F3N3O4
    CALL CalcH("diazinon",      "333-41-5", (5.6E-6+4.1E-6)/2.        ) ! C{12}H{21}N2O3PS dimpylate
    CALL CalcH("alachlor",    "15972-60-8", (5.0E-7+4.0E-7)/2.        ) ! C{14}H{20}ClNO2

  CONTAINS

    SUBROUTINE CalcH (chem_, casrn_, fog, wwc)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)           :: chem_
      CHARACTER(LEN=*), INTENT(IN)           :: casrn_
      REAL,             INTENT(IN)           :: fog
      REAL, OPTIONAL,   INTENT(IN)           :: wwc
      chem  = chem_  ! make value global, so Output will find it
      casrn = casrn_ ! make value global, so Output will find it
      Hominus = KHcc_TO_HcpSI(fog, 23+CtoK)
      CALL MakeNoteOtherTemp("296")
      CALL Output(Hominus)
      IF(PRESENT(wwc)) THEN
        Hominus = KHcc_TO_HcpSI(wwc, 23+CtoK) ! room temp = 22...24 C
        CALL MakeNote("901wwc", "Measured with the wetted-wall column at room temperature.")
        CALL Output(Hominus)
      ENDIF
    END SUBROUTINE CalcH

  END SUBROUTINE ref0901

  !---------------------------------------------------------------------------

  SUBROUTINE ref0903 ! KHpc [kPa*m3/mol]
    IMPLICIT NONE

    ref = "903"
    type = "M"

    ! Table I:

    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    CALL MakeNoteOtherTemp("302")
    CALL Output(KHpcSI_TIMES_HcpSI/(1.E3*0.649))

    ! carbontetrachloride data taken from Tab. II

    chem = "cyclohexane" ; casrn = "110-82-7" ! C6H{12}
    CALL MakeNoteOtherTemp("301")
    CALL Output(KHpcSI_TIMES_HcpSI/(1.E3*16.617))

    chem = "methylbenzene" ; casrn = "108-88-3" ! C6H5CH3 toluene
    CALL MakeNoteOtherTemp("302")
    CALL Output(KHpcSI_TIMES_HcpSI/(1.E3*0.722))

    ! Table II:

    ndata = 3
    ALLOCATE(temp(ndata), Harray(ndata))

    chem = "bromobenzene" ; casrn = "108-86-1" ! C6H5Br
    temp = (/30.,35.,44.8/)
    temp = temp+CtoK
    Harray = (/.256,.332,.579/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref), &
        "The same data were also published in \citet{618}.")
    CALL Output(Hominus, mindHR, r2)

    chem = "tetrachloromethane" ; casrn = "56-23-5" ! CCl4 carbontetrachloride
    temp = (/27.6,35.,45./)
    temp = temp+CtoK
    Harray = (/3.313,4.55,6.343/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "(E)-1,2-dichloroethene" ; casrn = "156-60-5" ! CHClCHCl
    temp = (/26.2,35.,46.1/)
    temp = temp+CtoK
    Harray = (/1.023,1.591,2.087/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "1,1,1-trichloroethane" ; casrn = "71-55-6" ! methylchloroform, MCF
    temp = (/26.3,35.,44.8/)
    temp = temp+CtoK
    Harray = (/1.763,2.412,3.232/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "1,1,2-trichloroethane" ; casrn = "79-00-5" ! CHCl2CH2Cl
    temp = (/26.2,35.8,44.8/)
    temp = temp+CtoK
    Harray = (/.082,.184,.259/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "(2-propyl)-benzene" ; casrn = "98-82-8" ! C6H5C3H7 isopropylbenzene; cumene
    temp = (/28.0,35.0,46.1/)
    temp = temp+CtoK
    Harray = (/1.323,1.547,2.422/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "2-methylnaphthalene" ; casrn = "91-57-6" ! C{10}H7CH3
    temp = (/26.,35.8,46./)
    temp = temp+CtoK
    Harray = (/20.265,22.9,26.243/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "1,4-dimethylbenzene" ; casrn = "106-42-3" ! C6H4(CH3)2 $p$-xylene
    temp = (/27.0,35.8,46.0/)
    temp = temp+CtoK
    Harray = (/0.856,1.189,1.576/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "1,2,4-trimethylbenzene" ; casrn = "95-63-6" ! C6H3(CH3)3
    temp = (/27.,35.,45./)
    temp = temp+CtoK
    Harray = (/.704,1.135,1.591/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "cyclopentane" ; casrn = "287-92-3" ! C5H{10}
    temp = (/27.9,35.8,45./)
    temp = temp+CtoK
    Harray = (/16.617,24.318,30.398/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "methylcyclohexane" ; casrn = "108-87-2" ! C6H{11}CH3
    temp = (/27.3,35.8,45./)
    temp = temp+CtoK
    Harray = (/12.666,34.653,72.447/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "2-methylhexane" ; casrn = "591-76-4" ! C7H{16}
    temp = (/26.9,35.,45./)
    temp = temp+CtoK
    Harray = (/51.878,31.512,25.939/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL MakeNote("903mehex", &
      TRIM(citet())//" found that the solubility of 2-methylhexane "// &
      "increases with temperature.")
    CALL Output(Hominus, mindHR, r2)

    chem = "heptane" ; casrn = "142-82-5" ! C7H{16}
    temp = (/26.,35.8,45./)
    temp = temp+CtoK
    Harray = (/91.294,121.083,193.024/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    chem = "octane" ; casrn = "111-65-9" ! C8H{18}
    temp = (/27.9,35.0,45.0/)
    temp = temp+CtoK
    Harray = (/39.213,93.827,167.693/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

    ndata = 2
    ALLOCATE(temp(ndata), Harray(ndata))

    chem = "hexachlorobenzene" ; casrn = "118-74-1" ! C6Cl6
    temp = (/26.0,46.0/)
    temp = temp+CtoK
    Harray = (/26.243,29.587/) ! KHpc
    Harray = KHpcSI_TIMES_HcpSI/(Harray*1.E3)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL MakeNote(TRIM(ref))
    CALL Output(Hominus, mindHR, r2)

    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0903

  !---------------------------------------------------------------------------

  SUBROUTINE ref0910
    IMPLICIT NONE

    ref = "910"

    chem = "hypobromous acid" ; casrn = "13517-11-8"
    type = "W"
    CALL MakeNote("910wrong", &
      TRIM(citet())//" extracted a value from wetted-wall "// &
      "flow tube experiments. However, it was later discovered that under "// &
      "the experimental conditions no evaluation of $\H$ is possible "// &
      "(J.\ Crowley, pers.\ comm., 1999).")
    CALL Output(DUMMY)

  END SUBROUTINE ref0910

  !---------------------------------------------------------------------------

  ! ref0916 see 1155

  !---------------------------------------------------------------------------

  SUBROUTINE ref0921 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "921"
    type = "M"

    chem = "carbon dioxide" ; casrn = "124-38-9" ! CO2
    ndata = 10
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.15,278.15,303.12,303.12,308.08,308.08,333.15,333.15,338.15&
      &,338.15/)
    Harray = (/87.16,87.61,182.7,183.7,204.9,206.6,328.9,327.8,353.8,355.3/)
    Harray = Harray * 1.E6 / atm ! convert to [atm]
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "chlorodifluoromethane" ; casrn = "75-45-6" ! CHF2Cl R22
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.24,278.24,308.06,308.06,338.11,338.11/)
    Harray = (/75.65,75.76,222.,225.3,435.4,425.1/)
    Harray = Harray * 1.E6 / atm ! convert to [atm]
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "trifluoromethane" ; casrn = "75-46-7" ! CHF3 R23
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.2,278.2,308.15,308.15,338.21,338.21/)
    Harray = (/235.1,234.1,560.9,561.4,915.9,925.6/)
    Harray = Harray * 1.E6 / atm ! convert to [atm]
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "1,1,1,2-tetrafluoroethane" ; casrn = "811-97-2" ! C2H2F4 R134a
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.16,278.16,308.08,308.08,338.12,338.12/)
    Harray = (/154.8,152.,453.6,453.7,822.6,837.3/)
    Harray = Harray * 1.E6 / atm ! convert to [atm]
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "1,1-difluoroethane" ; casrn = "75-37-6" ! C2H4F2 R152a
    ndata = 6
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.19,278.19,308.13,308.13,338.19,338.19/)
    Harray = (/53.84,53.53,149.3,149.8,275.9,271.5/)
    Harray = Harray * 1.E6 / atm ! convert to [atm]
    Harray = KHpx_TIMES_HcpSI / Harray
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0921

  !---------------------------------------------------------------------------

  SUBROUTINE ref0923 ! KHpx [MPa]
    IMPLICIT NONE

    ref = "923"
    type = "M"

    ! from Table A1.1.b:
    CALL CalcH("R32",     "75-10-5", 168.740,   -9337.20,  -23.349)
    CALL CalcH("R22",     "75-45-6", 182.510,   -10511.00, -24.956)
    CALL CalcH("R23",     "75-46-7", 171.529,   -9417.53,  -23.492)
    CALL CalcH("R11",     "75-69-4", 265.520,   -14704.00, -36.842)
    CALL CalcH("R160",    "75-00-3", 209.480,   -11896.00, -29.038)
    CALL CalcH("R152a",   "75-37-6", 178.070,   -10084.00, -24.492)
    CALL CalcH("R141b", "1717-00-6", 134.840,   -8922.50,  -17.490)
    CALL CalcH("R142b",   "75-68-3", 247.170,   -13500.00, -34.385)
    CALL CalcH("R133a",   "75-88-7", 248.340,   -13871.00, -34.541)
    CALL CalcH("R134a",  "811-97-2", 224.328,   -12267.69, -31.128)
    CALL CalcH("R124",  "2837-89-0", 287.720,   -15480.00, -40.281)
    CALL CalcH("R1150",   "74-85-1", 163.110,   -8646.20,  -22.305)
    CALL CalcH("R1122",  "359-10-4", 224.250,   -12573.00, -30.939)
    CALL CalcH("R1270",  "115-07-1", 169.430,   -9552.40,  -22.898)
    CALL CalcH("R1216",  "116-15-4", 187.318,   -10158.10, -25.317)

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

  END SUBROUTINE ref0923

  !---------------------------------------------------------------------------

  SUBROUTINE ref0935
    IMPLICIT NONE

    ref = "935"
    type = "?"

    chem = "hydrogen chloride" ; casrn = "7647-01-0" ! HCl
    CALL MakeNote(TRIM(ref), &
      TRIM(citet())//" probably refer to the incorrect value given by "// &
      "\citet{34}.")
    CALL Output(DUMMY)

    chem = "nitrogen trioxide" ; casrn = "12033-49-7" ! NO3 nitrate radical
    CALL MakeNote(TRIM(ref))
    CALL Output(DUMMY)

  END SUBROUTINE ref0935

  !---------------------------------------------------------------------------

  SUBROUTINE ref0937 ! Hcp [M/atm]
    IMPLICIT NONE

    ref = "937"
    type = "E"

    chem = "bromine chloride" ; casrn = "13863-41-7" ! BrCl
    CALL Output(0.59*Hcp_TO_HcpSI)

    chem = "hypobromous acid" ; casrn = "13517-11-8" ! HOBr
    CALL Output(6.1E3*Hcp_TO_HcpSI)

    chem = "nitryl bromide" ; casrn = "13536-70-4" ! BrNO2
    CALL Output(0.3*Hcp_TO_HcpSI)

    chem = "nitryl chloride" ; casrn = "13444-90-1" ! ClNO2
    CALL Output(4.6E-2*Hcp_TO_HcpSI)

  END SUBROUTINE ref0937

  !---------------------------------------------------------------------------

  SUBROUTINE ref0941 ! -lg(1/Hcp) [atm/M] = lg(Hcp) [M/atm]
    IMPLICIT NONE

    ref = "941"
    type = "V"
    CALL CalcH("H-(2-nitrophenol)",       "88-75-5",   1.87)
    CALL CalcH("3-me-(2-nitrophenol)",    "4920-77-8", 2.39)
    CALL CalcH("4me-(2-nitrophenol)",     "119-33-5",  1.79)
    CALL CalcH("5me-(2-nitrophenol)",     "700-38-9",  1.83)
    CALL CalcH("4-sbu-(2-nitrophenol)",   "3555-18-8", 1.38)
    CALL CalcH("4ome-(2-nitrophenol)",    "1568-70-3", 1.36)
    CALL CalcH("4cl-(2-nitrophenol)",     "89-64-5",   1.90)
    CALL CalcH("4cl5me-(2-nitrophenol)",  "7147-89-9", 1.56)
    CALL CalcH("5f-(2-nitrophenol)",      "446-36-6",  2.77)
    CALL CalcH("4cho",                    "3011-34-5", 2.98)
    CALL CalcH("4no2-(2-nitrophenol)",    "51-28-5",   3.55)
    CALL CalcH("5no2-(2-nitrophenol)",    "329-71-5",  3.18)
    CALL CalcH("4no26me-(2-nitrophenol)", "534-52-1",  3.37)
    CALL CalcH("4np",                     "100-02-7",  4.48)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, lgH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: lgH

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = 10.**lgH * Hcp_TO_HcpSI
      CALL MakeNoteOtherTemp("293")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0941

  !---------------------------------------------------------------------------

  SUBROUTINE ref0977 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "977"
    type = "V"
    CALL CalcH("methyl caprate",       "110-42-9",   73.)
    CALL CalcH("methyl laurate",       "111-82-0",   120.)
    CALL CalcH("methyl myristate",     "124-10-7",   200.)
    CALL CalcH("methyl palmitate",     "112-39-0",   340.)
    CALL CalcH("methyl stearate",      "112-61-8",   580.)
    CALL CalcH("methyl arachidate",    "1120-28-1",  1000.)
    CALL CalcH("methyl behenate",      "929-77-1",   1700.)
    CALL CalcH("Methyl linolenate",    "301-00-8",   3.6)
    CALL CalcH("Methyl linolate",      "112-63-0",   16.)
    CALL CalcH("methyl oleate",        "112-62-9",   80.)
    CALL CalcH("Methyl erucate",       "1120-34-9",  190.)
    CALL CalcH("Ethyl Laurate",        "106-33-2",   130.)
    CALL CalcH("propyl laurate",       "3681-78-5",  130.)
    CALL CalcH("butyl laurate",        "106-18-3",   140.)
    CALL CalcH("2-ethylhexyl laurate", "20292-08-4", 330.)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, KHpcSI)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: KHpcSI

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI / KHpcSI
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0977

  !---------------------------------------------------------------------------

  ! ref0978 see ref0998

  !---------------------------------------------------------------------------

  ! ref0979 SF6 only data at high T

  !---------------------------------------------------------------------------

  ! ref0980 C10-C12 HC's only data at high T

  !---------------------------------------------------------------------------

  SUBROUTINE ref0981 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "981"

    ! Table 3, measured:
    CALL CalcH("naphthalene",           "91-20-3", "M", 45.  ) ! C{10}H8
    CALL CalcH("2-methylnaphthalene",   "91-57-6", "M", 46.  ) ! C{10}H7CH3
    CALL CalcH("2,3-benzindene",        "86-73-7", "M", 6.5  ) ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",          "85-01-8", "M", 2.9  ) ! C{14}H{10}
    CALL CalcH("benzo[jk]fluorene",    "206-44-0", "M", 1.1  ) ! C{16}H{10} fluoranthene
    CALL CalcH("pyrene",               "129-00-0", "M", 2.0  ) ! C{16}H{10}
    CALL CalcH("pyrene",               "129-00-0", "M", 0.92 ) ! C{16}H{10}
    ! from ref1146:
    !CALL CalcH("benzo[b]fluoranthene", "205-99-2", "M", 0.051) ! C{20}H{12}
    !CALL CalcH("benzo[k]fluoranthene", "207-08-9", "M", 0.043) ! C{20}H{12}
    !CALL CalcH("benzo[a]pyrene",        "50-32-8", "M", 0.034) ! C{20}H{12} benz[a]pyrene
    !CALL CalcH("benzo[ghi]perylene",   "191-24-2", "M", 0.027) ! C{22}H{12}

    ! Table 3, P/S:
    CALL CalcH("naphthalene",           "91-20-3", "V", 31.64) ! C{10}H8
    CALL CalcH("2-methylnaphthalene",   "91-57-6", "V", 38.61) ! C{10}H7CH3
    CALL CalcH("2,3-benzindene",        "86-73-7", "V", 5.94 ) ! C{13}H{10} fluorene
    CALL CalcH("phenanthrene",          "85-01-8", "V", 2.65 ) ! C{14}H{10}
    CALL CalcH("benzo[jk]fluorene",    "206-44-0", "V", 0.701) ! C{16}H{10} fluoranthene
    CALL CalcH("pyrene",               "129-00-0", "V", 0.69 ) ! C{16}H{10}
    CALL CalcH("benzo[k]fluoranthene", "207-08-9", "V", 0.012) ! C{20}H{12}
    CALL CalcH("benzo[a]pyrene",        "50-32-8", "V", 0.035) ! C{20}H{12} benz[a]pyrene
    CALL CalcH("benzo[ghi]perylene",   "191-24-2", "V", 0.057) ! C{22}H{12}

  CONTAINS

    SUBROUTINE CalcH(chem_, casrn_, type_, HLC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: chem_
      CHARACTER(LEN=*), INTENT(IN) :: casrn_
      CHARACTER(LEN=*), INTENT(IN) :: type_
      REAL,             INTENT(IN) :: HLC
      chem   = chem_   ! make value global, so Output will find it
      casrn  = casrn_  ! make value global, so Output will find it
      type   = type_   ! make value global, so Output will find it
      CALL MakeNoteOtherTemp("293") ! 20 C
      CALL Output(KHpcSI_TIMES_HcpSI/HLC)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0981

  !---------------------------------------------------------------------------

  SUBROUTINE ref0982 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "982"
    type = "M"
    CALL CalcH("1,2,9,10-tetrachlorodecane",    "205646-11-3", 17.7)
    CALL CalcH("pentachlorodecane isomers",     "175801-37-3", 4.92) ! (a+b)
    CALL CalcH("pentachlorodecane isomers",     "175801-37-3", 2.62) ! (c+d)
    CALL CalcH("1,2,10,11-tetrachloroundecane", "210049-49-3", 6.32)
    CALL CalcH("pentachloroundecane isomers",   "210175-48-7", 1.46) ! (a+b)
    CALL CalcH("pentachloroundecane isomers",   "210175-48-7", 0.68) ! (c+d+e)
    type = "V"
    CALL CalcH("1,10-dichlorodecane",           "2162-98-3",   499.)
    CALL CalcH("1,12-dichlorododecane",         "3922-28-9",   648.)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, KHpcSI)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: KHpcSI

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI / KHpcSI
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0982

  !---------------------------------------------------------------------------

  SUBROUTINE ref0983 ! KHcc in [1]
    IMPLICIT NONE

    ref = "983"
    type = "M"
    chem = "dimethyl sulfide" ; casrn = "75-18-3" ! CH3SCH3 DMS
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/ 18., 25., 35., 44. /) + CtoK
    Harray = (/ 0.069, 0.094, 0.149, 0.211 /)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

  END SUBROUTINE ref0983

  !---------------------------------------------------------------------------

  SUBROUTINE ref0984 ! KHcc [1]
    IMPLICIT NONE

    ref = "984"
    type = "Q"

    ! Tab.1:
    ! aliphatic hydrocarbons:
    CALL CalcH("methane",                                     "74-82-8",  1.39) ! methane
    CALL CalcH("2-methylbutane",                              "78-78-4",  1.68) ! 2-methylbutane
    CALL CalcH("2,3-dimethylbutane",                          "79-29-8",  1.84) ! 2,3-dimethylbutane
    CALL CalcH("2-methylhexane",                             "591-76-4",  1.89) ! 2-methylhexane
    CALL CalcH("3-methylhexane",                             "589-34-4",  1.88) ! 3-methylhexane
    CALL CalcH("2,2-dimethylpentane",                        "590-35-2",  1.99) ! 2,2-dimethylpentane
    CALL CalcH("2,3-dimethylpentane",                        "565-59-3",  1.93) ! 2,3-dimethylpentane
    CALL CalcH("3,3-dimethylpentane",                        "562-49-2",  1.96) ! 3,3-dimethylpentane
    CALL CalcH("3-methylheptane",                            "589-81-1",  1.98) ! 3-methylheptane
    CALL CalcH("2,3,4-trimethylpentane",                     "565-75-3",  2.10) ! 2,3,4-trimethylpentane
    CALL CalcH("nonane",                                     "111-84-2",  2.03) ! n-nonane
    CALL CalcH("2,2,5-trimethylhexane",                     "3522-94-9",  2.26) ! 2,2,5-trimethylhexane
    CALL CalcH("decane",                                     "124-18-5",  2.14) ! n-decane
    CALL CalcH("cyclopropane",                                "75-19-4",  0.65) ! cyclopropane
    CALL CalcH("propylcyclopentane",                        "2040-96-2",  1.20) ! n-propylcyclopentane
    CALL CalcH("pentylcyclopentane",                        "3741-00-2",  1.41) ! n-pentylcyclopentane
    CALL CalcH("{cis}-1,2-dimethylcyclohexane",             "2207-01-4",  0.97) ! cis-1,2-dimethylcyclohexane
    CALL CalcH("{trans}-1,4-dimethylcyclohexane",           "2207-04-7",  1.26) ! trans-1,4-dimethylcyclohexane
    CALL CalcH("1,3,5-cycloheptatriene",                     "544-25-2", -0.32) ! cyclohepta-1,3,5-triene
    CALL CalcH("2-methyl-1-pentene",                         "763-29-1",  1.33) ! 2-methylpent-1-ene
    CALL CalcH("1-heptene",                                  "592-76-7",  1.38) ! heptene
    CALL CalcH("{trans}-2-heptene",                        "14686-13-6",  1.38) ! (e)-hept-2-ene
    CALL CalcH("1-nonene",                                   "124-11-8",  1.59) ! non-1-ene
    ! halogenated hydrocarbons:
    CALL CalcH("1,1,1,2-tetrachloroethane",                  "630-20-6", -1.13) ! 1,1,1,2-tetrachloroethane
    CALL CalcH("2-chlorobutane",                              "78-86-4", -0.02) ! 2-chlorobutane
    CALL CalcH("2-chloro-2-methylpropane",                   "507-20-0",  0.11) ! 2-chloro-2-methylpropane
    CALL CalcH("1,4-dichlorobutane",                         "110-56-5", -0.44) ! 1,4-dichlorobutane
    CALL CalcH("1-chlorohexane",                             "544-10-5",  0.12) ! 1-chlorohexane
    CALL CalcH("1-chloroheptane",                            "629-06-1",  0.22) ! 1-chloroheptane
    CALL CalcH("3-chloro-1-propene",                         "107-05-1", -0.64) ! 1-chloroprop-2-ene
    CALL CalcH("1-bromo-2-methylpropane",                     "78-77-3", -0.33) ! 1-bromo-2-methylpropane
    CALL CalcH("2-bromo-2-methylpropane",                    "507-19-7", -0.11) ! 2-bromo-2-methylpropane
    CALL CalcH("1-bromo-3-methylbutane",                     "107-82-4", -0.24) ! 1-bromo-3-methylbutane
    CALL CalcH("1-bromopentane",                             "110-53-2", -0.30) ! 1-bromopentane
    CALL CalcH("1-bromohexane",                              "111-25-1", -0.19) ! 1-bromohexane
    CALL CalcH("1-bromoheptane",                             "629-04-9", -0.09) ! 1-bromoheptane
    CALL CalcH("1-bromooctane",                              "111-83-1",  0.01) ! 1-bromooctane
    CALL CalcH("1-iodopentane",                              "628-17-1", -0.15) ! 1-iodopentane
    CALL CalcH("1-iodohexane",                               "638-45-9", -0.05) ! 1-iodohexane
    CALL CalcH("1-iodoheptane",                             "4282-40-0",  0.06) ! 1-iodoheptane
    ! esters/acids:
    CALL CalcH("pentyl ethanoate",                           "628-63-7", -1.71) ! n-pentylacetate
    CALL CalcH("2,2-dimethylpropanoic acid, methyl ester",   "598-98-1", -1.66) ! methyltrimethylacetate
    CALL CalcH("pentyl propanoate",                          "624-54-4", -1.74) ! n-pentylpropanoate
    CALL CalcH("(2-methylpropyl)-propanoate",                "540-42-1", -1.67) ! i-butylpropionate
    CALL CalcH("ethyl hexanoate",                            "123-66-0", -1.63) ! ethylhexanoate
    CALL CalcH("(2-methylpropyl)-2-methylpropanoate",         "97-85-8", -1.51) ! isobutylisobutanoate
    CALL CalcH("methanoic acid, hexyl ester",                "629-33-4", -1.61) ! hexylformate
    CALL CalcH("methanoic acid, pentyl ester",               "638-49-3", -1.84) ! amylformate
    CALL CalcH("methyl octanoate",                           "111-11-5", -2.07) ! methyloctanoate
    CALL CalcH("ethyl benzoate",                              "93-89-0", -2.73) ! ethylbenzoate
    CALL CalcH("pentanoic acid",                             "109-52-4", -4.73) ! pentanoicacid
    CALL CalcH("hexanoic acid",                              "142-62-1", -4.62) ! hexanoicacid
    ! aliphatic alcohols:
    ! next value probably belongs to another species:
    !CALL CalcH("3-methyl-1-butanol",                        "123-51-3", -4.05) ! 3-methylbutan-1-ol
    CALL CalcH("2-methyl-2-propanol",                         "75-65-0", -3.26) ! 2-methylpropan-2-ol
    CALL CalcH("3-pentanol",                                 "584-02-1", -3.28) ! pentan-3-ol
    CALL CalcH("3-methyl-1-butanol",                         "123-51-3", -3.23) ! 3-methylbutan-1-ol
    CALL CalcH("1-nonanol",                                  "143-08-8", -2.88) ! nonan-1-ol
    CALL CalcH("1-decanol",                                  "112-30-1", -2.77) ! decan-1-ol
    CALL CalcH("2-propen-1-ol",                              "107-18-6", -3.94) ! prop-2-en-1-ol
    ! aromatics:
    CALL CalcH("(2-propyl)-benzene",                          "98-82-8", -0.36) ! isopropylbenzene
    CALL CalcH("1,2,3-trimethylbenzene",                     "526-73-8", -0.31) ! 1,2,3-trimethylbenzene
    CALL CalcH("1,3,5-trimethylbenzene",                     "108-67-8", -0.30) ! 1,3,5-trimethylbenzene
    CALL CalcH("1,4-diethylbenzene",                         "105-05-5", -0.29) ! 1,4-diethylbenzene
    CALL CalcH("2-propenylbenzene",                          "300-57-2", -0.85) ! allylbenzene
    CALL CalcH("1-ethyl-2-methylbenzene",                    "611-14-3", -0.37) ! 2-ethyltoluene
    CALL CalcH("1-ethyl-4-methylbenzene",                    "622-96-8", -0.37) ! 4-ethyltoluene
    CALL CalcH("(2-methylpropyl)-benzene",                   "538-93-2", -0.24) ! isobutylbenzene
    CALL CalcH("(1-methylpropyl)-benzene",                   "135-98-8",  0.61) ! sec-butylbenzene
    CALL CalcH("1-methyl-4-(1-methylethyl)-benzene",          "99-87-6", -0.21) ! 4-isopropyltoluene
    CALL CalcH("pentylbenzene",                              "538-68-1", -0.20) ! n-pentylbenzene
    CALL CalcH("hexylbenzene",                              "1077-16-3", -0.09) ! n-hexylbenzene
    CALL CalcH("ethenylbenzene",                             "100-42-5", -0.96) ! styrene
    ! halogenated aromatics:
    CALL CalcH("fluorobenzene",                              "462-06-6", -1.09) ! fluorobenzene
    CALL CalcH("(trifluoromethyl)-benzene",                   "98-08-8", -1.68) ! benzotrifluoride
    CALL CalcH("1,2,4-trichlorobenzene",                     "120-82-1", -1.61) ! 1,2,4-trichlorobenzene
    CALL CalcH("1,3,5-trichlorobenzene",                     "108-70-3", -1.61) ! 1,3,5-trichlorobenzene
    CALL CalcH("1,2,3,5-tetrachlorobenzene",                 "634-90-2", -1.90) ! 1,2,3,5-tetrachlorobenzene
    CALL CalcH("1,2,4,5-tetrachlorobenzene",                  "95-94-3", -1.90) ! 1,2,4,5-tetrachlorobenzene
    CALL CalcH("1-chloro-2-methylbenzene",                    "95-49-8", -0.88) ! 2-chlorotoluene
    CALL CalcH("1-bromo-4-methylbenzene",                    "106-38-7", -1.11) ! 4-bromotoluene
    CALL CalcH("iodobenzene",                                "591-50-4", -0.97) ! iodobenzene
    ! polyaromatic hydrocarbons:
    CALL CalcH("1,3-dimethylnaphthalene",                    "575-41-7", -2.67) ! 1,3-dimethylnaphthalene
    CALL CalcH("1,4-dimethylnaphthalene",                    "571-58-4", -2.68) ! 1,4-dimethylnaphthalene
    CALL CalcH("2,3-dimethylnaphthalene",                    "581-40-8", -2.52) ! 2,3-dimethylnaphthalene
    CALL CalcH("2,6-dimethylnaphthalene",                    "581-42-0", -2.67) ! 2,6-dimethylnaphthalene
    CALL CalcH("1-ethylnaphthalene",                        "1127-76-0", -2.74) ! 1-ethylnaphthalene
    CALL CalcH("indane",                                     "496-11-7", -1.16) ! indane
    ! phenols:
    CALL CalcH("1-hydroxy-2,3-dimethylbenzene",              "526-75-0", -5.06) ! 2,3-dimethylphenol
    CALL CalcH("1-hydroxy-2,4-dimethylbenzene",              "105-67-9", -5.06) ! 2,4-dimethylphenol
    CALL CalcH("1-hydroxy-2,5-dimethylbenzene",               "95-87-4", -5.06) ! 2,5-dimethylphenol
    CALL CalcH("1-hydroxy-2,6-dimethylbenzene",              "576-26-1", -5.06) ! 2,6-dimethylphenol
    CALL CalcH("1-hydroxy-3,4-dimethylbenzene",               "95-65-8", -5.06) ! 3,4-dimethylphenol
    CALL CalcH("1-hydroxy-3,5-dimethylbenzene",              "108-68-9", -5.06) ! 3,5-dimethylphenol
    CALL CalcH("1-hydroxy-3-ethylbenzene",                   "620-17-7", -5.13) ! 3-ethylphenol
    CALL CalcH("1-hydroxy-4-ethylbenzene",                   "123-07-9", -5.13) ! 4-ethylphenol
    CALL CalcH("1-hydroxy-4-propylbenzene",                  "645-56-7", -5.03) ! 4-n-propylphenol
    CALL CalcH("4-{tert}-butylphenol",                        "98-54-4", -4.78) ! 4-tert-butylphenol
    CALL CalcH("2-hydroxyfluorobenzene",                     "367-12-4", -5.72) ! 2-fluorophenol
    CALL CalcH("4-hydroxyfluorobenzene",                     "371-41-5", -5.72) ! 4-fluorophenol
    CALL CalcH("2-hydroxychlorobenzene",                      "95-57-8", -5.65) ! 2-chlorophenol
    CALL CalcH("3-hydroxychlorobenzene",                     "108-43-0", -5.65) ! 3-chlorophenol
    CALL CalcH("4-hydroxychlorobenzene",                     "106-48-9", -5.65) ! 4-chlorophenol
    CALL CalcH("4-chloro-3-methylphenol",                     "59-50-7", -5.50) ! 4-chloro-3-methylphenol
    CALL CalcH("4-bromophenol",                              "106-41-2", -5.87) ! 4-bromophenol
    CALL CalcH("2-iodophenol",                               "533-58-4", -5.59) ! 2-iodophenol
    ! Tab.3:                                                              Tab.3:
    ! aldehydes:
    CALL CalcH("methanal",                                    "50-00-0", -2.66) ! formaldehyde
    CALL CalcH("ethanal",                                     "75-07-0", -2.56) ! acetaldehyde
    CALL CalcH("propanal",                                   "123-38-6", -2.47) ! propionaldehyde
    CALL CalcH("butanal",                                    "123-72-8", -2.37) ! butyraldehyde
    CALL CalcH("2-methylpropanal",                            "78-84-2", -2.31) ! isobutyraldehyde
    CALL CalcH("pentanal",                                   "110-62-3", -2.26) ! pentanal
    CALL CalcH("hexanal",                                     "66-25-1", -2.16) ! hexanal
    CALL CalcH("heptanal",                                   "111-71-7", -2.05) ! heptanal
    CALL CalcH("octanal",                                    "124-13-0", -1.95) ! octanal
    CALL CalcH("nonanal",                                    "124-19-6", -1.84) ! nonanal
    CALL CalcH("2-butenal",                                 "4170-30-3", -2.82) ! (e)-but-2-enal
    CALL CalcH("2-hexenal",                                  "505-57-7", -2.62) ! (e)-hex-2-enal
    CALL CalcH("2-octenal",                                 "2363-89-5", -2.41) ! (e)-oct-2-enal
    ! ketones:
    CALL CalcH("propanone",                                   "67-64-1", -2.71) ! propanone
    CALL CalcH("butanone",                                    "78-93-3", -2.61) ! butanone
    CALL CalcH("2-pentanone",                                "107-87-9", -2.46) ! pentan-2-one
    CALL CalcH("3-pentanone",                                 "96-22-0", -2.49) ! pentan-3-one
    CALL CalcH("3-methyl-2-butanone",                        "563-80-4", -2.41) ! 3-methylbutan-2-one
    CALL CalcH("2-hexanone",                                 "591-78-6", -2.36) ! hexan-2-one
    CALL CalcH("4-methyl-2-pentanone",                       "108-10-1", -2.29) ! 4-methylpentan-2-one
    CALL CalcH("2-heptanone",                                "110-43-0", -2.25) ! heptan-2-one
    CALL CalcH("4-heptanone",                                "123-19-3", -2.28) ! heptan-4-one
    CALL CalcH("2-octanone",                                 "111-13-7", -2.15) ! octan-2-one
    CALL CalcH("2-nonanone",                                 "821-55-6", -2.04) ! nonan-2-one
    CALL CalcH("5-nonanone",                                 "502-56-7", -2.07) ! nonan-5-one
    CALL CalcH("2-decanone",                                 "693-54-9", -2.53) ! decan-2-one
    CALL CalcH("2-undecanone",                               "112-12-9", -1.84) ! undecan-2-one
    CALL CalcH("3,3-dimethyl-2-butanone",                     "75-97-8", -2.29) ! 3,3-dimethylbutan-2-one
    CALL CalcH("2,4-dimethyl-3-pentanone",                   "565-80-0", -2.17) ! 2,4-dimethylpentan-3-one
    ! amines:
    CALL CalcH("methanamine",                                 "74-89-5", -3.14) ! methylamine
    CALL CalcH("ethanamine",                                  "75-04-7", -3.06) ! ethylamine
    CALL CalcH("1-propanamine",                              "107-10-8", -2.95) ! n-propylamine
    CALL CalcH("1-butanamine",                               "109-73-9", -2.84) ! n-butylamine
    CALL CalcH("1-pentanamine",                              "110-58-7", -2.74) ! n-pentylamine
    CALL CalcH("1-hexanamine",                               "111-26-2", -2.64) ! n-hexylamine
    CALL CalcH("1-heptanamine",                              "111-68-2", -2.53) ! n-heptylamine
    CALL CalcH("1-octanamine",                               "111-86-4", -2.42) ! n-octylamine
    CALL CalcH("dimethylamine",                              "124-40-3", -3.13) ! dimethylamine
    CALL CalcH("dipropylamine",                              "142-84-7", -2.76) ! di-n-propylamine
    CALL CalcH("N-(1-methylethyl)-2-propanamine",            "108-18-9", -2.65) ! di-isopropylamine
    CALL CalcH("dibutylamine",                               "111-92-2", -2.55) ! di-n-butylamine
    CALL CalcH("trimethylamine",                              "75-50-3", -3.07) ! trimethylamine
    CALL CalcH("triethylamine",                              "121-44-8", -2.91) ! triethylamine
    CALL CalcH("(methylamino)-benzene",                      "100-61-8", -3.83) ! N-methylaniline
    CALL CalcH("(dimethylamino)-benzene",                    "121-69-7", -3.78) ! N,N-dimethylaniljne
    CALL CalcH("2,6-dimethylbenzenamine",                     "87-62-7", -3.55) ! 2,6-dimethylaniline
    ! nitro compounds:
    CALL CalcH("ethane nitrile",                              "75-05-8", -1.85) ! acetonitrile
    CALL CalcH("pentane nitrile",                            "110-59-8", -1.57) ! 1-cyanobutane
    CALL CalcH("butane nitrile",                             "109-74-0", -1.68) ! 1-cyanopropane
    CALL CalcH("nitromethane",                                "75-52-5", -2.26) ! nitromethane
    CALL CalcH("nitroethane",                                 "79-24-3", -2.18) ! nitroethane
    CALL CalcH("1-nitropropane",                             "108-03-2", -2.07) ! 1-nitropropane
    CALL CalcH("2-nitropropane",                              "79-46-9", -2.01) ! 2-nitropropane
    CALL CalcH("1-nitrobutane",                              "627-05-4", -1.96) ! 1-nitrobutane
    CALL CalcH("1-nitropentane",                             "628-05-7", -1.85) ! 1-nitropentane
    CALL CalcH("benzenenitrile",                             "100-47-0", -2.57) ! benzonitrile
    CALL CalcH("nitrobenzene",                                "98-95-3", -3.91) ! nitrobenzene
    CALL CalcH("2-nitrotoluene",                              "88-72-2", -3.76) ! 2-nitrotoluene
    CALL CalcH("3-nitrotoluene",                              "99-08-1", -3.76) ! 3-nitrotoluene
    ! pyridines:
    CALL CalcH("pyridine",                                   "110-86-1", -3.65) ! pyridine
    CALL CalcH("2-methylpyridine",                           "109-06-8", -3.51) ! 2-methylpyridine
    CALL CalcH("3-methylpyridine",                           "108-99-6", -3.51) ! 3-methylpyridine
    CALL CalcH("4-methylpyridine",                           "108-89-4", -3.51) ! 4-methylpyridine
    CALL CalcH("2,3-dimethylpyridine",                       "583-61-9", -3.37) ! 2,3-dimethylpyridine
    CALL CalcH("2,4-dimethylpyridine",                       "108-47-4", -3.36) ! 2,4-dimethylpyridine
    CALL CalcH("2,5-dimethylpyridine",                       "589-93-5", -3.36) ! 2,5-dimethylpyridine
    CALL CalcH("2,6-dimethylpyridine",                       "108-48-5", -3.37) ! 2,6-dimethylpyridine
    CALL CalcH("3,4-dimethylpyridine",                       "583-58-4", -3.36) ! 3,4-dimethylpyridine
    CALL CalcH("3,5-dimethylpyridine",                       "591-22-0", -3.36) ! 3,5-dimethylpyridine
    CALL CalcH("2-ethylpyridine",                            "100-71-0", -3.43) ! 2-ethylpyridine
    CALL CalcH("3-ethylpyridine",                            "536-78-7", -3.43) ! 3-ethylpyridine
    CALL CalcH("4-ethylpyridine",                            "536-75-4", -3.43) ! 4-ethylpyridine
    CALL CalcH("2-chloropyridine",                           "109-09-1", -4.57) ! 2-chloropyridine
    CALL CalcH("3-chloropyridine",                           "626-60-8", -4.56) ! 3-chloropyridine
    CALL CalcH("3-formylpyridine",                           "500-22-1", -4.97) ! 3-formylpyridine
    CALL CalcH("4-formylpyridine",                           "872-85-5", -4.97) ! 4-formylpyridine
    CALL CalcH("3-acetylpyridine",                           "350-03-8", -4.83) ! 3-acetylpyridine
    CALL CalcH("4-acetylpyridine",                          "1122-54-9", -4.83) ! 4-acetylpyridine
    ! sulfonated compounds:
    CALL CalcH("dimethyl sulfide",                            "75-18-3", -1.21) ! dimethylsulfide
    CALL CalcH("diethyl sulfide",                            "352-93-2", -0.86) ! diethylsulfide
    CALL CalcH("dipropyl sulfide",                           "111-47-7", -0.65) ! dipropylsulfide
    CALL CalcH("di-(2-propyl)-sulfide",                      "625-80-9", -0.46) ! di-isopropylsulfide
    CALL CalcH("methyl phenyl sulfide",                      "100-68-5", -1.75) ! methylthiobenzene
    !CALL CalcH("methyl phenyl sulfide",                     "100-68-5", -1.75) ! phenylmethylsulfide
    CALL CalcH("ethyl methyl sulfide",                       "624-89-5", -1.04) ! methylethylsulfide
    CALL CalcH("diethyl disulfide",                          "110-81-6", -0.75) ! diethyldisulfide
    CALL CalcH("dimethyl disulfide",                         "624-92-0", -1.06) ! dimethyldisulfide
    ! Tab.4:                                                              Tab.4:
    CALL CalcH("benzaldehyde",                               "100-52-7", -3.25) ! benzaldehyde
    CALL CalcH("4-methylbenzaldehyde",                       "104-87-0", -3.11) ! 4-methylbenzaldehyde
    CALL CalcH("3-hydroxybenzaldehyde",                      "100-83-4", -7.87) ! 3-hydroxybenzaldehyde
    CALL CalcH("4-hydroxybenzaldehyde",                      "123-08-0", -7.87) ! 4-hydroxybenzaldehyde
    CALL CalcH("1-phenylethanone",                            "98-86-2", -3.12) ! acetophenone
    CALL CalcH("(4-methylphenyl)-ethanone",                  "122-00-9", -2.97) ! 4-methylacetophenone
    CALL CalcH("cyclopentanone",                             "120-92-3", -3.25) ! cyclopentanone
    CALL CalcH("cyclohexanone",                              "108-94-1", -3.14) ! cyclohexanone
    CALL CalcH("1-cyclopropyl-ethanone",                     "765-43-5", -3.20) ! methylcyclopropylketone
    CALL CalcH("cyclohexyl methyl ketone",                   "823-76-7", -2.89) ! methylcyclohexylketone
    CALL CalcH("4-methoxyphenyl methyl ketone",              "100-06-1", -3.51) ! 4-methoxyacetophenone
    CALL CalcH("N,N-dimethylmethanamide",                     "68-12-2", -5.73) ! N,N-dimethylformamide
    CALL CalcH("1-amino-2-chlorobenzene",                     "95-51-2", -4.13) ! 2-chloroaniline
    CALL CalcH("1-amino-3-chlorobenzene",                    "108-42-9", -4.12) ! 3-chloroaniline
    CALL CalcH("1-amino-4-chlorobenzene",                    "106-47-8", -4.12) ! 4-chloroaniline
    CALL CalcH("2-methoxy-benzenamine",                       "90-04-0", -4.57) ! 2-methoxyaniline
    CALL CalcH("3-methoxy-benzenamine",                      "536-90-3", -4.57) ! 3-methoxyaniline
    CALL CalcH("4-methoxy-benzenamine",                      "104-94-9", -4.57) ! 4-methoxyaniline
    CALL CalcH("2-nitrobenzenamine",                          "88-74-4", -6.05) ! 2-nitroaniline
    CALL CalcH("3-nitrobenzenamine",                          "99-09-2", -6.04) ! 3-nitroaniline
    CALL CalcH("4-nitrobenzenamine",                         "100-01-6", -6.04) ! 4-nitroaniline
    CALL CalcH("2-methylbenzenamine",                         "95-53-4", -3.69) ! o-toluidine
    CALL CalcH("4-methylbenzenamine",                        "106-49-0", -3.69) ! p-toluidine
    CALL CalcH("cyclohexanamine",                            "108-91-8", -3.48) ! cyclohexylamine
    CALL CalcH("1-naphthylamine",                            "134-32-7", -6.06) ! 1-naphthylamine
    CALL CalcH("2-naphthylamine",                             "91-59-8", -6.05) ! 2-naphthylamine
    CALL CalcH("benzamide",                                   "55-21-0", -7.31) ! benzamide
    CALL CalcH("cyclopentanol",                               "96-41-3", -4.04) ! cyclopentanol
    CALL CalcH("cyclohexanol",                               "108-93-0", -3.82) ! cycloheptanol
    CALL CalcH("2-methoxyethanol",                           "109-86-4", -4.57) ! 2-methoxyethanol
    CALL CalcH("2-ethoxyethanol",                            "110-80-5", -4.27) ! 2-ethoxyethanol
    CALL CalcH("3-oxa-1-hexanol",                           "2807-30-9", -4.16) ! 2-propoxyethanol
    CALL CalcH("3-oxa-1-heptanol",                           "111-76-2", -4.05) ! 2-butoxyethanol
    CALL CalcH("1-hydroxy-2-methoxybenzene",                  "90-05-1", -6.10) ! 2-methoxyphenol
    CALL CalcH("1-hydroxy-3-methoxybenzene",                 "150-19-6", -6.09) ! 3-methoxyphenol
    CALL CalcH("1-naphthalenol",                              "90-15-3", -6.57) ! 1-naphthol
    CALL CalcH("2-naphthalenol",                             "135-19-3", -6.62) ! 2-naphthol
    CALL CalcH("(hydroxymethyl)-benzene",                    "100-51-6", -5.23) ! benzylalcohol
    CALL CalcH("2-phenylethanol",                             "60-12-8", -5.12) ! 2-phenylethanol
    CALL CalcH("3-phenyl-1-propanol",                        "122-97-4", -5.02) ! 3-phenylpropanol
    CALL CalcH("3-hydroxybenzoic acid nitrile",              "873-62-1", -8.91) ! 3-cyanophenol
    CALL CalcH("4-hydroxybenzoic acid nitrile",              "767-00-0", -8.91) ! 4-cyanophenol
    CALL CalcH("2-nitrophenol",                               "88-75-5", -7.57) ! 2-nitrophenol
    CALL CalcH("3-nitrophenol",                              "554-84-7", -7.57) ! 3-nitrophenol
    CALL CalcH("4-nitrophenol",                              "100-02-7", -7.57) ! 4-nitrophenol
    CALL CalcH("3-cyanopyridine",                            "100-54-9", -5.48) ! 3-cyanopyridine
    CALL CalcH("4-cyanopyridine",                            "100-48-1", -5.48) ! 4-cyanopyridine
    CALL CalcH("benzo[$b$]pyridine",                          "91-22-5", -4.92) ! quinoline
    CALL CalcH("2-methylpyrazine",                           "109-08-0", -3.89) ! 2-methylpyrazine
    CALL CalcH("2-ethylpyrazine",                          "13925-00-3", -3.83) ! 2-ethylpyrazine
    CALL CalcH("2-isobutylpyrazine",                       "29460-92-2", -3.55) ! 2-isobutylpyrazine
    CALL CalcH("N-methylpiperidine",                         "626-67-5", -2.74) ! N-methylpiperidine
    CALL CalcH("diethyl ether",                               "60-29-7", -0.62) ! diethyl ether
    CALL CalcH("methyl propyl ether",                        "557-17-5", -0.57) ! methylpropyl ether
    CALL CalcH("dipropyl ether",                             "111-43-3", -0.40) ! di-n-propyl ether
    CALL CalcH("diisopropyl ether",                          "108-20-3", -0.30) ! di-isopropyl ether
    CALL CalcH("dibutyl ether",                              "142-96-1", -0.20) ! di-n-butyl ether
    CALL CalcH("dimethyl ether",                             "115-10-6", -0.74) ! dimethyl ether
    CALL CalcH("ethyl methyl ether",                         "540-67-0", -0.67) ! methylethyl ether
    CALL CalcH("methyl {tert}-butyl ether",                 "1634-04-4", -0.33) ! methyl-tert-butyl ether
    CALL CalcH("methoxybenzene",                             "100-66-3", -1.47) ! methylphenyl ether
    CALL CalcH("ethoxybenzene",                              "103-73-1", -1.41) ! ethylphenyl ether
    CALL CalcH("divinyl ether",                              "109-93-3", -0.70) ! divinyl ether
    CALL CalcH("methanethiol",                                "74-93-1", -0.86) ! methanethiol
    CALL CalcH("ethanethiol",                                 "75-08-1", -0.68) ! ethanethiol
    CALL CalcH("1-propanethiol",                             "107-03-9", -0.58) ! propanethiol
    CALL CalcH("1-butanethiol",                              "109-79-5", -0.47) ! butanethiol
    CALL CalcH("benzenethiol",                               "108-98-5", -1.40) ! thiophenol
    CALL CalcH("1-oxa-4-azacyclohexane",                     "110-91-8", -4.40) ! morpholine
    CALL CalcH("4-methyl-1-oxa-4-azacyclohexane",            "109-02-4", -4.62) ! N-methylmorpholine

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, logH)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: logH
      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = KHcc_TIMES_HcpSI_atT0/(10.**logH)
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0984

  !---------------------------------------------------------------------------

  SUBROUTINE ref0985 ! KHcc [1]
    IMPLICIT NONE

    ref = "985"
    type = "M"

    chem = "2-methylpropenal" ; casrn = "78-85-3" ! C4H6O methacrolein
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.,283.,288.,293.,298./)
    Harray = (/0.0028,0.0039,0.0054,0.0069,0.0095/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "3-buten-2-one" ; casrn = "78-94-4" ! C4H6O methyl vinyl ketone, MVK
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.,283.,288.,293.,298./)
    Harray = (/0.00029,0.00056,0.0008,0.0012,0.0019/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "benzaldehyde" ; casrn = "100-52-7" ! C6H5CHO
    ndata = 5
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/278.,283.,288.,293.,298./)
    Harray = (/0.00024,0.00035,0.00050,0.00074,0.00125/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "1-phenylethanone" ; casrn = "98-86-2" ! C6H5COCH3 acetophenone
    ndata = 4
    ALLOCATE(temp(ndata), Harray(ndata))
    temp = (/288.,290.5,293.,298./)
    Harray = (/0.00011,0.00015,0.00019,0.00043/)
    Harray = KHcc_TO_HcpSI(Harray,temp)
    CALL HTdep(temp, Harray, Hominus, mindHR)
    CALL Output(Hominus, mindHR, r2)
    DEALLOCATE(temp, Harray)

    chem = "benzene" ; casrn = "71-43-2" ! C6H6
    Hominus = KHcc_TO_HcpSI(0.227, 298.)
    CALL Output(Hominus)

    chem = "methylbenzene" ; casrn = "108-88-3" ! C6H5CH3 toluene
    Hominus = KHcc_TO_HcpSI(0.263, 298.)
    CALL Output(Hominus)

    chem = "ethylbenzene" ; casrn = "100-41-4" ! C6H5C2H5
    Hominus = KHcc_TO_HcpSI(0.320, 298.)
    CALL Output(Hominus)

  END SUBROUTINE ref0985

  !---------------------------------------------------------------------------

  SUBROUTINE ref0992 ! ln(Hbp) [mol/(kg*atm)]
    IMPLICIT NONE

    ref = "992"
    type = "M"
    chem = "trichloroethanoic acid" ; casrn = "76-03-9" ! CCl3COOH
    Hominus = EXP(11.21)*Hbp_TO_HcpSI
    mindHR = 8.66E3
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0992

  !---------------------------------------------------------------------------

  SUBROUTINE ref0995
    IMPLICIT NONE

    ref = "995"
    type = "M"
    chem = "peroxyacetyl radical" ; casrn = "36709-10-1"
    CALL Output(0.1*Hcp_TO_HcpSI, limit="<")

  END SUBROUTINE ref0995

  !---------------------------------------------------------------------------

  SUBROUTINE ref0997 ! KHpb [bar*kg/mol]
    IMPLICIT NONE
    REAL, PARAMETER :: A = +0.6342702616E+3
    REAL, PARAMETER :: B = +0.2709284796E+0
    REAL, PARAMETER :: C = -0.1113202904E-3
    REAL, PARAMETER :: D = -0.1671907660E+5
    REAL, PARAMETER :: E = -0.2619219571E+3

    ref = "997"
    type = "M"
    chem = "hydrogen sulfide" ; casrn = "7783-06-4" ! H2S
    ! T-dep with 5 parameters:
    ! lg(H) = A + BT + CT^2 + D/T + E*lg(T)
    Hominus = (rhoH2O/bar) / (10.**(A + B*T0 + C*T0**2 + D/T0 + E*LOG10(T0)))
    ! analytical derivative: (see also ref1909.gnu)
    ! d ln(H) / d (1/T) = ln(10) * (-BT^2 -2CT^3 + D - E*T/ln(10))
    mindHR = -LOG(10.) * (-B*T0**2 -2.*C*T0**3 + D - E*T0/LOG(10.))
    CALL Output(Hominus, mindHR)

  END SUBROUTINE ref0997

  !---------------------------------------------------------------------------

  SUBROUTINE ref0998 ! KHpcSI [Pa*m3/mol]
    IMPLICIT NONE

    ref = "998"
    type = "M"
    CALL CalcH("1,2C2",     "628-96-6",    1.28)
    CALL CalcH("1,3C3",     "3457-90-7",   0.76)
    CALL CalcH("1,4C4",     "3457-91-8",   0.64)
    CALL CalcH("1,5C5",     "3457-92-9",   0.85)
    CALL CalcH("1,6C6",     "3457-93-0",   0.67)
    CALL CalcH("1,7C7",     "3457-94-1",   0.87)
    CALL CalcH("1,8C8",     "3457-95-2",   1.28)
    CALL CalcH("1,10C10",   "3457-97-4",   2.35)
    CALL CalcH("1,2C3",     "6423-43-4",   3.12)
    CALL CalcH("1,2C4",     "20820-41-1",  4.85)
    CALL CalcH("1,2C5",     "89365-05-9",  7.57)
    CALL CalcH("1,2C6",     "110539-07-6", 10.41)
    CALL CalcH("1,2C8",     "121222-48-8", 19.18)
    CALL CalcH("1,2C10",    "60123-40-2",  51.09)
    CALL CalcH("1,3C4",     "6423-44-5",   1.75)
    CALL CalcH("2,3C4",     "6423-45-6",   8.21)
    CALL CalcH("1,4C5",     "25385-63-1",  2.57)
    CALL CalcH("C-2,4C5",   "208252-05-5", 4.64)
    CALL CalcH("T-2,4C5",   "208252-04-4", 6.98)
    CALL CalcH("1,5C6",     "206443-83-6", 3.67)
    CALL CalcH("2,5C6",     "99115-63-6",  3.21)
    CALL CalcH("T-1,2CYC6", "32342-29-3",  1.94)
    CALL CalcH("C-1,2CYC6", "32342-28-2",  0.79)
    CALL CalcH("T-1,3CYC6", "170994-41-9", 1.46)
    CALL CalcH("C-1,3CYC6", "170994-36-2", 0.29)
    CALL CalcH("T-1,2CYC7", "208252-06-6", 1.14)

  CONTAINS

    SUBROUTINE CalcH (CHEM_, CASRN_, KHpcSI)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CHEM_
      CHARACTER(LEN=*), INTENT(IN) :: CASRN_
      REAL, INTENT(IN)             :: KHpcSI

      chem  = CHEM_  ! make value global, so Output will find it
      casrn = CASRN_ ! make value global, so Output will find it
      Hominus = KHpcSI_TIMES_HcpSI / KHpcSI
      CALL MakeNote("sameas978", &
        "The same data were also published in \citet{978}.")
      CALL Output(Hominus)
    END SUBROUTINE CalcH

  END SUBROUTINE ref0998

  !---------------------------------------------------------------------------

END MODULE Henry_ref1000

!*****************************************************************************
!                                  end of file
!*****************************************************************************
