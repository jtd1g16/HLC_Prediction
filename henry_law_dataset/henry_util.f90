!*****************************************************************************
!                   Time-stamp: <2015-04-10 16:19:54 sander>
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

MODULE henry_util

  USE henry_mem
  IMPLICIT NONE
  SAVE
  PUBLIC

  INTERFACE str
     MODULE PROCEDURE str_logical
     MODULE PROCEDURE str_integer
     MODULE PROCEDURE str_real
  END INTERFACE

CONTAINS

  !---------------------------------------------------------------------------

  SUBROUTINE ShowConv

    ! How to add a new type of Henry solubility constant:
    ! - add another column "c" to \begin{tabular} (Tabs 2,4)
    ! - in henry_mem.f90:
    !   increase PARAMETER N_SOLUB
    !   define conv_solub(N_SOLUB)
    !   define texsymbol_solub(N_SOLUB)
    !   define txtsymbol_solub(N_SOLUB)
    !   define unit_fr(N_SOLUB)
    !   define unit_ol(N_SOLUB)

    ! How to add a new type of Henry volatility constant:
    ! - add another column "c" to \begin{tabular} (Tab 3)
    ! - in henry_mem.f90:
    !   increase PARAMETER N_VOLAT
    !   define conv_volat(n)
    !   define texsymbol_volat(n)
    !   define txtsymbol_volat(n)
    !   define unit_fr_inv(n)
    !   define unit_ol_inv(n)

    INTEGER, PARAMETER :: N_DIGITS=6
    INTEGER :: row, col

    OPEN (IO_CONV,FILE="henry_convert.tex", STATUS="REPLACE")

    ! Table II
    CALL TeX("\ifacpd\begin{table}\else\begin{table*}\fi")
    CALL TeX("\caption{\label{tab:conv-solub}Conversion factors between")
    CALL TeX("  several Henry's law solubility constants $\H$")
    CALL TeX("  (at $T^{\ominus}$~= 298.15~\unit{K} and")
    CALL TeX("  $\varrho^{\ominus}$~= 997~\unit{kg/m^3}).}")
    CALL TeX("\begin{center}")
    ! redefine \E and remove space around "-":
    ! CALL TeX('\renewcommand{\E}[1]{\mathcode`-="0200\mbox{E}#1}')
    CALL TeX("\renewcommand{\arraystretch}{2.5}")
    ! manually put here N_SOLUB times "c" into the next line:
    CALL TeX("\resizebox{\textwidth}{!}{\begin{tabular}{lccccccc}")
    CALL TeX("\hline")
    CALL TeX("\rule[-3ex]{0ex}{6ex}")
    DO col = 1, N_SOLUB
      CALL TeX("& $"//texsymbol_solub(col)//" = \dots \unit{"// &
        TRIM(unit_fr(col))//"}$")
    ENDDO
    CALL TeX("\\")
    CALL TeX("\hline")
    DO row = 1, N_SOLUB
      CALL TeX("$"//texsymbol_solub(row)//"$ = 1 \unit{"// &
        TRIM(unit_fr(row))//"}")
      DO col = 1, N_SOLUB
        CALL TEX("& $"//TeXnumber(conv_solub(row)/conv_solub(col),N_DIGITS)//"$")
      ENDDO
    CALL TeX("\\")
    ENDDO
    CALL TeX("\hline")
    CALL TeX("\end{tabular}}")
    CALL TeX("\end{center}")
    CALL TeX("\ifacpd\end{table}\else\end{table*}\fi")
    CALL TeX("")

    ! Table III
    CALL TeX("\ifacpd\begin{table}\else\begin{table*}\fi")
    CALL TeX("\caption{\label{tab:conv-volat}Conversion factors between")
    CALL TeX("  several Henry's law volatility constants $\KH$")
    CALL TeX("  (at $T^{\ominus}$~=")
    CALL TeX("  298.15~\unit{K} and $\varrho^{\ominus}$~=")
    CALL TeX("  997~\unit{kg/m^3}).}")
    CALL TeX("\begin{center}")
    ! redefine \E and remove space around "-":
    ! CALL TeX('\renewcommand{\E}[1]{\mathcode`-="0200\mbox{E}#1}')
    CALL TeX("\renewcommand{\arraystretch}{2.5}")
    CALL TeX("\begin{tabular}{lcccc}") ! manually put here N_VOLAT times "c"
    CALL TeX("\hline")
    CALL TeX("\rule[-3ex]{0ex}{6ex}")
    DO col = 1, N_VOLAT
      CALL TeX("& $"//texsymbol_volat(col)//" = \dots \unit{"// &
        TRIM(unit_fr_inv(col))//"}$")
    ENDDO
    CALL TeX("\\")
    CALL TeX("\hline")
    DO row = 1, N_VOLAT
      CALL TeX("$"//texsymbol_volat(row)//"$ = 1 \unit{"// &
        TRIM(unit_fr_inv(row))//"}")
      DO col = 1, N_VOLAT
        CALL TeX("& $"//TeXnumber(conv_volat(col)/conv_volat(row),N_DIGITS)//"$")
      ENDDO
    CALL TeX("\\")
    ENDDO
    CALL TeX("\hline")
    CALL TeX("\end{tabular}")
    CALL TeX("\end{center}")
    CALL TeX("\ifacpd\end{table}\else\end{table*}\fi")
    CALL TeX("")

    ! Table IV
    CALL TeX("\ifacpd\begin{table}\else\begin{table*}\fi")
    CALL TeX("\caption{%")
    CALL TeX("  \label{tab:conv-solub-volat}Products of Henry's law")
    CALL TeX("  solubility constants $\H$ and")
    CALL TeX("  Henry's law volatility constants $\KH$")
    CALL TeX("  (at $T^{\ominus}$~=")
    CALL TeX("  298.15~\unit{K} and $\varrho^{\ominus}$~=")
    CALL TeX("  997~\unit{kg/m^3}).")
    CALL TeX("  For example, if $\KHpx =$ 5~\unit{atm}, then ")
    CALL TeX("  $H^{bp} \approx$ 11~\unit{mol/(kg~atm)}")
    CALL TeX("  because $5 \times 11 \approx 55.5084$.}")
    CALL TeX("\begin{center}")
    ! redefine \E and remove space around "-":
    ! CALL TeX('\renewcommand{\E}[1]{\mathcode`-="0200\mbox{E}#1}')
    CALL TeX("\renewcommand{\arraystretch}{2.5}")
    ! manually put here N_SOLUB times "c":
    CALL TeX("\ifacpd\scalebox{0.7}[0.9]{\fi\begin{tabular}{cccccccc}")
    CALL TeX("\hline")
    CALL TeX("\rule[-3ex]{0ex}{6ex}")
    DO col = 1, N_SOLUB
      CALL TeX("& $\DS\frac{"//texsymbol_solub(col)//"}{\unit{"// &
        TRIM(unit_ol(col))//"}}$")
    ENDDO
    CALL TeX("\\")
    CALL TeX("\hline")
    DO row = 1, N_VOLAT
      CALL TeX("$\DS\frac{"//texsymbol_volat(row)//"}{\unit{"// &
        TRIM(unit_ol_inv(row))//"}}$")
      DO col = 1, N_SOLUB
        CALL TEX("& $"//TeXnumber(conv_volat(row)/conv_solub(col),N_DIGITS)//"$")
      ENDDO
    CALL TeX("\\")
    ENDDO
    CALL TeX("\hline")
    CALL TeX("\end{tabular}\ifacpd}\fi")
    CALL TeX("\end{center}")
    CALL TeX("\ifacpd\end{table}\else\end{table*}\fi")

    CLOSE(IO_CONV)

  CONTAINS

    !-------------------------------------------------------------------------

    SUBROUTINE TeX(TeXstring)
      CHARACTER(LEN=*), INTENT(IN)  :: TeXstring
      WRITE(IO_CONV,'(A)') TeXstring
    END SUBROUTINE TeX

    !-------------------------------------------------------------------------

  END SUBROUTINE ShowConv

  !---------------------------------------------------------------------------

  CHARACTER(LEN=strlen) FUNCTION TeXnumber(value,digits)
    ! take a REAL variable as input and return it as a string, with or
    ! without an exponent, depending on the magnitude of the value
    IMPLICIT NONE
    REAL,    INTENT(IN) :: value
    INTEGER, INTENT(IN) :: digits
    ! local:
    CHARACTER(LEN=strlen) :: workstr, formatstr
    IF ((ABS(value)<1E-2).OR.(ABS(value)>=10.**(digits-1))) THEN
      WRITE(formatstr,'(A,I0,A,I0,A)') '(1ES', digits+6, '.', digits-1, ')'
    ELSE
      WRITE(formatstr,'(A,I0,A)') &
        '(F12.', digits-FLOOR(LOG10(ABS(value)))-1, ')'
    ENDIF
    WRITE(workstr,formatstr) value
    TeXnumber = F90toTeX(workstr)
  END FUNCTION TeXnumber

  !---------------------------------------------------------------------------

  CHARACTER(LEN=STRLEN_VLONG) FUNCTION H_range(H_min, H_max)
    IMPLICIT NONE
    REAL, INTENT(IN) :: H_min, H_max
    H_range = TRIM(H_TeX(H_min))//" $"//lessthan//" \Hsymbol "//lessthan// &
      "$ "//TRIM(H_TeX(H_max))
  END FUNCTION H_range

  !---------------------------------------------------------------------------

  CHARACTER(LEN=STRLEN) FUNCTION H_TeX(H_)
    IMPLICIT NONE
    REAL, INTENT(IN) :: H_
    H_TeX = TRIM(H_string(H_))//" \unit{"//TRIM(H_unit_ol)//"}"
  END FUNCTION H_TeX

  !---------------------------------------------------------------------------

  CHARACTER(LEN=STRLEN) FUNCTION H_string(H_)
    IMPLICIT NONE
    REAL, INTENT(IN) :: H_
    ! convert REAL H_ to the currently selected unit and then
    ! create the TeX string H_string:
    IF (ABS(H_-DUMMY) > TINY(0.)) THEN
      WRITE(H_string,"(ES8.1)") H_*conv_fact
      H_string = F90toTeX(H_string)
    ELSE
      H_string = ""
    ENDIF
  END FUNCTION H_string

  !---------------------------------------------------------------------------

  CHARACTER(LEN=STRLEN_VLONG) FUNCTION citet()
    IMPLICIT NONE
    citet = "\citet{"//TRIM(ref)//"}"
  END FUNCTION citet

  !---------------------------------------------------------------------------

  CHARACTER(LEN=STRLEN) FUNCTION Hprime_string(Hprime_)
    IMPLICIT NONE
    REAL, INTENT(IN) :: Hprime_
    ! convert REAL Hprime_ to string Hprime_string:
    IF (ABS(Hprime_-DUMMY) > TINY(0.)) THEN
      Hprime_string = H_string(Hprime_*dm3) ! another conversion!!!
    ELSE
      Hprime_string = H_string(DUMMY)
    ENDIF
  END FUNCTION Hprime_string

  !---------------------------------------------------------------------------

  CHARACTER(LEN=STRLEN) FUNCTION mindHR_string(mindHR_)
    IMPLICIT NONE
    REAL, INTENT(IN) :: mindHR_
    ! convert REAL mindHR_ to string mindHR_string:
    IF (ABS(mindHR_-DUMMY) > TINY(0.)) THEN
      WRITE(mindHR_string,"(I0)") NINT(round(mindHR_,2))
      mindHR_string = F90toTeX(mindHR_string)
    ELSE
      mindHR_string = "" ! no mindHR available
    ENDIF
  END FUNCTION mindHR_string

  !---------------------------------------------------------------------------

  CHARACTER(LEN=STRLEN) FUNCTION F90toTeX(f90string)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: f90string
    ! local:
    CHARACTER(LEN=STRLEN) :: mantissa ! mantissa
    CHARACTER(LEN=STRLEN) :: expo_abs ! exponent, absolute value
    CHARACTER             :: expo_sgn ! exponent, sign
    INTEGER :: idx

    INTRINSIC :: ADJUSTL

    IF (VERIFY(f90string,' .0123456789+-eE')/=0) THEN
      PRINT *, 'ERROR in f90string: The string "'//TRIM(f90string)// &
        '" contains illegal characters.'
      STOP
    ENDIF

    ! check if the string contains "E" or "e":
    idx = SCAN(f90string,'Ee')
    IF (idx>0) THEN
      mantissa = ADJUSTL(f90string(1:idx-1))
      ! check if the next character is "+" or "-":
      SELECT CASE(f90string(idx+1:idx+1))
      CASE ('+')
        expo_sgn = ''
        expo_abs = f90string(idx+2:)
      CASE ('-')
        expo_sgn = '-'
        expo_abs = f90string(idx+2:)
      CASE DEFAULT
        expo_sgn = ''
        expo_abs = f90string(idx+1:)
      END SELECT
    ELSE
      ! no exponent found:
      mantissa = ADJUSTL(f90string)
      expo_sgn = ''
      expo_abs = ''
    ENDIF

    ! delete leading zeroes from exponent:
    idx = VERIFY(expo_abs,'0')
    ! idx now points to the first non-zero character in exponent
    IF (idx>0) THEN
      expo_abs = expo_abs(idx:)
    ELSE
      ! exponent is zero:
      expo_abs = ''
    ENDIF

    IF (TRIM(expo_abs)/='') THEN
      F90toTeX = TRIM(mantissa)//'\E{'// &
        TRIM(expo_sgn)//TRIM(ADJUSTL(expo_abs))//'}'
    ELSE
      F90toTeX = TRIM(mantissa)
    ENDIF

    ! PRINT *, '*************'
    ! PRINT *, 'inputstr = ', '"'//TRIM(f90string)//'"'
    ! PRINT *, 'mantissa = ', '"'//TRIM(mantissa)//'"'
    ! PRINT *, 'expo_sgn = ', '"'//TRIM(expo_sgn)//'"'
    ! PRINT *, 'expo_abs = ', '"'//TRIM(expo_abs)//'"'

  END FUNCTION F90toTeX

  !---------------------------------------------------------------------------

  SUBROUTINE LongtableHeader
    IMPLICIT NONE

    WRITE(IO_DATA,'(A)') "\tophline"
    WRITE(IO_DATA,'(A)') "\begin{tabular}{@{}l@{}}"
    WRITE(IO_DATA,'(A)') "Substance\\Formula\\(Trivial Name)\\{}%"
    ! WRITE(IO_DATA,'(A)') "[CAS Registry Number]\\InChIKey"
    WRITE(IO_DATA,'(A)') "[CAS Registry Number]"
    WRITE(IO_DATA,'(A)') "\end{tabular} &"
    WRITE(IO_DATA,'(A)') "\begin{tabular}{@{}c@{}}"
    WRITE(IO_DATA,'(A)') "$"//TRIM(H_symbol)//"$\\[1mm]"
    WRITE(IO_DATA,'(A)') "(at $T^{\ominus}$)\\[2mm]"
    WRITE(IO_DATA,'(A)') "\unit{\left["//TRIM(H_unit_fr)//"\right]}"
    WRITE(IO_DATA,'(A)') "\end{tabular} &"
    WRITE(IO_DATA,'(A)') "\begin{tabular}{@{}c@{}}"
    WRITE(IO_DATA,'(A)') "$\DS\frac{\dd\ln "//TRIM(H_symbol)// &
      "}{\dd(1/T)}$\\[6mm]"
    WRITE(IO_DATA,'(A)') "[\unit{K}]"
    WRITE(IO_DATA,'(A)') "\end{tabular} &"
    WRITE(IO_DATA,'(A)') "Reference & Type & Note\\"
    WRITE(IO_DATA,'(A)') "\middlehline"

  END SUBROUTINE LongtableHeader

  !---------------------------------------------------------------------------

  SUBROUTINE Initialize
    IMPLICIT NONE

    ! local:
    INTEGER :: j
    INTEGER :: iconv_fact
    NAMELIST /CTRL/ iconv_fact, l_showpics, l_showbiblabel, l_debug

    ! read namelist:
    OPEN  (IO_NML, FILE='henry.nml')
    READ  (IO_NML, NML=CTRL)
    CLOSE (IO_NML)
    PRINT *, "The following settings were read from the namelist in henry.nml:"
    PRINT *, "iconv_fact:     ", iconv_fact
    PRINT *, "l_showpics:     ", l_showpics
    PRINT *, "l_showbiblabel: ", l_showbiblabel
    PRINT *, "l_debug:        ", l_debug

    OPEN (IO_DATA,FILE="henry_data.tex",              STATUS="REPLACE")
    OPEN (IO_DEFS,FILE="henry_defs-private.sty",      STATUS="REPLACE")
    OPEN (IO_NOTE,FILE="henry_notes.tex",             STATUS="REPLACE")
    OPEN (IO_WARN,FILE="tmp_warnings.txt",            STATUS="REPLACE")
    OPEN (IO_ODBH,FILE="output/database_henry.csv",   STATUS="REPLACE")
    OPEN (IO_ODBS,FILE="output/database_species.csv", STATUS="REPLACE")
    IF (l_debug) THEN
      OPEN (IO_DEBG,FILE="tmp_debug.txt",             STATUS="REPLACE")
    ENDIF
    OPEN (IO_SPCA,FILE="tmp_species_chem.txt",        STATUS="REPLACE")
    OPEN (IO_SPCB,FILE="tmp_species_data.txt",        STATUS="REPLACE")
    OPEN (IO_SPCC,FILE="tmp_species_calch.txt",       STATUS="REPLACE")
    IF (l_showpics) THEN
      OPEN (IO_PICS,FILE="pics/getpics",              STATUS="REPLACE")
    ENDIF

    ! f90 output files:
    DO j=1,N_SOLUB
      OPEN (IO_SOLUB+j, FILE="output/"//TRIM(txtsymbol_solub(j))//".f90", &
        STATUS="REPLACE")
    ENDDO
    DO j=1,N_VOLAT
      OPEN (IO_VOLAT+j, FILE="output/"//TRIM(txtsymbol_volat(j))//".f90", &
        STATUS="REPLACE")
    ENDDO

    ! TAB-separated headers:
    WRITE(IO_ODBH,'(A)') 'casrn	HcpSI	Hcp	mindHr	ref	type'
    WRITE(IO_ODBS,'(A)') 'iupac	formula	trivial	casrn'

    IF (l_showpics) THEN
      WRITE(IO_PICS,'(A)') "#! /usr/bin/tcsh -f"
    ENDIF

    IF (l_showbiblabel) THEN
      WRITE(IO_DEFS,'(A)') &
        "\let\citepdummy=\citep\renewcommand{\citep}[1]{#1=\citepdummy{#1}}%"
      WRITE(IO_DEFS,'(A)') &
        "\let\citetdummy=\citet\renewcommand{\citet}[1]{#1=\citetdummy{#1}}%"
      WRITE(IO_DEFS,'(A)') &
        "\renewcommand{\egcite}[1]{#1=\citepdummy[e.g.][]{#1}}%"
    ENDIF

    n_refs = 0       ! number of references
    idx_s = 0        ! current index in allcasrn array
    idx_h = 0        ! number of entries in henry_data array
    idx_n = 0        ! number of entries in henry_notes array
    idx_allnotes = 1 ! current index in allnotes
    AllNotes = ""
    DO j = 1, MAXNOTES
      henry_notes(j)%start = 0
      henry_notes(j)%end   = 0
    ENDDO

    SELECT CASE(iconv_fact)
    CASE (1)
      conv_fact = 1./conv_solub(1)
      H_symbol  = texsymbol_solub(1)
      H_unit_ol = TRIM(unit_ol(1))
      H_unit_fr = TRIM(unit_fr(1)) ! unit shown as a fraction
      Hprime_unit_ol_TeX = "\unit{\DS\frac{mol^2}{m^6\,Pa}}" ! "\unit{mol^2/(m^6\,Pa)}"
      lessthan = "<"
      morethan = ">"
    CASE (2)
      conv_fact = 1./conv_solub(2)
      H_symbol  = texsymbol_solub(2)
      H_unit_ol = TRIM(unit_ol(2))
      H_unit_fr = TRIM(unit_fr(2)) ! unit shown as a fraction
      Hprime_unit_ol_TeX = "\unit{M^2/atm}"
      lessthan = "<"
      morethan = ">"
    CASE DEFAULT
      PRINT *, "ERROR: Unknown conversion factor."
      STOP
    END SELECT
    PRINT *, "You selected "//TRIM(H_symbol)//" with the unit: "//TRIM(H_unit_ol)

    WRITE(IO_DATA,'(A)') "\begin{longtable}"// &
      "{@{}p{50mm}p{15mm}p{10mm}p{51mm}p{5mm}p{11mm}}"
    WRITE(IO_DATA,'(A)') "\caption{Henry's law constants for water as solvent\label{tab:datatable}}\\"
    CALL LongtableHeader
    WRITE(IO_DATA,'(A)') "\endfirsthead"
    WRITE(IO_DATA,'(A)') "\caption{Henry's law constants for water as solvent (\dots continued)}\\"
    CALL LongtableHeader
    WRITE(IO_DATA,'(A)') "\endhead"
    ! WRITE(IO_DATA,'(A)') "\bottomhline"
    WRITE(IO_DATA,'(A)') "\endfoot"

    ! header for f90 output files:
    DO j=1,N_SOLUB
      WRITE(IO_SOLUB+j, '(A)') "! "//TRIM(txtsymbol_solub(j))
    ENDDO
    DO j=1,N_VOLAT
      WRITE(IO_VOLAT+j, '(A)') "! "//TRIM(txtsymbol_volat(j))
    ENDDO

  END SUBROUTINE Initialize

  !---------------------------------------------------------------------------

  SUBROUTINE Middle

    IMPLICIT NONE
    INTEGER :: i, j, n, count
    CHARACTER(MAXNOTES), POINTER :: labels(:)
    CHARACTER(LEN=*), PARAMETER :: types = "LMVRTXCQE?W"
    LOGICAL :: found
    TYPE(HENRY_DATUM), DIMENSION(MAXDATA) :: tmp_data

    ! loop through the entire AllLabels string and
    ! test if all notes for these labels have a text:
    CALL strcrack(AllLabels, '%', labels, n) ! split into individual labels
    DO i = 1, n

      found = .FALSE.
      DO j = 1, idx_n
        IF (TRIM(henry_notes(j)%label)==TRIM(labels(i))) THEN
          found = .TRUE.
          EXIT ! exit do loop because label has been found
        ENDIF
      ENDDO
      IF (.NOT.found) THEN
        PRINT *, "WARNING: This note was never defined: ", TRIM(labels(i))
      ENDIF
    ENDDO
    IF (ASSOCIATED(labels)) DEALLOCATE(labels) ; NULLIFY(labels)

    ! sort data entries by type:
    tmp_data = henry_data
    count = 0
    DO i = 1, LEN(types)
      DO j = 1, idx_h
        IF (tmp_data(j)%type==types(i:i)) THEN
          count = count + 1
          henry_data(count) = tmp_data(j)
          tmp_data(j)%type = "%"
        ENDIF
      ENDDO
    ENDDO
    ! check that no invalid types were there:
    DO j = 1, idx_h
      IF (tmp_data(j)%type/="%") THEN
        PRINT *, "WARNING: Invalid type: ", tmp_data(j)%type
        PRINT *, "         casrn:        ", TRIM(tmp_data(j)%casrn)
        PRINT *, "         ref:          ", TRIM(tmp_data(j)%ref)
      ENDIF
    ENDDO
    IF (count/=idx_h) THEN
      PRINT *, "ERROR: There were invalid types!"
      STOP
    ENDIF

    PRINT *, "Done with the references. Working on species now. Please wait..."

  END SUBROUTINE Middle

  !---------------------------------------------------------------------------

  SUBROUTINE Finalize

    IMPLICIT NONE
    INTEGER :: j

    CALL ShowConv ! show conversion factors between different H definitions

    ! loop over all data entries and confirm that it was used:
    DO j = 1, idx_h
      IF (TRIM(henry_data(j)%casrn) /= "%%%") THEN
        PRINT *, "WARNING: This data entry was never used:"
        PRINT *, "name:  ", TRIM(henry_data(j)%chem)
        PRINT *, "casrn: ", TRIM(henry_data(j)%casrn)
        PRINT *, "ref:   ", TRIM(henry_data(j)%ref)
      ENDIF
    ENDDO

    ! loop over all notes and confirm that it was used:
    DO j = 1, idx_n
      IF (TRIM(henry_notes(j)%label) /= "%%%") THEN
        PRINT *, "WARNING: This note was never used: ", &
          TRIM(henry_notes(j)%label)
      ENDIF
    ENDDO

    WRITE(*,'(A,I6,A,I6,A)') " Usage for AllLabels is ", LEN_TRIM(AllLabels), &
      " out of ", LEN(AllLabels), " characters."
    WRITE(*,'(A,I6,A,I6,A)') " Usage for AllRefs   is ", LEN_TRIM(AllRefs), &
      " out of ", LEN(AllRefs), " characters."
    WRITE(*,'(A,I6,A,I6,A)') " Usage for notes     is ", idx_allnotes, &
      " out of ", MAXNOTECHARS, " characters."
    WRITE(*,'(A,I5,A,I8,A)') " There were ", n_refs, &
      " references."
    WRITE(*,'(A,I5,A,I8,A)') " There were ", idx_s, &
      " species.         (MAXSPECIES   = ", MAXSPECIES,   ")"
    WRITE(*,'(A,I5,A,I8,A)') " There were ", idx_h, &
      " data entries.    (MAXDATA      = ", MAXDATA,      ")"
    WRITE(*,'(A,I5,A,I8,A)') " There were ", idx_n, &
      " notes.           (MAXNOTES     = ", MAXNOTES,     ")"

    WRITE(IO_DEFS,'(A,I0,A)') "\newcommand{\Nrefs}{",    n_refs,    "}"
    WRITE(IO_DEFS,'(A,I0,A)') "\newcommand{\Nspecies}{", idx_s,     "}"
    WRITE(IO_DEFS,'(A,I0,A)') "\newcommand{\Nentries}{", idx_h,     "}"
    WRITE(IO_DEFS,'(A,I0,A)') "\newcommand{\Nnotes}{",   idx_n,     "}"
    WRITE(IO_DEFS,'(A)') "\newcommand{\HunitOL}{\unit{"//TRIM(H_unit_ol)//"}}"
    WRITE(IO_DEFS,'(A)') "\newcommand{\HunitFR}{\unit{"//TRIM(H_unit_fr)//"}}"
    WRITE(IO_DEFS,'(A)') "\newcommand{\Hsymbol}{"//TRIM(H_symbol)//"}"

    IF (warnings) THEN
      PRINT *, "There were warnings. See file tmp_warnings.txt!"
    ELSE
      WRITE(IO_WARN,*) "There were no warnings."
    ENDIF

    WRITE(IO_DATA,'(A)') "\end{longtable}"

    CLOSE(IO_DATA)
    CLOSE(IO_DEFS)
    CLOSE(IO_NOTE)
    CLOSE(IO_WARN)
    CLOSE(IO_ODBH)
    CLOSE(IO_ODBS)
    IF (l_debug) THEN
      CLOSE(IO_DEBG)
    ENDIF
    CLOSE(IO_SPCA)
    CLOSE(IO_SPCB)
    CLOSE(IO_SPCC)
    IF (l_showpics) THEN
      CLOSE(IO_PICS)
    ENDIF

    ! f90 output files:
    DO j=1,N_SOLUB
      CLOSE(IO_SOLUB+j)
    ENDDO
    DO j=1,N_VOLAT
      CLOSE(IO_VOLAT+j)
    ENDDO

  END SUBROUTINE Finalize

  !---------------------------------------------------------------------------

  ELEMENTAL REAL FUNCTION round(x, k)
    IMPLICIT NONE
    REAL, INTENT(IN)    :: x
    INTEGER, INTENT(IN) :: k
    REAL                :: factor
    ! round x to k digits
    IF (ABS(x) > TINY(0.)) THEN
      factor = 10.**(FLOOR(LOG10(ABS(x)))+1.-k)
      round = ANINT(x/factor)*factor
    ELSE
      round = 0.
    ENDIF
  END FUNCTION round

  !---------------------------------------------------------------------------

  ELEMENTAL REAL FUNCTION psat (T)
    REAL, INTENT(IN) :: T
    psat = 6.982616467E+5 &
      - 1.888612677E+4*T    + 2.132971127E+2*T**2 &
      - 1.288405237E+0*T**3 + 4.393186046E-3*T**4 &
      - 8.023554873E-6*T**5 + 6.136820929E-9*T**6
  END FUNCTION psat

  !---------------------------------------------------------------------------

  ELEMENTAL REAL FUNCTION LtoH (L, T)
    ! according to [379], "l" gives the volume of the gas (in ml (at
    ! 273.15 K and 101325 Pa)) dissolved in 1 ml of water when the pressure
    ! of the gas plus that of the water vapor is 101325 Pa.
    REAL, INTENT(IN) :: L, T
    REAL             :: pcorr ! subtract p(water) from p(total)
    pcorr= 1.-psat(T)/p0
    LtoH = L * alpha_TO_HcpSI / pcorr
  END FUNCTION LtoH

  !---------------------------------------------------------------------------

  ELEMENTAL REAL FUNCTION Hcc_TO_HcpSI (Hcc, temp)
    IMPLICIT NONE
    REAL, INTENT(IN) :: Hcc, temp
    Hcc_TO_HcpSI = Hcc / (Rgas*temp)
  END FUNCTION Hcc_TO_HcpSI

  !---------------------------------------------------------------------------

  ELEMENTAL REAL FUNCTION KHcc_TO_HcpSI (KHcc, temp)
    IMPLICIT NONE
    REAL, INTENT(IN) :: KHcc, temp
    KHcc_TO_HcpSI = 1. / (KHcc*Rgas*temp)
  END FUNCTION KHcc_TO_HcpSI

  !---------------------------------------------------------------------------

  ELEMENTAL LOGICAL FUNCTION casrn_invalid (casrn)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: casrn
    ! local:
    INTEGER :: i, j, n, count
    CHARACTER(8) :: casrn_part1
    INTEGER :: checksum

    ! check the CAS registry number:
    casrn_invalid = .FALSE.
    IF (TRIM(casrn)=="") THEN
      casrn_invalid = .TRUE.
      RETURN
    ENDIF
    IF (VERIFY(casrn,'0123456789-')/=0) casrn_invalid = .TRUE.
    ! if casrn starts with "_", then the CAS is unknown:
    IF (casrn(1:1)=="_")                casrn_invalid = .TRUE.
    IF (casrn_invalid) THEN
      RETURN
    ELSE
      i = LEN(casrn)
      casrn_part1 = casrn(1:i-5)//casrn(i-3:i-2)
      READ(casrn(i:i),*) checksum
      count = 0
      n = LEN_TRIM(casrn_part1)
      DO i = 1, n
        READ(casrn_part1(n-i+1:n-i+1),*) j
        count = count + j*i
      ENDDO
      IF (checksum /= MOD(count,10)) THEN
        casrn_invalid = .TRUE.
      ENDIF
    ENDIF

  END FUNCTION casrn_invalid

  !---------------------------------------------------------------------------

  ELEMENTAL LOGICAL FUNCTION unread_bib (biblabel)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: biblabel
    ! if biblabel contains a non-number it is from unread.bib:
    unread_bib = (VERIFY(biblabel,'0123456789')/=0)
  END FUNCTION unread_bib

  !---------------------------------------------------------------------------

  PURE REAL FUNCTION avg(xdata)
    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(IN) :: xdata
    avg = SUM(xdata) / SIZE(xdata)
  END FUNCTION avg

  !---------------------------------------------------------------------------

  REAL FUNCTION H_from_dG(dG)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dG ! Delta G
    H_from_dG = EXP((-dG)/(Rgas*T0))
    ! depending on the standard state on which Delta G is based, the
    ! unit is now [mol/(kg*0.1MPa)], [M/atm], or something similar.
  END FUNCTION H_from_dG

  !---------------------------------------------------------------------------

  SUBROUTINE consistency_check(x1, x2, notetext, verbose)

    REAL,              INTENT(IN) :: x1, x2
    CHARACTER(LEN=*),  INTENT(IN) :: notetext
    LOGICAL, OPTIONAL, INTENT(IN) :: verbose
    REAL :: reldiff, maxdiff
    maxdiff = 0.05 ! 5% difference is okay
    reldiff = ABS(x1-x2)/MAX(x1,x2)
    IF (PRESENT(verbose)) THEN
      IF (verbose) THEN
        WRITE(*,'(3A,F6.2,A,1PG10.4,A,1PG10.4,2A)') "ref", TRIM(ref), ": ", &
          100.*reldiff, " % diff ( ", x1, " vs ", x2, ") for ", TRIM(chem)
      ENDIF
    ENDIF
    IF (reldiff>maxdiff) THEN
      CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-consicheck", &
        notetext//" of "//TRIM(citet())//" are inconsistent, with "// &
        TRIM(str(NINT(100.*reldiff))) // "~\% difference.")
    ENDIF

  END SUBROUTINE consistency_check

  !---------------------------------------------------------------------------

  SUBROUTINE LinReg (xdata, ydata, intercept, slope) ! linear regression

    IMPLICIT NONE
    REAL, DIMENSION(:), INTENT(IN) :: xdata, ydata
    REAL, INTENT(OUT)              :: intercept, slope
    INTEGER                        :: ndata
    REAL                           :: sumx, sumy, sumxx, sumxy, sumyy

    ndata = SIZE(xdata)
    sumx  = SUM(xdata)
    sumy  = SUM(ydata)
    sumxx = SUM(xdata**2)
    sumxy = SUM(xdata*ydata)
    sumyy = SUM(ydata**2)

    ! ydata = intercept + slope * xdata
    slope = (sumxy-sumx*sumy/ndata) / (sumxx-sumx*sumx/ndata)
    intercept = sumy/ndata - slope*sumx/ndata
    r2 = (ndata*sumxy-sumx*sumy)**2 &
      / ((ndata*sumxx-sumx*sumx) * (ndata*sumyy-sumy*sumy))

  END SUBROUTINE LinReg

  !---------------------------------------------------------------------------

  SUBROUTINE HTdep (temp, Harray, Hominus, mindHR, gnufilesuffix)

    ! calculate the T-dependence of H using linear regression
    IMPLICIT NONE
    REAL, DIMENSION(:),         INTENT(IN)  :: temp, Harray
    REAL,                       INTENT(OUT) :: Hominus, mindHR
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)  :: gnufilesuffix

    REAL, DIMENSION(SIZE(temp)) :: Tinv, lnH
    REAL                        :: A
    CHARACTER(STRLEN_VLONG)     :: filename
    CHARACTER(4)                :: ref0
    INTEGER                     :: i, Tmin, Tmax

    ! d(lnH)/d(1/T) = constant
    Tinv = 1./temp
    lnH = LOG(Harray)
    CALL LinReg(Tinv, lnH, A, mindHR)
    Hominus = EXP(A+mindHR/T0)

    ! plot the regression with gnuplot:
    ! add leading 0s to ref:
    ref0 = "0000"
    ref0(5-LEN_TRIM(ref):) = TRIM(ref)
    IF (PRESENT(gnufilesuffix)) THEN
      filename = "ref"//ref0//"-"//TRIM(casrn)//"-"//TRIM(gnufilesuffix)//".gnu"
    ELSE
      filename = "ref"//ref0//"-"//TRIM(casrn)//".gnu"
    ENDIF

!!$    ! check if gnu file exists already:
!!$    INQUIRE(file="output/gnuplot/"//TRIM(filename), exist=lex)
!!$    IF (lex) THEN
!!$      PRINT *, "WARNING: gnu file exists already ", TRIM(filename)
!!$    ENDIF

    OPEN (IO_GNUP,FILE="output/gnuplot/"//TRIM(filename), STATUS="REPLACE")
    ! (status=new because file must not exist yet)
    WRITE (IO_GNUP,'(A)') '# load "'//TRIM(filename)//'"'
    WRITE (IO_GNUP,'(A)') '# chem = "'//TRIM(chem)//'"'
    WRITE (IO_GNUP,'(A)')
    WRITE (IO_GNUP,'(A)') 'set terminal postscript eps color'
    WRITE (IO_GNUP,'(A)') 'set title "ref = '//TRIM(ref)//'; chem = ' &
      //TRIM(chem)//'; casrn = '//TRIM(casrn)//'"'
    WRITE (IO_GNUP,'(A)') 'set xlabel "temperature [K]"'
    WRITE (IO_GNUP,'(A)') 'set ylabel "H [mol*m-3*Pa-1]"'
    WRITE (IO_GNUP,'(A)') 'set dummy T'
    WRITE (IO_GNUP,'(A)')
    WRITE (IO_GNUP,'(A)') '# regresion from table data:'
    WRITE (IO_GNUP,'(A,G15.7,A,G15.7,A,G11.3,A)') &
      'H(T) = ', Hominus, ' * exp(', -mindHR, '*(1/', T0, '-1/T))'
    WRITE (IO_GNUP,'(A)')
    DO i = 1, SIZE(temp)
      WRITE (IO_GNUP,'(A,G15.7,A,G15.7,A)') &
        'set label "" at ',temp(i),', ',Harray(i), ' point'
    ENDDO
    WRITE (IO_GNUP,'(A,G15.7,A,G15.7,A)') &
      'set label "" at ',T0,', ',Hominus, ' point ps 2 pt 6'
    WRITE (IO_GNUP,'(A)')
    ! calculate x-range for plot:
    Tmin = FLOOR(MINVAL(temp)/10.) * 10
    Tmax = CEILING(MAXVAL(temp)/10.) * 10
    WRITE (IO_GNUP,'(A,I0,A,I0,A)') &
      'plot [', Tmin, ':', Tmax, '] H(T)'
    CLOSE (IO_GNUP)

  END SUBROUTINE HTdep

  !---------------------------------------------------------------------------

  SUBROUTINE SettypeX (xref_)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: xref_

    xref = xref_ ! make xref global
    type = "X"
    ! show in footnote where I found the data:
    CALL MakeNote("from"//TRIM(ref), &
      "Value given here as quoted by \citet{"//TRIM(ref)//"}.")

  END SUBROUTINE SettypeX

  !---------------------------------------------------------------------------

  SUBROUTINE MakeNote (label, notetext)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)           :: label
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: notetext
    ! local:
    INTEGER :: j
    CHARACTER(1600) :: ThisNote

    IF (TRIM(seenote) == "") THEN
      seenote = "\sn{"//label//"}"
    ELSE
      seenote = TRIM(seenote)//", \sn{"//label//"}" ! separate \sn{},\sn{},...
    ENDIF

    IF (PRESENT(notetext)) THEN
      ThisNote = TRIM(notetext)
    ELSE
      ThisNote = ""
    ENDIF
    IF (INDEX(AllLabels, "%"//label//"%")==0) THEN
      ! this is a note with a new label
      IF (PRESENT(notetext)) THEN
        idx_n = idx_n + 1
        IF (idx_n > MAXNOTES) THEN
          PRINT *, "ERROR: Too many notes. Increase MAXNOTES!"
          STOP
        ENDIF
        henry_notes(idx_n)%label = TRIM(label)
        henry_notes(idx_n)%start = idx_allnotes
        idx_allnotes = idx_allnotes + LEN_TRIM(ThisNote)
        henry_notes(idx_n)%end = idx_allnotes - 1
        AllNotes = TRIM(AllNotes)//TRIM(ThisNote)
      ELSE
        CALL PrintWarning("LABEL "//label//" not yet defined")
      ENDIF
      AllLabels = TRIM(AllLabels)//label//"%"
    ELSE
      ! this note has a label that has already been used
      IF (PRESENT(notetext)) THEN
        ! loop through the entire henry_notes array and confirm that
        ! this note had the same text when it was defined first:
        DO j = 1, idx_n-1
          IF (TRIM(henry_notes(j)%label)==label) THEN
            IF (TRIM(ThisNote) == & ! test if newdef==olddef
              TRIM(allnotes(henry_notes(j)%start:henry_notes(j)%end))) THEN
              EXIT ! note is okay; no need to continue DO loop anymore
            ELSE
              CALL PrintWarning('different text for MakeNote("'//label//'")')
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDIF

  END SUBROUTINE MakeNote

  !---------------------------------------------------------------------------

  SUBROUTINE MakeNoteOtherTemp (T)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: T
    CALL MakeNote("at"//T//"K", "Value at $T$~= "//T//"~\unit{K}.")

  END SUBROUTINE MakeNoteOtherTemp

  !---------------------------------------------------------------------------

  SUBROUTINE PrintGeneralNotes

    ! some general notes that apply to more than one paper
    IMPLICIT NONE

    CALL MakeNote("highTextrapol", &
      "Measured at high temperature and extrapolated to "// &
        "$T^{\ominus}$~= 298.15\,\unit{K}.")

    CALL MakeNote("supplement", "Data taken from the supplement.")

    CALL MakeNote("atroomT", "Value at ``room temperature''.")

    CALL MakeNote("whichref", &
      "Several references are given in the list of Henry's "// &
      "law constants but not assigned to specific species.")

    CALL MakeNote("morethanoneref", &
      "More than one reference is given as the source of this value.")

    CALL MakeNote("seawater", &
      "Solubility in sea water.")

    CALL MakeNote("alsosalt", &
      "Values for salt solutions are also available from this reference.")

    CALL MakeNote("infty", &
      "Fast, irreversible hydrolysis is assumed, which is equivalent to an "// &
      "infinite effective Henry's law constant.")

    CALL MakeNote("RCHOdiol", &
      "Effective value that takes into account the "// &
      "hydration of the aldehyde: "// &
      "$$\H = ([\chem{RCHO}]+[\chem{RCH(OH)_2}])/p(\chem{RCHO})$$")

    CALL MakeNote("HCHOdiol", &
      "Effective value that takes into account the "// &
      "hydration of \chem{HCHO}: "// &
      "$$\H = ([\chem{HCHO}]+[\chem{CH_2(OH)_2}])/p(\chem{HCHO})$$")

    CALL MakeNote("assumedMatm", &
      "It is assumed here that the thermodynamic data "// &
      "refers to the units~\unit{[mol~dm^{-3}]} "// &
      "and~\unit{[atm]} as standard states.")

    CALL MakeNote("DeltaHsolv", &
      "Calculated from the solvation enthalpy, using "// &
      "Eq.~(\ref{eq:vantHoff}).")

    CALL MakeNote("cdep", &
      "It was found that $\H$ changes with "// &
      "the concentration of the solution.")

    CALL MakeNote("481HCHO", &
      "\citet{481} (and also \citet{486}) measured the solubility of "// &
      "\chem{HCHO} at very high concentrations around 5 to 15~\unit{M}. "// &
      "Their value of $\H$ increases with \chem{HCHO} "// &
      "concentration. \citet{31}, \citet{443}, and \citet{34} all use "// &
      "these solubility data but do not specify how they extrapolated to "// &
      "lower concentrations. Since the concentration range is far "// &
      "from typical values in atmospheric chemistry, the value is not "// &
      "reproduced here.")

    seenote = "" ! reset to empty string so that next CALL Output won't use it

  END SUBROUTINE PrintGeneralNotes

  !---------------------------------------------------------------------------

  SUBROUTINE MakeNote_range(H_min, H_max)
    IMPLICIT NONE
    REAL, INTENT(IN) :: H_min, H_max
    CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn)//"-range", &
      TRIM(citet())//" give a range of "// &
      TRIM(H_range(H_min, H_max))//".")
  END SUBROUTINE MakeNote_range

  !---------------------------------------------------------------------------

  SUBROUTINE Species (iupac, formula, trivial, casrn, inchikey)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: iupac
    CHARACTER(LEN=*), INTENT(IN) :: formula
    CHARACTER(LEN=*), INTENT(IN) :: trivial
    CHARACTER(LEN=*), INTENT(IN) :: casrn
    CHARACTER(LEN=*), INTENT(IN) :: inchikey

    ! local:
    INTEGER :: i, j, n, count
    CHARACTER(11) :: casrn_sort       ! casrn with leading 0s
    CHARACTER(11) :: casrn_underscore ! casrn with underscores
    CHARACTER(10) :: H_string_        ! local string
    CHARACTER(5)  :: mindHR_string_   ! local string
    CHARACTER(3)  :: limit_string_    ! local string
    CHARACTER(STRLEN_VLONG) :: trivial_, chem_check
    CHARACTER(STRLEN_ULONG) :: description_str(6)

    IF ((iupac=="").OR.(formula=="")) THEN
      PRINT *, "WARNING: Incomplete species description:"
      PRINT *, "iupac name = ", iupac
      PRINT *, "formula    = ", formula
    ENDIF

    ! if casrn starts with "_", then the CAS is unknown:
    IF ((casrn_invalid(casrn)).AND.(casrn(1:1)/="_")) &
      PRINT *, "WARNING: Invalid CASRN ("//casrn//") for "//iupac

    idx_s = idx_s + 1       ! count this species
    allcasrn(idx_s) = casrn ! save CASRN in array
    DO i = 1, idx_s-1
      IF (TRIM(allcasrn(i)) == casrn) THEN
        PRINT *, "ERROR: The CASRN "//casrn//" has already been used!"
      ENDIF
    ENDDO
    allinchikey(idx_s) = inchikey ! save inchikey in array
    DO i = 1, idx_s-1
      IF ((TRIM(allinchikey(i))==inchikey).AND.(inchikey/="inchikey")) THEN
        PRINT *, "ERROR: The InChIKey "//inchikey// &
          " has already been used! (CASRN: "//casrn//")"
      ENDIF
    ENDDO

    ! write tmp_species_chem*.txt:
    WRITE(IO_SPCA,'(A)') '    chem = "'//iupac//'"'// &
      ' ; casrn = "'//casrn//'"'//              &
      ' ! '//formula//' '//trivial
    WRITE(IO_SPCB,'(A)') '"'//iupac//'"'// &
      ', "'//casrn//'"'//        &
      ',    ! '//formula//' '//trivial
    WRITE(IO_SPCC,'(A)') '    CALL CalcH("'//iupac// &
      '",                           "'// &
      casrn//'",     ) ! '//formula//' '//trivial

    ! write database file (TAB-separated):
    WRITE(IO_ODBS,'(A)') '"'//iupac//'"	"'// &
      formula//'"	"'//trivial// &
      '"	"'//casrn//'"	"'//inchikey//'"'

    ! write a line to get picture from casrn:
    IF (l_showpics) THEN
      WRITE(IO_PICS,'(A)') 'getpic "'//casrn//'"'
    ENDIF

    ! start new species in f90 output files:
    DO j=1,N_SOLUB
      WRITE(IO_SOLUB+j,'(A)') ""
      WRITE(IO_SOLUB+j,'(A)') "! species: "//iupac
      WRITE(IO_SOLUB+j,'(A)') "! formula: "//formula
      WRITE(IO_SOLUB+j,'(A)') "! trivial: "//trivial
      WRITE(IO_SOLUB+j,'(A)') "! casrn:   "//casrn
      WRITE(IO_SOLUB+j,'(A)') "! inchikey: "//inchikey
    ENDDO
    DO j=1,N_VOLAT
      WRITE(IO_VOLAT+j,'(A)') ""
      WRITE(IO_VOLAT+j,'(A)') "! species: "//iupac
      WRITE(IO_VOLAT+j,'(A)') "! formula: "//formula
      WRITE(IO_VOLAT+j,'(A)') "! trivial: "//trivial
      WRITE(IO_VOLAT+j,'(A)') "! casrn:   "//casrn
      WRITE(IO_VOLAT+j,'(A)') "! inchikey: "//inchikey
    ENDDO

    ! write \index commands for makeindex to LaTeX file?
    IF (l_makeindex) THEN
      ! 1) index for iupac name:
      WRITE(IO_DATA,'(A)') "\index{"//TRIM(sortlabel(iupac)) &
        //"@"//iupac//"}%"
      ! 2) index for trivial name:
      IF (trivial/="") THEN
        ! split multiple trivial names into individual names:
        trivial_ = trivial ! because trivial cannot be modified
        i = 1
        DO
          ! find name separator:
          n = INDEX(trivial_, "; ")
          IF (n==0) n = LEN_TRIM(trivial_)+1
          WRITE(IO_DATA,'(A)') "\index{"//TRIM(sortlabel(trivial_(i:n-1))) &
            //"@"//TRIM(trivial_(i:n-1))//"}%"
          IF (n==LEN_TRIM(trivial_)+1) EXIT ! exit DO loop, no more names
          i = n+2
          trivial_(n:n+1) = "%%" ! remove separator for next search
        ENDDO
      ENDIF
      ! 3) index for CASRN:
      ! if casrn starts with "_", then the CAS is unknown:
      IF (casrn(1:1)/="_") THEN
        ! add leading 0s to casrn and use it for sorting index entries:
        casrn_sort = "00000000000"
        casrn_sort(12-LEN_TRIM(casrn):) = casrn
        ! "###" ensures that CASRNs are listed before anything else:
        WRITE(IO_DATA,'(A)') &
          "\index{###"//casrn_sort//"@CASRN = ["//casrn//"]}%"
      ENDIF
    ENDIF

    ! define LaTeX description strings for new species:
    description_str(:) = ""
    i = 1
    description_str(i) = "{"//iupac//"}"
    IF (formula/="") THEN
      i = i + 1
      description_str(i) = "\chem{"//formula//"}"
    ENDIF
    IF (trivial/="") THEN
      i = i + 1
      description_str(i) = "("//trivial//")"
    ENDIF
    IF (.NOT.casrn_invalid(casrn)) THEN
      i = i + 1
      description_str(i) = "\href{http://webbook.nist.gov/cgi/cbook.cgi?ID="// &
        casrn//"}{["//casrn//"]}"
    ENDIF
    !IF (inchikey/="inchikey") THEN
    !  i = i + 1
    !  description_str(i) = inchikey
    !ENDIF
    IF (l_showpics) THEN
      i = i + 1
      description_str(i) = "\IfFileExists{pics/"//casrn// &
        ".png}{\includegraphics[width=4cm]{pics/"//casrn// &
        ".png}}{\fbox{png missing}}"
    ENDIF

    !-------------------------------------------------------------------------

    ! loop through the entire henry_data array and use all entries
    ! for the current species:
    count = 0
    DO i = 1, idx_h
      IF (TRIM(henry_data(i)%casrn) == casrn) THEN
        henry_data(i)%casrn = "%%%" ! avoid writing this data entry again
        count = count + 1
        H_string_      = TRIM(H_string(henry_data(i)%Hominus))
        mindHR_string_ = TRIM(mindHR_string(henry_data(i)%mindHR))
        seenote        = henry_data(i)%seenote
        ! define chem_check if name is not exactly the same as iupac name:
        IF (iupac==TRIM(henry_data(i)%chem)) THEN
          chem_check = ""
        ELSE
          chem_check = " % "//TRIM(henry_data(i)%chem)
        ENDIF
        limit_string_ = ""
        IF (henry_data(i)%limit/="") THEN
          SELECT CASE(henry_data(i)%limit)
          CASE ("i")
            H_string_ = "$\infty$"
          CASE ("<")
            limit_string_ = "$"//lessthan//"$"
          CASE (">")
            limit_string_ = "$"//morethan//"$"
          CASE DEFAULT
            PRINT *, "ERROR: Unknown limit character."
            STOP
          END SELECT
        ENDIF

        ! print one description line about the species:
        IF (count<=6) THEN
          IF (TRIM(description_str(count))/="") THEN
            WRITE(IO_DATA,'(A)') TRIM(description_str(count))//"%"
          ENDIF
        ENDIF

        ! write data entry to henry_data.tex:
        WRITE(IO_DATA,'(A)',ADVANCE='NO') &
          " & "//TRIM(limit_string_)//H_string_//" & "//mindHR_string_
        IF (TRIM(henry_data(i)%ref)=="thiswork") THEN
          WRITE(IO_DATA,'(A)',ADVANCE='NO') " & this work"
        ELSE
          WRITE(IO_DATA,'(A)',ADVANCE='NO') &
            " & \citet{"//TRIM(henry_data(i)%ref)//"}"
        ENDIF
        WRITE(IO_DATA,'(A)',ADVANCE='NO') &
          " & "//TRIM(henry_data(i)%type)//" & "//TRIM(seenote)
        IF (count<=6) THEN
          WRITE(IO_DATA,'(A)',ADVANCE='NO') "\\*" ! no page break here
        ELSE
          WRITE(IO_DATA,'(A)',ADVANCE='NO') "\\"
        ENDIF
        WRITE(IO_DATA,'(A)') TRIM(chem_check)

        ! write data entry to f90 output files:
        casrn_underscore = casrn
        IF (casrn(1:1)/="_") THEN
          ! add leading underscores to casrn:
          casrn_underscore = "___________"
          casrn_underscore(12-LEN_TRIM(casrn):) = casrn
          casrn_underscore(10:10) = '_' ! replace - by _
          casrn_underscore(7:7)   = '_' ! replace - by _
        ENDIF
        IF (ABS(henry_data(i)%Hominus-DUMMY) > TINY(0.)) THEN
          ! write definition of H:
          DO j=1,N_SOLUB
            ! high-prec: WRITE(IO_SOLUB+j,'(A,ES15.8)',ADVANCE='NO') &
            WRITE(IO_SOLUB+j,'(A,ES8.1)',ADVANCE='NO') &
              TRIM(txtsymbol_solub(j))//"_"//TRIM(casrn_underscore)//" = ", &
              henry_data(i)%Hominus/conv_solub(j)
          ENDDO
          DO j=1,N_VOLAT
            ! high-prec: WRITE(IO_VOLAT+j,'(A,ES15.8)',ADVANCE='NO') &
            WRITE(IO_VOLAT+j,'(A,ES8.1)',ADVANCE='NO') &
              TRIM(txtsymbol_volat(j))//"_"//TRIM(casrn_underscore)//" = ", &
              conv_volat(j)/henry_data(i)%Hominus
          ENDDO
          ! write temperature-dependence (or just spaces, if not available):
          IF (ABS(henry_data(i)%mindHR-DUMMY) > TINY(0.)) THEN
            DO j=1,N_SOLUB
              ! high-prec: WRITE(IO_SOLUB+j,'(A,F16.8,A)',ADVANCE='NO') &
              ! high-prec:   " * EXP(", henry_data(i)%mindHR+mindHR_offset(j), &
              WRITE(IO_SOLUB+j,'(A,F8.0,A)',ADVANCE='NO') &
                " * EXP(", round(henry_data(i)%mindHR+mindHR_offset(j),2), &
                "*(1./298.15-1./T))"
            ENDDO
            DO j=1,N_VOLAT
              ! high-prec: WRITE(IO_VOLAT+j,'(A,F16.8,A)',ADVANCE='NO') &
              ! high-prec:   " * EXP(", -(henry_data(i)%mindHR+mindHR_inv_offset(j)), &
              WRITE(IO_VOLAT+j,'(A,F8.0,A)',ADVANCE='NO') &
                " * EXP(", -round(henry_data(i)%mindHR+mindHR_inv_offset(j),2), &
                "*(1./298.15-1./T))"
            ENDDO
          ELSE
            DO j=1,N_SOLUB
              WRITE(IO_SOLUB+j,'(A33)',ADVANCE='NO') ""
            ENDDO
            DO j=1,N_VOLAT
              WRITE(IO_VOLAT+j,'(A33)',ADVANCE='NO') ""
            ENDDO
          ENDIF
          ! write f90 comment with reference and unit:
          DO j=1,N_SOLUB
            WRITE(IO_SOLUB+j,'(A)') &
              " ! ["// TRIM(unit_ol(j))//"] type: "// &
              TRIM(henry_data(i)%type)//", ref: "//TRIM(henry_data(i)%ref)
          ENDDO
          DO j=1,N_VOLAT
            WRITE(IO_VOLAT+j,'(A)') &
              " ! ["// TRIM(unit_ol_inv(j))//"] type: "// &
              TRIM(henry_data(i)%type)//", ref: "//TRIM(henry_data(i)%ref)
          ENDDO
        ENDIF

        ! split seenote into individual labels:
        DO
          ! find first "}", i.e. the end of the first label, in seenote:
          n = INDEX(TRIM(seenote), "}")
          ! if there is a note, then:
          ! seenote(1:4)   = "\sn{"
          ! seenote(5:n-1) = label
          ! seenote(n:n)   = "}"
          IF (n==0) EXIT ! exit DO loop, if there are no more labels
          ! loop through the entire henry_notes array and search for
          ! the current label:
          DO j = 1, idx_n
            ! INDEX tests if "label" is included in "seenote":
            IF (seenote(5:n-1) == TRIM(henry_notes(j)%label)) THEN
              WRITE(IO_NOTE,'(A)') &
                "\notetext{"//TRIM(henry_notes(j)%label)//"}"// &
                TRIM(allnotes(henry_notes(j)%start:henry_notes(j)%end)) &
                //"\\[1mm]"
              henry_notes(j)%label = "%%%" ! avoid writing this note again
            ENDIF
          ENDDO
          ! remove the first label in seenote(1:n) and also
          ! the separator ", " in seenote(n+1:n+2):
          seenote = seenote(n+3:)
        ENDDO
      ENDIF
    ENDDO

    ! if necessary, print remaining description lines about the species:
    IF (count<6) THEN
      DO i = count+1, 6
        IF (TRIM(description_str(i))/="") THEN
          WRITE(IO_DATA,'(A)') TRIM(description_str(i))//" & & & & & \\*"
        ENDIF
      ENDDO
    ENDIF

    !-------------------------------------------------------------------------

    IF (count==0) THEN
      PRINT *, "WARNING: No entries for species: ", iupac, " [", casrn, "]"
    !ELSE
    !  WRITE(*,'(I5,2A)') count, " entries for species: ", iupac
    ENDIF

    ! hline in LaTeX file before next species starts:
    WRITE(IO_DATA,'(A)') &
      "\middlehline\pagebreak[3] %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"

  CONTAINS

    CHARACTER(LEN=STRLEN_VLONG) FUNCTION sortlabel(name)

      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: name
      ! local:
      INTEGER :: i, j
      CHARACTER :: curchar ! current character

      sortlabel = name
      DO ; i = INDEX(sortlabel,"\it")
        IF (i/=0) THEN ; sortlabel(i:i+2) = "" ; ELSE ; EXIT ; ENDIF
      ENDDO
      DO ; i = INDEX(sortlabel,"\alpha")
        IF (i/=0) THEN ; sortlabel(i:i+5) = " alpha" ; ELSE ; EXIT ; ENDIF
      ENDDO
      DO ; i = INDEX(sortlabel,"\beta")
        IF (i/=0) THEN ; sortlabel(i:i+4) = " beta" ; ELSE ; EXIT ; ENDIF
      ENDDO
      DO ; i = INDEX(sortlabel,"\gamma")
        IF (i/=0) THEN ; sortlabel(i:i+5) = " gamma" ; ELSE ; EXIT ; ENDIF
      ENDDO
      DO ; i = INDEX(sortlabel,"$")
        IF (i/=0) THEN ; sortlabel(i:i) = "" ; ELSE ; EXIT ; ENDIF
      ENDDO
      i = 1
      DO j = 1, LEN_TRIM(sortlabel)
        curchar = sortlabel(j:j)
        IF (INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", &
          curchar)/=0) THEN ! current character is a letter
          IF (j>i) sortlabel(i+1:j) = sortlabel(i:j-1) ! shift to the right
          sortlabel(i:i) = curchar
          i = i + 1
        ENDIF
      ENDDO

    END FUNCTION sortlabel

  END SUBROUTINE Species

  !---------------------------------------------------------------------------

  SUBROUTINE Speciesgroup (level, groupname)

    IMPLICIT NONE
    INTEGER,          INTENT(IN) :: level
    CHARACTER(LEN=*), INTENT(IN) :: groupname

    WRITE(IO_DATA,'(A)') "\pagebreak[3]"
    WRITE(IO_DATA,'(A)') "\noalign{\vspace{2mm}}"
    SELECT CASE(level)
    CASE (1)
      WRITE(IO_DATA,'(A)') &
        "\multicolumn{6}{c}{\addcontentsline{spg}{section}{"// &
        TRIM(groupname)//"}\Large\bf "//TRIM(groupname)//"}\\*"
    CASE (2)
      WRITE(IO_DATA,'(A)') &
        "\multicolumn{6}{c}{\addcontentsline{spg}{subsection}{"// &
        TRIM(groupname)//"}\large\bf "//TRIM(groupname)//"}\\*"
    CASE DEFAULT
      PRINT *, "ERROR: Unknown level for Speciesgroup!"
      STOP
    END SELECT
    WRITE(IO_DATA,'(A)') "\middlehline %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"

  END SUBROUTINE Speciesgroup

  !---------------------------------------------------------------------------

  SUBROUTINE CleanUp

    IMPLICIT NONE

    ! reset several strings so that next CALL Output won't use them:
    ref     = ""
    type    = ""
    chem    = ""
    seenote = ""

  END SUBROUTINE CleanUp

  !---------------------------------------------------------------------------

  SUBROUTINE Output (Hominus, mindHR, r2_, limit)

    IMPLICIT NONE
    REAL,      INTENT(IN)           :: Hominus ! HcpSI [mol/(m3*Pa)]
    REAL,      INTENT(IN), OPTIONAL :: mindHR, r2_
    CHARACTER, INTENT(IN), OPTIONAL :: limit
    ! local:
    CHARACTER(STRLEN_VLONG)      :: ref_ ! local value for ref
    CHARACTER(STRLEN_VLONG)      :: mindHR_

    IF (TRIM(ref)=="") THEN
      PRINT *, "ERROR: Empty ref for "//TRIM(casrn)
      STOP
    ENDIF

    IF (TRIM(type)=="") THEN
      PRINT *, "ERROR: Empty type for "//TRIM(casrn)
      STOP
    ENDIF

    IF (TRIM(chem)=="") THEN
      PRINT *, "ERROR: Empty chem for "//TRIM(casrn)
      STOP
    ENDIF

    IF (casrn_invalid(TRIM(casrn))) &
      CALL PrintWarning("Invalid CASRN: "//TRIM(casrn))

    IF (INDEX(AllRefs, "%"//TRIM(ref)//"%")==0) THEN
      ! this is a new reference
      n_refs = n_refs + 1
      AllRefs = TRIM(AllRefs)//TRIM(ref)//"%"
    ENDIF

    idx_h = idx_h + 1
    IF (idx_h > MAXDATA) THEN
      PRINT *, "ERROR: Too many data entries. Increase MAXDATA!"
      STOP
    ENDIF
    ! store new data in struct
    henry_data(idx_h)%chem    = chem
    henry_data(idx_h)%casrn   = casrn
    henry_data(idx_h)%Hominus = Hominus
    IF (PRESENT(mindHR)) THEN
      henry_data(idx_h)%mindHR  = mindHR
    ELSE
      henry_data(idx_h)%mindHR  = DUMMY
    ENDIF
    IF (TRIM(type)=="X") THEN
      henry_data(idx_h)%ref     = xref
    ELSE
      henry_data(idx_h)%ref     = ref
    ENDIF
    henry_data(idx_h)%type    = type
    henry_data(idx_h)%seenote = seenote
    IF (PRESENT(limit)) THEN
      henry_data(idx_h)%limit = limit
    ELSE
      henry_data(idx_h)%limit = ""
    ENDIF

    WRITE(IO_ODBH,'(A)',ADVANCE='NO') '"'//TRIM(casrn)//'"	'
    IF (TRIM(casrn)=="") THEN
      CALL PrintWarning("empty casrn")
    ENDIF
    ! Hominus
    IF (ABS(Hominus-DUMMY) > TINY(0.)) THEN
      WRITE(IO_ODBH,"(A,ES8.1,A)",ADVANCE='NO') " ", Hominus, "	"
      WRITE(IO_ODBH,"(A,ES8.1,A)",ADVANCE='NO') " ", Hominus/Hcp_TO_HcpSI, "	"
      IF (Hominus<0.) CALL PrintWarning("negative H: ",Hominus)
    ELSE
      WRITE(IO_ODBH,'(A)',ADVANCE='NO') "	"
      WRITE(IO_ODBH,'(A)',ADVANCE='NO') "	"
      IF ((seenote=="").AND.(.NOT.PRESENT(mindHR))) &
        CALL PrintWarning("You must add a note here")
    ENDIF
    ! mindHR = - delta H / R
    IF (PRESENT(mindHR)) THEN
      WRITE(mindHR_,"(ES8.1,A)") mindHR, " K"
      IF (mindHR<0.)    CALL PrintWarning("negative -dH/R: ",   mindHR)
      IF (mindHR>1.5E4) CALL PrintWarning("very large -dH/R: ", mindHR)
      WRITE(IO_ODBH,"(ES8.1,A)",ADVANCE='NO') mindHR, "	"
    ELSE
      WRITE(mindHR_,"(A)") "unknown"
      WRITE(IO_ODBH,"(A)",ADVANCE='NO') "	"
    ENDIF
    ! r2
    IF (PRESENT(r2_)) THEN ! show correlation coefficient
      IF (r2_<0.9) CALL PrintWarning("low r2: ",r2_)
    ENDIF
    ! ref
    IF (TRIM(type)=="X") THEN
      ref_ = xref
    ELSE
      ref_ = ref
    ENDIF
    WRITE(IO_ODBH,"(A)",ADVANCE='NO') '"'//TRIM(ref_)//'"	'
    ! type
    WRITE(IO_ODBH,"(A)",ADVANCE='NO') '"'//TRIM(type)//'"'
    ! seenote
    IF (seenote /= "") THEN
    ENDIF
    seenote = "" ! reset to empty string so that next CALL Output won't use it
    WRITE(IO_ODBH,"(A)") ""

    ! debugging output only:
    IF (l_debug) THEN
      WRITE(IO_DEBG,"(A6,5A,ES8.1,2A)") TRIM(ref_), " ", &
        chem(1:20), " ", casrn(1:11), " ", Hominus, " ", TRIM(mindHR_)
    ENDIF

  END SUBROUTINE Output

  !---------------------------------------------------------------------------

  SUBROUTINE PrintWarning(text,value)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: text
    REAL, INTENT(IN), OPTIONAL   :: value

    warnings = .TRUE.
    WRITE(IO_WARN,*) "Ref:     ", TRIM(ref)
    WRITE(IO_WARN,*) "Species: ", TRIM(chem)
    IF (PRESENT(value)) THEN ! show bad value
      WRITE(IO_WARN,*) "Warning: ", text,value
    ELSE
      WRITE(IO_WARN,*) "Warning: ", text
    ENDIF
    WRITE(IO_WARN,*)

  END SUBROUTINE PrintWarning

  !---------------------------------------------------------------------------

  SUBROUTINE IncorrectCitation (OriRef, ChemTeX)

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: OriRef, ChemTeX

    CALL MakeNote(TRIM(ref)//"-"//TRIM(casrn), &
      "The data from \citet{"//TRIM(OriRef)//"} for "//TRIM(ChemTeX)// &
      " are incorrectly cited by \citet{"//TRIM(ref)//"}.")

  END SUBROUTINE IncorrectCitation

  !---------------------------------------------------------------------------

  ! various types of the function str (from: messy/smcl/messy_main_tools.f90)

  CHARACTER(LEN=5) FUNCTION str_logical(zlogical, fmt)
    ! create string from logical
    IMPLICIT NONE
    INTRINSIC :: PRESENT
    LOGICAL, INTENT(IN) :: zlogical
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fmt
    IF (PRESENT(fmt)) THEN
      WRITE(str_logical,fmt) zlogical
    ELSE
      IF (zlogical) THEN
        str_logical = 'TRUE '
      ELSE
        str_logical = 'FALSE'
      ENDIF
    ENDIF
  END FUNCTION str_logical

  CHARACTER(LEN=STRLEN_VLONG) FUNCTION str_integer(zinteger, fmt)
    ! create string from integer
    IMPLICIT NONE
    INTRINSIC :: ADJUSTL, PRESENT
    INTEGER, INTENT(IN) :: zinteger
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fmt
    IF (PRESENT(fmt)) THEN
      WRITE(str_integer,fmt) zinteger
    ELSE
      WRITE(str_integer,*) zinteger
      str_integer = ADJUSTL(str_integer) ! remove leading spaces
    ENDIF
  END FUNCTION str_integer

  CHARACTER(LEN=STRLEN_VLONG) FUNCTION str_real(zreal, fmt)
    ! create string from real
    IMPLICIT NONE
    INTRINSIC :: ADJUSTL, PRESENT
    REAL, INTENT(IN) :: zreal
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fmt
    IF (PRESENT(fmt)) THEN
      WRITE(str_real,fmt) zreal
    ELSE
      WRITE(str_real,*) zreal
      str_real = ADJUSTL(str_real) ! remove leading spaces
    ENDIF
  END FUNCTION str_real

  ! --------------------------------------------------------------------------

  SUBROUTINE strcrack(str, ch, el, n) ! from: messy/smcl/messy_main_tools.f90

    ! strcrack = string crack

    ! Split the string <str> into small pieces which are separated by
    ! the character <ch>. Delete trailing spaces from the resulting <n>
    ! pieces, then put them into the array <el>.

    IMPLICIT NONE

    INTRINSIC :: ADJUSTL, ASSOCIATED, INDEX, LEN_TRIM, TRIM

    ! I/O
    CHARACTER(LEN=*),               INTENT(IN)  :: str
    CHARACTER,                      INTENT(IN)  :: ch
    CHARACTER(LEN=*), DIMENSION(:), POINTER     :: el
    INTEGER,                        INTENT(OUT) :: n

    ! LOCAL
    INTEGER :: idx1, idx2, i

    ! INIT
    IF (ASSOCIATED(el)) DEALLOCATE(el)
    NULLIFY(el)
    n = 0

    ! EMPTY STRING
    IF ( (TRIM(str) == '') .OR. (TRIM(str) == ch) ) RETURN

    idx1 = 0
    idx2 = 0
    DO
       idx1 = idx2 + 1
       IF (idx1 > LEN_TRIM(str(:))) EXIT
       IF (INDEX(TRIM(str(idx1:)), ch) == 0) THEN
          idx2 = LEN_TRIM(str(:)) + 1
       ELSE
          idx2 = idx2 + INDEX(TRIM(str(idx1:)), ch)
       END IF
       IF (idx1 == idx2) CYCLE

       n = n + 1

    END DO

    ! ALLOCATE SPACE
    ALLOCATE(el(n))
    DO i=1, n
       el(i) = ''
    END DO

    n = 0
    idx1 = 0
    idx2 = 0
    DO
       idx1 = idx2 + 1
       IF (idx1 > LEN_TRIM(str(:))) EXIT
       IF (INDEX(TRIM(str(idx1:)), ch) == 0) THEN
          idx2 = LEN_TRIM(str(:)) + 1
       ELSE
          idx2 = idx2 + INDEX(TRIM(str(idx1:)), ch)
       END IF
       IF (idx1 == idx2) CYCLE

       n = n + 1

       el(n) = ADJUSTL(str(idx1:idx2-1))

    END DO

  END SUBROUTINE strcrack

  !---------------------------------------------------------------------------

END MODULE Henry_util

!*****************************************************************************
!                                  end of file
!*****************************************************************************
