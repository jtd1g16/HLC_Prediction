# load "ref0855-75-09-2.gnu"
# chem = "dichloromethane"

set terminal postscript eps color
set title "ref = 855; chem = dichloromethane; casrn = 75-09-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3938860E-02 * exp(  -3782.132    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.7805629E-02 point
set label "" at    293.1500    ,   0.4770854E-02 point
set label "" at    303.1500    ,   0.3236369E-02 point
set label "" at    298.1500    ,   0.3938860E-02 point ps 2 pt 6

plot [280:310] H(T)
