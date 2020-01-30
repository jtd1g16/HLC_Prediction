# load "ref2631-75-15-0.gnu"
# chem = "carbon disulfide"

set terminal postscript eps color
set title "ref = 2631; chem = carbon disulfide; casrn = 75-15-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6208455E-03 * exp(  -3831.798    *(1/   298.    -1/T))

set label "" at    273.6500    ,   0.1997780E-02 point
set label "" at    281.1500    ,   0.1296325E-02 point
set label "" at    289.1500    ,   0.9243366E-03 point
set label "" at    297.1500    ,   0.6860221E-03 point
set label "" at    305.1500    ,   0.4478884E-03 point
set label "" at    298.1500    ,   0.6208455E-03 point ps 2 pt 6

plot [270:310] H(T)
