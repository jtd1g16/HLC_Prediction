# load "ref0863-107-08-4.gnu"
# chem = "1-iodopropane"

set terminal postscript eps color
set title "ref = 863; chem = 1-iodopropane; casrn = 107-08-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1046196E-02 * exp(  -4557.727    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4296194E-02 point
set label "" at    283.1500    ,   0.2316461E-02 point
set label "" at    293.1500    ,   0.1341109E-02 point
set label "" at    303.1500    ,   0.8244048E-03 point
set label "" at    298.1500    ,   0.1046196E-02 point ps 2 pt 6

plot [270:310] H(T)
