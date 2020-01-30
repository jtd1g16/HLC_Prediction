# load "ref0863-106-94-5.gnu"
# chem = "1-bromopropane"

set terminal postscript eps color
set title "ref = 863; chem = 1-bromopropane; casrn = 106-94-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1081998E-02 * exp(  -4462.970    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4381241E-02 point
set label "" at    283.1500    ,   0.2327033E-02 point
set label "" at    293.1500    ,   0.1348074E-02 point
set label "" at    303.1500    ,   0.8746433E-03 point
set label "" at    298.1500    ,   0.1081998E-02 point ps 2 pt 6

plot [270:310] H(T)
