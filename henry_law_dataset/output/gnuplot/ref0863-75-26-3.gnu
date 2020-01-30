# load "ref0863-75-26-3.gnu"
# chem = "2-bromopropane"

set terminal postscript eps color
set title "ref = 863; chem = 2-bromopropane; casrn = 75-26-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8992263E-03 * exp(  -4528.382    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3691155E-02 point
set label "" at    283.1500    ,   0.1998038E-02 point
set label "" at    293.1500    ,   0.1099322E-02 point
set label "" at    303.1500    ,   0.7302068E-03 point
set label "" at    298.1500    ,   0.8992263E-03 point ps 2 pt 6

plot [270:310] H(T)
