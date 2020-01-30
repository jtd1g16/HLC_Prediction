# load "ref0863-75-30-9.gnu"
# chem = "2-iodopropane"

set terminal postscript eps color
set title "ref = 863; chem = 2-iodopropane; casrn = 75-30-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8451567E-03 * exp(  -4545.236    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3495012E-02 point
set label "" at    283.1500    ,   0.1828785E-02 point
set label "" at    293.1500    ,   0.1091465E-02 point
set label "" at    303.1500    ,   0.6676517E-03 point
set label "" at    298.1500    ,   0.8451567E-03 point ps 2 pt 6

plot [270:310] H(T)
