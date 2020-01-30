# load "ref0583-74-95-3.gnu"
# chem = "dibromomethane"

set terminal postscript eps color
set title "ref = 583; chem = dibromomethane; casrn = 74-95-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9227332E-02 * exp(  -4684.739    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3931394E-01 point
set label "" at    283.1500    ,   0.2072027E-01 point
set label "" at    293.1500    ,   0.1221059E-01 point
set label "" at    298.1500    ,   0.9227332E-02 point ps 2 pt 6

plot [270:300] H(T)
