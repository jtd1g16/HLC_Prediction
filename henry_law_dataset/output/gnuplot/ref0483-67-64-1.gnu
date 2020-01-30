# load "ref0483-67-64-1.gnu"
# chem = "propanone"

set terminal postscript eps color
set title "ref = 483; chem = propanone; casrn = 67-64-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2522275     * exp(  -4770.429    *(1/   298.    -1/T))

set label "" at    273.0000    ,    1.101395     point
set label "" at    298.0000    ,   0.2542671     point
set label "" at    298.1500    ,   0.2522275     point ps 2 pt 6

plot [270:300] H(T)
