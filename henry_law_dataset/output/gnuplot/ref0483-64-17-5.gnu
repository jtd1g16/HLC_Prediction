# load "ref0483-64-17-5.gnu"
# chem = "ethanol"

set terminal postscript eps color
set title "ref = 483; chem = ethanol; casrn = 64-17-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.875807     * exp(  -6627.314    *(1/   298.    -1/T))

set label "" at    273.0000    ,    14.53842     point
set label "" at    298.0000    ,    1.896913     point
set label "" at    298.1500    ,    1.875807     point ps 2 pt 6

plot [270:300] H(T)
