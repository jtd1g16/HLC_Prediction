# load "ref0483-71-23-8.gnu"
# chem = "1-propanol"

set terminal postscript eps color
set title "ref = 483; chem = 1-propanol; casrn = 71-23-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.315188     * exp(  -7467.961    *(1/   298.    -1/T))

set label "" at    273.0000    ,    13.21674     point
set label "" at    298.0000    ,    1.331875     point
set label "" at    298.1500    ,    1.315188     point ps 2 pt 6

plot [270:300] H(T)
