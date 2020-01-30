# load "ref3027-7782-44-7.gnu"
# chem = "oxygen"

set terminal postscript eps color
set title "ref = 3027; chem = oxygen; casrn = 7782-44-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1252390E-04 * exp(  -1240.408    *(1/   298.    -1/T))

set label "" at    293.5500    ,   0.1356174E-04 point
set label "" at    293.5500    ,   0.1356614E-04 point
set label "" at    293.5000    ,   0.1359256E-04 point
set label "" at    303.1700    ,   0.1150106E-04 point
set label "" at    303.1500    ,   0.1143941E-04 point
set label "" at    303.0800    ,   0.1152307E-04 point
set label "" at    313.1500    ,   0.1017130E-04 point
set label "" at    313.1700    ,   0.1014048E-04 point
set label "" at    313.1500    ,   0.1014929E-04 point
set label "" at    323.1200    ,   0.9202608E-05 point
set label "" at    323.1500    ,   0.9202608E-05 point
set label "" at    323.1300    ,   0.9198205E-05 point
set label "" at    298.1500    ,   0.1252390E-04 point ps 2 pt 6

plot [290:330] H(T)
