# load "ref1996-79-24-3.gnu"
# chem = "nitroethane"

set terminal postscript eps color
set title "ref = 1996; chem = nitroethane; casrn = 79-24-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2168979     * exp(  -4402.954    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.2823567     point
set label "" at    303.1500    ,   0.1682125     point
set label "" at    313.1500    ,   0.1052128     point
set label "" at    323.1500    ,   0.7023085E-01 point
set label "" at    298.1500    ,   0.2168979     point ps 2 pt 6

plot [290:330] H(T)
