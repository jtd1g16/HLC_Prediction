# load "ref0863-74-95-3.gnu"
# chem = "dibromomethane"

set terminal postscript eps color
set title "ref = 863; chem = dibromomethane; casrn = 74-95-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1124850E-01 * exp(  -4400.501    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4402225E-01 point
set label "" at    283.1500    ,   0.2424233E-01 point
set label "" at    293.1500    ,   0.1425585E-01 point
set label "" at    303.1500    ,   0.8953197E-02 point
set label "" at    298.1500    ,   0.1124850E-01 point ps 2 pt 6

plot [270:310] H(T)
