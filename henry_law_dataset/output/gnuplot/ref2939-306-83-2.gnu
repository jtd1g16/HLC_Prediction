# load "ref2939-306-83-2.gnu"
# chem = "2,2-dichloro-1,1,1-trifluoroethane"

set terminal postscript eps color
set title "ref = 2939; chem = 2,2-dichloro-1,1,1-trifluoroethane; casrn = 306-83-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2260680E-03 * exp(  -2416.960    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.2269924E-03 point
set label "" at    353.0000    ,   0.6415001E-04 point
set label "" at    298.1500    ,   0.2260680E-03 point ps 2 pt 6

plot [290:360] H(T)
