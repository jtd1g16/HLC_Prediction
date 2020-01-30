# load "ref1775-76-13-1.gnu"
# chem = "1,1,2-trichlorotrifluoroethane"

set terminal postscript eps color
set title "ref = 1775; chem = 1,1,2-trichlorotrifluoroethane; casrn = 76-13-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3070820E-04 * exp(  -4348.490    *(1/   298.    -1/T))

set label "" at    273.1900    ,   0.1247468E-03 point
set label "" at    273.1900    ,   0.1243335E-03 point
set label "" at    273.1900    ,   0.1241072E-03 point
set label "" at    273.8600    ,   0.1181149E-03 point
set label "" at    273.8600    ,   0.1186856E-03 point
set label "" at    282.7000    ,   0.6593533E-04 point
set label "" at    282.7000    ,   0.6655522E-04 point
set label "" at    282.7000    ,   0.6592549E-04 point
set label "" at    282.7000    ,   0.6591565E-04 point
set label "" at    282.7000    ,   0.6584677E-04 point
set label "" at    292.1500    ,   0.3876812E-04 point
set label "" at    292.1500    ,   0.3889604E-04 point
set label "" at    292.1500    ,   0.3871892E-04 point
set label "" at    292.1500    ,   0.3877796E-04 point
set label "" at    292.1500    ,   0.3881732E-04 point
set label "" at    301.6400    ,   0.2543543E-04 point
set label "" at    301.6400    ,   0.2518944E-04 point
set label "" at    302.6200    ,   0.2428419E-04 point
set label "" at    302.6200    ,   0.2436291E-04 point
set label "" at    302.6200    ,   0.2412676E-04 point
set label "" at    302.6200    ,   0.2431371E-04 point
set label "" at    302.6200    ,   0.2423500E-04 point
set label "" at    307.1000    ,   0.2042706E-04 point
set label "" at    312.7400    ,   0.1673720E-04 point
set label "" at    312.7400    ,   0.1685528E-04 point
set label "" at    312.7400    ,   0.1687496E-04 point
set label "" at    312.7400    ,   0.1669784E-04 point
set label "" at    298.1500    ,   0.3070820E-04 point ps 2 pt 6

plot [270:320] H(T)
