# load "ref2121-306-83-2.gnu"
# chem = "2,2-dichloro-1,1,1-trifluoroethane"

set terminal postscript eps color
set title "ref = 2121; chem = 2,2-dichloro-1,1,1-trifluoroethane; casrn = 306-83-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3313557E-03 * exp(  -3372.388    *(1/   298.    -1/T))

set label "" at    279.1500    ,   0.7533765E-03 point
set label "" at    285.1500    ,   0.5113592E-03 point
set label "" at    295.1500    ,   0.3855169E-03 point
set label "" at    303.1500    ,   0.2711328E-03 point
set label "" at    313.1500    ,   0.1942762E-03 point
set label "" at    298.1500    ,   0.3313557E-03 point ps 2 pt 6

plot [270:320] H(T)
