# load "ref2447-428-59-1.gnu"
# chem = "trifluoro(trifluoromethyl)-oxirane"

set terminal postscript eps color
set title "ref = 2447; chem = trifluoro(trifluoromethyl)-oxirane; casrn = 428-59-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8829486E-05 * exp(  -2950.598    *(1/   298.    -1/T))

set label "" at    283.2000    ,   0.1343608E-04 point
set label "" at    293.2000    ,   0.1059593E-04 point
set label "" at    303.2000    ,   0.7755787E-05 point
set label "" at    313.2000    ,   0.6117240E-05 point
set label "" at    323.2000    ,   0.4915640E-05 point
set label "" at    333.2000    ,   0.2457820E-05 point
set label "" at    298.1500    ,   0.8829486E-05 point ps 2 pt 6

plot [280:340] H(T)
