# load "ref2447-115-25-3.gnu"
# chem = "octafluorocyclobutane"

set terminal postscript eps color
set title "ref = 2447; chem = octafluorocyclobutane; casrn = 115-25-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1333029E-05 * exp(  -3091.854    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3139455E-05 point
set label "" at    283.1500    ,   0.2363876E-05 point
set label "" at    288.1500    ,   0.1843911E-05 point
set label "" at    293.1500    ,   0.1486162E-05 point
set label "" at    298.1500    ,   0.1215802E-05 point
set label "" at    303.1500    ,   0.1057955E-05 point
set label "" at    308.1500    ,   0.9301483E-06 point
set label "" at    313.1500    ,   0.8394820E-06 point
set label "" at    318.1500    ,   0.7755787E-06 point
set label "" at    298.1500    ,   0.1333029E-05 point ps 2 pt 6

plot [270:320] H(T)
