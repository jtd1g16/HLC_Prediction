# load "ref2831-2551-62-4.gnu"
# chem = "sulfur hexafluoride"

set terminal postscript eps color
set title "ref = 2831; chem = sulfur hexafluoride; casrn = 2551-62-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2553033E-05 * exp(  -2444.631    *(1/   298.    -1/T))

set label "" at    275.6500    ,   0.5645479E-05 point
set label "" at    278.1500    ,   0.4982596E-05 point
set label "" at    283.1500    ,   0.4008026E-05 point
set label "" at    285.6500    ,   0.3612931E-05 point
set label "" at    288.1500    ,   0.3296854E-05 point
set label "" at    290.6500    ,   0.3029067E-05 point
set label "" at    293.1500    ,   0.2756890E-05 point
set label "" at    295.6500    ,   0.2581292E-05 point
set label "" at    298.1500    ,   0.2392524E-05 point
set label "" at    300.6500    ,   0.2225706E-05 point
set label "" at    303.1500    ,   0.2089617E-05 point
set label "" at    308.1500    ,   0.1883290E-05 point
set label "" at    313.1500    ,   0.1751591E-05 point
set label "" at    318.1500    ,   0.1637452E-05 point
set label "" at    323.1500    ,   0.1545263E-05 point
set label "" at    298.1500    ,   0.2553033E-05 point ps 2 pt 6

plot [270:330] H(T)
