# load "ref2831-75-73-0.gnu"
# chem = "tetrafluoromethane"

set terminal postscript eps color
set title "ref = 2831; chem = tetrafluoromethane; casrn = 75-73-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2206048E-05 * exp(  -1854.087    *(1/   298.    -1/T))

set label "" at    275.5500    ,   0.3950957E-05 point
set label "" at    278.1500    ,   0.3626101E-05 point
set label "" at    283.1500    ,   0.3072967E-05 point
set label "" at    288.1500    ,   0.2607632E-05 point
set label "" at    293.1500    ,   0.2326675E-05 point
set label "" at    298.1500    ,   0.2080837E-05 point
set label "" at    303.1500    ,   0.1887679E-05 point
set label "" at    308.1500    ,   0.1734031E-05 point
set label "" at    313.1500    ,   0.1646232E-05 point
set label "" at    318.1500    ,   0.1545263E-05 point
set label "" at    323.1500    ,   0.1488194E-05 point
set label "" at    298.1500    ,   0.2206048E-05 point ps 2 pt 6

plot [270:330] H(T)
