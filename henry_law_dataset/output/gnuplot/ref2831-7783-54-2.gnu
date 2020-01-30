# load "ref2831-7783-54-2.gnu"
# chem = "nitrogen trifluoride"

set terminal postscript eps color
set title "ref = 2831; chem = nitrogen trifluoride; casrn = 7783-54-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8160964E-05 * exp(  -1861.985    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1369226E-04 point
set label "" at    283.1500    ,   0.1148851E-04 point
set label "" at    288.1500    ,   0.9965192E-05 point
set label "" at    293.1500    ,   0.8793074E-05 point
set label "" at    298.1500    ,   0.7888744E-05 point
set label "" at    303.1500    ,   0.6997584E-05 point
set label "" at    308.1500    ,   0.6492739E-05 point
set label "" at    313.1500    ,   0.6009845E-05 point
set label "" at    318.1500    ,   0.5641089E-05 point
set label "" at    323.1500    ,   0.5364522E-05 point
set label "" at    298.1500    ,   0.8160964E-05 point ps 2 pt 6

plot [270:330] H(T)
