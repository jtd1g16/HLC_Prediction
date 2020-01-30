# load "ref3026-1333-74-0.gnu"
# chem = "hydrogen"

set terminal postscript eps color
set title "ref = 3026; chem = hydrogen; casrn = 1333-74-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7880309E-05 * exp(  -499.4638    *(1/   298.    -1/T))

set label "" at    273.6500    ,   0.9427169E-05 point
set label "" at    273.7300    ,   0.9396347E-05 point
set label "" at    273.7700    ,   0.9387541E-05 point
set label "" at    283.2200    ,   0.8612584E-05 point
set label "" at    283.0000    ,   0.8634600E-05 point
set label "" at    283.1500    ,   0.8577359E-05 point
set label "" at    293.1500    ,   0.8048980E-05 point
set label "" at    293.1800    ,   0.7991739E-05 point
set label "" at    293.1500    ,   0.7991739E-05 point
set label "" at    303.1600    ,   0.7450150E-05 point
set label "" at    303.1500    ,   0.7467762E-05 point
set label "" at    303.1300    ,   0.7357683E-05 point
set label "" at    303.1500    ,   0.7538213E-05 point
set label "" at    303.1500    ,   0.7547019E-05 point
set label "" at    303.1500    ,   0.7511794E-05 point
set label "" at    313.1100    ,   0.7260814E-05 point
set label "" at    313.0400    ,   0.7216782E-05 point
set label "" at    313.0300    ,   0.7203573E-05 point
set label "" at    313.1500    ,   0.7238798E-05 point
set label "" at    313.1500    ,   0.7243201E-05 point
set label "" at    313.1500    ,   0.7256411E-05 point
set label "" at    323.1700    ,   0.7058268E-05 point
set label "" at    323.0000    ,   0.7084687E-05 point
set label "" at    323.1300    ,   0.7084687E-05 point
set label "" at    323.2500    ,   0.7115509E-05 point
set label "" at    323.1800    ,   0.7075881E-05 point
set label "" at    323.0500    ,   0.7071478E-05 point
set label "" at    298.1500    ,   0.7880309E-05 point ps 2 pt 6

plot [270:330] H(T)
