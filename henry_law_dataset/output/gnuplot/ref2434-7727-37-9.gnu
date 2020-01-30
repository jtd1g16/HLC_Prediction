# load "ref2434-7727-37-9.gnu"
# chem = "nitrogen"

set terminal postscript eps color
set title "ref = 2434; chem = nitrogen; casrn = 7727-37-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6601522E-05 * exp(  -1235.623    *(1/   298.    -1/T))

set label "" at    278.1240    ,   0.9319926E-05 point
set label "" at    278.1460    ,   0.9309015E-05 point
set label "" at    283.1540    ,   0.8335981E-05 point
set label "" at    288.1530    ,   0.7566655E-05 point
set label "" at    293.1490    ,   0.6932583E-05 point
set label "" at    298.1420    ,   0.6434178E-05 point
set label "" at    298.1480    ,   0.6428662E-05 point
set label "" at    298.1580    ,   0.6425169E-05 point
set label "" at    298.1580    ,   0.6428065E-05 point
set label "" at    298.1630    ,   0.6429200E-05 point
set label "" at    303.1360    ,   0.6026023E-05 point
set label "" at    308.1410    ,   0.5686533E-05 point
set label "" at    308.1420    ,   0.5688275E-05 point
set label "" at    313.1540    ,   0.5411009E-05 point
set label "" at    318.1510    ,   0.5185779E-05 point
set label "" at    323.1470    ,   0.5008979E-05 point
set label "" at    323.1500    ,   0.5007901E-05 point
set label "" at    298.1500    ,   0.6601522E-05 point ps 2 pt 6

plot [270:330] H(T)
