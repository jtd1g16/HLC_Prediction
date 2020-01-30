# load "ref2834-74-84-0.gnu"
# chem = "ethane"

set terminal postscript eps color
set title "ref = 2834; chem = ethane; casrn = 74-84-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1857757E-04 * exp(  -2661.316    *(1/   298.    -1/T))

set label "" at    273.5500    ,   0.4267544E-04 point
set label "" at    273.5700    ,   0.4278112E-04 point
set label "" at    273.6300    ,   0.4274149E-04 point
set label "" at    273.4000    ,   0.4311136E-04 point
set label "" at    273.4300    ,   0.4287359E-04 point
set label "" at    273.4700    ,   0.4281194E-04 point
set label "" at    283.1800    ,   0.2883190E-04 point
set label "" at    283.1700    ,   0.2887153E-04 point
set label "" at    283.1800    ,   0.2886273E-04 point
set label "" at    293.0800    ,   0.2090181E-04 point
set label "" at    293.1800    ,   0.2089300E-04 point
set label "" at    293.1700    ,   0.2087099E-04 point
set label "" at    293.1500    ,   0.2072568E-04 point
set label "" at    293.1500    ,   0.2072568E-04 point
set label "" at    293.1500    ,   0.2069046E-04 point
set label "" at    303.1500    ,   0.1600109E-04 point
set label "" at    303.1500    ,   0.1599228E-04 point
set label "" at    303.1500    ,   0.1602311E-04 point
set label "" at    303.1500    ,   0.1588661E-04 point
set label "" at    303.1500    ,   0.1592183E-04 point
set label "" at    303.1500    ,   0.1591743E-04 point
set label "" at    313.1300    ,   0.1282641E-04 point
set label "" at    313.1000    ,   0.1283522E-04 point
set label "" at    313.1500    ,   0.1286164E-04 point
set label "" at    298.1500    ,   0.1857757E-04 point ps 2 pt 6

plot [270:320] H(T)
