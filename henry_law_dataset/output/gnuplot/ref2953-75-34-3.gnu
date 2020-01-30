# load "ref2953-75-34-3.gnu"
# chem = "1,1-dichloroethane"

set terminal postscript eps color
set title "ref = 2953; chem = 1,1-dichloroethane; casrn = 75-34-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1774639E-02 * exp(  -3752.583    *(1/   298.    -1/T))

set label "" at    275.1500    ,   0.5751521E-02 point
set label "" at    279.1500    ,   0.4158804E-02 point
set label "" at    283.1500    ,   0.3522102E-02 point
set label "" at    291.1500    ,   0.2210242E-02 point
set label "" at    298.1500    ,   0.1687847E-02 point
set label "" at    303.1500    ,   0.1314151E-02 point
set label "" at    313.1500    ,   0.9445959E-03 point
set label "" at    323.1500    ,   0.6791741E-03 point
set label "" at    333.1500    ,   0.5243510E-03 point
set label "" at    298.1500    ,   0.1774639E-02 point ps 2 pt 6

plot [270:340] H(T)
