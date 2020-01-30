# load "ref2953-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 2953; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2582805E-02 * exp(  -4126.546    *(1/   298.    -1/T))

set label "" at    275.1500    ,   0.9260924E-02 point
set label "" at    279.1500    ,   0.6608161E-02 point
set label "" at    283.1500    ,   0.5516436E-02 point
set label "" at    291.1500    ,   0.3265566E-02 point
set label "" at    298.1500    ,   0.2453744E-02 point
set label "" at    303.1500    ,   0.1899196E-02 point
set label "" at    313.1500    ,   0.1298859E-02 point
set label "" at    323.1500    ,   0.8927499E-03 point
set label "" at    333.1500    ,   0.6679290E-03 point
set label "" at    298.1500    ,   0.2582805E-02 point ps 2 pt 6

plot [270:340] H(T)
