# load "ref2953-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2953; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1814655E-02 * exp(  -3735.336    *(1/   298.    -1/T))

set label "" at    275.1500    ,   0.5804988E-02 point
set label "" at    279.1500    ,   0.4244848E-02 point
set label "" at    283.1500    ,   0.3581497E-02 point
set label "" at    291.1500    ,   0.2266013E-02 point
set label "" at    298.1500    ,   0.1744036E-02 point
set label "" at    303.1500    ,   0.1351301E-02 point
set label "" at    313.1500    ,   0.9684132E-03 point
set label "" at    323.1500    ,   0.6866926E-03 point
set label "" at    333.1500    ,   0.5423913E-03 point
set label "" at    298.1500    ,   0.1814655E-02 point ps 2 pt 6

plot [270:340] H(T)
