# load "ref2895-79-27-6.gnu"
# chem = "1,1,2,2-tetrabromoethane"

set terminal postscript eps color
set title "ref = 2895; chem = 1,1,2,2-tetrabromoethane; casrn = 79-27-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9951883E-02 * exp(  -835.0871    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1012523E-01 point
set label "" at    308.1500    ,   0.8820884E-02 point
set label "" at    323.1500    ,   0.8113686E-02 point
set label "" at    298.1500    ,   0.9951883E-02 point ps 2 pt 6

plot [290:330] H(T)
