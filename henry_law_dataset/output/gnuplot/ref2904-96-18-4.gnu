# load "ref2904-96-18-4.gnu"
# chem = "1,2,3-trichlorobenzene"

set terminal postscript eps color
set title "ref = 2904; chem = 1,2,3-trichlorobenzene; casrn = 96-18-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2800649E-01 * exp(  -4971.637    *(1/   298.    -1/T))

set label "" at    298.5500    ,   0.2685700E-01 point
set label "" at    308.1500    ,   0.1696977E-01 point
set label "" at    318.1500    ,   0.9450916E-02 point
set label "" at    328.1500    ,   0.6430113E-02 point
set label "" at    333.1500    ,   0.4688515E-02 point
set label "" at    298.1500    ,   0.2800649E-01 point ps 2 pt 6

plot [290:340] H(T)
