# load "ref1773-75-18-3.gnu"
# chem = "dimethyl sulfide"

set terminal postscript eps color
set title "ref = 1773; chem = dimethyl sulfide; casrn = 75-18-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6386639E-02 * exp(  -4085.993    *(1/   298.    -1/T))

set label "" at    274.4000    ,   0.2131754E-01 point
set label "" at    283.4000    ,   0.1450777E-01 point
set label "" at    291.0000    ,   0.7105848E-02 point
set label "" at    303.4000    ,   0.5625463E-02 point
set label "" at    313.4000    ,   0.3256847E-02 point
set label "" at    298.1500    ,   0.6386639E-02 point ps 2 pt 6

plot [270:320] H(T)
