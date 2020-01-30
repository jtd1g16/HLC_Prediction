# load "ref0379-1333-74-0.gnu"
# chem = "hydrogen"

set terminal postscript eps color
set title "ref = 379; chem = hydrogen; casrn = 1333-74-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7717712E-05 * exp(  -638.1781    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.9457991E-05 point
set label "" at    274.1500    ,   0.9361122E-05 point
set label "" at    275.1500    ,   0.9268655E-05 point
set label "" at    276.1500    ,   0.9176189E-05 point
set label "" at    277.1500    ,   0.9088126E-05 point
set label "" at    278.1500    ,   0.9000063E-05 point
set label "" at    279.1500    ,   0.8916402E-05 point
set label "" at    280.1500    ,   0.8837146E-05 point
set label "" at    281.1500    ,   0.8757889E-05 point
set label "" at    282.1500    ,   0.8683035E-05 point
set label "" at    283.1500    ,   0.8608181E-05 point
set label "" at    284.1500    ,   0.8542134E-05 point
set label "" at    285.1500    ,   0.8476086E-05 point
set label "" at    286.1500    ,   0.8414442E-05 point
set label "" at    287.1500    ,   0.8352798E-05 point
set label "" at    288.1500    ,   0.8291154E-05 point
set label "" at    289.1500    ,   0.8229509E-05 point
set label "" at    290.1500    ,   0.8172268E-05 point
set label "" at    291.1500    ,   0.8119430E-05 point
set label "" at    292.1500    ,   0.8062189E-05 point
set label "" at    293.1500    ,   0.8009351E-05 point
set label "" at    294.1500    ,   0.7947707E-05 point
set label "" at    295.1500    ,   0.7890466E-05 point
set label "" at    296.1500    ,   0.7833225E-05 point
set label "" at    297.1500    ,   0.7775984E-05 point
set label "" at    298.1500    ,   0.7723146E-05 point
set label "" at    299.1500    ,   0.7670308E-05 point
set label "" at    300.1500    ,   0.7621873E-05 point
set label "" at    301.1500    ,   0.7573438E-05 point
set label "" at    302.1500    ,   0.7525003E-05 point
set label "" at    303.1500    ,   0.7480972E-05 point
set label "" at    298.1500    ,   0.7717712E-05 point ps 2 pt 6

plot [270:310] H(T)
