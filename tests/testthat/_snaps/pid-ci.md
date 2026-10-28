# estimateCI() works as expected using Hessian

    list(hessian = structure(730.829887625898, dim = c(1L, 1L)), 
        covMat = structure(0.0427616122764907, dim = c(1L, 1L)), 
        eigen = 0.0427616122764907, corMat = structure(1, dim = c(1L, 
        1L)))

---

    structure(list(group = "1", name = "Lipophilicity", path = "Vergin 1995 IV|Aciclovir|Lipophilicity", 
        unit = "Log Units", estimate = 1.31885961110325, sd = 0.206821692216866, 
        cv = 0.156818580594682, lowerCI = 0.913496543136562, upperCI = 1.72422267906993, 
        ciType = NA_character_, initialValue = -0.097), class = "data.frame", row.names = c(NA, 
    -1L))

# estimateCI() works with bootstrap and individual data

    structure(list(group = "1", name = "Lipophilicity", path = "Vergin 1995 IV|Aciclovir|Lipophilicity", 
        unit = "Log Units", estimate = 1.32962909906082, sd = 0.00854818044649831, 
        cv = 0.00642899621596451, lowerCI = NA_real_, upperCI = 1.35158826184487, 
        ciType = "one-sided", initialValue = -0.097), class = "data.frame", row.names = c(NA, 
    -1L))

# estimateCI() works with bootstrap and aggregated data

    structure(list(group = "1", name = "Lipophilicity", path = "Vergin 1995 IV|Aciclovir|Lipophilicity", 
        unit = "Log Units", estimate = 1.31890001595242, sd = 0.00631045408522872, 
        cv = 0.00478463417158407, lowerCI = 1.31280533029821, upperCI = 1.32509245284263, 
        ciType = "two-sided", initialValue = -0.097), class = "data.frame", row.names = c(NA, 
    -1L))

# bootstrap stops before the first replicate when aggregated data have no subject count

    Code
      piTask$estimateCI()
    Condition
      Error in `.validateSubjectCounts()`:
      ! The aggregated data set(s) "AciclovirLaskinData.Laskin 1982.Group A" have no subject count. Bootstrap confidence intervals redraw the means of aggregated data from their mean and SD divided by the square root of the number of subjects, so call `PIOutputMapping$setSubjectCount()` on their output mapping with that number.

# run() with autoEstimateCI and bootstrap checks the subject counts before the optimization

    Code
      piTask$run()
    Condition
      Error in `.validateSubjectCounts()`:
      ! The aggregated data set(s) "AciclovirLaskinData.Laskin 1982.Group A" have no subject count. Bootstrap confidence intervals redraw the means of aggregated data from their mean and SD divided by the square root of the number of subjects, so call `PIOutputMapping$setSubjectCount()` on their output mapping with that number.

# run() that stops on a missing subject count keeps the previous fit

    Code
      suppressMessages(piTask$run())
    Condition
      Error in `.validateSubjectCounts()`:
      ! The aggregated data set(s) "AciclovirLaskinData.Laskin 1982.Group A" have no subject count. Bootstrap confidence intervals redraw the means of aggregated data from their mean and SD divided by the square root of the number of subjects, so call `PIOutputMapping$setSubjectCount()` on their output mapping with that number.

# estimateCI() outputs expected messages for mixed datasets

    Code
      temp <- piTask$estimateCI()
    Message
      Classified observed data: 5 individual, 1 aggregated dataset(s).
      Starting confidence interval estimation using 'bootstrap' for parameter value(s):
        1.323
      Running bootstrap replicate 1 of 3.
      Running bootstrap replicate 2 of 3.
      Running bootstrap replicate 3 of 3.

# estimateCI() applies bootstrap resampling to individual data

    list(AciclovirDataIndividuals.Aciclovir.Synthetic.id1 = c(0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0), AciclovirDataIndividuals.Aciclovir.Synthetic.id2 = c(0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0), AciclovirDataIndividuals.Aciclovir.Synthetic.id3 = c(1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1), AciclovirDataIndividuals.Aciclovir.Synthetic.id4 = c(1.73205080756888, 
    1.73205080756888, 1.73205080756888, 1.73205080756888, 1.73205080756888, 
    1.73205080756888, 1.73205080756888, 1.73205080756888, 1.73205080756888, 
    1.73205080756888, 1.73205080756888), AciclovirDataIndividuals.Aciclovir.Synthetic.id5 = c(1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

# estimateCI() applies bootstrap resampling to individual data with initial weights

    list(AciclovirDataIndividuals.Aciclovir.Synthetic.id1 = c(1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731), AciclovirDataIndividuals.Aciclovir.Synthetic.id2 = c(3.16227766016838, 
    3.16227766016838, 3.16227766016838, 3.16227766016838, 3.16227766016838, 
    3.16227766016838, 3.16227766016838, 3.16227766016838, 3.16227766016838, 
    3.16227766016838, 3.16227766016838), AciclovirDataIndividuals.Aciclovir.Synthetic.id3 = c(1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731), AciclovirDataIndividuals.Aciclovir.Synthetic.id4 = c(1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731), AciclovirDataIndividuals.Aciclovir.Synthetic.id5 = c(0, 
    1, 1, 2, 2, 1, 1, 1, 1, 1, 0.5))

# estimateCI() applies bootstrap resampling to mixed data (individual and aggregated)

    list("AciclovirLaskinData.Laskin 1982.Group A" = c(20, 20, 20, 
    20, 20, 20, 20, 20, 20, 20, 20), AciclovirDataIndividuals.Aciclovir.Synthetic.id1 = c(0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0), AciclovirDataIndividuals.Aciclovir.Synthetic.id2 = c(1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1), AciclovirDataIndividuals.Aciclovir.Synthetic.id3 = c(1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731), AciclovirDataIndividuals.Aciclovir.Synthetic.id4 = c(1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731, 1.4142135623731, 1.4142135623731, 
    1.4142135623731, 1.4142135623731), AciclovirDataIndividuals.Aciclovir.Synthetic.id5 = c(0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

---

    list("AciclovirLaskinData.Laskin 1982.Group A" = c(2.37029894378793, 
    2.2591937067773, 3.0031228561711, 3.04406262330303, 2.17274009628454, 
    1.934701913342, 1.30778198581538, 0.732094122213311, 0.434305491125997, 
    0.164341798836176, 0.0264269601757405), AciclovirDataIndividuals.Aciclovir.Synthetic.id1 = c(2.37344988818222, 
    3.54193002749525, 3.28755595546681, 2.94819210466812, 2.59158605331322, 
    2.32353204410174, 1.69334396105114, 0.862799026890571, 0.42893879026451, 
    0.106031201596579, 0.0191173707975167), AciclovirDataIndividuals.Aciclovir.Synthetic.id2 = c(2.63292304225615, 
    2.51127698902565, 2.3553041046398, 2.14291890188179, 1.91710796570987, 
    1.75231605226145, 1.34624701786379, 0.78096030620145, 0.441768492009942, 
    0.158272897010647, 0.0239749002872713), AciclovirDataIndividuals.Aciclovir.Synthetic.id3 = c(3.29963904732722, 
    3.10147197524202, 2.85889404949558, 2.54151791523327, 2.20942592932261, 
    1.97155304704211, 1.43427098464599, 0.783900588885444, 0.445910188773269, 
    0.144408005553487, 0.0215437498951587), AciclovirDataIndividuals.Aciclovir.Synthetic.id4 = c(1.92897209672083, 
    1.83896702310449, 1.7306659856331, 1.5870909919613, 1.43101294725057, 
    1.31712397433148, 1.02973694993125, 0.62249171151052, 0.385659205903721, 
    0.143641699423824, 0.0549395799964714), AciclovirDataIndividuals.Aciclovir.Synthetic.id5 = c(3.14099588649697, 
    2.961556901937, 2.74659100796271, 2.45322803493764, 2.15097497857641, 
    1.92541392607382, 1.39388305342436, 0.746018372410617, 0.438098993527092, 
    0.156404496465257, 0.0530437809231898))

