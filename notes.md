for large csv, rainbow csv stops working, preview by head.

Use load Rdata instead of read csv.

Reflected sound is measured in negative decibels (dB) because decibels are a logarithmic ratio comparing the reflected sound's intensity to a reference level (usually 0 dB) rather than an absolute measure of volume. A negative value indicates the reflected sound is weaker than the original source due to energy loss during reflection, or it is softer than the reference threshold

I want to first clear up some confusion about the pyhsics of the measurements.
TS stands for Target Strength = 10 * log(I/I0)

In this equation:
- TS is the target strength in decibels (dB), always a negative value for reflected sound since Intensity is weaker than the original source (reference level)
- I is the intensity of the reflected sound
- I0 is the reference intensity, original sound source intensity

Decibels (dB): Measures sound pressure level relative to a reference threshold.Watts per Square Meter (\(\text{W/m}^{2}\)): Measures sound intensity or acoustic energy flowing through an area.

Then the filtering. The 6 dB filter is applied to remove targets that have had a large compensation applied, which could indicate that the original signal was weak and may not be reliable for analysis. By filtering out targets with more than 6 dB of compensation, we aim to focus on stronger, more reliable detections that are less likely to be influenced by noise or other factors.
why fish most intense? what if other targets are more intense than the fish?

In this repo, compensated TS is not computed by custom R code; it is exported by Echoview with “apply beam compensation = TRUE,” while uncompTS is exported with that flag FALSE
instrument/calibration setup 
parameters used in Echoview for compensation adjusted across datasets?
consistent compensation logic across datasets?
compensation filter much different from just low TS filter?
likelihood of the filter to remove real fish targets that are just weakly detected and let in false positives (actual noises)?



See if the other dataset has same filtering logic.