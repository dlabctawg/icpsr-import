# icpsr-import
Routines for importing [ICPSR congressional record data](http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/33501/version/2) for the Berkeley DLab [Computational Text Analysis Working Group](http://dlabctawg.github.io/).

The `icpsr-import.R` script contains the `icpsr2RData.f` and `icpsr2bow.f` functions, which respectively

- import the original ICPSR Congressional Record floor speeches and covariate data from the 104th through the 110th United States congresses as .RData files, and
- format these data as 1-gram tokenizations suitable for bag-of-words text analysis models.

The 1-gram database is approximately 140 MB compressed and nearly 1 GB uncompressed in memory; @berkeley.edu users can download or request access to it [here](https://drive.google.com/a/berkeley.edu/folderview?id=0B6bobRDQR96iVDZ4RkdkWl9sNUU&usp=sharing).