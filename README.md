Population Synthesizer
=======

MTC's Travel Model Two modeling system (currently under development) will use this population synthesizer.  For details on Travel Model Two, please see the [project webpage](http://bayareametro.github.io/travel-model-two/) and the [project GitHub repository](https://github.com/BayAreaMetro/travel-model-two). 

The source code for the Population Synthesizer was provided by the [Maricopa County Association of Governments](http://www.azmag.gov/). We are grateful for their investment in the software and their permission to use the software. MAG asks us to note:

> This report and/or data was funded in part through grant[s] from the Federal Highway Administration and/or Federal Transit Administration, U.S. Department of Transportation. The contents of this report reflect the views and opinions of the author(s) who is responsible for the facts and accuracy of the data presented herein. The contents do not necessarily state or reflect the official views or policies of the U.S. Department of Transportation, the Arizona Department of Transportation, or any other State or Federal Agency. This report does not constitute a standard, specification or regulation.

Within MTC's environment, to build from source:
* Download and install [Apache Ant](http://ant.apache.org)
* run `ant all` in the root directory of this repo (where [`build.xml`](build.xml) is located.)

See also [`go_go_build`](go_go_build.bat). 


File Structure
==============

* application - older files.  Will be cleaned/deprecated.
* build - Built java classes, created when `go_go_build.bat` is run.
* census_data - Census and PUMS data.  Not in github since these include huge files; these are copied from Box in `runPopSynIII.bat`
* outputs - final output, including intermediate files
* release - Built java Archive files, created when `go_go_build.bat` is run.
* runtime - final compiled executables and configuration
* scripts - batch and R scripts used to run the entire PopSyn process
* src - Not in github.  Put source java files here for building.