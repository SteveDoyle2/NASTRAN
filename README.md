# NASTRAN
NASTRAN has been released under the  
[NASA Open Source Agreement version 1.3](https://github.com/nasa/NASTRAN/raw/master/NASA%20Open%20Source%20Agreement-NASTRAN.doc).


NASTRAN is the NASA Structural Analysis System, a finite element analysis program (FEA) completed in the early 1970's. It was the first of its kind and opened the door to computer-aided engineering. Subsections of a design can be modeled and then larger groupings of these elements can again be modeled. NASTRAN can handle elastic stability analysis, complex eigenvalues for vibration and dynamic stability analysis, dynamic response for transient and steady state loads, and random excitation, and static response to concentrated and distributed loads, thermal expansion, and enforced deformations.

Based on the source code, this is based off a version of COSMIC Nastran that was last worked on in April of 1994 by Gordon Chan at Unisys.  http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19900015327.pdf

NOTE: There is no technical support available for this software.

I'd like this software to someday be useable to solve problems with the classic elements (e.g. CROD, CBAR, CBEAM, CTRIA3, CQUAD4, CTETRA, CHEXA) and the classic solutions (statics, modal, frequency, transient, static aero, flutter).  It's a ways off from that though.  It doesn't even build yet on a modern machine.  I'd like it to work on Windows using gfortran, which will also allow for porting to Linux and Mac.

To achieve that:
  1.  Port to Windows
  2.  Compare OP2/F06 results with those from NX/MSC Nastran using pyNastran
  3.  Establish list of supported cards
  4.  Establish limitations

If anybody has COSMIC Nastran documtation (possibly from NTRS), send me a link.