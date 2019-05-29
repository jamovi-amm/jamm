TITLE:	this is an example of two-level path 
analysis with continuous dependent
variables and random slopes
DATA:	FILE IS mp9.5.dat;
VARIABLE:	NAMES ARE y1 y2 x1 clus;
WITHIN = x1 ;
CLUSTER IS clus;
ANALYSIS:	TYPE = TWOLEVEL RANDOM;
ALGORITHM = INTEGRATION;
MODEL:
  %WITHIN%
  s2 | y2 ON y1 x1;
       y1 ON x1;
%BETWEEN%
  y2 ON y1 x1;
  y1 ON x1;

OUTPUT:	TECH1 TECH8;