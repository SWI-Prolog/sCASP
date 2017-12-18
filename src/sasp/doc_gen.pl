/*
* The code for the s(ASP) system contains structured comments for use with
* SWI-Prolog's PlDoc to automatically generate documentation. This file contains
* commands to start pldoc server and open a browser to the main page of the
* documentation.
*
* The documentation is intended for developers and those interested in the
* internals of the compiler. Normal users should see the README in the root
* directory of the distribution.
*/
:- doc_server(21000).    % Start PlDoc at port 21000
:- portray_text(true).  % Enable portray of strings
:- ensure_loaded('main.pl').
:- doc_browser.