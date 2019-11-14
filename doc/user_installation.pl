:- use_package(assertions).

:- doc(filetype, documentation).

:- doc(author, "Joaquin Arias").

:- doc(title,"Installation").

:- doc(module, "

@section{Quick installation}
Requirements
@begin{enumerate}
@item Ciao (visit @href{http://ciao-lang.org})
@end{enumerate}

@section{Build and installation}


You can automatically fetch, build, and install this bundle using:

@begin{verbatim}
ciao get gitlab.software.imdea.org/joaquin.arias/sCASP
@end{verbatim}

This command stores the source and generates the binaries in the Ciao
_workspace directory_. This directory is given by the value of the
`CIAOPATH` environment variable (or `~/.ciao` if unspecified).

Binaries are placed in the `$CIAOPATH/build/bin` directory (or
`~/.ciao/build/bin`). To call a binary without specifying its full
path it is recommended to include this directory in your `PATH`:

@begin{verbatim}
export PATH=$CIAOPATH/build/bin:$PATH
# or export PATH=~/.ciao/build/bin:$PATH
@end{verbatim}

@section{Installation for developers}

For installing s(CASP) independently from CIAO clone this repository
wherever you want (e.g., ~/devel/).

    cd ~/devel
    git clone git@gitlab.software.imdea.org:joaquin.arias/sCASP.git

Compile it using the Makefile.

    cd scasp
    make compile_scasp

To call a binary without specifying its full path it is recommended to
include this directory in your `PATH`:

@begin{verbatim}
export PATH=~/devel/scasp:$PATH
@end{verbatim}

").
