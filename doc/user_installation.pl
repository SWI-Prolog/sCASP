:- use_package(assertions).

:- doc(filetype, documentation).

:- doc(author, "Joaquin Arias").

:- doc(title,"Installation").

:- doc(module, "

@section{Quick installation}
Requirements
@begin{enumerate}
@item Ciao with TCLP (ask @email{joaquin.arias@@imdea.org})
@end{enumerate}

@section{Build and installation}


You can automatically fetch, build, and install this bundle using:

@begin{verbatim}
ciao get ciao-lang.org/scasp
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

For installing this bundle it is recommended to define `CIAOPATH`
(E.g., `~/ciao`) and clone this repository in your workspace.

	git clone ssh://gitolite@ciao-lang.org/scasp

Remember to update registered bundles after cloning

	ciao rescan-bundles ~/ciao

").