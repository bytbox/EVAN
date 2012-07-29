# - Determine if ccache is installed and if so, locate the executable.
#
# The following variables are set:
#
#  CCACHE_EXECUTABLE - path to the ccache program
#  CCACHE_VERSION - version of ccache found
#  CCACHE_FOUND - true if ccache was found
#

FIND_PROGRAM(CCACHE_EXECUTABLE ccache DOC "path to the ccache executable")
MARK_AS_ADVANCED(CCACHE_EXECUTABLE)

# TODO handle version

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS( CCACHE
	REQUIRED_VARS CCACHE_EXECUTABLE
	VERSION_VAR CCACHE_VERSION
)

# FindCCACHE.cmake ends here
