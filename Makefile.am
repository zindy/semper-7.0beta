
        SUBDIRS = Commands Graphics Kernel Middle ProgramM UserInterface \
                        Locals Patches Primitives Main Examples Test
# Add distribution directories
        DIST_SUBDIRS = Commands Graphics Kernel Middle ProgramM UserInterface \
                        Locals Primitives Main Includes Patches \
                        Demo Tutorial Examples Test

        EXTRA_DIST = AUTHORS CONTRIBUTE CHANGES BUGS NEWS README THANKS \
                        ChangeLog Current_Issues.txt ToDo f2c IO \
                        GettingStarted VersionChanges bug_list More_ToDo Readme.MacOS

# Extra clean (in case)
        CLEANFILES = Patches/*.o Patches/*.a


# Making the help structure
.PHONY : help
help:
	cd $(prefix)/help ; echo Making On-line Help ; $(exec_prefix)/bin/LinkHelp 1>/dev/null 2>Help.log ; \
        sh $(exec_prefix)/bin/Helpcom.sh 1>/dev/null 2>>Help.log

# Add reminder to user to do make help
install-exec-hook:
	echo " "
	echo "           *************"
	echo "Semper 7 has now been installed"
	echo "Remember to run make help to create help links"
	echo "           *************"
