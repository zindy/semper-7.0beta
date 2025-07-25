# Software Installation Guide

## EDM, Semper, and Numis Installation Instructions

## Prerequisites

If you do not have a unix system (Linux or Mac), you will need to download Cygwin, which is basically a unix emulator for windows or use the WLS structure in Windows 10. If you do have a unix system, you can skip down to the CVS part. Note: while it is convenient to have cygwin on your laptop, you can also use X-windows to connect to a linux box elsewhere.

## Cygwin Installation

1. Go to [www.cygwin.com](http://www.cygwin.com) and click the icon that says "Run setup.exe", downloading it to (for instance) your desktop

2. When you run this program there will be a series of prompts:
   a. Choose "Install from Internet"
   b. Choose "C:\cygwin" for the root directory. The other recommended options are fine.
   c. Choose whatever you want for the package directory. I usually put "C:\cygwin\install"
   d. Direct connection
   e. Any mirror is fine, e.g. Argonne's web server "http://mirror.mcs.anl.gov" as it's close by.

3. The next screen will be a categorical list of everything that can be installed. I recommend clicking on the arrows next to "All" so that everything is installed. You only need to click once, but it takes a few seconds to tick everything on. A full installation takes about 3 GB of disk space. What will probably be enough in most cases is to click on the two sections "Development" and "Xwindows" and install these two completely.

   In any case, there are certain things you may need to skip installing. In the "Libs" category, find the packages "fftw3:Discrete Fourier transform library" and "fftw3-doc: Pdf and html documentation for..." and click their version arrows twice so that it says they'll be skipped. In the "Math" category, skip "libfftw3-devel" and "libfftw3_3".

4. It will tell you that certain things are dependent on having the packages you skipped. Uncheck the (recommended) option to install the dependencies. It will ask you again if you want to proceed despite these conflicts, and say yes.

5. At this point everything will download in one progress bar. A second progress bar will monitor the setup, and a third will show the status of installation.

6. If everything installs successfully you should be able to place shortcuts on your start menu and desktop if you like.

## WLS

With Windows 10 there is a WLS linux system which allows you to use whatever system you want (Ubuntu, Debian etc) within windows. While the original version was a bit slow, the 2nd generation is significantly faster particularly if virtualization is enabled. This is somewhat complicated, so I suggest that you look it up on the web. If you are familiar with linux it is quite straight forward.

## CVS Access

The Concurrent Version System is a method of maintaining software distribution through changes in code. Some people (e.g. members of the LDM group and collaborators) have full access to the code so can download it, make improvements, then upload their improvements to the central server. This means that the job of maintaining the software is (hopefully) split, and improvements at (for instance) Oxford can be used by someone in India.

There are two ways to access the code:

a. If you have an account and are a developer, you can access using your account/password combination. In this case you can both checkout the code, refresh your version with improvements from others (the update command in cvs) or add your improvements (the commit command).

b. If you do not, you can do an anonymous checkout of the code and refresh your version but cannot add improvements for others to use.

In both cases, you can either set the appropriate parameters in your environment to download the files, e.g.

```bash
export CVSROOT=":pserver:MYACCOUNT@129.105.122.84:/home/cvsroot"
cvs checkout TheCode
```

or download directly, e.g.

```bash
cvs -d:pserver:MYACCOUNT@129.105.122.84:/home/cvsroot checkout TheCode
```

where "MYACCOUNT" is either your account name or anonymous, and "TheCode" is any of edm, Numis-2.0 or semper-7.0beta.

Rather similarly, to update you would use either

```bash
cvs update -Pd
```

(if you have setup CVSROOT) or

```bash
cvs -d:pserver:MYACCOUNT@129.105.122.84:/home/cvsroot update -Pd
```

## Compiling and Running EDM

1. To compile the code and install the program, use the following commands one at a time from the edm directory:
   a. Do a "cd fftw*" then type "./configure". This will check your system and set various flags -- make sure that there are no warnings/errors.
   b. Once that is done type "make"
   c. Finally type "make install"

2. Do a "cd ../forms*" and repeat ./configure, make, make install

3. Do a "cd ../" and repeat ./configure, make, make install

4. Click on (in the standard Start/Programs Menu) "CygwinX/Xwin Server" or type "XWin -multiwindow -clipboard -silent-dup-error &" (other unix will already have a server running) and X-windows should start in a different window and give you a command prompt.

5. Type "export PATH=$PATH:/usr/local/edm/bin". This should return you nothing and send you back to the prompt.

6. Type "edm" and edm should start.

### Important possible bug

There can be version mismatches of some files which will lead to complaints and the configure/make scripts not running. It may be just with libtool; if you have this then find the relevant version of libtool for your system (e.g. "which libtool") then copy this to the fftw/forms directory. You may also need to delete ltmain.sh

It can also be with some of the other files, for which an alternative (perhaps better) fix is to run

```bash
aclocal --force ; autoconf -f ; automake -a -c -f ; autoupdate
```

This forces the programs to use what is on your computer.

### Warning

More recent versions of the compilers gcc/g++/gfortran (release 10) are insisting on some features which are unusual. As of 4/20/2021 some changes have been made to the compilation options to hopefully cure this, but they might reappear.

## Compiling and Running Semper

1. To compile the code and install the program, use the following commands one at a time from the semper7 directory:
   a. First type "./configure". This will check your system and set various flags -- make sure that there are no warnings/errors.
   b. Once that is done type "make"
   c. Finally type "make install" then "make help"

2. Click on (in the standard Start/Programs Menu) "CygwinX/Xwin Server" or type "XWin -multiwindow -clipboard -silent-dup-error &" (other unix will already have a server running) and X-windows should start in a different window and give you a command prompt.

3. Type "export PATH=$PATH:/usr/local/semper-7.0b/bin". This should return you nothing and send you back to the prompt.

4. Type "semper7" and Semper should start.

## Compiling and Running Numis-2.0

1. To compile the code and install the program, use the following commands one at a time from the Numis-2.0 directory:
   a. First type "./configure". This will check your system and set various flags -- make sure that there are no warnings/errors.
   b. Once that is done type "make"
   c. Finally type "make install"

2. Add /usr/local/Numis-2.0/bin to your PATH (as before) and run the various commands. More documentation will be added about these.

## Warnings

1. These will not work if your computer is setup in Chinese, Japanese or Russian and there may also be problems with some other languages. The reason is that Chinese, Japanese and Russian all use 16bits for characters, and none of the code understands this.

2. You may have problems if there are spaces in your user name, and/or a mixture of upper and lower case letters.

3. Be aware that edm/semper use X-windows which in turn uses tcp communications. A firewall may consider this suspicious and block it.

## Some Shortcuts to Save Time with Cygwin

To open EDM and go straight to your data source using only two short command lines, follow these steps:

### Part 1
(Open EDM directly in XWin Server instead of going to Cygwin Bash Shell and typing startxwin)

1. Go to the folder in which you installed cygwin, for example, C:\cygwin\home\*yourname*.

2. Open the .bashrc file using WordPad.

3. Type "export PATH=$PATH:/usr/local/edm/bin" and click Save.

### Part 2
(Create a soft link to your data source so that you don't have to change directory every time you want to open a file in EDM.)

1. Find out the folder in which your files or subfolders are stored. That is your target directory.

2. Open Cygwin Bash Shell and type "ln -s {target directory} {shortcut name}"

Suppose that all your files are in C:\users\*yourname*\Desktop\myfolder, and you want to call your shortcut "myshortcut". Type:

```bash
ln -s /cygdrive/C/users/yourname/Desktop/myfolder myshortcut
```

Done! Open XWin Server, do a "cd myshortcut", then type "edm". When you open a file, you will already be in the folder in which your data are stored.

### Other Shortcuts for Cygwin

It is also useful to integrate cygwin and your windows environment better. One tool for this is the "chere" command, i.e. in a cygwin shell run "chere -fi". This will add an entry for right-click on directories to open a bash shell in that directory.

Also useful are some utilities in cygwin, for instance "cygstart". The command "cygstart ./" will open a standard Explorer view of your current directory. If you have included the correct path to "wordpad.exe" and/or "notepad.exe" in your .bashrc, then you can do "wordpad case.hkl" for instance to look at case.hkl.

In addition, sometimes you may want an editor. The programs "nano" and/or "pico". Alternatively, perhaps better, both semper-7.0 and Numis-2.0 have easyeditor, a command "ee" which is not bad but might not always work. For instance "ee case.hkl" can be used at the terminal. If you are unix proficient, there are several X-based editors available within cygwin. (It has been added to the latest releases.)
