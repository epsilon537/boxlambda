---
layout: post
title: 'Git Workflow and Setup.'
comments: true
---

Git Workflow
------------

BoxLambda is a GitHub project that depends on a bunch of other GitHub projects. How do I pull it all together under one umbrella? I don't just want to copy somebody else's code and submit it into my repository. That would be impolite and I would lose all tracking with the original project. I want to be able to track the original project, make my own changes, and integrate the project into the BoxLamba repository.

**Git submodules** are a great solution for this situation. Submodules allow you to keep a git repository in a subdirectory of another git repository. When you're making changes inside the submodule subdirectory, those get committed to that submodule's repository. The parent (supermodule?) repository on the other hand, just tracks submodule commits. From git's point of view, the submodule subdirectory is not a subdirectory, it's a file with a reference to a git repository and a specific commit within that repository.

OK, I guess that sounds pretty confusing. Here's a much better explanation:

[https://git-scm.com/book/en/v2/Git-Tools-Submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules)

### Directories and branches

I'll be using the following directory layout in the BoxLambda repository:

```
boxlambda/doc
boxlambda/fpga/ibex (ibex fork git submodule)
boxlambda/fpga/wbuart32 (wbuart32 fork git submodule)
boxlambda/fpga/<other FPGA git submodules>
boxlambda/fpga/<BoxLambda specific FPGA files that don't fit in any of the submodules> 
boxlambda/sw/<SW fork git submodules>
boxlambda/sw/<BoxLambda SW files that don't fit in any of the submodules> 
```

Each of the git submodules is a fork of a GitHub project discussed in earlier posts. For example, *boxlambda/fpga/ibex/* contains [my ibex fork](https://github.com/epsilon537/ibex), not the [original ibex repository](https://github.com/lowRISC/ibex).

In each of the forked submodules, two branches are relevant:

- **master**: I'm keeping the master branch in sync with the master branch of the repository I forked from. Having this branch makes it easy to pull in updates as well as to submit the occasional pull request to the original project.
- **boxlambda**: On this branch, I'll be making changes for BoxLambda.

In the BoxLambda repository itself, I have the following long-running branches:

- **master**: I will submit releases to this branch. The master branch should always be in good shape.
- **develop**: This is where the work is happening. Things will be in flux here. This branch will not always be in good shape.
- **gh-pages**: This branch holds the BoxLambda Blog files. GitHub Pages are by default on the *gh-pages* branch of a GitHub project.
- **boxlambda-gh-pages-wip**: This branch holds work-in-progress Blog updates. This branch also contains some config file modifs specifically for local previewing, which is why this is a long-running branch, rather than a topic branch. When updates are ready for release, I merge them to *gh-pages*. 

I already pushed this structure to GitHub. Feel free to take a look around:

[https://github.com/epsilon537/boxlambda](https://github.com/epsilon537/boxlambda)

GitHub does a great job displaying submodule subdirectories:

[https://github.com/epsilon537/boxlambda/tree/develop/fpga](https://github.com/epsilon537/boxlambda/tree/develop/fpga)

My Setup
--------

I'm working on Ubuntu WSL on Windows 11. It would be better to work on a native Linux box, but I need to be on Windows for other work, so WSL it is.

WSL is working well for me. My C: drive shows up as */mnt/c* under Linux, so sharing files between Linux and Windows is easy. The clipboard also works seamlessly between Windows and Linux and the Linux apps run right inside the Windows desktop.

Xilinx's Vivado installation was straightforward. As a test, I built Ibex's Arty A7 example using the [README](https://github.com/lowRISC/ibex/blob/master/examples/fpga/artya7/README.md) instructions. Synthesis, implementation, and bitstream generation went just fine. 

However, when I tried to program the bitstream on my Arty A7 board, connected via USB, I noticed that Vivado wasn't detecting the board. *Ugh*. WSL is not perfect after all.

As a workaround, I installed the Vivado Lab edition on the Windows side. Unlike a regular Vivado installation, the Lab edition is very small. It's intended for lab machines physically connected to FPGA targets. With the Vivado Lab edition on Windows, I can launch the hardware server, **hw_server.bat**, on the Windows side. The hardware server on the Windows side is detecting my USB connected target just fine. I can connect to the hardware server from Vivado on the Linux side by IP address. 

![Connecting to Target from Vivado on WSL](../assets/ConnectingToHWfromVivadoWSL.jpg){:class="img-responsive"}
*Connecting to Target from Vivado on WSL*

I look up the IP address to use in */etc/resolv.conf*. I have noticed the IP address varies from session to session.

It is a bit clumsy but good enough for me for the time being. If anybody has figured out how to make Vivado on WSL detect USB-connected targets, let me know, please.

With this workaround in place, I was able to download Ibex Arty A7 example bitstream. It's a simple Blinky type of example.

### Tools

I'm currently using the following tools:

- Vivado ML Edition V2021.2, Linux version.
- Vivado Lab Edition V2021.2, Windows version (for the hardware server).
- RISCV Compiler Toolchain **rv32imcb**. This is the cross compiler for building the code that'll run on the Ibex processor. I'm using the pre-built binaries from *lowRISC*: 

	[https://github.com/lowRISC/lowrisc-toolchains/releases](https://github.com/lowRISC/lowrisc-toolchains/releases)

To be able to build the Ibex Arty A7 example, I also installed *fusesoc*, but I don't intend to use this tool in BoxLambda:

[https://fusesoc.readthedocs.io/en/stable/user/installation.html](https://fusesoc.readthedocs.io/en/stable/user/installation.html)

Interesting Links
-----------------

[https://git-scm.com/book/en/v2](https://git-scm.com/book/en/v2): If you're using git and GitHub but you often find yourself googling *'how do I do ... in git'*, do yourself a favor and read this book. You need some background info to be able to make sense of git, especially if you're used to another VCS (Subversion, Perforce, etc.). You'll find much of the same terminology being used in git, but it doesn't have the same meaning. A git branch, for instance, is just a pointer to a specific commit, a commit is a snapshot of the entire branch, and *checkout* is a command to switch between branches. Also, because of its decentralized nature, there are a bunch of git concepts that don't exist in other VCSs. So, go read that book. You'll be glad you did.
