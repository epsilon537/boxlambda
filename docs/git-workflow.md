Git Workflow
------------

All GitHub repositories used by BoxLambda are instantiated in BoxLambda's repository as git submodules. The git submodules are located in the *sub/* directory:

```
sub
├── ibex
├── ibex_wb
├── wbuart32
└── ...
```

Each of the git submodules is a fork of a GitHub project discussed in earlier posts. For example, *boxlambda/sub/ibex/* contains [my ibex fork](https://github.com/epsilon537/ibex), not the [original ibex repository](https://github.com/lowRISC/ibex).

In each of the forked submodules, two branches are relevant:

- **master**: I'm keeping the master branch in sync with the master branch of the repository I forked from. Having this branch makes it easy to pull in updates as well as to submit the occasional pull request to the original project.
- **boxlambda**: On this branch, I'll be making changes for BoxLambda.

In the BoxLambda repository itself, I have the following long-running branches:

- **master**: I will submit releases to this branch. The master branch should always be in good shape.
- **develop**: This is where the work is happening. Things will be in flux here. This branch will not always be in good shape.
- **gh-pages**: This branch holds the BoxLambda Blog files. GitHub Pages are by default on the *gh-pages* branch of a GitHub project.
- **boxlambda-gh-pages-wip**: This branch holds work-in-progress Blog updates. This branch also contains some config file modifs specifically for local previewing, which is why this is a long-running branch, rather than a topic branch. When updates are ready for release, I merge them to *gh-pages*. 
