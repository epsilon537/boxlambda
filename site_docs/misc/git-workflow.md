# Git Workflow

All GitHub repositories used by BoxLambda are instantiated in BoxLambda's repository as git submodules. The git submodules are located in the `sub/` directory:

```
sub
├── ibex
├── ibex_wb
├── wbuart32
└── ...
```

Each git submodule is a fork of a GitHub project. For example, `sub/ibex/` contains [my ibex fork](https://github.com/epsilon537/ibex), not the [original ibex repository](https://github.com/lowRISC/ibex).

In each forked submodule, there are two relevant branches:

- `master`: I keep this branch in sync with the master branch of the original repository to make it easy to pull updates and submit occasional pull requests.
- `boxlambda`: This branch is where modifications for BoxLambda will be made.

In the BoxLambda repository, there are several long-running branches:

- `master`: Releases are submitted to this branch, ensuring that it's always in good shape.
- `develop`: Active development happens on this branch, with changes being constantly made, so it may not always be stable.
- `gh-pages`: This branch holds the BoxLambda Blog files. By default, GitHub Pages use the `gh-pages` branch of a project.
- `boxlambda-gh-pages-wip`: Work-in-progress Blog updates and some config file modifications for local previewing are in this branch. When updates are ready for release, they're merged to the `gh-pages` branch.

# Adding Submodules

To add a new git submodule:

1. Fork the repo on GitHub.
2. In your forked repo, create a `boxlambda` branch and set it as default.
3. Run the following commands in the root directory of your local BoxLambda repository:

```bash
git submodule add <URL to forked repo> sub/<submodule name>
git commit -m 'Add submodule <submodule name>'
cd sub/<submodule name>
git remote set-url origin git@github.com:epsilon537/<repo name>.git
```

The last step is necessary to switch the remote URL from HTTPS to SSH for authentication preference.

# Creating a new release (*Note to Self*)

On `develop` branch:

1. Update `CHANGELOG.md`.
2. In `sim_a7_100` build tree, build all and run ctest.
3. In `arty_a7_100` build tree, build all.
4. (Re)Populate `binaries/` by runing `export_binaries.sh` from the repo root directory.
5. Flash `boxlambda_base` gateware project build and bootloader.
6. Run the `boxkerntestfs` software test suite in `boxlambda_os` software project build directory.
8. Run `export_docs.sh develop`.
7. Check documentation links by running `lychee -i *.md` from the `site_docs/` directory.

On `master` branch:

1. Merge `develop`.
2. In `sim_a7_100` build tree, build all and run ctest.
4. `git tag vX.Y.Z`.
5. In `arty_a7_100` build tree, build all.
6. Flash `boxlambda_base` gateware project build, `bootloader`, and `boxlambda_os`.
7. Copy the contents of directory `fs/` to an SD card and boot the target from that SD card.
8. Check bootloader version string.
9. Run the `boxkerntestfs` software test suite in `boxlambda_os` software project build directory.
10. (Re)Populate `binaries/` by runing `export_binaries.sh` from the repo root directory.
11. `git commit` the binaries.
12. Run `export_docs.sh vX.Y.Z` from the repo root directory.
12. Update tag to include the new binaries and exported docs: `git tag -f vX.Y.Z`.
13. `git push origin vX.Y.Z`.
14. On GitHub boxlambda repo, navigate to *Releases*, then *Draft a new release*.
15. On GitHub boxlambda repo, update/close open issues.
16. In read-the-docs, create a new released based on the vX.Y.Z tag.


