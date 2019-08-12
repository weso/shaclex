# Guide for contributors

This project follows a standard [fork and pull][fork-and-pull] model for accepting contributions via
GitHub pull requests:

0. [Pick (or report) an issue](#pick-or-report-an-issue)
1. [Write code](#write-code)
2. [Write tests](#write-tests)
3. [Submit a pull request](#submit-a-pull-request)

## Pick or report an issue

We always welcome bug reports and feature requests—please don't feel like you need to have time to
contribute a fix or implementation for your issue to be appreciated.

## Write code

We prefer functional programming for the code. 

* Code and comments should be formatted to a width no greater than 100 columns.
* Files should not contain trailing spaces.
* Imports should be sorted alphabetically.

When in doubt, please run `sbt scalastyle` and let us know if you have any questions.

## Write tests

Shaclex uses [ScalaTest][scalatest] for testing.
 
## Submit a pull request

* Pull requests should be submitted from a separate branch (e.g. using
  `git checkout -b "username/fix-123"`).
* In general we discourage force pushing to an active pull-request branch that other people are
  commenting on or contributing to, and suggest using `git merge master` during development. 
  Once development is complete, use `git rebase master` and force push to [clean up the history][squash].
* The first line of a commit message should be no more than 72 characters long (to accommodate
  formatting in various environments).
* Commit messages should general use the present tense, normal sentence capitalization, and no final
  punctuation.
* If a pull request decreases code coverage more than by 2%, please file an issue to make sure that
  tests get added.
  
This guide for contributors is inspired by [circe's guide](https://github.com/circe/circe/blob/master/CONTRIBUTING.md).
