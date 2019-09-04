## Submission

We are resubmitting a small update, `DeclareDesign 0.18.0`, with bug fixes
and the addition of a small new feature. 

The package was returned with two errors on Windows. Our apologies.
- we have deleted the revdep folder, which was included accidentally.
- we cannot replicate the problem that estimatr was not available; we have had
a successful test from winbuilder on devrel which was where the error was reported.
we are assuming this was a temporary failure. Please let us know if you see 
another cause.

Thank you for your time reviewing the submission.

## Test environments
* local OS X install (release)
* ubuntu 12.04 on travis-ci (devel, release, oldrel)
* OS X on travis-ci (release, oldrel)
* win-builder (devel, release, oldrel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We see no problems with our reverse depends.

---
  