This issue refers to Merge Request (insert MR reference)

*Checklist when the module is first submitted*

- [ ] Have the relevant labels been added to the MR
- [ ] If submitted on someone elses behalf, has the software author been referenced (if they have a GitLab account) 

*Checklist at module completion*

- [ ] Is the module documentation sufficiently detailed?
- [ ] Is it mergeable? (i.e., there should be no merge conflicts)
- [ ] Are the build instructions sufficient? (If not the MR should be updated)
- [ ] Did it pass the tests? (Are there unit/regression tests? Do they pass?)
- [ ] If it introduces new functionality, is it tested? (Unit tests?)
- [ ] Is it well formatted? (typos, line length, brackets,...)
- [ ] Is all new source code sufficiently documented? (functions, their arguments,...)
- [ ] Did it change any interfaces? Only additions are allowed without a major version increment (if >v1.0). Changing file formats also requires a major version number increment.
