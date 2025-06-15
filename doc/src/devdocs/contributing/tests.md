### Writing tests

There are never enough tests. Track [code coverage at Codecov](https://codecov.io/github/JuliaLang/julia), and help improve it.

1. Go visit https://codecov.io/github/JuliaLang/julia.

2. Browse through the source files and find some untested functionality (highlighted in red) that you think you might be able to write a test for.

3. Write a test that exercises this functionality---you can add your test to one of the existing files, or start a new one, whichever seems most appropriate to you. If you're adding a new test file, make sure you include it in the list of tests in `test/choosetests.jl`. https://docs.julialang.org/en/v1/stdlib/Test/ may be helpful in explaining how the testing infrastructure works.

4. Run `make test-all` to rebuild Julia and run your new test(s). If you had to fix a bug or add functionality in `base`, this will ensure that your test passes and that you have not introduced extraneous whitespace.

5. Submit the test as a pull request (PR).

* Code for the buildbot configuration is maintained at: https://github.com/staticfloat/julia-buildbot
* You can see the current buildbot setup at: https://build.julialang.org/builders
* [Issue 9493](https://github.com/JuliaLang/julia/issues/9493) and [issue 11885](https://github.com/JuliaLang/julia/issues/11885) have more detailed discussion on code coverage.

Code coverage shows functionality that still needs "proof of concept" tests. These are important, as are tests for tricky edge cases, such as converting between integer types when the number to convert is near the maximum of the range of one of the integer types. Even if a function already has some coverage on Codecov, it may still benefit from tests for edge cases.
