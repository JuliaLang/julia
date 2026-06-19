---
name: test-changes
description: Run and update Julia tests after changing test files. Use when adding to or modifying any test file under test/ or stdlib/**/test/.
---

# Julia test changes

Use this when you change or add a Julia test.

- If you have changed a test (e.g. `foo`), run `make test-revise-foo` for the
  corresponding test to ensure that the test is still passing with your changes.
- If you are adding a new test, add it to an existing test file. Do not create a
  new test file unless explicitly instructed.
- Write one comment at the top of the test to explain what is being tested.
  Otherwise keep comments minimal.
- Use the environment variable `JULIA_TEST_FAILFAST=1` to make tests fail fast.
