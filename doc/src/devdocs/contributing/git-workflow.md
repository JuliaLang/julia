# Git workflow recommendations

### Git Recommendations For Pull Requests

 - Avoid working from the `master` branch of your fork. Create a new branch as it will make it easier to update your pull request if Julia's `master` changes.
 - Try to [squash](https://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html) together small commits that make repeated changes to the same section of code, so your pull request is easier to review. A reasonable number of separate well-factored commits is fine, especially for larger changes.
 - If any conflicts arise due to changes in Julia's `master`, prefer updating your pull request branch with `git rebase` versus `git merge` or `git pull`, since the latter will introduce merge commits that clutter the git history with noise that makes your changes more difficult to review.
 - Descriptive commit messages are good.
 - Using `git add -p` or `git add -i` can be useful to avoid accidentally committing unrelated changes.
 - When linking to specific lines of code in discussion of an issue or pull request, hit the `y` key while viewing code on GitHub to reload the page with a URL that includes the specific version that you're viewing. That way any lines of code that you refer to will still make sense in the future, even if the content of the file changes.
 - Whitespace can be automatically removed from existing commits with `git rebase`.
   - To remove whitespace for the previous commit, run
     `git rebase --whitespace=fix HEAD~1`.
   - To remove whitespace relative to the `master` branch, run
     `git rebase --whitespace=fix master`.

#### Git Recommendations For Pull Request Reviewers

- When merging, we generally like `squash+merge`. Unless it is the rare case of a PR with carefully staged individual commits that you want in the history separately, in which case `merge` is acceptable, but usually prefer `squash+merge`.
