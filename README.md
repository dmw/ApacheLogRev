Apache Log Rev
=====

Summary
---

Apache Log Revision is a command line tool that generate stats
reports for your Apache Access Log files. It provides Request
Status Pie Chart, Bytes Percent Bar Chart, Country Percent Pie
Chart, Day Based History Bar Chart among other Apache Acces Log
based charts.

For Country Based Charts, you must use the GeoLiteCity.dat
country database from Max Mind GeoIP software.


Installation
---

### What's in the Archive?

This archive contains a Haskell Cabal package, so you can download
it and install it using the cabal command.

### Install Instructions

```shell

tar xzvf logrev.tar.gz
cd logrev
cabal install

```

### Build (Only Build) And Test Instructions

```shell

tar xzvf logrev.tar.gz
cd logrev
cabal clean
cabal configure
cabal build
./dist/build/logrev/logrev --input=/var/log/apache2/any_access_log.log --output=report

```

Usage
---

### Command Help

```shell

logrev --help

logrev
  -i FILE  --input=FILE   input file
  -o FILE  --output=FILE  output file
  -g FILE  --geo=FILE     GeoIP database file
  -v       --verbose      verbose output
  -V       --version      displays program version
  -h       --help         displays this message

```

Contribution
---
Contributions are always welcome, please follow these steps to submit your changes:

1. Install git from [http://git-scm.com/]()
2. Create a github account on [https://github.com]()
3. Set up your git ssh key using these instructions [http://help.github.com/set-up-git-redirect]()
4. Open the Apache Log Rev project home page on github on [https://github.com/dmw/ApacheLogRev.git]()
5. Click the "Fork" button, this will get you to a new page: your own copy of the code.
6. Copy the SSH URL at the top of the page and clone the repository on your local machine
7. The oficial "development" branch is "devel", so you should place your pull requests there.

```shell
git clone git@github.com:dmw/ApacheLogRev.git logrev
```

7. Create a branch and switch to it

```shell
cd logrev
git branch my-logrev-branch
git checkout my-logrev-branch
```

8. Apply your changes, then commit using a meaningful comment, that's the comment everybody will see!

```shell
git add .
git commit -m "Fixing issue 666, bleh bleh bender bleh"
```

9. Push the changes back to github (under a different branch, here myfeature-patch)

```shell
git push origin my-logrev-branch
```

10. Open your forked repository on github at https://github.com/your-username/ApacheLogRev.git
11. Click "Switch Branches" and select your branch (my-logrev-branch)
12. Click "Pull Request"
13. Submit your pull request to ApacheLogRev Developers

Samples
---

![Country Percentage Report](https://github.com/dmw/ApacheLogRev/raw/master/report_country.png)

![Request Status Percentage Report](https://github.com/dmw/ApacheLogRev/raw/master/report_status.png)


Support
---
We offer limited support at [http://coder.cl/products/logrev/](http://coder.cl/products/logrev/)

License
---
Licensed under the BSD3 License


Authors
---

 Copyright(c) 2012 [Daniel Molina Wegener](https://github.com/dmw) [http://coder.cl/](http://coder.cl/)


