# [Ceramic](http://ceramic.github.io/)

Common Lisp web apps on the desktop.

## How this branch is different

This repository is a fork of the original https://github.com/ceramic/ceramic. It was made to
find an ability to build end-user application on OS instead of tar archive.

### Goals

Right now I'm going to use [electron-packager](https://github.com/electron-userland/electron-packager) for downloading
Electron and packaging application.

Secondary goal is to upgrade Electron's version from 1.2.7 to 2.x.

### Changes

* Now Ceramic uses log4cl and produces more logging in place where application
  is commbunicating with JS part.
* Part of bundler functionality was modified to use with ceramic.ros script from command line.

  Previos way to make application is broken now.


## Overview

Ceramic takes an ordinary Common Lisp web app, and bundles it into an executable
you can distribute, so your users can install it and run the app and server in a
dedicated desktop window like any other app.

See the [documentation](http://ceramic.github.io/docs/introduction.html).

## License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
