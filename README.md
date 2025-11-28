# file-server
A simple file server web application in Common Lisp.

## Overview
The purpose of this file server is not the file server itself, but allowing an interative process for developing database-backed, Role-Based Authorization Control (RBAC). If you want a real file server, you're probably better off looking elsewhere.

This code allows for rapid iteration. The goal is to develop a way to quickly develop apps to solve some kinds of problems.

Currently, this code uses HTML and CSS (little or no JavaScript). But the HTML is represented in Common Lisp and so is the CSS. See the file-server.lisp and css.lisp files. The SQL remains SQL because using Common Lisp instead does not significantly speed up or otherwise improve the development process.

JavaScript, when introduced, will likely be represented as Common Lisp as well.

The code base is currently pretty close to pure Common Lisp, which significantly improves development speed.

## Features

As a file server, this service provides the following features:

- User management.
- User roles: admin group has access to everything.
- Public role: anyone who's not logged in is treated as the user "guest", and has access to public directories (directories with the "public" role).
- File uploads, downloads and directory creation and deletion are supported.
- Directories are visible only to users who have access to those directories.
- Directories can have any number of roles. Users with those roles will have access to the directory.
- Users can have any number of roles.
- Roles have permissions.
- In order to perform a function in a directory, the user and the directory must share a role. The role's permissions dictate what the user can do there.
- Users can assign roles to directories.
- Files do not have roles, and are accessible to anyone with access to the file's parent directory
- Admin can add users and roles.
- The directory listing shows roles for each directory.


## Development
This software runs in a Kubernetes cluster. It uses one pod for PostgreSQL and another for the file server code, which is written in Common Lisp.

During development, you use Slime or some such to connect your code editor to the running Lisp image in the Kubernetes pod, then modify the code. As you work on the code, you can see results immediately in the server (without having to reload code or restart the server). This is normal in Common Lisp development, and allows for rapid-iteration development.

You must develop in the dev branch.

## Build
To build and deploy the system, use the scripts/build.sh script. Run it from the repo's root like this:

```sh
scripts/build.sh
```

That deploys to the development environment, for example https:/files-dev.sinistercode.com.

To deploy to the development environment, you must be in branch `dev`.

When you're happy with the code you've developed, you can move it to production by merging your code to master, switching to the master branch, then running this:

```sh
scripts/build.sh --env prod --latest
```

To deploy to master (`--env prod`), you must be in branch `master`.

That basically updates the production version to whatever the latest development version is and deploys the version to production.

Use `scripts/build.sh --help` for more information.

## Project Structure

The code is in the lisp directory.

The charts directory contains the Helm charts for deploying the code.

The scripts directory includes the build script.

There are dev and prod directories that contain deployment resources, including directories that are mounted for holding database files, uploaded files, logs, and other web site resources. These directories are mounted on the file-server pod and accessible there via the Common Lisp application code.

Most of the files in the kube directory (under dev or prod) are obsolete, because the application deploys using Helm now.
