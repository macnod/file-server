# file-server
A simple file server web application in Common Lisp.

## Overview
The purpose of this file server is not the file server itself, but allowing an interative process for developing database-backed, Role-Based Authorization Control (RBAC). If you want a real file server, you're probably better off looking elsewhere.

This code allows for rapid iteration. The goal is to develop a way to quickly develop apps to solve some kinds of problems.

As a file server, this service provides the following features:

- User management
- User roles: admin group has access to everything
- Public role: anyone who's not logged in is treated as the user "guest", and has access to public directories (directories with the "public" role).
- File uploads, downloads and directory creation and deletion are supported
- Directories are visible only to users who have access to those directories.
- Directories can have any number of roles. Users with those roles will have access to the directory.
- Users can have any number of roles.
- Roles have permissions.
- In order to perform a function in a directory, the user and the directory must share a role. The role's permissions dictate what the user can do there.
- Users can assign roles to directories
- Files do not have roles, and are accessible to anyone with access to the file's parent directory.
- Admin can add users and roles
- The directory listing shows roles for each directory


## Development
This software runs in Kubernetes cluster. It uses one pod for PostgreSQL and another for the file server code, which is written in Common Lisp.

During development, the developer uses Slime or some such to connect their code editor to the running Lisp image in the kubernetes pod, then adds, removes, or modifies functions. As the developer works on the code, they see results immediately in the server (without having to reload code or restart the server). This is normal in Common Lisp development.

## Build
To build and deploy the system, use the scripts/build.sh script. Run it from the repo's root like this:

```sh
scripts/build.sh
```

That deploys to the development environment, for example https:/files-dev.sinistercode.com.

When you're happy with the code you've developed, you can move it to production like this:

```sh
scripts/build.sh --env prod --latest
```

That basically updates the production version to whatever the latest development version is and deploys the version to production.

Use `scripts/build.sh --help` for more information.
