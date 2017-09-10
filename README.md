# fingerdaemon

fingerdaemon project from final chapter of [Haskell Book](http://haskellbook.com/)

### Background

Pre the public internet era, the finger service was a way to figure out how to contact colleagues or other people on the same computer network and whether they were on the network at a given time. `finger` was originally intended to be used to share an office number, email address and basic contact details.

### Project

The fingerdaemon (a `daemon` is a process that runs in the background) acts as a simple server for a finger client where data is stored in a SQLite database (instead of on the OS) and returns information on a user.


### Running Locally

The project is built using [stack](https://docs.haskellstack.org/en/stable/README/), a tool to build Haskell projects and manage dependencies (instructions on installing can be found [here](https://www.youtube.com/watch?v=sRonIB8ZStw)).

Clone the repo and run `stack build` to install the dependencies and build the executables.

### Executables

#### `debug`

Debug is a basic echo server to test the connection. To run it:

```
> sudo `stack exec which debug`
```

Type in a message and it should echo the message back.

#### `dbuser`

This sets up the SQLite database.

A user has the following fields:

+ `username`
+ `shell` (e.g. /bin/zsh)
+ `homeDirectory` (e.g. /Users/Andrew)
+ `realName`
+ `phone`


+ create the database with: `stack exec dbuser -- init`
+ add a user with: `stack exec dbuser -- add username shell homeDirectory realName phone`
+ update a user with: `stack exec dbuser -- update username shell homeDirectory realName phone`
+ show all users with `stack exec dbuser -- all`


#### `fingerdaemon`

After setting up the database run the fingerdaemon with

```
> sudo `stack exec which fingerdaemon`
```

in another shell session you can type

```
> finger andrew@localhost
```

if that user exists in the db it will send back the user details for that user
