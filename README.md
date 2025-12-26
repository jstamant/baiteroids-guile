# Baiteroids

A spin on the classic Asteroids arcade game! Keep yourself safe by smashing
asteroids together as they gravitate towards you!

I've decided on abandoning this project, as I kept running into issues with
[Guile Hoot](https://spritely.institute/hoot/). It works pretty well, but it
really took all the fun out of programming in Guile Scheme.

I would add SRFIs and try using built-in functions to be met with Hoot throwing
compilation errors.

![screenshot](https://raw.githubusercontent.com/jstamant/baiteroids-guile/refs/heads/main/screenshot.png)

I might continue this project in something that has better support for the
browser.

## Features

Lol?

- [x] Player movement
- [x] Asteroid movement
- [x] Asteroid spawning
- [ ] Gravity (the core mechanic)
- [ ] Collisions
- [ ] Score keeping

## Running

The fastest way to get everything you need is to use [GNU
Guix](https://guix.gnu.org), a wonderful package manager written in
Scheme.

Once you have Guix, the development environment with all necessary
dependencies can be created:

```
guix shell
```

Alternatively, there's also a Nix flake if you have access to Nix:

```
nix develop
```

To build the game, run:

```
make
```

To launch a development web server, run:

```
make serve
```

To check if the program works, visit https://localhost:8088 in your web browser.
Using Mozilla Firefox or Google Chrome is recommended. Hoot is not supported on
Safari at this time.

When it's time to publish the game to itch.io, run:

```
make bundle
```

## Attribution

This game is built from David Thompson's [Guile Hoot Game Jam
Template](https://gitlab.com/spritely/guile-hoot-game-jam-template).

