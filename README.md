# dev-team-sim

## 🖥️ Code

[![Edit with Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/HadrienMP/dev-team-sim)

### ❄️ Nix and Direnv
This project uses [Nix](https://nixos.org/) and [direnv](https://direnv.net/) for its tools. Install them to code on your machine.

### 👎 Without nix
Install:
- [just](https://github.com/casey/just), the command launcher
- [node 18](https://nodejs.org/en)
- [yarn](https://www.npmjs.com/package/yarn)

To stay up to date with future tools, we recommend using nix. Or check the new packages added

### 🏃 Commands
```bash
just install # to force install (done by direnv already)
just start # To start in dev mode
just test # Launch automated tests
just test --watch # In watch mode
```
