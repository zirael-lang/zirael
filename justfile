#!/usr/bin/env -S just --justfile

set windows-shell := ["powershell"]
set shell := ["bash", "-cu"]

_default:
    @just --list -u

fix:
    cargo clippy --workspace --fix --allow-staged --no-deps
    just fmt
    just check
    just lint
    git status

check:
    cargo check --workspace --all-features --all-targets --locked

lint:
    cargo clippy --workspace --all-targets --all-features --fix --allow-dirty -- --deny warnings

fmt:
    cargo shear --fix
    cargo fmt --all
    dprint fmt

comp-run args='':
    cargo run -p zirael playground/test.zr -d std=./std/src/lib.zr {{ args }} --name playground
