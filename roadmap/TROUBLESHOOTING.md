# Troubleshooting

Generally grand amount of problems can be solved with a simple command:
```
elchemy clean
```

It cleans all of the caches, temporary files, dependencies and code file outputs out of the current project.

If
```
elchemy clean
elchemy init
mix compile
```

still yields compilation errors feel free to report an issue.
For the sake of being able to reproduce please provide:

- Elchemy version (`elchemy version`)
- Elixir version (`elixir -v`)
- Erlang version (first line in `erl`)
- Elm version (`elm -v`)
- Operating system (`uname -msr`)

It's also helpful to include:
- Result of `tree -L 2` inside your project folder
- Code of the file failing
