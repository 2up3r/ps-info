# ps-info

A library for accessing process and system statistics (e.g. cpu and memory usage) on Linux- and Darwin-based systems. 

## Notes on usage

On darwin-based system we need priveliges for getting process information.

First install the library

```sh
$ cabal install
```

In the ouput it will list a location of the executable `libproc-helper-exe`.
Use this path and run:

```sh
$ sudo chown root <libproc-helper-exe>
$ sudo chmod u+s <libproc-helper-exe>
```

Now you are good to go!

## Known Issues

### PsInfo.Darwin

- getCPUUsage
    - May throw error if the delay is under 1s, because of insufficient updates to the ticks from the kernel.

### PsInfo.Darwin.Libproc

- getProcTaskInfo
    - All values appear as `0` if `root` user is not executing.
