# ps-info

A library for accessing process and system statistics (e.g. cpu and memory usage) on Linux and Darwin based systems. 

## Notes on usage

On darwin-based system `sudo` is required.

## Known Issues

### PsInfo.Darwin.Libproc

- getProcTaskInfo
    - All values appear as `0` if `sudo` is not used.

### PsInfo.Darwin.Mach

- *all*
    - Only works for *self*
        - Not fixed by using `sudo` or entitlements
