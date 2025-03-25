# ps-info

A library for accessing process and system statistics (e.g. cpu and memory usage) on Linux and Darwin based systems. 

## Known Issues

### PsInfo.Darwin.Libproc (getProcTaskInfo)

- only 0-values
    - need SUID (sudo) for values
    - signing notes
        - codesign --entitlements ps-info.entitlements -s - PATH
        - codesign -dvvv PATH
        - ./dist-newstyle/build/aarch64-osx/ghc-9.10.1/ps-info-0.1.0.0/build/PsInfo/Darwin/Libproc.o
        - ./dist-newstyle/build/aarch64-osx/ghc-9.10.1/ps-info-0.1.0.0/build/PsInfo/Darwin/Libproc.hi
        - ./dist-newstyle/build/aarch64-osx/ghc-9.10.1/ps-info-0.1.0.0/build/PsInfo/Darwin/Libproc.dyn_o
        - ./dist-newstyle/build/aarch64-osx/ghc-9.10.1/ps-info-0.1.0.0/build/PsInfo/Darwin/Libproc.dyn_hi
        - https://github.com/phracker/MacOSX-SDKs/blob/041600eda65c6a668f66cb7d56b7d1da3e8bcc93/MacOSX11.3.sdk/usr/include/sys/proc_info.h#L61

### PsInfo.Darwin.Mach

- KERN_FAILURE
    - maybe fixed by entitlements
