# givegif

[![Build Status](https://travis-ci.org/passy/givegif.svg?branch=master)](https://travis-ci.org/passy/givegif)

> Find and display GIFs from the command line.

Displaying the GIF inline requires [iTerm >=2.9](https://www.iterm2.com/downloads.html).

[![Power by Giphy](media/giphypowered.png)](https://github.com/Giphy/GiphyAPI)

## Demo

![](https://github.com/passy/givegif/raw/master/media/usage.gif)

## Usage

```
Usage: givegif [-n|--no-preview] ([-s|--search ARG] | [-t|--translate ARG] |
               [RANDOM_TAG])
  Find GIFs on the command line.

Available options:
  -h,--help                Show this help text
  -n,--no-preview          Don't render an inline image preview.
  -s,--search ARG          Use search to find a matching GIF.
  -t,--translate ARG       Use translate to find a matching GIF.
  -V,--version             Show version information
```

## Installation

### Homebrew

```bash
brew tap passy/givegif
brew install givegif
```

### Binaries

Check out the latest [releases](https://github.com/passy/givegif/releases) for
precompiled binaries.

### Stack

Alternatively, feel free to build it yourself with
[stack](http://haskellstack.org).


```bash
$ stack install givegif
$ givegif -V
givegif 1.0.2.0
```

## Known Issues

Even though I tried to make this work with screen/tmux, it still glitches out
every now and then. If you can figure out why, please let me know.

## License

BSD-3
