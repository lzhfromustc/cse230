**How to install dependencies**

`cabal update`

`sudo apt-get install libtinfo-dev`

`cabal install brick`

I also used `cabal install --lib brick` and `cabal install brick -f demos`

**How to learn Brick**

1. Read its README.md and guide.rst

2. See BorderDemo.hs, a good demo for just drawing something: const [ui]

3. See CustomEventDemo.hs, a good demo for taking input and change state.