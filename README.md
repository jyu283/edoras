# edoras
UCSD CSE 230 (Fall Quarter 2022) Group Project

![image](https://user-images.githubusercontent.com/37025108/200975468-7ba7b82c-067f-431c-84ff-d1515ca37902.png)


We propose a Haskell re-implementation of [the Dinosaur Game](https://en.wikipedia.org/wiki/Dinosaur_Game)
in the Google Chrome browser, where the player guides a pixelated T-Rex across a side-scrolling landscape, accumulating
points while avoiding various types of obstacles along the way. We have yet to investigate
the technical requirements of this program, but we expect the bulk of the implementation
to rely on [the brick library](https://github.com/jtdaugherty/brick/).

At a bare minimum, we will make an effort to reproduce the complete feature set of the original game,
which contains limited mechanisms for movement (jump and duck) and obstacle generation (cacti and Pteranodons).
Areas where we consider introducing additional complexities include: uneven terrain, coins (as seen in Mario games),
and even dual-player mode. The number of additional features we can achieve will become clearer as we familiarize
ourselves with the brick library and its capabilities.
