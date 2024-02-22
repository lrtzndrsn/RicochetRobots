
## About 
This is an (object oriented) F#-implementation of the popular game Ricochet Robots.

This work was conducted in 2021 together with my buddy Jonas Kramer (kram@itu.dk) and was one of our earliest larger coding projects.

More info about the project is to be found in the docs folder.
<img width="742" alt="image" src="https://github.com/lrtzndrsn/RicochetRobots/assets/123448847/06dfbb47-2f90-4cdd-b8db-a47e2e25056a">



## How to run this program
1. Open your cmd, e.g. Mono cmd
2. Locate the src-directory. Here is where the files needed to run to program are located.
3. Compile the files using the following command: fsharpc filename1.fs filename2.fsx
4. Run the program by executing the .exe-file using the following command: mono filename2.exe
5. Play the game and have fun!

## Alternative guide if the above approach did not work:
1. Open your cmd
2. Locate the src-directory. Here is where the files needed to run to program are located.
3. run dotnet fsi
4. enter the following in your cmd in f# interactive mode:
```
#load "robots.fs";;
#load "robots-game.fsx";;

```
5. Play the game and have fun! 
