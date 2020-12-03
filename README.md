#Functional programming laboratory work 3 Vilnius University

#Running

## Start two bots one against another
tic-tac-toe-defender $TASK_NUMBER $PATH_TO_YOUR_BOT_EXECUTABLE  
tic-tac-toe-offender $TASK_NUMBER $PATH_TO_YOUR_BOT_EXECUTABLE
```bash
tic-tac-toe-defender 9 C:\Users\Rytis\Desktop\funkc_prg_3\stack-project\.stack-work\dist\29cc6475\build\stack-project-exe\stack-project-exe.exe
```
```bash
tic-tac-toe-offender 9 C:\Users\Rytis\Desktop\funkc_prg_3\stack-project\.stack-work\dist\29cc6475\build\stack-project-exe\stack-project-exe.exe
```

## Start one bot with another input
$PATH_TO_YOUR_BOT_EXECUTABLE O  
enter  
$MY_CUSTOM_INPUT
```bash
C:\Users\Rytis\Desktop\funkc_prg_3\stack-project\.stack-work\dist\29cc6475\build\stack-project-exe\stack-project-exe.exe O 
d4:lastld4:datali1ei1e1:Oeee4:prevd4:lastld4:datali0ei0e1:Xeeeee
```
### Find custom message
http://tic-tac-toe.homedir.eu/arbitrary/3/MY_TASK_NUMBER?seed=ANY_SEED&limit=NM_OF_TURNS_DONE
```bash
http://tic-tac-toe.homedir.eu/arbitrary/3/9?seed=1119455557886915421&limit=2
```

## Build stack project 
cmd commands 
```bash
stack new my-project 
``` 
```bash
cd my-project
``` 
```bash
stack setup
``` 
```bash
stack build  
``` 

## Add Task3.hs code to stack project
- copy Task3.hs file into src folder in stack project
    
- change app/Main.hs in stack project 
add main function from Task3.hs to app/Main.hs
Change Task3.hs `module Main where` to `module Task3 where`
to app/Main.hs add `import Task3`, and imports needed for copied main

cmd command 
```bash
stack build  
``` 

### Task readme
https://git.mif.vu.lt/vipo/fp-2020/blob/master/README.md
